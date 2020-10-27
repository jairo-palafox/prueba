GLOBALS "OCGW07.inc"

DATABASE safre_viv

   DEFINE v_s_qry          STRING
   DEFINE v_ruta                VARCHAR(150)
   DEFINE v_usuario             CHAR(40)
   DEFINE v_password            CHAR(20)
   DEFINE v_intentos            SMALLINT
   DEFINE v_ws_status           SMALLINT
   DEFINE v_id_ocg_formalizacion DECIMAL(9,0)
   DEFINE v_id_derechohabiente   DECIMAL(9,0)
   DEFINE v_f_formalizacion      DATE

MAIN
   CALL fn_sdi() RETURNING v_ws_status
END MAIN



FUNCTION fn_sdi()
   DEFINE v_nss            CHAR(11)
   DEFINE v_ws_status      SMALLINT -- estatus de ejecución del ws
   DEFINE v_indice         INTEGER
   DEFINE v_f_ini          DATE
   DEFINE v_nrp            VARCHAR(100)
   DEFINE v_f_fin          DATE
   DEFINE v_idx            SMALLINT
   DEFINE v_nombre         VARCHAR(100)
   DEFINE v_salario        DECIMAL(12,2)

   LET v_idx       = 1
   LET v_ws_status = 0

   CALL fn_crea_temporal()
{
    LET v_s_qry = "SELECT ruta_servidor, 
                         usuario, 
                         password, 
                         num_reintento 
                    FROM safre_viv:wsv_cliente 
                   WHERE cve_cliente = 'ocg_1' "

   PREPARE prp_crl FROM v_s_qry
   EXECUTE prp_crl INTO v_ruta,
                        v_usuario,
                        v_password,
                        v_intentos

   LET v_ruta = v_ruta CLIPPED
}
  LET v_s_qry = " SELECT nss
                    FROM afi_derechohabiente
                   WHERE id_derechohabiente in (SELECT unique (id_derechohabiente)
                                                  FROM ocg_formalizacion
                                                 WHERE situacion in (55,60,70,80)
                                                   AND diagnostico = 1)"

   DISPLAY v_s_qry

   PREPARE prp_c_tmp FROM v_s_qry
   DECLARE cur_c_tmp CURSOR FOR prp_c_tmp

   DISPLAY "ruta :",v_ruta

   FOREACH cur_c_tmp INTO v_nss
      LET MT_SDI_req.I_NSS = v_nss

      SELECT id_derechohabiiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss

      SELECT MAX(id_ocg_formalizacion)
        INTO v_id_ocg_formalizacion
        FROM ocg_formalizacion
       WHERE id_derechohabiente = v_id_derechohabiente
         AND situacion in (55,60,70,80)
         AND diagnostico = 1

      SELECT f_formalizacion
        INTO v_f_formalizacion
        FROM ocg_acreditado
       WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
   
      -- se invoca la consulta

      LET v_ruta = "http://091402ai204.infonavit.net:50000/XISOAPAdapter/MessageServlet?senderParty=&senderService=BC_PORTAL&receiverParty=&receiverService=&interface=SI_SalarioDiarioIntegrado_SO&interfaceNamespace=http://infonavit.org.mx/SDI/PORTAL/sndSDI_NSS"
      CALL SI_SalarioDiarioIntegrado_SO_g(v_ruta) RETURNING v_ws_status
   
      -- si el webservice NO se ejecuto correctamente
      IF ( v_ws_status <> 0 ) THEN      
   
         DISPLAY "ERROR al invocar webservice de consulta de pago"
         DISPLAY "CODE       : ", wsError.code
         DISPLAY "CODENS     : ", wsError.codeNS
         DISPLAY "DESRIPTION : ", wsError.description
         DISPLAY "ACTION     : ", wsError.action
   	  
   	  -- se devuelve el codigo de error del WS y fecha nula
         RETURN wsError.code
      END IF
   
      FOR v_indice = 1 TO MT_SDI_res.ET_DATA.getLength()
         --se formatea la fecha
         LET v_f_ini = MT_SDI_res.ET_DATA[v_indice].FECHAF
         --LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
         --LET v_fecha_f = DATE(v_fecha_c)
   
         --consulta fecha alta
         LET v_f_fin = MT_SDI_res.ET_DATA[v_indice].FECHAT
         --LET v_f_alta_format = v_f_alta_format.subString(6,7),"/",v_f_alta_format.subString(9,10),"/",v_f_alta_format.subString(1,4)
         --LET v_f_alta = DATE(v_f_alta_format)

         LET v_nrp            = MT_SDI_res.ET_DATA[v_indice].NRP
         LET v_nombre         = MT_SDI_res.ET_DATA[v_indice].NOMBRE
         LET v_salario        = MT_SDI_res.ET_DATA[v_indice].SALARIO
   
         LET v_s_qry = "INSERT INTO safre_tmp:tmp_sdi(?,?,?,?,?,?);"
   
         PREPARE prp_ins_tmp FROM v_s_qry
         EXECUTE prp_ins_tmp USING v_nss,
                                   v_f_ini,
                                   v_f_fin,
                                   v_nrp,
                                   v_nombre,
                                   v_salario
   
      END FOR

      LET v_idx = v_idx + 1
   END FOREACH

   -- se devuelve el resultado de la ejecucion
   RETURN v_ws_status
   DISPLAY "status : ",v_ws_status
END FUNCTION

FUNCTION fn_crea_temporal()
      DATABASE safre_tmp
      LET v_s_qry = "DROP TABLE IF EXISTS tmp_sdi; \n
                     CREATE TABLE tmp_sdi(
                        nss      char(11),
                        f_ini    date,
                        f_fin    date,
                        nrp      varchar(40),
                        nombre   char(50),
                        salario  decimal(12,2)
                        );"

      PREPARE prp_Crea_t FROM v_s_qry
      EXECUTE prp_Crea_t

      IF sqlca.sqlcode = 0 THEN
         DISPLAY "Se crea la tabla temporal de salario diario integrado" 
      END IF 
END FUNCTION 
