GLOBALS "OCGW03.inc"

DATABASE safre_viv

DEFINE v_s_qry          STRING
   DEFINE v_ruta                VARCHAR(150)
   DEFINE v_usuario             CHAR(40)
   DEFINE v_password            CHAR(20)
   DEFINE v_intentos            SMALLINT
   DEFINE v_ws_status           SMALLINT

MAIN
   CALL fn_consulta_relacion_laboral() RETURNING v_ws_status
END MAIN



FUNCTION fn_consulta_relacion_laboral()
   DEFINE v_nss            CHAR(11)
   DEFINE v_ws_status      SMALLINT -- estatus de ejecución del ws
   DEFINE v_indice         INTEGER
   DEFINE v_fecha_fin      DATE
   DEFINE v_fecha_f        DATE
   DEFINE v_fecha_c        STRING
   DEFINE v_con_sin_rl     SMALLINT
   DEFINE v_nrp            VARCHAR(100)
   DEFINE v_f_alta         DATE
   DEFINE v_f_alta_format  STRING
   DEFINE v_f_baja         DATE
   DEFINE v_delegacion     DATE
   DEFINE v_nom_delegacion VARCHAR(100)
   DEFINE v_r_nss          CHAR(11)
   DEFINE v_idx            SMALLINT

   LET v_idx       = 1
   LET v_ws_status = 0
   
   CALL fn_crea_temporal()

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
{
   LET v_s_qry = " SELECT nss
                     FROM safre_tmp:tmp_rec_det_ocg43
                    WHERE subproceso = 3 "
}
  LET v_s_qry = " SELECT nss
                     FROM safre_tmp:cat_nss43"
   DISPLAY v_s_qry

   PREPARE prp_c_tmp FROM v_s_qry
   DECLARE cur_c_tmp CURSOR FOR prp_c_tmp

   DISPLAY "ruta :",v_ruta

   FOREACH cur_c_tmp INTO v_nss
      LET ns1consultarTrabajador.nss = v_nss
   
      -- se invoca la consulta
      CALL consultarTrabajador_g(v_ruta) RETURNING v_ws_status
   
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
   
      FOR v_indice = 1 TO ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab.getLength()
         --se formatea la fecha
         LET v_fecha_c = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja
         LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
         LET v_fecha_f = DATE(v_fecha_c)
   
         --consulta fecha alta
         LET v_f_alta_format = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaAlta
         LET v_f_alta_format = v_f_alta_format.subString(6,7),"/",v_f_alta_format.subString(9,10),"/",v_f_alta_format.subString(1,4)
         LET v_f_alta = DATE(v_f_alta_format)
   
         LET v_r_nss          = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.nss
         LET v_delegacion     = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].delegacion
         LET v_nom_delegacion = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nomDelegacion
         LET v_nrp            = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nrp
   
         LET v_s_qry = "INSERT INTO safre_tmp:tmp_relacion_laboral values(?,?,?);"
   
         PREPARE prp_ins_tmp FROM v_s_qry
         EXECUTE prp_ins_tmp USING v_r_nss,
                                   v_f_alta,
                                   v_fecha_f
         DISPLAY v_r_nss
         DISPLAY v_f_alta
         DISPLAY v_fecha_f
   
      END FOR

      LET v_idx = v_idx + 1
   END FOREACH

   -- se devuelve el resultado de la ejecucion
   RETURN v_ws_status
   DISPLAY "status : ",v_ws_status
END FUNCTION

FUNCTION fn_crea_temporal()
      DATABASE safre_tmp
      LET v_s_qry = "DROP TABLE IF EXISTS tmp_relacion_laboral; \n
                     CREATE TABLE tmp_relacion_laboral(
                        nss     char(11),
                        f_alta  date,
                        f_baja  date
                        );"

      PREPARE prp_Crea_t FROM v_s_qry
      EXECUTE prp_Crea_t

      IF sqlca.sqlcode = 0 THEN
         DISPLAY "Se crea la tabla temporal de relaciones laborales" 
      END IF 
END FUNCTION 