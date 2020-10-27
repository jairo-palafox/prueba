##################################################################################
#Modulo             => OCG                                                       #
#Programa           => OCGL15                                                    #
#Objetivo           => Programa para recuperar relación laboral                  #
#Autor              => J. Eduardo Ventura                                        #
#Fecha inicio       => 17 enero del 2018                                         #
##################################################################################
DATABASE safre_viv

GLOBALS "OCGW03.inc"

MAIN
   DEFINE p_proceso_cod     INTEGER    
   DEFINE p_opera_cod       INTEGER 
   DEFINE p_usuario_cod     CHAR(20)
   DEFINE p_tipo_ejecucion  SMALLINT
   DEFINE p_s_titulo        CHAR(20)
   DEFINE r_b_valida        SMALLINT
   DEFINE v_d_folio         LIKE glo_folio.folio
   DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin
   DEFINE p_pid             INTEGER
   DEFINE p_tipo_proceso    SMALLINT
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados
   DEFINE v_estado          SMALLINT
   DEFINE v_s_qry           STRING
   DEFINE v_ruta            VARCHAR(150)
   DEFINE v_usuario         CHAR(40)
   DEFINE v_password        CHAR(20)
   DEFINE v_intentos        SMALLINT
   DEFINE v_nss             CHAR(11)
   DEFINE v_ws_status      SMALLINT -- estatus de ejecución del ws
   DEFINE v_indice         INTEGER
   DEFINE v_fecha_f        DATE
   DEFINE v_fecha_c        STRING
   DEFINE v_nrp            VARCHAR(100)
   DEFINE v_f_alta         DATE
   DEFINE v_f_alta_format  STRING
   DEFINE v_delegacion     DATE
   DEFINE v_nom_delegacion VARCHAR(100)
   DEFINE v_r_nss          CHAR(11)
   DEFINE v_idx            SMALLINT
   DEFINE v_today          DATE
   DEFINE v_nss_1          CHAR(11)
   DEFINE v_cnt_nss        SMALLINT
   DEFINE v_s_qry_1        STRING

   -- se asignan los parametros que vienen del fglrun
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_tipo_proceso   = ARG_VAL(5)
   LET p_s_titulo       = ARG_VAL(6)

   IF p_proceso_cod is null THEN
      LET p_proceso_cod = 3925
      LET p_opera_cod   = 1
   END IF

      -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --CLOSE WINDOW SCREEN

   DISPLAY ""
   DISPLAY "Inicia proceso para recuperación de relaciones laborales : "
   DISPLAY ""

   --DELETE 
   --  FROM ocg_relacion_laboral

   DATABASE safre_tmp
   DROP TABLE IF EXISTS tmp_ocg_relacion_laboral;
   CREATE TABLE tmp_ocg_relacion_laboral (nss       char(11),
                                          f_alta    date,
                                          f_baja    date,
                                          f_proceso date)
   DATABASE safre_viv

   ---cve_cliente = ocg_1 ruta para consulta de relación laboral en TRM
   LET v_s_qry = "SELECT ruta_servidor, 
                         usuario, 
                         password, 
                         num_reintento 
                    FROM wsv_cliente 
                   WHERE cve_cliente = 'ocg_1' "

   PREPARE prp_crl FROM v_s_qry
   EXECUTE prp_crl INTO v_ruta,
                        v_usuario,
                        v_password,
                        v_intentos

   LET v_ruta = v_ruta CLIPPED

   LET v_s_qry = " SELECT d.nss
                     FROM ocg_detalle d, ocg_formalizacion f
                    WHERE d.id_ocg_detalle = f.id_ocg_detalle
                      AND d.subproceso = 2
                      AND f.situacion in (50,60,70,80) "

   PREPARE prp_c_tmp FROM v_s_qry
   DECLARE cur_c_tmp CURSOR FOR prp_c_tmp

   DISPLAY "ruta WS : ",v_ruta

   FOREACH cur_c_tmp INTO v_nss

      IF v_nss IS NOT NULL THEN
      LET ns1consultarTrabajador.nss = v_nss
  
      -- se invoca la consulta
      CALL consultarTrabajador_g(v_ruta) RETURNING v_ws_status
   
      -- si el webservice NO se ejecuto correctamente
      IF ( v_ws_status <> 0 ) THEN
         DISPLAY "NSS con detalle :",v_nss
   
         DISPLAY "ERROR al invocar webservice de consulta de relación laboral"
         DISPLAY "CODE       : ", wsError.code
         DISPLAY "CODENS     : ", wsError.codeNS
         DISPLAY "DESRIPTION : ", wsError.description
         DISPLAY "ACTION     : ", wsError.action
   	  
   	  -- se devuelve el codigo de error del WS y fecha nula
         RETURN wsError.code
         CONTINUE FOREACH
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
         LET v_today = TODAY

         LET v_s_qry = "INSERT INTO safre_tmp:tmp_ocg_relacion_laboral VALUES(?,?,?,?);"

         PREPARE prp_ins_tmp FROM v_s_qry
         EXECUTE prp_ins_tmp USING v_r_nss,
                                   v_f_alta,
                                   v_fecha_f,
                                   v_today
   
      END FOR

      LET v_idx = v_idx + 1
      ELSE
         DISPLAY "SIN NSS"
         CONTINUE FOREACH
      END IF
   END FOREACH

   DISPLAY "Termina consumo de WS"


--**************************************************************
   DELETE FROM ocg_relacion_laboral 
         WHERE nss not in (
        SELECT d.nss
          FROM ocg_detalle d, ocg_formalizacion f
         WHERE d.id_ocg_detalle = f.id_ocg_detalle
           AND d.subproceso = 2
           AND f.situacion in (50,60,70,80))

DISPLAY "Punto de control 1"
--**************************************************************
   LET v_s_qry_1 = " SELECT d.nss
                     FROM ocg_detalle d, ocg_formalizacion f
                    WHERE d.id_ocg_detalle = f.id_ocg_detalle
                      AND d.subproceso = 2
                      AND f.situacion in (50,60,70,80)"

   PREPARE prp_c_tmp_1 FROM v_s_qry_1
   DECLARE cur_c_tmp_1 CURSOR FOR prp_c_tmp_1


   FOREACH cur_c_tmp_1 INTO v_nss_1
      SELECT count(*)
        INTO v_cnt_nss
        FROM safre_tmp:tmp_ocg_relacion_laboral
       WHERE nss = v_nss_1

      IF v_cnt_nss >= 1 THEN
         DELETE FROM ocg_relacion_laboral
               WHERE nss = v_nss_1

         INSERT INTO ocg_relacion_laboral
              SELECT nss,
                     f_alta,
                     f_baja,
                     f_proceso
                FROM safre_tmp:tmp_ocg_relacion_laboral
               WHERE nss = v_nss_1     
      END IF

   END FOREACH

--******************************************************************

   LET v_estado = 0

   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF

   DISPLAY ""
   DISPLAY "Fin de proceso"
   DISPLAY ""

END MAIN
