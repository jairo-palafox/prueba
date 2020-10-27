###############################################################################
#Modulo            => AGR                                                     #
#Programa          => AGRS11                                                  #
#Objetivo          => Lanzado genera PDF y extractor de microfluj SACI.       #
#Autor             => Emilio Abarca EFP                                       #
#Fecha inicio      => 10/Abril/2017                                           #
###############################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE g_usuario        CHAR(20)   
   DEFINE p_pid            DECIMAL(9,0)
   DEFINE p_proceso_cod    INTEGER
   DEFINE p_opera_cod      INTEGER
   DEFINE g_ind_ejecuta    SMALLINT

   -- Record que recupera datos de los trámites y originaciones
   DEFINE r_datos RECORD
      id_cre_tramite     DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      num_credito        DECIMAL(10,0),
      estado             SMALLINT
   END RECORD  

   -- Variables que recuperan el acreditado
   DEFINE v_id_cre_acreditado DECIMAL(9,0)
   DEFINE v_sdo_deudor        DECIMAL(12,2)
   DEFINE v_tpo_credito       SMALLINT 
   DEFINE v_desc_credito      CHAR(30)

   TYPE rec_global RECORD
      estado            SMALLINT,
      tpo_credito       SMALLINT,
      desc_credito      CHAR(30),
      total             INTEGER,
      porcentaje        CHAR(6),
      t_deudores        INTEGER,
      t_no_deudor       INTEGER,
      sdo_deudor        DECIMAL(12,2),
      total_reg         CHAR(6),
      total_prc         CHAR(6),
      total_deudor      INTEGER,
      total_nd          INTEGER,
      total_sdo_deudor  DECIMAL(14,2)
   END RECORD

   TYPE arr_global DYNAMIC ARRAY OF rec_global

   -- Define record's
   DEFINE r_tmt_ex    rec_global
   DEFINE r_tmt_no_ex rec_global

   -- Define arreglos
   DEFINE arr_orig_ex    arr_global
   DEFINE arr_orig_no_ex arr_global

   # ARREGLOS CAUSALES DE RECHAZO
   -- Causales de rechazo trámites no exitosos
   DEFINE arr_rch_tmt  DYNAMIC ARRAY OF RECORD
      estado            SMALLINT,
      estado_rch        CHAR(4),
      rch_desc          CHAR(50),
      rch_concatena     CHAR(155),
      tpo_credito       SMALLINT,
      total             INTEGER,
      porcentaje        CHAR(6),
      t_deudores        INTEGER,
      t_no_deudor       INTEGER,
      sdo_deudor        DECIMAL(12,2),
      total_reg         CHAR(6),
      total_prc         CHAR(6),
      total_deudor      INTEGER,
      total_nd          INTEGER,
      total_sdo_deudor  DECIMAL(14,2) 
   END RECORD 

   -- Causales de rechazo originaciones no exitosas
   DEFINE arr_rch_orig  DYNAMIC ARRAY OF RECORD
      estado            SMALLINT,
      estado_rch        CHAR(4),
      rch_desc          CHAR(50),
      rch_concatena     CHAR(155),
      tpo_credito       SMALLINT,
      total             INTEGER,
      porcentaje        CHAR(6),
      t_deudores        INTEGER,
      t_no_deudor       INTEGER,
      sdo_deudor        DECIMAL(12,2),
      total_reg         CHAR(6),
      total_prc         CHAR(6),
      total_deudor      INTEGER,
      total_nd          INTEGER,
      total_sdo_deudor  DECIMAL(14,2) 
   END RECORD 

   DEFINE v_ruta_reporte   STRING 
   DEFINE v_ruta_envio     CHAR(40)
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_ruta_listados  CHAR(40)
   DEFINE v_f_ejecuta      CHAR(10)
   DEFINE v_estatus        BOOLEAN 
   DEFINE v_estatus_pdf    BOOLEAN
   DEFINE v_estatus_extr   BOOLEAN 
   DEFINE v_porcentaje     CHAR(4)
   DEFINE v_total_prc      CHAR(4)
   DEFINE v_suma           INTEGER
   DEFINE v_suma2          INTEGER
   DEFINE v_suma_rch_tmt   INTEGER 
   DEFINE v_suma_rch_orig  INTEGER 
   DEFINE v_arh_salida     STRING 
   DEFINE v_titulo_correo  STRING
   --DEFINE v_arh_correo     STRING 
   DEFINE v_mens_correo    STRING
   DEFINE g_f_procesa      DATE 
      
END GLOBALS 

MAIN

   LET g_usuario       = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = 339
   LET p_opera_cod     = 1
   LET g_ind_ejecuta   = ARG_VAL(5) -- Indicador que la ejecución fué manual desde el lanzador (AGRL61)

   LET g_f_procesa = TODAY -- Variable global para recuperar lo que se procesó en el día. 

   CALL STARTLOG(g_usuario CLIPPED || ".AGRS11.log")

   --Se obtiene la ruta donde se alojará el extractor
   SELECT ruta_envio,ruta_bin
     INTO v_ruta_envio,
          v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   -- Se obtiene la ruta donde se alojará el .PDF
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   CALL fn_display_proceso(0,"INICIA GENERACIÓN DEL INFORME MICROFLUJO")
   DISPLAY " Fecha proceso: ",g_f_procesa USING "dd/mm/yyyy"

   -- Crea tabla temporal para obtener causales de rechazo para el PDF.
   CALL crea_temporal()

   CALL fn_genera_extractor() RETURNING v_estatus_extr
   CALL fn_genera_pdf() RETURNING v_estatus_pdf

   IF (v_estatus_pdf <> 0) AND (v_estatus_extr <> 1)THEN
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_estatus
   ELSE 
      DISPLAY ""
      DISPLAY " => El archivo PDF y el Extractor de microflujo se ha generado correctamente"
      DISPLAY " => Los archivos de encuentran en la ruta /safreviv_int/agr/envio"
      DISPLAY ""
      DISPLAY " => Envío de correo del reporte y extractor generado"
      DISPLAY ""

      LET v_titulo_correo = "Proceso: INFORME MONITOREO MICROFLUJO SACI "
      -- LET v_arh_correo    = v_ruta_listados CLIPPED,"/",g_usuario CLIPPED,"-","AGRS11",".pdf"
      LET v_mens_correo   = "Pid proceso  : ",p_pid,"\n",
                         "Proceso      : MICROFLUJO INFORME \n",
                         "Operacion    : GENERA INFORME MICROFLUJO\n"
                         
      -- se invoca la función que envía por correo el elemento generado
      CALL fn_correo_proceso(p_pid,
                             p_proceso_cod,
                             p_opera_cod,
                             v_ruta_reporte,
                             v_titulo_correo,
                             v_mens_correo)
                           
      CALL fn_display_proceso(0,"FIN DE GENERACIÓN DEL INFORME MICROFLUJO")
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_estatus
   END IF 
      
END MAIN 

FUNCTION fn_genera_pdf()

   DEFINE reporte       om.SaxDocumentHandler -- Objeto para reporte PDF
   DEFINE v_reporte      STRING 
   DEFINE v_nombre_pdf   STRING 
   DEFINE v_query        STRING
   DEFINE v_indicador    BOOLEAN
   DEFINE k              INTEGER 
   DEFINE v_total_deudor INTEGER
   DEFINE v_total_nd     INTEGER 
   DEFINE v_suma_saldo   DECIMAL(14,2)
   DEFINE v_diagnostico  CHAR(4)
   DEFINE v_edo_rch_aux  CHAR(6)
   DEFINE p_fecha_inicio DATE 
   DEFINE p_fecha_fin    DATE 
      
   LET v_reporte    = v_ruta_bin CLIPPED, "/AGRS11.4rp"
   LET v_nombre_pdf = "Microflujo_informe.pdf"
   LET v_f_ejecuta  = TODAY USING "dd/mm/yyyy"

--*************** TRÁMITES EXITOSOS Y NO EXITOSOS (18,240)**********************

   LET v_query = "SELECT t.id_cre_tramite,
                         t.id_derechohabiente,
                         t.num_credito,
                         t.estado
                    FROM cre_tramite t,
                         cre_his_tramite h
                   WHERE t.id_cre_tramite = h.id_cre_tramite 
                     AND t.num_credito    = h.num_credito 
                     AND t.estado IN (18,240) 
                     AND t.estado = h.estado
                     AND DATE(h.f_proceso) = '",g_f_procesa,"'"

   INITIALIZE r_datos.* TO NULL 

   PREPARE prp_tmt_glo FROM v_query
   DECLARE crs_tmt_glo CURSOR FOR prp_tmt_glo

   FOREACH crs_tmt_glo INTO r_datos.id_cre_tramite,
                            r_datos.id_derechohabiente,
                            r_datos.num_credito,
                            r_datos.estado

      -- Por cada iteración resetea variables
      LET v_id_cre_acreditado = NULL
      LET v_tpo_credito       = NULL 
      LET v_desc_credito      = NULL 
      LET v_sdo_deudor        = NULL
      
      -- Obtiene el máximo acreditado
       SELECT FIRST 1 a.id_cre_acreditado,
                       a.tpo_credito,
                       r.desc_credito,
                       a.sdo_deudor
                  INTO v_id_cre_acreditado,
                       v_tpo_credito,
                       v_desc_credito,
                       v_sdo_deudor
                  FROM cre_acreditado a,
                       cat_tipo_credito r
                 WHERE a.id_derechohabiente = r_datos.id_derechohabiente
                   AND a.num_credito = r_datos.num_credito
                   AND a.tpo_credito = 10
                   AND a.tpo_credito = r.tpo_credito
                   AND a.tpo_originacion = r.tpo_originacion
                   ORDER BY a.id_cre_acreditadO DESC;

      IF(v_id_cre_acreditado IS NULL) THEN
         CONTINUE FOREACH
      ELSE 
         -- Guarda el registro en temporal
         INSERT INTO safre_tmp:tmp_datos_mic
                                   (id_cre_tramite    ,
                                    id_cre_acreditado ,
                                    id_derechohabiente,
                                    num_credito       ,       
                                    estado            , 
                                    tpo_credito       ,
                                    desc_credito      ,
                                    sdo_deudor        )
                           VALUES (r_datos.id_cre_tramite    ,
                                   v_id_cre_acreditado       ,
                                   r_datos.id_derechohabiente,
                                   r_datos.num_credito       ,
                                   r_datos.estado            ,
                                   v_tpo_credito             ,
                                   v_desc_credito            ,
                                   v_sdo_deudor);
      END IF 
      
   END FOREACH 

--*************** ORIGINACIONES EXITOSAS Y NO EXITOSAS (20,19)**********************

   LET v_query = "SELECT t.id_cre_tramite,
                         t.id_derechohabiente,
                         t.num_credito,
                         t.estado
                    FROM cre_tramite t,
                         cre_his_tramite h
                   WHERE t.id_cre_tramite = h.id_cre_tramite 
                     AND t.num_credito    = h.num_credito 
                     AND t.estado IN (20,19) 
                     AND t.estado = h.estado
                     AND DATE(h.f_proceso) = '",g_f_procesa,"'"

   INITIALIZE r_datos.* TO NULL 

   PREPARE prp_org_glo FROM v_query
   DECLARE crs_org_glo CURSOR FOR prp_org_glo

   FOREACH crs_org_glo INTO r_datos.id_cre_tramite,
                             r_datos.id_derechohabiente,
                             r_datos.num_credito,
                             r_datos.estado

      -- Por cada iteración resetea variables
      LET v_id_cre_acreditado = NULL
      LET v_tpo_credito       = NULL 
      LET v_desc_credito      = NULL 
      LET v_sdo_deudor        = NULL
      
      -- Obtiene el máximo acreditado
       SELECT FIRST 1 a.id_cre_acreditado,
                       a.tpo_credito,
                       r.desc_credito,
                       a.sdo_deudor
                  INTO v_id_cre_acreditado,
                       v_tpo_credito,
                       v_desc_credito,
                       v_sdo_deudor
                  FROM cre_acreditado a,
                       cat_tipo_credito r
                 WHERE a.id_derechohabiente = r_datos.id_derechohabiente
                   AND a.num_credito = r_datos.num_credito
                   AND a.tpo_credito = r.tpo_credito
                   AND a.tpo_originacion = r.tpo_originacion
                   ORDER BY a.id_cre_acreditadO DESC;

      IF(v_id_cre_acreditado IS NULL) THEN
         CONTINUE FOREACH
      ELSE 
         -- Guarda el registro en temporal
         INSERT INTO safre_tmp:tmp_datos_mic
                                   (id_cre_tramite    ,
                                    id_cre_acreditado ,
                                    id_derechohabiente,
                                    num_credito       ,       
                                    estado            , 
                                    tpo_credito       ,
                                    desc_credito      ,
                                    sdo_deudor        )
                            VALUES (r_datos.id_cre_tramite    ,
                                    v_id_cre_acreditado       ,
                                    r_datos.id_derechohabiente,
                                    r_datos.num_credito       ,
                                    r_datos.estado            ,
                                    v_tpo_credito             ,
                                    v_desc_credito            ,
                                    v_sdo_deudor);
      END IF 
      
   END FOREACH 


# ARREGLOS ---------> TRÁMITES EXITOSOS (18)

   --Inicializa arreglo en caso de que el cursor no obtenga registros
   LET r_tmt_ex.estado           = 0
   LET r_tmt_ex.tpo_credito      = 0
   LET r_tmt_ex.total            = 0
   LET r_tmt_ex.porcentaje       = 0
   LET r_tmt_ex.t_deudores       = 0
   LET r_tmt_ex.t_no_deudor      = 0
   LET r_tmt_ex.sdo_deudor       = 0
   LET r_tmt_ex.total_reg        = 0
   LET r_tmt_ex.total_prc        = 0
   LET r_tmt_ex.total_deudor     = 0
   LET r_tmt_ex.total_nd         = 0
   LET r_tmt_ex.total_sdo_deudor = 0

   -- Inicializando variables
   LET v_porcentaje = 0
   
   LET v_query = " SELECT tpo_credito,
                          desc_credito,COUNT(*)
                     FROM safre_tmp:tmp_datos_mic
                    WHERE estado     = 18
                      AND tpo_credito = 10
                      GROUP BY 1,2;"

   PREPARE prp_obt_tmt FROM v_query 
   DECLARE crs_obt_tmt CURSOR FOR prp_obt_tmt

   FOREACH crs_obt_tmt INTO r_tmt_ex.tpo_credito,
                             r_tmt_ex.desc_credito,
                             r_tmt_ex.total

      -- Obtiene total deudores por tipo de crédito
      SELECT COUNT(*)
         INTO r_tmt_ex.t_deudores
         FROM safre_tmp:tmp_datos_mic
        WHERE estado      = 18
          AND tpo_credito = 10
          AND sdo_deudor > 0;

      -- Obtiene total no deudor
      SELECT COUNT(*)
         INTO r_tmt_ex.t_no_deudor
         FROM safre_tmp:tmp_datos_mic
        WHERE estado      = 18
          AND tpo_credito = 10
          AND (sdo_deudor = 0
           OR sdo_deudor IS NULL 
           OR sdo_deudor = "");

      -- Obtiene suma total deudores
      SELECT SUM(sdo_deudor)
         INTO r_tmt_ex.sdo_deudor
         FROM safre_tmp:tmp_datos_mic
        WHERE estado      = 18
          AND tpo_credito = 10
          AND sdo_deudor > 0;

      IF(r_tmt_ex.sdo_deudor IS NULL) THEN
         LET r_tmt_ex.sdo_deudor = 0
      END IF 
           
   END FOREACH 

   LET r_tmt_ex.total_reg = r_tmt_ex.total
   
   -- Calcula porcentaje trámites exitosos
   LET v_porcentaje = (r_tmt_ex.total / r_tmt_ex.total_reg) * 100
   LET r_tmt_ex.porcentaje = v_porcentaje CLIPPED,"%"

#---------> TRÁMITES NO EXITOSOS (240)

   --Inicializa arreglo en caso de que el cursor no obtenga registros
   LET r_tmt_no_ex.estado           = 0
   LET r_tmt_no_ex.tpo_credito      = 0
   LET r_tmt_no_ex.total            = 0
   LET r_tmt_no_ex.porcentaje       = 0
   LET r_tmt_no_ex.t_deudores       = 0
   LET r_tmt_no_ex.t_no_deudor      = 0
   LET r_tmt_no_ex.sdo_deudor       = 0
   LET r_tmt_no_ex.total_reg        = 0
   LET r_tmt_no_ex.total_prc        = 0
   LET r_tmt_no_ex.total_deudor     = 0
   LET r_tmt_no_ex.total_nd         = 0
   LET r_tmt_no_ex.total_sdo_deudor = 0
   
   -- Inicializando variables
   LET v_porcentaje = 0
   
   LET v_query = " SELECT tpo_credito,
                          desc_credito,COUNT(*)
                     FROM safre_tmp:tmp_datos_mic
                    WHERE estado      = 240
                      AND tpo_credito = 10
                      GROUP BY 1,2;"

   PREPARE prp_obt_tmt2 FROM v_query 
   DECLARE crs_obt_tmt2 CURSOR FOR prp_obt_tmt2

   FOREACH crs_obt_tmt2 INTO r_tmt_no_ex.tpo_credito,
                              r_tmt_no_ex.desc_credito,
                              r_tmt_no_ex.total

      -- Obtiene total deudores por tipo de crédito
      SELECT COUNT(*)
         INTO r_tmt_no_ex.t_deudores
         FROM safre_tmp:tmp_datos_mic
        WHERE estado      = 240
          AND tpo_credito = 10
          AND sdo_deudor > 0;

      -- Obtiene total no deudor
      SELECT COUNT(*)
         INTO r_tmt_no_ex.t_no_deudor
         FROM safre_tmp:tmp_datos_mic
        WHERE estado      = 240
          AND tpo_credito = 10
          AND (sdo_deudor = 0
           OR sdo_deudor IS NULL 
           OR sdo_deudor = "");

      -- Obtiene suma total deudores
      SELECT SUM(sdo_deudor)
         INTO r_tmt_no_ex.sdo_deudor
         FROM safre_tmp:tmp_datos_mic
        WHERE estado      = 240
          AND tpo_credito = 10
          AND sdo_deudor > 0;

      IF(r_tmt_no_ex.sdo_deudor IS NULL) THEN
         LET r_tmt_no_ex.sdo_deudor = 0
      END IF 
           
   END FOREACH 

   LET r_tmt_no_ex.total_reg = r_tmt_no_ex.total

   -- Calcula porcentaje trámites exitosos
   LET v_porcentaje = (r_tmt_no_ex.total / r_tmt_no_ex.total_reg) * 100
   LET r_tmt_no_ex.porcentaje = v_porcentaje CLIPPED,"%"

#---------> ORIGINACIONES EXITOSAS (20)

   --Inicializa arreglo en caso de que el cursor no obtenga registros}
   LET arr_orig_ex[1].estado           = 0
   LET arr_orig_ex[1].tpo_credito      = 0
   LET arr_orig_ex[1].total            = 0
   LET arr_orig_ex[1].porcentaje       = 0
   LET arr_orig_ex[1].t_deudores       = 0
   LET arr_orig_ex[1].t_no_deudor      = 0
   LET arr_orig_ex[1].sdo_deudor       = 0
   LET arr_orig_ex[1].total_reg        = 0
   LET arr_orig_ex[1].total_prc        = 0
   LET arr_orig_ex[1].total_deudor     = 0
   LET arr_orig_ex[1].total_nd         = 0
   LET arr_orig_ex[1].total_sdo_deudor = 0

   LET v_query = " SELECT tpo_credito,
                          desc_credito,COUNT(*)
                     FROM safre_tmp:tmp_datos_mic
                    WHERE estado      = 20
                    GROUP BY 1,2;"

   PREPARE prp_obt_org FROM v_query 
   DECLARE crs_obt_org CURSOR FOR prp_obt_org

   -- Inicializa variables de conteo 
   LET k      = 1
   LET v_suma = 0
   LET v_suma_saldo   = 0
   LET v_total_deudor = 0
   LET v_total_nd     = 0
   
   FOREACH crs_obt_org INTO arr_orig_ex[k].tpo_credito,
                             arr_orig_ex[k].desc_credito,
                             arr_orig_ex[k].total

      IF(arr_orig_ex[k].tpo_credito IS NOT NULL) THEN 

         LET v_suma = v_suma + arr_orig_ex[k].total

         -- Obtiene total deudores por tipo de crédito
         SELECT COUNT(*)
            INTO arr_orig_ex[k].t_deudores
            FROM safre_tmp:tmp_datos_mic
           WHERE estado      = 20
             AND tpo_credito = arr_orig_ex[k].tpo_credito
             AND sdo_deudor > 0;

         -- Obtiene total no deudor
         SELECT COUNT(*)
            INTO arr_orig_ex[k].t_no_deudor
            FROM safre_tmp:tmp_datos_mic
           WHERE estado      = 20
             AND tpo_credito = arr_orig_ex[k].tpo_credito
             AND (sdo_deudor = 0
              OR sdo_deudor IS NULL 
              OR sdo_deudor = "");

         -- Obtiene suma total deudores
         SELECT SUM(sdo_deudor)
            INTO arr_orig_ex[k].sdo_deudor
            FROM safre_tmp:tmp_datos_mic
           WHERE estado      = 20
             AND tpo_credito = arr_orig_ex[k].tpo_credito
             AND sdo_deudor > 0;

         IF(arr_orig_ex[k].sdo_deudor IS NULL) THEN
            LET arr_orig_ex[k].sdo_deudor = 0
         END IF 

         -- Suma total saldo deudor
         LET v_suma_saldo = v_suma_saldo + arr_orig_ex[k].sdo_deudor

         -- Suma Total deudores
         LET v_total_deudor = v_total_deudor + arr_orig_ex[k].t_deudores

         -- Suma total de No deudores
         LET v_total_nd = v_total_nd + arr_orig_ex[k].t_no_deudor
         
      END IF

      -- Incrementa contador
      LET k = k + 1 
      
   END FOREACH 

   IF (k > 1) THEN 
      -- Total de registros
      LET arr_orig_ex[k -1].total_reg = v_suma

      -- Total deudores
      LET arr_orig_ex[k-1].total_deudor = v_total_deudor

      -- Total de No Deudores
      LET arr_orig_ex[k-1].total_nd = v_total_nd

      -- Suma total saldo deudor
      LET arr_orig_ex[k-1].total_sdo_deudor = v_suma_saldo
      
   END IF 

   -- Elimina la última fila en blanco
   IF(arr_orig_ex[arr_orig_ex.getLength()].tpo_credito IS NULL) AND 
     (arr_orig_ex[arr_orig_ex.getLength()].desc_credito IS NULL) THEN 
      CALL arr_orig_ex.deleteElement(arr_orig_ex.getLength()) 
   END IF 

#---------> ORIGINACIONES NO EXITOSAS (19)

   -- Inicializa arrego en caso de que el cursor no recupere registros
   LET arr_orig_no_ex[1].estado           = 0
   LET arr_orig_no_ex[1].tpo_credito      = 0
   LET arr_orig_no_ex[1].total            = 0
   LET arr_orig_no_ex[1].porcentaje       = 0
   LET arr_orig_no_ex[1].t_deudores       = 0
   LET arr_orig_no_ex[1].t_no_deudor      = 0
   LET arr_orig_no_ex[1].sdo_deudor       = 0
   LET arr_orig_no_ex[1].total_reg        = 0
   LET arr_orig_no_ex[1].total_prc        = 0
   LET arr_orig_no_ex[1].total_deudor     = 0
   LET arr_orig_no_ex[1].total_nd         = 0
   LET arr_orig_no_ex[1].total_sdo_deudor = 0

    LET v_query = " SELECT tpo_credito,
                          desc_credito,COUNT(*)
                     FROM safre_tmp:tmp_datos_mic
                    WHERE estado      = 19
                    GROUP BY 1,2;"

   PREPARE prp_obt_org2 FROM v_query 
   DECLARE crs_obt_org2 CURSOR FOR prp_obt_org2

   -- Inicializa variables de conteo 
   LET k      = 1
   LET v_suma2 = 0
   LET v_suma_saldo   = 0
   LET v_total_deudor = 0
   LET v_total_nd     = 0
   
   FOREACH crs_obt_org2 INTO arr_orig_no_ex[k].tpo_credito,
                             arr_orig_no_ex[k].desc_credito,
                             arr_orig_no_ex[k].total

      IF(arr_orig_no_ex[k].tpo_credito IS NOT NULL) THEN 

         LET v_suma2 = v_suma2 + arr_orig_no_ex[k].total

         -- Obtiene deudores
         -- Obtiene total deudores por tipo de crédito
         SELECT COUNT(*)
            INTO arr_orig_no_ex[k].t_deudores
            FROM safre_tmp:tmp_datos_mic
           WHERE estado      = 19
             AND tpo_credito = arr_orig_no_ex[k].tpo_credito
             AND sdo_deudor > 0;

         -- Obtiene total no deudor
         SELECT COUNT(*)
            INTO arr_orig_no_ex[k].t_no_deudor
            FROM safre_tmp:tmp_datos_mic
           WHERE estado      = 19
             AND tpo_credito = arr_orig_no_ex[k].tpo_credito
             AND (sdo_deudor = 0
              OR sdo_deudor IS NULL 
              OR sdo_deudor = "");

         -- Obtiene suma total deudores
         SELECT SUM(sdo_deudor)
            INTO arr_orig_no_ex[k].sdo_deudor
            FROM safre_tmp:tmp_datos_mic
           WHERE estado      = 19
             AND tpo_credito = arr_orig_no_ex[k].tpo_credito
             AND sdo_deudor > 0;

         IF(arr_orig_no_ex[k].sdo_deudor IS NULL) THEN
            LET arr_orig_no_ex[k].sdo_deudor = 0
         END IF 

         -- Suma total saldo deudor
         LET v_suma_saldo = v_suma_saldo + arr_orig_no_ex[k].sdo_deudor

         -- Suma Total deudores
         LET v_total_deudor = v_total_deudor + arr_orig_no_ex[k].t_deudores

         -- Suma total de No deudores
         LET v_total_nd = v_total_nd + arr_orig_no_ex[k].t_no_deudor
         
      END IF

      -- Incrementa contador
      LET k = k + 1 
      
   END FOREACH 

   IF (k > 1) THEN 
      -- Total de registros
      LET arr_orig_no_ex[k -1].total_reg = v_suma2

      -- Total deudores
      LET arr_orig_no_ex[k-1].total_deudor = v_total_deudor

      -- Total de No Deudores
      LET arr_orig_no_ex[k-1].total_nd = v_total_nd

      -- Suma total saldo deudor
      LET arr_orig_no_ex[k-1].total_sdo_deudor = v_suma_saldo
      
   END IF 

   -- Elimina la última fila en blanco
   IF(arr_orig_no_ex[arr_orig_no_ex.getLength()].tpo_credito IS NULL) AND 
     (arr_orig_no_ex[arr_orig_no_ex.getLength()].desc_credito IS NULL) THEN 
      CALL arr_orig_no_ex.deleteElement(arr_orig_no_ex.getLength()) 
   END IF 


-- Trámites No Exitosos (CAUSAL DE RECHAZO)
   -- Inicializando variables
   LET v_diagnostico  = NULL 
   LET v_porcentaje   = 0
   LET v_total_prc    = 0

   LET v_query = "SELECT estado,
                         estado_rch,
                         desc_rch,
                         COUNT(*)
                    FROM safre_tmp:tmp_rch_tramite
                    GROUP BY 1,2,3"

    --Inicializa arreglo en caso de que el cursor no obtenga registros
   LET arr_rch_tmt[1].estado           = 0
   LET arr_rch_tmt[1].estado_rch       = 0
   LET arr_rch_tmt[1].tpo_credito      = 0
   LET arr_rch_tmt[1].total            = 0
   LET arr_rch_tmt[1].porcentaje       = 0
   LET arr_rch_tmt[1].t_deudores       = 0
   LET arr_rch_tmt[1].t_no_deudor      = 0
   LET arr_rch_tmt[1].sdo_deudor       = 0
   LET arr_rch_tmt[1].total_reg        = 0
   LET arr_rch_tmt[1].total_prc        = 0
   LET arr_rch_tmt[1].total_deudor     = 0
   LET arr_rch_tmt[1].total_nd         = 0
   LET arr_rch_tmt[1].total_sdo_deudor = 0
   
   PREPARE prp_rch_tmt FROM v_query
   DECLARE crs_rch_tmt CURSOR FOR prp_rch_tmt

   LET k = 1

   -- Inicializa variables
   LET v_suma_rch_tmt = 0
   LET v_suma_saldo   = 0
   LET v_total_deudor = 0
   LET v_total_nd     = 0
   
   FOREACH crs_rch_tmt INTO arr_rch_tmt[k].estado,
                             arr_rch_tmt[k].estado_rch,
                             arr_rch_tmt[k].rch_desc,
                             arr_rch_tmt[k].total

                             
      -- Concatena descripción del rechazo
      LET arr_rch_tmt[k].rch_concatena = arr_rch_tmt[k].estado_rch CLIPPED,"-",arr_rch_tmt[k].rch_desc
         
      -- Suma total de regitros 
      LET v_suma_rch_tmt = v_suma_rch_tmt + arr_rch_tmt[k].total

      IF (arr_rch_tmt[k].estado IS NOT NULL) THEN 

         IF (arr_rch_tmt[k].estado_rch IS NOT NULL) AND 
            (arr_rch_tmt[k].estado_rch <> "    ") THEN 

            LET v_edo_rch_aux = '"',arr_rch_tmt[k].estado_rch,'"'

            -- Obtiene saldo deudor por causal de rechazo
            SELECT SUM (sdo_deudor),COUNT(*)
              INTO arr_rch_tmt[k].sdo_deudor,
                   arr_rch_tmt[k].t_deudores
              FROM safre_tmp:tmp_rch_tramite
             WHERE estado     = arr_rch_tmt[k].estado
               AND estado_rch = arr_rch_tmt[k].estado_rch
               AND sdo_deudor > 0;
          
            -- Obtiene No deudor
            SELECT COUNT(*)
              INTO arr_rch_tmt[k].t_no_deudor
              FROM safre_tmp:tmp_rch_tramite
             WHERE estado     = arr_rch_tmt[k].estado
               AND estado_rch = arr_rch_tmt[k].estado_rch
               AND (sdo_deudor = 0
                OR  sdo_deudor IS NULL 
                OR  sdo_deudor = "") 
      
        ELSE 
             -- Obtiene saldo deudor por causal de rechazo
            SELECT SUM (sdo_deudor),COUNT(*)
              INTO arr_rch_tmt[k].sdo_deudor,
                   arr_rch_tmt[k].t_deudores
              FROM safre_tmp:tmp_rch_tramite
             WHERE estado     = arr_rch_tmt[k].estado
               AND estado_rch IS NULL
                OR estado_rch = "    "
               AND sdo_deudor > 0;

            -- Obtiene No deudor
            SELECT COUNT(*)
              INTO arr_rch_tmt[k].t_no_deudor
              FROM safre_tmp:tmp_rch_tramite
             WHERE estado     = arr_rch_tmt[k].estado
               AND estado_rch IS NULL
               OR estado_rch = "    " 
               AND (sdo_deudor = 0
                OR  sdo_deudor IS NULL 
                OR  sdo_deudor = "") 
        END IF 

          IF (arr_rch_tmt[k].sdo_deudor IS NULL) THEN 
            LET arr_rch_tmt[k].sdo_deudor = 0
          END IF 

        -- Obtiene saldo
          LET v_suma_saldo = v_suma_saldo + arr_rch_tmt[k].sdo_deudor

         -- Suma deudores
         LET v_total_deudor = v_total_deudor + arr_rch_tmt[k].t_deudores


        LET v_total_nd = v_total_nd + arr_rch_tmt[k].t_no_deudor 
         
      END IF 

      LET k = k + 1
 
      
   END FOREACH 

   IF (k > 1) THEN
      -- Total de registros
      LET arr_rch_tmt[k-1].total_reg = v_suma_rch_tmt
      
      -- Total deudores
      LET arr_rch_tmt[k-1].total_deudor = v_total_deudor

       -- Total de No Deudores
      LET arr_rch_tmt[k-1].total_nd = v_total_nd

      -- Suma total saldo deudor
      LET arr_rch_tmt[k-1].total_sdo_deudor = v_suma_saldo

   END IF 

    -- Elimina la última fila en blanco
   IF (arr_rch_tmt[arr_rch_tmt.getLength()].estado IS NULL) AND 
      (arr_rch_tmt[arr_rch_tmt.getLength()].tpo_credito IS NULL) THEN
      CALL arr_rch_tmt.deleteElement(arr_rch_tmt.getLength())
   END IF 

-- Originaciones No Exitosas (CAUSAL DE RECHAZO)
   LET v_porcentaje   = 0
   LET v_total_prc    = 0
   LET v_edo_rch_aux  = NULL 

   LET v_query = "SELECT estado,
                         estado_rch,
                         desc_rch,
                         COUNT(*)
                    FROM safre_tmp:tmp_rch_originacion
                    GROUP BY 1,2,3"
   
   PREPARE prp_rch_orig FROM v_query
   DECLARE crs_rch_orig CURSOR FOR prp_rch_orig

   LET k = 1

   --Inicializa arreglo en caso de que el cursor no obtenga registros
   LET arr_rch_orig[1].estado           = 0
   LET arr_rch_orig[1].estado_rch       = 0
   LET arr_rch_orig[1].tpo_credito      = 0
   LET arr_rch_orig[1].total            = 0
   LET arr_rch_orig[1].porcentaje       = 0
   LET arr_rch_orig[1].t_deudores       = 0
   LET arr_rch_orig[1].t_no_deudor      = 0
   LET arr_rch_orig[1].sdo_deudor       = 0
   LET arr_rch_orig[1].total_reg        = 0
   LET arr_rch_orig[1].total_prc        = 0
   LET arr_rch_orig[1].total_deudor     = 0
   LET arr_rch_orig[1].total_nd         = 0
   LET arr_rch_orig[1].total_sdo_deudor = 0

   -- Inicializa valores
   LET v_suma_rch_orig = 0
   LET v_suma_saldo    = 0
   LET v_total_deudor  = 0
   LET v_total_nd      = 0

   FOREACH crs_rch_orig INTO arr_rch_orig[k].estado,
                             arr_rch_orig[k].estado_rch,
                             arr_rch_orig[k].rch_desc,
                             arr_rch_orig[k].total

         -- Concatena descripción del rechazo
         LET arr_rch_orig[k].rch_concatena = arr_rch_orig[k].estado_rch CLIPPED,"-",arr_rch_orig[k].rch_desc
         
      -- Suma total de regitros 
      LET v_suma_rch_orig = v_suma_rch_orig + arr_rch_orig[k].total

      IF (arr_rch_orig[k].estado IS NOT NULL) THEN 

         IF (arr_rch_orig[k].estado_rch IS NOT NULL) AND 
            (arr_rch_orig[k].estado_rch <> "    ")THEN 
            
            LET v_edo_rch_aux = '"',arr_rch_orig[k].estado_rch,'"'

            -- Obtiene saldo deudor por causal de rechazo
            SELECT SUM (sdo_deudor),COUNT(*)
              INTO arr_rch_orig[k].sdo_deudor,
                   arr_rch_orig[k].t_deudores
              FROM safre_tmp:tmp_rch_originacion
             WHERE estado     = arr_rch_orig[k].estado
               AND estado_rch = arr_rch_orig[k].estado_rch
               AND sdo_deudor > 0;

            -- Obtiene No deudor
            SELECT COUNT(*)
              INTO arr_rch_orig[k].t_no_deudor
              FROM safre_tmp:tmp_rch_originacion
             WHERE estado     = arr_rch_orig[k].estado
               AND estado_rch = arr_rch_orig[k].estado_rch
               AND (sdo_deudor = 0
                OR  sdo_deudor IS NULL 
                OR  sdo_deudor = "") 

        ELSE 
             -- Obtiene saldo deudor por causal de rechazo
            SELECT SUM (sdo_deudor),COUNT(*)
              INTO arr_rch_orig[k].sdo_deudor,
                   arr_rch_orig[k].t_deudores
              FROM safre_tmp:tmp_rch_originacion
             WHERE estado     = arr_rch_orig[k].estado
               AND estado_rch IS NULL 
               OR estado_rch = "    "
               AND sdo_deudor > 0;

            -- Obtiene No deudor
            SELECT COUNT(*)
              INTO arr_rch_orig[k].t_no_deudor
              FROM safre_tmp:tmp_rch_originacion
             WHERE estado     = arr_rch_orig[k].estado
               AND estado_rch IS NULL
               OR estado_rch = "    " 
               AND (sdo_deudor = 0
                OR  sdo_deudor IS NULL 
                OR  sdo_deudor = "") 
        END IF 

          IF (arr_rch_orig[k].sdo_deudor IS NULL) THEN 
            LET arr_rch_orig[k].sdo_deudor = 0
          END IF 

         -- Saldo total 
         LET v_suma_saldo = v_suma_saldo + arr_rch_orig[k].sdo_deudor

         --Suma total deudores
         LET v_total_deudor = v_total_deudor + arr_rch_orig[k].t_deudores


         LET v_total_nd = v_total_nd + arr_rch_orig[k].t_no_deudor 
         
      END IF 

      LET k = k + 1
      
   END FOREACH 

   IF (k > 1) THEN
      -- Total de registros
      LET arr_rch_orig[k-1].total_reg = v_suma_rch_orig
      
      -- Total deudores
      LET arr_rch_orig[k-1].total_deudor = v_total_deudor

       -- Total de No Deudores
       LET arr_rch_orig[k-1].total_nd = v_total_nd

      -- Suma total saldo deudor
      LET arr_rch_orig[k-1].total_sdo_deudor = v_suma_saldo

   END IF 

    -- Elimina la última fila en blanco
   IF (arr_rch_orig[arr_rch_orig.getLength()].estado IS NULL) THEN
      CALL arr_rch_orig.deleteElement(arr_rch_orig.getLength())
   END IF 
   
   # Carga del Reporte PDF
   LET v_ruta_reporte = v_ruta_listados CLIPPED,"/",
                        g_usuario CLIPPED,"-",
                        "AGRS11","-",
                        p_pid USING "&&&&&","-",
                        p_proceso_cod USING "&&&&&","-",
                        p_opera_cod USING "&&&&&",".pdf"
    
   IF (fgl_report_loadCurrentSettings(v_reporte)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()

      IF (reporte IS NOT NULL) THEN
         
         LET v_indicador = 0
         
         START REPORT resultados TO XML HANDLER reporte

            OUTPUT TO REPORT resultados()

         FINISH REPORT resultados  

      END IF 

   END IF 

   RETURN v_indicador
   
END FUNCTION 

REPORT resultados ()

   DEFINE z              INTEGER
 
   FORMAT 

      FIRST PAGE HEADER 
        PRINTX g_usuario
        PRINTX v_f_ejecuta

      ON EVERY ROW 

         -- Salida Trámites Exitosos
         IF(r_tmt_ex.estado IS NOT NULL) THEN 
            PRINTX r_tmt_ex.desc_credito
            PRINTX r_tmt_ex.total
            PRINTX r_tmt_ex.porcentaje
            PRINTX r_tmt_ex.t_deudores
            PRINTX r_tmt_ex.t_no_deudor
            PRINTX r_tmt_ex.sdo_deudor
            PRINTX r_tmt_ex.total_reg USING "###,###,###"
         END IF 
     
         -- Limpia  variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0
         
         -- Salida Originaciones Exitosas
         FOR z = 1 TO arr_orig_ex.getLength()

            -- Obtiene porcentaje por cada crédito
            LET v_porcentaje = (arr_orig_ex[z].total / v_suma ) * 100
            LET arr_orig_ex[z].porcentaje = v_porcentaje CLIPPED,"%"

            -- Obtiene porcentaje total
            LET v_total_prc = v_total_prc + v_porcentaje
            LET arr_orig_ex[z].total_prc = v_total_prc CLIPPED,"%"

            -- Imprime valores
           PRINTX arr_orig_ex[z].desc_credito 
           PRINTX arr_orig_ex[z].total
           PRINTX arr_orig_ex[z].porcentaje 
           PRINTX arr_orig_ex[z].t_deudores
           PRINTX arr_orig_ex[z].t_no_deudor 
           PRINTX arr_orig_ex[z].sdo_deudor
           PRINTX arr_orig_ex[z].total_reg USING "###,###,###"
           PRINTX arr_orig_ex[z].total_prc
           PRINTX arr_orig_ex[z].total_deudor
           PRINTX arr_orig_ex[z].total_nd 
           PRINTX arr_orig_ex[z].total_sdo_deudor 

         END FOR 

         -- Salida de Trámites no exitosos
         IF(r_tmt_ex.estado IS NOT NULL) THEN 
         
            PRINTX r_tmt_no_ex.desc_credito
            PRINTX r_tmt_no_ex.total
            PRINTX r_tmt_no_ex.porcentaje
            PRINTX r_tmt_no_ex.t_deudores
            PRINTX r_tmt_no_ex.t_no_deudor
            PRINTX r_tmt_no_ex.sdo_deudor
            PRINTX r_tmt_no_ex.total_reg USING "###,###,###"
            
         END IF

         -- Limpia variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0
         
         -- Salida Originaciones No Exitosas
         FOR z = 1 TO arr_orig_no_ex.getLength()

            -- Obtiene porcentaje por cada crédito
            LET v_porcentaje = (arr_orig_no_ex[z].total / v_suma2 ) * 100
            LET arr_orig_no_ex[z].porcentaje = v_porcentaje CLIPPED,"%"

            -- Obtiene porcentaje total
            LET v_total_prc = v_total_prc + v_porcentaje
            LET arr_orig_no_ex[z].total_prc = v_total_prc CLIPPED,"%"
            
            PRINTX arr_orig_no_ex[z].desc_credito
            PRINTX arr_orig_no_ex[z].total
            PRINTX arr_orig_no_ex[z].porcentaje
            PRINTX arr_orig_no_ex[z].t_deudores
            PRINTX arr_orig_no_ex[z].t_no_deudor 
            PRINTX arr_orig_no_ex[z].sdo_deudor
            PRINTX arr_orig_no_ex[z].total_reg USING "###,###,###"
            PRINTX arr_orig_no_ex[z].total_prc
            PRINTX arr_orig_no_ex[z].total_deudor
            PRINTX arr_orig_no_ex[z].total_nd 
            PRINTX arr_orig_no_ex[z].total_sdo_deudor 
            
         END FOR

         -- Resetea variables
          LET v_porcentaje = 0
          LET v_total_prc  = 0

         -- Salida de la causal de rechazos para trámites no exitosos
         FOR z = 1 TO arr_rch_tmt.getLength()

            --Obtiene porcentaje por cada causal de rechazo
            LET v_porcentaje = (arr_rch_tmt[z].total / v_suma_rch_tmt ) * 100
            LET arr_rch_tmt[z].porcentaje = v_porcentaje CLIPPED,"%"

            -- Obtiene porcentaje total
            LET v_total_prc = v_total_prc + v_porcentaje
            LET arr_rch_tmt[z].total_prc = v_total_prc CLIPPED,"%"

            PRINTX arr_rch_tmt[z].rch_concatena
            PRINTX arr_rch_tmt[z].total
            PRINTX arr_rch_tmt[z].total_reg USING "###,###,###"
            PRINTX arr_rch_tmt[z].porcentaje
            PRINTX arr_rch_tmt[z].total_prc
            PRINTX arr_rch_tmt[z].t_deudores
            PRINTX arr_rch_tmt[z].total_deudor
            PRINTX arr_rch_tmt[z].sdo_deudor
            PRINTX arr_rch_tmt[z].total_sdo_deudor
            PRINTX arr_rch_tmt[z].t_no_deudor
            PRINTX arr_rch_tmt[z].total_nd
            
         END FOR 

         -- Resetea variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0
          
         FOR z=1 TO arr_rch_orig.getLength()

            --Obtiene porcentaje por cada causal de rechazo
            LET v_porcentaje = (arr_rch_orig[z].total / v_suma_rch_orig ) * 100
            LET arr_rch_orig[z].porcentaje = v_porcentaje CLIPPED,"%"

            -- Obtiene porcentaje total
            LET v_total_prc = v_total_prc + v_porcentaje
            LET arr_rch_orig[z].total_prc = v_total_prc CLIPPED,"%"
            
            PRINTX arr_rch_orig[z].rch_concatena
            PRINTX arr_rch_orig[z].total
            PRINTX arr_rch_orig[z].total_reg USING "###,###,###"
            PRINTX arr_rch_orig[z].t_deudores
            PRINTX arr_rch_orig[z].total_deudor
            PRINTX arr_rch_orig[z].sdo_deudor
            PRINTX arr_rch_orig[z].total_sdo_deudor
            PRINTX arr_rch_orig[z].porcentaje
            PRINTX arr_rch_orig[z].total_prc
            PRINTX arr_rch_orig[z].t_no_deudor
            PRINTX arr_rch_orig[z].total_nd
            
         END FOR 
         
         
END REPORT 

FUNCTION fn_genera_extractor()

   DEFINE ch               base.channel
   DEFINE v_query_ext      STRING
   DEFINE contador         INTEGER
   DEFINE v_detalle        STRING 
   DEFINE i                INTEGER
   DEFINE v_filler         CHAR(34)
   DEFINE v_f_proceso_aux  CHAR(19)
   DEFINE v_diagnostico    CHAR(4)
   DEFINE v_id_cre_ctr_arh DECIMAL(9,0)
   DEFINE v_estado         CHAR(4)
   DEFINE v_fecha_bd       CHAR(8)
   DEFINE v_retorno        BOOLEAN 

   DEFINE rec_extractor_mic RECORD
      id_cre_tramite     DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      tipo_operativo     CHAR(1),
      estado             SMALLINT,
      nss                CHAR(11),
      num_credito        CHAR(10),
      tipo_credito       CHAR(3),
      status_credito     CHAR(3),
      saldo_deudor       CHAR(14),
      f_otorga           DATE,
      f_proceso          CHAR(8),
      h_proceso          CHAR(8),
      clave              CHAR(4),
      desc_rch           CHAR(50)
   END RECORD 

   DEFINE v_id_cre_acreditado DECIMAL(9,0) 

   LET v_arh_salida = v_ruta_envio CLIPPED,"/Microflujo_SACI_Bit_",TODAY USING "yyyymmdd",".mic" 
   LET v_retorno = 0 -- Bandera extractor no se ha generado

   LET ch = base.Channel.create()

   -- Abre archivo de salida para escritura
   CALL ch.openFile(v_arh_salida,"w")
  
   #- Trámites Exitosos (18)
   #- Originaciones Exitosas (20)
   #- Tramites No Exitosos (240)
   #- Originaciones No Exitosas (19)

   -- Query obtiene Trámites exitosos y no exitosos
   LET v_query_ext = "SELECT '1' tipo_operativo,
                              t.id_cre_tramite,
                              t.id_derechohabiente,
                              t.num_credito,
                              t.estado,
                              f.nss
                         FROM cre_tramite t,
                              cre_his_tramite h,
                              afi_derechohabiente f
                        WHERE t.id_derechohabiente = f.id_derechohabiente
                          AND t.id_cre_tramite     = h.id_cre_tramite
                          AND t.num_credito = h.num_credito
                          AND t.estado IN (18,240)
                          AND t.estado = h.estado
                          AND DATE(h.f_proceso) = '",g_f_procesa,"'",
                        " ORDER BY t.estado;"
   
   PREPARE prp_tramites FROM v_query_ext
   DECLARE crs_tramites CURSOR FOR prp_tramites

   
   LET contador = 1
   INITIALIZE rec_extractor_mic.* TO NULL 
   
   FOREACH crs_tramites INTO rec_extractor_mic.tipo_operativo,
                             rec_extractor_mic.id_cre_tramite,
                             rec_extractor_mic.id_derechohabiente,
                             rec_extractor_mic.num_credito,
                             rec_extractor_mic.estado,
                             rec_extractor_mic.nss

      -- Limpia variables por cada iteración
      LET v_id_cre_acreditado = NULL 
      LET rec_extractor_mic.saldo_deudor = NULL

      -- Búsca el máximo registro de cre_acreditado
      -- Sólo tipo de crédito 10 (COFINAVIT AG) ya que son trámites
      SELECT FIRST 1
                 a.id_cre_acreditado,
                 a.tpo_credito,
                 a.edo_credito,
                 a.f_otorga
            INTO v_id_cre_acreditado,
                 rec_extractor_mic.tipo_credito,
                 rec_extractor_mic.status_credito,
                 rec_extractor_mic.f_otorga
            FROM cre_acreditado a,
                 cat_tipo_credito r
          WHERE a.id_derechohabiente = rec_extractor_mic.id_derechohabiente
            AND a.num_credito = rec_extractor_mic.num_credito
            AND a.tpo_credito = 10
            AND a.tpo_credito = r.tpo_credito
            AND a.tpo_originacion = r.tpo_originacion
            ORDER BY a.id_cre_acreditadO DESC;

      -- En caso de que no encuentre el registro en acreditado se salta a otro registroo
      IF(v_id_cre_acreditado IS NULL) THEN 
         CONTINUE FOREACH 
      END IF 

       -- Obtiene saldo deudor
      SELECT sdo_deudor
        INTO rec_extractor_mic.saldo_deudor
        FROM cre_acreditado 
       WHERE id_cre_acreditado = v_id_cre_acreditado
         AND sdo_deudor > 0;

      IF(rec_extractor_mic.saldo_deudor IS NULL) THEN
         LET rec_extractor_mic.saldo_deudor = "0.00"
      END IF 

      --Obtiene fecha y hora de procesamiento    
      SELECT FIRST 1
                f_proceso,
                diagnostico
          INTO v_f_proceso_aux,
               v_diagnostico
          FROM cre_his_tramite
         WHERE id_cre_tramite = rec_extractor_mic.id_cre_tramite
           AND num_credito    = rec_extractor_mic.num_credito
           AND estado         = rec_extractor_mic.estado
           ORDER BY 1 DESC 

      IF(v_f_proceso_aux IS NOT NULL) THEN
         LET rec_extractor_mic.f_proceso = v_f_proceso_aux[1,4] CLIPPED,v_f_proceso_aux[6,7] CLIPPED,v_f_proceso_aux[9,10]
         LET rec_extractor_mic.h_proceso = v_f_proceso_aux[12,19]
      END IF 

      IF(rec_extractor_mic.estado = 18) THEN
         LET rec_extractor_mic.clave = "0000"
         LET rec_extractor_mic.desc_rch = "ACEPTADO"
      ELSE 
         IF(rec_extractor_mic.estado = 240) THEN
            -- Se obtiene el rechazo de acuerdo al diagnostico
            CASE 
               WHEN v_diagnostico = "3-1"   -- FALLO TÉCNICO PROCESAR"
                  LET rec_extractor_mic.clave = "3-1"
                  LET rec_extractor_mic.desc_rch = "FALLO TÉCNICO PROCESAR"

               WHEN v_diagnostico = "225"   -- NÚMERO DE CRÉDITO NO EXISTE
                  LET rec_extractor_mic.clave = "225"
                  LET rec_extractor_mic.desc_rch = "NÚMERO DE CRÉDITO NO EXISTE"
                  
               WHEN v_diagnostico = "208"   -- SALDO CERO
                  LET rec_extractor_mic.clave = "208"
                  LET rec_extractor_mic.desc_rch = "SALDO CERO"
                  
               WHEN v_diagnostico = "212"   -- CRÉDITO ANTERIOR EXISTENTE
                  LET rec_extractor_mic.clave = "212"
                  LET rec_extractor_mic.desc_rch = "CRÉDITO ANTERIOR EXISTENTE"
                  
               WHEN v_diagnostico = "211"   -- TRABAJADOR NO EXISTE
                  LET rec_extractor_mic.clave = "211"
                  LET rec_extractor_mic.desc_rch = "TRABAJADOR NO EXISTE"
                  
               WHEN v_diagnostico = "229"   -- REGISTRO SIN TRÁMITE DE INSCRIPCIÓN
                  LET rec_extractor_mic.clave = "229"
                  LET rec_extractor_mic.desc_rch = "REGISTRO SIN TRÁMITE DE INSCRIPCIÓN"
                  
               WHEN v_diagnostico MATCHES "2???"
                  --- Se omiten diagnostico "2000" y "2213" ya que son procedentes
                  IF(v_diagnostico <> "2000") AND (v_diagnostico <> "2213") THEN
                     LET rec_extractor_mic.clave = v_diagnostico[2,4]

                     --Busca descripción del rechazo en cat_rch_marca
                     SELECT rch_desc
                       INTO rec_extractor_mic.desc_rch
                       FROM cat_rch_marca
                      WHERE rch_cod = rec_extractor_mic.clave
                  END IF 
                  
               WHEN v_diagnostico MATCHES "3???"
                  -- Se omite el diagnostico "3000" ya que es procedente
                  IF(v_diagnostico <> "3000") THEN
                     LET rec_extractor_mic.clave = v_diagnostico[2,4]

                     -- Búsca descripción del rechazo en cat_rechazo
                     SELECT desc_rechazo
                       INTO rec_extractor_mic.desc_rch
                       FROM cat_rechazo
                      WHERE tpo_rechazo = "RCH"
                        AND cod_rechazo = rec_extractor_mic.clave
                  
                  END IF 
                  
            END CASE

            -- Guarda en la tabla temporal de rechazos
        
            INSERT INTO safre_tmp:tmp_rch_tramite 
                             (id_cre_tramite,
                              id_derechohabiente,
                              num_credito,
                              estado,
                              tipo_credito,
                              sdo_deudor,
                              estado_rch,
                              desc_rch,
                              f_proceso)
                     VALUES (rec_extractor_mic.id_cre_tramite,
                             rec_extractor_mic.id_derechohabiente,
                             rec_extractor_mic.num_credito,
                             rec_extractor_mic.estado,
                             rec_extractor_mic.tipo_credito,
                             rec_extractor_mic.saldo_deudor,
                             rec_extractor_mic.clave,
                             rec_extractor_mic.desc_rch,
                             v_f_proceso_aux); 
         END IF 
      END IF 

      -- Crea cadena a escribir en el archivo
      LET v_detalle = rec_extractor_mic.tipo_operativo,"|",
                      rec_extractor_mic.nss,"|",
                      rec_extractor_mic.num_credito CLIPPED,"|",
                      rec_extractor_mic.tipo_credito CLIPPED,"|",
                      rec_extractor_mic.status_credito CLIPPED,"|",
                      rec_extractor_mic.saldo_deudor CLIPPED,"|",
                      rec_extractor_mic.saldo_deudor CLIPPED,"|",
                      rec_extractor_mic.f_otorga USING "yyyymmdd","|",
                      rec_extractor_mic.f_proceso,"|",
                      rec_extractor_mic.clave CLIPPED,"|",
                      rec_extractor_mic.desc_rch CLIPPED,"|"
                     

      -- Escribe detalle en el archivo
      CALL ch.writeLine(v_detalle)
                      
      LET contador = contador + 1
      
   END FOREACH 

   INITIALIZE rec_extractor_mic.* TO NULL 

   -- Query obtiene Originaciones Exitosas y No Exitosas
   LET v_query_ext = "SELECT '2' tipo_operativo,
                             t.id_cre_tramite,
                             t.id_derechohabiente,
                             t.num_credito,
                             t.estado,
                             f.nss
                        FROM cre_tramite t,
                             cre_his_tramite h,
                             afi_derechohabiente f
                       WHERE t.id_derechohabiente = f.id_derechohabiente
                         AND t.id_cre_tramite     = h.id_cre_tramite
                         AND t.num_credito = h.num_credito
                         AND t.estado IN (20,19)
                         AND t.estado = h.estado
                         AND DATE(h.f_proceso) = '",g_f_procesa,"'",
                       " ORDER BY t.estado;"
   
   PREPARE prp_originacion FROM v_query_ext
   DECLARE crs_originacion CURSOR FOR prp_originacion
   
   FOREACH crs_originacion INTO rec_extractor_mic.tipo_operativo,
                                rec_extractor_mic.id_cre_tramite,
                                rec_extractor_mic.id_derechohabiente,
                                rec_extractor_mic.num_credito,
                                rec_extractor_mic.estado,
                                rec_extractor_mic.nss
                                 
      -- Limpia variables por cada iteración
      LET v_id_cre_acreditado = NULL 
      LET rec_extractor_mic.saldo_deudor = NULL

      -- Búsca el máximo registro de cre_acreditado
      -- Sólo tipo de crédito 10 (COFINAVIT AG) ya que son trámites
      SELECT FIRST 1
                 a.id_cre_acreditado,
                 a.tpo_credito,
                 a.edo_credito,
                 a.f_otorga
            INTO v_id_cre_acreditado,
                 rec_extractor_mic.tipo_credito,
                 rec_extractor_mic.status_credito,
                 rec_extractor_mic.f_otorga
            FROM cre_acreditado a,
                 cat_tipo_credito r
          WHERE a.id_derechohabiente = rec_extractor_mic.id_derechohabiente
            AND a.num_credito = rec_extractor_mic.num_credito
            AND a.tpo_credito = r.tpo_credito
            AND a.tpo_originacion = r.tpo_originacion
            ORDER BY a.id_cre_acreditadO DESC;

       -- En caso de que no encuentre el registro en acreditado se salta a otro registroo
      IF(v_id_cre_acreditado IS NULL) THEN 
         CONTINUE FOREACH 
      END IF 

        -- Obtiene saldo deudor
      SELECT sdo_deudor
        INTO rec_extractor_mic.saldo_deudor
        FROM cre_acreditado 
       WHERE id_cre_acreditado = v_id_cre_acreditado
         AND sdo_deudor > 0;
         
      IF(rec_extractor_mic.saldo_deudor IS NULL) THEN
         LET rec_extractor_mic.saldo_deudor = "0.00"
      END IF 
      
      --Obtiene fecha y hora de procesamiento    
      SELECT FIRST 1 f_proceso,
              diagnostico
         INTO v_f_proceso_aux,
              v_diagnostico
         FROM cre_his_tramite
        WHERE id_cre_tramite = rec_extractor_mic.id_cre_tramite
          AND num_credito    = rec_extractor_mic.num_credito
          AND estado         = rec_extractor_mic.estado
          ORDER BY 1 DESC 

      IF(v_f_proceso_aux IS NOT NULL) THEN
         LET rec_extractor_mic.f_proceso = v_f_proceso_aux[1,4] CLIPPED,v_f_proceso_aux[6,7] CLIPPED,v_f_proceso_aux[9,10]
         LET rec_extractor_mic.h_proceso = v_f_proceso_aux[12,19]
      END IF 

      -- Se obtiene el rechazo de acuerdo al diagnostico
       IF(rec_extractor_mic.estado = 20) THEN
         LET rec_extractor_mic.clave = "0000"
         LET rec_extractor_mic.desc_rch = "ACEPTADO"
      ELSE 
         -- Busca código de rechazo
         IF (rec_extractor_mic.estado = 19) THEN
            -- Se compone fecha para dejarla en formato "mmddyyyy"
            LET v_fecha_bd = rec_extractor_mic.f_proceso[5,6],
                             rec_extractor_mic.f_proceso[7,8],
                             rec_extractor_mic.f_proceso[1,4]
         
            SELECT id_cre_ctr_archivo
              INTO v_id_cre_ctr_arh
              FROM cre_acreditado
             WHERE id_cre_acreditado = v_id_cre_acreditado
               AND f_otorga    >= v_fecha_bd

            -- Busca en cre_rch_acreditado para obtener el estado (rechazo)
            SELECT estado
               INTO v_estado
               FROM cre_rch_acreditado
              WHERE id_cre_ctr_archivo = v_id_cre_ctr_arh
                AND nss = rec_extractor_mic.nss
                AND tpo_credito = rec_extractor_mic.tipo_credito
                AND num_credito = rec_extractor_mic.num_credito

            IF (v_estado IS NULL) OR (v_estado = "") THEN 
               LET rec_extractor_mic.clave = " "
               LET rec_extractor_mic.desc_rch = "REGISTRO RECHAZADO" 
            ELSE 

               -- Se asigna el rechazo para los que son estado = 19
               LET rec_extractor_mic.clave = v_estado
               
               --Busca la descripción del rechazo
               SELECT desc_estado
                 INTO rec_extractor_mic.desc_rch
                 FROM cat_rch_acreditado
                WHERE estado = v_estado
                
            END IF 
            
            -- Inserta en la temporal los rechazos
            INSERT INTO safre_tmp:tmp_rch_originacion(
                                     id_cre_tramite,
                                     id_derechohabiente,
                                     num_credito,
                                     estado,
                                     tipo_credito,
                                     sdo_deudor,
                                     estado_rch,
                                     desc_rch,
                                     f_proceso)
                             VALUES (rec_extractor_mic.id_cre_tramite,
                                     rec_extractor_mic.id_derechohabiente,
                                     rec_extractor_mic.num_credito,
                                     rec_extractor_mic.estado,
                                     rec_extractor_mic.tipo_credito,
                                     rec_extractor_mic.saldo_deudor,
                                     rec_extractor_mic.clave, -- Código de rechazo
                                     rec_extractor_mic.desc_rch, --Descripción del rechazo
                                     v_f_proceso_aux);
         
         END IF
         
      END IF 
      
      -- Crea cadena a escribir en el archivo
      LET v_detalle = rec_extractor_mic.tipo_operativo,"|",
                      rec_extractor_mic.nss,"|",
                      rec_extractor_mic.num_credito CLIPPED,"|",
                      rec_extractor_mic.tipo_credito CLIPPED,"|",
                      rec_extractor_mic.status_credito CLIPPED,"|",
                      rec_extractor_mic.saldo_deudor CLIPPED,"|",
                      rec_extractor_mic.saldo_deudor CLIPPED,"|",
                      rec_extractor_mic.f_otorga USING "yyyymmdd","|",
                      rec_extractor_mic.f_proceso,"|",
                      rec_extractor_mic.clave CLIPPED,"|",
                      rec_extractor_mic.desc_rch CLIPPED,"|"

      -- Escribe detalle en el archivo
      CALL ch.writeLine(v_detalle)
                      
      LET contador = contador + 1
      
   END FOREACH 
   
   -- En caso de que no haya registros
   IF(contador = 1) THEN
      LET  v_detalle = "NO SE ENCONTRARON REGISTROS PARA EL EXTRACTOR DE MICROFLUJO"
      CALL ch.writeLine(v_detalle)
   END IF 

   -- se crea filler para el archivo
   LET v_filler = " "

   FOR i=1 TO 33
      LET v_filler = v_filler," "
   END FOR 

   -- Escribe filler
   CALL ch.writeLine([v_filler])

   -- Cierra archivo
   CALL ch.close()

   LET v_retorno = 1 -- Extractor generado

   RETURN v_retorno

END FUNCTION 

FUNCTION crea_temporal()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_datos_mic;
      DROP TABLE tmp_rch_tramite;
      DROP TABLE tmp_rch_originacion;
      

   WHENEVER ERROR STOP 
     CREATE TABLE tmp_datos_mic(id_cre_tramite     DECIMAL(9,0),
                                id_cre_acreditado  DECIMAL(9,0),
                                id_derechohabiente DECIMAL(9,0),
                                num_credito        DECIMAL(10,0),
                                estado             SMALLINT,
                                tpo_credito        SMALLINT,
                                desc_credito       CHAR(30),
                                sdo_deudor         DECIMAL(12,2));
   
     CREATE TABLE tmp_rch_tramite(id_cre_tramite      DECIMAL(9,0),
                                  id_derechohabiente DECIMAL(9,0),
                                  num_credito        DECIMAL(10,0),
                                  estado             SMALLINT,
                                  tipo_credito       SMALLINT,
                                  sdo_deudor         DECIMAL(12,2),
                                  estado_rch         CHAR(4),
                                  desc_rch           CHAR(50),
                                  f_proceso          DATETIME YEAR TO SECOND);

      
      CREATE TABLE tmp_rch_originacion(id_cre_tramite    DECIMAL(9,0),
                                       id_derechohabiente DECIMAL(9,0),
                                       num_credito        DECIMAL(10,0),
                                       estado             SMALLINT,
                                       tipo_credito       SMALLINT,
                                       sdo_deudor         DECIMAL(12,2),
                                       estado_rch         CHAR(4),
                                       desc_rch           CHAR(50),
                                       f_proceso          DATETIME YEAR TO SECOND);

DATABASE safre_viv 

END FUNCTION 
