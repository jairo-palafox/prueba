
###############################################################
#Modulo        => AGR                                         #
#Programa      => AGRI02                                      #
#Objetivo      => Programa que genera informe del proceso de  #
#                 RECEPCIÓN RECURRENTE ORIGINACIÓN AG.        #
#Autor         => Emilio Abarca , EFP                         #
#Fecha inicio  => 16/JULIO/2017                               #
################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE g_usuario            CHAR(20)
   DEFINE g_pid                DECIMAL(9,0)
   DEFINE g_proceso_cod        INTEGER 
   DEFINE g_opera_cod          INTEGER 
   DEFINE v_ruta_listados      CHAR(40)
   DEFINE v_ruta_envio         CHAR(40)
   DEFINE g_arh_tot_reg        INTEGER 
   DEFINE g_tot_aceptados      INTEGER 
   DEFINE g_tot_rechazados     INTEGER 
   DEFINE g_tot_sin_origen     INTEGER 
   DEFINE g_id_cre_ctr_archivo DECIMAL(10,0)
   DEFINE g_folio              DECIMAL(10,0)
   DEFINE g_arh_proceso        VARCHAR(100)

   DEFINE r_b_valida           SMALLINT 

   --Variables para el reporte
   DEFINE v_f_ejecuta         CHAR(10)
   DEFINE v_total_cifras      INTEGER
   DEFINE v_prc_archivo       CHAR(4)
   DEFINE v_prc_procesamiento CHAR(4)
   DEFINE v_prc_exc_proc      CHAR(4)
   DEFINE v_prc_originacion   CHAR(4)
   DEFINE v_prc_total         CHAR(4)
   DEFINE v_concatena_prc     CHAR(6)
   DEFINE v_tot_proc          INTEGER 
   DEFINE v_orig_acep         INTEGER 
   DEFINE v_orig_canc         INTEGER
   DEFINE v_total_orig_d      INTEGER 
   DEFINE v_prc_orig_d        CHAR(4)  
   DEFINE v_prc_conc_d        CHAR(6)   
   DEFINE v_movimiento        CHAR(1)
   DEFINE v_ef_orig           CHAR(4)
   DEFINE v_prc_efectividad   CHAR(6)
   DEFINE v_s_titulo_correo   STRING 
   DEFINE v_s_archivo_correo  STRING  
   DEFINE v_s_mens_correo     STRING 
   DEFINE v_ind_extractor     SMALLINT 
   DEFINE v_ruta_reporte      STRING
   DEFINE v_archivo_salida    STRING
   
END GLOBALS

MAIN 

   -- Recibe parámetros que envía el programa AGRE01
   LET g_usuario            = ARG_VAL(1)   -- USUARIO
   LET g_pid                = ARG_VAL(2)   -- PID
   LET g_proceso_cod        = ARG_VAL(3)   -- Código del proceso
   LET g_opera_cod          = ARG_VAL(4)   -- Operación
   LET g_arh_tot_reg        = ARG_VAL(5)   -- Total de registros en el archivo
   LET g_tot_aceptados      = ARG_VAL(6)   -- Total de registros aceptados
   LET g_tot_rechazados     = ARG_VAL(7)   -- Total de registros rechazados
   LET g_tot_sin_origen     = ARG_VAL(8)  -- Total de registros excluidos del procesamiento
   LET g_id_cre_ctr_archivo = ARG_VAL(9)   --identificador del archivo cargado
   LET g_folio              = ARG_VAL(10)   -- Folio
   LET g_arh_proceso        = ARG_VAL(11)   -- Nombre del archivo

   LET r_b_valida = 0

   CALL STARTLOG(g_usuario CLIPPED ||".AGRI02.log")

   -- Se obtiene la ruta donde se alojará elinforme .PDF
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'agr'
    
   --Se obtiene la ruta donde se alojará el extractor
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

    
   CALL fn_display_proceso(0,"INICIA GENERACIÓN REPORTE RECURRENTE ORIGINACIÓN AGR")
   DISPLAY " "
   DISPLAY "==> PID : ",g_pid
   DISPLAY "==> NOMBRE DEL ARCHIVO :  ",g_arh_proceso
   DISPLAY "==> IDENTIFICADOR ARCHIVO : ",g_id_cre_ctr_archivo
   DISPLAY "==> FOLIO : ",g_folio

   -- Valida operación
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida
   
   IF(r_b_valida <> 0) THEN 
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operación"
   ELSE 
      --Inicializa operación
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,"","AGRI02","",g_usuario) RETURNING r_b_valida

       -- Llama la funcion que calcula información
      CALL calcula_datos() RETURNING r_b_valida  --> 1 Finalizado correctamente

      --Genera Extractor
      CALL fn_genera_extractor_originacion() RETURNING v_ind_extractor --> 1 Finalizado correctamente

      -- Ejecuta conciliación de marcas Item 5 (SACI2017-1)
      CALL conciliacion_marcas()
      
      IF(r_b_valida = 1) AND (v_ind_extractor = 1) THEN 
         -- Finaliza Operación
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

         DISPLAY "==> El reporte .PDF Originación AGR se ha generado en /safreviv_lst/agr"
         DISPLAY ""
         DISPLAY "==> El extractor de rechazos se ha generado en la ruta /safreviv_int/agr/envio"
         DISPLAY "    con nombre: ",v_archivo_salida 
         DISPLAY " "

          DISPLAY "ENVIA CORREO DEL REPORTE"
         --Asigna titulo del correo
         LET v_s_titulo_correo = "Proceso: GENERA REPORTE ORIGINACIÓN AG"

         -- Se asigna el archivo a adjuntar
         LET v_s_archivo_correo = v_ruta_reporte

         -- Se asigna cuerpo a correo
         LET v_s_mens_correo = "ID Proceso   : ",g_pid,"\n",
                                "Proceso      : RECEPCIÓN RECURRENTE ORIGINACIÓN AG\n",
                                "Operacion    : GENERA REPORTE ORIGINACIÓN AGR\n",
                                "Fecha Inicio : ",TODAY,"\n",
                                "Fecha Fin    : ",TODAY
                                
         -- se invoca la función que envía por correo el elemento generado
         CALL fn_correo_proceso(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod,
                                 v_s_archivo_correo,
                                 v_s_titulo_correo,
                                 v_s_mens_correo)

         DISPLAY "FIN DEL ENVÍO DE CORREO"
         
         CALL fn_display_proceso(1,"FIN REPORTE RECURRENTE ORIGINACIÓN AGR")
         DISPLAY " "
      ELSE 
         -- Muestra error al usuario
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida  
      END IF 
      
   END IF 
   
END MAIN

FUNCTION calcula_datos()

   DEFINE reporte         om.SaxDocumentHandler -- Objeto para reporte PDF
   DEFINE v_reporte       STRING 
   DEFINE v_nombre_pdf    STRING
   DEFINE v_estatus       SMALLINT 
   DEFINE v_query         STRING  

   DEFINE r_originaciones RECORD
      estado SMALLINT,
      total  INTEGER  
   END RECORD 

   -- Inicializa variables
   LET v_estatus           = 0 
   LET v_total_cifras      = 0
   LET v_prc_archivo       = 0
   LET v_prc_procesamiento = 0
   LET v_prc_exc_proc      = 0
   LET v_prc_total         = 0   
   LET v_tot_proc          = 0
   LET v_orig_acep         = 0
   LET v_orig_canc         = 0
   LET v_total_orig_d      = 0
   LET v_movimiento        = 0
   LET v_ef_orig           = 0

   LET v_reporte    = "AGRI021.4rp"
   LET v_nombre_pdf = "Recurrente_originacion_agr.pdf"
   LET v_f_ejecuta  = TODAY USING "dd/mm/yyyy"

   --Registros para procesamiento (Aceptados y rechazados)
   LET v_tot_proc = g_tot_aceptados + g_tot_rechazados
   
   -- Suma total de registros (Para procesamiento y excluidos)
   LET v_total_cifras = v_tot_proc + g_tot_sin_origen
 
   # Obtiene porcentaje por cada entidad
   
   -- Procentaje archivo
   LET v_prc_archivo = (g_arh_tot_reg / g_arh_tot_reg) * 100

   -- Porcentaje procesamiento
   LET v_prc_procesamiento = (v_tot_proc / g_arh_tot_reg) * 100

   -- Porcentaje excluidos del procesamiento
   LET v_prc_exc_proc = (g_tot_sin_origen / g_arh_tot_reg) * 100

   -- Porcentaje para ser originados
   LET v_prc_originacion = (g_tot_aceptados / g_arh_tot_reg) * 100

   -- Suma total porcentajes
   LET v_prc_total = v_prc_procesamiento + v_prc_exc_proc
   LET v_concatena_prc = v_prc_total CLIPPED,"%" 

   -- Obtiene Originaciones aceptadas y Rechazadas
   LET v_query = "SELECT estado,COUNT(*)
                     FROM cre_acreditado
                    WHERE id_cre_ctr_archivo = ", g_id_cre_ctr_archivo,
                    "GROUP BY 1;"

   INITIALIZE r_originaciones.* TO NULL

   PREPARE prp_orig FROM v_query 
   DECLARE crs_orig CURSOR FOR prp_orig

   FOREACH crs_orig INTO r_originaciones.estado,
                          r_originaciones.total

      --Recupera originaciones que fueron aceptadas
      IF(r_originaciones.estado = 20) THEN
         LET v_orig_acep = v_orig_acep + r_originaciones.total
      END IF 

      --Recupera originaciones que fueron canceladas
      IF(r_originaciones.estado <> 20) THEN
         LET v_orig_canc = v_orig_canc + r_originaciones.total
      END IF 

   END FOREACH 

   -- Total originaciones
   LET v_total_orig_d = v_orig_acep + v_orig_canc

   -- Porcentaje total
   LET v_prc_orig_d = (v_total_orig_d / v_total_orig_d) * 100
   LET v_prc_conc_d = v_prc_orig_d CLIPPED,"%"

   --Calcula efectividad
   LET v_ef_orig = (v_orig_acep / v_total_orig_d) * 100
   LET v_prc_efectividad = v_ef_orig CLIPPED,"%"

   # Carga del Reporte PDF
   LET v_ruta_reporte = v_ruta_listados CLIPPED,"/",
                        g_usuario CLIPPED,"-",
                        "AGRI02","-",
                        g_pid USING "&&&&&","-",
                        g_proceso_cod USING "&&&&&","-",
                        g_opera_cod USING "&&&&&",".pdf"

    
   IF (fgl_report_loadCurrentSettings(v_reporte)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()

      IF (reporte IS NOT NULL) THEN
         
         START REPORT resultados TO XML HANDLER reporte

            OUTPUT TO REPORT resultados()

         FINISH REPORT resultados  

      END IF 

   END IF 

   LET v_estatus = 1 -- Ha finalizado el cálculo

   RETURN v_estatus
    
END FUNCTION  

REPORT resultados ()
 
   FORMAT 

      FIRST PAGE HEADER 
--***** ENCABEZADO *****
        PRINTX g_usuario           --USUARIO 
        PRINTX v_f_ejecuta         --FECHA
        PRINTX g_folio             --FOLIO
        PRINTX g_arh_proceso       --NOMBRE DEL ARCHIVO

--***** RESUMEN ****        
        PRINTX g_arh_tot_reg       --TOTAL REGISTROS EN EL ARCHIVO
        PRINTX v_tot_proc          --TOTAL REGISTROS PARA PROCESAMIENTO
        PRINTX g_tot_sin_origen    --TOTAL REGISTROS EXCLUIDOS PROCESAMIENTO
        PRINTX g_tot_aceptados     --REGISTROS PARA SER ORIGINADOS      
        PRINTX v_total_cifras      --TOTAL CIFRAS
        PRINTX v_prc_archivo       --PRC ARCHIVO
        PRINTX v_prc_procesamiento --PRC PROCESAMIENTO
        PRINTX v_prc_exc_proc      --PRC EXCLUIDOS
        PRINTX v_prc_originacion   --PRC PARA SER ORIGINADOS
        PRINTX v_concatena_prc     --% SUMA

--***** DETALLE *****
       PRINTX v_total_orig_d       --TOTAL ORIGINACIONES
       PRINTX v_prc_orig_d         --PRC ORIGINACION
       PRINTX v_movimiento         --MOVIMIENTOS NO PROCESADO (APLICA SIEMPRE 0)
       PRINTX v_prc_conc_d         --PRC TOTAL
       PRINTX v_orig_acep          --ORIGINACIONES ACEPTADAS
       PRINTX v_orig_canc          --ORIGINACIONES CANCELADAS
       PRINTX v_ef_orig            --EFECTIVIAD ORIGINACIONES
       PRINTX v_prc_efectividad    --PORCENTAJE TOTAL EFETIVIDAD
         
END REPORT

#Extractor genera información de los registros cargados en el archivo de entrada
FUNCTION fn_genera_extractor_originacion()

   DEFINE ch            base.channel
   DEFINE v_arh_salida  STRING
   DEFINE v_indicador   SMALLINT 
   
   DEFINE r_rechazos RECORD 
      tipo_registro  CHAR(2),
      nss            CHAR(11),
      num_credito    CHAR(10),
      tpo_credito    CHAR(3),
      status_credito CHAR(3),
      sdo_deudor     CHAR(14),
      f_otorgamiento DATE ,
      f_proceso      DATE,
      clave          CHAR(4),
      desc_rch       CHAR(41)
   END RECORD 

   DEFINE v_query_ext    STRING
   DEFINE v_linea        STRING 
   DEFINE v_filler       CHAR(30)
   DEFINE i              INTEGER
   DEFINE v_curp         CHAR(18)
   DEFINE v_dt           DATETIME HOUR TO SECOND
   DEFINE v_aux_hora     CHAR(8)
   DEFINE v_hora         CHAR(6)

   LET v_dt = CURRENT
   LET v_aux_hora = v_dt
   LET v_hora     = v_aux_hora[1,2],v_aux_hora[4,5],v_aux_hora[7,8]

   LET v_archivo_salida = "rch_recurrente_originacion_agr_",TODAY USING "yyyymmdd","_",v_hora,".txt"
   LET v_arh_salida = v_ruta_envio CLIPPED,"/",v_archivo_salida
   LET v_indicador = 0 -- No se ha generado el extractor
   
   -- Se crea el objeto
   LET ch = base.Channel.create()
   
   -- Abre archivo de salida para escritura
   CALL ch.openFile(v_arh_salida,"w")
   
   --Consulta en tmp_nss_desmarcados 
   # Indicadores 1 --> Desmarca aceptada
               # 2 --> Desmarca rechazada
               # 3 --> Desmarca rechazada

   LET v_query_ext = "SELECT r.tpo_registro,
                              r.nss,
                              r.num_credito,
                              r.tpo_credito,
                              r.edo_credito,
                              r.sdo_deudor,
                              r.f_otorga,
                              a.f_proceso,
                              r.estado edo_rechazo,
                              c.desc_estado
                         FROM cre_rch_acreditado r,
                              cat_rch_acreditado c,
                              cre_ctr_archivo a
                        WHERE r.estado = c.estado
                          AND r.id_cre_ctr_archivo = ",g_id_cre_ctr_archivo,
                        " AND a.id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   INITIALIZE r_rechazos.* TO NULL 
   LET v_curp = NULL 

   PREPARE prp_extractor FROM v_query_ext
   DECLARE crs_extractor CURSOR FOR prp_extractor

   FOREACH crs_extractor INTO r_rechazos.tipo_registro,
                               r_rechazos.nss,
                               r_rechazos.num_credito,
                               r_rechazos.tpo_credito,
                               r_rechazos.status_credito,
                               r_rechazos.sdo_deudor,
                               r_rechazos.f_otorgamiento,
                               r_rechazos.f_proceso,
                               r_rechazos.clave,
                               r_rechazos.desc_rch

      -- Busca  curp
      SELECT curp 
         INTO v_curp
         FROM afi_derechohabiente
        WHERE nss = r_rechazos.nss
        
      LET v_linea = r_rechazos.tipo_registro CLIPPED,"|",
                    r_rechazos.nss CLIPPED,"|",
                    r_rechazos.num_credito CLIPPED,"|",
                    r_rechazos.tpo_credito CLIPPED,"|",
                    r_rechazos.status_credito CLIPPED,"|",
                    r_rechazos.sdo_deudor CLIPPED,"|",
                    r_rechazos.f_otorgamiento USING "YYYYMMDD" CLIPPED,"|",
                    r_rechazos.f_proceso USING "YYYYMMDD" CLIPPED,"|",
                    r_rechazos.clave CLIPPED,"|",
                    r_rechazos.desc_rch CLIPPED, "|",
                    v_curp CLIPPED,"|"
                    
      -- Escribe detalle en el archivo
      CALL ch.writeLine(v_linea)
      
   END FOREACH 

   -- Crea filler 
   LET v_filler = " "

   FOR  i = 1 TO 29
      LET v_filler = v_filler," "
   END FOR 

   -- Escribe filler
   CALL ch.writeLine([v_filler])


   CALL ch.close()

   LET v_indicador = 1 -- Etractor finalizado

   RETURN v_indicador
   
END FUNCTION

FUNCTION conciliacion_marcas()

   DEFINE v_cadena         STRING
   DEFINE r_conciliacion   RECORD
      id_referencia        DECIMAL(9,0),
      id_derechohabiente   DECIMAL(9,0),
      marca_orig           SMALLINT,
      folio_marca          DECIMAL(9,0),
      tpo_originar         SMALLINT,
      marca_fin            SMALLINT
   END RECORD
   DEFINE v_t_43bis        INTEGER
   DEFINE v_t_ag           INTEGER
   DEFINE r_inf_acreditado RECORD LIKE cre_acreditado.*
   DEFINE r_inf_ug         RECORD LIKE cre_uso_garantia.*
   DEFINE v_id_arh_43      DECIMAL(9,0)
   DEFINE v_id_arh_ag      DECIMAL(9,0)
   DEFINE v_folio_arh_ag   DECIMAL(9,0)
   DEFINE v_folio_arh_43   DECIMAL(9,0)
   DEFINE r_aux_archivo    RECORD
      id_archivo     DECIMAL(9,0),
      folio_archivo  DECIMAL(9,0),
      tpo_transf     CHAR(2),
      tpo_uso        SMALLINT
   END RECORD
   DEFINE v_id_referencia  DECIMAL(9,0)
   DEFINE v_fecha          CHAR(8)
   DEFINE v_periodo_pago   CHAR(6)
   DEFINE v_sts_marcaje    SMALLINT
   DEFINE v_folio_archivo  DECIMAL(9,0)
   DEFINE v_marca_activa   SMALLINT
   DEFINE v_num_credito    DECIMAL(9,0)
   DEFINE v_f_otorga       DATE
   DEFINE v_tpo_credito    SMALLINT
   DEFINE v_id_ref_marca   DECIMAL(9,0)
   DEFINE v_tab_movimiento CHAR(30)
   DEFINE v_f_movimiento   DATE
   DEFINE v_precio_fondo   DECIMAL(19,14)
   DEFINE v_id_dh_ws       DECIMAL(9,0)
   DEFINE r_recu_acciones  RECORD
      subcuenta      SMALLINT,
      monto_aivs     DECIMAL(16,6) 
   END RECORD
   DEFINE v_aux_aivs92     DECIMAL(16,6)
   DEFINE v_aux_aivs97     DECIMAL(16,6)

   LET v_fecha = TODAY USING "yyyymmdd"
   LET v_periodo_pago = v_fecha[1,6]

   LET v_f_movimiento = MDY(MONTH(TODAY),1,YEAR(TODAY))
   LET v_f_movimiento = v_f_movimiento + 1 UNITS MONTH;

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_movimiento
      AND fondo = 11;

   DISPLAY ""
   DISPLAY "Fecha de evaluación: ",v_f_movimiento
   DISPLAY "Precio fondo: ",v_precio_fondo
   DISPLAY ""

   LET v_cadena = "EXECUTE PROCEDURE sp_reversa_desmarca(?,?,?,?)"
   PREPARE prp_reversa_desm FROM v_cadena

   LET v_cadena = "EXECUTE FUNCTION fn_tab_movimiento(0,?,?)"
   PREPARE prp_tab_movimiento FROM v_cadena

   INITIALIZE r_conciliacion.* TO NULL
   INITIALIZE r_aux_archivo.* TO NULL

   LET v_id_arh_43 = 0
   LET v_id_arh_ag = 0

   -- Se reversan las desmarcas (221,223,225) en caso de que la nueva originación
   -- no haya sido exitosa
   DECLARE crs_reversa_desm CURSOR FOR
   SELECT id_referencia,
          id_derechohabiente,
          marca_ini,
          folio_marca
     FROM cre_marca_conciliacion
    WHERE id_proceso = 301
      AND estado     = 150   -- Marca infonavit rechazada en proceso operativo
      AND f_proceso = TODAY;

   FOREACH crs_reversa_desm INTO r_conciliacion.id_referencia,
                                 r_conciliacion.id_derechohabiente,
                                 r_conciliacion.marca_orig,
                                 r_conciliacion.folio_marca

      -- Ejecuta SP que reversa la desmarca.
      EXECUTE prp_reversa_desm USING r_conciliacion.id_derechohabiente,
                                     r_conciliacion.marca_orig,
                                     r_conciliacion.id_referencia,
                                     r_conciliacion.folio_marca

      DISPLAY "Reversa desmarca: --> ","Id_referencia: ",r_conciliacion.id_referencia," Marca_origen: ",r_conciliacion.marca_orig
      
   END FOREACH

   -- Genera identificador archivo 43BIS
   SELECT COUNT(*)
     INTO v_t_43bis
     FROM cre_marca_conciliacion
    WHERE id_proceso = 301
      AND estado IN (18,20)
      AND marca_fin = 223
      AND f_proceso = TODAY;

   -- Genera identificador archivo AG
   SELECT COUNT(*)
     INTO v_t_ag
     FROM cre_marca_conciliacion
    WHERE id_proceso = 301
      AND estado IN (18,20)
      AND marca_fin = 225
      AND f_proceso = TODAY;

   IF(v_t_43bis > 0) THEN
      LET v_cadena = "EXECUTE FUNCTION fn_genera_id_archivo(1202)"
      PREPARE prp_archivo_43 FROM v_cadena
      EXECUTE prp_archivo_43 INTO v_id_arh_43,v_folio_arh_43
   END IF

   IF(v_t_ag > 0) THEN
      LET v_cadena = "EXECUTE FUNCTION fn_genera_id_archivo(302)"
      PREPARE prp_archivo_ag FROM v_cadena
      EXECUTE prp_archivo_ag INTO v_id_arh_ag,v_folio_arh_ag
   END IF

   DISPLAY ""
   DISPLAY "Id_archivo conciliación"
   DISPLAY "43BIS: ",v_id_arh_43
   DISPLAY "AG: ",v_id_arh_ag
   DISPLAY ""
   INITIALIZE r_conciliacion.* TO NULL

   -- Genera la petición de saldo de las originaciones que fueron exitosas
   -- sólo se recuperan los registros máximos en caso de existir más de 1 registro por derechohabiente
   DECLARE crs_peticion_saldo CURSOR FOR
   SELECT id_derechohabiente,
          MAX(id_referencia)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 301
      AND estado IN (18,20)
      AND marca_fin IS NOT NULL
      AND f_proceso = TODAY
      GROUP BY 1;

   FOREACH crs_peticion_saldo INTO r_conciliacion.id_derechohabiente,
                                   r_conciliacion.id_referencia

      -- Recupera inf. unica
      SELECT FIRST 1 
             marca_ini,
             tpo_originar,
             marca_fin
        INTO r_conciliacion.marca_orig,
             r_conciliacion.tpo_originar,
             r_conciliacion.marca_fin
        FROM cre_marca_conciliacion
       WHERE id_referencia =  r_conciliacion.id_referencia
         AND f_proceso = TODAY
       ORDER BY f_proceso DESC;

      INITIALIZE r_inf_acreditado.* TO NULL
      INITIALIZE r_inf_ug.* TO NULL
      INITIALIZE r_aux_archivo.* TO NULL

      LET v_id_ref_marca   = NULL 
      LET v_folio_archivo  = 0
      LET v_marca_activa   = NULL
      LET v_id_dh_ws       = NULL 
      LET r_recu_acciones.subcuenta  = NULL
      LET r_recu_acciones.monto_aivs = 0
      LET v_aux_aivs92     = 0
      LET v_aux_aivs97     = 0

      CASE
         WHEN (r_conciliacion.marca_orig = 221)

            IF (r_conciliacion.marca_fin = 223) THEN
               LET r_aux_archivo.id_archivo    = v_id_arh_43
               LET r_aux_archivo.folio_archivo = v_folio_arh_43
               LET r_aux_archivo.tpo_transf    = '18'
               LET r_aux_archivo.tpo_uso       = 3
            ELSE
               IF (r_conciliacion.marca_fin = 225) THEN
                  LET r_aux_archivo.id_archivo    = v_id_arh_ag
                  LET r_aux_archivo.folio_archivo = v_folio_arh_ag
                  LET r_aux_archivo.tpo_transf    = '43'
                  LET r_aux_archivo.tpo_uso       = 2
               END IF
            END IF

             -- Obtiene información de la solicitud 221 anterior
            SELECT cre.*,
                   ctr.folio_archivo
              INTO r_inf_acreditado.*,
                   v_folio_archivo
              FROM cre_acreditado cre,
                   cre_ctr_archivo ctr
             WHERE cre.id_cre_acreditado  = r_conciliacion.id_referencia
               AND cre.id_cre_ctr_archivo = ctr.id_cre_ctr_archivo;

            -- Verifica que no se haya realizado la desmarca vía WS
             SELECT MAX(id_derechohabiente)
               INTO v_id_dh_ws
               FROM cta_marca_ws
              WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                AND id_origen   = r_conciliacion.id_referencia
                AND modulo_cod  = "03"
                AND tpo_credito = r_inf_acreditado.tpo_credito
                AND marca       = 221;

            IF(v_id_dh_ws IS NULL) THEN 
               -- Al desmarcarse en SACI, se realiza la solicitud de desmarca 221 a Procesar
               INSERT INTO cta_marca_ws(
                              id_derechohabiente,
                              id_origen         ,
                              modulo_cod        ,
                              tpo_credito       ,
                              marca             ,
                              f_solicita        ,
                              intento           ,
                              cod_result_op     ,
                              diagnostico       ,
                              situacion         ,
                              num_credito       ,
                              f_infonavit       ,
                              marca_procesar    ,
                              folio_archivo     ,
                              usuario)
                       VALUES(r_conciliacion.id_derechohabiente,
                              r_conciliacion.id_referencia     ,
                              '03'                             ,
                              r_inf_acreditado.tpo_credito     ,
                              221                              ,
                              TODAY                            ,
                              1                                ,
                              NULL                             ,
                              NULL                             ,
                              0                                ,
                              r_inf_acreditado.num_credito     ,
                              r_inf_acreditado.f_otorga        ,
                              '01'                             ,
                              v_folio_archivo                  ,
                              g_usuario)
            END IF -- Solicita desmarca 221 WS

            -- Valida si tiene activa la marca procesar 231 que corresponde a la marca de los créditos
            -- con tipo de originacion 1.
            SELECT MAX(marca)
              INTO v_marca_activa
              FROM sfr_marca_activa
             WHERE n_referencia = r_conciliacion.id_referencia
               AND marca = 231;

            IF(v_marca_activa IS NOT NULL) THEN
               -- Verifica que no se haya realizado la desmarca vía WS
               SELECT MAX(id_derechohabiente)
                 INTO v_id_dh_ws
                 FROM cta_marca_ws
                WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                  AND id_origen   = r_conciliacion.id_referencia
                  AND modulo_cod  = "03"
                  AND tpo_credito = r_inf_acreditado.tpo_credito
                  AND marca       = 231;

               IF(v_id_dh_ws IS NULL) THEN
                  --Solicita la desmarca a Procesar de la 231
                  INSERT INTO cta_marca_ws(
                              id_derechohabiente,
                              id_origen         ,
                              modulo_cod        ,
                              tpo_credito       ,
                              marca             ,
                              f_solicita        ,
                              intento           ,
                              cod_result_op     ,
                              diagnostico       ,
                              situacion         ,
                              num_credito       ,
                              f_infonavit       ,
                              marca_procesar    ,
                              folio_archivo     ,
                              usuario)
                       VALUES(r_conciliacion.id_derechohabiente,
                              r_conciliacion.id_referencia     ,
                              '03'                             ,
                              r_inf_acreditado.tpo_credito     ,
                              231                              ,
                              TODAY                            ,
                              1                                ,
                              NULL                             ,
                              NULL                             ,
                              0                                ,
                              r_inf_acreditado.num_credito     ,
                              r_inf_acreditado.f_otorga        ,
                              '01'                             ,
                              v_folio_archivo                  ,
                              g_usuario)
               END IF -- Verifica desmarca 231 vía WS
            END IF

            -- Obtiene monto acciones por el folio_liquida.
            IF(r_inf_acreditado.folio_liquida > 0) THEN
            
               -- Obtiene tabla de movimiento donde se hizo el cargo
               EXECUTE prp_tab_movimiento USING r_inf_acreditado.folio_liquida,
                                                " "
                                           INTO v_tab_movimiento

               LET v_cadena = "SELECT subcuenta,SUM(monto_acciones) 
                                FROM ",v_tab_movimiento,"
                               WHERE folio_liquida      = ",r_inf_acreditado.folio_liquida,
                              " AND id_derechohabiente = ",r_conciliacion.id_derechohabiente,
                              " GROUP BY 1;"

               PREPARE prp_monto_221 FROM v_cadena
               DECLARE crs_monto_221 CURSOR FOR prp_monto_221

               -- Inicializa aivs 
               LET v_aux_aivs92       = 0
               LET v_aux_aivs97       = 0

               FOREACH crs_monto_221 INTO r_recu_acciones.subcuenta,
                                          r_recu_acciones.monto_aivs

                  IF(r_recu_acciones.subcuenta = 4) THEN
                     LET v_aux_aivs97 = v_aux_aivs97 +  r_recu_acciones.monto_aivs
                  ELSE 
                     IF(r_recu_acciones.subcuenta = 8) THEN
                        LET v_aux_aivs92 = v_aux_aivs92 + r_recu_acciones.monto_aivs
                     END IF 
                  END IF

               END FOREACH

               IF(v_aux_aivs92 IS NULL) THEN
                  LET v_aux_aivs92 = 0
               END IF 

               IF(v_aux_aivs97 IS NULL) THEN
                  LET v_aux_aivs97 = 0 
               END IF
               
               -- Actualiza tabla de conciliación de marcas
               UPDATE cre_marca_conciliacion
                  SET aivs92 = v_aux_aivs92,
                      aivs97 = v_aux_aivs97
                WHERE id_referencia      = r_conciliacion.id_referencia
                  AND id_derechohabiente = r_conciliacion.id_derechohabiente
                  AND f_proceso = TODAY;

               -- Obtiene monto pesos
               LET r_inf_acreditado.sdo_deudor = ((v_aux_aivs92 + v_aux_aivs97) * v_precio_fondo)

            END IF 

            -- Solicitud nueva
            SELECT seq_cre_uso.NEXTVAL
              INTO v_id_referencia
              FROM systables
             WHERE tabid = 1;

            -- Realiza petición de saldo de la nueva originación
            INSERT INTO cre_uso_garantia(id_cre_uso_garantia,
                                         id_cre_ctr_archivo ,
                                         folio_liquida      ,
                                         id_derechohabiente ,
                                         tpo_transferencia  ,
                                         tpo_uso            ,
                                         num_credito        ,
                                         f_presentacion     ,
                                         f_movimiento       ,
                                         periodo_pago       ,
                                         importe_v97        ,
                                         nss_afore          ,
                                         rfc_afore          ,
                                         paterno_afore      ,
                                         materno_afore      ,
                                         nombre_afore       ,
                                         nom_imss           ,
                                         edo_procesar       ,
                                         diagnostico        ,
                                         estado             ,
                                         f_proceso)
                                 VALUES (v_id_referencia                  ,
                                         r_aux_archivo.id_archivo         ,
                                         0                                ,
                                         r_conciliacion.id_derechohabiente,
                                         r_aux_archivo.tpo_transf         ,
                                         r_aux_archivo.tpo_uso            ,
                                         0                                ,
                                         TODAY                            ,
                                         TODAY                            ,
                                         v_periodo_pago                   , --calcular periodo_pago
                                         r_inf_acreditado.sdo_deudor      ,
                                         '00000000000'                    ,
                                         ''                               ,
                                         ''                               ,
                                         ''                               ,
                                         ''                               ,
                                         ''                               ,
                                         70                               ,
                                         ''                               ,
                                         142                              ,
                                         TODAY);


            -- Marca cuenta como petición de saldo
            LET v_cadena = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,0,0,'','',?,?)"
            PREPARE prp1_marca_cuenta FROM v_cadena
            EXECUTE prp1_marca_cuenta USING  r_conciliacion.id_derechohabiente,
                                             r_conciliacion.marca_fin,
                                             v_id_referencia,
                                             g_folio,
                                             g_usuario,
                                             g_proceso_cod
                                       INTO  v_sts_marcaje

            -- Actualiza la solicitud anterior 221 si el crédito está vigente
            UPDATE cre_acreditado
               SET estado = 285 --> Crédito liquidado por nueva originación
             WHERE id_cre_acreditado  = r_conciliacion.id_referencia
               AND id_derechohabiente = r_conciliacion.id_derechohabiente
               AND estado IN (SELECT estado
                                FROM cat_maq_credito
                               WHERE entidad = 1);

         -- Marca origen 223-43 Bis                -- Marca origen 225-AG
         WHEN (r_conciliacion.marca_orig = 223) OR (r_conciliacion.marca_orig = 225)

            LET v_tpo_credito  = NULL
            LET v_num_credito  = 0
            LET v_f_otorga     = NULL
            LET v_id_ref_marca = NULL

            IF(r_conciliacion.marca_fin = 223) THEN
               LET r_aux_archivo.id_archivo    = v_id_arh_43
               LET r_aux_archivo.folio_archivo = v_folio_arh_43
               LET r_aux_archivo.tpo_transf    = '18'
               LET r_aux_archivo.tpo_uso       = 3
            ELSE
               IF(r_conciliacion.marca_fin = 225) THEN
                  LET r_aux_archivo.id_archivo    = v_id_arh_ag
                  LET r_aux_archivo.folio_archivo = v_folio_arh_ag
                  LET r_aux_archivo.tpo_transf    = '43'
                  LET r_aux_archivo.tpo_uso       = 2
               END IF
            END IF

            SELECT uso.*,
                   ctr.folio_archivo
              INTO r_inf_ug.*,
                   v_folio_archivo
              FROM cre_uso_garantia uso,
                   cre_ctr_archivo  ctr
             WHERE uso.id_cre_uso_garantia = r_conciliacion.id_referencia
               AND uso.id_cre_ctr_archivo  = ctr.id_cre_ctr_archivo;

            IF(v_folio_archivo IS NULL) THEN
               LET v_folio_archivo = 0
            END IF
            
            -- Solicitud de desmarca a PROCESAR
            IF(r_conciliacion.marca_orig = 223) AND (r_conciliacion.marca_fin = 225) THEN

               -- Al ser 223 quiere decir que tuvo un crédito 2-Apoyo infonavit
               -- el cuál en su marca procesar es 232, por lo que buscamos esa marca activa para
               -- solicitar la desmarca en Procesar
               SELECT MAX(n_referencia)
                 INTO v_id_ref_marca
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                  AND marca = 232;
                 
               IF(v_id_ref_marca IS NOT NULL) THEN
                  -- Recupera inf del crédito
                  SELECT tpo_credito,
                         num_credito,
                         f_otorga
                    INTO v_tpo_credito, 
                         v_num_credito,
                         v_f_otorga
                    FROM cre_acreditado
                   WHERE id_cre_acreditado = v_id_ref_marca

                   IF(v_tpo_credito IS NULL) THEN
                      LET v_tpo_credito = 2
                   END IF 

                   --Verifica si hay solicitud desmarca vía WS
                   SELECT MAX(id_derechohabiente)
                     INTO v_id_dh_ws
                     FROM cta_marca_ws
                    WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                      AND id_origen   = v_id_ref_marca
                      AND modulo_cod  = "16"
                      AND tpo_credito = v_tpo_credito
                      AND marca       = 232;

                   IF(v_id_dh_ws IS NULL) THEN 
                      -- Solicita la desmarca a Procesar de la 232
                      INSERT INTO cta_marca_ws(
                                  id_derechohabiente,
                                  id_origen         ,
                                  modulo_cod        ,
                                  tpo_credito       ,
                                  marca             ,
                                  f_solicita        ,
                                  intento           ,
                                  cod_result_op     ,
                                  diagnostico       ,
                                  situacion         ,
                                  num_credito       ,
                                  f_infonavit       ,
                                  marca_procesar    ,
                                  folio_archivo     ,
                                  usuario)
                           VALUES(r_conciliacion.id_derechohabiente,
                                  v_id_ref_marca                   ,
                                  '16'                             ,
                                  v_tpo_credito                    ,
                                  232                              ,
                                  TODAY                            ,
                                  1                                ,
                                  NULL                             ,
                                  NULL                             ,
                                  0                                ,
                                  v_num_credito                    ,
                                  v_f_otorga                       ,
                                  '02'                             ,
                                  v_folio_archivo                  ,
                                  g_usuario);
                   END IF 
               ELSE 
                  -- Se obtiene inf. por el id_Derechohabiente
                  SELECT FIRST 1 
                        tpo_credito,
                        num_credito,
                        f_otorga
                   INTO v_tpo_credito,
                        v_num_credito,
                        v_f_otorga
                   FROM cre_acreditado
                  WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                    AND tpo_originacion    = 2
                  ORDER BY f_otorga DESC;
               END IF

               IF(v_tpo_credito IS NULL) THEN
                  LET v_tpo_credito = 2
               END IF 

               --Verifica si hay solicitud desmarca vía WS
               SELECT MAX(id_derechohabiente)
                 INTO v_id_dh_ws
                 FROM cta_marca_ws
                WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                  AND id_origen   = r_conciliacion.id_referencia
                  AND modulo_cod  = "16"
                  AND tpo_credito = v_tpo_credito
                  AND marca       = 223;

               IF(v_id_dh_ws IS NULL) THEN 
                  -- Solicita desmarca de la 223
                  INSERT INTO cta_marca_ws(
                              id_derechohabiente,
                              id_origen         ,
                              modulo_cod        ,
                              tpo_credito       ,
                              marca             ,
                              f_solicita        ,
                              intento           ,
                              cod_result_op     ,
                              diagnostico       ,
                              situacion         ,
                              num_credito       ,
                              f_infonavit       ,
                              marca_procesar    ,
                              folio_archivo     ,
                              usuario)
                       VALUES(r_conciliacion.id_derechohabiente,
                              r_conciliacion.id_referencia     ,
                              '16'                             ,
                              v_tpo_credito                    ,
                              223                              ,
                              TODAY                            ,
                              1                                ,
                              NULL                             ,
                              NULL                             ,
                              0                                ,
                              v_num_credito                    ,
                              v_f_otorga                       ,
                              '02'                             ,
                              v_folio_archivo                  ,
                              g_usuario);
               END IF 
            END IF

            IF(r_conciliacion.marca_orig = 225) AND (r_conciliacion.marca_fin = 223) THEN

               -- Al ser 225 quiere decir que tuvo un crédito de AG
               -- el cuál en su marca procesar es 234, por lo que buscamos esa marca activa para
               -- solicitar la desmarca en Procesar
               SELECT MAX(n_referencia)
                 INTO v_id_ref_marca
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                  AND marca = 234;

               IF(v_id_ref_marca IS NOT NULL) THEN 
                  -- Recupera inf del crédito
                  SELECT tpo_credito,
                         num_credito,
                         f_otorga
                    INTO v_tpo_credito, 
                         v_num_credito,
                         v_f_otorga
                    FROM cre_acreditado
                   WHERE id_cre_acreditado = v_id_ref_marca

                   IF(v_tpo_credito IS NULL) THEN
                      LET v_tpo_credito = 10
                   END IF 

                   --Verifica si hay solicitud desmarca vía WS
                   SELECT MAX(id_derechohabiente)
                     INTO v_id_dh_ws
                     FROM cta_marca_ws
                    WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                      AND id_origen   = v_id_ref_marca
                      AND modulo_cod  = "04"
                      AND tpo_credito = v_tpo_credito
                      AND marca       = 234;

                   IF(v_id_dh_ws IS NULL) THEN 
                      -- Solicita la desmarca a Procesar de la 234
                      INSERT INTO cta_marca_ws(
                                  id_derechohabiente,
                                  id_origen         ,
                                  modulo_cod        ,
                                  tpo_credito       ,
                                  marca             ,
                                  f_solicita        ,
                                  intento           ,
                                  cod_result_op     ,
                                  diagnostico       ,
                                  situacion         ,
                                  num_credito       ,
                                  f_infonavit       ,
                                  marca_procesar    ,
                                  folio_archivo     ,
                                  usuario)
                           VALUES(r_conciliacion.id_derechohabiente,
                                  v_id_ref_marca                   ,
                                  '04'                             ,
                                  v_tpo_credito                    ,
                                  234                              ,
                                  TODAY                            ,
                                  1                                ,
                                  NULL                             ,
                                  NULL                             ,
                                  0                                ,
                                  v_num_credito                    ,
                                  v_f_otorga                       ,
                                  '04'                             ,
                                  v_folio_archivo                  ,
                                  g_usuario);
                  END IF 
               ELSE 
                  -- obt inf por derechohabiente.
                  SELECT FIRST 1
                        tpo_credito,
                        num_credito,
                        f_otorga
                   INTO v_tpo_credito,
                        v_num_credito,
                        v_f_otorga
                   FROM cre_acreditado
                  WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                    AND tpo_originacion    = 4
                  ORDER BY f_otorga DESC;
               END IF 

               IF(v_tpo_credito IS NULL) THEN
                  LET v_tpo_credito = 10
               END IF 

               --Verifica si hay solicitud desmarca vía WS
               SELECT MAX(id_derechohabiente)
                 INTO v_id_dh_ws
                 FROM cta_marca_ws
                WHERE id_derechohabiente = r_conciliacion.id_derechohabiente
                  AND id_origen   = r_conciliacion.id_referencia
                  AND modulo_cod  = "04"
                  AND tpo_credito = v_tpo_credito
                  AND marca       = 225;

               IF(v_id_dh_ws IS NULL) THEN 
                  -- Solicita la desmarca a Procesar de la 225
                  INSERT INTO cta_marca_ws(
                              id_derechohabiente,
                              id_origen         ,
                              modulo_cod        ,
                              tpo_credito       ,
                              marca             ,
                              f_solicita        ,
                              intento           ,
                              cod_result_op     ,
                              diagnostico       ,
                              situacion         ,
                              num_credito       ,
                              f_infonavit       ,
                              marca_procesar    ,
                              folio_archivo     ,
                              usuario)
                       VALUES(r_conciliacion.id_derechohabiente,
                              r_conciliacion.id_referencia     ,
                              '04'                             ,
                              v_tpo_credito                    ,
                              225                              ,
                              TODAY                            ,
                              1                                ,
                              NULL                             ,
                              NULL                             ,
                              0                                ,
                              v_num_credito                    ,
                              v_f_otorga                       ,
                              '04'                             ,
                              v_folio_archivo                  ,
                              g_usuario);
               END IF
            END IF

            -- Obtiene monto acciones por el folio_liquida.
            IF(r_inf_ug.folio_liquida > 0) THEN
               -- Obtiene tabla de movimiento donde se hizo el cargo
               EXECUTE prp_tab_movimiento USING r_inf_ug.folio_liquida,
                                                " "
                                           INTO v_tab_movimiento

               LET v_cadena = "SELECT subcuenta,SUM(monto_acciones) 
                                 FROM ",v_tab_movimiento,"
                                WHERE folio_liquida      = ",r_inf_ug.folio_liquida,
                                " AND id_derechohabiente = ",r_conciliacion.id_derechohabiente,
                                " GROUP BY 1;"

               PREPARE prp_monto_uso FROM v_cadena
               DECLARE crs_monto_uso CURSOR FOR prp_monto_uso

               -- Inicializa aivs 
               LET v_aux_aivs92       = 0
               LET v_aux_aivs97       = 0

               FOREACH crs_monto_221 INTO r_recu_acciones.subcuenta,
                                          r_recu_acciones.monto_aivs

                  IF(r_recu_acciones.subcuenta = 4) THEN
                     LET v_aux_aivs97 = v_aux_aivs97 +  r_recu_acciones.monto_aivs
                  ELSE 
                     IF(r_recu_acciones.subcuenta = 8) THEN
                        LET v_aux_aivs92 = v_aux_aivs92 + r_recu_acciones.monto_aivs
                     END IF 
                  END IF

               END FOREACH

               IF(v_aux_aivs97 IS NULL) THEN
                  LET v_aux_aivs97 = 0 
               END IF

               IF(v_aux_aivs92 IS NULL) THEN
                  LET v_aux_aivs92 = 0 
               END IF

               -- Actualiza tabla de conciliación de marcas
               UPDATE cre_marca_conciliacion
                  SET aivs92 = v_aux_aivs92,
                      aivs97 = v_aux_aivs97
                WHERE id_referencia      = r_conciliacion.id_referencia
                  AND id_derechohabiente = r_conciliacion.id_derechohabiente
                  AND f_proceso = TODAY;

               -- Obtiene monto pesos
               LET r_inf_ug.importe_v97 = ((v_aux_aivs92 + v_aux_aivs97) * v_precio_fondo)

            END IF 
            
            SELECT seq_cre_uso.NEXTVAL
              INTO v_id_referencia
              FROM systables
             WHERE tabid = 1;

            -- Realiza petición de saldo de la nueva originación
            INSERT INTO cre_uso_garantia(id_cre_uso_garantia,
                                         id_cre_ctr_archivo ,
                                         folio_liquida      ,
                                         id_derechohabiente ,
                                         tpo_transferencia  ,
                                         tpo_uso            ,
                                         num_credito        ,
                                         f_presentacion     ,
                                         f_movimiento       ,
                                         periodo_pago       ,
                                         importe_v97        ,
                                         nss_afore          ,
                                         rfc_afore          ,
                                         paterno_afore      ,
                                         materno_afore      ,
                                         nombre_afore       ,
                                         nom_imss           ,
                                         edo_procesar       ,
                                         diagnostico        ,
                                         estado             ,
                                         f_proceso)
                                 VALUES (v_id_referencia                  ,
                                         r_aux_archivo.id_archivo         ,
                                         0                                ,
                                         r_conciliacion.id_derechohabiente,
                                         r_aux_archivo.tpo_transf         ,
                                         r_aux_archivo.tpo_uso            ,
                                         0                                ,
                                         TODAY                            ,
                                         TODAY                            ,
                                         v_periodo_pago                   , --calcular periodo_pago
                                         r_inf_ug.importe_v97             ,
                                         '00000000000'                    ,
                                         ''                               ,
                                         ''                               ,
                                         ''                               ,
                                         ''                               ,
                                         ''                               ,
                                         70                               ,
                                         ''                               ,
                                         142                              ,
                                         TODAY);

            -- Marca cuenta como petición de saldo
            LET v_cadena = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,0,0,'','',?,?)"
            PREPARE prp2_marca_cuenta FROM v_cadena
            EXECUTE prp2_marca_cuenta USING  r_conciliacion.id_derechohabiente,
                                             r_conciliacion.marca_fin,
                                             v_id_referencia,
                                             g_folio,
                                             g_usuario,
                                             g_proceso_cod
                                       INTO  v_sts_marcaje

            -- Actualiza la solicitud
            UPDATE cre_uso_garantia
               SET estado = 285
             WHERE id_cre_uso_garantia  = r_conciliacion.id_referencia
               AND id_derechohabiente   = r_conciliacion.id_derechohabiente
               AND estado IN (SELECT estado
                               FROM cat_maq_credito
                              WHERE entidad = 1);
      END CASE

   END FOREACH
   
END FUNCTION