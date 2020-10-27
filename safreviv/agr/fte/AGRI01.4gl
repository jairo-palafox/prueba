
###############################################################
#Modulo        => AGR                                         #
#Programa      => AGRi01                                      #
#Objetivo      => Programa que genera informe del proceso de  #
#                 Solicitud de  Desmarca                      #
#                 Reactivación, Liquidación, Cancelación      #
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
   DEFINE g_id_cre_ctr_archivo DECIMAL(10,0)
   DEFINE g_folio              DECIMAL(10,0)
   DEFINE g_arh_proceso        VARCHAR(100)
   DEFINE g_arh_tot_reg        INTEGER  
   DEFINE g_tot_aceptados      INTEGER 
   DEFINE g_tot_rechazados     INTEGER 
   DEFINE g_tot_sin_origen     INTEGER 
   DEFINE g_tot_reac_acep      INTEGER 
   DEFINE g_tot_reac_rech      INTEGER
   DEFINE g_tot_cnt_acep       INTEGER 
   DEFINE g_tot_cnt_rech       INTEGER
   DEFINE g_tot_originacion    INTEGER 
   DEFINE r_b_valida           SMALLINT 

   --Variables para el reporte
   DEFINE v_f_ejecuta         CHAR(10)
   DEFINE v_tot_proc          INTEGER 
   DEFINE v_total_cifras      INTEGER 
   DEFINE v_tot_glo_cnt       INTEGER
   DEFINE v_tot_liq           INTEGER 
   DEFINE v_tot_glo_reac      INTEGER
   DEFINE v_tot_acep_sin_orig INTEGER 
   DEFINE v_prc_cancel        CHAR(4)
   DEFINE v_prc_liq           CHAR(4)
   DEFINE v_prc_orig          CHAR(4)
   DEFINE v_prc_react         CHAR(4)
   DEFINE v_prc_sin_orig      CHAR(4)
   DEFINE v_total_procentaje  CHAR(4)
   DEFINE v_concatena_prc     CHAR(6)
   DEFINE v_tot_liq_acep      INTEGER 
   DEFINE v_tot_liq_rech      INTEGER
   DEFINE v_total_aceptadas   INTEGER 
   DEFINE v_total_rechazadas  INTEGER
   DEFINE v_ind_extractor     SMALLINT 
   DEFINE v_s_titulo_correo   STRING 
   DEFINE v_s_archivo_correo  STRING 
   DEFINE v_ruta_reporte      STRING 
   DEFINE v_s_mens_correo     STRING
   DEFINE v_archivo_salida    STRING
   
END GLOBALS

MAIN 

   -- Recibe parámetros que envía el programa AGRE06
   LET g_usuario            = ARG_VAL(1)   -- USUARIO
   LET g_pid                = ARG_VAL(2)   -- PID
   LET g_proceso_cod        = ARG_VAL(3)   -- Código del proceso
   LET g_opera_cod          = ARG_VAL(4)   -- Operación
   LET g_id_cre_ctr_archivo = ARG_VAL(5)   --id_cre_ctr_archivo
   LET g_folio              = ARG_VAL(6)   -- Folio
   LET g_arh_proceso        = ARG_VAL(7)   -- Nombre del archivo
   LET g_arh_tot_reg        = ARG_VAL(8)   -- Total de registros en el archivo
   LET g_tot_aceptados      = ARG_VAL(9)   -- Total de registros aceptados
   LET g_tot_rechazados     = ARG_VAL(10)   -- Total de registros rechazados
   LET g_tot_sin_origen     = ARG_VAL(11)  -- Total de registros excluidos del procesamiento
   LET g_tot_reac_acep      = ARG_VAL(12)  -- Total reactivaciones aceptadas
   LET g_tot_reac_rech      = ARG_VAL(13)  -- Total reactivaciones rechazadas
   LET g_tot_cnt_acep       = ARG_VAL(14)  -- Total de registros aceptados cancelados
   LET g_tot_cnt_rech       = ARG_VAL(15)  -- Total de registros rechazados cancelados
   LET g_tot_originacion    = ARG_VAL(16)  -- Total de originaciones

   LET r_b_valida = 0

   CALL STARTLOG(g_usuario CLIPPED ||".AGRI01.log")

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

    
   CALL fn_display_proceso(0,"INICIA GENERACIÓN REPORTE DE DESMARCA AGR")
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
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,"","AGRI01","",g_usuario) RETURNING r_b_valida

       -- Llama la funcion que calcula información
      CALL calcula_datos() RETURNING r_b_valida  --> 1 Finalizado correctamente
      --Genera Extractor
      CALL fn_genera_extractor() RETURNING v_ind_extractor --> 1 Finalizado correctamente

      IF(r_b_valida = 1) AND (v_ind_extractor = 1) THEN 
         -- Finaliza Operación
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

         DISPLAY "==> El reporte .PDF de cifras de desmarca AGR se ha generado en /safreviv_lst/agr"
         DISPLAY ""
         DISPLAY "==> El extractor de rechazos se ha generado en la ruta /safreviv_int/agr/envio"
         DISPLAY "    con nombre: ",v_archivo_salida
         DISPLAY " "

         DISPLAY "ENVIA CORREO DEL REPORTE"
         --Asigna titulo del correo
         LET v_s_titulo_correo = "Proceso: REPORTE SOLICITUD DE DESMARCA AGR"

         -- Se asigna el archivo a adjuntar
         LET v_s_archivo_correo = v_ruta_reporte

         -- Se asigna cuerpo a correo
         LET v_s_mens_correo = "ID Proceso   : ",g_pid,"\n",
                                "Proceso      : SOLICITUD DESMARCA CRÉDITOS AG\n",
                                "Operacion    : REPORTE SOLICITUD DESMARCA AGR\n",
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
                             
         CALL fn_display_proceso(1,"FIN REPORTE DESMARCA AGR")
         DISPLAY " "
      ELSE 
         -- Muestra error al usuario
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida  
      END IF 
      
   END IF 
   
END MAIN

FUNCTION calcula_datos()

   DEFINE reporte       om.SaxDocumentHandler -- Objeto para reporte PDF
   DEFINE v_query              STRING 
   DEFINE v_reporte            STRING 
   DEFINE v_nombre_pdf         STRING 
   DEFINE v_estatus            SMALLINT 
   DEFINE v_aux_estado         DECIMAL(3,0)

   DEFINE r_liquidaciones RECORD
      nss         CHAR(11),
      tpo_credito DECIMAL(3,0)
   END RECORD 

   LET v_estatus = 0 
   
   -- Inicializa variables
   LET v_tot_proc          = 0
   LET v_prc_react         = 0
   LET v_tot_glo_reac      = 0
   LET v_prc_sin_orig      = 0
   LET v_total_procentaje  = 0
   LET v_tot_glo_cnt       = 0
   LET v_tot_liq           = 0
   LET v_tot_liq_acep      = 0
   LET v_tot_liq_rech      = 0
   LET v_total_cifras      = 0
   LET v_prc_cancel        = 0
   LET v_prc_liq           = 0
   LET v_prc_orig          = 0
   LET v_total_aceptadas   = 0
   LET v_total_rechazadas  = 0
   LET v_tot_acep_sin_orig = 0

   INITIALIZE r_liquidaciones.* TO NULL 
   LET v_aux_estado = NULL 
   LET v_reporte    = "AGRI011.4rp"
   LET v_nombre_pdf = "Recurrente_desmarca.pdf"
   LET v_f_ejecuta  = TODAY USING "dd/mm/yyyy"

   -- Registros para ser cancelados (aceptadas y rechazadas)
   LET v_tot_glo_cnt = g_tot_cnt_acep + g_tot_cnt_rech
   
   --Registros para procesamiento (Aceptados,rechazados y cancelaciones) 
   LET v_tot_proc = g_tot_aceptados + g_tot_rechazados + v_tot_glo_cnt
   
    -- Registros para ser liquidados
   {LET v_query = "SELECT d.estado,COUNT(DISTINCT d.nss)
                      FROM safre_tmp:tmp_desmarca_det_agr a,
                           safre_tmp:tmp_nss_desmarcados_agr d
                          WHERE a.nss = d.nss
                            AND a.tpo_registro = 11
                          GROUP BY 1;"}

   LET v_query = "SELECT nss,tpo_credito
                     FROM safre_tmp:tmp_desmarca_det_agr
                    WHERE tpo_registro = 11"

   PREPARE prp_liquidaciones FROM v_query
   DECLARE crs_liquidaciones CURSOR FOR prp_liquidaciones

   FOREACH crs_liquidaciones INTO r_liquidaciones.nss,
                                   r_liquidaciones.tpo_credito

      --busca si ese registro de aceptó o rechazó para la desmarca
      SELECT MAX(estado)
        INTO v_aux_estado
        FROM safre_tmp:tmp_nss_desmarcados_agr
       WHERE nss = r_liquidaciones.nss
         AND tpo_credito = r_liquidaciones.tpo_credito;
         
      IF(v_aux_estado = 1) THEN
         LET  v_tot_liq_acep = v_tot_liq_acep + 1
      END IF 

      IF(v_aux_estado = 2) OR 
        (v_aux_estado = 3) THEN
         LET v_tot_liq_rech =  v_tot_liq_rech +  1
      END IF 
      
   END FOREACH 

   -- Total liquidaciones (Aceptadas y Rechazadas)
   LET v_tot_liq = v_tot_liq_acep + v_tot_liq_rech

   -- Suma registros para ser reactivados (aceptadas y rechazadas)
   LET v_tot_glo_reac = g_tot_reac_acep + g_tot_reac_rech 
  
   -- Suma total de registros
   LET v_total_cifras = v_tot_glo_cnt + v_tot_liq + g_tot_originacion + v_tot_glo_reac + g_tot_sin_origen
 
   -- Obtiene porcentaje por cada entidad

   -- Cancelaciones
   LET v_prc_cancel = (v_tot_glo_cnt / v_total_cifras) * 100

   -- Liquidados
   LET v_prc_liq = (v_tot_liq / v_total_cifras) * 100

   -- Originaciones
   LET v_prc_orig = (g_tot_originacion / v_total_cifras) * 100

   -- Reactivaciones
   LET v_prc_react = (v_tot_glo_reac / v_total_cifras) * 100

   --Sin Originaciónn
   LET v_prc_sin_orig = (g_tot_sin_origen / v_total_cifras) * 100
   
   -- Suma porcentaje
   LET v_total_procentaje = v_prc_cancel + v_prc_liq + v_prc_orig + v_prc_react + v_prc_sin_orig
   LET v_concatena_prc    = v_total_procentaje CLIPPED,"%"

   -- Total aceptadas
   LET v_total_aceptadas = g_tot_cnt_acep + v_tot_liq_acep + g_tot_reac_acep + v_tot_acep_sin_orig

   --Total Rechazadas
   LET v_total_rechazadas = g_tot_cnt_rech + v_tot_liq_rech + g_tot_reac_rech + g_tot_sin_origen

   # Carga del Reporte PDF
   LET v_ruta_reporte = v_ruta_listados CLIPPED,"/",
                        g_usuario CLIPPED,"-",
                        "AGRI01","-",
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
--***** DETALLE *****
        PRINTX v_tot_glo_cnt       --TOTAL CANCELACIONES
        PRINTX v_tot_liq           --TOTAL LIQUIDACIONES
        PRINTX g_tot_originacion   --TOTAL ORIGINACIONES
        PRINTX v_tot_glo_reac      --TOTAL REGISTROS PARA SER REACTIVADOS
        PRINTX v_total_cifras      --TOTAL CIFRAS
        PRINTX v_prc_cancel        --% CANCEACIONES
        PRINTX v_prc_liq           --% LIQUIDACIONES
        PRINTX v_prc_orig          --% ORIGINACIONES
        PRINTX v_prc_react         --% REACTIVACIONES
        PRINTX v_prc_sin_orig      --% SIN ORIGINACIÓN
        PRINTX v_concatena_prc     --% SUMA
        PRINTX g_tot_cnt_acep      --CANCELACIONES ACEPTADAS
        PRINTx g_tot_cnt_rech      --CANCELACIONES RECHAZADAS
        PRINTX v_tot_liq_acep      --LIQUIDACIONES ACEPTADAS
        PRINTX v_tot_liq_rech      --LIQUIDACIONES RECHAZADAS
        PRINTX g_tot_reac_acep     --REACTIVACIONES ACEPTADAS
        PRINTX g_tot_reac_rech     --REACTIVACIONES RECHAZADAS
        PRINTX v_tot_acep_sin_orig --SIN ORIGINACIÓN ACEPTADAS
        PRINTX v_total_aceptadas   --TOTAL ACEPTADAS
        PRINTX v_total_rechazadas  --TOTAL RECHAZADAS
         
END REPORT

#Extractor genera información de los registros cargados en el archivo de entrada
FUNCTION fn_genera_extractor()

   DEFINE ch            base.channel
   DEFINE v_arh_salida  STRING
   DEFINE v_indicador   SMALLINT 
   
   DEFINE r_rechazos RECORD 
      tipo_operativo CHAR(1),
      tipo_registro  CHAR(2),
      nss            CHAR(11),
      num_credito    CHAR(10),
      tpo_credito    CHAR(3),
      status_credito CHAR(3),
      sdo_deudor     CHAR(14),
      f_otorgamiento DATE ,
      f_liquidacion  DATE,
      f_proceso      DATE,
      clave          CHAR(4),
      desc_rch       CHAR(41)
   END RECORD 

   DEFINE v_id_derechohabiente DECIMAL(9,0)
   DEFINE v_query_ext          STRING
   DEFINE v_qry_liquida        STRING 
   DEFINE v_linea              STRING 
   DEFINE v_resultado          SMALLINT
   DEFINE v_tpo_originacion    SMALLINT 
   DEFINE v_tpo_credito        SMALLINT 
   DEFINE v_num_credito        DECIMAL(10,0)
   DEFINE v_f_otorga           DATE 
   DEFINE v_f_liquida          DATE 
   DEFINE v_tpo_dscto          SMALLINT 
   DEFINE v_filler             CHAR(24)
   DEFINE i                    INTEGER
   DEFINE v_dt                 DATETIME HOUR TO SECOND
   DEFINE v_aux_hora           CHAR(8)
   DEFINE v_hora               CHAR(6)

   -- Formato a la hora
   LET v_dt = CURRENT
   LET v_aux_hora = v_dt
   LET v_hora     = v_aux_hora[1,2],v_aux_hora[4,5],v_aux_hora[7,8]

   LET v_archivo_salida = "rch_recurrente_desmarca_agr_",TODAY USING "yyyymmdd","_",v_hora,".txt"
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

   LET v_query_ext = "SELECT '' tipo_operativo,
                              r.tpo_registro,
                              r.nss,
                              r.num_credito,
                              r.tpo_credito,
                              r.edo_credito,
                              r.sdo_deudor,
                              r.f_otorga,
                              '' f_liquidacion,
                              a.f_proceso,
                              r.estado edo_rechazo,
                              c.desc_estado
                         FROM cre_rch_acreditado r,
                              cat_rch_acreditado c,
                              cre_ctr_archivo a
                        WHERE r.estado = c.estado
                          AND r.id_cre_ctr_archivo = ",g_id_cre_ctr_archivo,
                        " AND a.id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_extractor FROM v_query_ext
   DECLARE crs_extractor CURSOR FOR prp_extractor

   FOREACH crs_extractor INTO r_rechazos.tipo_operativo,
                               r_rechazos.tipo_registro,
                               r_rechazos.nss,
                               r_rechazos.num_credito,
                               r_rechazos.tpo_credito,
                               r_rechazos.status_credito,
                               r_rechazos.sdo_deudor,
                               r_rechazos.f_otorgamiento,
                               r_rechazos.f_liquidacion,
                               r_rechazos.f_proceso,
                               r_rechazos.clave,
                               r_rechazos.desc_rch

      --Tipo registro cancelaciones
      IF (r_rechazos.tipo_registro = 5) THEN 
          LET r_rechazos.tipo_operativo = 3
      END IF

      --Tipo registro liquidaciones
      IF (r_rechazos.tipo_registro = 11) THEN 
          LET r_rechazos.tipo_operativo = 4
      END IF    

       --Tipo registro reactivaciones
      IF (r_rechazos.tipo_registro = 4) THEN  
          LET r_rechazos.tipo_operativo = 5
      END IF  

      --Búsca el id_derechohabiente
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss =  r_rechazos.nss;

      -- La fecha liquida sólo aplica para las Cancelaciones y Liquidaciones

      IF (r_rechazos.tipo_registro = 11) OR (r_rechazos.tipo_registro = 5) THEN 
         LET v_qry_liquida = "EXECUTE FUNCTION fn_cred_liq_viv(?,?)"
         PREPARE prp_funcion FROM v_qry_liquida
         EXECUTE prp_funcion USING v_id_derechohabiente,'1'
                              INTO  v_resultado,  
                                     v_tpo_originacion,  
                                     v_tpo_credito,      
                                     v_num_credito,      
                                     v_f_otorga,         
                                     v_f_liquida,        
                                     v_tpo_dscto

        LET r_rechazos.f_liquidacion = v_f_liquida
     END IF 

      LET v_linea = r_rechazos.tipo_operativo CLIPPED,"|",
                    r_rechazos.tipo_registro CLIPPED,"|",
                    r_rechazos.nss CLIPPED,"|",
                    r_rechazos.num_credito CLIPPED,"|",
                    r_rechazos.tpo_credito CLIPPED,"|",
                    r_rechazos.status_credito CLIPPED,"|",
                    r_rechazos.sdo_deudor CLIPPED,"|",
                    r_rechazos.f_otorgamiento CLIPPED USING "YYYYMMDD","|",
                    r_rechazos.f_liquidacion CLIPPED USING "YYYYMMDD","|",
                    r_rechazos.f_proceso USING "YYYYMMDD","|",
                    r_rechazos.clave CLIPPED,"|",
                    r_rechazos.desc_rch CLIPPED, "|"

      -- Escribe detalle en el archivo
      CALL ch.writeLine(v_linea)
      
   END FOREACH 

   -- Crea filler 
   LET v_filler = " "

   FOR  i = 1 TO 23
      LET v_filler = v_filler," "
   END FOR 

   -- Escribe filler
   CALL ch.writeLine([v_filler])


   CALL ch.close()

   LET v_indicador = 1 -- Etractor finalizado

   RETURN v_indicador
   
END FUNCTION   