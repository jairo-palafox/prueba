####################################################################################
#Módulo            => GRT                                                          #
#Programa          => GRTS07                                                       #
#Objetivo          => Programa lanzado para el proceso Generación del archivo      #
#                     de solicitudes UG histórico, el cual contendrá el detalle de #
#                     los registros que Cartera (SAS) solicitó para pagos UG 43BIS #
#Autor             => Emilio Abarca, EFP / Mauro Muñiz Caballero                   #
#Fecha inicio      => 3/julio/2018                                                 #
####################################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE p_usuario                 CHAR(20)
   DEFINE p_pid                     DECIMAL(9,0)
   DEFINE p_proceso_cod             SMALLINT
   DEFINE p_opera_cod               SMALLINT
   DEFINE p_id_archivo              DECIMAL(9,0)
   DEFINE P_nom_archivo             CHAR(40) 
   DEFINE P_f_proceso               DATE
   
   # Variables para información del reporte PDF
   DEFINE v_ruta_rpt                STRING 
   DEFINE v_ruta_lst                CHAR(40)
   DEFINE v_ruta_bin                CHAR(40)
   DEFINE v_ruta_envio              CHAR(40)
   DEFINE v_reporte_bin             STRING 
   DEFINE object_rpt                om.SaxDocumentHandler
   DEFINE r_b_valida                SMALLINT
   DEFINE v_bnd_calculo             SMALLINT
   DEFINE v_bnd_extractor           SMALLINT
   DEFINE v_f_presentacion          DATE

   --  Record para el RESUMEN
   DEFINE r_resumen RECORD
      f_ini_procesa                 CHAR(25),
      f_fin_procesa                 CHAR(25)
   END RECORD 

   -- Record detalle total global
   DEFINE r_det_total_glo RECORD
      tot_global                    INTEGER,
      monto_global                  DECIMAL(12,2),
      prc_global                    CHAR(8),
      tot_aceptados                 INTEGER,
      monto_aceptados               DECIMAL(12,2),
      prc_aceptados                 CHAR(8),
      tot_rechazados                INTEGER,
      monto_rechazados              DECIMAL(12,2),
     prc_rechazados                CHAR(8)
   END RECORD

    -- Record detalle UG
   DEFINE r_det_UG RECORD
      tot_global                    INTEGER,
      monto_global                  DECIMAL(12,2),
      prc_global                    CHAR(8),
      tot_aceptados                 INTEGER,
      monto_aceptados               DECIMAL(12,2),
      prc_aceptados                 CHAR(8),
      tot_rechazados                INTEGER,
      monto_rechazados              DECIMAL(12,2),
      prc_rechazados                CHAR(8)
   END RECORD 

   -- arreglo detale rechazos UG
   DEFINE arr_det_rch_UG DYNAMIC ARRAY OF RECORD
      tot_registros                 INTEGER,
      monto_pesos                   DECIMAL(12,2),
      estado                        SMALLINT,
      diagnostico                   CHAR(3),
      desc_causal                   CHAR(40)
   END RECORD 

END GLOBALS

MAIN

   -- Recibe valores enviados por el Lanzador GRTL50
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_id_archivo  = ARG_VAL(5)
   LET P_nom_archivo = ARG_VAL(6)
   LET P_f_proceso   = ARG_VAL(7)
   LET v_bnd_calculo = 0  -- No ha iniciado el cálculo de valores

   -- Log en caso de errores
   CALL STARTLOG(P_usuario CLIPPED|| ".GRTS07.log")

   SELECT ruta_bin,ruta_listados,ruta_envio
     INTO v_ruta_bin,v_ruta_lst,v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'grt'

   CALL fn_display_proceso(0,"GENERA ARCHIVO SOLICITUDES UG")

   DISPLAY ""
   DISPLAY " IDENT. ARCHIVO : ",p_id_archivo
   DISPLAY " NOMBRE ARCHIVO : ",p_nom_Archivo CLIPPED
   DISPLAY " FECHA PROCESO  : ",p_f_proceso USING "dd/mm/yyyy"
   DISPLAY ""

   CALL fn_calcula_informacion() RETURNING v_bnd_calculo
   CALL extractor_rechazos_ug() RETURNING v_bnd_extractor

   IF(v_bnd_calculo <> 1 AND 
      v_bnd_extractor <> 1) THEN 
      -- Muestra error al usuario
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida
      DISPLAY " ERROR : OCURRIÓ UN ERROR AL GENERAR LA INFORMACIÓN"
   ELSE
      -- Finaliza operación como correcta
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

      SELECT fecha_fin
        INTO r_resumen.f_fin_procesa
        FROM bat_ctr_operacion
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod   = p_opera_cod

      -- Genera reporte
      CALL configura_salida_reporte ()

      DISPLAY " El archivo de rechazos se ha generado en la ruta /safreviv_int/grt/envio \n con el nombre: Sol_UG_sas_",TODAY USING "yyyymmdd_",p_pid USING "&&&&&&",".cgs"

      CALL fn_display_proceso(1,"GENERA ARCHIVO SOLICITUDES UG -sas-")
   END IF

END MAIN

FUNCTION fn_calcula_informacion()

   DEFINE v_porcentaje              DECIMAL(4,1) 
   DEFINE v_indicador               SMALLINT 
   DEFINE K                         INTEGER 

   -- Inicializa variables
   LET v_indicador      = 0  -- Bandera apagada no ha terminado el cálculo
   LET v_f_presentacion = TODAY 
   LET v_porcentaje     = 0

   SELECT fecha_ini
     INTO r_resumen.f_ini_procesa
     FROM bat_ctr_operacion
    WHERE pid         = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

-----> CALCULA DETALLE TOTALES GLOBALES <-----

   -- Total global y el monto($)
   SELECT SUM (importe_v97), COUNT(*)
     INTO r_det_total_glo.monto_global,
          r_det_total_glo.tot_global
     FROM cre_uso_garantia
    WHERE tpo_transferencia  = "48" 
      AND id_cre_ctr_archivo = p_id_archivo
      AND f_proceso          = p_f_proceso;

   IF(r_det_total_glo.monto_global IS NULL) THEN
      LET r_det_total_glo.monto_global = 0
   END IF

   LET v_porcentaje =  (r_det_total_glo.tot_global /  r_det_total_glo.tot_global) * 100

   IF(v_porcentaje > 95) THEN
      LET v_porcentaje = 100
   END IF

   LET r_det_total_glo.prc_global = v_porcentaje CLIPPED,"%"

   -- Total Aceptados
   SELECT SUM (importe_v97),COUNT(*)
     INTO r_det_total_glo.monto_aceptados,
          r_det_total_glo.tot_aceptados
     FROM cre_uso_garantia
    WHERE tpo_transferencia  = "48" 
      AND estado             = 140
      AND edo_procesar       = 120
      AND id_cre_ctr_archivo = p_id_archivo
      AND f_proceso          = p_f_proceso;

   IF(r_det_total_glo.monto_aceptados IS NULL) THEN
      LET r_det_total_glo.monto_aceptados = 0 
   END IF 

   LET v_porcentaje = 0
   LET v_porcentaje = (r_det_total_glo.tot_aceptados / r_det_total_glo.tot_global) * 100

   IF(v_porcentaje > 95) THEN
      LET v_porcentaje = 100
   END IF

   LET r_Det_total_glo.prc_aceptados = v_porcentaje CLIPPED,"%"

   -- Total Rechazados
   SELECT SUM (importe_v97),COUNT(*)
     INTO r_det_total_glo.monto_rechazados,
          r_det_total_glo.tot_rechazados
     FROM cre_uso_garantia
    WHERE tpo_transferencia  = "48" 
      AND id_cre_ctr_archivo = p_id_archivo
      AND f_proceso          = p_f_proceso
      AND ((estado IN (150,240)) OR (estado = 20 AND edo_procesar <> 120))

   IF(r_det_total_glo.monto_rechazados IS NULL) THEN
      LET r_det_total_glo.monto_rechazados = 0 
   END IF 

   LET v_porcentaje = 0
   LET v_porcentaje = (r_det_total_glo.tot_rechazados / r_det_total_glo.tot_global) * 100

   IF(v_porcentaje > 95) THEN
      LET v_porcentaje = 100
   END IF 

   LET r_det_total_glo.prc_rechazados = v_porcentaje CLIPPED,"%"

----->  CALCULA DETALLE USO DE GARANTÍA  <-----

   -- Total global y monto($) UG  
   SELECT SUM (importe_v97),COUNT(*)
     INTO r_det_UG.monto_global,
          r_det_UG.tot_global
     FROM cre_uso_garantia
    WHERE tpo_transferencia  = "48" 
      AND id_cre_ctr_archivo = p_id_archivo
      AND f_proceso          = p_f_proceso;

   IF(r_det_UG.monto_global IS NULL) THEN
      LET r_det_UG.monto_global = 0 
   END IF 

   LET v_porcentaje = 0
   LET v_porcentaje = (r_det_UG.tot_global / r_det_UG.tot_global) * 100

   IF(v_porcentaje > 95) THEN
      LET v_porcentaje = 100
   END IF 

   LET r_det_UG.prc_global = v_porcentaje CLIPPED,"%"

   -- Total aceptados UG
   SELECT SUM (importe_v97),COUNT(*)
     INTO r_det_UG.monto_aceptados,
          r_det_UG.tot_aceptados
     FROM cre_uso_garantia
    WHERE tpo_transferencia  = "48" 
      AND estado             = 140
      AND edo_procesar       = 120
      AND id_cre_ctr_archivo = p_id_archivo
      AND f_proceso          = p_f_proceso;

   IF(r_det_UG.monto_aceptados IS NULL) THEN
      LET r_det_UG.monto_aceptados = 0 
   END IF

   LET v_porcentaje = 0
   LET v_porcentaje = (r_det_UG.tot_aceptados / r_det_UG.tot_global) * 100

   IF(v_porcentaje > 95) THEN
      LET v_porcentaje = 100
   END IF

   LET r_det_UG.prc_aceptados = v_porcentaje CLIPPED,"%"

   -- Total rechazados UG
   SELECT SUM (importe_v97),COUNT(*)
     INTO r_det_UG.monto_rechazados,
          r_det_UG.tot_rechazados
     FROM cre_uso_garantia
    WHERE tpo_transferencia  = "48" 
      AND id_cre_ctr_archivo = p_id_archivo
      AND f_proceso          = p_f_proceso
      AND ((estado IN (150,240)) OR (estado = 20 AND edo_procesar <> 120))

   IF(r_det_UG.monto_rechazados IS NULL) THEN
      LET  r_det_UG.monto_rechazados = 0
   END IF

   LET v_porcentaje = 0
   LET v_porcentaje = (r_det_UG.tot_rechazados / r_det_UG.tot_global) * 100

   IF(v_porcentaje > 95) THEN
      LET v_porcentaje = 100
   END IF

   LET r_det_UG.prc_rechazados = v_porcentaje CLIPPED,"%"

-----> DETALLE RECHAZOS USO DE GARANTIA  <-----

    -- Inicializa arreglo, en caso de no encontrar rechazos
    LET arr_det_rch_UG[1].tot_registros = 0
    LET arr_det_rch_UG[1].monto_pesos   = 0
    LET arr_det_rch_UG[1].estado        = 0
    LET arr_det_rch_UG[1].diagnostico   = 0
    LET arr_det_rch_UG[1].desc_causal   = 0

   -- Detalle rechazos UG
   DECLARE crs_rch_UG_240 CURSOR FOR
      SELECT estado,
             diagnostico,
             SUM (importe_v97),
             COUNT(*)
        FROM cre_uso_garantia
       WHERE tpo_transferencia  = "48"
         AND estado             = 240
         AND id_cre_ctr_archivo = p_id_archivo
         AND f_proceso          = p_f_proceso
         GROUP BY 1,2;

   LET K = 1

   FOREACH crs_rch_UG_240 INTO arr_det_rch_UG[k].estado,
                               arr_det_rch_UG[k].diagnostico,
                               arr_det_rch_UG[k].monto_pesos,
                               arr_det_rch_UG[k].tot_registros

      -- Búsca la desc. del rechazo
      IF (arr_det_rch_UG[k].estado IS NOT NULL) THEN 
         SELECT desc_estado
            INTO arr_det_rch_UG[k].desc_causal
            FROM cat_rch_acreditado
           WHERE estado = arr_det_rch_UG[k].diagnostico
      END IF 

      LET k = k + 1
   END FOREACH 

   CLOSE crs_rch_UG_240
   FREE crs_rch_UG_240

   -- Recupera el rechazo 150
   DECLARE crs_rch_UG_150 CURSOR FOR
      SELECT estado,
             diagnostico,
             SUM (importe_v97),
             COUNT(*)
        FROM cre_uso_garantia
       WHERE tpo_transferencia  = "48"
         AND estado             = 150
         AND id_cre_ctr_archivo = p_id_archivo
         AND f_proceso          = p_f_proceso
         GROUP BY 1,2;

   FOREACH crs_rch_UG_150 INTO arr_det_rch_UG[k].estado,
                               arr_det_rch_UG[k].diagnostico,
                               arr_det_rch_UG[k].monto_pesos,
                               arr_det_rch_UG[k].tot_registros

      -- Búsca la desc. del rechazo
      IF (arr_det_rch_UG[k].estado IS NOT NULL) THEN
         ---LET arr_det_rch_UG[k].diagnostico =  arr_det_rch_UG[k].estado
         SELECT estado_desc
            INTO arr_det_rch_UG[k].desc_causal
            FROM cat_maq_credito
           WHERE estado = arr_det_rch_UG[k].estado
      END IF

      LET k = k + 1
   END FOREACH

   CLOSE crs_rch_UG_150
   FREE crs_rch_UG_150

   -- Recupera el rechazo 20 edo_procesar <> 120
   DECLARE crs_rch_UG_20 CURSOR FOR
      SELECT estado,
             diagnostico,
             SUM (importe_v97),
             COUNT(*)
        FROM cre_uso_garantia
       WHERE tpo_transferencia  = "48"
         AND estado             = 20
         AND edo_procesar       <> 120
         AND id_cre_ctr_archivo = p_id_archivo
         AND f_proceso          = p_f_proceso
         GROUP BY 1,2;

   FOREACH crs_rch_UG_20 INTO arr_det_rch_UG[k].estado,
                              arr_det_rch_UG[k].diagnostico,
                              arr_det_rch_UG[k].monto_pesos,
                              arr_det_rch_UG[k].tot_registros

      -- Búsca la desc. del rechazo
      IF (arr_det_rch_UG[k].estado IS NOT NULL) THEN
         ---LET arr_det_rch_UG[k].diagnostico =  arr_det_rch_UG[k].estado
         SELECT estado_desc
            INTO arr_det_rch_UG[k].desc_causal
            FROM cat_maq_credito
           WHERE estado = arr_det_rch_UG[k].estado
      END IF

      LET k = k + 1
   END FOREACH

   CLOSE crs_rch_UG_20
   FREE crs_rch_UG_20

   -- Elimina Última fila en blanco
   IF (arr_det_rch_UG[arr_det_rch_UG.getLength()].estado IS NULL) THEN
      CALL arr_det_rch_UG.deleteElement(arr_det_rch_UG.getLength()) 
   END IF

   LET v_indicador = 1 -- Cálculo terminado

   RETURN v_indicador

END FUNCTION

FUNCTION configura_salida_reporte()

   # -----> CONFIGURACIÓN DEL REPORTE PDF <-----
   LET v_reporte_bin = v_ruta_bin CLIPPED,"/GRTS07.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",p_usuario CLIPPED,"-GRTS07-",p_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&","-",p_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN

         START REPORT genera_PDF TO XML HANDLER object_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF 

      END IF

   END IF

END FUNCTION 

REPORT genera_PDF()

   DEFINE r                         INTEGER

   FORMAT

      FIRST PAGE HEADER 

         # ENCABEZADO
         PRINTX p_usuario
         PRINTX v_f_presentacion USING "dd/mm/yyyy"

         # RESUMEN
         PRINTX P_nom_archivo 
         PRINTX r_resumen.f_ini_procesa
         PRINTX r_resumen.f_fin_procesa

         # DETALLE GLOBAL
         PRINTX r_det_total_glo.tot_global
         PRINTX r_det_total_glo.monto_global
         PRINTX r_det_total_glo.prc_global
         PRINTX r_det_total_glo.tot_aceptados
         PRINTX r_det_total_glo.monto_aceptados
         PRINTX r_det_total_glo.prc_aceptados
         PRINTX r_det_total_glo.tot_rechazados
         PRINTX r_det_total_glo.monto_rechazados
         PRINTX r_det_total_glo.prc_rechazados

         # DETALLE USO GARANTIA
         PRINTX r_det_UG.tot_global
         PRINTX r_det_UG.monto_global
         PRINTX r_det_UG.prc_global
         PRINTX r_det_UG.tot_aceptados
         PRINTX r_det_UG.monto_aceptados
         PRINTX r_det_UG.prc_aceptados
         PRINTX r_det_UG.tot_rechazados
         PRINTX r_det_UG.monto_rechazados
         PRINTX r_det_UG.prc_rechazados

      ON EVERY ROW 
      
         # DETALLE RECHAZOS AG
         FOR r = 1 TO arr_det_rch_UG.getLength()
            PRINTX arr_det_rch_UG[r].tot_registros
            PRINTX arr_det_rch_UG[r].monto_pesos
            PRINTX arr_det_rch_UG[r].diagnostico
            PRINTX arr_det_rch_UG[r].desc_causal
         END FOR

END REPORT

FUNCTION extractor_rechazos_ug()

   DEFINE archivo                   base.Channel
   DEFINE v_query                   STRING
   DEFINE v_qry_rch                 STRING
   DEFINE cont                      INTEGER
   DEFINE v_detalle                 STRING
   DEFINE v_arh_salida              STRING
   DEFINE v_aux_id_derechohabiente  DECIMAL(9,0)
   DEFINE v_aux_f_proceso           CHAR(10)
   DEFINE v_aux_estado              SMALLINT
   DEFINE v_aux_estado_rch          SMALLINT
   DEFINE v_aux_diagnostico_rch     CHAR(3)
   DEFINE v_aux_f_procedo_rch       DATE
   DEFINE v_aux_edo_procesar        SMALLINT
   DEFINE v_aux_diag                CHAR(3)
   DEFINE v_ind_extractor           SMALLINT
   DEFINE v_aux_importe             DECIMAL(12,2)
   DEFINE v_diag_rch                SMALLINT

   DEFINE r_det_rechazos RECORD
      nss                           CHAR(11),
      periodo                       CHAR(6),
      importe                       CHAR(15),
      f_solicitud                   CHAR(8),
      resp_saci                     CHAR(1),
      status                        CHAR(2),
      origen_rch                    CHAR(1),
      causal                        CHAR(3),
      tipo_sol                      CHAR(2),
      reintento                     CHAR(4)
   END RECORD 

   INITIALIZE r_det_rechazos.* TO NULL

   LET v_ind_extractor = 0
   LET v_arh_salida    = v_ruta_envio CLIPPED,"/Sol_UG_sas_",TODAY USING "yyyymmdd_",p_pid USING "&&&&&&",".cgs"

   LET v_query = "SELECT af.nss,
                       \n ug.id_derechohabiente,
                       \n ug.f_proceso,
                       \n ug.periodo_pago,
                       \n ug.importe_v97,
                       \n ug.estado,
                       \n ug.edo_procesar,
                       \n ug.diagnostico,
                       \n ug.tpo_uso
                  \n FROM cre_uso_garantia ug,
                       \n afi_derechohabiente af
                 \n WHERE ug.tpo_transferencia = '48'
                   \n AND ug.id_derechohabiente = af.id_derechohabiente
                   \n AND ug.edo_procesar       <> 120
                   \n AND ug.id_cre_ctr_archivo = ",p_id_archivo,
                   "\n AND ug.f_proceso = '",p_f_proceso,"'"

   LET archivo = base.Channel.create()

   CALL archivo.openFile(v_arh_salida,"w")

   INITIALIZE r_det_rechazos.* TO NULL

   LET v_aux_f_proceso    = ""
   LET v_aux_estado       = ""
   LET v_aux_estado_rch   = ""
   LET v_aux_edo_procesar = ""
   LET v_aux_diag         = ""
   LET v_aux_importe      = 0

   PREPARE prp_det_rch FROM v_query
   DECLARE crs_det_rch CURSOR FOR prp_det_rch

   LET cont = 1

   FOREACH crs_det_rch INTO r_det_rechazos.nss,
                            v_aux_id_derechohabiente,
                            v_aux_f_proceso,
                            r_det_rechazos.periodo,
                            v_aux_importe,
                            v_aux_estado,
                            v_aux_edo_procesar,
                            v_aux_diag,
                            r_det_rechazos.tipo_sol

      LET r_det_rechazos.importe     = v_aux_importe * 100
      LET r_det_rechazos.f_solicitud = v_aux_f_proceso[7,10],v_aux_f_proceso[1,2],v_aux_f_proceso[4,5]
      LET r_det_rechazos.tipo_sol    = "UG"
      LET r_det_rechazos.reintento   = 1
      LET r_det_rechazos.causal      = "" --Limpia causal
      LET r_det_rechazos.causal      = v_aux_diag

      IF (v_aux_estado = 240) OR (v_aux_estado = 150) THEN
         #rechazados
         LET r_det_rechazos.resp_saci  = 2
         LET r_det_rechazos.status     = "RS"
         LET r_det_rechazos.origen_rch = "1"

         IF r_det_rechazos.causal = "" OR r_det_rechazos.causal IS NULL THEN
            LET r_det_rechazos.causal = "INF"
         END IF
      ELSE
         #aceptados
         LET r_det_rechazos.resp_saci  = 1
         LET r_det_rechazos.status     = "SP"
         LET r_det_rechazos.origen_rch = "1"

         LET v_qry_rch = "SELECT FIRST 1 edo_procesar, diagnostico,f_proceso\n",
                          " FROM cre_uso_garantia\n",
                         " WHERE id_derechohabiente = ",v_aux_id_derechohabiente,"\n",
                         "   AND tpo_transferencia = '48'\n",
                         "   AND f_proceso > '",v_aux_f_proceso,"'\n",
                         "   AND periodo_pago = '",r_det_rechazos.periodo,"'\n",
                         "   AND edo_procesar IN(90,100,110)\n",
                         "ORDER BY f_proceso DESC"

         IF v_aux_edo_procesar > 10 THEN
            PREPARE prp_rch_prc FROM v_qry_rch
            DECLARE crs_rch_prc CURSOR FOR prp_rch_prc

            FOREACH crs_rch_prc INTO v_aux_estado_rch,
                                     v_aux_diagnostico_rch,
                                     v_aux_f_procedo_rch

               IF v_aux_estado_rch <> "" OR v_aux_estado_rch IS NOT NULL THEN
                  LET r_det_rechazos.causal = v_aux_diagnostico_rch

                  IF v_aux_estado_rch = 90 THEN
                     LET r_det_rechazos.origen_rch = "2"

                     IF r_det_rechazos.causal = "" OR r_det_rechazos.causal IS NULL THEN
                        LET r_det_rechazos.causal = "RCH"
                     END IF
                  ELSE
                     LET r_det_rechazos.origen_rch = "3"

                     IF v_aux_estado_rch = 110 THEN
                        LET r_det_rechazos.causal = "NA "
                     END IF

                     IF r_det_rechazos.causal = "" OR r_det_rechazos.causal OR r_det_rechazos.causal = "   " IS NULL THEN
                        LET r_det_rechazos.causal = "DEV"
                     END IF
                  END IF
               END IF
            END FOREACH

            CLOSE crs_rch_prc
            FREE crs_rch_prc
         END IF
      END IF

      IF r_det_rechazos.causal = "" OR r_det_rechazos.causal OR r_det_rechazos.causal = "   " IS NULL THEN
         LET r_det_rechazos.causal = "18"
      END IF

      -- Crea cadena para excribir en el archivo
      LET v_detalle = r_det_rechazos.nss,
                      r_det_rechazos.f_solicitud,
                      r_det_rechazos.periodo,
                      r_det_rechazos.importe USING "&&&&&&&&&&&&&&&",
                      r_det_rechazos.status,
                      r_det_rechazos.origen_rch,
                      r_det_rechazos.causal,
                      r_det_rechazos.reintento

      CALL archivo.writeLine(v_detalle)

      LET cont = cont + 1

      LET r_det_rechazos.nss         = ""
      LET r_det_rechazos.periodo     = ""
      LET r_det_rechazos.importe     = ""
      LET r_det_rechazos.f_solicitud = ""
      LET r_det_rechazos.resp_saci   = ""
      LET r_det_rechazos.status      = ""
      LET r_det_rechazos.origen_rch  = ""
      LET r_det_rechazos.causal      = ""
      LET r_det_rechazos.tipo_sol    = ""
      LET r_det_rechazos.reintento   = ""

      LET v_aux_id_derechohabiente   = ""
      LET v_aux_f_proceso            = ""
      LET v_aux_estado               = ""
      LET v_aux_estado_rch           = ""
      LET v_aux_edo_procesar         = ""
      LET v_aux_diag                 = ""
      LET v_aux_importe              = 0

   END FOREACH

   CLOSE crs_det_rch
   FREE crs_det_rch

   -- cierra archivo posterior a la escritura
   CALL archivo.close()
   LET v_ind_extractor = 1

   RETURN v_ind_extractor

END FUNCTION
