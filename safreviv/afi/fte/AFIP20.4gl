################################################################################
#Modulo           => GLO                                                       #
#Programa         => AFIP20                                                    #
#Objetivo         => Lanza SP para integrar datos de contacto                  #
#Fecha de Inicio  => FEBERERO 2015                                             #
################################################################################
DATABASE safre_viv

DEFINE g_pid                        DECIMAL(9,0)
DEFINE g_proceso_cod                SMALLINT
DEFINE g_usuario_cod                CHAR(20)
DEFINE g_opera_cod                  SMALLINT
DEFINE g_folio                      DECIMAL(9,0)
DEFINE g_nom_archivo                CHAR(40)

MAIN
   DEFINE v_estado              SMALLINT
   DEFINE p_titulo              STRING
   DEFINE p_mensaje             STRING
   DEFINE v_existe_rechazos     INTEGER

   LET g_usuario_cod           = ARG_VAL(1)
   LET g_pid                   = ARG_VAL(2)
   LET g_proceso_cod           = ARG_VAL(3)
   LET g_opera_cod             = ARG_VAL(4)
   LET g_folio                 = ARG_VAL(5)
   LET g_nom_archivo           = ARG_VAL(6)

   CALL fn_display_proceso(0,"INICIA INTEGRACION")

   CALL fn_integra_archivo() RETURNING v_estado

   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod)
                                 RETURNING v_estado

      UPDATE glo_ctr_archivo
         SET estado= 2,
             folio = g_folio
       WHERE proceso_cod = g_proceso_cod
         AND opera_cod = 1
         AND nombre_archivo = g_nom_archivo
         
      LET p_mensaje = "Integración notificaciones realizada con éxito."

      CALL fn_genera_reporte()

      SELECT COUNT(*)
        INTO v_existe_rechazos
        FROM afi_rch_ind_not
      IF v_existe_rechazos > 0 THEN
         CALL fn_genera_rechazos()
      END IF
   ELSE
      LET p_mensaje = "El proceso de integración ha finalizado pero con excepciones.\nVerificar en el sistema SACI."
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
      UPDATE glo_ctr_archivo
         SET estado= 3,
             folio = g_folio
       WHERE proceso_cod = g_proceso_cod
         AND opera_cod = 1
         AND nombre_archivo = g_nom_archivo
   END IF
   --Se ejecutan los displays
   CALL fn_display_proceso(1,"INICIA INTEGRACION")

   LET p_titulo = "Integración archivo de indicadores de envio notificaciones"
   CALL fn_correo_proceso(g_pid, g_proceso_cod,
                          g_opera_cod,
                          NULL, p_titulo,p_mensaje)

END MAIN

FUNCTION fn_integra_archivo ()

   DEFINE v_sql_procedure  STRING
   DEFINE v_estatus        SMALLINT
   DEFINE r_cod_error      SMALLINT
   DEFINE r_error_isam     INTEGER
   DEFINE r_mensaje_error  VARCHAR(255)

   LET v_estatus = 0 --El cero indica que se jecuto con exito
   LET v_sql_procedure = "EXECUTE PROCEDURE  sp_integra_ind_notificacion(?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_integra_ind FROM v_sql_procedure
   EXECUTE prp_integra_ind  USING g_folio
      INTO r_cod_error, r_error_isam, r_mensaje_error

   IF (r_cod_error = 0) THEN
      DISPLAY r_mensaje_error
      DISPLAY r_cod_error
   ELSE
 --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "\n [SAFREVIV EXCEPCION ] "
      DISPLAY "Error de ejecución en 'sp_integra_ind_notificacion' (SQL): ",r_cod_error
      DISPLAY "Error en 'sp_integra_ind_notificacion' (ISAM):",r_error_isam,"\n"
      DISPLAY "Error en 'sp_integra_ind_notificacion' (Mensaje):",r_mensaje_error,"\n"
   END IF

   RETURN r_cod_error
END FUNCTION

FUNCTION fn_genera_reporte()
   DEFINE v_reporte           STRING
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_ruta_reporte      STRING
   DEFINE v_excepcion         SMALLINT

   DEFINE report_handler      om.SaxDocumentHandler

   DEFINE r_reporte           RECORD
      indicador                  SMALLINT,
      tpo_notificacion           SMALLINT,
      edo_indicador              SMALLINT,
      tot_indicador              INTEGER
   END RECORD

   LET v_reporte = "AFIP201.4rp"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario_cod CLIPPED , "-", -- usuario
                        "AFIP20", "-", -- programa
                        g_pid USING "&&&&&","-", -- PID
                        g_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SACI EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   
   IF NOT v_excepcion THEN
      DECLARE cur_ind_marca CURSOR FOR SELECT indicador, tpo_notificacion, edo_indicador, COUNT(*)
                                         FROM afi_his_ind_notifica
                                        WHERE folio_lote = g_folio
                                        GROUP BY 1,2,3

      START REPORT rep_indicadores TO XML HANDLER report_handler
      FOREACH cur_ind_marca INTO r_reporte.*
         OUTPUT TO REPORT rep_indicadores(r_reporte.*)
      END FOREACH
      CLOSE cur_ind_marca
      FREE cur_ind_marca
      
      DECLARE cur_rechazo CURSOR FOR SELECT indicador, tpo_notificacion, cod_excepcion, COUNT(*)
                                       FROM afi_rch_ind_not
                                      GROUP BY 1,2,3
      FOREACH cur_rechazo INTO r_reporte.*
         OUTPUT TO REPORT rep_indicadores(r_reporte.*)
      END FOREACH

      FINISH REPORT rep_indicadores
   END IF
END FUNCTION

REPORT rep_indicadores(p_reporte)
   DEFINE p_reporte           RECORD
      indicador                  SMALLINT,
      tpo_notificacion           SMALLINT,
      edo_indicador              SMALLINT,
      tot_indicador              INTEGER
   END RECORD
   DEFINE v_desc_indicador    STRING
   DEFINE v_desc_tpo_indica   CHAR(30)
   DEFINE v_desc_estado       STRING

   DEFINE p_r_encabezado    RECORD
        p_usuario_cod         STRING,
        p_fecha               DATE,
        p_folio               DECIMAL(9,0),
        p_archivo             STRING
   END RECORD

   DEFINE r_archivo           RECORD
      folio                DECIMAL(9,0),
      desmarca_sms         INTEGER,
      marca_sms            INTEGER,
      desmarca_correo      INTEGER,
      marca_correo         INTEGER,
      bloqueo_sms          INTEGER,
      bloqueo_correo       INTEGER,
      tot_det              INTEGER,
      tot_sum              INTEGER
   END RECORD
   DEFINE v_total             INTEGER
   DEFINE v_suma              INTEGER

   ORDER BY p_reporte.indicador, p_reporte.tpo_notificacion
   FORMAT
      
      FIRST PAGE HEADER
         LET p_r_encabezado.p_fecha = TODAY
         LET p_r_encabezado.p_usuario_cod = g_usuario_cod
         LET p_r_encabezado.p_folio = g_folio
         LET p_r_encabezado.p_archivo = g_nom_archivo

         SELECT *
           INTO r_archivo.*
           FROM afi_ind_cifras
          WHERE folio_lote = g_folio
                
         PRINTX p_r_encabezado.*, r_archivo.*

      BEFORE GROUP OF p_reporte.indicador

         CASE p_reporte.indicador
            WHEN 0 
               LET v_desc_indicador = "DESMARCA DE ENVIO DE NOTIFICACIONES "
            WHEN 1 
               LET v_desc_indicador = "MARCA DE ENVIO DE NOTIFICACIONES "
         END CASE

         PRINTX v_desc_indicador
         LET v_total = 0

      AFTER GROUP OF p_reporte.indicador
         PRINTX v_total

      ON EVERY ROW
         SELECT desc_notificacion
           INTO v_desc_tpo_indica
           FROM cat_afi_tpo_notifica
          WHERE tpo_notificacion = p_reporte.tpo_notificacion

         CASE p_reporte.edo_indicador
            WHEN 0
               LET v_desc_estado = "ACEPTADOS"
            WHEN 1
               LET v_desc_estado = "EL DERECHOHABIENTE NO EXISTE"
            WHEN 2
               LET v_desc_estado = "EL TIPO DE NOTIFICACIÓN NO EXISTE"
            WHEN 3
               LET v_desc_estado = "EL IDENTIFICADOR O MARCA ES INCORRECTO"
            WHEN 90
               LET v_desc_estado = "YA EXISTE LA MARCA DE NOTIFICACIÓN"
            WHEN 99
               LET v_desc_estado = "NO CONTABA CON LA MARCA DE NOTIFICACIÓN"
         END CASE

         PRINTX p_reporte.*, v_desc_tpo_indica, v_desc_estado

         LET v_total = p_reporte.tot_indicador + v_total

      ON LAST ROW
         LET v_suma = SUM(p_reporte.tot_indicador)
         PRINTX v_suma
END REPORT

FUNCTION fn_genera_rechazos()
   DEFINE v_proceso_cod_rch         SMALLINT
   DEFINE v_opera_cod_rch           SMALLINT
   DEFINE v_pid_rch                 DECIMAL(9,0)
   DEFINE v_programa                STRING
   DEFINE v_comando                 STRING
   DEFINE v_mensaje                 STRING
   DEFINE v_ruta_bin                CHAR(40)
   DEFINE v_ruta_bat                CHAR(40)

   DEFINE v_resultado               SMALLINT

   LET v_proceso_cod_rch = 1816
   LET v_opera_cod_rch   = 1
   LET v_programa = 'AFIP21'

   DISPLAY "[ SACI ] EXISTEN RECHAZOS, SE EJECUTARA PROCESO DE ARCHIVO DE RECHAZOS"
   
   CALL fn_valida_operacion(0,v_proceso_cod_rch,v_opera_cod_rch) RETURNING v_resultado

   IF v_resultado = 0 THEN
      CALL fn_genera_pid(v_proceso_cod_rch,v_opera_cod_rch,g_usuario_cod) RETURNING v_pid_rch
      CALL fn_inicializa_proceso(v_pid_rch,v_proceso_cod_rch,v_opera_cod_rch,0,
                                             v_programa,'',g_usuario_cod)
                                    RETURNING v_resultado

      IF v_resultado = 0 THEN
         CALL fn_actualiza_opera_ini(v_pid_rch,
                                     v_proceso_cod_rch,
                                     v_opera_cod_rch,
                                     0,
                                     v_programa,
                                     '',
                                     g_usuario_cod)
                           RETURNING v_resultado

         IF v_resultado = 0 THEN
            SELECT s.ruta_bin
              INTO v_ruta_bin
              FROM seg_modulo s
             WHERE s.modulo_cod = 'afi'

            SELECT b.ruta_listados
              INTO v_ruta_bat
              FROM seg_modulo b
             WHERE b.modulo_cod = 'bat'
            
            LET v_comando = " nohup time fglrun ",v_ruta_bin CLIPPED,"/AFIP21 ",
                            g_usuario_cod CLIPPED, " ",
                            v_pid_rch            , " ",
                            v_proceso_cod_rch    , " ",
                            v_opera_cod_rch      , " 0 ' ' ",
                            " 1>",v_ruta_bat CLIPPED,
                            "/nohup:",v_pid_rch USING "&&&&&",":",
                            v_proceso_cod_rch   USING "&&&&&",":",
                            v_opera_cod_rch     USING "&&&&&" ,
                            " 2>&1 &"
            RUN v_comando
            DISPLAY "[ SACI ] SE HA ENVIADO LA GENERACIÓN DEL ARCHIVO DE RECHAZOS"
            DISPLAY "[ SACI ] PODRÁ REVISAR EL RESULTADO EN EL MONITOR DE PROCESOS PID:", v_pid_rch
         ELSE
            CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
            DISPLAY "[ SACI ] ", v_mensaje
         END IF
      ELSE
         CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
         DISPLAY "[ SACI ] ", v_mensaje
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      DISPLAY "[ SACI ] ", v_mensaje
   END IF
END FUNCTION