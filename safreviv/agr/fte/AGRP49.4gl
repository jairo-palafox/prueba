###########################################################################
#Modulo            => AGR                                                 #
#Programa          => AGRP49                                              #
#Objetivo          => Envío de archivos de Solicitud de saldos y archivos #
#                     de Restitución a Procesar, este programa lo lanza   #
#                     el AGRL70, AGRL71.                                  #
#Autor             => Emilio Abarca, EFP                                  #
#Fecha inicio      => 10/Julio/2018                                       #
###########################################################################

IMPORT os

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario           CHAR(20)
   DEFINE p_proceso_cod       SMALLINT  
   DEFINE p_opera_cod         SMALLINT
   DEFINE p_pid               DECIMAL(9,0)
   DEFINE p_tpo_archivo       SMALLINT
   DEFINE r_b_valida          SMALLINT
   DEFINE v_ind_rpt           SMALLINT
   DEFINE v_titulo_proceso    STRING

   --Arreglo global
   DEFINE arr_envio_archivo DYNAMIC ARRAY OF RECORD
      id_proceso   SMALLINT,
      archivo      CHAR(40),
      ruta_envio   CHAR(40),
      f_proceso    DATE,
      h_proceso    CHAR(20),
      t_registros  INTEGER,
      estado_envio CHAR(20)
   END RECORD

END GLOBALS

MAIN

   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_tpo_archivo    = ARG_VAL(5)

   -- Log en caso de errores
   CALL STARTLOG(p_usuario CLIPPED|| ".AGRP48.log")

   DISPLAY ""
   DISPLAY " = INICIA AGRP49 ="
   DISPLAY " USUARIO : ", p_usuario
   DISPLAY " PID     : ", p_pid  USING "<<<<<<<<<"
   DISPLAY " TPO ARCH: ", p_tpo_archivo

   IF(p_tpo_archivo = 1) THEN
      LET v_titulo_proceso = "ENVÍO ARCHIVOS SOLICITUD DE SALDOS A PROCESAR"
   ELSE
      IF(p_tpo_archivo = 2) THEN
         LET v_titulo_proceso = "ENVÍO ARCHIVOS DEVOLUCIÓN SDO EXC A PROCESAR"
      END IF 
   END IF

   CALL fn_display_proceso(0,v_titulo_proceso CLIPPED)

   CASE
      WHEN p_tpo_archivo = 1
         CALL envia_arh_sol_sdo()

      WHEN p_tpo_archivo = 2
         CALL envia_arh_dev_exc()
   END CASE

   --Genera reporte PDF
   LET v_ind_rpt = genera_rpt_pdf(p_tpo_archivo)

   IF (v_ind_rpt = 0) THEN
      DISPLAY ""
      DISPLAY " GENERA REPORTE PDF ...COMPLETADO"
   ELSE
      DISPLAY ""
      DISPLAY " HA OCURRIDO UN ERROR AL GENERAR EL REPORTE PDF ..."
       LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
    END IF

   -- Finaliza proceso
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   IF r_b_valida <> 0 THEN

      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

      -- en caso de error se muestra un mensaje a usuario
      CALL fn_desplega_inc_operacion(r_b_valida)

   END IF

   CALL fn_display_proceso(1,v_titulo_proceso CLIPPED)

END MAIN

FUNCTION envia_arh_sol_sdo()

   DEFINE v_qry_sol   STRING
   DEFINE v_c         INTEGER
   DEFINE v_dir_arh   STRING
   DEFINE v_size      STRING
   DEFINE v_time      STRING
   DEFINE v_comando   STRING
   DEFINE v_status    SMALLINT
   DEFINE v_aux_hora  CHAR(10)

   --Archivos de solicitud de saldos a enviar
   LET v_qry_sol = "SELECT id_proceso, \n",
                         " archivo,    \n",
                         " ruta_envio, \n",
                         " f_proceso,  \n",
                         " h_proceso,  \n",
                         " t_registros \n",
                   " FROM tmp_envio_arh_sol_sdo;\n"

   PREPARE prp_arh_sol FROM v_qry_sol
   DECLARE crs_arh_sol CURSOR FOR prp_arh_sol

   --Limpia arreglo
   CALL arr_envio_archivo.clear()

   LET v_dir_arh  = NULL
   LET v_size     = NULL
   LET v_time     = NULL
   LET v_status   = 0
   LET v_aux_hora = NULL

   LET v_c = 1

   FOREACH crs_arh_sol INTO arr_envio_archivo[v_c].id_proceso,
                            arr_envio_archivo[v_c].archivo,
                            arr_envio_archivo[v_c].ruta_envio,
                            arr_envio_archivo[v_c].f_proceso,
                            v_aux_hora,
                            arr_envio_archivo[v_c].t_registros



      --Concatena Hora
      LET arr_envio_archivo[v_c].h_proceso = v_aux_hora CLIPPED," Hrs"

      --crea cadena para verificar si existe el archivo
      LET v_dir_arh = arr_envio_archivo[v_c].ruta_envio CLIPPED,"/",arr_envio_archivo[v_c].archivo CLIPPED

      LET v_size   = NULL
      LET v_time   = NULL
      LET v_status = 0 -- Status ejecución correcta

      #Comienza proceso de envío de acuerdo al archivo
      CASE
         --Archivo TA Acreditados
         WHEN arr_envio_archivo[v_c].archivo = "sol_ta.acr"
            DISPLAY ""
            DISPLAY " ******************** Envío archivo sol_ta.acr ******************** "
            --Verifica que exista el archivo
            IF NOT(os.Path.exists(v_dir_arh))THEN
               DISPLAY ""
               DISPLAY " No existe el archivo sol_ta.acr por enviar"
               LET arr_envio_archivo[v_c].estado_envio = "No enviado"
            ELSE
               DISPLAY ""
               DISPLAY " Ruta archivo: ",v_dir_arh
               DISPLAY " Número de registros: ",arr_envio_archivo[v_c].t_registros
               --Obtiene Fecha y hora del archivo como guía para verificar cual se está enviando
               LET v_time = os.Path.mtime(v_dir_arh)
               DISPLAY " Fecha y hora archivo servidor: ",v_time
               --Obtiene tamaño del archivo para verificar cuál se está enviando
               LET v_size = os.Path.size(v_dir_arh)
               DISPLAY " Tamaño archivo servidor: ",v_size

               DISPLAY ""
               DISPLAY " Ejecuta shell AXWAY .."
               LET v_comando = "sh /opt/Interpel/Scripts/sol_ta.sh"
               DISPLAY " Comando ejecuta: ",v_comando
               DISPLAY ""

               RUN v_comando RETURNING v_status

               IF(v_status = 0) THEN
                  LET arr_envio_archivo[v_c].estado_envio = "Enviado"

                  --Guarda en bitácora de envío
                  INSERT INTO cre_envio_arh_procesar(
                                   id_proceso ,
                                   archivo,
                                   ruta_envio,
                                   f_proceso,
                                   h_proceso,
                                   t_registros,
                                   f_envio,
                                   usuario
                                   )
                           VALUES (arr_envio_archivo[v_c].id_proceso,
                                   arr_envio_archivo[v_c].archivo,
                                   arr_envio_archivo[v_c].ruta_envio,
                                   arr_envio_archivo[v_c].f_proceso,
                                   v_aux_hora,
                                   arr_envio_archivo[v_c].t_registros,
                                   TODAY,
                                   p_usuario);
               ELSE
                  LET arr_envio_archivo[v_c].estado_envio = "No enviado"
               END IF
            END IF
            DISPLAY ""
            DISPLAY " ****************************************************************** "

         WHEN arr_envio_archivo[v_c].archivo = "sol_ag.agr"

            DISPLAY ""
            DISPLAY " ******************** Envío archivo sol_ag.agr ******************** "
            --Verifica que exista el archivo
            IF NOT(os.Path.exists(v_dir_arh))THEN
               DISPLAY ""
               DISPLAY " No existe el archivo sol_ag.agr por enviar"
               LET arr_envio_archivo[v_c].estado_envio = "No enviado"
            ELSE
               DISPLAY ""
               DISPLAY " Ruta archivo: ",v_dir_arh
               DISPLAY " Número de registros: ",arr_envio_archivo[v_c].t_registros
               --Obtiene Fecha y hora del archivo como guía para verificar cual se está enviando
               LET v_time = os.Path.mtime(v_dir_arh)
               DISPLAY " Fecha y hora archivo servidor: ",v_time
               --Obtiene tamaño del archivo para verificar cuál se está enviando
               LET v_size = os.Path.size(v_dir_arh)
               DISPLAY " Tamaño archivo servidor: ",v_size

               DISPLAY ""
               DISPLAY " Ejecuta shell AXWAY .."
               LET v_comando = "sh /opt/Interpel/Scripts/sol_ag.sh"
               DISPLAY " Comando ejecuta: ",v_comando
               DISPLAY ""

               RUN v_comando RETURNING v_status

               IF(v_status = 0) THEN
                  LET arr_envio_archivo[v_c].estado_envio = "Enviado"

                  --Guarda en bitácora de envío
                  INSERT INTO cre_envio_arh_procesar(
                                   id_proceso ,
                                   archivo,
                                   ruta_envio,
                                   f_proceso,
                                   h_proceso,
                                   t_registros,
                                   f_envio,
                                   usuario
                                   )
                           VALUES (arr_envio_archivo[v_c].id_proceso,
                                   arr_envio_archivo[v_c].archivo,
                                   arr_envio_archivo[v_c].ruta_envio,
                                   arr_envio_archivo[v_c].f_proceso,
                                   v_aux_hora,
                                   arr_envio_archivo[v_c].t_registros,
                                   TODAY,
                                   p_usuario);
               ELSE
                  LET arr_envio_archivo[v_c].estado_envio = "No enviado"
               END IF

            END IF
            DISPLAY ""
            DISPLAY " ****************************************************************** "

         WHEN arr_envio_archivo[v_c].archivo = "sol_ug.usog"

            DISPLAY ""
            DISPLAY " ******************* Envío archivo sol_ug.usog ******************** "
            --Verifica que exista el archivo
            IF NOT(os.Path.exists(v_dir_arh))THEN
               DISPLAY ""
               DISPLAY " No existe el archivo sol_ug.asog por enviar"
               LET arr_envio_archivo[v_c].estado_envio = "No enviado"
            ELSE
               DISPLAY ""
               DISPLAY " Ruta archivo: ",v_dir_arh
               DISPLAY " Número de registros: ",arr_envio_archivo[v_c].t_registros
               --Obtiene Fecha y hora del archivo como guía para verificar cual se está enviando
               LET v_time = os.Path.mtime(v_dir_arh)
               DISPLAY " Fecha y hora archivo servidor: ",v_time
               --Obtiene tamaño del archivo para verificar cuál se está enviando
               LET v_size = os.Path.size(v_dir_arh)
               DISPLAY " Tamaño archivo servidor: ",v_size

               DISPLAY ""
               DISPLAY " Ejecuta shell AXWAY .."
               LET v_comando = "sh /opt/Interpel/Scripts/sol_ug.sh"
               DISPLAY " Comando ejecuta: ",v_comando
               DISPLAY ""

               RUN v_comando RETURNING v_status

               IF(v_status = 0) THEN
                  LET arr_envio_archivo[v_c].estado_envio = "Enviado"

                  --Guarda en bitácora de envío
                  INSERT INTO cre_envio_arh_procesar(
                                   id_proceso ,
                                   archivo,
                                   ruta_envio,
                                   f_proceso,
                                   h_proceso,
                                   t_registros,
                                   f_envio,
                                   usuario
                                   )
                           VALUES (arr_envio_archivo[v_c].id_proceso,
                                   arr_envio_archivo[v_c].archivo,
                                   arr_envio_archivo[v_c].ruta_envio,
                                   arr_envio_archivo[v_c].f_proceso,
                                   v_aux_hora,
                                   arr_envio_archivo[v_c].t_registros,
                                   TODAY,
                                   p_usuario);
               ELSE
                  LET arr_envio_archivo[v_c].estado_envio = "No enviado"
               END IF

            END IF
            DISPLAY ""
            DISPLAY " ****************************************************************** "
         
      END CASE

      LET v_c = v_c + 1

   END FOREACH

END FUNCTION

FUNCTION envia_arh_dev_exc()

   DEFINE v_qry_dev      STRING
   DEFINE v_r            INTEGER
   DEFINE v_dir_dev      STRING
   DEFINE v_size_dev     STRING
   DEFINE v_time_dev     STRING
   DEFINE v_comando_dev  STRING
   DEFINE v_status_dev   SMALLINT
   DEFINE v_aux_hora_dev CHAR(10)

   --Archivos de solicitud de saldos a enviar
   LET v_qry_dev = "SELECT id_proceso, \n",
                         " archivo,    \n",
                         " ruta_envio, \n",
                         " f_proceso,  \n",
                         " h_proceso,  \n",
                         " t_registros \n",
                   " FROM tmp_envio_arh_dev_exc;\n"

   PREPARE prp_arh_dev FROM v_qry_dev
   DECLARE crs_arh_dev CURSOR FOR prp_arh_dev

   --Limpia arreglo
   CALL arr_envio_archivo.clear()

   LET v_dir_dev      = NULL
   LET v_size_dev     = NULL
   LET v_time_dev     = NULL
   LET v_status_dev   = 0
   LET v_aux_hora_dev = NULL

   LET v_r = 1

   FOREACH crs_arh_dev INTO arr_envio_archivo[v_r].id_proceso,
                            arr_envio_archivo[v_r].archivo,
                            arr_envio_archivo[v_r].ruta_envio,
                            arr_envio_archivo[v_r].f_proceso,
                            v_aux_hora_dev,
                            arr_envio_archivo[v_r].t_registros

      --Concatena Hora
      LET arr_envio_archivo[v_r].h_proceso = v_aux_hora_dev CLIPPED," Hrs"

      --crea cadena para verificar si existe el archivo
      LET v_dir_dev = arr_envio_archivo[v_r].ruta_envio CLIPPED,"/",arr_envio_archivo[v_r].archivo CLIPPED

      LET v_size_dev   = NULL
      LET v_time_dev   = NULL
      LET v_status_dev = 0 -- Status ejecución correcto

      #Comienza proceso de envío de acuerdo al archivo
      CASE
         --Archivo TA Acreditados
         WHEN arr_envio_archivo[v_r].archivo = "sol_dev_acr.dse"

            DISPLAY ""
            DISPLAY " ***************** Envío archivo sol_dev_acr.dse ****************** "
            --Verifica que exista el archivo
            IF NOT(os.Path.exists(v_dir_dev))THEN
               DISPLAY ""
               DISPLAY " No existe el archivo sol_dev_acr.dse por enviar"
               LET arr_envio_archivo[v_r].estado_envio = "No enviado"
            ELSE
               DISPLAY ""
               DISPLAY " Ruta archivo: ",v_dir_dev
               DISPLAY " Número de registros: ",arr_envio_archivo[v_r].t_registros
               --Obtiene Fecha y hora del archivo como guía para verificar cual se está enviando
               LET v_time_dev = os.Path.mtime(v_dir_dev)
               DISPLAY " Fecha y hora archivo servidor: ",v_time_dev
               --Obtiene tamaño del archivo para verificar cuál se está enviando
               LET v_size_dev = os.Path.size(v_dir_dev)
               DISPLAY " Tamaño archivo servidor: ",v_size_dev

               DISPLAY ""
               DISPLAY " Ejecuta shell AXWAY .."
               LET v_comando_dev = "sh /opt/Interpel/Scripts/sol_dev_acr.sh"
               DISPLAY " Comando ejecuta: ",v_comando_dev
               DISPLAY ""

               RUN v_comando_dev RETURNING v_status_dev

               IF(v_status_dev = 0) THEN
                  LET arr_envio_archivo[v_r].estado_envio = "Enviado"

                  --Guarda en bitácora de envío
                  INSERT INTO cre_envio_arh_procesar(
                                  id_proceso ,
                                  archivo,
                                  ruta_envio,
                                  f_proceso,
                                  h_proceso,
                                  t_registros,
                                  f_envio,
                                  usuario
                                  )
                          VALUES (arr_envio_archivo[v_r].id_proceso,
                                  arr_envio_archivo[v_r].archivo,
                                  arr_envio_archivo[v_r].ruta_envio,
                                  arr_envio_archivo[v_r].f_proceso,
                                  v_aux_hora_dev,
                                  arr_envio_archivo[v_r].t_registros,
                                  TODAY,
                                  p_usuario);

               ELSE
                  LET arr_envio_archivo[v_r].estado_envio = "No enviado"
               END IF

            END IF
            DISPLAY ""
            DISPLAY " ****************************************************************** "

         WHEN arr_envio_archivo[v_r].archivo = "sol_dev_ug.cdse"

            DISPLAY ""
            DISPLAY " ***************** Envío archivo sol_dev_ug.cdse ****************** "
            --Verifica que exista el archivo
            IF NOT(os.Path.exists(v_dir_dev))THEN
               DISPLAY ""
               DISPLAY " No existe el archivo sol_dev_ug.cdse por enviar"
               LET arr_envio_archivo[v_r].estado_envio = "No enviado"
            ELSE
               DISPLAY ""
               DISPLAY " Ruta archivo: ",v_dir_dev
               DISPLAY " Número de registros: ",arr_envio_archivo[v_r].t_registros
               --Obtiene Fecha y hora del archivo como guía para verificar cual se está enviando
               LET v_time_dev = os.Path.mtime(v_dir_dev)
               DISPLAY " Fecha y hora archivo servidor: ",v_time_dev
               --Obtiene tamaño del archivo para verificar cuál se está enviando
               LET v_size_dev = os.Path.size(v_dir_dev)
               DISPLAY " Tamaño archivo servidor: ",v_size_dev

               DISPLAY ""
               DISPLAY " Ejecuta shell AXWAY .."
               LET v_comando_dev = "sh /opt/Interpel/Scripts/sol_dev_ug.sh"
               DISPLAY " Comando ejecuta: ",v_comando_dev
               DISPLAY ""

               RUN v_comando_dev RETURNING v_status_dev

               IF(v_status_dev = 0) THEN
                  LET arr_envio_archivo[v_r].estado_envio = "Enviado"

                  --Guarda en bitácora de envío
                  INSERT INTO cre_envio_arh_procesar(
                                  id_proceso ,
                                  archivo,
                                  ruta_envio,
                                  f_proceso,
                                  h_proceso,
                                  t_registros,
                                  f_envio,
                                  usuario
                                  )
                          VALUES (arr_envio_archivo[v_r].id_proceso,
                                  arr_envio_archivo[v_r].archivo,
                                  arr_envio_archivo[v_r].ruta_envio,
                                  arr_envio_archivo[v_r].f_proceso,
                                  v_aux_hora_dev,
                                  arr_envio_archivo[v_r].t_registros,
                                  TODAY,
                                  p_usuario);
               ELSE
                  LET arr_envio_archivo[v_r].estado_envio = "No enviado"
               END IF

            END IF
            DISPLAY ""
            DISPLAY " ****************************************************************** "
         
      END CASE

      LET v_r = v_r + 1

   END FOREACH

END FUNCTION

FUNCTION genera_rpt_pdf(p_tpo_arh)

   DEFINE p_tpo_arh        SMALLINT
   DEFINE v_reporte_bin    STRING
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_ruta_lst       CHAR(40)
   DEFINE v_ruta_rpt       STRING
   DEFINE v_manejador_rpt  OM.SaxDocumentHandler
   DEFINE bnd_rpt          SMALLINT

   SELECT ruta_bin,ruta_listados
     INTO v_ruta_bin,v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = 'agr';

   #################################################
   #   CONFIGURACION PARA SALIDA DEL REPORTE PDF   #
   #################################################

   LET bnd_rpt = 0 --OK

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP491.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",
                       p_usuario CLIPPED,"-AGRP49-",
                       p_pid USING "&&&&&","-",
                       p_proceso_cod USING "&&&&&","-",
                       p_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN

         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT genera_PDF(p_tpo_arh)

         FINISH REPORT genera_PDF

      END IF
   ELSE
       DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
       LET bnd_rpt  = 1
   END IF

   RETURN bnd_rpt

END FUNCTION

REPORT genera_PDF(p_archivo)

   DEFINE p_archivo        SMALLINT
   DEFINE v_f_presentacion DATE
   DEFINE v_titulo_rpt     CHAR(60)
   DEFINE f                INTEGER

   FORMAT
   FIRST PAGE HEADER
      LET v_f_presentacion = TODAY

      IF(p_archivo = 1) THEN
         LET v_titulo_rpt = "ENVÍO ARCHIVOS SOLICITUD DE SALDOS"
      ELSE
         LET v_titulo_rpt = "ENVÍO ARCHIVOS DEVOLUCIÓN SDO EXC"
      END IF

      #ENCABEZADO
      PRINTX p_usuario
      PRINTX v_f_presentacion USING "dd/mm/yyyy"
      PRINTX v_titulo_rpt

   ON EVERY ROW
      --rechazos solicitudes nuevas
      FOR f = 1 TO arr_envio_archivo.getLength()

         PRINTX arr_envio_archivo[f].archivo
         PRINTX arr_envio_archivo[f].ruta_envio
         PRINTX arr_envio_archivo[f].f_proceso USING "dd/mm/yyyy"
         PRINTX arr_envio_archivo[f].h_proceso
         PRINTX arr_envio_archivo[f].t_registros
         PRINTX arr_envio_archivo[f].estado_envio

      END FOR

END REPORT




