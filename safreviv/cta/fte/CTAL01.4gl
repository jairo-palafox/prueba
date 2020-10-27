#################################################################################
# Modulo       => AFI                                                           #
# Programa     => AFIL27                                                        #
# Objetivo     => Lanzador para generar archivo de marcas consumiendo WS de     #
#                 consulta de saldos en procesar                                #
# Autor        => Jose Eduardo Ventura Bonola                                   #
# Fecha        => 06/JUNIO/2016                                                 #
#################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE        p_proceso_cod   INTEGER
   DEFINE        p_opera_cod     INTEGER
   DEFINE        p_usuario       CHAR (20)
   DEFINE        p_nom_ventana   STRING
   DEFINE        p_tpo_ejecucion SMALLINT
   DEFINE        r_b_valida      SMALLINT
   DEFINE        p_pid           INTEGER

END GLOBALS

MAIN

   LET p_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_val (2)
   LET p_nom_ventana   = ARG_VAL (3)
   LET p_proceso_cod   = 718 -- numero de proceso correspondiente
   LET p_opera_cod     = 1    -- numero de operacion correspondiente
   CALL fn_carga_archivo()

END MAIN


FUNCTION fn_carga_archivo()

   DEFINE v_archivo_d              STRING   -- Variable para saber ruta y nombre donde se dejará el archivo
   DEFINE v_archivo                STRING   -- Nombre de archivo seleccionado
   DEFINE v_pos                    INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant                     INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE buf                      base.StringBuffer
   DEFINE v_extension              STRING
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_nss                    CHAR(11)
   DEFINE v_entrada                STRING
   DEFINE v_ruta_bin               LIKE seg_modulo.ruta_bin
   DEFINE v_s_comando              STRING

   OPEN WINDOW CTAL01 WITH FORM "CTAL011"

   INPUT BY NAME v_archivo ATTRIBUTES (UNBUFFERED)
   AFTER INPUT
   ON ACTION ACCEPT

--Se valida que el archivo tenga nombre y extensión correctos

      IF v_archivo IS NULL THEN
         CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
         NEXT FIELD v_archivo
      END IF

      IF v_archivo.getIndexOf(" ", 1) THEN
         CALL fn_mensaje ("Transferencia Archivo","El nombre del archivo no debe contener espacios en blanco","information")
         LET v_archivo = ""
         DISPLAY BY NAME v_archivo
         NEXT FIELD v_archivo
      END IF

      IF v_archivo IS NOT NULL THEN
         LET v_entrada = v_archivo
         --CALL fn_mensaje ("Transferencia" ,v_archivo,"information")
         LET buf = base.StringBuffer.create()
         CALL buf.append(v_archivo)

         LET cant         = LENGTH(v_archivo)
         LET v_pos        = buf.getIndexof(".",1)
         LET v_extension  = buf.subString(v_pos,cant)

            IF (v_extension <> ".mca") THEN 
            CALL fn_mensaje ("Transferencia Archivo","La extensión del archivo no es correcta,  \n
                               el archivo debe tener extensión .mca","information")
                           LET v_archivo = ""
               DISPLAY BY NAME v_archivo
               NEXT FIELD v_archivo

            END IF 
            --CALL fn_mensaje ("Transferencia" ,v_nom_archivo,"information")

        IF  v_extension   = ".mca" THEN

         LET v_archivo_d = v_archivo
            --DISPLAY "archivo :",v_archivo_d
--**********************************************************************************************************

--***************************************************
--Se  recupera el archivo y se deja en ruta rescate *
--***************************************************

-- Se recuperan las rutas de rescate y envio para el archivo
         SELECT ruta_rescate,
                ruta_envio,
                ruta_bin
           INTO v_ruta_rescate,
                v_ruta_envio,
                v_ruta_bin
           FROM seg_modulo
          WHERE modulo_cod ="cta"

         LET v_archivo_d = v_ruta_rescate CLIPPED,"/",v_archivo--||v_extension
         TRY
         CALL FGL_GETFILE(v_archivo,v_archivo_d)
         LET v_entrada = v_archivo_d
         --CALL fn_busca_datos(v_entrada)
         --fglrun
         --se obtienen rutas necesarias

         SELECT ruta_listados
           INTO v_ruta_listados
           FROM seg_modulo
          WHERE modulo_cod = 'bat'

         CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_usuario) RETURNING p_pid
         CALL fn_inicializa_proceso(p_pid,p_proceso_cod,p_opera_cod,
                              "","CTAW18","",p_usuario)  RETURNING r_b_valida

         CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,
                                     p_opera_cod,"",
                                     "CTAW18","",
                                     p_usuario)  RETURNING r_b_valida

         LET v_s_comando = "nohup fglrun ",
                           v_ruta_bin CLIPPED,
                           "/CTAW18 ",
                           p_usuario    ," ",
                           p_pid        ," ",
                           p_proceso_cod," ",
                           p_opera_cod  ," ",
                           v_entrada    ," ",
                           " ' '  1>",
                           v_ruta_listados CLIPPED ,
                           "/nohup:",
                           p_pid         USING "&&&&&",":",
                           p_proceso_cod USING "&&&&&",":",
                           p_opera_cod   USING "&&&&&" ," 2>&1 &"

                     

   RUN v_s_comando

  DISPLAY "v_s_comando", v_s_comando

  LET v_s_comando = "Se ejecutó la generación de archivo BUC"," ",
                    "Verificar en el monitor de proceso la ejecución el PID ", p_pid USING "<<<<<<<<<"
  CALL fn_mensaje("Cuentas",v_s_comando,"information")
         EXIT INPUT
         CONTINUE INPUT

         CATCH
            ERROR "NO SE PUDO TRANSFERIR"
            CONTINUE INPUT
            END TRY
         EXIT INPUT
      CLOSE WINDOW CTAL01

      END IF
    END IF
      EXIT INPUT
      CLOSE WINDOW AGRL521
      END INPUT

END FUNCTION