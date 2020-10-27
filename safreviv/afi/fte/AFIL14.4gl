####################################################################################
#Proyecto     => SAFRE VIVIENDA                                                    #
#Propietario  => E.F.P.                                                            #
#----------------------------------------------------------------------------------#
#Modulo       => AFI                                                               #
#Programa     => AFIL14                                                            #
#Objetivo     => Programa que lanza la generación del archivo de salida            #
#                de los registros de derechohabientes - domicilios                 #
#Fecha inicio => 21 de octubre de 2013                                             #
####################################################################################


DATABASE safre_viv

GLOBALS

   DEFINE p_pid                  LIKE bat_ctr_operacion.pid          -- PID del proceso
   DEFINE p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod  -- codigo del proceso
   DEFINE p_opera_cod            LIKE bat_ctr_operacion.opera_cod    -- codigo de la operacion
   DEFINE p_usuario              LIKE seg_usuario.usuario_cod        -- clave del usuario firmado
   DEFINE p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo

   DEFINE p_tipo_carga           SMALLINT -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE v_valida               SMALLINT

   DEFINE p_folio                DECIMAL(9,0)

   DEFINE v_extension            CHAR(3)
   DEFINE v_ax_fecha             CHAR(8)

   DEFINE v_f_genera             DATE

   DEFINE p_nom_pgm              VARCHAR(30)
   DEFINE p_pgm_exe              VARCHAR(10)
   DEFINE v_ruta_salida          VARCHAR(40)   -- directorio de archivos de salida
   DEFINE v_ruta_listado         VARCHAR(40)
   DEFINE v_ruta_listado_bat     VARCHAR(40)

   DEFINE v_mensaje              STRING
   DEFINE v_qryTxt               STRING
   DEFINE v_comando              STRING

END GLOBALS

MAIN

   -- se asignan los parametros que vienen del fglrun
   LET p_usuario     = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_nom_pgm     = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_usuario CLIPPED|| ".AFIL14.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   IF ( p_nom_pgm IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_nom_pgm)
   END IF

   -- se inicializan variables
   LET p_proceso_cod    = 1811 -- generación extractor de acreditados
   LET p_opera_cod      = 1 -- generación extractor de acreditados
   LET p_pid            = 0
   LET p_folio          = 0
   LET p_pgm_exe        = "AFIP14"
   LET v_f_genera       = TODAY

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("afi") RETURNING v_ruta_salida, v_ruta_listado

   -- se obtienen la ruta de listados para el modulo bat
   LET v_qryTxt = " SELECT ruta_listados\n",
                  "   FROM seg_modulo\n",
                  "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_ruta_listado_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod) RETURNING v_valida

   -- se verifica si la operacion en proceso es valida
   IF v_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_valida)

      EXIT PROGRAM
   END IF

   OPEN WINDOW w_genera_arh WITH FORM "AFIL141"

   MENU
      BEFORE MENU
         LET v_f_genera = TODAY
         DISPLAY v_f_genera USING "DD-MM-YYYY" TO v_f_genera

      -- en caso de aceptar...
      ON ACTION ACCEPT
         -- se valida la fecha de generación del archivo
         IF v_f_genera IS NULL THEN
            CALL fn_mensaje("Archivo de salida","Debe ingresar una fecha para continuar","stop")

            CONTINUE MENU
         END IF

         -- se obtiene la extensión del archivo
         LET v_extension = fn_recupera_extension(p_proceso_cod, p_opera_cod)

         -- se cambia el formato de la fecha de liquidación a YYYYMMDD
         LET v_ax_fecha = v_f_genera USING "YYYYMMDD"
         LET p_nombre_archivo = "Arhdhdom"||v_ax_fecha||"." || v_extension CLIPPED

         IF ( f_existe_proceso_operacion_ejecutando(703, NULL) ) THEN
            -- alguna operacion de Precalificación se encuentra ejecutando, por lo que no se permite realizar la generación
            CALL fn_mensaje("Atención","No se puede ejecutar este proceso ya que la generación del archivo de Precalificación ya se encuentra en proceso de generación.\nEspere a que finalice.","information")

            CONTINUE MENU
         END IF

         -- se crea la sentencia sql que ejecuta la funcion que genera el pid
         LET p_pid = fn_genera_pid(p_proceso_cod, p_opera_cod, p_usuario)

         -- se invoca la funcion que inicializa el proceso
         LET v_valida = fn_inicializa_proceso(p_pid,
                                              p_proceso_cod,
                                              p_opera_cod,
                                              p_folio,
                                              p_pgm_exe,
                                              p_nombre_archivo,
                                              p_usuario)

         -- se verifica si fue posible inicializar el proceso
         IF v_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(v_valida)

            EXIT MENU
         END IF

         -- se invoca la función que deja la operación en estado Procesando
         LET v_valida = fn_actualiza_opera_ini(p_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               p_folio,
                                               p_pgm_exe,
                                               p_nombre_archivo,
                                               p_usuario)

         -- se verifica si fue posible inicializar la operacion
         IF v_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(v_valida)

            EXIT MENU
         END IF

         -- se crea el comando que ejecuta el modulo que genera el archivo de salida de amortización
         LET v_comando = " nohup time fglrun ",v_ruta_salida CLIPPED,"/AFIP14 ",
                                                 p_usuario, " ",
                                                 p_pid, " ",
                                                 p_proceso_cod, " ",
                                                 p_opera_cod, " ",
                                                 p_folio, " ",
                                                 p_nombre_archivo, " 1> ",
                                                 v_ruta_listado_bat CLIPPED,
                                                 "/nohup:",p_pid USING "&&&&&",":",
                                                 p_proceso_cod USING "&&&&&",":",
                                                 p_opera_cod USING "&&&&&",
                                                 " 2>&1 &"

         -- se ejecuta el comando armado
         RUN v_comando
         
         -- se asigna el mensaje a mostrar al usuario
         LET v_mensaje = "Se ha enviado la ejecución de la extracción con PID: ",p_pid CLIPPED,
                           ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
         CALL fn_mensaje("Aviso",v_mensaje,"information")

         EXIT MENU

      ON ACTION CANCEL
         EXIT MENU
   END MENU

   CLOSE WINDOW w_genera_arh

END MAIN