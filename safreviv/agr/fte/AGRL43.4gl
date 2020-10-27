--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL43                                        #
#Objetivo          =>Programa que genera el archivo de salida de   #
#                    acreditados para cualquier módulo             #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>17 Junio 2013                                 #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN

   DEFINE p_v_nom_prog        VARCHAR(30) -- nombre del programa
   DEFINE p_b_tipo_carga      SMALLINT -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_usuario         LIKE seg_usuario.usuario -- usuario firmado al sistema
   DEFINE v_i_proceso_cod     LIKE cat_proceso.proceso_cod -- proceso que llama las funciones
   DEFINE v_i_opera_cod       LIKE cat_operacion.opera_cod -- operación que llama la funcion
   DEFINE v_d_pid             LIKE bat_ctr_operacion.pid -- identificador del proceso
   DEFINE v_d_folio           LIKE bat_ctr_operacion.folio -- folio del proceso
   DEFINE v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo en proceso
   DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_s_comando         STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt          STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_dt_f_generacion   DATE -- fecha de generación del archivo
   DEFINE v_c_f_generac_aux   CHAR(8) -- fecha ayxiliar de presentacion con formato YYYYMMDD
   DEFINE v_i_cuenta_regs     DECIMAL(9,0) -- numerica para contar registros
   DEFINE v_c_extension       LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_mensaje         STRING -- mensaje a mostrar al usuario
   DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_dia_semana        SMALLINT

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".AGRL43.log")

   CLOSE WINDOW SCREEN

      -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan variables
   LET v_i_proceso_cod   = g_proc_cod_agr_extrac_acred -- generación extractor de acreditados
   LET v_i_opera_cod     = 1 -- generación extractor de acreditados
   LET v_d_pid           = 0
   LET v_d_folio         = 0
   LET v_v_nom_archivo   = "N/A"
   LET v_c_programa_cod  = "AGRL43"
   LET v_dt_f_generacion = ""

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   OPEN WINDOW w_genera_archAmortizacion WITH FORM "AGRL431"
   MENU
      BEFORE MENU
         LET v_dt_f_generacion = TODAY
         DISPLAY v_dt_f_generacion USING "DD-MM-YYYY" TO dt_f_generacion

      -- en caso de aceptar
      ON ACTION ACCEPT
         -- se valida la fecha de generación del archivo
         IF v_dt_f_generacion IS NULL THEN
            CALL fn_mensaje("Archivo de salida","Debe ingresar una fecha para continuar","stop")

            CONTINUE MENU
         END IF

         -- se obtiene la extensión del archivo
         LET v_c_extension = fn_recupera_extension(v_i_proceso_cod, v_i_opera_cod)

         -- se cambia el formato de la fecha de liquidación a YYYYMMDD
         LET v_c_f_generac_aux = v_dt_f_generacion USING "YYYYMMDD"
         LET v_v_nom_archivo = "extr_cred_" || v_c_f_generac_aux || "." || v_c_extension CLIPPED

         IF ( f_existe_proceso_operacion_ejecutando(703, NULL) ) THEN
            -- alguna operacion de Precalificación se encuentra ejecutando, por lo que no se permite realizar la generación
            CALL fn_mensaje("Atención","No se puede ejecutar este proceso ya que la generación del archivo de Precalificación ya se encuentra en proceso de generación.\nEspere a que finalice.","information")

            CONTINUE MENU
         END IF

        {
         SELECT UNIQUE WEEKDAY(TODAY)
           INTO v_dia_semana
            FROM cat_mes

         IF v_dia_semana > 2 THEN
            -- alguna operacion de Precalificación se encuentra ejecutando, por lo que no se permite realizar la generación
            CALL fn_mensaje("Atención","No se puede ejecutar este proceso ya que la generación del archivo de Precalificación ya se encuentra en proceso de preparación o generación.\nEspere a que finalice.","information")

            CONTINUE MENU
         END IF
         }

         -- se valida que exista informacion para generar el archivo
         -- (usando el mismo query que se usa en el archivo de salida)
         SELECT COUNT(*)
           INTO v_i_cuenta_regs
           FROM cre_acreditado c,
                cat_maq_credito m
          WHERE c.estado = m.estado
            AND m.entidad IN(1,2,5)

        -- si hay por lo menos un registro
         IF ( v_i_cuenta_regs <= 0 ) THEN
            -- se le indica al usuario que no hay datos para generar el archivo
            CALL fn_mensaje("Archivo de salida","No se tienen datos para generar el archivo","stop")

            CONTINUE MENU
         END IF

         DISPLAY v_i_cuenta_regs

         -- se crea la sentencia sql que ejecuta la funcion que genera el pid
         LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)

         -- se invoca la funcion que inicializa el proceso
         LET r_b_valida = fn_inicializa_proceso(v_d_pid,
                                                v_i_proceso_cod,
                                                v_i_opera_cod,
                                                v_d_folio,
                                                v_c_programa_cod,
                                                v_v_nom_archivo,
                                                p_v_usuario)

         -- se verifica si fue posible inicializar el proceso
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT MENU
         END IF

         -- se invoca la función que deja la operación en estado Procesando
         LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                                 v_i_proceso_cod,
                                                 v_i_opera_cod,
                                                 v_d_folio,
                                                 v_c_programa_cod,
                                                 v_v_nom_archivo,
                                                 p_v_usuario)

         -- se verifica si fue posible inicializar la operacion
         IF r_b_valida <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)

            EXIT MENU
         END IF

         -- se crea el comando que ejecuta el modulo que genera el archivo de salida de amortización
         LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/AGRS08 ",
                                                 p_v_usuario, " ",
                                                 v_d_pid, " ",
                                                 v_i_proceso_cod, " ",
                                                 v_i_opera_cod, " ",
                                                 v_d_folio, " ",
                                                 v_v_nom_archivo, " 1> ",
                                                 v_c_ruta_list_bat CLIPPED,
                                                 "/nohup:",v_d_pid USING "&&&&&",":",
                                                 v_i_proceso_cod USING "&&&&&",":",
                                                 v_i_opera_cod USING "&&&&&",
                                                 " 2>&1 &"

         -- se ejecuta el comando armado
         RUN v_s_comando
         
         -- se asigna el mensaje a mostrar al usuario
         LET v_s_mensaje = "Se ha enviado la ejecución de la extracción con PID: ",v_d_pid CLIPPED,
                           ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
         CALL fn_mensaje("Aviso",v_s_mensaje,"information")

         EXIT MENU

      ON ACTION CANCEL
         EXIT MENU
   END MENU
   CLOSE WINDOW w_genera_archAmortizacion
END MAIN