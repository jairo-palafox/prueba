--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRL21                                        #
#Objetivo          =>Programa que pide al usuario la seleccion del #
#                    proceso a conciliar y lanza el proceso de     #
#                    conciliación correspondiente                  #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>25 ENERO 2012                                 #
####################################################################
DATABASE safre_viv
GLOBALS "ACRG10.4gl"

MAIN
   DEFINE p_v_nom_prog        VARCHAR(30), -- nombre del programa
          p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
          v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_d_folio           LIKE bat_ctr_operacion.folio, -- folio
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          v_fech_concilia     DATE, -- Fecha de conciliacion
          v_cmb_operacion     CHAR(02), -- combo de las operaciones
          v_s_mensaje         STRING, -- mensaje a mostrar al usuario
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   # Se asignan los parámetros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   # se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".ACRL21.log")

   # Se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   # Se inializan variables
   LET v_i_proceso_cod = g_proc_cod_acr_conciliacion -- Conciliación acr
   LET v_i_opera_cod = 1 -- Conciliación acr
   LET v_d_pid = 0
   LET v_d_folio = 0
   LET v_v_nom_archivo = "N/A"
   LET v_c_programa_cod = "ACRL21"
   LET v_fech_concilia = TODAY

   # Se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   # Se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   # Se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   OPEN WINDOW w_genera_conciliacion WITH FORM "ACRL211"
   INPUT v_cmb_operacion WITHOUT DEFAULTS FROM cmb_opera ATTRIBUTES(UNBUFFERED)
      BEFORE INPUT
         -- se invoca la función que llena el combo
         CALL fn_llena_combo("cmb_opera")

         -- se muestra de fecha de la conciliación
         DISPLAY v_fech_concilia TO f_concilia

      # Se llama a la funcion que genera el archivo
      ON ACTION ACCEPT
         # Se valida la operacion seleccionada
         IF v_cmb_operacion IS NULL THEN
            CALL fn_mensaje("Conciliación","Debe seleccionar una operación a conciliar","stop")
            CONTINUE INPUT
         END IF

         -- se invoca la funcion que genera el PID
         CALL fn_genera_pid(v_i_proceso_cod,v_i_opera_cod,p_v_usuario) RETURNING v_d_pid 

         # Se invoca la funcion que inicializa el proceso
         LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                                v_d_folio, v_c_programa_cod,
                                                v_v_nom_archivo, p_v_usuario)

         IF r_b_valida = 0 THEN
            # Actualiza la operacion a inicializada
            CALL fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod, v_d_folio,
                                        v_c_programa_cod,"NA",p_v_usuario) 
                                        RETURNING r_b_valida

            IF r_b_valida = 0 THEN
               # Construye la cadena para ejecutar el comando
               LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/"

               CASE v_cmb_operacion
                  WHEN "01" -- Rechazo de saldos
                     LET v_s_comando = v_s_comando,"ACRP18 "

                  WHEN "06" -- Devolución solicitada
                     LET v_s_comando = v_s_comando,"ACRP27 "

                  WHEN "09" -- Saldos transferidos
                     LET v_s_comando = v_s_comando,"ACRP26 "

                  WHEN "14" -- Solicitud no atendida
                     LET v_s_comando = v_s_comando,"ACRP28 "

                  OTHERWISE
                     # Muestra actualiza error
                     CALL fn_mensaje("Error","Opción no reconocida","stop")

                     CALL fn_error_opera(v_d_pid, v_i_proceso_cod, v_i_opera_cod) 
                                         RETURNING r_b_valida

                     IF r_b_valida <> 0 THEN
                        # En caso de error se muestra un mensaje a usuario y no continua
                        CALL fn_muestra_inc_operacion(r_b_valida)
                     END IF

                     EXIT INPUT
               END CASE

               # Se crea el comando que ejecuta el modulo que genera la concliación
               LET v_s_comando = v_s_comando,
                                 p_v_usuario, " ",
                                 v_d_pid, " ",
                                 v_i_proceso_cod, " ",
                                 v_i_opera_cod, " ",
                                 v_d_folio, " ",
                                 v_v_nom_archivo,
                                 " 1> ",v_c_ruta_list_bat CLIPPED,
                                 "/nohup:",v_d_pid USING "&&&&&",":",
                                 v_i_proceso_cod USING "&&&&&",":",
                                 v_i_opera_cod USING "&&&&&",
                                 " 2>&1 &"

               --DISPLAY v_s_comando
               RUN v_s_comando

               -- se asigna el mensaje a mostrar al usuario
               LET v_s_mensaje = "Se ha enviado la conciliación con PID: ",v_d_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
               CALL fn_mensaje("Conciliación",v_s_mensaje,"information")
            ELSE
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)

               -- se actializa la operación como ERRONEA
               CALL fn_error_opera(v_d_pid, v_i_proceso_cod, v_i_opera_cod) RETURNING r_b_valida
                                     
               IF r_b_valida <> 0 THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)

                  EXIT PROGRAM
               END IF
               EXIT PROGRAM
            END IF
         ELSE
            # En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)
            --EXIT PROGRAM
         END IF
         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_genera_conciliacion
END MAIN

#Objetivo: Genera el ComboBox operaciones
FUNCTION fn_llena_combo(v_v_nombre_combo)               
DEFINE v_v_nombre_combo VARCHAR(20),   -- nombre del combobox
       cb               ui.ComboBox # Variable de Combobox

   LET cb = ui.ComboBox.forName(v_v_nombre_combo) #Asignación del combo a la forma

   # Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   # Limpia el combo
   CALL cb.clear()

   # Opciones del combo
   CALL cb.addItem("01","01 - Rechazo de saldos")
   CALL cb.addItem("06","06 - Devolución de saldos")
   CALL cb.addItem("09","09 - Saldos transferidos")
   CALL cb.addItem("14","14 - Solicitud no atendida")
        
END FUNCTION

