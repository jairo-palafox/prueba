--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

##########################################################################
#MÓdulo             => ACR                                               #
#Programa           => ACRL30                                            #
#Objetivo           => Programa que efectúa el archivo para Solicitud    #
#                      de Devoluciones para el módulo de TA              #
#Fecha inicio       => 23 de enero de 2011                               #
#Actualización      => Fecha de envío cambia a 12 día hábil              #
#Fecha actualización => 29 de octubre de 2014                            #
#Modifica:          => Eduardo Ventura Bonola                            #
#Fecha modif:       => 12 de noviembre de 2015                           #
#Adecuación         => Generación archivo "agrupación de la agrupación"  #
#                      y registros sin afectación a la cuenta individual #
#Modifica:          => Mauro Muñiz Caballero                             #
#Fecha modif:       => 13 de noviembre de 2016                           #
#Adecuación         => Eliminación de adelantos                          #
##########################################################################

DATABASE safre_viv

GLOBALS "ACRG10.4gl"

   DEFINE g_usuario_cod             LIKE seg_usuario.usuario_cod
   DEFINE g_pid                     LIKE bat_ctr_proceso.pid --ID del proceso
   DEFINE g_proceso_cod             LIKE cat_proceso.proceso_cod --codigo del proceso
   DEFINE g_opera_cod               LIKE cat_operacion.opera_cod --codigo de operacion
   DEFINE g_num_folio               DECIMAL(9) -- numero de folio del proceso
   DEFINE g_nom_archivo             LIKE bat_ctr_proceso.nom_archivo -- nombre del archivo
   DEFINE p_tpo_ejecucion           INTEGER
   DEFINE p_v_nom_prog              STRING

MAIN

   -- Se recuperan los parametros recibidos
   LET g_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_v_nom_prog    = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (g_usuario_cod CLIPPED|| ".ACRL30.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan variables
   LET g_proceso_cod = g_proc_cod_acr_arch_solic_dse -- generación solicitud devolución sdo exc acr
   LET g_opera_cod   = 1
   LET g_pid         = 0
   LET g_num_folio   = 0

   --Se ejecuta la funcion para genera la parte visual del menu o continuar en batch segun el argumento recibido
   CALL fn_ini_solcitud_devolucion()

END MAIN

#Objetivo: Verifica si se esta ejecutando el proceso en batch o en linea para
#          mandar llamar la funcion correspondiente.
FUNCTION fn_ini_solcitud_devolucion()

   DEFINE v_c_tpo_transf_dse        LIKE dse_agrupa_devolucion.tpo_transferencia
   DEFINE v_c_tpo_transf_sol        LIKE dse_agrupa_devolucion.tpo_transferencia
   DEFINE v_i_num_regs              INTEGER -- contador de registros
   DEFINE v_dt_f_presentacion       DATE --Fecha de presentacion
   DEFINE v_dt_f_movimiento         DATE --Fecha de movimiento (14 dia habil del mes)
   DEFINE v_f_fecha_archivo         DATE -- fecha para nomenclatura de archivo
   DEFINE v_dt_f_solic_saldos       DATE -- fecha para la generación de solicitud de saldos
   DEFINE v_i_lote                  LIKE dse_ctr_archivo.lote -- lote del archivo
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_c_lote                  CHAR(1) -- lote del archivo
   DEFINE v_estatus                 SMALLINT
   DEFINE v_s_mensaje               STRING -- mensaje a mostrar al usuario
   DEFINE v_s_qryTxt                STRING -- contiene sentencia sql a ejecutar
   DEFINE v_cuenta_accion           SMALLINT --variable que guarda si existe valor de una accion

   -- se inicilizan variables
   LET v_f_fecha_archivo = TODAY
   LET v_c_tpo_transf_dse = "15" -- Tipo de transferencia de DSE
   LET v_c_tpo_transf_sol = "51" -- Tipo de transferencia de Solicitud de DSE

   -- se obtiene el catorceavo dia habil del mes actual
   LET v_dt_f_presentacion = v_f_fecha_archivo - DAY(v_f_fecha_archivo) + 1

   PREPARE prp_obtiene_habil FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_dt_f_presentacion||"',11)"
   EXECUTE prp_obtiene_habil INTO v_dt_f_presentacion

  {
   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_dt_f_presentacion < v_f_fecha_archivo THEN
      -- se obtiene el décimosegundo día hábil del mes siguiente
      LET v_dt_f_presentacion = v_f_fecha_archivo - DAY(v_f_fecha_archivo) + 1
      LET v_dt_f_presentacion = v_dt_f_presentacion + 1 UNITS MONTH

      PREPARE prp_obtiene_habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_dt_f_presentacion||"',11)"
      EXECUTE prp_obtiene_habil_sig INTO v_dt_f_presentacion
   END IF
  }

   -- Se contruye la fecha movimiento: Primer dia natural del mes siguiente a la fecha de presentación
   LET v_dt_f_movimiento = v_dt_f_presentacion - DAY(v_dt_f_presentacion) + 1
   LET v_dt_f_movimiento = v_dt_f_movimiento + 1 UNITS MONTH

   -- se obtiene el maximo lote para la fecha de presentación
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM dse_ctr_archivo\n",
                    "  WHERE tpo_transferencia = '",v_c_tpo_transf_sol,"'\n",
                    "    AND f_lote = '",v_dt_f_presentacion,"'"

   PREPARE prp_max_lote FROM v_s_qryTxt
   EXECUTE prp_max_lote INTO v_i_lote

   -- se valida el lote
   IF v_i_lote IS NULL THEN
      LET v_i_lote = 1
   ELSE
      LET v_i_lote = v_i_lote + 1
   END IF

   -- se asigna el lote en variable CHAR(1)
   LET v_c_lote = v_i_lote

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se asigna el nombre del archivo
   LET g_nom_archivo = "sol_dev", v_f_fecha_archivo USING "yyyymmdd", v_c_lote, ".", v_c_extension CLIPPED

   -- se verifica si ya se terminó el proceso anterior, si no,
   -- no se permite generar una nueva solicitud de devolucion
   LET v_estatus = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)

   CLOSE WINDOW screen

   IF ( v_estatus = 0 ) THEN
      --Se ejecuta en linea el proceso
      OPEN WINDOW w_solicitud_devolucion WITH FORM "ACRL301"
         CLEAR FORM

         MENU
            BEFORE MENU
               DISPLAY v_dt_f_presentacion USING "DD-MM-YYYY" TO dt_presentacion
               DISPLAY v_dt_f_movimiento USING "DD-MM-YYYY" TO dt_movimiento
            COMMAND "Editar Fecha"
               INPUT v_dt_f_presentacion,v_dt_f_movimiento FROM dt_presentacion,dt_movimiento ATTRIBUTES(UNBUFFERED)
                  ON ACTION ACCEPT
                     EXIT INPUT
                  ON ACTION CANCEL
                     DISPLAY v_dt_f_presentacion USING "DD-MM-YYYY" TO dt_presentacion
                     DISPLAY v_dt_f_movimiento USING "DD-MM-YYYY" TO dt_movimiento
                     EXIT INPUT
               END INPUT

            COMMAND "Generar"
               -- valida si existe precio de fondo de inversión
               LET v_s_qryTxt = " SELECT COUNT(*)\n",
                                "   FROM glo_valor_fondo\n",
                                "  WHERE fondo = 11\n",
                                "    AND f_valuacion ='",v_dt_f_movimiento,"'"

               PREPARE prp_glo_val_fondo FROM v_s_qryTxt
               EXECUTE prp_glo_val_fondo INTO v_cuenta_accion

               -- se verifica si se encontró precio de fondo para el día de hoy
               IF v_cuenta_accion = 0 THEN
                  LET v_s_mensaje = "No es posible generar el archivo, ya que no hay precio\n",
                                    "de fondo para la fecha de movimiento"
                  CALL fn_mensaje("Aviso",v_s_mensaje,"stop")
                  EXIT MENU
               END IF

               -- se obtiene la fecha del ultimo día del mes anterior de la fecha de presentación
               LET v_dt_f_solic_saldos = v_dt_f_presentacion - DAY(v_dt_f_presentacion)

               --Se inicia el cursos de la tabla temporal
               LET v_s_qryTxt = " SELECT COUNT(*)\n",
                                "   FROM dse_agrupa_devolucion\n",
                                "  WHERE tpo_transferencia = '",v_c_tpo_transf_dse,"'\n",
                                "    AND estado IN(20, 140, 142)\n",
                                "    AND edo_procesar IN (20,70)"

               PREPARE prp_rep_selicitud_pago FROM v_s_qryTxt
               EXECUTE prp_rep_selicitud_pago INTO v_i_num_regs

               IF v_i_num_regs = 0 THEN
                  -- se indica que no hay registros a procesar y no continua
                  LET v_s_mensaje = "No se encontró información a procesar, no puede continuar con la operación"
                  CALL fn_mensaje("Solicitud de Devolución",v_s_mensaje,"information")
                  CONTINUE MENU
               END IF

               --Se obtiene el pid
               CALL fn_genera_pid(g_proceso_cod, g_opera_cod,g_usuario_cod) RETURNING g_pid

               --Se da de alta el proceso en el monitor
               CALL fn_inicializa_proceso(g_pid,
                                          g_proceso_cod,
                                          g_opera_cod,
                                          g_num_folio,
                                          "ACRL30",
                                          g_nom_archivo,
                                          g_usuario_cod)
                                RETURNING v_estatus

               IF v_estatus <> 0 THEN
                  -- Si no se peude ejecutar el proceso se indica la razon
                  CALL fn_muestra_inc_operacion(v_estatus)

                  EXIT MENU
               END IF

               -- se marca el proceso como inciado
               CALL fn_actualiza_opera_ini(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_num_folio,
                                           "ACRL30",
                                           g_nom_archivo,
                                           g_usuario_cod)
                                           RETURNING v_estatus

               IF v_estatus <> 0 THEN
                  -- Si no se puede ejecutar el proceso se indica la razon
                  CALL fn_muestra_inc_operacion(v_estatus)

                  EXIT MENU
               END IF

               --Se inicia el proceso de devolucion
               CALL fn_acr_gen_solicitud_devolucion()

               -- se asigna el mensaje a mostrar al usuario
               LET v_s_mensaje = "Se ha enviado la Solicitud de Devolución con PID: ",g_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
               CALL fn_mensaje("Solicitud de Devolución",v_s_mensaje,"information")

               EXIT MENU

            COMMAND "Cancelar"
               EXIT MENU
         END MENU

      CLOSE WINDOW w_solicitud_devolucion
   ELSE
      --Si no se peude ejecutar el proceso se indica la razón
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF

END FUNCTION

#Objetivo: Funcion que ejecuta el store procedure para la generar la tabla
#          temporal  de las solicutudes de devolucion
FUNCTION fn_acr_gen_solicitud_devolucion()

   DEFINE v_comando                 STRING
   DEFINE v_ruta_ejecutable         STRING
   DEFINE v_ruta_listados           STRING
   DEFINE v_cadena_pid              STRING
   DEFINE v_cadena_proc             STRING
   DEFINE v_cadena_opera            STRING
   DEFINE v_ruta_vacia              STRING

   -- obtenemos las rutas
   CALL fn_rutas("acr") RETURNING v_ruta_ejecutable, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia     , v_ruta_listados

   -- se crean las cadenas para el nombre del archivo log
   LET v_cadena_pid   = g_pid USING "&&&&&"
   LET v_cadena_proc  = g_proceso_cod USING "&&&&&"
   LET v_cadena_opera = g_opera_cod USING "&&&&&" 

   --Se invoca el store procedure que almacena la información en las tablas históricas
   LET v_comando = " nohup time fglrun ",v_ruta_ejecutable CLIPPED,"/ACRS14 ",
                                         g_usuario_cod, " ",
                                         g_pid, " ",
                                         g_proceso_cod," ",
                                         g_opera_cod," ",
                                         g_num_folio, " ",
                                         g_nom_archivo," 1>",
                                         v_ruta_listados CLIPPED,
                                         "/nohup:",v_cadena_pid,":",
                                          v_cadena_proc,":",
                                         v_cadena_opera,
                                         " 2>&1 &"

   DISPLAY "Comando:\n", v_comando

   RUN v_comando

END FUNCTION
