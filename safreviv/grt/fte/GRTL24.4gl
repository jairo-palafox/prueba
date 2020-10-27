--===============================================================
-- Versi�n: 1.0.0
-- Fecha �ltima modificaci�n:
--===============================================================

#####################################################################
#M�dulo            => GRT                                           #
#Programa          => GRTL24                                        #
#Objetivo          => Programa que permite la integraci�n del       #
#                     archivo de entrada Solicitudes no Atendidas   #
#                     para el m�dulo de Uso en Garant�a 43 bis      #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 26 Abril  2012                                #
#Modifica:         => Mauro Mu�iz Caballero                         #
#Fecha modif:      => 23 de marzo de 2016                           #
#Adecuaci�n        => Eliminaci�n de adelantos                      #
#####################################################################


DATABASE safre_viv

GLOBALS "GRTG01.4gl"

#Objetivo: Proceso que permite la ejecutaci�n del proceso de integracion de la informacion
#  del archivo de entrada Solicitudes no atendidas en la base de datos de historicos (his transferencia)
MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario,-- usuario firmado al sistema
   DEFINE p_b_tipo_carga            SMALLINT -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_nom_prog              VARCHAR(30) -- nombre del programa
   DEFINE v_arr_arch_pend           DYNAMIC ARRAY OF VARCHAR(100) -- arreglo que contiene los archivos pendientes
   DEFINE v_arr_arch_int            DYNAMIC ARRAY OF VARCHAR(100) -- arreglo que contiene los archivos a integrar

   DEFINE v_arr_integrados DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos integrados
             nom_archivo            LIKE cre_ctr_archivo.nom_archivo,
             fecha_trans            LIKE cre_ctr_archivo.f_lote,
             lote                   LIKE cre_ctr_archivo.lote,
             fecha_proceso          LIKE cre_ctr_archivo.f_proceso,
             tot_registros          LIKE cre_ctr_archivo.tot_registros,
             id_cre_ctr_archivo     LIKE cre_ctr_archivo.id_cre_ctr_archivo,
             folio                  LIKE cre_ctr_archivo.folio_archivo
   END RECORD

   DEFINE v_r_cre_ctr_arch          RECORD LIKE cre_ctr_archivo.* -- registro del control de archivo
   DEFINE v_v_nomArch_proc          VARCHAR(100) -- nombre del archivo en proceso
   DEFINE v_ui_dnd                  UI.DRAGDROP -- manejador del arrastrar y soltar (drag and drop)
   DEFINE v_drag_index              INTEGER -- indice del drag
   DEFINE v_drop_index              INTEGER -- indice del drop
   DEFINE v_drag_source             STRING -- fuente del drag
   DEFINE v_drag_value              STRING -- valor del drag
   DEFINE v_i_num_arch              SMALLINT -- numero de archivos a integrar
   DEFINE v_i_iter                  SMALLINT -- variable usada para iteracion
   DEFINE v_i_indice                SMALLINT -- indice del arrego de archivos pendientes
   DEFINE v_i_proceso_cod           LIKE cat_proceso.proceso_cod -- proceso que llama las funciones
   DEFINE v_i_opera_cod_ant         LIKE cat_operacion.opera_cod -- operaci�n de la etapa anterior
   DEFINE v_i_opera_cod             LIKE cat_operacion.opera_cod -- operaci�n de la etapa que llama la funcion
   DEFINE v_i_operacion             LIKE cre_ctr_archivo.operacion -- operacion del proceso
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso -- id proceso
   DEFINE v_d_folio                 LIKE glo_ctr_archivo.folio -- folio
   DEFINE v_d_pid                   LIKE bat_ctr_proceso.pid -- identificador del proceso
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin del m�dulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados del m�dulo
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_c_programa_cod          LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_s_cadena                STRING -- se asigna un mensaje que ser� presentado al usuario
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_s_mensaje               STRING -- mensaje a mostrar al usuario
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   CONSTANT l_nom_tbl_pend = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
   CONSTANT l_nom_tbl_int  = "tbl_integrar" -- se asigna el nombre de la tabla de archivos a integrar

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".GRTL24.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se asignan los valores necesarios para la intergraci�n
   LET v_i_proceso_cod   = g_proc_cod_grt_uso_no_atendidas -- recepci�n solic no atendidas uso 43bis
   LET v_i_opera_cod     = 2 -- integra solicitudes no atendidas
   LET v_i_opera_cod_ant = 1 -- Valida Solicitudes No Atendidas
   LET v_i_operacion     = 14 -- operacion del proceso
   LET v_si_id_proceso   = g_id_proceso_grt_uso -- Uso de Garant�a 43 bis
   LET v_d_folio         = 0
   LET v_c_programa_cod  = "GRTL24"
   LET v_i_indice        = 1 -- indice del arreglo

   -- se crea la sentencia sql que obtiene el pid perteneciente al folio
   LET v_s_qryTxt = " SELECT MAX(pid)\n",
                    "   FROM bat_ctr_proceso\n",
                    "  WHERE proceso_cod = ",v_i_proceso_cod

   PREPARE prp_unq_pid_batCtrProc FROM v_s_qryTxt
   EXECUTE prp_unq_pid_batCtrProc INTO v_d_pid

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la ruta bin y de listados del m�dulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat2 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat2 INTO v_c_ruta_list_bat

   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET v_s_qryTxt = " SELECT nombre_archivo\n",
                    "   FROM glo_ctr_archivo\n",
                    "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                    "    AND opera_cod = ",v_i_opera_cod_ant,"\n",
                    "    AND estado = 1" -- cargado

   PREPARE prp_archivos_val FROM v_s_qryTxt
   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 

   FOREACH cur_archivos_val INTO v_arr_arch_pend[v_i_indice]
      -- se incrementa el indice del arreglo
      LET v_i_indice = v_i_indice + 1
   END FOREACH

   -- se borra el ultimo registro para quitar el elemento nulo que deja el foreach
   CALL v_arr_arch_pend.deleteElement(v_i_indice)

   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW w_integra_arch_noat WITH FORM "GRTL021"
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_arr_arch_pend TO tbl_pendientes.*
            ON DRAG_START(v_ui_dnd)
               DISPLAY "v_ui_dnd: ",v_ui_dnd
               LET v_drag_source = l_nom_tbl_pend
               DISPLAY "v_drag_source = ",v_drag_source
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_pend[v_drag_index]

            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            ON DROP(v_ui_dnd)
               IF v_drag_source == l_nom_tbl_pend THEN
                  CALL v_ui_dnd.dropInternal()
               ELSE
                  LET v_drop_index = v_ui_dnd.getLocationRow()
                  CALL DIALOG.insertRow(l_nom_tbl_pend, v_drop_index)
                  CALL DIALOG.setCurrentRow(l_nom_tbl_pend, v_drop_index)
                  LET v_arr_arch_pend[v_drop_index] = v_drag_value
                  CALL DIALOG.deleteRow(l_nom_tbl_int, v_drag_index)
               END IF
         END DISPLAY

         DISPLAY ARRAY v_arr_arch_int TO tbl_integrar.*
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = l_nom_tbl_int
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_int[v_drag_index]

            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            ON DROP(v_ui_dnd)
                IF v_drag_source == l_nom_tbl_int THEN
                    CALL v_ui_dnd.dropInternal()
                ELSE
                    LET v_drop_index = v_ui_dnd.getLocationRow()
                    CALL DIALOG.insertRow(l_nom_tbl_int, v_drop_index)
                    CALL DIALOG.setCurrentRow(l_nom_tbl_int, v_drop_index)
                    LET v_arr_arch_int[v_drop_index] = v_drag_value
                    CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                END IF
         END DISPLAY

         DISPLAY ARRAY v_arr_integrados TO tbl_integrados.*
         END DISPLAY

         BEFORE DIALOG
            -- se oculta el bot�n de Integrar
            CALL DIALOG.setActionHidden("integrar",1)

         ON ACTION ACCEPT
            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_arch_int.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena = "Debe arrastrar al menos un archivo a integrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")

               CONTINUE DIALOG
            END IF

            -- se limpia el arreglo de los archivos ya integrados
            CALL v_arr_integrados.clear()

            -- se procesan los archivos seleccionados para integrar
            FOR v_i_iter = 1 TO v_i_num_arch
               -- se asigna el nombre del archivo en la variable parametro
               LET v_v_nomArch_proc = v_arr_arch_int[v_i_iter]

               -- se realiza la consulta a la informacion del archivo
               LET v_s_qryTxt = " SELECT *\n",
                                "   FROM cre_ctr_archivo\n",
                                "  WHERE id_proceso = ",v_si_id_proceso,"\n",
                                "    AND operacion = ",v_i_operacion,"\n",
                                "    AND estado = 10\n", -- cargado
                                "    AND nom_archivo = '",v_v_nomArch_proc CLIPPED,"'"

                PREPARE prp_info_archivo FROM v_s_qryTxt
                EXECUTE prp_info_archivo INTO v_r_cre_ctr_arch.*

                -- se valida la informaci�n del archivo consultado
                IF v_r_cre_ctr_arch.id_cre_ctr_archivo IS NULL THEN
                   CALL fn_mensaje("Integraci�n No Atendidas","El archivo est� marcado como Rechazado, no se puede continuar\ncon la integraci�n del archivo","stop")
                   CONTINUE DIALOG
                END IF

                -- se asigna la informacion del archivo integrado
                LET v_arr_integrados[v_i_iter].nom_archivo = v_r_cre_ctr_arch.nom_archivo
                LET v_arr_integrados[v_i_iter].fecha_trans = v_r_cre_ctr_arch.f_lote
                LET v_arr_integrados[v_i_iter].lote = v_r_cre_ctr_arch.lote
                LET v_arr_integrados[v_i_iter].fecha_proceso = v_r_cre_ctr_arch.f_proceso
                LET v_arr_integrados[v_i_iter].tot_registros = v_r_cre_ctr_arch.tot_registros
                LET v_arr_integrados[v_i_iter].id_cre_ctr_archivo = v_r_cre_ctr_arch.id_cre_ctr_archivo
                LET v_arr_integrados[v_i_iter].folio = v_r_cre_ctr_arch.folio_archivo
            END FOR

            -- si se muestra la informacion del archivo a integrar se habilita el boton de integrar
            IF v_arr_integrados.getLength() > 0 THEN
               -- Se muestra la opcion del reporte
               CALL DIALOG.setActionHidden("integrar",0)
               CALL DIALOG.setActionHidden("accept",1)
            END IF

            CONTINUE DIALOG

         ON ACTION integrar
            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_integrados.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena =  "No se han seleccionado archivos a intergrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")

               CONTINUE DIALOG
            END IF

            -- se invoca la funci�n a valida la informaci�n antes de integrar
            CALL f_valida_arch_integra() RETURNING r_b_valida

            -- se verifica si se encontraron registro que no se encuentran en la Solicitud de Saldos
            IF r_b_valida = 1 THEN
               -- se informa que no existen los registros en la solicitud de saldo. No continua
               LET v_s_cadena = "Se encontraron registros sin originaci�n de cr�dito. Si contin�a con la integraci�n\n",
                                "estos registros ser� marcados como Rechazados. �Desea continuar?"
               CALL fn_ventana_confirma("Integraci�n", v_s_cadena, "question") RETURNING r_b_valida

               IF NOT r_b_valida THEN
                  CONTINUE DIALOG
               END IF
            ELSE
               -- se verifica si se encontraron registro con estado diferente al requerido
               IF r_b_valida = 2 THEN
                  -- se informa que no existen los registros en la solicitud de saldo. No continua
                  LET v_s_cadena = "Se encontraron registros con estados diferente al requerido. Si contin�a con la\n",
                                   "integraci�n estos registros ser� marcados como Rechazados. �Desea continuar?"
                  CALL fn_ventana_confirma("Integraci�n", v_s_cadena, "question") RETURNING r_b_valida

                  IF NOT r_b_valida THEN
                     CONTINUE DIALOG
                  END IF
               END IF
            END IF

            -- se procesan los archivos seleccionados para integrar
            FOR v_i_iter = 1 TO v_i_num_arch
               -- se asigna el nombre del archivo en la variable
               LET v_v_nomArch_proc = v_arr_integrados[v_i_iter].nom_archivo

               -- se invoca la funci�n que deja la operaci�n en estado Procesando
               LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,v_i_proceso_cod,v_i_opera_cod,
                                                       v_d_folio, v_c_programa_cod,
                                                       v_v_nomArch_proc, p_v_usuario)

               -- se verifica si fue posible inicializar la operacion
               IF r_b_valida = 0 THEN
                  -- se crea el comando que ejecuta el m�dulo que reliza la integraci�n del archivo
                  LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/GRTE11 ",
                                    p_v_usuario, " ",
                                    v_d_pid, " ",
                                    v_i_proceso_cod, " ",
                                    v_i_opera_cod, " ",
                                    v_d_folio, " ",
                                    v_v_nomArch_proc, " 1> ",
                                    v_c_ruta_list_bat CLIPPED,
                                    "/nohup:",v_d_pid USING "&&&&&",":",
                                    v_i_proceso_cod USING "&&&&&",":",
                                    v_i_opera_cod USING "&&&&&",
                                    " 2>&1 &"

                  --DISPLAY v_s_comando
                  RUN v_s_comando
               ELSE
                  -- en caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)

                  EXIT PROGRAM
               END IF
            END FOR

            -- se asigna el mensaje a mostrar al usuario
            LET v_s_mensaje = "Se ha enviado la integraci�n con PID: ",v_d_pid CLIPPED,
                              ".\nPuede revisar el avance del proceso en el monitor de ejecuci�n de procesos"
            CALL fn_mensaje("Integraci�n",v_s_mensaje,"information")

            -- se limpia el arreglo que contiene los archivos a integrar
            CALL v_arr_arch_int.clear()

            EXIT DIALOG

         ON ACTION cancelar
            EXIT DIALOG
      END DIALOG

   CLOSE WINDOW w_integra_arch_noat

END MAIN

# Objetivo: Funci�n que valida la informaci�n a integrar. Regresa alguno de los siguientes estatus:
-- 0.- Informaci�n a integrar correcta
-- 1.- Advertencia: Existen registros sin originaci�n de credito
-- 2.- Advertencia: Existen registros con estados no correcto
FUNCTION f_valida_arch_integra()

   DEFINE v_si_tpo_originac         LIKE cre_acreditado.tpo_originacion -- tipo de originaci�n
   DEFINE v_d_id_derechohab         LIKE afi_derechohabiente.id_derechohabiente -- identificador del derechohabiente
   DEFINE v_id_cre_uso_garantia     DECIMAL(9,0)
   DEFINE v_estado                  SMALLINT --  estado del registro
   DEFINE v_c_nss_tmp               CHAR(11) -- nss infonavit de la temporal
   DEFINE v_periodo_pago            CHAR(6)  --periodo pago
   DEFINE v_c_tpo_transf            CHAR(2) -- tipo de transferencia
   DEFINE v_s_qryTxt                STRING -- contiene una sentencia sql a ejecutar
   DEFINE v_si_sts_retorno          SMALLINT -- status que indica si la integraci�n procede
   DEFINE v_b_exist_orig            SMALLINT -- booleana que indica si existe originaci�n de cr�dito o no
   DEFINE v_b_exist_edo             SMALLINT -- booleana que indica si el cr�dito est� en estado requerido o no

   -- se inicializan variables
   LET v_si_tpo_originac = 2 -- Cr�ditos en Garant�a

   -- se procesan los registros de la temporal
   LET v_s_qryTxt = " SELECT nss, tpo_transferencia, per_pago\n",
                    "   FROM safre_tmp:tmp_no_atend_det_uso"

   PREPARE prp_tmp_nss FROM v_s_qryTxt
   DECLARE cur_tmp_nss CURSOR FOR prp_tmp_nss

   FOREACH cur_tmp_nss INTO v_c_nss_tmp, v_c_tpo_transf, v_periodo_pago
      -- se inicializan variables
      LET v_b_exist_orig = FALSE -- se asume que no existir� Originaci�n de Cr�dito
      LET v_b_exist_edo = FALSE -- se asume que los estados no ser�n correctos

      -- se obtiene el id derechohabiente para el nss en proceso
      LET v_s_qryTxt = " SELECT id_derechohabiente\n",
                       "   FROM afi_derechohabiente\n",
                       "  WHERE nss = '",v_c_nss_tmp CLIPPED,"'"

      PREPARE prp_id_derechohab FROM v_s_qryTxt
      EXECUTE prp_id_derechohab INTO v_d_id_derechohab

      -- verifica si el registro corresponde a Solicitud de Saldo en Garant�a
      IF v_c_tpo_transf = "16" THEN
         DISPLAY "Es 16"
         -- se busca si el id derechohabiente est� en estatus requerido
         LET v_s_qryTxt = " SELECT estado\n",
                          "   FROM cre_acreditado\n",
                          "  WHERE edo_procesar < 120\n",
                          "    AND id_derechohabiente = ",v_d_id_derechohab,"\n",
                          "    AND tpo_originacion = ",v_si_tpo_originac

         PREPARE prp_cont_regs FROM v_s_qryTxt
         DECLARE cur_valida_edo CURSOR FOR prp_cont_regs

         FOREACH cur_valida_edo INTO v_estado 
            -- se indica que s� existi� Originaci�n de Cr�dito para el registro en proceso
            LET v_b_exist_orig = TRUE

            -- se valida el estado obtenido
            IF v_estado IS NOT NULL AND v_estado >= 20 THEN
               -- se indica que el estado es el correcto
               LET v_b_exist_edo = TRUE
            ELSE
               -- se indica que el estado no es el correcto
               LET v_b_exist_edo = FALSE

               EXIT FOREACH
            END IF
         END FOREACH
      ELSE
         -- verifica si el registro corresponde a Uso de Garant�a
         IF v_c_tpo_transf = "18" OR v_c_tpo_transf = "48" THEN
            LET v_s_qryTxt = " SELECT FIRST 1 id_cre_uso_garantia\n",
                             "   FROM safre_tmp:tmp_uso_solic_sdo2\n",
                             "  WHERE nss = '",v_c_nss_tmp CLIPPED,"'",
                             "    AND periodo_pago = '",v_periodo_pago CLIPPED,"'"

            PREPARE prp_id_cre_ug FROM v_s_qryTxt
            EXECUTE prp_id_cre_ug INTO v_id_cre_uso_garantia

            -- se busca si el id derechohabiente est� en estatus requerido
            LET v_s_qryTxt = " SELECT estado\n",
                             "   FROM cre_uso_garantia\n",
                             "  WHERE id_cre_uso_garantia = ",v_id_cre_uso_garantia

            PREPARE prp_cont_regs_uso FROM v_s_qryTxt
            DECLARE cur_valida_edo_uso CURSOR FOR prp_cont_regs_uso

            FOREACH cur_valida_edo_uso INTO v_estado
               -- se indica que s� existi� Originaci�n de Cr�dito para el registro en proceso
               LET v_b_exist_orig = TRUE

               -- se valida el estado obtenido
               IF v_estado IS NOT NULL AND v_estado >= 140 THEN
                  -- se indica que el estado es el correcto
                  LET v_b_exist_edo = TRUE
               ELSE
                  -- se indica que el estado no es el correcto
                  LET v_b_exist_edo = FALSE

                  EXIT FOREACH
               END IF
            END FOREACH
         END IF
      END IF

      -- se validan las banderas para asignar el valor de retorno
      IF NOT v_b_exist_orig OR NOT v_b_exist_edo THEN
         EXIT FOREACH
      END IF
   END FOREACH

   -- se validan las banderas para asignar el valor de retorno
   IF NOT v_b_exist_orig THEN
      LET v_si_sts_retorno = 1
   ELSE
      IF NOT v_b_exist_edo THEN
         LET v_si_sts_retorno = 2
      ELSE
         LET v_si_sts_retorno = 0
      END IF
   END IF

   RETURN v_si_sts_retorno

END FUNCTION

