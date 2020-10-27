--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

################################################################################
#Modulo            =>SEP                                                       #
#Programa          =>SEPL41                                                    #
#Objetivo          =>Integra solicitud marca operacion 27                      #
#Autor             =>Alexandro Hollmann, EFP                                   #
#Fecha inicio      =>15 Mayo    2012                                           #
#Modificado        =>                                                          #
################################################################################

DATABASE safre_viv

DEFINE v_ventana ui.Window

MAIN
   DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_nom_prog      VARCHAR(30), -- nombre del programa
          v_arr_arch_pend   DYNAMIC ARRAY OF VARCHAR(100), -- arreglo que contiene los archivos pendientes 
          v_arr_arch_eleg   DYNAMIC ARRAY OF VARCHAR(100), -- arreglo que contiene los archivos a integrar
          v_arr_elegidos  DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos integrados
             tot_pareja     INTEGER,
             tot_nrp_inv    INTEGER,
             tot_nrp_aso    INTEGER,
             tot_registro   INTEGER
          END RECORD,
          v_v_nomArch_proc  LIKE glo_ctr_archivo.nombre_archivo, --VARCHAR(100), -- nombre del archivo en proceso
          v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
          v_drag_index      INTEGER, -- indice del drag
          v_drop_index      INTEGER, -- indice del drop
          v_drag_source     STRING, -- fuente del drag
          v_drag_value      STRING, -- valor del drag
          v_i_num_arch      SMALLINT, -- numero de archivos a integrar
          v_i_iter          SMALLINT, -- variable usada para iteracion
          v_i_iter2         SMALLINT, -- variable usada para iteracion
          v_i_indice        SMALLINT, -- indice del arrego de archivos pendientes
          v_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod     LIKE cat_operacion.opera_cod, -- operación de la etapa que llama la funcion
          v_i_opera_cod_ant LIKE cat_operacion.opera_cod, -- operación de la etapa anterior
          --v_i_operacion     LIKE acr_ctr_archivo.operacion, -- operacion del proceso
          v_d_folio         LIKE glo_ctr_archivo.folio, -- folio
          v_d_pid           LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_c_ruta_bin      LIKE seg_modulo.ruta_bin, -- ruta del bin de acr
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_cadena        STRING, -- se asigna un mensaje que será presentado al usuario
          v_s_comando       STRING, -- contiene al comando a correr
          v_s_qryTxt        STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
          ,v_ruta_vacia     STRING,
          v_consulta        STRING,
          v_tabla           LIKE cat_layout.tabla
          CONSTANT l_nom_tbl_pend = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
          CONSTANT l_nom_tbl_int = "tbl_eleg" -- se asigna el nombre de la tabla de archivos a integrar
   DEFINE v_total_gral      INTEGER
   DEFINE v_fec_infosar     CHAR(10)
         
   DEFINE w               ui.window
   DEFINE f               ui.form
   DEFINE v_s_msj         STRING
   DEFINE v_count_detalle INTEGER
   DEFINE v_pos           INTEGER
   
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se inicializa el indice del arreglo
   LET v_i_indice = 1

   --se asignan los valores necesarios para la intergración
   LET v_i_proceso_cod = 2203 -- VALIDACION DE archivo de operacion 27
   LET v_i_opera_cod = 2 -- Integracion de VALIDACION DE archivo de operacion 27
   LET v_i_opera_cod_ant = 1 -- Carga (Validar) de archivo
   LET v_d_folio = 0      --Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_c_programa_cod = "SEPL41"

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

   --sE OBTIENEN las rutas de los ejecutables
   CALL fn_rutas("sep") RETURNING v_c_ruta_bin, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
   
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
   
   --Si el ultimo registro viene nulo se elimina el ultimo
   IF v_arr_arch_pend[v_arr_arch_pend.getLength()] IS NULL THEN
      CALL v_arr_arch_pend.deleteElement(v_arr_arch_pend.getLength())
   END IF
   
   -- se abre la ventana para elejir los archivos a elegir
   OPEN WINDOW w_elige_acred WITH FORM "SEPL411"
      -- se asigna el titulo del programa
      IF ( p_v_nom_prog IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_v_nom_prog)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_v_nom_prog)
      END IF
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
               CALL DIALOG.setActionHidden("integrar",1)
               INITIALIZE v_arr_elegidos TO NULL
         END DISPLAY

         DISPLAY ARRAY v_arr_arch_eleg TO tbl_eleg.*
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = l_nom_tbl_int
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_eleg[v_drag_index]

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
                   IF v_arr_arch_eleg.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      LET v_s_msj = "Solo se permite elegir un archivo a la vez"
                      CALL fn_mensaje("Aviso",v_s_msj,"stop")
                   ELSE 
                      LET v_drop_index = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(l_nom_tbl_int, v_drop_index)
                      CALL DIALOG.setCurrentRow(l_nom_tbl_int, v_drop_index)
                      LET v_arr_arch_eleg[v_drop_index] = v_drag_value
                      CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                   END IF
                END IF
                --CALL DIALOG.setActionHidden("integrar",1)
         END DISPLAY

         DISPLAY ARRAY v_arr_elegidos TO tbl_elegidos.*
         END DISPLAY

         BEFORE DIALOG
            -- se ocultan los botones (reporte, integrar)
            CALL DIALOG.setActionHidden("btn_reporte",1)
            CALL DIALOG.setActionHidden("integrar",1)
            CALL DIALOG.setActionHidden("cancelar",1)

         

         ON ACTION ACCEPT
            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_arch_eleg.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena = "Debe arrastrar al menos un archivo a integrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")
               CONTINUE DIALOG
            END IF

            -- se limpia el arreglo de los archivos ya integrados
            CALL v_arr_elegidos.clear()

            -- se procesan los archivos seleccionados para integrar
            SELECT NVL(COUNT(*),0)
              INTO v_count_detalle
              FROM safre_tmp:tmp_sep_det_03_op29
            
            LET v_arr_elegidos[1].tot_pareja = 0
            LET v_arr_elegidos[1].tot_pareja = v_arr_elegidos[1].tot_pareja + v_count_detalle

            SELECT NVL(COUNT(*),0)
              INTO v_count_detalle
              FROM safre_tmp:tmp_sep_det_03_op29
            
            LET v_arr_elegidos[1].tot_pareja = v_arr_elegidos[1].tot_pareja + v_count_detalle

            SELECT NVL(COUNT(*),0)
              INTO v_arr_elegidos[1].tot_nrp_inv
              FROM safre_tmp:tmp_sep_det_05_op29

            SELECT NVL(COUNT(*),0)
              INTO v_arr_elegidos[1].tot_nrp_aso
              FROM safre_tmp:tmp_sep_det_06_op29
            
            LET v_arr_elegidos[1].tot_registro  = v_arr_elegidos[1].tot_pareja  +
                                                  v_arr_elegidos[1].tot_nrp_inv +
                                                  v_arr_elegidos[1].tot_nrp_aso
-- jdym
            LET v_arr_elegidos[1].tot_pareja = v_arr_elegidos[1].tot_pareja / 2
----            
            -- si se muestra la informacion del archivo a integrar se habilita el boton de integrar
            IF v_arr_elegidos.getLength() > 0 THEN
               -- Se muestra la opcion del reporte
               CALL DIALOG.setActionHidden("integrar",0)
               CALL DIALOG.setActionHidden("cancelar",0)
               CALL DIALOG.setActionHidden("accept",1)
               CALL DIALOG.setActionHidden("close",1)
            END IF

            CONTINUE DIALOG

         ON ACTION integrar
            -- se invoca la funcion que valida la operacion
            CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida
   
            -- se verifica si la operacion en proceso es valida
            IF r_b_valida <> 0 THEN
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)
               EXIT DIALOG
            END IF
            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_elegidos.getLength()
            
            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena =  "No se han seleccionado archivos a intergrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")
               CONTINUE DIALOG
            END IF

            IF fn_ventana_confirma("Confimar","¿Integrar Archivo ?","info") = 1 THEN
                -- se asigna el nombre del archivo en la variable 
                LET v_v_nomArch_proc = v_arr_arch_eleg[1]
                -- se invoca la función que deja la operación en estado Procesando
                LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, 
                                                        v_i_proceso_cod, 
                                                        v_i_opera_cod,
                                                        v_d_folio, 
                                                        v_c_programa_cod,
                                                        v_v_nomArch_proc, 
                                                        p_v_usuario)
                
                -- se verifica si fue posible inicializar la operacion
                IF r_b_valida <> 0 THEN
                   -- en caso de error se muestra un mensaje a usuario y no continua
                   CALL fn_muestra_inc_operacion(r_b_valida)
                   CONTINUE DIALOG
                END IF
             
                -- se verifica si fue posible inicializar la operacion
                IF r_b_valida = 0 THEN
                   LET v_s_comando = "nohup fglrun ",v_c_ruta_bin CLIPPED,"/SEPE41.42r "
                                                    ,p_v_usuario, " "
                                                    ,v_d_pid, " "
                                                    ,v_i_proceso_cod," "
                                                    ,v_i_opera_cod," "
                                                    ,v_d_folio, " "
                                                    ,v_v_nomArch_proc
                                             ," 1>", v_c_ruta_list_bat CLIPPED
                                          ,"/nohup:",v_d_pid USING "&&&&&",":"
                                                    ,v_i_proceso_cod USING "&&&&&",":"
                                                    ,v_i_opera_cod USING "&&&&&"
                                          ," 2>&1 &"
                   
                   DISPLAY v_s_comando
                   RUN v_s_comando
                   IF(STATUS)THEN
                      CALL fn_mensaje(p_v_nom_prog,"Ocurrio un error al ejecutar la integración","about")
                   ELSE
                      CALL fn_mensaje(p_v_nom_prog,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                   END IF
                   EXIT DIALOG
             
                ELSE
                   -- en caso de error se muestra un mensaje a usuario y no continua
                   CALL fn_muestra_inc_operacion(r_b_valida)
             
                   CONTINUE DIALOG
                END IF
             
            END IF
            
            -- se limpia el arreglo que contiene los archivos a integrar
            CALL v_arr_arch_eleg.clear()
            CALL v_arr_elegidos.clear()
            CALL DIALOG.setActionHidden("integrar",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)
            CALL v_arr_elegidos.clear()
            
            -- Se muestra la opcion del reporte
            --CALL DIALOG.setActionHidden("btn_reporte",0)

            CONTINUE DIALOG

            
         ON ACTION btn_reporte
            LET v_s_cadena = "REPORTE"
            CALL fn_mensaje("Integración",v_s_cadena,"information")

            CONTINUE DIALOG
            
         ON ACTION cancelar
            CALL DIALOG.setActionHidden("integrar",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)
            CALL v_arr_elegidos.clear()


            CALL v_arr_arch_pend.clear()
            CALL v_arr_arch_eleg.clear()
            
            -- se crea la sentencia que busca los archivos disponibles por integrar
            LET v_s_qryTxt = " SELECT nombre_archivo\n",
                             "   FROM glo_ctr_archivo\n",
                             "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                             "    AND opera_cod = ",v_i_opera_cod_ant,"\n",
                             "    AND estado = 1" -- cargado
            
            LET v_i_indice = 1
            PREPARE prp_rec_archivos_val FROM v_s_qryTxt
            DECLARE cur_rec_archivos_val CURSOR FOR prp_rec_archivos_val
            
            FOREACH cur_rec_archivos_val INTO v_arr_arch_pend[v_i_indice]
               -- se incrementa el indice del arreglo
               LET v_i_indice = v_i_indice + 1
            END FOREACH
            --Si el ultimo registro viene nulo se elimina el ultimo
            IF v_arr_arch_pend[v_arr_arch_pend.getLength()] IS NULL THEN
               CALL v_arr_arch_pend.deleteElement(v_arr_arch_pend.getLength())
            END IF

            CONTINUE DIALOG
         
         ON ACTION close
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW w_elige_acred

END MAIN