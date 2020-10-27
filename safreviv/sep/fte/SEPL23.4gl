--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPL23                                        #
#Objetivo          =>Integra expediente tipo D                     #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>17 Mayo    2012                               #
#Modificado        =>                                              #
####################################################################

DATABASE safre_viv
DEFINE v_ventana ui.Window

MAIN
   DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_nom_prog      VARCHAR(30), -- nombre del programa
          v_arr_arch_pend   DYNAMIC ARRAY OF VARCHAR(100), -- arreglo que contiene los archivos pendientes 
          v_arr_arch_int    DYNAMIC ARRAY OF VARCHAR(100), -- arreglo que contiene los archivos a integrar
          v_arr_integrados  DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos integrados
             nom_archivo    VARCHAR(100),
             clave_afore    CHAR(3),
             fecha_infosar  DATE,
             total_gral     INTEGER
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
          CONSTANT l_nom_tbl_int = "tbl_integrar" -- se asigna el nombre de la tabla de archivos a integrar
   DEFINE v_total_gral      INTEGER
   DEFINE v_fec_infosar     CHAR(10)
         
   DEFINE w               ui.window
   DEFINE f               ui.form
   DEFINE v_s_msj         STRING
   
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   

   -- se inicializa el indice del arreglo
   LET v_i_indice = 1

   --se asignan los valores necesarios para la intergración
   # HCRG se modifica el proceso a peticion de usuario
   LET v_i_proceso_cod = 2206 -- VALIDACION DE archivo de expediente tipo D
   LET v_i_opera_cod = 2 -- Integracion de VALIDACION DE archivo de expediente tipo D
   LET v_i_opera_cod_ant = 1 -- Carga (Validar) de archivo
   LET v_d_folio = 0      --Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_c_programa_cod = "SEPL23"

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
   OPEN WINDOW w_elige_acred WITH FORM "SEPL231"
      -- se asigna el titulo del programa
      IF ( p_v_nom_prog IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_v_nom_prog)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_v_nom_prog)
      END IF
      DIALOG ATTRIBUTE(UNBUFFERED)
         -- Despliegue del arreglo de pendientes
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
               INITIALIZE v_arr_integrados TO NULL
         END DISPLAY

         -- Despliegue del arreglo por integrar
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
                   IF v_arr_arch_int.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      LET v_s_msj = "Solo se permite elegir un archivo a la vez"
                      CALL fn_mensaje("Aviso",v_s_msj,"stop")
                   ELSE 
                      LET v_drop_index = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(l_nom_tbl_int, v_drop_index)
                      CALL DIALOG.setCurrentRow(l_nom_tbl_int, v_drop_index)
                      LET v_arr_arch_int[v_drop_index] = v_drag_value
                      CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                   END IF
                END IF
                --CALL DIALOG.setActionHidden("integrar",1)
         END DISPLAY

         -- Despliegue del arreglo de integrados
         DISPLAY ARRAY v_arr_integrados TO tbl_integrados.*
         END DISPLAY

         BEFORE DIALOG
            -- se ocultan los botones (reporte, integrar)
            CALL DIALOG.setActionHidden("btn_reporte",1)
            CALL DIALOG.setActionHidden("integrar",1)
            CALL DIALOG.setActionHidden("cancelar",1)

         

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
            
            -- AHM TMP Por definir -- CALL fn_despliegue_integrados()
            
            DISPLAY v_total_gral TO total_gral
            -- si se muestra la informacion del archivo a integrar se habilita el boton de integrar
            IF v_arr_integrados.getLength() > 0 THEN
               -- Se muestra la opcion del reporte
               CALL DIALOG.setActionHidden("integrar",0)
               CALL DIALOG.setActionHidden("cancelar",0)
               CALL DIALOG.setActionHidden("accept",1)
               CALL DIALOG.setActionHidden("salir",1)
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
            IF v_i_num_arch > 1 THEN
               CALL fn_mensaje("Aviso","Solo es posible integrar un archivo a la vez","stop")
               CONTINUE DIALOG
            END IF

            IF fn_ventana_confirma("Confimar","¿Integrar Archivo ?","info") = 1 THEN
               -- se asigna el nombre del archivo en la variable 
               LET v_v_nomArch_proc = v_arr_integrados[1].nom_archivo

               -- se verifica si fue posible inicializar la operacion
               IF r_b_valida = 0 THEN
                  -- AHM TMP Por definir -- LET v_s_comando = "nohup fglrun "
                  -- AHM TMP Por definir --                ,v_c_ruta_bin CLIPPED
                  -- AHM TMP Por definir --                ,"/SEPE23.42r "
                  -- AHM TMP Por definir --                ,p_v_usuario, " "
                  -- AHM TMP Por definir --                ,v_d_pid, " "
                  -- AHM TMP Por definir --                ,v_i_proceso_cod," "
                  -- AHM TMP Por definir --                ,v_i_opera_cod," "
                  -- AHM TMP Por definir --                ,v_d_folio, " "
                  -- AHM TMP Por definir --                ,v_v_nomArch_proc
                  -- AHM TMP Por definir --                ," 1>", v_c_ruta_list_bat CLIPPED
                  -- AHM TMP Por definir --                ,"/nohup:",v_d_pid USING "&&&&&",":"
                  -- AHM TMP Por definir --                ,v_i_proceso_cod USING "&&&&&",":"
                  -- AHM TMP Por definir --                ,v_i_opera_cod USING "&&&&&"
                  -- AHM TMP Por definir --                ," 2>&1 &"
                  -- AHM TMP Por definir -- 
                  -- AHM TMP Por definir -- DISPLAY v_s_comando
                  -- AHM TMP Por definir -- RUN v_s_comando
                  -- AHM TMP Por definir -- IF(STATUS)THEN
                  -- AHM TMP Por definir --    CALL fn_mensaje(p_v_nom_prog,"Ocurrio un error al ejecutar la integración","about")
                  -- AHM TMP Por definir -- ELSE
                  -- AHM TMP Por definir --    CALL fn_mensaje(p_v_nom_prog,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                  -- AHM TMP Por definir -- END IF
                  EXIT DIALOG
 
               ELSE
                  -- en caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)

                  CONTINUE DIALOG
               END IF

            END IF
            
            -- se limpia el arreglo que contiene los archivos a integrar
            CALL v_arr_arch_int.clear()

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
            CALL DIALOG.setActionHidden("salir",0)
            CALL v_arr_integrados.clear()
            DISPLAY NULL TO total_gral

            CALL v_arr_arch_pend.clear()
            CALL v_arr_arch_int.clear()
            
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
            --Si el ultimo registro viene nulo se elimina
            IF v_arr_arch_pend[v_arr_arch_pend.getLength()] IS NULL THEN
               CALL v_arr_arch_pend.deleteElement(v_arr_arch_pend.getLength())
            END IF

            CONTINUE DIALOG
         
         ON ACTION salir
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW w_elige_acred

END MAIN