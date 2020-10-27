--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07-04-2012
--==============================================================================

################################################################################
#Modulo            =>MDT                                                       #
#Programa          =>MDTL07                                                    #
#Objetivo          =>Genera rechazos de mandatos con origen                    #
#Autor             =>Francisco López                                           #
#Fecha inicio      =>17 Febrero 2012                                           #
################################################################################

DATABASE safre_viv
DEFINE v_ventana ui.Window
MAIN
   DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_nom_prog      VARCHAR(30), -- nombre del programa
          v_arr_arch_pend   DYNAMIC ARRAY OF VARCHAR(20), --VARCHAR(100), -- arreglo que contiene los archivos pendientes 
          v_arr_arch_int    DYNAMIC ARRAY OF STRING, --VARCHAR(100), -- arreglo que contiene los archivos a integrar
          v_arr_integrados  DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos integrados
             nom_archivo    VARCHAR(20),   --VARCHAR(100),
             aceptadas      INTEGER,
             rechazadas     INTEGER,
             total_archivo  INTEGER
          END RECORD,
          --v_r_acr_ctr_arch  RECORD LIKE acr_ctr_archivo.*, -- registro del control de archivo  
          v_v_nomArch_proc  LIKE glo_ctr_archivo.nombre_archivo, --VARCHAR(100), -- nombre del archivo en proceso
          v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
          v_drag_index      INTEGER, -- indice del drag
          v_drop_index      INTEGER, -- indice del drop
          v_drag_source     STRING, -- fuente del drag
          v_drag_value      STRING, -- valor del drag
          v_i_num_arch      SMALLINT, -- numero de archivos a integrar
          v_i_iter          SMALLINT, -- variable usada para iteracion
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
          ,v_ruta_vacia      STRING
          CONSTANT l_nom_tbl_pend = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
          CONSTANT l_nom_tbl_int = "tbl_integrar" -- se asigna el nombre de la tabla de archivos a integrar
         
   DEFINE w               ui.window
   DEFINE f               ui.form

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializa el indice del arreglo
   LET v_i_indice = 1

   --se asignan los valores necesarios para la intergración
   # HCRG se modifica el proceso a peticion de usuario
   LET v_i_proceso_cod = 1306 -- VALIDACION DE SUSTENTABILIDAD
   --LET v_i_proceso_cod = 48  -- VALIDACION DE SUSTENTABILIDAD
   LET v_i_opera_cod = 3     -- Integracion de SOLICITUD DE VALIDACION DE SUSTENTABILIDAD
   LET v_i_opera_cod_ant = 2 -- Carga (Validar) de archivo
   --LET v_i_operacion = 01  -- operacion del proceso
   LET v_d_folio = 0         --Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_c_programa_cod = "MDTL07"

   -- se crea la sentencia sql que obtiene el pid perteneciente al folio
   LET v_s_qryTxt = " SELECT NVL(MAX(pid),0) \n",
                    "   FROM bat_ctr_proceso \n",
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
   CALL fn_rutas("mdt") RETURNING v_c_ruta_bin, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

   LET v_s_qryTxt = "\n SELECT UNIQUE a.folio              "
                   ,"\n FROM   mdt_lote_mandato AS a        "
                   ,"\n WHERE  a.id_origen = 2              "
                   ,"\n AND    a.estado    = 102            "
                   ,"\n AND EXISTS                          "
                   ,"\n (SELECT 'OK'                        "
                   ,"\n  FROM   mdt_solicitud_mandato AS b  "
                   ,"\n  WHERE  b.estado IN (105,108)       "
                   ,"\n  AND    b.folio = a.folio         "
                   --,"\n  AND    b.lote = a.lote             "
                   ,"\n  AND    b.id_origen = 2             "
                   ,"\n )                                   "
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
   OPEN WINDOW w_elige_acred WITH FORM "MDTL071"
      #Se asigna el titulo de la ventana
      IF(p_v_nom_prog IS NOT NULL)THEN
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
               CALL DIALOG.setActionHidden("accept",0)
               INITIALIZE v_arr_integrados TO NULL
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
            -- se ocultan los botones (reporte, integrar)
            CALL DIALOG.setActionHidden("btn_reporte",1)
            CALL DIALOG.setActionHidden("integrar",1)
            CALL DIALOG.setActionHidden("accept",0)

         

         ON ACTION ACCEPT
            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_arch_int.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena = "Debe arrastrar al menos un lote a integrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")

               CONTINUE DIALOG
            END IF

            -- se limpia el arreglo de los archivos ya integrados
            CALL v_arr_integrados.clear()

            -- se procesan los archivos seleccionados para integrar
            FOR v_i_iter = 1 TO v_i_num_arch
               -- se asigna el nombre del archivo en la variable paramentro 
               LET v_v_nomArch_proc = v_arr_arch_int[v_i_iter]

                -- se asigna la informacion del archivo integrado
                LET v_arr_integrados[v_i_iter].nom_archivo = v_v_nomArch_proc
                #####

                #####
                --Obtenemos el total ACEPTADAS del archivo
                SELECT COUNT(*)
                INTO   v_arr_integrados[v_i_iter].aceptadas
                FROM   mdt_solicitud_mandato AS a
                WHERE  a.estado = 105
                AND    a.id_origen = 2
                AND EXISTS
                (SELECT 'OK' FROM mdt_lote_mandato AS b
                 --WHERE  b.f_lote = a.f_lote
                 WHERE  b.folio = a.folio
                 --AND    b.lote = a.lote
                 AND    b.id_origen = 2
                 AND    b.estado    = 102
                 --AND    b.f_lote = v_arr_integrados[v_i_iter].nom_archivo
                 AND    b.folio = v_arr_integrados[v_i_iter].nom_archivo
                )

                --Obtenemos el total RECHAZADAS del archivo
                SELECT COUNT(*)
                INTO   v_arr_integrados[v_i_iter].rechazadas
                FROM   mdt_solicitud_mandato AS a
                WHERE  a.estado = 108
                AND    a.id_origen = 2
                AND EXISTS
                (SELECT 'OK' FROM mdt_lote_mandato AS b
                 --WHERE  b.f_lote = a.f_lote
                 WHERE  b.folio = a.folio        
                 --AND    b.lote = a.lote
                 AND    b.id_origen = 2
                 AND    b.estado    = 102
                 --AND    b.f_lote = v_arr_integrados[v_i_iter].nom_archivo
                 AND    b.folio = v_arr_integrados[v_i_iter].nom_archivo
                )
                --Obtenemos el TOTAL del archivo
                SELECT COUNT(*)
                INTO   v_arr_integrados[v_i_iter].total_archivo
                FROM   mdt_solicitud_mandato AS a
                WHERE  a.estado IN (105, 108)
                AND    a.id_origen = 2
                AND EXISTS
                (SELECT 'OK' FROM mdt_lote_mandato AS b
                 --WHERE  b.f_lote = a.f_lote
                 WHERE  b.folio = a.folio
                 --AND    b.lote = a.lote
                 AND    b.id_origen = 2
                 AND    b.estado    = 102
                 --AND    b.f_lote = v_arr_integrados[v_i_iter].nom_archivo
                 AND    b.folio = v_arr_integrados[v_i_iter].nom_archivo
                )
            END FOR

            -- si se muestra la informacion del archivo a integrar se habilita el boton de integrar
            IF v_arr_integrados.getLength() > 0 THEN
               -- Se muestra la opcion del reporte
               CALL DIALOG.setActionHidden("integrar",0)
               CALL DIALOG.setActionHidden("accept",1)
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
            LET v_i_num_arch = v_arr_integrados.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena =  "No se han seleccionado un lote a intergrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")

               CONTINUE DIALOG
            END IF
            IF v_i_num_arch > 1 THEN
               CALL fn_mensaje("Aviso","Solo es posible integrar un lote a la vez","stop")
               CONTINUE DIALOG
            END IF
            --Se marca el proceso como iniciado
            CALL fn_actualiza_opera_ini (v_d_pid,
                                         v_i_proceso_cod,
                                         v_i_opera_cod,
                                         v_d_folio,
                                         "MDTL07",
                                         v_v_nomArch_proc,
                                         p_v_usuario)
               RETURNING r_b_valida

            IF r_b_valida <> 0 THEN
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)   
               EXIT DIALOG
            END IF
            -- se procesan los archivos seleccionados para integrar
            FOR v_i_iter = 1 TO v_i_num_arch
               -- se asigna el nombre del archivo en la variable 
               LET v_v_nomArch_proc = v_arr_integrados[v_i_iter].nom_archivo

               -- se verifica si fue posible inicializar la operacion
               IF r_b_valida = 0 THEN

                  LET v_s_comando = "nohup fglrun ",v_c_ruta_bin CLIPPED,"/MDTS07.42r "
                                                   ,p_v_usuario, " "
                                                   ,v_d_pid, " "
                                                   ,v_i_proceso_cod," "
                                                   ,v_i_opera_cod," "
                                                   ,v_v_nomArch_proc, " "
                                                   ,"NA"
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
            END FOR

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
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW w_elige_acred

END MAIN