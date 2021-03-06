--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20-07-2012 
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL43                                                   #
#Objetivo          => Reverso integraci�n operaci�n 29                         #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 20 Julio, 2012                                           #
#Modificado        =>                                                          #
################################################################################

DATABASE safre_viv

DEFINE v_ventana ui.Window

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario, # usuario logeado
       p_tipo_carga    SMALLINT, # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo        VARCHAR(30), # nombre de la ventana
       v_folios_pend   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios pendientes 
       v_folios_eleg   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios a reversar
       v_det_folio     DYNAMIC ARRAY OF RECORD  # arreglo que contiene el detalle
         tot_pareja    INTEGER,
         tot_nrp_inv   INTEGER,
         tot_nrp_aso   INTEGER,
         tot_registro  INTEGER
       END RECORD,
       v_proceso_cod_rev LIKE cat_proceso.proceso_cod, # proceso que realiza el reverso
       v_opera_cod_rev   LIKE cat_operacion.opera_cod, # operaci�n que realiza el reverso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # proceso a reversar
       v_opera_cod       LIKE cat_operacion.opera_cod, # operaci�n a reversar
       v_indice        SMALLINT, # indice del arrego de folios pendientes
       v_programa_cod  LIKE bat_ctr_operacion.programa_cod, # nombre del programa que ejecuta el reverso
       v_consulta      STRING, # variable para consultas SQL
       v_pid           LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_pid_rev       LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_ruta_bin      LIKE seg_modulo.ruta_bin, # ruta ejecutable sep
       v_ruta_lst      LIKE seg_modulo.ruta_listados, # ruta listados de bat
       v_ruta_vacia    STRING,
       v_confirma      BOOLEAN,
       v_folio         INTEGER,--LIKE glo_ctr_archivo.folio, # folio a reversar
       v_comando       STRING, # variable para contener comando para ejecutar batch
       v_archivo       LIKE glo_ctr_archivo.nombre_archivo,# nombre del archivo en proceso
       r_res_opera     SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
       
       v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
       v_drag_index      INTEGER, -- indice del drag
       v_drop_index      INTEGER, -- indice del drop
       v_drag_source     STRING, -- fuente del drag
       v_drag_value      STRING -- valor del drag
       
       CONSTANT v_tbl_pendientes = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
       CONSTANT v_tbl_elegidos = "tbl_elegidos" -- se asigna el nombre de la tabla de archivos a integrar

DEFINE v_count_detalle INTEGER,
       v_ind       SMALLINT,
       v_diag      CHAR(3),
       v_sql_error INTEGER,
       v_folio_val LIKE glo_ctr_archivo.folio,
       v_pid_val   LIKE bat_ctr_proceso.pid,
       v_isam_error INTEGER,
       v_msg_error  CHAR(100),
       v_mensaje    STRING


   
   # Par�metros del menu
   LET p_usuario_cod = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo      = ARG_VAL(3)

   # se inicializa el indice del arreglo
   LET v_indice = 1

   # se asignan los valores necesarios para el reverso
   LET v_proceso_cod_rev = 2219 # reverso de integracion de operacion 29
   LET v_opera_cod_rev   = 1 # reverso de integracion de operacion 29
   LET v_proceso_cod = 2203 # integracion de operacion 29
   LET v_opera_cod   = 2 # integracion de operacion 29

   
   LET v_folio = 0      # Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_programa_cod = "SEPL41"

   # recuepra el pid del proceso a reversar
   --CALL fn_max_pid(v_proceso_cod,v_opera_cod) RETURNING v_pid
   
   # se invoca la funcion que valida ejecucion de reverso
   CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
   # se verifica si la operacion en proceso es valida
   IF(r_res_opera <> 0)THEN
      # en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_res_opera)
      EXIT PROGRAM
   END IF
   
   # valida que se pueda ejecutar el reverso
   {CALL fn_valida_reverso(v_pid,v_proceso_cod,v_opera_cod)RETURNING r_res_opera
   IF(r_res_opera <> 0)THEN
      CALL fn_muestra_inc_operacion(r_res_opera)
      EXIT PROGRAM
   END IF}
   
   LET v_comando = "EXECUTE FUNCTION fn_sep_vr_integrar_op29()"
   PREPARE prp_val_rev_op29 FROM v_comando
   EXECUTE prp_val_rev_op29 INTO v_ind,
                                 v_diag,
                                 v_sql_error,
                                 v_isam_error,
                                 v_msg_error,
                                 v_folio,
                                 v_pid
   # mensajes para el caso de insonsistencias y errores
   IF(v_sql_error <> 0)THEN # error sql
      CALL fn_mensaje("Aviso","Reverso no procedente, c�digo: "||v_sql_error||"\nMensaje: "||v_msg_error,"information")
      EXIT PROGRAM
   ELSE
      IF(v_ind <> 0)THEN # inconsistencias
         CASE v_diag
            WHEN "002"
               LET v_mensaje = "no se encuentra folio a reversar"
            WHEN "003"
               LET v_mensaje = "no se encuentra el pid a ser reversado"
            WHEN "004"
               LET v_mensaje = "error en validacion de reverso de proceso"
         END CASE 
         CALL fn_mensaje("Aviso","Reverso no procedente: "||v_mensaje CLIPPED,"information")
         EXIT PROGRAM
      END IF
   END IF
   # solo se procesa el ultimo folio de op 29
   LET v_folios_pend[1] = v_folio

   
   # se obtienen las rutas de separacion
   CALL fn_rutas("sep") RETURNING v_ruta_bin, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_lst

   {LET v_consulta = "\n SELECT glo.folio",
                    "\n  FROM glo_folio glo JOIN sep_det_02_op29 sep",
                    "\n    ON glo.folio = sep.folio",
                    "\n WHERE glo.proceso_cod = ?",
                    "\n   AND glo.opera_cod = ?",
                    "\n   AND glo.status = 0",
                    "\n ORDER BY glo.folio DESC"
   PREPARE prp_rec_folios FROM v_consulta
   
   DECLARE cur_rec_folios CURSOR FOR prp_rec_folios

   FOREACH cur_rec_folios USING v_proceso_cod,v_opera_cod
                           INTO v_folios_pend[v_indice]}
      -- se incrementa el indice del arreglo
     -- LET v_indice = v_indice + 1
   {END FOREACH
   # Si el ultimo registro es nulo, se elimina
   IF v_folios_pend[v_folios_pend.getLength()] IS NULL THEN
      CALL v_folios_pend.deleteElement(v_folios_pend.getLength())
   END IF}
   
   OPEN WINDOW vtna_reversa_folio WITH FORM v_ruta_bin CLIPPED||"/SEPL431"
      # se asigna el titulo de la ventana
      IF ( p_titulo IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_folios_pend TO tbl_pendientes.*
            # recupera datos de origen
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = v_tbl_pendientes
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_folios_pend[v_drag_index]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
               IF v_drag_source == v_tbl_pendientes THEN
                  CALL v_ui_dnd.dropInternal()
               ELSE
                  LET v_drop_index = v_ui_dnd.getLocationRow()
                  CALL DIALOG.insertRow(v_tbl_pendientes, v_drop_index)
                  CALL DIALOG.setCurrentRow(v_tbl_pendientes, v_drop_index)
                  LET v_folios_pend[v_drop_index] = v_drag_value
                  CALL DIALOG.deleteRow(v_tbl_elegidos, v_drag_index)
               END IF
               CALL DIALOG.setActionHidden("reverso",1)
               INITIALIZE v_det_folio TO NULL
         END DISPLAY

         DISPLAY ARRAY v_folios_eleg TO tbl_elegidos.*
            # recupera datos de origen
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = v_tbl_elegidos
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_folios_eleg[v_drag_index]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
                IF v_drag_source == v_tbl_elegidos THEN
                    CALL v_ui_dnd.dropInternal()
                ELSE
                   IF v_folios_eleg.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      CALL fn_mensaje("Aviso","Solo se permite elegir un folio a la vez","stop")
                   ELSE 
                      LET v_drop_index = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(v_tbl_elegidos, v_drop_index)
                      CALL DIALOG.setCurrentRow(v_tbl_elegidos, v_drop_index)
                      LET v_folios_eleg[v_drop_index] = v_drag_value
                      CALL DIALOG.deleteRow(v_tbl_pendientes, v_drag_index)
                   END IF
                END IF
                --CALL DIALOG.setActionHidden("integrar",1)
         END DISPLAY

         DISPLAY ARRAY v_det_folio TO tbl_det_elegidos.*
         END DISPLAY

         BEFORE DIALOG
            -- se ocultan los botones (reporte, integrar)
            --CALL DIALOG.setActionHidden("btn_reporte",1)
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)

         ON ACTION ACCEPT
            # en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF(v_folios_eleg.getLength() = 0)THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un folio a reversar","stop")
               CONTINUE DIALOG
            END IF

            # se limpia el arreglo de detalle
            CALL v_det_folio.clear()

            # se recupera el detalle
            LET v_consulta = "\n SELECT NVL(COUNT(*),0)",
                             "\n   FROM sep_det_03_op29 det3 JOIN sep_det_02_op29 det2",
                             "\n     ON det3.id_det_02_op29 = det2.id_det_02_op29",
                             "\n  WHERE det2.folio = ?"

            PREPARE prp_rec_conteo_det3 FROM v_consulta
            EXECUTE prp_rec_conteo_det3 USING v_folios_eleg[1] 
                                         INTO v_count_detalle 
            LET v_det_folio[1].tot_pareja = 0
            LET v_det_folio[1].tot_pareja = v_det_folio[1].tot_pareja + v_count_detalle

            LET v_consulta = "\n SELECT NVL(COUNT(*),0)",
                             "\n   FROM sep_det_02_op29 det2",
                             "\n  WHERE det2.folio = ?"
                             
            PREPARE prp_rec_conteo_det2 FROM v_consulta
            EXECUTE prp_rec_conteo_det2 USING v_folios_eleg[1] 
                                         INTO v_count_detalle
            LET v_det_folio[1].tot_pareja = v_det_folio[1].tot_pareja + v_count_detalle

            LET v_consulta = "\n SELECT NVL(COUNT(*),0)",
                             "\n   FROM sep_det_02_op29 det2 JOIN sep_det_05_op29 det5",
                             "\n     ON det5.id_det_02_op29 = det2.id_det_02_op29",
                             "\n  WHERE det2.folio = ?"
                             
            PREPARE prp_rec_conteo_det5 FROM v_consulta
            EXECUTE prp_rec_conteo_det5 USING v_folios_eleg[1] 
                                         INTO v_count_detalle
            LET v_det_folio[1].tot_nrp_inv = 0
            LET v_det_folio[1].tot_nrp_inv = v_count_detalle

            LET v_consulta = "\n SELECT NVL(COUNT(*),0)",
                             "\n   FROM sep_det_03_op29 det3 JOIN sep_det_02_op29 det2",
                             "\n     ON det3.id_det_02_op29 = det2.id_det_02_op29",
                             "\n        JOIN sep_det_06_op29 det6",
                             "\n     ON det6.id_det_03_op29 = det3.id_det_03_op29",
                             "\n  WHERE det2.folio = ?"
                             
            PREPARE prp_rec_conteo_det6 FROM v_consulta
            EXECUTE prp_rec_conteo_det6 USING v_folios_eleg[1] 
                                         INTO v_count_detalle
            LET v_det_folio[1].tot_nrp_aso = 0
            LET v_det_folio[1].tot_nrp_aso = v_count_detalle
            
            LET v_det_folio[1].tot_registro = v_det_folio[1].tot_pareja  +
                                              v_det_folio[1].tot_nrp_inv +
                                              v_det_folio[1].tot_nrp_aso

            LET v_det_folio[1].tot_pareja = v_det_folio[1].tot_pareja / 2

            IF v_det_folio.getLength() > 0 THEN
               -- Se muestra la opcion del reporte
               CALL DIALOG.setActionHidden("reverso",0)
               CALL DIALOG.setActionHidden("cancelar",0)
               CALL DIALOG.setActionHidden("accept",1)
               CALL DIALOG.setActionHidden("close",1)
            END IF
            CONTINUE DIALOG

         ON ACTION reverso            
            # en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF(v_det_folio.getLength() = 0)THEN
               CALL fn_mensaje("Aviso","No se han seleccionado folios a reversar","stop")
               CONTINUE DIALOG
            END IF
            CALL fn_ventana_confirma("Confimar","Reversar Folio?","info") RETURNING v_confirma
            IF(v_confirma)THEN
                
                # se invoca la funcion que valida ejecucion de reverso
                CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                
                # se verifica si fue posible inicializar la operacion
                IF(r_res_opera = 0)THEN
                   # se recupera el folio elegido
                   LET v_folio = v_folios_eleg[1]
                   LET v_archivo = "NA"
                   CALL fn_genera_pid(v_proceso_cod_rev,v_opera_cod_rev,p_usuario_cod)
                           RETURNING v_pid_rev 
                   #inicializa el proceso con todas sus operaciones en estado LISTO
                   CALL fn_inicializa_proceso(v_pid_rev, 
                                              v_proceso_cod_rev, 
                                              v_opera_cod_rev, 
                                              v_folio,
                                              "SEPL43",
                                              "NA", 
                                              p_usuario_cod) 
                               RETURNING r_res_opera                        
                   IF(r_res_opera  <> 0)THEN
                      # Imprime el mensaje de inconsistencia en pantalla
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      EXIT DIALOG
                   END IF

                   CALL fn_actualiza_opera_ini(v_pid_rev,         # pid que realiza el reverso
                                               v_proceso_cod_rev, # proceso que realiza el reverso
                                               v_opera_cod_rev,   # operacion que realiza el reverso
                                               v_folio,                 # folio
                                               "SEPL43",          # programa
                                               "NA",              # archivo
                                               p_usuario_cod)     # usuario
                            RETURNING r_res_opera
                   IF(r_res_opera <> 0)THEN
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                      EXIT DIALOG
                   END IF
                   
                   LET v_comando = "nohup fglrun ",v_ruta_bin CLIPPED,"/SEPR43.42r ",
                                                   p_usuario_cod, " ",
                                                   v_pid_rev, " ",
                                                   v_proceso_cod_rev," ",
                                                   v_opera_cod_rev," ",
                                                   v_folio, " '",
                                                   v_archivo,
                                           "' 1>", v_ruta_lst CLIPPED,
                                         "/nohup:",v_pid_rev USING "&&&&&",":",
                                                   v_proceso_cod_rev USING "&&&&&",":",
                                                   v_opera_cod_rev USING "&&&&&",
                                         " 2>&1 &"
                   
                   DISPLAY v_comando
                   RUN v_comando
                   IF(STATUS)THEN
                      CALL fn_mensaje(p_titulo,"Ocurrio un error al ejecutar el reverso","about")
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                   ELSE
                      CALL fn_mensaje(p_titulo,"Se ha enviado la operacion.\nPodr� revisar el detalle en el monitoreo de procesos","about")
                   END IF
                   EXIT DIALOG
             
                ELSE
                   # en caso de error se muestra un mensaje a usuario y no continua
                   CALL fn_muestra_inc_operacion(r_res_opera)             
                   CONTINUE DIALOG
                END IF
            END IF
            
            # se limpia el arreglo que contiene los folios
            CALL v_folios_eleg.clear()
            CALL v_det_folio.clear()
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)
            
            CONTINUE DIALOG

            
         {ON ACTION btn_reporte
            LET v_s_cadena = "REPORTE"
            CALL fn_mensaje("Integraci�n",v_s_cadena,"information")

            CONTINUE DIALOG}
            
         ON ACTION cancelar
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)
            CALL v_det_folio.clear()

            CALL v_folios_pend.clear()
            CALL v_folios_eleg.clear()
            
            LET v_folios_pend[1] = v_folio
            CONTINUE DIALOG
         
         ON ACTION close
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW vtna_reversa_folio

END MAIN