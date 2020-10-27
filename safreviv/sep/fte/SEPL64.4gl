--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01-11-2012 
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL64                                                   #
#Objetivo          => Reverso de liquidacion de expedientes s�lo infonavit     #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 01 Noviembre 2012                                        #
#Modificado        =>                                                          #
################################################################################

DATABASE safre_viv
GLOBALS "SEPG02.4gl"
DEFINE v_ventana ui.Window,
       p_usuario_cod   LIKE seg_usuario.usuario, # usuario logeado
       p_tipo_carga    SMALLINT, # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo        VARCHAR(30), # nombre de la ventana
       v_folios_pend   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios pendientes 
       v_folios_eleg   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios a reversar
       v_proceso_cod_rev LIKE cat_proceso.proceso_cod, # proceso que realiza el reverso
       v_opera_cod_rev   LIKE cat_operacion.opera_cod, # operaci�n que realiza el reverso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # proceso a reversar
       v_opera_cod       LIKE cat_operacion.opera_cod # operaci�n a reversar

MAIN
DEFINE v_indice          SMALLINT, # indice del arrego de folios pendientes
       v_programa_cod    LIKE bat_ctr_operacion.programa_cod, # nombre del programa que ejecuta el reverso
       v_consulta        STRING, # variable para consultas SQL
       v_pid             LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_pid_rev         LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_ruta_bin        LIKE seg_modulo.ruta_bin, # ruta ejecutable sep
       v_ruta_lst        LIKE seg_modulo.ruta_listados, # ruta listados de bat
       v_ruta_vacia      STRING,
       v_confirma        BOOLEAN,
       v_folio           LIKE glo_ctr_archivo.folio, # folio a reversar
       v_comando         STRING, # variable para contener comando para ejecutar batch
       v_archivo         LIKE glo_ctr_archivo.nombre_archivo,# nombre del archivo en proceso
       r_res_opera       SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
       
       v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
       v_indice_arrastre INTEGER, -- indice del drag
       v_indice_destino  INTEGER, -- indice del drop
       v_origen_arrastre STRING, -- fuente del drag
       v_valor_arrastre  STRING -- valor del drag
       
       CONSTANT v_tbl_pendientes = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
       CONSTANT v_tbl_elegidos = "tbl_elegidos" -- se asigna el nombre de la tabla de archivos a integrar

DEFINE v_count_detalle INTEGER,
       v_ind           SMALLINT,
       v_diag          CHAR(3),
       v_sql_error     INTEGER,
       v_folio_val     LIKE glo_ctr_archivo.folio,
       v_pid_val       LIKE bat_ctr_proceso.pid,
       v_isam_error    INTEGER,
       v_msg_error     CHAR(100),
       v_mensaje       STRING,
       v_bnd_rev_cnt   SMALLINT
   
   # Par�metros del menu
   LET p_usuario_cod = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo      = ARG_VAL(3)

   # se inicializa el indice del arreglo
   LET v_indice = 1

   # se asignan los valores necesarios para el reverso
   LET v_proceso_cod_rev = v_proc_reverso_liq_exp_solo_infonavit # reverso de liquidaci�n de expedientes s�lo infonavit
   LET v_opera_cod_rev   = v_opera_reverso_liq_exp_solo_infonavit # reverso de liquidaci�n de expedientes s�lo infonavit
   LET v_proceso_cod     = v_proc_expedientes_solo_infonavit # expedientes s�lo infonavit
   LET v_opera_cod       = v_opera_liq_expedientes_solo_infonavit # liquidacion de expedientes s�lo infonavit

   
   LET v_folio = 0      # Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_programa_cod = "SEPL64"

  
   # se invoca la funcion que valida ejecucion de reverso
   CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
   # se verifica si la operacion en proceso es valida
   IF ( r_res_opera <> 0 ) THEN
      # en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_res_opera)
      EXIT PROGRAM
   END IF
   
   # se valida que se pueda ejecutar el reverso
   {LET v_comando = "EXECUTE FUNCTION fn_sep_vr_liquidar_op28()"
   PREPARE prp_val_rev_op28 FROM v_comando
   EXECUTE prp_val_rev_op28 INTO v_ind,
                                 v_diag,
                                 v_sql_error,
                                 v_isam_error,
                                 v_msg_error,
                                 v_folio,
                                 v_pid                                 
                                 
   # mensajes para el caso de insonsistencias y errores
   IF ( v_sql_error <> 0 ) THEN # error sql
      CALL fn_mensaje("Aviso","Reverso no procedente, c�digo: "||v_sql_error||"\nMensaje: "||v_msg_error,"information")
      EXIT PROGRAM
   ELSE
      IF ( v_ind <> 0 ) THEN # inconsistencias
         CASE v_diag
            WHEN "001"
               LET v_mensaje = "folio preliquidado existente no procede reverso"
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
   END IF}

   

   # solo se procesa el ultimo folio de op 28
   LET v_folios_pend[1] = v_folio USING "########&"

   
   # se obtienen las rutas de separacion
   CALL fn_rutas("sep") RETURNING v_ruta_bin, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_lst

   OPEN WINDOW vtna_reversa_folio WITH FORM v_ruta_bin CLIPPED||"/SEPL641"
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
               LET v_origen_arrastre = v_tbl_pendientes
               LET v_indice_arrastre  = arr_curr()
               LET v_valor_arrastre  = v_folios_pend[v_indice_arrastre]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_origen_arrastre TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_origen_arrastre IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
               IF v_origen_arrastre == v_tbl_pendientes THEN
                  CALL v_ui_dnd.dropInternal()
               ELSE
                  LET v_indice_destino = v_ui_dnd.getLocationRow()
                  CALL DIALOG.insertRow(v_tbl_pendientes, v_indice_destino)
                  CALL DIALOG.setCurrentRow(v_tbl_pendientes, v_indice_destino)
                  LET v_folios_pend[v_indice_destino] = v_valor_arrastre
                  CALL DIALOG.deleteRow(v_tbl_elegidos, v_indice_arrastre)
               END IF
               CALL DIALOG.setActionHidden("reverso",1)
         END DISPLAY

         DISPLAY ARRAY v_folios_eleg TO tbl_elegidos.*
            # recupera datos de origen
            ON DRAG_START(v_ui_dnd)
               LET v_origen_arrastre = v_tbl_elegidos
               LET v_indice_arrastre = arr_curr()
               LET v_valor_arrastre = v_folios_eleg[v_indice_arrastre]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_origen_arrastre TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_origen_arrastre IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
                IF v_origen_arrastre == v_tbl_elegidos THEN
                    CALL v_ui_dnd.dropInternal()
                ELSE
                   IF v_folios_eleg.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      CALL fn_mensaje("Aviso","Solo se permite elegir un folio a la vez","stop")
                   ELSE 
                      LET v_indice_destino = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(v_tbl_elegidos, v_indice_destino)
                      CALL DIALOG.setCurrentRow(v_tbl_elegidos, v_indice_destino)
                      LET v_folios_eleg[v_indice_destino] = v_valor_arrastre
                      CALL DIALOG.deleteRow(v_tbl_pendientes, v_indice_arrastre)
                   END IF
                END IF
         END DISPLAY

         BEFORE DIALOG
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL fn_recupera_folios()

         ON ACTION ACCEPT
            # en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF ( v_folios_eleg.getLength() = 0 ) THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un folio a reversar","stop")
               CONTINUE DIALOG
            END IF

            CALL DIALOG.setActionHidden("reverso",0)
            CALL DIALOG.setActionHidden("cancelar",0)
            CALL DIALOG.setActionHidden("accept",1)
            CALL DIALOG.setActionHidden("close",1)
            CONTINUE DIALOG

         ON ACTION reverso
            # Valida si se puede ejecutar reverso para registro contable
            {CALL fn_reverso_reg_cnt(v_folio) RETURNING v_bnd_rev_cnt
            IF(v_bnd_rev_cnt <> 0)THEN
               LET v_mensaje = "registro contable realizado ",v_bnd_rev_cnt
               CALL fn_mensaje("Aviso","Reverso no procedente: "||v_mensaje CLIPPED,"information")
               CONTINUE DIALOG
            END IF}
            
            SELECT NVL(MAX(pid),0)
              INTO v_pid
              FROM bat_ctr_operacion
             WHERE proceso_cod = v_proceso_cod
               AND opera_cod = v_opera_cod
               AND folio = v_folios_eleg[1]

            DISPLAY v_pid
            IF(v_pid = 0)THEN
               CALL fn_mensaje("Aviso","No se encuentr� informaci�n del folio a reversar","information")
               CONTINUE DIALOG
            END IF
            # valida que se pueda ejecutar el reverso del proceso que se reversar�
            CALL fn_valida_reverso(v_pid,v_proceso_cod,v_opera_cod)RETURNING r_res_opera
            IF(r_res_opera <> 0)THEN
               CALL fn_muestra_inc_operacion(r_res_opera)
               CONTINUE DIALOG
            END IF
            
            CALL fn_ventana_confirma("Confimar","�Reversar folio?","info") RETURNING v_confirma
            IF ( v_confirma ) THEN
                
                # se invoca la funcion que valida ejecucion de reverso
                CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                
                # se verifica si fue posible inicializar la operacion
                IF ( r_res_opera = 0 ) THEN
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
                                              "SEPL64",
                                              "NA", 
                                              p_usuario_cod) 
                               RETURNING r_res_opera                        
                   IF ( r_res_opera  <> 0 ) THEN
                      # Imprime el mensaje de inconsistencia en pantalla
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      EXIT DIALOG
                   END IF

                   CALL fn_actualiza_opera_ini(v_pid_rev,         # pid que realiza el reverso
                                               v_proceso_cod_rev, # proceso que realiza el reverso
                                               v_opera_cod_rev,   # operacion que realiza el reverso
                                               v_folio,           # folio
                                               "SEPL64",          # programa
                                               "NA",              # archivo
                                               p_usuario_cod)     # usuario
                            RETURNING r_res_opera
                   IF ( r_res_opera <> 0 ) THEN
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                      EXIT DIALOG
                   END IF
                   
                   -- se construye el comando para ser ejecutado el reverso
                   LET v_comando = "nohup fglrun ",v_ruta_bin CLIPPED,"/SEPR64.42r ",
                                                   p_usuario_cod CLIPPED, " ",
                                                   v_pid_rev            , " ",
                                                   v_proceso_cod_rev    , " ",
                                                   v_opera_cod_rev      , " ",
                                                   v_folio              , " '",
                                                   v_archivo CLIPPED    ,
                                           "' 1>", v_ruta_lst CLIPPED,
                                         "/nohup:",v_pid_rev         USING "&&&&&",":",
                                                   v_proceso_cod_rev USING "&&&&&",":",
                                                   v_opera_cod_rev   USING "&&&&&",
                                           " 2>&1 &"
                   
                   DISPLAY v_comando
                   RUN v_comando
                   IF ( STATUS ) THEN
                      CALL fn_mensaje(p_titulo,"Ocurri� un error al ejecutar el reverso","about")
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                   ELSE
                      CALL fn_mensaje(p_titulo,"Se ha enviado la operaci�n.\nPodr� revisar el detalle en el monitoreo de procesos","about")
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
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)           
            CONTINUE DIALOG

         ON ACTION cancelar
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)

            CALL v_folios_pend.clear()
            CALL v_folios_eleg.clear()
            CALL fn_recupera_folios()
            --LET v_folios_pend[1] = v_folio
            CONTINUE DIALOG
         
         ON ACTION close
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW vtna_reversa_folio

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL63                                                   #
#Descripcion       => Funcion que recupera los los folios en estado            #
#                     preliquidado                                             #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 31 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_recupera_folios()
DEFINE v_consulta STRING,
       v_indice   INTEGER,
       v_folio    LIKE glo_folio.folio

   LET v_indice = 1
   CALL v_folios_pend.clear()
   LET v_consulta = "\n SELECT MAX(folio)",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
                    --"\n    AND opera_cod = ?",
                    "\n    AND status = 2" # folio preliquidado
                    
   PREPARE prp_rec_folios FROM v_consulta
   DECLARE cur_rec_folios CURSOR FOR prp_rec_folios
   FOREACH cur_rec_folios USING v_proceso_cod
                                --v_opera_cod
                           INTO v_folio
      LET v_folios_pend[v_indice] = v_folio
      LET v_indice = v_indice + 1
                           
   END FOREACH
   FREE cur_rec_folios

END FUNCTION