--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 25-10-2012
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL54                                                   #
#Objetivo          => Reverso integración de compensación deudor               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################

DATABASE safre_viv

GLOBALS "SEPG02.4gl"

DEFINE v_ventana       ui.Window,
       v_forma         ui.Form,
       p_usuario_cod   LIKE seg_usuario.usuario, # usuario logeado
       p_tipo_carga    SMALLINT,   # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo        VARCHAR(30), # nombre de la ventana
       v_det_deudor    DYNAMIC ARRAY OF RECORD
         v_num          INTEGER,
         v_nss          LIKE sep_deudor.nss,
         v_credito      LIKE sep_deudor.num_credito,
         v_periodo_pago LIKE sep_deudor.periodo_pago
       END RECORD

MAIN
   # Parámetros del menu
   LET p_usuario_cod = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo      = ARG_VAL(3)

   CALL fn_reversa_int_compensacion_deudor()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL54                                                   #
#Descripcion       => Ejecuta el SP que realiza el reverso de la integración   #
#                     de compensacion                                          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_reversa_int_compensacion_deudor()
DEFINE v_folios_pend   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios pendientes 
       v_folios_eleg   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios a reversar
       v_det_folio     DYNAMIC ARRAY OF RECORD  # arreglo que contiene el detalle
         tot_pareja    INTEGER,
         tot_nrp_inv   INTEGER,
         tot_nrp_aso   INTEGER,
         tot_registro  INTEGER
       END RECORD,
       v_proceso_cod_rev LIKE cat_proceso.proceso_cod, # proceso que realiza el reverso
       v_opera_cod_rev   LIKE cat_operacion.opera_cod, # operación que realiza el reverso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # proceso a reversar
       v_opera_cod       LIKE cat_operacion.opera_cod, # operación a reversar
       v_indice        SMALLINT, # indice del arrego de folios pendientes
       v_programa_cod  LIKE bat_ctr_operacion.programa_cod, # nombre del programa que ejecuta el reverso
       v_consulta      STRING, # variable para consultas SQL
       v_pid           LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_pid_rev       LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_ruta_bin      LIKE seg_modulo.ruta_bin, # ruta ejecutable sep
       v_ruta_lst      LIKE seg_modulo.ruta_listados, # ruta listados de bat
       v_ruta_vacia    STRING,
       v_confirma      BOOLEAN,
       v_folio         INTEGER, # folio a reversar
       v_comando       STRING,  # variable para contener comando para ejecutar batch
       v_archivo       LIKE glo_ctr_archivo.nombre_archivo,# nombre del archivo en proceso
       r_res_opera     SMALLINT,  -- booleana que indica si el proceso se puede ejecutar o no
       
       v_dragdrop        UI.DRAGDROP,
       v_arrastra_origen STRING,
       v_indice_arrastre INTEGER,
       v_indice_suelta   INTEGER,
       v_valor_arrastre  STRING
       
       CONSTANT v_tbl_pendientes = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
       CONSTANT v_tbl_elegidos = "tbl_elegidos" -- se asigna el nombre de la tabla de archivos a integrar
       
   # se inicializa el indice del arreglo
   LET v_indice = 1

   # se asignan los valores necesarios para el reverso
   LET v_proceso_cod_rev = v_proc_reverso_int_compensacion_deudor  # reverso de integracion compensación deudor
   LET v_opera_cod_rev   = v_opera_reverso_int_compensacion_deudor # reverso de integracion compensación deudor
   LET v_proceso_cod = v_proc_compensacion_deudor          # compensación deudor
   LET v_opera_cod   = v_opera_integra_compensacion_deudor # integracion de compensación deudor

   
   LET v_folio = 0      # Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_programa_cod = "SEPL54"
   
   # se invoca la funcion que valida ejecucion de reverso
   CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
   # se verifica si la operacion en proceso es valida
   IF(r_res_opera <> 0)THEN
      # en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_res_opera)
      EXIT PROGRAM
   END IF

   ###
   # valida que se pueda ejecutar reveros de la operación a reversar
   ###
   # recuepra el pid del proceso a reversar
   CALL fn_max_pid(v_proceso_cod,v_opera_cod) RETURNING v_pid
   
   # valida que se pueda ejecutar el reverso
   CALL fn_valida_reverso(v_pid,v_proceso_cod,v_opera_cod)RETURNING r_res_opera
   IF(r_res_opera <> 0)THEN
      CALL fn_muestra_inc_operacion(r_res_opera)
      EXIT PROGRAM
   END IF
   
   
   # se obtienen las rutas de separacion
   CALL fn_rutas("sep") RETURNING v_ruta_bin, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_lst
   
   OPEN WINDOW vtna_reversa_folio WITH FORM v_ruta_bin CLIPPED||"/SEPL541"
      # se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF ( p_titulo IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_titulo)         
         CALL v_ventana.setText(p_titulo)
      END IF
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_folios_pend TO tbl_pendientes.*
         --DISPLAY ARRAY v_archivos_pendientes TO sr_pendientes.*

            ON DRAG_START(v_dragdrop)
               # se recupera tabla origen de arrastre
               LET v_arrastra_origen = v_tbl_pendientes
               # se recupera el indice de la tabla origen de arrastre
               LET v_indice_arrastre = ARR_CURR()
               # se recupera el valor de la tabla origen de arrastre
               LET v_valor_arrastre = v_folios_pend[v_indice_arrastre]
   
            ON DRAG_FINISHED(v_dragdrop)
               # se indica que no hay tabla origen
               INITIALIZE v_arrastra_origen TO NULL
   
            ON DRAG_ENTER(v_dragdrop)
               IF v_arrastra_origen IS NULL THEN
                  CALL v_dragdrop.setOperation(NULL)
               END IF

            ON DROP(v_dragdrop)
               IF(v_arrastra_origen == v_tbl_pendientes)THEN
                  CALL v_dragdrop.dropInternal()
               ELSE
                  # Se recupera el indice donde se soltó el archivo
                  LET v_indice_suelta = v_dragdrop.getLocationRow()
                  # Se inserta el archivo en el indice que se recuperó
                  CALL DIALOG.insertRow(v_tbl_pendientes, v_indice_suelta)
                  # se establese el foco en la tabla destino
                  CALL DIALOG.setCurrentRow(v_tbl_pendientes, v_indice_suelta)
                  # se agrega al arreglo el nomre del archivo
                  LET v_folios_pend[v_indice_suelta] = v_valor_arrastre
                  # elimina el registro de la tabla origen
                  CALL DIALOG.deleteRow(v_tbl_elegidos, v_indice_arrastre)
               END IF
            
         END DISPLAY
   
         --DISPLAY ARRAY v_archivos_elegidos TO sr_elegidos.*
         DISPLAY ARRAY v_folios_eleg TO tbl_elegidos.*
            ON DRAG_START(v_dragdrop)
               # se recupera tabla origen de arrastre
               LET v_arrastra_origen = v_tbl_elegidos
               # se recupera el indice de la tabla origen de arrastre
               LET v_indice_arrastre = ARR_CURR()
               # se recupera el valor de la tabla origen de arrastre
               LET v_valor_arrastre = v_folios_eleg[v_indice_arrastre]
  
            ON DRAG_FINISHED(v_dragdrop)
               # se indica que no hay tabla origen
               INITIALIZE v_arrastra_origen TO NULL

            ON DRAG_ENTER(v_dragdrop)
               IF( v_arrastra_origen IS NULL )THEN
                  CALL v_dragdrop.setOperation(NULL)
               END IF
   
            ON DROP(v_dragdrop)
                IF( v_arrastra_origen == v_tbl_elegidos )THEN                       
                    CALL v_dragdrop.dropInternal()
                ELSE
                    # Se recupera el indice donde se soltó el archivo
                    LET v_indice_suelta = v_dragdrop.getLocationRow()
                    # Se inserta el archivo en el indice que se recuperó
                    CALL DIALOG.insertRow(v_tbl_elegidos, v_indice_suelta)
                    # se establese el foco en la tabla destino
                    CALL DIALOG.setCurrentRow(v_tbl_elegidos, v_indice_suelta)
                    # se agrega al arreglo el nomre del archivo
                    LET v_folios_eleg[v_indice_suelta] = v_valor_arrastre
                    # elimina el registro de la tabla origen
                    CALL DIALOG.deleteRow(v_tbl_pendientes, v_indice_arrastre)
                END IF

         END DISPLAY
         DISPLAY ARRAY v_det_deudor TO tbl_detalle.*

         END DISPLAY

         BEFORE DIALOG
            # recupera el ultimo folio procesado
            SELECT MAX(folio)
              INTO v_folio
              FROM  glo_folio
             WHERE proceso_cod = v_proceso_cod
               AND opera_cod = v_opera_cod  
            # solo se procesa el ultimo folio de op 27
            LET v_folios_pend[1] = v_folio
            CALL v_forma.setElementHidden("gpo_det",1)
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)

         ON ACTION ACCEPT
            # en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF(v_folios_eleg.getLength() = 0)THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un folio a reversar","stop")
               CONTINUE DIALOG
            END IF
            
            CALL fn_recupera_detalle_dedudor(v_folios_eleg[1])
            CALL v_forma.setElementHidden("gpo_det",0)
            CALL DIALOG.setActionHidden("reverso",0)
            CALL DIALOG.setActionHidden("cancelar",0)
            CALL DIALOG.setActionHidden("accept",1)
            CALL DIALOG.setActionHidden("close",1)
            
            CONTINUE DIALOG

         ON ACTION reverso            
            # en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF(v_folios_eleg.getLength() = 0)THEN
               CALL fn_mensaje("Aviso","No se ha seleccionado folio a reversar","stop")
               CONTINUE DIALOG
            END IF

            CALL fn_ventana_confirma("Confimar","Reversar Folio?","information") RETURNING v_confirma
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
                                              "SEPL54",
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
                                               v_folio,           # folio
                                               "SEPL54",          # programa
                                               "NA",              # archivo
                                               p_usuario_cod)     # usuario
                            RETURNING r_res_opera
                   IF(r_res_opera <> 0)THEN
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                      EXIT DIALOG
                   END IF
                   
                   LET v_comando = "nohup fglrun ",v_ruta_bin CLIPPED,"/SEPR54.42r ",
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
                      CALL fn_mensaje(p_titulo,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos para el PID "||v_pid_rev,"about")
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
            
         ON ACTION cancelar
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)
            CALL v_det_folio.clear()
            CALL v_forma.setElementHidden("gpo_det",1)
            CALL v_folios_pend.clear()
            CALL v_folios_eleg.clear()
            
            LET v_folios_pend[1] = v_folio
            CONTINUE DIALOG
         
         ON ACTION close
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW vtna_reversa_folio

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL54                                                   #
#Descripcion       => Recupera detalle de deudor                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Noviembre 2012                                        #
################################################################################
FUNCTION fn_recupera_detalle_dedudor(p_folio)
DEFINE p_folio    LIKE sep_deudor.folio,
       v_consulta STRING,
       v_det_deudor_aux RECORD
         v_nss          LIKE sep_deudor.nss,
         v_credito      LIKE sep_deudor.num_credito,
         v_periodo_pago LIKE sep_deudor.periodo_pago
       END RECORD,
       v_indice INTEGER

   LET v_indice = 1
   CALL v_det_deudor.clear()
   LET v_consulta = "\n SELECT nss,",
                    "\n        num_credito,",
                    "\n        periodo_pago",
                    "\n   FROM sep_deudor",
                    "\n  WHERE folio = ?",
                    "\n  GROUP BY 1,2,3"
   PREPARE prp_rec_det_deudor FROM v_consulta
   DECLARE cur_rec_det_deudor CURSOR FOR prp_rec_det_deudor
   FOREACH cur_rec_det_deudor USING p_folio
                               INTO v_det_deudor_aux.*
      LET v_det_deudor[v_indice].v_num          = v_indice 
      LET v_det_deudor[v_indice].v_nss          = v_det_deudor_aux.v_nss
      LET v_det_deudor[v_indice].v_credito      = v_det_deudor_aux.v_credito
      LET v_det_deudor[v_indice].v_periodo_pago = v_det_deudor_aux.v_periodo_pago
      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_rec_det_deudor

END FUNCTION