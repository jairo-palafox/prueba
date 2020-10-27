--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/03/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTL02                                                        #
#Objetivo     => Integración de transferencia de saldos receptora              #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio => 19 Marzo 2015                                                 #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"

DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion INTEGER,
       p_cad_ventana   STRING,
       g_pid           LIKE glo_pid.pid,
       v_ventana       ui.Window,
       v_forma         ui.Form,       
       v_dragdrop      UI.DRAGDROP, # manejador de arrastrar y soltar (drag and drop)
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados
       
MAIN
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_bin          
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"

   CALL fn_inicializa_consultas()
   CALL fn_integra_traspaso_saldos_receptora()

END MAIN

# Descripción: Inicializa consultas sql
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT MAX(pid)",
                    "   FROM bat_ctr_proceso",
                    "  WHERE proceso_cod = ?",
                    "    AND estado_cod = 2" # procesando
   PREPARE prp_recupera_pid FROM v_consulta

   # Recupera archivos pendientes de integrar
   LET v_consulta = " SELECT nombre_archivo",
                    "   FROM glo_ctr_archivo",
                    "  WHERE proceso_cod = ?",
                    "    AND opera_cod = ?",
                    "    AND estado = 1" # Cargado
   PREPARE prp_recupera_pendientes FROM v_consulta

END FUNCTION

# Descripción: 
FUNCTION fn_integra_traspaso_saldos_receptora()
DEFINE v_archivos_pendientes DYNAMIC ARRAY OF STRING,
       v_archivos_elegidos   DYNAMIC ARRAY OF STRING,
       r_valida          SMALLINT,
       v_arrastra_origen STRING,
       v_indice_arrastre INTEGER,
       v_indice_suelta   INTEGER,
       v_valor_arrastre  STRING,
       v_can_int         BOOLEAN,
       v_renglon_actual  INTEGER,
       r_error           BOOLEAN
CONSTANT v_sr_pendientes = "sr_pendientes" # Se asigna el nombre de la tabla de archivos pendientes
CONSTANT v_sr_elegido    = "sr_elegido" # Se asigna el nombre de la tabla de archivos elegidos

   OPEN WINDOW vtna_integra_trasp_receptora WITH FORM v_ruta_ejecutable CLIPPED||"/PRTL021"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF( p_cad_ventana IS NOT NULL )THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      DIALOG ATTRIBUTE(UNBUFFERED)

         DISPLAY ARRAY v_archivos_pendientes TO sr_pendientes.*

            ON DRAG_START(v_dragdrop)
               # se recupera tabla origen de arrastre
               LET v_arrastra_origen = v_sr_pendientes
               # se recupera el indice de la tabla origen de arrastre
               LET v_indice_arrastre = ARR_CURR()
               # se recupera el valor de la tabla origen de arrastre
               LET v_valor_arrastre = v_archivos_pendientes[v_indice_arrastre]
   
            ON DRAG_FINISHED(v_dragdrop)
               # se indica que no hay tabla origen
               INITIALIZE v_arrastra_origen TO NULL
   
            ON DRAG_ENTER(v_dragdrop)
               IF( v_arrastra_origen IS NULL )THEN
                  CALL v_dragdrop.setOperation(NULL)
               END IF
   
            ON DROP(v_dragdrop)
               IF( v_arrastra_origen == v_sr_pendientes )THEN
                  CALL v_dragdrop.dropInternal()
               ELSE
                  # Se recupera el indice donde se soltó el archivo
                  LET v_indice_suelta = v_dragdrop.getLocationRow()
                  # Se inserta el archivo en el indice que se recuperó
                  CALL DIALOG.insertRow(v_sr_pendientes, v_indice_suelta)
                  # se establese el foco en la tabla destino
                  CALL DIALOG.setCurrentRow(v_sr_pendientes, v_indice_suelta)
                  # se agrega al arreglo el nomre del archivo
                  LET v_archivos_pendientes[v_indice_suelta] = v_valor_arrastre
                  # elimina el registro de la tabla origen
                  CALL DIALOG.deleteRow(v_sr_elegido, v_indice_arrastre)
               END IF
            
         END DISPLAY
   
         DISPLAY ARRAY v_archivos_elegidos TO sr_elegido.*
            ON DRAG_START(v_dragdrop)
               # se recupera tabla origen de arrastre
               LET v_arrastra_origen = v_sr_elegido
               LET v_indice_arrastre = ARR_CURR()
               # se recupera el valor de la tabla origen de arrastre
               LET v_valor_arrastre = v_archivos_elegidos[v_indice_arrastre]
   
            ON DRAG_FINISHED(v_dragdrop)
               INITIALIZE v_arrastra_origen TO NULL
   
            ON DRAG_ENTER(v_dragdrop)
               IF( v_arrastra_origen IS NULL )THEN
                  CALL v_dragdrop.setOperation(NULL)
               END IF
   
            ON DROP(v_dragdrop)
               IF( v_arrastra_origen == v_sr_elegido )THEN                       
                  CALL v_dragdrop.dropInternal()
               ELSE
                  # Se recupera el indice donde se soltó el archivo
                  LET v_indice_suelta = v_dragdrop.getLocationRow()
                  # Se inserta el archivo en el indice que se recuperó
                  CALL DIALOG.insertRow(v_sr_elegido, v_indice_suelta)
                  # se establese el foco en la tabla destino
                  CALL DIALOG.setCurrentRow(v_sr_elegido, v_indice_suelta)
                  # se agrega al arreglo el nomre del archivo
                  LET v_archivos_elegidos[v_indice_suelta] = v_valor_arrastre
                  # elimina el registro de la tabla origen
                  CALL DIALOG.deleteRow(v_sr_pendientes, v_indice_arrastre)
               END IF

         END DISPLAY

         BEFORE DIALOG
            EXECUTE prp_recupera_pid USING C_PROCESO_COD_TRANS_SDO_RECEPTORA
                                      INTO g_pid

            CALL fn_valida_operacion(g_pid,
                                     C_PROCESO_COD_TRANS_SDO_RECEPTORA,
                                     C_OPERA_COD_INTEGRACION) RETURNING r_valida
            # Se verifica si la operacion es valida
            IF( r_valida <> 0 )THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_valida)
               EXIT DIALOG
            END IF
               
            LET v_can_int = FALSE
            # Se recuperan los archivos pendientes
            CALL fn_recupera_archivos_pendientes() RETURNING v_archivos_pendientes

            CALL DIALOG.setActionHidden("integrar",1)
                  
         # botones para elegir archivo
         ON ACTION aceptar
            # se valida que se haya seleccionado un archivo
            IF(v_archivos_elegidos.getLength() = 0)THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un archivo a integrar","info")
               CONTINUE DIALOG
            END IF
            # se valida que solo se integre un archivo
            IF( v_archivos_elegidos.getLength() > 1 )THEN
               CALL fn_mensaje("Aviso","Solo puede seleccionar un archivo","info")
               CONTINUE DIALOG
            END IF
            LET v_can_int = TRUE          
            CALL DIALOG.setActionHidden("integrar",0)
            CALL DIALOG.setActionHidden("aceptar",1)

         # botones para integrar archivo
         ON ACTION integrar
            LET v_renglon_actual = DIALOG.getcurrentRow(v_sr_elegido)
            CALL fn_ejecuta_integracion_diagnostico(v_archivos_elegidos[v_renglon_actual])RETURNING r_error
            IF( r_error )THEN
               CONTINUE DIALOG
            ELSE
               CALL fn_mensaje(p_cad_ventana,"Se ha iniciado la operación.\nProdrá revisar el detalle en el monitor de procesos","information")
               ACCEPT DIALOG
            END IF
            
         ON ACTION cancelar
            IF(v_can_int)THEN
               # regresa a la seleccion del archivo
               CALL DIALOG.setActionHidden("integrar",1)
               CALL DIALOG.setActionHidden("aceptar",0)
               LET v_archivos_pendientes[1] = v_archivos_elegidos[1]
               CALL v_archivos_elegidos.clear()
               LET v_can_int = FALSE
               CONTINUE DIALOG
            ELSE
               EXIT DIALOG
            END IF

      END DIALOG

   CLOSE WINDOW vtna_integra_trasp_receptora

END FUNCTION

# Descripción: Recupera los archivos a integrar
FUNCTION fn_recupera_archivos_pendientes()
DEFINE v_archivos_pendientes DYNAMIC ARRAY OF STRING,
       v_archivo  LIKE glo_ctr_archivo.nombre_archivo,
       v_indice   SMALLINT

   LET v_indice = 1
   DECLARE cur_recupera_pendientes CURSOR FOR prp_recupera_pendientes
   FOREACH cur_recupera_pendientes USING C_PROCESO_COD_TRANS_SDO_RECEPTORA,
                                         C_OPERA_COD_CARGA
                                    INTO v_archivo
      LET v_archivos_pendientes[v_indice] = v_archivo
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_pendientes
   
   RETURN v_archivos_pendientes
END FUNCTION

# Descripción: Ejecuta batch de integración
FUNCTION fn_ejecuta_integracion_diagnostico(p_archivo)
DEFINE p_archivo  STRING,
       v_consulta STRING,
       r_resultado_opera SMALLINT, # bandera de resultado de cambio de estatus
       v_error           BOOLEAN,
       r_folio           LIKE glo_folio.folio

   # inicializa suponiendo que ejecuta correctamente
   LET v_error = FALSE
      
   CALL fn_valida_operacion(g_pid,
                            C_PROCESO_COD_TRANS_SDO_RECEPTORA,
                            C_OPERA_COD_INTEGRACION) RETURNING r_resultado_opera
     
   IF( r_resultado_opera <> 0 )THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # Indica que ocurrió error
      RETURN v_error
   END IF

   # Folio se genera en lanzado
   LET r_folio = 0
   CALL fn_actualiza_opera_ini(g_pid,
                               C_PROCESO_COD_TRANS_SDO_RECEPTORA,
                               C_OPERA_COD_INTEGRACION,
                               r_folio,
                               "PRTL02",
                               p_archivo,
                               p_usuario_cod) RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      RETURN v_error
   END IF
   
   LET v_consulta = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PRTE02.42r '",p_usuario_cod CLIPPED, "' ",
                                                                              g_pid CLIPPED, " ",
                                                                              C_PROCESO_COD_TRANS_SDO_RECEPTORA CLIPPED," ",
                                                                              C_OPERA_COD_INTEGRACION CLIPPED," ",
                                                                              r_folio CLIPPED, " '",
                                                                              p_archivo CLIPPED,
                     "' 1>", v_ruta_listados CLIPPED,"/nohup:",g_pid USING "&&&&&",":",
                                                               C_PROCESO_COD_TRANS_SDO_RECEPTORA USING "&&&&&",":",
                                                               C_OPERA_COD_INTEGRACION USING "&&&&&"," 2>&1 &"
   TRY
      RUN v_consulta
   CATCH
      IF(STATUS)THEN
         LET v_error = TRUE
         CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la integración","information")
      ELSE
         CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","information")
         LET v_error = FALSE
      END IF
   END TRY
   
   RETURN v_error
END FUNCTION