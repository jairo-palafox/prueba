--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/04/2015
--==============================================================================
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTL08                                                   #
#Descripcion       => Preliquidación de traspasos de saldos subsecuentes       #
#                     receptora                                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 06 Abril 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"

DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion INTEGER,
       p_cad_ventana   STRING,
       g_pid           LIKE glo_pid.pid,
       v_ventana       ui.Window,
       v_forma         ui.Form,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_dragdrop        UI.DRAGDROP, # manejador de arrastrar y soltar (drag and drop)
       r_resultado_opera SMALLINT

MAIN
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   # Recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   CALL fn_inicializa_consultas()
   
   CALL fn_recupera_folio_traspasos_receptora()

END MAIN

# Descripción: Inicializa consultas sql
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT MAX(pid)",
                    "   FROM bat_ctr_proceso",
                    "  WHERE proceso_cod = ?",
                    "    AND estado_cod = 2" # procesando
   PREPARE prp_recupera_pid FROM v_consulta

   LET v_consulta = " SELECT folio_liquida",
                    "   FROM prt_cza_receptora",
                    "  WHERE estado = ?",
                    "    AND tipo_traspaso = ?"
                    --"  GROUP BY folio"
   PREPARE prp_rec_folios_pendientes FROM v_consulta

   LET v_consulta = " SELECT CASE tpo_operacion WHEN '01' THEN 'TRANSFERENCIA TOTAL' WHEN '02' THEN 'TRANSFERENCIA PARCIAL' ELSE 'NO CLASIFICADO'  END CASE,",
                    "        SUM(mto_pesos_fov2008)",
                    "   FROM prt_traspaso_receptora",
                    "  WHERE folio_liquida = ?",
                    "  GROUP BY 1"
   PREPARE prp_cifras_preliquidar FROM v_consulta 

END FUNCTION

# Descripción: Recupera folio a preliquidar
FUNCTION fn_recupera_folio_traspasos_receptora()
DEFINE r_valida          SMALLINT,
       v_arrastra_origen STRING,
       v_indice_arrastre INTEGER,
       v_indice_suelta   INTEGER,
       v_valor_arrastre  STRING,
       v_continua        BOOLEAN,
       v_folios_pendientes DYNAMIC ARRAY OF STRING,
       v_folios_elegidos DYNAMIC ARRAY OF STRING,
       v_folio_preliquidar STRING

CONSTANT v_sr_pendientes = "sr_pendientes" # Se asigna el nombre de la tabla de archivos pendientes
CONSTANT v_sr_elegido    = "sr_elegido" # Se asigna el nombre de la tabla de archivos elegidos

   OPEN WINDOW vtna_preliq_trasp_subsec_receptora WITH FORM v_ruta_ejecutable CLIPPED||"/PRTL081"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      DIALOG ATTRIBUTE(UNBUFFERED)

         DISPLAY ARRAY v_folios_pendientes TO sr_pendientes.*

            ON DRAG_START(v_dragdrop)
               # se recupera tabla origen de arrastre
               LET v_arrastra_origen = v_sr_pendientes
               # se recupera el indice de la tabla origen de arrastre
               LET v_indice_arrastre = ARR_CURR()
               # se recupera el valor de la tabla origen de arrastre
               LET v_valor_arrastre = v_folios_pendientes[v_indice_arrastre]
   
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
                  LET v_folios_pendientes[v_indice_suelta] = v_valor_arrastre
                  # elimina el registro de la tabla origen
                  CALL DIALOG.deleteRow(v_sr_elegido, v_indice_arrastre)
               END IF
            
         END DISPLAY
   
         DISPLAY ARRAY v_folios_elegidos TO sr_elegido.*
            ON DRAG_START(v_dragdrop)
               # se recupera tabla origen de arrastre
               LET v_arrastra_origen = v_sr_elegido
               # se recupera el indice de la tabla origen de arrastre
               LET v_indice_arrastre = ARR_CURR()
               # se recupera el valor de la tabla origen de arrastre
               LET v_valor_arrastre = v_folios_elegidos[v_indice_arrastre]
   
            ON DRAG_FINISHED(v_dragdrop)
               # se indica que no hay tabla origen
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
                  LET v_folios_elegidos[v_indice_suelta] = v_valor_arrastre
                  # elimina el registro de la tabla origen
                  CALL DIALOG.deleteRow(v_sr_pendientes, v_indice_arrastre)
               END IF

         END DISPLAY

         BEFORE DIALOG
            EXECUTE prp_recupera_pid USING C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA
                                      INTO g_pid
            CALL fn_valida_operacion(g_pid,
                                     C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA,
                                     C_OPERA_COD_PRELIQUIDACION) RETURNING r_valida
            # Se verifica si la operacion es valida
            IF(r_valida <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_valida)
               EXIT DIALOG
            END IF
            # se ocultan las tablas del detalle del archivo
            CALL v_forma.setElementHidden("gpo_saldos_preliquidar",1)
               
            # Se recuperan los archivos pendientes
            CALL fn_recupera_folios_pendientes() RETURNING v_folios_pendientes
            IF(v_folios_pendientes.getLength() = 0)THEN
               CALL fn_mensaje(p_cad_ventana,"No hay información para preliquidar","about")
               EXIT DIALOG
            END IF              
                  
         # botones para elegir archivo
         ON ACTION aceptar
            # se valida que se haya seleccionado un archivo
            IF(v_folios_elegidos.getLength() = 0)THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un folio a integrar","info")
               CONTINUE DIALOG
            END IF
            # se valida que solo se integre un archivo
            IF(v_folios_elegidos.getLength() > 1)THEN
               CALL fn_mensaje("Aviso","Solo puede seleccionar un folio","info")
               CONTINUE DIALOG
            END IF
            LET v_folio_preliquidar = v_folios_elegidos[1]
            
            
            
            # Se muestra las tablas del detalle del archivo
            CALL v_forma.setElementHidden("gpo_saldos_preliquidar",FALSE)

            # Llama funcion que muestra detalles del archivo a preliquidar
            CALL fn_preliquida_traspasos(v_folio_preliquidar) RETURNING v_continua
            IF( v_continua )THEN
               CALL v_forma.setElementHidden("gpo_saldos_preliquidar",TRUE)
               CONTINUE DIALOG
            ELSE
               ACCEPT DIALOG
            END IF

         ON ACTION cancelar
            EXIT DIALOG

      END DIALOG

   CLOSE WINDOW vtna_preliq_trasp_subsec_receptora

END FUNCTION

# Descripción: Recupera folios pendientes de preliquidar
FUNCTION fn_recupera_folios_pendientes()
DEFINE v_folio    LIKE glo_folio.folio,
       v_indice   SMALLINT,
       v_folios_pendientes DYNAMIC ARRAY OF STRING

   LET v_indice = 1
   CALL v_folios_pendientes.clear()
   DECLARE cur_recupera_pendientes CURSOR FOR prp_rec_folios_pendientes
   FOREACH cur_recupera_pendientes USING C_ESTADO_TRASPASO_INTEGRADA,
                                         C_TIPO_TRASPASO_SUBSECUENTE
                                    INTO v_folio
      LET v_folios_pendientes[v_indice] = v_folio
      LET v_indice = v_indice + 1
   END FOREACH
   
   FREE cur_recupera_pendientes

   RETURN v_folios_pendientes
END FUNCTION

# Descripción: Recupera detalle de folio a preliquidar
FUNCTION fn_recupera_detalle(p_folio_preliquidar)
DEFINE p_folio_preliquidar LIKE glo_folio.folio,
       v_indice            INTEGER,
       v_saldos_preliquidar_aux RECORD
         v_tpo_operacion VARCHAR(50),
         v_monto_pesos   LIKE prt_traspaso_receptora.mto_pesos_fov2008
       END RECORD,
       v_saldos_preliquidar DYNAMIC ARRAY OF RECORD
         v_tpo_operacion VARCHAR(50),
         v_monto_pesos   LIKE prt_traspaso_receptora.mto_pesos_fov2008
       END RECORD

   LET v_indice = 1
   
   DECLARE cur_cifras_preliquidar CURSOR FOR prp_cifras_preliquidar
   FOREACH cur_cifras_preliquidar USING p_folio_preliquidar 
                                   INTO v_saldos_preliquidar_aux.*
      LET v_saldos_preliquidar[v_indice].v_tpo_operacion = v_saldos_preliquidar_aux.v_tpo_operacion
      LET v_saldos_preliquidar[v_indice].v_monto_pesos   = v_saldos_preliquidar_aux.v_monto_pesos USING "##,###,##&.&&"
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_cifras_preliquidar

   RETURN v_saldos_preliquidar
END FUNCTION

# Descripción: Muestra montos de preliquidación
FUNCTION fn_preliquida_traspasos(p_folio_preliquidar)
DEFINE p_folio_preliquidar STRING,
       v_continua BOOLEAN,
       r_confirma BOOLEAN,
       v_cifras_preliquidar DYNAMIC ARRAY OF RECORD
          v_tpo_operacion VARCHAR(50),
          v_monto_pesos   LIKE prt_traspaso_receptora.mto_pesos_fov2008
       END RECORD,
       r_error BOOLEAN
       
   # suponiendo que se realiza la integracion
   LET v_continua = FALSE
   
   # se despliega los posibles saldos a separar
   DISPLAY ARRAY v_cifras_preliquidar TO sr_cifras.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

      BEFORE DISPLAY
         CALL v_cifras_preliquidar.clear()
         CALL fn_recupera_detalle(p_folio_preliquidar) RETURNING v_cifras_preliquidar
         IF( v_cifras_preliquidar.getLength() = 0 )THEN
            CALL fn_mensaje(p_cad_ventana,"No se encontraron registros","information")
            LET v_continua = TRUE
            EXIT DISPLAY
         END IF
         
      # botones para integrar archivo
      ON ACTION preliquidar
         CALL fn_ventana_confirma(p_cad_ventana,"¿Preliquidar Folio?","question") RETURNING r_confirma
         IF( r_confirma )THEN
            CALL fn_valida_operacion(g_pid,
                                     C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA,
                                     C_OPERA_COD_PRELIQUIDACION) RETURNING r_resultado_opera
            IF( r_resultado_opera <> 0 )THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
            ELSE
               CALL fn_ejecuta_preliquidacion_traspaso_receptora(p_folio_preliquidar) RETURNING r_error
               IF( r_error )THEN
                  LET v_continua = TRUE
               END IF
               CALL fn_mensaje(p_cad_ventana,"Se ha iniciado la operación.\nProdrá revisar el detalle en el monitor de procesos","information")
               ACCEPT DISPLAY
            END IF
         END IF
         CONTINUE DISPLAY
            
      ON ACTION cancelar
         # indica que regresa a la pantalla para elegir archivo
         LET v_continua = TRUE
         EXIT DISPLAY
   
   END DISPLAY

   RETURN v_continua
END FUNCTION

# Descripción: Ejecuta el programa que realiza la preliquidación
FUNCTION fn_ejecuta_preliquidacion_traspaso_receptora(p_folio_preliquidar)
DEFINE p_folio_preliquidar STRING, 
       v_comando          STRING,
       v_ruta_listados    LIKE seg_modulo.ruta_listados,
       v_ruta_bin_aux     LIKE seg_modulo.ruta_bin,
       v_error            BOOLEAN

   LET v_error = FALSE
   
   CALL fn_actualiza_opera_ini(g_pid,
                               C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA,
                               C_OPERA_COD_PRELIQUIDACION,
                               p_folio_preliquidar,
                               'PRTL08',
                               '',
                               p_usuario_cod) RETURNING r_resultado_opera
   IF( r_resultado_opera <> 0 )THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      LET v_error = TRUE
   ELSE
      CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
   
      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PRTE08.42r ",p_usuario_cod, " ",
                                                                               g_pid, " ",
                                                                               C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA," ",
                                                                               C_OPERA_COD_PRELIQUIDACION," ",
                                                                               p_folio_preliquidar,
                                                                               " NA",
                      " 1>", v_ruta_listados CLIPPED,"/nohup:",g_pid USING "&&&&&",":",
                                                               C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA USING "&&&&&",":",
                                                               C_OPERA_COD_PRELIQUIDACION USING "&&&&&",
                      " 2>&1 &"
      RUN v_comando
      IF(STATUS)THEN
         CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la preliquidación","information")
         LET v_error = TRUE
      END IF
   END IF

   RETURN v_error
END FUNCTION