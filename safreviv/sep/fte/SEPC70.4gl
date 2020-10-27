--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08/12/2014
--==============================================================================

################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPC70                                                        #
#Objetivo     => Consulta balance de deudor                                    #
#Fecha inicio => 08 Diciembre 2014                                             #
################################################################################

DATABASE safre_viv

PRIVATE DEFINE 
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ventana         ui.Window,
       v_forma           ui.Form

MAIN
   # recupera parámetros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   # Recupera la ruta ejecutable y listados del módulo
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_listados

   CALL fn_inicializa_consultas()
   CALL fn_consulta_folio_lote()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => Inicializa consultas SQL                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Diciembre 2014                                        #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT DISTINCT folio_liquida,",
                    "        f_registro",
                    "   FROM sep_mov_deudor",
                    "  WHERE movimiento = 381",# Sólo los abonos por restitución, no se debe considerar los cargos por restitución (382) que probienen de la compensación del deudor 
                    "  ORDER BY folio_liquida DESC, f_registro DESC"
   PREPARE prp_recupera_folios_lote FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_sep_cifras_totales_lote_restitucion(?)"
   PREPARE prp_recupera_cifras_lote FROM v_consulta 

   LET v_consulta = "EXECUTE FUNCTION fn_sep_detalle_lote_restitucion(?)"
   PREPARE prp_recupera_detalle_lote FROM v_consulta 

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => Consulta cifras folio                                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Diciembre 2014                                        #
################################################################################
FUNCTION fn_consulta_folio_lote()
DEFINE v_folio     LIKE sep_mov_deudor.folio_liquida,
       v_cb_folios ui.ComboBox

   OPEN WINDOW vtna_consulta_folio WITH FORM r_ruta_ejecutable CLIPPED||"/SEPC701"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      INPUT v_folio 
       FROM folios_lote ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

         BEFORE INPUT
            --CALL v_forma.setFieldHidden("tipo_reporte",TRUE)
            CALL v_forma.setElementHidden("gpo_reporte",TRUE)
            LET v_cb_folios = ui.ComboBox.forName("folios_lote")
            CALL fn_llena_combo_folios(v_cb_folios)

         ON ACTION aceptar
            IF(v_folio IS NULL)THEN
               CALL fn_mensaje(p_cad_ventana,"Capture folio","information")
               CONTINUE INPUT
            END IF
            CALL fn_consulta_lote(v_folio)
            INITIALIZE v_folio TO NULL
            CALL fn_llena_combo_folios(v_cb_folios)
 
         ON ACTION cancelar
            EXIT INPUT

      END INPUT
      
   CLOSE WINDOW vtna_consulta_folio 

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => recupera filtros para detalle de movimientos             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Diciembre 2014                                        #
################################################################################
FUNCTION fn_llena_combo_folios(p_cb_folios)
DEFINE p_cb_folios ui.ComboBox,
       v_folios RECORD
         v_folio          LIKE sep_mov_deudor.folio_liquida,
         v_fecha_registro LIKE sep_mov_deudor.f_registro
       END RECORD

   CALL p_cb_folios.clear()
   DECLARE cur_recupera_folios_lote CURSOR FOR prp_recupera_folios_lote
   FOREACH cur_recupera_folios_lote INTO v_folios.*
      CALL p_cb_folios.addItem(v_folios.v_folio,v_folios.v_folio||" - "||(v_folios.v_fecha_registro USING "dd-mm-yyyy"))
   END FOREACH
   FREE cur_recupera_folios_lote

END FUNCTION


################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => recupera filtros para detalle de movimientos             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Diciembre 2014                                        #
################################################################################
FUNCTION fn_consulta_lote(p_folio)
DEFINE p_folio LIKE sep_mov_deudor.folio_liquida,
       r_lote DYNAMIC ARRAY OF RECORD # cada lote corresponde a un folio, pero se mencionan como lote ya que agrupan varios registros de restitución
          v_folio               LIKE sep_mov_deudor.folio_liquida,
          v_fecha_registro      LIKE sep_mov_deudor.f_registro,
          v_f_val_deudor        LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor        LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos  LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor        LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos  LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado         LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                  DECIMAL(16,2),
          v_int_97                  DECIMAL(16,2)
       END RECORD,
       r_detalle_lote DYNAMIC ARRAY OF RECORD
          v_numeracion      SMALLINT,
          v_id_expediente   LIKE sep_expediente.id_expediente,
          v_nss             LIKE sep_nss_expediente.nss,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                  DECIMAL(16,2),          
          v_int_97                  DECIMAL(16,2)
       END RECORD,
       v_tipo_reporte       SMALLINT,
       r_bnd_genero_reporte BOOLEAN,
       r_nombre_reporte     STRING ,
       v_item               SMALLINT

              
   DIALOG ATTRIBUTES(UNBUFFERED)

      INPUT v_tipo_reporte FROM tipo_reporte

      END INPUT

      DISPLAY ARRAY r_lote TO sr_cifras_lote.*
         

      END DISPLAY 

      DISPLAY ARRAY r_detalle_lote TO sr_detalle_lote.*
      
      END DISPLAY

      BEFORE DIALOG
         LET v_tipo_reporte = 0 # Reporte global por defecto
         --CALL v_forma.setFieldHidden("tipo_reporte",FALSE)
         CALL v_forma.setElementHidden("gpo_reporte",FALSE)
         DISPLAY "" TO lbl_reporte
         DISPLAY "" TO reporte
         CALL fn_recupera_cifras_lote(p_folio) RETURNING r_lote
         CALL fn_recupera_detalle_lote(p_folio) RETURNING r_detalle_lote
         

      ON ACTION reporte
         CASE v_tipo_reporte

            WHEN 0 # Reporte global
               CALL fn_genera_reporte_cifras_globales(r_lote) RETURNING r_bnd_genero_reporte,
                                                                        r_nombre_reporte
               IF(r_bnd_genero_reporte)THEN # Muestra ícono para recuperar reporte
                  DISPLAY "Reporte global:" TO lbl_reporte
                  DISPLAY "<a gwc:attributes=\"href resourceUri('"||r_nombre_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>"  TO reporte
                  CALL v_forma.setElementHidden("gpo_reporte",FALSE)
               END IF

            WHEN 1 # Reporte detalle de lote (folio)

               IF DIALOG.getCurrentItem() == "sr_detalle_lote" THEN
                  LET v_item = 1
               ELSE 
                  LET v_item = 0
               END IF
            
               CALL fn_genera_reporte_detalle_lote(p_folio,
                                                   r_detalle_lote[ARR_CURR()].*,
                                                   r_detalle_lote,v_item) RETURNING r_bnd_genero_reporte,
                                                                             r_nombre_reporte
               IF(r_bnd_genero_reporte)THEN # Muestra ícono para recuperar reporte
                  DISPLAY "Reporte detalle:" TO lbl_reporte
                  DISPLAY "<a gwc:attributes=\"href resourceUri('"||r_nombre_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>"  TO reporte
                  CALL v_forma.setElementHidden("gpo_reporte",FALSE)
               END IF
         END CASE

      ON ACTION cancelar
         CALL r_lote.clear()
         CALL r_detalle_lote.clear()
         --CALL v_forma.setFieldHidden("tipo_reporte",TRUE)
         DISPLAY "" TO reporte
         CALL v_forma.setElementHidden("gpo_reporte",TRUE)
         EXIT DIALOG

   END DIALOG

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => Recupera las cifras totales del folio de separación,     #
#                     liquidadas y provenientes de porcesar                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Diciembre 2014                                        #
################################################################################
FUNCTION fn_recupera_cifras_lote(p_folio)
DEFINE p_folio        LIKE sep_mov_deudor.folio_liquida,
       r_lotes DYNAMIC ARRAY OF RECORD # cada lote corresponde a un folio, pero se mencionan como lote ya que agrupan varios registros de restitución
          v_folio           LIKE sep_mov_deudor.folio_liquida,
          v_fecha_registro  LIKE sep_mov_deudor.f_registro,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                  DECIMAL(16,2),
          v_int_97                  DECIMAL(16,2)
       END RECORD,
       v_lote RECORD
          v_error_sql       INTEGER,
          v_isam_error      INTEGER,
          v_mensaje_error   VARCHAR(254),
          v_fecha_registro  LIKE sep_mov_deudor.f_registro,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                  DECIMAL(16,2),
          v_int_97                  DECIMAL(16,2)
       END RECORD,
       v_indice       SMALLINT
   
   LET v_indice = 1

   
   DECLARE cur_recupera_cifras_lote CURSOR FOR prp_recupera_cifras_lote
   FOREACH cur_recupera_cifras_lote USING p_folio
                                     INTO v_lote.*
      LET r_lotes[v_indice].v_folio           = p_folio
      LET r_lotes[v_indice].v_fecha_registro  = v_lote.v_fecha_registro
      LET r_lotes[v_indice].v_f_val_deudor    = v_lote.v_f_val_deudor
      LET r_lotes[v_indice].v_mov92_deudor    = v_lote.v_mov92_deudor
      LET r_lotes[v_indice].v_mov92_deudor_pesos    = v_lote.v_mov92_deudor_pesos
      LET r_lotes[v_indice].v_mov97_deudor    = v_lote.v_mov97_deudor
      LET r_lotes[v_indice].v_mov97_deudor_pesos    = v_lote.v_mov97_deudor_pesos
      LET r_lotes[v_indice].v_f_val_liquidado = v_lote.v_f_val_liquidado
      LET r_lotes[v_indice].v_mov92_liquidado = v_lote.v_mov92_liquidado
      LET r_lotes[v_indice].v_mov92_liquidado_pesos = v_lote.v_mov92_liquidado_pesos
      LET r_lotes[v_indice].v_mov97_liquidado = v_lote.v_mov97_liquidado
      LET r_lotes[v_indice].v_mov97_liquidado_pesos = v_lote.v_mov97_liquidado_pesos
      --LET r_lotes[v_indice].v_f_val_procesar  = v_lote.v_f_val_procesar
      LET r_lotes[v_indice].v_mov92_procesar  = v_lote.v_mov92_procesar
      LET r_lotes[v_indice].v_mov92_procesar_pesos  = v_lote.v_mov92_procesar_pesos
      LET r_lotes[v_indice].v_mov97_procesar  = v_lote.v_mov97_procesar
      LET r_lotes[v_indice].v_mov97_procesar_pesos  = v_lote.v_mov97_procesar_pesos
      LET r_lotes[v_indice].v_int_92  = v_lote.v_int_92
      LET r_lotes[v_indice].v_int_97  = v_lote.v_int_97
      
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_cifras_lote
   IF(v_lote.v_error_sql <> 0)THEN
      DISPLAY "Ocurrió un error"
      DISPLAY "Código:",v_lote.v_error_sql
      DISPLAY "Mensaje:",v_lote.v_mensaje_error
   END IF
   
   RETURN r_lotes
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => Recupera el detalle del folio que son las cifras totales #
#                     por expediente                                           #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Diciembre 2014                                        #
################################################################################
FUNCTION fn_recupera_detalle_lote(p_folio)
DEFINE p_folio        LIKE sep_mov_deudor.folio_liquida,
       r_detalle_lotes DYNAMIC ARRAY OF RECORD
          v_numeracion    SMALLINT,
          v_id_expediente LIKE sep_expediente.id_expediente,
          v_nss           LIKE sep_nss_expediente.nss,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                  DECIMAL(16,2),          
          v_int_97                  DECIMAL(16,2)
       END RECORD,
       v_detalle_lote RECORD
          v_error_sql       INTEGER,
          v_isam_error      INTEGER,
          v_mensaje_error   VARCHAR(254),
          v_id_expediente   LIKE sep_expediente.id_expediente,
          v_nss             LIKE sep_nss_expediente.nss,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                  DECIMAL(16,2),          
          v_int_97                  DECIMAL(16,2)
       END RECORD,
       v_indice       SMALLINT
   
   LET v_indice = 1   
   DECLARE cur_recupera_detalle_lote CURSOR FOR prp_recupera_detalle_lote
   FOREACH cur_recupera_detalle_lote USING p_folio
                                      INTO v_detalle_lote.*
      LET r_detalle_lotes[v_indice].v_numeracion      = v_indice 
      LET r_detalle_lotes[v_indice].v_id_expediente   = v_detalle_lote.v_id_expediente
      LET r_detalle_lotes[v_indice].v_nss             = v_detalle_lote.v_nss
      LET r_detalle_lotes[v_indice].v_f_val_deudor    = v_detalle_lote.v_f_val_deudor
      LET r_detalle_lotes[v_indice].v_mov92_deudor    = v_detalle_lote.v_mov92_deudor
      LET r_detalle_lotes[v_indice].v_mov92_deudor_pesos    = v_detalle_lote.v_mov92_deudor_pesos
      LET r_detalle_lotes[v_indice].v_mov97_deudor    = v_detalle_lote.v_mov97_deudor
      LET r_detalle_lotes[v_indice].v_mov97_deudor_pesos    = v_detalle_lote.v_mov97_deudor_pesos
      LET r_detalle_lotes[v_indice].v_f_val_liquidado = v_detalle_lote.v_f_val_liquidado
      LET r_detalle_lotes[v_indice].v_mov92_liquidado = v_detalle_lote.v_mov92_liquidado
      LET r_detalle_lotes[v_indice].v_mov92_liquidado_pesos = v_detalle_lote.v_mov92_liquidado_pesos
      LET r_detalle_lotes[v_indice].v_mov97_liquidado = v_detalle_lote.v_mov97_liquidado
      LET r_detalle_lotes[v_indice].v_mov97_liquidado_pesos = v_detalle_lote.v_mov97_liquidado_pesos
      --LET r_detalle_lotes[v_indice].v_f_val_procesar  = v_detalle_lote.v_f_val_procesar
      LET r_detalle_lotes[v_indice].v_mov92_procesar  = v_detalle_lote.v_mov92_procesar
      LET r_detalle_lotes[v_indice].v_mov92_procesar_pesos  = v_detalle_lote.v_mov92_procesar_pesos
      LET r_detalle_lotes[v_indice].v_mov97_procesar  = v_detalle_lote.v_mov97_procesar
      LET r_detalle_lotes[v_indice].v_mov97_procesar_pesos  = v_detalle_lote.v_mov97_procesar_pesos
      LET r_detalle_lotes[v_indice].v_int_92  = v_detalle_lote.v_int_92
      LET r_detalle_lotes[v_indice].v_int_97  = v_detalle_lote.v_int_97

      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_detalle_lote
   IF(v_detalle_lote.v_error_sql <> 0)THEN
      DISPLAY "Ocurrió un error"
      DISPLAY "Código:",v_detalle_lote.v_error_sql
      DISPLAY "Mensaje:",v_detalle_lote.v_mensaje_error
   END IF
   
   RETURN r_detalle_lotes
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => Genera reporte de cifras globales de restitucion         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Diciembre 2014                                        #
################################################################################
FUNCTION fn_genera_reporte_cifras_globales(p_lote)
DEFINE p_lote DYNAMIC ARRAY OF RECORD # cada lote corresponde a un folio, pero se mencionan como lote ya que agrupan varios registros de restitución
          v_folio           LIKE sep_mov_deudor.folio_liquida,
          v_fecha_registro  LIKE sep_mov_deudor.f_registro,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                  DECIMAL(16,2),
          v_int_97                  DECIMAL(16,2)
       END RECORD,
       v_manejador_rpt      OM.SaxDocumentHandler,
       v_nombre_reporte     STRING,
       v_indice             SMALLINT,
       v_bnd_genero_reporte BOOLEAN

   LET v_bnd_genero_reporte = FALSE # inicializa como reporte no generado
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPC701.4rp"))THEN
      LET v_nombre_reporte = p_usuario_cod CLIPPED, "-SEPC70-", 
                             "00000" USING "&&&&&", "-", 
                             "00000" USING "&&&&&", "-", 
                             "00001" USING "&&&&&",
                             ".pdf"
      # Ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nombre_reporte)

      # COnfigura la salida en tipo PDF
      CALL fgl_report_selectDevice("PDF")
      
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # Asigna la configuración en el menejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      START REPORT fn_reporte_cifras_globales TO XML HANDLER v_manejador_rpt
         
         FOR v_indice = 1 TO p_lote.getLength()
            OUTPUT TO REPORT fn_reporte_cifras_globales(p_lote[v_indice].*)
         END FOR

      FINISH REPORT fn_reporte_cifras_globales
      LET v_bnd_genero_reporte = TRUE

   ELSE
      CALL fn_mensaje("AVISO","Ocurrió un error al cargar plantilla de reporte","information")
   END IF

   RETURN v_bnd_genero_reporte,
          v_nombre_reporte
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => Genera reporte de detalle de lote de restitucion         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Diciembre 2014                                        #
################################################################################
FUNCTION fn_genera_reporte_detalle_lote(p_folio,p_detalle_lote,p_arr_detalle_lote,p_item)
DEFINE p_folio LIKE sep_mov_deudor.folio_liquida,
       p_detalle_lote RECORD
          v_numeracion      SMALLINT,
          v_id_expediente   LIKE sep_expediente.id_expediente,
          v_nss             LIKE sep_nss_expediente.nss,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                 DECIMAL(16,2),
          v_int_97                 DECIMAL(16,2)
       END RECORD,
       p_arr_detalle_lote DYNAMIC ARRAY OF RECORD
          v_numeracion      SMALLINT,
          v_id_expediente   LIKE sep_expediente.id_expediente,
          v_nss             LIKE sep_nss_expediente.nss,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                 DECIMAL(16,2),
          v_int_97                 DECIMAL(16,2)
       END RECORD,
       v_manejador_rpt      OM.SaxDocumentHandler,
       v_nombre_reporte     STRING,
       v_bnd_genero_reporte BOOLEAN ,
       p_item               SMALLINT, 
       i                    INTEGER

   LET v_bnd_genero_reporte = FALSE # Inicializa como reporte no generado
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPC702.4rp"))THEN
      LET v_nombre_reporte = p_usuario_cod CLIPPED, "-SEPC70-", 
                             "00000" USING "&&&&&", "-", 
                             "00000" USING "&&&&&", "-", 
                             "00002" USING "&&&&&",
                             ".pdf"
      # Ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nombre_reporte)

      # COnfigura la salida en tipo PDF
      CALL fgl_report_selectDevice("PDF")

      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # Asigna la configuración en el menejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      START REPORT fn_reporte_detalle_lote TO XML HANDLER v_manejador_rpt

      IF p_item = 1 THEN

      
         OUTPUT TO REPORT fn_reporte_detalle_lote(p_folio,
                                                  p_detalle_lote.*)
      ELSE

         FOR i = 1 TO p_arr_detalle_lote.getLength()

             OUTPUT TO REPORT fn_reporte_detalle_lote(p_folio,
                                                      p_arr_detalle_lote[i].*)
         END FOR
      END IF
      FINISH REPORT fn_reporte_detalle_lote
      LET v_bnd_genero_reporte = TRUE

   ELSE
      CALL fn_mensaje("AVISO","Ocurrió un error al cargar plantilla de reporte","information")
   END IF

   RETURN v_bnd_genero_reporte,
          v_nombre_reporte
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => Funcion que crea reporte de cifras globales              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Diciembre 2014                                        #
################################################################################
REPORT fn_reporte_cifras_globales(p_lote)
DEFINE p_lote RECORD # cada lote corresponde a un folio, pero se mencionan como lote ya que agrupan varios registros de restitución
          v_folio           LIKE sep_mov_deudor.folio_liquida,
          v_fecha_registro  LIKE sep_mov_deudor.f_registro,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                 DECIMAL(16,2),
          v_int_97                 DECIMAL(16,2)
       END RECORD,
       v_fecha_actual DATE,
       v_pagina       SMALLINT

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY
         PRINTX v_fecha_actual USING "dd-mm-yyyy"

      ON EVERY ROW
         PRINTX p_lote.v_folio,
                p_lote.v_fecha_registro  USING "dd-mm-yyyy",
                p_lote.v_f_val_deudor    USING "dd-mm-yyyy",
                p_lote.v_mov92_deudor,
                p_lote.v_mov92_deudor_pesos,
                p_lote.v_mov97_deudor,
                p_lote.v_mov97_deudor_pesos,
                p_lote.v_f_val_liquidado USING "dd-mm-yyyy",
                p_lote.v_mov92_liquidado,
                p_lote.v_mov92_liquidado_pesos,
                p_lote.v_mov97_liquidado,
                p_lote.v_mov97_liquidado_pesos,
                --p_lote.v_f_val_procesar  USING "dd-mm-yyyy",
                p_lote.v_mov92_procesar,
                p_lote.v_mov92_procesar_pesos,
                p_lote.v_mov97_procesar,
                p_lote.v_mov97_procesar_pesos,
                p_lote.v_int_92,
                p_lote.v_int_97                
                
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina
         
END REPORT

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC70                                                   #
#Descripcion       => Funcion que crea reporte de cifras globales              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Diciembre 2014                                        #
################################################################################
REPORT fn_reporte_detalle_lote(p_folio,p_detalle_lote)
DEFINE p_folio LIKE sep_mov_deudor.folio_liquida,
       p_detalle_lote RECORD
          v_numeracion      SMALLINT,
          v_id_expediente   LIKE sep_expediente.id_expediente,
          v_nss             LIKE sep_nss_expediente.nss,
          v_f_val_deudor    LIKE dse_devolucion.f_movimiento,
          v_mov92_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov92_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor            LIKE sep_mov_deudor.monto_acciones,
          v_mov97_deudor_pesos      LIKE sep_mov_deudor.monto_acciones,
          v_f_val_liquidado LIKE cta_movimiento.f_valor,
          v_mov92_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov92_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado         LIKE cta_movimiento.monto_acciones,
          v_mov97_liquidado_pesos   LIKE cta_movimiento.monto_acciones,
          --v_f_val_procesar  LIKE dse_his_devolucion.f_proceso,
          v_mov92_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov92_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar          LIKE sep_mov_deudor.monto_acciones,
          v_mov97_procesar_pesos    LIKE sep_mov_deudor.monto_acciones,
          v_int_92                 DECIMAL(16,2),
          v_int_97                 DECIMAL(16,2)
       END RECORD,
       v_fecha_actual DATE,
       v_pagina       SMALLINT

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY
         PRINTX p_folio USING "########&",
                v_fecha_actual USING "dd-mm-yyyy"

      ON EVERY ROW
         PRINTX p_detalle_lote.v_id_expediente,
                p_detalle_lote.v_nss,
                p_detalle_lote.v_f_val_deudor    USING "dd-mm-yyyy",
                p_detalle_lote.v_mov92_deudor,
                p_detalle_lote.v_mov92_deudor_pesos,
                p_detalle_lote.v_mov97_deudor,
                p_detalle_lote.v_mov97_deudor_pesos,
                p_detalle_lote.v_f_val_liquidado USING "dd-mm-yyyy",
                p_detalle_lote.v_mov92_liquidado,
                p_detalle_lote.v_mov92_liquidado_pesos,
                p_detalle_lote.v_mov97_liquidado,
                p_detalle_lote.v_mov97_liquidado_pesos,
                --p_detalle_lote.v_f_val_procesar  USING "dd-mm-yyyy",
                p_detalle_lote.v_mov92_procesar,
                p_detalle_lote.v_mov92_procesar_pesos,
                p_detalle_lote.v_mov97_procesar,
                p_detalle_lote.v_mov97_procesar_pesos,
                p_detalle_lote.v_int_92,
                p_detalle_lote.v_int_97
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina
       
END REPORT