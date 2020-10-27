################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificación  => 29/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Módulo            => CNT                                                      #
#Programa          => CNTP01                                                   #
#Objetivo          => Programa para consultar el histórico del registro        #
#                     de pólizas contables                                     #
#Fecha inicio      => 16/07/2012                                               #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     20140123      Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
IMPORT util

DATABASE safre_viv

GLOBALS "CNTG01.4gl"

GLOBALS 
  --Arreglo para el Detalle Información Póliza Contable
  DEFINE g_arr_inf_poliza_contable DYNAMIC ARRAY OF RECORD
    arr_po_fecha             LIKE cnt_transaccion.f_liquida,
    arr_po_folio_liquida     LIKE cnt_transaccion.folio_liquida,
    arr_po_folio             LIKE cnt_transaccion.folio_cnt,
    arr_po_proceso           CHAR(42),
    --arr_po_documento         LIKE cnt_ctr_proceso.num_poliza,
    arr_po_cuenta            LIKE cnt_transaccion.cta_contable,
    arr_po_descripcion       LIKE cat_cuenta_contable.desc_cta_contable,
    arr_po_cargo             LIKE cnt_transaccion.importe,
    arr_po_abono             LIKE cnt_transaccion.importe,
    --arr_po_estado            CHAR(27),
    arr_po_estado            VARCHAR (40),
    arr_po_cod_pro           LIKE cnt_transaccion.cod_proceso_cnt,
    arr_po_id_cuenta         LIKE cnt_transaccion.id_cuenta_contable
  END RECORD

  --Arreglo para el Detalle Información Montos de las Operaciones
  DEFINE g_arr_inf_montos_op DYNAMIC ARRAY OF RECORD 
    --arr_op_fecha             CHAR(10),
    arr_op_fecha             DATE,
    arr_op_folio             LIKE cta_movimiento.folio_liquida,
    arr_op_subcuenta         CHAR(20),
    arr_op_tipo              CHAR(42),
    arr_op_fondo             CHAR(10),
    arr_op_pesos             DECIMAL(22,2),
    arr_op_aivs              DECIMAL(22,2)
  END RECORD

  --Arreglo para los folios registrados
  DEFINE arr_folios_registrados DYNAMIC ARRAY OF RECORD 
    arr_folio_registrado     LIKE cnt_error_poliza.folio_cnt
  END RECORD 

  --Arreglo para el los errores de la póliza contable
  DEFINE g_arr_rechazo_pol_cnt DYNAMIC ARRAY OF RECORD
    arr_re_fecha_liquida     LIKE cnt_error_poliza.f_respuesta, 
    arr_re_fecha_resp        LIKE cnt_error_poliza.f_respuesta,
    -- arr_re_fecha_emi        LIKE cnt_error_poliza.f_emision,
    arr_re_folo_liquida      LIKE cnt_error_poliza.folio_cnt,
    arr_re_folo_cnt          LIKE cnt_error_poliza.folio_cnt,
    arr_re_error             LIKE cnt_error_poliza.error
  END RECORD 

  --Sección de variables del programa
  DEFINE 
    f_fecha_liquidacion      DATE,
    f_fecha_liquidacion_final DATE,
    f_folio                  CHAR(9),
    f_folio_liquida          LIKE cnt_transaccion.folio_liquida,
    f_doc_contable           CHAR(10),
    v_folio_liq              LIKE cnt_transaccion.folio_liquida,
    v_f_liquida              LIKE cnt_transaccion.f_liquida,
    v_cod_proceso_cnt        SMALLINT,
    v_desc_cod_proceso       CHAR(40)

  --Sección de variables para el lanzado de la póliza contable
  DEFINE 
    p_ruta_ejecutable        LIKE seg_modulo.ruta_bin,
    p_aux                    LIKE seg_modulo.ruta_bin,
    p_ruta_listados          LIKE seg_modulo.ruta_listados,
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tpo_transaccion        SMALLINT -- 0 es transacción normal, 1 es reversada

  DEFINE 
    v_encabezado             SMALLINT -- 1 póliza, 2 detalle

  DEFINE
    v_r_tot_fol_liq          SMALLINT

   CONSTANT Por_Folio = 0
   CONSTANT Por_Fecha = 1
   CONSTANT Sin = 0
   DEFINE v_tbl_mov    VARCHAR(50) 
      
END GLOBALS 

MAIN 
  --Sección de variables UI
  DEFINE 
    f_ventana                ui.Window, --provee la interfaz para la ventana
    f_forma                  ui.Form --provee la interfaz para la forma

  --Sección de variables de retorno
  DEFINE 
    v_indice1                SMALLINT,
    v_indice2                SMALLINT,
    v_cargos                 DECIMAL(22,2), --Sumatoria de cargos
    v_abonos                 DECIMAL(22,2),  --Sumatoria de abonos
    v_pesos                  DECIMAL(22,2),  --Sumatoria de pesos
    v_aivs                   DECIMAL(22,2),  --Sumatoria de AIVS
    v_fecha_valida           SMALLINT,
    v_proceso_valido         SMALLINT,
    v_folio_valido           SMALLINT,
    v_doc_valido             SMALLINT, 
    v_flag_rechazo           SMALLINT,
    v_flag_folio             SMALLINT,
    v_comp_query             STRING,
    v_manejador_rpt          om.SaxDocumentHandler,
    v_ind_rep                SMALLINT,
    v_estado_consult         LIKE cnt_transaccion.estado,
    v_cond_reporte           SMALLINT, --Indicador para manejar la impresión del reporte
    v_comando                STRING,
    v_cod_pro                LIKE cnt_transaccion.cod_proceso_cnt,
    v_matematico             FLOAT --Variable para validar folio
      
  --Sección de parámetros   
  DEFINE 
    p_programa               CHAR(10),
    v_transaccion            SMALLINT

  LET g_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
  LET g_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
  LET g_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa

  LET v_transaccion  = 2

  
  CLOSE WINDOW SCREEN

  OPEN WINDOW v_cntp01 WITH FORM "CNTP011"
    DIALOG   ATTRIBUTES(UNBUFFERED) 
      --INPUT BY NAME v_cmb_proceso, f_fecha_liquidacion, f_folio, f_folio_liquida
      INPUT BY NAME v_cmb_proceso, f_fecha_liquidacion, f_fecha_liquidacion_final, f_folio, f_folio_liquida
         
        BEFORE INPUT 
            
          CLEAR FORM 
            
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
            
          --Invocamos la función para asignar el título a la ventana
          CALL ui.Interface.setText(g_nom_prog)
            
          --Oculta las secciones de detalle y botones adicionales (TRUE oculta y FALSE muestra)
          CALL f_forma.setElementHidden("gr_inf_pol_cont",TRUE) --Oculta la sección de Detalle Información Póliza Contable
          CALL f_forma.setElementHidden("lbl_total_registros",TRUE) --Oculta la etiqueta del Total de Registros
          CALL f_forma.setFieldHidden("f_total_reg",TRUE) --Oculta el cuadro de texto Total de Registros
          CALL f_forma.setElementHidden("lbl_totales_poliza",TRUE) --Oculta la etiqueta Totales
          CALL f_forma.setFieldHidden("f_total_cargo",TRUE) --Oculta el cuadro de texto de la sumatoria de cargos
          CALL f_forma.setFieldHidden("f_total_abono",TRUE) --Oculta el cuadro de texto de la sumatoria de abonos
          CALL f_forma.setElementHidden("lbl_totales_montos",TRUE) --Oculta la etiqueta Totales
          CALL f_forma.setFieldHidden("f_total_pesos",TRUE) --Oculta el cuadro de texto de la sumatoria en pesos
          CALL f_forma.setFieldHidden("f_total_aivs",TRUE) --Oculta el cuadro de texto de la sumatoria en AIVS    
          CALL f_forma.setElementHidden("gr_inf_montos_op",TRUE) --Oculta la sección de Detalle Información Montos Operaciones
          --CALL f_forma.setElementHidden("gr_inf_montos_op",TRUE) --Oculta el botón de Reporte
          --CALL DIALOG.setActionHidden("rechazo",TRUE) --Oculta el botón de Rechazo
          --CALL DIALOG.setActionHidden("enviar_poliza",TRUE) --Oculta el botón de Rechazo

          CALL f_forma.setElementHidden("f_doc_contable",TRUE) --Oculta el cuadro de entrada del doc. contable
          CALL f_forma.setElementHidden("lbl_Doc_Contable",TRUE) --Oculta el cuadro de entrada del doc. contable
            
          --Llena el combo de proceso 
          CALL fn_llena_combo_proceso()

          ON ACTION cancelar 
             EXIT DIALOG  
            
          --Realiza consultas y llena los detalles en la forma
          ON ACTION aceptar
             --Valida que se ingrese al menos un campo
             --IF f_fecha_liquidacion IS NULL AND f_folio IS NULL AND v_cmb_proceso IS NULL AND f_doc_contable IS NULL THEN
             IF f_fecha_liquidacion IS NULL AND f_folio IS NULL AND v_cmb_proceso IS NULL AND f_folio_liquida IS NULL AND f_fecha_liquidacion_final IS NULL THEN  
                CALL fn_mensaje("Error", "Debe ingresar al menos un parámetro de entrada.", "information")
                --Limpia tablas
                CALL g_arr_inf_poliza_contable.clear()
                CALL g_arr_inf_montos_op.clear()
                CALL arr_folios_registrados.clear()
                CALL g_arr_rechazo_pol_cnt.clear()
                NEXT FIELD v_cmb_proceso

             ELSE 
         
                --Valida que el proceso exista en cnt_transaccion
                IF v_cmb_proceso IS NOT NULL THEN 
                   CALL f_valida_proceso_cnt() RETURNING v_proceso_valido
                   IF v_proceso_valido == 0 THEN 
                      CALL fn_mensaje("Error", "Proceso no existe", "information")
                      --Limpia tablas
                      CALL g_arr_inf_poliza_contable.clear()
                      CALL g_arr_inf_montos_op.clear()
                      CALL arr_folios_registrados.clear()
                      CALL g_arr_rechazo_pol_cnt.clear()
                     
                      NEXT FIELD v_cmb_proceso
                   ELSE 
                      LET v_comp_query = v_comp_query , "\n AND P.cod_proceso_cnt = ",v_cmb_proceso
                   END IF 
                END IF 

{               --Valida que la fecha exista en cnt_transaccion
                IF f_fecha_liquidacion IS NOT NULL THEN

                   CALL f_valida_fecha_cnt() RETURNING v_fecha_valida
                
                   IF v_fecha_valida == 0 THEN 
                      CALL fn_mensaje("Error", "Fecha de liquidación no existe", "information")
                      --Limpia tablas
                      CALL g_arr_inf_poliza_contable.clear()
                      CALL g_arr_inf_montos_op.clear()
                      CALL arr_folios_registrados.clear()
                      CALL g_arr_rechazo_pol_cnt.clear()
                      NEXT FIELD f_fecha_liquidacion
                   ELSE  
                      --Arma el final del query
                      LET v_comp_query = v_comp_query, "\n AND T.f_liquida = '",f_fecha_liquidacion,"'"
                   END IF                
                END IF 
}

                --Valida que se ingrese tanto fecha inicial como fecha final
                IF f_fecha_liquidacion IS NOT NULL AND f_fecha_liquidacion_final IS NULL THEN
                   CALL fn_mensaje("Error", "Error: Debe ingresar ambas fechas de búsqueda", "information")
                   NEXT FIELD f_fecha_liquidacion_final
                END IF 

                --Valida que se ingrese tanto fecha inicial como fecha final
                IF f_fecha_liquidacion IS NULL AND f_fecha_liquidacion_final IS NOT NULL THEN
                   CALL fn_mensaje("Error", "Error: Debe ingresar ambas fechas de búsqueda", "information")
                   NEXT FIELD f_fecha_liquidacion
                END IF 

                --Valida que la fecha final inicial no sea mayor a la final
                IF f_fecha_liquidacion_final > TODAY THEN 
                   CALL fn_mensaje("Error", "Error: Fecha posterior a la fecha actual", "information")
                   NEXT FIELD f_fecha_liquidacion_final
                END IF 

                --Valida que la fecha final inicial no sea mayor a la final
                IF f_fecha_liquidacion > f_fecha_liquidacion_final THEN 
                   CALL fn_mensaje("Error", "Error: Fecha Improcedente", "information")
                   NEXT FIELD f_fecha_liquidacion
                END IF 
            
                --Valida que se ingrese tanto fecha inicial como fecha final
                IF f_fecha_liquidacion IS NOT NULL AND f_fecha_liquidacion_final IS NOT NULL THEN
                   --Arma el final del query
                   LET v_comp_query = v_comp_query, "\n AND T.f_liquida BETWEEN '",f_fecha_liquidacion,"' AND '",f_fecha_liquidacion_final,"'"
                END IF 
            
                --Valida que el folio exista en cnt_transaccion
                IF f_folio IS NOT NULL THEN

                   --Valida que se introduzcan dígitos y no caracteres
                   CALL util.Math.pow(f_folio,0) RETURNING v_matematico
                   IF v_matematico IS NULL THEN 
                      CALL fn_mensaje("Error", "Error, verifique el folio contable introducido", "information")
                      NEXT FIELD f_folio
                   END IF 
            
                   CALL f_valida_folio_cnt() RETURNING v_folio_valido
                   IF v_folio_valido == 0 THEN
                      CALL fn_mensaje("Error", "Folio del registro contable no existe", "information")
                      --Limpia tablas
                      CALL g_arr_inf_poliza_contable.clear()
                      CALL g_arr_inf_montos_op.clear()
                      CALL arr_folios_registrados.clear()
                      CALL g_arr_rechazo_pol_cnt.clear()
                      NEXT FIELD f_folio 
                   ELSE 
                      LET v_comp_query = v_comp_query, "\n AND T.folio_cnt = '",f_folio,"'"
                      --LET v_comp_query = v_comp_query, "\n AND T.folio_cnt = R.folio_cnt "       
                   END IF 
                END IF 

                --Valida que el folio exista
                IF f_folio_liquida IS NOT NULL THEN 

                   --Valida que se introduzcan dígitos y no caracteres
                   CALL util.Math.pow(f_folio_liquida,0) RETURNING v_matematico
                   IF v_matematico IS NULL THEN 
                      CALL fn_mensaje("Error", "Error, verifique el folio de liquidación introducido", "information")
                      NEXT FIELD f_folio_liquida
                   END IF

                   CALL f_valida_folio_liquida() RETURNING v_folio_valido
                   IF v_folio_valido == 0 THEN
                      CALL fn_mensaje("Error", "Folio de liquidación no existe", "information")
                      --Limpia tablas
                      CALL g_arr_inf_poliza_contable.clear()
                      CALL g_arr_inf_montos_op.clear()
                      CALL arr_folios_registrados.clear()
                      CALL g_arr_rechazo_pol_cnt.clear()
                      NEXT FIELD f_folio_liquida
                   ELSE 
                      LET v_comp_query = v_comp_query, "\n AND T.folio_liquida = '",f_folio_liquida,"'"
                      --LET v_comp_query = v_comp_query, "\n AND T.folio_cnt = R.folio_cnt "       
                   END IF 
                END IF 

                --Valida que el documento contable exista en cnt_ctr_proceso - Por ahora se omitió este parámetro
                {IF f_doc_contable IS NOT NULL THEN

                   --Valida que se introduzcan dígitos y no caracteres
                   CALL util.Math.pow(f_doc_contable,0) RETURNING v_matematico
                   IF v_matematico IS NULL THEN 
                      CALL fn_mensaje("Error", "Error, verifique el documento contable introducido", "information")
                      --Limpia tablas
                      CALL g_arr_inf_poliza_contable.clear()
                      CALL g_arr_inf_montos_op.clear()
                      CALL arr_folios_registrados.clear()
                      CALL g_arr_rechazo_pol_cnt.clear()
                      NEXT FIELD f_doc_contable
                   END IF 
            
                   CALL f_valida_doc_cnt() RETURNING v_doc_valido 
                   IF v_doc_valido == 0 THEN
                      CALL fn_mensaje("Error", "Documento contable no existe", "information")
                      --Limpia tablas
                      CALL g_arr_inf_poliza_contable.clear()
                      CALL g_arr_inf_montos_op.clear()
                      CALL arr_folios_registrados.clear()
                      CALL g_arr_rechazo_pol_cnt.clear()
                      NEXT FIELD f_doc_contable
                   ELSE
                      LET v_comp_query = v_comp_query,  "\n AND R.num_poliza = '",f_doc_contable,"'"      
                   END IF
                END IF} 

             END IF 
               
             CALL f_forma.setElementHidden("gr_inf_pol_cont",FALSE) --Muestra la sección de Detalle Información Póliza Contable
             CALL f_forma.setElementHidden("lbl_total_registros",FALSE) --Muestra la etiqueta del Total de Registros
             CALL f_forma.setFieldHidden("f_total_reg",FALSE) --Muestra el cuadro de texto Total de Registros
             CALL f_forma.setElementHidden("lbl_totales_poliza",FALSE) --Muestra la etiqueta Totales
             CALL f_forma.setFieldHidden("f_total_cargo",FALSE) --Muestra el cuadro de texto de la sumatoria de cargos
             CALL f_forma.setFieldHidden("f_total_abono",FALSE) --Muestra el cuadro de texto de la sumatoria de abonos
         
             --Llena el primer detalle
             CALL fn_llena_det_inf_pol_cont(v_comp_query)
             RETURNING v_indice1, v_cargos, v_abonos, v_r_tot_fol_liq

             DISPLAY "v_indice1 --",v_indice1
             IF v_indice1 < 1 THEN 
                CALL fn_mensaje ("INFORMACIÓN", "No hay registros para estos criterios de búsqueda", "info")
                --Limpia tablas
                CALL g_arr_inf_poliza_contable.clear()
                CALL g_arr_inf_montos_op.clear()
                CALL arr_folios_registrados.clear()
                CALL g_arr_rechazo_pol_cnt.clear()
             ELSE 
                --Muestra resultados en la tabla src_inf_pol_cont
                DISPLAY ARRAY g_arr_inf_poliza_contable TO Record1.* ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE )
                
                  BEFORE DISPLAY 
                    DISPLAY v_indice1 TO f_total_reg
                    DISPLAY v_cargos  TO f_total_cargo
                    DISPLAY v_abonos  TO f_total_abono
               
                    --Verifica si existen registros con estado = 30-CONTABILIZADO SAP-FICO
                    CALL verifica_rechazo(v_comp_query)
                    RETURNING v_flag_rechazo

                    DISPLAY "Rechazo? -- ", v_flag_rechazo
                    IF v_flag_rechazo = 0 OR v_flag_rechazo IS NULL THEN 
                       CALL DIALOG.setActionHidden("enviar_poliza",FALSE) --Muestra el botón de Enviar Póliza
                       CALL DIALOG.setActionHidden("rechazo",TRUE) --Muestra el botón de Rechazo
                    ELSE 
                       CALL DIALOG.setActionHidden("enviar_poliza",TRUE ) --Oculta el botón de Enviar Póliza
                    END IF

                    --Verifica si existen errores para el folio_cnt
                    CALL verifica_folio_reg_contable(v_comp_query) RETURNING v_flag_folio

                    IF v_flag_folio > 0 THEN 
                       CALL DIALOG.setActionHidden("rechazo",FALSE) --Muestra el botón de Rechazo
                       CALL DIALOG.setActionHidden("enviar_poliza",TRUE ) --Oculta el botón de Enviar Póliza
                    END IF 

                    --CALL DIALOG.setActionHidden("reporte",TRUE) --Oculta el botón de Reporte
                    --CALL DIALOG.setActionHidden("rechazo",TRUE) --Oculta el botón de Rechazo
                    --CALL DIALOG.setActionHidden("enviar_poliza",TRUE) --Oculta el botón de Rechazo
               
                    ON ACTION cancelar
                       EXIT PROGRAM 
                        
                    ON ACTION aceptar
                       IF v_r_tot_fol_liq = 1 THEN
                                              
                          LET v_cod_pro          = g_arr_inf_poliza_contable [arr_curr()].arr_po_cod_pro
                          DISPLAY "El proceso a buscar ",v_cod_pro
               
                          LET v_desc_cod_proceso = g_arr_inf_poliza_contable[arr_curr()].arr_po_proceso
                          DISPLAY "v_desc_cod_proceso -- ",v_desc_cod_proceso
               
                          --Llena el segundo detalle scr_inf_montos_op
                          CALL fn_llena_det_inf_montos_op(v_cod_pro) RETURNING v_indice2, v_pesos, v_aivs

                          IF v_indice2 < 1 THEN
                             CALL fn_mensaje ("INFORMACIÓN", "No existe información de los montos de las operaciones.", "info")
                          ELSE 
                             CALL f_forma.setElementHidden("gr_inf_montos_op",FALSE) --Muestra la sección de Detalle Información Montos Operaciones
                             CALL f_forma.setElementHidden("lbl_totales_montos",FALSE) --Muestra la etiqueta Totales
                             CALL f_forma.setFieldHidden("f_total_pesos",FALSE) --Muestra el cuadro de texto de la sumatoria en pesos
                             CALL f_forma.setFieldHidden("f_total_aivs",FALSE) --Muestra el cuadro de texto de la sumatoria en AIVS    
                                                   
                             --Muestra resultados en la tabla
                             DISPLAY ARRAY g_arr_inf_montos_op TO Record2.*
                             ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE )

                               BEFORE DISPLAY 
                                 --CALL DIALOG.setActionHidden("reporte",TRUE) --Oculta el botón de Reporte
                                 --CALL DIALOG.setActionHidden("rechazo",TRUE) --Oculta el botón de Rechazo
                                 --CALL DIALOG.setActionHidden("enviar_poliza",TRUE) --Oculta el botón de Rechazo

                                 --Verifica si existen registros con estado = 30-CONTABILIZADO SAP-FICO
                                 --CALL verifica_rechazo(v_comp_query)
                                 --RETURNING v_flag_rechazo

                                 IF v_flag_rechazo = 0 OR v_flag_rechazo IS NULL THEN 
                                    --CALL fn_mensaje ("INFORMACIÓN", "No hay registros en estado 30-CONTABILIZADO SAP-FICO", "info")
                                    CALL DIALOG.setActionHidden("reporte",FALSE) --Muestra el botón de Reporte
                                    --IF f_fecha_liquidacion IS NOT NULL THEN 
                                         --CALL DIALOG.setActionHidden("enviar_poliza",FALSE) --Muestra el botón de Enviar Póliza
                                    --END IF 
                                 ELSE 
                                    --CALL fn_mensaje ("INFORMACIÓN", "Se encontraron " || v_flag_rechazo || " registros en estado 30-CONTABILIZADO SAP-FICO" , "info")
                                 END IF

                                 --Verifica si existen errores para el folio_cnt
                                 --CALL verifica_folio_reg_contable(v_comp_query)
                                 --RETURNING v_flag_folio
                                 --IF v_flag_folio >=  1 THEN 
                                      --
                                      --CALL DIALOG.setActionHidden("rechazo",FALSE) --Muestra el botón de Rechazo
                                      --
                                 --END IF 

                                 DISPLAY v_pesos TO f_total_pesos
                                 DISPLAY v_aivs  TO f_total_aivs
                              
                                 ON ACTION cancelar  
                                    CALL f_forma.setElementHidden("gr_inf_montos_op",TRUE ) --Muestra la sección de Detalle Información Montos Operaciones
                                    CALL f_forma.setElementHidden("lbl_totales_montos",TRUE ) --Muestra la etiqueta Totales
                                    CALL f_forma.setFieldHidden("f_total_pesos",TRUE ) --Muestra el cuadro de texto de la sumatoria en pesos
                                    CALL f_forma.setFieldHidden("f_total_aivs",TRUE ) --Muestra el cuadro de texto de la sumatoria en AIVS
                                    EXIT DISPLAY 
       
                                 --Genera el reporte en formato PDF
                                 ON ACTION reporte
                                    --Inicia el reporte de registros con rechazo
                                    LET v_ind_rep = 1

                                    --LET v_desc_cod_proceso = g_arr_inf_poliza_contable[arr_curr()].arr_po_proceso
                                    --DISPLAY "v_desc_cod_proceso -- ",v_desc_cod_proceso

                                    IF fgl_report_loadCurrentSettings("CNTP014.4rp") THEN 
                                       CALL fgl_report_selectDevice ("PDF")
                                       LET v_manejador_rpt = fgl_report_commitCurrentSettings()
                                    END IF

                                    START REPORT rp_poliza_contable TO XML HANDLER v_manejador_rpt               

                                      DISPLAY g_usuario
                                      LET v_cond_reporte = 1
                                      LET v_encabezado   = 1

                                      -- Llena la primera sección del reporte                 
                                      FOR v_ind_rep = 1 TO v_indice1
                                          OUTPUT TO REPORT rp_poliza_contable(g_arr_inf_poliza_contable[v_ind_rep].*, 
                                                                              g_arr_inf_montos_op[v_ind_rep].*, 
                                                                              v_cond_reporte,
                                                                              v_indice1, 
                                                                              v_cargos, 
                                                                              v_abonos, 
                                                                              v_pesos, 
                                                                              v_aivs, 
                                                                              g_usuario,
                                                                              v_encabezado)
                                          DISPLAY g_arr_inf_poliza_contable[v_ind_rep].arr_po_cuenta
                                          --DISPLAY "primero ",v_encabezado
                                          --LET v_encabezado = 2
                                      END FOR

                                      LET v_cond_reporte = 2
                                      LET v_encabezado   = 2
                                      FOR v_ind_rep = 1 TO v_indice2
                                          OUTPUT TO REPORT rp_poliza_contable(g_arr_inf_poliza_contable[v_ind_rep].*, 
                                                                              g_arr_inf_montos_op[v_ind_rep].*, 
                                                                              v_cond_reporte,
                                                                              v_indice1, 
                                                                              v_cargos, 
                                                                              v_abonos, 
                                                                              v_pesos, 
                                                                              v_aivs, 
                                                                              g_usuario,
                                                                              v_encabezado)
                                          --DISPLAY g_arr_inf_montos_op[v_ind_rep].arr_op_pesos
                                          --DISPLAY "segundo ", v_encabezado
                                      END FOR

                                      -- Llena la segunda sección del reporte 
                                      {
                                      LET v_ind_rep = 1
                                      FOR v_ind_rep = 1 TO v_indice2 -1
                                          OUTPUT TO REPORT rp_poliza_contable(g_arr_inf_montos_op[v_ind_rep].*)
                                      END FOR
                                      }

                                    FINISH REPORT rp_poliza_contable  
                                    --EXIT DIALOG

                               AFTER DISPLAY 
                             END DISPLAY     
                          END IF
                       ELSE
                          CALL fn_mensaje ("INFORMACIÓN", "Sólo se puede consultar montos operativos por un folio de liquidación.", "info")                           
                       END IF
                       
                    --Llama a la función para generar la póliza, la cual valida el inicio de la operación
                    ON ACTION enviar_poliza
                       --LET g_nom_prog  = "CNTP01"
                       --LET v_comando = "fglrun  CNTS01 ", g_usuario," ",g_nom_prog," ", 
                       -- v_transaccion --2 indica generación de póliza manual; 1 es batch

                       ---##########---
                       --LET v_comando = "fglrun  CNTS01 ", g_usuario,"NA",g_proceso_cod,g_opera_cod1,"NA","NA", 
                       --                v_transaccion --2 indica generación de póliza manual; 1 es batch
                       CALL fn_genera_poliza_contable()
                
                       --RUN v_comando
               
                    --CALL fn_mensaje("Información", "Pólizas generadas","information")
                    --Abre la ventana de ayuda para mostrar los errores encontradoes en el folio_cnt
                    ON ACTION rechazo
                       OPEN WINDOW w_rechazo WITH FORM "CNTP012"
                         DIALOG   ATTRIBUTES(UNBUFFERED) 
                           DISPLAY ARRAY g_arr_rechazo_pol_cnt TO rec_error.* END DISPLAY
                             ON ACTION CANCEL
                                EXIT DIALOG 
                         END DIALOG 
                      CLOSE WINDOW w_rechazo

                END DISPLAY  
             END IF 
             --Sale del programa
      END INPUT
    END DIALOG
  CLOSE WINDOW v_cntp01
END MAIN

--Valida si existe la fecha de entrada en cnt_transaccion
FUNCTION f_valida_fecha_cnt()
  DEFINE 
    f_fecha2                 SMALLINT
     
  SELECT COUNT(*) 
  INTO   f_fecha2 
  FROM   cnt_transaccion T 
  WHERE  T.f_liquida = f_fecha_liquidacion
   
  RETURN f_fecha2
END FUNCTION 

--Valida si existe el folio de entrada en cnt_transaccion
FUNCTION f_valida_folio_cnt()
  DEFINE 
    f_val_folio              SMALLINT
     
  SELECT COUNT(*) 
  INTO   f_val_folio 
  FROM   cnt_transaccion T 
  WHERE  T.folio_cnt = f_folio
   
  RETURN f_val_folio
END FUNCTION 

--Valida si existe el proceso de entrada en cnt_transaccion
FUNCTION f_valida_proceso_cnt()
  DEFINE 
    f_proceso                SMALLINT
     
  SELECT COUNT(*) 
  INTO   f_proceso 
  FROM   cnt_transaccion T 
  WHERE  T.cod_proceso_cnt = v_cmb_proceso
   
  RETURN f_proceso
END FUNCTION 

--Valida si existe el documento contable de entrada en tablas
FUNCTION f_valida_doc_cnt()
  DEFINE 
    f_doc                    SMALLINT
     
  SELECT COUNT(*) 
  INTO   f_doc 
  FROM   cnt_ctr_proceso P 
  WHERE  P.num_poliza = f_doc_contable
   
  RETURN f_doc 
END FUNCTION 

--Valida si existe el folio de liquidación cnt_transaccion
FUNCTION f_valida_folio_liquida()
  DEFINE 
    f_liq                    SMALLINT
     
  SELECT COUNT(*) 
  INTO   f_liq 
  FROM   cnt_transaccion T 
  WHERE  T.folio_liquida = f_folio_liquida
   
  RETURN f_liq 
END FUNCTION 

--Función que llena el primer detalle de Póliza Contable al recibir los parámetros de entrada
FUNCTION fn_llena_det_inf_pol_cont(v_comp_query) 
  DEFINE 
    v_query                  STRING,
    v_id_det1                INTEGER,
    v_counter                INTEGER,
    v_id_folio               INTEGER, 
    v_suma_cargos            LIKE cnt_transaccion.importe,
    v_suma_abonos            LIKE cnt_transaccion.importe,
    v_comp_query             STRING,
    v_tot_fol_liq            SMALLINT

  LET v_id_folio = 1

  --Creamos tabla temporal
  CALL fn_crea_tabla_tmp()
   
  -- Consulta para obtener el folio de cnt_transaccion
 
  --Declara la sentencia SQL para llenar el primer detalle
  LET v_query = "\n SELECT T.folio_cnt, ",
                "\n        T.folio_liquida, ",
                "\n        f_liquida, ",
                "\n        T.cod_proceso_cnt",
                "\n FROM   cnt_transaccion T, cat_proceso_cnt P ",
                "\n WHERE  P.cod_proceso_cnt = T.cod_proceso_cnt ",
                "\n AND    T.tpo_transaccion = 0" -- 0 es sin reverso y 1 es reversado

  LET v_query = v_query || v_comp_query

  LET v_query = v_query ||   "\n GROUP BY 1,2,3,4 "  

  DISPLAY "Consulta para obtener el folio ",v_query

  PREPARE prp_obtiene_folio FROM v_query
  DECLARE cur_obtiene_folio CURSOR FOR prp_obtiene_folio

  FOREACH cur_obtiene_folio INTO f_folio, v_folio_liq, v_f_liquida, v_cod_proceso_cnt

    --Insertamos en la tabla temporal
    INSERT INTO tmp_cnt_folio_liquida (folio_liquida, 
                                       f_liquida, 
                                       cod_proceso_cnt )
                               VALUES (v_folio_liq, 
                                       v_f_liquida, 
                                       v_cod_proceso_cnt)
    
    LET v_id_folio = v_id_folio + 1

    DISPLAY "Folio cnt-- ",f_folio
    DISPLAY "Folio liquida-- ",v_folio_liq
    DISPLAY "Fecha liquida-- ",v_f_liquida
   
  END FOREACH 

  --DATABASE safre_tmp
    EXECUTE IMMEDIATE 
    "CREATE INDEX xcnt_fol_liquida ON tmp_cnt_folio_liquida
    (folio_liquida) USING BTREE IN cnt_ix_dbs"

    UPDATE STATISTICS FOR TABLE tmp_cnt_folio_liquida     

    SELECT COUNT(*)
      INTO v_tot_fol_liq
      FROM tmp_cnt_folio_liquida
      
  DATABASE safre_viv
  
  LET v_query =  "\n SELECT T.f_liquida, ",
                 "\n	    T.folio_liquida, ",
                 "\n	    T.folio_cnt, ",
                 "\n	    T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                 "\n	    T.cta_contable, ",
                 "\n  	    C.desc_cta_contable, ",
                 "\n	    T.importe , ", --Cargo
                 "\n	    0, ", --Abono
                 "\n	    T.estado || '-' || E.desc_estado_cnt Estado, ",
                 "\n	    T.cod_proceso_cnt, ",
                 "\n	    T.id_cuenta_contable,",
                 "\n	    T.tpo_transaccion",
                 "\n   FROM cnt_transaccion T, ",
                 "\n	    cat_proceso_cnt P, ",
                 "\n	    cat_cuenta_contable C, ",
                 "\n	    cat_estado_cnt E ",
                 "\n  WHERE P.cod_proceso_cnt    = T.cod_proceso_cnt ",
                 "\n	AND C.cta_contable       = T.cta_contable ",
                 "\n	AND E.cod_estado_cnt     = T.estado ",
                 "\n	AND T.cod_naturaleza_cta = 2 "
                 --"\n			AND T.folio_cnt = ",f_folio, 
                 --"\n			AND T.folio_liquida = ",v_folio_liq, 
                 LET v_query = v_query || v_comp_query
                 LET v_query = v_query ||
                 --"\n GROUP BY 4,2,3,1,5,6,7,8,9,10 ",
                 --"\n ORDER BY 10 ",,
                 "\n UNION ",
                 "\n SELECT T.f_liquida, ",
                 "\n	    T.folio_liquida, ",
                 "\n	    T.folio_cnt, ",
                 "\n	    T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                 "\n	    T.cta_contable, ",
                 "\n	    C.desc_cta_contable, ",
                 "\n	    0, ", --Cargo
                 "\n	    T.importe, ", --Abono
                 "\n	    T.estado || '-' || E.desc_estado_cnt Estado, ",
                 "\n	    T.cod_proceso_cnt, ",
                 "\n	    T.id_cuenta_contable, ",
                 "\n	    T.tpo_transaccion",
                 "\n  FROM  cnt_transaccion T, ",
                 "\n	    cat_proceso_cnt P, ",
                 "\n	    cat_cuenta_contable C, ",
                 "\n	    cat_estado_cnt E ",
                 "\n  WHERE P.cod_proceso_cnt    = T.cod_proceso_cnt ",
                 "\n	AND C.cta_contable       = T.cta_contable ",
                 "\n	AND E.cod_estado_cnt     = T.estado ",
                 "\n	AND T.cod_naturaleza_cta = 1 "
                 --"\n			AND T.folio_cnt = ",f_folio,
                 --"\n			AND T.folio_liquida = ",v_folio_liq,
                 LET v_query = v_query || v_comp_query
                 LET v_query = v_query ||  
                 --"\n GROUP BY 4,2,3,1,5,6,7,8,9,10 ",
                 "\n ORDER BY 1,2,3,11 " --Ordenado por fecha e id de la cuenta

  DISPLAY "Consulta de detalle 1 -- ", v_query
  PREPARE prp_det_inf_pol_cont1 FROM v_query

  LET v_id_det1 = 1

  --Declara el cursor para la consulta
  DECLARE cur_pol_cont1 CURSOR FOR prp_det_inf_pol_cont1
  FOREACH cur_pol_cont1 INTO g_arr_inf_poliza_contable[v_id_det1].arr_po_fecha,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_folio_liquida,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_folio,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_proceso,     
                             --g_arr_inf_poliza_contable[v_id_det1].arr_po_documento,     
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_cuenta,     
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_descripcion,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_cargo,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_abono,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_estado,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_cod_pro,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_id_cuenta,
                             v_tpo_transaccion

    --Si la transacción es 1 entonces se le concatena al estado la leyenda "Reversado"
    DISPLAY "v_tpo_transaccion -- ",v_tpo_transaccion
    DISPLAY "antes ",g_arr_inf_poliza_contable[v_id_det1].arr_po_estado
    IF v_tpo_transaccion = 1 THEN 
       LET g_arr_inf_poliza_contable[v_id_det1].arr_po_estado = g_arr_inf_poliza_contable[v_id_det1].arr_po_estado CLIPPED ||"-REVERSO"
    END IF 

    DISPLAY "después ",g_arr_inf_poliza_contable[v_id_det1].arr_po_estado
                           
    LET v_id_det1 = v_id_det1 + 1
  END FOREACH  

  LET v_suma_cargos = 0
  LET v_suma_abonos = 0

  --Borra el último elemento vacío
  CALL g_arr_inf_poliza_contable.deleteElement(v_id_det1)
      
  LET v_id_det1 = v_id_det1 - 1

  FOR v_counter = 1 TO v_id_det1 
      LET v_suma_cargos = v_suma_cargos + g_arr_inf_poliza_contable[v_counter].arr_po_cargo
      LET v_suma_abonos = v_suma_abonos + g_arr_inf_poliza_contable[v_counter].arr_po_abono
  END FOR 
      
  RETURN v_id_det1, v_suma_cargos, v_suma_abonos, v_tot_fol_liq
   
END FUNCTION 

--Función que llena el segundo detalle a partir de las fechas y folios de liquidación obtenidas en la consulta de la Póliza Contable
FUNCTION fn_llena_det_inf_montos_op(v_cod_pro_cnt) 
  DEFINE 
    v_query                  STRING,
    v_id_det1                INTEGER,
    v_flag                   BOOLEAN,
    v_counter                INTEGER,
    v_suma_pesos             DECIMAL(22,2),
    v_suma_aivs              DECIMAL(22,2),
    v_comp_query             STRING,
    v_liq                    INTEGER,
    v_cod_pro_cnt            SMALLINT,
    v_folio_liquida_t          LIKE cnt_transaccion.folio_liquida

   LET v_liq     = 1
   LET v_id_det1 = 1
   LET v_query = ""

   PREPARE fn_tbl_mov FROM "execute function fn_tab_movimiento(?,?,?)"

  IF v_cod_proceso_cnt = 1 OR
     v_cod_proceso_cnt = 3 OR
     v_cod_proceso_cnt = 4 THEN
     LET v_query = "\nSELECT M1.f_liquida, M1.folio_liquida, ",
                   "\n       M1.subcuenta || '-' || S1.subcuenta_desc Subcuenta, ",
                   "\n       M1.movimiento || '-' || N1.movimiento_desc Movimiento, ",
                   "\n       '', ",
                   "\n       SUM(M1.importe) pesos, 0",
                   "\n FROM  tmp_cnt_folio_liquida T1 ",
                   "\n       LEFT JOIN cta_fondo72 M1    ON M1.folio_liquida = T1.folio_liquida ",
                   "\n       LEFT JOIN cat_subcuenta S1  ON S1.subcuenta     = M1.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento N1 ON N1.movimiento    = M1.movimiento ", 
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "                 
  END IF

  IF v_cod_proceso_cnt = 5 OR
     v_cod_proceso_cnt = 8 THEN
     LET v_query = "\nSELECT M2.f_liquida, M2.folio_liquida, ",
                   "\n       M2.subcuenta || '-' || S2.subcuenta_desc Subcuenta, ",
                   "\n       M2.movimiento || '-' || N2.movimiento_desc Movimiento, ",
                   "\n       '', ",
                   "\n       SUM(M2.monto_pesos) pesos, SUM(M2.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T2 ",
                   "\n       LEFT JOIN cta_decreto M2    ON M2.folio_liquida = T2.folio_liquida ",
                   "\n       LEFT JOIN cat_subcuenta S2  ON S2.subcuenta     = M2.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento N2 ON N2.movimiento    = M2.movimiento ", 
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "                 
  END IF

  IF v_cod_proceso_cnt = 6 THEN
     SELECT folio_liquida
     INTO v_folio_liquida_t
     FROM  tmp_cnt_folio_liquida
     --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida_t,Sin INTO v_tbl_mov --por folio
      LET v_query = "\nSELECT M.f_liquida, M.folio_liquida, ",
                   "\n       M.subcuenta || '-' || S.subcuenta_desc Subcuenta, ",
                   "\n	     M.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       M.fondo_inversion || '-' || F.razon_social Fondo, ",
                   "\n       SUM (M.monto_pesos) pesos, SUM(M.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T ",
                   "\n       LEFT JOIN ",v_tbl_mov," M  ON    M.folio_liquida   = T.folio_liquida ",
                   "\n                                   AND   T.cod_proceso_cnt = ",v_cod_pro_cnt,
                   "\n                                   AND   T.f_liquida       = M.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta  S  ON    M.subcuenta       = S.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento N  ON    M.movimiento      = N.movimiento ",
                   "\n       LEFT JOIN cat_fondo_local F ON    M.fondo_inversion = F.fondo ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL",
                   "\nSELECT M2.f_liquida, M2.folio_liquida, ",
                   "\n       M2.subcuenta || '-' || S2.subcuenta_desc Subcuenta, ",
                   "\n       M2.movimiento || '-' || N2.movimiento_desc Movimiento, ",
                   "\n       '', ",
                   "\n       SUM(M2.monto_pesos) pesos, SUM(M2.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T2 ",
                   "\n       LEFT JOIN cta_decreto M2    ON    M2.folio_liquida   = T2.folio_liquida ",
                   "\n                                   AND   T2.f_liquida       = M2.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta S2  ON    M2.subcuenta       = S2.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento N2 ON    M2.movimiento      = N2.movimiento ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "                 
  END IF

  IF v_cod_proceso_cnt = 7 THEN
     LET v_query = "\nSELECT M3.f_saldo, M3.folio, ",
                   "\n       M3.subcuenta || '-' || S3.subcuenta_desc Subcuenta, ",
                   "\n       'RENDIMIENTOS', ",
                   "\n       '', ",
                   "\n       SUM(M3.monto_pesos) pesos, SUM(M3.monto_acciones) acciones ",
                   "\n FROM  safre_sdo@vivws_tcp:cta_saldo_diario_global M3, ",
                   "\n       tmp_cnt_folio_liquida T3, ",
                   "\n       cat_subcuenta S3 ",
                   "\n WHERE M3.folio            = T3.folio_liquida ",
                   "\n AND   T3.f_liquida        = M3.f_saldo ",
                   "\n AND   M3.subcuenta        = S3.subcuenta ",
                   "\n AND   M3.ind_consistencia = 1 ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT M4.f_saldo, M4.folio, ",
                   "\n       M4.subcuenta || '-' || S4.subcuenta_desc Subcuenta,",
                   "\n       'RENDIMIENTOS NO CONSISTENTES', ",
                   "\n       '', ",
                   "\n       SUM(M4.monto_pesos) pesos, SUM(M4.monto_acciones) acciones ",
                   "\n FROM  safre_sdo@vivws_tcp:cta_saldo_diario_global M4, ",
                   "\n       tmp_cnt_folio_liquida T4, ",
                   "\n       cat_subcuenta S4 ",
                   "\n WHERE M4.folio            = T4.folio_liquida ",
                   "\n AND   T4.f_liquida        = M4.f_saldo ",
                   "\n AND   M4.subcuenta        = S4.subcuenta ",
                   "\n AND   M4.subcuenta        = 48 ",
                   "\n AND   M4.ind_consistencia = 0 ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "                 
  END IF

  IF v_cod_proceso_cnt = 14 OR
     v_cod_proceso_cnt = 17 OR 
     v_cod_proceso_cnt = 18 OR 
     v_cod_proceso_cnt = 22 OR 
     v_cod_proceso_cnt = 26 OR 
     v_cod_proceso_cnt = 27 OR 
     v_cod_proceso_cnt = 28 OR 
     v_cod_proceso_cnt = 29 OR 
     v_cod_proceso_cnt = 30 OR 
     v_cod_proceso_cnt = 31 OR 
     v_cod_proceso_cnt = 32 OR 
     v_cod_proceso_cnt = 36 OR 
     v_cod_proceso_cnt = 37 OR 
     v_cod_proceso_cnt = 38 OR 
     v_cod_proceso_cnt = 39 OR 
     v_cod_proceso_cnt = 42 OR 
     v_cod_proceso_cnt = 43 OR 
     v_cod_proceso_cnt = 44 OR 
     v_cod_proceso_cnt = 45 OR
     v_cod_proceso_cnt = 49 OR
     v_cod_proceso_cnt = 51 THEN
     SELECT folio_liquida
     INTO v_folio_liquida_t
     FROM  tmp_cnt_folio_liquida
     --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida_t,Sin INTO v_tbl_mov --por folio
     LET v_query = "\nSELECT M.f_liquida, M.folio_liquida, ",
                   "\n       M.subcuenta || '-' || S.subcuenta_desc Subcuenta, ",
                   "\n	     M.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       M.fondo_inversion || '-' || F.razon_social Fondo, ",
                   "\n       SUM (M.monto_pesos) pesos, SUM(M.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T ",
                   "\n       LEFT JOIN ",v_tbl_mov,"  M  ON    M.folio_liquida   = T.folio_liquida ",
                   "\n                                    AND   T.cod_proceso_cnt = ",v_cod_pro_cnt,
                   "\n                                    AND   T.f_liquida       = M.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta   S  ON    M.subcuenta       = S.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento  N  ON    M.movimiento      = N.movimiento ",
                   "\n       LEFT JOIN cat_fondo_local F  ON    M.fondo_inversion = F.fondo ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "                 
  END IF

  IF v_cod_proceso_cnt = 15 OR
     v_cod_proceso_cnt = 16 THEN
     SELECT folio_liquida
     INTO v_folio_liquida_t
     FROM  tmp_cnt_folio_liquida
     --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida_t,Sin INTO v_tbl_mov --por folio
     LET v_query = "\nSELECT M.f_liquida, M.folio_liquida, ",
                   "\n       M.subcuenta || '-' || S.subcuenta_desc Subcuenta, ",
                   "\n       M.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       M.fondo_inversion || '-' || F.razon_social Fondo, ",
                   "\n       SUM (M.monto_pesos) pesos, SUM(M.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T ",
                   "\n       LEFT JOIN ",v_tbl_mov,"  M  ON    M.folio_liquida   = T.folio_liquida ",
                   "\n                                    AND   T.cod_proceso_cnt = ",v_cod_pro_cnt,
                   "\n                                    AND   T.f_liquida       = M.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta   S  ON    M.subcuenta       = S.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento  N  ON    M.movimiento      = N.movimiento ",
                   "\n       LEFT JOIN cat_fondo_local F  ON    M.fondo_inversion = F.fondo ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL",
                   "\nSELECT pag.f_proceso, pag.folio,",
                   "\n       '', 'APORTACIÓN DEL ACLARATORIO',",
                   "\n       '', SUM(pag.imp_ap_pat) pesos, 0",
                   "\n FROM  tmp_cnt_folio_liquida c",
                   "\n       LEFT JOIN cta_his_pagos pag  ON   pag.folio           = c.folio_liquida ",
                   "\n                                    AND  pag.ind_liquidacion = 1 ",
                   "\n GROUP BY 1,2,3,4,5 ",
                    "\n UNION ALL",
                   "\nSELECT pag.f_proceso, pag.folio,",
                   "\n       '', 'AMORTIZACIÓN DEL ACLARATORIO',",
                   "\n       '', SUM(pag.imp_am_cre) pesos, 0",
                   "\n FROM  tmp_cnt_folio_liquida c",
                   "\n       LEFT JOIN cta_his_pagos pag  ON   pag.folio           = c.folio_liquida ",
                   "\n                                    AND  pag.ind_liquidacion = 1 ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL",
                   "\nSELECT pag.f_proceso, pag.folio,",
                   "\n       '', 'INTERESES DEL ACLARATORIO',",
                   "\n       '', SUM(pag.int_gen_pgo_ext) pesos, 0",
                   "\n FROM  tmp_cnt_folio_liquida c",
                   "\n       LEFT JOIN cta_his_pagos pag  ON   pag.folio           = c.folio_liquida ",
                   "\n                                    AND  pag.ind_liquidacion = 1 ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "                 
  END IF

  IF v_cod_proceso_cnt = 19 THEN
     SELECT folio_liquida
     INTO v_folio_liquida_t
     FROM  tmp_cnt_folio_liquida
     --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida_t,Sin INTO v_tbl_mov --por folio
     LET v_query = "\nSELECT M.f_liquida, M.folio_liquida, ",
                   "\n       M.subcuenta || '-' || S.subcuenta_desc Subcuenta, ",
                   "\n       M.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       M.fondo_inversion || '-' || F.razon_social Fondo, ",
                   "\n       SUM (M.monto_pesos) pesos, SUM(M.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T ",
                   "\n       LEFT JOIN ",v_tbl_mov,"  M  ON    M.folio_liquida   = T.folio_liquida ",
                   "\n                                    AND   T.cod_proceso_cnt = ",v_cod_pro_cnt,
                   "\n                                    AND   T.f_liquida       = M.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta   S  ON    M.subcuenta       = S.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento  N  ON    M.movimiento      = N.movimiento ",
                   "\n       LEFT JOIN cat_fondo_local F  ON    M.fondo_inversion = F.fondo ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), M5.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'AVANCES DE PAGO - APORTACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M5.monto_apo_avance * -1) pesos, 0 ",
                   "\n FROM  tmp_cnt_folio_liquida T5 ",
                   "\n       LEFT JOIN dis_compensa_avance M5 ON    M5.folio_dis         = T5.folio_liquida ",
                   "\n                                        AND   M5.edo_compensa_apo  IN (0,1,2) ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), M6.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'AVANCES DE PAGO - AMORTIZACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M6.monto_amo_avance * -1) pesos, 0 ",
                   "\n FROM  tmp_cnt_folio_liquida T6 ",
                   "\n       LEFT JOIN dis_compensa_avance M6 ON    M6.folio_dis         = T6.folio_liquida ",
                   "\n                                        AND   M6.edo_compensa_amo  IN (0,1,2) ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), M7.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO VIRTUAL', ",
                   "\n       ' ', ",
                   "\n       SUM(M7.monto_amo_avance * -1) pesos, 0 ",
                   "\n FROM  tmp_cnt_folio_liquida T7 ",
                   "\n       LEFT JOIN dis_compensa_avance M7 ON    M7.folio_dis         = T7.folio_liquida ",
                   "\n                                        AND   M7.edo_compensa_amo  IN (3,4,5) ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), M8.folio_liquida, ",
                   "\n       ' ', ",
                   "\n       'PAGO REAL', ",
                   "\n       ' ', ",
                   "\n       SUM(M8.imp_ap_pat + M8.imp_am_cre) * -1 pesos, 0 ",
                   "\n FROM  tmp_cnt_folio_liquida T8 ",
                   "\n       LEFT JOIN dis_interface_hs M8 ON    M8.folio_liquida         = T8.folio_liquida ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K8.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO MENOR AL AVANCE - APORTACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M8.monto_dif_apo * -1) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M8, ",
                   "\n       dis_compensa_avance K8, ",
                   "\n       tmp_cnt_folio_liquida T8 ",
                   "\n WHERE K8.id_dis_det_avance_pago = M8.id_dis_det_avance_pago ",
                   "\n AND   K8.folio_dis              = T8.folio_liquida ",
                   "\n AND   K8.edo_compensa_apo       = 1",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K9.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO MENOR AL AVANCE - AMORTIZACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M9.monto_dif_amo * -1) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M9, ",
                   "\n       dis_compensa_avance K9, ",
                   "\n       tmp_cnt_folio_liquida T9 ",
                   "\n WHERE K9.id_dis_det_avance_pago = M9.id_dis_det_avance_pago ",
                   "\n AND   K9.folio_dis              = T9.folio_liquida ",
                   "\n AND   K9.edo_compensa_amo       = 1",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K8.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO MAYOR AL AVANCE - APORTACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M8.monto_dif_apo) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M8, ",
                   "\n       dis_compensa_avance K8, ",
                   "\n       tmp_cnt_folio_liquida T8 ",
                   "\n WHERE K8.id_dis_det_avance_pago = M8.id_dis_det_avance_pago ",
                   "\n AND   K8.folio_dis              = T8.folio_liquida ",
                   "\n AND   K8.edo_compensa_apo       = 2",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K9.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO MAYOR AL AVANCE - AMORTIZACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M9.monto_dif_amo) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M9, ",
                   "\n       dis_compensa_avance K9, ",
                   "\n       tmp_cnt_folio_liquida T9 ",
                   "\n WHERE K9.id_dis_det_avance_pago = M9.id_dis_det_avance_pago ",
                   "\n AND   K9.folio_dis              = T9.folio_liquida ",
                   "\n AND   K9.edo_compensa_amo       = 2",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), M10.folio_liquida, ",
                   "\n       ' ', ",
                   "\n       'PAGO ENTIDADES FINANCIERAS', ",
                   "\n       ' ', ",
                   "\n       SUM(M10.imp_ap_pat * -1) pesos, 0 ",
                   "\n FROM  tmp_cnt_folio_liquida T10 ",
                   "\n       LEFT JOIN dis_interface_ef M10   ON  M10.folio_liquida   = T10.folio_liquida ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), M11.folio_referencia, ",
                   "\n       M11.subcuenta || '-' || S11.subcuenta_desc Subcuenta, ",
                   "\n       'DEVOLUCION DE PAGOS - RESTITUCION', ",
                   "\n       ' ', ",
                   "\n       SUM(M11.monto_aportacion * -1) pesos, 0 ",
                   "\n FROM  tmp_cnt_folio_liquida T11 ",
                   "\n       LEFT JOIN dse_devolucion M11   ON  M11.folio_referencia = T11.folio_liquida ",
                   "\n       LEFT JOIN cat_subcuenta  S11   ON  M11.subcuenta        = S11.subcuenta  ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K8.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO VIRTUAL MENOR AL AVANCE - APORTACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M8.monto_dif_apo * -1) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M8, ",
                   "\n       dis_compensa_avance K8, ",
                   "\n       tmp_cnt_folio_liquida T8 ",
                   "\n WHERE K8.id_dis_det_avance_pago = M8.id_dis_det_avance_pago ",
                   "\n AND   K8.folio_dis              = T8.folio_liquida ",
                   "\n AND   K8.edo_compensa_apo       = 4",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K9.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO VIRTUAL MENOR AL AVANCE - AMORTIZACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M9.monto_dif_amo * -1) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M9, ",
                   "\n       dis_compensa_avance K9, ",
                   "\n       tmp_cnt_folio_liquida T9 ",
                   "\n WHERE K9.id_dis_det_avance_pago = M9.id_dis_det_avance_pago ",
                   "\n AND   K9.folio_dis              = T9.folio_liquida ",
                   "\n AND   K9.edo_compensa_amo       = 4",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K8.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO VIRTUAL MAYOR AL AVANCE - APORTACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M8.monto_dif_apo) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M8, ",
                   "\n       dis_compensa_avance K8, ",
                   "\n       tmp_cnt_folio_liquida T8 ",
                   "\n WHERE K8.id_dis_det_avance_pago = M8.id_dis_det_avance_pago ",
                   "\n AND   K8.folio_dis              = T8.folio_liquida ",
                   "\n AND   K8.edo_compensa_apo       = 5",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL ",
                   "\nSELECT CAST('01/01/0001' AS DATE), K9.folio_dis, ",
                   "\n       ' ', ",
                   "\n       'PAGO VIRTUAL MAYOR AL AVANCE - AMORTIZACION', ",
                   "\n       ' ', ",
                   "\n       SUM(M9.monto_dif_amo) pesos, 0 ",
                   "\n FROM  dis_det_avance_pago M9, ",
                   "\n       dis_compensa_avance K9, ",
                   "\n       tmp_cnt_folio_liquida T9 ",
                   "\n WHERE K9.id_dis_det_avance_pago = M9.id_dis_det_avance_pago ",
                   "\n AND   K9.folio_dis              = T9.folio_liquida ",
                   "\n AND   K9.edo_compensa_amo       = 5",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "
  END IF

  IF v_cod_proceso_cnt = 20 OR
     v_cod_proceso_cnt = 34 THEN
     SELECT folio_liquida
     INTO v_folio_liquida_t
     FROM  tmp_cnt_folio_liquida
     --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida_t,Sin INTO v_tbl_mov --por folio
     LET v_query = "\nSELECT M.f_liquida, M.folio_liquida, ",
                   "\n       M.subcuenta || '-' || S.subcuenta_desc Subcuenta, ",
                   "\n       M.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       M.fondo_inversion || '-' || F.razon_social Fondo, ",
                   "\n       SUM (M.monto_pesos) pesos, SUM(M.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T ",
                   "\n       LEFT JOIN ",v_tbl_mov," M   ON  M.folio_liquida   = T.folio_liquida ",
                   "\n                                    AND T.cod_proceso_cnt = ",v_cod_pro_cnt,
                   "\n                                    AND T.f_liquida       = M.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta  S   ON  M.subcuenta       = S.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento N   ON  M.movimiento      = N.movimiento ",
                   "\n       LEFT JOIN cat_fondo_local F  ON  M.fondo_inversion = F.fondo ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL",
                   "\nSELECT a.f_movimiento, a.folio_referencia, ",
                   "\n       '', a.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       '', SUM(a.monto_pesos) pesos, SUM(a.monto_aivs) acciones",
                   "\n FROM  tmp_cnt_folio_liquida c ",
                   "\n       LEFT JOIN cre_saldo_deudor a  ON  a.folio_referencia = c.folio_liquida ",
                   "\n                                     AND a.movimiento      IN (252,262) ",
                   "\n       LEFT JOIN cat_movimiento N    ON  a.movimiento       = N.movimiento ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL",
                   "\nSELECT M1.f_liquida, M1.folio_liquida, ",
                   "\n       M1.subcuenta || '-' || S1.subcuenta_desc Subcuenta, ",
                   "\n       M1.movimiento || '-' || N1.movimiento_desc Movimiento, ",
                   "\n       '', ",
                   "\n       SUM(M1.importe) pesos, 0",
                   "\n FROM  tmp_cnt_folio_liquida T1 ",
                   "\n       LEFT JOIN cta_fondo72 M1     ON  M1.folio_liquida   = T1.folio_liquida ",
                   "\n                                    AND T1.f_liquida       = M1.f_liquida ",
                   "\n       LEFT JOIN cat_movimiento N1  ON  M1.movimiento      = N1.movimiento ",
                   "\n       LEFT JOIN cat_subcuenta S1   ON  M1.subcuenta       = S1.subcuenta ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "
  END IF

  IF v_cod_proceso_cnt = 46 OR
     v_cod_proceso_cnt = 47 OR
     v_cod_proceso_cnt = 48 THEN
     SELECT folio_liquida
     INTO v_folio_liquida_t
     FROM  tmp_cnt_folio_liquida
     --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida_t,Sin INTO v_tbl_mov --por folio
     LET v_query = "\nSELECT M.f_liquida, M.folio_liquida, ",
                   "\n       M.subcuenta || '-' || S.subcuenta_desc Subcuenta, ",
                   "\n       M.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       M.fondo_inversion || '-' || F.razon_social Fondo, ",
                   "\n       SUM (M.monto_pesos) pesos, SUM(M.monto_acciones) acciones",
                   "\n FROM  tmp_cnt_folio_liquida T ",
                   "\n       LEFT JOIN ",v_tbl_mov," M   ON  M.folio_liquida   = T.folio_liquida ",
                   "\n                                    AND T.cod_proceso_cnt = ",v_cod_pro_cnt,
                   "\n                                    AND T.f_liquida       = M.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta S    ON  M.subcuenta       = S.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento N   ON  M.movimiento      = N.movimiento ",
                   "\n       LEFT JOIN cat_fondo_local F  ON  M.fondo_inversion = F.fondo ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n UNION ALL",
                   "\nSELECT M1.f_liquida, M1.folio_liquida, ",
                   "\n       M1.subcuenta || '-' || S1.subcuenta_desc Subcuenta, ",
                   "\n       M1.movimiento || '-' || N1.movimiento_desc Movimiento, ",
                   "\n       '', ",
                   "\n       SUM(M1.importe) pesos, 0",
                   "\n FROM  tmp_cnt_folio_liquida T1 ",
                   "\n       LEFT JOIN cta_fondo72 M1     ON  M1.folio_liquida   = T1.folio_liquida ",
                   "\n                                    AND T1.f_liquida       = M1.f_liquida ",
                   "\n       LEFT JOIN cat_subcuenta S1   ON  M1.subcuenta       = S1.subcuenta ",
                   "\n       LEFT JOIN cat_movimiento N1  ON  M1.movimiento      = N1.movimiento ",
                   "\n GROUP BY 1,2,3,4,5 ",
                   "\n ORDER BY 1,2,3,4,5 "
  END IF

  DISPLAY "Consulta 2 -- ",v_query
  IF v_query IS NULL THEN
     CALL fn_mensaje("Error", "Error: No se tiene consulta para el proceso", "information")
  ELSE
  
     PREPARE prp_det_inf_montos_op FROM v_query

     LET v_id_det1 = 1

     --Declara el cursor para la consulta
     DECLARE cur_montos_op CURSOR FOR prp_det_inf_montos_op
     FOREACH cur_montos_op INTO g_arr_inf_montos_op[v_id_det1].arr_op_fecha,
                                g_arr_inf_montos_op[v_id_det1].arr_op_folio,
                                g_arr_inf_montos_op[v_id_det1].arr_op_subcuenta,
                                g_arr_inf_montos_op[v_id_det1].arr_op_tipo,
                                g_arr_inf_montos_op[v_id_det1].arr_op_fondo,
                                g_arr_inf_montos_op[v_id_det1].arr_op_pesos,
                                g_arr_inf_montos_op[v_id_det1].arr_op_aivs

       IF g_arr_inf_montos_op[v_id_det1].arr_op_aivs IS NULL THEN
          LET g_arr_inf_montos_op[v_id_det1].arr_op_aivs = 0.00
       END IF 

       IF g_arr_inf_montos_op[v_id_det1].arr_op_pesos IS NULL THEN
          LET g_arr_inf_montos_op[v_id_det1].arr_op_pesos = 0.00
       END IF
                                  
       LET v_id_det1 = v_id_det1 + 1
     END FOREACH 
         
     LET v_suma_pesos = 0
     LET v_suma_aivs  = 0

     --Borra el último elemento vacío
     CALL g_arr_inf_montos_op.deleteElement(v_id_det1)

     LET v_id_det1 = v_id_det1 - 1
        
     FOR v_counter = 1 TO v_id_det1
         LET v_suma_pesos = v_suma_pesos + g_arr_inf_montos_op[v_counter].arr_op_pesos
         LET v_suma_aivs  = v_suma_aivs  + g_arr_inf_montos_op[v_counter].arr_op_aivs
     END FOR 

     DISPLAY "SUMA AIVS ",v_suma_aivs
     DISPLAY "SUMA PESOS ",v_suma_pesos
  END IF
      
  RETURN v_id_det1, v_suma_pesos, v_suma_aivs

END FUNCTION 

FUNCTION fn_limpia_campos_entrada()

  LET v_cmb_proceso       = NULL 
  LET f_fecha_liquidacion = NULL 
  LET f_folio             = NULL 
  LET f_doc_contable      = NULL
  LET f_folio_liquida     = NULL 

END FUNCTION

--Función para verificar si existen registros de la consulta hecha con estado = 30-CONTABILIZADO SAP - FICO

FUNCTION verifica_rechazo(v_comp_query)
  DEFINE 
    v_query                  STRING,
    v_counter                INTEGER,
    v_comp_query             STRING 

  LET v_counter = 0

  LET v_query   =  "\n SELECT count(*)", 
                   "\n   FROM cnt_transaccion T, cat_proceso_cnt P, cat_estado_cnt E, cat_cuenta_contable C ",
                   "\n	WHERE T.cod_proceso_cnt = P.cod_proceso_cnt ",
                   "\n    AND T.cta_contable    = C.cta_contable ",
                   "\n    AND T.estado          = E.cod_estado_cnt ",
             --      "\n   AND T.folio_cnt         = R.folio_cnt ",
                   "\n    AND T.estado         in (20,30,70) "

  LET v_query   = v_query || v_comp_query

  PREPARE prp_verifica_rechazo FROM v_query
  EXECUTE prp_verifica_rechazo INTO v_counter  

  RETURN v_counter
                  
END FUNCTION 

--Función para verificar que exista un error con base en el folio_cnt
FUNCTION verifica_folio_reg_contable(v_comp_query)
  DEFINE 
    v_query                  STRING,
    v_comp_query             STRING,
    v_id_det1                SMALLINT

  --Agregar fecha liquidación y folio liquida
  LET v_query =  "\n	SELECT T.f_liquida, ER.f_respuesta, T.folio_liquida, T.folio_cnt, ER.error ",
                 "\n	FROM   cnt_transaccion T, cnt_error_poliza ER, cat_proceso_cnt P, cat_estado_cnt E, cat_cuenta_contable C ",
                 "\n	WHERE  T.folio_cnt       = ER.folio_cnt ",
                 "\n	AND    T.cod_proceso_cnt = P.cod_proceso_cnt ",
                 "\n	AND    T.cta_contable    = C.cta_contable ",
                 "\n	AND    T.estado          = E.cod_estado_cnt "
                  
          --       "\n	AND T.folio_cnt = R.folio_cnt "
                  
  LET v_query =  v_query || v_comp_query
  LET v_query = v_query||"\n	GROUP BY 1,2,3,4,5"
   
  PREPARE prp_verifica_folio_reg_contable FROM v_query

  LET v_id_det1 = 1
  DECLARE cur_verifica_folio CURSOR FOR prp_verifica_folio_reg_contable
  FOREACH cur_verifica_folio INTO g_arr_rechazo_pol_cnt[v_id_det1].arr_re_fecha_liquida, --Se cambia a feche de liquidación
                                  g_arr_rechazo_pol_cnt[v_id_det1].arr_re_fecha_resp,
                                  g_arr_rechazo_pol_cnt[v_id_det1].arr_re_folo_liquida,
                                  g_arr_rechazo_pol_cnt[v_id_det1].arr_re_folo_cnt,
                                  g_arr_rechazo_pol_cnt[v_id_det1].arr_re_error
    LET v_id_det1 = v_id_det1 + 1
  END FOREACH 
   
  CALL g_arr_rechazo_pol_cnt.deleteElement(v_id_det1)
  DISPLAY "Tamaño del rechazo -- ",g_arr_rechazo_pol_cnt.getLength() 
     
  RETURN g_arr_rechazo_pol_cnt.getLength() 
END FUNCTION 

REPORT rp_poliza_contable(g_arr_inf_poliza_contable, 
                          g_arr_inf_montos_op, 
                          v_cond_reporte, 
                          v_total_registros, 
                          v_total_cargo, 
                          v_total_abono, 
                          v_total_pesos, 
                          v_total_aivs, 
                          g_usuario,
                          v_indicador )

  --Arreglo para la primera sección del reporte
  DEFINE g_arr_inf_poliza_contable RECORD
    arr_po_fecha             LIKE cnt_transaccion.f_liquida,
    arr_po_folio_liquida     LIKE cnt_transaccion.folio_liquida,
    arr_po_folio             CHAR (10),
    arr_po_proceso           CHAR(42),
    --arr_po_documento         CHAR (10),
    arr_po_cuenta            LIKE cnt_transaccion.cta_contable,
    arr_po_descripcion       LIKE cat_cuenta_contable.desc_cta_contable,
    arr_po_cargo             DECIMAL(22,2),
    arr_po_abono             DECIMAL(22,2),
    --arr_po_estado            CHAR(27),
    arr_po_estado            CHAR(40),
    arr_po_cod_pro           LIKE cnt_transaccion.cod_proceso_cnt,
    arr_po_id_cuenta         LIKE cnt_transaccion.id_cuenta_contable
  END RECORD 

  --Arreglo para la segunda sección del reporte
  DEFINE g_arr_inf_montos_op RECORD 
    arr_op_fecha             LIKE cnt_transaccion.f_liquida,
    arr_op_folio             CHAR(10),
    arr_op_subcuenta         CHAR(20),
    arr_op_tipo              CHAR(42),
    arr_op_fondo             CHAR(10),
    arr_op_pesos             DECIMAL(22,2),
    arr_op_aivs              DECIMAL(22,2)
  END RECORD

  DEFINE 
    --Define variables del encabezado
    r_fecha_reporte          DATE,
    v_titulo_reporte         STRING,
    v_cond_reporte           SMALLINT,
    v_indicador              SMALLINT,
    g_usuario                LIKE seg_usuario.usuario,      -- Recibe la variable de usuario
      
    v_total_registros        SMALLINT,
    v_total_cargo            DECIMAL(22,2),
    v_total_abono            DECIMAL(22,2),
    v_total_pesos            DECIMAL(22,2),
    v_total_aivs             DECIMAL(22,2)

   FORMAT 
     FIRST PAGE HEADER
       LET r_fecha_reporte = TODAY CLIPPED
       PRINTX r_fecha_reporte USING "dd-mm-yyyy" 
       DISPLAY g_usuario || " reporte"
         
     BEFORE GROUP OF v_cond_reporte
       --PRINTX v_indicador
       --DISPLAY "before -- ",v_indicador
       PRINTX v_cond_reporte
       PRINTX v_total_registros
       PRINTX v_total_cargo
       PRINTX v_total_abono
       PRINTX v_total_pesos
       PRINTX v_total_aivs
       PRINTX g_usuario
       PRINTX v_desc_cod_proceso    
            
     ON EVERY ROW 
        --PRINTX v_cond_reporte
        --PRINTX v_indicador
        --DISPLAY "v_indicador -- ",v_indicador
      
        --Define los campos para el primer detalle
        PRINTX g_arr_inf_poliza_contable.arr_po_fecha USING "dd-mm-yyyy" 
        PRINTX g_arr_inf_poliza_contable.arr_po_folio
        PRINTX g_arr_inf_poliza_contable.arr_po_folio_liquida
        PRINTX g_arr_inf_poliza_contable.arr_po_proceso
        PRINTX g_arr_inf_poliza_contable.arr_po_estado
        --PRINTX g_arr_inf_poliza_contable.arr_po_documento
        PRINTX g_arr_inf_poliza_contable.arr_po_cuenta
        PRINTX g_arr_inf_poliza_contable.arr_po_descripcion
        PRINTX g_arr_inf_poliza_contable.arr_po_cargo
        PRINTX g_arr_inf_poliza_contable.arr_po_abono

        --Define los campos para el segundo detalle
        PRINTX g_arr_inf_montos_op.arr_op_fecha USING "dd-mm-yyyy" 
        PRINTX g_arr_inf_montos_op.arr_op_folio
        PRINTX g_arr_inf_montos_op.arr_op_subcuenta
        PRINTX g_arr_inf_montos_op.arr_op_tipo
        PRINTX g_arr_inf_montos_op.arr_op_fondo
        PRINTX g_arr_inf_montos_op.arr_op_pesos
        PRINTX g_arr_inf_montos_op.arr_op_aivs
      
END REPORT 

#Función para crear la tabla temporal con base en cnt_transaccion para agilizar las consultas de la póliza contable
FUNCTION fn_crea_tabla_tmp()

DEFINE 
  v_query                    STRING,
  v_existe                   SMALLINT 

  WHENEVER ERROR CONTINUE; 
     DROP TABLE tmp_cnt_folio_liquida;
  WHENEVER ERROR STOP;

  CREATE TABLE tmp_cnt_folio_liquida (folio_liquida     DECIMAL (9,0),
                                      f_liquida         DATE,
                                      cod_proceso_cnt   SMALLINT  
                                      );

  CREATE INDEX xpktmp_cnt_folio_liquida 
  ON tmp_cnt_folio_liquida(folio_liquida,cod_proceso_cnt )
   
END FUNCTION 

--Función que genera el archivo de la póliza contable
FUNCTION fn_genera_poliza_contable()
  DEFINE 
    v_comando                STRING,
    r_bandera                SMALLINT,
    p_programa               CHAR(10),
    v_folio_proc_cnt         DECIMAL(9,0),
    v_s_mensaje              STRING,
    v_ruta_nomarch           VARCHAR(100) -- ruta y nombre del archivo de salida

  LET p_programa       = "CNTP01"
  LET v_folio_proc_cnt = 0
   
  ---## Recupera rutas ##---
  CALL fn_rutas("cnt") RETURNING p_ruta_ejecutable,p_aux
  CALL fn_rutas("bat") RETURNING p_aux,p_ruta_listados

  ---## Genera PID ##---
  CALL fn_genera_pid (g_proceso_cod, g_opera_cod1,g_usuario)
  RETURNING p_pid

  --CALL fn_genera_folio(g_proceso_cod, g_opera_cod1,g_usuario)
  --     RETURNING v_folio_proc_cnt

  CALL fn_inicializa_proceso (p_pid, g_proceso_cod, g_opera_cod1, 0, 
                              p_programa, v_ruta_nomarch, g_usuario)
  RETURNING r_bandera

  -- Inicia la operación asignando el estatus de PROCESANDO
  CALL fn_actualiza_opera_ini(p_pid, g_proceso_cod, g_opera_cod1, 0, 
                              p_programa, v_ruta_nomarch, g_usuario)
  RETURNING r_bandera
      
  LET v_comando = " nohup fglrun ",p_ruta_ejecutable CLIPPED,"/CNTS01 ", 
                  g_usuario," ", 
                  p_pid, " ",
                  g_proceso_cod, " ",
                  g_opera_cod1, " ",
                  v_folio_proc_cnt, "  ",
                  "NA", " ",
                  2, -- 2 indica generación de póliza manual; 1 es batch
                  " 1>", p_ruta_listados CLIPPED ,
                  "/nohup:",p_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod1   USING "&&&&&" ,
                  " 2>&1 &" 

  DISPLAY " exec -- ", v_comando
  RUN v_comando

  LET v_s_mensaje = "Se ha enviado la generación de la póliza contable con el PID: ",
                    p_pid CLIPPED,
                    ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
  CALL fn_mensaje("Generación de póliza contable",v_s_mensaje,"information")
  EXIT PROGRAM 
                           
END FUNCTION 
