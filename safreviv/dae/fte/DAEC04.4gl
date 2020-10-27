--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12/07/2013
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEC04                                                   #
#Objetivo          => Programa para consultar datos generales de Devolución de #
#                     Amortizaciones Excedentes                                #
#Fecha inicio      => 12/07/2013                                               #
################################################################################

DATABASE safre_viv

GLOBALS "DAEG01.4gl"
GLOBALS

CONSTANT MAX_REGISTROS     SMALLINT = 50

DEFINE arr_detalles_globales DYNAMIC ARRAY OF RECORD 
          v_folio                DECIMAL(9,0),
          v_ent_recaudadora      CHAR(3),
          v_desc_ent_recaudadora CHAR(25),
          v_total_registros      DECIMAL(10,0),
          v_monto_pesos          DECIMAL(16,6),
          v_monto_aivs           DECIMAL(16,6),
          v_resul_opera          CHAR(10),
          v_origen               CHAR(10)
END RECORD
 
DEFINE arr_detalles_dae DYNAMIC ARRAY OF RECORD 
          v_det_folio         DECIMAL(9,0),
          v_det_num_credito   CHAR(10),
          v_det_nss           CHAR(11),
          v_det_fecha_pago    DATE,
          v_det_importe_amort DECIMAL(16,6),
          v_det_tot_aivs      DECIMAL(16,6),
          v_det_desc_resul    CHAR(12),
          v_det_origen        CHAR(10),
          v_resultado         CHAR(2)
END RECORD

DEFINE arr_motivo_rechazo DYNAMIC ARRAY OF RECORD 
          v_desc_motivo_rch   CHAR(30)
END RECORD

DEFINE v_estado_registro    SMALLINT,
       v_nombre_archivo     CHAR (40), 
       v_i_det              INTEGER,
       v_id_derechohabiente DECIMAL(9,0),
       v_importe_total_amort DECIMAL(16,6),
       v_total_registros_det INTEGER
      
DEFINE var_dis_hist DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_folio          DECIMAL(9,0),
          v_origen_folio   CHAR(22),
          v_origen_desc    CHAR (10)
END RECORD

DEFINE v_i_rch INTEGER,
       arr_dae_rechazos DYNAMIC ARRAY OF RECORD
          folio         DECIMAL(9,0),
          nss           CHAR(11),
          diagnostico   SMALLINT,
          campo_valor   CHAR(30)
       END RECORD
       
DEFINE v_edit_Folio     LIKE dis_sum_avance_pago.folio, --Recibe el valor del folio a consultar
       v_indice         INTEGER,                        -- Variable del indice
       v_folio_liquida DECIMAL(9,0),
       v_fecha_liquida DATE,
       v_precio_accion DECIMAL(16,6)

--VARIABLES DE TOTALES PARA REPORTE
DEFINE v_suma_aceptados_amort   DECIMAL(16,2), 
       v_d_aivs_aceptados       DECIMAL(16,2), 
       v_total_aceptados        INTEGER,
       v_suma_rechazados_amort  DECIMAL(16,2),      
       v_d_aivs_rechazados      DECIMAL(16,2),      
       v_total_rechazados       INTEGER,
       v_suma_pendientes_amort  DECIMAL(16,2),      
       v_d_aivs_pendientes      DECIMAL(16,2),      
       v_total_pendientes       INTEGER,
       v_i_tot_registros        INTEGER, -- Contador de numero de registro de registros
       v_d_tot_vivienda         DECIMAL(16,2), -- Contador de numero de vivienda
       v_d_tot_aivs             DECIMAL(16,2) -- Contador de numero de vivienda
       
END GLOBALS 

#OBJETIVO: Controla las principales opciones de consulta de folios
MAIN 
DEFINE 
       v_de_Fecha       DATE,     --Recibe el valor de la fecha a consultar
       p_usuario        LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tipo_proc      CHAR(1),
       p_nombre_menu    LIKE seg_menu.opcion,
       v_valida_fecha   SMALLINT, 
       v_valida_folio   SMALLINT,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados  LIKE seg_modulo.ruta_listados, -- Rute del log
       p_pid            LIKE bat_ctr_proceso.pid, -- ID del proceso
       p_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_programa       CHAR(10),       
       r_bandera        SMALLINT,
       v_folio_lote     DECIMAL(9,0),
       v_nss            CHAR(11),
       v_estado         CHAR(2),
       v_id_estado      SMALLINT,
       v_nombre_arch    STRING,
       v_comando        STRING,
       v_ent_receptora  CHAR(3)
       
DEFINE f_ventana        ui.Window,   -- Define las propìedades de la Ventana
       f_forma          ui.Form,     -- Define las propiedades de la forma
       manejador_rpt    om.SaxDocumentHandler  -- Contenedor de Documentos para el reporte
DEFINE v_ind_for        INTEGER, --
       v_inicia         INTEGER

   LET p_programa    = "DAEC04"       
   LET p_usuario     = ARG_VAL(1) -- Recibe la variable de usuario
   LET p_tipo_proc   = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_proceso_cod = ARG_VAL(4) -- Proceso Cod 2400 DAE

   --LET p_proceso_cod = 5
   LET p_opera_cod = 3

   CALL fn_max_pid(p_proceso_cod, 1) RETURNING r_bandera

   LET p_pid = r_bandera
   INITIALIZE v_valida_fecha TO NULL   
   INITIALIZE v_de_Fecha   TO NULL  -- Inicializa la variable fecha a nulo
   INITIALIZE v_edit_Folio TO NULL  -- Inicializa la variable folio a nulo

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'dae'

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

    
--Abre la ventana para ingresar parámetros de consulta   
CLOSE WINDOW SCREEN  
OPEN WINDOW vtn_AvancePagos WITH FORM "DAEC041.4fd"
DIALOG   ATTRIBUTES(UNBUFFERED) 

   --Se obtienen los parámetros de folio, estado, NSS para consulta
   INPUT v_folio_lote,
         v_estado,
         v_ent_receptora

   FROM  cb_folio_lote,
         cb_estado,
         v_ed_ent_receptora

      BEFORE INPUT 
         --Llama función para llenar combo
         CALL fn_llena_combo_folio() 
         RETURNING v_folio_lote, v_nombre_arch
      
         LET f_ventana = ui.Window.getCurrent()
         LET f_forma = f_ventana.getForm()

         --Se invoca la función que asigna el titulo a la ventana
         CALL ui.Interface.setText(p_nombre_menu)
         CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta la Sección de Detalles
         CALL f_forma.setElementHidden("gr_montos_totales", 1) --Oculta la Sección de Detalles                 
         CALL f_forma.setElementHidden("regresar", 1) --Oculta el botón regresar
   END INPUT

   -- Botón aceptar que realiza la consulta en base a folio y fecha
   ON ACTION ACCEPT
      -- Llama la función de consulta de detalles 
      CALL fn_consulta_det_globales(v_folio_lote,v_estado,v_ent_receptora)
      
      --Si existen registros                  
      IF v_i_det > 1 THEN
         -- Si el folio capturado existe en el histórico ejecuta la consulta
         CALL f_forma.setElementHidden("gr_detalles", 0) --Muestra la Sección de Detalles
         CALL f_forma.setElementHidden("regresar", 0) --Muestra el botón regresar
      
         DISPLAY ARRAY arr_detalles_globales TO scr_detalles_globales.*
         ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
            --Obtiene montos totales
            BEFORE  DISPLAY 
               CALL fn_calcula_totales(v_folio_lote, v_estado,v_ent_receptora)
               CALL f_forma.setElementHidden("gr_montos_totales", 0) --Muestra la Sección de Detalles                 
      
            ON ACTION Consulta_Saldo
               SELECT id_derechohabiente
               INTO   v_id_derechohabiente
               FROM   afi_derechohabiente
               WHERE  nss = arr_detalles_dae[ARR_CURR()].v_det_nss
               
               CALL fn_eje_consulta(1,p_usuario,v_id_derechohabiente,p_tipo_proc, p_nombre_menu)
      
            ON ACTION reporte
              
               IF fgl_report_loadCurrentSettings("DAEC040.4rp") THEN
                  CALL fgl_report_selectDevice ("PDF")
                  LET manejador_rpt = fgl_report_commitCurrentSettings()
               END IF
               
               --Inicia el reporte de detalles PATRONES
               START REPORT rpt_detalles_amortizacion TO XML HANDLER manejador_rpt
                  FOR v_inicia = 1 TO v_i_det
                     OUTPUT TO REPORT rpt_detalles_amortizacion (p_usuario,
                                                                 v_folio_lote,
                                                                 arr_detalles_globales[v_inicia].*,
                                                                 v_suma_aceptados_amort, 
                                                                 v_d_aivs_aceptados,
                                                                 v_total_aceptados,
                                                                 v_suma_rechazados_amort,
                                                                 v_d_aivs_rechazados,
                                                                 v_total_rechazados,
                                                                 v_suma_pendientes_amort,
                                                                 v_d_aivs_pendientes,
                                                                 v_total_pendientes,
                                                                 v_i_tot_registros,
                                                                 v_d_tot_vivienda,
                                                                 v_d_tot_aivs
                                                                 )
                  END FOR
               FINISH REPORT rpt_detalles_amortizacion 
      
            ON ACTION CANCEL
               EXIT PROGRAM
               
            ON ACTION regresar
               CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta detalles
               CALL f_forma.setElementHidden("gr_montos_totales", 1)
               CALL arr_detalles_globales.clear()
               EXIT DISPLAY
         END DISPLAY
      ELSE 
         CALL fn_mensaje("Atención","No se econtraron registros","information")
         NEXT FIELD cb_folio_lote
      END IF    

    -- Botón cancel que da salida a la consulta y terminar la captura de los parámetros
   ON ACTION cancelar
      EXIT DIALOG               
END DIALOG 
 
CLOSE WINDOW  vtn_AvancePagos
END MAIN

#OBJETIVO: Realiza consulta de detalles de solicitudes por entidad receptora
FUNCTION fn_consulta_det_globales(p_folio_lote,p_estado,p_ent_receptora)
DEFINE  p_folio_lote    DECIMAL (9,0),
        p_estado        SMALLINT,
        p_ent_receptora CHAR(3),
        v_QryTxt        STRING 
        
          LET v_QryTxt = "\n SELECT a.folio,", 
                         "\n        a.entidad_receptora,", 
                         "\n        b.desc_corta,",
                         "\n        count(a.folio),", 
                         "\n        sum(a.importe_amort),", 
                         "\n        sum(a.monto_aivs),", 
                         "\n        a.resul_opera,", 
                         "\n        a.id_origen", 
                         "\n FROM   dae_det_solicitud a, OUTER", 
                         "\n        cat_ent_recaudadora b", 
                         "\n WHERE  b.id_entidad = a.entidad_receptora  "     
                         
   IF p_folio_lote IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  a.folio = ", p_folio_lote
   END IF
                         
   IF p_estado IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  a.resul_opera = ", p_estado
   END IF

   IF p_ent_receptora IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  a.entidad_receptora = ", "'",p_ent_receptora, "'" 
   END IF 


   LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,2,3,7,8",
                              "\n ORDER BY 1,2,3,7,8"

   PREPARE prp_dae_det_globales FROM v_QryTxt
   DECLARE cur_dae_det_globales CURSOR FOR prp_dae_det_globales 

   LET v_i_det = 1

   FOREACH cur_dae_det_globales INTO arr_detalles_globales[v_i_det].* 
      --Determina la descripción del Estado
      CASE 
      WHEN arr_detalles_globales[v_i_det].v_resul_opera = "01" 
         LET arr_detalles_globales[v_i_det].v_resul_opera = "ACEPTADO"
      WHEN arr_detalles_globales[v_i_det].v_resul_opera = "02"
         LET arr_detalles_globales[v_i_det].v_resul_opera = "RECHAZADO"   
      WHEN arr_detalles_globales[v_i_det].v_resul_opera = "03"
         LET arr_detalles_globales[v_i_det].v_resul_opera = "PENDIENTE"   
      END CASE

      --Determina la descripción del orígen   
      IF arr_detalles_globales[v_i_det].v_origen = 1 THEN 
         LET arr_detalles_globales[v_i_det].v_origen  = "CARTERA"
      ELSE
         LET arr_detalles_globales[v_i_det].v_origen = "DISPERSION"
      END IF

      IF arr_detalles_globales[v_i_det].v_desc_ent_recaudadora IS NULL THEN 
         LET arr_detalles_globales[v_i_det].v_desc_ent_recaudadora = "SIN DESC EN CATALOGO"
      END IF 

      LET v_i_det = v_i_det + 1      
   END FOREACH  

   CALL arr_detalles_globales.deleteElement(v_i_det)

END FUNCTION 

#OBJETIVO: Ejecutar la consulta de resumen de archivo a generar.
FUNCTION fn_calcula_totales(p_folio, p_estado, p_ent_receptora)
DEFINE p_folio, v_folio     LIKE glo_folio.folio,
       v_QryTxt             STRING, -- cadena con una instruccion SQL
       v_QryTxt_A           STRING, -- cadena con una instruccion SQL
       v_QryTxt_R           STRING, -- cadena con una instruccion SQL
       v_QryTxt_P           STRING, -- cadena con una instruccion SQL
       v_folio_lote         DECIMAL(9,0),
       p_estado             SMALLINT, 
       p_ent_receptora      CHAR(3)

   LET v_i_tot_registros       = 0
   LET v_total_aceptados       = 0
   LET v_total_rechazados      = 0
   LET v_total_pendientes      = 0
   
   LET v_d_tot_vivienda        = 0.00
   LET v_d_tot_aivs            = 0.00   

   LET v_suma_aceptados_amort  = 0.00
   LET v_d_aivs_aceptados      = 0.00

   LET v_suma_rechazados_amort = 0.00
   LET v_d_aivs_rechazados     = 0.00   

   --Llena Totales 
   
   LET v_QryTxt = "\n SELECT COUNT(*), ",
                  "\n SUM(importe_amort),",
                  "\n SUM(monto_aivs)",
                  "\n FROM dae_det_solicitud ",
                  "\n WHERE 1 = 1 "
                  
   IF p_folio IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  folio = ", p_folio
   END IF
                         
   IF p_estado IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  resul_opera = ", p_estado
   END IF

   IF p_ent_receptora IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  entidad_receptora = ", "'",p_ent_receptora, "'" 
   END IF

--DISPLAY v_QryTxt
                  
   PREPARE Prpr_Obt_sumas FROM v_QryTxt CLIPPED
   EXECUTE Prpr_Obt_sumas INTO v_i_tot_registros,
                               v_d_tot_vivienda,
                               v_d_tot_aivs
-------------------
--LLENA ACEPTADOS

   LET v_QryTxt_A = "\n SELECT COUNT(*), ",
                    "\n SUM(importe_amort),",
                    "\n SUM(monto_aivs)",
                    "\n FROM dae_det_solicitud ",
                    "\n WHERE 1 = 1 "

   IF p_folio IS NOT NULL THEN
      LET v_QryTxt_A = v_QryTxt_A || "\n AND  folio = ", p_folio
   END IF

   IF p_estado IS NOT NULL AND p_estado = 1 THEN
      LET v_QryTxt_A = v_QryTxt_A || "\n AND  resul_opera = 1 "
   END IF    
   IF p_estado IS NULL THEN
      LET v_QryTxt_A = v_QryTxt_A || "\n AND  resul_opera = 1 "
   END IF

   IF p_estado = 2 THEN
      LET v_QryTxt_A = v_QryTxt_A || "\n AND  resul_opera = 0 "
   END IF
   
   IF p_estado = 3 THEN
      LET v_QryTxt_A = v_QryTxt_A || "\n AND  resul_opera = 0 "
   END IF
   
   IF p_ent_receptora IS NOT NULL THEN
      LET v_QryTxt_A = v_QryTxt_A || "\n AND  entidad_receptora = ", "'",p_ent_receptora, "'" 
   END IF 
--DISPLAY "ACEPTADOS \n ",v_QryTxt_A
   PREPARE prp_Total_Aceptados FROM v_QryTxt_A CLIPPED
   EXECUTE prp_Total_Aceptados INTO v_total_aceptados,
                                    v_suma_aceptados_amort, 
                                    v_d_aivs_aceptados

-----------------------
--LLENA RECHAZADOS
   LET v_QryTxt_R = "\n SELECT COUNT(*), ",
                    "\n SUM(importe_amort),",
                    "\n SUM(monto_aivs)",
                    "\n FROM dae_det_solicitud ",
                    "\n WHERE 1 = 1 "
 
   IF p_folio IS NOT NULL THEN
      LET v_QryTxt_R = v_QryTxt_R || "\n AND  folio = ", p_folio
   END IF                    

   IF p_estado IS NOT NULL AND p_estado = 2 THEN
      LET v_QryTxt_R = v_QryTxt_R || "\n AND  resul_opera = 2 "
   END IF    
   IF p_estado IS NULL THEN
      LET v_QryTxt_R = v_QryTxt_R || "\n AND  resul_opera = 2 "
   END IF

   IF p_estado = 1 THEN
      LET v_QryTxt_R = v_QryTxt_R || "\n AND  resul_opera = 0 "
   END IF

   IF p_estado = 3 THEN
      LET v_QryTxt_R = v_QryTxt_R || "\n AND  resul_opera = 0 "
   END IF
   
   IF p_ent_receptora IS NOT NULL THEN
      LET v_QryTxt_R = v_QryTxt_R || "\n AND  entidad_receptora = ", "'",p_ent_receptora, "'" 
   END IF
   
--DISPLAY "RECHAZADOS \n",v_QryTxt_R
  
   PREPARE prp_Total_Rechazados FROM v_QryTxt_R CLIPPED
   EXECUTE prp_Total_Rechazados INTO v_total_rechazados,
                                     v_suma_rechazados_amort, 
                                     v_d_aivs_rechazados

-----------------------
--LLENA PENDIENTES
DISPLAY p_estado
   LET v_QryTxt_P = "\n SELECT COUNT(*), ",
                    "\n SUM(importe_amort),",
                    "\n SUM(monto_aivs)",
                    "\n FROM dae_det_solicitud ",
                    "\n WHERE 1 = 1 "

   IF p_folio IS NOT NULL THEN
      LET v_QryTxt_P = v_QryTxt_P || "\n AND  folio = ", p_folio
   END IF                    

   IF p_estado = 3 THEN
      LET v_QryTxt_P = v_QryTxt_P || "\n AND  resul_opera = 3 "
   END IF

   IF p_estado IS NULL THEN
      LET v_QryTxt_P = v_QryTxt_P || "\n AND  resul_opera = 3 "
   END IF

   IF p_estado = 1 THEN
      LET v_QryTxt_P = v_QryTxt_P || "\n AND  resul_opera = 0 "
   END IF

   IF p_estado = 2 THEN
      LET v_QryTxt_P = v_QryTxt_P || "\n AND  resul_opera = 0 "
   END IF
   
   IF p_ent_receptora IS NOT NULL THEN
      LET v_QryTxt_P = v_QryTxt_P || "\n AND  entidad_receptora = ", "'",p_ent_receptora, "'" 
   END IF
   
--DISPLAY "pendientes", v_QryTxt_P

   PREPARE prp_Total_Pendientes FROM v_QryTxt_P CLIPPED
   EXECUTE prp_Total_Pendientes INTO v_total_pendientes,
                                     v_suma_pendientes_amort, 
                                     v_d_aivs_pendientes
-----------------------

      --Asigna valores en cero a valores nulos
      IF v_total_aceptados = 0 OR v_total_aceptados IS NULL THEN
         LET v_total_aceptados      = 0.01
         LET v_suma_aceptados_amort = 0.00 USING "#,###,###,##&.&&"
         LET v_d_aivs_aceptados     = 0.00 USING "#,###,###,##&.&&"
      END IF 

      IF v_total_rechazados = 0 OR v_total_rechazados IS NULL THEN
         LET v_total_rechazados      = 0 
         LET v_suma_rechazados_amort = 0.00 USING "#,###,###,##&.&&"
         LET v_d_aivs_rechazados     = 0.00 USING "#,###,###,##&.&&"
      END IF 

      IF v_total_pendientes = 0 OR v_total_pendientes IS NULL THEN
         LET v_total_pendientes      = 0 
         LET v_suma_pendientes_amort = 0.00 USING "#,###,###,##&.&&"
         LET v_d_aivs_pendientes     = 0.00 USING "#,###,###,##&.&&"
      END IF

   --Muestra TOTALES
   DISPLAY v_i_tot_registros,
           v_d_tot_vivienda,
           v_d_tot_aivs,
           v_suma_aceptados_amort,
           v_d_aivs_aceptados,
           v_total_aceptados, 
           v_suma_rechazados_amort,
           v_d_aivs_rechazados,
           v_total_rechazados,
           v_suma_pendientes_amort,
           v_d_aivs_pendientes,
           v_total_pendientes

        TO v_i_tot_registros, 
           v_d_tot_vivienda,
           v_d_tot_aivs,
           ---
           v_d_importe_aceptados,
           v_d_aivs_aceptados,
           v_i_tot_aceptados,
           ---
           v_d_importe_rechazados,
           v_d_aivs_rechazados,
           v_i_tot_rechazados,
           ---
           v_d_importe_pendientes,
           v_d_aivs_pendientes,
           v_i_tot_pendientes

   --RETURN p_folio

END FUNCTION

#OBJETIVO: Llenar el combo con los folios a consultar
FUNCTION fn_llena_combo_folio()               
DEFINE v_cb_folio   LIKE glo_folio.folio,--Almacena folio en el combobox
       v_QryTxt     STRING,                     -- Cadena para almacenar Query
       v_cmbx           ui.ComboBox                 -- Variable de Combobox

   LET v_cmbx = ui.ComboBox.forName("cb_folio_lote") --Asignación del combo a la forma
 
   -- Validación si el combo es nulo 
   IF v_cmbx IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1
   
   LET v_QryTxt = "\n SELECT folio,",
                  "\n        id_origen",
                  "\n FROM   dae_det_solicitud",
                  "\n GROUP BY 1,2 ",
                  "\n ORDER BY 1"
                 
   -- Prepara la consulta para obtener folios liquidados
   PREPARE prp_folios_dpe FROM v_QryTxt
   
   -- Limpia el combo
   CALL v_cmbx.clear()

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_combo_folio CURSOR FOR prp_folios_dpe
      FOREACH cur_llena_combo_folio INTO var_dis_hist[v_indice].v_folio,
                                         var_dis_hist[v_indice].v_origen_folio

         IF var_dis_hist[v_indice].v_origen_folio <> 6 THEN 
            LET var_dis_hist[v_indice].v_origen_desc = "CARTERA" 
         ELSE 
            LET var_dis_hist[v_indice].v_origen_desc = "DISPERSION"
         END IF    

         -- Agrega elementos al combobox
         CALL v_cmbx.addItem(var_dis_hist[v_indice].v_folio,
                             var_dis_hist[v_indice].v_folio ||'-'|| var_dis_hist[v_indice].v_origen_desc)

         LET v_indice = v_indice + 1
      END FOREACH

   CALL var_dis_hist.deleteElement(v_indice)
         
   RETURN v_cb_folio, v_indice
   
END FUNCTION

#OBJETIVO: Generar el reporte global en base
REPORT rpt_detalles_amortizacion (p_usuario,
                                  p_folio_lote,
                                  p_arr_detalles_globales,
                                  p_suma_aceptados_amort,
                                  p_d_aivs_aceptados,
                                  p_total_aceptados,
                                  p_suma_rechazados_amort,
                                  p_d_aivs_rechazados,
                                  p_total_rechazados,
                                  p_suma_pendientes_amort,
                                  p_d_aivs_pendientes,
                                  p_total_pendientes,
                                  p_i_tot_registros,
                                  p_d_tot_vivienda,
                                  p_d_tot_aivs)

DEFINE v_fecha_reporte          DATE,
       p_usuario                LIKE seg_modulo.usuario,
       p_folio_lote             DECIMAL(9,0),
       p_suma_aceptados_amort   DECIMAL(16,2), 
       p_d_aivs_aceptados       DECIMAL(16,2), 
       p_total_aceptados        INTEGER,
       p_suma_rechazados_amort  DECIMAL(16,2),      
       p_d_aivs_rechazados      DECIMAL(16,2),      
       p_total_rechazados       INTEGER,
       p_suma_pendientes_amort  DECIMAL(16,2),
       p_d_aivs_pendientes      DECIMAL(16,2),
       p_total_pendientes       INTEGER,
       p_i_tot_registros        INTEGER, -- Contador de numero de registro de registros
       p_d_tot_vivienda         DECIMAL(16,2), -- Contador de numero de vivienda
       p_d_tot_aivs             DECIMAL(16,2) -- Contador de numero de vivienda

DEFINE p_arr_detalles_globales RECORD 
          v_folio                DECIMAL(9,0),
          v_ent_recaudadora      CHAR(3),
          v_desc_ent_recaudadora CHAR(25),
          v_total_registros      DECIMAL(10,0),
          v_monto_pesos          DECIMAL(16,6),
          v_monto_aivs           DECIMAL(16,6),
          v_resul_opera          CHAR(10),
          v_origen               CHAR(10)
END RECORD

FORMAT

   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED
      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_folio_lote
      PRINTX p_usuario

   ON EVERY ROW
      PRINTX p_arr_detalles_globales.v_folio
      PRINTX p_arr_detalles_globales.v_ent_recaudadora
      PRINTX p_arr_detalles_globales.v_desc_ent_recaudadora
      PRINTX p_arr_detalles_globales.v_total_registros
      PRINTX p_arr_detalles_globales.v_monto_pesos
      PRINTX p_arr_detalles_globales.v_monto_aivs
      PRINTX p_arr_detalles_globales.v_resul_opera
      PRINTX p_arr_detalles_globales.v_origen

   ON LAST ROW
      PRINTX p_suma_aceptados_amort
      PRINTX p_d_aivs_aceptados
      PRINTX p_total_aceptados
      PRINTX p_suma_rechazados_amort
      PRINTX p_d_aivs_rechazados
      PRINTX p_total_rechazados
      PRINTX p_suma_pendientes_amort
      PRINTX p_d_aivs_pendientes
      PRINTX p_total_pendientes
      PRINTX p_i_tot_registros
      PRINTX p_d_tot_vivienda
      PRINTX p_d_tot_aivs

END REPORT