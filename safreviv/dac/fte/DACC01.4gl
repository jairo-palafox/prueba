--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/03/2014
--===============================================================

################################################################################
#Modulo       => DAC                                                           #
#Programa     => DACC01                                                        #
#Objetivo     => Consulta de Diagnósticos Devolución de Amortización           #
#                Mejora tu Casa                                                #
#Fecha inicio => 05/03/2014                                                    #
################################################################################

DATABASE safre_viv

GLOBALS
CONSTANT MAX_REGISTROS     SMALLINT = 50
 
DEFINE arr_detalles_dac DYNAMIC ARRAY OF RECORD
          v_id_derechohabiente DECIMAL(9,0) ,  
          v_num_credito        DECIMAL(10,0),    
          v_nss                CHAR(11)     ,    
          v_nombre             CHAR(30)     ,
          v_periodo_pago       CHAR(4)   ,    
          v_imp_amortizacion   DECIMAL(8,2) ,    
          v_folio_sua          DECIMAL(6,0) ,    
          v_nrp                CHAR(11)     ,    
          v_id_sdd             CHAR(6)      ,    
          v_desc_resul_opera   CHAR(10)     ,    
          v_desc_diagnostico   CHAR(30)        
END RECORD

DEFINE v_estado_registro     SMALLINT,
       v_nombre_archivo      CHAR (40), 
       v_i_det               INTEGER,
       v_id_derechohabiente  DECIMAL(9,0)

DEFINE var_dis_hist DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_folio          DECIMAL(9,0),
          v_archivo        CHAR (30)
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
       v_ent_receptora  CHAR(3),
       v_monto_pesos_tot    DECIMAL(16,6),
       v_monto_acciones_tot DECIMAL(16,6),
       v_total_rendimiento  DECIMAL(16,6),
       v_total_registros_det INTEGER,
       v_tot_aceptados    INTEGER,
       v_tot_rechazados   INTEGER,
       v_tot_solicitudes  INTEGER,
       v_monto_aceptados  DECIMAL(16,6),
       v_monto_rechazados DECIMAL(16,6),
       v_monto_total      DECIMAL(16,6)
       
       
       
DEFINE f_ventana        ui.Window,   -- Define las propìedades de la Ventana
       f_forma          ui.Form,     -- Define las propiedades de la forma
       manejador_rpt    om.SaxDocumentHandler  -- Contenedor de Documentos para el reporte
DEFINE v_ind_for        INTEGER, --
       v_inicia         INTEGER,
       v_archivo        STRING,  
       v_nss_consulta   CHAR(11)

   LET p_programa    = "DAEC01"       
   LET p_usuario     = ARG_VAL(1) -- Recibe la variable de usuario
   LET p_tipo_proc   = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_proceso_cod = ARG_VAL(4) -- Proceso Cod 2400 DAE

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
OPEN WINDOW vtn_Dianosticos_DAC WITH FORM "DACC010"
DIALOG   ATTRIBUTES(UNBUFFERED) 

   --Se obtienen los parámetros de folio, estado, NSS para consulta
   INPUT v_folio_lote,
         v_estado,
         v_nss 

   FROM  cb_folio_lote,
         cb_estado,
         v_ed_nss

   BEFORE INPUT 
      --Llama función para llenar combo
      CALL fn_llena_combo_folio() 

      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()

      --Se invoca la función que asigna el titulo a la ventana
      CALL ui.Interface.setText(p_nombre_menu)
      CALL f_forma.setElementHidden("gr_detalles", 0) --Oculta la Sección de Detalles
      CALL f_forma.setElementHidden("regresar", 1) --Oculta el botón regresar
   END INPUT

   -- Botón cancel que da salida a la consulta y terminar la captura de los parámetros
   ON ACTION cancelar
      EXIT DIALOG

   -- Botón aceptar que realiza la consulta en base a folio y fecha
   ON ACTION ACCEPT  
      -- Si el folio es válido se llama la función de consulta de detalles 
      CALL fn_consulta_detalles_ajuste(v_folio_lote, v_estado,v_nss)
      RETURNING v_tot_aceptados, v_tot_rechazados, v_tot_solicitudes, 
                v_monto_aceptados, v_monto_rechazados, v_monto_total

         --Si existen registros
         IF v_i_det > 1 THEN
            -- Si el folio capturado existe en el histórico ejecuta la consulta
            CALL f_forma.setElementHidden("gr_detalles", 0) --Muestra la Sección de Detalles
            CALL f_forma.setElementHidden("regresar", 0) --Muestra el botón regresar

            DISPLAY ARRAY arr_detalles_dac TO scr_detalles.*
            ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
         
               ON ACTION Consulta_Derechohabiente
                 -- se ejecuta la consulta del derechohabiente usando la consulta general
                 LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || p_usuario || "1 'Consulta de Derechohabiente' " || arr_detalles_dac[ARR_CURR()].v_nss
                 RUN v_comando
         
               {ON ACTION edo_cuenta
                  LET v_nss_consulta = arr_detalles_ajustes [ARR_CURR()].v_nss
         
                  SELECT id_derechohabiente  
                  INTO   v_id_derechohabiente
                  FROM   afi_derechohabiente
                  WHERE  nss = v_nss_consulta 

                  CALL fn_pantalla_edo_cuenta(v_nss_consulta) RETURNING v_archivo
               }
               ON ACTION reporte
                  IF fgl_report_loadCurrentSettings("DACC011.4rp") THEN
                     CALL fgl_report_selectDevice ("PDF")
                     LET manejador_rpt = fgl_report_commitCurrentSettings()
                  END IF
                  
                  --Inicia el reporte de detalles
                  START REPORT rpt_detalles_mtc TO XML HANDLER manejador_rpt
                 
                     FOR v_inicia = 1 TO v_i_det
                        OUTPUT TO REPORT rpt_detalles_mtc (
                                                           p_usuario,
                                                           arr_detalles_dac[v_inicia].*,
                                                           v_tot_aceptados, 
                                                           v_tot_rechazados, 
                                                           v_tot_solicitudes, 
                                                           v_monto_aceptados, 
                                                           v_monto_rechazados, 
                                                           v_monto_total         
                                                           )
                     END FOR
                  FINISH REPORT rpt_detalles_mtc 

               ON ACTION CANCEL
                  EXIT PROGRAM
                  
               ON ACTION regresar
                  CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta detalles
                  CALL arr_detalles_dac.clear()                           
                  EXIT DISPLAY
               END DISPLAY
         ELSE 
            CALL fn_mensaje("Atención","No se econtraron registros","information")
            NEXT FIELD cb_folio_lote
         END IF    
 END DIALOG 
CLOSE WINDOW  vtn_Dianosticos_DAC
END MAIN  
  
#OBJETIVO: Consultar los detalles de la Consulta del Ajuste de Amortizaciones Excedentes
FUNCTION fn_consulta_detalles_ajuste(p_folio_lote, p_estado, p_nss)
--Arreglo que almacena los detalles 
DEFINE rec_detalles_dac RECORD
          v_id_dac_solicitud  DECIMAL(9,0) ,
          v_id_derechohabiente DECIMAL(9,0) , 
          v_num_credito      DECIMAL(10,0),
          v_nss              CHAR(11)     ,
          v_periodo_pago     DECIMAL(4)   ,
          v_imp_amortizacion DECIMAL(16,6) ,
          v_folio_sua        DECIMAL(6,0) ,
          v_nrp              CHAR(11)     ,
          v_id_sdd           CHAR(6)      ,
          v_resul_opera      SMALLINT     ,
          v_diagnostico      SMALLINT     ,
          v_desc_diagnostico CHAR(30)     ,
          v_nombre_dh        CHAR(40)     ,
          v_desc_resul_op    CHAR(10)     
END RECORD 

DEFINE v_QryTxt             STRING,
       v_QryTxt_1           STRING,
       v_QryTxt_2           STRING,
       p_folio_lote         DECIMAL(9,0),
       p_estado             SMALLINT, 
       p_nss                CHAR(11),
       v_tot_aceptados    INTEGER,
       v_tot_rechazados   INTEGER,
       v_tot_solicitudes  INTEGER,
       v_monto_aceptados  DECIMAL(16,6),
       v_monto_rechazados DECIMAL(16,6),
       v_monto_total      DECIMAL(16,6)

   LET v_QryTxt = "\n SELECT a.id_dac_solicitud,",
                  "\n        a.id_derechohabiente,",
                  "\n        a.num_credito,",
                  "\n        a.nss,       ", 
                  "\n        a.periodo_pago,",
                  "\n        a.imp_amortizacion,",
                  "\n        a.folio_sua,",
                  "\n        a.nrp,",
                  "\n        a.id_sdd,",
                  "\n        a.resul_opera,",
                  "\n        a.diagnostico",
                  "\n FROM   dac_det_solicitud a  ",
                  "\n WHERE 1 = 1"

   -- se completa el filtro del select
   IF p_folio_lote IS NOT NULL THEN
      LET v_QryTxt  = v_QryTxt || " \n AND a.folio_integracion = ", p_folio_lote 
   END IF

   IF p_estado IS NOT NULL THEN
      LET v_QryTxt  = v_QryTxt || " \n AND a.resul_opera = ", p_estado
   END IF

   IF p_nss IS NOT NULL THEN
      LET v_QryTxt  = v_QryTxt || " \n AND a.nss = ", p_nss
   END IF

   LET v_QryTxt  = v_QryTxt ||  " \n GROUP BY 1,2,3,4,5,6,7,8,9,10,11"
   LET v_QryTxt  = v_QryTxt ||  " \n ORDER BY 10"

--DISPLAY v_QryTxt
   PREPARE prp_detalles_ajuste FROM v_QryTxt
   DECLARE cur_detalles_ajuste CURSOR FOR prp_detalles_ajuste 

   LET v_i_det            = 1  
   LET v_monto_aceptados  = 0
   LET v_monto_rechazados = 0

   FOREACH cur_detalles_ajuste INTO rec_detalles_dac.v_id_dac_solicitud  ,
                                    rec_detalles_dac.v_id_derechohabiente,
                                    rec_detalles_dac.v_num_credito       ,
                                    rec_detalles_dac.v_nss               ,
                                    rec_detalles_dac.v_periodo_pago      ,
                                    rec_detalles_dac.v_imp_amortizacion  ,
                                    rec_detalles_dac.v_folio_sua         ,
                                    rec_detalles_dac.v_nrp               ,
                                    rec_detalles_dac.v_id_sdd            ,
                                    rec_detalles_dac.v_resul_opera       ,
                                    rec_detalles_dac.v_diagnostico     

      IF rec_detalles_dac.v_id_derechohabiente IS NULL THEN
         LET rec_detalles_dac.v_id_derechohabiente  = 0
      END IF   
      --Recupera nombre del derechohabiente 
      LET v_QryTxt_1 = "\n SELECT nombre_imss",
                       "\n FROM   afi_derechohabiente ",
                       "\n WHERE  id_derechohabiente = ", rec_detalles_dac.v_id_derechohabiente

      PREPARE prp_dae_nombre FROM v_QryTxt_1
      EXECUTE prp_dae_nombre INTO rec_detalles_dac.v_nombre_dh

      --Asigna la descripción del resultado de la operación
      IF rec_detalles_dac.v_resul_opera = 1 THEN 
         LET rec_detalles_dac.v_desc_resul_op = "ACEPTADO"
      ELSE
         LET rec_detalles_dac.v_desc_resul_op  = "RECHAZADO"
      END IF

      --Consulta la descripción del diagnóstico
      LET v_QryTxt_2 = "\n SELECT diag_desc_corta",
                       "\n FROM   dac_diagnosticos ",
                       "\n WHERE  diagnostico = ", rec_detalles_dac.v_diagnostico

      PREPARE prp_diagnostico FROM v_QryTxt_2
      EXECUTE prp_diagnostico INTO rec_detalles_dac.v_desc_diagnostico

      IF rec_detalles_dac.v_resul_opera = 1 THEN 
         LET v_tot_aceptados = v_tot_aceptados + 1
         LET v_monto_aceptados = v_monto_aceptados + rec_detalles_dac.v_imp_amortizacion 
      ELSE
         LET v_tot_rechazados = v_tot_rechazados + 1
         LET v_monto_rechazados = v_monto_rechazados + rec_detalles_dac.v_imp_amortizacion 
      END IF   

      LET v_tot_solicitudes = v_tot_aceptados + v_tot_rechazados
      LET v_monto_total = v_monto_aceptados + v_monto_rechazados
      
      --Asigna los valores al arreglo.
      LET arr_detalles_dac[v_i_det].v_id_derechohabiente = rec_detalles_dac.v_id_derechohabiente
      LET arr_detalles_dac[v_i_det].v_num_credito        = rec_detalles_dac.v_num_credito
      LET arr_detalles_dac[v_i_det].v_nss                = rec_detalles_dac.v_nss
      LET arr_detalles_dac[v_i_det].v_nombre             = rec_detalles_dac.v_nombre_dh
      LET arr_detalles_dac[v_i_det].v_periodo_pago       = rec_detalles_dac.v_periodo_pago
      LET arr_detalles_dac[v_i_det].v_imp_amortizacion   = rec_detalles_dac.v_imp_amortizacion
      LET arr_detalles_dac[v_i_det].v_folio_sua          = rec_detalles_dac.v_folio_sua
      LET arr_detalles_dac[v_i_det].v_nrp                = rec_detalles_dac.v_nrp
      LET arr_detalles_dac[v_i_det].v_id_sdd             = rec_detalles_dac.v_id_sdd
      LET arr_detalles_dac[v_i_det].v_desc_resul_opera   = rec_detalles_dac.v_desc_resul_op
      LET arr_detalles_dac[v_i_det].v_desc_diagnostico   = rec_detalles_dac.v_desc_diagnostico

      LET v_i_det = v_i_det  + 1      
   END FOREACH

   DISPLAY v_tot_aceptados,
           v_monto_aceptados,
           v_tot_rechazados,
           v_monto_rechazados,
           v_tot_solicitudes,
           v_monto_total
   TO      ed_tot_aceptados,
           ed_monto_aceptados,
           ed_tot_rechazados,
           ed_monto_rechazados,
           ed_tot_solicitudes,
           ed_monto_total

   RETURN v_tot_aceptados,
          v_tot_rechazados,
          v_tot_solicitudes,
          v_monto_aceptados,
          v_monto_rechazados,
          v_monto_total
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
   
   LET v_QryTxt = "\n SELECT g.folio, a.nombre_archivo    ",
                  "\n FROM glo_ctr_archivo a, glo_folio g ",
                  "\n WHERE a.proceso_cod = 2601 ", 
                  "\n AND a.proceso_cod = g.proceso_cod   ",
                  "\n AND a.folio = g.folio               "

   -- Prepara la consulta para obtener folios liquidados
   PREPARE prp_folios_dpe FROM v_QryTxt
   
   -- Limpia el combo
   CALL v_cmbx.clear()

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_combo_folio CURSOR FOR prp_folios_dpe
   FOREACH cur_llena_combo_folio INTO var_dis_hist[v_indice].v_folio,
                                      var_dis_hist[v_indice].v_archivo

         -- Agrega elementos al combobox
         CALL v_cmbx.addItem(var_dis_hist[v_indice].v_folio,
                             var_dis_hist[v_indice].v_folio ||'-'|| var_dis_hist[v_indice].v_archivo)

         LET v_indice = v_indice + 1
      END FOREACH

   CALL var_dis_hist.deleteElement(v_indice)
END FUNCTION

#OBJETIVO: Obtener el precio de la acción por día
FUNCTION fn_precio_x_dia(p_folio)
DEFINE p_folio         DECIMAL(9,0)

   SELECT folio, 
          f_actualiza
   INTO   v_folio_liquida, 
          v_fecha_liquida
   FROM   glo_folio
   WHERE  folio_referencia = p_folio
   AND    proceso_cod = 2400  

   SELECT precio_fondo
   INTO   v_precio_accion
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = v_fecha_liquida;
 
   IF v_fecha_liquida = "12/31/1899" THEN 
      LET v_fecha_liquida = ""
   END IF   
  
END FUNCTION 

#OBJETIVO: Consultar el detalle del rechazo en base al NSS y Folio
REPORT rpt_detalles_mtc (p_usuario,
                         p_arr_detalles_dac,
                         p_tot_aceptados,
                         p_tot_rechazados, 
                         p_tot_solicitudes, 
                         p_monto_aceptados, 
                         p_monto_rechazados, 
                         p_monto_total                                            
                         ) 

DEFINE v_fecha_reporte    DATE,
       p_usuario          LIKE seg_modulo.usuario,
       p_folio_lote       DECIMAL(9,0),
       p_tot_aceptados    INTEGER,
       p_tot_rechazados   INTEGER,
       p_tot_solicitudes  INTEGER,
       p_monto_aceptados  DECIMAL(16,6),
       p_monto_rechazados DECIMAL(16,6),
       p_monto_total      DECIMAL(16,6)
DEFINE p_arr_detalles_dac RECORD
          v_id_derechohabiente DECIMAL(9,0),  
          v_num_credito        CHAR(10)    ,
          v_nss                CHAR(11)    ,    
          v_nombre             CHAR(30)    ,
          v_periodo_pago       CHAR(4)     ,    
          v_imp_amortizacion   DECIMAL(8,2),    
          v_folio_sua          CHAR(6)     ,
          v_nrp                CHAR(11)    ,    
          v_id_sdd             CHAR(6)     ,    
          v_desc_resul_opera   CHAR(10)    ,    
          v_desc_diagnostico   CHAR(30)
END RECORD 

FORMAT

   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario

   ON EVERY ROW
      PRINTX p_arr_detalles_dac.v_num_credito      
      PRINTX p_arr_detalles_dac.v_nss              
      PRINTX p_arr_detalles_dac.v_nombre           
      PRINTX p_arr_detalles_dac.v_periodo_pago  USING "&&&&"   
      PRINTX p_arr_detalles_dac.v_imp_amortizacion 
      PRINTX p_arr_detalles_dac.v_folio_sua USING "&&&&&&"   
      PRINTX p_arr_detalles_dac.v_nrp              
      PRINTX p_arr_detalles_dac.v_id_sdd           
      PRINTX p_arr_detalles_dac.v_desc_resul_opera 
      PRINTX p_arr_detalles_dac.v_desc_diagnostico
      
   ON LAST ROW 
      PRINTX p_tot_aceptados   
      PRINTX p_tot_rechazados  
      PRINTX p_tot_solicitudes 
      PRINTX p_monto_aceptados 
      PRINTX p_monto_rechazados
      PRINTX p_monto_total     
END REPORT