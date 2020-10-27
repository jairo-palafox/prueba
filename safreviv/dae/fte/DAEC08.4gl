--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17/10/2016
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEC08                                                   #
#Objetivo          => Programa para consultar datos generales del Ajuste de    #
#                     Amortizaciones Excedentes                                #
#Fecha inicio      => 17/10/2016                                               #
################################################################################

DATABASE safre_viv

GLOBALS "DAEG01.4gl"
GLOBALS

CONSTANT MAX_REGISTROS     SMALLINT = 50
 
DEFINE arr_detalles_ajustes DYNAMIC ARRAY OF RECORD
          v_nss             CHAR(11),
          v_nombre          CHAR(40), 
          v_monto_pesos     DECIMAL(16,6),
          v_monto_aivs      DECIMAL(16,6),
          v_rendimiento     DECIMAL(16,6),
          v_folio_liquida   DECIMAL(9,0), 
          v_fecha_valor     DATE,
          v_resul_operacion CHAR(10),
          v_diagnostico     CHAR(30)
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
       v_total_registros_det INTEGER       
       
DEFINE f_ventana        ui.Window,   -- Define las propìedades de la Ventana
       f_forma          ui.Form,     -- Define las propiedades de la forma
       manejador_rpt    om.SaxDocumentHandler  -- Contenedor de Documentos para el reporte
DEFINE v_ind_for        INTEGER, --
       v_inicia         INTEGER,
       v_archivo        STRING,  
       v_nss_consulta   CHAR(11)

   --LET p_programa    = ARG_VAL(1)     
   LET p_usuario     = ARG_VAL(1) -- Recibe la variable de usuario
   LET p_tipo_proc   = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu = ARG_VAL(3) -- Recibe el nombre del programa

   LET p_proceso_cod = 2406


DISPLAY "USUARIO :",p_usuario    
DISPLAY "TIPO :", p_tipo_proc  
DISPLAY "MENU :",p_nombre_menu
DISPLAY "PROCESO :", p_proceso_cod


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
OPEN WINDOW vtn_AvancePagos WITH FORM "DAEC080"
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
         CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta la Sección de Detalles
         CALL f_forma.setElementHidden("regresar", 1) --Oculta el botón regresar
         CALL f_forma.setElementHidden("rechazos", 1) --Oculta el boton de rechazos   
   END INPUT

      -- Botón aceptar que realiza la consulta en base a folio y fecha
      ON ACTION ACCEPT  
         -- Si el folio es válido se llama la función de consulta de detalles 
         CALL fn_consulta_detalles_ajuste(v_folio_lote, v_estado,v_nss)
         RETURNING v_total_registros_det, v_monto_pesos_tot, v_monto_acciones_tot, v_total_rendimiento
         --Si existen registros                  
         IF v_i_det > 1 THEN
            -- Si el folio capturado existe en el histórico ejecuta la consulta
            CALL f_forma.setElementHidden("gr_detalles", 0) --Muestra la Sección de Detalles
            CALL f_forma.setElementHidden("regresar", 0) --Muestra el botón regresar

            DISPLAY ARRAY arr_detalles_ajustes TO scr_detalles.*
            ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
         
               ON ACTION Consulta_Derechohabiente
                 -- se ejecuta la consulta del derechohabiente usando la consulta general
                 LET v_comando = "cd ../../afi/bin/; fglrun AFIC01.42r " || p_usuario || "1 'Consulta de Derechohabiente' " || arr_detalles_ajustes [ARR_CURR()].v_nss
                 RUN v_comando
         
{               ON ACTION edo_cuenta
                  LET v_nss_consulta = arr_detalles_ajustes [ARR_CURR()].v_nss
         
                  SELECT id_derechohabiente  
                  INTO   v_id_derechohabiente
                  FROM   afi_derechohabiente
                  WHERE  nss = v_nss_consulta 

                  CALL fn_pantalla_edo_cuenta(v_nss_consulta) RETURNING v_archivo
}
               ON ACTION reporte
                  IF fgl_report_loadCurrentSettings("DAEC081.4rp") THEN
                     CALL fgl_report_selectDevice ("PDF")
                     LET manejador_rpt = fgl_report_commitCurrentSettings()
                  END IF
                  
                  --Inicia el reporte de detalles
                  START REPORT rpt_detalles_amortizacion TO XML HANDLER manejador_rpt
                 
                     FOR v_inicia = 1 TO v_i_det
                        OUTPUT TO REPORT rpt_detalles_amortizacion (
                                                                    p_usuario,
                                                                    arr_detalles_ajustes[v_inicia].*,
                                                                    v_total_registros_det,
                                                                    v_monto_pesos_tot,
                                                                    v_monto_acciones_tot,
                                                                    v_total_rendimiento
                                                                    )
                     END FOR
                  FINISH REPORT rpt_detalles_amortizacion 
         
               ON ACTION CANCEL
                  EXIT PROGRAM
                  
               ON ACTION regresar
                  CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta detalles
                  CALL arr_detalles_ajustes.clear()                           
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
  
#OBJETIVO: Consultar los detalles de la Consulta del Ajuste de Amortizaciones Excedentes
FUNCTION fn_consulta_detalles_ajuste(p_folio_lote, p_estado, p_nss)
--Arreglo que almacena los detalles 
DEFINE rec_detalles_ajuste RECORD
          v_id_dae_referencia  DECIMAL(9,0) ,
          v_id_derechohabiente DECIMAL(9,0) ,
          v_nss                CHAR(11)     ,
          v_monto_pesos        DECIMAL(16,6),
          v_monto_acciones     DECIMAL(16,6),
          v_folio_liquida      DECIMAL(9,0) ,
          v_fecha_valor        DATE         ,
          v_fecha_liquida      DATE         ,
          v_resul_operacion    SMALLINT     ,
          v_diagnostico        SMALLINT     ,
          v_desc_resul_op      CHAR(10)     , 
          v_desc_diagnostico   CHAR(30)     ,
          v_nombre_dh          CHAR(40)     ,
          v_rendimiento        DECIMAL(16,6)
END RECORD 
DEFINE v_QryTxt             STRING,
       v_QryTxt_1           STRING,
       v_QryTxt_2           STRING,
       p_folio_lote         DECIMAL(9,0),
       p_estado             SMALLINT, 
       p_nss                CHAR(11),
       v_precio_accion_ini  DECIMAL(16,6),
       v_precio_accion_fin  DECIMAL(16,6),
       v_hay_filtro         BOOLEAN,
       v_monto_pesos_tot    DECIMAL(16,6),
       v_monto_acciones_tot DECIMAL(16,6),
       v_total_rendimiento  DECIMAL(16,6),
       v_tot_regs           INTEGER

   LET v_QryTxt = "\n SELECT a.id_dae_ref_ajuste, ",
                  "\n        a.id_derechohabiente,",
                  "\n        a.nss,               ",
                  "\n        b.monto_pesos,       ",
                  "\n        b.monto_acciones,    ",
                  "\n        b.folio_liquida,     ",
                  "\n        b.fecha_valor,       ",
                  "\n        b.fecha_liquida,     ",
                  "\n        a.resul_operacion,   ",
                  "\n        a.diagnostico        ",
                  "\n FROM   dae_det_ajuste a     ",
                  "\n LEFT OUTER JOIN dae_aceptados_ajuste b ",
                  "\n ON     a.id_dae_ref_ajuste = b.id_dae_ref_ajuste "                  
  
   -- se completa el filtro del select
   IF p_folio_lote IS NOT NULL THEN
      LET v_QryTxt  = v_QryTxt || " \n WHERE a.folio_lote = ", p_folio_lote

   ELSE
      LET v_QryTxt  = v_QryTxt || " \n WHERE 1 = 1"
   END IF 

   IF p_estado IS NOT NULL THEN 
      LET v_QryTxt  = v_QryTxt || " \n AND a.resul_operacion = ", p_estado
   END IF 

   IF p_nss IS NOT NULL THEN
      LET v_QryTxt  = v_QryTxt || " \n AND a.nss = '", p_nss, "'"
   END IF
   
   LET v_QryTxt  = v_QryTxt ||  " \n ORDER BY 3,7,9"
   
DISPLAY v_QryTxt
   PREPARE prp_detalles_ajuste FROM v_QryTxt
   DECLARE cur_detalles_ajuste CURSOR FOR prp_detalles_ajuste 

   LET v_i_det              = 1  
   LET v_monto_pesos_tot    = 0
   LET v_monto_acciones_tot = 0
   LET v_total_rendimiento  = 0

   FOREACH cur_detalles_ajuste INTO rec_detalles_ajuste.v_id_dae_referencia , 
                                    rec_detalles_ajuste.v_id_derechohabiente,
                                    rec_detalles_ajuste.v_nss               ,
                                    rec_detalles_ajuste.v_monto_pesos       ,
                                    rec_detalles_ajuste.v_monto_acciones    ,
                                    rec_detalles_ajuste.v_folio_liquida     ,
                                    rec_detalles_ajuste.v_fecha_valor       ,
                                    rec_detalles_ajuste.v_fecha_liquida     ,
                                    rec_detalles_ajuste.v_resul_operacion   ,
                                    rec_detalles_ajuste.v_diagnostico  

      IF rec_detalles_ajuste.v_id_derechohabiente IS NULL THEN
         LET rec_detalles_ajuste.v_id_derechohabiente  = 0
      END IF   
      --Recupera nombre del derechohabiente 
      {
      LET v_QryTxt_1 = "\n SELECT nombre_imss",
                       "\n FROM   afi_derechohabiente ",
                       "\n WHERE  id_derechohabiente = ", rec_detalles_ajuste.v_id_derechohabiente

      PREPARE prp_dae_nombre FROM v_QryTxt_1
      EXECUTE prp_dae_nombre INTO rec_detalles_ajuste.v_nombre_dh
      }

      SELECT nombre_imss
      INTO   rec_detalles_ajuste.v_nombre_dh
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = rec_detalles_ajuste.v_id_derechohabiente

      --Asigna la descripción del resultado de la operación
      IF rec_detalles_ajuste.v_resul_operacion = 1 THEN 
         LET rec_detalles_ajuste.v_desc_resul_op = "ACEPTADO"
      ELSE
         LET rec_detalles_ajuste.v_desc_resul_op  = "RECHAZADO"

         IF rec_detalles_ajuste.v_folio_liquida IS NULL THEN 
            LET rec_detalles_ajuste.v_folio_liquida = 0
         END IF
      END IF

      --Consulta la descripción del diagnóstico
      {LET v_QryTxt_2 = "\n SELECT rch_desc_corta",
                       "\n FROM   dae_cat_rechazo ",
                       "\n WHERE  rechazo = ", rec_detalles_ajuste.v_diagnostico

      PREPARE prp_diagnostico FROM v_QryTxt_2
      EXECUTE prp_diagnostico INTO rec_detalles_ajuste.v_desc_diagnostico
      }

      SELECT rch_desc_corta
      INTO   rec_detalles_ajuste.v_desc_diagnostico
      FROM   dae_cat_rechazo
      WHERE  rechazo = rec_detalles_ajuste.v_diagnostico
      
      -- se obtiene fecha de fin
      SELECT f_actualiza
      INTO   v_fecha_liquida
      FROM   glo_folio
      WHERE  folio = rec_detalles_ajuste.v_folio_liquida
      AND    proceso_cod = 2403  

      --se obtiene el valor de una accion el dia que termina
      SELECT precio_fondo
      INTO   v_precio_accion_fin
      FROM   glo_valor_fondo
      WHERE  fondo = 11
      AND    f_valuacion = v_fecha_liquida;

      --se obtiene el valor de una accion el dia se inicio
      SELECT precio_fondo
      INTO   v_precio_accion_ini
      FROM   glo_valor_fondo
      WHERE  fondo = 11
      AND    f_valuacion = rec_detalles_ajuste.v_fecha_valor;

      --Calcula el importe del rendimiento
      LET rec_detalles_ajuste.v_rendimiento = rec_detalles_ajuste.v_monto_acciones*(v_precio_accion_fin-v_precio_accion_ini)

      --Asigna los valores al arreglo.
      LET arr_detalles_ajustes[v_i_det].v_nss             = rec_detalles_ajuste.v_nss
      LET arr_detalles_ajustes[v_i_det].v_nombre          = rec_detalles_ajuste.v_nombre_dh
      LET arr_detalles_ajustes[v_i_det].v_monto_pesos     = rec_detalles_ajuste.v_monto_pesos
      LET arr_detalles_ajustes[v_i_det].v_monto_aivs      = rec_detalles_ajuste.v_monto_acciones
      LET arr_detalles_ajustes[v_i_det].v_rendimiento     = rec_detalles_ajuste.v_rendimiento  
      LET arr_detalles_ajustes[v_i_det].v_folio_liquida   = rec_detalles_ajuste.v_folio_liquida
      LET arr_detalles_ajustes[v_i_det].v_fecha_valor     = rec_detalles_ajuste.v_fecha_valor
      LET arr_detalles_ajustes[v_i_det].v_resul_operacion = rec_detalles_ajuste.v_desc_resul_op
      LET arr_detalles_ajustes[v_i_det].v_diagnostico     = rec_detalles_ajuste.v_desc_diagnostico

      --sumas de totales
      IF rec_detalles_ajuste.v_monto_pesos IS NOT NULL THEN
         LET v_monto_pesos_tot    = v_monto_pesos_tot    + rec_detalles_ajuste.v_monto_pesos
      END IF
      IF rec_detalles_ajuste.v_monto_acciones IS NOT NULL THEN
         LET v_monto_acciones_tot = v_monto_acciones_tot + rec_detalles_ajuste.v_monto_acciones
      END IF
      IF rec_detalles_ajuste.v_rendimiento IS NOT NULL THEN 
         LET v_total_rendimiento = v_total_rendimiento  + rec_detalles_ajuste.v_rendimiento
      END IF 
      
      LET v_i_det = v_i_det  + 1      
   END FOREACH

   LET v_tot_regs = v_i_det - 1
   DISPLAY v_tot_regs           TO ed_tot_regs
   DISPLAY v_monto_pesos_tot    TO ed_tot_amortizacion
   DISPLAY v_monto_acciones_tot TO ed_tot_aivs
   DISPLAY v_total_rendimiento  TO ed_total_rendimiento

   RETURN v_tot_regs, v_monto_pesos_tot, v_monto_acciones_tot, v_total_rendimiento
END FUNCTION
 

#OBJETIVO: Llenar el combo con los folios a consultar
FUNCTION fn_llena_combo_folio()               
DEFINE v_cb_folio   LIKE glo_folio.folio, --Almacena folio en el combobox
       v_QryTxt     STRING,               -- Cadena para almacenar Query
       v_cmbx           ui.ComboBox       -- Variable de Combobox

   LET v_cmbx = ui.ComboBox.forName("cb_folio_lote") --Asignación del combo a la forma
 
   -- Validación si el combo es nulo 
   IF v_cmbx IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1
   
   LET v_QryTxt = "\n SELECT g.folio, a.nombre_archivo    ",
                  "\n FROM glo_ctr_archivo a, glo_folio g ",
                  "\n WHERE a.proceso_cod = 2406 ", 
                  "\n AND a.proceso_cod = g.proceso_cod   ",
                  "\n AND a.folio = g.folio               ",
                  "\n AND g.status = 0                    "

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
REPORT rpt_detalles_amortizacion (p_usuario,
                                  p_arr_detalles_ajustes ,
                                  p_total_registros_det, 
                                  p_monto_pesos_tot,
                                  p_monto_acciones_tot,
                                  p_total_rendimiento
                                  ) 

DEFINE v_fecha_reporte        DATE,
       p_usuario              LIKE seg_modulo.usuario,
       p_folio_lote           DECIMAL(9,0),
       p_total_registros_det  INTEGER, 
       p_monto_pesos_tot      DECIMAL(16,6),
       p_monto_acciones_tot   DECIMAL(16,6),
       p_total_rendimiento    DECIMAL(16,6)
DEFINE p_arr_detalles_ajustes RECORD
          v_nss             CHAR(11),
          v_nombre          CHAR(40), 
          v_monto_pesos     DECIMAL(16,6),
          v_monto_aivs      DECIMAL(16,6),
          v_rendimiento     DECIMAL(16,6),
          v_folio_liquida   DECIMAL(9,0), 
          v_fecha_valor     DATE,
          v_resul_operacion CHAR(10),
          v_diagnostico     CHAR(30)
END RECORD

FORMAT

   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario

   ON EVERY ROW
      PRINTX p_arr_detalles_ajustes.v_nss            
      PRINTX p_arr_detalles_ajustes.v_nombre         
      PRINTX p_arr_detalles_ajustes.v_monto_pesos    
      PRINTX p_arr_detalles_ajustes.v_monto_aivs
      PRINTX p_arr_detalles_ajustes.v_rendimiento
      PRINTX p_arr_detalles_ajustes.v_folio_liquida  
      PRINTX p_arr_detalles_ajustes.v_fecha_valor USING "dd-mm-yyyy" 
      PRINTX p_arr_detalles_ajustes.v_resul_operacion
      PRINTX p_arr_detalles_ajustes.v_diagnostico 

   ON LAST ROW 
      PRINTX p_total_registros_det
      PRINTX p_monto_pesos_tot
      PRINTX p_monto_acciones_tot
      PRINTX p_total_rendimiento
END REPORT