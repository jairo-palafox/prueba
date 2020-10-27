-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGC90
-- Objetivo      => Consulta de carga de registro
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 24 de Agosto de 2018
-- Requerimiento => 
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS
	 DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
END GLOBALS

MAIN

   DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
          p_s_titulo       STRING    -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF p_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida LQINFO
   CALL fn_consulta_folio(p_usuario_cod)

END MAIN

FUNCTION fn_consulta_folio(p_usuario_co)

   DEFINE 
      p_usuario_co	      LIKE seg_usuario.usuario_cod,  -- clave del usuario
      v_folio             DECIMAL(9,0),                  -- folio
      v_cbx_folios        ui.ComboBox,                   -- combo de afores
      v_s_cadena          STRING,                        -- cadena de texto
      v_r_glo_folio       RECORD LIKE glo_folio.*

   DEFINE 
      p_folio DECIMAL(9,0), 
      vindice SMALLINT
      
   DEFINE reg_cifras RECORD
   	  ind_registro CHAR(25),
   	  f_archivo    DATE,    --DATETIME YEAR TO SECOND,
   	  nss          CHAR(11),
      curp         CHAR(18),
      credito_inf  DECIMAL(10,0),
      cod_ident    CHAR(10),
      marca_cambio CHAR(02),
      num_caso     DECIMAL(10,0),
      f_pago       DATE,
      monto        DECIMAL(12,2),
      result_opera CHAR(10)
   END RECORD
   
   DEFINE
      p_b_despliegue_pantalla SMALLINT  -- booleana que indica si el archivo se escribe en disco o se envia a pantalla

   DEFINE w   ui.window
   DEFINE f_w ui.form 

   OPEN WINDOW w_consulta_folio WITH FORM "PAGC901"
   
   LET  w = ui.Window.getCurrent()
   LET  f_w = w.getForm()

   CALL f_w.setelementhidden("HBox1",1)
   CALL f_w.setelementhidden("Group2",1)
   CALL f_w.setelementhidden("Group3",1)
   
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una folio
   --CALL v_cbx_folios.addItem(-1," ")
   
   INPUT v_folio WITHOUT DEFAULTS
      FROM cmb_folio    ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
         -- se le asigna el apuntado del combo a la variable
         LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
         
         -- se inicia el combobox en blanco
         CALL v_cbx_folios.clear()   
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a,
                pag_cza_cvt b
         WHERE  a.proceso_cod = g_proceso_reg_pag_svt  --1416
         AND    a.folio = b.folio
         ORDER BY a.folio DESC

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
        
      ON ACTION ACCEPT
         DISPLAY "@folio: ",v_folio
         
         LET v_folio = v_folio CLIPPED
         
         IF v_folio IS NULL OR v_folio = 0 THEN
            CALL fn_mensaje("Aviso","Folio incorrecto","stop")
            CONTINUE INPUT
         ELSE
        
            CALL f_w.setelementhidden("Group1",0)
            CALL f_w.setelementhidden("Group2",0)
            CALL f_w.setelementhidden("Group3",0)
            
            LET p_b_despliegue_pantalla = TRUE

            SELECT CASE cza.ind_registro
                      WHEN 0 THEN ""
                      WHEN 3 THEN "NO TIENE CRÉDITO INF."
                      WHEN 7 THEN "FECHA DE PAGO NULA"
                      WHEN 8 THEN "IMPORTE NULO"
                   END,
   	               cza.f_archivo,    
   	               afi.nss,
                   det.curp,
                   det.credito_infonavit,
                   det.cod_identificacion,
                   det.marca_cambio_casa,
                   det.num_caso,
                   det.f_pago,
                   det.monto_deposito,
                   CASE det.result_operacion
                      WHEN "01" THEN "ACEPTADO"
                      ELSE "RECHAZADO"
                   END
   	        INTO   reg_cifras.ind_registro,
   	               reg_cifras.f_archivo,
   	               reg_cifras.nss,
                   reg_cifras.curp,
                   reg_cifras.credito_inf,
                   reg_cifras.cod_ident,
                   reg_cifras.marca_cambio,
                   reg_cifras.num_caso,
                   reg_cifras.f_pago,
                   reg_cifras.monto,
                   reg_cifras.result_opera
            FROM   pag_cza_cvt cza,
                   pag_det_cvt det,
                   afi_derechohabiente afi
            WHERE  cza.folio = det.folio
            AND    cza.folio = v_folio
            AND    afi.id_derechohabiente = det.id_derechohabiente
            DISPLAY "reg_cifras.* ",reg_cifras.*
            DISPLAY BY NAME reg_cifras.*
               
            MENU ""
               COMMAND "Cancelar"
                  CLEAR FORM
                  EXIT MENU
               COMMAND "Reporte"
                  CALL fn_reporte_carga_archivo(v_folio, reg_cifras.*, TRUE)
            END MENU
        END IF
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION


FUNCTION fn_reporte_carga_archivo(p_folio, reg_cifras, p_b_despliegue_pantalla)
   
   DEFINE p_folio INTEGER
   
   DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
   DEFINE reg_cifras RECORD
   	  ind_registro CHAR(25),
   	  f_archivo    DATE,     --DATETIME YEAR TO SECOND,
   	  nss          CHAR(11),
      curp         CHAR(18),
      credito_inf  DECIMAL(10,0),
      cod_ident    CHAR(10),
      marca_cambio CHAR(02),
      num_caso     DECIMAL(10,0),
      f_pago       DATE,
      monto        DECIMAL(12,2),
      result_opera CHAR(10)
   END RECORD

   DEFINE
      manejador_rpt     om.SaxDocumentHandler,
   	  v_ruta_reporte    STRING, -- ruta del archivo del reporte
   	  v_ruta_listados   STRING, -- ruta de los listados
   	  v_ruta_ejecutable STRING, -- ruta del ejecutable
   	  v_nombre_usuario  LIKE seg_usuario.usuario_desc
                                                                                            
   # Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados         
    
   --Se consulta el nombre del usuario para despliegue en reporte 
   SELECT usuario_desc
   INTO   v_nombre_usuario
   FROM   seg_usuario 
   WHERE  usuario_cod = p_usuario_cod                                      
    
   --Se asigna la plantilla para generar el reporte
   IF fgl_report_loadCurrentSettings("PAGC90.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
                  
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_pago_cvt"
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE         
      DISPLAY "no fue posible generar el reporte"
      EXIT PROGRAM 
   END IF
   
   --Inicia el reporte de registros con rechazo
   START REPORT rpt_carga_lqinfo TO XML HANDLER manejador_rpt
      OUTPUT TO REPORT rpt_carga_lqinfo(reg_cifras.*, p_folio,v_nombre_usuario)                                                                
   FINISH REPORT rpt_carga_lqinfo
   
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_carga_lqinfo(reg_cifras, p_folio,p_nombre_usuario)

   DEFINE  p_folio DECIMAL(9,0)
   DEFINE reg_cifras RECORD
   	  ind_registro CHAR(25),
   	  f_archivo    DATE,     --DATETIME YEAR TO SECOND,
   	  nss          CHAR(11),
      curp         CHAR(18),
      credito_inf  DECIMAL(10,0),
      cod_ident    CHAR(10),
      marca_cambio CHAR(02),
      num_caso     DECIMAL(10,0),
      f_pago       DATE,
      monto        DECIMAL(12,2),
      result_opera CHAR(10)
   END RECORD

   DEFINE
      v_fecha_reporte  DATE,
      p_nombre_usuario LIKE seg_usuario.usuario_desc,
      v_rechazo        CHAR(25),
      v_estado         CHAR(10)
                                                                                                                                                                                          
   FORMAT                                                                                        
                                                                                              
      FIRST PAGE HEADER                                                                          
                                                                                              
         LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                                 
         PRINTX v_fecha_reporte USING "dd-mm-yyyy"                                                  
         PRINTX p_folio  
         PRINTX p_nombre_usuario 
         PRINTX p_usuario_cod                                                                 
                                                                                              
      ON EVERY ROW

         {-- código_rechazo = 0
         IF reg_cifras.ind_registro = 0 THEN
            LET v_rechazo = ""
         END IF
         -- código_rechazo = 3
         IF reg_cifras.ind_registro = 3 THEN
            LET v_rechazo = "NO TIENE CRÉDITO INF."
         END IF
         -- código_rechazo = 7
         IF reg_cifras.ind_registro = 7 THEN
            LET v_rechazo = "FECHA DE PAGO NULA"
         END IF
         -- código_rechazo = 8
         IF reg_cifras.ind_registro = 8 THEN
            LET v_rechazo = "IMPORTE NULO"
         END IF

         IF reg_cifras.result_opera = "01" THEN
            LET v_estado = "ACEPTADO"
         ELSE
            LET v_estado = "RECHAZADO"
         END IF}
         
         --LET reg_cifras.f_pago = reg_cifras.f_pago USING "dd-mm-yyyy"

         PRINTX reg_cifras.*
                                                                                           
END REPORT  