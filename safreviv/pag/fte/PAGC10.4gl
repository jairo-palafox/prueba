--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => pag                                                                    #
#Programa     => PAGC10                                                                 #
#Objetivo     => Consulta Totales de archivo de registro de pagos LQINFO                #
#Fecha inicio => ENERO 10, 2012                                                         # 
#Autor        => Gerardo Alfonso Vega Paredes                                           #
#########################################################################################

DATABASE safre_viv

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida LQINFO
   CALL fn_consulta_folio(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: PAGC10
Nombre: fn_consulta_info_recibida
Fecha creacion: ENERO 10, 2012
Autor: Gerardo Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_folio(p_usuario_co)
DEFINE p_usuario_co	            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio                  DECIMAL(9,0), -- folio
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_s_cadena               STRING, -- cadena de texto
       v_r_glo_ctr_archivo      RECORD LIKE glo_ctr_archivo.*,
       v_r_glo_folio            RECORD LIKE glo_folio.*
DEFINE 
      p_folio               DECIMAL(9,0), 
      vindice               SMALLINT,
      reg_cifras RECORD
         v_total_registros       DECIMAL(9,0),
         v_imp_ap_pat            DECIMAL(22,2),
         v_imp_am_cre            DECIMAL(22,2),
         v_aiv_ap_pat            DECIMAL(22,2),
         v_int_gen_pgo_ext       DECIMAL(22,2),
         v_aiv_gen_pgo_ext       DECIMAL(18,6)
      END RECORD,
      p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
      DEFINE        w                      ui.window
      DEFINE        f_w                    ui.form 

   OPEN WINDOW w_consulta_folio WITH FORM "PAGC101"
   
   LET  w = ui.Window.getCurrent()
   LET  f_w = w.getForm()

   CALL f_w.setelementhidden("HBox1",1)
   CALL f_w.setelementhidden("Group2",1)
   CALL f_w.setelementhidden("Group3",1)
   
  
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una afore
   --CALL v_cbx_folios.addItem(-1," ")
   
   INPUT v_folio WITHOUT DEFAULTS
      FROM cmb_folio    ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()   
         -- se asignan los valores por omision
         --LET v_folio = -1
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a,
                pag_sum_recauda b
         WHERE  a.proceso_cod = 1401 -- Liquidación de pagos
         AND    a.folio = b.folio
         ORDER BY a.folio DESC
         --AND
         -- estado = 2 -- integrado

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
         
         -- se envian los parametros de consulta
         --CALL fn_consulta_archivo(v_folio)
         ###
           LET p_b_despliegue_pantalla = TRUE
           -- se realiza la consulta para obtener las cifras

           SELECT total_registros
           INTO   reg_cifras.v_total_registros
           FROM   pag_sum_recauda
           WHERE  folio = v_folio 

           -- se obtienen las sumas de las cantidades de cta_his_pagos
           SELECT  SUM(imp_ap_pat)
           INTO    reg_cifras.v_imp_ap_pat
           FROM    cta_his_pagos
           WHERE   folio = v_folio
           
           SELECT  SUM(imp_am_cre)
           INTO    reg_cifras.v_imp_am_cre
           FROM    cta_his_pagos
           WHERE   folio = v_folio
           
           SELECT  SUM(int_gen_pgo_ext)
           INTO    reg_cifras.v_int_gen_pgo_ext
           FROM    cta_his_pagos
           WHERE   folio = v_folio
           
           SELECT  SUM(aiv_ap_pat)
           INTO    reg_cifras.v_aiv_ap_pat
           FROM    cta_his_pagos
           WHERE   folio = v_folio
           
           SELECT  SUM(aiv_gen_pgo_ext)
           INTO    reg_cifras.v_aiv_gen_pgo_ext
           FROM    cta_his_pagos
           WHERE   folio = v_folio           

--           LET vindice = 1
--           FOREACH cur_pag INTO reg_cifras.*
--              LET vindice = vindice + 1
--           END FOREACH

           DISPLAY BY NAME reg_cifras.*

            MENU ""
               COMMAND "Cancelar"
                  --LET reg_cifras.aiv_ap_pat = 0
                  CLEAR FORM
                  EXIT MENU

               COMMAND "Reporte"
                   CALL fn_reporte_carga_archivo(v_folio, reg_cifras.*, TRUE)
            END MENU
        END IF
         ###
         
      ON ACTION CANCEL
         EXIT INPUT

   
   END INPUT
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION


-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de Integración LQINFO
FUNCTION fn_reporte_carga_archivo(p_folio, reg_cifras, p_b_despliegue_pantalla)
    DEFINE p_folio                 INTEGER
    DEFINE p_b_despliegue_pantalla SMALLINT, -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    reg_cifras RECORD
           v_total_registros       DECIMAL(9,0),
           v_imp_ap_pat            DECIMAL(22,2),
           v_imp_am_cre            DECIMAL(22,2),
           v_aiv_ap_pat            DECIMAL(22,2),
           v_int_gen_pgo_ext       DECIMAL(22,2),
           v_aiv_gen_pgo_ext       DECIMAL(18,6)
      END RECORD,
      manejador_rpt        om.SaxDocumentHandler,
    	v_ruta_reporte       STRING, -- ruta del archivo del reporte
    	v_ruta_listados      STRING, -- ruta de los listados
    	v_ruta_ejecutable    STRING, -- ruta del ejecutable
    	v_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo,
    	v_nombre_usuario     LIKE seg_usuario.usuario_desc


        
        # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados         
                                                --r_ruta_bin, r_ruta_listados
    
    --Se consulta el nombre del usuario para despliegue en reporte 
    SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario 
     WHERE usuario_cod = p_usuario_cod                                      
     
   -- se consulta el nombre del archivo que se carga para despliegue en reporte
    SELECT nombre_archivo 
      INTO v_nombre_archivo
      FROM glo_ctr_archivo
     WHERE folio = p_folio
       AND proceso_cod = 1401
     
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("PAGC10.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_lqinfo"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
    --Inicia el reporte de registros con rechazo
    START REPORT rpt_carga_lqinfo TO XML HANDLER manejador_rpt
    -- Asigna el titulo del reporte
                
    OUTPUT TO REPORT rpt_carga_lqinfo(reg_cifras.*, p_folio, v_nombre_archivo, v_nombre_usuario)                                                                
    FINISH REPORT rpt_carga_lqinfo 
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_carga_lqinfo(reg_cifras, p_folio, p_nombre_archivo, p_nombre_usuario)
DEFINE  p_folio DECIMAL(9,0), 
        reg_cifras RECORD
           v_total_registros       DECIMAL(9,0),
           v_imp_ap_pat            DECIMAL(22,2),
           v_imp_am_cre            DECIMAL(22,2),
           v_aiv_ap_pat            DECIMAL(22,2),
           v_int_gen_pgo_ext       DECIMAL(22,2),
           v_aiv_gen_pgo_ext       DECIMAL(18,6)
        END RECORD,
        v_fecha_reporte          DATE,
        p_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo,
        p_nombre_usuario         LIKE seg_usuario.usuario_desc
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER                                                                          
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"                                                  
      PRINTX p_folio  
      PRINTX p_nombre_archivo
      PRINTX p_nombre_usuario 
      PRINTX p_usuario_cod                                                                 
                                                                                              
   ON EVERY ROW                                                                               
      PRINTX reg_cifras.*
                                                                                           
END REPORT  