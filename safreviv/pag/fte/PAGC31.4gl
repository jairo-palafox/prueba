--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC                                                                   #
#Objetivo     => Consulta de cifras control de Solo Infonavit                           #
#Autor        => Ivan Vega                                                              #
#Fecha inicio => Junio 20, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo     RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
        ruta_listados   CHAR(40)
       END RECORD,
       g_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion     SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo           STRING                       -- titulo de la ventana      
       ,v_folio              LIKE glo_folio.folio -- folio de consulta
       
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "PAGC311"
   
   -- se inicia el folio en null
   LET v_folio = NULL
   
   INPUT v_folio WITHOUT DEFAULTS
     FROM folio
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_folio IS NOT NULL ) THEN
            CALL f_consulta_cc_sinf(v_folio)
         ELSE
            CALL fn_mensaje("Atención","Debe capturar un folio","stop")
            CONTINUE INPUT
         END IF
         
      
      ON ACTION cancel
         EXIT INPUT
   
   END INPUT   
   CLOSE WINDOW w_consulta

   
END MAIN

{ ======================================================================
Clave: PAGC05
Nombre: f_consulta_cc_sinf
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Realiza una consulta sobre las cifras control de Solo Infonavit

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     Ago 27, 2012         - El renglon de amortizacion al credito se tomara
                                     la suma de montos cuando amortizacion es diferente de cero
                                   - El renglon de -- se tomara cuando el monto de amortizacion
                                     sea cero
======================================================================
}
FUNCTION f_consulta_cc_sinf(p_folio)
DEFINE    p_folio                 LIKE glo_folio.folio, -- folio de consulta
          v_r_despliegue          RECORD -- registro de consulta
             num_regs              INTEGER      ,
             imp_ap_pat            DECIMAL(12,2),
             imp_am_cre            DECIMAL(12,2),
             aiv_ap_pat            DECIMAL(12,2)
          END RECORD,             
          v_arr_despliegue        DYNAMIC ARRAY OF RECORD -- arreglo de despliegue
             concepto              VARCHAR(50)  ,
             num_regs              INTEGER      ,
             mxn                   DECIMAL(12,2), -- vivienda
             aivs                  DECIMAL(12,2), -- 
             am_cre                DECIMAL(12,2)  -- credito
          END RECORD,
          v_contador              SMALLINT,
          v_sql                   STRING, -- cadena con instruccion SQL
          v_handler               om.SaxDocumentHandler -- handler para el reporte
          
   -- ==================================================================================
   -- Aportaciones sin amortizacion
   LET v_sql = "\nSELECT COUNT(*)       ,",
               "\n       SUM(imp_ap_pat),", -- aportaciones
               "\n       SUM(imp_am_cre),", -- amortizacion del credito
               "\n       SUM(aiv_ap_pat) ", -- AIVS aportacion
               "\nFROM   cta_his_pagos   ",
               "\nWHERE  folio =         ", p_folio,
               "\nAND    imp_ap_pat <> 0  "
   
   DISPLAY v_sql
   
   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_cc_sinamort FROM v_sql
   DECLARE cur_consulta_cc_sinamort CURSOR FOR sid_consulta_cc_sinamort
   
   -- se inicia el contador
   LET v_contador = 1
   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_cc_sinamort INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].concepto = "Pagos PAU"
      
      LET v_arr_despliegue[v_contador].num_regs = v_r_despliegue.num_regs  
      LET v_arr_despliegue[v_contador].mxn      = v_r_despliegue.imp_ap_pat
      LET v_arr_despliegue[v_contador].aivs     = v_r_despliegue.aiv_ap_pat
      LET v_arr_despliegue[v_contador].am_cre   = v_r_despliegue.imp_am_cre
      
      -- se incrementa el contador
      LET v_contador = v_contador + 1
      
   END FOREACH
 
   -- ==================================================================================
   -- Aportaciones con amortizacion
   LET v_sql = "\nSELECT COUNT(*)       ,",
               "\n       SUM(imp_ap_pat),", -- aportaciones
               "\n       SUM(imp_am_cre),", -- amortizacion del credito
               "\n       SUM(aiv_ap_pat) ", -- AIVS aportacion
               "\nFROM   cta_his_pagos   ",
               "\nWHERE  folio =         ", p_folio,
               "\nAND    imp_ap_pat = 0 "
   
   DISPLAY v_sql
   
   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_cc_conamort FROM v_sql
   DECLARE cur_consulta_cc_conamort CURSOR FOR sid_consulta_cc_conamort
   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_cc_conamort INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].concepto = "Pagos PAU (Amortización)"
      
      LET v_arr_despliegue[v_contador].num_regs = v_r_despliegue.num_regs  
      LET v_arr_despliegue[v_contador].mxn      = v_r_despliegue.imp_ap_pat
      LET v_arr_despliegue[v_contador].aivs     = v_r_despliegue.aiv_ap_pat
      LET v_arr_despliegue[v_contador].am_cre   = v_r_despliegue.imp_am_cre
      
      -- se incrementa el contador
      LET v_contador = v_contador + 1
      
   END FOREACH
   
   -- se abre la ventana de despliegue de resultados
   OPEN WINDOW w_resultados WITH FORM "PAGC312"
   
   DISPLAY p_folio TO folio
   
   -- se despliegan los resultados
   DISPLAY ARRAY v_arr_despliegue TO tbl_despliegue.*
   ATTRIBUTES ( UNBUFFERED )
   
      ON ACTION accept
         EXIT DISPLAY
         
      ON ACTION cancel
         EXIT DISPLAY
         
      ON ACTION reporte
      
         -- se indica que se usara la plantilla
         IF ( fgl_report_loadCurrentSettings("PAGC31.4rp") ) THEN
            LET v_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         ELSE
            CALL fn_mensaje("Error","No se encuentra la plantilla PAGC31.4rp. No se puede emitir el reporte","stop")
            CONTINUE DISPLAY
         END IF
      
         -- se inicia la emision del reporte
         START REPORT rpt_consulta_cc TO XML HANDLER v_handler
         
         -- se transfieren los datos
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            OUTPUT TO REPORT rpt_consulta_cc(p_folio, v_arr_despliegue[v_contador].*)
         END FOR
         
         -- se finaliza el reporte
         FINISH REPORT rpt_consulta_cc
   
   END DISPLAY
   
   CLOSE WINDOW w_resultados
END FUNCTION

{ ======================================================================
Clave: PAGC05
Nombre: rpt_consulta_cc
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de cifras de control

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_consulta_cc(v_folio, v_r_despliegue)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
             concepto              VARCHAR(50)  ,
             num_regs              INTEGER      ,
             mxn                   DECIMAL(12,2),
             aivs                  DECIMAL(12,2),
             am_cre                DECIMAL(12,2)
          END RECORD,
          v_folio                 LIKE glo_folio.folio, -- folio
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para el totalizador
          v_total_num_regs           DECIMAL(9,0),
          v_total_mxn                DECIMAL(12,2),
          v_total_aivs               DECIMAL(12,2),
          v_total_amcre              DECIMAL(12,2)

FORMAT

   FIRST PAGE HEADER
      
      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = g_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED

      LET v_total_num_regs = 0
      LET v_total_mxn      = 0
      LET v_total_aivs     = 0
      LET v_total_amcre    = 0
      
      PRINTX v_folio, g_usuario_cod, v_fecha, v_nombre_usuario
  
   ON EVERY ROW
      PRINTX v_r_despliegue.*

      LET v_total_num_regs = v_total_num_regs + v_r_despliegue.num_regs
      LET v_total_mxn      = v_total_mxn      + v_r_despliegue.mxn
      LET v_total_aivs     = v_total_aivs     + v_r_despliegue.aivs
      LET v_total_amcre    = v_total_amcre    + v_r_despliegue.am_cre

   ON LAST ROW
      PRINTX   v_total_num_regs,
               v_total_mxn     ,
               v_total_aivs    ,
               v_total_amcre


             
END REPORT   
             
             