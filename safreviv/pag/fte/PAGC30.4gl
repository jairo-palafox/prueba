--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC                                                                   #
#Objetivo     => Consulta de cifras control de SAR92                                    #
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
   OPEN WINDOW w_consulta WITH FORM "PAGC301"
   
   -- se inicia el folio en null
   LET v_folio = NULL
   
   INPUT v_folio WITHOUT DEFAULTS
     FROM folio
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_folio IS NOT NULL ) THEN
            CALL f_consulta_cc_sar92(v_folio)
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
Nombre: f_consulta_cc_sar92
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Realiza una consulta sobre las cifras control de SAR92

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_consulta_cc_sar92(p_folio)
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
             mxn                   DECIMAL(12,2),
             aivs                  DECIMAL(12,2)
          END RECORD,
          v_contador              SMALLINT,
          v_sql                   STRING, -- cadena con instruccion SQL
          v_handler               om.SaxDocumentHandler -- handler para el reporte
          
   LET v_sql = "\nSELECT COUNT(*)       ,",
               "\n       SUM(imp_viv)   ,", -- aportaciones
               "\n       0              ,", -- amortizacion del credito
               "\n       SUM(aivs_ap)    ", -- AIVS aportacion
               "\nFROM   pag_det_sar92   ",
               "\nWHERE  folio =         ", p_folio
   
   DISPLAY v_sql
   
   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_cc FROM v_sql
   DECLARE cur_consulta_cc CURSOR FOR sid_consulta_cc
   
   -- se inicia el contador
   LET v_contador = 1
   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_cc INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].concepto = "APORTACIONES"
      
      LET v_arr_despliegue[v_contador].num_regs = v_r_despliegue.num_regs  
      LET v_arr_despliegue[v_contador].mxn      = v_r_despliegue.imp_ap_pat
      LET v_arr_despliegue[v_contador].aivs     = v_r_despliegue.aiv_ap_pat
      

      -- se incrementa el contador
      LET v_contador = v_contador + 1      
   END FOREACH
 
   
   -- se abre la ventana de despliegue de resultados
   OPEN WINDOW w_resultados WITH FORM "PAGC302"
   
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
         IF ( fgl_report_loadCurrentSettings("PAGC30.4rp") ) THEN
            LET v_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         ELSE
            CALL fn_mensaje("Error","No se encuentra la plantilla PAGC30.4rp. No se puede emitir el reporte","stop")
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
             aivs                  DECIMAL(12,2)
          END RECORD,
          v_folio                 LIKE glo_folio.folio, -- folio
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular por destino
          v_destino_imp_ap_pat       DECIMAL(12,2),
          v_destino_aiv_ap_pat       DECIMAL(12,2),
          v_destino_imp_am_cre       DECIMAL(12,2),
          v_destino_int_gen_pgo_ext  DECIMAL(12,2),
          v_destino_aiv_gen_pgo_ext  DECIMAL(12,2),
          -- variables para acumular por ind liquidacion
          v_indliq_imp_ap_pat        DECIMAL(12,2),
          v_indliq_aiv_ap_pat        DECIMAL(12,2),
          v_indliq_imp_am_cre        DECIMAL(12,2),
          v_indliq_int_gen_pgo_ext   DECIMAL(12,2),
          v_indliq_aiv_gen_pgo_ext   DECIMAL(12,2),
          -- variables para acumular por localiza trabajador
          v_localiza_imp_ap_pat      DECIMAL(12,2),
          v_localiza_aiv_ap_pat      DECIMAL(12,2),
          v_localiza_imp_am_cre      DECIMAL(12,2),
          v_localiza_int_gen_pgo_ext DECIMAL(12,2),
          v_localiza_aiv_gen_pgo_ext DECIMAL(12,2),
          -- variables para el totalizador
          -- variables para acumular por localiza trabajador
          v_total_imp_ap_pat         DECIMAL(12,2),
          v_total_aiv_ap_pat         DECIMAL(12,2),
          v_total_imp_am_cre         DECIMAL(12,2),
          v_total_int_gen_pgo_ext    DECIMAL(12,2),
          v_total_aiv_gen_pgo_ext    DECIMAL(12,2)


          
          
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
      
      PRINTX v_folio, g_usuario_cod, v_fecha, v_nombre_usuario
      
      



   ON EVERY ROW
      PRINTX v_r_despliegue.*
{      
      -- se acumulan los montos 
      LET v_localiza_imp_ap_pat      = v_localiza_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_localiza_aiv_ap_pat      = v_localiza_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_localiza_imp_am_cre      = v_localiza_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_localiza_int_gen_pgo_ext = v_localiza_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_localiza_aiv_gen_pgo_ext = v_localiza_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext

      -- se acumulan los montos de ind_liquidacion
      LET v_indliq_imp_ap_pat        = v_indliq_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_indliq_aiv_ap_pat        = v_indliq_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_indliq_imp_am_cre        = v_indliq_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_indliq_int_gen_pgo_ext   = v_indliq_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_indliq_aiv_gen_pgo_ext   = v_indliq_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext
      
      -- se acumulan los montos por destino
      LET v_destino_imp_ap_pat       = v_destino_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_destino_aiv_ap_pat       = v_destino_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_destino_imp_am_cre       = v_destino_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_destino_int_gen_pgo_ext  = v_destino_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_destino_aiv_gen_pgo_ext  = v_destino_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext

      -- se acumula el total
      LET v_total_imp_ap_pat         = v_total_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_total_aiv_ap_pat         = v_total_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_total_imp_am_cre         = v_total_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_total_int_gen_pgo_ext    = v_total_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_total_aiv_gen_pgo_ext    = v_total_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext
   
   ON LAST ROW
      PRINTX v_total_imp_ap_pat     ,
             v_total_aiv_ap_pat     ,
             v_total_imp_am_cre     ,
             v_total_int_gen_pgo_ext,
             v_total_aiv_gen_pgo_ext
}
             
END REPORT   
             
             