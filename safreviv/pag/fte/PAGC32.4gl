--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC                                                                   #
#Objetivo     => Consulta de cifras control de Fortalecimiento al credito               #
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
   OPEN WINDOW w_consulta WITH FORM "PAGC321"
   
   -- se inicia el folio en null
   LET v_folio = NULL
   
   INPUT v_folio WITHOUT DEFAULTS
     FROM folio
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_folio IS NOT NULL ) THEN
            CALL f_consulta_cc_fc(v_folio)
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
Nombre: f_consulta_cc_fc
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Realiza una consulta sobre las cifras control de Pagos Fortalecimiento
al credito

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_consulta_cc_fc(p_folio)
DEFINE    p_folio                 LIKE glo_folio.folio, -- folio de consulta
          v_r_despliegue          RECORD -- registro de consulta
             resultado_operacion   CHAR(2)      ,
             num_regs              INTEGER      ,
             imp_ap_fc             DECIMAL(12,2)
          END RECORD,             
          v_arr_despliegue        DYNAMIC ARRAY OF RECORD -- arreglo de despliegue
             concepto              VARCHAR(50)  ,
             num_regs              INTEGER      ,
             mxn                   DECIMAL(12,2),
             aivs                  DECIMAL(12,2),
             resultado             VARCHAR(20)
          END RECORD,
          v_contador              SMALLINT,
          v_sql                   STRING, -- cadena con instruccion SQL
          v_handler               om.SaxDocumentHandler -- handler para el reporte
          
   LET v_sql = "\nSELECT   result_operacion,", -- resultado para agrupar por rechazados y aceptados
               "\n         COUNT(*)        ,", -- num. de registros
               "\n         SUM(imp_ap_fc)   ", -- aportacion de FC
               "\nFROM     pag_det_fc       ", -- tabla de detalle de fortalecimiento al credito
               "\nWHERE    folio =          ", p_folio,
               "\nGROUP BY result_operacion ", -- agrupado por el resultado
               "\nORDER BY result_operacion " -- tabla de detalle de fortalecimiento al credito
               
   
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
      LET v_arr_despliegue[v_contador].mxn      = v_r_despliegue.imp_ap_fc
      LET v_arr_despliegue[v_contador].aivs     = 0

      -- se separan aceptados y rechazados
      IF ( v_r_despliegue.resultado_operacion = "01" ) THEN
         -- aceptado
         LET v_arr_despliegue[v_contador].resultado = "ACEPTADO"
      ELSE
         -- rechazado
         LET v_arr_despliegue[v_contador].resultado = "RECHAZADO"
      END IF
      
      
      -- se incrementa el contador
      LET v_contador = v_contador + 1
      
   END FOREACH
 
   
   -- se abre la ventana de despliegue de resultados
   OPEN WINDOW w_resultados WITH FORM "PAGC322"
   
   DISPLAY p_folio TO folio
   
   -- se despliegan los resultados
   DISPLAY ARRAY v_arr_despliegue TO tbl_despliegue.*
   ATTRIBUTES ( UNBUFFERED )

      BEFORE DISPLAY 
         IF(v_arr_despliegue.getLength() = 0)THEN
            CALL fn_mensaje("Aviso","No se encontraron registros con criterio dado","information")
            EXIT DISPLAY
         END IF
   
      ON ACTION accept
         EXIT DISPLAY
         
      ON ACTION cancel
         EXIT DISPLAY
         
      ON ACTION reporte
      
         -- se indica que se usara la plantilla
         IF ( fgl_report_loadCurrentSettings("PAGC32.4rp") ) THEN
            LET v_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         ELSE
            CALL fn_mensaje("Error","No se encuentra la plantilla PAGC32.4rp. No se puede emitir el reporte","stop")
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
Clave: PAGC32
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
DEFINE    v_r_despliegue             RECORD -- registro de consulta
             concepto                 VARCHAR(50)  ,
             num_regs                 INTEGER      ,
             mxn                      DECIMAL(12,2),
             aivs                     DECIMAL(12,2),
             resultado                VARCHAR(20)
          END RECORD,
          v_folio                    LIKE glo_folio.folio, -- folio
          v_fecha                    STRING, -- fecha de emision del reporte
          v_nombre_usuario           VARCHAR(100),
          v_tipo_registro            VARCHAR(20),
          v_nombre_archivo           VARCHAR(40),
          v_nombre_archivo_rechazos  STRING,
          v_ruta_envio               VARCHAR(40),
          v_archivo_rechazos         VARCHAR(100),
          -- variables para el totalizador
          v_total_regs               DECIMAL(12,2),
          v_total_mxn                DECIMAL(12,2),
          v_total_aivs               DECIMAL(12,2),
          v_folio_formato            VARCHAR(9),
          v_contador                 INTEGER

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

      -- se obtiene el nombre del archivo
      SELECT nombre_archivo
      INTO   v_nombre_archivo
      FROM   glo_ctr_archivo
      WHERE  folio = v_folio

      -- si no se encuentra el nombre del archivo indicamos error
      IF ( v_nombre_archivo IS NULL ) THEN
         LET v_nombre_archivo = "No se encuentra nombre archivo"
      END IF

      -- se inician los totalizadores
      LET v_total_regs  = 0
      LET v_total_mxn   = 0
      LET v_total_aivs  = 0

      
      PRINTX v_folio, g_usuario_cod, v_fecha, v_nombre_usuario, v_nombre_archivo
      
      

   BEFORE GROUP OF v_r_despliegue.resultado
      IF ( v_r_despliegue.resultado = "ACEPTADO" ) THEN
         LET v_tipo_registro = "Registros Aceptados"
      ELSE
         LET v_tipo_registro = "Registros Rechazados"
      END IF
      
      PRINTX v_tipo_registro

   ON EVERY ROW
      PRINTX v_r_despliegue.*

      -- se acumulan los totalizadores
      LET v_total_regs  = v_total_regs + v_r_despliegue.num_regs
      LET v_total_mxn   = v_total_mxn  + v_r_despliegue.mxn
      LET v_total_aivs  = v_total_aivs + v_r_despliegue.aivs

   
   ON LAST ROW
      -- se revisa si hubo registros rechazados
      SELECT COUNT(*)
      INTO   v_contador
      FROM   pag_det_fc
      WHERE  folio = v_folio
      AND    result_operacion = '02'
      
      IF ( v_contador < 1 ) THEN
         LET v_nombre_archivo_rechazos = "No se tienen registros rechazados para este folio."
      ELSE
         -- se recupera el nombre del archivo de rechazos
         SELECT ruta_envio
         INTO   v_ruta_envio
         FROM   seg_modulo
         WHERE  modulo_cod = "pag"
              
         -- el nombre del archivo es
         -- NombreArchivoOriginal + _FOLIO_rechazoFC.fort"
         LET v_folio_formato = v_folio USING "&&&&&&&&&"
         --LET v_archivo_rechazos = "Fortalecimiento_cred_crechazos_" || v_folio_formato || "_rechazoFC.fort"
         
         LET v_contador = LENGTH(v_nombre_archivo CLIPPED)
         
         LET v_nombre_archivo_rechazos = v_nombre_archivo[1,v_contador - 5] || "_" || v_folio_formato || "_rechazoFC.fort"
      END IF
         
      PRINTX  v_total_regs             ,
              v_total_mxn              ,
              v_total_aivs             ,
              v_nombre_archivo_rechazos
              

END REPORT   
             
             