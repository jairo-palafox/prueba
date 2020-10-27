--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17/04/2013
--==============================================================================


--==============================================================================
-- Modulo       => PAG                                                           
-- Programa     => PAGC67                                                        
-- Objetivo     => Consulta de antiguedad de aportaciones voluntarias            
-- Fecha inicio => 23 Enero 2015                                                 
-- Autor        => GERARDO ALFONSO VEGA PAREDES
-- Nota         => Programa basado en el PAGC64
--==============================================================================

DATABASE safre_viv
GLOBALS "PAGG01.4gl"  # archivo de variables globales proceso_cod, opera_cod

DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado

MAIN
   DEFINE p_tipo_ejecucion SMALLINT,              -- forma como ejecutara el programa
          p_s_titulo       STRING,                -- titulo de la ventana      
          f_fecha          DATE                   -- folio de consulta
       
   # se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   # si se obtuvo el titulo, se pone como titulo de programa
   IF( p_s_titulo IS NOT NULL )THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = 'pag'

   OPEN WINDOW w_consulta WITH FORM v_ruta_ejecutable CLIPPED||"/PAGC671"
   
   LET f_fecha = NULL
   
   INPUT f_fecha WITHOUT DEFAULTS FROM fecha ATTRIBUTES (UNBUFFERED)
   
      ON ACTION ACCEPT
         IF( f_fecha IS NOT NULL )THEN
            CALL f_consulta_cifras_control_ap_voluntarias(f_fecha)
         ELSE
            CALL fn_mensaje("Atención","Debe capturar una fecha","stop")
            CONTINUE INPUT
         END IF

      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   CLOSE WINDOW w_consulta
   
END MAIN

{ ==============================================================================
Nombre: f_consulta_cifras_control_ap_voluntarias
Fecha creacion: 23 Enero 2015
Autor: GERARDO ALFONSO VEGA PAREDES
Narrativa del proceso que realiza:
Reporte de antiguedad de aportaciones voluntarias

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================
}

FUNCTION f_consulta_cifras_control_ap_voluntarias(p_fecha)

   DEFINE p_fecha DATE
   
--   DEFINE v_detalle_apvol DYNAMIC ARRAY OF RECORD # arreglo de despliegue
   DEFINE v_detalle_apvol RECORD
      v_f_pago     DATE,
      v_imp_ap_vol DECIMAL(16,6)
   END RECORD
   
   DEFINE v_imp RECORD
   	  imp_12 DECIMAL(12,2),
      imp_24 DECIMAL(12,2),
      imp_36 DECIMAL(12,2),
      imp_48 DECIMAL(12,2),
      imp_60 DECIMAL(12,2),
      imp_72 DECIMAL(12,2)
   END RECORD
          
   DEFINE v_cont RECORD
      cont_12 INTEGER,
      cont_24 INTEGER,
      cont_36 INTEGER,
      cont_48 INTEGER,
      cont_60 INTEGER,
      cont_72 INTEGER
   END RECORD

   DEFINE v_dias,i INTEGER
   
   DEFINE v_siefore         SMALLINT,
          v_fecha_valuacion DATE,
          v_contador        SMALLINT,
          v_precio_fondo    LIKE glo_valor_fondo.precio_fondo,
          v_consulta        STRING,                           # cadena con instruccion SQL
          v_manejador       om.SaxDocumentHandler,            # handler para el reporte
          v_ruta_reporte    LIKE seg_modulo.ruta_listados,
          v_ruta_listados   LIKE seg_modulo.ruta_listados

   LET v_siefore = 11
   
   LET v_consulta = "\n SELECT f_pago,",         # num. de registros
                    "\n        imp_ap_vol ",   # aportacion voluntaria
                    "\n   FROM pag_det_apvol ",     # tabla de detalle de aportacion voluntaria
                    "\n  WHERE f_pago <= ?",
                    "\n  ORDER BY f_pago"  
   
   # se prepara y ejecuta la consulta
   PREPARE prp_consulta_detalle_apvol FROM v_consulta
   DECLARE cur_consulta_detalle_apvol CURSOR FOR prp_consulta_detalle_apvol

   LET v_contador = 0

   LET v_imp.imp_12   = 0;
   LET v_cont.cont_12 = 0;
   LET v_imp.imp_24   = 0;
   LET v_cont.cont_24 = 0;
   LET v_imp.imp_36   = 0;
   LET v_cont.cont_36 = 0;
   LET v_imp.imp_48   = 0;
   LET v_cont.cont_48 = 0;
   LET v_imp.imp_60   = 0;
   LET v_cont.cont_60 = 0;
   LET v_imp.imp_72   = 0;
   LET v_cont.cont_72 = 0;
   
   # se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_detalle_apvol USING p_fecha
                            INTO v_detalle_apvol.*

      LET v_dias = p_fecha - v_detalle_apvol.v_f_pago

      CASE 
         WHEN v_dias <= 365
            LET v_imp.imp_12 = v_imp.imp_12 + v_detalle_apvol.v_imp_ap_vol
            LET v_cont.cont_12 = v_cont.cont_12 + 1
            
         WHEN v_dias > 365   AND v_dias <= 730
            LET v_imp.imp_24 = v_imp.imp_24 + v_detalle_apvol.v_imp_ap_vol
            LET v_cont.cont_24 = v_cont.cont_24 + 1
            
         WHEN v_dias > 730   AND v_dias <= 1095
            LET v_imp.imp_36 = v_imp.imp_36 + v_detalle_apvol.v_imp_ap_vol
            LET v_cont.cont_36 = v_cont.cont_36 + 1
            
         WHEN v_dias > 1095  AND v_dias <= 1460
            LET v_imp.imp_48 = v_imp.imp_48 + v_detalle_apvol.v_imp_ap_vol
            LET v_cont.cont_48 = v_cont.cont_48 + 1
            
         WHEN v_dias > 1460  AND v_dias <= 1825
            LET v_imp.imp_60 = v_imp.imp_60 + v_detalle_apvol.v_imp_ap_vol
            LET v_cont.cont_60 = v_cont.cont_60 + 1

         OTHERWISE            
--         WHEN v_dias > 1825
            LET v_imp.imp_72 = v_imp.imp_72 + v_detalle_apvol.v_imp_ap_vol
            LET v_cont.cont_72 = v_cont.cont_72 + 1
            
      END CASE

      LET v_contador = v_contador + 1      

     
   END FOREACH
   FREE cur_consulta_detalle_apvol

   # se indica que se usara la plantilla
   IF ( fgl_report_loadCurrentSettings("PAGC67.4rp") ) THEN
      CALL fgl_report_selectDevice ("PDF")
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_ap_vol_antiguedad"
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET v_manejador = fgl_report_commitCurrentSettings()
   ELSE
      CALL fn_mensaje("Error","No se encuentra la plantilla PAGC67.4rp. No se puede emitir el reporte","stop")
   END IF

   
   # se inicia la emision del reporte
   START REPORT rpt_genera_reporte_cifras_control TO XML HANDLER v_manejador
      -- se transfieren los datos
--      FOR v_contador = 1 TO v_detalle_apvol.getLength()
      FOR i = 1 TO v_contador - 1
         --LET v_fecha_valuacion = v_detalle_apvol[v_contador].v_f_pago --USING "ddmmyyyy"
         --DISPLAY "v_fecha_valuacion:",v_fecha_valuacion 
         OUTPUT TO REPORT rpt_genera_reporte_cifras_control(p_fecha,v_imp.*,v_cont.*)
      END FOR            
   FINISH REPORT rpt_genera_reporte_cifras_control
   
END FUNCTION

REPORT rpt_genera_reporte_cifras_control(p_fecha,v_imp,v_cont)
   
   DEFINE p_fecha DATE
   
   DEFINE v_imp RECORD
   	  imp_12 DECIMAL(12,2),
      imp_24 DECIMAL(12,2),
      imp_36 DECIMAL(12,2),
      imp_48 DECIMAL(12,2),
      imp_60 DECIMAL(12,2),
      imp_72 DECIMAL(12,2)
   END RECORD
          
   DEFINE v_cont RECORD
      cont_12 INTEGER,
      cont_24 INTEGER,
      cont_36 INTEGER,
      cont_48 INTEGER,
      cont_60 INTEGER,
      cont_72 INTEGER
   END RECORD   
   
   DEFINE p_detalle_apvol RECORD       # arreglo de despliegue
      v_concepto         VARCHAR(50),
      v_f_pago           DATE,
      v_conteo_registros INTEGER,
      v_imp_ap_vol       DECIMAL(12,2),
      v_aivs             DECIMAL(16,6),
      v_resultado        VARCHAR(20)
   END RECORD
   
   DEFINE p_precio_fondo            LIKE glo_valor_fondo.precio_fondo,
          p_fecha_valuacion         DATE,
          v_fecha                   STRING, # fecha de emision del reporte
          v_nombre_usuario          VARCHAR(100),
          v_tipo_registro           VARCHAR(20),
          v_nombre_archivo          VARCHAR(40),
          v_nombre_archivo_rechazos STRING,
          v_ruta_envio              VARCHAR(40),
          
          # variables para el totalizador
          v_total_regs    DECIMAL(12,2),
          v_total_mxn     DECIMAL(12,2),
          v_total_aivs    DECIMAL(12,2),
          f_fecha_formato VARCHAR(9),
          v_contador      INTEGER

   FORMAT

      FIRST PAGE HEADER
         # se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"

         #LET p_fecha = p_fecha USING "dd-mm-yyyy"
         
         # se obtiene el nombre del usuario
         SELECT usuario_desc
         INTO   v_nombre_usuario
         FROM   seg_usuario
         WHERE  usuario_cod = g_usuario_cod
      
         LET v_nombre_usuario = v_nombre_usuario CLIPPED


         PRINTX p_fecha USING "dd-mm-yyyy", 
                g_usuario_cod, 
                v_fecha, 
                v_nombre_usuario

         PRINTX v_cont.*,v_imp.*

END REPORT