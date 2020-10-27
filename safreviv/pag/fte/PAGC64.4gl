--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17/04/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGC61                                                        #
#Objetivo     => Consulta de cifras control de aportaciones voluntarias        #
#Fecha inicio => 17 Abril de 2013                                              #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  # archivo de variables globales proceso_cod, opera_cod

DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado

MAIN
DEFINE p_tipo_ejecucion     SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo           STRING                       -- titulo de la ventana      
       ,v_folio              LIKE glo_folio.folio -- folio de consulta
       
   # se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   # si se obtuvo el titulo, se pone como titulo de programa
   IF( p_s_titulo IS NOT NULL )THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'pag'

   # se abre la ventana
   OPEN WINDOW w_consulta WITH FORM v_ruta_ejecutable CLIPPED||"/PAGC641"
   
      # se inicia el folio en null
      LET v_folio = NULL
   
      INPUT v_folio WITHOUT DEFAULTS FROM folio ATTRIBUTES (UNBUFFERED)
   
         ON ACTION ACCEPT
            IF( v_folio IS NOT NULL )THEN
               CALL f_consulta_cifras_control_ap_voluntarias(v_folio)
            ELSE
               CALL fn_mensaje("Atención","Debe capturar un folio","stop")
               CONTINUE INPUT
            END IF

         ON ACTION CANCEL
            EXIT INPUT
   
      END INPUT
   CLOSE WINDOW w_consulta
   
END MAIN

{ ==============================================================================
Nombre: f_consulta_cifras_control_ap_voluntarias
Fecha creacion: 17 Abril del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
Realiza una consulta sobre las cifras control de Pagos de aportaciones
voluntarias

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================
}
FUNCTION f_consulta_cifras_control_ap_voluntarias(p_folio)
DEFINE p_folio                 LIKE glo_folio.folio, # folio de consulta
       v_detalle_apvol_aux   RECORD # registro de consulta
         v_f_pago            DATE,
         v_conteo_registros  INTEGER,
         v_sum_pesos         DECIMAL(12,2),
         v_sum_acciones      DECIMAL(16,6)
       END RECORD,             
       v_detalle_apvol DYNAMIC ARRAY OF RECORD # arreglo de despliegue
         v_concepto            VARCHAR(50),
         v_f_pago              DATE,
         v_conteo_registros    INTEGER,
         v_imp_ap_vol          DECIMAL(16,6),
         v_aivs                DECIMAL(16,6),
         v_resultado           VARCHAR(20)
       END RECORD,
       v_acciones            RECORD
         v_estado            SMALLINT,
         v_valor             DECIMAL(18,6)
       END RECORD,
       v_siefore             SMALLINT,
       v_fecha_valuacion     DATE,
       v_contador            SMALLINT,
       v_precio_fondo        LIKE glo_valor_fondo.precio_fondo,
       v_consulta            STRING, # cadena con instruccion SQL
       v_manejador           om.SaxDocumentHandler, # handler para el reporte
       v_ruta_reporte        LIKE seg_modulo.ruta_listados,
       v_ruta_listados       LIKE seg_modulo.ruta_listados


   LET v_siefore = 11
   
   LET v_consulta = "\n SELECT f_liquida,",         # num. de registros
                    "\n        COUNT(*),",         # num. de registros
                    "\n        SUM(monto_pesos),",   # aportacion voluntaria
                    "\n        SUM(monto_acciones)",   # aportacion voluntaria
                    "\n   FROM cta_movimiento",     # tabla de detalle de aportacion voluntaria
                    "\n  WHERE folio_liquida = ?",
                    "\n  GROUP BY f_liquida ", # agrupado por el resultado
                    "\n  ORDER BY f_liquida"  
   
   # se prepara y ejecuta la consulta
   PREPARE prp_consulta_detalle_apvol FROM v_consulta
   DECLARE cur_consulta_detalle_apvol CURSOR FOR prp_consulta_detalle_apvol

   LET v_consulta = "EXECUTE FUNCTION fn_del_cal_total_acciones(?,?,?)"
   PREPARE prp_rec_aivs FROM v_consulta

   LET v_consulta = "\n SELECT precio_fondo",
                    "\n   FROM glo_valor_fondo",
                    "\n  WHERE fondo = ?",
                    "\n    AND f_valuacion = ?"
   PREPARE prp_recupera_precio_fondo FROM v_consulta

   LET v_consulta = "\n SELECT FIRST 1 f_liquida",
                    "\n   FROM cta_movimiento",
                    "\n  WHERE folio_liquida = ?"                    
   PREPARE prp_recupera_fecha FROM v_consulta
   EXECUTE prp_recupera_fecha USING p_folio
                               INTO v_fecha_valuacion
            
   # se inicia el contador
   LET v_contador = 1
   
   # se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_detalle_apvol USING p_folio
                            INTO v_detalle_apvol_aux.*
      LET v_detalle_apvol[v_contador].v_concepto         = "VOLUNTARIAS"
      LET v_detalle_apvol[v_contador].v_f_pago           = v_detalle_apvol_aux.v_f_pago --USING "ddmmyyyy"
      LET v_detalle_apvol[v_contador].v_conteo_registros = v_detalle_apvol_aux.v_conteo_registros  
      LET v_detalle_apvol[v_contador].v_imp_ap_vol       = v_detalle_apvol_aux.v_sum_pesos
      LET v_detalle_apvol[v_contador].v_aivs             = v_detalle_apvol_aux.v_sum_acciones

      
      {EXECUTE prp_recupera_precio_fondo USING v_siefore,
                                              v_fecha_valuacion
                                         INTO v_precio_fondo
      
      LET v_detalle_apvol[v_contador].v_aivs = v_detalle_apvol[v_contador].v_imp_ap_vol/ v_precio_fondo 
      }

      # se separan aceptados y rechazados
      {IF( v_detalle_apvol_aux.v_resultado_operacion = "01" )THEN
         # aceptado
         LET v_detalle_apvol[v_contador].v_resultado = "ACEPTADO"
      ELSE
         # rechazado
         LET v_detalle_apvol[v_contador].v_resultado = "RECHAZADO"
      END IF}
      
      LET v_contador = v_contador + 1
      
   END FOREACH
   FREE cur_consulta_detalle_apvol
 
   
   # se abre la ventana de despliegue de resultados
   OPEN WINDOW w_resultados WITH FORM v_ruta_ejecutable CLIPPED||"/PAGC642"
   
      DISPLAY p_folio TO folio
   
      -- se despliegan los resultados
      DISPLAY ARRAY v_detalle_apvol TO tbl_despliegue.* ATTRIBUTES ( UNBUFFERED )

         BEFORE DISPLAY 
            IF(v_detalle_apvol.getLength() = 0)THEN
               CALL fn_mensaje("Aviso","No se encontraron registros con criterio dado","information")
               EXIT DISPLAY
            END IF
   
         ON ACTION ACCEPT
            EXIT DISPLAY
         
         ON ACTION CANCEL
            EXIT DISPLAY
         
         ON ACTION reporte
            # se indica que se usara la plantilla
            IF ( fgl_report_loadCurrentSettings("PAGC64.4rp") ) THEN
               CALL fgl_report_selectDevice ("PDF")
               LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_aportaciones_voluntarias"
               CALL fgl_report_setOutputFileName(v_ruta_reporte)
               CALL fgl_report_selectPreview(1)
               LET v_manejador = fgl_report_commitCurrentSettings()
            ELSE
               CALL fn_mensaje("Error","No se encuentra la plantilla PAGC64.4rp. No se puede emitir el reporte","stop")
               CONTINUE DISPLAY
            END IF

      
            # se inicia la emision del reporte
            START REPORT rpt_genera_reporte_cifras_control TO XML HANDLER v_manejador
               -- se transfieren los datos
               FOR v_contador = 1 TO v_detalle_apvol.getLength()
                  --LET v_fecha_valuacion = v_detalle_apvol[v_contador].v_f_pago --USING "ddmmyyyy"
                  --DISPLAY "v_fecha_valuacion:",v_fecha_valuacion 
                  EXECUTE prp_recupera_precio_fondo USING v_siefore,
                                                          v_fecha_valuacion
                                                     INTO v_precio_fondo
                  OUTPUT TO REPORT rpt_genera_reporte_cifras_control(p_folio, v_detalle_apvol[v_contador].*, v_precio_fondo,v_fecha_valuacion)
               END FOR            
            FINISH REPORT rpt_genera_reporte_cifras_control
   
      END DISPLAY
   
   CLOSE WINDOW w_resultados
END FUNCTION

{ ======================================================================
Nombre: rpt_genera_reporte_cifras_control
Fecha creacion: 17 Abril del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
Genera el reporte de cifras de control de aportaciones voluntarias

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_genera_reporte_cifras_control(p_folio, p_detalle_apvol,p_precio_fondo,p_fecha_valuacion)
DEFINE p_folio         LIKE glo_folio.folio, # folio
       p_detalle_apvol RECORD # arreglo de despliegue
         v_concepto         VARCHAR(50),
         v_f_pago           DATE,
         v_conteo_registros INTEGER,
         v_imp_ap_vol       DECIMAL(12,2),
         v_aivs             DECIMAL(16,6),
         v_resultado        VARCHAR(20)
       END RECORD,
       p_precio_fondo             LIKE glo_valor_fondo.precio_fondo,
       p_fecha_valuacion          DATE,
       v_fecha                    STRING, # fecha de emision del reporte
       v_nombre_usuario           VARCHAR(100),
       v_tipo_registro            VARCHAR(20),
       v_nombre_archivo           VARCHAR(40),
       v_nombre_archivo_rechazos  STRING,
       v_ruta_envio               VARCHAR(40),
       # variables para el totalizador
       v_total_regs               DECIMAL(12,2),
       v_total_mxn                DECIMAL(12,2),
       v_total_aivs               DECIMAL(12,2),
       v_folio_formato            VARCHAR(9),
       v_contador                 INTEGER

   FORMAT

      FIRST PAGE HEADER
         # se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"

         LET p_fecha_valuacion = p_fecha_valuacion USING "dd-mm-yyyy"
         
         # se obtiene el nombre del usuario
         SELECT usuario_desc
           INTO v_nombre_usuario
           FROM seg_usuario
          WHERE usuario_cod = g_usuario_cod
      
         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         # se obtiene el nombre del archivo
         SELECT nombre_archivo
           INTO v_nombre_archivo
           FROM glo_ctr_archivo
          WHERE folio = p_folio

         # si no se encuentra el nombre del archivo indicamos error
         IF( v_nombre_archivo IS NULL ) THEN
            LET v_nombre_archivo = "No se encuentra nombre archivo"
         END IF

         # se inician los totalizadores
         LET v_total_regs  = 0
         LET v_total_mxn   = 0
         LET v_total_aivs  = 0
         
         PRINTX p_folio, 
                g_usuario_cod, 
                v_fecha, 
                v_nombre_usuario, 
                v_nombre_archivo,
                p_precio_fondo,
                p_fecha_valuacion

         
      BEFORE GROUP OF p_detalle_apvol.v_resultado
         IF( p_detalle_apvol.v_resultado = "ACEPTADO" ) THEN
            LET v_tipo_registro = "Registros Aceptados"
         ELSE
            LET v_tipo_registro = "Registros Rechazados"
         END IF
      
         PRINTX v_tipo_registro

         
      ON EVERY ROW
         PRINTX p_detalle_apvol.*
         # se acumulan los totalizadores
         LET v_total_regs  = v_total_regs + p_detalle_apvol.v_conteo_registros
         LET v_total_mxn   = v_total_mxn  + p_detalle_apvol.v_imp_ap_vol
         LET v_total_aivs  = v_total_aivs + p_detalle_apvol.v_aivs

      ON LAST ROW
         # se revisa si hubo registros rechazados
         SELECT COUNT(*)
           INTO v_contador
           FROM pag_det_apvol
          WHERE folio = p_folio
            AND result_operacion = '02'
      
         IF( v_contador < 1 )THEN
            LET v_nombre_archivo_rechazos = "No se tienen registros rechazados para este folio."
         ELSE
            # se recupera el nombre del archivo de rechazos
            SELECT ruta_envio
              INTO v_ruta_envio
              FROM seg_modulo
             WHERE modulo_cod = "pag"

            LET v_folio_formato = p_folio USING "&&&&&&&&&"
            LET v_contador = LENGTH(v_nombre_archivo CLIPPED)
            LET v_nombre_archivo_rechazos = v_nombre_archivo[1,v_contador - 5] || "_" || v_folio_formato || "_rechazoFA.fort"
         END IF
         
         PRINTX v_total_regs,
                v_total_mxn,
                v_total_aivs,
                v_nombre_archivo_rechazos

END REPORT