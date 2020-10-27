--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-06-2013
--==============================================================================

################################################################################
#Modulo       => pag                                                           #
#Programa     => PAGC66                                                        #
#Objetivo     => Consulta  de nss aperturados vol                              #
#Fecha inicio => 10 junio 2013                                                 # 
#Autor        => Hugo Ramírez                                                  #
#Modificaiones                                                                 #
################################################################################
DATABASE safre_viv

DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion  SMALLINT, # forma como ejecutara el programa
       p_titulo_vtna     STRING,   # titulo de la ventana
       v_cuentas_aperturadas DYNAMIC ARRAY OF RECORD
         v_nss         LIKE afi_derechohabiente.nss,
         v_f_apertura  STRING,--LIKE afi_derechohabiente.f_apertura,
         v_nombre_imss LIKE afi_derechohabiente.nombre_imss,
         v_folio_lote  LIKE afi_derechohabiente.folio_lote
       END RECORD


MAIN

   # se recupera la clave de usuario desde parametro 
   # argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)


   SELECT ruta_bin
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "pag"
   
   # consulta de informacion
   CALL fn_consulta_aperturados()

END MAIN

{===============================================================================
Nombre: fn_consulta_registros
Fecha creacion: 10 junio 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 
Registro de modificaciones:
Autor            Fecha                 Descripcion

================================================================================}
FUNCTION fn_consulta_aperturados()
DEFINE v_ventana   ui.WINDOW,
       v_folio     LIKE afi_derechohabiente.folio_lote,
       v_proceso   LIKE cat_proceso.proceso_cod,
       v_continuar BOOLEAN,
       v_combo_proceso ui.ComboBox

   LET v_continuar = TRUE
   # se abre la ventana para captura de parametros
   OPEN WINDOW w_consulta_registros WITH FORM v_ruta_ejecutable CLIPPED||"/PAGC661"
      LET  v_ventana = UI.WINDOW.GETCURRENT()
      CALL v_ventana.SETTEXT(p_titulo_vtna)
      LET v_combo_proceso = ui.ComboBox.forName("combo_proceso")
      # si se obtuvo el titulo, se pone como titulo de programa
      IF ( p_titulo_vtna IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_titulo_vtna)
      END IF
      WHILE v_continuar
         INPUT v_folio, v_proceso FROM folio, combo_proceso ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS, ACCEPT = FALSE, CANCEL = FALSE)
            BEFORE INPUT 
               CALL fn_llena_combo_proceso(v_combo_proceso)

            ON ACTION aceptar
               IF ( v_folio IS NULL ) THEN
         	      CALL fn_mensaje("Aviso", "Debe de ingresar un folio","information")
                  NEXT FIELD folio
               END IF
               IF ( v_proceso IS NULL ) THEN
         	      CALL fn_mensaje("Aviso", "Debe de ingresar un proceso","information")
                  NEXT FIELD combo_proceso
               END IF
               DISPLAY "fn_recupera_cuentas ",  v_folio,"  ", v_proceso
               CALL fn_recupera_cuentas(v_folio, v_proceso) RETURNING v_continuar
               IF(v_continuar)THEN
               DISPLAY "lo que regresa la funcion recupera cuentas ", v_continuar
                  ACCEPT INPUT
               ELSE
                  CALL fn_mensaje("Aviso", "No se encontraron registros con criterio dado","information")
                  CONTINUE INPUT
               END IF
            

            ON ACTION cancelar
               LET v_continuar = FALSE
               EXIT INPUT

         END INPUT
         IF(v_continuar)THEN
            DISPLAY ARRAY v_cuentas_aperturadas TO sr_cuentas_aperturadas.* ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

               ON ACTION reporte
                  CALL fn_genera_detalle_rpt(v_folio)

               ON ACTION cancelar
                  CALL v_cuentas_aperturadas.clear()
                  EXIT DISPLAY

            END DISPLAY 

         END IF
      END WHILE

   CLOSE WINDOW w_consulta_registros
END FUNCTION

{===============================================================================
Nombre: fn_consulta_registros
Fecha creacion: 10 junio 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 
Registro de modificaciones:
Autor            Fecha                 Descripcion

================================================================================}
FUNCTION fn_llena_combo_proceso(p_combo_proceso)
DEFINE p_combo_proceso ui.ComboBox,
       v_consulta      STRING,
       v_proceso RECORD
        v_proceso_cod LIKE cat_proceso.proceso_cod,
        v_descripcion LIKE cat_proceso.proceso_desc
       END RECORD,
       v_cadena_aux STRING

   CALL p_combo_proceso.clear()
   LET v_consulta = "\n SELECT proceso_cod ,",
                    "\n        proceso_desc",
                    "\n   FROM cat_proceso",
                    "\n  WHERE modulo_cod = 'pag'"
   PREPARE prp_recupera_procesos FROM v_consulta
   DECLARE cur_recupera_procesos CURSOR FOR prp_recupera_procesos
   FOREACH cur_recupera_procesos INTO v_proceso.*
      LET v_cadena_aux = v_proceso.v_descripcion CLIPPED
      CALL p_combo_proceso.addItem(v_proceso.v_proceso_cod,v_cadena_aux)
   END FOREACH
   FREE cur_recupera_procesos

END FUNCTION
{===============================================================================
Nombre: fn_consulta_registros
Fecha creacion: 10 junio 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 
Registro de modificaciones:
Autor            Fecha                 Descripcion

================================================================================}
FUNCTION fn_recupera_cuentas(p_folio,p_proceso)
DEFINE p_folio    LIKE afi_derechohabiente.folio_lote,
       p_proceso   LIKE cat_proceso.proceso_cod,
       v_consulta STRING,
       v_cuenta   RECORD
         v_nss         LIKE afi_derechohabiente.nss,
         v_f_apertura  LIKE afi_derechohabiente.f_apertura,
         v_nombre_imss LIKE afi_derechohabiente.nombre_imss,
         v_folio_lote  LIKE afi_derechohabiente.folio_lote
       END RECORD,
       v_indice INTEGER,
       v_recupero BOOLEAN

   LET v_indice = 1
   LET v_recupero = FALSE
   CALL v_cuentas_aperturadas.clear()
   LET v_consulta = "\n SELECT afi.nss,",
                    "\n        afi.f_apertura,",
                    "\n        afi.nombre_imss,",
                    "\n        afi.folio_lote",
                    "\n   FROM afi_derechohabiente afi JOIN glo_folio glo",
                    "\n     ON glo.proceso_cod = ?",
                    "\n    AND afi.folio_lote = glo.folio",
                    "\n  WHERE afi.folio_lote = ?"
 
   PREPARE prp_recupera_cuentas FROM v_consulta
   DECLARE cur_recupera_cuentas CURSOR FOR prp_recupera_cuentas
   FOREACH cur_recupera_cuentas USING p_proceso,
                                      p_folio
                                 INTO v_cuenta.*

      LET v_recupero = TRUE
      LET v_cuentas_aperturadas[v_indice].v_nss         = v_cuenta.v_nss
      LET v_cuentas_aperturadas[v_indice].v_f_apertura  = v_cuenta.v_f_apertura USING "dd-mm-yyyy"
      LET v_cuentas_aperturadas[v_indice].v_nombre_imss = v_cuenta.v_nombre_imss
      LET v_cuentas_aperturadas[v_indice].v_folio_lote  = v_cuenta.v_folio_lote
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_cuentas


   
   RETURN v_recupero
END FUNCTION

{===============================================================================
Nombre: fn_genera_detalle_rpt
Fecha creacion: 10 junio 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Define y genera atributos de reporte
Registro de modificaciones:
Autor            Fecha                 Descripcion

================================================================================}
FUNCTION fn_genera_detalle_rpt(p_folio)
DEFINE p_folio          LIKE afi_derechohabiente.folio_lote,
       v_manejador_rpt  om.SaxDocumentHandler,
       v_nombre_reporte STRING,
       v_indice         INTEGER
       
   # se indica que se usara la plantilla
   IF ( fgl_report_loadCurrentSettings("PAGC66.4rp") ) THEN
      CALL fgl_report_selectDevice ("PDF")
      LET v_nombre_reporte = v_ruta_listados CLIPPED,"/","cuentas_vol"
      CALL fgl_report_setOutputFileName(v_nombre_reporte)
      CALL fgl_report_selectPreview(1)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      # se inicia la emision del reporte
      START REPORT rpt_genera_reporte_cuentas_vol TO XML HANDLER v_manejador_rpt
         -- se transfieren los datos
         FOR v_indice = 1 TO v_cuentas_aperturadas.getLength()         
            OUTPUT TO REPORT rpt_genera_reporte_cuentas_vol(p_folio, v_cuentas_aperturadas[v_indice].*)
         END FOR            
      FINISH REPORT rpt_genera_reporte_cuentas_vol
   ELSE
      CALL fn_mensaje("Aviso","No se encuentra la plantilla PAGC66.4rp. No se puede emitir el reporte","information")
   END IF
   
END FUNCTION

{===============================================================================
Nombre: rpt_genera_reporte_cuentas_vol
Fecha creacion: 10 Junio 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 Genera el reporte de cuentas aperturadas de aportaciones voluntarias

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
REPORT rpt_genera_reporte_cuentas_vol(p_folio, p_cuenta)
DEFINE p_folio         LIKE glo_folio.folio, # folio
       p_cuenta   RECORD
         v_nss         LIKE afi_derechohabiente.nss,
         v_f_apertura  STRING,--LIKE afi_derechohabiente.f_apertura,
         v_nombre_imss LIKE afi_derechohabiente.nombre_imss,
         v_folio_lote  LIKE afi_derechohabiente.folio_lote
       END RECORD,
       v_fecha                    STRING, # fecha de emision del reporte
       v_nombre_usuario           VARCHAR(100),
       v_nombre_archivo           VARCHAR(40),
       # variables para el totalizador
       v_total_regs               DECIMAL(12,2)       

   FORMAT

      FIRST PAGE HEADER
         # se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         # se obtiene el nombre del usuario
         SELECT usuario_desc
           INTO v_nombre_usuario
           FROM seg_usuario
          WHERE usuario_cod = p_usuario_cod
      
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
                  
         PRINTX p_folio, 
                p_usuario_cod, 
                v_fecha, 
                v_nombre_usuario, 
                v_nombre_archivo

         
      ON EVERY ROW
         PRINTX p_cuenta.*
         LET v_total_regs  = v_total_regs + 1

      ON LAST ROW                  
         PRINTX v_total_regs

END REPORT