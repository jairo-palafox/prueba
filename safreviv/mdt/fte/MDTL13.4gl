--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17-07-2012
--===============================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL13                                                        #
#Objetivo     => Programa lanzador del reverso de integración de instrucciones # 
#                con origen recurrente                                         #
#Fecha inicio => Julio 17, 2012                                                #
#Autor        => Hugo César Ramírez García                                     #
################################################################################
DATABASE safre_viv

DEFINE r_usuario_cod     LIKE seg_usuario.usuario_cod,
       r_tpo_ejecucion   SMALLINT,
       r_cad_ventana     STRING,
       v_ventana         ui.Window,
       v_detalle_folio   DYNAMIC ARRAY OF RECORD
        v_folio          LIKE mdt_solicitud_mandato.folio,
        v_fecha_lote     LIKE mdt_lote_mandato.f_proceso,
        v_tipo_operacion CHAR(12),--LIKE mdt_solicitud_mandato.tipo_operacion,
        v_estado         LIKE mdt_solicitud_mandato.estado,
        v_total_grupo    INTEGER
       END RECORD,
       v_folios DYNAMIC ARRAY OF RECORD
        v_folio        STRING,-- LIKE mdt_solicitud_mandato.lote,
        v_fecha_proceso   LIKE mdt_lote_mandato.f_proceso
       END RECORD,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       v_pid             LIKE bat_ctr_operacion.pid,
       p_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       p_aux             LIKE seg_modulo.ruta_bin,
       p_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_continua        BOOLEAN,
       v_total_registros INTEGER

MAIN

   LET r_usuario_cod   = ARG_VAL(1)
   LET r_tpo_ejecucion = ARG_VAL(2)
   LET r_cad_ventana   = ARG_VAL(3)

   CALL fn_consulta_lote_reverso()

END MAIN


################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL13                                                        #
#Objetivo     => Pantalla de consulta para el folio a reversar de validación   #
#                de instrrucciones con origen recurrente                       #
#Fecha inicio => Julio 17, 2012                                                #
#Autor        => Hugo César Ramírez García                                     #
################################################################################
FUNCTION fn_consulta_lote_reverso()
DEFINE v_lote_reversar  STRING,-- LIKE mdt_lote_mandato.lote,
       cb_lote          ui.ComboBox,
       v_indice         SMALLINT,       
       v_confirma       BOOLEAN,
       v_cadena_aux     STRING
       

   LET v_proceso_cod = 1307  # Reverso de Integración recurrente
   LET v_opera_cod   = 1
   
   # Recupera rutas
   CALL fn_rutas("mdt") RETURNING p_ruta_ejecutable,p_aux
   CALL fn_rutas("bat") RETURNING p_aux,p_ruta_listados

   OPEN WINDOW vtna_reverso_integracion_recurrente WITH FORM p_ruta_ejecutable CLIPPED||"/MDTL131"
      #Se asigna el titulo de la ventana
      IF(r_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(r_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(r_cad_ventana)
      END IF
      LET cb_lote = ui.ComboBox.forName("formonly.cb_lote_reverso")
      CALL fn_consulta_lote(cb_lote) RETURNING v_continua

      INPUT v_lote_reversar WITHOUT DEFAULTS FROM cb_lote_reverso ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT
            IF NOT(v_continua)THEN
               CALL DIALOG.setActionActive("aceptar",0)
            END IF

         ON CHANGE cb_lote_reverso
            # Se recupera la fecha para el respectivo lote            
            FOR v_indice = 1 TO v_folios.getLength()
               LET v_cadena_aux = v_folios[v_indice].v_folio CLIPPED||"-"||v_folios[v_indice].v_fecha_proceso CLIPPED
               IF(v_lote_reversar  = v_cadena_aux)THEN
                  EXIT FOR
               END IF
            END FOR
            IF(v_indice > v_folios.getLength())THEN
               CONTINUE INPUT
            END IF
            # Recupera el detalle del folio
            CALL fn_recupera_detalle_lote(v_folios[v_indice].v_folio,v_folios[v_indice].v_fecha_proceso)
            DISPLAY ARRAY v_detalle_folio TO sr_detalle_lote.*
               BEFORE ROW
                  DISPLAY v_detalle_folio.getLength() TO flbl_total
                  EXIT DISPLAY

            END DISPLAY

         ON ACTION aceptar
            # Se valida que haya informacion a procesar
            IF(v_indice > v_folios.getLength())THEN
               CALL fn_mensaje(r_cad_ventana,"No hay información para procesar","about")
               CONTINUE INPUT
            END IF
            CALL fn_ventana_confirma(r_cad_ventana,"Se reversará el folio: "||v_folios[v_indice].v_folio||" de fecha: "||v_folios[v_indice].v_fecha_proceso,"about")
                     RETURNING v_confirma
            IF(v_confirma)THEN
               CALL fn_incializa_proceso(v_folios[v_indice].v_folio,v_folios[v_indice].v_fecha_proceso)
               ACCEPT INPUT
            ELSE
               CONTINUE INPUT
            END IF

         ON ACTION cancelar
            EXIT INPUT

      END INPUT
   CLOSE WINDOW vtna_reverso_validacion_recurrente

END FUNCTION

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL13                                                        #
#Objetivo     => Funcion que recupera los folios a reversar                    #
#Fecha inicio => Julio 17, 2012                                                #
#Autor        => Hugo César Ramírez García                                     #
################################################################################
FUNCTION fn_consulta_lote(cb_lote)
DEFINE cb_lote     ui.ComboBox,
       v_folio     LIKE mdt_lote_mandato.folio,
       v_f_proceso LIKE mdt_lote_mandato.f_proceso,
       v_indice SMALLINT

   WHENEVER ERROR CONTINUE
   # Se recuperan los lotes posteriores a la ultima fecha de aplicacion de intrucciones
   {DECLARE cur_recupera_lotes CURSOR FOR SELECT DISTINCT lote, f_lote
                                            FROM mdt_lote_mandato
                                           WHERE f_procesof_mdt  > (SELECT MAX(f_liquida_pago)
                                                                      FROM mdt_ctr_aplica_mandato)}
   DECLARE cur_recupera_lotes CURSOR FOR SELECT DISTINCT folio, f_proceso
                                           FROM mdt_lote_mandato
                                          WHERE estado = 101                                          
                                            AND id_origen = 1
   LET v_indice = 1
   FOREACH cur_recupera_lotes INTO v_folio, v_f_proceso
      CALL cb_lote.addItem(v_folio CLIPPED||"-"||v_f_proceso CLIPPED,v_folio CLIPPED||"  -  "||v_f_proceso CLIPPED)
      # se recuperan todos los folios y sus fechas
      LET v_folios[v_indice].v_folio      = v_folio
      LET v_folios[v_indice].v_fecha_proceso = v_f_proceso
      LET v_indice = v_indice + 1
   END FOREACH   
   FREE cur_recupera_lotes
   IF(v_indice = 1)THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF
END FUNCTION

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL13                                                        #
#Objetivo     => Funcion que recupera el detalle del lote(folio)               #
#Fecha inicio => Julio 17, 2012                                                #
#Autor        => Hugo César Ramírez García                                     #
################################################################################
FUNCTION fn_recupera_detalle_lote(p_folio, p_f_proceso)
DEFINE v_consulta        STRING,
       p_folio           LIKE mdt_solicitud_mandato.folio,
       p_f_proceso       LIKE mdt_lote_mandato.f_proceso,
       v_detalle         RECORD
        v_folio          LIKE mdt_solicitud_mandato.folio,
        v_fecha_proceso  LIKE mdt_lote_mandato.f_proceso,
        v_tipo_operacion CHAR(12),--LIKE mdt_solicitud_mandato.tipo_operacion,
        v_estado         LIKE mdt_solicitud_mandato.estado,
        v_total_grupo    INTEGER
       END RECORD,
       v_indice          SMALLINT

   WHENEVER ERROR CONTINUE
   CALL v_detalle_folio.clear()
   LET v_indice = 1
   LET v_total_registros = 0

   LET v_consulta = "\n SELECT sol.folio, lot.f_proceso, CASE sol.tipo_operacion",
                    "\n                         WHEN 'A'",
                    "\n                            THEN 'ALTA'",
                    "\n                         WHEN 'B'",
                    "\n                            THEN 'BAJA'",
                    "\n                         WHEN 'R'",
                    "\n                            THEN 'REACTIVACIÓN'",
                    "\n                         WHEN 'M'",
                    "\n                            THEN 'MODIFICACIÓN'",
                    "\n                      END CASE,",
                    "\n        sol.estado, COUNT(*)",
                    "\n   FROM mdt_solicitud_mandato sol JOIN mdt_lote_mandato lot",
                    "\n     ON lot.folio = sol.folio",
                    "\n  WHERE sol.folio = ?",
                    --"\n    AND sol.f_lote = ?",
                    "\n    AND sol.id_origen = 1",
                    "\n  GROUP BY sol.folio, lot.f_proceso, sol.tipo_operacion, sol.estado",
                    "\n  ORDER BY 1,2,3,4"
   
   PREPARE prp_recupera_detalle_folio FROM v_consulta
   DECLARE cur_recupera_detalle_folio CURSOR FOR prp_recupera_detalle_folio
   {FOREACH cur_recupera_detalle_folio USING p_folio, p_f_proceso
                                      INTO v_detalle_folio[v_indice].*
      LET v_total_registros = v_total_registros + v_detalle_folio[v_indice].v_total_grupo 
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_detalle_folio
   IF(v_detalle_folio.getLength() > 0)THEN
      IF(LENGTH(v_detalle_folio[v_detalle_folio.getLength()].v_folio) = 0)THEN
         CALL v_detalle_folio.deleteElement(v_detalle_folio.getLength())
      END IF
   END IF}

   FOREACH cur_recupera_detalle_folio USING p_folio
                                      INTO v_detalle.*
      LET v_detalle_folio[v_indice].* = v_detalle.*
      LET v_total_registros = v_total_registros + v_detalle_folio[v_indice].v_total_grupo 
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_detalle_folio
   {IF(v_detalle_folio.getLength() > 0)THEN
      IF(LENGTH(v_detalle_folio[v_detalle_folio.getLength()].v_folio) = 0)THEN
         CALL v_detalle_folio.deleteElement(v_detalle_folio.getLength())
      END IF
   END IF}
   
END FUNCTION

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL13                                                        #
#Objetivo     => Funcion para lanzar la ejecucion del reverso                  #
#Fecha inicio => Julio 17, 2012                                                #
#Autor        => Hugo César Ramírez García                                     #
################################################################################
FUNCTION fn_incializa_proceso(v_folio,v_f_proceso)
DEFINE v_folio     LIKE mdt_lote_mandato.folio,
       v_f_proceso LIKE mdt_lote_mandato.f_proceso,
       v_comando   STRING 

   WHENEVER ERROR CONTINUE
   CALL fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) RETURNING v_continua
   IF(v_continua <> 0)THEN
      # Imprime en pantalla
      CALL fn_muestra_inc_operacion(v_continua)
      RETURN 0
   END IF
   #Genera identificador del proceso
   CALL fn_genera_pid(v_proceso_cod, v_opera_cod,r_usuario_cod)
                     RETURNING v_pid
   IF(SQLCA.SQLCODE <> 0)THEN
      CALL fn_mensaje(r_cad_ventana,"Ocurrió un error al generar PID","about")
      DISPLAY "Error al generar pid (Codigo):",SQLCA.SQLCODE
      RETURN 0         
   END IF
   #inicializa el proceso con todas sus operaciones en estado LISTO
   CALL fn_inicializa_proceso(v_pid, 
                              v_proceso_cod, 
                              v_opera_cod, 
                              v_folio,
                              "MDTL13",
                              "", 
                              r_usuario_cod) 
                     RETURNING v_continua                        
   IF(v_continua  <> 0)THEN
      # Imprime el mensaje de inconsistencia en pantalla
      CALL fn_muestra_inc_operacion(v_continua)
      RETURN 0
   END IF
   CALL fn_actualiza_opera_ini(v_pid,         # pid
                               v_proceso_cod, # proceso
                               v_opera_cod,   # operacion
                               v_folio,       # folio
                               "MDTL13",      # programa
                               "",            # archivo
                               r_usuario_cod)
                 RETURNING v_continua
   IF(v_continua  <> 0)THEN
      # Imprime el mensaje de inconsistencia en pantalla
      CALL fn_muestra_inc_operacion(v_continua)
      CALL fn_error_opera(v_pid,v_proceso_cod,v_opera_cod)
                         RETURNING v_continua
      RETURN 0
   END IF 

   
   #Construye comando
   LET v_comando = "nohup fglrun ",p_ruta_ejecutable CLIPPED,"/MDTR13.42r ",
                                   r_usuario_cod," ",
                                   v_pid," ",
                                   v_proceso_cod," ",
                                   v_opera_cod," ",
                                   v_folio," '",
                                   "NA",
                             "' 1>", p_ruta_listados CLIPPED ,
                          "/nohup:",v_pid USING "&&&&&",":",
                                    v_proceso_cod USING "&&&&&",":",
                                    v_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
   RUN v_comando

   IF(STATUS)THEN
      CALL fn_mensaje(r_cad_ventana,"Ocurrió un error al iniciar el proceso batch","about")
      CALL fn_error_opera(v_pid,v_proceso_cod,v_opera_cod)
                         RETURNING v_continua
      IF(v_continua)THEN
         # Imprime el mensaje de inconsistencia en consola y archivo
         CALL fn_muestra_inc_operacion(v_continua)
      END IF
   END IF
   CALL fn_mensaje(r_cad_ventana, 
                   "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                   "bn_about")
END FUNCTION