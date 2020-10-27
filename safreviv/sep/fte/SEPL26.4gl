--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14/06/2012
--===============================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPL26                                                     #
#Objetivo        => Programa Lanzador de Generar Archivo de movimientos de     #
#                   ajustes al crédito batch contabilidad                      #
#Fecha Inicio    => Junio 15, 2012                                             #
################################################################################
DATABASE safre_viv
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_cad_ventana     STRING,
       v_ruta            LIKE seg_modulo.ruta_bin,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_movimientos     DYNAMIC ARRAY OF RECORD
         v_consecutivo   INTEGER,
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_invadido      LIKE sep_nss_expediente.nss,
         v_asociado      LIKE sep_nss_expediente.nss,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         v_monto         LIKE sep_batch_contabilidad.monto,
         v_estado_desc   LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_monto_total     LIKE sep_batch_contabilidad.monto,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       r_pid             LIKE bat_ctr_operacion.pid,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       v_folio           LIKE glo_folio.folio,
       v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo
       

MAIN

    LET p_usuario_cod   = ARG_VAL(1)
    LET p_tpo_ejecucion = ARG_VAL(2)
    LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_genera_archivo_ajustes()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Lanzador de de Generar Archivo de movimientos de         #
#                     ajustes al crédito batch contabilidad                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Junio 2012                                            #
################################################################################
FUNCTION fn_genera_archivo_ajustes()
DEFINE v_continua BOOLEAN,
       r_confirma BOOLEAN,
       v_consulta BOOLEAN

   
   LET v_proceso_cod = 2214
   LET v_opera_cod   = 1
   LET v_continua = FALSE

   CALL fn_rutas("sep") RETURNING v_ruta_ejecutable,v_ruta
   CALL fn_rutas("bat") RETURNING v_ruta,v_ruta_listados

   OPEN WINDOW vtna_genera_ajustes WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL261"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      DISPLAY ARRAY v_movimientos TO sr_movimientos.*
              ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY 
            # Recupera registros de ajustes
            CALL fn_recupera_registros() RETURNING v_consulta
            IF NOT(v_consulta)THEN
               CALL fn_mensaje(p_cad_ventana,"No se encontraron registros para ajustes al crédito","information")
               EXIT DISPLAY
            END IF

            DISPLAY v_movimientos.getLength() TO flbl_total_registros
            DISPLAY v_monto_total TO flbl_monto_total

         ON ACTION aceptar
            CALL fn_ventana_confirma(p_cad_ventana,"Confirmar Generar Archivo","question")
                   RETURNING r_confirma
            IF(r_confirma)THEN
               CALL fn_lanza_batch_ajustes() RETURNING v_continua
               IF(v_continua)THEN
                  CONTINUE DISPLAY
               ELSE
                  EXIT DISPLAY
               END IF
            ELSE
               CONTINUE DISPLAY
            END IF

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_genera_ajustes

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Lanzador de de Generar Archivo de movimientos de         #
#                     ajustes al crédito batch contabilidad                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Junio 2012                                            #
################################################################################
FUNCTION fn_recupera_registros()
DEFINE v_consulta STRING,
       v_ajustes RECORD
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         v_estado_desc   LIKE sep_estado_expediente.descripcion,
         v_monto         LIKE sep_batch_contabilidad.monto
       END RECORD,
       v_indice          INTEGER

   WHENEVER ERROR CONTINUE 
   LET v_indice = 1
   CALL v_movimientos.clear()
   LET v_monto_total = 0
   
   LET v_consulta = "\n SELECT nss",
                    "\n   FROM sep_nss_expediente",
                    "\n  WHERE id_expediente = ?",
                    "\n    AND tipo_nss = 1"
   PREPARE prp_rec_nss_inv FROM v_consulta

   LET v_consulta = "\n SELECT nss",
                    "\n   FROM sep_nss_expediente",
                    "\n  WHERE id_expediente = ?",
                    "\n    AND tipo_nss = 2"
   PREPARE prp_rec_nss_asc FROM v_consulta

   LET v_consulta = "\n SELECT exp.id_expediente,con.nss,",
                    "\n        edo.descripcion, SUM(con.monto)",
                    "\n   FROM sep_batch_contabilidad con JOIN sep_expediente exp",
                    "\n     ON con.id_expediente = exp.id_expediente",
                    "\n        LEFT OUTER JOIN sep_estado_expediente edo",
                    "\n     ON edo.estado = exp.estado",
                    "\n  WHERE exp.estado in (40,45,46,50) ",
                    --"\n  WHERE exp.estado = 50 ",
                    "\n    AND con.ind_envio = 0",
                    "\n  GROUP BY 1,2,3"
   PREPARE prp_recupera_ajustes FROM v_consulta
   DECLARE cur_recupera_ajustes CURSOR FOR prp_recupera_ajustes
   FOREACH cur_recupera_ajustes INTO v_ajustes.*
      LET v_movimientos[v_indice].v_consecutivo   = v_indice
      LET v_movimientos[v_indice].v_id_expediente = v_ajustes.v_id_expediente
      LET v_movimientos[v_indice].v_nss_ajuste    = v_ajustes.v_nss_ajuste 
      LET v_movimientos[v_indice].v_estado_desc   = v_ajustes.v_estado_desc
      LET v_movimientos[v_indice].v_monto         = v_ajustes.v_monto
      LET v_monto_total = v_monto_total + v_ajustes.v_monto
      # Recupera invadido del expediente
      EXECUTE prp_rec_nss_inv USING v_ajustes.v_id_expediente 
                               INTO v_movimientos[v_indice].v_invadido
      # Recupera asociado del expediente
      EXECUTE prp_rec_nss_asc USING v_ajustes.v_id_expediente 
                               INTO v_movimientos[v_indice].v_asociado

      LET v_indice = v_indice + 1
   END FOREACH
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY SQLCA.SQLCODE 
   END IF
   FREE cur_recupera_ajustes

   IF(v_movimientos.getLength() > 0)THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF 
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Lanzador de de Generar Archivo de movimientos de         #
#                     ajustes al crédito batch contabilidad                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Junio 2012                                            #
################################################################################
FUNCTION fn_lanza_batch_ajustes()
DEFINE v_comando   STRING,
       v_resultado SMALLINT,
       v_continuar BOOLEAN

   LET v_continuar = FALSE

   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING v_resultado
   # Se verifica si la operacion es valida
   IF(v_resultado <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_resultado)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF

   LET v_folio = 0
   LET v_nom_archivo = "NA"
   CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod) 
          RETURNING r_pid 
   CALL fn_inicializa_proceso(r_pid,v_proceso_cod,v_opera_cod,0,
                              "SEPL26",v_nom_archivo,p_usuario_cod)
              RETURNING v_resultado
   IF( v_resultado <> 0 ) THEN
      CALL fn_muestra_inc_operacion(v_resultado)
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   
   CALL fn_actualiza_opera_ini(r_pid,v_proceso_cod,v_opera_cod,v_folio,"SEPL26",
                               v_nom_archivo,p_usuario_cod) RETURNING v_resultado
   IF(v_resultado <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_resultado)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   
   LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPP26.42r ",
                                   p_usuario_cod CLIPPED, " ",r_pid, " ",
                                   v_proceso_cod," ",v_opera_cod," ",
                                   v_folio, " ",v_nom_archivo,
                   " 1> ", v_ruta_listados CLIPPED,
                   "/nohup:",r_pid USING "&&&&&",":",
                             v_proceso_cod USING "&&&&&",":",
                             v_opera_cod USING "&&&&&",
                    " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
   IF(STATUS)THEN
      LET v_continuar = TRUE
      CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la operación","about")
   ELSE
      CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
      LET v_continuar = FALSE
   END IF

   RETURN v_continuar  
END FUNCTION