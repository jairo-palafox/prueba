--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20 Enero 2015
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL32                                                   #
#Objetivo          => Programa lanzador de solicitud de restitucion            #
#                     complementaria                                           # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################

DATABASE safre_viv

DEFINE r_pid             LIKE bat_ctr_proceso.pid,     -- ID del proceso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, -- Código del proceso
       v_opera_cod       LIKE cat_operacion.opera_cod, -- Código de operacion
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_lst        LIKE seg_modulo.ruta_bin,
       v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       v_folio           LIKE glo_folio.folio,
       v_ventana         ui.Window,
       p_cad_ventana     STRING,
       v_sol_restitucion DYNAMIC ARRAY OF RECORD
         v_confirmado    BOOLEAN,
         v_consecutivo   INTEGER,
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_invadido      LIKE sep_nss_expediente.nss,
         v_asociado      LIKE sep_nss_expediente.nss,
         v_nss           LIKE sep_batch_contabilidad.nss,
         v_sar92_avis    DECIMAL(22,2),
         v_sar92_pesos   DECIMAL(22,2),
         v_viv97_aivs    DECIMAL(22,2),
         v_viv97_pesos   DECIMAL(22,2),
         v_ap_avis       DECIMAL(22,2),
         v_ap_pesos      DECIMAL(22,2),
         v_tipo          STRING
       END RECORD,
       v_total_sar92_avis    DECIMAL(22,2),
       v_total_sar92_pesos   DECIMAL(22,2),
       v_total_viv97_aivs    DECIMAL(22,2),
       v_total_viv97_pesos   DECIMAL(22,2),
       v_total_ap_avis       DECIMAL(22,2),
       v_total_ap_pesos      DECIMAL(22,2)

MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_inicializa_consultas()
   CALL fn_despliega_inf_sol_restitucion()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL32                                                   #
#Descripcion       => Inicializa consultas                                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = "\n SELECT nss",
                    "\n   FROM sep_nss_expediente",
                    "\n  WHERE id_expediente = ?",
                    "\n    AND tipo_nss = 1"
   PREPARE prp_rec_invadido FROM v_consulta

   LET v_consulta = "\n SELECT nss",
                    "\n   FROM sep_nss_expediente",
                    "\n  WHERE id_expediente = ?",
                    "\n    AND tipo_nss = 2"
   PREPARE prp_rec_asociado FROM v_consulta

   LET v_consulta = "\n SELECT UNIQUE bat.id_expediente,",  
                    "\n        bat.nss,",  
                    "\n        mto.sar92_pesos_devolver,",
                    "\n        mto.sar92_aivs_devolver,",
                    "\n        mto.viv97_pesos_devolver,",
                    "\n        mto.viv97_aivs_devolver,",
                    "\n        mto.subsc_viv97_pesos_devolver,",
                    "\n        mto.subsc_viv97_aivs_devolver",
                    "\n   FROM sep_nss_expediente bat JOIN sep_mto_restitucion_analisis mto",
                    "\n     ON mto.id_expediente = bat.id_expediente",
                    "\n    AND bat.tipo_nss = 1",    
                    "\n    AND mto.ind_restitucion = 0",
                    "\n  GROUP BY 1,2,3,4,5,6,7,8"

   PREPARE prp_rec_res_analisis FROM v_consulta

   LET v_consulta = "\n SELECT UNIQUE res115.id_expediente,",
                    "\n        exp.nss,",
                    "\n        res115.sar92_pesos_115_trabajador ,",
                    "\n        res115.sar92_aivs_115_trabajador ,",
                    "\n        res115.viv97_pesos_115_trabajador ,",
                    "\n        res115.viv97_aivs_115_trabajador ,",
                    "\n        res115.subsc_viv97_pesos_115_trabajador ,",
                    "\n        res115.subsc_viv97_aivs_115_trabajador ",
                    "\n   FROM sep_115_restitucion res115, ",
                    "\n        sep_nss_expediente exp ",                    
                    "\n  WHERE res115.ind_restitucion = 0",
                    "\n    AND res115.id_expediente = exp.id_expediente ", 
                    "\n    AND exp.tipo_nss = 2 ",                    
                    "\n  GROUP BY 1,2,3,4,5,6,7,8 ",
                    
                    "\n  UNION ",                    
                    
                    "\n SELECT UNIQUE res115.id_expediente,",
                    "\n        exp.nss,",
                    "\n        res115.sar92_pesos_115_acreditado,",
                    "\n        res115.sar92_aivs_115_acreditado,",
                    "\n        res115.viv97_pesos_115_acreditado,",
                    "\n        res115.viv97_aivs_115_acreditado,",
                    "\n        res115.subsc_viv97_pesos_115_acreditado,",
                    "\n        res115.subsc_viv97_aivs_115_acreditado ",
                    "\n   FROM sep_115_restitucion res115, ",
                    "\n        sep_nss_expediente exp ",
                    "\n   WHERE res115.ind_restitucion = 0",
                    "\n     AND res115.id_expediente = exp.id_expediente ",
                    "\n     AND exp.tipo_nss = 1 ",                    
                    "\n    GROUP BY 1,2,3,4,5,6,7,8 " 

   PREPARE prp_rec_res_115 FROM v_consulta

   # NO aplican sep_mto_restitucion_no_aplicados para las complementarias
   {LET v_consulta = "\n SELECT UNIQUE bat.id_expediente,",
                    "\n        bat.nss,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        apl.subsc_viv97_pesos_no_aplica,",
                    "\n        apl.subsc_viv97_aivs_no_aplica",
                    "\n   FROM sep_nss_expediente bat JOIN sep_mto_restitucion_no_aplicados apl",
                    "\n     ON apl.id_expediente = bat.id_expediente",
                    "\n    AND bat.tipo_nss = 1",
                    "\n    AND apl.subsc_viv97_aivs_no_aplica > 0 ",                    
                    "\n   WHERE apl.ind_restitucion = 0"

   PREPARE prp_rec_res_no_aplicados FROM v_consulta}

   {LET v_consulta = "\n SELECT UNIQUE bat.id_expediente,",
                    "\n        bat.nss,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        apl.subsc_viv97_pesos_no_aplica_pscd,",
                    "\n        apl.subsc_viv97_aivs_no_aplica_pscd ",
                    "\n   FROM sep_nss_expediente bat JOIN sep_mto_restitucion_no_aplicados apl",
                    "\n     ON apl.id_expediente = bat.id_expediente",
                    "\n    AND bat.tipo_nss = 1",
                    "\n    AND apl.subsc_viv97_aivs_no_aplica_pscd > 0",
                    "\n   WHERE apl.ind_restitucion = 0"

   PREPARE prp_rec_res_no_aplicados_pscd FROM v_consulta}

   LET v_consulta = "\n UPDATE sep_115_restitucion",
                    "\n    SET ind_restitucion = 1", # RESTITUCION SOLICITADA
                    "\n  WHERE id_expediente = ?" ,
                    "\n    AND ind_restitucion = 0"
   PREPARE prp_actualiza_115_solicitada FROM v_consulta

   LET v_consulta = "\n UPDATE sep_mto_restitucion_analisis",
                    "\n    SET ind_restitucion = 1", # RESTITUCION SOLICITADA
                    "\n  WHERE id_expediente = ?",
                    "\n    AND ind_restitucion = 0" 
   PREPARE prp_actualiza_als_solicitada FROM v_consulta

   LET v_consulta = "\n UPDATE sep_expediente",
                    "\n    SET ind_restitucion_complementario_1 = 2", # archivo confirmado
                    "\n  WHERE id_expediente = ?",
                    "\n    AND ind_restitucion_complementario_1 = 1"
   PREPARE prp_actualiza_exp_compl_1 FROM v_consulta

   LET v_consulta = "\n UPDATE sep_expediente",
                    "\n    SET ind_restitucion_complementario_2 = 2", # archivo confirmado
                    "\n  WHERE id_expediente = ?",
                    "\n    AND ind_restitucion_complementario_2 = 1"
   PREPARE prp_actualiza_exp_compl_2 FROM v_consulta

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL32                                                   #
#Descripcion       => Pantalla lanzador de restitución complementaria          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_despliega_inf_sol_restitucion()
DEFINE v_ruta            LIKE seg_modulo.ruta_bin,
       r_resultado_opera SMALLINT,
       v_continua  BOOLEAN,
       r_confirma  BOOLEAN,
       v_comando   STRING,
       v_contador  SMALLINT

   #Se dan las variables de proceso y operacion
   LET v_proceso_cod = 2215  # Generacion de solicitudes de restitucion
   LET v_opera_cod   = 1 # 
   LET v_folio = 0
   LET v_nom_archivo = " "

   CALL fn_rutas("sep") RETURNING v_ruta_ejecutable,v_ruta
   CALL fn_rutas("bat") RETURNING v_ruta,v_ruta_lst

   OPEN WINDOW vtna_sol_restitucion WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL321"

      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      # Verifica si se puede iniciar el proceso      
      CALL fn_valida_operacion(0,
                               v_proceso_cod,
                               v_opera_cod) RETURNING r_resultado_opera
                               
      IF(r_resultado_opera = 0)THEN
         CALL fn_recupera_sol_restitucion() RETURNING v_continua
         
         IF(v_continua)THEN
            INPUT ARRAY v_sol_restitucion WITHOUT DEFAULTS 
                   FROM sr_solicitudes_restitucion.*
               ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE,UNBUFFERED)

               BEFORE INPUT
                  CALL DIALOG.setActionHidden("append",TRUE)                  
                  DISPLAY v_sol_restitucion.getLength() TO flbl_total_registros
                  DISPLAY v_total_sar92_avis  USING "###,###,##&.&&" TO flbl_aivs_sar92
                  DISPLAY v_total_sar92_pesos USING "###,###,##&.&&" TO flbl_monto_sar92
                  DISPLAY v_total_viv97_aivs  USING "###,###,##&.&&" TO flbl_aivs_viv97
                  DISPLAY v_total_viv97_pesos USING "###,###,##&.&&" TO flbl_monto_viv97
                  DISPLAY v_total_ap_avis     USING "###,###,##&.&&" TO flbl_aivs_ap
                  DISPLAY v_total_ap_pesos    USING "###,###,##&.&&" TO flbl_monto_ap

               ON CHANGE confirmar
                  IF(v_sol_restitucion[ARR_CURR()].v_confirmado)THEN
                     CALL fn_selecciona_registros_expediente(v_sol_restitucion[ARR_CURR()].v_id_expediente,
                                                             TRUE) # selecciona los registros
                  ELSE
                     CALL fn_selecciona_registros_expediente(v_sol_restitucion[ARR_CURR()].v_id_expediente,
                                                             FALSE) # quita selección a los registros
                  END IF

               ON ACTION aceptar
                   # Valida se haya confirmado almenos un registro
                  LET v_continua = FALSE
                  FOR v_contador = 1 TO v_sol_restitucion.getLength()
                    IF(v_sol_restitucion[v_contador].v_confirmado)THEN
                       LET v_continua = TRUE
                       EXIT FOR
                    END IF
                  END FOR
                  IF NOT( v_continua )THEN
                     CALL fn_mensaje(p_cad_ventana,"Debe confirmar al menos un registro","about")
                     CONTINUE INPUT
                  END IF
                  #Se verifica si se puede iniciar el proceso      
                  CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) 
                            RETURNING r_resultado_opera
                  IF(r_resultado_opera <> 0)THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                     CONTINUE INPUT
                  END IF
                  CALL fn_ventana_confirma(p_cad_ventana,"¿Desea generar solicitud(es) de Restitución al SSV Complementaria seleccionada(s)?","question")
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     CALL fn_genera_pid(v_proceso_cod,
                                        v_opera_cod,
                                        p_usuario_cod) RETURNING r_pid

                     CALL fn_inicializa_proceso(r_pid,
                                                v_proceso_cod,
                                                v_opera_cod,
                                                v_folio,
                                                "SEPL32",
                                                v_nom_archivo,
                                                p_usuario_cod) RETURNING r_resultado_opera
                     IF(r_resultado_opera <> 0)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                        CONTINUE INPUT
                     END IF

                     CALL fn_actualiza_opera_ini(r_pid,
                                                 v_proceso_cod,
                                                 v_opera_cod,
                                                 v_folio,
                                                 "SEPL32",
                                                 v_nom_archivo,
                                                 p_usuario_cod) RETURNING r_resultado_opera
                     IF(r_resultado_opera <> 0)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                        CONTINUE INPUT
                     END IF                            
                     # Función para actualizar a solicitadas los regitros de restitución complemntarias y
                     # que serán porcesadas para devolver
                     CALL fn_actualiza_confirmado_complementarias()

                     LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPP28.42r ",
                                                     p_usuario_cod, " ",
                                                     r_pid, " ",
                                                     v_proceso_cod," ",
                                                     v_opera_cod," ",
                                                     v_folio, " ",
                                                     v_nom_archivo,
                                     " 1>", v_ruta_lst CLIPPED,
                                     "/nohup:",r_pid USING "&&&&&",":",
                                               v_proceso_cod USING "&&&&&",":",
                                               v_opera_cod USING "&&&&&",
                                     " 2>&1 &"
                  
                     DISPLAY v_comando
                     RUN v_comando
                     IF(STATUS)THEN
                        CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar generación de solicitudes de \nrestitución complementaria","about")
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                     END IF
                     ACCEPT INPUT
                  ELSE
                     CONTINUE INPUT
                  END IF


               ON ACTION cancelar
                  EXIT INPUT

            END INPUT 

         ELSE
            CALL fn_mensaje(p_cad_ventana,"No se encontraron registros para solicitudes de restitución complementaria","information")
         END IF
      ELSE
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      END IF

   CLOSE WINDOW vtna_sol_restitucion

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL32                                                   #
#Descripcion       => Recupera solicitudes de restitucion complementarias      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_recupera_sol_restitucion()
DEFINE v_sol_restitucion_aux RECORD
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_nss           LIKE sep_batch_contabilidad.nss,
         v_sar92_pesos   DECIMAL(22,2),
         v_sar92_avis    DECIMAL(22,2),
         v_viv97_pesos   DECIMAL(22,2),
         v_viv97_aivs    DECIMAL(22,2),
         v_ap_pesos      DECIMAL(22,2),
         v_ap_avis       DECIMAL(22,2)
       END RECORD,
       v_invadido LIKE sep_nss_expediente.nss,
       v_asociado LIKE sep_nss_expediente.nss,
       v_indice   INTEGER

   LET v_total_sar92_avis  = 0
   LET v_total_sar92_pesos = 0
   LET v_total_viv97_aivs  = 0
   LET v_total_viv97_pesos = 0
   LET v_total_ap_avis     = 0
   LET v_total_ap_pesos    = 0   
   CALL v_sol_restitucion.clear()

   ### 1   Recupera montos de analisis
   
   LET v_indice = 1
   DECLARE cur_rec_res_analisis CURSOR FOR prp_rec_res_analisis
   FOREACH cur_rec_res_analisis INTO v_sol_restitucion_aux.*

      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado
      
      LET v_sol_restitucion[v_indice].v_confirmado    = 0 # sin confirmar
      LET v_sol_restitucion[v_indice].v_consecutivo   = v_indice
      LET v_sol_restitucion[v_indice].v_id_expediente = v_sol_restitucion_aux.v_id_expediente
      LET v_sol_restitucion[v_indice].v_invadido      = v_invadido
      LET v_sol_restitucion[v_indice].v_asociado      = v_asociado
      LET v_sol_restitucion[v_indice].v_nss           = v_sol_restitucion_aux.v_nss
      LET v_sol_restitucion[v_indice].v_sar92_avis    = v_sol_restitucion_aux.v_sar92_avis
      LET v_sol_restitucion[v_indice].v_sar92_pesos   = v_sol_restitucion_aux.v_sar92_pesos
      LET v_sol_restitucion[v_indice].v_viv97_aivs    = v_sol_restitucion_aux.v_viv97_aivs
      LET v_sol_restitucion[v_indice].v_viv97_pesos   = v_sol_restitucion_aux.v_viv97_pesos
      LET v_sol_restitucion[v_indice].v_ap_avis       = v_sol_restitucion_aux.v_ap_avis
      LET v_sol_restitucion[v_indice].v_ap_pesos      = v_sol_restitucion_aux.v_ap_pesos
      LET v_sol_restitucion[v_indice].v_tipo          = "ANÁLISIS"

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,##&.&&" 
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,##&.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,##&.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,##&.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,##&.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,##&.&&"

      LET v_indice = v_indice + 1

   END FOREACH  

   ### 2  recupera los montos de restitucion 115
   
   DECLARE cur_rec_res_115 CURSOR FOR prp_rec_res_115
   FOREACH cur_rec_res_115 INTO v_sol_restitucion_aux.*

      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado

      DISPLAY "------>2"
      LET v_sol_restitucion[v_indice].v_confirmado    = 0 # sin confirmar
      LET v_sol_restitucion[v_indice].v_consecutivo   = v_indice
      LET v_sol_restitucion[v_indice].v_id_expediente = v_sol_restitucion_aux.v_id_expediente
      LET v_sol_restitucion[v_indice].v_invadido      = v_invadido
      LET v_sol_restitucion[v_indice].v_asociado      = v_asociado
      LET v_sol_restitucion[v_indice].v_nss           = v_sol_restitucion_aux.v_nss
      LET v_sol_restitucion[v_indice].v_sar92_avis    = v_sol_restitucion_aux.v_sar92_avis
      LET v_sol_restitucion[v_indice].v_sar92_pesos   = v_sol_restitucion_aux.v_sar92_pesos
      LET v_sol_restitucion[v_indice].v_viv97_aivs    = v_sol_restitucion_aux.v_viv97_aivs
      LET v_sol_restitucion[v_indice].v_viv97_pesos   = v_sol_restitucion_aux.v_viv97_pesos
      LET v_sol_restitucion[v_indice].v_ap_avis       = v_sol_restitucion_aux.v_ap_avis
      LET v_sol_restitucion[v_indice].v_ap_pesos      = v_sol_restitucion_aux.v_ap_pesos
      LET v_sol_restitucion[v_indice].v_tipo          = "BASE 115 - 28"

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,##&.&&"  
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,##&.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,##&.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,##&.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,##&.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,##&.&&" 

      LET v_indice = v_indice + 1
                                
   END FOREACH 

   
   ### 3   recupera los montos no aplicados antes pscd
   {
   DECLARE cur_rec_res_no_aplicados CURSOR FOR prp_rec_res_no_aplicados 
   FOREACH cur_rec_res_no_aplicados INTO v_sol_restitucion_aux.*

      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado

      DISPLAY "------>3"
      LET v_sol_restitucion[v_indice].v_confirmado    = 0 # sin confirmar
      LET v_sol_restitucion[v_indice].v_consecutivo   = v_indice
      LET v_sol_restitucion[v_indice].v_id_expediente = v_sol_restitucion_aux.v_id_expediente
      LET v_sol_restitucion[v_indice].v_invadido      = v_invadido
      LET v_sol_restitucion[v_indice].v_asociado      = v_asociado
      LET v_sol_restitucion[v_indice].v_nss           = v_sol_restitucion_aux.v_nss
      LET v_sol_restitucion[v_indice].v_sar92_avis    = v_sol_restitucion_aux.v_sar92_avis
      LET v_sol_restitucion[v_indice].v_sar92_pesos   = v_sol_restitucion_aux.v_sar92_pesos
      LET v_sol_restitucion[v_indice].v_viv97_aivs    = v_sol_restitucion_aux.v_viv97_aivs
      LET v_sol_restitucion[v_indice].v_viv97_pesos   = v_sol_restitucion_aux.v_viv97_pesos
      LET v_sol_restitucion[v_indice].v_ap_avis       = v_sol_restitucion_aux.v_ap_avis
      LET v_sol_restitucion[v_indice].v_ap_pesos      = v_sol_restitucion_aux.v_ap_pesos
      LET v_sol_restitucion[v_indice].v_tipo          = "NO APLICADOS AL CRÉDITO"

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,##&.&&" 
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,##&.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,##&.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,##&.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,##&.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,##&.&&" 

      LET v_indice = v_indice + 1

   END FOREACH
}
   ### 4   recupera los montos no aplicados pscd
{
   DECLARE cur_rec_res_no_aplicados_pscd CURSOR FOR prp_rec_res_no_aplicados_pscd 
   FOREACH cur_rec_res_no_aplicados_pscd INTO v_sol_restitucion_aux.*

      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado

      DISPLAY "------>4"
      LET v_sol_restitucion[v_indice].v_confirmado    = 0 # sin confirmar
      LET v_sol_restitucion[v_indice].v_consecutivo   = v_indice
      LET v_sol_restitucion[v_indice].v_id_expediente = v_sol_restitucion_aux.v_id_expediente
      LET v_sol_restitucion[v_indice].v_invadido      = v_invadido
      LET v_sol_restitucion[v_indice].v_asociado      = v_asociado
      LET v_sol_restitucion[v_indice].v_nss           = v_sol_restitucion_aux.v_nss
      LET v_sol_restitucion[v_indice].v_sar92_avis    = v_sol_restitucion_aux.v_sar92_avis
      LET v_sol_restitucion[v_indice].v_sar92_pesos   = v_sol_restitucion_aux.v_sar92_pesos
      LET v_sol_restitucion[v_indice].v_viv97_aivs    = v_sol_restitucion_aux.v_viv97_aivs
      LET v_sol_restitucion[v_indice].v_viv97_pesos   = v_sol_restitucion_aux.v_viv97_pesos
      LET v_sol_restitucion[v_indice].v_ap_avis       = v_sol_restitucion_aux.v_ap_avis
      LET v_sol_restitucion[v_indice].v_ap_pesos      = v_sol_restitucion_aux.v_ap_pesos
      LET v_sol_restitucion[v_indice].v_tipo          = "NO APLICADOS AL CRÉDITO PSCD"

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,##&.&&" 
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,##&.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,##&.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,##&.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,##&.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,##&.&&" 

      LET v_indice = v_indice + 1

   END FOREACH
}
   IF(v_indice > 1)THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL32                                                   #
#Descripcion       => Actualiza el estado de los registros de solicitudes de   #
#                     de restitución complementarias para ser procesadas       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_actualiza_confirmado_complementarias()
DEFINE v_contador SMALLINT

   FOR v_contador = 1 TO v_sol_restitucion.getLength()

      IF(v_sol_restitucion[v_contador].v_confirmado)THEN
         EXECUTE prp_actualiza_115_solicitada USING v_sol_restitucion[v_contador].v_id_expediente 

         EXECUTE prp_actualiza_als_solicitada USING v_sol_restitucion[v_contador].v_id_expediente

         # Actualiza el estado del documento de complementarias, sólo se actulizara una de las dos complementarias, ya que los indicadores
         # sólo permitiran una de las dos
         EXECUTE prp_actualiza_exp_compl_1 USING v_sol_restitucion[v_contador].v_id_expediente
         
         EXECUTE prp_actualiza_exp_compl_2 USING v_sol_restitucion[v_contador].v_id_expediente
         
      END IF

   END FOR  
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL32                                                   #
#Descripcion       => Selecciona o quita selecciona a todos los registros      #
#                     relacionados al mismo expediente                         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_selecciona_registros_expediente(p_id_expediente,p_selecciona)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente,
       p_selecciona    SMALLINT,
       v_indice        SMALLINT

       
   FOR v_indice = 1 TO v_sol_restitucion.getLength()
      IF(p_selecciona)THEN         
         IF(v_sol_restitucion[v_indice].v_id_expediente = p_id_expediente)THEN
            LET v_sol_restitucion[v_indice].v_confirmado = 1 # selecciona registro
         END IF
      ELSE
         IF(v_sol_restitucion[v_indice].v_id_expediente = p_id_expediente)THEN
            LET v_sol_restitucion[v_indice].v_confirmado = 0 # quita seleción
         END IF
      END IF
   END FOR

END FUNCTION