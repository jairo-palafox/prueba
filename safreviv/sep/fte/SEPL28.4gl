--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23-06-2012
--===============================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL28                                                   #
#Objetivo          => Programa lanzador de solicitud de restituciones          # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 25, 2012                                           #
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

   CALL fn_despliega_inf_sol_restitucion()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL28                                                   #
#Descripcion       => Pantalla lanzador de sol restitucion                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 25, 2012                                           #
################################################################################
FUNCTION fn_despliega_inf_sol_restitucion()
DEFINE v_ruta            LIKE seg_modulo.ruta_bin,
       r_resultado_opera SMALLINT,
       v_continua  BOOLEAN,
       r_confirma  BOOLEAN,
       v_comando   STRING

   #Se dan las variables de proceso y operacion
   LET v_proceso_cod = 2215  # Generacion de solicitudes de restitucion
   LET v_opera_cod   = 1 # 
   LET v_folio = 0
   LET v_nom_archivo = " "

   CALL fn_rutas("sep") RETURNING v_ruta_ejecutable,v_ruta
   CALL fn_rutas("bat") RETURNING v_ruta,v_ruta_lst

   OPEN WINDOW vtna_sol_restitucion WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL281"

      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      #Se verifica si se puede iniciar el proceso      
      CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) 
           RETURNING r_resultado_opera
      IF(r_resultado_opera = 0)THEN
         CALL fn_recupera_sol_restitucion() RETURNING v_continua
         IF(v_continua)THEN
            DISPLAY ARRAY v_sol_restitucion TO sr_solicitudes_restitucion.*
               ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

               BEFORE DISPLAY 
                  DISPLAY v_sol_restitucion.getLength() TO flbl_total_registros
                  DISPLAY v_total_sar92_avis  USING "###,###,###.&&" TO flbl_aivs_sar92
                  DISPLAY v_total_sar92_pesos USING "###,###,###.&&" TO flbl_monto_sar92
                  DISPLAY v_total_viv97_aivs  USING "###,###,###.&&" TO flbl_aivs_viv97
                  DISPLAY v_total_viv97_pesos USING "###,###,###.&&" TO flbl_monto_viv97
                  DISPLAY v_total_ap_avis     USING "###,###,###.&&" TO flbl_aivs_ap
                  DISPLAY v_total_ap_pesos    USING "###,###,###.&&" TO flbl_monto_ap

               ON ACTION aceptar
                  #Se verifica si se puede iniciar el proceso      
                  CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) 
                            RETURNING r_resultado_opera
                  IF(r_resultado_opera <> 0)THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                     EXIT DISPLAY
                  END IF
                  CALL fn_ventana_confirma(p_cad_ventana,"Confirmar Generar Solicitudes de Restitución al SSV","question")
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod) 
                         RETURNING r_pid

                     CALL fn_inicializa_proceso(r_pid,
                                                v_proceso_cod,
                                                v_opera_cod,
                                                v_folio,
                                                "SEPL28",
                                                v_nom_archivo,
                                                p_usuario_cod)
                           RETURNING r_resultado_opera
                     IF(r_resultado_opera <> 0)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                        CONTINUE DISPLAY
                     END IF

                     CALL fn_actualiza_opera_ini(r_pid,
                                                 v_proceso_cod,
                                                 v_opera_cod,
                                                 v_folio,
                                                 "SEPL28",
                                                 v_nom_archivo,
                                                 p_usuario_cod)
                           RETURNING r_resultado_opera
                     IF(r_resultado_opera <> 0)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                        CONTINUE DISPLAY
                     END IF                            


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
                        CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar generación de solicitudes de restitución","about")
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                     END IF
                     EXIT DISPLAY
                  ELSE
                     CONTINUE DISPLAY
                  END IF


               ON ACTION cancelar
                  EXIT DISPLAY


            END DISPLAY 

         ELSE
            CALL fn_mensaje(p_cad_ventana,"No se encontraron registros para solicitudes de restitución","information")
         END IF
         
      ELSE
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      END IF

   CLOSE WINDOW vtna_sol_restitucion


END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL28                                                   #
#Descripcion       => Recupera solicitudes de restitucion                      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 25, 2012                                           #
################################################################################
FUNCTION fn_recupera_sol_restitucion()
DEFINE v_consulta STRING,
       v_sol_restitucion_aux RECORD
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

   WHENEVER ERROR CONTINUE
   LET v_total_sar92_avis  = 0
   LET v_total_sar92_pesos = 0
   LET v_total_viv97_aivs  = 0
   LET v_total_viv97_pesos = 0
   LET v_total_ap_avis     = 0
   LET v_total_ap_pesos    = 0   
   CALL v_sol_restitucion.clear()
   
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

   # 1   Recupera montos de analisis
   LET v_consulta = --"\n SELECT UNIQUE bat.id_expediente,bat.nss,",
                    "\n SELECT UNIQUE bat.id_expediente,",  
                    "\n        bat.nss,",  
                    "\n        mto.sar92_pesos_devolver,",
                    "\n        mto.sar92_aivs_devolver,",
                    "\n        mto.viv97_pesos_devolver,",
                    "\n        mto.viv97_aivs_devolver,",
                    "\n        mto.subsc_viv97_pesos_devolver,",
                    "\n        mto.subsc_viv97_aivs_devolver",
                 --   "\n   FROM sep_batch_contabilidad bat JOIN sep_mto_restitucion_analisis mto",
                 --   "\n     ON mto.id_expediente = bat.id_expediente",
                    "\n   FROM sep_nss_expediente bat JOIN sep_mto_restitucion_analisis mto",
                    "\n     ON mto.id_expediente = bat.id_expediente",
                    "\n    AND bat.tipo_nss = 1",    
                    "\n    AND mto.ind_restitucion = 1",
                    "\n  GROUP BY 1,2,3,4,5,6,7,8"


   LET v_indice = 1
   PREPARE prp_rec_res_analisis FROM v_consulta
   DECLARE cur_rec_res_analisis CURSOR FOR prp_rec_res_analisis
   FOREACH cur_rec_res_analisis INTO v_sol_restitucion_aux.*
      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
--      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado

      DISPLAY "------>1"
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

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,###.&&" 
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,###.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,###.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,###.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,###.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,###.&&" 

      LET v_indice = v_indice + 1

   END FOREACH  

   # 2  recupera los montos de restitucion 115
   LET v_consulta = --"\n SELECT UNIQUE bat.id_expediente,exp.nss,",
                    "\n SELECT UNIQUE res115.id_expediente,",
                    "\n        exp.nss,",
                    "\n        res115.sar92_pesos_115_trabajador ,",
                    "\n        res115.sar92_aivs_115_trabajador ,",
                    "\n        res115.viv97_pesos_115_trabajador ,",
                    "\n        res115.viv97_aivs_115_trabajador ,",
                    "\n        res115.subsc_viv97_pesos_115_trabajador ,",
                    "\n        res115.subsc_viv97_aivs_115_trabajador ",
                   -- "\n   FROM sep_batch_contabilidad bat JOIN sep_115_restitucion res115",
                   -- "\n     ON res115.id_expediente = bat.id_expediente,",
                    "\n   FROM sep_115_restitucion res115 ",
                    "\n        sep_nss_expediente exp ",                    
                    "\n   WHERE res115.ind_restitucion = 1",
                   -- "\n     AND bat.id_expediente = exp.id_expediente ",
                   "\n      AND res115.id_expediente = exp.id_expediente ", 
                    "\n     AND exp.tipo_nss = 2 ",                    
                    "\n    GROUP BY 1,2,3,4,5,6,7,8 ",
                    "\n   UNION ",                    
                    --"\n SELECT UNIQUE bata.id_expediente,bata.nss,",
                    "\n SELECT UNIQUE res115.id_expediente,",
                    "\n        exp.nss,",
                    "\n        res115a.sar92_pesos_115_acreditado,",
                    "\n        res115a.sar92_aivs_115_acreditado,",
                    "\n        res115a.viv97_pesos_115_acreditado,",
                    "\n        res115a.viv97_aivs_115_acreditado,",
                    "\n        res115a.subsc_viv97_pesos_115_acreditado,",
                    "\n        res115a.subsc_viv97_aivs_115_acreditado ",
                   -- "\n   FROM sep_batch_contabilidad bata JOIN sep_115_restitucion res115a",
                   -- "\n     ON res115a.id_expediente = bata.id_expediente,",
                    "\n   FROM sep_115_restitucion res115 ",
                    "\n        sep_nss_expediente exp ",                    
                    "\n   WHERE res115a.ind_restitucion = 1",
                    --"\n     AND bata.id_expediente = exp.id_expediente ",
                    "\n     AND res115.id_expediente = exp.id_expediente ",
                    "\n     AND exp.tipo_nss = 1 ",                    
                    "\n    GROUP BY 1,2,3,4,5,6,7,8 " 

   PREPARE prp_rec_res_115 FROM v_consulta
   DECLARE cur_rec_res_115 CURSOR FOR prp_rec_res_115
   FOREACH cur_rec_res_115 INTO v_sol_restitucion_aux.*

      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado

      DISPLAY "------>2"
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

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,###.&&"  
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,###.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,###.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,###.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,###.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,###.&&" 

      LET v_indice = v_indice + 1
                                
   END FOREACH 

   # 3   recupera los montos no aplicados antes pscd
   LET v_consulta = "\n SELECT UNIQUE bat.id_expediente,",
                    "\n        bat.nss,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        apl.subsc_viv97_pesos_no_aplica,",
                    "\n        apl.subsc_viv97_aivs_no_aplica",
                    --"\n   FROM sep_batch_contabilidad bat JOIN sep_mto_restitucion_no_aplicados apl",
                    --"\n     ON apl.id_expediente = bat.id_expediente",
                    "\n   FROM sep_nss_expediente bat JOIN sep_mto_restitucion_no_aplicados apl",
                    "\n     ON apl.id_expediente = bat.id_expediente",
                    "\n    AND bat.tipo_nss = 1",
                    "\n    AND apl.subsc_viv97_aivs_no_aplica > 0 ",                    
                    "\n   WHERE apl.ind_restitucion = 1"

   PREPARE prp_rec_res_no_aplicados FROM v_consulta
   DECLARE cur_rec_res_no_aplicados CURSOR FOR prp_rec_res_no_aplicados 
   FOREACH cur_rec_res_no_aplicados INTO v_sol_restitucion_aux.*

      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado

      DISPLAY "------>3"
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

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,###.&&" 
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,###.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,###.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,###.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,###.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,###.&&" 

      LET v_indice = v_indice + 1

   END FOREACH

   # 4   recupera los montos no aplicados pscd
   LET v_consulta = "\n SELECT UNIQUE bat.id_expediente,",
                    "\n        bat.nss,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        apl.subsc_viv97_pesos_no_aplica_pscd,",
                    "\n        apl.subsc_viv97_aivs_no_aplica_pscd ",
                    --"\n   FROM sep_batch_contabilidad bat JOIN sep_mto_restitucion_no_aplicados apl",
                    --"\n     ON apl.id_expediente = bat.id_expediente",
                    "\n   FROM sep_nss_expediente bat JOIN sep_mto_restitucion_no_aplicados apl",
                    "\n     ON apl.id_expediente = bat.id_expediente",
                    "\n    AND bat.tipo_nss = 1",
                    "\n    AND apl.subsc_viv97_aivs_no_aplica_pscd > 0",
                    "\n   WHERE apl.ind_restitucion = 1"

   PREPARE prp_rec_res_no_aplicados_pscd FROM v_consulta
   DECLARE cur_rec_res_no_aplicados_pscd CURSOR FOR prp_rec_res_no_aplicados_pscd 
   FOREACH cur_rec_res_no_aplicados_pscd INTO v_sol_restitucion_aux.*

      # Recupera invadido para el expediente
      EXECUTE prp_rec_invadido USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_invadido
      # Recupera asociado para el expediente
      EXECUTE prp_rec_asociado USING v_sol_restitucion_aux.v_id_expediente 
                                INTO v_asociado

      DISPLAY "------>4"
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

      LET v_total_sar92_avis  = v_total_sar92_avis  + v_sol_restitucion[v_indice].v_sar92_avis   USING "###,###,###.&&" 
      LET v_total_sar92_pesos = v_total_sar92_pesos + v_sol_restitucion[v_indice].v_sar92_pesos  USING "###,###,###.&&" 
      LET v_total_viv97_aivs  = v_total_viv97_aivs  + v_sol_restitucion[v_indice].v_viv97_aivs   USING "###,###,###.&&" 
      LET v_total_viv97_pesos = v_total_viv97_pesos + v_sol_restitucion[v_indice].v_viv97_pesos  USING "###,###,###.&&" 
      LET v_total_ap_avis     = v_total_ap_avis     + v_sol_restitucion[v_indice].v_ap_avis      USING "###,###,###.&&" 
      LET v_total_ap_pesos    = v_total_ap_pesos    + v_sol_restitucion[v_indice].v_ap_pesos     USING "###,###,###.&&" 

      LET v_indice = v_indice + 1

   END FOREACH

   
--   IF(v_sol_restitucion.getLength() > 0)THEN
   IF(v_indice > 1)THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF

END FUNCTION
