--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22/10/2012
--===============================================================
################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEC13                                                        #
#Objetivo     => Consulta para revisar los registros que se liquidaron en la   #
#                complementaria de respuesta de PROCESAR                       #
#Fecha inicio => Octubre 22, 2012                                              #
################################################################################

DATABASE safre_viv
GLOBALS "DPEG01.4gl"
GLOBALS 
DEFINE 
       g_usuario_cod    LIKE seg_usuario.usuario_cod,
       g_tipo_ejecucion SMALLINT,
       g_pid            LIKE bat_ctr_proceso.pid,
       g_proceso_cod    SMALLINT,
       g_opera_cod      SMALLINT,
       g_s_titulo       CHAR(25),
       v_folio          DECIMAL(9,0), 
       v_estado         SMALLINT, 
       v_relacion       SMALLINT

DEFINE g_reg_modulo   RECORD
          ruta_exp      CHAR(40),
          ruta_rescate  CHAR(40),
          ruta_listados CHAR(40)
       END RECORD

DEFINE seg_modulo_bat   RECORD
          ruta_listados CHAR(40)
       END RECORD
       
DEFINE v_chbx_acep_rel  SMALLINT,
       v_chbx_rech_rel  SMALLINT,
       v_chbx_pend_rel  SMALLINT,
       v_chbx_acep_srel SMALLINT,
       v_chbx_rech_srel SMALLINT,
       v_chbx_pend_srel SMALLINT

DEFINE v_regs_acep_rel   INTEGER      ,
       v_regs_rech_rel   INTEGER      ,
       v_regs_pend_rel   INTEGER      ,
       v_aivs_acep_rel   DECIMAL(16,6),
       v_aivs_rech_rel   DECIMAL(16,6),
       v_aivs_pend_rel   DECIMAL(16,6),
       v_total_regs_rel  INTEGER      ,
       v_total_aivs_rel  DECIMAL(16,6),
       v_regs_acep_srel  INTEGER,
       v_regs_rech_srel  INTEGER,
       v_regs_pend_srel  INTEGER,
       v_aivs_acep_srel  DECIMAL(16,6),
       v_aivs_rech_srel  DECIMAL(16,6),
       v_aivs_pend_srel  DECIMAL(16,6),
       v_total_regs_srel INTEGER      ,
       v_total_aivs_srel DECIMAL(16,6),
       v_diagnostico_consulta SMALLINT,
       v_diagnostico_descripcion CHAR(30)
       
DEFINE arr_patrones DYNAMIC ARRAY OF RECORD 
          v_nrp                CHAR(11),
          v_bimestre_per_pago  CHAR(2),
          v_anio_per_pago      CHAR(4),
          v_folio_sua          DECIMAL(9,0),  
          v_total_regs         INTEGER,
          v_periodo_pago       CHAR(6)
END RECORD 

DEFINE arr_det_solicitud DYNAMIC ARRAY OF RECORD
           v_nss_det     CHAR(11),
          v_aivs_solicitud  DECIMAL(16,6),
          v_pesos_solicitud DECIMAL(16,6),
          v_aivs_parcial    DECIMAL(16,6),
          v_pesos_parcial   DECIMAL(16,6),
          v_aivs_procesar   DECIMAL(16,6),
          v_pesos_procesar  DECIMAL(16,6)
END RECORD

DEFINE arr_cifras_globales DYNAMIC ARRAY OF RECORD 
          v_folio_lote      DECIMAL(9,0),
          v_total_regs_rel  INTEGER,
          V_total_aivs_rel  DECIMAL (16,6),
          v_diagnostico_rel SMALLINT,
          v_diag_desc_rel   CHAR(10)
END RECORD  

DEFINE arr_cifras_globales_sr DYNAMIC ARRAY OF RECORD 
          v_folio_lote       DECIMAL(9,0),
          v_total_regs_srel  INTEGER,
          V_total_aivs_srel  DECIMAL (22,2),
          v_diagnostico_srel SMALLINT,
          v_diag_desc_srel   CHAR(10)
END RECORD  

--Arreglo de detalles para reporte de PROCESAR VS SACI
DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
   v_tipo_registro    CHAR(10), --Tipo de registro ACEPTADO, RECHAZADO, PENDIENTE
   v_PROCESAR         CHAR(8),  --Etiqueta de resultados PROCESAR
   v_proc_nrp         CHAR(11), 
   v_proc_nss         CHAR(11),
   v_proc_periodo     CHAR(6),
   v_proc_folio_sua   DECIMAL(9,0),
   v_proc_aivs_dev    DECIMAL(16,6),
   v_proc_pesos       DECIMAL(16,6),
   v_proc_estatus     SMALLINT,
   v_proc_diagnostico SMALLINT,
   v_SACI             CHAR(8),  --Etiquete de resultados SACI 
   v_saci_nrp         CHAR(11),     
   v_saci_nss         CHAR(11),     
   v_saci_periodo     CHAR(6),      
   v_saci_folio_sua   DECIMAL(9,0), 
   v_saci_aivs_dev    DECIMAL(16,6),
   v_saci_pesos       DECIMAL(16,6),
   v_saci_estatus     SMALLINT,     
   v_saci_diagnostico SMALLINT,     
   v_DIFERENCIAS      CHAR(11), 
   v_dif_aivs_dev     DECIMAL(16,6),
   v_dif_pesos        DECIMAL(16,6)
END RECORD

--Arreglo de detalles para reporte de PROCESAR VS SACI
DEFINE arr_det_compl DYNAMIC ARRAY OF RECORD 
   v_tipo_registro    CHAR(10), --Tipo de registro ACEPTADO, RECHAZADO, PENDIENTE
   v_PROCESAR         CHAR(8),  --Etiqueta de resultados PROCESAR
   v_proc_nrp         CHAR(11), 
   v_proc_nss         CHAR(11),
   v_proc_periodo     CHAR(6),
   v_proc_folio_sua   DECIMAL(9,0),
   v_proc_aivs_dev    DECIMAL(16,6),
   v_proc_pesos       DECIMAL(16,6),
   v_proc_estatus     SMALLINT,
   v_proc_diagnostico SMALLINT,
   v_SACI             CHAR(8),  --Etiquete de resultados SACI 
   v_saci_nrp         CHAR(11),     
   v_saci_nss         CHAR(11),     
   v_saci_periodo     CHAR(6),      
   v_saci_folio_sua   DECIMAL(9,0), 
   v_saci_aivs_dev    DECIMAL(16,6),
   v_saci_pesos       DECIMAL(16,6),
   v_saci_estatus     SMALLINT,     
   v_saci_diagnostico SMALLINT,     
   v_DIFERENCIAS      CHAR(11), 
   v_dif_aivs_dev     DECIMAL(16,6),
   v_dif_pesos        DECIMAL(16,6)
END RECORD

END GLOBALS        
MAIN

DEFINE f_ventana        ui.Window,             -- Define las propìedades de la Ventana
       f_forma          ui.Form,               -- Define las propiedades de la forma
       manejador_rpt    om.SaxDocumentHandler  -- Contenedor de Documentos para el reporte
DEFINE v_i_pat              INTEGER,           -- Cursor Patrones
       v_i_pat_sr           INTEGER,           -- Cursor Patrones sin relación
       v_i_det              INTEGER,           -- Cursor detalles de solicitud
       v_i_det_sol_sr       INTEGER,           -- Cursor detalles de solicitud sin relación
       v_i_arch             INTEGER,           -- Cursor Archivos para combo  
       v_inicia             INTEGER,           -- Cursor para generar el reporte 
       v_tot_seleccion      SMALLINT,          -- Elementos seleccionados
       v_comando            STRING,            -- Comando para ejecutar consultas 
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- Id derechohabiente  
       v_cmb_folio          ui.ComboBox,        --Combo de folios
       v_i_rel              INTEGER,
       v_i_srel             INTEGER,
       v_diagnostico_rel    SMALLINT,
       v_diagnostico_srel   SMALLINT,
       v_ind_det_vs         INTEGER,
       v_i_comp_vs          INTEGER,
       i                    INTEGER,
       v_rpt_diferencias    STRING,
       v_nombre_reporte     STRING,
       v_fecha_dia          CHAR(8),
       bdn_open             STRING   
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_tipo_ejecucion = ARG_VAL(2)
   LET g_s_titulo       = ARG_VAL(3)

   LET g_proceso_cod = 1001
   LET g_opera_cod   = 1

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'dpe'
      
   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

CLOSE WINDOW SCREEN    
OPEN WINDOW v_consulta WITH FORM "DPEC130"
   DIALOG   ATTRIBUTES(UNBUFFERED)

      --Captura parámetros de consulta
      INPUT v_folio, 
            v_estado, 
            v_relacion
      FROM v_cmb_folio,
           v_cmb_estado,
           v_cmb_relacion
        
      BEFORE INPUT 
         LET f_ventana = ui.Window.getCurrent()
         LET f_forma = f_ventana.getForm()

         --Llama función para llenar combo con fecha nula
         CALL fn_llena_combo_folio() RETURNING v_folio, v_i_arch

         LET v_chbx_acep_rel  = 0
         LET v_chbx_rech_rel  = 0
         LET v_chbx_pend_rel  = 0
         LET v_chbx_acep_srel = 0
         LET v_chbx_rech_srel = 0
         LET v_chbx_pend_srel = 0
         INITIALIZE v_tot_seleccion TO NULL

        --Se invoca la función que asigna el titulo a la ventana
         CALL ui.Interface.setText(g_s_titulo)
         CALL f_forma.setElementHidden("gr_globales_rel", 1) --Oculta la Sección Cifras globales
         CALL f_forma.setElementHidden("gr_globales_srel", 1) --Oculta la Sección Cifras globales
         CALL f_forma.setElementHidden("gr_patrones", 1) --Oculta la sección de Patrones
         CALL f_forma.setElementHidden("gr_solicitudes", 1) --Oculta la sección de Solicitudes
         CALL DIALOG.setActionHidden("regresar", 1) --Oculta el botón Regresar
      END INPUT      

      DISPLAY ARRAY arr_cifras_globales TO scr_cifras_globales_rel.*
      END DISPLAY

      DISPLAY ARRAY arr_cifras_globales_sr TO scr_cifras_globales_srel.*
      END DISPLAY
      
      ON ACTION accept
         CALL f_forma.setElementHidden("gr_globales_rel", 0) --Muestra la Sección Cifras globales
         CALL f_forma.setElementHidden("gr_globales_srel", 0) --Muestra la Sección Cifras globales
         CALL DIALOG.setActionHidden("regresar", 0) --Muestra el botón Regresar

         --Ejecuta consulta para mostrar globales 
         CALL fn_consulta_cifras_globales(v_folio, v_estado, v_relacion)
            ON ACTION cons_patrones
               LET v_diagnostico_rel  = arr_cifras_globales[ARR_CURR()].v_diagnostico_rel
               LET v_diagnostico_srel = arr_cifras_globales_sr[ARR_CURR()].v_diagnostico_srel 

               --Consulta los detalles de los patrones
               CALL fn_consulta_patrones(v_diagnostico_rel,
                                         v_diagnostico_srel,
                                         v_folio)
                  RETURNING v_i_pat, 
                            v_i_pat_sr
                            
               IF v_i_pat > 1 OR v_i_pat_sr > 1 THEN 
                  --Muestra la sección de Patrones
                  CALL f_forma.setElementHidden("gr_patrones", 0)

                  DISPLAY ARRAY arr_patrones TO  scr_patrones.*
                  ATTRIBUTES (ACCEPT = FALSE, CANCEL = TRUE)
                  --Muestra detalles de la solicitud  
                  ON ACTION muestra_det_sol
                     CALL fn_consulta_solicitudes(arr_patrones[ARR_CURR()].v_nrp,
                                                  arr_patrones[ARR_CURR()].v_periodo_pago,
                                                  arr_patrones[ARR_CURR()].v_folio_sua, 
                                                  v_diagnostico_rel,
                                                  v_diagnostico_srel,
                                                  v_folio)
                                                  
                     RETURNING v_i_det, v_i_det_sol_sr

                     IF v_i_det > 1 OR v_i_det_sol_sr > 1 THEN 
                        CALL f_forma.setElementHidden("gr_solicitudes", 0)
                        DISPLAY ARRAY arr_det_solicitud TO scr_solicitudes.*
                        ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
                           ON ACTION CANCEL
                              EXIT PROGRAM
                              
                           ON ACTION regresar
                              CALL f_forma.setElementHidden("gr_solicitudes", 1)
                              CALL arr_det_solicitud.clear()                           
                              EXIT DISPLAY 
                              
                           ON ACTION reporte_solicitudes
                              IF fgl_report_loadCurrentSettings("DPEC133.4rp") THEN
                                 CALL fgl_report_selectDevice ("PDF")
                                 LET manejador_rpt = fgl_report_commitCurrentSettings()
                              END IF
                              
                              --Inicia el reporte de detalles de la SOLICITUD
                              START REPORT rpt_detalle_solicitud TO XML HANDLER manejador_rpt
                                 -- Asigna el titulo del reporte
                                 FOR v_inicia = 1 TO v_i_det
                                 --arr_VerRechazos.getLength()
                                    CASE
                                       WHEN v_diagnostico_rel = 1
                                          LET v_diagnostico_descripcion = "Aceptado"
                                       WHEN v_diagnostico_rel = 2
                                          LET v_diagnostico_descripcion = "Rechazado"
                                       WHEN v_diagnostico_rel = 3
                                          LET v_diagnostico_descripcion = "Pendiente"
                                       WHEN v_diagnostico_rel = 4
                                          LET v_diagnostico_descripcion = "Parcial"
                                    END CASE
                        
                                    OUTPUT TO REPORT rpt_detalle_solicitud (g_usuario_cod,
                                                                            v_folio,
                                                                            v_diagnostico_descripcion,
                                                                            arr_patrones[ARR_CURR()].v_nrp,
                                                                            arr_patrones[ARR_CURR()].v_folio_sua,
                                                                            arr_patrones[ARR_CURR()].v_periodo_pago,
                                                                            arr_det_solicitud[v_inicia].*)
                                 END FOR
                              FINISH REPORT rpt_detalle_solicitud 
                        
                           ON ACTION btn_derechohabiente
                              -- se ejecuta la consulta del derechohabiente usando la consulta general
                              LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || g_usuario_cod || "1 'Consulta de Derechohabiente' " ||arr_det_solicitud[ARR_CURR()].v_nss_det                           
                              RUN v_comando
                        
                           ON ACTION btn_saldo
                              --Recupera el id_derechohabiente 
                              SELECT id_derechohabiente
                                INTO v_id_derechohabiente
                                FROM afi_derechohabiente
                                WHERE nss = arr_det_solicitud[ARR_CURR()].v_nss_det
                              --Ejecuta la consulta de saldo
                              CALL fn_eje_consulta(1,g_usuario_cod,v_id_derechohabiente,g_tipo_ejecucion,g_s_titulo)
                        END DISPLAY
                     ELSE
                        CALL fn_mensaje("Atención", "No se encontraron registros de detalle","stop" )
                     END IF
                     
                  ON ACTION reporte_patrones
                     IF fgl_report_loadCurrentSettings("DPEC132.4rp") THEN
                        CALL fgl_report_selectDevice ("PDF")
                        LET manejador_rpt = fgl_report_commitCurrentSettings()
                     END IF
                     
                     --Inicia el reporte de detalles PATRONES
                     START REPORT rpt_detalles_patrones TO XML HANDLER manejador_rpt
                        FOR v_inicia = 1 TO v_i_pat
                        
                           CASE
                              WHEN v_diagnostico_rel = 1
                                 LET v_diagnostico_descripcion = "Aceptado"
                              WHEN v_diagnostico_rel = 2
                                 LET v_diagnostico_descripcion = "Rechazado"
                              WHEN v_diagnostico_rel = 3
                                 LET v_diagnostico_descripcion = "Pendiente"
                              WHEN v_diagnostico_rel = 4
                                 LET v_diagnostico_descripcion = "Parcial"   
                           END CASE
                        
                           OUTPUT TO REPORT rpt_detalles_patrones (g_usuario_cod,
                                                                   v_folio,
                                                                   v_diagnostico_descripcion,
                                                                   arr_patrones[v_inicia].*)
                        END FOR
                     FINISH REPORT rpt_detalles_patrones 
                  -- Sale del display y oculta sección patrones 
                  ON ACTION regresar
                     CALL f_forma.setElementHidden("gr_patrones", 1) --Oculta la sección de Patrones
                     CALL arr_patrones.clear()
                     EXIT DISPLAY

                  ON ACTION CANCEL
                     EXIT PROGRAM 
               END DISPLAY
              ELSE
                 CALL fn_mensaje ("Atención" , "No se encontraron registros", "stop")
              END IF  

            
         ON ACTION regresar
            --Limpia el arreglo 
            CALL arr_cifras_globales.clear() 
            --Oculta la Sección Cifras globales 
            CALL f_forma.setElementHidden("gr_globales_rel", 1) --Oculta la Sección Cifras globales
            CALL f_forma.setElementHidden("gr_globales_srel", 1) --Oculta la Sección Cifras globales
            CALL DIALOG.setActionHidden("regresar", 1) --Oculta el botón Regresar
 
         ON ACTION reporte_globales
            IF fgl_report_loadCurrentSettings("DPEC131.4rp") THEN
               CALL fgl_report_selectDevice ("PDF")
               LET manejador_rpt = fgl_report_commitCurrentSettings()
            END IF

            --Inicia el reporte de registros con rechazo
            START REPORT rpt_cifras_globales TO XML HANDLER manejador_rpt

                  OUTPUT TO REPORT rpt_cifras_globales(--Encabezado
                                                       g_usuario_cod,
                                                       v_folio,
                                                       --Detalles
                                                       v_regs_acep_rel,
                                                       v_regs_rech_rel,
                                                       v_regs_pend_rel,
                                                       v_aivs_acep_rel, 
                                                       v_aivs_rech_rel, 
                                                       v_aivs_pend_rel, 
                                                       v_regs_acep_srel,
                                                       v_regs_rech_srel,
                                                       v_regs_pend_srel,
                                                       v_aivs_acep_srel,
                                                       v_aivs_rech_srel,
                                                       v_aivs_pend_srel,
                                                       --Totales 
                                                       v_total_regs_rel,
                                                       v_total_aivs_rel,
                                                       v_total_regs_srel,
                                                       v_total_aivs_srel)
            FINISH REPORT rpt_cifras_globales
            
         ON ACTION cancelar            
            EXIT DIALOG 

         --Ejecuta reporte en TXT de confrontación
         ON ACTION reporte
            --IF v_folio IS NULL THEN
            --   CALL fn_mensaje("Atención","Debe seleccionar un folio para generar el reporte","stop")
            --ELSE
               CALL fn_dpe_ejecuta_generacion_archivo(v_folio, g_usuario_cod)
            --END IF 
   END DIALOG 
CLOSE WINDOW v_consulta 
END MAIN 

#OBJETIVO: Obtener los resultados de la consulta de cifras globales 
FUNCTION fn_consulta_cifras_globales(p_folio, p_estado, p_relacion)
DEFINE p_folio          DECIMAL(9,0), 
       p_estado         SMALLINT, 
       p_relacion       SMALLINT,
       v_Qry_Txt        STRING,
       v_i_rel          INTEGER,
       v_i_srel         INTEGER,
       i                SMALLINT

DEFINE arr_loc_cifras_globales DYNAMIC ARRAY OF RECORD 
          v_total_regs_rel   INTEGER,
          V_total_aivs_rel   DECIMAL (16,6),
          v_diagnostico_rel  SMALLINT
END RECORD
DEFINE arr_loc_cifras_globales_sr DYNAMIC ARRAY OF RECORD 
          v_total_regs_srel  INTEGER,
          V_total_aivs_srel  DECIMAL (22,2),
          v_diagnostico_srel SMALLINT
END RECORD   

   LET v_total_regs_rel  = 0
   LET v_total_aivs_rel  = 0
   LET v_total_regs_srel = 0
   LET v_total_aivs_srel = 0

   LET v_Qry_Txt = "\n SELECT r.folio,",
                   "\n        COUNT(d.resul_op),",
                   "\n        SUM(d.aivs_viv_dev ),", 
                   "\n        d.resul_op,",
                   "\n        ''",
                   "\n FROM   dpe_resp_procesar d,",
                   "\n        dpe_sol_trabajador r",
                   "\n WHERE  d.reg_patronal_imss  = r.reg_patronal_imss ",
                   "\n AND    d.periodo_pago       = r.periodo_pago      ",
                   "\n AND    d.nss                = r.nss               "


   IF p_folio IS NOT NULL THEN 
      LET v_Qry_Txt = v_Qry_Txt ||"\n AND    d.folio = ",p_folio    
   END IF 

   IF p_estado IS NULL THEN 
      LET v_Qry_Txt = v_Qry_Txt || "\n AND    d.resul_op           IN (1,2,4)"
   ELSE
      CASE p_estado

      WHEN 1 --Aceptados   
       LET v_Qry_Txt = v_Qry_Txt || "\n  AND    d.resul_op           = 1"

      WHEN 2 --Rechazados
       LET v_Qry_Txt = v_Qry_Txt || "\n  AND    d.resul_op           = 2"
       
      WHEN 4 --Pendientes
       LET v_Qry_Txt = v_Qry_Txt || "\n  AND    d.resul_op           = 4"
      END CASE  
   END IF 
   
   LET v_Qry_Txt = v_Qry_Txt ||"\n GROUP BY 1,4","\n ORDER BY 1,4"
                   
   DISPLAY v_Qry_Txt

   PREPARE prp_globales_relacion FROM v_Qry_Txt
   DECLARE cur_globales_relacion  CURSOR FOR prp_globales_relacion

   LET v_i_rel = 1
   FOREACH cur_globales_relacion INTO arr_cifras_globales[v_i_rel].v_folio_lote,
                                      arr_cifras_globales[v_i_rel].v_total_regs_rel,   
                                      arr_cifras_globales[v_i_rel].v_total_aivs_rel,  
                                      arr_cifras_globales[v_i_rel].v_diagnostico_rel,
                                      arr_cifras_globales[v_i_rel].v_diag_desc_rel
      CASE
         WHEN arr_cifras_globales[v_i_rel].v_diagnostico_rel = 1
            LET  arr_cifras_globales[v_i_rel].v_diag_desc_rel = "Aceptado"
         WHEN arr_cifras_globales[v_i_rel].v_diagnostico_rel = 2
            LET  arr_cifras_globales[v_i_rel].v_diag_desc_rel = "Rechazado"
         WHEN arr_cifras_globales[v_i_rel].v_diagnostico_rel = 3
            LET  arr_cifras_globales[v_i_rel].v_diag_desc_rel = "Pendiente"
         WHEN arr_cifras_globales[v_i_rel].v_diagnostico_rel = 4
            LET  arr_cifras_globales[v_i_rel].v_diag_desc_rel = "Parciales"
      END CASE 

      LET v_total_regs_rel  =  v_total_regs_rel + arr_cifras_globales[v_i_rel].v_total_regs_rel 
      LET v_total_aivs_rel  =  v_total_aivs_rel + arr_cifras_globales[v_i_rel].v_total_aivs_rel      

      LET v_i_rel = v_i_rel + 1
   END FOREACH 

   LET v_Qry_Txt = "\n SELECT folio_integra,",
                   "\n        COUNT(d.diagnostico),         ",
                   "\n        SUM(d.num_aplicaciones_inter),", 
                   "\n        d.diagnostico,                ",
                   "\n        ''                            ",
                   "\n FROM   dpe_sol_trab_complementario d ",
                   "\n WHERE  1=1 "

   IF p_folio IS NOT NULL THEN 
      LET v_Qry_Txt = v_Qry_Txt ||"\n AND    d.folio_integra = ",p_folio    
   END IF 
                   
   IF p_estado IS NULL THEN 
      LET v_Qry_Txt = v_Qry_Txt || "\n AND d.diagnostico IN (1,2,3,4)"
   ELSE
      CASE p_estado

      WHEN 1 --Aceptados   
       LET v_Qry_Txt = v_Qry_Txt || "\n AND d.diagnostico = 1"

      WHEN 2 --Rechazados
       LET v_Qry_Txt = v_Qry_Txt || "\n AND d.diagnostico = 2"
       
      WHEN 4 --Pendientes
       LET v_Qry_Txt = v_Qry_Txt || "\n AND d.diagnostico = 4"
      END CASE  
   END IF 

   LET v_Qry_Txt = v_Qry_Txt ||"\n GROUP BY 1,4","\n ORDER BY 1,4"

   LET v_i_srel = 1
   
DISPLAY v_Qry_Txt   
   PREPARE prp_globales_srel FROM v_Qry_Txt
   DECLARE cur_globales_srel CURSOR FOR prp_globales_srel    
   FOREACH cur_globales_srel  INTO arr_cifras_globales_sr[v_i_srel].v_folio_lote,
                                   arr_cifras_globales_sr[v_i_srel].v_total_regs_srel,   
                                   arr_cifras_globales_sr[v_i_srel].v_total_aivs_srel,  
                                   arr_cifras_globales_sr[v_i_srel].v_diagnostico_srel,
                                   arr_cifras_globales_sr[v_i_srel].v_diag_desc_srel
      CASE 
         WHEN arr_cifras_globales_sr[v_i_srel].v_diagnostico_srel    = 1
            LET  arr_cifras_globales_sr[v_i_srel].v_diag_desc_srel = "Aceptado"
         WHEN arr_cifras_globales_sr[v_i_srel].v_diagnostico_srel    = 2
            LET  arr_cifras_globales_sr[v_i_srel].v_diag_desc_srel = "Rechazado"
         WHEN  arr_cifras_globales_sr[v_i_srel].v_diagnostico_srel   = 3
            LET  arr_cifras_globales_sr[v_i_srel].v_diag_desc_srel = "Pendiente"
         WHEN  arr_cifras_globales_sr[v_i_srel].v_diagnostico_srel   = 4
            LET  arr_cifras_globales_sr[v_i_srel].v_diag_desc_srel = "Parciales"
      END CASE 

      LET v_total_regs_srel = arr_cifras_globales_sr[v_i_srel].v_total_regs_srel   
      LET v_total_aivs_srel = arr_cifras_globales_sr[v_i_srel].v_total_aivs_srel  
      
      LET v_i_srel = v_i_srel + 1
      
   END FOREACH

   CALL arr_cifras_globales.deleteElement(v_i_rel)
   CALL arr_cifras_globales_sr.deleteElement(v_i_srel)

   --Totales relación
   --Total registros
   --IF arr_cifras_globales[1].v_total_regs_rel IS NULL THEN 
      --LET arr_cifras_globales[1].v_total_regs_rel = 0
   --END IF    
   --IF arr_cifras_globales[2].v_total_regs_rel IS NULL THEN 
      --LET arr_cifras_globales[2].v_total_regs_rel = 0
   --END IF    
   --IF arr_cifras_globales[3].v_total_regs_rel IS NULL THEN 
      --LET arr_cifras_globales[3].v_total_regs_rel = 0
   --END IF    
   --Total AIVS
   --IF arr_cifras_globales[1].v_total_aivs_rel IS NULL THEN
        --LET arr_cifras_globales[1].v_total_aivs_rel  = 0.00 
   --END IF 
   --IF arr_cifras_globales[2].v_total_aivs_rel IS NULL THEN
        --LET arr_cifras_globales[2].v_total_aivs_rel  = 0.00 
   --END IF 
   --IF arr_cifras_globales[3].v_total_aivs_rel IS NULL THEN
        --LET arr_cifras_globales[3].v_total_aivs_rel  = 0.00 
   --END IF
   --
   --Totales sin relación
   --Total registros
   --IF arr_cifras_globales_sr[1].v_total_regs_srel IS NULL THEN 
      --LET arr_cifras_globales_sr[1].v_total_regs_srel = 0
   --END IF    
   --IF arr_cifras_globales_sr[2].v_total_regs_srel IS NULL THEN 
      --LET arr_cifras_globales_sr[2].v_total_regs_srel = 0
   --END IF    
   --IF arr_cifras_globales_sr[3].v_total_regs_srel IS NULL THEN 
      --LET arr_cifras_globales_sr[3].v_total_regs_srel = 0
   --END IF    
   --Total AIVS
   --IF arr_cifras_globales_sr[1].v_total_aivs_srel IS NULL THEN
        --LET arr_cifras_globales_sr[1].v_total_aivs_srel  = 0.00 
   --END IF 
   --IF arr_cifras_globales_sr[2].v_total_aivs_srel IS NULL THEN
        --LET arr_cifras_globales_sr[2].v_total_aivs_srel  = 0.00 
   --END IF 
   --IF arr_cifras_globales_sr[3].v_total_aivs_srel IS NULL THEN
        --LET arr_cifras_globales_sr[3].v_total_aivs_srel  = 0.00 
   --END IF

   --LET v_regs_acep_rel  = arr_cifras_globales[1].v_total_regs_rel
   --LET v_regs_rech_rel  = arr_cifras_globales[2].v_total_regs_rel
   --LET v_regs_pend_rel  = arr_cifras_globales[3].v_total_regs_rel
--
   --LET v_aivs_acep_rel  = arr_cifras_globales[1].v_total_aivs_rel
   --LET v_aivs_rech_rel  = arr_cifras_globales[2].v_total_aivs_rel
   --LET v_aivs_pend_rel  = arr_cifras_globales[3].v_total_aivs_rel
--
   --LET v_regs_acep_srel = arr_cifras_globales_sr[1].v_total_regs_srel
   --LET v_regs_rech_srel = arr_cifras_globales_sr[2].v_total_regs_srel
   --LET v_regs_pend_srel = arr_cifras_globales_sr[3].v_total_regs_srel
--
   --LET v_aivs_acep_srel = arr_cifras_globales_sr[1].v_total_aivs_srel
   --LET v_aivs_rech_srel = arr_cifras_globales_sr[2].v_total_aivs_srel
   --LET v_aivs_pend_srel = arr_cifras_globales_sr[3].v_total_aivs_srel
   
   --LET v_total_regs_rel  = arr_cifras_globales[1].v_total_regs_rel +
                           --arr_cifras_globales[2].v_total_regs_rel +
                           --arr_cifras_globales[3].v_total_regs_rel
--
   --FOR i= 1 TO arr_cifras_globales.getlength()
      --LET v_total_regs_rel  =  arr_cifras_globales[v_i_rel].v_total_regs_rel +
                               --arr_cifras_globales[v_i_rel].v_total_regs_rel   
   --END FOR 
                           
   --LET v_total_aivs_rel  = arr_cifras_globales[1].v_total_aivs_rel +
                           --arr_cifras_globales[2].v_total_aivs_rel +
                           --arr_cifras_globales[3].v_total_aivs_rel
                           --
   --LET v_total_regs_srel = arr_cifras_globales_sr[1].v_total_regs_srel +
                           --arr_cifras_globales_sr[2].v_total_regs_srel +
                           --arr_cifras_globales_sr[3].v_total_regs_srel
                           --
   --LET v_total_aivs_srel = arr_cifras_globales_sr[1].v_total_aivs_srel +
                           --arr_cifras_globales_sr[2].v_total_aivs_srel +
                           --arr_cifras_globales_sr[3].v_total_aivs_srel
                           
   DISPLAY  v_total_regs_rel  TO ed_total_regs_rel
   DISPLAY  v_total_aivs_rel  TO ed_total_aivs_rel
   DISPLAY  v_total_regs_srel TO ed_total_regs_srel
   DISPLAY  v_total_aivs_srel TO ed_total_aivs_srel

END FUNCTION 

#OBJETIVO: Obtener los resultados de la consulta patrones 
FUNCTION fn_consulta_patrones(p_dianostico_relacion, 
                              p_dianostico_srelacion,
                              p_folio)
DEFINE p_dianostico_relacion  SMALLINT,
       p_dianostico_srelacion SMALLINT,
       p_folio                DECIMAL(9,0)

DEFINE rec_patrones RECORD
          v_nrp          CHAR(11),
          v_periodo_pago CHAR(6),
          v_bimestre_per_pago  CHAR(2),
          v_anio_per_pago      CHAR(4),
          v_folio_sua    DECIMAL(9,0),
          v_total_regs   INTEGER      
END RECORD 
DEFINE v_QryTxt            STRING,
       v_ind_pat           INTEGER,
       v_ind_pat_sr        INTEGER
       
IF p_dianostico_relacion IS NOT NULL THEN 
   LET v_QryTxt  = "\n SELECT a.reg_patronal_imss,",
                   "\n        a.periodo_pago,",
                   "\n        b.folio_sua,",
                   "\n        COUNT(*)",
                   "\n FROM   dpe_resp_procesar a, ",
                   "\n         dpe_patron b",
                   "\n WHERE  a.folio = ",p_folio,
                   "\n AND    b.id_dpe_referencia in (select id_dpe_patron",
                   "\n                                from dpe_sol_trabajador",
                   "\n                                where folio_respuesta =",p_folio,")",
                   "\n AND    a.reg_patronal_imss = b.reg_patronal_imss",
                   "\n AND    a.periodo_pago = b.periodo_pago"
                   

   IF p_dianostico_relacion IS NOT NULL THEN 
      LET v_QryTxt = v_QryTxt || "\n AND a.resul_op =", p_dianostico_relacion
   END IF 

   LET v_QryTxt = v_QryTxt ||"\n GROUP BY 1,2,3", "\n ORDER BY 1,2,3"
   
   PREPARE prp_cons_patrones FROM  v_QryTxt
   DECLARE cur_cons_patrones CURSOR FOR prp_cons_patrones

   LET v_ind_pat = 1
   
   FOREACH cur_cons_patrones INTO rec_patrones.v_nrp,
                                  rec_patrones.v_periodo_pago,
                                  rec_patrones.v_folio_sua,
                                  rec_patrones.v_total_regs

      LET rec_patrones.v_bimestre_per_pago =  rec_patrones.v_periodo_pago[5,6]
      LET rec_patrones.v_anio_per_pago     =  rec_patrones.v_periodo_pago[1,4] 

      CASE 
         WHEN rec_patrones.v_bimestre_per_pago = '01' OR rec_patrones.v_bimestre_per_pago = '02'
            LET rec_patrones.v_bimestre_per_pago = '01'
         WHEN rec_patrones.v_bimestre_per_pago = '03' OR rec_patrones.v_bimestre_per_pago = '04'
            LET rec_patrones.v_bimestre_per_pago = '02'
         WHEN rec_patrones.v_bimestre_per_pago = '05' OR rec_patrones.v_bimestre_per_pago = '06'
            LET rec_patrones.v_bimestre_per_pago = '03'
         WHEN rec_patrones.v_bimestre_per_pago = '07' OR rec_patrones.v_bimestre_per_pago = '08'
            LET rec_patrones.v_bimestre_per_pago = '04'
         WHEN rec_patrones.v_bimestre_per_pago = '09' OR rec_patrones.v_bimestre_per_pago = '10'
            LET rec_patrones.v_bimestre_per_pago = '05'
         WHEN rec_patrones.v_bimestre_per_pago = '11' OR rec_patrones.v_bimestre_per_pago = '12'
            LET rec_patrones.v_bimestre_per_pago = '06'            
      END CASE 

      LET arr_patrones[v_ind_pat].v_nrp                = rec_patrones.v_nrp
      LET arr_patrones[v_ind_pat].v_bimestre_per_pago  = rec_patrones.v_bimestre_per_pago
      LET arr_patrones[v_ind_pat].v_anio_per_pago      = rec_patrones.v_anio_per_pago
      LET arr_patrones[v_ind_pat].v_folio_sua          = rec_patrones.v_folio_sua
      LET arr_patrones[v_ind_pat].v_total_regs         = rec_patrones.v_total_regs
      LET arr_patrones[v_ind_pat].v_periodo_pago       = rec_patrones.v_periodo_pago

      LET v_ind_pat = v_ind_pat + 1
      
   END FOREACH
END IF 
----------------------------------
--Consulta Patrones Sin Relación--
----------------------------------

IF p_dianostico_srelacion IS NOT NULL THEN 
   LET v_QryTxt  = "\n SELECT num_reg_pat_imss,",              
                   "\n        per_pago,",              
                   "\n        folio_sua,",              
                   "\n        COUNT(nss_aportacion)   ",              
                   "\n FROM   dpe_sol_trab_complementario",
                   "\n WHERE  resul_operacion =", p_dianostico_srelacion,
                   "\n AND    folio_integra =",p_folio

   IF p_dianostico_srelacion IS NOT NULL THEN 
      LET v_QryTxt = v_QryTxt || "\n AND resul_operacion =", p_dianostico_srelacion
   END IF 

   LET v_QryTxt = v_QryTxt ||"\n GROUP BY 1,2,3", "\n ORDER BY 1,2,3"

   PREPARE prp_cons_patrones_sr FROM  v_QryTxt
   DECLARE cur_cons_patrones_sr CURSOR FOR prp_cons_patrones_sr

   LET v_ind_pat_sr = 1
   
   FOREACH cur_cons_patrones_sr INTO rec_patrones.v_nrp,
                                     rec_patrones.v_periodo_pago,
                                     rec_patrones.v_folio_sua,
                                     rec_patrones.v_total_regs

      LET rec_patrones.v_bimestre_per_pago =  rec_patrones.v_periodo_pago[5,6]
      LET rec_patrones.v_anio_per_pago     =  rec_patrones.v_periodo_pago[1,4] 

      CASE 
         WHEN rec_patrones.v_bimestre_per_pago = '01' OR rec_patrones.v_bimestre_per_pago = '02'
            LET rec_patrones.v_bimestre_per_pago = '01'
         WHEN rec_patrones.v_bimestre_per_pago = '03' OR rec_patrones.v_bimestre_per_pago = '04'
            LET rec_patrones.v_bimestre_per_pago = '02'
         WHEN rec_patrones.v_bimestre_per_pago = '05' OR rec_patrones.v_bimestre_per_pago = '06'
            LET rec_patrones.v_bimestre_per_pago = '03'
         WHEN rec_patrones.v_bimestre_per_pago = '07' OR rec_patrones.v_bimestre_per_pago = '08'
            LET rec_patrones.v_bimestre_per_pago = '04'
         WHEN rec_patrones.v_bimestre_per_pago = '09' OR rec_patrones.v_bimestre_per_pago = '10'
            LET rec_patrones.v_bimestre_per_pago = '05'
         WHEN rec_patrones.v_bimestre_per_pago = '11' OR rec_patrones.v_bimestre_per_pago = '12'
            LET rec_patrones.v_bimestre_per_pago = '06'            
      END CASE 

      LET arr_patrones[v_ind_pat_sr].v_nrp                = rec_patrones.v_nrp
      LET arr_patrones[v_ind_pat_sr].v_bimestre_per_pago  = rec_patrones.v_bimestre_per_pago
      LET arr_patrones[v_ind_pat_sr].v_anio_per_pago      = rec_patrones.v_anio_per_pago
      LET arr_patrones[v_ind_pat_sr].v_folio_sua          = rec_patrones.v_folio_sua
      LET arr_patrones[v_ind_pat_sr].v_total_regs         = rec_patrones.v_total_regs
      
      LET v_ind_pat_sr = v_ind_pat_sr + 1 
   END FOREACH
END IF
 
   RETURN v_ind_pat,
          v_ind_pat_sr
END FUNCTION 

#OBJETIVO: Obtener los resultados de la consulta patrones 
FUNCTION fn_consulta_solicitudes(p_nrp,
                                 p_periodo_pago,
                                 p_folio_sua, 
                                 p_diagnostico_rel,
                                 p_diagnostico_srel,
                                 p_folio)
DEFINE p_nrp               CHAR(11),
       p_diagnostico_rel   SMALLINT,
       p_diagnostico_srel  SMALLINT,
       p_periodo_pago      CHAR(6),    
       p_folio_sua         DECIMAL(9,0),
       v_QryTxt            STRING,
       v_ind_det_sol       INTEGER,
       v_ind_det_sol_sr    INTEGER,
       v_periodo_pago      CHAR(6),
       p_folio             DECIMAL(9,0)
DEFINE rec_det_solicitud RECORD
          v_nss   CHAR(11),
          v_id_dpe_referencia DECIMAL(9,0),
          v_aivs_solicitud  DECIMAL(16,6),
          v_pesos_solicitud DECIMAL(16,6),
          v_aivs_parcial    DECIMAL(16,6),
          v_pesos_parcial   DECIMAL(16,6),
          v_aivs_procesar   DECIMAL(16,6),
          v_pesos_procesar  DECIMAL(16,6)
END RECORD 

IF p_diagnostico_rel IS NOT NULL THEN
   LET v_QryTxt  = "\n SELECT a.nss,",       
                   "\n        a.id_dpe_referencia,",
                   "\n        a.avis_viv_dev,",
                   "\n        a.imp_viv_dev,",
                   "\n        c.aivs_viv_dev,",
                   "\n        c.imp_viv_dev",
                   "\n FROM   dpe_sol_trabajador a,",     
                   "\n        dpe_resp_procesar c",     
                   "\n WHERE  a.folio_respuesta = ", p_folio,
                   "\n   AND  a.reg_patronal_imss = ","'",p_nrp,"'",
                   "\n   AND  a.periodo_pago      = ","'",p_periodo_pago,"'",
                   "\n   AND  a.resul_op          = ",p_diagnostico_rel,  
                   "\n   AND  a.folio_respuesta = c.folio",
                   "\n   AND  a.reg_patronal_imss = c.reg_patronal_imss",
                   "\n   AND  a.nss = c.nss ",
                   "\n   AND  a.periodo_pago = c.periodo_pago"

   --DISPLAY v_QryTxt
                   
   PREPARE prp_det_solicitud FROM  v_QryTxt
   DECLARE cur_det_solicitud CURSOR FOR prp_det_solicitud 

   LET v_ind_det_sol = 1   
   FOREACH cur_det_solicitud INTO rec_det_solicitud.v_nss,
                                  rec_det_solicitud.v_id_dpe_referencia,
                                  rec_det_solicitud.v_aivs_solicitud,
                                  rec_det_solicitud.v_pesos_solicitud,
                                  rec_det_solicitud.v_aivs_procesar ,
                                  rec_det_solicitud.v_pesos_procesar


      LET v_QryTxt = "\n SELECT b.avis_viv_dev,",
                     "\n        b.imp_viv_dev",
                     "\n FROM   dpe_sol_trab_parcial b",
                     "\n WHERE  id_dpe_referencia = ", rec_det_solicitud.v_id_dpe_referencia

     PREPARE prp_det_liquidados FROM  v_QryTxt
     EXECUTE prp_det_liquidados INTO  rec_det_solicitud.v_aivs_parcial,
                                      rec_det_solicitud.v_pesos_parcial
 
 
      LET arr_det_solicitud[v_ind_det_sol].v_nss_det   = rec_det_solicitud.v_nss
      LET arr_det_solicitud[v_ind_det_sol].v_aivs_solicitud  = rec_det_solicitud.v_aivs_solicitud
      LET arr_det_solicitud[v_ind_det_sol].v_pesos_solicitud = rec_det_solicitud.v_pesos_solicitud
      
      LET arr_det_solicitud[v_ind_det_sol].v_aivs_parcial    = rec_det_solicitud.v_aivs_parcial    
      LET arr_det_solicitud[v_ind_det_sol].v_pesos_parcial   = rec_det_solicitud.v_pesos_parcial   
                                                             
      LET arr_det_solicitud[v_ind_det_sol].v_aivs_procesar   = rec_det_solicitud.v_aivs_procesar   
      LET arr_det_solicitud[v_ind_det_sol].v_pesos_procesar  = rec_det_solicitud.v_pesos_procesar  

      
      LET v_ind_det_sol = v_ind_det_sol + 1 
   END FOREACH
END IF 

-----------------------------------------------
--CONSULTA DETALLES DE SOLICITUD SIN RELACION--
-----------------------------------------------
IF p_diagnostico_srel IS NOT NULL THEN

   LET v_QryTxt  = "\n SELECT nss_aportacion,",        
                   "\n        num_aplicaciones_inter,",
                   "\n        monto_aplicado",       
                   "\n FROM   dpe_sol_trab_complementario",     
                   "\n WHERE  folio_integra = ", p_folio,
                   "\n   AND  num_reg_pat_imss = ","'",p_nrp,"'",
                   "\n   AND  per_pago      = ","'",p_periodo_pago,"'",
                   "\n   AND  resul_operacion_compl= ",p_diagnostico_srel,
                   "\n GROUP BY 1,2,3",
                   "\n ORDER BY 1,2,3"

   PREPARE prp_det_solicitud_sr FROM v_QryTxt
   DECLARE cur_det_solicitud_sr CURSOR FOR prp_det_solicitud_sr 

   LET v_ind_det_sol_sr = 1   
   FOREACH cur_det_solicitud_sr INTO rec_det_solicitud.v_nss,
                                     rec_det_solicitud.v_aivs_solicitud,
                                     rec_det_solicitud.v_pesos_solicitud

      LET arr_det_solicitud[v_ind_det_sol_sr].v_nss_det   = rec_det_solicitud.v_nss

      LET arr_det_solicitud[v_ind_det_sol_sr].v_aivs_solicitud  = rec_det_solicitud.v_aivs_solicitud
      LET arr_det_solicitud[v_ind_det_sol_sr].v_pesos_solicitud = rec_det_solicitud.v_pesos_solicitud
      
      LET arr_det_solicitud[v_ind_det_sol_sr].v_aivs_parcial    = 0.00
      LET arr_det_solicitud[v_ind_det_sol_sr].v_pesos_parcial   = 0.00
                                                             
      LET arr_det_solicitud[v_ind_det_sol_sr].v_aivs_procesar   = 0.00
      LET arr_det_solicitud[v_ind_det_sol_sr].v_pesos_procesar  = 0.00
      
      LET v_ind_det_sol_sr = v_ind_det_sol_sr + 1

   END FOREACH
END IF 

   RETURN v_ind_det_sol,
          v_ind_det_sol_sr

END FUNCTION 

#OBJETIVO: Llena el combo con los folios a consultar
FUNCTION fn_llena_combo_folio()               
DEFINE var_dis_hist DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_folio      LIKE glo_folio.folio,  -- Campo de Folio
          v_arch_fol   VARCHAR(60)
END RECORD

DEFINE v_cb_folio   LIKE glo_folio.folio,--Almacena folio en el combobox
       v_indice     INTEGER,                   -- Variable del indice  
       v_QryTxt     STRING,                     -- Cadena para almacenar Query
       v_cmbx           ui.ComboBox                 -- Variable de Combobox

   LET v_cmbx = ui.ComboBox.forName("v_cmb_folio") --Asignación del combo a la forma
 
   -- Validación si el combo es nulo 
   IF v_cmbx IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1
   
   LET v_QryTxt = "\n SELECT gf.folio,",
                  "\n        gf.folio||'-'||gc.nombre_archivo,",
                  "\n        gf.f_actualiza",
                  "\n   FROM glo_folio gf,",
                  "\n        glo_ctr_archivo gc",
                  "\n  WHERE gf.proceso_cod IN (1001,1002)",
                  "\n    AND gf.folio = gc.folio",
                  "\n    AND gc.opera_cod = 3 ",
                  "\n  ORDER BY 1 DESC"
DISPLAY v_QryTxt
   -- Prepara la consulta para obtener folios liquidados
   PREPARE prp_folios_dpe FROM v_QryTxt
   
   -- Limpia el combo
   CALL v_cmbx.clear()

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_combo_folio CURSOR FOR prp_folios_dpe
      FOREACH cur_llena_combo_folio INTO var_dis_hist[v_indice].v_folio,
                                         var_dis_hist[v_indice].v_arch_fol
         -- Agrega elementos al combobox
         CALL v_cmbx.addItem(var_dis_hist[v_indice].v_folio,
                             var_dis_hist[v_indice].v_arch_fol)
         LET v_indice = v_indice + 1
      END FOREACH

   CALL var_dis_hist.deleteElement(v_indice)
         DISPLAY v_cb_folio, v_indice
   RETURN v_cb_folio, v_indice
   
END FUNCTION

#OBJETIVO: Genera el reporte de las cifras globales
REPORT rpt_cifras_globales(--Encabezado
                           p_usuario,
                           p_folio_resp_procesar,
                           --Detalles
                           p_regs_acep_rel ,
                           p_regs_rech_rel ,
                           p_regs_pend_rel ,
                           p_aivs_acep_rel ,
                           p_aivs_rech_rel ,
                           p_aivs_pend_rel ,
                           p_regs_acep_srel,
                           p_regs_rech_srel,
                           p_regs_pend_srel,
                           p_aivs_acep_srel,
                           p_aivs_rech_srel,
                           p_aivs_pend_srel,
                           --Totales
                           p_total_regs_rel,
                           p_total_aivs_rel,
                           p_total_regs_srel,
                           p_total_aivs_srel)
                                            
DEFINE p_regs_acep_rel   INTEGER      ,     
       p_regs_rech_rel   INTEGER      ,     
       p_regs_pend_rel   INTEGER      ,     
       p_aivs_acep_rel   DECIMAL(16,6),     
       p_aivs_rech_rel   DECIMAL(16,6),     
       p_aivs_pend_rel   DECIMAL(16,6),     
       p_total_regs_rel  INTEGER      ,     
       p_total_aivs_rel  DECIMAL(16,6),     
       p_regs_acep_srel  INTEGER,           
       p_regs_rech_srel  INTEGER,           
       p_regs_pend_srel  INTEGER,           
       p_aivs_acep_srel  DECIMAL(16,6),     
       p_aivs_rech_srel  DECIMAL(16,6),     
       p_aivs_pend_srel  DECIMAL(16,6),     
       p_total_regs_srel INTEGER      ,     
       p_total_aivs_srel DECIMAL(16,6),
       p_folio_resp_procesar DECIMAL(9,0),
       p_usuario             LIKE seg_modulo.usuario   

DEFINE p_arr_cifras_globales RECORD 
          v_total_regs_rel   INTEGER,
          V_total_aivs_rel   DECIMAL (16,6),
          v_diagnostico_rel  SMALLINT
END RECORD  

DEFINE p_arr_cifras_globales_sr RECORD 
          v_total_regs_srel  INTEGER,
          V_total_aivs_srel  DECIMAL (22,2),
          v_diagnostico_srel SMALLINT
END RECORD 
       
DEFINE v_fecha_reporte DATE

FORMAT       
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED
      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_folio_resp_procesar
      PRINTX p_usuario
      PRINTX p_regs_acep_rel
      PRINTX p_regs_rech_rel   
      PRINTX p_regs_pend_rel   
      PRINTX p_aivs_acep_rel   
      PRINTX p_aivs_rech_rel   
      PRINTX p_aivs_pend_rel   
      PRINTX p_total_regs_rel  
      PRINTX p_total_aivs_rel  
      PRINTX p_regs_acep_srel  
      PRINTX p_regs_rech_srel  
      PRINTX p_regs_pend_srel  
      PRINTX p_aivs_acep_srel  
      PRINTX p_aivs_rech_srel  
      PRINTX p_aivs_pend_srel  
      PRINTX p_total_regs_srel 
      PRINTX p_total_aivs_srel
END REPORT

#OBJETIVO: Genera el reporte con los detalles por NRP 
REPORT rpt_detalles_patrones(p_usuario,
                             p_folio,
                             p_diagnostico_desc,
                             p_arr_patrones) 

DEFINE v_fecha_reporte        DATE,
       p_usuario              LIKE seg_modulo.usuario,
       p_folio                DECIMAL(9,0),
       p_diagnostico_desc     CHAR(30)
DEFINE p_arr_patrones RECORD 
          v_nrp               CHAR(11),      
          v_bimestre_per_pago CHAR(2),       
          v_anio_per_pago     CHAR(4),       
          v_folio_sua         DECIMAL(9,0),  
          v_total_regs        INTEGER,
          v_periodo_pago      CHAR(6)
END RECORD

FORMAT
   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED
      LET p_diagnostico_desc = p_diagnostico_desc CLIPPED

      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_folio
      PRINTX p_usuario
      PRINTX p_diagnostico_desc 

   ON EVERY ROW
      PRINTX p_arr_patrones.v_nrp
      PRINTX p_arr_patrones.v_bimestre_per_pago
      PRINTX p_arr_patrones.v_anio_per_pago
      PRINTX p_arr_patrones.v_folio_sua
      PRINTX p_arr_patrones.v_total_regs
      
END REPORT

#OBJETIVO: Genera el reporte con los detalles de la solicitud en base al NRP seleccionado 
REPORT rpt_detalle_solicitud(p_usuario,
                             p_folio,
                             p_diagnostico_desc,
                             p_nrp,
                             p_folio_sua,
                             p_periodo_pago,
                             p_arr_det_solicitud)
                             
DEFINE p_usuario              LIKE seg_modulo.usuario,
       p_folio                DECIMAL(9,0),
       p_diagnostico          SMALLINT,
       p_diagnostico_desc     CHAR(30),
       p_nrp                  CHAR(11),
       v_fecha_reporte        DATE,
       p_folio_sua            DECIMAL(9,0),
       p_periodo_pago         DECIMAL(9,0)

DEFINE p_arr_det_solicitud  RECORD
          v_nss_det     CHAR(11),
          v_aivs_solicitud  DECIMAL(16,6),
          v_pesos_solicitud DECIMAL(16,6),
          v_aivs_parcial    DECIMAL(16,6),
          v_pesos_parcial   DECIMAL(16,6),
          v_aivs_procesar   DECIMAL(16,6),
          v_pesos_procesar  DECIMAL(16,6)
END RECORD 
FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte    = TODAY CLIPPED
      LET p_diagnostico_desc = p_diagnostico_desc CLIPPED

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_folio
      PRINTX p_usuario
      PRINTX p_nrp
      PRINTX p_diagnostico_desc 
      PRINTX p_folio_sua
      PRINTX p_periodo_pago

   ON EVERY ROW
      PRINTX p_arr_det_solicitud.*
END REPORT 

#OBJETIVO: Genera el reporte de diferencias entre la respuesta de PROCESAR y SACI
REPORT rpt_dif_saci_procesar(p_rec_detalles, p_rec_det_compl)
DEFINE p_usuario              LIKE seg_modulo.usuario,
       p_folio                DECIMAL(9,0),
       p_diagnostico          SMALLINT,
       p_diagnostico_desc     CHAR(30),
       p_nrp                  CHAR(11),
       v_fecha_reporte        DATE,
       p_folio_sua            DECIMAL(9,0),
       p_periodo_pago         DECIMAL(9,0)

--Arreglo de detalles para reporte de PROCESAR VS SACI
DEFINE p_rec_detalles RECORD 
   v_tipo_registro    CHAR(9), --Tipo de registro ACEPTADO, RECHAZADO, PENDIENTE
   v_PROCESAR         CHAR(8),  --Etiqueta de resultados PROCESAR
   v_proc_nrp         CHAR(11), 
   v_proc_nss         CHAR(11),
   v_proc_periodo     CHAR(6),
   v_proc_folio_sua   DECIMAL(9,0),
   v_proc_aivs_dev    DECIMAL(16,2),
   v_proc_pesos       DECIMAL(16,2),
   v_proc_estatus     SMALLINT,
   v_proc_diagnostico SMALLINT,
   v_SACI             CHAR(4),  --Etiqueta de resultados SACI 
   v_saci_nrp         CHAR(11),     
   v_saci_nss         CHAR(11),     
   v_saci_periodo     CHAR(6),      
   v_saci_folio_sua   DECIMAL(9,0), 
   v_saci_aivs_dev    DECIMAL(16,2),
   v_saci_pesos       DECIMAL(16,2),
   v_saci_estatus     SMALLINT,     
   v_saci_diagnostico SMALLINT,     
   v_DIFERENCIAS      CHAR(11), 
   v_dif_aivs_dev     DECIMAL(16,2),
   v_dif_pesos        DECIMAL(16,2)
END RECORD
DEFINE p_rec_det_compl RECORD 
   v_tipo_registro    CHAR(9), --Tipo de registro ACEPTADO, RECHAZADO, PENDIENTE
   v_PROCESAR         CHAR(8),  --Etiqueta de resultados PROCESAR
   v_proc_nrp         CHAR(11), 
   v_proc_nss         CHAR(11),
   v_proc_periodo     CHAR(6),
   v_proc_folio_sua   DECIMAL(9,0),
   v_proc_aivs_dev    DECIMAL(16,2),
   v_proc_pesos       DECIMAL(16,2),
   v_proc_estatus     SMALLINT,
   v_proc_diagnostico SMALLINT,
   v_SACI             CHAR(4),  --Etiqueta de resultados SACI 
   v_saci_nrp         CHAR(11),     
   v_saci_nss         CHAR(11),     
   v_saci_periodo     CHAR(6),      
   v_saci_folio_sua   DECIMAL(9,0), 
   v_saci_aivs_dev    DECIMAL(16,2),
   v_saci_pesos       DECIMAL(16,2),
   v_saci_estatus     SMALLINT,     
   v_saci_diagnostico SMALLINT,     
   v_DIFERENCIAS      CHAR(11), 
   v_dif_aivs_dev     DECIMAL(16,2),
   v_dif_pesos        DECIMAL(16,2)
END RECORD

FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte    = TODAY CLIPPED
      PRINT "1.- REGISTROS CORRESPONDIENTES AL LOTE PROCESADO."
      PRINT "DESCRIPCION|PROCESAR|NRP|NSS|PERIODO|FOLIO_SUA|AIVS_DEVUELTAS|PESOS|ESTATUS|DIAGNOSTICO|SACI|NRP|NSS|PERIODO|FOLIO_SUA|AIVS_DEVUELTAS|PESOS|ESTATUS|CLAVE_DE_DIAGNOSTICO|DIFERENCIAS|AIVS_DEVUELTAS|PESOS|"

   ON EVERY ROW
      PRINT p_rec_detalles.v_tipo_registro CLIPPED
      ||"|"||p_rec_detalles.v_PROCESAR
      ||"|"||p_rec_detalles.v_proc_nrp
      ||"|"||p_rec_detalles.v_proc_nss
      ||"|"||p_rec_detalles.v_proc_periodo
      ||"|"||p_rec_detalles.v_proc_folio_sua   
      ||"|"||p_rec_detalles.v_proc_aivs_dev
      ||"|"||p_rec_detalles.v_proc_pesos
      ||"|"||p_rec_detalles.v_proc_estatus
      ||"|"||p_rec_detalles.v_proc_diagnostico
      ||"|"||p_rec_detalles.v_SACI
      ||"|"||p_rec_detalles.v_saci_nrp
      ||"|"||p_rec_detalles.v_saci_nss
      ||"|"||p_rec_detalles.v_saci_periodo
      ||"|"||p_rec_detalles.v_saci_folio_sua
      ||"|"||p_rec_detalles.v_saci_aivs_dev 
      ||"|"||p_rec_detalles.v_saci_pesos 
      ||"|"||p_rec_detalles.v_saci_estatus
      ||"|"||p_rec_detalles.v_saci_diagnostico
      ||"|"||p_rec_detalles.v_DIFERENCIAS
      ||"|"||p_rec_detalles.v_dif_aivs_dev 
      ||"|"||p_rec_detalles.v_dif_pesos

      PRINT  p_rec_det_compl.v_tipo_registro CLIPPED
      ||"|"||p_rec_det_compl.v_PROCESAR
      ||"|"||p_rec_det_compl.v_proc_nrp
      ||"|"||p_rec_det_compl.v_proc_nss
      ||"|"||p_rec_det_compl.v_proc_periodo
      ||"|"||p_rec_det_compl.v_proc_folio_sua   
      ||"|"||p_rec_det_compl.v_proc_aivs_dev
      ||"|"||p_rec_det_compl.v_proc_pesos
      ||"|"||p_rec_det_compl.v_proc_estatus
      ||"|"||p_rec_det_compl.v_proc_diagnostico
      ||"|"||p_rec_det_compl.v_SACI
      ||"|"||p_rec_det_compl.v_saci_nrp
      ||"|"||p_rec_det_compl.v_saci_nss
      ||"|"||p_rec_det_compl.v_saci_periodo
      ||"|"||p_rec_det_compl.v_saci_folio_sua
      ||"|"||p_rec_det_compl.v_saci_aivs_dev 
      ||"|"||p_rec_det_compl.v_saci_pesos 
      ||"|"||p_rec_det_compl.v_saci_estatus
      ||"|"||p_rec_det_compl.v_saci_diagnostico
      ||"|"||p_rec_det_compl.v_DIFERENCIAS
      ||"|"||p_rec_det_compl.v_dif_aivs_dev 
      ||"|"||p_rec_det_compl.v_dif_pesos      
END REPORT 

#OBJETIVO: Genera el archivo de salida con diferencias entre la respuesta de PROCESAR y SACI
FUNCTION fn_dpe_ejecuta_generacion_archivo(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper    SMALLINT,
       v_mensaje         STRING

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "ARCHIVOAQUI"

   -- se verifica si se puede continuar con la operacion
          LET v_s_comando = " fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DPES06 ",
                             p_usuario_cod, " ",
                             g_pid  , " " ,
                             g_proceso_cod , " " ,
                             g_opera_cod ," ",
                             p_folio
                             
          DISPLAY v_s_comando
          RUN v_s_comando
END FUNCTION