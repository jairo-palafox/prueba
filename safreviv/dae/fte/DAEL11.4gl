--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/10/2013
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEL11                                                   #
#Objetivo          => Programa lanzador para ejecutar la preliquidación manual #
#                     de los registros con estatus de PENDIENTE                #
#Fecha inicio      => 09/10/2013 (dd/mm/yyyy)                                  #
################################################################################

DATABASE safre_viv

GLOBALS "DAEG01.4gl"
GLOBALS

CONSTANT MAX_REGISTROS     SMALLINT = 50
 
DEFINE arr_detalles_dae DYNAMIC ARRAY OF RECORD 
          v_det_folio              DECIMAL(9,0),
          v_det_num_credito        CHAR(10),
          v_det_nss                CHAR(11),
          v_det_fecha_pago         DATE,
          v_det_precio_fondo       DECIMAL(16,6), 
          v_det_importe_amort      DECIMAL(16,6),
          v_det_tot_aivs           DECIMAL(16,6),
          v_det_desc_resul         CHAR(12),
          v_det_origen             CHAR(10),
          v_monto_valida           DECIMAL(16,6),
          v_resultado              CHAR(2),
          v_diagnostica            SMALLINT,
          v_det_id_derechohabiente DECIMAL(9,0),
          v_det_id_referencia      DECIMAL(9,0)
END RECORD

DEFINE arr_motivo_rechazo DYNAMIC ARRAY OF RECORD 
          v_desc_motivo_rch   CHAR(30)
END RECORD

DEFINE v_estado_registro     SMALLINT,
       v_nombre_archivo      CHAR (40), 
       v_i_det               INTEGER,
       v_id_derechohabiente  DECIMAL(9,0),
       v_importe_total_amort DECIMAL(16,6),
       v_monto_total_aivs    DECIMAL(16,6),
       v_total_registros_det INTEGER
      
DEFINE var_dis_hist DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_folio          DECIMAL(9,0),
          v_origen_folio   CHAR(22),
          v_origen_desc    CHAR (10)
END RECORD

DEFINE v_i_rch INTEGER,
       arr_dae_rechazos DYNAMIC ARRAY OF RECORD
          folio         DECIMAL(9,0),
          nss           CHAR(11),
          diagnostico   SMALLINT,
          campo_valor   CHAR(30)
       END RECORD
       
DEFINE v_edit_Folio     LIKE dis_sum_avance_pago.folio, --Recibe el valor del folio a consultar
       v_indice         INTEGER,                        -- Variable del indice
       v_folio_liquida DECIMAL(9,0),
       v_fecha_liquida DATE,
       v_precio_accion DECIMAL(16,6),
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados  LIKE seg_modulo.ruta_listados,
       g_pid            LIKE bat_ctr_proceso.pid, -- ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       v_folio_dictamen DECIMAL(9,0)

DEFINE arr_referencias  DYNAMIC ARRAY OF RECORD 
          v_nss         CHAR(11),
          v_id_dh       DECIMAL(9,0),
          v_id_dae_ref  DECIMAL(9,0)
END RECORD

DEFINE g_reg_modulo   RECORD
          ruta_bin      CHAR(40),
          ruta_rescate  CHAR(40),
          ruta_listados CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
          ruta_listados CHAR(40)
       END RECORD
END GLOBALS 

#OBJETIVO: Controla las principales opciones de consulta de folios
MAIN 
DEFINE 
       v_de_Fecha       DATE,     --Recibe el valor de la fecha a consultar
       p_usuario        LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tipo_proc      CHAR(1),
       p_nombre_menu    LIKE seg_menu.opcion,
       v_valida_fecha   SMALLINT, 
       p_programa       CHAR(10),       
       r_bandera        SMALLINT,
       v_folio_lote     DECIMAL(9,0),
       v_nss            CHAR(11),
       v_estado         CHAR(2),
       v_f_inicial      DATE,
       v_f_final        DATE, 
       v_nombre_arch    STRING,
       v_comando        STRING
DEFINE f_ventana        ui.Window,   -- Define las propìedades de la Ventana
       f_forma          ui.Form,     -- Define las propiedades de la forma
       manejador_rpt    om.SaxDocumentHandler  -- Contenedor de Documentos para el reporte
DEFINE v_inicia           INTEGER,
       v_archivo          STRING,  
       v_nss_consulta     CHAR(11),
       v_tot_sel_aceptados  INTEGER,
       v_tot_sel_rechazados INTEGER,
       bnd_confirma       SMALLINT,
       i                  INTEGER,
       v_i_resultado      INTEGER, 
       v_isam_err         INTEGER, 
       err_txt            CHAR(200),
       v_correcto_integra SMALLINT,
       v_QryTxt           STRING
DEFINE v_inicia_proceso   SMALLINT,
       v_valida_operacion SMALLINT,
       v_valida_opera_dic SMALLINT,
       r_bnd_opera_ini    SMALLINT,
       v_mensaje          STRING,
       r_bnd_fin_oper     SMALLINT

   LET p_programa    = "DAEL011"       
   LET p_usuario     = ARG_VAL(1) -- Recibe la variable de usuario
   LET p_tipo_proc   = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu = ARG_VAL(3) -- Recibe el nombre del programa
   LET g_proceso_cod = ARG_VAL(4) -- Proceso Cod 2400 DAE

   CALL STARTLOG (p_usuario CLIPPED|| ".DAEL11.log")
   
   LET g_proceso_cod = 2402
   LET g_opera_cod = 1

   INITIALIZE v_valida_fecha TO NULL   
   INITIALIZE v_de_Fecha   TO NULL  -- Inicializa la variable fecha a nulo
   INITIALIZE v_edit_Folio TO NULL  -- Inicializa la variable folio a nulo

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'dae'

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

    
--Abre la ventana para ingresar parámetros de consulta   
CLOSE WINDOW SCREEN  
OPEN WINDOW vtn_Dictamen WITH FORM "DAEL110"
DIALOG   ATTRIBUTES(UNBUFFERED) 

   --Se obtienen los parámetros de folio, estado, NSS para consulta
   INPUT v_folio_lote,
         v_nss,
         v_f_inicial,
         v_f_final

   FROM  cb_folio_lote,
         v_ed_nss,
         f_inicial,
         f_final
         
      BEFORE INPUT 
      --Valida que se pueda ejecutar si no hay proceso de DAE activo
      CALL fn_valida_operacion(g_pid,2400,1)
      RETURNING v_valida_operacion

      IF ( v_valida_operacion = 0 ) THEN 
         --Llama función para llenar combo
         CALL fn_llena_combo_folio() 
         RETURNING v_folio_lote, v_nombre_arch

         LET f_ventana = ui.Window.getCurrent()
         LET f_forma = f_ventana.getForm()

         --Se invoca la función que asigna el titulo a la ventana
         CALL ui.Interface.setText(p_nombre_menu)
         CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta la Sección de Detalles
         CALL f_forma.setElementHidden("gr_rechazos", 1) --Oculta la Sección de Detalles         
         CALL f_forma.setElementHidden("regresar", 1) --Oculta el botón regresar
         CALL f_forma.setElementHidden("rechazos", 1) --Oculta el boton de rechazos
      ELSE 
         CALL fn_mensaje("Atención", "Existe un proceso de DAE Activo", "stop")
         EXIT PROGRAM
      END IF    
   END INPUT      

       -- Botón cancel que da salida a la consulta y terminar la captura de los parámetros
      ON ACTION cancelar
         EXIT DIALOG
      -- Botón aceptar que realiza la consulta en base a folio y fecha
      ON ACTION ACCEPT
            --Validaciones de Fecha 
            IF  v_f_inicial IS NOT NULL AND v_f_final IS NULL THEN 
               CALL fn_mensaje("Atención", "Es necesario capturar la fecha final", "stop")
               NEXT FIELD  f_final 
            ELSE 
               IF v_f_final IS NOT NULL AND v_f_inicial IS NULL THEN
                  CALL fn_mensaje("Atención", "Es necesario capturar la fecha inicial", "stop") 
                  NEXT FIELD f_inicial
               ELSE 
               END IF
            END IF

            IF v_f_final < v_f_inicial  THEN 
               CALL fn_mensaje("Atención", "La fecha inicial no puede ser posterior a la fecha final", "stop") 
               NEXT FIELD f_inicial
            END IF

            IF v_f_final > TODAY THEN
               CALL fn_mensaje("Atención", "La fecha final no puede ser posterior a la fecha actual", "stop") 
               NEXT FIELD f_final
            END IF 

            -- Si el folio es válido se llama la función de consulta de detalles 
            CALL fn_consulta_det_amortizacion(v_folio_lote, v_estado,v_nss)
            
            --Si existen registros                  
            IF v_i_det > 1 THEN
               -- Si el folio capturado existe en el histórico ejecuta la consulta
               CALL f_forma.setElementHidden("gr_detalles", 0) --Muestra la Sección de Detalles
               CALL f_forma.setElementHidden("regresar", 0) --Muestra el botón regresar

               INPUT ARRAY arr_detalles_dae WITHOUT DEFAULTS
               FROM scr_detalles.*
               ATTRIBUTES  (INSERT ROW = FALSE,
                            APPEND ROW = FALSE,
                            DELETE ROW = FALSE)

                  ON CHANGE v_diagnostica
                     --Obtiene el total de aceptados
                     IF arr_detalles_dae[ARR_CURR()].v_diagnostica = 1 THEN
                        LET v_tot_sel_aceptados  = v_tot_sel_aceptados + 1
                     END IF
                     --Obtiene el total de rechazados
                     IF arr_detalles_dae[ARR_CURR()].v_diagnostica = 2 THEN
                        LET v_tot_sel_rechazados = v_tot_sel_rechazados + 1 
                     END IF

                     DISPLAY v_tot_sel_aceptados TO ed_tot_sel_aceptados
                     DISPLAY v_tot_sel_rechazados TO ed_tot_sel_rechazados

                  ON ACTION ACCEPT
                  IF v_tot_sel_aceptados = 0 AND v_tot_sel_rechazados = 0 THEN 
                     CALL fn_mensaje ("Atención", "No se ha seleccionado ningún registro", "stop")
                  ELSE 
                     --Valida que se pueda ejecutar si no hay proceso de DAE activo
                     CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING v_valida_opera_dic

                     IF (v_valida_opera_dic = 0) THEN  
                        CALL fn_ventana_confirma("Atención", "¿Desea actualizar los registros seleccionados?", "question")
                        RETURNING bnd_confirma --0 Cancela - 1 Acepta                                 

                        IF bnd_confirma = 1 THEN
                     
                           -- se obtiene el ID del proceso
                           CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario)
                           RETURNING g_pid
                           --Inicia proceso
                           CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"DAEL11","",p_usuario)
                           RETURNING v_inicia_proceso
                              
                           IF ( v_inicia_proceso = 0 ) THEN
                             --   Inicio operacion.
                              CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,v_folio_lote,"DAEL11","",p_usuario)
                              RETURNING r_bnd_opera_ini
                           
                              CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario)
                              RETURNING v_folio_dictamen
                                        
                              --Actualiza Rechazados  x
                              FOR i = 1 TO arr_detalles_dae.getLength() 
                                 IF arr_detalles_dae[i].v_diagnostica = 1 THEN
                                    UPDATE dae_det_solicitud
                                    SET    resul_opera = "01",
                                           folio_dictamen = v_folio_dictamen 
                                    WHERE  resul_opera = "03"
                                    AND    folio = arr_detalles_dae[i].v_det_folio
                                    AND    nss = arr_detalles_dae[i].v_det_nss
                                    AND    id_derechohabiente = arr_detalles_dae[i].v_det_id_derechohabiente
                                    AND    id_dae_referencia = arr_detalles_dae[i].v_det_id_referencia
                              
                                 END IF --Si es aceptado
                                 
                                 IF arr_detalles_dae[i].v_diagnostica = 2 THEN
                                    UPDATE dae_det_solicitud
                                    SET    resul_opera = "02",
                                           motivo_rechazo = 8,
                                           folio_dictamen = v_folio_dictamen 
                                    WHERE  resul_opera = "03"
                                    AND    folio = arr_detalles_dae[i].v_det_folio 
                                    AND    nss = arr_detalles_dae[i].v_det_nss
                                    AND    id_derechohabiente = arr_detalles_dae[i].v_det_id_derechohabiente
                                    AND    id_dae_referencia = arr_detalles_dae[i].v_det_id_referencia
                                    
                                    LET v_QryTxt = "EXECUTE FUNCTION fn_dae_inserta_rechazo(?,?,?)"
                              
                                    PREPARE prp_inserta_rechazo FROM v_QryTxt 
                                    EXECUTE prp_inserta_rechazo USING v_folio_lote,
                                                                      arr_detalles_dae[i].v_det_nss,
                                                                      arr_detalles_dae[i].v_det_id_referencia
                                                                INTO  v_i_resultado,     
                                                                      v_isam_err,        
                                                                      err_txt,
                                                                      v_correcto_integra
                                 END IF  --Si es rechazado
                              END FOR
                              
                              CALL fn_mensaje("Atención", 
                                              "Se han actualizado los registros seleccionados \n puede continuar con la preliquidación", 
                                              "question")
                                              
                              CALL fn_genera_log(v_folio_dictamen, p_usuario)
                              
                              -- se invoca la finalizacion de la operacion
                              --CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                              --RETURNING r_bnd_fin_oper                                              

                              --Finaliza proceso si todo el dictamen es rechazado
                              IF v_tot_sel_aceptados  = 0 THEN
                                 CALL fn_finaliza_proceso()
                              END IF

                              EXIT INPUT   
                              CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta la Sección de Detalles
                              CALL ui.Interface.refresh()                
                              
                           ELSE  
                              CALL fn_recupera_inconsis_opera(v_inicia_proceso) RETURNING v_mensaje
                              CALL fn_mensaje ("Atención", v_mensaje CLIPPED, "stop")
                           END IF --Inicia proceso
                        ELSE
                           EXIT INPUT    
                        END IF --Cancela Confirma 
                     ELSE   
                        CALL fn_recupera_inconsis_opera(v_valida_opera_dic) RETURNING v_mensaje
                        CALL fn_mensaje ("Atención", v_mensaje CLIPPED, "stop")
                        EXIT PROGRAM
                     END IF--SI operación es inválida
                  END IF --Si total regs seleccionados > 0
                  ON ACTION Consulta_Derechohabiente
                    -- se ejecuta la consulta del derechohabiente usando la consulta general
                    LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || p_usuario || "1 'Consulta de Derechohabiente' " || arr_detalles_dae[ARR_CURR()].v_det_nss
                    RUN v_comando
            
                  ON ACTION edo_cuenta
                     LET v_nss_consulta = arr_detalles_dae[ARR_CURR()].v_det_nss
            
                     SELECT id_derechohabiente
                     INTO   v_id_derechohabiente
                     FROM   afi_derechohabiente
                     WHERE  nss = arr_detalles_dae[ARR_CURR()].v_det_nss

                     CALL fn_pantalla_edo_cuenta(v_nss_consulta) RETURNING v_archivo
                     
                  ON ACTION reporte
                     IF fgl_report_loadCurrentSettings("DAEC010.4rp") THEN
                        CALL fgl_report_selectDevice ("PDF")
                        LET manejador_rpt = fgl_report_commitCurrentSettings()
                     END IF
            
                     CALL fn_consulta_rechazos(arr_detalles_dae[ARR_CURR()].v_det_folio, "") 
                     
                     --Inicia el reporte de detalles PATRONES
                     START REPORT rpt_detalles_amortizacion TO XML HANDLER manejador_rpt
                    
                        FOR v_inicia = 1 TO v_i_det
                           OUTPUT TO REPORT rpt_detalles_amortizacion (p_usuario,
                                                                       arr_detalles_dae[v_inicia].*,
                                                                       arr_motivo_rechazo[v_inicia].v_desc_motivo_rch,
                                                                       v_total_registros_det, 
                                                                       v_importe_total_amort,
                                                                       v_monto_total_aivs
                                                                       )
                        END FOR
                     FINISH REPORT rpt_detalles_amortizacion 
            
                  ON ACTION CANCEL
                     EXIT PROGRAM
                     
                  ON ACTION regresar
                     CALL f_forma.setElementHidden("gr_detalles", 1) --Oculta detalles
                     CALL arr_detalles_dae.clear()                           
                     EXIT INPUT

                END INPUT
            ELSE 
               CALL fn_mensaje("Atención","No se econtraron registros con estatus PENDIENTE","information")
               NEXT FIELD cb_folio_lote
            END IF    
 END DIALOG 
CLOSE WINDOW vtn_Dictamen
END MAIN  

#OBJETIVO: Realiza consulta de detalles de solicitudes de DAE
FUNCTION fn_consulta_det_amortizacion(p_folio,p_estado, p_nss)
DEFINE p_folio    DECIMAL (9,0),
       p_estado   SMALLINT, 
       p_nss      CHAR(11),
       v_QryTxt   STRING,
       v_QryTxt_1 STRING
DEFINE rec_detalles_dae RECORD 
          v_det_folio         DECIMAL(9,0),
          v_det_num_credito   CHAR(10),
          v_det_nss           CHAR(11),
          v_det_fecha_pago    DATE,
          v_det_importe_amort DECIMAL(16,6),
          v_det_resul_opera   CHAR(2),
          v_det_estado        SMALLINT,
          v_det_origen        CHAR(10),
          v_det_desc_resul    CHAR(12),
          v_desc_motivo_rch   CHAR(33),
          v_det_total_imp_amort DECIMAL(16,6),
          v_det_dae_id_referencia  DECIMAL(9,0),
          v_det_id_derechohabiente DECIMAL(9,0),
          v_det_folio_liquida DECIMAL(9,0),
          v_det_total_aivs    DECIMAL(16,2),
          v_det_precio_fondo  DECIMAL(16,6),
          v_det_monto_valida  DECIMAL(16,6),
          v_diagnostica       SMALLINT
END RECORD      

   LET v_QryTxt = "\n SELECT a.folio,",
                  "\n        a.num_credito,",
                  "\n        a.nss,",
                  "\n        a.fecha_pago,",
                  "\n        a.importe_amort,",
                  "\n        a.resul_opera,",
                  "\n        a.id_origen,",
                  "\n        a.id_dae_referencia,",
                  "\n        a.id_derechohabiente,",
                  "\n        a.folio_liquida,",
                  "\n        a.monto_aivs,",
                  "\n        b.monto_valida,",
                  "\n        '' ",
                  "\n FROM   dae_det_solicitud a, ",
                  "\n        dae_ctr_archivo_montos b",
                  "\n WHERE  a.resul_opera = 3",
                  "\n AND    b.folio = a.folio" 

   IF p_folio IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  a.folio = ",p_folio
   END IF 

   IF p_nss IS NOT NULL THEN
      LET v_QryTxt = v_QryTxt || "\n AND  a.nss = ",p_nss 
   END IF 

      LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12",
                                 "\n ORDER BY 6,2,3"
  --DISPLAY v_QryTxt
   PREPARE prp_dae_detalles FROM v_QryTxt
   DECLARE cur_dae_detalles CURSOR FOR prp_dae_detalles 

   LET v_i_det = 1
   LET v_importe_total_amort = 0
   LET v_monto_total_aivs = 0

   INITIALIZE arr_detalles_dae TO NULL
   INITIALIZE  rec_detalles_dae.v_det_total_aivs TO NULL
   
   FOREACH cur_dae_detalles  INTO rec_detalles_dae.v_det_folio,
                                  rec_detalles_dae.v_det_num_credito,
                                  rec_detalles_dae.v_det_nss,
                                  rec_detalles_dae.v_det_fecha_pago,
                                  rec_detalles_dae.v_det_importe_amort,
                                  rec_detalles_dae.v_det_resul_opera,
                                  rec_detalles_dae.v_det_estado,
                                  rec_detalles_dae.v_det_dae_id_referencia,
                                  rec_detalles_dae.v_det_id_derechohabiente,
                                  rec_detalles_dae.v_det_folio_liquida,
                                  rec_detalles_dae.v_det_total_aivs,
                                  rec_detalles_dae.v_det_monto_valida,
                                  rec_detalles_dae.v_diagnostica

      SELECT precio_fondo
      INTO   rec_detalles_dae.v_det_precio_fondo
      FROM   glo_valor_fondo
      WHERE  fondo = 11
      AND    f_valuacion = rec_detalles_dae.v_det_fecha_pago

      IF rec_detalles_dae.v_det_estado = 2 THEN
         SELECT nss 
         INTO   rec_detalles_dae.v_det_nss
         FROM   afi_derechohabiente 
         WHERE  id_derehcohabiente = rec_detalles_dae.v_det_id_derechohabiente
      END IF 

      --Determina la descripción del Orígen   
      IF rec_detalles_dae.v_det_estado = 1 THEN
         LET rec_detalles_dae.v_det_origen = "CARTERA" 
      ELSE 
         LET rec_detalles_dae.v_det_origen = "DISPERSIÓN"
      END IF 
      --Determina la descripción del Estado
      IF rec_detalles_dae.v_det_resul_opera = "01" OR 
         rec_detalles_dae.v_det_resul_opera = 1 THEN 
         LET rec_detalles_dae.v_det_desc_resul = "ACEPTADO"

         IF rec_detalles_dae.v_det_folio_liquida IS NULL THEN 
            LET rec_detalles_dae.v_det_folio_liquida = 0
         END IF    
      ELSE
         IF rec_detalles_dae.v_det_resul_opera = "02" OR 
            rec_detalles_dae.v_det_resul_opera = 2 THEN 
            LET rec_detalles_dae.v_det_desc_resul = "RECHAZADO"

           LET v_QryTxt_1 = "\n SELECT a.campo_valor",
                            "\n FROM   dae_rch_archivo a,",
                            "\n        dae_det_solicitud c",
                            "\n WHERE  a.folio = ",rec_detalles_dae.v_det_folio,
                            "\n AND    a.id_dae_referencia = c.id_dae_referencia",
                            "\n AND    a.id_dae_referencia = ", rec_detalles_dae.v_det_dae_id_referencia

           PREPARE prp_dae_det_rch FROM v_QryTxt_1
           EXECUTE prp_dae_det_rch INTO rec_detalles_dae.v_desc_motivo_rch
        ELSE
           LET rec_detalles_dae.v_det_desc_resul = "PENDIENTE"
        END IF
      END IF

      LET arr_detalles_dae[v_i_det].v_det_folio          = rec_detalles_dae.v_det_folio        
      LET arr_detalles_dae[v_i_det].v_det_num_credito    = rec_detalles_dae.v_det_num_credito  
      LET arr_detalles_dae[v_i_det].v_det_nss            = rec_detalles_dae.v_det_nss          
      LET arr_detalles_dae[v_i_det].v_det_fecha_pago     = rec_detalles_dae.v_det_fecha_pago   
      LET arr_detalles_dae[v_i_det].v_det_importe_amort  = rec_detalles_dae.v_det_importe_amort
      LET arr_detalles_dae[v_i_det].v_det_desc_resul     = rec_detalles_dae.v_det_desc_resul   
      LET arr_detalles_dae[v_i_det].v_det_origen         = rec_detalles_dae.v_det_origen       
      LET arr_detalles_dae[v_i_det].v_resultado          = rec_detalles_dae.v_det_resul_opera
      LET arr_detalles_dae[v_i_det].v_det_tot_aivs       = rec_detalles_dae.v_det_total_aivs
      LET arr_detalles_dae[v_i_det].v_det_precio_fondo   = rec_detalles_dae.v_det_precio_fondo
      LET arr_detalles_dae[v_i_det].v_diagnostica        = rec_detalles_dae.v_diagnostica
      LET arr_detalles_dae[v_i_det].v_monto_valida       = rec_detalles_dae.v_det_monto_valida
      LET arr_detalles_dae[v_i_det].v_det_id_derechohabiente = rec_detalles_dae.v_det_id_derechohabiente
      LET arr_detalles_dae[v_i_det].v_det_id_referencia  = rec_detalles_dae.v_det_dae_id_referencia
      LET arr_referencias[v_i_det].v_id_dae_ref          = rec_detalles_dae.v_det_dae_id_referencia 

      IF rec_detalles_dae.v_det_resul_opera = "01" THEN 
         LET arr_motivo_rechazo[v_i_det].v_desc_motivo_rch = "N/A" 
      ELSE 
         LET arr_motivo_rechazo[v_i_det].v_desc_motivo_rch = rec_detalles_dae.v_desc_motivo_rch
      END IF  

      LET v_i_det = v_i_det + 1
      --OBTIENE CIFRAS GLOBALES
      --Total de registros
      LET v_total_registros_det = v_i_det
      --Total de pesos
      LET v_importe_total_amort = v_importe_total_amort + rec_detalles_dae.v_det_importe_amort
      --Total de AIVS
      LET v_monto_total_aivs = v_monto_total_aivs + rec_detalles_dae.v_det_total_aivs

      IF v_i_det > MAX_REGISTROS THEN
         LET v_total_registros_det = MAX_REGISTROS
         CALL fn_mensaje("Atención",
                         "Acotar mas el criterio de búsqueda. \n"||
                         "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                         "about")
         EXIT FOREACH           
      END IF

      
   END FOREACH
   
   CALL arr_detalles_dae.deleteElement(v_i_det)

   LET v_total_registros_det = v_total_registros_det - 1 

   DISPLAY v_total_registros_det TO  ed_tot_regs
   DISPLAY v_importe_total_amort TO  ed_tot_amortizacion
   DISPLAY v_monto_total_aivs    TO  ed_tot_aivs

END FUNCTION  

#OBJETIVO: Llenar el combo con los folios a consultar
FUNCTION fn_llena_combo_folio()               
DEFINE v_cb_folio   LIKE glo_folio.folio,--Almacena folio en el combobox
       v_QryTxt     STRING,                     -- Cadena para almacenar Query
       v_cmbx           ui.ComboBox                 -- Variable de Combobox

   LET v_cmbx = ui.ComboBox.forName("cb_folio_lote") --Asignación del combo a la forma
 
   -- Validación si el combo es nulo 
   IF v_cmbx IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1
   
   LET v_QryTxt = "\n SELECT folio,",
                  "\n        id_origen",
                  "\n FROM   dae_det_solicitud",
                  "\n WHERE  resul_opera = '03'", --Solo pendientes
                  "\n GROUP BY 1,2 ",
                  "\n ORDER BY 1,2 "
--DISPLAY v_QryTxt                 
   -- Prepara la consulta para obtener folios liquidados
   PREPARE prp_folios_dpe FROM v_QryTxt
   
   -- Limpia el combo
   CALL v_cmbx.clear()

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_combo_folio CURSOR FOR prp_folios_dpe
      FOREACH cur_llena_combo_folio INTO var_dis_hist[v_indice].v_folio,
                                         var_dis_hist[v_indice].v_origen_folio

         IF var_dis_hist[v_indice].v_origen_folio = 1 THEN 
            LET var_dis_hist[v_indice].v_origen_desc = "CARTERA" 
         ELSE 
            LET var_dis_hist[v_indice].v_origen_desc = "DISPERSION"
         END IF    

         -- Agrega elementos al combobox
         CALL v_cmbx.addItem(var_dis_hist[v_indice].v_folio,
                             var_dis_hist[v_indice].v_folio ||'-'|| var_dis_hist[v_indice].v_origen_desc)

         LET v_indice = v_indice + 1
      END FOREACH

   CALL var_dis_hist.deleteElement(v_indice)
         
   RETURN v_cb_folio, v_indice
   
END FUNCTION

#OBJETIVO: Consultar el detalle del rechazo en base al NSS y Folio
FUNCTION fn_consulta_rechazos(p_folio, p_nss)
DEFINE p_folio DECIMAL(9,0), 
       p_nss   CHAR(11),
       v_QryTxt     STRING

    LET v_QryTxt = "\n SELECT a.folio,",
                  "\n         b.nss,",
                  "\n         a.diagnostico,",
                  "\n         a.campo_valor",
                  "\n FROM    dae_rch_archivo a,",
                  "\n         afi_derechohabiente b,",
                  "\n         dae_det_solicitud c",
                  "\n WHERE   a.folio = ", p_folio, 
                  "\n AND     a.id_dae_referencia = c.id_dae_referencia   ",
                  "\n AND     b.id_derechohabiente = c.id_derechohabiente "

   IF p_nss IS NOT NULL THEN
      LET  v_QryTxt = v_QryTxt || "\n AND    c.nss = ", "'", p_nss ,"'"
   ELSE 
      LET  v_QryTxt = v_QryTxt || "\n AND    c.motivo_rechazo = 1"
   END IF    

   --DISPLAY v_QryTxt
       
   LET v_i_rch = 1
   
   INITIALIZE arr_dae_rechazos TO NULL
   
   PREPARE prp_consulta_rechazo FROM v_QryTxt
   DECLARE cur_consulta_rechazo CURSOR FOR prp_consulta_rechazo  
   FOREACH cur_consulta_rechazo INTO arr_dae_rechazos[v_i_rch].*

      IF arr_dae_rechazos[v_i_rch].folio IS NULL THEN
         LET  arr_dae_rechazos[v_i_rch].folio = p_folio
         LET  arr_dae_rechazos[v_i_rch].campo_valor = "EL NSS NO EXISTE EN SACI"
      END IF
      LET v_i_rch = v_i_rch + 1
   END FOREACH

   CALL arr_dae_rechazos.deleteElement(v_i_det)
   
END FUNCTION

#OBJETIVO: Obtener el precio de la acción por día
FUNCTION fn_precio_x_dia(p_folio)
DEFINE p_folio         DECIMAL(9,0)

   SELECT folio, 
          f_actualiza
   INTO   v_folio_liquida, 
          v_fecha_liquida
   FROM   glo_folio
   WHERE  folio_referencia = p_folio
   AND    proceso_cod = 2400  

   SELECT precio_fondo
   INTO   v_precio_accion
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = v_fecha_liquida;
 
   IF v_fecha_liquida = "12/31/1899" THEN 
      LET v_fecha_liquida = ""
   END IF   
  
END FUNCTION 

#OBJETIVO: Consultar el detalle del rechazo en base al NSS y Folio
REPORT rpt_detalles_amortizacion (p_usuario,
                                  p_arr_detalles_dae,
                                  p_arr_dae_rechazos,
                                  p_total_registros_det, 
                                  p_importe_total_amort,
                                  p_monto_total_aivs) 

DEFINE v_fecha_reporte        DATE,
       p_usuario              LIKE seg_modulo.usuario,
       p_folio_lote           DECIMAL(9,0),
       p_total_registros_det  INTEGER, 
       p_importe_total_amort  DECIMAL(16,6),
       p_monto_total_aivs     DECIMAL(16,6)

DEFINE p_arr_detalles_dae RECORD 
          v_det_folio         DECIMAL(9,0),
          v_det_num_credito   CHAR(10),
          v_det_nss           CHAR(11),
          v_det_fecha_pago    DATE,
          v_det_precio_fondo  DECIMAL(16,6),
          v_det_importe_amort DECIMAL(16,6),
          v_det_tot_aivs      DECIMAL(16,6),
          v_det_desc_resul    CHAR(12),
          v_det_origen        CHAR(10),
          v_det_monto_valida  DECIMAL(16,6),
          v_resultado         CHAR(2),
          v_diagnostica       SMALLINT,
          v_id_derechohabiente DECIMAL(9,0),
          v_det_id_referencia DECIMAL(9,0)
END RECORD

DEFINE p_arr_dae_rechazos RECORD 
          campo_valor CHAR(30)
END RECORD 

FORMAT

   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED
      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario
      PRINTX p_arr_detalles_dae.v_det_origen

   ON EVERY ROW
      PRINTX p_arr_detalles_dae.v_det_folio
      PRINTX p_arr_detalles_dae.v_det_num_credito
      PRINTX p_arr_detalles_dae.v_det_nss
      PRINTX p_arr_detalles_dae.v_det_fecha_pago USING "dd-mm-yyyy" 
      PRINTX p_arr_detalles_dae.v_det_precio_fondo
      PRINTX p_arr_detalles_dae.v_det_importe_amort
      PRINTX p_arr_detalles_dae.v_det_tot_aivs
      PRINTX p_arr_detalles_dae.v_det_desc_resul
      PRINTX p_arr_dae_rechazos.campo_valor

   ON LAST ROW 
      PRINTX p_total_registros_det
      PRINTX p_importe_total_amort
      PRINTX p_monto_total_aivs

END REPORT

#OBJETIVO: Finaliza todo el proceso en caso de que no se encuentre con ningún registro Aceptado
FUNCTION fn_finaliza_proceso()

   UPDATE bat_ctr_operacion
   SET  estado_cod = 4,
        fecha_ini = CURRENT,
        fecha_fin = CURRENT
   WHERE pid = g_pid
   AND proceso_cod = 2402
   AND   opera_cod IN (2,3) ;

   UPDATE bat_ctr_proceso
   SET    estado_cod = 4,
          fecha_fin = CURRENT
   WHERE  pid = g_pid
   AND    proceso_cod = 2402; 
          
END FUNCTION

#OBJETIVO: Finaliza todo el proceso en caso de que no se encuentre con ningún registro Aceptado
FUNCTION fn_genera_log(p_folio_dictamen, p_usuario)
DEFINE p_folio_dictamen DECIMAL(9,0),
       p_usuario        CHAR(20),
       v_s_comando      STRING    

       LET v_nombre_archivo = "N/A"
       
       LET v_s_comando = " nohup time fglrun ",v_ruta_ejecutable CLIPPED,"/DAEP06 ",
                           p_usuario, " ",
                           g_pid  , " " ,
                           g_proceso_cod , " " ,
                           g_opera_cod ," ",
                           p_folio_dictamen ," ",--
                           v_nombre_archivo ," ",
                           p_folio_dictamen ," ",
                           " 1>",v_ruta_listados clipped ,
                           "/nohup:",g_pid        USING "&&&&&",":",
                           g_proceso_cod USING "&&&&&",":",
                           g_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"

            DISPLAY v_s_comando
            RUN v_s_comando
END FUNCTION