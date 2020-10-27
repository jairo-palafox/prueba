################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 07/03/2019                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC30                                                    #
#Objetivo         => Realizar la consulta histórica de los avances de pago,    #
#                    ESPECIAL donde se podrá identificar el estado             #
#                    del avance de pago.                                       #
#Fecha de Inicio  => 19/10/2015                                                #
################################################################################
--DATABASE safre_viv
SCHEMA safre_viv

GLOBALS 
DEFINE g_folio               DECIMAL(9,0),   --Almacena el Folio capturado  
       g_periodo             CHAR(6),        --Almacena el periodo de pago
       g_estado_avance       LIKE dis_det_avance_pago.estado, --Almacena el estado seleccionado
       g_usuario             LIKE seg_usuario.usuario_cod,    --Clave de usuario
       v_QryTxt              STRING,         --Cadena para consultas
       v_ind_detalle         DECIMAL(10,0),  --Índice del arreglo de detalle 
       v_ind_resumen         INTEGER,        --Índice del arreglo de resumen
       g_id_arr              DECIMAL(10,0),  --Índice de fila seleccionada
       g_tpo_enc             DECIMAL(10,0),  --Agrupa arreglos para reporte
       v_ind_parametros      DECIMAL(10,0),
       v_totales_monto_apo   DECIMAL(22,2),
       v_totales_monto_amo   DECIMAL(22,2),
       v_ind_canc_par        SMALLINT
       
DEFINE g_arr_parametros      DYNAMIC ARRAY OF RECORD 
       r_folio               DECIMAL(10,0), --Folio que se regresa  
       r_periodo             DECIMAL(10,0), --Periodo que se regresa
       r_estado              DECIMAL(10,0), --Estado que devuelve
       r_des_edo             CHAR(08),
       r_tot_reg             BIGINT
END RECORD

DEFINE g_arr_detalles_avpag  DYNAMIC ARRAY OF RECORD 
  v_folio                    LIKE dis_det_avance_pago.folio,
  v_nss                      LIKE afi_derechohabiente.nss,
  v_num_credito              LIKE dis_det_avance_pago.num_credito,
  v_periodo_pago             LIKE dis_det_avance_pago.periodo_pago,
  v_f_pago                   LIKE dis_det_avance_pago.f_pago,
  v_nrp                      LIKE dis_det_avance_pago.nrp,
  v_monto_aportacion         LIKE dis_det_avance_pago.monto_aportacion,
  v_monto_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion,
  v_estado_des               CHAR(52),
  v_id_dh                    LIKE dis_det_avance_pago.id_derechohabiente,
  v_estado                   INTEGER,
  v_id_avance_pagos          DECIMAL(9,0)
END RECORD

DEFINE g_arr_resumen         DYNAMIC ARRAY OF RECORD
  v_folio_pago               LIKE dis_compensa_avance.folio_pago,
  v_nss_resumen              CHAR(11),
  v_monto_apo_pag            LIKE dis_compensa_avance.monto_apo_pag,
  v_monto_amo_pag            LIKE dis_compensa_avance.monto_amo_pag,
  v_monto_dif_apo            LIKE dis_compensa_avance.monto_apo_pag,
  v_monto_dif_amo            LIKE dis_compensa_avance.monto_amo_pag,
  v_edo_compensa_apo         CHAR(52),
  v_edo_compensa_amo         CHAR(52),   
  v_estado_avance            CHAR(52),
  v_estado_cred              CHAR(52)
END RECORD

DEFINE v_valida_folio        LIKE dis_det_avance_pago.folio,        --Almacena el Folio si es valido  
       v_valida_periodo      LIKE dis_det_avance_pago.periodo_pago, --Almacena el periodo si es valido
       v_valida_estado       LIKE dis_det_avance_pago.estado,       --Almacena el estado valido
       v_valida_nss          LIKE dis_det_avance_pago.nrp,          --Almacena el nss válido
       v_estado_desc         CHAR(07),
       cons_parametros_fpe   STRING,
       v_valida_nrp          CHAR(11),
       v_valida_id_ava_pag   DECIMAL(9,0)

DEFINE v_nombre_completo     VARCHAR (100) --Nombre del derechohabiente

DEFINE 
  f_folio                    LIKE dis_det_avance_pago.folio,
  f_periodo                  INTEGER,
  f_nss                      LIKE afi_derechohabiente.nss,
  cb_estado_avance           LIKE glo_folio.folio--Almacena folio en el combobox

DEFINE
  g_sql_txt                  STRING,
  v_proc_entra               SMALLINT,
  v_proc_val                 SMALLINT,
  v_cod_conv                 SMALLINT,
  v_desc_proc_val            CHAR(40),
  v_mensaje_val              STRING

DEFINE r_valida_nss          SMALLINT
DEFINE v_msj_alerta          STRING --Mensaje de alerta para consultas rojas

DEFINE g_canc_par            DYNAMIC ARRAY OF RECORD
       ava_orig_apo          DECIMAL(12,2),
       ava_orig_amo          DECIMAL(12,2),
       ava_canc_par_apo      DECIMAL(12,2),
       ava_canc_par_amo      DECIMAL(12,2),
       ava_pag_fin_apo       DECIMAL(12,2),
       ava_pag_fin_amo       DECIMAL(12,2),
       pag_pat_apo           DECIMAL(12,2),
       pag_pat_amo           DECIMAL(12,2),
       ajuste_sua_apo        DECIMAL(12,2),
       ajuste_sua_amo        DECIMAL(12,2),
       sdo_rem_apo           DECIMAL(12,2),
       sdo_rem_amo           DECIMAL(12,2),
       bimestre              CHAR(6),
       num_credito           DECIMAL(10,0),
       f_cancelacion         DATE,
       fol_cancelacion       DECIMAL(9,0)
       END RECORD

END GLOBALS

MAIN 
DEFINE p_tipo_proc           CHAR(1),              --Tipo de proceso
       p_nombre_menu         LIKE seg_menu.opcion, --Nombre del programa tomado del menú
       p_proceso_cod         LIKE cat_proceso.proceso_cod, --Código del proceso
       p_programa            CHAR(10),             --Nombre del programa que se ejecuta
       r_bnd_periodo         INTEGER,
       v_periodo_cvt         VARCHAR(6),
       v_qwery_ibx           STRING  
       
DEFINE f_ventana             ui.Window,  --Define las propìedades de la Ventana
       f_forma               ui.Form     --Define las propiedades de la forma

  LET p_programa    = "DISC30"   --Programa que se ejecuta       
  LET g_usuario     = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) --Recibe el nombre del programa
  LET p_proceso_cod = 902        --Código de proceso

  LET cons_parametros_fpe = ""
  LET r_bnd_periodo       = 0

  --DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
  CONNECT TO "safre_viv"
  
  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance

  CALL fn_alertamiento()
  RETURNING v_msj_alerta
  DISCONNECT CURRENT
  
  CLOSE WINDOW SCREEN 

  OPEN WINDOW v_cons_historica WITH FORM "DISC301"
    IF ( p_nombre_menu IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_nombre_menu)
    END IF

    LET f_ventana = ui.Window.getCurrent()
    LET f_forma   = f_ventana.getForm()

    CALL f_forma.setElementHidden("gr_det_avpag", 1)  --Oculta la Sección Detalle Registro Av. Pagos
    CALL f_forma.setElementHidden("gr_res_pag", 1)    --Oculta la sección de Resúmen de Registro Avance Pagos
    CALL f_forma.setElementHidden("gr_detalle_canc_pav", 1) --Oculta la Sección Detalle Canc Parcial Ava Pag
    CALL f_forma.setFieldHidden("v_nombre", TRUE )    --Oculta el nombre del trabajador
    CALL f_forma.setElementHidden("lb_nombre", TRUE ) --Oculta el nombre del trabajador
    CALL f_forma.setElementHidden("btn_reporte", 1) --Oculta el boton Reporte del detalle
    CALL f_forma.setElementHidden("btn_salir",1)  --Oculta botón Salir del detalle

    CONNECT TO "safre_viv"
    --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
    LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                    "       a.cod_proceso_valida,  ",
                    "       b.proceso_desc         ",
                    "FROM   cat_convivencia_dis a, ",
                    "       cat_proceso b          ",
                    "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
                    "AND    a.cod_proceso_valida = b.proceso_cod ",
                    "AND    a.cod_convivencia    = 0             ",
                    "ORDER BY cod_proceso_valida   "
    PREPARE ps_val_proc FROM g_sql_txt
    DECLARE cur_val_proc CURSOR FOR ps_val_proc
    FOREACH cur_val_proc INTO v_proc_entra,
                              v_proc_val,
                              v_desc_proc_val
      IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
         LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
         MENU "No se puede ejecutar" 
           ATTRIBUTES ( STYLE="dialog",
           COMMENT= v_mensaje_val,
           IMAGE="information" )

           ON ACTION salir
              RETURN
         END MENU
      END IF
    END FOREACH
    DISCONNECT CURRENT

    INPUT BY NAME f_nss WITHOUT DEFAULTS
    ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

      --Sale de la consulta sin hacer nada 
      ON ACTION CANCEL           
         EXIT INPUT

      --Acciones del botón ACEPTAR
      ON ACTION ACCEPT
         LET cons_parametros_fpe = ""
         
         IF f_nss IS NULL THEN 
            CALL fn_mensaje ("ATENCION", "Debe capturar el NSS", "stop")
         ELSE
            --Valida si el estado es capturado
            IF f_nss IS NOT NULL THEN
               CONNECT TO "safre_viv"
               CALL fn_valida_nss()
               DISCONNECT CURRENT

               IF r_valida_nss = 1 THEN
                  CALL fn_mensaje("Atención", v_msj_alerta, "stop")
               END IF
                 
               LET cons_parametros_fpe = cons_parametros_fpe , "\n AND ad.nss = '",f_nss, "'"
            END IF
              
            --Valida que el folio capturado exista en la tabla de historico
            CONNECT TO "safre_viv"
            CALL fn_valida_folio_periodo_estado(cons_parametros_fpe) 
            RETURNING v_ind_parametros
            DISCONNECT CURRENT

            --Si los parámetros capturados existen y son válidos 
            IF v_ind_parametros >= 2 THEN
               CONNECT TO "safre_viv"              
               CALL fn_consulta_detalle(cons_parametros_fpe) 
               RETURNING v_ind_detalle

               LET v_valida_folio      = g_arr_detalles_avpag[ARR_CURR()].v_folio   
               LET v_valida_periodo    = g_arr_detalles_avpag[ARR_CURR()].v_periodo_pago            
               LET v_valida_estado     = g_arr_detalles_avpag[ARR_CURR()].v_estado
               LET v_valida_nss        = g_arr_detalles_avpag[ARR_CURR()].v_nss
               LET v_valida_nrp        = g_arr_detalles_avpag[ARR_CURR()].v_nrp
               LET v_valida_id_ava_pag = g_arr_detalles_avpag[ARR_CURR()].v_id_avance_pagos

               --DISPLAY "Id -- ",g_arr_detalles_avpag[ARR_CURR()].v_id_dh
               CALL fn_obtiene_nombre_derechohabiente(g_arr_detalles_avpag[ARR_CURR()].v_id_dh)
               DISCONNECT CURRENT
               
               IF v_valida_estado = 30 THEN 
                  LET v_estado_desc = "ABIERTO"
               END IF 

               IF v_valida_estado = 50 THEN 
                  LET v_estado_desc = "CERRADO"
               END IF
             ELSE
                IF v_ind_parametros <= 1 THEN
                   CALL fn_mensaje("ATENCION","No existe información para mostrar","about")
                   LET cons_parametros_fpe = ""
                   LET f_folio             = ""
                   LET f_periodo           = ""
                   LET cb_estado_avance    = ""
                   LET f_nss               = ""
                   CLEAR FORM
                   NEXT FIELD f_nss
                END IF
             END IF
            
             --Si los parámetros de búsqueda no existen 
             IF v_ind_detalle <= 1 THEN 
                CALL fn_mensaje("ATENCION","No existe información para mostrar","about")
                LET cons_parametros_fpe = ""
                LET f_folio             = ""
                LET f_periodo           = ""
                LET cb_estado_avance    = ""
                LET f_nss               = ""
                CLEAR FORM
                --DISPLAY BY NAME f_folio,f_periodo,cb_estado_avance,f_nss
                --DISPLAY BY NAME f_nss
                NEXT FIELD f_nss
             ELSE 
                IF v_ind_detalle >= 1 THEN                        
                   DISPLAY ARRAY g_arr_detalles_avpag TO scr_detalles.*
                   ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
                     BEFORE DISPLAY 
                       CALL f_forma.setElementHidden("gr_det_avpag", 0)        --Muestra la Sección Detalle Registro Av. Pagos
                       CALL f_forma.setFieldHidden("v_nombre", FALSE )         --Oculta el nombre del trabajador
                       CALL f_forma.setElementHidden("lb_nombre", FALSE )      --Oculta el nombre del trabajado
                       CALL f_forma.setElementHidden("btn_reporte", 0) 
                       CALL f_forma.setElementHidden("btn_salir", 0)
                
                     BEFORE ROW
                       LET v_valida_folio      = g_arr_detalles_avpag[ARR_CURR()].v_folio   
                       LET v_valida_periodo    = g_arr_detalles_avpag[ARR_CURR()].v_periodo_pago            
                       LET v_valida_estado     = g_arr_detalles_avpag[ARR_CURR()].v_estado
                       LET v_valida_nss        = g_arr_detalles_avpag[ARR_CURR()].v_nss
                       LET v_valida_nrp        = g_arr_detalles_avpag[ARR_CURR()].v_nrp
                       LET v_valida_id_ava_pag = g_arr_detalles_avpag[ARR_CURR()].v_id_avance_pagos

                       CONNECT TO "safre_viv"
                       --Consulta informacion del resumen
                       CALL fn_consulta_resumen(g_arr_detalles_avpag[ARR_CURR()].v_id_dh, 
                                                 v_valida_id_ava_pag) 
                       RETURNING v_ind_resumen

                       CALL fn_obtiene_nombre_derechohabiente(g_arr_detalles_avpag[ARR_CURR()].v_id_dh)
                       DISCONNECT CURRENT

                       IF v_ind_resumen > 1 THEN
                          CALL f_forma.setElementHidden("gr_res_pag", 0) --Muestra la sección de Resúmen de Registro Avance Pagos 
                          DISPLAY ARRAY g_arr_resumen TO scr_resumen.*
                          ATTRIBUTES (CANCEL = FALSE )
                            BEFORE DISPLAY
                              DISPLAY "int flag --- ", INT_FLAG

                              --Hace que el display salga automáticamente sin esperar acción
                              IF INT_FLAG = 0 THEN 
                                 ACCEPT DISPLAY 
                              END IF 
                          END DISPLAY
                       ELSE                        
                          CALL f_forma.setElementHidden("gr_res_pag", 1) --Oculta la sección de Resúmen de Registro Avance Pagos
                       END IF

                       CONNECT TO "safre_viv"
                       --Consulta informacion del resumen Cancelación Parcial
                       CALL fn_consulta_canc_parc(v_valida_nss,
                                                  v_valida_folio,
                                                  v_valida_periodo,
                                                  v_valida_nrp,
                                                  v_valida_id_ava_pag)
                       RETURNING g_canc_par, v_ind_canc_par

                       IF v_ind_canc_par > 1 THEN
                          DISCONNECT CURRENT
                          CALL f_forma.setElementHidden("gr_detalle_canc_pav", 0) --Muestra la sección de Resúmen de Registro Avance Pagos 
                          DISPLAY ARRAY g_canc_par TO scr_canc_par.*
                            ATTRIBUTES (CANCEL = FALSE )
                              BEFORE  DISPLAY 
                                DISPLAY "int flag --- ", INT_FLAG

                                --Hace que el display salga automáticamente sin esperar acción
                                IF INT_FLAG = 0 THEN 
                                   ACCEPT DISPLAY 
                                END IF 
                          END DISPLAY
                       ELSE 
                          DISCONNECT CURRENT 
                          --Si no existe muestra mensaje de atención 
                          --CALL fn_mensaje("ATENCION","No existe información de resumen para mostrar","stop")
                          CALL f_forma.setElementHidden("gr_detalle_canc_pav", 1) --Oculta la Sección Detalle Canc Parcial Ava Pag                           
                       END IF

                       --Para salir termina el display y oculta el detalle y los botones
                       ON ACTION salir
                          CALL f_forma.setElementHidden("gr_det_avpag", 1)  --Oculta la Sección Detalle Registro Av. Pagos
                          CALL f_forma.setElementHidden("gr_res_pag", 1)    --Oculta la sección de Resúmen de Registro Avance Pagos
                          CALL f_forma.setElementHidden("gr_detalle_canc_pav", 1) --Oculta la Sección Detalle Canc Parcial Ava Pag
                          CALL f_forma.setFieldHidden("v_nombre", TRUE )    --Oculta el nombre del trabajador
                          CALL f_forma.setElementHidden("lb_nombre", TRUE ) --Oculta el nombre del trabajador
                          CALL f_forma.setElementHidden("btn_reporte", 1) 
                          CALL f_forma.setElementHidden("btn_salir", 1)
                          LET v_ind_detalle  = 0
                          LET v_ind_resumen  = 0
                          LET v_ind_canc_par = 0
                          EXIT DISPLAY
                          CLEAR FORM

                      ON ACTION reporte
                          DISPLAY ""
                          DISPLAY "ON ACTION reporte"
                          CONNECT TO "safre_viv"
                          CALL fn_genera_reporte()
                          DISCONNECT CURRENT

                      --Acciones del botón CANCELAR 
                      ON ACTION CANCEL           
                         EXIT INPUT

                    END DISPLAY
                 END IF
              END IF 
           END IF 
      END INPUT 
  CLOSE WINDOW v_cons_historica
END MAIN

#OBJETIVO: Llenar el combo para que el usuario elija el Estado
FUNCTION fn_llena_combo_estado()
DEFINE cb                    ui.ComboBox   --Variable de Combobox

  LET cb = ui.ComboBox.forName("cb_estado_avance") --Asignación del combo a la forma

  --Validación si el combo es nulo 
  IF cb IS NULL THEN
     ERROR "Form field not found in current form"
     EXIT PROGRAM
  END IF

  --Limpia el combo
  CALL cb.clear()

  --Agrega elementos al combobox
  CALL cb.addItem(30, "ABIERTOS")
  CALL cb.addItem(50, "CERRADOS")

  RETURN cb_estado_avance
END FUNCTION 

#OBJETIVO: Validar que el folio y periodo existan en la tabla histórica
FUNCTION fn_valida_folio_periodo_estado(p_cons_parametros_fpe)
DEFINE 
  p_cons_parametros_fpe      STRING, --String con los 3 parametros
  v_ind_param_fol            DECIMAL(10,0),
  v_cuenta_folio             DECIMAL(10,0),
  v_cuenta_periodo           DECIMAL(10,0),
  v_cuenta_estado            DECIMAL(10,0),
  v_cuenta_total             DECIMAL(10,0) 

  LET v_ind_param_fol = 0
       
  LET v_QryTxt = "\n SELECT COUNT(folio), COUNT(periodo_pago), COUNT(estado), COUNT(*)",
                 "\n FROM   dis_det_avance_pago dd, afi_derechohabiente ad",
                 "\n WHERE  dd.id_derechohabiente = ad.id_derechohabiente",
                 "\n AND    dd.estado             IN (30,50,70) " 
  LET v_QryTxt = v_QryTxt ,p_cons_parametros_fpe                   

  DISPLAY "VALIDA PARAMETROS", v_QryTxt

  PREPARE prp_valida_folio_dis_det FROM v_QryTxt
  --DECLARE cur_parametros CURSOR FOR prp_valida_folio_dis_det
  EXECUTE  prp_valida_folio_dis_det INTO v_cuenta_folio,v_cuenta_periodo,v_cuenta_estado,v_cuenta_total

  IF v_cuenta_folio   IS NULL THEN LET v_cuenta_folio   = 0 END IF
  IF v_cuenta_periodo IS NULL THEN LET v_cuenta_periodo = 0 END IF 
  IF v_cuenta_estado  IS NULL THEN LET v_cuenta_estado  = 0 END IF 
  IF v_cuenta_total   IS NULL THEN LET v_cuenta_total   = 0 END IF  

  LET v_ind_param_fol = v_ind_param_fol + v_cuenta_folio + v_cuenta_periodo + v_cuenta_estado + v_cuenta_total
  LET v_ind_param_fol = v_ind_param_fol + 1

  DISPLAY "listo ",v_ind_param_fol

  RETURN v_ind_param_fol
END FUNCTION

#OBJETIVO: Consultar la sección de detalle del Regi stro de Avances de Pago
FUNCTION fn_consulta_detalle(p_cons_parametros_fpe)
DEFINE 
  p_cons_parametros_fpe      STRING, --String con los 3 parametros
  v_detalle_consulta_ind     DECIMAL(10,0)

  CALL g_arr_detalles_avpag.CLEAR()

  LET v_detalle_consulta_ind = 1

  LET v_QryTxt = "\n SELECT dd.folio,ad.nss,dd.num_credito,dd.periodo_pago,dd.f_pago,",
                 "\n        dd.nrp,dd.monto_aportacion,dd.monto_amortizacion,",
                 "\n        dd.estado||'-'||ce.desc_edo_avance,",
                 "\n        dd.id_derechohabiente, dd.estado, dd.id_dis_det_avance_pago",
                 "\n FROM   dis_det_avance_pago dd,",
                 "\n        afi_derechohabiente ad,",
                 "\n        cat_edo_avance_pago ce",
                 "\n WHERE  dd.id_derechohabiente = ad.id_derechohabiente",
                 "\n AND    dd.estado             = ce.cod_edo_avance",
                 "\n AND    dd.estado            IN (30, 50, 70) "
                 --"\n     AND ",p_cons_parametros_fpe,

  LET v_QryTxt = v_QryTxt, p_cons_parametros_fpe

  LET v_QryTxt = v_QryTxt, "\n GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12",
                           "\n ORDER BY dd.folio DESC, dd.periodo_pago DESC"

  DISPLAY "DETALLE ", v_QryTxt

  --Prepara la consulta para el display
  PREPARE prp_det_av_pag FROM v_QryTxt
  --Declara el cursor para la consulta
  DECLARE cur_det_av_pag CURSOR FOR prp_det_av_pag
    LET g_tpo_enc = 1
    FOREACH cur_det_av_pag INTO g_arr_detalles_avpag[v_detalle_consulta_ind].*
      LET v_detalle_consulta_ind = v_detalle_consulta_ind + 1
    END FOREACH

    CALL g_arr_detalles_avpag.deleteElement(v_detalle_consulta_ind)

    --LET v_detalle_consulta_ind = v_detalle_consulta_ind - 1     

  RETURN v_detalle_consulta_ind   
END FUNCTION

#OBJETIVO: Generar la consulta del resúmen de avance  de pago
FUNCTION fn_consulta_resumen(p_id_dh, p_id_avance_pagos)
DEFINE p_id_dh               LIKE dis_det_avance_pago.id_derechohabiente
DEFINE p_id_avance_pagos     DECIMAL(9,0)
DEFINE l_arr_resumen         DYNAMIC ARRAY OF RECORD
   v_folio_pago              LIKE dis_compensa_avance.folio_pago,
   v_nss_resumen             CHAR(11),
   v_monto_apo_pag           LIKE dis_compensa_avance.monto_apo_pag,
   v_monto_amo_pag           LIKE dis_compensa_avance.monto_amo_pag,
   v_monto_dif_apo           LIKE dis_compensa_avance.monto_apo_pag,
   v_monto_dif_amo           LIKE dis_compensa_avance.monto_amo_pag,
   v_edo_compensa_apo        CHAR(52),
   v_edo_compensa_amo        CHAR(52),   
   v_estado_avance           LIKE dis_det_avance_pago.estado, 
   v_estado_cred             LIKE cta_his_credito.estado
END RECORD

  CALL g_arr_resumen.CLEAR()

  LET v_QryTxt = "\n SELECT dc.folio_pago,af.nss,dc.monto_apo_pag,dc.monto_amo_pag,",
                 "\n        dd.monto_dif_apo,dd.monto_dif_amo,",
                 "\n        dc.edo_compensa_apo||'-'||ce.tpo_deudor_desc,",
                 "\n        dc.edo_compensa_amo||'-'||ce1.tpo_deudor_desc,",
                 "\n        dd.estado, hc.estado",
                 "\n FROM   dis_compensa_avance dc,",
                 "\n        dis_det_avance_pago dd,",
                 "\n OUTER  dis_cat_tpo_deudor ce,",
                 "\n OUTER  dis_cat_tpo_deudor ce1,",
                 "\n        afi_derechohabiente af,",
                 "\n OUTER  cta_his_credito hc",
                 "\n WHERE  dc.id_dis_det_avance_pago = dd.id_dis_det_avance_pago",
                 "\n AND    dc.edo_compensa_apo       = ce.tpo_deudor",
                 "\n AND    dc.edo_compensa_amo       = ce1.tpo_deudor",
                 "\n AND    hc.num_credito            = dd.num_credito",
                 "\n AND    hc.id_derechohabiente     =", p_id_dh,
                 "\n AND    dc.id_derechohabiente     = hc.id_derechohabiente",
                 "\n AND    dc.id_dis_det_avance_pago = ", p_id_avance_pagos,
                 "\n AND    dd.id_dis_det_avance_pago = dc.id_dis_det_avance_pago",
                 "\n AND    hc.id_derechohabiente     = af.id_derechohabiente",
                 "\n AND    dc.id_derechohabiente     = af.id_derechohabiente"
  DISPLAY "RESUMEN ",v_QryTxt

  PREPARE prp_resumen_av_pag FROM v_QryTxt
  DECLARE cur_resumen_av_pag CURSOR FOR prp_resumen_av_pag
  CALL g_arr_resumen.clear()
  CALL l_arr_resumen.clear()
  LET v_ind_resumen = 1
  --Inicia el reporte de registros con rechazo
  FOREACH cur_resumen_av_pag INTO l_arr_resumen[v_ind_resumen].*
    LET g_arr_resumen[v_ind_resumen].v_folio_pago       = l_arr_resumen[v_ind_resumen].v_folio_pago
    LET g_arr_resumen[v_ind_resumen].v_nss_resumen      = l_arr_resumen[v_ind_resumen].v_nss_resumen   
    LET g_arr_resumen[v_ind_resumen].v_monto_apo_pag    = l_arr_resumen[v_ind_resumen].v_monto_apo_pag
    LET g_arr_resumen[v_ind_resumen].v_monto_amo_pag    = l_arr_resumen[v_ind_resumen].v_monto_amo_pag
    LET g_arr_resumen[v_ind_resumen].v_monto_dif_apo    = l_arr_resumen[v_ind_resumen].v_monto_dif_apo
    LET g_arr_resumen[v_ind_resumen].v_monto_dif_amo    = l_arr_resumen[v_ind_resumen].v_monto_dif_amo
    LET g_arr_resumen[v_ind_resumen].v_edo_compensa_apo = l_arr_resumen[v_ind_resumen].v_edo_compensa_apo 
    LET g_arr_resumen[v_ind_resumen].v_edo_compensa_amo = l_arr_resumen[v_ind_resumen].v_edo_compensa_amo

    IF g_arr_resumen[v_ind_resumen].v_edo_compensa_apo IS NULL THEN
       LET g_arr_resumen[v_ind_resumen].v_edo_compensa_apo = 'SIN AVANCE'
    END IF 

    IF l_arr_resumen[v_ind_resumen].v_estado_avance = 30 THEN 
       LET  g_arr_resumen[v_ind_resumen].v_estado_avance = "ABIERTO"
    END IF 

    IF l_arr_resumen[v_ind_resumen].v_estado_avance  = 50 OR 
       l_arr_resumen[v_ind_resumen].v_estado_avance <> 30 OR
       l_arr_resumen[v_ind_resumen].v_estado_avance <> 10 THEN 
       LET  g_arr_resumen[v_ind_resumen].v_estado_avance = "CERRADO"
    END IF 

    IF l_arr_resumen[v_ind_resumen].v_estado_cred = 2 THEN
       LET  g_arr_resumen[v_ind_resumen].v_estado_cred = "LIQUIDADO"
    ELSE 
       LET  g_arr_resumen[v_ind_resumen].v_estado_cred = "VIGENTE"
    END IF

    LET v_ind_resumen  = v_ind_resumen  + 1
  END FOREACH

  CALL g_arr_resumen.deleteElement(v_ind_resumen)

  RETURN v_ind_resumen
END FUNCTION 

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE   
    v_id_consulta            LIKE afi_derechohabiente.id_derechohabiente 

  SELECT rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO   v_nombre_completo
  FROM   afi_derechohabiente
  WHERE  id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#OBJETIVO: Generar el reporte histórico
FUNCTION fn_genera_reporte()
DEFINE manejador_rpt         om.SaxDocumentHandler
DEFINE v_ind_1               INTEGER
DEFINE v_ind_res             INTEGER

  --Se asigna la plantilla para generar el reporte
  --IF fgl_report_loadCurrentSettings("cons_his_avance_pago.4rp") THEN
  IF fgl_report_loadCurrentSettings("DISC302.4rp") THEN
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF 

  LET v_ind_res = 1
   
  START REPORT rpt_historico TO XML HANDLER manejador_rpt
    FOR v_ind_1 = 1 TO v_ind_detalle
        CALL fn_consulta_resumen(g_arr_detalles_avpag[v_ind_1].v_id_dh, 
                                 g_arr_detalles_avpag[v_ind_1].v_id_avance_pagos) 
        RETURNING v_ind_resumen
                        
        OUTPUT TO REPORT rpt_historico (g_arr_detalles_avpag[v_ind_1].*, 
                                        g_arr_resumen[v_ind_res].*,
                                        v_valida_folio, 
                                        v_valida_periodo, 
                                        v_valida_estado,
                                        g_tpo_enc,
                                        g_usuario,
                                        v_estado_desc)

    END FOR
  FINISH REPORT rpt_historico       
END FUNCTION

--Reporte del histórico
REPORT rpt_historico(p_arr_detalles_avpag,
                     p_arr_resumen,
                     p_valida_folio, 
                     p_valida_periodo,
                     p_valida_estado,
                     p_tpo_enc,
                     p_usuario,
                     p_estado_desc)

DEFINE p_usuario             LIKE seg_usuario.usuario,
       p_tpo_enc             INTEGER,
       p_valida_folio        LIKE dis_det_avance_pago.folio,        --Almacena el Folio si es valido  
       p_valida_periodo      LIKE dis_det_avance_pago.periodo_pago, --Almacena el periodo si es valido
       p_valida_estado       LIKE dis_det_avance_pago.estado,       --Almacena el estado valido
       v_fecha_reporte       DATE, 
       p_estado_desc         CHAR (07)
       
DEFINE p_arr_detalles_avpag  RECORD 
   v_folio                   LIKE dis_det_avance_pago.folio,
   v_nss                     LIKE afi_derechohabiente.nss,
   v_num_credito             LIKE dis_det_avance_pago.num_credito,
   v_periodo_pago            LIKE dis_det_avance_pago.periodo_pago,
   v_f_pago                  LIKE dis_det_avance_pago.f_pago,
   v_nrp                     LIKE dis_det_avance_pago.nrp,
   v_monto_aportacion        LIKE dis_det_avance_pago.monto_aportacion,
   v_monto_amortizacion      LIKE dis_det_avance_pago.monto_amortizacion,
   v_estado_des              CHAR(52),
   v_id_dh                   LIKE dis_det_avance_pago.id_derechohabiente,
   v_estado                  INTEGER,
   v_id_avance_pagos         DECIMAL(9,0)
END RECORD

DEFINE p_arr_resumen         RECORD
   v_folio_pago              LIKE dis_compensa_avance.folio_pago,
   v_nss                     CHAR(11),
   v_monto_apo_pag           LIKE dis_compensa_avance.monto_apo_pag,
   v_monto_amo_pag           LIKE dis_compensa_avance.monto_amo_pag,
   v_monto_dif_apo           LIKE dis_compensa_avance.monto_apo_pag,
   v_monto_dif_amo           LIKE dis_compensa_avance.monto_amo_pag,
   v_edo_compensa_apo        CHAR(52),
   v_edo_compensa_amo        CHAR(52),   
   v_estado_avance           CHAR(52),
   v_estado_cred             CHAR(52)
END RECORD

FORMAT 
  FIRST PAGE HEADER
    LET v_fecha_reporte = TODAY
    PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
    PRINTX p_valida_folio
    PRINTX p_valida_periodo
    PRINTX p_valida_estado
    PRINTX p_usuario
    PRINTX p_estado_desc 
    PRINTX v_nombre_completo
      
  ON EVERY ROW
     PRINTX p_arr_detalles_avpag.v_folio
     PRINTX p_arr_detalles_avpag.v_nss
     PRINTX p_arr_detalles_avpag.v_num_credito
     PRINTX p_arr_detalles_avpag.v_periodo_pago
     PRINTX p_arr_detalles_avpag.v_f_pago USING "dd-mm-yyyy"
     PRINTX p_arr_detalles_avpag.v_nrp
     PRINTX p_arr_detalles_avpag.v_monto_aportacion
     PRINTX p_arr_detalles_avpag.v_monto_amortizacion
     PRINTX p_arr_detalles_avpag.v_estado_des

     PRINTX p_arr_resumen.* 
    
END REPORT

FUNCTION fn_valida_nss()
  DEFINE v_funcion_nss       STRING
  DEFINE v_cadena            CHAR(11)
  DEFINE v_tpo_consulta      SMALLINT

  LET v_tpo_consulta = 2

  LET v_funcion_nss  = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

  PREPARE prp_valida_nss FROM v_funcion_nss

  LET v_cadena = f_nss

  EXECUTE prp_valida_nss USING v_cadena, v_tpo_consulta, g_usuario
  INTO r_valida_nss
  
END FUNCTION

#OBJETIVO: Consultar la sección de cancelación parcial de Avances de Pago
FUNCTION fn_consulta_canc_parc(p_nss, p_folio, p_periodo_pago, p_nrp, p_id_dis_det_avance_pago)
DEFINE r_arr_canc_par        DYNAMIC ARRAY OF RECORD
       ava_orig_apo          DECIMAL(12,2),
       ava_orig_amo          DECIMAL(12,2),
       ava_canc_par_apo      DECIMAL(12,2),
       ava_canc_par_amo      DECIMAL(12,2),
       ava_pag_fin_apo       DECIMAL(12,2),
       ava_pag_fin_amo       DECIMAL(12,2),
       pag_pat_apo           DECIMAL(12,2),
       pag_pat_amo           DECIMAL(12,2),
       ajuste_sua_apo        DECIMAL(12,2),
       ajuste_sua_amo        DECIMAL(12,2),
       sdo_rem_apo           DECIMAL(12,2),
       sdo_rem_amo           DECIMAL(12,2),
       bimestre              CHAR(6),
       num_credito           DECIMAL(10,0),
       f_cancelacion         DATE,
       fol_cancelacion       DECIMAL(9,0)
       END RECORD,

       v_QryTxt              STRING,     -- Cadena para almacenar Query 
       v_indice              INTEGER,    -- Variable de indice
       v_id_dh_ci            DECIMAL(9,0),
       p_nss                 LIKE afi_derechohabiente.nss,
       p_folio               DECIMAL(9,0),
       p_periodo_pago        CHAR(6),
       p_nrp                 CHAR(11),
       p_id_dis_det_avance_pago DECIMAL(9,0),
       v_val_cpav            SMALLINT,
       v_val_comp            SMALLINT,
       v_c_monto_apo_pag     DECIMAL(12,2),
       v_c_monto_amo_pag     DECIMAL(12,2),
       v_c_edo_comp_apo      SMALLINT,
       v_c_edo_comp_amo      SMALLINT,
       v_monto_dif_apo       DECIMAL(12,2),
       v_monto_dif_amo       DECIMAL(12,2)

  LET v_indice          = 0;
  LET v_val_cpav        = 0;
  LET v_val_comp        = 0;
  LET v_c_monto_apo_pag = 0;
  LET v_c_monto_amo_pag = 0;
  LET v_c_edo_comp_apo  = 0;
  LET v_c_edo_comp_amo  = 0;
  LET v_monto_dif_apo   = 0;
  LET v_monto_dif_amo   = 0;

  CALL r_arr_canc_par.CLEAR()

  SELECT MAX (id_derechohabiente) 
  INTO   v_id_dh_ci
  FROM   afi_derechohabiente
  WHERE  nss = p_nss

  SELECT COUNT(*)
  INTO   v_val_cpav
  FROM   dis_canc_par_ava_pag a
  WHERE  a.id_derechohabiente = v_id_dh_ci
  AND    a.periodo_pago       = p_periodo_pago
  AND    a.nrp                = p_nrp
  AND    a.estado             = 1

  IF v_val_cpav >= 1 THEN
     --Realiza consulta de datos solicitados en tabla de rechazos y de detalle
     LET v_QryTxt = "\n SELECT a.monto_apo_ava,      ",
                    "\n        a.monto_amo_ava,      ",
                    "\n        a.monto_aportacion,   ",
                    "\n        a.monto_amortizacion, ",
                    "\n        a.monto_apo_fin,      ",
                    "\n        a.monto_amo_fin,      ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        a.periodo_pago,       ",
                    "\n        a.num_credito,        ",
                    "\n        b.f_actualiza,        ",
                    "\n        a.folio               ",
                    "\n FROM   dis_canc_par_ava_pag a,",
                    "\n        glo_folio b           ",
                    "\n WHERE  a.id_derechohabiente = ",v_id_dh_ci,
                    "\n AND    a.periodo_pago       = '",p_periodo_pago, "'",
                    "\n AND    a.nrp                = '",p_nrp, "'",
                    "\n AND    a.estado             = 1 ",
                    "\n AND    a.folio              = b.folio ",
                    "\n ORDER BY b.f_actualiza, a.folio"

     DISPLAY v_QryTxt
     -- Prepara la consulta para el display
     PREPARE prp_DetCancParc FROM v_QryTxt
     -- Declara el cursor para la consulta
     DECLARE cur_DetCancParc CURSOR FOR prp_DetCancParc
     LET v_indice = 1
     FOREACH cur_DetCancParc INTO r_arr_canc_par[v_indice].*
       SELECT COUNT(*)
       INTO   v_val_comp
       FROM   dis_compensa_avance
       WHERE  id_dis_det_avance_pago = p_id_dis_det_avance_pago
       AND    periodo_pago           = p_periodo_pago
       AND    nrp                    = p_nrp
       IF v_val_comp > 0 THEN
          SELECT a.monto_apo_pag,
                 a.monto_amo_pag,
                 a.edo_compensa_apo,
                 a.edo_compensa_amo
          INTO   v_c_monto_apo_pag,
                 v_c_monto_amo_pag,
                 v_c_edo_comp_apo,
                 v_c_edo_comp_amo
          FROM   dis_compensa_avance a
          WHERE  a.id_dis_det_avance_pago = p_id_dis_det_avance_pago
          AND    a.periodo_pago           = p_periodo_pago
          AND    a.nrp                    = p_nrp;

          LET v_monto_dif_apo = 0;
          LET v_monto_dif_amo = 0;

          SELECT a.monto_dif_apo,
                 a.monto_dif_amo
          INTO   v_monto_dif_apo,
                 v_monto_dif_amo
          FROM   dis_det_avance_pago a
          WHERE  a.periodo_pago           = p_periodo_pago
          AND    a.id_derechohabiente     = v_id_dh_ci
          AND    a.id_dis_det_avance_pago = p_id_dis_det_avance_pago

          LET r_arr_canc_par[v_indice].pag_pat_apo = v_c_monto_apo_pag;
          LET r_arr_canc_par[v_indice].pag_pat_amo = v_c_monto_amo_pag; 

          IF v_c_edo_comp_apo = 1 THEN
             LET r_arr_canc_par[v_indice].ajuste_sua_apo = v_monto_dif_apo;
          END IF
          IF v_c_edo_comp_amo = 1 THEN
             LET r_arr_canc_par[v_indice].ajuste_sua_amo = v_monto_dif_amo;
          END IF
          IF v_c_edo_comp_apo = 2 THEN
             LET r_arr_canc_par[v_indice].sdo_rem_apo    = v_monto_dif_apo;
          END IF
          IF v_c_edo_comp_amo = 2 THEN
             LET r_arr_canc_par[v_indice].sdo_rem_amo    = v_monto_dif_amo;
          END IF
       END IF

       LET v_indice = v_indice + 1
     END FOREACH

     CALL r_arr_canc_par.deleteElement(v_indice)

     --Obtiene nombre del derechohabiente
     CALL fn_obtiene_nombre_derechohabiente(v_id_dh_ci)
  END IF
  
  -- Retorna el arreglo con detalles del folio rechazado y tamaño del arreglo
  RETURN r_arr_canc_par, v_indice
END FUNCTION