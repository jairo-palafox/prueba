################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 27/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC08                                                    #
#Objetivo         => Realizar la consulta histórica de los avances de pago,    #
#                    donde se podrá identificar el estado del avance de pago.  #
#Fecha de Inicio  => 26/06/2012                                                #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose
################################################################################
DATABASE safre_viv
GLOBALS 
DEFINE g_folio               DECIMAL(9,0),  --Almacena el Folio capturado  
       g_periodo             CHAR(6),       --Almacena el periodo de pago
       g_estado_avance       LIKE dis_det_avance_pago.estado, --Almacena el estado seleccionado
       g_usuario             LIKE seg_usuario.usuario_cod, --Clave de usuario
       v_QryTxt              STRING,        --Cadena para consultas
       v_ind_detalle         DECIMAL(10,0), --Indice del arreglo de detalle 
       v_ind_resumen         INTEGER,       --Indice del arreglo de resumen
       g_id_arr              DECIMAL(10,0), --Indice de fila seleccionada
       g_tpo_enc             DECIMAL(10,0), --Agrupa arreglos para reporte
       v_ind_parametros      DECIMAL(10,0),
       v_totales_monto_apo   DECIMAL(22,2),
       v_totales_monto_amo   DECIMAL(22,2)

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
       cons_parametros_fpe   STRING 

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
END GLOBALS

MAIN 
DEFINE p_tipo_proc           CHAR(1),  --Tipo de proceso
       p_nombre_menu         LIKE seg_menu.opcion,         --Nombre del programa tomado del menú
       p_proceso_cod         LIKE cat_proceso.proceso_cod, --Codigo del proceso
       p_programa            CHAR(10), --Nombre del programa que se ejecuta
       r_bnd_periodo         INTEGER,
       v_periodo_cvt         VARCHAR(6),
       v_qwery_ibx           STRING  
       
DEFINE f_ventana             ui.Window,  --Define las propìedades de la Ventana
       f_forma               ui.Form     --Define las propiedades de la forma

  LET p_programa    = "DISC08"   --Programa que se ejecuta       
  LET g_usuario     = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) --Recibe el nombre del programa
  LET p_proceso_cod = 902        --Codigo de proceso

  LET cons_parametros_fpe = ""
  LET r_bnd_periodo       = 0

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
   
  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
      
  CLOSE WINDOW SCREEN 

  OPEN WINDOW v_cons_historica WITH FORM "DISC081"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      --Se capturan los 3 parámetros para consulta (folio, periodo, estado)
      --CONSTRUCT cons_parametros_fpe
      --ON folio,
           --periodo_pago,
           --estado,
           --nss
      --FROM f_folio, 
           --f_periodo, 
           --cb_estado_avance,
           --f_nss

      INPUT BY NAME f_folio,f_periodo,cb_estado_avance,f_nss
        --BEFORE CONSTRUCT
        BEFORE INPUT  
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL fn_llena_combo_estado() RETURNING g_estado_avance

          CALL ui.Interface.setText(p_nombre_menu)          --Se asigna el titulo a la ventana

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

          CALL f_forma.setElementHidden("gr_det_avpag", 1)  --Oculta la Sección Detalle Registro Av. Pagos
          CALL f_forma.setElementHidden("gr_res_pag", 1)    --Oculta la sección de Resúmen de Registro Avance Pagos
          CALL f_forma.setElementHidden("gr_nombre", TRUE ) --Oculta el nombre del trabajador
         
          CALL f_forma.setElementHidden("lbl_totales",TRUE)       --Oculta la etiqueta Totales
          CALL f_forma.setFieldHidden("v_totales_monto_apo",TRUE) --Oculta el cuadro de texto de la sumatoria de abonos
          CALL f_forma.setFieldHidden("v_totales_monto_amo",TRUE) --Oculta el cuadro de texto de la sumatoria de abonos

          LET cons_parametros_fpe = ""

        --Acciones del botón CANCELAR
        ON ACTION cancelar 
            EXIT DIALOG
      
        --Acciones del botón ACEPTAR
        ON ACTION ACCEPT
           LET cons_parametros_fpe = ""
           --IF cons_parametros_fpe = " 1=1" THEN
           IF f_folio IS NULL AND f_periodo IS NULL AND cb_estado_avance IS NULL AND f_nss IS NULL THEN 
              CALL fn_mensaje ("ATENCION", "Debe capturar al menos un parámetro", "stop")
           ELSE
              --Valida si el folio fue capturado
              IF f_folio IS NOT NULL THEN 
                 LET cons_parametros_fpe = cons_parametros_fpe , "\n AND dd.folio = ",f_folio
              END IF 

              --Valida si el periodo de pago fue capturado
              IF (f_periodo IS NOT NULL) THEN
                 LET v_periodo_cvt = f_periodo
                 IF LENGTH(v_periodo_cvt) <> 6 THEN  
                    CALL fn_mensaje ("ATENCION", "El periodo de pago debe ser de 6 caracteres", "stop")
                    NEXT FIELD f_periodo
                 END IF 

                 DISPLAY "f_periodo antes -- ",v_periodo_cvt
            
                 PREPARE prp_verifica_periodo FROM "EXECUTE FUNCTION fn_valida_formato_periodo_pago(?)"
                 EXECUTE prp_verifica_periodo USING f_periodo INTO f_periodo, r_bnd_periodo

                 LET v_periodo_cvt = f_periodo
                 DISPLAY "f_periodo después -- ",v_periodo_cvt

                 --La función "fn_valida_formato_periodo_pago" regresa 2 parámetros, uno el periodo y dos el estatus
                 --Si el estatus es 0, no hay error y el periodo es válido
                 --Si el estatus es 1, entonces el año del periodo es inválido, es decir, es mayor al año actual
                 --Si el estatus es 2, entonces el mes es incorrecto
                 IF r_bnd_periodo = 0 THEN 
                    LET cons_parametros_fpe = cons_parametros_fpe , "\n AND dd.periodo_pago = '",v_periodo_cvt,"'"
                 ELSE 
                    IF r_bnd_periodo = 1 THEN 
                       CALL fn_mensaje ("ATENCION", "Verifique el periodo de pago. El año es incorrecto", "stop")
                    END IF

                    IF r_bnd_periodo = 2 THEN 
                       CALL fn_mensaje ("ATENCION", "Verifique el periodo de pago. El mes es incorrecto", "stop")
                    END IF  

                    NEXT FIELD f_periodo
                 END IF
              END IF

              --Valida si el estado es capturado
              IF cb_estado_avance IS NOT NULL THEN 
                 LET cons_parametros_fpe = cons_parametros_fpe , "\n AND dd.estado = ",cb_estado_avance
              END IF

              --Valida si el estado es capturado
              IF f_nss IS NOT NULL THEN 
                 LET cons_parametros_fpe = cons_parametros_fpe , "\n AND ad.nss = '",f_nss, "'"
              END IF
            
              --Valida que el folio capturado exista en la tabla de historico
              CALL fn_valida_folio_periodo_estado(cons_parametros_fpe) 
              RETURNING v_ind_parametros

              --Si los parámetros capturados existen y son válidos 
              IF v_ind_parametros >= 2 THEN                 
                 CALL fn_consulta_detalle(cons_parametros_fpe) 
                 RETURNING v_ind_detalle
                 LET v_valida_folio   = g_arr_detalles_avpag[ARR_CURR()].v_folio   
                 LET v_valida_periodo = g_arr_detalles_avpag[ARR_CURR()].v_periodo_pago            
                 LET v_valida_estado  = g_arr_detalles_avpag[ARR_CURR()].v_estado
                 LET v_valida_nss     = g_arr_detalles_avpag[ARR_CURR()].v_nss

                 --DISPLAY "Id -- ",g_arr_detalles_avpag[ARR_CURR()].v_id_dh
                 CALL fn_obtiene_nombre_derechohabiente(g_arr_detalles_avpag[ARR_CURR()].v_id_dh)
               
                 IF v_valida_estado = 30 THEN 
                    LET v_estado_desc = "ABIERTO"
                 END IF 

                 IF v_valida_estado = 50 THEN 
                    LET v_estado_desc = "CERRADO"
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
                 DISPLAY BY NAME f_folio,f_periodo,cb_estado_avance,f_nss
                 NEXT FIELD f_folio
              ELSE 
                 IF v_ind_detalle > 1 THEN 
                    CALL f_forma.setElementHidden("gr_det_avpag", 0) --Muestra la Sección Detalle Registro Av. Pagos
                    CALL f_forma.setElementHidden("lbl_totales",FALSE) --Muestra la etiqueta Totales
                    CALL f_forma.setFieldHidden("v_totales_monto_apo",FALSE ) --Muestra el cuadro de texto de la sumatoria de abonos
                    CALL f_forma.setFieldHidden("v_totales_monto_amo",FALSE ) --Muestra el cuadro de text
                    CALL f_forma.setElementHidden("gr_nombre", FALSE ) --Muestra el nombre del trabajador
                        
                    DISPLAY ARRAY g_arr_detalles_avpag TO scr_detalles.*
                    ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
                      BEFORE DISPLAY 
                       DISPLAY BY NAME v_totales_monto_apo,v_totales_monto_amo
                      
                      BEFORE ROW
                        --LET g_id_arr = DIALOG.getCurrentRow("scr_detalles") --Asigna el índice para seleccionar registro
                        --Consulta informacion del resumen
                        CALL fn_consulta_resumen(g_arr_detalles_avpag[ARR_CURR()].v_id_dh, 
                                                 g_arr_detalles_avpag[ARR_CURR()].v_id_avance_pagos) 
                        RETURNING v_ind_resumen

                        CALL fn_obtiene_nombre_derechohabiente(g_arr_detalles_avpag[ARR_CURR()].v_id_dh)

                        IF v_ind_resumen > 1 THEN
                           CALL f_forma.setElementHidden("gr_res_pag", 0) --Muestra la sección de Resúmen de Registro Avance Pagos 
                           DISPLAY ARRAY g_arr_resumen TO scr_resumen.*
                           ATTRIBUTES (CANCEL = FALSE )
                             --Acciones del botón SALIR
                             --ON ACTION salir
                                  --CALL f_forma.setElementHidden("gr_res_pag", 1) --Oculta la sección de Resúmen de Registro Avance Pagos 
                                  --EXIT DISPLAY
 
                             --Acciones del botón REPORTE
                             --ON ACTION reporte
                                  --CALL fn_genera_reporte()

                              BEFORE  DISPLAY 
                                DISPLAY "int flag --- ", INT_FLAG

                                --Hace que el display salga automáticamente sin esperar acción
                                IF INT_FLAG = 0 THEN 
                                   ACCEPT DISPLAY 
                                END IF 
                           END DISPLAY
                        ELSE 
                           --Si no existe muestra mensaje de atención 
                           CALL fn_mensaje("ATENCION","No existe información de resumen para mostrar","stop")
                        END IF
                        
                      ON ACTION cancelar
                         CALL f_forma.setElementHidden("gr_det_avpag", 1)  --Oculta la Sección Detalle Registro Av. Pagos 
                         CALL f_forma.setElementHidden("gr_nombre", TRUE ) --Oculta el nombre del trabajador
 
                         CALL f_forma.setElementHidden("lbl_totales",TRUE)       --Oculta la etiqueta Totales
                         CALL f_forma.setFieldHidden("v_totales_monto_apo",TRUE) --Oculta el cuadro de texto de la sumatoria de abonos
                         CALL f_forma.setFieldHidden("v_totales_monto_amo",TRUE) --Oculta el cuadro de
                        
                         EXIT DISPLAY  
                  
                      ON ACTION ACCEPT
                         --Valida si cuenta con información muestra resumen 
                         IF v_ind_resumen > 1 THEN
                            CALL f_forma.setElementHidden("gr_res_pag", 0) --Muestra la sección de Resúmen de Registro Avance Pagos 
                            DISPLAY ARRAY g_arr_resumen TO scr_resumen.*
                            ATTRIBUTES (CANCEL = FALSE )
                              --Acciones del botón SALIR
                              --ON ACTION salir
                                 --CALL f_forma.setElementHidden("gr_res_pag", 1) --Oculta la sección de Resúmen de Registro Avance Pagos 
                                --EXIT DISPLAY

                              --Acciones del botón REPORTE
                              --ON ACTION reporte
                                 --CALL fn_genera_reporte()

                              BEFORE  DISPLAY 
                                DISPLAY "int flag --- ", INT_FLAG

                                --Hace que el display salga automáticamente sin esperar acción
                                IF INT_FLAG = 0 THEN 
                                   ACCEPT DISPLAY 
                                END IF 
                            END DISPLAY
                         ELSE 
                            --Si no existe muestra mensaje de atención 
                            CALL fn_mensaje("ATENCION","No existe información de resumen para mostrar","stop")
                         END IF
                        
                      ON ACTION reporte
                         CALL fn_genera_reporte()
                    END DISPLAY 
                 ELSE   
                    --Si no existe muestra mensaje de atención 
                    CALL fn_mensaje("ATENCION","No existe información para mostrar","stop")
                 END IF 
              END IF 
           END IF 
      END INPUT 
    END DIALOG
  CLOSE WINDOW v_cons_historica
END MAIN

#OBJETIVO: Llenar el combo para que el usuario elija el Estado
FUNCTION fn_llena_combo_estado()
DEFINE cb                    ui.ComboBox                 -- Variable de Combobox

  LET cb = ui.ComboBox.forName("cb_estado_avance") --Asignación del combo a la forma

  -- Validación si el combo es nulo 
  IF cb IS NULL THEN
     ERROR "Form field not found in current form"
     EXIT PROGRAM
  END IF

  -- Limpia el combo
  CALL cb.clear()

  -- Agrega elementos al combobox
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
       
  LET v_QryTxt = "\n SELECT COUNT(folio),COUNT(periodo_pago),COUNT(estado),count(*)",
                 "\n FROM   dis_det_avance_pago dd, afi_derechohabiente ad",
                 "\n WHERE  dd.id_derechohabiente = ad.id_derechohabiente" 

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

  --FOREACH cur_parametros INTO g_arr_parametros[v_ind_param_fol].*
  --FOREACH cur_parametros INTO 
      {IF g_arr_parametros[v_ind_param_fol].r_estado = 30 THEN 
          --LET g_arr_parametros[v_ind_param_fol].r_des_edo = "ABIERTO"
      END IF
      
      IF g_arr_parametros[v_ind_param_fol].r_estado = 50 THEN 
         --LET g_arr_parametros[v_ind_param_fol].r_des_edo = "CERRADO"
      END IF }  
      LET v_ind_param_fol = v_ind_param_fol + 1
  --END FOREACH 

  DISPLAY "listo ",v_ind_param_fol
  RETURN v_ind_param_fol
   
END FUNCTION

#OBJETIVO: Consultar la sección de detalle del Regi stro de Avances de Pago
FUNCTION fn_consulta_detalle(p_cons_parametros_fpe)
DEFINE 
  p_cons_parametros_fpe      STRING, --String con los 3 parametros
  v_detalle_consulta_ind     DECIMAL(10,0)

  LET v_totales_monto_apo    = 0.00
  LET v_totales_monto_amo    = 0.00
  LET v_detalle_consulta_ind = 1

  LET v_QryTxt = "\n SELECT dd.folio, ad.nss,dd.num_credito,dd.periodo_pago,dd.f_pago,",
                 "\n        dd.nrp,dd.monto_aportacion,dd.monto_amortizacion,",
                 "\n        dd.estado||'-'||ce.desc_edo_avance,",
                 "\n        dd.id_derechohabiente, dd.estado, dd.id_dis_det_avance_pago",
                 "\n FROM   dis_det_avance_pago dd,",
                 "\n        afi_derechohabiente ad,",
                 "\n        cat_edo_avance_pago ce",
                 "\n WHERE  dd.id_derechohabiente = ad.id_derechohabiente",
                 "\n AND    dd.estado             = ce.cod_edo_avance"
                 --"\n AND ",p_cons_parametros_fpe,

  LET v_QryTxt = v_QryTxt , p_cons_parametros_fpe

  LET v_QryTxt = v_QryTxt ,                        
                 "\n GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12",
                 "\n ORDER BY dd.folio DESC, dd.periodo_pago DESC"

                  
  DISPLAY "DETALLE ", v_QryTxt
  -- Prepara la consulta para el display
  PREPARE prp_det_av_pag FROM v_QryTxt
  -- Declara el cursor para la consulta
  DECLARE cur_det_av_pag CURSOR FOR prp_det_av_pag     
  LET g_tpo_enc = 1
  FOREACH cur_det_av_pag INTO g_arr_detalles_avpag[v_detalle_consulta_ind].*
    LET v_totales_monto_apo    = v_totales_monto_apo + g_arr_detalles_avpag[v_detalle_consulta_ind].v_monto_aportacion
    LET v_totales_monto_amo    = v_totales_monto_amo + g_arr_detalles_avpag[v_detalle_consulta_ind].v_monto_amortizacion
    LET v_detalle_consulta_ind = v_detalle_consulta_ind + 1     
  END FOREACH

  CALL g_arr_detalles_avpag.deleteElement(v_detalle_consulta_ind)

  RETURN v_detalle_consulta_ind   
END FUNCTION

#OBJETIVO: Generar la consulta del resúmen de avance  de pago
FUNCTION fn_consulta_resumen(p_id_dh, p_id_avance_pagos)
DEFINE p_id_dh               LIKE dis_det_avance_pago.id_derechohabiente
DEFINE p_id_avance_pagos     DECIMAL(9,0)
DEFINE l_arr_resumen         DYNAMIC ARRAY OF RECORD
  v_folio_pago               LIKE dis_compensa_avance.folio_pago,
  v_nss_resumen              CHAR(11),
  v_monto_apo_pag            LIKE dis_compensa_avance.monto_apo_pag,
  v_monto_amo_pag            LIKE dis_compensa_avance.monto_amo_pag,
  v_monto_dif_apo            LIKE dis_compensa_avance.monto_apo_pag,
  v_monto_dif_amo            LIKE dis_compensa_avance.monto_amo_pag,
  v_edo_compensa_apo         CHAR(52),
  v_edo_compensa_amo         CHAR(52),   
  v_estado_avance            LIKE dis_det_avance_pago.estado, 
  v_estado_cred              LIKE cta_his_credito.estado
  END RECORD

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

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#OBJETIVO: Generar el reporte histórico
FUNCTION fn_genera_reporte()
DEFINE manejador_rpt         om.SaxDocumentHandler,
       v_ind_1               INTEGER

  --Se asigna la plantilla para generar el reporte
  --IF fgl_report_loadCurrentSettings("cons_his_avance_pago.4rp") THEN
  IF fgl_report_loadCurrentSettings("DISC082.4rp") THEN
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF 

  START REPORT rpt_historico TO XML HANDLER manejador_rpt
    FOR v_ind_1 = 1 TO v_ind_detalle
        OUTPUT TO REPORT rpt_historico (g_arr_detalles_avpag[v_ind_1].*, 
                                        g_arr_resumen[v_ind_1].*,
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

DEFINE p_arr_resumen         RECORD
  v_folio_pago               LIKE dis_compensa_avance.folio_pago,
  v_nss                      CHAR(11),
  v_monto_apo_pag            LIKE dis_compensa_avance.monto_apo_pag,
  v_monto_amo_pag            LIKE dis_compensa_avance.monto_amo_pag,
  v_monto_dif_apo            LIKE dis_compensa_avance.monto_apo_pag,
  v_monto_dif_amo            LIKE dis_compensa_avance.monto_amo_pag,
  v_edo_compensa_apo         CHAR(52),
  v_edo_compensa_amo         CHAR(52),   
  v_estado_avance            CHAR(52),
  v_estado_cred              CHAR(52)
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