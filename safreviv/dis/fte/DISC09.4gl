################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 14/01/2020                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC09                                                    #
#Objetivo         => Realizar la consulta de saldos de dispersión.             #
#Fecha de Inicio  => 17/09/2012                                                #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose
#Eneas Armas     20140123      Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa

DATABASE safre_viv
GLOBALS
  --Arreglo para la sección de pagos superiories al 200505
  DEFINE arr_superiores      DYNAMIC ARRAY OF RECORD 
    f_descripcion_sup        VARCHAR(50),
    f_pago_apo               DECIMAL(22,2),
    f_pago_amo               DECIMAL(22,2),
    f_avance_apo             DECIMAL(22,2),
    f_avance_amo             DECIMAL(22,2),
    f_entrega_apo            DECIMAL(22,2),
    f_entrega_amo            DECIMAL(22,2)
  END RECORD   
         
  --Arreglo para la sección de pagos inferiores o igual al 200505
  DEFINE arr_anteriores      DYNAMIC ARRAY OF RECORD 
    f_descripcion_ant        VARCHAR(50),
    f_pago_apo               DECIMAL(22,2),
    f_pago_amo               DECIMAL(22,2),
    f_virtual_apo            DECIMAL(22,2),
    f_virtual_amo            DECIMAL(22,2),
    f_entrega_apo            DECIMAL(22,2),
    f_entrega_amo            DECIMAL(22,2)
  END RECORD 

  --Arreglo para la sección de pagos inferiores o igual al 200505
  DEFINE r_arr_mandatos      DYNAMIC ARRAY OF RECORD
    v_servicios              CHAR(40),
    v_monto_pesos            DECIMAL(22,2),
    v_total_cuentas          DECIMAL(10,0)
  END RECORD,
  
  v_tot_monto_pesos_mdt      LIKE cta_movimiento.monto_pesos,
  v_tot_total_cuentas_mdt    INTEGER

  --Sección de detalle
  DEFINE 
    f_folio                  DECIMAL(9,0),
    f_apo_subs               DECIMAL(22,2),
    f_predial                DECIMAL(22,2),
    f_mantenimiento          DECIMAL(22,2),
    f_restitucion            DECIMAL(22,2),
    f_man_monto_pesos        DECIMAL(22,2),
    f_man_tot_ctas           DECIMAL(22,2)
      
  --Sección de variables UI
  DEFINE 
    f_ventana                ui.Window, --provee la interfaz para la ventana
    f_forma                  ui.Form    --provee la interfaz para la forma

  --Sección de variables del programa
  DEFINE 
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT
      
  DEFINE 
    v_desc_servicios         VARCHAR(40),
    v_desc_mantenimiento     VARCHAR(40),
    v_desc_predial           VARCHAR(40),
    v_sum_servicios          DECIMAL(22,2),
    v_sum_mantenimiento      DECIMAL(22,2),
    v_sum_predial            DECIMAL(22,2),
    v_tot_servicios          DECIMAL(10,0),
    v_tot_predial            DECIMAL(10,0),
    v_tot_mantenimiento      DECIMAL(10,0)
      
  DEFINE 
    v_avance_pago_mayor_apo  DECIMAL(22,2),
    v_avance_pago_mayor_amo  DECIMAL(22,2),
    v_avance_pago_menor_apo  DECIMAL(22,2),
    v_avance_pago_menor_amo  DECIMAL(22,2)

  --Variables para total de cuentas afectadas con pagos superiores
  DEFINE 
    v_cuentas_pago_sin_avance_sup DECIMAL(10,0),
    v_cuentas_pago_sin_sup   DECIMAL(10,0),
    v_cuentas_pago_mayor_sup DECIMAL(10,0),
    v_cuentas_pago_menor_sup DECIMAL(10,0)

  DEFINE
    v_cuentas_abono_esp      DECIMAL(10,0),
    v_cuentas_cargo_esp      DECIMAL(10,0)

  --Variables para total de cuentas afectadas con pagos anteriores
  DEFINE 
    v_cuentas_pago_sin_avance_ant DECIMAL(10,0),
    v_cuentas_virtual_sin_ant     DECIMAL(10,0),
    v_cuentas_virtual_mayor_ant   DECIMAL(10,0),
    v_cuentas_virtual_menor_ant   DECIMAL(10,0)

  DEFINE 
    v_cuentas_apo_subs       DECIMAL(10,0),
    v_cuentas_restitucion    DECIMAL(10,0)

  DEFINE 
    v_sumado                 DECIMAL(10,0),
    v_folio_reg_pag          DECIMAL(9,0),
    v_etiqueta               STRING,
    v_etiqueta_precio        STRING,
    v_etiqueta_dae           STRING

  DEFINE
    v_tot_registros_fuera    DECIMAL(10,0),
    v_tot_aportacion_fuera   DECIMAL(22,2),
    v_tot_amortizacion_fuera DECIMAL(22,2),
    v_tpo_inconsistente      SMALLINT 

  DEFINE v_arr_rubros        DYNAMIC ARRAY OF RECORD 
    v_rubro                  VARCHAR(40),
    v_subcuenta              VARCHAR(40),
    v_aivs                   DECIMAL(22,2),
    v_aport                  DECIMAL(22,2),
    v_amort                  DECIMAL(22,2),
    v_total                  INTEGER
  END RECORD 

  DEFINE 
    v_tipo_subcuenta         SMALLINT 

  DEFINE 
    f_tot_rub_aivs           DECIMAL(22,2),
    f_tot_rub_aport          DECIMAL(22,2),
    f_tot_rub_amort          DECIMAL(22,2),
    v_qwery_ibx              STRING
      
  DEFINE v_total_sin_dif     DECIMAL(10,0)
  DEFINE v_tot_mdt           SMALLINT

  CONSTANT Por_Folio = 0
  CONSTANT Por_Fecha = 1
  CONSTANT Sin       = 0

  DEFINE v_tbl_mov           VARCHAR(50),
         g_sql_txt           STRING,
         v_proc_entra        SMALLINT,
         v_proc_val          SMALLINT,
         v_cod_conv          SMALLINT,
         v_desc_proc_val     CHAR(40),
         v_mensaje_val       STRING,
         p_proceso_cod       SMALLINT
END GLOBALS 

MAIN
  LET p_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa

  LET p_proceso_cod  = 901

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####

  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance

  --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
  PREPARE fn_tbl_mov FROM "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

  CLOSE WINDOW SCREEN

  OPEN WINDOW v_consulta WITH FORM "DISC091"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          --Invocamos la función para asignar el título a la ventana
          CALL ui.Interface.setText(p_nom_prog)

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

          --Oculta las secciones de detalle de pagos
          CALL f_forma.setElementHidden("gr_pago_superior",TRUE) --Oculta la sección de pagos superiores
          CALL f_forma.setElementHidden("gr_pago_anterior",TRUE) --Oculta la sección de pagos anteriores
          CALL f_forma.setElementHidden("gr_detalle",TRUE) --Oculta la sección de detalle
          CALL f_forma.setElementHidden("gr_info_reg_pag",TRUE) --Oculta la sección de información de registro de pagos
          CALL f_forma.setElementHidden("gr_info_precio",TRUE) --Oculta la sección de información del precio del fondo al día
          CALL f_forma.setElementHidden("gr_rubros",TRUE) --Oculta la sección del detalle por rubros

          ON ACTION cancelar 
             EXIT DIALOG

          ON ACTION aceptar
             IF f_folio IS NULL THEN
                CALL fn_mensaje("Error", "Capture el folio para realizar la búsqueda", "information")
                NEXT FIELD f_folio
             ELSE 
                CALL fn_consulta_saldos_dispersion()
             END IF 

             --Muestra las secciones de detalle de pagos
             CALL f_forma.setElementHidden("gr_info_reg_pag",FALSE) --Muestra la sección de información de registro de pagos
             CALL f_forma.setElementHidden("gr_info_precio",FALSE) --Muestra la sección de información del precio del fondo al día
             CALL f_forma.setElementHidden("gr_pago_superior",FALSE) --Muestra la sección de pagos superiores
             CALL f_forma.setElementHidden("gr_pago_anterior",FALSE) --Muestra la sección de pagos anteriores
             --CALL f_forma.setElementHidden("gr_detalle",FALSE) --Muestra la sección de detalle
                      
             DISPLAY ARRAY arr_superiores TO src_superiores.* ATTRIBUTES (ACCEPT = FALSE , CANCEL = FALSE )
               BEFORE DISPLAY 
                 --Muestra folio de registro de pagos
                 IF v_folio_reg_pag IS NULL THEN 
                    LET v_etiqueta = "No existe Folio de Registro de Pagos asociado."
                 ELSE 
                    LET v_etiqueta = "Folio de Registro de Pagos:",v_folio_reg_pag
                    CALL fn_obtiene_precio_valor_fondo()
                 END IF  
                  
                 CALL f_forma.setElementText("lbl_reg_pag",v_etiqueta)
                 CALL f_forma.setElementText("lbl_precio",v_etiqueta_precio)
                 CALL f_forma.setElementText("lbl_dis_dae",v_etiqueta_dae)
                  
                 DISPLAY ARRAY arr_anteriores TO src_anteriores.* ATTRIBUTES (ACCEPT = FALSE , CANCEL = FALSE )
                   BEFORE DISPLAY
                     LET v_tot_mdt = r_arr_mandatos.getLength()

                     DISPLAY ARRAY r_arr_mandatos TO svr_servicios.*  ATTRIBUTES (COUNT=v_tot_mdt, ACCEPT = FALSE , CANCEL = FALSE )
                       BEFORE DISPLAY 
                         CALL f_forma.setElementHidden("gr_rubros",FALSE) --Muestra la sección del detalle por rubros
                         CALL fn_obtiene_rubros()

                         ON ACTION cancelar 
                            EXIT PROGRAM 

                         ON ACTION reporte
                            CALL arr_superiores.appendElement()
                            CALL arr_anteriores.appendElement()
                            CALL arr_superiores.appendElement()
                            CALL arr_anteriores.appendElement()
                            CALL fn_genera_reporte_saldos()
                     END DISPLAY 
                 END DISPLAY 
             END DISPLAY            
      END INPUT 
    END DIALOG 
  CLOSE WINDOW v_consulta
END MAIN 

-- Objetivo: Consulta la información para las cifras de control de dispersión de acuerdo a aun folio
FUNCTION fn_consulta_saldos_dispersion()
  DEFINE 
    v_indice                 SMALLINT,
    v_final                  SMALLINT  

  LET v_indice                      = 1
  LET v_final                       = 4
  LET v_avance_pago_mayor_apo       = 0.00
  LET v_avance_pago_mayor_amo       = 0.00
  LET v_avance_pago_menor_apo       = 0.00
  LET v_avance_pago_menor_amo       = 0.00

  LET v_cuentas_pago_sin_sup        = 0
  LET v_cuentas_pago_sin_avance_sup = 0
  LET v_cuentas_pago_mayor_sup      = 0
  LET v_cuentas_pago_menor_sup      = 0

  LET v_cuentas_abono_esp           = 0
  LET v_cuentas_cargo_esp           = 0

  LET v_cuentas_pago_sin_avance_ant = 0
  LET v_cuentas_virtual_sin_ant     = 0
  LET v_cuentas_virtual_mayor_ant   = 0
  LET v_cuentas_virtual_menor_ant   = 0
   
  --Asigna descripciones
  LET arr_superiores[1].f_descripcion_sup = "Pago sin Avance"
  LET arr_superiores[2].f_descripcion_sup = "Pago Asociado a Avance sin Diferencia"
  LET arr_superiores[3].f_descripcion_sup = "Pago Asociado a Avance Pago > Avance"
  LET arr_superiores[4].f_descripcion_sup = "Pago Asociado a Avance Pago < Avance"
  LET arr_superiores[5].f_descripcion_sup = "Sumatorias"
  LET arr_superiores[6].f_descripcion_sup = "Total"

  LET arr_anteriores[1].f_descripcion_ant = "Pago sin Avance"
  LET arr_anteriores[2].f_descripcion_ant = "Pago Asociado a Pago Virtual sin Diferencia"
  LET arr_anteriores[3].f_descripcion_ant = "Pago Asociado a Pago Virtual Pago > PV"
  LET arr_anteriores[4].f_descripcion_ant = "Pago Asociado a Pago Virtual Pago < PV"
  LET arr_anteriores[5].f_descripcion_ant = "Sumatorias"
  LET arr_anteriores[6].f_descripcion_ant = "Total"

  LET v_total_sin_dif = 0

  --Obtiene folio de registro de pagos
  SELECT folio_referencia
  INTO   v_folio_reg_pag 
  FROM   glo_folio
  WHERE  folio = f_folio
   
  --- PAGOS DEL 6°de 2005 y posteriores
  ---PAGO
  ---PAGO SIN AVANCE
  SELECT SUM(imp_ap_pat) apo, SUM(imp_am_cre) amo, COUNT (*) cuentas
  INTO   arr_superiores[1].f_pago_apo, arr_superiores[1].f_pago_amo,
         v_cuentas_pago_sin_avance_sup
  FROM   dis_interface_hs
  WHERE  folio_liquida = f_folio
  AND    periodo_pago  > '200505'; 
   
  LET arr_superiores[1].f_avance_apo  = 0.00
  LET arr_superiores[1].f_avance_amo  = 0.00
  LET arr_superiores[1].f_entrega_apo = arr_superiores[1].f_pago_apo
  LET arr_superiores[1].f_entrega_amo = arr_superiores[1].f_pago_amo

  ---PAGO
  ---PAGO ASOCIADO A AVANCE SIN DIFERENCIA
  SELECT SUM(monto_apo_pag) apo, COUNT (*)
  INTO   arr_superiores[2].f_pago_apo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_apo = 0;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_sin_sup = v_cuentas_pago_sin_sup + v_sumado
      
  SELECT SUM(monto_amo_pag) amo, COUNT (*)
  INTO   arr_superiores[2].f_pago_amo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_amo = 0;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_sin_sup = v_cuentas_pago_sin_sup + v_sumado

  ---AVANCE
  ---PAGO ASOCIADO A AVANCE SIN DIFERENCIA
  SELECT SUM(monto_apo_avance) apo, COUNT (*)
  INTO   arr_superiores[2].f_avance_apo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_apo = 0;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_sin_sup = v_cuentas_pago_sin_sup + v_sumado

  SELECT SUM(monto_amo_avance) amo, COUNT (*)
  INTO   arr_superiores[2].f_avance_amo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_amo = 0;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_sin_sup = v_cuentas_pago_sin_sup + v_sumado

  LET arr_superiores[2].f_entrega_apo = arr_superiores[2].f_pago_apo - arr_superiores[2].f_avance_apo
  LET arr_superiores[2].f_entrega_amo = arr_superiores[2].f_pago_amo - arr_superiores[2].f_avance_amo

  ###
  ---PAGO
  ---PAGO ASOCIADO A AVANCE PAGO > AVANCE
  --Aportaciones 
  SELECT SUM(comp.monto_apo_pag) pago_mayor_apo, SUM(comp.monto_apo_avance) avan_mayor_apo
  INTO   arr_superiores[3].f_pago_apo, arr_superiores[3].f_avance_apo
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_apo      = 2;

  SELECT COUNT(*) apo
  INTO   v_sumado
  FROM   dis_compensa_avance di,
         dis_det_avance_pago av
  WHERE  folio_dis                   = f_folio
  AND    av.id_dis_det_avance_pago   = di.id_dis_det_avance_pago
  AND   ((di.edo_compensa_apo IN (2) AND av.monto_dif_apo <>0));
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_mayor_sup = v_cuentas_pago_mayor_sup + v_sumado
  
  ---AVANCE
  ---PAGO ASOCIADO A AVANCE PAGO > AVANCE
  --Amortizaciones
  SELECT SUM(comp.monto_amo_pag) pago_mayor_amo, SUM(comp.monto_amo_avance) avan_mayor_amo
  INTO   arr_superiores[3].f_pago_amo, arr_superiores[3].f_avance_amo
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_amo      = 2;

  SELECT COUNT(*) amo
  INTO   v_sumado
  FROM   dis_compensa_avance di,
         dis_det_avance_pago av
  WHERE  folio_dis                 = f_folio
  AND    av.id_dis_det_avance_pago = di.id_dis_det_avance_pago
  AND    ((di.edo_compensa_amo IN (2) AND av.monto_dif_amo <>0));
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_mayor_sup = v_cuentas_pago_mayor_sup + v_sumado

  --Realiza diferencia para obtener la entrega a cartera
  LET arr_superiores[3].f_entrega_apo = arr_superiores[3].f_pago_apo - arr_superiores[3].f_avance_apo
  LET arr_superiores[3].f_entrega_amo = arr_superiores[3].f_pago_amo - arr_superiores[3].f_avance_amo 
  
  ---PAGO
  ---PAGO ASOCIADO A AVANCE PAGO < AVANCE
  --Aportaciones
  SELECT SUM(comp.monto_apo_pag) pago_menor_apo, SUM(comp.monto_apo_avance) avan_menor_apo,COUNT (*)
  INTO   arr_superiores[4].f_pago_apo, arr_superiores[4].f_avance_apo, v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_apo      = 1;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_menor_sup = v_cuentas_pago_menor_sup + v_sumado

  ---AVANCE
  ---PAGO ASOCIADO A AVANCE PAGO < AVANCE
  --Amortizaciones
  SELECT SUM(comp.monto_amo_pag) pago_menor_amo, SUM(comp.monto_amo_avance) avan_menor_amo, COUNT (*)
  INTO   arr_superiores[4].f_pago_amo, arr_superiores[4].f_avance_amo, v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_amo      = 1;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_pago_menor_sup = v_cuentas_pago_menor_sup + v_sumado

  --Realiza diferencia para obtener la entrega a cartera
  LET arr_superiores[4].f_entrega_apo = arr_superiores[4].f_pago_apo - arr_superiores[4].f_avance_apo
  LET arr_superiores[4].f_entrega_amo = arr_superiores[4].f_pago_amo - arr_superiores[4].f_avance_amo 

  -- TOTAL CUENTAS INTERFACE > $2
  -- Interface Especial Abono
  SELECT COUNT(*)
  INTO   v_sumado  
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_apo      = 2
  AND    ava.monto_dif_apo*(-1)     > 2;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_abono_esp = v_cuentas_abono_esp + v_sumado

  SELECT COUNT(*)
  INTO   v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_amo      = 2
  AND    ava.monto_dif_amo*(-1)     > 2;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_abono_esp = v_cuentas_abono_esp + v_sumado
  
  -- Interface Especial Cargo
  SELECT COUNT(*)
  INTO   v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_apo      = 1
  AND    ava.monto_dif_apo          > 2;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_cargo_esp = v_cuentas_cargo_esp + v_sumado

  SELECT COUNT(*)
  INTO   v_sumado  
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_amo      = 1
  AND    ava.monto_dif_amo          > 2;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_cargo_esp = v_cuentas_cargo_esp + v_sumado

  --- PAGOS DEL 5°de 2005 y anteriores
  ---PAGO
  ---PAGO SIN AVANCE
  SELECT SUM(imp_ap_pat) apo, SUM(imp_am_cre) amo, COUNT (*)
  INTO   arr_anteriores[1].f_pago_apo, arr_anteriores[1].f_pago_amo,
         v_cuentas_pago_sin_avance_ant
  FROM   dis_interface_hs
  WHERE  folio_liquida = f_folio
  AND    periodo_pago <= '200505'; 

  --Asigna la entrega directa a cartera dado que no existen diferencias
  LET arr_anteriores[1].f_virtual_apo = 0.00
  LET arr_anteriores[1].f_virtual_amo = 0.00
  LET arr_anteriores[1].f_entrega_apo = arr_anteriores[1].f_pago_apo
  LET arr_anteriores[1].f_entrega_amo = arr_anteriores[1].f_pago_amo
   
  ---PAGO
  ---PAGO ASOCIADO A AVANCE SIN DIFERENCIA
  SELECT SUM(monto_apo_pag) apo, COUNT (*)
  INTO   arr_anteriores[2].f_pago_apo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_apo = 3;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_virtual_sin_ant = v_cuentas_virtual_sin_ant + v_sumado
   
  SELECT SUM(monto_amo_pag) amo, COUNT (*)
  INTO   arr_anteriores[2].f_pago_amo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_amo = 3;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_virtual_sin_ant = v_cuentas_virtual_sin_ant + v_sumado

  ---AVANCE
  ---PAGO ASOCIADO A AVANCE SIN DIFERENCIA
  SELECT SUM(monto_apo_avance) apo, COUNT (*)
  INTO   arr_anteriores[2].f_virtual_apo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_apo = 3;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_virtual_sin_ant = v_cuentas_virtual_sin_ant + v_sumado

  SELECT SUM(monto_amo_avance) amo, COUNT (*)
  INTO   arr_anteriores[2].f_virtual_amo, v_sumado
  FROM   dis_compensa_avance
  WHERE  folio_dis        = f_folio
  AND    edo_compensa_amo = 3;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN 
     LET v_sumado = 0 
  END IF

  LET v_cuentas_virtual_sin_ant = v_cuentas_virtual_sin_ant + v_sumado

  --Realiza diferencia para obtener entrega a cartera
  LET arr_anteriores[2].f_entrega_apo = arr_anteriores[2].f_pago_apo - arr_anteriores[2].f_virtual_apo
  LET arr_anteriores[2].f_entrega_amo = arr_anteriores[2].f_pago_amo - arr_anteriores[2].f_virtual_amo
   
  ###
  ---PAGO
  ---PAGO ASOCIADO A AVANCE PAGO > AVANCE
  --Aportaciones
  SELECT SUM(comp.monto_apo_pag) pago_mayor_apo, SUM(comp.monto_apo_avance) avan_mayor_apo, COUNT (*)
  INTO   arr_anteriores[3].f_pago_apo, arr_anteriores[3].f_virtual_apo, v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_apo      = 5;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN
     LET v_sumado = 0
  END IF

  LET v_cuentas_virtual_mayor_ant = v_cuentas_virtual_mayor_ant + v_sumado
 
  ---AVANCE
  ---PAGO ASOCIADO A AVANCE PAGO > AVANCE
  --Amortizaciones
  SELECT SUM(comp.monto_amo_pag) pago_mayor_amo, SUM(comp.monto_amo_avance) avan_mayor_amo, COUNT (*)
  INTO   arr_anteriores[3].f_pago_amo, arr_anteriores[3].f_virtual_amo, v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_amo      = 5;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN
     LET v_sumado = 0
  END IF

  LET v_cuentas_virtual_mayor_ant = v_cuentas_virtual_mayor_ant + v_sumado

  --Realiza diferencia para obtener entrega a cartera
  LET arr_anteriores[3].f_entrega_apo = arr_anteriores[3].f_pago_apo - arr_anteriores[3].f_virtual_apo
  LET arr_anteriores[3].f_entrega_amo = arr_anteriores[3].f_pago_amo - arr_anteriores[3].f_virtual_amo

  ---PAGO
  ---PAGO ASOCIADO A AVANCE PAGO < AVANCE
  --Aportaciones
  SELECT SUM(comp.monto_apo_pag) pago_menor_apo, SUM(comp.monto_apo_avance) avan_menor_apo, COUNT (*)
  INTO   arr_anteriores[4].f_pago_apo, arr_anteriores[4].f_virtual_apo, v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_apo      = 4;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN
     LET v_sumado = 0
  END IF

  LET v_cuentas_virtual_menor_ant = v_cuentas_virtual_menor_ant + v_sumado

  ---AVANCE
  ---PAGO ASOCIADO A AVANCE PAGO < AVANCE
  --Amortizaciones
  SELECT SUM(comp.monto_amo_pag) pago_menor_amo, SUM(comp.monto_amo_avance) avan_menor_amo, COUNT (*)
  INTO   arr_anteriores[4].f_pago_amo, arr_anteriores[4].f_virtual_amo, v_sumado
  FROM   dis_det_avance_pago ava,
         dis_compensa_avance comp
  WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
  AND    comp.folio_dis             = f_folio
  AND    comp.edo_compensa_amo      = 4;
  --Valida que no sea nulo
  IF v_sumado IS NULL THEN
     LET v_sumado = 0
  END IF

  LET v_cuentas_virtual_menor_ant = v_cuentas_virtual_menor_ant + v_sumado

  --Obtiene total de registros sin diferencia en avance
  SELECT COUNT(*)
  INTO   v_total_sin_dif  
  FROM   dis_compensa_avance a
  WHERE  a.folio_dis          = f_folio
  AND    ((a.edo_compensa_amo =  0    AND
           a.monto_amo_avance <> 0)   OR
          (a.edo_compensa_apo =  0    AND
           a.monto_apo_avance <> 0));
  --Valida que no sea nulo
  IF v_total_sin_dif IS NULL THEN 
     LET v_total_sin_dif = 0 
  END IF           

  --Obtiene total de registros sin diferencia en avance virtual
  SELECT COUNT(*)
  INTO   v_cuentas_virtual_sin_ant  
  FROM   dis_compensa_avance a
  WHERE  a.folio_dis          = f_folio
  AND    ((a.edo_compensa_amo =  3    AND
           a.monto_amo_avance <> 0)   OR
          (a.edo_compensa_apo =  3    AND
           a.monto_apo_avance <> 0));
  --Valida que no sea nulo
  IF v_cuentas_virtual_sin_ant IS NULL THEN 
     LET v_cuentas_virtual_sin_ant = 0 
  END IF
       
  --Realiza diferencia para obtener entrega a cartera
  LET arr_anteriores[4].f_entrega_apo = arr_anteriores[4].f_pago_apo - arr_anteriores[4].f_virtual_apo
  LET arr_anteriores[4].f_entrega_amo = arr_anteriores[4].f_pago_amo - arr_anteriores[4].f_virtual_amo

  #####################################
  DISPLAY "v_cuentas_pago_sin_sup        -- ",v_cuentas_pago_sin_sup
  DISPLAY "v_cuentas_pago_sin_avance_sup -- ",v_cuentas_pago_sin_avance_sup
  DISPLAY "v_cuentas_pago_sin_avance_sup -- ",v_total_sin_dif  
  DISPLAY "v_cuentas_pago_mayor_sup      -- ",v_cuentas_pago_mayor_sup
  DISPLAY "v_cuentas_pago_menor_sup      -- ",v_cuentas_pago_menor_sup

  DISPLAY "v_cuentas_pago_sin_avance_ant -- ",v_cuentas_pago_sin_avance_ant
  DISPLAY "v_cuentas_virtual_sin_ant     -- ",v_cuentas_virtual_sin_ant
  DISPLAY "v_cuentas_virtual_mayor_ant   -- ",v_cuentas_virtual_mayor_ant
  DISPLAY "v_cuentas_virtual_menor_ant   -- ",v_cuentas_virtual_menor_ant
  
  ---########   RESTITUCIÓN   ##########
  SELECT SUM(DE.monto_aportacion) aportacion_rest, COUNT (*) cuentas
  INTO   f_restitucion, v_cuentas_restitucion
  FROM   dse_devolucion DE
  WHERE  DE.folio_referencia = f_folio;
  IF f_restitucion IS NULL THEN 
     LET f_restitucion = 0.00 
  END IF  

  ---########   APORTACIONES SUBSECUENTES 43 BIS   ##########
  SELECT SUM(EF.imp_ap_pat) aportacion_sub, COUNT (*) cuentas
  INTO   f_apo_subs, v_cuentas_apo_subs
  FROM   dis_interface_ef EF
  WHERE  folio_liquida = f_folio;
  IF f_apo_subs IS NULL THEN 
     LET f_apo_subs = 0.00 
  END IF  
   
  ---########   MANDATOS   ##########
  --CALL fn_obtiene_inf_mandatos()
   
  --Valida que no haya datos nulos
  FOR v_indice = 1 TO v_final  
   IF arr_superiores[v_indice].f_pago_apo    IS NULL THEN LET arr_superiores[v_indice].f_pago_apo    = 0.00 END IF 
   IF arr_superiores[v_indice].f_pago_amo    IS NULL THEN LET arr_superiores[v_indice].f_pago_amo    = 0.00 END IF 
   IF arr_superiores[v_indice].f_avance_apo  IS NULL THEN LET arr_superiores[v_indice].f_avance_apo  = 0.00 END IF 
   IF arr_superiores[v_indice].f_avance_amo  IS NULL THEN LET arr_superiores[v_indice].f_avance_amo  = 0.00 END IF 
   IF arr_superiores[v_indice].f_entrega_apo IS NULL THEN LET arr_superiores[v_indice].f_entrega_apo = 0.00 END IF 
   IF arr_superiores[v_indice].f_entrega_amo IS NULL THEN LET arr_superiores[v_indice].f_entrega_amo = 0.00 END IF 
   
   IF arr_anteriores[v_indice].f_pago_apo    IS NULL THEN LET arr_anteriores[v_indice].f_pago_apo    = 0.00 END IF
   IF arr_anteriores[v_indice].f_pago_amo    IS NULL THEN LET arr_anteriores[v_indice].f_pago_amo    = 0.00 END IF 
   IF arr_anteriores[v_indice].f_virtual_apo IS NULL THEN LET arr_anteriores[v_indice].f_virtual_apo = 0.00 END IF 
   IF arr_anteriores[v_indice].f_virtual_amo IS NULL THEN LET arr_anteriores[v_indice].f_virtual_amo = 0.00 END IF 
   IF arr_anteriores[v_indice].f_entrega_apo IS NULL THEN LET arr_anteriores[v_indice].f_entrega_apo = 0.00 END IF 
   IF arr_anteriores[v_indice].f_entrega_amo IS NULL THEN LET arr_anteriores[v_indice].f_entrega_amo = 0.00 END IF  
  END FOR 

  ---########   OBTIENE SUMATORIAS   ##########
  LET arr_anteriores[5].f_pago_apo    = arr_anteriores[1].f_pago_apo +
                                        arr_anteriores[2].f_pago_apo +
                                        arr_anteriores[3].f_pago_apo +
                                        arr_anteriores[4].f_pago_apo
                                          
  LET arr_anteriores[5].f_pago_amo    = arr_anteriores[1].f_pago_amo +
                                        arr_anteriores[2].f_pago_amo +
                                        arr_anteriores[3].f_pago_amo +
                                        arr_anteriores[4].f_pago_amo
   
  LET arr_anteriores[5].f_virtual_apo = arr_anteriores[1].f_virtual_apo +
                                        arr_anteriores[2].f_virtual_apo +
                                        arr_anteriores[3].f_virtual_apo +
                                        arr_anteriores[4].f_virtual_apo

  LET arr_anteriores[5].f_virtual_amo = arr_anteriores[1].f_virtual_amo +
                                        arr_anteriores[2].f_virtual_amo +
                                        arr_anteriores[3].f_virtual_amo +
                                        arr_anteriores[4].f_virtual_amo

  LET arr_anteriores[5].f_entrega_apo = arr_anteriores[1].f_entrega_apo +
                                        arr_anteriores[2].f_entrega_apo +
                                        arr_anteriores[3].f_entrega_apo +
                                        arr_anteriores[4].f_entrega_apo

  LET arr_anteriores[5].f_entrega_amo = arr_anteriores[1].f_entrega_amo +
                                        arr_anteriores[2].f_entrega_amo +
                                        arr_anteriores[3].f_entrega_amo +
                                        arr_anteriores[4].f_entrega_amo
  ##################################################
  LET arr_superiores[5].f_pago_apo    = arr_superiores[1].f_pago_apo +
                                        arr_superiores[2].f_pago_apo +
                                        arr_superiores[3].f_pago_apo +
                                        arr_superiores[4].f_pago_apo
                                          
  LET arr_superiores[5].f_pago_amo    = arr_superiores[1].f_pago_amo +
                                        arr_superiores[2].f_pago_amo +
                                        arr_superiores[3].f_pago_amo +
                                        arr_superiores[4].f_pago_amo
   
  LET arr_superiores[5].f_avance_apo  = arr_superiores[1].f_avance_apo +
                                        arr_superiores[2].f_avance_apo +
                                        arr_superiores[3].f_avance_apo +
                                        arr_superiores[4].f_avance_apo

  LET arr_superiores[5].f_avance_amo  = arr_superiores[1].f_avance_amo +
                                        arr_superiores[2].f_avance_amo +
                                        arr_superiores[3].f_avance_amo +
                                        arr_superiores[4].f_avance_amo

  LET arr_superiores[5].f_entrega_apo = arr_superiores[1].f_entrega_apo +
                                        arr_superiores[2].f_entrega_apo +
                                        arr_superiores[3].f_entrega_apo +
                                        arr_superiores[4].f_entrega_apo

  LET arr_superiores[5].f_entrega_amo = arr_superiores[1].f_entrega_amo +
                                        arr_superiores[2].f_entrega_amo +
                                        arr_superiores[3].f_entrega_amo +
                                        arr_superiores[4].f_entrega_amo

  ---########   OBTIENE TOTALES   ##########
  LET arr_superiores[6].f_pago_amo    = arr_superiores[5].f_pago_apo    + arr_superiores[5].f_pago_amo
  LET arr_superiores[6].f_avance_amo  = arr_superiores[5].f_avance_apo  + arr_superiores[5].f_avance_amo  
  LET arr_superiores[6].f_entrega_amo = arr_superiores[5].f_entrega_apo + arr_superiores[5].f_entrega_amo

  LET arr_anteriores[6].f_pago_amo    = arr_anteriores[5].f_pago_apo    + arr_anteriores[5].f_pago_amo
  LET arr_anteriores[6].f_virtual_amo = arr_anteriores[5].f_virtual_apo + arr_anteriores[5].f_virtual_amo  
  LET arr_anteriores[6].f_entrega_amo = arr_anteriores[5].f_entrega_apo + arr_anteriores[5].f_entrega_amo    
      
END FUNCTION 

#OBJETIVO: Función para recuperar la información de Mandatos
FUNCTION fn_obtiene_inf_mandatos()
  DEFINE 
    QryTxt                   STRING,
    v_indice_mdt             INTEGER

   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio,Sin INTO v_tbl_mov --por folio

  LET QryTxt = "   SELECT a.movimiento||'-'||b.movimiento_desc as Servicio, ",
               "\n        SUM(a.monto_pesos), ",
               "\n        COUNT(a.id_derechohabiente)",
               "\n FROM ",v_tbl_mov," a,",
               "\n        cat_movimiento b ",
               "\n WHERE  a.movimiento   IN (312,322,332)",
               "\n AND    a.folio_liquida = ",f_folio,
               "\n AND    a.movimiento    = b.movimiento",
               "\n GROUP BY 1",
               "\n ORDER BY 1"
  PREPARE prp_cons_mdt FROM QryTxt

  LET v_indice_mdt      = 1
  LET f_man_monto_pesos = 0.00
  LET f_man_tot_ctas    = 0.00

  DECLARE cur_cons_mdt CURSOR FOR prp_cons_mdt
  FOREACH cur_cons_mdt INTO r_arr_mandatos[v_indice_mdt].v_servicios,
                            r_arr_mandatos[v_indice_mdt].v_monto_pesos,
                            r_arr_mandatos[v_indice_mdt].v_total_cuentas
                                      
    LET f_man_monto_pesos = f_man_monto_pesos + 
                            r_arr_mandatos[v_indice_mdt].v_monto_pesos
                                 
    LET f_man_tot_ctas    = f_man_tot_ctas + 
                            r_arr_mandatos[v_indice_mdt].v_total_cuentas

    LET v_indice_mdt      = v_indice_mdt + 1
  END FOREACH

  --Asigna variables para el reporte
  LET v_desc_predial       = r_arr_mandatos[1].v_servicios
  LET v_desc_servicios     = r_arr_mandatos[2].v_servicios
  LET v_desc_mantenimiento = r_arr_mandatos[3].v_servicios

  LET v_sum_predial        = r_arr_mandatos[1].v_monto_pesos
  LET v_sum_servicios      = r_arr_mandatos[2].v_monto_pesos
  LET v_sum_mantenimiento  = r_arr_mandatos[3].v_monto_pesos

  LET v_tot_predial        = r_arr_mandatos[1].v_total_cuentas
  LET v_tot_servicios      = r_arr_mandatos[2].v_total_cuentas
  LET v_tot_mantenimiento  = r_arr_mandatos[3].v_total_cuentas

  DISPLAY BY NAME f_man_monto_pesos, f_man_tot_ctas
                         
  CALL r_arr_mandatos.deleteElement(v_indice_mdt)

END FUNCTION 

--Obtiene total de registros, aportación y amortización que quedaron fuera por
--número de crédito en ceros
FUNCTION fn_obtiene_registros_fuera()
  DEFINE
    v_query_inc              STRING,
    v_ind_inc                INTEGER 

  LET v_ind_inc = 1

  --Consulta información de las cuentas inconsistentes del folio
  LET v_query_inc = "\n SELECT SUM(imp_ap_pat), SUM(imp_am_cre), COUNT(*), tpo_inconsistente",
                    "\n FROM dis_info_inconsistente",
                    "\n WHERE folio_liquida   = ",f_folio,
                    "\n AND tpo_inconsistente = 0", --los que no tienen número de crédito
                    "\n GROUP BY 4",
                    "\n UNION ALL",
                    "\n SELECT SUM(imp_ap_pat), SUM(imp_am_cre), COUNT(*), 0",
                    "\n FROM dis_crd_ceros",
                    "\n WHERE folio_liquida   = ",f_folio,
                    "\n GROUP BY 4"
  DISPLAY v_query_inc
  PREPARE prp_info_inconsistente FROM v_query_inc
  DECLARE cur_info_inconsistente CURSOR FOR prp_info_inconsistente  
  FOREACH cur_info_inconsistente INTO v_tot_aportacion_fuera,
                                      v_tot_amortizacion_fuera,
                                      v_tot_registros_fuera,
                                      v_tpo_inconsistente
                                       
    LET v_ind_inc = v_ind_inc + 1
  END FOREACH 

  IF v_tot_aportacion_fuera   IS NULL THEN LET v_tot_aportacion_fuera   = 0.00 END IF
  IF v_tot_amortizacion_fuera IS NULL THEN LET v_tot_amortizacion_fuera = 0.00 END IF 
  IF v_tot_registros_fuera    IS NULL THEN LET v_tot_registros_fuera    = 0    END IF  
   
  DISPLAY BY NAME v_tot_aportacion_fuera, v_tot_amortizacion_fuera, v_tot_registros_fuera

  END FUNCTION 

--Obtiene el precio del valor del fondo el día que se liquidó
FUNCTION fn_obtiene_precio_valor_fondo()
--v_etiqueta_precio
  DEFINE 
     v_precio_fondo          LIKE glo_valor_fondo.precio_fondo,
     v_fecha_precio          DATE,
     v_query_inc             STRING

   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio,Sin INTO v_tbl_mov --por folio
  --Obtiene fecha de liquidación
  LET v_query_inc =  "\n SELECT DISTINCT f_liquida "
                    ,"\n FROM   ",v_tbl_mov
                    ,"\n WHERE  folio_liquida = ?"
  PREPARE prp_mov_a FROM v_query_inc
  EXECUTE prp_mov_a USING f_folio INTO v_fecha_precio

  IF v_fecha_precio IS NULL OR v_fecha_precio = '12-31-1899' THEN 
     SELECT DISTINCT f_liquida 
     INTO   v_fecha_precio
     FROM   dis_preliquida 
     WHERE  folio_liquida = f_folio 
  END IF 

  DISPLAY "v_fecha_precio: ", v_fecha_precio
      
  SELECT precio_fondo
  INTO   v_precio_fondo
  FROM   glo_valor_fondo
  WHERE  f_valuacion = v_fecha_precio
  AND    fondo       = 11
  IF v_fecha_precio IS NOT NULL THEN 
     LET v_etiqueta_precio = "El precio de fondo es ",v_precio_fondo USING  "######.&&&&&&", " al día ", v_fecha_precio USING "dd-mm-yyyy" 
  END IF 

END FUNCTION 

#Objetivo: Obtiene los totales por rubro que se envían a cartera, a interfaces e inconsistentes
FUNCTION fn_obtiene_rubros()
  DEFINE  
    v_query_rub              STRING, 
    v_ind_rub                INTEGER

  LET f_tot_rub_aivs  = 0.00
  LET f_tot_rub_aport = 0.00
  LET f_tot_rub_amort = 0.00

  LET v_ind_rub   = 1
  --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
  EXECUTE fn_tbl_mov USING Por_Folio,f_folio,Sin INTO v_tbl_mov --por folio
  --f_folio
  LET v_query_rub = "\n SELECT 'INTERFACE CARTERA' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(aiv_ap_pat) AIVS, ", 
                    "\n        SUM(imp_ap_pat) APORT, ",
                    "\n        SUM(imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_interface_hs ",
                    "\n WHERE  folio_liquida = ", f_folio,
                    "\n AND    tipo_hs       = 0",
                    "\n UNION ALL",
                    "\n SELECT 'INTERFACE CARTERA EDOS - MUN' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        0.00 AIVS, ",
                    "\n        0.00 APORT, ",
                    "\n        SUM(imp_am_cre)  AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_interface_hs ",
                    "\n WHERE  folio_liquida = ", f_folio, 
                    "\n AND    tipo_hs       = 1",
                    "\n UNION ALL ",
                    "\n SELECT 'INTERFACE ENTIDADES FINANCIERAS' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(aiv_ap_pat) AIVS, ",
                    "\n        SUM(imp_ap_pat) APORT, ",
                    "\n        0 AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_interface_ef ",
                    "\n WHERE  folio_liquida = ", f_folio, 
                    "\n UNION ALL ",
                    "\n SELECT 'SIN NUMERO DE CREDITO' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(aiv_ap_pat) AIVS, ",
                    "\n        SUM(imp_ap_pat) APORT, ",
                    "\n        SUM(imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_info_inconsistente ",
                    "\n WHERE  folio_liquida     = ", f_folio,
                    "\n AND    tpo_inconsistente = 0 ",
                    "\n UNION ALL ",
                    "\n SELECT 'SIN NUMERO DE CREDITO' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(aiv_ap_pat) AIVS, ",
                    "\n        SUM(imp_ap_pat) APORT, ",
                    "\n        SUM(imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_crd_ceros ",
                    "\n WHERE  folio_liquida = ", f_folio,
                    "\n UNION ALL ",
                    "\n SELECT 'ACLARATORIO SIN DESTINO' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(aiv_ap_pat) AIVS, ",
                    "\n        SUM(imp_ap_pat) APORT, ",
                    "\n        SUM(imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_info_inconsistente ",
                    "\n WHERE  folio_liquida     = ", f_folio,
                    "\n AND    tpo_inconsistente = 2 ",
                    "\n UNION ALL ",
                    "\n SELECT 'ACLARATORIO AFORE SIN MARCA CONFIRMADA' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(aiv_ap_pat) AIVS, ",
                    "\n        SUM(imp_ap_pat) APORT, ",
                    "\n        SUM(imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_info_inconsistente ",
                    "\n WHERE  folio_liquida     = ", f_folio,
                    "\n AND    tpo_inconsistente = 3 ",                    
                    "\n UNION ALL ",
                    "\n SELECT 'INTERFACE RESTITUCION' RUBRO, ",
                    "\n        subcuenta, ",
                    "\n        SUM(monto_aivs) AIVS, ",
                    "\n        SUM(monto_pesos) APORT, ",
                    "\n        0 AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dse_devolucion ",
                    "\n WHERE  folio_referencia = ", f_folio, 
                    "\n AND    subcuenta       IN (4,8) ",
                    "\n GROUP BY 1,2 ",
                    "\n UNION ALL ",
                    "\n SELECT 'INTERFACE RESTITUCION' RUBRO, ",
                    "\n        subcuenta, ",
                    "\n        SUM(monto_aivs) AIVS, ",
                    "\n        0 APORT, ",
                    "\n        SUM(monto_pesos) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dse_devolucion ",
                    "\n WHERE  folio_referencia = ", f_folio, 
                    "\n AND    subcuenta        = 41",
                    "\n GROUP BY 1,2 ",
                    "\n UNION ALL",
                    "\n SELECT 'PAGO TRM' RUBRO, ",
                    "\n        subcuenta,  ",
                    "\n        SUM(monto_acciones) AIVS, ",
                    "\n        SUM(monto_pesos) APORT, ",
                    "\n        0 AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM ",v_tbl_mov,
                    "\n WHERE  folio_liquida = ", f_folio, 
                    "\n AND    movimiento    = 1202",
                    "\n AND    subcuenta     = 4",
                    "\n GROUP BY 1,2",
                    "\n UNION ALL",
                    "\n SELECT 'PAGO TRM' RUBRO, ",
                    "\n        subcuenta, ",
                    "\n        SUM(monto_acciones) AIVS, ",
                    "\n        0 APORT, ",
                    "\n        SUM(monto_pesos) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM ",v_tbl_mov,
                    "\n WHERE  folio_liquida = ", f_folio, 
                    "\n AND    movimiento    = 1202 ",
                    "\n AND    subcuenta     = 41",
                    "\n GROUP BY 1,2",
                    "\n UNION ALL",
                    "\n SELECT 'APO VOL RISS LQ' RUBRO, ",
                    "\n        subcuenta, ",
                    "\n        SUM(monto_acciones) AIVS, ",
                    "\n        SUM(monto_pesos) APORT, ",
                    "\n        0 AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM ",v_tbl_mov,
                    "\n WHERE  folio_liquida = ", f_folio, 
                    "\n AND    movimiento    = 1532 ",
                    "\n AND    subcuenta     = 55",
                    "\n GROUP BY 1,2",
                    "\n UNION ALL",
                    "\n SELECT 'APO VOL RISS ACL' RUBRO, ",
                    "\n        subcuenta, ",
                    "\n        SUM(monto_acciones) AIVS, ",
                    "\n        SUM(monto_pesos) APORT, ",
                    "\n        0 AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM ",v_tbl_mov,
                    "\n WHERE  folio_liquida = ", f_folio, 
                    "\n AND    movimiento    = 1542 ",
                    "\n AND    subcuenta     = 55",
                    "\n GROUP BY 1,2"
                    {"\n UNION ALL",
                    "\n SELECT 'INTERFACE CARTERA PORT REC' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(aiv_ap_pat) AIVS, ",
                    "\n        SUM(imp_ap_pat) APORT, ",
                    "\n        SUM(imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_interface_prt ",
                    "\n WHERE  folio_liquida = ", f_folio, 
                    "\n AND    tipo_prt      = 2" --Receptora
                    "\n SELECT 'CRÉDITO FAMILIAR' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(b.aiv_ap_pat) AIVS, ",
                    "\n        SUM(b.imp_ap_pat) APORT, ",
                    "\n        SUM(b.imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_interface_hs a, ",
                    "\n        dis_his_transaccion b ",
                    "\n WHERE  a.folio_liquida      = ", f_folio,
                    "\n AND    a.folio_liquida      = b.folio_liquida ",
                    "\n AND    a.id_derechohabiente = b.id_derechohabiente ",
                    "\n AND    b.tpo_credito        = 29 ",
                    "\n UNION ALL",
                    "\n SELECT 'CRÉDITO CORRESIDENCIAL' RUBRO, ",
                    "\n        0 SUBCUENTA, ",
                    "\n        SUM(b.aiv_ap_pat) AIVS, ",
                    "\n        SUM(b.imp_ap_pat) APORT, ",
                    "\n        SUM(b.imp_am_cre) AMORT, ",
                    "\n        COUNT(*) TOTAL ",
                    "\n FROM   dis_interface_hs a, ",
                    "\n        dis_his_transaccion b ",
                    "\n WHERE  a.folio_liquida      = ", f_folio,
                    "\n AND    a.folio_liquida      = b.folio_liquida ",
                    "\n AND    a.id_derechohabiente = b.id_derechohabiente ",
                    "\n AND    b.tpo_credito        = 30 "}
                                   
  PREPARE prp_consulta_rubro FROM v_query_rub
  DECLARE cur_consulta_rubro CURSOR FOR prp_consulta_rubro
  FOREACH cur_consulta_rubro INTO  v_arr_rubros[v_ind_rub].v_rubro,
                                   v_tipo_subcuenta,
                                   v_arr_rubros[v_ind_rub].v_aivs,
                                   v_arr_rubros[v_ind_rub].v_aport,
                                   v_arr_rubros[v_ind_rub].v_amort,
                                   v_arr_rubros[v_ind_rub].v_total

    IF v_arr_rubros[v_ind_rub].v_total = 0 THEN
       CONTINUE FOREACH
    END IF
    
    IF v_arr_rubros[v_ind_rub].v_aivs  IS NULL THEN LET v_arr_rubros[v_ind_rub].v_aivs  = 0.00 END IF
    IF v_arr_rubros[v_ind_rub].v_aport IS NULL THEN LET v_arr_rubros[v_ind_rub].v_aport = 0.00 END IF
    IF v_arr_rubros[v_ind_rub].v_amort IS NULL THEN LET v_arr_rubros[v_ind_rub].v_amort = 0.00 END IF
    IF v_arr_rubros[v_ind_rub].v_total IS NULL THEN LET v_arr_rubros[v_ind_rub].v_total = 0    END IF  

    --si son negativos los muestra positivos
    IF v_arr_rubros[v_ind_rub].v_aivs  < 0 THEN LET v_arr_rubros[v_ind_rub].v_aivs  = v_arr_rubros[v_ind_rub].v_aivs  *-1 END IF
    IF v_arr_rubros[v_ind_rub].v_aport < 0 THEN LET v_arr_rubros[v_ind_rub].v_aport = v_arr_rubros[v_ind_rub].v_aport *-1 END IF 
    IF v_arr_rubros[v_ind_rub].v_amort < 0 THEN LET v_arr_rubros[v_ind_rub].v_amort = v_arr_rubros[v_ind_rub].v_amort *-1 END IF 
                                    
    LET f_tot_rub_aivs  = f_tot_rub_aivs  + v_arr_rubros[v_ind_rub].v_aivs
    LET f_tot_rub_aport = f_tot_rub_aport + v_arr_rubros[v_ind_rub].v_aport
    LET f_tot_rub_amort = f_tot_rub_amort + v_arr_rubros[v_ind_rub].v_amort
   
    --Verificar texto de la subcuenta
    IF v_tipo_subcuenta = 0 THEN 
       LET v_arr_rubros[v_ind_rub].v_subcuenta = "0-Sin Descripción" 
    ELSE 
       SELECT subcuenta || '-' || subcuenta_desc
       INTO   v_arr_rubros[v_ind_rub].v_subcuenta
       FROM   cat_subcuenta
       WHERE  subcuenta = v_tipo_subcuenta
    END IF 
      
    LET v_ind_rub = v_ind_rub + 1
  END FOREACH 

  CALL v_arr_rubros.deleteElement(v_ind_rub)

  DISPLAY ARRAY v_arr_rubros TO sc_rubros.* ATTRIBUTES (CANCEL = FALSE )
    BEFORE DISPLAY
      --DISPLAY BY NAME f_tot_rub_aivs, f_tot_rub_aport, f_tot_rub_amort    
        IF NOT INT_FLAG THEN 
           ACCEPT DISPLAY 
        END IF  
  END DISPLAY

  CALL v_arr_rubros.appendElement()
  LET v_arr_rubros[v_ind_rub].v_rubro = "1"
  LET v_arr_rubros[v_ind_rub].v_aivs  = f_tot_rub_aivs
  LET v_arr_rubros[v_ind_rub].v_aport = f_tot_rub_aport
  LET v_arr_rubros[v_ind_rub].v_amort = f_tot_rub_amort

  DISPLAY "Salida:",v_ind_rub
  DISPLAY v_arr_rubros[v_ind_rub].v_rubro
  DISPLAY v_arr_rubros[v_ind_rub].v_aivs

END FUNCTION  

#Objetivo Genera reporte de la consulta de saldos de dispersión
FUNCTION fn_genera_reporte_saldos()
  DEFINE 
    v_mandato                INTEGER,
    v_manejador_rpt          om.SaxDocumentHandler

  LET v_mandato = 0

  DISPLAY "r_arr_mandatos.getLength() -- ",r_arr_mandatos.getLength()

  IF fgl_report_loadCurrentSettings("DISC091.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET v_manejador_rpt = fgl_report_commitCurrentSettings()
  END IF  
 
  --Si la consulta de mandatos no trae información sólo mandamos el primer registro
  LET v_mandato = 1 

  START REPORT rpt_saldos_dispersion TO XML HANDLER v_manejador_rpt
    FOR v_mandato = 1 TO arr_superiores.getLength()
        OUTPUT TO REPORT rpt_saldos_dispersion(r_arr_mandatos[v_mandato].*, --Arreglo de mandatos
                                               --arreglo periodo superior
                                               arr_superiores[v_mandato].*,
                                               --arreglo periodo anterior
                                               arr_anteriores[v_mandato].*,
                                               --Variables de montos y totales
                                               f_apo_subs, 
                                               f_restitucion, 
                                               f_man_monto_pesos, 
                                               f_man_tot_ctas,
                                               v_arr_rubros[v_mandato].*)
    END FOR 
  FINISH REPORT rpt_saldos_dispersion

END FUNCTION 

#Estructura del reporte de saldos dispersión
REPORT rpt_saldos_dispersion(p_arr_mandatos, --Arreglo de mandatos
                             p_arr_superiores,
                             p_arr_anteriores,                           
                             --Variables de montos y totales
                             p_apo_subs, 
                             p_restitucion, 
                             p_man_monto_pesos, 
                             p_man_tot_ctas,
                             p_rubros)
                             
  DEFINE p_arr_mandatos      RECORD
    v_servicios              CHAR(25),
    v_monto_pesos            LIKE cta_movimiento.monto_pesos,
    v_total_cuentas          INTEGER 
  END RECORD

  DEFINE p_arr_superiores    RECORD
    f_descripcion_ant        VARCHAR(50),
    p_pago_apo_sup           DECIMAL(22,2),
    p_pago_amo_sup           DECIMAL(22,2),
    p_avance_apo_sup         DECIMAL(22,2),
    p_avance_amo_sup         DECIMAL(22,2),
    p_entrega_apo_sup        DECIMAL(22,2),
    p_entrega_amo_sup        DECIMAL(22,2)
  END RECORD 

  DEFINE p_arr_anteriores    RECORD
    f_descripcion_ant        VARCHAR(50),
    p_pago_apo_ant           DECIMAL(22,2), 
    p_pago_amo_ant           DECIMAL(22,2),
    p_virtual_apo_ant        DECIMAL(22,2),
    p_virtual_amo_ant        DECIMAL(22,2),
    p_entrega_apo_ant        DECIMAL(22,2),
    p_entrega_amo_ant        DECIMAL(22,2)
  END RECORD 

  DEFINE 
    p_apo_subs               DECIMAL(22,2),
    p_restitucion            DECIMAL(22,2),
    p_man_monto_pesos        DECIMAL(22,2),
    p_man_tot_ctas           DECIMAL(22,2),
    v_fecha_reporte          DATE
      
  DEFINE p_rubros            RECORD 
    v_rubro                  VARCHAR (40),
    v_subcuenta              VARCHAR (40),
    v_aivs                   DECIMAL (22,2),
    v_aport                  DECIMAL (22,2),
    v_amort                  DECIMAL (22,2),
    v_total                  INTEGER
  END RECORD 
      
  FORMAT   
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario
      PRINTX f_folio
      PRINTX v_etiqueta_precio

      PRINTX --Variables de montos y totales
      PRINTX p_apo_subs 
      PRINTX p_restitucion 
      PRINTX p_man_monto_pesos 
      PRINTX p_man_tot_ctas

      PRINTX v_desc_servicios        
      PRINTX v_desc_mantenimiento    
      PRINTX v_desc_predial      
      PRINTX v_sum_servicios        
      PRINTX v_sum_mantenimiento    
      PRINTX v_sum_predial     
      PRINTX v_tot_servicios        
      PRINTX v_tot_predial         
      PRINTX v_tot_mantenimiento 

      PRINTX v_cuentas_pago_sin_avance_sup    
      PRINTX v_cuentas_pago_sin_sup    
      PRINTX v_total_sin_dif
      PRINTX v_cuentas_pago_mayor_sup         
      PRINTX v_cuentas_pago_menor_sup 

      PRINTX v_cuentas_abono_esp
      PRINTX v_cuentas_cargo_esp

      PRINTX v_cuentas_pago_sin_avance_ant    
      PRINTX v_cuentas_virtual_sin_ant        
      PRINTX v_cuentas_virtual_mayor_ant      
      PRINTX v_cuentas_virtual_menor_ant
      PRINTX v_folio_reg_pag

    ON EVERY ROW 
       PRINTX  p_arr_anteriores.*
       PRINTX  p_arr_mandatos.*
       PRINTX  p_arr_superiores.*
       PRINTX  p_rubros.*
 
END REPORT