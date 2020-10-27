################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 08/03/2019                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC39                                                   #
#Objetivo          => CONSULTA TRANSACCIONES DISPERSIÓN DE PAGOS               #
#Fecha Inicio      => 24/07/2017                                               #
################################################################################
--DATABASE safre_viv
SCHEMA safre_viv

GLOBALS
  DEFINE v_arr_trans         DYNAMIC ARRAY OF RECORD
    v_f_liq_dis              DATE,
    v_fol_dis                DECIMAL (9,0), 
    v_fol_reg                DECIMAL (9,0),
    v_per_pag                CHAR(06),
    v_num_cre                DECIMAL(10,0),
    v_ori_num_cre            CHAR(50),
    v_tip_ori                CHAR(50), 
    v_tip_cre                CHAR(50), 
    v_f_oto                  DATE, 
    v_f_liq_cre              DATE,
    v_est_cre                CHAR(50),
    des_apo_viv              CHAR(50)
  END RECORD

  DEFINE v_arr_dis           DYNAMIC ARRAY OF RECORD
    v_interface              CHAR(40),
    v_destino                CHAR(50),
    v_nrp                    CHAR(11),
    v_periodo_pago           CHAR(06),
    v_folio_sua              DECIMAL(6,0),
    v_f_pago                 DATE, 
    v_aivs                   DECIMAL(18,6),
    v_aportacion             DECIMAL(12,2),
    v_amortizacion           DECIMAL(12,2)
  END RECORD      

  DEFINE 
    v_nombre_com             CHAR(150),
    v_id_derechohabiente     DECIMAL(9,0)

  DEFINE
    d_periodo_pago           DECIMAL(6,0),
    d_f_pago                 DATE,
    d_aivs_aportacion        DECIMAL(18,6),
    d_monto_aportacion       DECIMAL(12,2),
    v_edo_compensa_apo       SMALLINT,
    v_edo_compensa_amo       SMALLINT,
    v_destino_credito        SMALLINT,
    v_tot_reg                SMALLINT,
    v_tot_aivs               DECIMAL(18,6),
    v_tot_apo                DECIMAL(12,2),
    v_tot_amo                DECIMAL(12,2),
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING,
    p_proceso_cod            SMALLINT

END GLOBALS

MAIN
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, -- Clave del usuario
    p_tipo_ejecucion         SMALLINT, -- Forma como ejecutara el programa
    p_s_titulo               STRING,   -- Titulo de la ventana
    v_nss                    CHAR(11), -- NSS
    r_existe_registro        INTEGER   --SMALLINT,

  DEFINE     
    v_ind_tra                INTEGER,
    v_ind_dis                INTEGER, 
    v_fol_liq                DECIMAL(9,0), 
    v_row                    INTEGER,
    v_row_dis                INTEGER,
    v_row_dis_i              INTEGER
                
  DEFINE 
    f_ventana                ui.Window, -- Define las propìedades de la Ventana
    f_forma                  ui.Form,   -- Define las propiedades de la forma
    i                        INTEGER, 
    v_num                    INTEGER, 
    v_num_val                INTEGER, 
    v_periodo_pago           CHAR(06)

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)
  LET p_proceso_cod    = 901

  CONNECT TO "safre_viv"
  -- si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

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
    
  CLOSE WINDOW SCREEN 

  OPEN WINDOW vtn_trans WITH FORM "DISC39"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_nss FROM p_nss 
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma = f_ventana.getForm()
          CALL f_forma.setElementHidden("gr_nombre", 1) --Oculta el nombre
          CALL f_forma.setElementHidden("gr_transaccion", 1) --Oculta el detalle de transacciones 
          CALL f_forma.setElementHidden("gr_dispersion", 1) --Oculta el detalle de dispersion
          --CALL f_forma.setElementHidden("gr_totales",1) --Oculta los totales
          --CALL DIALOG.setActionHidden("reporte", 1) --Oculta el botón reporte 
               
        ON ACTION Aceptar
           IF v_nss IS NULL THEN
              CALL fn_mensaje("Atención",
                              "El NSS es obligatorio.",
                              "about") 
              EXIT PROGRAM
           END IF

           IF LENGTH(v_nss) <> 11 THEN
              CALL fn_mensaje("Atención",
                              "El NSS debe tener un longitud de 11 caractéres.",
                              "about") 
              EXIT PROGRAM
           END IF

          { FOR i = 1 TO 11
              DISPLAY "v_nss[i]: ",v_nss[i]
              IF v_nss[i] <> 1 AND v_nss[i] <> 2 AND v_nss[i] <> 3 AND v_nss[i] <> 4 AND v_nss[i] <> 5 AND
                 v_nss[i] <> 6 AND v_nss[i] <> 7 AND v_nss[i] <> 8 AND v_nss[i] <> 9 AND v_nss[i] <> 0 THEN 
                 CALL fn_mensaje("Atención",
                                 "El NSS no es numérico.",
                                 "about")
                 EXIT PROGRAM
              END IF
           END FOR}
           
           CONNECT TO "safre_viv"
           CALL fn_existe_nss(v_nss) RETURNING r_existe_registro
           DISCONNECT CURRENT

           IF r_existe_registro > 0 THEN
              CALL f_forma.setElementHidden("gr_nombre", 0) --Muestra el nombre              
              --CALL DIALOG.setActionHidden("accept", 1) --Muestra el botón reporte 

              CONNECT TO "safre_viv"
              --Obtiene informacion de transacciones               
              CALL fn_obt_inf_tra(v_nss) RETURNING v_ind_tra
              DISCONNECT CURRENT
              
              IF v_ind_tra > 0 THEN                 
                 CALL f_forma.setElementHidden("gr_transaccion", 0) --Muestra el detalle de transacciones                            
                 DISPLAY ARRAY v_arr_trans TO r_transaccion.* ATTRIBUTES (CANCEL = FALSE, ACCEPT = TRUE)                                  
                    ON ACTION ACCEPT  
                       LET v_row          = DIALOG.getCurrentRow("r_transaccion")
                       LET v_fol_liq      = v_arr_trans[v_row].v_fol_dis 
                       LET v_periodo_pago = v_arr_trans[v_row].v_per_pag

                       --DISPLAY "v_row         : ",v_row
                       --DISPLAY "v_fol_liq     : ",v_fol_liq
                       --DISPLAY "v_periodo_pago: ",v_periodo_pago

                       CONNECT TO "safre_viv"
                       --Obtiene informacion de dispersión de pagos
                       CALL fn_obt_dis(v_fol_liq, v_periodo_pago) RETURNING v_ind_dis
                       DISCONNECT CURRENT

                       --DISPLAY "v_arr_dis.getLength(): ",v_arr_dis.getLength()

                       IF v_ind_dis >= 1 THEN
                          CALL f_forma.setElementHidden("gr_dispersion", 0) --Muestra el detalle de dispersión 
                          --CALL f_forma.setElementHidden("gr_totales",0) --Muestra los totales
                          DISPLAY ARRAY v_arr_dis TO r_dispersion.* ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                             BEFORE DISPLAY
                                {LET v_row_dis = DIALOG.getCurrentRow("r_dispersion") 
                                LET v_row_dis = v_row_dis - 1      
                                FOR v_row_dis_i = 1 TO v_row_dis
                                    LET v_tot_apo = v_arr_dis[v_row_dis_i].v_aportacion
                                    LET v_tot_amo = v_arr_dis[v_row_dis_i].v_amortizacion
                                END FOR
                                
                                DISPLAY v_tot_reg  TO tot_reg
                                DISPLAY v_tot_aivs TO tot_aivs
                                DISPLAY v_tot_apo  TO tot_apo
                                DISPLAY v_tot_amo  TO tot_amo}
                                EXIT DISPLAY
                          END DISPLAY
                       ELSE
                          IF v_ind_dis = 0 THEN 
                             CALL fn_mensaje("Atención",
                                             "La transacción no tiene dispersión",
                                             "about")
                          END IF
                       END IF

                       {DISPLAY ARRAY v_arr_dis TO r_dispersion.* ATTRIBUTES (CANCEL = FALSE )
                          AFTER DISPLAY 
                             EXIT DISPLAY                     
                       END DISPLAY}

                    ON ACTION CANCEL
                       EXIT DIALOG
                 END DISPLAY         
              END IF                                 
           END IF 
           
        ON ACTION cancelar
           EXIT DIALOG

      END INPUT 
    END DIALOG  
  CLOSE WINDOW vtn_trans 
END MAIN

#Objetivo: Consulta para validar que exista el NSS
FUNCTION fn_existe_nss(v_nss)
  DEFINE
    v_nss                    CHAR(11), --NSS   
    v_ext_der                INTEGER, -- Contador si existen registros
    v_existe_liquida_movto1  INTEGER, -- Contador si existen registros
    v_QryTxt                 STRING

  --Validar id_derechohabiente
  LET v_QryTxt = "\n SELECT id_derechohabiente",
                 "\n FROM   afi_derechohabiente",
                 "\n WHERE  nss = ?"
          
  PREPARE prp_cnt_der FROM v_QryTxt
  EXECUTE prp_cnt_der USING v_nss INTO v_id_derechohabiente

  IF v_id_derechohabiente IS NULL THEN 
     CALL fn_mensaje("Atención",
                     "Trabajador no está registrado como derechohabiente.",
                     "about") 
     EXIT PROGRAM
  ELSE 
     LET v_QryTxt = "\n SELECT COUNT (*)",
                    "\n FROM   dis_his_transaccion ",
                    "\n WHERE  id_derechohabiente = '" ,v_id_derechohabiente,"'"
          
     PREPARE prp_cnt_dht FROM v_QryTxt
     EXECUTE prp_cnt_dht INTO v_ext_der

     DISPLAY "v_ext_der: ",v_ext_der

     IF v_ext_der IS NULL OR v_ext_der = 0 THEN    
        CALL fn_mensaje("Atención",
                        "Trabajador no cuenta con información de transacciones de dispersión.",
                        "about")      
        EXIT PROGRAM
     END IF 
  END IF
   
  RETURN v_ext_der

END FUNCTION

#Objetivo: Función para obtener la información de la tabla dis_his_transaccion
FUNCTION fn_obt_inf_tra(v_nss)
  DEFINE 
    v_nss                    CHAR(11), --NSS
    v_sql_txt                STRING

  DEFINE
    v_indice                 INTEGER,  
    v_id_derechohabiente     DECIMAL(9,0),
    v_nombre_af              CHAR(50),
    v_ap_paterno_af          CHAR(50),
    v_ap_materno_af          CHAR(50)

  -- Busca el nombre del derechohabiente
  LET v_sql_txt= "\n SELECT id_derechohabiente, ",
                 "\n        nombre_af, ", 
                 "\n        ap_paterno_af, ",
                 "\n        ap_materno_af ",  
                 "\n FROM   afi_derechohabiente ",
                 "\n WHERE  nss = ?"

  PREPARE prp_nom_der FROM v_sql_txt 
  EXECUTE prp_nom_der USING v_nss
                       INTO v_id_derechohabiente,
                            v_nombre_af,
                            v_ap_paterno_af,
                            v_ap_materno_af
  
  LET v_nombre_com = v_nombre_af CLIPPED," ",v_ap_paterno_af CLIPPED," ",v_ap_materno_af CLIPPED

  DISPLAY v_nombre_com TO v_nom_com
  
  -- Busca la información de las transacciones
  LET v_sql_txt= "\n SELECT gf.f_actualiza, ",
                 "\n        dh.folio_liquida, ",
                 "\n        dh.folio_reg_pagos, ",
                 "\n        dh.periodo_pago, ",
                 "\n        dh.num_credito, ",
                 "\n        dh.origen_num_credito, ",
                 "\n        co.tpo_originacion||' - '||co.originacion_desc as tp_originacion, ",
                 "\n        dh.tpo_credito||' - '||ct.desc_credito as tp_credito, ",
                 "\n        dh.f_otorga, ",
                 "\n        dh.f_liquida_cred, ",
                 "\n        dh.edo_credito, ",
                 "\n        dh.destino_ap_viv",
                 "\n FROM   dis_his_transaccion dh, ",
                 "\n        glo_folio gf, ",
                 "\n OUTER  cat_cre_originacion co, ",
                 "\n OUTER  cat_tipo_credito ct ",
                 --"\n OUTER  dis_ctr_archivo dc ",
                 "\n WHERE  dh.id_derechohabiente = ?",                  
                 "\n AND    dh.folio_liquida      = gf.folio ",
                 --"\n AND    dh.folio_liquida      = dc.folio_liquida ",
                 "\n AND    dh.tpo_originacion    = co.tpo_originacion ",
                 "\n AND    dh.tpo_originacion    = ct.tpo_originacion ",
                 "\n AND    dh.tpo_credito        = ct.tpo_credito ",
                 "\n ORDER BY dh.folio_liquida DESC, ",
                 "\n          dh.periodo_pago DESC "

  PREPARE prp_sql_trans FROM v_sql_txt

  LET v_indice = 1  

  DECLARE cur_trans CURSOR FOR prp_sql_trans
  FOREACH cur_trans USING v_id_derechohabiente INTO v_arr_trans[v_indice].v_f_liq_dis, 
                                                    v_arr_trans[v_indice].v_fol_dis,
                                                    v_arr_trans[v_indice].v_fol_reg,
                                                    v_arr_trans[v_indice].v_per_pag,
                                                    v_arr_trans[v_indice].v_num_cre, 
                                                    v_arr_trans[v_indice].v_ori_num_cre, 
                                                    v_arr_trans[v_indice].v_tip_ori, 
                                                    v_arr_trans[v_indice].v_tip_cre, 
                                                    v_arr_trans[v_indice].v_f_oto,
                                                    v_arr_trans[v_indice].v_f_liq_cre, 
                                                    v_arr_trans[v_indice].v_est_cre,
                                                    v_arr_trans[v_indice].des_apo_viv

    IF v_arr_trans[v_indice].v_ori_num_cre CLIPPED = 0 THEN 
       LET v_arr_trans[v_indice].v_ori_num_cre = "0 - SIN ORIGEN"
    ELSE 
       IF v_arr_trans[v_indice].v_ori_num_cre CLIPPED = 1 THEN
          LET v_arr_trans[v_indice].v_ori_num_cre = "1 - SACI"
       ELSE 
          IF v_arr_trans[v_indice].v_ori_num_cre CLIPPED = 2 THEN
             LET v_arr_trans[v_indice].v_ori_num_cre = "2 - LQ"
          ELSE
             --IF v_arr_trans[v_indice].v_ori_num_cre IS NULL OR 
                --v_arr_trans[v_indice].v_ori_num_cre = ""    THEN
                LET v_arr_trans[v_indice].v_ori_num_cre = "3 - AVANCE"
             --END IF
          END IF 
       END IF 
    END IF

    LET v_destino_credito = 0

    CALL fn_obt_dis_dest(v_arr_trans[v_indice].v_fol_dis, v_arr_trans[v_indice].v_per_pag)
    RETURNING v_destino_credito

    {IF (v_destino_credito = 2   OR
        v_destino_credito = 9   OR
        v_destino_credito = 10  OR
        v_destino_credito = 11  OR
        v_destino_credito = 12) THEN
        LET v_arr_trans[v_indice].v_ori_num_cre = "3 - AVANCE"
    ELSE
        IF v_arr_trans[v_indice].v_ori_num_cre CLIPPED = 0 THEN 
           LET v_arr_trans[v_indice].v_ori_num_cre = "0 - SIN ORIGEN"
        ELSE 
           IF v_arr_trans[v_indice].v_ori_num_cre CLIPPED = 1 THEN
              LET v_arr_trans[v_indice].v_ori_num_cre = "1 - SACI"
           ELSE 
              IF v_arr_trans[v_indice].v_ori_num_cre CLIPPED = 2 THEN
                 LET v_arr_trans[v_indice].v_ori_num_cre = "2 - LQ"
              END IF 
           END IF 
        END IF
    END IF}

    IF (v_destino_credito = 8) THEN
        LET v_arr_trans[v_indice].v_tip_cre     = " "
        --LET v_arr_trans[v_indice].v_ori_num_cre = " "
        LET v_arr_trans[v_indice].v_ori_num_cre = "3 - AVANCE"
    END IF

    IF v_arr_trans[v_indice].v_est_cre CLIPPED = -2 THEN
       LET v_arr_trans[v_indice].v_est_cre = "-2 - NO EXISTE NSS"
    ELSE
        IF v_arr_trans[v_indice].v_est_cre CLIPPED = -1 THEN
          LET v_arr_trans[v_indice].v_est_cre = "-1 - NO EXISTE ID DERECHOHABIENTE"
       ELSE
           IF v_arr_trans[v_indice].v_est_cre CLIPPED = 0 THEN
              LET v_arr_trans[v_indice].v_est_cre = "0 - CRÉDITO VIGENTE"
           ELSE 
              IF v_arr_trans[v_indice].v_est_cre CLIPPED = 1 THEN
                 LET v_arr_trans[v_indice].v_est_cre = "1 - SIN CRÉDITO"
              ELSE 
                 IF v_arr_trans[v_indice].v_est_cre CLIPPED = 2 THEN
                    LET v_arr_trans[v_indice].v_est_cre = "2 - CRÉDITO LIQUIDADO"
                 ELSE
                    IF v_arr_trans[v_indice].v_est_cre CLIPPED = 3 THEN
                       LET v_arr_trans[v_indice].v_est_cre = "3 - CRÉDITO EN TRÁMITE"
                    END IF                     
                 END IF 
              END IF
           END IF    
       END IF
    END IF   

    IF v_arr_trans[v_indice].des_apo_viv CLIPPED = 1 THEN 
       LET v_arr_trans[v_indice].des_apo_viv = "1 - INFONAVIT"
    ELSE 
       IF v_arr_trans[v_indice].des_apo_viv CLIPPED = 2 THEN 
          LET v_arr_trans[v_indice].des_apo_viv = "2 - AFORE"
       END IF
    END IF 

    LET v_indice = v_indice + 1
  END FOREACH

  CALL v_arr_trans.deleteElement(v_indice)
  LET v_indice = v_indice - 1
  --DISPLAY "v_ind_tra: ",v_indice
  RETURN v_indice
END FUNCTION

#OBJETIVO: Función para recuperar la información de Dispersión de Pagos
FUNCTION fn_obt_dis(p_folio, p_periodo_pago)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    QryTxt                   STRING,
    v_indice                 INTEGER, 
    v_cve_destino            SMALLINT,
    p_periodo_pago           CHAR(06)

  LET v_indice = 1

  CALL v_arr_dis.CLEAR()

  LET v_tot_aivs = 0
  LET v_tot_apo  = 0
  LET v_tot_amo  = 0

  LET QryTxt = "\n SELECT UNIQUE dc.nombre_archivo, ",
               "\n        dc.cve_destino, ",
               "\n        cd.desc_destino ",
               "\n FROM   dis_his_transaccion dh, ",
               "\n        dis_ctr_archivo dc, ", 
               "\n        cat_dis_destino cd ",
               "\n WHERE  dc.folio_liquida      = ? ",
               "\n AND    dh.id_derechohabiente = ? ",
               "\n AND    dh.periodo_pago       = ? ",
               "\n AND    dc.cve_destino        = cd.cve_destino "

  --DISPLAY "\n", QryTxt  -- JANN 
  PREPARE prp_disp FROM QryTxt  
  DECLARE cur_disp CURSOR FOR prp_disp
  FOREACH cur_disp USING p_folio, v_id_derechohabiente, p_periodo_pago
                    INTO v_arr_dis[v_indice].v_interface, 
                         v_cve_destino,
                         v_arr_dis[v_indice].v_destino

    IF (v_cve_destino = 7   OR
        --v_cve_destino = 8   OR
        v_cve_destino = 13  OR
        v_cve_destino = 14) THEN
        CONTINUE FOREACH
    END IF

    LET v_arr_dis[v_indice].v_destino = v_cve_destino," - ",v_arr_dis[v_indice].v_destino 

    --CARTERA
    IF v_cve_destino = 1 THEN 
       LET QryTxt = "\n SELECT nrp, ",
                    "\n        periodo_pago, ",
                    "\n        folio_sua, ", 
                    "\n        f_pago, ", 
                    "\n        aiv_ap_pat, ", 
                    "\n        imp_ap_pat, ", 
                    "\n        imp_am_cre ",
                    "\n FROM   dis_interface_hs ",
                    "\n WHERE  folio_liquida      = ? ",
                    "\n AND    id_derechohabiente = ? ", 
                    "\n AND    periodo_pago       = ? "

       --DISPLAY "\n", QryTxt
       PREPARE prp_dis_1 FROM QryTxt
       EXECUTE prp_dis_1 USING p_folio, v_id_derechohabiente, p_periodo_pago 
                          INTO v_arr_dis[v_indice].v_nrp, 
                               v_arr_dis[v_indice].v_periodo_pago,
                               v_arr_dis[v_indice].v_folio_sua, 
                               v_arr_dis[v_indice].v_f_pago, 
                               v_arr_dis[v_indice].v_aivs, 
                               v_arr_dis[v_indice].v_aportacion,
                               v_arr_dis[v_indice].v_amortizacion
    END IF
    --CARTERA

    --RECUPERACIÓN AVANCE DE PAGOS
    IF (v_cve_destino = 2   OR 
        v_cve_destino = 9   OR
        v_cve_destino = 10  OR
        v_cve_destino = 11  OR 
        v_cve_destino = 12) THEN
       LET v_arr_dis[v_indice].v_aivs         = 0;
       LET v_arr_dis[v_indice].v_aportacion   = 0; 
       LET v_arr_dis[v_indice].v_amortizacion = 0;
       LET v_edo_compensa_apo                 = 0;
       LET v_edo_compensa_amo                 = 0;

       LET QryTxt = "\n SELECT dc.nrp,", 
                    "\n        dc.periodo_pago, ",  
                    "\n	       ch.folio_sua, ", 
                    "\n	       dc.f_pago, ",  
                    "\n	       dd.monto_dif_apo, ",
                    "\n	       dd.monto_dif_amo, ",
                    "\n	       dc.edo_compensa_apo, ",
                    "\n	       dc.edo_compensa_amo ",
                    "\n FROM   dis_compensa_avance dc, ", 
                    "\n        cta_his_pagos ch, ", 
                    "\n        dis_det_avance_pago dd ",	     
                    "\n WHERE  dc.folio_dis              = ? ",
                    "\n AND    dc.id_derechohabiente     = ? ",                   
                    "\n AND    dc.folio_pago             = ch.folio ",
                    "\n AND    dc.id_referencia          = ch.id_referencia ",
                    "\n AND    dc.id_dis_det_avance_pago = dd.id_dis_det_avance_pago ",
                    "\n AND    dc.periodo_pago           = ? " 

       PREPARE prp_dis_2 FROM QryTxt
       EXECUTE prp_dis_2 USING p_folio, v_id_derechohabiente, p_periodo_pago
                          INTO v_arr_dis[v_indice].v_nrp, 
                               v_arr_dis[v_indice].v_periodo_pago, 
                               v_arr_dis[v_indice].v_folio_sua, 
                               v_arr_dis[v_indice].v_f_pago,
                               v_arr_dis[v_indice].v_aportacion, 
                               v_arr_dis[v_indice].v_amortizacion,
                               v_edo_compensa_apo,
                               v_edo_compensa_amo

       IF v_edo_compensa_amo == 3  OR
          v_edo_compensa_amo == 4  OR
          v_edo_compensa_amo == 5  OR
         (v_edo_compensa_apo == 0  AND
          v_edo_compensa_amo == 0) THEN
          --LET v_arr_dis[v_indice].v_interface    = NULL
          --LET v_arr_dis[v_indice].v_destino      = NULL
          LET v_arr_dis[v_indice].v_nrp          = NULL
          LET v_arr_dis[v_indice].v_periodo_pago = NULL 
          LET v_arr_dis[v_indice].v_folio_sua    = NULL
          LET v_arr_dis[v_indice].v_f_pago       = NULL
          LET v_arr_dis[v_indice].v_aivs         = NULL
          LET v_arr_dis[v_indice].v_aportacion   = NULL
          LET v_arr_dis[v_indice].v_amortizacion = NULL
          LET v_tot_apo                          = 0
          LET v_tot_amo                          = 0
       END IF

       IF v_edo_compensa_apo == 0 AND
          v_edo_compensa_amo == 1 THEN
          LET v_arr_dis[v_indice].v_aportacion = 0
          LET v_tot_apo                        = 0

          IF v_cve_destino == 11 OR 
             v_cve_destino == 12 THEN
             LET v_arr_dis[v_indice].v_nrp          = NULL
             LET v_arr_dis[v_indice].v_periodo_pago = NULL 
             LET v_arr_dis[v_indice].v_folio_sua    = NULL
             LET v_arr_dis[v_indice].v_f_pago       = NULL
             LET v_arr_dis[v_indice].v_aivs         = NULL
             LET v_arr_dis[v_indice].v_aportacion   = NULL
             LET v_arr_dis[v_indice].v_amortizacion = NULL
             LET v_tot_apo                          = 0
             LET v_tot_amo                          = 0
          END IF
          
          IF v_cve_destino == 10 THEN
             IF (v_arr_dis[v_indice].v_amortizacion <= 2)  AND
                (v_arr_dis[v_indice].v_amortizacion >= -2) THEN
                LET v_arr_dis[v_indice].v_nrp          = NULL
                LET v_arr_dis[v_indice].v_periodo_pago = NULL 
                LET v_arr_dis[v_indice].v_folio_sua    = NULL
                LET v_arr_dis[v_indice].v_f_pago       = NULL
                LET v_arr_dis[v_indice].v_aivs         = NULL
                LET v_arr_dis[v_indice].v_amortizacion = NULL
                LET v_tot_amo                          = 0
             END IF
          END IF
       END IF

       IF v_edo_compensa_apo == 0 AND
          v_edo_compensa_amo == 2 THEN
          LET v_arr_dis[v_indice].v_aportacion = 0
          LET v_tot_apo                        = 0

          IF v_cve_destino == 9  OR 
             v_cve_destino == 10 THEN
             LET v_arr_dis[v_indice].v_nrp          = NULL
             LET v_arr_dis[v_indice].v_periodo_pago = NULL 
             LET v_arr_dis[v_indice].v_folio_sua    = NULL
             LET v_arr_dis[v_indice].v_f_pago       = NULL
             LET v_arr_dis[v_indice].v_aivs         = NULL
             LET v_arr_dis[v_indice].v_aportacion   = NULL
             LET v_arr_dis[v_indice].v_amortizacion = NULL
             LET v_tot_apo                          = 0
             LET v_tot_amo                          = 0
          END IF
          
          IF v_cve_destino == 12 THEN
             IF (v_arr_dis[v_indice].v_amortizacion <= 2)  AND
                (v_arr_dis[v_indice].v_amortizacion >= -2) THEN
                LET v_arr_dis[v_indice].v_nrp          = NULL
                LET v_arr_dis[v_indice].v_periodo_pago = NULL 
                LET v_arr_dis[v_indice].v_folio_sua    = NULL
                LET v_arr_dis[v_indice].v_f_pago       = NULL
                LET v_arr_dis[v_indice].v_aivs         = NULL
                LET v_arr_dis[v_indice].v_amortizacion = NULL
                LET v_tot_amo                          = 0
             END IF
          END IF
       END IF

       IF v_edo_compensa_apo == 1 AND
          v_edo_compensa_amo == 0 THEN
          LET v_arr_dis[v_indice].v_amortizacion = 0
          LET v_tot_amo                          = 0

          IF v_cve_destino == 11 OR 
             v_cve_destino == 12 THEN
             LET v_arr_dis[v_indice].v_nrp          = NULL
             LET v_arr_dis[v_indice].v_periodo_pago = NULL 
             LET v_arr_dis[v_indice].v_folio_sua    = NULL
             LET v_arr_dis[v_indice].v_f_pago       = NULL
             LET v_arr_dis[v_indice].v_aivs         = NULL
             LET v_arr_dis[v_indice].v_aportacion   = NULL
             LET v_arr_dis[v_indice].v_amortizacion = NULL
             LET v_tot_apo                          = 0
             LET v_tot_amo                          = 0
          END IF

          IF v_cve_destino == 10 THEN
             IF (v_arr_dis[v_indice].v_aportacion <= 2)  AND
                (v_arr_dis[v_indice].v_aportacion >= -2) THEN
                LET v_arr_dis[v_indice].v_nrp          = NULL
                LET v_arr_dis[v_indice].v_periodo_pago = NULL 
                LET v_arr_dis[v_indice].v_folio_sua    = NULL
                LET v_arr_dis[v_indice].v_f_pago       = NULL
                LET v_arr_dis[v_indice].v_aivs         = NULL
                LET v_arr_dis[v_indice].v_amortizacion = NULL
                LET v_tot_amo                          = 0
             END IF
          END IF
       END IF
       
       IF v_edo_compensa_apo == 1 AND
          v_edo_compensa_amo == 1 THEN
          IF v_cve_destino == 11 OR 
             v_cve_destino == 12 THEN
             LET v_arr_dis[v_indice].v_nrp          = NULL
             LET v_arr_dis[v_indice].v_periodo_pago = NULL 
             LET v_arr_dis[v_indice].v_folio_sua    = NULL
             LET v_arr_dis[v_indice].v_f_pago       = NULL
             LET v_arr_dis[v_indice].v_aivs         = NULL
             LET v_arr_dis[v_indice].v_aportacion   = NULL
             LET v_arr_dis[v_indice].v_amortizacion = NULL
             LET v_tot_apo                          = 0
             LET v_tot_amo                          = 0
          END IF
          IF v_cve_destino == 10 THEN
             IF (v_arr_dis[v_indice].v_aportacion <= 2)  AND
                (v_arr_dis[v_indice].v_aportacion >= -2) THEN
                LET v_arr_dis[v_indice].v_aportacion = 0
                LET v_tot_apo                        = 0
             END IF

             IF (v_arr_dis[v_indice].v_amortizacion <= 2)  AND
                (v_arr_dis[v_indice].v_amortizacion >= -2) THEN
                LET v_arr_dis[v_indice].v_amortizacion = 0
                LET v_tot_amo                          = 0
             END IF
          END IF
       END IF

       IF v_edo_compensa_apo == 1 AND
          v_edo_compensa_amo == 2 THEN

          IF v_edo_compensa_apo == 1 THEN
             IF v_cve_destino == 9 OR
                v_cve_destino == 10 THEN
                LET v_arr_dis[v_indice].v_amortizacion = 0
                LET v_tot_amo                          = 0
             END IF
             IF v_cve_destino == 10 THEN
                IF (v_arr_dis[v_indice].v_aportacion <= 2)  AND
                   (v_arr_dis[v_indice].v_aportacion >= -2) THEN
                   LET v_arr_dis[v_indice].v_aportacion = 0
                   LET v_tot_apo                        = 0
                END IF
             END IF
          END IF
          
          IF v_edo_compensa_amo == 2 THEN
             IF v_cve_destino == 11 OR
                v_cve_destino == 12 THEN
                LET v_arr_dis[v_indice].v_aportacion = 0
                LET v_tot_apo                        = 0
             END IF
             IF v_cve_destino == 12 THEN
                IF (v_arr_dis[v_indice].v_amortizacion <= 2)  AND
                   (v_arr_dis[v_indice].v_amortizacion >= -2) THEN
                   LET v_arr_dis[v_indice].v_amortizacion = 0
                   LET v_tot_amo                          = 0
                END IF
             END IF
          END IF
       END IF

       IF v_edo_compensa_apo == 2 AND
          v_edo_compensa_amo == 0 THEN
          LET v_arr_dis[v_indice].v_amortizacion = 0
          LET v_tot_amo                          = 0

          IF v_cve_destino == 9  OR 
             v_cve_destino == 10 THEN
             LET v_arr_dis[v_indice].v_nrp          = NULL
             LET v_arr_dis[v_indice].v_periodo_pago = NULL 
             LET v_arr_dis[v_indice].v_folio_sua    = NULL
             LET v_arr_dis[v_indice].v_f_pago       = NULL
             LET v_arr_dis[v_indice].v_aivs         = NULL
             LET v_arr_dis[v_indice].v_aportacion   = NULL
             LET v_arr_dis[v_indice].v_amortizacion = NULL
             LET v_tot_apo                          = 0
             LET v_tot_amo                          = 0
          END IF
          
          IF v_cve_destino == 12 THEN
             IF (v_arr_dis[v_indice].v_aportacion <= 2)  AND
                (v_arr_dis[v_indice].v_aportacion >= -2) THEN
                LET v_arr_dis[v_indice].v_nrp          = NULL
                LET v_arr_dis[v_indice].v_periodo_pago = NULL 
                LET v_arr_dis[v_indice].v_folio_sua    = NULL
                LET v_arr_dis[v_indice].v_f_pago       = NULL
                LET v_arr_dis[v_indice].v_aivs         = NULL
                LET v_arr_dis[v_indice].v_aportacion   = NULL
                LET v_arr_dis[v_indice].v_amortizacion = NULL
                LET v_tot_apo                          = 0
                LET v_tot_amo                          = 0
             END IF
          END IF
       END IF

       IF v_edo_compensa_apo == 2 AND
          v_edo_compensa_amo == 1 THEN

          IF v_edo_compensa_apo == 2 THEN
             IF v_cve_destino == 11 OR
                v_cve_destino == 12 THEN
                LET v_arr_dis[v_indice].v_amortizacion = 0
                LET v_tot_amo                          = 0
             END IF
             IF v_cve_destino == 12 THEN
                IF (v_arr_dis[v_indice].v_aportacion <= 2)  AND
                   (v_arr_dis[v_indice].v_aportacion >= -2) THEN
                   LET v_arr_dis[v_indice].v_aportacion = 0
                   LET v_tot_apo                        = 0
                END IF
             END IF
          END IF

          IF v_edo_compensa_amo == 1 THEN
             IF v_cve_destino == 9 OR
                v_cve_destino == 10 THEN
                LET v_arr_dis[v_indice].v_aportacion = 0
                LET v_tot_apo                        = 0
             END IF
             IF v_cve_destino == 10 THEN
                IF (v_arr_dis[v_indice].v_amortizacion <= 2)  AND
                   (v_arr_dis[v_indice].v_amortizacion >= -2) THEN
                   LET v_arr_dis[v_indice].v_amortizacion = 0
                   LET v_tot_amo                          = 0
                END IF
             END IF
          END IF
       END IF
       
       IF v_edo_compensa_apo == 2 AND
          v_edo_compensa_amo == 2 THEN
          IF v_cve_destino == 9  OR 
             v_cve_destino == 10 THEN
             LET v_arr_dis[v_indice].v_nrp          = NULL
             LET v_arr_dis[v_indice].v_periodo_pago = NULL 
             LET v_arr_dis[v_indice].v_folio_sua    = NULL
             LET v_arr_dis[v_indice].v_f_pago       = NULL
             LET v_arr_dis[v_indice].v_aivs         = NULL
             LET v_arr_dis[v_indice].v_aportacion   = NULL
             LET v_arr_dis[v_indice].v_amortizacion = NULL
             LET v_tot_apo                          = 0
             LET v_tot_amo                          = 0
          END IF
          IF v_cve_destino == 12 THEN
             IF (v_arr_dis[v_indice].v_aportacion <= 2)  AND
                (v_arr_dis[v_indice].v_aportacion >= -2) THEN
                LET v_arr_dis[v_indice].v_aportacion = 0
                LET v_tot_apo                        = 0
             END IF

             IF (v_arr_dis[v_indice].v_amortizacion <= 2)  AND
                (v_arr_dis[v_indice].v_amortizacion >= -2) THEN
                LET v_arr_dis[v_indice].v_amortizacion = 0
                LET v_tot_amo                          = 0
             END IF
          END IF
       END IF

    END IF
    --RECUPERACIÓN AVANCE DE PAGOS

    --ENTIDAD FINANCIERA
    IF v_cve_destino = 3 THEN
       LET v_arr_dis[v_indice].v_amortizacion = 0;
	   	  
       LET QryTxt = "\n SELECT nrp, ", 
                    "\n        periodo_pago, ", 
                    "\n        folio_sua, ", 
                    "\n        f_pago, ", 
                    "\n        aiv_ap_pat, ", 
                    "\n        imp_ap_pat ", 		      
                    "\n FROM   dis_interface_ef ", 
                    "\n WHERE  folio_liquida      = ? ", 
                    "\n AND    id_derechohabiente = ? ", 
                    "\n AND    periodo_pago       = ? "

       PREPARE prp_dis_3 FROM QryTxt
       EXECUTE prp_dis_3 USING p_folio, v_id_derechohabiente, p_periodo_pago
                          INTO v_arr_dis[v_indice].v_nrp,
                               v_arr_dis[v_indice].v_periodo_pago, 
                               v_arr_dis[v_indice].v_folio_sua, 
                               v_arr_dis[v_indice].v_f_pago, 
                               v_arr_dis[v_indice].v_aivs, 
                               v_arr_dis[v_indice].v_aportacion
    END IF
    --ENTIDAD FINANCIERA

    --DEVOLUCIÓN DE PAGOS EN EXCESO
    IF (v_cve_destino = 4  OR 
        v_cve_destino = 6 )THEN
       LET v_arr_dis[v_indice].v_nrp          = "";	
       LET v_arr_dis[v_indice].v_folio_sua    = "";
       LET v_arr_dis[v_indice].v_amortizacion = 0;
		       
       LET QryTxt = "\n SELECT periodo_pago, ",
                    "\n        f_pago, ", 
                    "\n        aivs_aportacion, ", 
                    "\n        monto_aportacion ",		         
                    "\n FROM   dse_devolucion ",
                    "\n WHERE  id_derechohabiente = ? ", 
                    "\n AND    folio_referencia   = ? ", 
                    "\n AND    periodo_pago       = ? "

        PREPARE prp_dis_4 FROM QryTxt
        EXECUTE prp_dis_4 USING p_folio, v_id_derechohabiente, p_periodo_pago
                           INTO v_arr_dis[v_indice].v_periodo_pago, 
                                v_arr_dis[v_indice].v_f_pago, 
                                v_arr_dis[v_indice].v_aivs,
                                v_arr_dis[v_indice].v_aportacion
    END IF
    --DEVOLUCIÓN DE PAGOS EN EXCESO

    --AVANCE DE PAGOS
    IF v_cve_destino = 8 THEN
       LET v_arr_dis[v_indice].v_aivs         = 0;
       LET v_arr_dis[v_indice].v_folio_sua    = "";
 
       LET QryTxt = "\n SELECT nrp, ",
                    "\n        periodo_pago, ",
                    "\n        f_pago, ", 
                    "\n        monto_aportacion, ", 
                    "\n        monto_amortizacion ",
                    "\n FROM   dis_det_avance_pago ",
                    "\n WHERE  folio              = ? ",
                    "\n AND    id_derechohabiente = ? ", 
                    "\n AND    periodo_pago       = ? "

       --DISPLAY "\n", QryTxt
       PREPARE prp_dis_5 FROM QryTxt
       EXECUTE prp_dis_5 USING p_folio, v_id_derechohabiente, p_periodo_pago 
                          INTO v_arr_dis[v_indice].v_nrp, 
                               v_arr_dis[v_indice].v_periodo_pago,
                               v_arr_dis[v_indice].v_f_pago, 
                               v_arr_dis[v_indice].v_aportacion,
                               v_arr_dis[v_indice].v_amortizacion
    END IF
    --AVANCE DE PAGOS

    LET v_indice = v_indice + 1
  END FOREACH

  --DEVOLUCIÓN DE PAGOS EN EXCESO
  LET d_periodo_pago     = ""
  LET d_f_pago           = ""
  LET d_aivs_aportacion  = 0
  LET d_monto_aportacion = 0
  
  SELECT a.periodo_pago,
         a.f_pago,
         a.aivs_aportacion,
         a.monto_aportacion
  INTO   d_periodo_pago,
         d_f_pago,
         d_aivs_aportacion,
         d_monto_aportacion
  FROM   dse_devolucion a
  WHERE  a.id_derechohabiente = v_id_derechohabiente
  AND    a.folio_referencia   = p_folio
  AND    a.periodo_pago       = p_periodo_pago
  IF d_periodo_pago IS NOT NULL THEN
     LET v_arr_dis[v_indice].v_interface    = ""
     LET v_arr_dis[v_indice].v_nrp          = ""	
     LET v_arr_dis[v_indice].v_folio_sua    = ""
     LET v_arr_dis[v_indice].v_amortizacion = 0
     LET v_arr_dis[v_indice].v_periodo_pago = d_periodo_pago
     LET v_arr_dis[v_indice].v_f_pago       = d_f_pago
     LET v_arr_dis[v_indice].v_aivs         = d_aivs_aportacion
     LET v_arr_dis[v_indice].v_aportacion   = d_monto_aportacion

     LET v_cve_destino = 6

     SELECT dest.desc_destino
     INTO   v_arr_dis[v_indice].v_destino 
     FROM   cat_dis_destino dest
     WHERE  dest.cve_destino = v_cve_destino
 
     LET v_arr_dis[v_indice].v_destino = v_cve_destino," - ",v_arr_dis[v_indice].v_destino 

     LET v_indice = v_indice + 1
  ELSE
     CALL v_arr_dis.deleteElement(v_indice)
     LET v_indice = v_indice - 1      
  END IF

  LET v_tot_reg  = v_indice
  IF v_arr_dis[v_indice].v_aivs IS NOT NULL THEN
     LET v_tot_aivs = v_arr_dis[v_indice].v_aivs
  END IF
  IF v_arr_dis[v_indice].v_aportacion IS NOT NULL THEN
     LET v_tot_apo  = v_arr_dis[v_indice].v_aportacion
  END IF
  IF v_arr_dis[v_indice].v_amortizacion IS NOT NULL THEN
     LET v_tot_amo  = v_arr_dis[v_indice].v_amortizacion
  END IF
   
  RETURN v_indice
END FUNCTION

FUNCTION fn_obt_dis_dest(p_folio, p_periodo_pago)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    QryTxt                   STRING,
    p_periodo_pago           CHAR(06)

  LET QryTxt = "\n SELECT UNIQUE dc.cve_destino ",
               "\n FROM   dis_his_transaccion dh, ",
               "\n        dis_ctr_archivo dc ", 
               "\n WHERE  dc.folio_liquida      = ? ",
               "\n AND    dh.id_derechohabiente = ? ",
               "\n AND    dh.periodo_pago       = ? "
               
  PREPARE prp_dest_cred FROM QryTxt  
  DECLARE cur_dest_cred CURSOR FOR prp_dest_cred
  FOREACH cur_dest_cred USING p_folio, v_id_derechohabiente, p_periodo_pago
                         INTO v_destino_credito

    IF (v_destino_credito = 7   OR
        v_destino_credito = 13  OR
        v_destino_credito = 14) THEN
        CONTINUE FOREACH
    END IF

    IF (v_destino_credito = 8) THEN
        EXIT FOREACH
    END IF

    {IF (v_destino_credito = 2   OR
        v_destino_credito = 9   OR
        v_destino_credito = 10  OR
        v_destino_credito = 11  OR
        v_destino_credito = 12) THEN
        EXIT FOREACH
    END IF}
  END FOREACH

  RETURN v_destino_credito
END FUNCTION