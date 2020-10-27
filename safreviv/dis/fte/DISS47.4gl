################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 04/05/2018                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS47                                                    #
#Objetivo         => Generar las archivos de control de pagos (AS) sin         #
#                    dispersión de pagos.
#Fecha de Inicio  => 04/05/2018                                                #
################################################################################
DATABASE safre_viv
GLOBALS
  --Sección de variables del programa
  DEFINE 
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    p_proceso_cod            LIKE cat_proceso.proceso_cod,
    p_opera_cod              LIKE cat_operacion.opera_cod,
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tot_registros          DECIMAL(9,0) --Total de registros

  --Arreglo para información de la interface de pp ant 201205 sin mov de abono
  --por registro de pagos
  DEFINE v_arr_pp_ant_sin_mov_ab DYNAMIC ARRAY OF RECORD
    v_nrp                    CHAR(11),  
    v_nss                    CHAR(11),
    v_rfc                    CHAR(13),
    v_periodo_pago           CHAR(6), 
    v_folio_sua              DECIMAL(6,0),
    v_f_pago                 DATE,
    v_curp                   CHAR(18), 
    v_nombre                 CHAR(50),
    v_num_credito            DECIMAL(10,0),
    v_imp_apo_aivs           DECIMAL(21,6),
    v_imp_apo_pat            DECIMAL(9,2),
    v_valor_apl_apo          DECIMAL(17,6),
    v_afore                  CHAR(03), 
    v_f_liquida              DATE
  END RECORD

  --Arreglo para información de la interface de pp ant 201205 con mov de abono
  --por registro de pagos
  DEFINE v_arr_pp_ant_con_mov_ab DYNAMIC ARRAY OF RECORD
    v_nrp                    CHAR(11),  
    v_nss                    CHAR(11),
    v_rfc                    CHAR(13),
    v_periodo_pago           CHAR(6), 
    v_folio_sua              DECIMAL(6,0),
    v_f_pago                 DATE,
    v_curp                   CHAR(18), 
    v_nombre                 CHAR(50),
    v_num_credito            DECIMAL(10,0),
    v_imp_apo_aivs           DECIMAL(21,6),
    v_imp_apo_pat            DECIMAL(9,2),
    v_valor_apl_apo          DECIMAL(17,6),
    v_afore                  CHAR(03), 
    v_f_liquida              DATE
  END RECORD

  --Arreglo para información de la interface de pp post 201205 sin mov de abono
  --por registro de pagos
  DEFINE v_arr_pp_pos_sin_mov_ab DYNAMIC ARRAY OF RECORD
    v_nrp                    CHAR(11),  
    v_nss                    CHAR(11),
    v_rfc                    CHAR(13),
    v_periodo_pago           CHAR(6), 
    v_folio_sua              DECIMAL(6,0),
    v_f_pago                 DATE,
    v_curp                   CHAR(18), 
    v_nombre                 CHAR(50),
    v_num_credito            DECIMAL(10,0),
    v_imp_apo_aivs           DECIMAL(21,6),
    v_imp_apo_pat            DECIMAL(9,2),
    v_valor_apl_apo          DECIMAL(17,6),
    v_afore                  CHAR(03), 
    v_f_liquida              DATE
  END RECORD

  --Arreglo para información de la interface de pp post 201205 con mov de abono
  --por registro de pagos
  DEFINE v_arr_pp_pos_con_mov_ab DYNAMIC ARRAY OF RECORD
    v_nrp                    CHAR(11),  
    v_nss                    CHAR(11),
    v_rfc                    CHAR(13),
    v_periodo_pago           CHAR(6), 
    v_folio_sua              DECIMAL(6,0),
    v_f_pago                 DATE,
    v_curp                   CHAR(18), 
    v_nombre                 CHAR(50),
    v_num_credito            DECIMAL(10,0),
    v_imp_apo_aivs           DECIMAL(21,6),
    v_imp_apo_pat            DECIMAL(9,2),
    v_valor_apl_apo          DECIMAL(17,6),
    v_afore                  CHAR(03), 
    v_f_liquida              DATE
  END RECORD

  --Variables auxiliares
  DEFINE 
    g_sql_txt                STRING,
    g_sql_txt_m              STRING,
    v_qwery_ibx              STRING, 
    i_folio_dis              VARCHAR(9),
    i_nss                    CHAR(11),
    i_nrp                    CHAR(11),
    i_periodo_pago           CHAR(06),
    i_monto_apo_avance       VARCHAR(14),
    i_monto_amo_avance       VARCHAR(14),
    i_folio_pago             VARCHAR(9),
    i_monto_apo_pag          VARCHAR(14),
    i_monto_amo_pag          VARCHAR(14)
      
  DEFINE 
    f_folio                  DECIMAL(9,0),
    v_total_registros        BIGINT

  DEFINE 
    v_tot_ava_apo            VARCHAR(22),
    v_tot_ava_amo            VARCHAR(22),
    v_tot_pag_apo            VARCHAR(22),
    v_tot_pag_amo            VARCHAR(22)

  DEFINE 
    v_folio_reg_pag          DECIMAL(9,0)

  DEFINE
    v_archivo_copia          VARCHAR(30),
    v_comando_dos            STRING

  DEFINE
    v_indice_asrp            INTEGER,
    v_indice_acrp            INTEGER,
    v_indice_psrp            INTEGER,
    v_indice_pcrp            INTEGER

  DEFINE v_indice_cta        INT
  DEFINE arr_tbl_mov         DYNAMIC ARRAY OF VARCHAR(50)
  DEFINE v_indice_sc         INTEGER
  DEFINE v_QryTxt            STRING

  DEFINE g_arr_mov_cta       DYNAMIC ARRAY OF RECORD
    id_derechohabiente       DECIMAL(9,0),
    folio_liquida            DECIMAL(9,0),
    f_liquida                DATE,
    movimiento               SMALLINT
  END RECORD

  DEFINE v_bnd_mov_div_disp  SMALLINT

  DEFINE
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_tip_reg_enc            CHAR(02),
    v_id_ser                 CHAR(02),
    v_id_ope                 CHAR(02),
    v_tip_ent_ori            CHAR(02),
    v_cla_ent_ori            CHAR(03),
    v_tip_ent_des            CHAR(02),
    v_cla_ent_des            CHAR(03),
    v_mod_rec_env_arc        CHAR(02), 
    v_fec_hoy                VARCHAR(08),
    v_tip_reg                CHAR(02),
    v_tip_reg_sum            CHAR(02)

  DEFINE
    v_espacios_enc_c11       STRING,
    v_espacios_det_c29       STRING,
    v_espacios_sum_c18       STRING,
    v_extension              CHAR(10)

END GLOBALS 

MAIN
  DEFINE r_bandera           SMALLINT
    
  --Asignación de parametros generales
  LET p_usuario         = ARG_VAL(1)
  LET p_pid             = ARG_VAL(2)
  LET p_proceso_cod     = ARG_VAL(3)
  LET p_opera_cod       = ARG_VAL(4)
  LET f_folio           = ARG_VAL(5) --Valor de argumento uno de DISL63

  LET v_modulo_cod      = "dis"    
  LET v_total_registros = 0

  LET v_tip_reg_enc      = '01'
  LET v_id_ser           = '03'
  LET v_id_ope           = '59'
  LET v_tip_ent_ori      = '03' 
  LET v_cla_ent_ori      = '001'
  LET v_tip_ent_des      = '04'
  LET v_cla_ent_des      = '002'
  LET v_fec_hoy          = TODAY USING "yyyymmdd"  --Fecha del dia
  LET v_mod_rec_env_arc  = '02'
  LET v_espacios_enc_c11 = 266 SPACES

  LET v_tip_reg          = '02'
  LET v_espacios_det_c29 = 39 SPACES
  
  LET v_tip_reg_sum      = '09'
  LET v_espacios_sum_c18 = 127 SPACES

  LET v_extension        = 'APOSUBS'

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  DISPLAY "Inicia la generación de las interfaces de Control de Pagos AS. ", TIME
  DISPLAY ""
  --Llama a la función que identifica los registros para el control de pagos
  --(AS) sin dispersión de pago o restitución.
  CALL fn_identifica_ctr_pagos()

  --Identifica los tipos de control de pagos
  CALL fn_info_as_sc()
  
  --Pagos anteriores al periodo de pago 201205 sin abono 
  IF v_indice_asrp > 0 THEN
     CALL fn_genera_interfase1()
     DISPLAY ""
  ELSE
     DISPLAY "Interface Pagos Anteriores al pp 201205 sin abono por registro de pagos, no genero datos."
     DISPLAY ""
  END IF
  
  --Pagos anteriores al periodo de pago 201205 con abono
  IF v_indice_acrp > 0 THEN
     CALL fn_genera_interfase2()
     DISPLAY ""
  ELSE
     DISPLAY "Interface Pagos Anteriores al pp 201205 con abono por registro de pagos, no genero datos."
     DISPLAY ""
  END IF
  
  --Pagos posteriores al periodo de pago 201205 sin abono
  IF v_indice_psrp > 0 THEN
     CALL fn_genera_interfase3()
     DISPLAY ""
  ELSE
     DISPLAY "Interface Pagos Posteriores al pp 201205 sin abono por registro de pagos, no genero datos."
     DISPLAY ""
  END IF

  --Pagos posteriores al periodo de pago 201205 con abono
  IF v_indice_pcrp > 0 THEN
     CALL fn_genera_interfase4()
     DISPLAY ""
  ELSE
     DISPLAY "Interface Pagos Posteriores al pp 201205 con abono por registro de pagos, no genero datos."
     DISPLAY ""
  END IF
  
  --DISPLAY "p_pid:-",p_pid,"-"
  --DISPLAY "p_proceso_cod:-",p_proceso_cod,"-"
  --DISPLAY "p_opera_cod:-",p_opera_cod,"-"

  --Finaliza la operación
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
  RETURNING r_bandera

  --DISPLAY "fn_actualiza_opera_fin - r_bandera: -",r_bandera,"-"

  IF r_bandera = 0 THEN
     DISPLAY ""
     DISPLAY "Finaliza la generación de las interfaces de Control de Pagos AS. ", TIME
     EXIT PROGRAM
  ELSE --Si ocurrió error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
     --DISPLAY "fn_error_opera - r_bandera: -",r_bandera,"-"
     CALL fn_desplega_inc_operacion(r_bandera)
     EXIT PROGRAM
  END IF
END MAIN

--Función que genera el archivo de salida del extractor de dispersión
FUNCTION fn_identifica_ctr_pagos()
   DEFINE r_bnd                 INTEGER,
          v_status_err          INTEGER ,
          v_desc_err            VARCHAR(200)

   WHENEVER ERROR CONTINUE
   PREPARE ps_sp_dis_hs FROM "EXECUTE PROCEDURE fn_val_mov_dis_as(?)"
   EXECUTE ps_sp_dis_hs USING f_folio
                          INTO r_bnd, v_status_err, v_desc_err
   WHENEVER ERROR STOP
END FUNCTION

#Llena tabla de aportaciones subsecuentes sin cargo y llena arreglo
FUNCTION fn_info_as_sc()

  --Pagos anteriores pp 201205 sin abono por registro de pagos
  LET g_sql_txt = "\n SELECT a.nrp,            ",
                  "\n        id.nss,           ",
                  "\n        id.rfc,           ",
                  "\n        a.periodo_pago,   ",
                  "\n        a.folio_sua,      ",
                  "\n        a.f_pago,         ",
                  "\n        id.curp,          ",
                  "\n        id.nombre_imss,   ",
                  "\n        0,                ",
                  "\n        a.apl_apo_pat,    ",
                  "\n        a.imp_apo_pat,    ",
                  "\n        a.valor_apl_apo,  ",
                  "\n        a.afore,          ",
                  "\n        a.f_liquida       ",
                  "\n FROM   tmp_dis_as_cp a,  ",
                  "\n        afi_derechohabiente id                       ",
                  "\n WHERE  a.id_derechohabiente = id.id_derechohabiente ",
                  --"\n AND    a.f_pago             <= '10032012'            ",
                  "\n AND    a.periodo_pago      <= '201205'              ",
                  "\n AND    a.edo_dispersion    IN (1,2)                 ",
                  "\n AND    a.edo_reg_pagos      = 0                     " 

  PREPARE prp_int_1 FROM g_sql_txt
  DECLARE cur_int_1 CURSOR FOR prp_int_1

  LET v_indice_asrp = 1

  FOREACH cur_int_1 INTO v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_nrp,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_nss,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_rfc,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_periodo_pago,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_folio_sua, 
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_f_pago,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_curp,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_nombre,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_num_credito,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_imp_apo_aivs,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_imp_apo_pat,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_valor_apl_apo, 
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_afore,
                         v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_f_liquida

    LET v_indice_asrp = v_indice_asrp + 1
  END FOREACH
  
  CALL v_arr_pp_ant_sin_mov_ab.deleteElement(v_indice_asrp)  
  LET v_indice_asrp = v_indice_asrp - 1

  --Pagos anteriores pp 201205 con abono por registro de pagos
  LET g_sql_txt = "\n SELECT a.nrp,            ",
                  "\n        id.nss,           ",
                  "\n        id.rfc,           ",
                  "\n        a.periodo_pago,   ",
                  "\n        a.folio_sua,      ",
                  "\n        a.f_pago,         ",
                  "\n        id.curp,          ",
                  "\n        id.nombre_imss,   ",
                  "\n        0,                ",
                  "\n        a.apl_apo_pat,    ",
                  "\n        a.imp_apo_pat,    ",
                  "\n        a.valor_apl_apo,  ",
                  "\n        a.afore,          ",
                  "\n        a.f_liquida       ",
                  "\n FROM   tmp_dis_as_cp a,  ",
                  "\n        afi_derechohabiente id                       ",
                  "\n WHERE  a.id_derechohabiente = id.id_derechohabiente ",
                  --"\n AND    a.f_pago             <= '10032012'           ",
                  "\n AND    a.periodo_pago      <= '201205'              ",
                  "\n AND    a.edo_dispersion    IN (1,2)                 ",
                  "\n AND    a.edo_reg_pagos      = 1                     " 

  PREPARE prp_int_2 FROM g_sql_txt
  DECLARE cur_int_2 CURSOR FOR prp_int_2

  LET v_indice_acrp = 1

  FOREACH cur_int_2 INTO v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_nrp,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_nss,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_rfc,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_periodo_pago,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_folio_sua, 
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_f_pago,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_curp,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_nombre,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_num_credito,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_imp_apo_aivs,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_imp_apo_pat,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_valor_apl_apo, 
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_afore,
                         v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_f_liquida

    LET v_indice_acrp = v_indice_acrp + 1
  END FOREACH
  
  CALL v_arr_pp_ant_con_mov_ab.deleteElement(v_indice_acrp)  
  LET v_indice_acrp = v_indice_acrp - 1

  --Pagos posteriores pp 201205 sin abono por registro de pagos
  LET g_sql_txt = "\n SELECT a.nrp,            ",
                  "\n        id.nss,           ",
                  "\n        id.rfc,           ",
                  "\n        a.periodo_pago,   ",
                  "\n        a.folio_sua,      ",
                  "\n        a.f_pago,         ",
                  "\n        id.curp,          ",
                  "\n        id.nombre_imss,   ",
                  "\n        0,                ",
                  "\n        a.apl_apo_pat,    ",
                  "\n        a.imp_apo_pat,    ",
                  "\n        a.valor_apl_apo,  ",
                  "\n        a.afore,          ",
                  "\n        a.f_liquida       ",
                  "\n FROM   tmp_dis_as_cp a,  ",
                  "\n        afi_derechohabiente id                       ",
                  "\n WHERE  id.id_derechohabiente = a.id_derechohabiente ",
                  --"\n AND    a.f_pago              > '10032012'           ",
                  "\n AND    a.periodo_pago        > '201205'             ",
                  "\n AND    a.edo_dispersion     IN (1,2)                ",
                  "\n AND    a.edo_reg_pagos       = 0                    " 

  PREPARE prp_int_3 FROM g_sql_txt
  DECLARE cur_int_3 CURSOR FOR prp_int_3

  LET v_indice_psrp = 1

  FOREACH cur_int_3 INTO v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_nrp,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_nss,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_rfc,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_periodo_pago,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_folio_sua, 
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_f_pago,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_curp,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_nombre,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_num_credito,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_imp_apo_aivs,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_imp_apo_pat,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_valor_apl_apo,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_afore,
                         v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_f_liquida

    LET v_indice_psrp = v_indice_psrp + 1
  END FOREACH
  
  CALL v_arr_pp_pos_sin_mov_ab.deleteElement(v_indice_psrp)  
  LET v_indice_psrp = v_indice_psrp - 1

  --Pagos posteriores pp 201205 con abono por registro de pagos
  LET g_sql_txt = "\n SELECT a.nrp,            ",
                  "\n        id.nss,           ",
                  "\n        id.rfc,           ",
                  "\n        a.periodo_pago,   ",
                  "\n        a.folio_sua,      ",
                  "\n        a.f_pago,         ",
                  "\n        id.curp,          ",
                  "\n        id.nombre_imss,   ",
                  "\n        0,                ",
                  "\n        a.apl_apo_pat,    ",
                  "\n        a.imp_apo_pat,    ",
                  "\n        a.valor_apl_apo,  ",
                  "\n        a.afore,          ",
                  "\n        a.f_liquida       ",
                  "\n FROM   tmp_dis_as_cp a,  ",
                  "\n        afi_derechohabiente id                       ",
                  "\n WHERE  a.id_derechohabiente = id.id_derechohabiente ",
                  --"\n AND    a.f_pago              > '10032012'           ",
                  "\n AND    a.periodo_pago       > '201205'              ",
                  "\n AND    a.edo_dispersion    IN (1,2)                 ",
                  "\n AND    a.edo_reg_pagos      = 1                     " 

  PREPARE prp_int_4 FROM g_sql_txt
  DECLARE cur_int_4 CURSOR FOR prp_int_4

  LET v_indice_pcrp = 1

  FOREACH cur_int_4 INTO v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_nrp,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_nss,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_rfc,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_periodo_pago,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_folio_sua, 
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_f_pago,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_curp,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_nombre,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_num_credito,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_imp_apo_aivs,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_imp_apo_pat,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_valor_apl_apo,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_afore,
                         v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_f_liquida

    LET v_indice_pcrp = v_indice_pcrp + 1
  END FOREACH
  
  CALL v_arr_pp_pos_con_mov_ab.deleteElement(v_indice_pcrp)  
  LET v_indice_pcrp = v_indice_pcrp - 1
END FUNCTION

# Genera un archivo txt con los datos del arreglo
--Pagos anteriores al periodo de pago 201205 sin abono por registro de pago
FUNCTION fn_genera_interfase1()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    sum_imp_apo_pat          DECIMAL(19,2),
    sum_imp_apo_aivs         DECIMAL(24,6)

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_pa_sa_"              --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"     --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_pa_sa_" || v_ddmmaaaa --Concatena nombre a buscar
  
  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  --LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_extension
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = v_tip_reg_enc,
                     v_id_ser,
                     v_id_ope,
                     v_tip_ent_ori,
                     v_cla_ent_ori,
                     v_tip_ent_des,
                     v_cla_ent_des,
                     v_fec_hoy,
                     v_reg_dia,
                     v_mod_rec_env_arc,
                     v_espacios_enc_c11
                     
  CALL v_ch_arch_salida.write([v_encabezado])

  LET sum_imp_apo_pat  = 0 
  LET sum_imp_apo_aivs = 0 

  --Imprime el detalle del archivo
  FOR v_indice_asrp = 1 TO v_arr_pp_ant_sin_mov_ab.getLength()
      --Concatenación del detalle
      LET v_detalle = v_tip_reg,                                                --Tipo de registro
                      v_id_ser,                                                 --Identificador del servicio
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_nrp,             --Numero de registro patronal
                      "             ",                                          --RFC del patrón
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_periodo_pago,    --Periodo de pago
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_folio_sua USING "&&&&&&",     --Folio SUA
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_f_pago USING "yyyymmdd",      --Fecha de pago
                      "        ",                                               --Fecha valor Institutos
                      --Datos del trabajador
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_nss,             --Número de Seguridad Social 
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_rfc,             --RFC
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_curp,            --CURP
                      "    ",                                                   --Filler queda en blanco,
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_nombre,          --Nombre
                      "       ",                                                --Ultimo salario diario integrado del periodo
                      "    ",                                                   --Filler
                      --Dias para pago bimestral
                      "  ",                                                     --Días cotizados en el bimestre
                      "  ",                                                     --Días de incapacidad en el bimestre
                      "  ",                                                     --Días de ausentismo en el bimestre
                      --Pagos por vivienda
                      "       ",                                                --Filler
                      (v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_imp_apo_pat * 100 USING "&&&&&&&"), --Importe Aportación Patronal INFONAVIT
                      --Pagos por Amortización Créditos de vivienda
                      "       ",                                                --Filler
                      "      0",                                                --Importe Amortización de Credito INFONAVIT
                      (v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&", --Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                      (v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_valor_apl_apo* 1000000) USING "&&&&&&&&&&&",     --Valor de la aplicación de intereses de vivienda de la aportación patronal INFONAVIT
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_afore,           --Afore
                      v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_f_liquida USING "yyyymmdd",   --Fecha de liquidación
                      "      0",                                                --Intereses generados por pagos extemporaneos de vivienda
                      "              0",                                        --Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                      v_espacios_det_c29                                        --Filler

      LET sum_imp_apo_pat  = sum_imp_apo_pat  + v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_imp_apo_pat
      LET sum_imp_apo_aivs = sum_imp_apo_aivs + v_arr_pp_ant_sin_mov_ab[v_indice_asrp].v_imp_apo_aivs
      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
 
  --Escribe el sumario
  LET v_sumario = v_tip_reg_sum,
                  v_id_ser,
                  v_id_ope,
                  v_tip_ent_ori,
                  v_cla_ent_ori,
                  v_tip_ent_des,
                  v_cla_ent_des,
                  v_fec_hoy,
                  v_reg_dia,
                  v_arr_pp_ant_sin_mov_ab.getLength() USING "&&&&&&&&",         --Total de registros
                  --Pagos por vivienda y amortización de créditos
                  "                 ",                                          --Filler
                  (sum_imp_apo_pat * 100)      USING "&&&&&&&&&&&&&&&&&",       --Sumatoria Importe Aportación Patronal INFONAVIT 
                  "                                  ",                         --Filler
                  "00000000000000000",                                          --Sumatoria Importe de Amortización del Credito INFONAVIT
                  (sum_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&&&&",      --Sumatoria Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                  "00000000000 ",                                                --Sumatoria Intereses generados por pagos extemporaneos de vivienda
                  "000000000000000000",                                         --Sumatoria Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                  v_espacios_sum_c18                                            --Filler

  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Pagos Anteriores al pp 201205 sin abono en la ruta. ",v_ruta_nomarch
  DISPLAY "Total de registros: ", v_arr_pp_ant_sin_mov_ab.getLength() USING "&&&&&&&&"
  DISPLAY ""
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,       --Cadena de comando a ejecutar
    v_consecutivo            INTEGER       --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(25)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"
  LET ch    = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[19,21] --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN      --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo    --Regresa el consecutivo del siguiente archivo del día
END FUNCTION

# Genera un archivo txt con los datos del arreglo
--Pagos anteriores al periodo de pago 201205 con abono por registro de pago
FUNCTION fn_genera_interfase2()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    sum_imp_apo_pat          DECIMAL(19,2),
    sum_imp_apo_aivs         DECIMAL(24,6)

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_pa_ca_"              --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"     --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_pa_ca_" || v_ddmmaaaa --Concatena nombre a buscar
  
  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  --LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_extension
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = v_tip_reg_enc,
                     v_id_ser,
                     v_id_ope,
                     v_tip_ent_ori,
                     v_cla_ent_ori,
                     v_tip_ent_des,
                     v_cla_ent_des,
                     v_fec_hoy,
                     v_reg_dia,
                     v_mod_rec_env_arc,
                     v_espacios_enc_c11
                     
  CALL v_ch_arch_salida.write([v_encabezado])

  LET sum_imp_apo_pat  = 0 
  LET sum_imp_apo_aivs = 0 

  --Imprime el detalle del archivo
  FOR v_indice_acrp = 1 TO v_arr_pp_ant_con_mov_ab.getLength()
      --Concatenación del detalle
      LET v_detalle = v_tip_reg,                                                --Tipo de registro
                      v_id_ser,                                                 --Identificador del servicio
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_nrp,             --Numero de registro patronal
                      "             ",                                          --RFC del patrón
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_periodo_pago,    --Periodo de pago
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_folio_sua USING "&&&&&&", --Folio SUA
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_f_pago USING "yyyymmdd",  --Fecha de pago
                      "        ",                                               --Fecha valor Institutos
                      --Datos del trabajador
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_nss,             --Número de Seguridad Social 
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_rfc,             --RFC
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_curp,            --CURP
                      "    ",                                                   --Filler queda en blanco,
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_nombre,          --Nombre
                      "       ",                                                --Ultimo salario diario integrado del periodo
                      "    ",                                                   --Filler
                      --Dias para pago bimestral
                      "  ",                                                     --Días cotizados en el bimestre
                      "  ",                                                     --Días de incapacidad en el bimestre
                      "  ",                                                     --Días de ausentismo en el bimestre
                      --Pagos por vivienda
                      "       ",                                                --Filler
                      (v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_imp_apo_pat * 100 USING "&&&&&&&"), --Importe Aportación Patronal INFONAVIT
                      --Pagos por Amortización Créditos de vivienda
                      "       ",                                                --Filler
                      "      0",                                                --Importe Amortización de Credito INFONAVIT
                      (v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&", --Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                      (v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_valor_apl_apo* 1000000) USING "&&&&&&&&&&&",     --Valor de la aplicación de intereses de vivienda de la aportación patronal INFONAVIT
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_afore,                    --Afore
                      v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_f_liquida USING "yyyymmdd",   --Fecha de liquidación
                      "      0",                                                --Intereses generados por pagos extemporaneos de vivienda
                      "              0",                                        --Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                      v_espacios_det_c29                                        --Filler

      LET sum_imp_apo_pat  = sum_imp_apo_pat  + v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_imp_apo_pat
      LET sum_imp_apo_aivs = sum_imp_apo_aivs + v_arr_pp_ant_con_mov_ab[v_indice_acrp].v_imp_apo_aivs
      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
 
  --Escribe el sumario
  LET v_sumario = v_tip_reg_sum,
                  v_id_ser,
                  v_id_ope,
                  v_tip_ent_ori,
                  v_cla_ent_ori,
                  v_tip_ent_des,
                  v_cla_ent_des,
                  v_fec_hoy,
                  v_reg_dia,
                  v_arr_pp_ant_con_mov_ab.getLength() USING "&&&&&&&&",         --Total de registros
                  --Pagos por vivienda y amortización de créditos
                  "                 ",                                          --Filler
                  (sum_imp_apo_pat * 100)      USING "&&&&&&&&&&&&&&&&&",       --Sumatoria Importe Aportación Patronal INFONAVIT 
                  "                                  ",                         --Filler
                  "00000000000000000",                                          --Sumatoria Importe de Amortización del Credito INFONAVIT
                  (sum_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&&&&",      --Sumatoria Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                  "00000000000 ",                                                --Sumatoria Intereses generados por pagos extemporaneos de vivienda
                  "000000000000000000",                                         --Sumatoria Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                  v_espacios_sum_c18                                            --Filler

  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Pagos Anteriores al pp 201205 con abono en la ruta. ",v_ruta_nomarch
  DISPLAY "Total de registros: ", v_arr_pp_ant_con_mov_ab.getLength() USING "&&&&&&&&" 
  DISPLAY ""
END FUNCTION

# Genera un archivo txt con los datos del arreglo
--Pagos posteriores al periodo de pago 201205 sin abono por registro de pago
FUNCTION fn_genera_interfase3()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    sum_imp_apo_pat          DECIMAL(19,2),
    sum_imp_apo_aivs         DECIMAL(24,6)

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_pp_sa_"              --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"     --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_pp_sa_" || v_ddmmaaaa --Concatena nombre a buscar
  
  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  --LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_extension
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = v_tip_reg_enc,
                     v_id_ser,
                     v_id_ope,
                     v_tip_ent_ori,
                     v_cla_ent_ori,
                     v_tip_ent_des,
                     v_cla_ent_des,
                     v_fec_hoy,
                     v_reg_dia,
                     v_mod_rec_env_arc,
                     v_espacios_enc_c11
                     
  CALL v_ch_arch_salida.write([v_encabezado])

  LET sum_imp_apo_pat  = 0 
  LET sum_imp_apo_aivs = 0 

  --Imprime el detalle del archivo
  FOR v_indice_psrp = 1 TO v_arr_pp_pos_sin_mov_ab.getLength()
      --Concatenación del detalle
      LET v_detalle = v_tip_reg,                                                --Tipo de registro
                      v_id_ser,                                                 --Identificador del servicio
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_nrp,             --Numero de registro patronal
                      "             ",                                          --RFC del patrón
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_periodo_pago,    --Periodo de pago
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_folio_sua USING "&&&&&&",     --Folio SUA
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_f_pago USING "yyyymmdd",      --Fecha de pago
                      "        ",                                               --Fecha valor Institutos
                      --Datos del trabajador
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_nss,             --Número de Seguridad Social 
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_rfc,             --RFC
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_curp,            --CURP
                      "    ",                                                   --Filler queda en blanco,
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_nombre,          --Nombre
                      "       ",                                                --Ultimo salario diario integrado del periodo
                      "    ",                                                   --Filler
                      --Dias para pago bimestral
                      "  ",                                                     --Días cotizados en el bimestre
                      "  ",                                                     --Días de incapacidad en el bimestre
                      "  ",                                                     --Días de ausentismo en el bimestre
                      --Pagos por vivienda
                      "       ",                                                --Filler
                      (v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_imp_apo_pat * 100 USING "&&&&&&&"), --Importe Aportación Patronal INFONAVIT
                      --Pagos por Amortización Créditos de vivienda
                      "       ",                                                --Filler
                      "      0",                                                --Importe Amortización de Credito INFONAVIT
                      (v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&", --Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                      (v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_valor_apl_apo* 1000000) USING "&&&&&&&&&&&",     --Valor de la aplicación de intereses de vivienda de la aportación patronal INFONAVIT
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_afore,           --Afore
                      v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_f_liquida USING "yyyymmdd",   --Fecha de liquidación
                      "      0",                                                --Intereses generados por pagos extemporaneos de vivienda
                      "              0",                                        --Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                      v_espacios_det_c29                                        --Filler

      LET sum_imp_apo_pat  = sum_imp_apo_pat  + v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_imp_apo_pat
      LET sum_imp_apo_aivs = sum_imp_apo_aivs + v_arr_pp_pos_sin_mov_ab[v_indice_psrp].v_imp_apo_aivs
      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
 
  --Escribe el sumario
  LET v_sumario = v_tip_reg_sum,
                  v_id_ser,
                  v_id_ope,
                  v_tip_ent_ori,
                  v_cla_ent_ori,
                  v_tip_ent_des,
                  v_cla_ent_des,
                  v_fec_hoy,
                  v_reg_dia,
                  v_arr_pp_pos_sin_mov_ab.getLength() USING "&&&&&&&&",         --Total de registros
                  --Pagos por vivienda y amortización de créditos
                  "                 ",                                          --Filler
                  (sum_imp_apo_pat * 100)      USING "&&&&&&&&&&&&&&&&&",       --Sumatoria Importe Aportación Patronal INFONAVIT 
                  "                                  ",                         --Filler
                  "00000000000000000",                                          --Sumatoria Importe de Amortización del Credito INFONAVIT
                  (sum_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&&&&",      --Sumatoria Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                  "00000000000 ",                                                --Sumatoria Intereses generados por pagos extemporaneos de vivienda
                  "000000000000000000",                                         --Sumatoria Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                  v_espacios_sum_c18                                            --Filler

  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Pagos Posteriores al pp 201205 sin abono en la ruta. ",v_ruta_nomarch
  DISPLAY "Total de registros: ", v_arr_pp_pos_sin_mov_ab.getLength() USING "&&&&&&&&"
  DISPLAY ""
END FUNCTION 

# Genera un archivo txt con los datos del arreglo
--Pagos posteriores al periodo de pago 201205 con abono por registro de pago
FUNCTION fn_genera_interfase4()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    sum_imp_apo_pat          DECIMAL(19,2),
    sum_imp_apo_aivs         DECIMAL(24,6)

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_pp_ca_"              --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"     --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_pp_ca_" || v_ddmmaaaa --Concatena nombre a buscar
  
  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  --LET v_nom_archivo       = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_extension
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = v_tip_reg_enc,
                     v_id_ser,
                     v_id_ope,
                     v_tip_ent_ori,
                     v_cla_ent_ori,
                     v_tip_ent_des,
                     v_cla_ent_des,
                     v_fec_hoy,
                     v_reg_dia,
                     v_mod_rec_env_arc,
                     v_espacios_enc_c11
                     
  CALL v_ch_arch_salida.write([v_encabezado])

  LET sum_imp_apo_pat  = 0 
  LET sum_imp_apo_aivs = 0 

  --Imprime el detalle del archivo
  FOR v_indice_pcrp = 1 TO v_arr_pp_pos_con_mov_ab.getLength()
      --Concatenación del detalle
      LET v_detalle = v_tip_reg,                                                --Tipo de registro
                      v_id_ser,                                                 --Identificador del servicio
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_nrp,             --Numero de registro patronal
                      "             ",                                          --RFC del patrón
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_periodo_pago,    --Periodo de pago
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_folio_sua USING "&&&&&&", --Folio SUA
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_f_pago USING "yyyymmdd",  --Fecha de pago
                      "        ",                                               --Fecha valor Institutos
                      --Datos del trabajador
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_nss,             --Número de Seguridad Social 
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_rfc,             --RFC
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_curp,            --CURP
                      "    ",                                                   --Filler queda en blanco,
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_nombre,          --Nombre
                      "       ",                                                --Ultimo salario diario integrado del periodo
                      "    ",                                                   --Filler
                      --Dias para pago bimestral
                      "  ",                                                     --Días cotizados en el bimestre
                      "  ",                                                     --Días de incapacidad en el bimestre
                      "  ",                                                     --Días de ausentismo en el bimestre
                      --Pagos por vivienda
                      "       ",                                                --Filler
                      (v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_imp_apo_pat * 100 USING "&&&&&&&"), --Importe Aportación Patronal INFONAVIT
                      --Pagos por Amortización Créditos de vivienda
                      "       ",                                                --Filler
                      "      0",                                                --Importe Amortización de Credito INFONAVIT
                      (v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&", --Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                      (v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_valor_apl_apo* 1000000) USING "&&&&&&&&&&&",     --Valor de la aplicación de intereses de vivienda de la aportación patronal INFONAVIT
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_afore,                    --Afore
                      v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_f_liquida USING "yyyymmdd",   --Fecha de liquidación
                      "      0",                                                --Intereses generados por pagos extemporaneos de vivienda
                      "              0",                                        --Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                      v_espacios_det_c29                                        --Filler

      LET sum_imp_apo_pat  = sum_imp_apo_pat  + v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_imp_apo_pat
      LET sum_imp_apo_aivs = sum_imp_apo_aivs + v_arr_pp_pos_con_mov_ab[v_indice_pcrp].v_imp_apo_aivs
      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
 
  --Escribe el sumario
  LET v_sumario = v_tip_reg_sum,
                  v_id_ser,
                  v_id_ope,
                  v_tip_ent_ori,
                  v_cla_ent_ori,
                  v_tip_ent_des,
                  v_cla_ent_des,
                  v_fec_hoy,
                  v_reg_dia,
                  v_arr_pp_pos_con_mov_ab.getLength() USING "&&&&&&&&",         --Total de registros
                  --Pagos por vivienda y amortización de créditos
                  "                 ",                                          --Filler
                  (sum_imp_apo_pat * 100)      USING "&&&&&&&&&&&&&&&&&",       --Sumatoria Importe Aportación Patronal INFONAVIT 
                  "                                  ",                         --Filler
                  "00000000000000000",                                          --Sumatoria Importe de Amortización del Credito INFONAVIT
                  (sum_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&&&&",      --Sumatoria Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                  "00000000000 ",                                                --Sumatoria Intereses generados por pagos extemporaneos de vivienda
                  "000000000000000000",                                         --Sumatoria Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                  v_espacios_sum_c18                                            --Filler

  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Pagos Posteriores al pp 201205 con abono en la ruta. ",v_ruta_nomarch
  DISPLAY "Total de registros: ", v_arr_pp_pos_con_mov_ab.getLength() USING "&&&&&&&&"
  DISPLAY ""
END FUNCTION

FUNCTION fn_dis_mov_cta(p_id_derechohabiente)
  DEFINE 
    p_id_derechohabiente     DECIMAL(9,0)

  DROP TABLE IF EXISTS tmp_cta_movimiento_sc;
  CREATE TABLE tmp_cta_movimiento_sc(id_derechohabiente DECIMAL(9,0),
                                     folio_liquida      DECIMAL(9,0),
                                     f_liquida          DATE,
                                     movimiento         SMALLINT);

  LET v_indice_sc = 1
   
  --Se agrega uso busqueda en tablas separadas de cta_movimiento
  FOR v_indice_cta = 1 TO arr_tbl_mov.getLength()
      LET v_QryTxt = "\n SELECT mov.id_derechohabiente, ",
                     "\n        mov.folio_liquida, ",
                     "\n        mov.f_liquida, ",
                     "\n        mov.movimiento ",
                     "\n FROM " ,arr_tbl_mov[v_indice_cta]," mov",
                     "\n WHERE  mov.id_derechohabiente = ", p_id_derechohabiente,
                     "\n AND    mov.subcuenta          = 4 ",
                     "\n AND    mov.movimiento        IN (SELECT b.movimiento ",
                     "\n                                  FROM   cat_movimiento b ",
                     "\n                                  WHERE  b.categoria = 2) "
      -- DISPLAY v_QryTxt

      PREPARE prp_cons_mov FROM v_QryTxt
      DECLARE cur_cons_mov CURSOR FOR prp_cons_mov
      FOREACH cur_cons_mov INTO g_arr_mov_cta[v_indice_sc].*
        INSERT INTO tmp_cta_movimiento_sc VALUES(g_arr_mov_cta[v_indice_sc].id_derechohabiente,
                                                 g_arr_mov_cta[v_indice_sc].folio_liquida,
                                                 g_arr_mov_cta[v_indice_sc].f_liquida,
                                                 g_arr_mov_cta[v_indice_sc].movimiento)
        LET v_indice_sc = v_indice_sc + 1
      END FOREACH
  END FOR

  CALL g_arr_mov_cta.deleteElement(v_indice_sc)

END FUNCTION