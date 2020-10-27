################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 25/01/2016                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS45                                                    #
#Objetivo         => Generar la interface de las aportaciones subsecuentes sin #
#                    conciliar.                                                #
#Fecha de Inicio  => 29/04/2014                                                #
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
    
  --Arreglo para información de la interface 
  DEFINE v_arr_dis_as_sc     DYNAMIC ARRAY OF RECORD   
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

  --Arreglo para información de la interface 
  DEFINE v_arr_dis_as_sc_m   DYNAMIC ARRAY OF RECORD   
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
    v_folio_dis              VARCHAR(9),
    v_nss                    CHAR(11),
    v_nrp                    CHAR(11),
    v_periodo_pago           CHAR(06),
    v_monto_apo_avance       VARCHAR(14),
    v_monto_amo_avance       VARCHAR(14),
    v_folio_pago             VARCHAR(9),
    v_monto_apo_pag          VARCHAR(14),
    v_monto_amo_pag          VARCHAR(14),
    v_folio_sua              DECIMAL(6,0),
    v_f_pago                 DATE
      
  DEFINE 
    f_folio                  DECIMAL(9,0),
    v_total_registros        BIGINT,
    f_tot_ava_apo            DECIMAL(22,2),
    f_tot_ava_amo            DECIMAL(22,2),
    f_tot_pag_apo            DECIMAL(22,2),
    f_tot_pag_amo            DECIMAL(22,2)

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
    v_indice                 INTEGER,
    v_indice_m               INTEGER

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

END GLOBALS 

MAIN
  DEFINE 
    r_bandera                SMALLINT
    
  --Asignación de parametros generales
  LET p_usuario         = ARG_VAL(1)
  LET p_pid             = ARG_VAL(2)
  LET p_proceso_cod     = ARG_VAL(3)
  LET p_opera_cod       = ARG_VAL(4)
  LET f_folio           = ARG_VAL(5) --Valor de argumento uno de DISL51

  LET v_modulo_cod      = "dis"

  LET v_total_registros = 0
    
  CALL fn_info_as_sc()
  --Pagos mayores al periodo de pago 201205 
  CALL fn_genera_interfase()
  --Pagos menores al periodo de pago 201205   
  CALL fn_genera_interfase_m()


  --DISPLAY "p_pid:-",p_pid,"-"
  --DISPLAY "p_proceso_cod:-",p_proceso_cod,"-"
  --DISPLAY "p_opera_cod:-",p_opera_cod,"-"

  --Finaliza la operación
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
  RETURNING r_bandera

  --DISPLAY "fn_actualiza_opera_fin - r_bandera: -",r_bandera,"-"

  IF r_bandera = 0 THEN
     DISPLAY "Finalizó la generación de la interface de Aportaciones Subsecuentes Sin Conciliar."
     EXIT PROGRAM
  ELSE --Si ocurrió error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
     --DISPLAY "fn_error_opera - r_bandera: -",r_bandera,"-"
     CALL fn_desplega_inc_operacion(r_bandera)
     EXIT PROGRAM
  END IF

END MAIN

#Llena tabla de aportaciones subsecuentes sin conciliar y llena arreglo
FUNCTION fn_info_as_sc()
  DEFINE 
    v_id_derechohabiente     DECIMAL(9,0),
    v_apl_apo_pat            DECIMAL(15,6),
    v_reg_pat_imss           CHAR(11),
    v_folio_liquida          DECIMAL(9,0),
    v_f_liquida              DATE,
    v_imp_apo_pat            DECIMAL(9,2),
    v_valor_apl_apo          DECIMAL(17,6),
    v_afore                  CHAR(03) 

  DEFINE v_num_credito       DECIMAL(10,0);
  DEFINE v_ind_concilia      SMALLINT;

  DEFINE v_cr_cred           SMALLINT;

  --Se genera el listado de tablas cta_movimiento
  LET v_indice_cta = 1
  
  DECLARE cur_tbl_mov CURSOR FOR 
  SELECT tabla 
  FROM   cat_tab_movimiento
  FOREACH cur_tbl_mov INTO arr_tbl_mov[v_indice_cta]
    LET v_indice_cta = v_indice_cta + 1
  END FOREACH

  LET arr_tbl_mov[v_indice_cta] = "cta_movimiento"

  LET v_ind_concilia = 1;
  LET v_cr_cred      = 1;

  LET g_sql_txt = "\n SELECT da.id_derechohabiente, ", 
                  "\n        da.apl_apo_pat, ", 
                  "\n        da.reg_pat_imss, ", 
                  "\n        da.periodo_pago, ",
                  "\n        da.folio_sua, ", 
                  "\n        da.f_pago, ",
                  "\n        da.folio_liquida, ", 
                  "\n        da.f_liquida, ", 
                  "\n        da.imp_apo_pat, ",
                  "\n        da.valor_apl_apo, ", 
                  "\n        da.afore ", 
                  "\n FROM   dis_ap_subsecuente da ", 
                  "\n WHERE  NOT EXISTS( ", 
                  "\n SELECT de.folio_liquida ", 
                  "\n FROM   dis_interface_ef de ", 
                  "\n WHERE  de.folio_liquida = da.folio_liquida) ",
                  "\n AND    NOT EXISTS ( ",
                  "\n SELECT sc.folio_ap_subs ",
                  "\n FROM   dis_as_sin_conciliar sc ",
                  "\n WHERE  sc.folio_ap_subs = da.folio_liquida) ",
                  "\n AND    da.apl_apo_pat   > 0 ", 
                  "\n AND    da.imp_apo_pat   > 0 " 

  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE prp_ext_apo FROM g_sql_txt
  DECLARE cur_ext_apo CURSOR FOR prp_ext_apo

  PREPARE prp_cred_viv FROM "EXECUTE PROCEDURE fn_credito_vivienda (?,?)"

  FOREACH cur_ext_apo INTO v_id_derechohabiente, 
                           v_apl_apo_pat,
                           v_reg_pat_imss,
                           v_periodo_pago,
                           v_folio_sua, 
                           v_f_pago,
                           v_folio_liquida, 
                           v_f_liquida, 
                           v_imp_apo_pat,
                           v_valor_apl_apo,
                           v_afore

    --EXECUTE prp_cred_viv USING v_id_derechohabiente, v_cr_cred
    --                      INTO v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida;

    CALL fn_dis_mov_cta(v_id_derechohabiente)

    LET v_bnd_mov_div_disp = 0

    SELECT COUNT(*)
    INTO   v_bnd_mov_div_disp
    FROM   tmp_cta_movimiento_sc a
    WHERE  a.id_derechohabiente = v_id_derechohabiente
    AND    a.f_liquida         >= v_f_pago; --Fecha Pago
    IF v_bnd_mov_div_disp = 0 THEN
       INSERT INTO dis_as_sin_conciliar VALUES (seq_dis_as_sin_conciliar.NEXTVAL, 
                                                v_id_derechohabiente, 
                                                v_num_credito, 
                                                v_apl_apo_pat, 
                                                v_imp_apo_pat,
                                                v_reg_pat_imss, 
                                                v_periodo_pago, 
                                                v_folio_sua, 
                                                v_f_pago,
                                                v_folio_liquida,
                                                f_folio, 
                                                v_ind_concilia,
                                                v_f_liquida,
                                                v_valor_apl_apo,
                                                v_afore)
    END IF
  END FOREACH

  --Pagos mayores al periodo de pago 201205
  LET g_sql_txt = "\n SELECT d.nrp, ",
                  "\n        ad.nss, ",
                  "\n        ad.rfc, ",
                  "\n        d.periodo_pago, ",
                  "\n        d.folio_sua, ",  
                  "\n        d.f_pago, ", 
                  "\n        ad.curp, ",
                  "\n        ad.nombre_imss, ", 
                  "\n        d.num_credito, ",
                  "\n        d.imp_apo_aivs, ",
                  "\n        d.imp_apo_pat, ",
                  "\n        d.valor_apl_apo, ",
                  "\n        d.afore, ", 
                  "\n        d.f_liquida ", 
                  "\n FROM   dis_as_sin_conciliar d, ",
                  "\n        afi_derechohabiente ad ",
                  "\n WHERE  d.id_derechohabiente = ad.id_derechohabiente ",
                  "\n AND    d.f_pago             > '10032012'"

  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE prp_dis_as FROM g_sql_txt
  DECLARE cur_dis_as CURSOR FOR prp_dis_as

  LET v_indice = 1  
  
  FOREACH cur_dis_as INTO v_arr_dis_as_sc[v_indice].v_nrp,
                          v_arr_dis_as_sc[v_indice].v_nss,
                          v_arr_dis_as_sc[v_indice].v_rfc,
                          v_arr_dis_as_sc[v_indice].v_periodo_pago, 
                          v_arr_dis_as_sc[v_indice].v_folio_sua, 
                          v_arr_dis_as_sc[v_indice].v_f_pago,
                          v_arr_dis_as_sc[v_indice].v_curp,
                          v_arr_dis_as_sc[v_indice].v_nombre, 
                          v_arr_dis_as_sc[v_indice].v_num_credito, 
                          v_arr_dis_as_sc[v_indice].v_imp_apo_aivs, 
                          v_arr_dis_as_sc[v_indice].v_imp_apo_pat, 
                          v_arr_dis_as_sc[v_indice].v_valor_apl_apo, 
                          v_arr_dis_as_sc[v_indice].v_afore,
                          v_arr_dis_as_sc[v_indice].v_f_liquida

    LET v_indice = v_indice + 1  
  END FOREACH
  
  CALL v_arr_dis_as_sc.deleteElement(v_indice)  
  LET v_indice = v_indice - 1

  --Pagos menores al periodo de pago 201205
  LET g_sql_txt_m = "\n SELECT d.nrp, ",
                    "\n        ad.nss, ",
                    "\n        ad.rfc, ",
                    "\n        d.periodo_pago, ",
                    "\n        d.folio_sua, ",  
                    "\n        d.f_pago, ", 
                    "\n        ad.curp, ",
                    "\n        ad.nombre_imss, ", 
                    "\n        d.num_credito, ",
                    "\n        d.imp_apo_aivs, ",
                    "\n        d.imp_apo_pat, ",
                    "\n        d.valor_apl_apo, ",
                    "\n        d.afore, ", 
                    "\n        d.f_liquida ", 
                    "\n FROM   dis_as_sin_conciliar d, ",
                    "\n        afi_derechohabiente ad ",
                    "\n WHERE  d.id_derechohabiente = ad.id_derechohabiente ",
                    "\n AND    d.f_pago            <= '10032012'"

  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE prp_dis_as_m FROM g_sql_txt_m
  DECLARE cur_dis_as_m CURSOR FOR prp_dis_as_m

  LET v_indice_m = 1  
  
  FOREACH cur_dis_as_m INTO v_arr_dis_as_sc_m[v_indice_m].v_nrp,
                            v_arr_dis_as_sc_m[v_indice_m].v_nss,
                            v_arr_dis_as_sc_m[v_indice_m].v_rfc,
                            v_arr_dis_as_sc_m[v_indice_m].v_periodo_pago, 
                            v_arr_dis_as_sc_m[v_indice_m].v_folio_sua, 
                            v_arr_dis_as_sc_m[v_indice_m].v_f_pago,
                            v_arr_dis_as_sc_m[v_indice_m].v_curp,
                            v_arr_dis_as_sc_m[v_indice_m].v_nombre, 
                            v_arr_dis_as_sc_m[v_indice_m].v_num_credito, 
                            v_arr_dis_as_sc_m[v_indice_m].v_imp_apo_aivs, 
                            v_arr_dis_as_sc_m[v_indice_m].v_imp_apo_pat, 
                            v_arr_dis_as_sc_m[v_indice_m].v_valor_apl_apo, 
                            v_arr_dis_as_sc_m[v_indice_m].v_afore,
                            v_arr_dis_as_sc_m[v_indice_m].v_f_liquida

    LET v_indice_m = v_indice_m + 1  
  END FOREACH
  
  CALL v_arr_dis_as_sc_m.deleteElement(v_indice_m)  
  LET v_indice_m = v_indice_m - 1
  
END FUNCTION 

# Genera un archivo txt con los datos del arreglo
--Pagos mayores al periodo de pago 201205 
FUNCTION fn_genera_interfase()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    --v_indice                 INTEGER,
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
    v_tip_reg_sum            CHAR(02), 
    sum_imp_apo_pat          DECIMAL(19,2),
    sum_imp_apo_aivs         DECIMAL(24,6)

  LET v_modulo_cod      = "dis"    

  LET v_tip_reg_enc     = '01'
  LET v_id_ser          = '03'
  LET v_id_ope          = '59'
  LET v_tip_ent_ori     = '03' 
  LET v_cla_ent_ori     = '001'
  LET v_tip_ent_des     = '04'
  LET v_cla_ent_des     = '002'
  LET v_mod_rec_env_arc = '02'
  LET v_fec_hoy         = TODAY USING "yyyymmdd"  --Fecha del dia

  LET v_tip_reg         = '02'
  LET v_tip_reg_sum     = '09'

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_as_sc_"              --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"     --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_as_sc_" || v_ddmmaaaa --Concatena nombre a buscar
  
  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia           = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo       = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch      = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida    = base.Channel.create()
   
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
                     v_mod_rec_env_arc
                     
  CALL v_ch_arch_salida.write([v_encabezado])

  LET sum_imp_apo_pat  = 0 
  LET sum_imp_apo_aivs = 0 

  --Imprime el detalle del archivo
  FOR v_indice = 1 TO v_arr_dis_as_sc.getLength()
      --Concatenación del detalle
      LET v_detalle = v_tip_reg,                                                --Tipo de registro
                      v_id_ser,                                                 --Identificador del servicio
                      v_arr_dis_as_sc[v_indice].v_nrp,                          --Numero de registro patronal
                      "             ",                                          --RFC del patrón
                      v_arr_dis_as_sc[v_indice].v_periodo_pago,                 --Periodo de pago
                      v_arr_dis_as_sc[v_indice].v_folio_sua USING "&&&&&&",     --Folio SUA
                      v_arr_dis_as_sc[v_indice].v_f_pago USING "yyyymmdd",      --Fecha de pago
                      "        ",                                               --Fecha valor Institutos
                      --Datos del trabajador
                      v_arr_dis_as_sc[v_indice].v_nss,                          --Número de Seguridad Social 
                      v_arr_dis_as_sc[v_indice].v_rfc,                          --RFC
                      v_arr_dis_as_sc[v_indice].v_curp,                         --CURP
                      "    ",                                                   --Filler queda en blanco,
                      v_arr_dis_as_sc[v_indice].v_nombre,                       --Nombre
                      "       ",                                                --Ultimo salario diario integrado del periodo
                      "    ",                                                   --Filler
                      --Dias para pago bimestral
                      "  ",                                                     --Días cotizados en el bimestre
                      "  ",                                                     --Días de incapacidad en el bimestre
                      "  ",                                                     --Días de ausentismo en el bimestre
                      --Pagos por vivienda
                      "       ",                                                --Filler
                      (v_arr_dis_as_sc[v_indice].v_imp_apo_pat * 100 USING "&&&&&&&"), --Importe Aportación Patronal INFONAVIT
                      --Pagos por Amortización Créditos de vivienda
                      "       ",                                                --Filler
                      "      0",                                                --Importe Amortización de Credito INFONAVIT
                      (v_arr_dis_as_sc[v_indice].v_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&", --Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                      (v_arr_dis_as_sc[v_indice].v_valor_apl_apo* 1000000) USING "&&&&&&&&&&&",     --Valor de la aplicación de intereses de vivienda de la aportación patronal INFONAVIT
                      v_arr_dis_as_sc[v_indice].v_afore,                        --Afore
                      v_arr_dis_as_sc[v_indice].v_f_liquida USING "yyyymmdd",   --Fecha de liquidación
                      "      0",                                                --Intereses generados por pagos extemporaneos de vivienda
                      "              0",                                        --Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                      "                                                            " --Filler

      LET sum_imp_apo_pat  = sum_imp_apo_pat  + v_arr_dis_as_sc[v_indice].v_imp_apo_pat
      LET sum_imp_apo_aivs = sum_imp_apo_aivs + v_arr_dis_as_sc[v_indice].v_imp_apo_aivs
      
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
                  v_arr_dis_as_sc.getLength(),                                  --Total de registros
                  --Pagos por vivienda y amortización de créditos
                  "                 ",                                          --Filler
                  (sum_imp_apo_pat * 100)      USING "&&&&&&&&&&&&&&&&&",       -- Sumatoria Importe Aportación Patronal INFONAVIT 
                  "                                  ",                         --Filler
                  "                0",                                          --Sumatoria Importe de Amortización del Credito INFONAVIT
                  (sum_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&&&&",      --Sumatoria Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                  "          0",                                                --Sumatoria Intereses generados por pagos extemporaneos de vivienda
                  "                 0",                                         --Sumatoria Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                  "                                                      "      --Filler

  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Aportaciones Subsecuentes Sin Conciliar\nen la ruta ",v_ruta_nomarch
  DISPLAY ""
  
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
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

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día
END FUNCTION

# Genera un archivo txt con los datos del arreglo
--Pagos menores al periodo de pago 201205 
FUNCTION fn_genera_interfase_m()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    --v_indice                 INTEGER,
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
    v_tip_reg_sum            CHAR(02), 
    sum_imp_apo_pat          DECIMAL(19,2),
    sum_imp_apo_aivs         DECIMAL(24,6)

  LET v_modulo_cod      = "dis"    

  LET v_tip_reg_enc     = '01'
  LET v_id_ser          = '03'
  LET v_id_ope          = '59'
  LET v_tip_ent_ori     = '03' 
  LET v_cla_ent_ori     = '001'
  LET v_tip_ent_des     = '04'
  LET v_cla_ent_des     = '002'
  LET v_mod_rec_env_arc = '02'
  LET v_fec_hoy         = TODAY USING "yyyymmdd"  --Fecha del dia

  LET v_tip_reg         = '02'
  LET v_tip_reg_sum     = '09'

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_as_sc_m_"              --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"       --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_as_sc_m_" || v_ddmmaaaa --Concatena nombre a buscar
  
  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo_m(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia           = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo       = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch      = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida    = base.Channel.create()
   
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
                     v_mod_rec_env_arc
                     
  CALL v_ch_arch_salida.write([v_encabezado])

  LET sum_imp_apo_pat  = 0 
  LET sum_imp_apo_aivs = 0 

  --Imprime el detalle del archivo
  FOR v_indice_m = 1 TO v_arr_dis_as_sc_m.getLength()
      --Concatenación del detalle
      LET v_detalle = v_tip_reg,                                                --Tipo de registro
                      v_id_ser,                                                 --Identificador del servicio
                      v_arr_dis_as_sc_m[v_indice_m].v_nrp,                      --Numero de registro patronal
                      "             ",                                          --RFC del patrón
                      v_arr_dis_as_sc_m[v_indice_m].v_periodo_pago,             --Periodo de pago
                      v_arr_dis_as_sc_m[v_indice_m].v_folio_sua USING "&&&&&&", --Folio SUA
                      v_arr_dis_as_sc_m[v_indice_m].v_f_pago USING "yyyymmdd",  --Fecha de pago
                      "        ",                                               --Fecha valor Institutos
                      --Datos del trabajador
                      v_arr_dis_as_sc_m[v_indice_m].v_nss,                      --Número de Seguridad Social 
                      v_arr_dis_as_sc_m[v_indice_m].v_rfc,                      --RFC
                      v_arr_dis_as_sc_m[v_indice_m].v_curp,                     --CURP
                      "    ",                                                   --Filler queda en blanco,
                      v_arr_dis_as_sc_m[v_indice_m].v_nombre,                   --Nombre
                      "       ",                                                --Ultimo salario diario integrado del periodo
                      "    ",                                                   --Filler
                      --Dias para pago bimestral
                      "  ",                                                     --Días cotizados en el bimestre
                      "  ",                                                     --Días de incapacidad en el bimestre
                      "  ",                                                     --Días de ausentismo en el bimestre
                      --Pagos por vivienda
                      "       ",                                                --Filler
                      (v_arr_dis_as_sc_m[v_indice_m].v_imp_apo_pat * 100 USING "&&&&&&&"), --Importe Aportación Patronal INFONAVIT
                      --Pagos por Amortización Créditos de vivienda
                      "       ",                                                --Filler
                      "      0",                                                --Importe Amortización de Credito INFONAVIT
                      (v_arr_dis_as_sc_m[v_indice_m].v_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&", --Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                      (v_arr_dis_as_sc_m[v_indice_m].v_valor_apl_apo* 1000000) USING "&&&&&&&&&&&",     --Valor de la aplicación de intereses de vivienda de la aportación patronal INFONAVIT
                      v_arr_dis_as_sc_m[v_indice_m].v_afore,                    --Afore
                      v_arr_dis_as_sc_m[v_indice_m].v_f_liquida USING "yyyymmdd",   --Fecha de liquidación
                      "      0",                                                --Intereses generados por pagos extemporaneos de vivienda
                      "              0",                                        --Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                      "                                                            " --Filler

      LET sum_imp_apo_pat  = sum_imp_apo_pat  + v_arr_dis_as_sc_m[v_indice_m].v_imp_apo_pat
      LET sum_imp_apo_aivs = sum_imp_apo_aivs + v_arr_dis_as_sc_m[v_indice_m].v_imp_apo_aivs
      
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
                  v_arr_dis_as_sc_m.getLength(),                                --Total de registros
                  --Pagos por vivienda y amortización de créditos
                  "                 ",                                          --Filler
                  (sum_imp_apo_pat * 100)      USING "&&&&&&&&&&&&&&&&&",       -- Sumatoria Importe Aportación Patronal INFONAVIT 
                  "                                  ",                         --Filler
                  "                0",                                          --Sumatoria Importe de Amortización del Credito INFONAVIT
                  (sum_imp_apo_aivs * 1000000) USING "&&&&&&&&&&&&&&&&&&",      --Sumatoria Número de aplicaciones de intereses de vivienda de la aportación patronal INFONAVIT
                  "          0",                                                --Sumatoria Intereses generados por pagos extemporaneos de vivienda
                  "                 0",                                         --Sumatoria Número de aplicaciones de intereses generados por pagos extemporaneos de vivienda
                  "                                                      "      --Filler

  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Aportaciones Subsecuentes Sin Conciliar (Pagos menores al período de pago 201205)\nen la ruta ",v_ruta_nomarch
  DISPLAY ""
  
END FUNCTION

FUNCTION fn_crea_nombre_archivo_m(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,       --Cadena de comando a ejecutar
    v_consecutivo            INTEGER       --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(27)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"
  LET ch    = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[21,23] --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día
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