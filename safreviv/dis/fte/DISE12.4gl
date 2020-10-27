################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 13/08/2013                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE12                                                   #
#Objetivo          => Programa para integrar el archivo de nuevos              #                   
#                     acreditados especiales que ha sido validado              #
#Fecha inicio      => 13/08/2013                                               #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     20140123      Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
DATABASE
  safre_viv

GLOBALS
  DEFINE 
    v_usurio                 VARCHAR(30), -- Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod, -- codigo de proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, -- codigo de operacion
    g_folio                  LIKE dis_det_avance_pago.folio, -- Folio generdo
    l_pid                    LIKE glo_pid.pid,
    g_qrytxt                 STRING -- Prepara consultas

  DEFINE
    v_tot_leidos             DECIMAL(10,0),
    v_tot_ceros              DECIMAL(10,0),
    v_tot_no_enc             DECIMAL(10,0),
    v_tot_si_enc             DECIMAL(10,0),
    v_tot_monto_igual        DECIMAL(10,0),
    v_tot_monto_dif          DECIMAL(10,0),
    v_tot_dup_ap             DECIMAL(10,0)

  DEFINE
    l_arch_proceso           VARCHAR(100)
      
   CONSTANT Por_Folio = 0
   CONSTANT Por_Fecha = 1
   CONSTANT Sin = 0
   DEFINE v_tbl_mov    VARCHAR(50) 

END GLOBALS

--Objetivo: Funcion que realiza la carga de tablas hitoricas de avance de pago
MAIN
  DEFINE 
    l_s_qryTxt               STRING, -- guarda una sentencia SQL a ejecutar
    r_b_valida               SMALLINT,
    r_bnd_edo_act_archivo    SMALLINT,
    r_ruta_reporte           STRING, -- ruta del archivo del reporte
    r_edo_rech               SMALLINT,
    r_bnd_oera_error         SMALLINT,
    p_transaccion            SMALLINT, --Bandera que indica si la ejecución es manual o automática

    r_bnd                    INTEGER, 
    v_status_err             INTEGER ,
    v_desc_err               VARCHAR(200),
    error_info               CHAR(70),
    v_bnd_transaccion        SMALLINT
    
  --Definición de variables que se usarán como parte del llamado a los SP de dispersión(sp_dis_transaccion) 
  DEFINE
    v_precio_fec_hoy_11      DECIMAL(19,14),  --Precio de fondo del dia, DEL FONDO 11
    v_precio_fec_hoy_10      DECIMAL(19,14),  --Precio del fondo del día, fondo 10
    v_derechohabiente_pag    DECIMAL(9,0),   --Derechohabiente de cuenta credito
    v_folio_sua              DECIMAL(6),     --ID de referencia
    v_periodo_pago           CHAR(6),        --Periodo de pago
    v_f_pago                 DATE,           --Fecha de pago
    v_nrp                    CHAR(11),       --Registro patronal
    v_id_referencia          DECIMAL(9,0),   --ID de referencia
    v_localiza_trabajador    CHAR(01),       --Localización del trabajador
    v_tpo_patron             CHAR(02),       --Tipo de patron
    v_imp_ap_pat             DECIMAL(12,2),  --Importe aportaciones patronales -- Son los pesos no convertidos
    v_imp_am_cre             DECIMAL(12,2),  --Importe amortizaciones de credito                              
	 v_aiv_ap_pat             DECIMAL(18,6),
    v_num_credito_pag        DECIMAL(10,0),
    v_num_credito            DECIMAL(10,0),
    v_num_credito_crd        DECIMAL(10,0),
    v_cve_ent_receptora      CHAR(3),
    v_f_actual               DATE,
	 v_periodo_bimestre       CHAR(6),        --Período de Pago Bimestre
	 v_verifica_cifras        SMALLINT,
	 v_nss                    LIKE afi_derechohabiente.nss, --Número de seguridad social para el derechohabiente
    v_aivs                   DECIMAL (16,6)
    
  LET v_usurio       = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)
  
  --Inicialización de variables
  LET v_precio_fec_hoy_11       = 0.00
  LET v_precio_fec_hoy_10       = 0.00
  LET v_nrp                  = ""
  LET v_periodo_pago         = ""
  LET v_folio_sua            = 0
  LET v_id_referencia        = 0
  LET v_imp_ap_pat           = 0.00
  LET v_imp_am_cre           = 0.00
  LET v_f_pago               = TODAY
  LET v_localiza_trabajador  = ""
  LET v_tpo_patron           = ""
  LET v_derechohabiente_pag  = 0
  LET v_num_credito_pag      = 0
  LET v_num_credito          = 0
  LET v_num_credito_crd      = 0
  LET v_cve_ent_receptora	  = ""
  LET v_f_actual             = TODAY

  LET v_periodo_bimestre     = ""
  LET p_transaccion          = 0
  LET v_verifica_cifras      = 0
  LET v_aivs                 = 0.0

  PREPARE fn_tbl_mov FROM "execute function fn_tab_movimiento(?,?,?)"

  --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  SELECT ind_tipo_ejecucion 
  INTO   p_transaccion
  FROM   bat_ctr_operacion 
  WHERE  proceso_cod = g_proceso_cod   
  AND    pid         = l_pid
  AND    opera_cod   = g_opera_cod

  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usurio)
     RETURNING g_folio
  END IF

  -- Validaciones de detalle vs sumario
  CALL fn_valida_aportaciones(g_folio) RETURNING r_edo_rech

   
  --Si el estado del rechazo es 10, estatus correcto
  IF r_edo_rech = 10 THEN

     WHENEVER ERROR CONTINUE
        --Identifica duplicidad de registro de aportaciones subsecuentes
          --Se comenta store y se incluye función de identificación de duplicados
          --basicamente para no hacer modificaciones innecesarias al store
          CALL fn_dis_apo_esp_subs4(r_edo_rech)
          RETURNING r_bnd, v_status_err,  v_desc_err
          WHENEVER ERROR STOP
    
     WHENEVER ERROR CONTINUE 

     IF r_bnd <> 0 THEN
        DISPLAY "Error2: ",v_status_err," - ",v_desc_err," - ",r_bnd
         
        -- Actualiza el estado del archivo procesado
        CALL fn_act_edo_archivo(l_arch_proceso,g_folio,2,v_usurio)
        RETURNING r_bnd_edo_act_archivo
         
        -- Función para finalizar la operación en error
        CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
        RETURNING r_bnd_oera_error
     
        EXIT PROGRAM 
     END IF
     
        --Identificar precio de acción del día para el fondo 11
        SELECT precio_fondo
        INTO   v_precio_fec_hoy_11
        FROM   glo_valor_fondo
        WHERE  fondo       = 11
        AND    f_valuacion = TODAY;
        
        --Selección del precio de fondo para el fondo 10
  	     SELECT precio_fondo
        INTO   v_precio_fec_hoy_10
        FROM   glo_valor_fondo
        WHERE  fondo = 10
        AND    f_valuacion = TODAY;
        
        
     --Leer todos los registros de la tabla cta_his_pagos
     --Identifica precio de fondo de Fecha de Pago patronal
                       
      LET l_s_qryTxt="SELECT reg_pat_imss,\n",              
                     "nss,\n",                              
                     "imp_apo_pat/100,\n",
                     "imp_amo_cred/100,\n",
                     "aplic_int_viv_apo_pat/1000000,\n",
                     "folio_sua,\n",
                     "periodo_pago,\n",
                     "f_pago\n",
                     "FROM safre_tmp:tmp_dis_aposubs_e2"                                     
                     
                                              
     --Preparación del statement de selección               
     PREPARE stm_det_apo_esp FROM l_s_qryTxt                
     DECLARE cur_det_apo_esp CURSOR FOR stm_det_apo_esp     
     
     FOREACH cur_det_apo_esp INTO v_nrp,
     	                            v_nss,
     	                            v_imp_ap_pat,
     	                            v_imp_am_cre,
     	                            v_aiv_ap_pat,
                                  v_folio_sua,
                                  v_periodo_pago,
                                  v_f_pago
                                  
	      --Se invoca el procedimiento para la obtención del bimestre de pag
	      --a partir de la fecha de pago
	      LET l_s_qryTxt="EXECUTE PROCEDURE fn_bimestre_pago(?)"
	      PREPARE stm_bimestre_pago FROM l_s_qryTxt
	      EXECUTE stm_bimestre_pago INTO v_periodo_bimestre USING v_periodo_pago
	      
	      --Selección del derechohabiente
	      LET l_s_qryTxt="SELECT id_derechohabiente\n",
                        "FROM afi_derechohabiente\n",
                        "WHERE nss='",v_nss,"'"
        --Ejecución del statement para la obtención de id_derechohabiente               
        PREPARE stm_id_derechohabiente FROM l_s_qryTxt
        EXECUTE stm_id_derechohabiente INTO v_derechohabiente_pag
	      
	      --Si el importe de las aportaciones patronales es nulo se incluye 0                            
         IF v_aiv_ap_pat IS NULL THEN
            LET v_aiv_ap_pat = 0
         END IF
         
         --Si el valor de las amortizaciones del crédito son nulas se asigna 0
         IF v_imp_am_cre IS NULL THEN
            LET v_imp_am_cre = 0
         END IF
         
         --Si las aportaciones y amortizaciones son menores o
         --iguales a cero no se dispersa el registro
         IF (v_aiv_ap_pat <= 0.00 AND v_imp_am_cre <= 0.00) THEN
            CONTINUE FOREACH
         END IF
         --Temporalmente se envían 0's a los valores que aún no hay definición
         LET v_num_credito          = 0
         LET v_tpo_patron           = '00'
         LET v_localiza_trabajador  = '0'
   
                   --NO ESTA LIQUIDADO se crea registro de Pago a entidades financieras
                    	
                       LET l_s_qryTxt=" EXECUTE PROCEDURE sp_dis_esp_Transaccion2(?,\n",
		         				                                                       "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",                                
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
                                                                                  "?,\n",
		         				                                                       "?,\n",
		         				                                                       "?)"                             
		         		 --Preparación del statement de transacción
		         
		         		 PREPARE stm_sp_dis_transaccion2 FROM l_s_qryTxt
		         		 EXECUTE stm_sp_dis_transaccion2                        
                      INTO v_bnd_transaccion, v_status_err, error_info USING v_derechohabiente_pag,--Se obtiene de la tabla afi_derechohabiente
                                                                             v_imp_ap_pat,
                                                                             v_imp_am_cre, --Se toma el valor del v_imp_am_cre
                                                                             g_folio,
                                                                             v_id_referencia,--v_id_referencia-->Verificar que la referencia no sea necesaria
                                                                             v_precio_fec_hoy_11,
                                                                             v_precio_fec_hoy_10,
                                                                             v_folio_sua,
                                                                             v_periodo_bimestre,
                                                                             g_proceso_cod,
                                                                             v_tpo_patron,--v_tpo_patron se envia 00 en tanto se conoce de donde obtenerse
                                                                             v_f_pago,
                                                                             v_nrp,--Se obtiene de la 
                                                                             v_num_credito,--v_num_credito-->Se envia 0 en tanto se conoce de donde tomarse
                                                                             v_localiza_trabajador,--v_localiza_patron se envia 1 para que se tome subcuenta 4, en tanto se conoce su procedencia
                                                                             v_aiv_ap_pat --Revisar valor a aplicar

     
     
     END FOREACH   
     

     -- Actualiza el estado del archivo procesado
     CALL fn_act_edo_archivo(l_arch_proceso,g_folio,2,v_usurio)
     RETURNING r_bnd_edo_act_archivo
      
     -- Función para finalizar la operacion
     CALL fn_actualiza_opera_fin(l_pid,g_proceso_cod,g_opera_cod)
     RETURNING r_b_valida

     --Si la operacin no se finaliza, envia mensaje de error
     IF r_b_valida <> 0 THEN
        CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
        RETURNING r_bnd_oera_error
     ELSE
        CALL fn_genera_reporte_aportaciones(g_folio) 
        RETURNING r_ruta_reporte

        --CALL fn_genera_reporte_dup_apo()
        --RETURNING r_ruta_reporte
     END IF

  ELSE
     -- Función para finalizar la operación en error
     CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
     RETURNING r_bnd_oera_error

     -- Actualiza el estado del archivo procesado
     CALL fn_act_edo_archivo(l_arch_proceso,g_folio,2,v_usurio)
     RETURNING r_bnd_edo_act_archivo

     CALL fn_estado_rechazo(r_edo_rech)
  END IF
   
END MAIN

#Objetivo: Valida registros de detalle vs registros de sumario(cifras control)
FUNCTION fn_valida_aportaciones(v_folio)
  DEFINE 
    v_qrytxt                 STRING,
    v_folio                  DECIMAL(9,0),
    v_reg_det                DECIMAL(10,0),
    v_reg_sum                DECIMAL(10,0),
    v_sum_reg                DECIMAL(10,0)
       
  DEFINE 
    v_det_amo                DECIMAL(22,2),
    v_det_apo                DECIMAL(22,2),
    v_sum_amo                DECIMAL(22,2),
    v_sum_apo                DECIMAL(22,2),
    v_det_aiv                DECIMAL(26,6),
    v_sum_aiv                DECIMAL(26,6),
    v_amo_dif                DECIMAL(26,6),
    v_apo_dif                DECIMAL(26,6),
    v_edo_rech               SMALLINT

  LET v_det_apo  = 0.00
  LET v_det_amo  = 0.00
  LET v_sum_apo  = 0.00
  LET v_sum_amo  = 0.00
  LET v_apo_dif  = 0.00
  LET v_det_aiv  = 0.00
  LET v_sum_aiv  = 0.00
  LET v_amo_dif  = 0.00
  LET v_reg_det  = 0.00
  LET v_reg_sum  = 0.00
  LET v_sum_reg  = 0.00
  LET v_edo_rech = 10 -- Sin rechazo
   
  -- Valida total de registros de detalle vs no. registros de sumario
  LET v_qrytxt = "\n SELECT COUNT(*)",
                 "\n FROM safre_tmp:tmp_dis_aposubs_e2"
                  
  PREPARE prp_sql_total_registros FROM v_qrytxt
  EXECUTE prp_sql_total_registros INTO v_reg_det
   
  LET v_qrytxt = " SELECT tot_registros",
                 " FROM safre_tmp:tmp_dis_aposubs_e9"
                  
  PREPARE prp_sql_registros_sumario FROM v_qrytxt
  EXECUTE prp_sql_registros_sumario INTO v_reg_sum

  LET v_sum_reg = v_reg_det - v_reg_sum
  
  IF v_sum_reg <> 0 THEN
     LET v_edo_rech = 20 -- Rechazo por diferencia en numero de registros
     
     RETURN v_edo_rech
  END IF 

  -- Valida total de aportaciones de detalle vs sumarion
  LET v_qrytxt = "\n SELECT SUM(aplic_int_viv_apo_pat), SUM(imp_amo_cred)",
                 "\n FROM safre_tmp:tmp_dis_aposubs_e2"
  PREPARE prp_sql_det_sumas FROM v_qrytxt
  EXECUTE prp_sql_det_sumas INTO v_det_aiv,v_det_amo

  -- Valida total de aportaciones de detalle vs sumarion
  LET v_qrytxt = "\n SELECT tot_aplic_int_viv_apo, tot_imp_amo_cred",
                 "\n FROM safre_tmp:tmp_dis_aposubs_e9"
  PREPARE prp_sql_sum_sumas FROM v_qrytxt
  EXECUTE prp_sql_sum_sumas INTO v_sum_aiv,v_sum_amo

  LET v_apo_dif = v_det_aiv - v_sum_aiv

  --LET v_amo_dif = v_det_amo - v_sum_amo

  IF v_apo_dif <> 0 THEN
     LET v_edo_rech = 21 -- Rechazo por diferencia en aportaciones
     
     RETURN v_edo_rech
  END IF

  RETURN v_edo_rech
END FUNCTION

#Objetivo: Función que identifica duplicidad de aportaciones subsecuentes especiales #                
FUNCTION fn_dis_apo_esp_subs4(v_edo_rech)

DEFINE v_edo_rech SMALLINT,
       v_id_derechohabiente  DECIMAL(9,0),
       v_nss                 CHAR(11),
       v_folio_sua           DECIMAL(6,0),
       v_periodo_pago        CHAR(6),
       v_f_pago              DATE,
       v_reg_pat_imss        CHAR(11),
       v_tot_apo_subs        INTEGER,
       v_qry_tmp             STRING
       
DEFINE ef_id_derechohabiente DECIMAL(9,0),
       ef_nss                CHAR(11),
       ef_folio_sua          DECIMAL(6,0),
       ef_periodo_pago       CHAR(6),
       ef_f_pago             DATE,
       ef_nrp                CHAR(11),
       ef_ind_liquidacion    SMALLINT,
       ef_tot_apo_subs       INTEGER,
       ef_id_dis_interface   DECIMAL(9,0)

DEFINE v_char                CHAR(70),
       v_bnd_proceso         SMALLINT       --Estatus del proceso
       

  --Inicialia variables
  LET v_bnd_proceso         = 0 --Estado correcto
  LET v_id_derechohabiente  = 0
  LET v_nss                 = ""
  LET v_folio_sua           = 0
  LET v_periodo_pago        = ""
  LET v_f_pago              = ""
  LET v_reg_pat_imss        = ""
  LET v_tot_apo_subs        = 0
  LET ef_id_derechohabiente = 0
  LET ef_nss                = ""
  LET ef_folio_sua          = 0
  LET ef_periodo_pago       = ""
  LET ef_f_pago             = ""
  LET ef_nrp                = ""
  LET ef_ind_liquidacion    = 0
  LET ef_tot_apo_subs       = 0
  LET ef_id_dis_interface   = 0

  LET v_qry_tmp="DROP TABLE IF EXISTS dis_dup_ap_esp_subsecuente;"
  PREPARE  stm_dup_ap_esp FROM v_qry_tmp
  EXECUTE  stm_dup_ap_esp
  
  LET v_qry_tmp="CREATE TABLE dis_dup_ap_esp_subsecuente\n",
                              "(id_derechohabiente DECIMAL(9,0) NOT NULL ,\n",
                               "nss                CHAR(11),\n     ",
                               "folio_sua          DECIMAL(6,0),\n ",
                               "periodo_pago       CHAR(6),\n      ",
                               "f_pago             DATE,\n         ",
                               "reg_pat_imss       CHAR(11),\n     ",
                               "ind_liquidacion    SMALLINT,\n     ",
                               "tpo_registro       INTEGER,\n      ",
                               "tot_apo_subs       INTEGER)\n      ",
                  "FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs"
   PREPARE stm_crt_dis_dupapesp FROM v_qry_tmp               
   EXECUTE  stm_crt_dis_dupapesp;
   
   --Prepara query de duplicidad
   
   LET v_qry_tmp=" SELECT nss, \n           ",
                         " folio_sua,\n     ",
                         " periodo_pago,\n  ",
                         " f_pago, \n       ",
                         " reg_pat_imss, \n ",
                         " COUNT(*)\n       ",
                    "FROM   safre_tmp:tmp_dis_aposubs_e2\n",
                    "GROUP BY 1,2,3,4,5\n               ",
                    "HAVING COUNT(*) > 1"          
   PREPARE stm_dup_esp FROM  v_qry_tmp
   DECLARE cur_dup_esp CURSOR FOR stm_dup_esp 
   	
  --Obtiene duplicidad del archivo
  FOREACH   cur_dup_esp  INTO   v_nss,
                                v_folio_sua,
                                v_periodo_pago,
                                v_f_pago,
                                v_reg_pat_imss,
                                v_tot_apo_subs 

       SELECT id_derechohabiente
       INTO   v_id_derechohabiente
       FROM   afi_derechohabiente
       WHERE  nss = v_nss;

       INSERT INTO dis_dup_ap_esp_subsecuente
       VALUES (v_id_derechohabiente,
               v_nss,
               v_folio_sua,
               v_periodo_pago,
               v_f_pago,
               v_reg_pat_imss,
               "",
               1,
               v_tot_apo_subs);

  END FOREACH

  --Obtiene duplicidad de la tabla de Entidades Financieras
  --Preparación de la consulta de duplicados
  LET v_qry_tmp=   "SELECT id_derechohabiente,\n    ",
                   "       folio_sua,\n             ",
                   "       periodo_pago,\n          ",
                   "       f_pago,\n                ",
                   "       nrp,\n                   ",
                   "       ind_liquidacion,\n       ",
                   "       COUNT(*)\n               ",
                   "FROM   dis_interface_ef\n       ",
                   "WHERE  ind_liquidacion = 0\n    ",
                   "GROUP BY 1,2,3,4,5,6\n          ",
                   "HAVING COUNT(*) > 1          "
  --Preparación del statement para la obtención del duplicado de derechohabientes
  PREPARE stm_dis_esp_entf FROM v_qry_tmp
  DECLARE cur_dis_esp_entf CURSOR FOR stm_dis_esp_entf
                
  --Recorrido del cursor
  FOREACH cur_dis_esp_entf  INTO   ef_id_derechohabiente, 
                                   ef_folio_sua,
                                   ef_periodo_pago,
                                   ef_f_pago,
                                   ef_nrp,
                                   ef_ind_liquidacion,
                                   ef_tot_apo_subs
   
       SELECT nss
       INTO   ef_nss
       FROM   afi_derechohabiente
       WHERE  id_derechohabiente = ef_id_derechohabiente

       INSERT INTO dis_dup_ap_esp_subsecuente
       VALUES (ef_id_derechohabiente,
               ef_nss,
               ef_folio_sua,
               ef_periodo_pago,
               ef_f_pago,
               ef_nrp,
               ef_ind_liquidacion,
               2,
               ef_tot_apo_subs)
    --END IF
  END FOREACH 

  UPDATE STATISTICS FOR TABLE dis_dup_ap_esp_subsecuente;

  LET ef_id_derechohabiente = 0
  LET ef_nss                = ""
  LET ef_folio_sua          = 0
  LET ef_periodo_pago       = "" 
  LET ef_f_pago             = ""
  LET ef_nrp                = ""
  LET ef_ind_liquidacion    = 0

  --Actualiza indicador registros duplicados Entidades Financieras
  --Preparación del query
  
  LET v_qry_tmp="  SELECT id_derechohabiente,\n        ",
                   "       folio_sua,\n                ",
                   "       periodo_pago,\n             ",
                   "       f_pago,\n                   ",
                   "       reg_pat_imss,\n             ",
                   "       ind_liquidacion\n           ",
                   "FROM   dis_dup_ap_esp_subsecuente\n",
                   "WHERE  tpo_registro = 2"
                   
 --Preparación del statement
 PREPARE stm_dis_esp_act FROM v_qry_tmp
 DECLARE cur_dis_esp_act CURSOR FOR  stm_dis_esp_act
  
  FOREACH cur_dis_esp_act INTO  ef_id_derechohabiente,
                                ef_folio_sua,
                                ef_periodo_pago,
                                ef_f_pago,
                                ef_nrp,
                                ef_ind_liquidacion

       LET v_qry_tmp="SELECT FIRST 1 (id_dis_interface_ef)\n",
                     "FROM   dis_interface_ef\n",
                     "WHERE  id_derechohabiente = ?\n",
                     "AND    folio_sua          = ?\n",
                     "AND    periodo_pago       = ?\n",
                     "AND    f_pago             = ?\n",
                     "AND    nrp                = ?\n",
                     "AND    ind_liquidacion    = ?\n"
       
       PREPARE stm_dis_interface_first FROM v_qry_tmp
       EXECUTE stm_dis_interface_first INTO ef_id_dis_interface USING ef_id_derechohabiente, 
                                                                      ef_folio_sua,
                                                                      ef_periodo_pago,  
                                                                      ef_f_pago,        
                                                                      ef_nrp,           
                                                                      ef_ind_liquidacion

       UPDATE dis_interface_ef                                        
       SET    ind_liquidacion      = 100                               
       WHERE  id_derechohabiente   = ef_id_derechohabiente            
       AND    folio_sua            = ef_folio_sua
       AND    periodo_pago         = ef_periodo_pago
       AND    f_pago               = ef_f_pago
       AND    nrp                  = ef_nrp
       AND    ind_liquidacion      = ef_ind_liquidacion
       AND    id_dis_interface_ef <> ef_id_dis_interface;
    --END IF
  END FOREACH;

  LET v_char = "Terminado fn_dis_apo_esp_subs4"
  RETURN v_bnd_proceso , 0 , v_char;

END FUNCTION

#Objetivo. Función que  valida totales detalle contra sumario  #
FUNCTION fn_dis_apo_esp_subs2(v_edo_rech)

DEFINE v_edo_rech               SMALLINT,
       v_folio_liquida 	        DECIMAL(9,0),
       v_id_interface_ef        DECIMAL(9,0),
       v_id_derechohabiente     DECIMAL(9,0),
       v_reg_pag_imss 	        CHAR(11),
       v_rfc_pat 	              CHAR(13),
       v_periodo_pago 	        CHAR(6),
       v_folio_sua 	           DECIMAL(6,0),
       v_f_pago 	              DATE,
       v_f_valor 	              DATE,
       v_nss 		              CHAR(11),
       v_rfc 		              CHAR(13),
       v_curp 		              CHAR(18),
       v_nombre 	              CHAR(50),
       v_usdi_periodo 	        DECIMAL(12,2),
       v_dias_cotizados_bim     INTEGER,
       v_dias_incapacidad_bim   INTEGER,
       v_dias_ausentismo_bim    INTEGER,
       v_imp_apo_pat 	        DECIMAL(12,2),
       v_imp_amo_cred 	        DECIMAL(12,2),
       v_dif_apo_sub 	        DECIMAL(12,2),
       v_afore 		           SMALLINT,
       v_f_liquidacion 	        DATE,
       v_estado 	              SMALLINT,
       v_bnd_transaccion        SMALLINT,
       v_bnd_proceso            SMALLINT,       --Estatus del proceso
       v_char                   CHAR(20),
       v_periodo_bimestre       CHAR(6),        --Período de Pago Bimestre
       v_aplic_int_viv_apo_pat  DECIMAL(15,6),
       v_val_aplic_int_viv      DECIMAL(15,6),
       v_sql_tmp                STRING


   --Inicialización de variables
   LET v_bnd_proceso            = 0 --Estado correcto
   LET v_folio_liquida          = 0
   LET v_id_interface_ef        = 0.00
   LET v_id_derechohabiente     = 0
   LET v_reg_pag_imss           = ""
   LET v_rfc_pat                = ""
   LET v_periodo_pago           = ""
   LET v_folio_sua              = 0
   LET v_f_pago                 = ""
   LET v_f_valor                = ""
   LET v_nss                    = ""
   LET v_rfc                    = ""
   LET v_curp                   = ""
   LET v_nombre                 = ""
   LET v_usdi_periodo           = 0.00
   LET v_dias_cotizados_bim     = 0
   LET v_dias_incapacidad_bim   = 0
   LET v_dias_ausentismo_bim    = 0
   LET v_imp_apo_pat            = 0.00
   LET v_imp_amo_cred           = 0.00
   LET v_dif_apo_sub            = 0.00
   LET v_afore                  = 0
   LET v_f_liquidacion          = ""
   LET v_estado                 = 0
   LET v_bnd_transaccion        = 0
   LET v_periodo_bimestre       = ""
   LET v_aplic_int_viv_apo_pat  = 0.000000
   LET v_val_aplic_int_viv	     = 0.000000
   
   --Se crea la selección de la tabla tmp_dis_aposubs_e2
   LET v_sql_tmp="SELECT reg_pat_imss,\n           ",       
                       "rfc_pat,\n                       ",
                       "periodo_pago,\n                  ",
                       "folio_sua,\n                     ",
                       "f_pago,\n                        ",
                       "f_valor,\n                       ",
                       "nss,\n                           ",
                       "rfc,\n                           ",
                       "curp,\n                          ",
                       "nombre,\n                        ",
                       "usdi_periodo,\n                  ",
                       "dias_cotizados_bim,\n            ",
                       "dias_incapacidad_bim,\n          ",
                       "dias_ausentismo_bim,\n           ",
                       "imp_apo_pat / 100,\n             ",
                       "imp_amo_cred / 100,\n            ",
                       "aplic_int_viv_apo_pat /1000000, \n",
                       "val_aplic_int_viv / 1000000,   \n",
                       "afore,\n",                         
                       "f_liquidacion\n       ",
                 "FROM safre_tmp:tmp_dis_aposubs_e2"
   --Preparación del statement
   PREPARE stm_dis_apo_tmp FROM v_sql_tmp
   DECLARE cur_dis_apo_tmp CURSOR FOR stm_dis_apo_tmp
 
   FOREACH cur_dis_apo_tmp INTO v_reg_pag_imss,
                                v_rfc_pat,
                                v_periodo_pago,
                                v_folio_sua,
                                v_f_pago,
                                v_f_valor,
                                v_nss,
                                v_rfc,
                                v_curp,
                                v_nombre,
                                v_usdi_periodo,
                                v_dias_cotizados_bim,
                                v_dias_incapacidad_bim,
                                v_dias_ausentismo_bim,
                                v_imp_apo_pat,
                                v_imp_amo_cred,
	                             v_aplic_int_viv_apo_pat,
	                             v_val_aplic_int_viv,
                                v_afore,
                                v_f_liquidacion
                                

      LET v_sql_tmp="EXECUTE PROCEDURE fn_bimestre_pago(?)"
      PREPARE stm_fn_bimestre FROM v_sql_tmp
      EXECUTE stm_fn_bimestre INTO v_periodo_bimestre USING v_periodo_pago

     LET v_estado = 10
			
     --Obtenemos id_derechohabiente según número seguro socila
     SELECT id_derechohabiente
     INTO   v_id_derechohabiente
     FROM   afi_derechohabiente
     WHERE  nss = v_nss
			
     --Obtiene folio de liquidación
     SELECT folio_liquida,id_dis_interface_ef
     INTO   v_folio_liquida,v_id_interface_ef
     FROM   dis_interface_ef
     WHERE  id_derechohabiente = v_id_derechohabiente
     AND    folio_sua          = v_folio_sua
     AND    periodo_pago       = v_periodo_bimestre
     AND    f_pago             = v_f_pago
     AND    nrp                = v_reg_pag_imss
     AND    ind_liquidacion    = 0
           
     --Asigna id_derechohabiente si no se encuentra en tabla
     IF v_id_derechohabiente IS NULL THEN
        LET v_id_derechohabiente = "999999999"
        --LET v_folio_liquida = 0;
        --Rechazo por no existir en el maestro de derechohabientes
        LET v_estado = 23
     END IF

     --Valida que valor de aportacion --amortizacion sea mayor a cero
     IF v_imp_apo_pat           <= 0.00 OR 
        v_aplic_int_viv_apo_pat <= 0.00 THEN--AND v_imp_amo_cred <= 0.00 THEN
        --Rechazo por no incluir montos
        LET v_estado = 24
     END IF
   
     --TRACE 'Valida valor de periodo_pago no sea nulo o blanco';
     --Valida valor de periodo_pago no sea nulo o blanco
     IF v_periodo_bimestre IS NULL OR LENGTH(v_periodo_pago) = 0 THEN
        --Rechazo por no contar con el periodo de pago
        LET v_estado = 25
     END IF

     --TRACE 'Valida valor de Folio Liquida no sea nulo o blanco';
     --Valida valor de Liquida  no sea nulo o blanco
     IF v_folio_liquida IS NULL THEN
        LET v_folio_liquida = 0
     END IF

     --TRACE 'Valida valor de v_id_interface_ef no sea nulo o blanco';
     --Valida valor de v_id_interface_ef  no sea nulo o blanco
     IF v_id_interface_ef IS NULL THEN
        LET v_id_interface_ef = 0
     END IF         

   END FOREACH
   
   --UPDATE STATISTICS FOR TABLE dis_ap_subsecuente;
   
   LET v_char = "Terminado fn_dis_apo_esp_subs2";
   RETURN v_bnd_proceso , 0 , v_char;

END FUNCTION

#Objetivo: Genera reporte de cifras globales de aportaciones subsecuentes
FUNCTION fn_genera_reporte_aportaciones(p_folio)
  DEFINE 
    p_folio                  DECIMAL(10,0)  --Aportaciones
    
  DEFINE 
    v_sum_det_apo                 DECIMAL(12,2),--Suma aportacion detalle
    v_sum_det_acciones            DECIMAL(22,6),--Suma detalle en acciones
    v_sum_det_pesos               DECIMAL(18,2),--Suma detalle en pesos
    v_sum_aiv_pat                 DECIMAL(22,6),
    v_fec_pago               DATE,--Fecha de pago
    v_fec_proc               DATE, --Fecha de proceso
    v_rec_dif                INTEGER, --Numero rechazos con fiferencia
    v_sumdif_pag_aiv         DECIMAL(26,6), --AIVS pagadas
    v_sumdif_sub_aiv         DECIMAL(26,6), --AIVS subsecuente
    v_sum_dif_aiv            DECIMAL(26,6) --Suma de diferencias aivs
    

  DEFINE 
    v_desc_edo_arch          CHAR(50),
    v_fol_archivo            SMALLINT,
    r_edo_archivo            CHAR(50),
    v_reg_no_dif             INTEGER

  DEFINE v_origen_datos      STRING
  DEFINE v_ruta_reporte      STRING -- ruta del archivo del reporte
  DEFINE v_ruta_listados     STRING -- ruta de los listados 
  DEFINE v_ruta_ejecutable   STRING -- ruta del ejecutable
  --DEFINE v_estado_operacion  LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos para reporte 

  DEFINE arr_apo_sub_glob    DYNAMIC ARRAY OF RECORD 
    v_det_nss                CHAR(11),
    v_det_derechohabiente    DECIMAL(9,0),                                              
    v_folio_pago            DECIMAL(6,0),                                              
    v_det_periodo_pago       CHAR(6),                                                   
    v_det_f_pago             DATE,                                                      
    v_nrp                    CHAR(11),                                                  
    v_imp_ap_pat             DECIMAL(15,6),--Importe de aportaciones patronales             
    v_imp_aiv_ap_pat         DECIMAL(18,6),--Importe de AIV's de aportaciones patronales    
    v_subcuenta              SMALLINT,--Subcuenta obtenida de la cuenta movimiento          
    v_movimiento             SMALLINT,       --Movimiento de la cta_movimiento           
    v_monto_acciones         DECIMAL(16,6),--Importe en pesos de cta_movimiento                                
    v_monto_pesos            DECIMAL(12,6)--Movimiento en AIVS de cta_movimiento
   
  END RECORD
       
  DEFINE 
    v_indice_1               INTEGER,    
    v_ind_rpt                INTEGER --Indice para el reporte  
  

  LET v_fec_proc = TODAY

  --Obtiene el estado del archivo
  SELECT estado
  INTO   v_fol_archivo
  FROM   glo_ctr_archivo
  WHERE  proceso_cod = g_proceso_cod
  AND    folio       = p_folio 

  --Obtiene la descripcion del estado del archivo
  SELECT estado_descripcion
  INTO   v_desc_edo_arch
  FROM   cat_edo_archivo
  WHERE  estado_cod = v_fol_archivo

  --Concatena el estado y la descripcion del archivo
  LET r_edo_archivo = v_fol_archivo || '-' || v_desc_edo_arch CLIPPED
  
  --Obtiene la fecha de pago
  LET g_qrytxt="SELECT FIRST 1 f_pago\n",
                "FROM dis_interface_ef\n",
                "WHERE folio_liquida=",p_folio
                
  PREPARE stm_fpago FROM g_qrytxt
  EXECUTE stm_fpago INTO v_fec_pago

  --Obtiene la cantidad de registros
  LET g_qrytxt="SELECT COUNT(*)\n",
               "FROM   dis_interface_ef     \n",
               "WHERE folio_liquida=",p_folio
  
  PREPARE stm_tot_leidos FROM  g_qrytxt
  EXECUTE stm_tot_leidos INTO v_tot_leidos
  --Despliega información en el log
  DISPLAY "\n ############### INTEGRACIÓN APORTACIONES SUBSECUENTES ESPECIALES ###############"

 
  DISPLAY " Total de registros en archivo             : ", v_tot_leidos 

  DISPLAY " Nombre del archivo   : ", l_arch_proceso CLIPPED 
   
   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,p_folio,Sin INTO v_tbl_mov --por folio

  LET g_qrytxt = " SELECT af.nss, \n",
                 "        ef.id_derechohabiente, \n",
                 "        ef.folio_liquida,          \n",
                 "        ef.periodo_pago,       \n",
                 "        ef.f_pago,             \n",
                 "        ef.nrp,        \n",
                 "        ef.imp_ap_pat,     \n",
                 "        ef.aiv_ap_pat,   \n",
                 "        cm.subcuenta,    \n",
                 "        cm.movimiento,   \n",
                 "        cm.monto_acciones,  \n",
                 "        cm.monto_pesos   \n",
                 "  FROM  ",v_tbl_mov," cm, \n",
                 "         (dis_interface_ef ef LEFT OUTER JOIN \n",
                 "           afi_derechohabiente af\n",
                 "          ON     af.id_derechohabiente=ef.id_derechohabiente)\n",
                 "  WHERE ef.folio_liquida =",p_folio," \n",
                 "  AND    cm.folio_liquida=  ef.folio_liquida  \n",
                 "   AND    cm.id_referencia=ef.id_dis_interface_ef"
                 
    PREPARE prp_consulta_detalle FROM g_qrytxt

  LET v_origen_datos = v_usurio

  -- se construye la ruta del archivo
  CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(),"-",
                       "DISE12","-",
                       l_pid USING "&&&&&","-",
                       g_proceso_cod USING "&&&&&","-",
                       g_opera_cod USING "&&&&&",".pdf"                         

  DISPLAY "Ruta del reporte: ",v_ruta_reporte
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE121.4rp") THEN

     CALL fgl_report_selectDevice ("PDF")        
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()

     ### Inicia Reporte ###
     --Inicializamos variables para suma de totales
     LET v_sumdif_sub_aiv = 0.00
     LET v_sumdif_pag_aiv = 0.00
     LET v_sum_dif_aiv    = 0.00
     LET v_rec_dif        = 0
     LET v_reg_no_dif     = 0
     LET v_indice_1       = 1
     LET v_sum_det_apo    = 0
     LET v_sum_det_acciones = 0.0
     LET v_sum_det_pesos    = 0.0
     LET v_sum_aiv_pat      = 0.0

            
  
     DECLARE cur_diferencias_montos CURSOR FOR prp_consulta_detalle
     FOREACH cur_diferencias_montos INTO arr_apo_sub_glob[v_indice_1].*	
     	
     	 --Valida si cuenta con NSS
     	 IF arr_apo_sub_glob[v_indice_1].v_det_nss IS NULL 
     	 	 OR arr_apo_sub_glob[v_indice_1].v_det_nss='0' THEN
     	 	 	--Asigna valor del NSS
     	 	 	LET arr_apo_sub_glob[v_indice_1].v_det_nss="ND"
     	 	 	
     	 END IF
   
       --Suma importe en pesos
        IF arr_apo_sub_glob[v_indice_1].v_monto_pesos IS NOT NULL THEN
        	
        	--Solo se suma para la subcuenta 4
        	IF arr_apo_sub_glob[v_indice_1].v_subcuenta= 4 THEN 
       	   LET v_sum_det_pesos=v_sum_det_pesos + arr_apo_sub_glob[v_indice_1].v_monto_pesos
         END IF
         
       END IF 
       
       
       --Suma importe en acciones
        IF arr_apo_sub_glob[v_indice_1].v_monto_acciones IS NOT NULL THEN
        	
        	--Solo se suma para la subcuenta 4
        	IF arr_apo_sub_glob[v_indice_1].v_subcuenta = 4 THEN
       	  LET v_sum_det_acciones=v_sum_det_acciones + arr_apo_sub_glob[v_indice_1].v_monto_acciones
         END IF
       END IF 
       
      

       LET v_indice_1 = v_indice_1  + 1
                  
     END FOREACH

     
     --Se obtiene la suma de las aportaciones y aivs de dis_interface_ef
     
     LET g_qrytxt="SELECT SUM(imp_ap_pat),SUM(aiv_ap_pat)\n",
                  "FROM dis_interface_ef\n",
                  "WHERE folio_liquida=",p_folio
     PREPARE stm_tot_intef FROM g_qrytxt
     EXECUTE stm_tot_intef INTO v_sum_det_apo,v_sum_aiv_pat
     
     
     
     CALL  arr_apo_sub_glob.deleteElement(v_indice_1)         

     LET v_indice_1 = v_indice_1 - 1

            IF v_indice_1 = 0 THEN 
            	--No se encontraron registros
               DISPLAY "No se puede generar el reporte por falta de información."
             
             ELSE
                 IF v_usurio IS NULL THEN 
                    LET v_usurio = "infonavit"
                 END IF 
                
                  --Inicia el reporte
                 START REPORT rp_apo_sub_esp_glob TO XML HANDLER manejador_rpt
                 
                 FOR v_ind_rpt = 1 TO v_indice_1
                 	 
                     OUTPUT TO REPORT rp_apo_sub_esp_glob(g_folio,
                                                          v_usurio,
                                                          v_fec_proc,
                                                          v_sum_det_apo,
                                                          v_sum_aiv_pat,
                                                          v_sum_det_pesos,
                                                          v_sum_det_acciones,
                                                          v_tot_leidos,
                                                          v_fec_pago,
                                                          arr_apo_sub_glob[v_ind_rpt].*,
                                                          r_edo_archivo)
                                                      
                 END FOR 
                 
                 FINISH REPORT rp_apo_sub_esp_glob 
          END IF 
  ELSE
     DISPLAY "no funciono"
     EXIT PROGRAM
  END IF

  RETURN v_ruta_reporte
END FUNCTION

#Objetivo:Estado del rechazo del archivo
FUNCTION fn_estado_rechazo(p_edo_rech)
  DEFINE 
    p_edo_rech SMALLINT

  CASE
    WHEN p_edo_rech = 20
      DISPLAY "Estado del rechazo "||p_edo_rech||
              ". Rechazo por diferencia en número de registros" 
    WHEN p_edo_rech = 21
      DISPLAY "Estado del rechazo "||p_edo_rech||
              ". Rechazo por diferencia en aportaciones" 
  END CASE

END FUNCTION

#OBJETIVO: Generar el reporte de Aportaciones Subsecuentes
REPORT rp_apo_sub_esp_glob(v_rfolio,
                       v_rusurio,
                       v_rfec_proc,         
                       v_rsum_det_apo,      
                       v_rsum_det_aiv_pat,  
                       v_rsum_det_pesos,    
                       v_rsum_det_acciones, 
                       v_rsum_tot_reg,
                       v_rsum_f_transf,
                       arr_apo_sub_glob,
                       r_redo_archivo
                      )              
                       
  DEFINE 
    v_rusurio                VARCHAR(30),--Usuario de proceso
    v_rfec_proc              DATE,--Fecha de procesos
    v_rfolio                 DECIMAL(9,0),
    v_rsum_f_transf          DATE,
    v_rsum_det_aiv_pat       DECIMAL(22,6),
    v_rsum_det_pesos         DECIMAL(18,2),
    v_rsum_det_acciones      DECIMAL(22,6),
    v_rsum_det_apo           DECIMAL(12,2),
    v_rsum_tot_reg           SMALLINT
    

  DEFINE r_redo_archivo      VARCHAR(50)--Descripción del estado del archivo

  DEFINE arr_apo_sub_glob    RECORD 
    v_det_nss                CHAR(11),
    v_det_derechohabiente    DECIMAL(9,0),                                              
    v_folio_pago             DECIMAL(6,0),                                              
    v_det_periodo_pago       CHAR(6),                                                   
    v_det_f_pago             DATE,                                                      
    v_nrp                    CHAR(11),                                                  
    v_imp_ap_pat             DECIMAL(15,6),--Importe de aportaciones patronales             
    v_imp_aiv_ap_pat         DECIMAL(18,6),--Importe de AIV's de aportaciones patronales    
    v_subcuenta              SMALLINT,--Subcuenta obtenida de la cuenta movimiento          
    v_movimiento             SMALLINT,       --Movimiento de la cta_movimiento           
    v_monto_acciones         DECIMAL(16,6),--Importe en pesos de cta_movimiento                                
    v_monto_pesos            DECIMAL(12,6)--Movimiento en AIVS de cta_movimiento  
  END RECORD

  FORMAT
    FIRST PAGE HEADER

       
      PRINTX v_rfolio
      PRINTX v_rusurio
      PRINTX v_rfec_proc     USING "dd-mm-yyyy" 
      PRINTX v_rsum_f_transf USING "dd-mm-yyyy" 
      PRINTX v_rsum_det_apo   
      PRINTX v_rsum_det_aiv_pat
      PRINTX v_rsum_det_pesos  
      PRINTX v_rsum_det_acciones 
      PRINTX v_rsum_tot_reg
      PRINTX r_redo_archivo
      PRINTX l_arch_proceso


    ON EVERY ROW
       
       PRINTX arr_apo_sub_glob.v_det_nss
       PRINTX arr_apo_sub_glob.v_folio_pago
       PRINTX arr_apo_sub_glob.v_det_f_pago USING "dd-mm-yyyy"
       PRINTX arr_apo_sub_glob.v_det_periodo_pago
       PRINTX arr_apo_sub_glob.v_imp_ap_pat    
       PRINTX arr_apo_sub_glob.v_imp_aiv_ap_pat
       PRINTX arr_apo_sub_glob.v_subcuenta     
       PRINTX arr_apo_sub_glob.v_movimiento    
       PRINTX arr_apo_sub_glob.v_monto_acciones
       PRINTX arr_apo_sub_glob.v_monto_pesos   

END REPORT
