################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 17/11/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE23                                                   #
#Objetivo          => Programa para integrar el archivo de aportaciones        #                   
#                     subsecuentes (Sin Adelanto)                              #
#Fecha inicio      => 17/11/2015                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS
  DEFINE 
    v_usurio                 VARCHAR(30),                  --almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo de proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    g_folio                  LIKE dis_det_avance_pago.folio,--folio generdo
    l_pid                    LIKE glo_pid.pid,
    g_qrytxt                 STRING                        --prepara consultas

  DEFINE
    v_tot_leidos             DECIMAL(10,0),
    v_tot_ceros              DECIMAL(10,0),
    v_tot_no_enc             DECIMAL(10,0),
    {v_tot_si_enc             DECIMAL(10,0),
    v_tot_monto_igual        DECIMAL(10,0),}
    v_tot_monto_dif          DECIMAL(10,0),
    v_tot_dup_ap             DECIMAL(10,0),
    v_tot_dup_cargo_ap       DECIMAL(10,0)
    
  DEFINE
    l_arch_proceso           VARCHAR(100)
      
END GLOBALS

-- Objetivo: Funcion que realiza la carga de tablas hitoricas de avance de pago
MAIN
  DEFINE 
    l_s_qryTxt               STRING, --guarda una sentencia SQL a ejecutar
    r_b_valida               SMALLINT,
    r_bnd_edo_act_archivo    SMALLINT,
    r_ruta_reporte           STRING, --ruta del archivo del reporte
    r_edo_rech               SMALLINT,
    r_bnd_oera_error         SMALLINT,
    p_transaccion            SMALLINT, --bandera que indica si la ejecución es manual o automática

    r_bnd                    INTEGER, 
    v_status_err             INTEGER ,
    v_desc_err               VARCHAR(200)

  LET v_usurio       = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)

  LET p_transaccion  = 0

  -- Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
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
   
  -- Si el estado del rechazo es 10, estatus correcto
  IF r_edo_rech = 10 THEN
     WHENEVER ERROR CONTINUE 
       -- Inserta datos de control del proceso de aportaciones subsecuentes
       PREPARE prp_sp_apo_sub FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs1(?,?)"
       EXECUTE prp_sp_apo_sub USING g_folio, r_edo_rech 
       INTO r_bnd, v_status_err, v_desc_err
     WHENEVER ERROR STOP 

     IF r_bnd <> 0 THEN
        DISPLAY "Error1: ", v_status_err, " - ", v_desc_err, " - ", r_bnd

        CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
        RETURNING r_bnd_oera_error

        EXIT PROGRAM 
     END IF

     WHENEVER ERROR CONTINUE
       -- Identifica duplicidad de registro de aportaciones subsecuentes
       PREPARE prp_sp_apo_sub6 FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs6(?,?)"
       EXECUTE prp_sp_apo_sub6 USING g_folio, r_edo_rech
       INTO r_bnd, v_status_err, v_desc_err
     WHENEVER ERROR STOP
    
     WHENEVER ERROR CONTINUE 
       -- Inserta detalles del proceso de aportaciones subsecuentes
       PREPARE prp_sp_apo_sub1 FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs7(?,?)"
       EXECUTE prp_sp_apo_sub1 USING g_folio, r_edo_rech
 
       INTO r_bnd, v_status_err, v_desc_err
     WHENEVER ERROR STOP 
      
     IF r_bnd <> 0 THEN
        DISPLAY "Error2: ",v_status_err," - ",v_desc_err," - ",r_bnd
         
        -- Actualiza el estado del archivo procesado
        CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usurio)
        RETURNING r_bnd_edo_act_archivo
         
        -- Función para finalizar la operación en error
        CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
        RETURNING r_bnd_oera_error

        EXIT PROGRAM 
     END IF

     -- Actualiza el estado del archivo procesado
     CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usurio)
     RETURNING r_bnd_edo_act_archivo
      
     -- Función para finalizar la operacion
     CALL fn_actualiza_opera_fin(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_b_valida

     -- Si la operacin no se finaliza, envia mensaje de error
     IF r_b_valida <> 0 THEN
        CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
        RETURNING r_bnd_oera_error
     ELSE
        CALL fn_genera_reporte_aportaciones(g_folio) 
        RETURNING r_ruta_reporte

        --CALL fn_genera_reporte_dup_apo()
        --RETURNING r_ruta_reporte
     END IF
  ELSE
     -- Función para finalizar la operación en error
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_oera_error

     -- Actualiza el estado del archivo procesado
     CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usurio)
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
    v_edo_rech               SMALLINT,
    r_bnd                    SMALLINT, 
    v_status_err             SMALLINT ,
    v_desc_err               VARCHAR(200)

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
                 "\n FROM   safre_tmp:tmp_dis_aposubs2"
  PREPARE prp_sql_total_registros FROM v_qrytxt
  EXECUTE prp_sql_total_registros INTO v_reg_det
   
  LET v_qrytxt = " SELECT tot_registros",
                 " FROM   safre_tmp:tmp_dis_aposubs9"
  PREPARE prp_sql_registros_sumario FROM v_qrytxt
  EXECUTE prp_sql_registros_sumario INTO v_reg_sum

  LET v_sum_reg = v_reg_det - v_reg_sum
  
  IF v_sum_reg <> 0 THEN
     LET v_edo_rech = 20 -- Rechazo por diferencia en numero de registros
     PREPARE prp_sp_apo_sub_tot FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs1(?,?)"
     EXECUTE prp_sp_apo_sub_tot USING g_folio, v_edo_rech
        INTO r_bnd, v_status_err, v_desc_err

     IF r_bnd <> 0 THEN
        DISPLAY "Error-S1: ", v_status_err, " ", v_desc_err
     END IF
      
     RETURN v_edo_rech
  END IF 

  -- Valida total de aportaciones de detalle vs sumario
  LET v_qrytxt = "\n SELECT SUM(aplic_int_viv_apo_pat), SUM(imp_amo_cred)",
                 "\n FROM   safre_tmp:tmp_dis_aposubs2"
  PREPARE prp_sql_det_sumas FROM v_qrytxt
  EXECUTE prp_sql_det_sumas INTO v_det_aiv, v_det_amo

  -- Valida total de aportaciones de detalle vs sumario
  LET v_qrytxt = "\n SELECT tot_aplic_int_viv_apo, tot_imp_amo_cred",
                 "\n FROM   safre_tmp:tmp_dis_aposubs9"
  PREPARE prp_sql_sum_sumas FROM v_qrytxt
  EXECUTE prp_sql_sum_sumas INTO v_sum_aiv, v_sum_amo

  IF v_sum_apo IS NULL THEN 
    LET v_sum_apo = 0.00
  END IF

  LET v_apo_dif = v_det_aiv - v_sum_aiv
  --LET v_amo_dif = v_det_amo - v_sum_amo

  IF v_apo_dif <> 0 THEN
     LET v_edo_rech = 21 -- Rechazo por diferencia en aportaciones
     PREPARE prp_sp_apo_sub_apo FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs1(?,?)"
     EXECUTE prp_sp_apo_sub_apo USING g_folio, v_edo_rech
        INTO r_bnd, v_status_err, v_desc_err 

     IF r_bnd <> 0 THEN
        DISPLAY "Error-S21: ", v_status_err, " ", v_desc_err
     END IF
      
     RETURN v_edo_rech
  END IF

  RETURN v_edo_rech
END FUNCTION

#Objetivo: Genera reporte de cifras globales de aportaciones subsecuentes
FUNCTION fn_genera_reporte_aportaciones(p_folio)
  DEFINE 
    p_folio                  DECIMAL(10,0)  --aportaciones
    
  DEFINE 
    v_sum_det_apo            DECIMAL(12,2), --suma aportacion detalle
    v_sum_det_amo            DECIMAL(12,2), --suma amortizacion detalle
    v_sum_det_aiv            DECIMAL(15,6), --Suma aivs detalle
    v_sum_apo                DECIMAL(17,2), --tot aport sumario
    v_sum_amo                DECIMAL(17,2), --tot amort sumario
    v_sum_aiv                DECIMAL(20,6), --tot aivs  sumario
    v_sum_tot_reg            DECIMAL(10,0), --tot registros sumario
    v_sum_f_transf           DATE,          --fech transferencai
    v_sum_estado             SMALLINT,      --estado del archivo
    v_folio_lq               DECIMAL(9,0),  --Folio LQ                     
    v_det_nss                CHAR(11),      --NSS trabajador
    v_det_derechohabiente    DECIMAL(9,0),  --id_derechohab
    v_folio_sua              DECIMAL(6,0),  --folio SUA
    v_det_periodo_pago       CHAR(6),       --perido de pago
    v_det_f_pago             DATE,          --fecha de pago
    v_nrp                    CHAR(11),      --NRP
    --v_dif_apo                DECIMAL(26,6),  --diferencia aportacion
    --v_edo_desc               CHAR(50),       --desc edo rechazo
    --v_edo_desc_ov            CHAR(50),       --desc edo rechazo
    v_fec_proc               DATE,          --fecha de proceso
    v_rec_dif                INTEGER,       --numero rechazos con fiferencia
    --v_sumdif_pag_apo         DECIMAL(12,2), --aportaciones pagadas
    --v_sumdif_sub_apo         DECIMAL(12,2), --aportacion subsecuente
    --v_sum_dif                DECIMAL(12,2), --suma de diferencias
    v_sumdif_pag_aiv         DECIMAL(26,6), --AIVS pagadas
    v_sumdif_sub_aiv         DECIMAL(26,6), --AIVS subsecuente
    v_sum_dif_aiv            DECIMAL(26,6)  --suma de diferencias aivs
    --p_b_despliegue_pantalla  SMALLINT,
    --v_ruta_rep               STRING

  DEFINE 
    v_desc_edo_arch          CHAR(50),
    v_fol_archivo            SMALLINT,
    r_edo_archivo            CHAR(50),
    v_reg_no_dif             INTEGER

  DEFINE v_origen_datos      STRING
  DEFINE v_ruta_reporte      STRING --ruta del archivo del reporte
  DEFINE v_ruta_listados     STRING --ruta de los listados 
  DEFINE v_ruta_ejecutable   STRING --ruta del ejecutable
  DEFINE v_edo_opera_cve     SMALLINT
  DEFINE v_edo_opera_des     LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub
  DEFINE v_estado_operacion  LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub
  DEFINE manejador_rpt       om.SaxDocumentHandler --contenedor documentos para reporte 

  DEFINE arr_apo_sub_glob    DYNAMIC ARRAY OF RECORD 
    v_det_nss                CHAR(11),
    v_det_derechohabiente    DECIMAL(9,0),
    v_folio_sua              DECIMAL(6,0),
    v_det_periodo_pago       CHAR(6),
    v_det_f_pago             DATE,
    v_nrp                    CHAR(11),
    v_dif_pag_aiv            DECIMAL(15,6), --AIVS pagadas
    v_dif_estado             SMALLINT,      --estado del registro
    v_desc_estado            VARCHAR(40),   --descripción Estado del registro
    v_folio_pago             DECIMAL,       --folio de pago
    v_dif_sub_aiv            DECIMAL(26,6), --AIVS pagadas
    v_sumdif_aiv             DECIMAL(26,6), --diferencia aivs
    v_sumdif_pag_aiv         DECIMAL(26,6), --AIVS pagadas
    v_sumdif_sub_aiv         DECIMAL(25,6), --AIVS subsecuente
    v_sum_dif                DECIMAL(25,6)  --suma de diferencias     
  END RECORD
       
  DEFINE 
    v_indice_1               INTEGER,    
    v_ind_rpt                INTEGER, --indice para el reporte  
    v_cantidad               STRING   --variable auxiliar para cantidades

  LET v_fec_proc = TODAY

  -- Obtiene el estado del archivo
  SELECT estado
  INTO   v_fol_archivo
  FROM   glo_ctr_archivo
  WHERE  proceso_cod = g_proceso_cod
  AND    folio       = p_folio 

  -- Obtiene la descripcion del estado del archivo
  SELECT estado_descripcion
  INTO   v_desc_edo_arch
  FROM   cat_edo_archivo
  WHERE  estado_cod = v_fol_archivo

  -- Concatena el estado y la descripcion del archivo
  LET r_edo_archivo = v_fol_archivo || '-' || v_desc_edo_arch CLIPPED

  -- Obtiene el código de estado del proceso
  SELECT estado
  INTO   v_edo_opera_cve
  FROM   dis_ctr_ap_subsecuente
  WHERE  folio = p_folio
   
  -- Obtiene la descripción del estado de la operación
  SELECT desc_edo_apo_sub
  INTO   v_edo_opera_des
  FROM   cat_edo_ap_subsecuente
  WHERE  cod_edo_apo_sub = v_edo_opera_cve

  -- Concatena la clave y la descripción del estado de la operacion
  LET v_estado_operacion = v_edo_opera_cve || '-' || v_edo_opera_des CLIPPED

  -- Obtiene total aportaciones y amortizaciones de tabla detalle
  SELECT SUM(apl_apo_pat), SUM(imp_amo_cred)
  INTO   v_sum_det_aiv, v_sum_det_amo
  FROM   dis_ap_subsecuente
  WHERE  folio = g_folio

  IF v_sum_det_amo IS NULL THEN 
     LET v_sum_det_amo = 0.00
  END IF
   
  -- Obtiene total aportaciones y amortizaciones de tabla sumario
  SELECT tot_imp_apo_aivs, 
         tot_imp_amo_cred,
         tot_registros,
         f_transferencia,
         estado
  INTO   v_sum_aiv,
         v_sum_amo,
         v_sum_tot_reg,
         v_sum_f_transf,
         v_sum_estado
  FROM   dis_ctr_ap_subsecuente
  WHERE  folio = g_folio

  IF  v_sum_amo IS NULL THEN
    LET v_sum_amo = 0.00 
  END IF

  IF v_sum_tot_reg IS NULL THEN 
    LET v_sum_tot_reg = 0
  END IF

  IF v_sum_f_transf IS NULL THEN 
    LET v_sum_f_transf = 0
  END IF

  -- Obtiene total de registros leídos
  SELECT COUNT (*)
  INTO   v_tot_leidos
  FROM   safre_tmp:tmp_dis_aposubs2

  IF v_tot_leidos IS NULL THEN 
    LET v_tot_leidos = 0 
  END IF

  -- Obtiene total de registros con cero en aportación
  SELECT COUNT (*)
  INTO   v_tot_ceros
  FROM   dis_ap_subsecuente
  WHERE  folio        = g_folio
  AND    (apl_apo_pat = 0.00 OR apl_apo_pat IS NULL)

  -- Obtiene total de registros no encontrados en SACI
  SELECT COUNT (*)
  INTO   v_tot_no_enc
  FROM   dis_ap_subsecuente
  WHERE  folio  = g_folio
  AND    estado = 23 -- No encontrado en maestro de derechohabientes

  -- Obtiene total de registros encontrados en SACI
  {SELECT COUNT (*)
  INTO   v_tot_si_enc
  FROM   dis_ap_subsecuente
  WHERE  folio   = g_folio
  AND    estado <> 23 -- No encontrado en maestro de derechohabientes

  -- Obtiene total de registros con montos igual
  SELECT COUNT (*)
  INTO   v_tot_monto_igual
  FROM   dis_ap_subsecuente
  WHERE  folio   = g_folio
  AND    estado <> 26}

  -- Obtiene total de registros con montos diferentes
  SELECT COUNT (*)
  INTO   v_tot_monto_dif
  FROM   dis_ap_subsecuente
  WHERE  folio  = g_folio
  --AND    estado = 26
  AND    estado IN (100,105)

  -- Obtiene total de registros duplicados
  {SELECT COUNT(unique id_derechohabiente)
  INTO   v_tot_dup_ap
  FROM   dis_dup_ap_subsecuente}
  SELECT COUNT(UNIQUE id_derechohabiente)
  INTO   v_tot_dup_ap
  FROM   dis_ap_subsecuente
  WHERE  folio  = g_folio
  AND    estado = 100

  -- Obtiene total de registros de cargo duplicados
  SELECT COUNT(UNIQUE id_derechohabiente)
  INTO   v_tot_dup_cargo_ap
  FROM   dis_ap_subsecuente
  WHERE  folio  = g_folio
  AND    estado = 105
  
  -- Obtiene la suma del detalle de AIVS
  SELECT SUM(aplic_int_viv_apo_pat / 1000000)
  INTO   v_sum_det_apo
  FROM   safre_tmp:tmp_dis_aposubs2;

  IF v_sum_det_apo IS NULL THEN 
     LET v_sum_det_apo = 0.00
  END IF

  -- Obtiene la suma del detalle de Aportaciones en pesos
  SELECT SUM(imp_apo_pat/100)
  INTO   v_sum_apo
  FROM   safre_tmp:tmp_dis_aposubs2;

  --Obtiene el Folio LQ 
  SELECT FIRST 1 folio
  INTO   v_folio_lq
  FROM   glo_folio
  WHERE  proceso_cod = 1401
  ORDER BY folio DESC

  IF v_folio_lq IS NULL THEN 
     LET v_folio_lq = 0
  END IF

  IF v_sum_apo IS NULL THEN 
    LET v_sum_apo = 0.00
  END IF

  -- Despliega información en el log
  DISPLAY "\n ############### INTEGRACIÓN APORTACIONES SUBSECUENTES ###############"

  LET v_cantidad = v_tot_leidos
  DISPLAY " Total de registros en archivo                 : ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_ceros
  DISPLAY " Total de registros con ceros en aportación    : ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_no_enc
  DISPLAY " Total de registros no encontrados en SACI     : ", v_cantidad CLIPPED 

  {LET v_cantidad = v_tot_si_enc
  DISPLAY " Total de registros encontrados en SACI        : ", v_cantidad CLIPPED} 

  LET v_cantidad = v_tot_dup_ap
  DISPLAY " Total de registros duplicados en llave        : ", v_cantidad CLIPPED

  LET v_cantidad = v_tot_dup_cargo_ap
  DISPLAY " Total de registros duplicados por cargo previo: ", v_cantidad CLIPPED
 
  {LET v_cantidad = v_tot_monto_igual
  DISPLAY "    Con monto Igual   : ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_monto_dif
  DISPLAY "    Con monto desigual: ", v_cantidad CLIPPED}
  
  DISPLAY " "
  DISPLAY " Nombre del archivo                            : ", l_arch_proceso CLIPPED
  DISPLAY " "
  
  LET g_qrytxt = " SELECT nss, ",
                 "        id_derechohabiente, ",
                 "        folio_sua,          ",
                 "        periodo_pago,       ",
                 "        f_pago,             ",
                 "        reg_pat_imss        ",
                 " FROM   dis_ap_subsecuente",
                 " WHERE  folio  = ",g_folio,
                 " AND    estado = 100 ",
                 " UNION ALL ",
                 " SELECT nss, ",
                 "        id_derechohabiente, ",
                 "        folio_sua, ",
                 "        periodo_pago, ",
                 "        f_pago, ",
                 "        nrp ",
                 " FROM   dis_ap_cargo_dup ",
                 " WHERE  folio  = ",g_folio,
                 " AND    estado = 105 "

  PREPARE prp_consulta_detalle FROM g_qrytxt

  LET v_origen_datos = v_usurio

  -- Se construye la ruta del archivo
  CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(), "-",
                       "DISE23", "-",
                       l_pid USING "&&&&&", "-",
                       g_proceso_cod USING "&&&&&", "-",
                       g_opera_cod USING "&&&&&",".pdf"                         

  DISPLAY " Ruta del reporte                              : ", v_ruta_reporte 

  -- Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE231.4rp") THEN
     CALL fgl_report_selectDevice ("PDF")        
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()

     ### Inicia Reporte ###
     -- Inicializamos variables para suma de totales
     LET v_sumdif_sub_aiv = 0.00
     LET v_sumdif_pag_aiv = 0.00
     LET v_sum_dif_aiv    = 0.00
     LET v_rec_dif        = 0
     LET v_reg_no_dif     = 0
     LET v_indice_1       = 1 

     -- Inicia el reporte de registros con rechazo
     START REPORT rp_apo_sub_glob TO XML HANDLER manejador_rpt
     DECLARE cur_diferencias_montos CURSOR FOR prp_consulta_detalle   
     FOREACH cur_diferencias_montos INTO arr_apo_sub_glob[v_indice_1].v_det_nss,
                                         arr_apo_sub_glob[v_indice_1].v_det_derechohabiente,
                                         arr_apo_sub_glob[v_indice_1].v_folio_sua,
                                         arr_apo_sub_glob[v_indice_1].v_det_periodo_pago,
                                         arr_apo_sub_glob[v_indice_1].v_det_f_pago,
                                         arr_apo_sub_glob[v_indice_1].v_nrp
      
       LET v_det_derechohabiente = arr_apo_sub_glob[v_indice_1].v_det_derechohabiente
       LET v_folio_sua           = arr_apo_sub_glob[v_indice_1].v_folio_sua
       LET v_det_periodo_pago    = arr_apo_sub_glob[v_indice_1].v_det_periodo_pago
       LET v_det_f_pago          = arr_apo_sub_glob[v_indice_1].v_det_f_pago
       LET v_nrp                 = arr_apo_sub_glob[v_indice_1].v_nrp

       --DISPLAY "v_det_derechohabiente: ",v_det_derechohabiente
       --DISPLAY "v_folio_sua: ",v_folio_sua
       --DISPLAY "v_det_periodo_pago: ",v_det_periodo_pago
       --DISPLAY "v_det_f_pago: ",v_det_f_pago
       --DISPLAY "v_nrp: ",v_nrp

       LET g_qrytxt = "\n SELECT dd.apl_apo_pat, ",
                      "\n        dd.estado, ", 
                      "\n        dd.estado||'-'||ce.desc_edo_apo_sub, ", 
                      "\n        dd.folio_liquida, ",
                      "\n        dd.apl_apo_pat ",
                      "\n FROM   dis_ap_subsecuente dd, ",
                      "\n        cat_edo_ap_subsecuente ce ",
                      "\n WHERE  dd.id_derechohabiente =  ", v_det_derechohabiente,
                      "\n AND    dd.folio_sua          =  ", v_folio_sua,
                      "\n AND    dd.periodo_pago       =  '", v_det_periodo_pago,"'",
                      "\n AND    dd.f_pago             =  '", v_det_f_pago,"'",
                      "\n AND    dd.reg_pat_imss       =  '", v_nrp, "'",
                      "\n AND    dd.estado             = ce.cod_edo_apo_sub ",
                      "\n AND    dd.estado            <> 10",
                      "\n AND    dd.folio              =  ", p_folio
       --DISPLAY g_qrytxt
                      
       PREPARE prp_dis_apo_sub FROM g_qrytxt
       EXECUTE prp_dis_apo_sub INTO --arr_apo_sub_glob[v_indice_1].v_dif_pag_apo,
                                    arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv,
                                    arr_apo_sub_glob[v_indice_1].v_dif_estado,
                                    arr_apo_sub_glob[v_indice_1].v_desc_estado,
                                    arr_apo_sub_glob[v_indice_1].v_folio_pago,
                                    arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv

       --DISPLAY "pag aiv: ",arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv
       --DISPLAY "sub_aiv: ",arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv
       IF arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv IS NULL THEN
          LET arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv  = 0.00
       END IF

       IF arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv IS NULL THEN
          LET arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv  = 0.00
       END IF

       -- Obtiene diferencia en aportaciones
       LET arr_apo_sub_glob[v_indice_1].v_sumdif_aiv = arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv - 
                                                       arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv 

       --DISPLAY "Resta: ",arr_apo_sub_glob[v_indice_1].v_sumdif_aiv       
       IF arr_apo_sub_glob[v_indice_1].v_sumdif_aiv IS NULL THEN
          LET arr_apo_sub_glob[v_indice_1].v_sumdif_aiv  = 0.00
       END IF
            
       -- Sumatoria de totales por campo
       LET v_sumdif_sub_aiv = v_sumdif_sub_aiv + arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv
       LET v_sumdif_pag_aiv = v_sumdif_pag_aiv + arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv
       LET v_sum_dif_aiv    = v_sum_dif_aiv    + arr_apo_sub_glob[v_indice_1].v_sumdif_aiv

       IF v_sumdif_sub_aiv IS NULL THEN 
          LET v_sumdif_sub_aiv = 0.00
       END IF

       IF v_sumdif_pag_aiv IS NULL THEN 
          LET v_sumdif_pag_aiv = 0.00
       END IF

       IF v_sum_dif_aiv IS NULL THEN 
          LET v_sum_dif_aiv = 0.00
       END IF

       --LET arr_apo_sub_glob[v_indice_1].v_desc_estado = "REGISTRO RECHAZADO"

       {IF arr_apo_sub_glob[v_indice_1].v_sumdif_aiv <> 0.00 THEN 
          --Incrementa la variable de registros con diferencias
          LET arr_apo_sub_glob[v_indice_1].v_desc_estado = "CON DIFERENCIAS"
          LET v_rec_dif  = v_rec_dif + 1
          LET v_indice_1 = v_indice_1  + 1
       ELSE 
          CALL  arr_apo_sub_glob.deleteElement(v_indice_1)         
          --LET arr_apo_sub_glob[v_indice_1].v_desc_estado = "SIN DIFERENCIAS"
          --LET v_reg_no_dif = v_reg_no_dif + 1
       END IF}

       LET v_indice_1 = v_indice_1  + 1
     END FOREACH

     CALL arr_apo_sub_glob.deleteElement(v_indice_1)         

     LET v_indice_1 = v_indice_1 - 1

     {IF v_indice_1 = 0 THEN 
        DISPLAY "No se puede generar el reporte por falta de información."
     END IF}  

     IF v_reg_no_dif = 0 OR v_reg_no_dif IS NULL THEN 
        LET v_rec_dif = 0
     ELSE 
        LET v_rec_dif = v_reg_no_dif
     END IF 

     IF v_usurio IS NULL THEN 
        LET v_usurio = "infonavit"        
     END IF

     IF v_indice_1 < 1 THEN 
        LET arr_apo_sub_glob[1].v_det_nss             = ""
        LET arr_apo_sub_glob[1].v_det_derechohabiente = 0 
        LET arr_apo_sub_glob[1].v_folio_sua           = 0 
        LET arr_apo_sub_glob[1].v_det_periodo_pago    = ""
        LET arr_apo_sub_glob[1].v_det_f_pago          = ""
        LET arr_apo_sub_glob[1].v_nrp                 = ""
        LET arr_apo_sub_glob[1].v_dif_pag_aiv         = 0.0
        LET arr_apo_sub_glob[1].v_dif_estado          = 0 
        LET arr_apo_sub_glob[1].v_desc_estado         = ""
        LET arr_apo_sub_glob[1].v_folio_pago          = 0
        LET arr_apo_sub_glob[1].v_dif_sub_aiv         = 0.0
        LET arr_apo_sub_glob[1].v_sumdif_aiv          = 0.0
        LET arr_apo_sub_glob[1].v_sumdif_pag_aiv      = 0.0
        LET arr_apo_sub_glob[1].v_sumdif_sub_aiv      = 0.0
        LET arr_apo_sub_glob[1].v_sum_dif             = 0.0
     
        OUTPUT TO REPORT rp_apo_sub_glob(g_folio,
                                         v_usurio,
                                         v_fec_proc,
                                         v_sum_det_apo,
                                         v_sum_det_amo,
                                         v_sum_apo,
                                         v_sum_amo,
                                         v_sum_tot_reg,
                                         v_sum_f_transf, 
                                         arr_apo_sub_glob[1].*,                                       
                                         v_rec_dif,
                                         --v_edo_desc_ov,
                                         v_estado_operacion,
                                         r_edo_archivo,
                                         v_sumdif_sub_aiv,
                                         v_sumdif_pag_aiv,
                                         v_sum_dif_aiv,
                                         v_folio_lq) --Sumatorias

     ELSE
        FOR v_ind_rpt = 1 TO v_indice_1
            OUTPUT TO REPORT rp_apo_sub_glob(g_folio,
                                             v_usurio,
                                             v_fec_proc,
                                             v_sum_det_apo,
                                             v_sum_det_amo,
                                             v_sum_apo,
                                             v_sum_amo,
                                             v_sum_tot_reg,
                                             v_sum_f_transf,
                                             arr_apo_sub_glob[v_ind_rpt].*,
                                             v_rec_dif,
                                             --v_edo_desc_ov,
                                             v_estado_operacion,
                                             r_edo_archivo,
                                             v_sumdif_sub_aiv,
                                             v_sumdif_pag_aiv,
                                             v_sum_dif_aiv,
                                             v_folio_lq) --Sumatorias
        END FOR 
     END IF 
     FINISH REPORT rp_apo_sub_glob
  ELSE
     DISPLAY "no funciono"
     EXIT PROGRAM
  END IF

  RETURN v_ruta_reporte
END FUNCTION

#Objetivo:Estado del rechazo del archivo
FUNCTION fn_estado_rechazo(p_edo_rech)
  DEFINE 
    p_edo_rech               SMALLINT

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
REPORT rp_apo_sub_glob(v_rfolio,
                       v_rusurio,
                       v_rfec_proc,
                       v_rsum_det_apo,
                       v_rsum_det_amo,
                       v_rsum_apo,
                       v_rsum_amo,
                       v_rsum_tot_reg,
                       v_rsum_f_transf,
                       arr_apo_sub_glob,
                       v_rrec_dif,
                       --v_redo_desc_ov,
                       v_restado_operacion,
                       r_redo_archivo,
                       v_sumrdif_sub_aiv, 
                       v_sumrdif_pag_aiv, 
                       v_sumrdif_aiv,
                       v_folio_lq)              
                       
  DEFINE 
    v_rusurio                VARCHAR(30),   --usuario de proceso
    v_rfec_proc              DATE,          --fecha de procesos
    v_rfolio                 DECIMAL(9,0),
    v_rsum_det_apo           DECIMAL(22,2),
    v_rsum_det_amo           DECIMAL(12,2),
    v_rsum_apo               DECIMAL(22,2),
    v_rsum_amo               DECIMAL(12,2),
    v_rsum_tot_reg           DECIMAL(10,0),
    v_rsum_f_transf          DATE,
    --v_redo_desc_ov           CHAR(50), 
    v_restado_operacion      LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub,
    v_rrec_dif               INTEGER,       --registros con diferencia
    v_sumrdif_aiv            DECIMAL(15,6), --AIVS
    v_sumrdif_sub_aiv        DECIMAL(15,6), --AIVS
    v_sumrdif_pag_aiv        DECIMAL(15,6), --AIVS
    v_folio_lq               DECIMAL(9,0)

  DEFINE r_redo_archivo      CHAR(50)       --descripción del estado del archivo

  DEFINE arr_apo_sub_glob    RECORD 
    v_det_nss                CHAR(11),
    v_det_derechohabiente    DECIMAL(9,0),
    v_folio_sua              DECIMAL(6,0),
    v_det_periodo_pago       CHAR(6),
    v_det_f_pago             DATE,          --fecha de pago
    v_nrp                    CHAR(11),      --NRP       
    v_dif_pag_aiv            DECIMAL(15,6), --AIVS pagadas
    v_dif_estado             SMALLINT,      --estado del registro
    v_desc_estado            VARCHAR(40),   --descripción del Estado del registro
    v_folio_pago             DECIMAL,       --folio de pago
    v_dif_sub_aiv            DECIMAL(15,2), --AIVS pagadas
    v_sumdif_aiv             DECIMAL(15,6), --diferencia aivs
    v_sumdif_pag_aiv         DECIMAL(15,6), --AIVS pagadas
    v_sumdif_sub_aiv         DECIMAL(15,6), --AIVS subsecuente
    v_sum_dif                DECIMAL(15,6)  --suma de diferencias     
  END RECORD  

  FORMAT
    FIRST PAGE HEADER
      PRINTX v_rusurio
      PRINTX v_rfec_proc     USING "dd-mm-yyyy" 
      PRINTX v_rsum_f_transf USING "dd-mm-yyyy" 
      PRINTX v_rrec_dif
       
      PRINTX v_rfolio
      PRINTX v_rsum_det_apo
      PRINTX v_rsum_det_amo
      PRINTX v_rsum_apo
      PRINTX v_rsum_amo
      PRINTX v_rsum_tot_reg
      PRINTX r_redo_archivo
      PRINTX v_restado_operacion
      PRINTX l_arch_proceso      
  
      PRINTX v_tot_leidos        
      PRINTX v_tot_ceros         
      PRINTX v_tot_no_enc
      PRINTX v_tot_dup_cargo_ap
      PRINTX v_folio_lq      
      --PRINTX v_tot_si_enc        
      --PRINTX v_tot_monto_igual   
      PRINTX v_tot_monto_dif     

    ON EVERY ROW
       PRINTX arr_apo_sub_glob.v_folio_pago
       PRINTX arr_apo_sub_glob.v_det_nss
       PRINTX arr_apo_sub_glob.v_det_derechohabiente
       PRINTX arr_apo_sub_glob.v_det_periodo_pago
       PRINTX arr_apo_sub_glob.v_det_f_pago USING "dd-mm-yyyy"
  
       PRINTX arr_apo_sub_glob.v_desc_estado
       PRINTX arr_apo_sub_glob.v_dif_sub_aiv
       PRINTX arr_apo_sub_glob.v_dif_pag_aiv
       PRINTX arr_apo_sub_glob.v_sumdif_aiv
        
       PRINTX arr_apo_sub_glob.v_dif_sub_aiv
       PRINTX arr_apo_sub_glob.v_dif_pag_aiv
       PRINTX v_sumrdif_aiv
       
    ON LAST ROW
       PRINTX v_sumrdif_sub_aiv
       PRINTX v_sumrdif_pag_aiv
       PRINTX v_sumrdif_aiv
        
       PRINTX v_rrec_dif
      
END REPORT