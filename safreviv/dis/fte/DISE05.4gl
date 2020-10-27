################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 07/01/2013                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE05                                                   #
#Objetivo          => Programa para integrar el archivo de nuevos              #                   
#                     acreditados que ha sido validado                         #
#Fecha inicio      => 03/02/2012                                               #
################################################################################
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
    v_desc_err               VARCHAR(200)

  LET v_usurio       = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)

  LET p_transaccion  = 0

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
        --Inserta datos de control del proceso de aportaciones subsecuentes
        PREPARE prp_sp_apo_sub
           FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs1(?,?)"
        EXECUTE prp_sp_apo_sub USING g_folio, r_edo_rech
           INTO r_bnd, v_status_err, v_desc_err
     WHENEVER ERROR STOP 

     IF r_bnd <> 0 THEN
        DISPLAY "Error1: ",v_status_err," - ",v_desc_err, " - ",r_bnd

        CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
        RETURNING r_bnd_oera_error

        EXIT PROGRAM 
     END IF

     WHENEVER ERROR CONTINUE
        --Identifica duplicidad de registro de aportaciones subsecuentes
        PREPARE prp_sp_apo_sub4
           FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs4(?,?)"
        EXECUTE prp_sp_apo_sub4 USING g_folio,r_edo_rech
           INTO r_bnd, v_status_err,  v_desc_err
     WHENEVER ERROR STOP
    
     WHENEVER ERROR CONTINUE 
        --Inserta detalles del proceso de aportaciones subsecuentes
        PREPARE prp_sp_apo_sub1
           FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs2(?,?)"
        EXECUTE prp_sp_apo_sub1 USING g_folio,r_edo_rech
           INTO r_bnd, v_status_err,  v_desc_err
     WHENEVER ERROR STOP 
      
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
                 "\n FROM safre_tmp:tmp_dis_aposubs2"
                  
  PREPARE prp_sql_total_registros FROM v_qrytxt
  EXECUTE prp_sql_total_registros INTO v_reg_det
   
  LET v_qrytxt = " SELECT tot_registros",
                 " FROM safre_tmp:tmp_dis_aposubs9"
                  
  PREPARE prp_sql_registros_sumario FROM v_qrytxt
  EXECUTE prp_sql_registros_sumario INTO v_reg_sum

  LET v_sum_reg = v_reg_det - v_reg_sum
  
  IF v_sum_reg <> 0 THEN
     LET v_edo_rech = 20 -- Rechazo por diferencia en numero de registros
     PREPARE prp_sp_apo_sub_tot
        FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs1(?,?)"
     EXECUTE prp_sp_apo_sub_tot USING g_folio,v_edo_rech
        INTO r_bnd, v_status_err, v_desc_err

     IF r_bnd <> 0 THEN
        DISPLAY "Error-S1: ",v_status_err," ",v_desc_err
     END IF
      
     RETURN v_edo_rech
  END IF 

  -- Valida total de aportaciones de detalle vs sumarion
  LET v_qrytxt = "\n SELECT SUM(aplic_int_viv_apo_pat), SUM(imp_amo_cred)",
                 "\n FROM safre_tmp:tmp_dis_aposubs2"
  PREPARE prp_sql_det_sumas FROM v_qrytxt
  EXECUTE prp_sql_det_sumas INTO v_det_aiv,v_det_amo

  -- Valida total de aportaciones de detalle vs sumarion
  LET v_qrytxt = "\n SELECT tot_aplic_int_viv_apo, tot_imp_amo_cred",
                 "\n FROM safre_tmp:tmp_dis_aposubs9"
  PREPARE prp_sql_sum_sumas FROM v_qrytxt
  EXECUTE prp_sql_sum_sumas INTO v_sum_aiv,v_sum_amo

  LET v_apo_dif = v_det_aiv - v_sum_aiv
  --LET v_amo_dif = v_det_amo - v_sum_amo

  IF v_apo_dif <> 0 THEN
     LET v_edo_rech = 21 -- Rechazo por diferencia en aportaciones
     PREPARE prp_sp_apo_sub_apo
        FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs1(?,?)"
     EXECUTE prp_sp_apo_sub_apo USING g_folio, v_edo_rech
        INTO r_bnd, v_status_err, v_desc_err 

     IF r_bnd <> 0 THEN
        DISPLAY "Error-S21: ",v_status_err," ",v_desc_err
     END IF
      
     RETURN v_edo_rech
  END IF

  RETURN v_edo_rech
END FUNCTION

#Objetivo: Genera reporte de cifras globales de aportaciones subsecuentes
FUNCTION fn_genera_reporte_aportaciones(p_folio)
  DEFINE 
    p_folio                  DECIMAL(10,0)  --Aportaciones
    
  DEFINE 
    v_sum_det_apo            DECIMAL(12,2),--Suma aportacion detalle
    v_sum_det_amo            DECIMAL(12,2),--Suma amortizacion detalle
    v_sum_det_aiv            DECIMAL(15,6),--Suma aivs detalle
    v_sum_apo                DECIMAL(17,2),--Tot aport sumario
    v_sum_amo                DECIMAL(17,2),--Tot amort sumario
    v_sum_aiv                DECIMAL(20,6),--Tot aivs  sumario
    v_sum_tot_reg            INTEGER,--Tot registros sumario
    v_sum_f_transf           DATE,--Fech transferencai
    v_sum_estado             SMALLINT,-- Estado del archivo
    v_det_nss                CHAR(11),--Nss trabajador
    v_det_derechohabiente    DECIMAL(9,0),--Id_derechohab
    v_folio_sua              DECIMAL(6,0), --Folio SUA
    v_det_periodo_pago       CHAR(6),--perido de pago
    v_det_f_pago             DATE,--Fecha de pago
    v_nrp                    CHAR(11), --NRP
    v_dif_apo                DECIMAL(26,6),--Diferencia aportacion
    v_edo_desc               CHAR(50),--Desc edo rechazo
    v_edo_desc_ov            CHAR(50),--Desc edo rechazo
    v_fec_proc               DATE, --Fecha de proceso
    v_rec_dif                INTEGER, --Numero rechazos con fiferencia
    --v_sumdif_pag_apo         DECIMAL(12,2),--Aportaciones pagadas
    --v_sumdif_sub_apo         DECIMAL(12,2),--Aportacion subsecuente
    --v_sum_dif                DECIMAL(12,2), --Suma de diferencias
    v_sumdif_pag_aiv         DECIMAL(26,6), --AIVS pagadas
    v_sumdif_sub_aiv         DECIMAL(26,6), --AIVS subsecuente
    v_sum_dif_aiv            DECIMAL(26,6), --Suma de diferencias aivs
    p_b_despliegue_pantalla  SMALLINT,
    v_ruta_rep               STRING

  DEFINE 
    v_desc_edo_arch          CHAR(50),
    v_fol_archivo            SMALLINT,
    r_edo_archivo            CHAR(50),
    v_reg_no_dif             INTEGER

  DEFINE v_origen_datos      STRING
  DEFINE v_ruta_reporte      STRING -- ruta del archivo del reporte
  DEFINE v_ruta_listados     STRING -- ruta de los listados 
  DEFINE v_ruta_ejecutable   STRING -- ruta del ejecutable
  DEFINE v_edo_opera_cve     SMALLINT
  DEFINE v_edo_opera_des     LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub
  DEFINE v_estado_operacion  LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos para reporte 

  DEFINE arr_apo_sub_glob    DYNAMIC ARRAY OF RECORD 
    v_det_nss                CHAR(11),
    v_det_derechohabiente    DECIMAL(9,0),
    v_folio_sua              DECIMAL(6,0),
    v_det_periodo_pago       CHAR(6),
    v_det_f_pago             DATE,
    v_nrp                    CHAR(11),
    v_dif_pag_aiv            DECIMAL(15,6),--AIVS pagadas
    v_dif_estado             SMALLINT,--Estado del registro
    v_desc_estado            VARCHAR (40),--Descripción del Estado del registro
    v_folio_pago             DECIMAL,       --Folio de pago
    v_dif_sub_aiv            DECIMAL(26,6),--AIVS pagadas
    v_sumdif_aiv             DECIMAL(26,6),--Diferencia aivs
    v_sumdif_pag_aiv         DECIMAL(26,6),--AIVS pagadas
    v_sumdif_sub_aiv         DECIMAL(25,6),--AIVS subsecuente
    v_sum_dif                DECIMAL(25,6) --Suma de diferencias     
  END RECORD
       
  DEFINE 
    v_indice_1               INTEGER,    
    v_ind_rpt                INTEGER, --Indice para el reporte  
    v_cantidad               STRING --Variable auxiliar para cantidades

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

  --Obtiene el código de estado del proceso
  SELECT estado
  INTO   v_edo_opera_cve
  FROM   dis_ctr_ap_subsecuente
  WHERE  folio = p_folio
   
  --Obtiene la descripción del estado de la operación
  SELECT desc_edo_apo_sub
  INTO   v_edo_opera_des
  FROM   cat_edo_ap_subsecuente
  WHERE  cod_edo_apo_sub = v_edo_opera_cve

  --Concatena la clave y la descripción del estado de la operacion
  LET v_estado_operacion = v_edo_opera_cve || '-' || v_edo_opera_des CLIPPED

  -- Obtiene total aportaciones y amortizaciones de tabla detalle
  SELECT SUM(apl_apo_pat), SUM(imp_amo_cred)
  INTO   v_sum_det_aiv, v_sum_det_amo
  FROM   dis_ap_subsecuente
  WHERE  folio = g_folio
   
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

  --Obtiene total de registros leídos
  SELECT COUNT (*)
  INTO   v_tot_leidos
  FROM   safre_tmp:tmp_dis_aposubs2

  --Obtiene total de registros con cero en aportación
  SELECT COUNT (*)
  INTO   v_tot_ceros
  FROM   dis_ap_subsecuente
  WHERE  folio        = g_folio
  AND    (apl_apo_pat = 0.00 OR apl_apo_pat IS NULL)

  --Obtiene total de registros no encontrados en SACI
  SELECT COUNT (*)
  INTO   v_tot_no_enc
  FROM   dis_ap_subsecuente
  WHERE  folio  = g_folio
  AND    estado = 23 --No encontrado en maestro de derechohabientes

  --Obtiene total de registros encontrados en SACI
  SELECT COUNT (*)
  INTO   v_tot_si_enc
  FROM   dis_ap_subsecuente
  WHERE  folio   = g_folio
  AND    estado <> 23 --No encontrado en maestro de derechohabientes

  --Obtiene total de registros con montos igual
  SELECT COUNT (*)
  INTO   v_tot_monto_igual
  FROM   dis_ap_subsecuente
  WHERE  folio   = g_folio
  AND    estado <> 26

  --Obtiene total de registros con montos diferentes
  SELECT COUNT (*)
  INTO   v_tot_monto_dif
  FROM   dis_ap_subsecuente
  WHERE  folio  = g_folio
  AND    estado = 26

  --Obtiene total de registros duplicados
  SELECT COUNT(unique id_derechohabiente)
  INTO   v_tot_dup_ap
  FROM   dis_dup_ap_subsecuente

  --Obtiene la suma del detalle de aportaciones
  SELECT SUM(aplic_int_viv_apo_pat/1000000)
  INTO v_sum_det_apo
  FROM safre_tmp:tmp_dis_aposubs2;

  --Obtiene el total del sumario de aportaciones
  SELECT (tot_aplic_int_viv_apo/1000000)
  INTO v_sum_apo
  FROM safre_tmp:tmp_dis_aposubs9; 
   

  --Despliega información en el log
  DISPLAY "\n ############### INTEGRACIÓN APORTACIONES SUBSECUENTES ###############"

  LET v_cantidad = v_tot_leidos
  DISPLAY " Total de registros en archivo             : ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_ceros
  DISPLAY " Total de registros con ceros en aportación: ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_no_enc
  DISPLAY " Total de registros no encontrados en SACI : ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_si_enc
  DISPLAY " Total de registros encontrados en SACI    : ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_dup_ap
  DISPLAY " Total de registros duplicados en llave    : ", v_cantidad CLIPPED

  LET v_cantidad = v_tot_monto_igual
  DISPLAY "    Con monto Igual   : ", v_cantidad CLIPPED 

  LET v_cantidad = v_tot_monto_dif
  DISPLAY "    Con monto desigual: ", v_cantidad CLIPPED 

  DISPLAY " Nombre del archivo   : ", l_arch_proceso CLIPPED 
   

  LET g_qrytxt = " SELECT nss, ",
                 "        id_derechohabiente, ",
                 "        folio_sua,          ",
                 "        periodo_pago,       ",
                 "        f_pago,             ",
                 "        reg_pat_imss        ",
                 " FROM   dis_ap_subsecuente",
                 " WHERE  folio = ",g_folio,
                 " AND    estado = 26 "

  PREPARE prp_consulta_detalle FROM g_qrytxt

  LET v_origen_datos = v_usurio

  -- se construye la ruta del archivo
  CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(),"-",
                       "DISE05","-",
                       l_pid USING "&&&&&","-",
                       g_proceso_cod USING "&&&&&","-",
                       g_opera_cod USING "&&&&&",".pdf"                         

  DISPLAY "Ruta del reporte: ",v_ruta_reporte
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE051.4rp") THEN

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
            
     --Inicia el reporte de registros con rechazo
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
       
       LET g_qrytxt = "\n  SELECT dd.apl_apo_pat, ",
                      "\n         dd.estado, ", 
                      "\n         dd.estado||'-'||ce.desc_edo_apo_sub, ", 
                      "\n         dd.folio_liquida, ",
                      "\n         di.aiv_ap_pat ",
                      "\n  FROM   dis_ap_subsecuente dd, ",
                      "\n         cat_edo_ap_subsecuente    ce, ",
                      "\n         dis_interface_ef          di ",
                      "\n  WHERE  dd.id_derechohabiente =  ", v_det_derechohabiente,
                      "\n    AND  dd.folio_sua          =  ", v_folio_sua,
                      "\n    AND  dd.periodo_pago       =  '", v_det_periodo_pago,"'",
                      "\n    AND  dd.f_pago             =  '", v_det_f_pago,"'",
                      "\n    AND  dd.reg_pat_imss       =  '", v_nrp, "'",
                      "\n    AND  dd.id_derechohabiente = di.id_derechohabiente ",
                      "\n    AND  dd.folio_sua          = di.folio_sua ",
                      "\n    AND  dd.periodo_pago       = di.periodo_pago ",
                      "\n    AND  dd.f_pago             = di.f_pago ",
                      "\n    AND  dd.reg_pat_imss       = di.nrp ",
                      "\n    AND  dd.estado             = ce.cod_edo_apo_sub ",
                      "\n    AND  dd.folio              =  ", p_folio
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


       
       
       --Obtiene diferencia en aportaciones
       LET arr_apo_sub_glob[v_indice_1].v_sumdif_aiv = arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv - 
                                                       arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv 


       --DISPLAY "Resta: ",arr_apo_sub_glob[v_indice_1].v_sumdif_aiv
       
       IF arr_apo_sub_glob[v_indice_1].v_sumdif_aiv IS NULL THEN
          LET arr_apo_sub_glob[v_indice_1].v_sumdif_aiv  = 0.00
       END IF
            
       --Sumatoria de totales por campo
       LET v_sumdif_sub_aiv = v_sumdif_sub_aiv + arr_apo_sub_glob[v_indice_1].v_dif_sub_aiv
       LET v_sumdif_pag_aiv = v_sumdif_pag_aiv + arr_apo_sub_glob[v_indice_1].v_dif_pag_aiv
       LET v_sum_dif_aiv    = v_sum_dif_aiv + arr_apo_sub_glob[v_indice_1].v_sumdif_aiv

       IF arr_apo_sub_glob[v_indice_1].v_sumdif_aiv <> 0.00 THEN 
          --Incrementa la variable de registros con diferencias
          LET arr_apo_sub_glob[v_indice_1].v_desc_estado = "CON DIFERENCIAS"
          LET v_rec_dif = v_rec_dif + 1
          LET v_indice_1 = v_indice_1  + 1
       ELSE 
          CALL  arr_apo_sub_glob.deleteElement(v_indice_1)         
          --LET arr_apo_sub_glob[v_indice_1].v_desc_estado = "SIN DIFERENCIAS"
          --LET v_reg_no_dif = v_reg_no_dif + 1
       END IF

       --LET v_indice_1 = v_indice_1  + 1
                  
     END FOREACH

     CALL  arr_apo_sub_glob.deleteElement(v_indice_1)         

     LET v_indice_1 = v_indice_1 - 1

     IF v_indice_1 = 0 THEN 
        DISPLAY "No se puede generar el reporte por falta de información."
     END IF  

     IF v_reg_no_dif = 0 OR v_reg_no_dif IS NULL THEN 
        LET v_rec_dif = 0
     ELSE 
        LET v_rec_dif = v_reg_no_dif
     END IF 

     IF v_usurio IS NULL THEN 
        LET v_usurio = "infonavit"
     END IF 
                  
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
                                          v_edo_desc_ov,
                                          v_estado_operacion,
                                          r_edo_archivo,
                                          v_sumdif_sub_aiv,
                                          v_sumdif_pag_aiv,
                                          v_sum_dif_aiv --Sumatorias
                                          )
     END FOR 

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
                       v_redo_desc_ov,
                       v_restado_operacion,
                       r_redo_archivo,
                       v_sumrdif_sub_aiv, 
                       v_sumrdif_pag_aiv, 
                       v_sumrdif_aiv
                      )              
                       
  DEFINE 
    v_rusurio                VARCHAR(30),--Usuario de proceso
    v_rfec_proc              DATE,--Fecha de procesos
    v_rfolio                 DECIMAL(9,0),
    v_rsum_det_apo           DECIMAL(12,2),
    v_rsum_det_amo           DECIMAL(12,2),
    v_rsum_apo               DECIMAL(12,2),
    v_rsum_amo               DECIMAL(12,2),
    v_rsum_tot_reg           SMALLINT,
    v_rsum_f_transf          DATE,
    v_redo_desc_ov           CHAR(50),--LIKE cat_edo_apo_sub.desc_edo_apo_sub,
    v_restado_operacion      LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub,
    v_rrec_dif               SMALLINT,--Registrso con diferencia
    v_sumrdif_aiv            DECIMAL(15,6),--AIVS
    v_sumrdif_sub_aiv        DECIMAL(15,6),--AIVS
    v_sumrdif_pag_aiv        DECIMAL(15,6)--AIVS

  DEFINE r_redo_archivo      CHAR(50)--Descripción del estado del archivo

  DEFINE arr_apo_sub_glob    RECORD 
    v_det_nss                CHAR(11),
    v_det_derechohabiente    DECIMAL(9,0),
    v_folio_sua              DECIMAL(6,0),
    v_det_periodo_pago       CHAR(6),
    v_det_f_pago             DATE,--Fecha de pago
    v_nrp                    CHAR(11), --NRP       
    v_dif_pag_aiv            DECIMAL(15,6),--AIVS pagadas
    v_dif_estado             SMALLINT,--Estado del registro
    v_desc_estado            VARCHAR (40),--Descripción del Estado del registro
    v_folio_pago             DECIMAL,       --Folio de pago
    v_dif_sub_aiv            DECIMAL(15,2),--AIVS pagadas
    v_sumdif_aiv             DECIMAL(15,6),--Diferencia aivs
    v_sumdif_pag_aiv         DECIMAL(15,6),--AIVS pagadas
    v_sumdif_sub_aiv         DECIMAL(15,6),--AIVS subsecuente
    v_sum_dif                DECIMAL(15,6) --Suma de diferencias     
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
      PRINTX v_tot_si_enc        
      PRINTX v_tot_monto_igual   
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
       PRINTX   v_sumrdif_sub_aiv
       PRINTX   v_sumrdif_pag_aiv
       PRINTX   v_sumrdif_aiv
        
       PRINTX   v_rrec_dif
      
END REPORT