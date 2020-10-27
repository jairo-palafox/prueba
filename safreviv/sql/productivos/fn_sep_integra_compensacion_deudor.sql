






CREATE FUNCTION "safreviv".fn_sep_integra_compensacion_deudor(p_folio DECIMAL(10,0), 
                                                   p_archivo CHAR(40), 
                                                   p_usuario CHAR(20))
RETURNING INTEGER,INTEGER,INTEGER,CHAR(200);

-- Datos de tabla sep_cza_deudor
DEFINE v_cza_folio          DECIMAL(9,0);
DEFINE v_cza_f_lote         DATE;
DEFINE v_cza_nombre_archivo CHAR(40);
-----------------------------------
-- Datos de tabla sep_deudor
DEFINE v_det_id_det_sep_deudor   INTEGER;
DEFINE v_det_folio               DECIMAL(9,0);
DEFINE v_det_tpo_registro        CHAR(2);
DEFINE v_det_consecutivo         SMALLINT;
DEFINE v_det_nss                 CHAR(11);
DEFINE v_det_id_derechohabiente  DECIMAL(9,0);
DEFINE v_det_num_credito         DECIMAL(10,0);
DEFINE v_det_periodo_pago        CHAR(6);
DEFINE v_det_f_pago              DATE;
DEFINE v_det_tpo_pago            CHAR(4);
DEFINE v_det_rechazo             CHAR(2);
DEFINE v_det_desc_rechazo        CHAR(40);
DEFINE v_det_num_cuenta          CHAR(10);
DEFINE v_det_f_restitucion       DATE;
DEFINE v_det_resultado_operacion CHAR(2);
DEFINE v_det_ind_control         SMALLINT;
DEFINE v_det_usuario             CHAR(20);
------------------------------------
-- Datos de tabla sep_sum_deudor
DEFINE v_sum_folio          DECIMAL(9,0);
DEFINE v_sum_tpo_registro   CHAR(2);
DEFINE v_sum_tot_registro   DECIMAL(10,0);
DEFINE v_sum_sum_aportacion DECIMAL(22,2);
------------------------------------
-- Datos de tabla sep_mov_deudor
DEFINE v_mov_id_restitucion         DECIMAL(9,0);
DEFINE v_mov_f_liquida              DATE;
DEFINE v_mov_id_derechohabiente     DECIMAL(9,0);
DEFINE v_mov_movimiento             SMALLINT;
DEFINE v_mov_folio_liquida          DECIMAL(9,0);
DEFINE v_mov_id_referencia          DECIMAL(9,0);
DEFINE v_mov_monto_acciones         DECIMAL(16,6);
DEFINE v_mov_monto_pesos            DECIMAL(12,2);
DEFINE v_mov_f_valor                DATE;
DEFINE v_mov_f_registro             DATE;
DEFINE v_mov_origen                 CHAR(20);
DEFINE v_mov_f_respuesta_deudor     DATE;
DEFINE v_mov_folio_respuesta_deudor DECIMAL(9,0);
------------------------------------
--Datos de tabla tmp_sep_det_compensacion_deud
DEFINE v_det_tmp_tpo_regitro    CHAR(2);
DEFINE v_det_tmp_consecutivo    DECIMAL(3,0);
DEFINE v_det_tmp_nss            CHAR(11);
DEFINE v_det_tmp_credito        CHAR(10);
DEFINE v_det_tmp_periodo_pago   CHAR(6);
DEFINE v_det_tmp_fecha_pago     CHAR(8);
DEFINE v_det_tmp_tipo_pago      CHAR(4);
DEFINE v_det_tmp_rechazo        CHAR(2);
DEFINE v_det_tmp_desc_rechazo   CHAR(40);
DEFINE v_det_tmp_tot_aportacion CHAR(10);
DEFINE v_det_tmp_num_cuenta     CHAR(10);
DEFINE v_det_tmp_fecha          CHAR(8);
DEFINE v_det_tmp_res_operacion  CHAR(2);
------------------------------------
--Datos de tabla tmp_sep_sum_compensacion_deud
DEFINE v_sum_tmp_tpo_registro   CHAR(2);
DEFINE v_sum_tmp_num_registros  DECIMAL(4,0);
DEFINE v_sum_tmp_tot_aportacion CHAR(10);
------------------------------------
-- cifras de registros
DEFINE v_total_procesados    INTEGER;
DEFINE v_total_no_procesados INTEGER;

DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);

DEFINE v_sql_error INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error CHAR(200);


   -- Captura el error sql
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_total_procesados    = 0;
      LET v_total_no_procesados = 0;
      RETURN v_total_procesados,v_total_no_procesados,v_sql_error,v_msg_error;
   END EXCEPTION WITH RESUME;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_integra_compensacion_deudor.trace';
   LET v_sql_error = 0;
   LET v_msg_error = NULL;
   
   LET v_total_procesados    = 0;
   LET v_total_no_procesados = 0;
   
   
   LET v_cza_folio          = p_folio;
   LET v_cza_f_lote         = TODAY;
   LET v_cza_nombre_archivo = p_archivo;
   -- Almacena los datos del encabezado de compensacion deudor
   INSERT INTO safre_viv:sep_cza_deudor
                      (
                       folio,
                       f_lote,
                       nombre_archivo
                       )
                VALUES(
                       v_cza_folio,
                       v_cza_f_lote,
                       v_cza_nombre_archivo
                       );
   
   LET v_mov_movimiento = 382; -- CARGO POR COMPENSACION POR RESTITUCION SEPARACIÓN
   
   --TRACE 'INICIA';
   -- Almacena los datos del detalle de compensacion deudor
   FOREACH SELECT tpo_regitro   ,
                  consecutivo   ,
                  nss           ,
                  credito       ,
                  periodo_pago  ,
                  fecha_pago    ,
                  tipo_pago     ,
                  rechazo       ,
                  desc_rechazo  ,
                  tot_aportacion,
                  num_cuenta    ,
                  fecha         ,
                  res_operacion 
             INTO v_det_tmp_tpo_regitro   ,
                  v_det_tmp_consecutivo   ,
                  v_det_tmp_nss           ,
                  v_det_tmp_credito       ,
                  v_det_tmp_periodo_pago  ,
                  v_det_tmp_fecha_pago    ,
                  v_det_tmp_tipo_pago     ,
                  v_det_tmp_rechazo       ,
                  v_det_tmp_desc_rechazo  ,
                  v_det_tmp_tot_aportacion,
                  v_det_tmp_num_cuenta    ,
                  v_det_tmp_fecha         ,
                  v_det_tmp_res_operacion 

             FROM safre_tmp:tmp_sep_det_compensacion_deud
             
      LET v_det_id_det_sep_deudor = 0;
      SELECT NVL(MAX(id_det_sep_deudor),0)+1 
        INTO v_det_id_det_sep_deudor
        FROM sep_deudor;
        
      LET v_det_folio         = p_folio;
      LET v_det_tpo_registro  = v_det_tmp_tpo_regitro;
      LET v_det_consecutivo   = v_det_tmp_consecutivo;
      LET v_det_nss           = v_det_tmp_nss;
      -- recupera el derechohabiente para el nss
      LET v_det_id_derechohabiente = 0;
      SELECT id_derechohabiente
        INTO v_det_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_det_tmp_nss;
       
      LET v_det_num_credito   = v_det_tmp_credito;
      LET v_det_periodo_pago  = v_det_tmp_periodo_pago;
      LET v_det_f_pago        = v_det_tmp_fecha_pago;
      LET v_det_tpo_pago      = v_det_tmp_tipo_pago;
      LET v_det_rechazo       = v_det_tmp_rechazo;
      LET v_det_desc_rechazo  = v_det_tmp_desc_rechazo;
      LET v_det_num_cuenta    = v_det_tmp_num_cuenta;
      LET v_det_f_restitucion = v_det_tmp_fecha;
      LET v_det_resultado_operacion = v_det_tmp_res_operacion;
      LET v_det_ind_control         = 1;
      LET v_det_usuario             = p_usuario;
      
      INSERT INTO safre_viv:sep_deudor
                              (
                               id_det_sep_deudor  ,
                               folio              ,
                               tpo_registro       ,
                               consecutivo        ,
                               nss                ,
                               id_derechohabiente ,
                               num_credito        ,
                               periodo_pago       ,
                               f_pago             ,
                               tpo_pago           ,
                               rechazo            ,
                               desc_rechazo       ,
                               num_cuenta         ,
                               f_restitucion      ,
                               resultado_operacion,
                               ind_control        ,
                               usuario            
                               )
                        --VALUES(seq_sep_deudor.NEXTVAL  ,
                        VALUES(v_det_id_det_sep_deudor  ,
                               v_det_folio              ,
                               v_det_tpo_registro       ,
                               v_det_consecutivo        ,
                               v_det_nss                ,
                               v_det_id_derechohabiente ,
                               v_det_num_credito        ,
                               v_det_periodo_pago       ,
                               v_det_f_pago             ,
                               v_det_tpo_pago           ,
                               v_det_rechazo            ,
                               v_det_desc_rechazo       ,
                               v_det_num_cuenta         ,
                               v_det_f_restitucion      ,
                               v_det_resultado_operacion,
                               v_det_ind_control        ,
                               v_det_usuario            
                               );
                               
      LET v_mov_id_restitucion = 0;
      SELECT MIN(id_restitucion)
        INTO v_mov_id_restitucion
        FROM sep_mov_deudor
       WHERE id_derechohabiente = v_det_id_derechohabiente;
       
      IF(v_mov_id_restitucion <> 0 AND v_mov_id_restitucion IS NOT NULL)THEN
       
         SELECT f_liquida         ,
                id_derechohabiente,
                folio_liquida     ,
                id_referencia     ,
                monto_acciones    ,
                monto_pesos       ,
                f_valor           ,
                f_registro        ,
                origen            ,
                f_respuesta_deudor,
                folio_respuesta_deudor
           INTO v_mov_f_liquida         ,
                v_mov_id_derechohabiente,
                v_mov_folio_liquida     ,
                v_mov_id_referencia     ,
                v_mov_monto_acciones    ,
                v_mov_monto_pesos       ,
                v_mov_f_valor           ,
                v_mov_f_registro        ,
                v_mov_origen            ,
                v_mov_f_respuesta_deudor,
                v_mov_folio_respuesta_deudor
           FROM sep_mov_deudor
          WHERE id_restitucion = v_mov_id_restitucion;
          
         LET v_mov_id_restitucion = 0;
         SELECT NVL(MAX(id_restitucion),0)+1 
           INTO v_mov_id_restitucion
           FROM sep_mov_deudor;
           
         LET v_mov_monto_acciones     = v_mov_monto_acciones * -1;
         LET v_mov_monto_pesos        = v_mov_monto_pesos * -1;
         LET v_mov_f_respuesta_deudor = TODAY;
         LET v_mov_folio_respuesta_deudor = p_folio;
         
          
         INSERT INTO safre_viv:sep_mov_deudor
                            (
                             id_restitucion    ,
                             f_liquida         ,   
                             id_derechohabiente,   
                             movimiento        ,   
                             folio_liquida     ,   
                             id_referencia     ,   
                             monto_acciones    ,   
                             monto_pesos       ,   
                             f_valor           ,   
                             f_registro        ,   
                             origen            ,   
                             f_respuesta_deudor,   
                             folio_respuesta_deudor
                             )
                      VALUES(
                             --seq_sep_mov_deudor.NEXTVAL,
                             v_mov_id_restitucion,
                             v_mov_f_liquida         ,
                             v_mov_id_derechohabiente,
                             v_mov_movimiento        ,
                             v_mov_folio_liquida     ,
                             v_mov_id_referencia     ,
                             v_mov_monto_acciones    ,
                             v_mov_monto_pesos       ,
                             v_mov_f_valor           ,
                             v_mov_f_registro        ,
                             v_mov_origen            ,
                             v_mov_f_respuesta_deudor,
                             v_mov_folio_respuesta_deudor
                            );

          
      ELSE
         LET v_total_no_procesados = v_total_no_procesados + 1;
      END IF



      LET v_total_procesados = v_total_procesados + 1;
   END FOREACH
   
   -- Almacena los datos del sumario de compensacion deudor
   FOREACH SELECT tpo_registro  ,
                  num_registros ,
                  tot_aportacion
             INTO v_sum_tmp_tpo_registro  ,
                  v_sum_tmp_num_registros ,
                  v_sum_tmp_tot_aportacion
             FROM safre_tmp:tmp_sep_sum_compensacion_deud
             
      LET v_sum_folio          = p_folio;
      LET v_sum_tpo_registro   = v_sum_tmp_tpo_registro;
      LET v_sum_tot_registro   = v_sum_tmp_num_registros;
      LET v_sum_sum_aportacion = v_sum_tmp_tot_aportacion;
             
      INSERT INTO safre_viv:sep_sum_deudor
                         (
                          folio         ,
                          tpo_registro  ,
                          tot_registro  ,
                          sum_aportacion
                          )
                   VALUES(
                          v_sum_folio       ,
                          v_sum_tpo_registro,
                          v_sum_tot_registro,
                          v_sum_sum_aportacion
                          );
            
   END FOREACH


   --TRACE '16';
   UPDATE glo_ctr_archivo
      SET estado = 2,
          folio = p_folio
    WHERE nombre_archivo = p_archivo;

   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;


   UPDATE STATISTICS FOR TABLE safre_viv:sep_deudor;
   UPDATE STATISTICS FOR TABLE safre_viv:sep_sum_deudor;
   UPDATE STATISTICS FOR TABLE safre_viv:sep_cza_deudor;
   UPDATE STATISTICS FOR TABLE safre_viv:sep_mov_deudor;

   RETURN v_total_procesados,v_total_no_procesados,v_sql_error,v_msg_error;
END FUNCTION;


