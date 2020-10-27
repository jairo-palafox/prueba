






CREATE FUNCTION "safreviv".fn_registra_solicitud_restitucion_ssv(p_proceso_cod SMALLINT, 
                                                      p_opera_cod SMALLINT, 
                                                      p_usuario_cod CHAR(20))
RETURNING INTEGER,
          DECIMAL(9,0),
          INTEGER,
          CHAR(200);

DEFINE r_folio            DECIMAL(9,0);
DEFINE v_total_procesados INTEGER;

DEFINE v_sql_error      INTEGER;
DEFINE v_isam_error     SMALLINT;
DEFINE v_msg_error      CHAR(200);
DEFINE v_id_expediente  DECIMAL(9,0);
DEFINE v_id_restitucion DECIMAL(9,0);
DEFINE v_f_movimiento_dev CHAR(8);
--------------------------
--- sep_nss_expediente
DEFINE v_id_derechohabiente_inv DECIMAL(10,0);
DEFINE v_nss_inv                CHAR(11);
DEFINE v_num_credito_inv        DECIMAL(10,0);
DEFINE v_tpo_credito_inv        CHAR(18);


DEFINE v_id_derechohabiente_asoc DECIMAL(10,0);
DEFINE v_nss_asoc         CHAR(11);
DEFINE v_num_credito_asoc DECIMAL(10,0);
DEFINE v_tpo_credito_asoc CHAR(18);

--------------------------
---sep_mto_restitucion_analisis
DEFINE v_id_mto_restitucion         DECIMAL(9,0);
DEFINE v_sar92_pesos_devolver       DECIMAL(22,2);
DEFINE v_sar92_aivs_devolver        DECIMAL(22,2);
DEFINE v_viv97_pesos_devolver       DECIMAL(22,2);
DEFINE v_viv97_aivs_devolver        DECIMAL(22,2);
DEFINE v_subsc_viv97_pesos_devolver DECIMAL(22,2);
DEFINE v_subsc_viv97_aivs_devolver  DECIMAL(22,2);
--------------------------
--- afi_derechohabiente
DEFINE v_tipo_trabajador CHAR(1);
--------------------------
--- dse_devolucion
DEFINE v_subcuenta_92 DECIMAL(9,0);
DEFINE v_subcuenta_97 DECIMAL(9,0);
--------------------------
--- sep_115_restitucion
DEFINE v_id_115_restitucion DECIMAL(9,0);
DEFINE v_nss_115 CHAR(11);
DEFINE v_sar92_pesos_115_trabajador       DECIMAL(22,2);
DEFINE v_sar92_aivs_115_trabajador        DECIMAL(22,2);
DEFINE v_viv97_pesos_115_trabajador       DECIMAL(22,2);
DEFINE v_viv97_aivs_115_trabajador        DECIMAL(22,2);
DEFINE v_subsc_viv97_pesos_115_trabajador DECIMAL(22,2);
DEFINE v_subsc_viv97_aivs_115_trabajador  DECIMAL(22,2);
DEFINE v_sar92_pesos_115_acreditado       DECIMAL(22,2);
DEFINE v_sar92_aivs_115_acreditado        DECIMAL(22,2);
DEFINE v_viv97_pesos_115_acreditado       DECIMAL(22,2);
DEFINE v_viv97_aivs_115_acreditado        DECIMAL(22,2);
DEFINE v_subsc_viv97_pesos_115_acreditado DECIMAL(22,2);
DEFINE v_subsc_viv97_aivs_115_acreditado  DECIMAL(22,2);
---------------------------
--- sep_mto_restitucion_no_plicados
DEFINE v_id_mto_restitucion_no_aplicados  DECIMAL(9,0);
DEFINE v_subsc_viv97_pesos_no_aplica      DECIMAL(22,2);
DEFINE v_subsc_viv97_aivs_no_aplica       DECIMAL(22,2);
DEFINE v_subsc_viv97_pesos_no_aplica_pscd DECIMAL(22,2);
DEFINE v_subsc_viv97_aivs_no_aplica_pscd  DECIMAL(22,2);
---------------------------
--- sep_restitucion
DEFINE v_rest_id_restitucion DECIMAL(9,0);
DEFINE v_rest_id_expediente  DECIMAL(9,0);
DEFINE v_rest_invadido       CHAR(11);
DEFINE v_rest_aivs           DECIMAL(16,6);
DEFINE v_rest_pesos          DECIMAL(12,2);
DEFINE v_rest_flujo_desc     CHAR(40);

DEFINE v_rest_fecha_actual   DATE;
DEFINE v_afi_id_derechohabiente DECIMAL(9,0);

DEFINE v_mes_siguiente CHAR(2);
DEFINE v_precio_fondo  DECIMAL(19,14);
DEFINE v_observacion   CHAR(011);
DEFINE v_fecha_val     DATE;


   -- Captura el error sql
   ON EXCEPTION SET v_sql_error,
                    v_isam_error,
                    v_msg_error

      LET v_total_procesados = 0;
      LET r_folio = 0;
      RETURN v_total_procesados, 
             r_folio,
             v_sql_error,
             v_msg_error;
   END EXCEPTION;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_registra_solicitud_restitucion_ssv.trace';
   LET v_sql_error = 0;
   LET v_msg_error = NULL;
   
   LET v_total_procesados = 0;
   LET r_folio = 0;
   
   EXECUTE FUNCTION fn_genera_folio(p_proceso_cod, 
                                    p_opera_cod,
                                    p_usuario_cod) INTO r_folio;
   
   LET v_mes_siguiente = MONTH(TODAY) + 1;
   
   IF(v_mes_siguiente = 13)THEN
      LET v_mes_siguiente = "01";
   ELSE
     IF(v_mes_siguiente < 10)THEN
        LET v_mes_siguiente = "0"||v_mes_siguiente;
     END IF
   END IF

   LET v_f_movimiento_dev = v_mes_siguiente||
                            "01"||
                            YEAR(TODAY);

	 -- registro de los movimientos para el deudor
   LET v_rest_fecha_actual = TODAY;
	 LET v_subcuenta_92 = 8;
   LET v_subcuenta_97 = 4;

   FOREACH SELECT id_mto_restitucion,
                  id_restitucion,
                  id_expediente,
                  sar92_pesos_devolver,
                  sar92_aivs_devolver,
                  viv97_pesos_devolver,
                  viv97_aivs_devolver,
                  subsc_viv97_pesos_devolver,
                  subsc_viv97_aivs_devolver
    	       INTO v_id_mto_restitucion,	
                  v_id_restitucion,
                  v_id_expediente,
                  v_sar92_pesos_devolver,
                  v_sar92_aivs_devolver,
                  v_viv97_pesos_devolver,
                  v_viv97_aivs_devolver,
                  v_subsc_viv97_pesos_devolver,
                  v_subsc_viv97_aivs_devolver
    	       FROM sep_mto_restitucion_analisis
    	      WHERE ind_restitucion = 1

      SELECT id_derechohabiente,
             nss,
             num_credito,
             tpo_credito
        INTO v_id_derechohabiente_inv,
             v_nss_inv,
             v_num_credito_inv,
             v_tpo_credito_inv
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 1;
      				 
      SELECT id_derechohabiente,
             nss,
             num_credito,
             tpo_credito
        INTO v_id_derechohabiente_asoc,
             v_nss_asoc,
             v_num_credito_asoc,
             v_tpo_credito_asoc
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 2;	  
    				 
      -- Registro de subcuenta viv97 
      IF v_viv97_pesos_devolver <> 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep", -- modulo
                v_id_derechohabiente_inv,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio   ,  --folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_viv97_aivs_devolver,  
                v_viv97_pesos_devolver,
                NULL,
                NULL,
                v_observacion,
                10); -- estado
         
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_viv97_aivs_devolver,
                v_viv97_pesos_devolver,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "ANALISIS V97",
                0);
      END IF       

      -- Registro de subcuenta sar92
      IF v_sar92_pesos_devolver > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_inv,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,       -- folio_referencia
                v_subcuenta_92,      -- subcuenta
                v_sar92_aivs_devolver,  
                v_sar92_pesos_devolver,
                NULL, 
                NULL, 
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_sar92_aivs_devolver,
                v_sar92_pesos_devolver,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "ANALISIS V92",
                0);
      END IF

      -- Registro de subsecuentes subcuenta = 4
      IF v_subsc_viv97_pesos_devolver > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0 ,
                "sep",
                v_id_derechohabiente_inv,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,      -- folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_subsc_viv97_aivs_devolver,  
                v_subsc_viv97_pesos_devolver,
                NULL, 
                NULL, 
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida,
                id_referencia,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_subsc_viv97_aivs_devolver,
                v_subsc_viv97_pesos_devolver,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "ANALISIS SBSEC V97",
                0);
      END IF

     -- Actualiza el registro, para indicar que ya se proceso
     UPDATE sep_mto_restitucion_analisis
        SET ind_restitucion = 2 -- RESTITUCION REGISTRADA
      WHERE id_mto_restitucion = v_id_mto_restitucion
        AND id_expediente = v_id_expediente;
     
     --TRACE '7';
     UPDATE sep_restitucion
        SET folio = r_folio
      WHERE id_restitucion = v_id_restitucion
        AND id_expediente = v_id_expediente;
     --TRACE '8';
     -- Registro para reporte

      SELECT a.observacion
      INTO   v_observacion  -- nss a devolver segun carga
      FROM   sep_restitucion a
      WHERE  a.id_restitucion = v_id_restitucion ;

     INSERT INTO safre_tmp:tmp_sep_restitucion
           (id_expediente,
            invadido,
            asociado,
            nss_restitucion,
            aivs_sar92,
            pesos_sar92,
            aivs_viv97,
            pesos_viv97,
            aivs_subsc,
            pesos_subsc,
            tipo_restitucion)
     VALUES(v_id_expediente,
            v_nss_inv,
            v_nss_asoc,
            v_observacion,
            v_sar92_aivs_devolver,
            v_sar92_pesos_devolver,
            v_viv97_aivs_devolver,
            v_viv97_pesos_devolver,
            v_subsc_viv97_aivs_devolver,
            v_subsc_viv97_pesos_devolver,
            'ANÁLISIS');
      --TRACE '9';
      LET v_total_procesados = v_total_procesados + 1;
         				 
   END FOREACH
   
   FOREACH SELECT id_115_restitucion,
                  id_restitucion,
                  id_expediente,
                  nss_115,
                  sar92_pesos_115_trabajador,
                  sar92_aivs_115_trabajador,
                  viv97_pesos_115_trabajador,
                  viv97_aivs_115_trabajador,
                  subsc_viv97_pesos_115_trabajador,
                  subsc_viv97_aivs_115_trabajador,
                  sar92_pesos_115_acreditado,
                  sar92_aivs_115_acreditado,
                  viv97_pesos_115_acreditado,
                  viv97_aivs_115_acreditado,
                  subsc_viv97_pesos_115_acreditado,
                  subsc_viv97_aivs_115_acreditado
             INTO v_id_115_restitucion,
                  v_id_restitucion,
                  v_id_expediente,
                  v_nss_115,
                  v_sar92_pesos_115_trabajador,
                  v_sar92_aivs_115_trabajador,
                  v_viv97_pesos_115_trabajador,
                  v_viv97_aivs_115_trabajador,
                  v_subsc_viv97_pesos_115_trabajador,
                  v_subsc_viv97_aivs_115_trabajador,
                  v_sar92_pesos_115_acreditado,
                  v_sar92_aivs_115_acreditado,
                  v_viv97_pesos_115_acreditado,
                  v_viv97_aivs_115_acreditado,
                  v_subsc_viv97_pesos_115_acreditado,
                  v_subsc_viv97_aivs_115_acreditado
             FROM sep_115_restitucion
            WHERE ind_restitucion = 1
   
      SELECT id_derechohabiente,
             nss,
             num_credito,
             tpo_credito
        INTO v_id_derechohabiente_inv,
             v_nss_inv,
             v_num_credito_inv,
             v_tpo_credito_inv
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 1;
      				 
      SELECT id_derechohabiente,
             nss,
             num_credito,
      	     tpo_credito
        INTO v_id_derechohabiente_asoc,
             v_nss_asoc,
             v_num_credito_asoc,
             v_tpo_credito_asoc
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 2;
         
      -- Registro de subcuenta viv97 Invadido
      IF v_viv97_pesos_115_trabajador > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_asoc,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,   -- folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_viv97_aivs_115_trabajador,  
                v_viv97_pesos_115_trabajador,
                NULL,
                NULL,
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_asoc,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_viv97_aivs_115_trabajador,
                v_viv97_pesos_115_trabajador,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "BASE 115-V97",
                0);
      END IF

      -- Registro de subcuenta sar92 Invadido
     
      IF v_sar92_pesos_115_trabajador > 0 THEN  

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_asoc,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,    -- folio_referencia
                v_subcuenta_92,      -- subcuenta
                v_sar92_aivs_115_trabajador,  
                v_sar92_pesos_115_trabajador,
                NULL, 
                NULL, 
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion, --ok
                TODAY, --ok
                v_id_derechohabiente_asoc,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_sar92_aivs_115_trabajador,
                v_sar92_pesos_115_trabajador,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "BASE 115-V92",
                0);
      END IF

      -- Registro de subcuenta sar92 Invadido
      IF v_subsc_viv97_pesos_115_trabajador > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_asoc,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,   -- folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_subsc_viv97_aivs_115_trabajador,  
                v_subsc_viv97_pesos_115_trabajador,
                NULL, 
                NULL, 
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_asoc,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_subsc_viv97_aivs_115_trabajador,
                v_subsc_viv97_pesos_115_trabajador,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "BASE 115-SBCV97",
                0);
      END IF

      -- Registro de subcuenta viv97 asociado
      IF v_viv97_pesos_115_acreditado > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_inv,
                v_num_credito_asoc,
                v_tpo_credito_asoc,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,   -- folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_viv97_aivs_115_acreditado,  
                v_viv97_pesos_115_acreditado,
                NULL,
                NULL,
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_viv97_aivs_115_acreditado,
                v_viv97_pesos_115_acreditado,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "BASE 115-V97",
                0);
      END IF
  
      -- Registro de subcuenta sar92 Asociado

      IF v_sar92_pesos_115_acreditado > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_inv,
                v_num_credito_asoc,
                v_tpo_credito_asoc,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,    -- folio_referencia
                v_subcuenta_92,      -- subcuenta
                v_sar92_aivs_115_acreditado,  
                v_sar92_pesos_115_acreditado,
                NULL, 
                NULL, 
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_sar92_aivs_115_acreditado,
                v_sar92_pesos_115_acreditado,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "BASE 115-V92",
                0);
      END IF

      -- Registro de subcuenta viv97 asociado
      IF v_subsc_viv97_pesos_115_acreditado > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_inv,
                v_num_credito_asoc,
                v_tpo_credito_asoc,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,   -- folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_subsc_viv97_aivs_115_acreditado,  
                v_subsc_viv97_pesos_115_acreditado,
                NULL,
                NULL,
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_subsc_viv97_aivs_115_acreditado,
                v_subsc_viv97_pesos_115_acreditado,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "BASE 115-SBCV97",
                0);
      END IF

      -- Actualiza el registro, para indicar que ya se proceso
      UPDATE sep_115_restitucion
         SET ind_restitucion = 2 -- RESTITUCION REGISTRADA
       WHERE id_115_restitucion = v_id_115_restitucion
         AND id_expediente = v_id_expediente;

      UPDATE sep_restitucion
         SET folio = r_folio
       WHERE id_restitucion = v_id_restitucion
         AND id_expediente = v_id_expediente;

      --Registro para reporte
    	INSERT INTO safre_tmp:tmp_sep_restitucion
            (id_expediente,
             invadido,
             asociado,
             nss_restitucion,
             aivs_sar92,
             pesos_sar92,
             aivs_viv97,
             pesos_viv97,
             aivs_subsc,
             pesos_subsc,
             tipo_restitucion)
      VALUES(v_id_expediente,
             v_nss_inv,
             v_nss_asoc,
             v_nss_115, -- nss restitucion
             v_sar92_aivs_115_trabajador,
             v_sar92_pesos_115_trabajador,
             v_viv97_aivs_115_trabajador,
             v_viv97_pesos_115_trabajador,
             v_subsc_viv97_aivs_115_trabajador,
             v_subsc_viv97_pesos_115_trabajador,
             'BASE 115 - 28');

      -- Registro para reporte
    	INSERT INTO safre_tmp:tmp_sep_restitucion
            (id_expediente,
             invadido,
             asociado,
             nss_restitucion,
             aivs_sar92,
             pesos_sar92,
             aivs_viv97,
             pesos_viv97,
             aivs_subsc,
             pesos_subsc,
             tipo_restitucion)
      VALUES(v_id_expediente,
             v_nss_inv,
             v_nss_asoc,
             v_nss_115, -- nss restitucion
             v_sar92_aivs_115_acreditado,
             v_sar92_pesos_115_acreditado,
             v_viv97_aivs_115_acreditado,
             v_viv97_pesos_115_acreditado,
             v_subsc_viv97_aivs_115_acreditado,
             v_subsc_viv97_pesos_115_acreditado,
             'BASE 115 - 28');

      LET v_total_procesados = v_total_procesados + 2;
   END FOREACH
   
   FOREACH SELECT id_mto_restitucion_no_aplicados,
                  id_expediente,
                  id_restitucion,
                  subsc_viv97_pesos_no_aplica,
                  subsc_viv97_aivs_no_aplica,
                  subsc_viv97_pesos_no_aplica_pscd,
                  subsc_viv97_aivs_no_aplica_pscd
             INTO v_id_mto_restitucion_no_aplicados,
                  v_id_expediente,
                  v_id_restitucion,
                  v_subsc_viv97_pesos_no_aplica,
                  v_subsc_viv97_aivs_no_aplica,
                  v_subsc_viv97_pesos_no_aplica_pscd,
                  v_subsc_viv97_aivs_no_aplica_pscd
             FROM sep_mto_restitucion_no_aplicados
            WHERE ind_restitucion = 1

      SELECT id_derechohabiente,
             nss,
             num_credito,
             tpo_credito
        INTO v_id_derechohabiente_inv,
             v_nss_inv,
             v_num_credito_inv,
             v_tpo_credito_inv
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 1;
      				 
      SELECT id_derechohabiente,
             nss,
             num_credito,
      	     tpo_credito
        INTO v_id_derechohabiente_asoc,
             v_nss_asoc,
             v_num_credito_asoc,
             v_tpo_credito_asoc
        FROM sep_nss_expediente
       WHERE id_expediente = v_id_expediente
         AND tipo_nss = 2;
         
      -- Registro de subcuenta viv97 
      IF v_subsc_viv97_aivs_no_aplica > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_inv,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,    -- folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_subsc_viv97_aivs_no_aplica,  
                v_subsc_viv97_pesos_no_aplica,
                NULL,
                NULL,
                v_observacion,
                10); -- estado
         
         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_subsc_viv97_aivs_no_aplica,
                v_subsc_viv97_pesos_no_aplica,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "NO APLICADOS PSCD",
                0);
      END IF 


      IF v_subsc_viv97_pesos_no_aplica_pscd > 0 THEN

         SELECT a.observacion
           INTO v_observacion  -- nss a devolver segun carga
           FROM sep_restitucion a
          WHERE a.id_restitucion = v_id_restitucion ;
         
         INSERT INTO dse_devolucion
               (id_dse_devolucion,
                folio,
                modulo_cod,
                id_derechohabiente,
                num_credito,
                tpo_transferencia,
                origen_devolucion,
                f_pago,
                f_movimiento,
                periodo_pago,
                folio_referencia,
                subcuenta,
                monto_aivs,
                monto_pesos,
                monto_aportacion,
                aivs_aportacion,
                nss_separacion,
                estado)
         VALUES(seq_dse_devolucion.NEXTVAL,
                0,
                "sep",
                v_id_derechohabiente_inv,
                v_num_credito_inv,
                v_tpo_credito_inv,
                NULL, -- Revisar explicacion origen devolucion
                NULL,
                v_f_movimiento_dev, -- revisar como obtener f_movimiento
                NULL,
                r_folio,   -- folio_referencia
                v_subcuenta_97,      -- subcuenta
                v_subsc_viv97_aivs_no_aplica_pscd,  
                v_subsc_viv97_pesos_no_aplica_pscd,
                NULL,
                NULL,
                v_observacion,
                10); -- estado

         INSERT INTO sep_mov_deudor
               (id_restitucion,
                f_liquida ,
                id_derechohabiente,
                movimiento,
                folio_liquida ,
                id_referencia ,
                monto_acciones,
                monto_pesos,
                f_valor,
                f_registro ,
                origen,
                folio_respuesta_deudor)
         VALUES(v_id_restitucion   , --ok
                TODAY , --ok
                v_id_derechohabiente_inv,
                381, -- ABONO POR RESTITUCIÓN SEPARACIÓN
                r_folio, -- folio de control generado en este SP y que luego se insert en sep_ctr_restitucion 
                v_id_expediente,
                v_subsc_viv97_aivs_no_aplica_pscd,
                v_subsc_viv97_pesos_no_aplica_pscd,
                v_rest_fecha_actual,
                v_rest_fecha_actual,
                "NO APLICADOS PSCD",
                0);
      END IF

      -- Actualiza el registro, para indicar que ya se proceso
      UPDATE sep_mto_restitucion_no_aplicados
         SET ind_restitucion = 2 -- RESTITUCION REGISTRADA
       WHERE id_mto_restitucion_no_aplicados = v_id_mto_restitucion_no_aplicados
         AND id_expediente = v_id_expediente;

      -- se actualiza el folio generado en el expediente procesado
      UPDATE sep_mto_restitucion_no_aplicados
         SET folio_no_aplicados = r_folio
       WHERE id_mto_restitucion_no_aplicados = v_id_mto_restitucion_no_aplicados
         AND id_expediente = v_id_expediente;

      UPDATE sep_restitucion
         SET folio = r_folio
       WHERE id_restitucion = v_id_restitucion
         AND id_expediente = v_id_expediente;

      -- Registro para reporte

      SELECT a.observacion
        INTO v_observacion  -- nss a devolver segun carga
        FROM sep_restitucion a
       WHERE a.id_restitucion = v_id_restitucion ;

    	INSERT INTO safre_tmp:tmp_sep_restitucion
            (id_expediente,
             invadido,
             asociado,
             nss_restitucion,
             aivs_sar92,
             pesos_sar92,
             aivs_viv97,
             pesos_viv97,
             aivs_subsc,
             pesos_subsc,
             tipo_restitucion)
      VALUES(v_id_expediente,
             v_nss_inv,
             v_nss_asoc,
             v_observacion,
             NULL,
             NULL,
             v_subsc_viv97_aivs_no_aplica,
             v_subsc_viv97_pesos_no_aplica,
             v_subsc_viv97_aivs_no_aplica_pscd,
             v_subsc_viv97_pesos_no_aplica_pscd,
             'NO APLICADOS AL CREDITO');
      LET v_total_procesados = v_total_procesados + 1;
            
   END FOREACH
   
   -- Inserta El registro de control para el folio del proceso
   INSERT INTO sep_ctr_restitucion
         (folio,
          f_proceso)
   VALUES(r_folio,
          TODAY);

   LET v_precio_fondo = 0;
   --LET v_fecha_val = TODAY;

   -- Fecha de valuación al día 12 del mes
   EXECUTE FUNCTION fn_habil_mensual(12) INTO v_fecha_val;

   SELECT a.precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo a
    WHERE a.f_valuacion = v_fecha_val
      AND a.fondo       = 11;

   UPDATE dse_devolucion
   SET    monto_pesos = monto_aivs * v_precio_fondo
   WHERE  folio_referencia = r_folio
   AND    folio            = 0
   AND    modulo_cod       = "sep";
   
   -- Actualiza el monto pesos para que se refleje en el reporte (sar92, viv97 y subsecuentes)
   UPDATE safre_tmp:tmp_sep_restitucion
      SET pesos_sar92 = aivs_sar92 * v_precio_fondo,
          pesos_viv97 = aivs_viv97 * v_precio_fondo,
          pesos_subsc = aivs_subsc * v_precio_fondo
    WHERE 1 = 1;

   RETURN v_total_procesados, 
          r_folio,
          v_sql_error,
          v_msg_error;

END FUNCTION;


