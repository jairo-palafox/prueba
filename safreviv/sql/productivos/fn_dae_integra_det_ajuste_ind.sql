






CREATE FUNCTION "safreviv".fn_dae_integra_det_ajuste_ind(p_usuario    CHAR(20),
                                              p_pid            DECIMAL(9,0),
                                              p_nombre_archivo CHAR(40),
                                              p_folio          DECIMAL(9,0),
                                              p_proceso_cod    SMALLINT)

   RETURNING INTEGER,
             INTEGER,
             CHAR(200),
             INTEGER,
             INTEGER,
             INTEGER

   --Variables de tabla temporal
   DEFINE v_tot_regs_insertados    INTEGER;
   DEFINE v_total_rechazados       INTEGER;
   DEFINE v_total_aceptados        INTEGER;

   DEFINE v_precio_fondo           DECIMAL(16,6);
   DEFINE v_fondo_inversion        SMALLINT;

   DEFINE v_tmp_tipo_registro      CHAR(2) ;
   DEFINE v_tmp_numero_credito     CHAR(10);
   DEFINE v_tmp_fecha_pago         CHAR(8) ;
   DEFINE v_tmp_d_fecha_pago       DATE;
   DEFINE v_tmp_periodo_pago       CHAR(4) ;
   DEFINE v_tmp_fecha_reg_pago     CHAR(8) ;
   DEFINE v_tmp_imp_amortizacion   DECIMAL(12,2);
   DEFINE v_tmp_nss                CHAR(11);
   DEFINE v_resultado_fecha        SMALLINT;
   --Variables de rechazo
   DEFINE v_rch_folio              DECIMAL(9,0);
   DEFINE v_rch_tipo_registro      SMALLINT;
   DEFINE v_rch_resul_opera        CHAR(2);
   DEFINE v_rch_diagnostico        SMALLINT;
   DEFINE v_rch_campo_valor        CHAR(50);

   --Variables de tabla de detalles
   DEFINE v_resul_operacion        SMALLINT;
   DEFINE v_nss_ajustar            CHAR(11);
   DEFINE v_diagnostico            SMALLINT;
   DEFINE v_id_derechohabiente_afi DECIMAL(9,0);
   DEFINE v_det_status_retiro      SMALLINT;
   DEFINE v_det_id_dae_referencia  DECIMAL(9,0);
   DEFINE v_det_id_referencia      DECIMAL(9,0);

   --Saldos y marcas
   DEFINE v_resultado              SMALLINT;
   DEFINE v_saldo_aivs             DECIMAL(16,6);
   DEFINE v_saldo_pesos            DECIMAL(16,6);
   DEFINE v_marca_ajuste           SMALLINT;
   DEFINE v_resultado_marca        SMALLINT;
   DEFINE v_i_estado_marca         INTEGER;
   DEFINE v_marca_inhabilita       SMALLINT;
   DEFINE v_importe_amort          DECIMAL(16,6);
   DEFINE v_monto_aivs             DECIMAL(16,6);
   DEFINE v_fecha_pago             DATE;
   DEFINE v_tmp_d_imp_amortizacion DECIMAL(16,2);
   -- Control de Excepciones
   DEFINE sql_err                  INTEGER;
   DEFINE isam_err                 INTEGER;
   DEFINE v_isam_err               INTEGER;
   DEFINE err_txt                  CHAR(200);
   DEFINE v_i_resultado            SMALLINT;
   DEFINE v_secuencia_ajuste       DECIMAL(9,0);
   DEFINE v_secuencia_aceptados    DECIMAL(9,0);
   DEFINE v_det_id_derechohabiente DECIMAL(9,0);
   DEFINE v_det_nss                CHAR(11);
   --Solo aceptados
   DEFINE v_acep_id_dae_ref_aceptados DECIMAL(9,0) ;
   DEFINE v_acep_id_dae_ref_ajuste    DECIMAL(9,0) ;
   DEFINE v_acep_id_dae_referencia    DECIMAL(9,0) ;
   DEFINE v_acep_folio_liquida        DECIMAL(9,0) ;
   DEFINE v_acep_fecha_liquida        DATE         ;
   DEFINE v_acep_monto_pesos          DECIMAL(16,6);
   DEFINE v_acep_monto_acciones       DECIMAL(16,6);

   --Datos de movimiento
   DEFINE v_movimiento                SMALLINT;
   DEFINE v_folio_liquida             DECIMAL(9,0);
   DEFINE v_f_liquida                 DATE;
   DEFINE v_fecha_liquidacion         DATE;
   DEFINE v_id_dh_solicitud           DECIMAL(9,0);
   DEFINE bnd_desmarca                INTEGER;

   ON EXCEPTION SET sql_err, isam_err, err_txt

      LET v_i_resultado = sql_err;
      LET v_isam_err = isam_err;

      RETURN v_i_resultado,
             isam_err,
             err_txt,
             v_tot_regs_insertados,
             v_total_rechazados,
             v_total_aceptados;
   END EXCEPTION

   --SET DEBUG FILE TO "/safreviv_int/BD/dae_integra_ajustes.trace";
   --TRACE ON;

   --Iniciar variables
   LET v_resul_operacion       = "";
   LET v_diagnostico           = 0;
   LET v_tot_regs_insertados   = 0;
   LET v_i_resultado           = 0;
   LET err_txt                 = "Ok";
   LET v_isam_err              = 0;
   LET v_total_rechazados      = 0;
   LET v_total_aceptados       = 0;
   LET v_fondo_inversion       = 11;
   LET v_precio_fondo          = 0;
   LET v_det_status_retiro     = "";
   LET v_secuencia_ajuste      = 0;
   LET v_secuencia_aceptados   = 0;
   LET v_det_id_dae_referencia = 0;
   LET v_det_id_referencia     = 0;
   LET v_fecha_liquidacion     = NULL;
   LET v_tmp_d_fecha_pago      = NULL;
   LET bnd_desmarca            = 0;

FOREACH
   SELECT tipo_registro   ,
          numero_credito  ,
          fecha_pago      ,
          periodo_pago    ,
          fecha_reg_pago  ,
          importe_amortizacion/100,
          nss
   INTO   v_tmp_tipo_registro   ,
          v_tmp_numero_credito  ,
          v_tmp_fecha_pago      ,
          v_tmp_periodo_pago    ,
          v_tmp_fecha_reg_pago  ,
          v_tmp_imp_amortizacion,
          v_tmp_nss
   FROM   safre_tmp:tmp_dae_detalle_ajuste_ind

   SELECT seq_dae_det_ajuste.NEXTVAL
   INTO   v_secuencia_ajuste
   FROM systables
   WHERE tabid = 1;

   -- Cambia formato de fecha de YYYYMMDD a MMDDYYYY
   EXECUTE PROCEDURE sp_cambia_formato_fecha (p_proceso_cod,v_tmp_fecha_pago)
           INTO  v_resultado_fecha,
                 v_tmp_d_fecha_pago;


   IF v_tmp_nss = "00000000000" THEN
         --Valores para dae_rch_archivo
         LET v_rch_folio             = p_folio;
         LET v_rch_tipo_registro     = 2;
         LET v_rch_resul_opera       = "02";
         LET v_rch_diagnostico       = 2;
         LET v_rch_campo_valor       = "NSS INVALIDO 00000000000";

         --Rechazos en ajuste
         LET v_resul_operacion = 2;
         LET v_diagnostico     = 2;

         LET v_total_rechazados = v_total_rechazados + 1;
   ELSE
      --Obtiene el id_derechohabiente de SAFRE
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_afi
      FROM   afi_derechohabiente
      WHERE  nss = v_tmp_nss;

      --Si el Derechohabiente no existe se inserta RECHAZO;
      IF v_id_derechohabiente_afi IS NULL THEN
         LET v_rch_folio             = p_folio;
         LET v_rch_tipo_registro     = 2;
         LET v_rch_resul_opera       = "02";
         LET v_rch_diagnostico       = 1;
         LET v_rch_campo_valor       = "NO EXISTE NSS EN SACI";

         --Rechazos en ajuste
         LET v_resul_operacion = 2;
         LET v_diagnostico     = 1;

         LET v_total_rechazados = v_total_rechazados + 1;
      ELSE
         LET v_id_dh_solicitud = NULL;
         LET v_fecha_liquidacion = NULL;
         LET v_det_status_retiro = NULL ;

         SELECT id_derechohabiente, MAX(fecha_liquida), status_retiro
         INTO   v_id_dh_solicitud, v_fecha_liquidacion, v_det_status_retiro
         FROM   dae_det_solicitud
         WHERE  id_derechohabiente= v_id_derechohabiente_afi
         AND    num_credito   = v_tmp_numero_credito
         AND    fecha_pago    = v_tmp_d_fecha_pago
         AND    periodo_pago  = v_tmp_periodo_pago
         AND    registro_pago  = v_tmp_fecha_reg_pago
         AND    importe_amort = v_tmp_imp_amortizacion
         AND    nss = v_tmp_nss
         AND    resul_opera = "01"
         AND    folio_liquida IS NOT NULL
         AND    folio_ajuste IS NULL
         AND    (status_retiro = 1
         OR      status_retiro = 3)
         GROUP BY  1,3;

         --Si id_derechohabiente no tiene amortizacion
         IF v_id_dh_solicitud IS NULL THEN
            LET v_rch_folio             = p_folio;
            LET v_rch_tipo_registro     = 2;
            LET v_rch_resul_opera       = "02";
            LET v_rch_diagnostico       = 16;
            LET v_rch_campo_valor       = "NO TIENE AMORTIZACION";

            --Rechazos en ajuste
            LET v_resul_operacion = 2;
            LET v_diagnostico     = 16;

            LET v_total_rechazados = v_total_rechazados + 1;
         ELSE
            --Si DH existe consulta saldo
            EXECUTE FUNCTION fn_saldo_dia(v_tmp_nss, v_id_derechohabiente_afi,46, TODAY)
            INTO v_resultado, v_saldo_aivs, v_saldo_pesos;

            LET v_marca_ajuste = 403;
            --Si saldo > 0
            --IF v_resultado = 0 AND v_saldo_pesos > 0 THEN
            IF v_resultado = 0 AND v_saldo_aivs > 0 THEN --Se cambia validacion de pesos por validacion de AIVS --AG 26/08/2014.
               LET v_resul_operacion = 1;

               --Marca cuenta 1
               EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_afi,
                                                v_marca_ajuste,
                                                v_secuencia_ajuste,
                                                p_folio,
                                                0, -- estado marca
                                                0, -- codigo de rechazo
                                                0, -- marca de la causa
                                                NULL, -- fecha de la causa
                                                p_usuario,
                                                p_proceso_cod)
               INTO v_i_estado_marca;

               --Si la cuenta se marca
               IF v_i_estado_marca = 0 THEN

                  --Consulta si ya se ejecuto algon retiro.
                  {SELECT MAX (fecha_liquida)
                  INTO   v_fecha_liquidacion
                  FROM   dae_det_solicitud
                  WHERE  id_derechohabiente = v_id_derechohabiente_afi
                  AND    nss = v_tmp_nss
                  AND    resul_opera = "01"
                  AND    folio_ajuste IS NULL;

                  SELECT status_retiro
                  INTO   v_det_status_retiro
                  FROM   dae_det_solicitud
                  WHERE  id_derechohabiente = v_id_derechohabiente_afi
                  AND    fecha_liquida = v_fecha_liquidacion
                  AND    resul_opera = "01"
                  AND    periodo_pago = v_tmp_periodo_pago
                  AND    folio_ajuste IS NULL
                  GROUP BY 1;}

                  --Acepta todas las solicitudes
                  LET v_resul_operacion = 1;
                  LET v_diagnostico     = 13;

                  --oExiste retiro?
                  IF v_det_status_retiro = 4 THEN
                     LET v_resul_operacion = 1;
                     LET v_diagnostico     = 11;
                  END IF

                  --oExiste restitucion?
                  IF v_det_status_retiro = 3 THEN
                     LET v_resul_operacion = 1;
                     LET v_diagnostico     = 12;
                  END IF

--########################################################
                  IF v_det_status_retiro = 1 OR v_det_status_retiro = 3 THEN
                     LET v_det_id_derechohabiente = NULL;
                     LET v_det_nss                = NULL;
                     LET v_det_id_dae_referencia  = NULL;
                     LET v_importe_amort          = NULL;
                     LET v_monto_aivs             = NULL;
                     LET v_fecha_pago             = NULL;

                     FOREACH
                        --Consulta si derechohabiente tiene amortizacion
                        SELECT id_derechohabiente,
                               nss,
                               id_dae_referencia,
                               importe_amort,
                               monto_aivs,
                               fecha_pago
                        INTO   v_det_id_derechohabiente,
                               v_det_nss,
                               v_det_id_dae_referencia,
                               v_importe_amort,
                               v_monto_aivs,
                               v_fecha_pago
                        FROM   dae_det_solicitud
                        WHERE  id_derechohabiente= v_id_derechohabiente_afi
                        AND    num_credito   = v_tmp_numero_credito
                        AND    fecha_pago    = v_tmp_d_fecha_pago
                        AND    periodo_pago  = v_tmp_periodo_pago
                        AND    registro_pago  = v_tmp_fecha_reg_pago
                        AND    importe_amort = v_tmp_imp_amortizacion
                        AND    nss = v_tmp_nss
                        AND    resul_opera = "01"
                        AND    folio_liquida IS NOT NULL
                        AND    folio_ajuste IS NULL
                        AND    (status_retiro = 1
                        OR      status_retiro = 3)

                        SELECT seq_dae_aceptados_ajuste.NEXTVAL
                        INTO   v_secuencia_aceptados
                        FROM   systables
                        WHERE  tabid = 1;

                        INSERT INTO dae_aceptados_ajuste
                                (
                                id_dae_ref_aceptados,  --Referencia tabla aceptados
                                id_dae_ref_ajuste   ,  --Referencia tabla detalles de ajuste
                                id_dae_referencia   ,  --Referencia dae_det_solicitud
                                monto_pesos         ,
                                monto_acciones      ,
                                folio_lote_ajuste   ,
                                fecha_valor
                                )
                        VALUES (
                                v_secuencia_aceptados   ,
                                v_secuencia_ajuste      ,
                                v_det_id_dae_referencia ,
                                v_importe_amort         ,
                                v_monto_aivs            ,
                                p_folio                 , --folio del lote de ajuste
                                v_fecha_pago
                                );

                        --Actualiza el folio en la tabla de detalles
                        UPDATE dae_det_solicitud
                        SET    folio_ajuste = p_folio
                        WHERE  id_derechohabiente = v_id_derechohabiente_afi
                        AND    id_dae_referencia = v_det_id_dae_referencia;

                        LET v_total_aceptados = v_total_aceptados + 1;
                     END FOREACH;
                  ELSE
                     IF v_det_status_retiro = 2 THEN
                        --Si la cuenta ya tiene ajuste
                        LET v_rch_folio             = p_folio;
                        LET v_rch_tipo_registro     = 2;
                        LET v_rch_resul_opera       = "02";
                        LET v_rch_diagnostico       = 14;
                        LET v_rch_campo_valor       = "LA CUENTA YA FUE AJUSTADA";

                        --Rechazos en ajuste
                        LET v_resul_operacion = 2;
                        LET v_diagnostico     = 14;

                        LET v_total_rechazados = v_total_rechazados + 1;
                     ELSE
                        --Si la cuenta ya tiene retiro
                        LET v_rch_folio             = p_folio;
                        LET v_rch_tipo_registro     = 2;
                        LET v_rch_resul_opera       = "02";
                        LET v_rch_diagnostico       = 15;
                        LET v_rch_campo_valor       = "LA CUENTA TIENE RETIRO";

                        --Rechazos en ajuste
                        LET v_resul_operacion = 2;
                        LET v_diagnostico     = 15;

                        LET v_total_rechazados = v_total_rechazados + 1;

                        EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_afi,
                                                            v_marca_ajuste          ,
                                                            v_secuencia_ajuste      ,
                                                            0                       ,
                                                            0                       ,
                                                            p_usuario               ,
                                                            p_proceso_cod )
                        INTO bnd_desmarca;
                     END IF
                  END IF
--########################################################
               --Si la cuenta NO SE PUDO MARCAR.
               ELSE
                  LET v_rch_folio             = p_folio;
                  LET v_rch_tipo_registro     = 2;
                  LET v_rch_resul_opera       = "02";
                  LET v_rch_diagnostico       = 10;
                  LET v_rch_campo_valor       = "CONVIVENCIA DE MARCA";

                  --Se rechaza por marca activa
                  LET v_resul_operacion = 2;
                  LET v_diagnostico     = 10;

                  LET v_total_rechazados = v_total_rechazados + 1;
               END IF ---Si la cuenta se marca

            ELSE
               --Si la cuenta no tiene saldo
               LET v_rch_folio             = p_folio;
               LET v_rch_tipo_registro     = 2;
               LET v_rch_resul_opera       = "02";
               LET v_rch_diagnostico       = 9;
               LET v_rch_campo_valor       = "CUENTA SIN SALDO";

               --Rechazos en ajuste
               LET v_resul_operacion = 2;
               LET v_diagnostico     = 9;

               LET v_total_rechazados = v_total_rechazados + 1;
            END IF --Si saldo > 0
         END IF -- Si DH no tiene amortizacion
      END IF --IF dh IS NULL
   END IF--IF NSS = 00000000000

   --Se aceptan todos los registrso y se inserta en tabla de detalles _
   INSERT INTO dae_det_ajuste
               (
               id_dae_ref_ajuste,
               id_dae_referencia,
               id_derechohabiente,
               nss,
               folio_lote,
               resul_operacion,
               diagnostico
               )
        VALUES (
                v_secuencia_ajuste,
                v_det_id_dae_referencia,
                v_id_derechohabiente_afi,
                v_tmp_nss,
                p_folio,
                v_resul_operacion,
                v_diagnostico
                );

      IF v_resul_operacion = 2 THEN
         INSERT INTO dae_rch_archivo
         VALUES (
                 seq_dae_rch_archivo.NEXTVAL,
                 v_rch_folio,
                 v_rch_tipo_registro,
                 v_secuencia_ajuste,
                 v_rch_resul_opera,
                 v_rch_diagnostico,
                 v_rch_campo_valor
                );
      END IF

      LET v_tot_regs_insertados = v_tot_regs_insertados + 1;
END FOREACH

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio     = p_folio,
          estado    = 2 -- integrado
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 1   -- etapa de carga
   AND    estado      = 1   -- archivo cargado
   AND    nombre_archivo = p_nombre_archivo;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE statistics FOR TABLE dae_rch_archivo;
   UPDATE statistics FOR TABLE dae_det_ajuste;
   UPDATE statistics FOR TABLE dae_aceptados_ajuste;

 RETURN v_i_resultado,
        v_isam_err,
        err_txt,
        v_tot_regs_insertados, -- Totales
        v_total_rechazados,    -- Rechazadas
        v_total_aceptados;     -- Aceptados
END FUNCTION;


