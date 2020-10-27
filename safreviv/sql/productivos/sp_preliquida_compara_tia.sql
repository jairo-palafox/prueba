






CREATE PROCEDURE "safreviv".sp_preliquida_compara_tia(p_folio DECIMAL(9,0), p_usuario CHAR(20))
RETURNING INTEGER, INTEGER, VARCHAR(250)

   --tia movimiento datos a insertar
   DEFINE tia_mov_decreto_f_liquida                   DATE;
   DEFINE tia_mov_decreto_id_decreto                  DECIMAL(9,0);
   DEFINE tia_mov_decreto_v_rec_cta_decreto_subcuenta SMALLINT;
   DEFINE tia_mov_decreto_fondo_inversion             SMALLINT;
   DEFINE tia_mov_decreto_movimiento                  SMALLINT;
   DEFINE tia_mov_decreto_folio_liquida               DECIMAL(9,0);
   DEFINE tia_mov_decreto_id_referencia               DECIMAL(9,0);
   DEFINE tia_mov_decreto_monto_acciones              DECIMAL(22,2);
   DEFINE tia_mov_decreto_monto_pesos                 DECIMAL(22,2);
   DEFINE tia_mov_decreto_f_valor                     DATE;
   DEFINE tia_mov_decreto_f_registro                  DATE;
   DEFINE tia_mov_decreto_h_registro                  DATETIME HOUR TO SECOND;
   DEFINE tia_mov_decreto_origen                      CHAR(20);
--===================================================================================================
   --acumala los importes (monto_pesos) de tia_movimiento
   DEFINE  v_acumulado                                          DECIMAL(16,2)           ;
--===================================================================================================
   --tia movimiento datos a insertar
   DEFINE tia_mov_f_liquida                   DATE;
   DEFINE tia_mov_id_decreto                  DECIMAL(9,0);
   DEFINE tia_mov_v_rec_cta_decreto_subcuenta SMALLINT;
   DEFINE tia_mov_fondo_inversion             SMALLINT;
   DEFINE tia_mov_movimiento                  SMALLINT;
   DEFINE tia_mov_folio_liquida               DECIMAL(9,0);
   DEFINE tia_mov_id_referencia               DECIMAL(9,0);
   DEFINE tia_mov_monto_acciones              DECIMAL(22,2);
   DEFINE tia_mov_monto_pesos                 DECIMAL(22,2);
   DEFINE tia_mov_f_valor                     DATE;
   DEFINE tia_mov_f_registro                  DATE;
   DEFINE tia_mov_h_registro                  DATETIME HOUR TO SECOND;
   DEFINE tia_mov_origen                      CHAR(20);

   --importe de tia_det_traspaso
   DEFINE v_ti_det_sdo_viv92 DECIMAL(22,2);
   DEFINE v_ti_det_int_viv92 DECIMAL(14,2);
   DEFINE v_ind_consistencia SMALLINT;

--===================================================================================================
  --acumala los importes (monto_pesos) de tia_movimiento
   DEFINE v_suma_tia_det      DECIMAL(16,2);
   DEFINE v_monto_pesos       DECIMAL(16,2);
   DEFINE v_monto_pesos_cargo DECIMAL(16,2);

--===================================================================================================

   DEFINE tia_preliquida_tia_folio_liquida   DECIMAL(9,0);
   DEFINE tia_preliquida_tia_id_decreto      DECIMAL(9,0);
   DEFINE tia_preliquida_tia_id_referencia   DECIMAL(9,0);
   DEFINE tia_preliquida_tia_monto_acciones  DECIMAL(22,2);
   DEFINE tia_preliquida_tia_f_movimiento    DATE;
   DEFINE tia_preliquida_tiadt_rfc_afo_recep CHAR(13);
   DEFINE tia_preliquida_tia_nci_icefa       CHAR(30);
   DEFINE v_fecha_today                      DATE;
   DEFINE v_id_decreto                       DECIMAL(9,0);
   DEFINE v_id_referencia                    DECIMAL(9,0);
   DEFINE v_id_decreto_tia_det_traspaso      DECIMAL(9,0);
   DEFINE v_id_referencia_tia_det_traspaso   DECIMAL(9,0);
   DEFINE v_aivs_viv92                       DECIMAL(16,2);   DEFINE v_nss_afo_recep                    CHAR(11);
   DEFINE v_origen_traspaso                  CHAR(02);
   DEFINE v_curp                             CHAR(18);

   --DEFINE v_contador_detalle               INTEGER;
   --DEFINE v_contador_mov                   INTEGER;

   -- variables para recepcion de errores
   DEFINE v_error_sql  INTEGER;
   DEFINE v_error_isam INTEGER;
   DEFINE v_mensaje    VARCHAR(250);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_mensaje

      -- se devuelve el error
      RETURN v_error_sql, v_error_isam, v_mensaje;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv/tialog.dir/sp_preliquida_trapasos_tia.txt';
   --trace 'Inicia Preliquidacion con Folio:' || p_folio ||" - Fecha:" || TODAY;

   -- se asume que no hay errores
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_mensaje    = "Preliquidacion TIA finaliza correctamente";

   --trace "Proceso 2: Termina FOREACH y actualiza folios";
   --Se actualiza la tabla que lleva el control de los folios
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio = p_folio;

   LET v_ti_det_sdo_viv92    = 0;
   LET v_ti_det_int_viv92    = 0;
   LET v_suma_tia_det        = 0;
   LET v_monto_pesos         = 0;
   LET v_monto_pesos_cargo   = 0;
   LET v_acumulado           = 0;
   LET v_fecha_today         = TODAY ;
   LET tia_preliquida_tia_monto_acciones = 0;

---======================================================================================
 --se inicializa la seleccion  de tia_movimiento

   FOREACH
            SELECT tia.folio,
             tia.id_referencia,
             tia.f_movimiento,
             tia.id_decreto,
             tia.nci_icefa,
             tia.sdo_viv92,
             tia.aivs_viv92, -- aivs 19 sep 2012
             afi.ind_consistencia,
             tia.nss_afo_recep,
             tia.origen_traspaso,
             tia.curp
      INTO   tia_preliquida_tia_folio_liquida,
                   tia_preliquida_tia_id_referencia,
                   tia_preliquida_tia_f_movimiento,
                   tia_preliquida_tia_id_decreto,
                   tia_preliquida_tia_nci_icefa,
                   v_ti_det_sdo_viv92,
                   v_aivs_viv92,
                   v_ind_consistencia,
                   v_nss_afo_recep,
                   v_origen_traspaso,
                   v_curp
      FROM   tia_det_traspaso tia,
             afi_decreto      afi
      WHERE  tia.folio = p_folio
      AND    tia.result_operacion = "01"
      AND    tia.id_decreto = afi.id_decreto

      -- se busca el monto especifico solicitado
      SELECT monto_acciones
      INTO   tia_preliquida_tia_monto_acciones
      FROM   cta_decreto
      WHERE  id_decreto = tia_preliquida_tia_id_decreto
--      AND    monto_pesos = v_ti_det_sdo_viv92;  --se comenta por cambio de regla de Adrian 22-sep-2012
      AND    monto_acciones =  v_aivs_viv92
      AND    movimiento not in (521,1212);

      --trace "ID_DECRETO: " || tia_preliquida_tia_id_decreto;
      --trace "Monto solicitado: " || v_ti_det_sdo_viv92;

      -- si se encontro el monto, se preliquida
      IF tia_preliquida_tia_monto_acciones IS NOT NULL THEN

         IF tia_preliquida_tia_monto_acciones <> 0 THEN
            EXECUTE PROCEDURE sp_preliquida_tia (
               p_folio,
               p_usuario,
               tia_preliquida_tia_id_referencia,
               v_ti_det_sdo_viv92,
               tia_preliquida_tia_id_decreto,
               tia_preliquida_tia_f_movimiento,
               tia_preliquida_tia_nci_icefa,
               v_aivs_viv92,
               v_ind_consistencia,
               v_nss_afo_recep,
               v_origen_traspaso,
               v_curp
                );
         ELSE

                  -- se busca el monto en cta_his_decreto
            SELECT monto_acciones
            INTO   tia_preliquida_tia_monto_acciones
            FROM   cta_his_decreto
            WHERE  id_decreto     = tia_preliquida_tia_id_decreto
            AND    monto_acciones = v_aivs_viv92;

            IF tia_preliquida_tia_monto_acciones IS NOT NULL THEN
               IF tia_preliquida_tia_monto_acciones <> 0 THEN
                 -- MARCA LOS REGISTROS PARA QUE EL USUARIO DECIDA SI SE LIQUIDAN
                  UPDATE tia_det_traspaso
                  SET    result_operacion = "10"
                  WHERE  folio         = tia_preliquida_tia_folio_liquida
                  AND    id_referencia = tia_preliquida_tia_id_referencia;
               ELSE
                  -- se rechaza el registro por AIVS EN CERO
                  UPDATE tia_det_traspaso
                  SET    result_operacion = "05"
                  WHERE  folio = tia_preliquida_tia_folio_liquida
                  AND    id_referencia = tia_preliquida_tia_id_referencia;      
                     END IF
                  ELSE
                     -- aiv no encontrada --
               UPDATE tia_det_traspaso
               SET    result_operacion = "03"
               WHERE  folio         = tia_preliquida_tia_folio_liquida
               AND    id_referencia = tia_preliquida_tia_id_referencia;         
                  END IF
         END IF

      ELSE

         -- se busca el monto en cta_his_decreto
         SELECT monto_acciones
         INTO   tia_preliquida_tia_monto_acciones
         FROM   cta_his_decreto
         WHERE  id_decreto     = tia_preliquida_tia_id_decreto
         AND    monto_acciones =  v_aivs_viv92;

         IF tia_preliquida_tia_monto_acciones IS NOT NULL THEN
                  IF tia_preliquida_tia_monto_acciones <> 0 THEN
                     -- MARCA LOS REGISTROS PARA QUE EL USUARIO DECIDA SI SE LIQUIDAN
               UPDATE tia_det_traspaso
               SET    result_operacion = "10"
               WHERE  folio         = tia_preliquida_tia_folio_liquida
               AND    id_referencia = tia_preliquida_tia_id_referencia;
            ELSE
               -- se rechaza el registro por AIVS EN CERO
               UPDATE tia_det_traspaso
               SET    result_operacion = "05"
               WHERE  folio = tia_preliquida_tia_folio_liquida
               AND    id_referencia = tia_preliquida_tia_id_referencia;         
            END IF
         ELSE
                  -- aiv no encontrada --
            UPDATE tia_det_traspaso
            SET    result_operacion = "03"
            WHERE  folio         = tia_preliquida_tia_folio_liquida
            AND    id_referencia = tia_preliquida_tia_id_referencia;
         END IF

      END IF
   END FOREACH ;

   RETURN v_error_sql, v_error_isam, v_mensaje;
END PROCEDURE;


