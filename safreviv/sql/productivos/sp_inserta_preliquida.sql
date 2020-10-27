






CREATE PROCEDURE "safreviv".sp_inserta_preliquida(
   p_origen                    SMALLINT,
   p_preliq_f_liquida          DATE,
   p_preliq_id_derechohabiente DECIMAL(9,0),
   p_preliq_subcuenta          SMALLINT,
   p_preliq_fondo_inversion    SMALLINT,
   p_preliq_movimiento         SMALLINT,
   p_preliq_folio_liquida      DECIMAL(9,0),
   p_preliq_id_referencia      DECIMAL(9,0),
   p_preliq_monto_acciones     DECIMAL(22,2),
   p_preliq_monto_pesos        DECIMAL(22,2),
   p_preliq_f_valor            DATE,
   p_preliq_f_registro         DATE,
   p_preliq_h_registro         DATETIME HOUR TO SECOND,
   p_det_nrp                   CHAR(11)
   )

   IF p_origen = 1 THEN

      INSERT INTO acl_preliquida VALUES
         (
         p_preliq_f_liquida,
         p_preliq_id_derechohabiente,
         p_preliq_subcuenta,
         p_preliq_fondo_inversion,
         p_preliq_movimiento,
         p_preliq_folio_liquida,
         p_preliq_id_referencia,
         p_preliq_monto_acciones,
         p_preliq_monto_pesos,
         p_preliq_f_valor,
         p_preliq_f_registro,
         p_preliq_h_registro,
         p_det_nrp
         );
   ELSE
      INSERT INTO pag_lqinfo_preliquida VALUES
         (
         p_preliq_f_liquida,
         p_preliq_id_derechohabiente,
         p_preliq_subcuenta,
         p_preliq_fondo_inversion,
         p_preliq_movimiento,
         p_preliq_folio_liquida,
         p_preliq_id_referencia,
         p_preliq_monto_acciones,
         p_preliq_monto_pesos,
         p_preliq_f_valor,
         p_preliq_f_registro,
         p_preliq_h_registro,
         p_det_nrp
         );

   END IF

END PROCEDURE;


