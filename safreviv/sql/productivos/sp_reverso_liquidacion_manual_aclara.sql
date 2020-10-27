






CREATE PROCEDURE "safreviv".sp_reverso_liquidacion_manual_aclara(p_folio  decimal(9,0),
                                                      p_id_referencia decimal(9,0))
   RETURNING SMALLINT;
   DEFINE v_error SMALLINT;

   ON EXCEPTION SET v_error
      --Ocurrio un error al realizar el reverso de la liquidación
      --Se regresa el número de error que ocurrio
      RETURN v_error;
   END EXCEPTION

   --Se inicia el error en 0 para indicar que por default no ocurrio un error
   LET v_error = 0;

   DELETE FROM safre_viv:cta_movimiento
   WHERE folio_liquida = p_folio
   AND   id_referencia = p_id_referencia;
  
   RETURN v_error;

END PROCEDURE;


