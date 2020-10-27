






CREATE FUNCTION "safreviv".fn_agr_liquida_fondo72(p_folio DECIMAL(9,0))
   RETURNING SMALLINT;
   DEFINE v_ax_error SMALLINT; -- contiene el código de error en caso de ocurrir

   ON EXCEPTION SET v_ax_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrLiquidaFondo72.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_ax_error = 0;

   -- se insertan los registros de la temporal a la tabla de liquidación fondo72
   INSERT INTO safre_viv:cta_fondo72 SELECT * FROM safre_tmp:tmp_agr_fondo72;

	 RETURN v_ax_error;
END FUNCTION;


