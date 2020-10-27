






CREATE FUNCTION "safreviv".fn_ret_valida_fecha(p_dia  SMALLINT,
                                    p_mes  SMALLINT,
                                    p_year SMALLINT)
   RETURNING SMALLINT
   DEFINE v_ax_error          SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_isam_err          INTEGER;
   DEFINE v_c_msj             VARCHAR(250);
   DEFINE v_fecha_valida                             DATE;

   ON EXCEPTION  SET v_ax_error, v_isam_err,v_c_msj

      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error;
   END EXCEPTION

      LET v_fecha_valida                     =  mdy(p_mes,p_dia,p_year);
      IF v_fecha_valida IS NULL THEN
          RETURN 1;
      ELSE
          RETURN 0;
      END IF
END FUNCTION;


