






CREATE FUNCTION "safreviv".fn_reversa_saldo_remanente_acr(p_i_tpo_originacion SMALLINT)
RETURNING SMALLINT;
   DEFINE v_sql_error         SMALLINT;
   DEFINE v_id_cre_acreditado DECIMAL(9,0);
   DEFINE v_edo_procesar      SMALLINT; 

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Devolvera el codigo de error que ocasione la excepcion      
      RETURN v_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO 'reverso_saldos_acr.trace';

   -- se iniacilizan variables
   LET v_sql_error = 0;

   -- se procesan los registros de la tabla maestro
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT UNIQUE id_cre_acreditado, edo_procesar
        INTO v_id_cre_acreditado, v_edo_procesar
        FROM safre_viv:cre_acreditado
       WHERE edo_procesar IN (5,70)
         AND estado = 25
         AND tpo_originacion = p_i_tpo_originacion

      -- se actualizan los registros de historicos
      DELETE
        FROM safre_viv:cre_his_acreditado
       WHERE estado = 25
         AND edo_procesar IN (70,5)
         AND id_cre_acreditado = v_id_cre_acreditado;

      IF v_edo_procesar = 5 THEN
         -- se actualiza unicamente el estado a Liquidado(140)
         UPDATE safre_viv:cre_acreditado
            SET estado = 140
          WHERE tpo_originacion = p_i_tpo_originacion
            AND edo_procesar = v_edo_procesar
            AND estado = 25
            AND id_cre_acreditado = v_id_cre_acreditado;
      ELSE
         -- se actualiza estado a Liquidado(140) y estado procesar a 120
         UPDATE safre_viv:cre_acreditado
            SET estado = 140,
                edo_procesar = 120
          WHERE tpo_originacion = p_i_tpo_originacion
            AND edo_procesar = v_edo_procesar
            AND estado = 25
            AND id_cre_acreditado = v_id_cre_acreditado;
      END IF
   END FOREACH;

   --########
   -- En caso de terminar correctamente, devolvera 0
   RETURN v_sql_error;
END FUNCTION;


