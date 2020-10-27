






CREATE FUNCTION "safreviv".fn_cre_uso_unificacion( p_id_derechohabiente DECIMAL(9,0),
                                       p_id_cre_uso_garantia DECIMAL(9,0) )

   RETURNING DECIMAL(9,0), DECIMAL(9,0)

   DEFINE v_id_dh                     DECIMAL(9,0);
   DEFINE v_id_cre_ug                 DECIMAL(9,0);
   DEFINE v_r_id_dh                   DECIMAL(9,0);
   DEFINE v_r_id_cre_ug               DECIMAL(9,0);
   DEFINE v_id_dh_unificador          DECIMAL(9,0);
   DEFINE v_id_cre_ug_unificador      DECIMAL(9,0);


   SET DEBUG FILE TO '/safreviv_int/BD/fn_cre_uso_unificacion.trace';
   TRACE ON;

   -- Se inicializan las variables
   LET v_id_dh     = 0;
   LET v_id_cre_ug = 0;

   SELECT id_derechohabiente,
          id_cre_uso_garantia,
          id_dh_unificador,
          id_cre_ug_unificador
     INTO v_id_dh,
          v_id_cre_ug,
          v_id_dh_unificador,
          v_id_cre_ug_unificador
     FROM cre_uso_unificacion
    WHERE id_dh_unificador     = p_id_derechohabiente
      AND id_cre_ug_unificador = p_id_cre_uso_garantia;

   -- Si las variables son nulas quiere decir que no existe en cre_uso_unificación
   IF ( v_id_dh IS NULL OR v_id_dh = 0 ) AND (v_id_cre_ug IS NULL OR v_id_cre_ug = 0 ) THEN
      -- Se asigna el valor que se recibio de parametro a las variables que retorna la funcion 
      LET v_r_id_dh     = p_id_derechohabiente;
      LET v_r_id_cre_ug = p_id_cre_uso_garantia;
   ELSE    -- Sí existe en cre_uso_unificacion
      -- Se asignan a las variables de retorno los parametros de entrada
      LET v_r_id_dh     = v_id_dh;
      LET v_r_id_cre_ug = v_id_cre_ug;
   END IF;

   RETURN v_r_id_dh, v_r_id_cre_ug;
END FUNCTION;


