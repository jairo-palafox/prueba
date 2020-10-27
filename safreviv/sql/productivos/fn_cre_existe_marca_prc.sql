






CREATE FUNCTION "safreviv".fn_cre_existe_marca_prc(p_id_derechohabiente  DECIMAL(9,0),
                                        p_marca_prc           SMALLINT)
   RETURNING SMALLINT
   DEFINE v_exist_marca_prc   SMALLINT; -- indica si existe la marca o no
   
   --ON EXCEPTION SET v_error
   --  -- Devolvera el codigo de error cuando ocurra una excepción
   --  RETURN v_error;
   --END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/creExisteMarcaPrc.trace';
   --TRACE ON;

   -- se valida que el id_derechohabiente obtenido no exista en la tabla maestro (vigente)
   IF EXISTS (
   SELECT marca
     FROM sfr_marca_activa
    WHERE id_derechohabiente = p_id_derechohabiente
      AND marca = p_marca_prc) THEN
      -- se prende la bandera
      LET v_exist_marca_prc = 1;
   ELSE
      -- se mantiene la bandera apagada
      LET v_exist_marca_prc = 0;
   END IF

   RETURN v_exist_marca_prc;
END FUNCTION

;


