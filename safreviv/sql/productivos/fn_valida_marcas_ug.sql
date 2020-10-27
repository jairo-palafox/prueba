






CREATE FUNCTION "safreviv".fn_valida_marcas_ug(p_id_derechohabiente DECIMAL(9,0))
   RETURNING SMALLINT, SMALLINT, CHAR(40), DATE;

   DEFINE v_marca                   SMALLINT;
   DEFINE v_desc_marca              CHAR(40);
   DEFINE v_f_marca                 DATE;
   DEFINE v_cod_resp                SMALLINT;

   /*
   SET DEBUG FILE TO '/safreviv_int/archivos/marcasCredGtia.trace';
   TRACE ON;
   */

   LET v_marca      = "";
   LET v_desc_marca = "";
   LET v_f_marca    = "";
   LET v_cod_resp   = 0;

   IF NOT EXISTS(SELECT c.marca
                   FROM sfr_marca_activa s,
                        cat_marca_ef c
                  WHERE s.id_derechohabiente = p_id_derechohabiente
                    AND s.marca = c.marca) THEN

      RETURN v_cod_resp, v_marca, v_desc_marca, v_f_marca;
   ELSE
      FOREACH SELECT 1,
                     c.marca,
                     c.marca_desc,
                     s.f_inicio
                INTO v_cod_resp,
                     v_marca,
                     v_desc_marca,
                     v_f_marca
                FROM sfr_marca_activa s,
                     cat_marca_ef c
               WHERE s.id_derechohabiente = p_id_derechohabiente
                 AND s.marca = c.marca

         RETURN v_cod_resp,
                v_marca,
                v_desc_marca,
                v_f_marca WITH RESUME;
      END FOREACH;
   END IF
END FUNCTION;


