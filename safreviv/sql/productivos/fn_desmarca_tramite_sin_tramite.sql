






create function  "safreviv".fn_desmarca_tramite_sin_tramite()
   RETURNING SMALLINT

   DEFINE v_error                  SMALLINT;
   DEFINE v_id_dh                  DECIMAL(9,0);
   DEFINE v_id_ocg_tramite         DECIMAL(9,0);
   DEFINE v_rch_marca              smallint;

 ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   LET v_error              = 0;

   FOREACH
select t.id_derechohabiente ,s.n_referencia
  into v_id_dh,
       v_id_ocg_tramite
  from ocg_tramite t, sfr_marca_activa s
 where t.id_derechohabiente = s.id_derechohabiente
   and t.situacion = 155
   and s.marca = 206

EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                                    206,
                                    v_id_ocg_tramite,
                                    0,
                                    0,
                                    "safreviv",
                                    3903) INTO v_rch_marca;

   LET v_id_dh = "";
   LET v_id_ocg_tramite = "";

   END FOREACH;
   RETURN v_error;
END FUNCTION;


