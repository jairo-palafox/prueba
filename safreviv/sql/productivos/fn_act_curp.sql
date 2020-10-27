






CREATE FUNCTION "safreviv".fn_act_curp()
   RETURNING SMALLINT

DEFINE v_nss     char(11);
DEFINE v_curp    char(18);
DEFINE v_error   smallint;

ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_act_curp.trace';
   TRACE ON;

   let v_error = 0;

   FOREACH

      SELECT nss,
             curp
        INTO v_nss,
             v_curp
        FROM tmp_act_curp

      UPDATE afi_derechohabiente
         SET curp = v_curp
       WHERE nss = v_nss;

      INSERT INTO tmp_curp_act
           VALUES(v_nss,
                  v_curp);

      LET v_nss  = "";
      LET v_curp = "";

   END FOREACH

   --DROP TABLE IF EXISTS tmp_act_curp;
   --DROP TABLE IF EXISTS tmp_curp_act;

   RETURN v_error;
END FUNCTION;


