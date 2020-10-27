






CREATE FUNCTION "safreviv".fn_verifica_fallecido(p_nss CHAR(11))
RETURNING CHAR(2), CHAR(25)

   DEFINE v_estado                  SMALLINT;
   DEFINE v_fallecido               CHAR(2);
   DEFINE v_desc_fall               CHAR(25);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);

   ---SET DEBUG FILE TO '/safreviv_int/archivos/fn_verifica_fallecido.trace';
   ---TRACE ON;

   -- se asume que dh no está fallecido
   LET v_id_derechohabiente = 0;
   LET v_estado    = "";
   LET v_fallecido = "00";

   SELECT id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = p_nss;

   IF v_id_derechohabiente = 0 OR v_id_derechohabiente IS NULL OR v_id_derechohabiente = "" THEN
      LET v_fallecido = "02";
      LET v_desc_fall = "Trabajador no localizado";
   ELSE
      SELECT estado
        INTO v_estado
        FROM afi_fallecido
       WHERE id_derechohabiente = v_id_derechohabiente;

      IF v_estado = 10 THEN
         LET v_fallecido = "01";
         LET v_desc_fall = "Trabajador fallecido";
      ELSE
         LET v_fallecido = "00";
         LET v_desc_fall = "Trabajador no fallecido";
      END IF
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_fallecido, v_desc_fall;

END FUNCTION
;


