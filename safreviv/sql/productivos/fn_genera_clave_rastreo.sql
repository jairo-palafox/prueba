






CREATE FUNCTION "safreviv".fn_genera_clave_rastreo (p_cve CHAR(25)) RETURNING CHAR(30)
   DEFINE v_respuesta         CHAR(30);
   DEFINE i                   INT;
   DEFINE v_suma              INTEGER;
 
   LET v_suma = 0; 
   FOR i = 1 TO 25
      LET v_suma = v_suma + POW(SUBSTRING(p_cve FROM i FOR 1) :: INTEGER,2);
   END FOR
   LET v_respuesta = p_cve || MOD(v_suma,9) || '    ';
   RETURN v_respuesta;
END FUNCTION;


