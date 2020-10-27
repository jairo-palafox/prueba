






CREATE FUNCTION "safreviv".fn_fnacimiento(p_nss   CHAR(11),
                               p_curp  CHAR(18),
                               p_rfc   CHAR(13))
RETURNING DATE;

DEFINE v_anio     SMALLINT;
DEFINE v_fena     DATE;
DEFINE r_criterio SMALLINT;

   IF p_curp IS NOT NULL AND p_curp[1] <> ' ' AND
      fn_valida_fecha( p_curp[5,10] ) = 1 THEN

      IF (p_curp[17] NOT MATCHES '[0-9]*') THEN
      ---IF p_curp[17] = "A" THEN
         LET v_anio = 2000 + p_curp[5,6] ;
      ELSE
         LET v_anio = 1900 + p_curp[5,6] ;
      END IF

      LET v_fena     = MDY( p_curp[7,8], p_curp[9,10], v_anio );
      LET r_criterio = 1;
   ELSE
      IF p_rfc IS NOT NULL AND p_rfc[1] <> ' ' AND
        fn_valida_fecha( p_rfc[5,10] ) = 1 THEN

        IF p_rfc[5,6] = "00" THEN
           LET v_anio = 2000 + p_rfc[5,6] ;
        ELSE
           LET v_anio = 1900 + p_rfc[5,6] ;
        END IF

        LET v_fena     = MDY( p_rfc[7,8], p_rfc[9,10], v_anio );
        LET r_criterio = 2;
      ELSE
        LET v_anio     = 1900 + p_nss[5,6] ;
        LET v_fena     = MDY( 1,1, v_anio );
        LET r_criterio = 0;
      END IF
   END IF 

   RETURN v_fena ;

END FUNCTION;


