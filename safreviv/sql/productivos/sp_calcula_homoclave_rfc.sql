






CREATE PROCEDURE "safreviv".sp_calcula_homoclave_rfc(p_nombre char(50),p_rfc char(10))
RETURNING SMALLINT,CHAR(45),CHAR(010),CHAR(10),CHAR(13);

DEFINE v_residuo           INTEGER;
DEFINE v_cociente          INTEGER;
DEFINE i                   INTEGER;
DEFINE j                   INTEGER;
DEFINE v_acumulado         INTEGER;
DEFINE v_longitud          SMALLINT;
DEFINE v_unidad            SMALLINT;
DEFINE v_long_total        SMALLINT ;
DEFINE v_long_ini          SMALLINT;
DEFINE v_ind               SMALLINT;
DEFINE v_encuentra         SMALLINT;
DEFINE v_cadena_convertida CHAR(120);
DEFINE v_valor             CHAR(002);
DEFINE v_letra             CHAR(001);
DEFINE v_par               CHAR(002);
DEFINE v_acumulado_c       CHAR(100);
DEFINE v_acumulado_parcial CHAR(003);
DEFINE v_digito1           CHAR(001);
DEFINE v_digito2           CHAR(001);
DEFINE v_homonimia         CHAR(002);
DEFINE v_rfc_homonimia     CHAR(012);
DEFINE v_rfc_completo      CHAR(013);
DEFINE v_caracter_invalido CHAR(010);
DEFINE v_desc              CHAR(045);
DEFINE v_letra_1           CHAR(001);

--SET DEBUG FILE TO '/tmp/sp_calcula_homoclave_rfc.trace';
--TRACE ON;
LET v_longitud = LENGTH(p_nombre);
LET v_cadena_convertida[1] = "0";
LET i           = 1;
LET v_ind       = 0;
LET v_encuentra = 0;
LET v_rfc_completo = "";
LET v_caracter_invalido = "";

-- se verifica que el nombre sea válido es decir que contenga
-- unicamente caracteres válidos, el unico caracter especial
-- permitido es el "&"

FOR i = 1 TO LENGTH(TRIM(p_nombre))

    LET v_letra_1 = substr(p_nombre,i,i);

    SELECT NVL(MAX(0),1)
    INTO   v_encuentra
    FROM   cat_fondo_local  -- se utiliza de comodin
    WHERE  UPPER(v_letra_1) BETWEEN "A" AND "Z"
    OR           v_letra_1  IN (" ","&","Ñ","ñ");

    IF v_encuentra = 1 THEN

       LET v_caracter_invalido = CONCAT(TRIM(v_caracter_invalido),v_letra_1);
       LET v_ind = 1;

    END IF

END FOR;


-- devuelve error si encuentra caracteres especiales
-- no se puede generar homoclave hasta corregir nombre
-- desde el modulo de modificación fondo72

IF v_ind = 1 THEN

   LET v_desc = "Caracteres especiales invalidos en nombre";

   RETURN v_ind                , -- error
          v_desc               , -- descripcion del error
          v_caracter_invalido  , -- detalle del error
          p_rfc                ,
          v_rfc_completo       ;

END IF;

LET i = 1;

-- se convierte la cadena del nombre segun tabla de conversion

WHILE i <= v_longitud

     LET v_letra = UPPER(substr(p_nombre,i,i));

     IF v_letra = "ñ" THEN LET v_letra = "Ñ"; END IF

     SELECT a.valor
     INTO   v_valor
     FROM   cat_caracter_homoclave a
     WHERE  a.caracter = v_letra ;

     LET v_cadena_convertida = CONCAT(TRIM(v_cadena_convertida),TRIM(v_valor));
     LET i = i + 1;

END WHILE;

LET v_acumulado = 0;

-- se suman los resultados de los pares multiplicados por sus unidades

FOR i = 1 TO (LENGTH(TRIM(v_cadena_convertida)) - 1)

   LET v_par       = substr(v_cadena_convertida,i,i+1);
   LET v_unidad    = substr(v_par,2,2);
   LET v_acumulado = v_acumulado + (v_par * v_unidad);

END FOR;

   LET v_acumulado_c = v_acumulado;
   LET v_long_total  = length(trim(v_acumulado_c));
   LET v_long_ini    = v_long_total - 2 ;

-- se obtienen los tres ultimos digitos

   LET v_acumulado_parcial = SUBSTR(TRIM(v_acumulado_c),
                                    v_long_ini,
                                    v_long_total);

-- se obtiene cociente y residuo

LET v_residuo = MOD(v_acumulado_parcial,34);
LET v_cociente = v_acumulado_parcial  / 34 ;

-- se obtiene equivalencia en tabla anexo II para cociente y residuo

   SELECT a.valor
   INTO   v_digito1
   FROM   cat_coeficiente_residuo_homoclave a
   WHERE  a.digito = v_cociente;

   SELECT a.valor
   INTO   v_digito2
   FROM   cat_coeficiente_residuo_homoclave a
   WHERE  a.digito = v_residuo;

   LET v_homonimia = CONCAT(v_digito1,v_digito2);
   LET v_rfc_homonimia = CONCAT(p_rfc,v_homonimia);

--  CALCULO DIGITO VERIFICADOR

LET i = 1;

-- se convierte la cadena del rfc segun tabla de conversion

LET v_cadena_convertida = "";

WHILE i <= 12

     LET v_letra = substr(v_rfc_homonimia,i,i);

     SELECT a.valor
     INTO   v_valor
     FROM   cat_digito_verificador_rfc a
     WHERE  a.digito = v_letra ;

     LET v_cadena_convertida = CONCAT(TRIM(v_cadena_convertida),TRIM(v_valor));
     LET i = i + 1;

END WHILE;


-- se obtiene el acumulado de factor 13 descendiente

   LET v_acumulado = 0;
   LET j = 13;

FOR i = 1 TO (LENGTH(TRIM(v_cadena_convertida)) - 1) STEP 2

   LET v_par       = substr(v_cadena_convertida,i,i+1);
   LET v_acumulado = v_acumulado + v_par * j;
   LET j = j - 1;

END FOR;

-- se obtiene cociente y residuo considerando el factor 11
TRACE "ACUM "|| v_acumulado;
TRACE "RES "|| v_residuo;
LET v_residuo = MOD(v_acumulado,11);

-- se obtiene el digito verificador de acuerdo a posicion en la tabla

   IF v_residuo = 0 THEN
      LET v_digito1 = 0;
   ELSE
      SELECT a.digito
      INTO   v_digito1
      FROM   cat_digito_verificador_rfc a
      WHERE  a.valor = (11 - v_residuo);
   END IF

-- se concatena el rfc con su complemento de homonimia y digito verificador

   LET v_rfc_completo = CONCAT(TRIM(v_rfc_homonimia),TRIM(v_digito1));

   LET v_desc = "";
   LET v_caracter_invalido = "";

   RETURN v_ind                , -- error
          v_desc               , -- descripcion del error
          v_caracter_invalido  , -- detalle del error
          p_rfc                ,
          v_rfc_completo       ;
END PROCEDURE
;


