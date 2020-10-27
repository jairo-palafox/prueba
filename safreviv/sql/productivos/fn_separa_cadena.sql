






CREATE FUNCTION "safreviv".fn_separa_cadena(p_nombre CHAR(50))
RETURNING VARCHAR(40),
          VARCHAR(40),
          VARCHAR(40);


DEFINE r_paterno VARCHAR(40) ;
DEFINE r_materno VARCHAR(40) ;
DEFINE r_nombres VARCHAR(40) ;

DEFINE i          INTEGER ;       --Contador de posición
DEFINE v_opcion   SMALLINT ;      --Se ocuparan 3 opciones: 1=Paterno; 2=Materno; 3=Nombres
                                  --NOTA: Cuando p_nombre contiene más de 2 caracteres '$'
                                  --las cadenas posteriores se ignoran
DEFINE v_caracter CHAR(1);        --Caracter a evaluar

--Se inicializan las variables de respuesta a cadenas vacias
LET r_paterno = "" ;
LET r_materno = "" ;
LET r_nombres = "" ;

--Se inicializa la variable de opcion para iniciar con el apellido paterno
LET v_opcion = 1;  

FOR i=1 TO LENGTH(p_nombre)
   LET v_caracter = SUBSTR(p_nombre,i,1);
   IF v_caracter = "$" THEN
      LET v_opcion = v_opcion + 1;
   ELSE
      IF v_opcion = 1 THEN
            LET r_paterno = r_paterno || v_caracter;
      ELSE
         IF v_opcion = 2 THEN
            LET r_materno = r_materno || v_caracter;
         ELSE
            IF v_opcion = 3 THEN
               LET r_nombres = r_nombres || v_caracter;
            END IF
         END IF
      END IF
   END IF
END FOR;

IF LENGTH(r_nombres) = 0 THEN
   LET r_nombres = 'S/N';
END IF

RETURN r_paterno,
       r_materno,
       r_nombres;

END FUNCTION;


