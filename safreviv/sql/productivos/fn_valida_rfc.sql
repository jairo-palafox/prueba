






CREATE FUNCTION "safreviv".fn_valida_rfc(p_rfc_modificado CHAR(13), p_apellido_paterno CHAR(50), p_apellido_materno CHAR(50), p_nombres CHAR(50))

RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(100)

DEFINE v_rfc_es_correcto         SMALLINT; -- booleana que indica si un RFC esta correctamente construido
DEFINE v_rfc_analisis            CHAR(13); -- RFC que se analizara
DEFINE v_rfc_aux                 CHAR(13); -- RFC auxiliar para sugerir correccion
DEFINE v_indice                  SMALLINT;
DEFINE v_cadena                  CHAR(50);
DEFINE v_fecha_nacimiento        VARCHAR(6); -- seccion de la fecha de nacimiento del RFC
DEFINE v_mensaje                 CHAR(100); -- cadena con mensaje para usuario
DEFINE v_altisonante             CHAR(4); -- busqueda de palabra altisonante
DEFINE v_rfc_sustituto           CHAR(4);
DEFINE v_continuar               SMALLINT;
DEFINE v_pos_ini                 SMALLINT;
DEFINE v_pos_fin                 SMALLINT;
DEFINE v_invalida                SMALLINT;

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  VARCHAR(250);

   ON EXCEPTION SET sql_err, isam_err,  err_txt
      LET v_rfc_es_correcto = sql_err;
      LET v_mensaje = err_txt;

      RETURN v_rfc_es_correcto, isam_err, err_txt, v_mensaje;
   END EXCEPTION

    SET DEBUG FILE TO '/safreviv_int/BD/fn_valida_rfc.trace';   
   -- se asume que el RFC esta correcto
   TRACE ON;
   
   LET v_rfc_es_correcto = 0;
   LET v_mensaje = "";
   LET v_continuar = 0;
   LET v_indice = 0;
   LET v_invalida = 0;

   -- se asigna el RFC para analisis
   LET v_rfc_analisis = TRIM(p_rfc_modificado);
--    trace "VALORES RECIBIDOS:"
--    trace "RFC              :"||p_rfc_modificado
--    trace "Nombre           :"||p_nombres
--    trace "Ape Paterno      :"||p_apellido_paterno
--    trace "Ape Materno      :"||p_apellido_materno
    -- ==========================================================
   -- se busca que el RFC este bien conformado
   -- validacion de particula obtenida de apellido paterno
   -- se verifica si el apellido paterno tiene mas de una particula
   LET v_pos_ini = INSTR(TRIM(p_apellido_paterno)," ");
   IF ( v_pos_ini > 0 ) THEN
      LET v_cadena = SUBSTR(p_apellido_paterno,1,v_pos_ini - 1);
      -- se obtiene la primera particula que no sea una preposicion o particula invalida     
      WHILE ( v_continuar = 0 )
         -- si es diferente a una palabra invalida, se toma esa palabra
         IF ( v_cadena <> "DA"  AND
              v_cadena <> "DAS" AND
              v_cadena <> "DE"  AND
              v_cadena <> "DEL" AND
              v_cadena <> "DER" AND
              v_cadena <> "DI"  AND
              v_cadena <> "DIE" AND
              v_cadena <> "DD"  AND
              v_cadena <> "EL"  AND
              v_cadena <> "LA"  AND
              v_cadena <> "LOS" AND
              v_cadena <> "LAS" AND
              v_cadena <> "LE"  AND
              v_cadena <> "LES" AND
              v_cadena <> "MAC" AND
              v_cadena <> "MC"  AND
              v_cadena <> "VAN" AND
              v_cadena <> "VON" AND
              v_cadena <> "Y"   ) THEN
            EXIT WHILE;
         ELSE
            -- se incrementa el contador de palabra
            LET v_indice = v_indice + 1;
            LET v_pos_fin = INSTR(TRIM(p_apellido_paterno)," ",v_pos_ini+1);
            IF v_pos_fin > 0 THEN  
                LET v_cadena = SUBSTR(p_apellido_paterno,v_pos_ini+1,v_pos_fin - v_pos_ini);
                LET v_pos_ini = v_pos_fin;
            ELSE
                IF v_invalida = 1 THEN 
                    LET v_cadena = "";
                    EXIT WHILE;
                ELSE 
                    LET v_cadena = SUBSTR(p_apellido_paterno,v_pos_ini+1,LENGTH(TRIM(p_apellido_paterno)) - v_pos_ini);
                    LET v_invalida = 1;
                END IF 
            END IF 
         END IF
      END WHILE
   ELSE
      -- solo es una palabra
      -- si es diferente a una palabra invalida, se toma esa palabra
      IF ( p_apellido_paterno <> "DA"  AND
           p_apellido_paterno <> "DAS" AND
           p_apellido_paterno <> "DE"  AND
           p_apellido_paterno <> "DEL" AND
           p_apellido_paterno <> "DER" AND
           p_apellido_paterno <> "DI"  AND
           p_apellido_paterno <> "DIE" AND
           p_apellido_paterno <> "DD"  AND
           p_apellido_paterno <> "EL"  AND
           p_apellido_paterno <> "LA"  AND
           p_apellido_paterno <> "LOS" AND
           p_apellido_paterno <> "LAS" AND
           p_apellido_paterno <> "LE"  AND
           p_apellido_paterno <> "LES" AND
           p_apellido_paterno <> "MAC" AND
           p_apellido_paterno <> "MC"  AND
           p_apellido_paterno <> "VAN" AND
           p_apellido_paterno <> "VON" AND
           p_apellido_paterno <> "Y"   ) THEN
         LET v_cadena = p_apellido_paterno;
      ELSE
         -- se le pone una X
         LET v_cadena = "X";
      END IF
   END IF

   -- si no se encontro una palabra valida
   IF v_cadena = "" THEN
      -- se ponen 2 X
      LET v_rfc_aux = "XX";
   ELSE
      -- se obtiene la primera letra
      IF ( SUBSTR(v_cadena,1,1) = "Ñ" ) THEN
         -- si es Ñ se cambia por X
         LET v_rfc_aux = "X";
      ELSE
         LET v_rfc_aux = SUBSTR(v_cadena,1,1);
      END IF

      -- se busca la primera vocal seguida de la primera letra
      FOR v_indice = 2 TO LENGTH(TRIM(v_cadena))
         IF ( SUBSTR(v_cadena,v_indice,1) = "A" OR
              SUBSTR(v_cadena,v_indice,1) = "E" OR
              SUBSTR(v_cadena,v_indice,1) = "I" OR
              SUBSTR(v_cadena,v_indice,1) = "O" OR
              SUBSTR(v_cadena,v_indice,1) = "U" ) THEN
            -- se sale del ciclo para quedarse con el indice
            EXIT FOR;
         END IF
      END FOR

      -- se verifica si se encontro alguna vocal. Esto ocurriria si el indice esta entre 2 y la longitud de la palabra
      IF ( v_indice > 1 AND v_indice <= LENGTH(TRIM(v_cadena)) ) THEN
         -- se agrega la vocal encontrada
         LET v_rfc_aux = TRIM(v_rfc_aux) || SUBSTR(v_cadena,v_indice,1);
      ELSE
         -- no se encontro vocal, se agrega una X
         LET v_rfc_aux = TRIM(v_rfc_aux) || "X";
      END IF
   END IF

   -- =========================================
   -- particula obtenida del apellido materno
   IF ( p_apellido_materno IS NULL ) THEN
      -- se asigna X cuando no se tiene apellido materno
      LET v_rfc_aux = TRIM(v_rfc_aux) || "X";
   ELSE

       LET v_pos_ini = INSTR(TRIM(p_apellido_materno)," ");
       IF ( v_pos_ini > 0 ) THEN
          LET v_cadena = SUBSTR(p_apellido_materno,1,v_pos_ini - 1);
          -- se obtiene la primera particula que no sea una preposicion o particula invalida     
          WHILE ( v_continuar = 0 )
             -- si es diferente a una palabra invalida, se toma esa palabra
             IF ( v_cadena <> "DA"  AND
                  v_cadena <> "DAS" AND
                  v_cadena <> "DE"  AND
                  v_cadena <> "DEL" AND
                  v_cadena <> "DER" AND
                  v_cadena <> "DI"  AND
                  v_cadena <> "DIE" AND
                  v_cadena <> "DD"  AND
                  v_cadena <> "EL"  AND
                  v_cadena <> "LA"  AND
                  v_cadena <> "LOS" AND
                  v_cadena <> "LAS" AND
                  v_cadena <> "LE"  AND
                  v_cadena <> "LES" AND
                  v_cadena <> "MAC" AND
                  v_cadena <> "MC"  AND
                  v_cadena <> "VAN" AND
                  v_cadena <> "VON" AND
                  v_cadena <> "Y"   ) THEN
                EXIT WHILE;
             ELSE
                -- se incrementa el contador de palabra
                LET v_indice = v_indice + 1;
                LET v_pos_fin = INSTR(TRIM(p_apellido_materno)," ",v_pos_ini+1);
                IF v_pos_fin > 0 THEN  
                    LET v_cadena = SUBSTR(p_apellido_materno,v_pos_ini+1,v_pos_fin - v_pos_ini);
                    LET v_pos_ini = v_pos_fin;
                ELSE
                    IF v_invalida = 1 THEN 
                        LET v_cadena = "";
                        EXIT WHILE;
                    ELSE 
                        LET v_cadena = SUBSTR(p_apellido_materno,v_pos_ini+1,LENGTH(TRIM(p_apellido_materno)) - v_pos_ini);
                        LET v_invalida = 1;
                    END IF 
                END IF 
             END IF
          END WHILE
       ELSE
          -- solo es una palabra
          -- si es diferente a una palabra invalida, se toma esa palabra
          IF ( p_apellido_materno <> "DA"  AND
               p_apellido_materno <> "DAS" AND
               p_apellido_materno <> "DE"  AND
               p_apellido_materno <> "DEL" AND
               p_apellido_materno <> "DER" AND
               p_apellido_materno <> "DI"  AND
               p_apellido_materno <> "DIE" AND
               p_apellido_materno <> "DD"  AND
               p_apellido_materno <> "EL"  AND
               p_apellido_materno <> "LA"  AND
               p_apellido_materno <> "LOS" AND
               p_apellido_materno <> "LAS" AND
               p_apellido_materno <> "LE"  AND
               p_apellido_materno <> "LES" AND
               p_apellido_materno <> "MAC" AND
               p_apellido_materno <> "MC"  AND
               p_apellido_materno <> "VAN" AND
               p_apellido_materno <> "VON" AND
               p_apellido_materno <> "Y"   ) THEN
             LET v_cadena = p_apellido_materno;
          ELSE
             -- se le pone una X
             LET v_cadena = "X";
          END IF
       END IF
    
       -- si no se encontro una palabra valida
       IF v_cadena = "" THEN
          -- se pone 1 X
          LET v_rfc_aux = TRIM(v_rfc_aux)||"X";
       ELSE
          -- se obtiene la primera letra
          IF ( SUBSTR(v_cadena,1,1) = "Ñ" ) THEN
             -- si es Ñ se cambia por X
             LET v_rfc_aux = TRIM(v_rfc_aux)||"X";
          ELSE
             LET v_rfc_aux = TRIM(v_rfc_aux)||SUBSTR(v_cadena,1,1);
          END IF

       END IF
    END IF 








   -- ===========================================
    -- particula del nombre. se copia la primera inicial
    LET v_pos_ini = INSTR(TRIM(p_nombres)," ");
    IF ( v_pos_ini > 0 ) THEN
        LET v_indice = 1;
        LET v_cadena = SUBSTR(p_nombres,1,v_pos_ini - 1);
        WHILE ( v_continuar = 0 )

         -- si es diferente a una palabra invalida, se toma esa palabra
         IF ( v_cadena = "MARIA" OR
              v_cadena = "MA."   OR
              v_cadena = "MA"    OR
              v_cadena = "JOSE"  OR
              v_cadena = "J"     OR
              v_cadena = "J."      ) THEN
            -- si es el primer nombre, no se puede tomar
            IF ( v_indice = 1 ) THEN
               LET v_indice = v_indice + 1;
                LET v_pos_fin = INSTR(TRIM(p_nombres)," ",v_pos_ini+1);
                IF v_pos_fin > 0 THEN  
                    LET v_cadena = SUBSTR(p_nombres,v_pos_ini+1,v_pos_fin - v_pos_ini);
                    LET v_pos_ini = v_pos_fin;
                ELSE
                    IF v_invalida = 1 THEN 
                        LET v_cadena = "";
                        EXIT WHILE;
                    ELSE 
                        LET v_cadena = SUBSTR(p_nombres,v_pos_ini+1,LENGTH(TRIM(p_nombres)) - v_pos_ini);
                        LET v_invalida = 1;
                    END IF 
                END IF 
               CONTINUE WHILE;
            ELSE
               -- se sale del while para tomar el nombre en turno
               EXIT WHILE;
            END IF
         ELSE
            -- si se trata de una palabra invalida, se salta esa palabra
            
            IF ( v_cadena = "DA"  OR
                 v_cadena = "DAS" OR
                 v_cadena = "DE"  OR
                 v_cadena = "DEL" OR
                 v_cadena = "DER" OR
                 v_cadena = "DI"  OR
                 v_cadena = "DIE" OR
                 v_cadena = "DD"  OR
                 v_cadena = "EL"  OR
                 v_cadena = "LA"  OR
                 v_cadena = "LOS" OR
                 v_cadena = "LAS" OR
                 v_cadena = "LE"  OR
                 v_cadena = "LES" OR
                 v_cadena = "MAC" OR
                 v_cadena = "MC"  OR
                 v_cadena = "VAN" OR
                 v_cadena = "VON" OR
                 v_cadena = "Y"   ) THEN
               LET v_indice = v_indice + 1;
                LET v_pos_fin = INSTR(TRIM(p_nombres)," ",v_pos_ini+1);
                IF v_pos_fin > 0 THEN  
                    LET v_cadena = SUBSTR(p_nombres,v_pos_ini+1,v_pos_fin - v_pos_ini);
                    LET v_pos_ini = v_pos_fin;
                ELSE
                    IF v_invalida = 1 THEN 
                        LET v_cadena = "";
                        EXIT WHILE;
                    ELSE 
                        LET v_cadena = SUBSTR(p_nombres,v_pos_ini+1,LENGTH(TRIM(p_nombres)) - v_pos_ini);
                        LET v_invalida = 1;
                    END IF 
                END IF 
               CONTINUE WHILE;
            ELSE
               -- se toma la palabra en turno
               EXIT WHILE;
            END IF
         END IF
      END WHILE

      -- si no se encontro una palabra valida
      IF ( v_cadena = "" ) THEN
         LET v_cadena = "X";
      END IF
   ELSE
      -- solo es una palabra
      -- si es diferente a una palabra invalida, se toma esa palabra
      IF ( p_nombres <> "DA"  AND
           p_nombres <> "DAS" AND
           p_nombres <> "DE"  AND
           p_nombres <> "DEL" AND
           p_nombres <> "DER" AND
           p_nombres <> "DI"  AND
           p_nombres <> "DIE" AND
           p_nombres <> "DD"  AND
           p_nombres <> "EL"  AND
           p_nombres <> "LA"  AND
           p_nombres <> "LOS" AND
           p_nombres <> "LAS" AND
           p_nombres <> "LE"  AND
           p_nombres <> "LES" AND
           p_nombres <> "MAC" AND
           p_nombres <> "MC"  AND
           p_nombres <> "VAN" AND
           p_nombres <> "VON" AND
           p_nombres <> "Y"   ) THEN
         LET v_cadena = p_nombres;
      ELSE
         -- se le pone una X
         LET v_cadena = "X";
      END IF
   END IF

   -- se obtiene la primera letra del nombre
   IF ( SUBSTR(v_cadena,1,1) = "Ñ" ) THEN
      -- si es Ñ se cambia por X
      LET v_rfc_aux = TRIM(v_rfc_aux) || "X";
   ELSE
      -- se concatena la primera inicial
      LET v_rfc_aux = TRIM(v_rfc_aux) || SUBSTR(v_cadena,1,1);
   END IF

   -- se verifica que la parte del RFC sacada de los nombres no sea una palabra invalida altisonante
   LET v_altisonante = TRIM(v_rfc_aux);

   SELECT palabra_sust
   INTO   v_rfc_sustituto
   FROM   cat_rfc_inconv
   WHERE  palabra_inconv = v_altisonante;

   -- si se encontro, se usa la palabra sustituto
   IF ( v_rfc_sustituto IS NOT NULL ) THEN
      LET v_rfc_aux = v_rfc_sustituto;
   END IF

   -- ===================================================
   -- verificacion de la seccion de la fecha de nacimiento
   -- se obtiene la fecha de nacimiento
   LET v_fecha_nacimiento = SUBSTR(p_rfc_modificado,5,6);

   -- el mes no puede ser menor a 1 ni mayor a 12
   IF ( SUBSTR(v_fecha_nacimiento,1,2) < "00" OR SUBSTR(v_fecha_nacimiento,1,2) > "99" ) THEN
      LET v_mensaje = "El AÑO en la fecha del RFC es inválido";
      LET v_rfc_es_correcto = 1;
   END IF

   -- el mes no puede ser menor a 1 ni mayor a 12
   IF ( SUBSTR(v_fecha_nacimiento,3,2) < "01" OR SUBSTR(v_fecha_nacimiento,3,2) > "12" ) THEN
      LET v_mensaje = "El MES en la fecha del RFC es inválido";
      LET v_rfc_es_correcto = 1;
   ELSE
      -- se verifica que el dia sea valido
      IF ( SUBSTR(v_fecha_nacimiento,5,2) < "01" OR SUBSTR(v_fecha_nacimiento,5,2) > "31" ) THEN
         LET v_mensaje = "El DIA en la fecha del RFC es inválido";
         LET v_rfc_es_correcto = 1;
      ELSE
         -- la fecha es correcta, se concatena al rfc auxiliar
         LET v_rfc_aux = TRIM(v_rfc_aux)|| v_fecha_nacimiento;

         -- si el rfc modificado tiene homoclave, se concatena al rfc_auxiliar
         IF ( LENGTH(p_rfc_modificado) = 13 ) THEN
            LET v_rfc_aux = TRIM(v_rfc_aux)|| SUBSTR(p_rfc_modificado,11,3);
         END IF

         -- se verifica si el RFC calculado es igual al original modificado
         IF ( p_rfc_modificado <> v_rfc_aux ) THEN
            LET v_rfc_es_correcto = 1;
            LET v_mensaje = "El RFC difiere del obtenido por la función, pasado:"||p_rfc_modificado||" Obtenido:"||v_rfc_aux;
         END IF
      END IF
   END IF
    TRACE OFF;
   -- se devuelve el resultado de la consulta
   RETURN v_rfc_es_correcto, 0, p_rfc_modificado, v_mensaje;
END FUNCTION

;


