






CREATE PROCEDURE "safreviv".sp_bus_convierte_arreglo(p_id_registro           VARCHAR(120),     -- consecutivo unico que identifica el registro
                                          p_campo_unico           VARCHAR(120),     -- nombre del campo unico de la tabla de negocio
                                          p_campo_multiset        VARCHAR(120),     -- nombre del campo multiset/arreglo
                                          p_tabla_negocio         VARCHAR(120),     -- nombre de la tabla de negocio
                                          p_proceso               CHAR(003),     -- codigo de proceso
                                          p_operacion             CHAR(004))     -- codigo de operacion
RETURNING SMALLINT, 
          DECIMAL(9,0), 
          VARCHAR(050),
          VARCHAR(120);

DEFINE estructura1  MULTISET(ROW(c1 VARCHAR(120))NOT NULL);
DEFINE estructura2  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120))NOT NULL);
DEFINE estructura3  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120))NOT NULL);
DEFINE estructura4  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120))NOT NULL);
DEFINE estructura5  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120))NOT NULL);
DEFINE estructura6  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120))NOT NULL);
DEFINE estructura7  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120)) NOT NULL);
DEFINE estructura8  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120)) NOT NULL);
DEFINE estructura9  MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120)) NOT NULL);
DEFINE estructura10 MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120),c10 VARCHAR(120)) NOT NULL);
DEFINE estructura11 MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120),c10 VARCHAR(120),c11 VARCHAR(120)) NOT NULL);
DEFINE estructura12 MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120),c10 VARCHAR(120),c11 VARCHAR(120),c12 VARCHAR(120)) NOT NULL);
DEFINE estructura13 MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120),c10 VARCHAR(120),c11 VARCHAR(120),c12 VARCHAR(120),c13 VARCHAR(120)) NOT NULL);
DEFINE estructura14 MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120),c10 VARCHAR(120),c11 VARCHAR(120),c12 VARCHAR(120),c13 VARCHAR(120),c14 VARCHAR(120)) NOT NULL);
DEFINE estructura15 MULTISET(ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120),c10 VARCHAR(120),c11 VARCHAR(120),c12 VARCHAR(120),c13 VARCHAR(120),c14 VARCHAR(120),c15 VARCHAR(120)) NOT NULL);

DEFINE v_valores ROW(c1 VARCHAR(120),c2 VARCHAR(120),c3 VARCHAR(120),c4 VARCHAR(120),c5 VARCHAR(120),c6 VARCHAR(120),c7 VARCHAR(120),c8 VARCHAR(120),c9 VARCHAR(120),c10 VARCHAR(120),c11 VARCHAR(120),c12 VARCHAR(120),c13 VARCHAR(120),c14 VARCHAR(120),c15 VARCHAR(120));

DEFINE i                   SMALLINT;
DEFINE v_seq_bloque        DECIMAL(9,0);

DEFINE tot_campos          SMALLINT;
DEFINE cadena              CHAR(1000);

DEFINE v_tag               VARCHAR(120);
DEFINE v_valor_elemento    VARCHAR(120);

DEFINE v_c1                VARCHAR(120);
DEFINE v_c2                VARCHAR(120);
DEFINE v_c3                VARCHAR(120);
DEFINE v_c4                VARCHAR(120);
DEFINE v_c5                VARCHAR(120);
DEFINE v_c6                VARCHAR(120);
DEFINE v_c7                VARCHAR(120);
DEFINE v_c8                VARCHAR(120);
DEFINE v_c9                VARCHAR(120);
DEFINE v_c10                VARCHAR(120);
DEFINE v_c11                VARCHAR(120);
DEFINE v_c12                VARCHAR(120);
DEFINE v_c13                VARCHAR(120);
DEFINE v_c14                VARCHAR(120);
DEFINE v_c15                VARCHAR(120);

--SET DEBUG FILE TO '/ds/safrebus/sp_prueba_lista.trace';
--TRACE ON;

SELECT COUNT(*)
INTO   tot_campos
FROM sysxtdtypes  a  ,
     sysattrtypes b  , -- multiset
     sysattrtypes b1 ,
     systables    c  ,
     syscolumns   d
WHERE d.colname = p_campo_multiset
AND   d.coltype = 20
AND   c.tabname = p_tabla_negocio
AND   d.tabid = c.tabid
AND   d.extended_id = b.extended_id -- extrae multiset
AND   b.levelno = 1
AND   b.xtd_type_id = b1.extended_id -- liga el multiset al row type
AND   b1.levelno = 1
AND   b1.extended_id = a.extended_id;

LET cadena = TRIM("SELECT "||TRIM(p_campo_multiset)||" FROM "||TRIM(p_tabla_negocio)||" WHERE "||TRIM(p_campo_unico)||" = "||"'"||TRIM(p_id_registro)||"'");

PREPARE prp_cadena FROM cadena;
DECLARE cur_cadena CURSOR FOR prp_cadena;
OPEN cur_cadena;

IF tot_campos = 1 THEN FETCH cur_cadena INTO estructura1; END IF;
IF tot_campos = 2 THEN FETCH cur_cadena INTO estructura2; END IF;
IF tot_campos = 3 THEN FETCH cur_cadena INTO estructura3; END IF;
IF tot_campos = 4 THEN FETCH cur_cadena INTO estructura4; END IF;
IF tot_campos = 5 THEN FETCH cur_cadena INTO estructura5; END IF;
IF tot_campos = 6 THEN FETCH cur_cadena INTO estructura6; END IF;
IF tot_campos = 7 THEN FETCH cur_cadena INTO estructura7; END IF;
IF tot_campos = 8 THEN FETCH cur_cadena INTO estructura8; END IF;
IF tot_campos = 9 THEN FETCH cur_cadena INTO estructura9; END IF;
IF tot_campos = 10 THEN FETCH cur_cadena INTO estructura10; END IF;
IF tot_campos = 11 THEN FETCH cur_cadena INTO estructura11; END IF;
IF tot_campos = 12 THEN FETCH cur_cadena INTO estructura12; END IF;
IF tot_campos = 13 THEN FETCH cur_cadena INTO estructura13; END IF;
IF tot_campos = 14 THEN FETCH cur_cadena INTO estructura14; END IF;
IF tot_campos = 15 THEN FETCH cur_cadena INTO estructura15; END IF;

CLOSE cur_cadena;
FREE cur_cadena;

IF tot_campos = 1 THEN

  FOREACH cur_multiset FOR
     SELECT c1
     INTO   v_c1
     FROM TABLE ( estructura1) AS e

     LET v_valores = ROW(v_c1,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque,v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 2 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2
     INTO   v_c1,v_c2
     FROM TABLE ( estructura2 ) AS e

     LET v_valores = ROW(v_c1,v_c2,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque,v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 3 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3
     INTO   v_c1,v_c2,v_c3
     FROM TABLE ( estructura3 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque,v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 4 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4
     INTO   v_c1,v_c2,v_c3,v_c4
     FROM TABLE ( estructura4 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque,v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 5 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5
     FROM TABLE ( estructura5 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 6 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6
     FROM TABLE ( estructura6 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 7 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7
     FROM TABLE ( estructura7 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 8 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8
     FROM TABLE ( estructura8 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 9 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8,c9
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9
     FROM TABLE ( estructura9 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,NULL,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 10 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8,c9,c10
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10
     FROM TABLE ( estructura10 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,NULL,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 11 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11
     FROM TABLE ( estructura11 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,NULL,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 12 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12
     FROM TABLE ( estructura12 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12,NULL,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 13 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12,v_c13
     FROM TABLE ( estructura13 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12,v_c13,NULL,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 14 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12,v_c13,v_c14
     FROM TABLE ( estructura14 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12,v_c13,v_c14,NULL);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
ELIF tot_campos = 15 THEN
  FOREACH cur_multiset FOR
     SELECT c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
     INTO   v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12,v_c13,v_c14,v_c15
     FROM TABLE ( estructura15 ) AS e

     LET v_valores = ROW(v_c1,v_c2,v_c3,v_c4,v_c5,v_c6,v_c7,v_c8,v_c9,v_c10,v_c11,v_c12,v_c13,v_c14,v_c15);
     LET v_seq_bloque = seq_bus_bloque_multiset.NEXTVAL;

     FOR i = 1 TO tot_campos
         EXECUTE PROCEDURE sp_bus_liga_tag_elemento(v_valores,p_campo_multiset,p_tabla_negocio,i,p_proceso,p_operacion) INTO v_tag,v_valor_elemento;
         RETURN i,v_seq_bloque, v_tag, v_valor_elemento WITH RESUME;
     END FOR;
  END FOREACH;
END IF

END PROCEDURE;


