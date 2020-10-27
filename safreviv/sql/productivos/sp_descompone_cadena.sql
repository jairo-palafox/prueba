






CREATE PROCEDURE "safreviv".sp_descompone_cadena(p_cadena lvarchar(1000))
RETURNING CHAR(004),CHAR(100),SMALLINT;
DEFINE v_diag      CHAR(104);
DEFINE v_diag_cod  CHAR(004);
DEFINE v_diag_desc CHAR(100);
DEFINE i           SMALLINT;
DEFINE j           SMALLINT;
DEFINE v_longitud  SMALLINT;
DEFINE v_letra     CHAR(1);
DEFINE v_cadena    LVARCHAR(1000);
DEFINE v_length    SMALLINT;
DEFINE v_ind       SMALLINT;

--  SET DEBUG FILE TO '/ds/safre/BD/sp_descompone_cadena.trace';

  LET v_diag_cod  = " ";
  LET v_diag_desc = " ";
  LET v_ind       = 1;

  LET v_longitud = LENGTH(TRIM(p_cadena));
  LET v_cadena   = p_cadena;
  LET j = 0;
  FOR i = 1 TO v_longitud
    LET v_letra = substr(p_cadena,i,i);
    IF v_letra = "|" THEN
       LET v_length = i - (j+1);
       LET v_diag = substr(v_cadena,j+1,v_length);

       -- se obtiene codigo a 4 posiciones y descripcion a 100
       LET v_diag_cod  = substr(TRIM(v_diag),1,4);
       LET v_diag_desc = substr(TRIM(v_diag),5,100);
       LET j = i;
       IF v_diag_cod <> " " AND v_diag_cod <> "    " THEN
           LET v_ind = 0;
           RETURN v_diag_cod,v_diag_desc,v_ind WITH RESUME;
       ELSE LET v_ind = 1; CONTINUE FOR; END IF;
    ELSE
      CONTINUE FOR;
    END IF;
  END FOR;
  IF v_ind = 1 THEN
     RETURN v_diag_cod , v_diag_desc, v_ind;
  END IF;
END PROCEDURE;


