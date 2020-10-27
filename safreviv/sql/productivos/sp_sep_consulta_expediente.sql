






CREATE FUNCTION "safreviv".sp_sep_consulta_expediente(p_invadido char(11) ,
                                           p_asociado char(11) ,
                                           p_estado   smallint )

RETURNING SMALLINT     ,    -- v_ind
          DECIMAL(9,0) ,    -- v_id_expediente
          INTEGER      ;   -- v_sql_error

DEFINE v_id_expediente DEC(9,0)        ;
DEFINE v_invadido      CHAR(11)        ;
DEFINE v_asociado      CHAR(11)        ;

DEFINE v_txt             CHAR(1000)    ;
DEFINE v_where           CHAR(200)     ;

DEFINE v_ind             SMALLINT      ;
DEFINE v_sql_error       INTEGER       ;


   ON EXCEPTION SET v_sql_error

      LET v_ind  = -1   ;

      RETURN v_ind           ,
             v_id_expediente ,
             v_sql_error     ;

   END EXCEPTION WITH RESUME

--SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_consulta_expediente.trace';

--
--asignacion inicial de variables para evitar el error -696
--

      LET v_ind           = 0 ;
      LET v_sql_error     = '';
      LET v_id_expediente = 0 ;
      LET v_invadido      = '';
      LET v_asociado      = '';
--
-- construccion de consulta dinamica para expedientes por estado y tipo de flujo
--

LET v_txt=" select a.id_expediente "||
          " from sep_expediente a,sep_cat_tipo_flujo b,sep_cat_canal_recepcion_exp c,sep_estado_expediente d "||
          " where a.estado = "||p_estado||" and a.flujo_cod = b.flujo_cod and a.canal_cod = c.canal_cod and a.estado = d.estado;";

--trace "query: "||v_txt;

      PREPARE prp_consulta FROM v_txt;
      DECLARE cur_consulta CURSOR FOR prp_consulta;
      OPEN cur_consulta;

      WHILE (SQLCODE ==0)

      FETCH cur_consulta INTO v_id_expediente ; 

      LET v_invadido = '';
      LET v_asociado = '';

--
--verificacion de datos de expedientes encontrados
--

--se busca invadido

     SELECT a.nss
     INTO   v_invadido
     FROM   sep_nss_expediente a
     WHERE  a.id_expediente = v_id_expediente
     AND    a.tipo_nss      = 1
     AND    a.nss           = p_invadido;

     IF v_invadido IS NULL OR v_invadido = '' THEN
        LET v_invadido = '';
     END IF

    --trace "invadido: "||v_invadido;

-- se busca asociado

     SELECT a.nss
     INTO   v_asociado
     FROM   sep_nss_expediente a
     WHERE  a.id_expediente = v_id_expediente
     AND    a.tipo_nss      = 2
     AND    a.nss           = p_asociado;

     IF v_asociado IS NULL OR v_asociado = '' THEN
        LET v_asociado = '';
     END IF

    --trace "asociado: "||v_asociado;


     IF v_invadido <> '' and v_asociado <> '' THEN

        LET v_ind = 1;

        RETURN v_ind           ,
               v_id_expediente ,
               v_sql_error     ;
     END IF

END WHILE

    IF v_ind <> 1 THEN

       RETURN v_ind           ,
              v_id_expediente ,
              v_sql_error     ;

    END IF

END FUNCTION;


