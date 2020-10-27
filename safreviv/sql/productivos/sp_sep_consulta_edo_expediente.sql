






CREATE FUNCTION "safreviv".sp_sep_consulta_edo_expediente(p_estado smallint ,
                                           p_flujo  smallint )

RETURNING SMALLINT     ,   -- v_ind
          CHAR(3)      ,   -- vi_diag
          INTEGER      ,   -- v_sql_error
          DECIMAL(9,0) ,   -- id_expediente
          CHAR(011)    ,   -- invadido
          CHAR(011)    ,   -- asociado
          CHAR(40)     ,   -- flujo
          CHAR(40)     ,   -- canal
          DATE         ,   -- fecha recepcion
          DATE         ,   -- fecha captura
          CHAR(20)     ;   -- estado


-- 
--variables de los expedientes a regresar
--

DEFINE v_id_expediente   DECIMAL(9,0)  ;
DEFINE v_invadido        CHAR(11)      ;
DEFINE v_asociado        CHAR(11)      ;
DEFINE v_flujo           CHAR(40)      ;
DEFINE v_canal           CHAR(40)      ;
DEFINE v_f_recepcion     DATE          ;
DEFINE v_f_captura       DATE          ;
DEFINE v_estado          CHAR(20)      ;

DEFINE v_txt             CHAR(1000)    ;
DEFINE v_where           CHAR(200)     ;

DEFINE v_ind             SMALLINT      ;
DEFINE v_diag            CHAR(003)     ;
DEFINE v_sql_error       INTEGER       ;


   ON EXCEPTION SET v_sql_error

      LET v_ind  = -1   ;
      LET v_diag = '001';

      RETURN v_ind           , 
             v_diag          ,
             v_sql_error     ,
             v_id_expediente ,
             v_invadido      ,
             v_asociado      ,
             v_flujo         ,
             v_canal         ,
             v_f_recepcion   ,
             v_f_captura     ,
             v_estado        ;

   END EXCEPTION WITH RESUME
 
--SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_consulta_edo_expediente.trace';

--
-- CONSTURCCION DE CONSTRUCT
--

      IF (p_estado = 0 and p_flujo = 0 ) THEN
       LET v_where = "1=1";
      END IF;

      IF (p_estado = 0 and p_flujo <> 0 ) THEN
       LET v_where = " a.flujo_cod = "||p_flujo;
      END IF;

      IF (p_estado <> 0 and p_flujo = 0 ) THEN
       LET v_where = " a.estado = "||p_estado;
      END IF;

      IF (p_estado <> 0 and p_flujo <> 0 ) THEN
       LET v_where = " a.estado = "||p_estado||" AND a.flujo_cod = "||p_flujo;
      END IF;

--
--asignacion inicial de variables para evitar el error -696
--

      LET v_id_expediente = '';
      LET v_asociado      = '';
      LET v_invadido      = '';
      LET v_flujo         = '';
      LET v_canal         = '';
      LET v_f_recepcion   = '';
      LET v_f_captura     = '';
      LET v_estado        = '';
      LET v_ind           = 0;
      LET v_diag          = "000";
      LET v_sql_error    = '';

--
-- construccion de consulta dinamica para expedientes por estado y tipo de flujo
--

LET v_txt=" select a.id_expediente,b.flujo_desc,c.canal_desc,a.f_recepcion_infonavit,a.f_captura,d.descripcion"||
          " from sep_expediente a,sep_cat_tipo_flujo b,sep_cat_canal_recepcion_exp c,sep_estado_expediente d "||
          " where "||v_where||" and a.flujo_cod = b.flujo_cod and a.canal_cod = c.canal_cod and a.estado = d.estado;";

      --TRACE v_txt;
      PREPARE prp_consulta FROM v_txt;
      DECLARE cur_consulta CURSOR FOR prp_consulta;
      OPEN cur_consulta;

      WHILE (SQLCODE ==0)

      FETCH cur_consulta INTO v_id_expediente  ,
                              v_flujo          ,
                              v_canal          ,
                              v_f_recepcion    ,
                              v_f_captura      ,
                              v_estado         ;


--
--verificacion de datos de epedientes encontrados
--

    --trace "expediente: "||v_id_expediente;
    --trace "v_flujo: "||v_flujo;
    --trace "v_canal: "||v_canal;
    --trace "v_f_recepcion: "||v_f_recepcion;
    --trace "v_f_captura: "||v_f_captura;
    --trace "v_estado: "||v_estado;

--se busca invadido

     SELECT a.nss
     INTO   v_invadido
     FROM   sep_nss_expediente a
     WHERE  a.id_expediente = v_id_expediente
     AND    a.tipo_nss      = 1;

     IF v_invadido IS NULL OR v_invadido = '' THEN 
        LET v_invadido = '';
     END IF


    --trace "invadido: "||v_invadido;

-- se busca asociado

     SELECT a.nss
     INTO   v_asociado
     FROM   sep_nss_expediente a
     WHERE  a.id_expediente = v_id_expediente
     AND    a.tipo_nss      = 2;

     IF v_asociado IS NULL OR v_asociado = '' THEN 
        LET v_asociado = '';
     END IF

    --trace "asociado: "||v_asociado;

-- regresa datos encontrados mientras existan

   IF (SQLCODE == 0) THEN

     RETURN v_ind             ,
            v_diag            ,
            v_sql_error       ,
            v_id_expediente   ,
            v_invadido        ,
            v_asociado        ,
            v_flujo           ,
            v_canal           ,
            v_f_recepcion     ,
            v_f_captura       ,
            v_estado          WITH RESUME  ;
   END IF

END WHILE

END FUNCTION;


