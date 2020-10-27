






CREATE FUNCTION "safreviv".fn_sep_liq_expedientes_fondo72(p_usuario     CHAR(20),
                                               p_proceso_cod SMALLINT, 
                                               p_folio       DECIMAL(9,0))
RETURNING SMALLINT,
          INTEGER,
          CHAR(200),
          INTEGER,
          INTEGER;

DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  CHAR(200);

-- varibles para datos recuperados
DEFINE v_id_expediente      DECIMAL(9,0);
DEFINE v_id_derechohabiente DECIMAL(9,0);
-- datos para desmarcar cuenta

DEFINE r_cod_rechazo      SMALLINT;
DEFINE v_ind_error        SMALLINT;

DEFINE v_estado_destino SMALLINT;
DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);

DEFINE v_total_rechazados  INTEGER;
DEFINE v_total_desmarcados INTEGER;

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_msg_error
      LET v_ind_error = 0;
      LET v_total_rechazados  = 0;
      LET v_total_desmarcados = 0;
      RETURN v_ind_error,
             v_sql_error,
             v_msg_error,
             v_total_rechazados,
             v_total_desmarcados;
   END EXCEPTION ;

   -- inicializa variables de retorno
   LET v_ind_error = 0;
   LET v_sql_error = 0;
   LET v_msg_error = NULL;
   LET v_total_rechazados  = 0;
   LET v_total_desmarcados = 0;


   FOREACH SELECT mov.id_expediente_fondo72
           INTO   v_id_expediente
           FROM   cta_fondo72 cta  ,
                  sep_movimiento_invadido_fondo72 mov
           WHERE  cta.folio_liquida = p_folio 
           AND    cta.movimiento    = 382 -- mov cargo x separación 
           AND    cta.id_referencia = mov.id_sep_movimiento_invadido_fondo72
           GROUP BY 1

      --Ejecuta avance de maquinaria de expediente

          UPDATE sep_expediente_fondo72 
          SET    estado = 85 -- liquidado
          WHERE  id_expediente_fondo72 = v_id_expediente;

   END FOREACH;
   
   RETURN v_ind_error,
          v_sql_error,
          v_msg_error,
          v_total_rechazados,
          v_total_desmarcados;
END FUNCTION;


