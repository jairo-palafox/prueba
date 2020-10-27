






CREATE FUNCTION "safreviv".fn_sep_liq_expedientes_solo_inf(p_usuario     CHAR(20),
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

   FOREACH SELECT UNIQUE mov.id_expediente,
                  mov.id_derechohabiente_invadido
             INTO v_id_expediente,
                  v_id_derechohabiente
             FROM sep_preliquida_solo_infonavit pre JOIN sep_movimiento_invadido mov
               ON mov.id_expediente = pre.id_referencia
            WHERE pre.folio_liquida = p_folio
   
         
      EXECUTE FUNCTION safre_viv:fn_desmarca_cuenta(v_id_derechohabiente,
                                                    280, -- marca entrada
                                                    v_id_expediente, -- identificador de referencia
                                                    0,
                                                    280, 
                                                    p_usuario,
                                                    p_proceso_cod)
                                               INTO r_cod_rechazo;
      IF (r_cod_rechazo <> 0) THEN
         LET v_total_rechazados = v_total_rechazados + 1;
      ELSE
         LET v_total_desmarcados = v_total_desmarcados + 1;
      END IF;
      
      --Ejecuta avance de maquinaria de expediente
      EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_expediente",
                                                v_id_expediente    ,
                                                "id_expediente",
                                                85,
                                                p_usuario)
                                           INTO v_ind,
                                                v_diag,
                                                v_estado_destino;
      IF (v_ind <> 0) THEN 
          LET v_ind_error = -1;
      END IF 
   
   END FOREACH;
   
   RETURN v_ind_error,
          v_sql_error,
          v_msg_error,
          v_total_rechazados,
          v_total_desmarcados;
END FUNCTION;


