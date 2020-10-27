






CREATE FUNCTION "safreviv".fn_prt_liquidar_traspaso_cedente(p_usuario_cod CHAR(20),
                                                 p_folio       DECIMAL(9,0))

RETURNING SMALLINT,
          VARCHAR(254);

DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);
DEFINE v_sql_error      INTEGER;
DEFINE v_isam_error     INTEGER;
DEFINE v_msg_error      VARCHAR(254);
DEFINE v_estado_destino SMALLINT;

DEFINE v_estado_traspaso SMALLINT;

DEFINE v_id_prt_solicitud_cedente DECIMAL(9,0);
DEFINE v_id_prt_traspaso_cedente DECIMAL(9,0);

DEFINE v_tipo_traspaso CHAR(2);

DEFINE v_senal SMALLINT;
DEFINE v_id_maquinaria SMALLINT;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_prt_liquidar_traspaso_cedente.trace';
   --TRACE ON;

   LET v_senal         = 70; -- Liquidar solicitud cedente
   LET v_id_maquinaria = 2; -- Maquinaria de portabilidad cedente
   LET v_sql_error     = 0;
   LET v_msg_error     = "";
   
   UPDATE glo_folio 
      SET status = 2 -- Liquidado
    WHERE folio = p_folio;
    
   UPDATE prt_cza_cedente
      SET estado = 40 -- Folio liquidado
    WHERE folio_liquida = p_folio;
    
   SELECT tipo_traspaso
     INTO v_tipo_traspaso
     FROM prt_cza_cedente
    WHERE folio_liquida = p_folio;

   FOREACH SELECT id_prt_solicitud_cedente,
                  id_prt_traspaso_cedente
             INTO v_id_prt_solicitud_cedente,
                  v_id_prt_traspaso_cedente
             FROM prt_traspaso_cedente
            WHERE folio_liquida = p_folio
              AND estado = 30 -- Traspaso preliquidado
             
      IF( v_tipo_traspaso = '01')THEN -- Traspaso corriente
         EXECUTE FUNCTION fn_glo_maq_individual(v_id_maquinaria,
                                                v_id_prt_solicitud_cedente,
                                                v_senal,
                                                p_usuario_cod) 
            INTO v_ind,
                 v_diag,
                 v_sql_error,
                 v_isam_error,
                 v_msg_error,
                 v_estado_destino;
         
         IF( v_ind <> 0 OR v_sql_error <> 0)THEN 
            LET v_estado_traspaso = 35; -- No liquidado
         ELSE
            LET v_estado_traspaso = 40; -- liquidado
         END IF
      ELSE -- Traspaso subsecuente
         LET v_estado_traspaso = 40; -- liquidado
      END IF
      
      UPDATE prt_traspaso_cedente
         SET estado = v_estado_traspaso
       WHERE id_prt_traspaso_cedente = v_id_prt_traspaso_cedente;
      
   END FOREACH

   RETURN v_sql_error,
          v_msg_error;
END FUNCTION;


