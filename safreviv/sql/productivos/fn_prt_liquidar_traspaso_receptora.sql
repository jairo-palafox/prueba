






CREATE FUNCTION "safreviv".fn_prt_liquidar_traspaso_receptora(p_usuario_cod CHAR(20),
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

DEFINE v_id_prt_solicitud_receptora DECIMAL(9,0);
DEFINE v_id_prt_traspaso_receptora DECIMAL(9,0);

DEFINE v_tipo_traspaso CHAR(2);

DEFINE v_senal SMALLINT;
DEFINE v_id_maquinaria SMALLINT;
DEFINE v_tpo_operacion CHAR(02);

   LET v_senal         = 40; -- Liquidar solicitud receptora
   LET v_id_maquinaria = 3; -- Maquinaria de portabilidad receptora
   LET v_sql_error     = 0;
   LET v_msg_error     = "";
   LET v_tpo_operacion = "01";
   
   UPDATE glo_folio 
      SET status = 2 -- Liquidado
    WHERE folio = p_folio;
    
   UPDATE prt_cza_receptora
      SET estado = 30 -- Folio liquidado
    WHERE folio_liquida = p_folio;
    
   SELECT tipo_traspaso
     INTO v_tipo_traspaso
     FROM prt_cza_receptora
    WHERE folio_liquida = p_folio;

   FOREACH SELECT id_prt_solicitud_receptora,
                  id_prt_traspaso_receptora ,
                  tpo_operacion
             INTO v_id_prt_solicitud_receptora,
                  v_id_prt_traspaso_receptora ,
                  v_tpo_operacion
             FROM prt_traspaso_receptora rec
            WHERE folio_liquida = p_folio
              AND estado = 20 -- Traspaso preliquidado

      LET v_ind = 0; LET v_sql_error = 0;       

      IF( v_tpo_operacion = '01')THEN -- Traspaso corriente
         EXECUTE FUNCTION fn_glo_maq_individual(v_id_maquinaria,
                                                v_id_prt_solicitud_receptora,
                                                v_senal,
                                                p_usuario_cod) 
            INTO v_ind,
                 v_diag,
                 v_sql_error,
                 v_isam_error,
                 v_msg_error,
                 v_estado_destino;
         
         IF( v_ind <> 0 OR v_sql_error <> 0)THEN 
            LET v_estado_traspaso = 25; -- No liquidado
         ELSE
            LET v_estado_traspaso = 30; -- liquidado
         END IF
      ELSE -- Traspaso subsecuente y devoluciones
         LET v_estado_traspaso = 30; -- liquidado
      END IF
      
      UPDATE prt_traspaso_receptora
         SET estado = v_estado_traspaso
       WHERE id_prt_traspaso_receptora = v_id_prt_traspaso_receptora;
      
   END FOREACH

   RETURN v_sql_error,
          v_msg_error;
END FUNCTION
;


