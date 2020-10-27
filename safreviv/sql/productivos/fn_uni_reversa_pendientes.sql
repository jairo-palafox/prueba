






CREATE FUNCTION "safreviv".fn_uni_reversa_pendientes(p_folio DECIMAL(9,0))

  RETURNING INTEGER,
            CHAR(200),
            INTEGER,
            INTEGER;

-- Control de Excepciones
DEFINE sql_err                         INTEGER  ;
DEFINE isam_err                        INTEGER  ;
DEFINE err_txt                         CHAR(200);
DEFINE v_resultado                     SMALLINT ;
DEFINE v_total_unificadores            INTEGER  ;
DEFINE v_total_unificados              INTEGER  ;
DEFINE v_total_registros               INTEGER  ;
DEFINE v_id_unificador_procedencia     DECIMAL(9,0);
DEFINE v_des_id_unificador             DECIMAL(9,0);
DEFINE v_des_dor_id_derechohabiente    DECIMAL(9,0);
DEFINE v_des_dor_estado_unificacion    SMALLINT    ;
DEFINE v_des_id_unificado              DECIMAL(9,0);
DEFINE v_des_ado_id_derechohabiente    DECIMAL(9,0);
DEFINE v_des_ado_estado_unificacion    SMALLINT    ;
DEFINE v_rd_id_pre_unificador          DECIMAL(9,0);
DEFINE v_rd_folio_lote                 DECIMAL(9,0);
DEFINE v_rd_id_pre_unificado           DECIMAL(9,0);
DEFINE v_rd_folio_lote_ado             DECIMAL(9,0);
DEFINE v_det_estatus_convocatoria      SMALLINT;
DEFINE v_tot_marca_dor                 SMALLINT;
DEFINE v_tot_marca_ado                 SMALLINT; 

   ON EXCEPTION SET sql_err,
                    isam_err

      LET v_resultado          = sql_err;
      LET v_total_unificadores = 0;
      LET v_total_unificados   = 0;
      LET v_total_registros    = 0;

      RETURN v_resultado,
             err_txt,
             v_total_unificadores,
             v_total_unificados;
   END EXCEPTION

  --Se habilita el LOG del SP
  --SET DEBUG FILE TO '/home/jgomez/inf/uni/sql/reversa.trace';
  --TRACE ON;

LET v_resultado                     = 0;
LET sql_err                         = NULL;
LET isam_err                        = NULL;
LET err_txt                         = NULL;
LET v_total_unificadores            = 0;
LET v_total_unificados              = 0;
LET v_des_id_unificador             = 0;
LET v_des_dor_id_derechohabiente    = 0;
LET v_des_dor_estado_unificacion    = 0;
LET v_des_id_unificado              = 0;
LET v_des_ado_id_derechohabiente    = 0;
LET v_des_ado_estado_unificacion    = 0;
LET v_rd_id_pre_unificador          = 0;
LET v_rd_folio_lote                 = 0;
LET v_rd_id_pre_unificado           = 0;
LET v_rd_folio_lote_ado             = 0;
LET v_tot_marca_dor                 = 0;
LET v_tot_marca_ado                 = 0;
--

   FOREACH
      SELECT id_unificador,
             id_derechohabiente,
             estado_unificacion,
             estatus_convocatoria
      INTO   v_des_id_unificador,
             v_des_dor_id_derechohabiente,
             v_des_dor_estado_unificacion,
             v_det_estatus_convocatoria
      FROM   uni_det_unificador
      WHERE  folio_unificacion = p_folio
      AND    ind_procedencia = 0

      IF v_des_dor_estado_unificacion = 1 THEN
        --DESMARCAR UNIFICADOR
        SELECT COUNT(*)
        INTO   v_tot_marca_dor
        FROM   sfr_marca_activa
        WHERE  id_derechohabiente = v_des_dor_id_derechohabiente
        AND    n_referencia = v_des_id_unificador
        AND    folio = p_folio
        AND    marca = 501;
        
        IF (v_tot_marca_dor > 0) THEN
           EXECUTE PROCEDURE sp_reversa_marca(v_des_dor_id_derechohabiente,
                                              501,
                                              v_des_id_unificador,
                                              p_folio
                                             );

           FOREACH
              SELECT id_pre_unificador,
                     folio_lote
              INTO   v_rd_id_pre_unificador,
                     v_rd_folio_lote
              FROM   uni_pre_unificador
              WHERE  id_derechohabiente = v_des_dor_id_derechohabiente
              AND    estado = 10

              LET v_tot_marca_dor = 0;
              
			        SELECT COUNT(*)
			        INTO   v_tot_marca_dor
			        FROM   sfr_marca_activa
			        WHERE  id_derechohabiente = v_des_dor_id_derechohabiente
			        AND    n_referencia = v_rd_id_pre_unificador
			        AND    folio = v_rd_folio_lote
			        AND    marca = 511;
   
              IF (v_tot_marca_dor > 0) THEN 
	               ---REVERSAR DESMARCA DE 511
	               EXECUTE PROCEDURE sp_reversa_desmarca(v_des_dor_id_derechohabiente,
	                                                     511,
	                                                     v_rd_id_pre_unificador,
	                                                     v_rd_folio_lote);
	   
	               UPDATE uni_pre_unificador
	               SET    estado = 1
	               WHERE  id_pre_unificador = v_rd_id_pre_unificador
	               AND    id_derechohabiente = v_des_dor_id_derechohabiente
	               AND    estado = 10;
	            END IF    
           END FOREACH;
           LET v_tot_marca_dor = 0;
        END IF
        
         UPDATE uni_det_unificador
         SET    ind_procedencia = 2
         WHERE  folio_unificacion = p_folio
         AND    ind_procedencia = 0
         AND    id_unificador = v_des_id_unificador;
      END IF

      FOREACH
         SELECT id_unificado,
                id_derechohabiente,
                estado_unificacion
         INTO   v_des_id_unificado,
                v_des_ado_id_derechohabiente,
                v_des_ado_estado_unificacion
         FROM   uni_det_unificado
         WHERE  folio_unificacion = p_folio
         AND    id_unificador  = v_des_id_unificador

         IF v_des_ado_estado_unificacion = 1 THEN
            SELECT COUNT(*)
            INTO   v_tot_marca_ado
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = v_des_ado_id_derechohabiente
            AND    n_referencia = v_des_id_unificado
            AND    folio = p_folio
            AND    marca = 502;
            
            IF (v_tot_marca_ado > 1) THEN       
               --DESMARCAR UNIFICADO
               EXECUTE PROCEDURE sp_reversa_marca(v_des_ado_id_derechohabiente,
                                                  502,
                                                  v_des_id_unificado,
                                                  p_folio
                                                  );
               
               ---REVERSAR DESMARCA DE 512
               FOREACH
                  SELECT a.id_pre_unificado,
                         b.folio_lote
                  INTO   v_rd_id_pre_unificado,
                         v_rd_folio_lote_ado
                  FROM   uni_pre_unificado a,
                         uni_pre_unificador b
                  WHERE  a.id_pre_unificador  = b.id_pre_unificador
                  AND    a.id_derechohabiente = v_des_ado_id_derechohabiente
                  AND    a.estado = 10
               
                 LET v_tot_marca_ado = 0;

                 SELECT COUNT(*)
                 INTO   v_tot_marca_ado
                 FROM   sfr_marca_activa
                 WHERE  id_derechohabiente = v_des_ado_id_derechohabiente
                 AND    n_referencia = v_rd_id_pre_unificado
                 AND    folio = v_rd_folio_lote_ado
                 AND    marca = 512;
                 
                 IF (v_tot_marca_ado > 1) THEN 
                    EXECUTE PROCEDURE sp_reversa_desmarca(v_des_ado_id_derechohabiente,
                                                          512,
                                                          v_rd_id_pre_unificado,
                                                          v_rd_folio_lote_ado);
                    
                    UPDATE uni_pre_unificado
                    SET    estado = 1
                    WHERE  id_pre_unificado   = v_rd_id_pre_unificado
                    AND    id_derechohabiente = v_des_dor_id_derechohabiente
                    AND    estado = 10;
                 END IF
               END FOREACH;
            END IF
            
            LET v_tot_marca_ado = 0;
         END IF
      END FOREACH;
   END FOREACH;

   FOREACH
      SELECT id_unificador
      INTO   v_id_unificador_procedencia
      FROM   uni_det_procedencia
      WHERE  folio_resp_confronta = p_folio
      AND    ind_procedencia = 0

      DELETE FROM uni_det_unificado
      WHERE  id_unificador   = v_id_unificador_procedencia;

      DELETE FROM uni_det_unificador
      WHERE  id_unificador   = v_id_unificador_procedencia;

   END FOREACH

   IF (p_folio = 134364) THEN
   
	    DELETE FROM  uni_cza_unificacion
	    WHERE  folio_unificacion = p_folio
	    ;
	    DELETE FROM uni_det_unificador
	    WHERE folio_unificacion = p_folio
	    ;
	    DELETE FROM uni_det_unificado
	    WHERE folio_unificacion = p_folio
	    ;
	    DELETE FROM uni_sum_unificacion
	    WHERE  folio_unificacion = p_folio
	    ; 
	    DELETE FROM uni_det_rechazos
	    WHERE folio_unificacion = p_folio
	    ;
	    DELETE FROM uni_det_procedencia
	    WHERE folio_unificacion = p_folio
	    ;
	    DELETE FROM glo_folio
	    WHERE folio = p_folio
	    ;
	    DELETE FROM glo_ctr_archivo
	    WHERE  nombre_archivo = "OP2120191204C002.cnmu"
	    ;
	    UPDATE uni_det_procedencia
	    SET    folio_resp_confronta = NULL,
	           ind_procedencia      = 0
	    WHERE  folio_resp_confronta = p_folio
	    ;
      DELETE FROM uni_det_unificado
      WHERE folio_unificacion = p_folio
      ;
      DELETE FROM uni_det_unificador
      WHERE folio_unificacion = p_folio
      ;
   END IF

   DELETE FROM uni_det_procedencia
   WHERE folio_unificacion = p_folio
   AND   ind_procedencia = 0
   ;

   RETURN v_resultado,
          err_txt,
          v_total_unificadores,
          v_total_unificados;
END FUNCTION;


