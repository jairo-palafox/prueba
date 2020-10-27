






CREATE PROCEDURE "safreviv".sp_uni_inserta_rechazo(p_rch_folio DECIMAL(9,0),
                                        p_rch_consec DECIMAL(1,0),
                                        p_rch_tipo_registro SMALLINT,
                                        p_rch_id_referencia DECIMAL(9,0),
                                        p_rch_result_operacion CHAR(2),
                                        p_rch_diagnostico SMALLINT,
                                        p_rch_campo_valor CHAR(50))

   INSERT INTO uni_det_rechazos
     (id_rechazo,
      folio_unificacion,
      consec_motivo,   
      tipo_registro,
      id_referencia,
      result_operacion,
      diagnostico,
      campo_valor)
    VALUES
     (seq_uni_det_rechazos.NEXTVAL, --id_rechazo
      p_rch_folio,
      p_rch_consec,
      p_rch_tipo_registro,
      p_rch_id_referencia,
      p_rch_result_operacion,
      p_rch_diagnostico,
      p_rch_campo_valor);

END PROCEDURE -- sp_uni_inserta_rechazo
;


