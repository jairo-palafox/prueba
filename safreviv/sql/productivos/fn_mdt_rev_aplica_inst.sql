






CREATE PROCEDURE "safreviv".fn_mdt_rev_aplica_inst (p_folio_dispersion DECIMAL(9,0), p_folio_pago DECIMAL(9,0))
                             RETURNING SMALLINT, SMALLINT, DECIMAL(9,0);

   DEFINE v_instrucciones_afectadas INTEGER;
   DEFINE v_id_ctr_aplica_mandato   DECIMAL(9,0);
   DEFINE v_id_det_aplica_mandato   DECIMAL(9,0);
   DEFINE v_id_det_aplica_monto     DECIMAL(9,0);
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_mdt_rev_aplica_inst.trace';
   --TRACE 'Inicia el store procedure de reverso de mandatos';

   --TRACE "Parte 1 - verifica datos validos ";
   
   IF p_folio_dispersion = 0 OR p_folio_dispersion IS NULL THEN
      RETURN 1,1,0;
   END IF;

   IF p_folio_pago = 0 OR p_folio_pago IS NULL THEN
      RETURN 1,1,0;
   END IF;

   LET v_id_ctr_aplica_mandato = 0;
   
   SELECT NVL(id_ctr_aplica_mandato,0)
     INTO v_id_ctr_aplica_mandato
     FROM safre_viv:mdt_ctr_aplica_mandato
    WHERE folio_dispersion = p_folio_dispersion
      AND folio_pago       = p_folio_pago;

   --TRACE "Parte 2 - verifica lote con estado 101 ";
   IF v_id_ctr_aplica_mandato = 0 OR v_id_ctr_aplica_mandato IS NULL THEN
      RETURN 1,1,0;
   END IF;
   
   LET v_instrucciones_afectadas = 0;
   
   FOREACH SELECT id_det_aplica_mandato INTO v_id_det_aplica_mandato
             FROM safre_viv:mdt_det_aplica_mandato
            WHERE id_ctr_aplica_mandato = v_id_ctr_aplica_mandato

      FOREACH SELECT id_det_aplica_monto INTO v_id_det_aplica_monto
                FROM safre_viv:mdt_det_aplica_monto
               WHERE id_det_aplica_mandato = v_id_det_aplica_mandato

          DELETE FROM safre_viv:mdt_det_aplica_monto
           WHERE id_det_aplica_monto = v_id_det_aplica_monto;
          
      END FOREACH;
      
      DELETE FROM safre_viv:mdt_det_aplica_mandato
       WHERE id_det_aplica_mandato = v_id_det_aplica_mandato;
      
      IF SQLCODE = 0 THEN
         LET v_instrucciones_afectadas = v_instrucciones_afectadas + 1;
      END IF;
   
   END FOREACH;
   
   DELETE FROM safre_viv:mdt_ctr_aplica_mandato
    WHERE id_ctr_aplica_mandato = v_id_ctr_aplica_mandato;
   
   --TRACE "Fin y reverso del SP";
   RETURN 0,0, v_instrucciones_afectadas;

END PROCEDURE;


