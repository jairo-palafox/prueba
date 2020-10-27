






CREATE PROCEDURE "safreviv".sp_baja_mandato(p_id_solicitud_mandato DECIMAL(10,0),
                                            p_id_credito           DECIMAL(10,0),
                                            p_id_derechohabiente   DECIMAL(10,0),
                                            p_v_usuario            CHAR(20)     )
   RETURNING INTEGER;
   
   DEFINE v_num_credito      DECIMAL(10,0);
   DEFINE v_existe           INTEGER;
   DEFINE v_id_ctr_mandato   DECIMAL(9,0);
   DEFINE v_estado           SMALLINT;

   -- arreglo insert mdt_his_mandato
   DEFINE v_h_id_mdt_his_mandato      DECIMAL(9,0);
   DEFINE v_h_id_det_ctr_mandato      DECIMAL(9,0);
   DEFINE v_h_id_cat_dato_actualizado SMALLINT    ;
   DEFINE v_h_f_modificacion          DATE        ;
   DEFINE v_h_valor_modificado        CHAR(40)    ;
   DEFINE v_h_valor_actual            CHAR(40)    ;
   
   DEFINE v_id_cat_mandato            SMALLINT    ;
   DEFINE v_cve_mandato               CHAR(18)    ;
   
   --Se habilita el LOG del SP

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_baja_mandato.trace';
   --TRACE 'Inicia el store procedure de baja de mandatos';
   --TRACE "Parte 1 - verifica existencia en mdt_ctr_mandato ";
   --TRACE "p_id_credito           - "||p_id_credito;
   --TRACE "p_id_derechohabiente   - "||p_id_derechohabiente;
   --TRACE "p_id_solicitud_mandato - "||p_id_solicitud_mandato;

   SELECT NVL(id_ctr_mandato,0) INTO v_id_ctr_mandato
     FROM safre_viv:mdt_ctr_mandato
    WHERE id_credito         = p_id_credito         
      AND id_derechohabiente = p_id_derechohabiente;

   IF v_id_ctr_mandato = 0 OR v_id_ctr_mandato IS NULL THEN
      UPDATE safre_viv:mdt_solicitud_mandato
         SET estado = 106, diagnostico = '003'
       WHERE id_solicitud_mandato = p_id_solicitud_mandato;
       
       RETURN 106;
   END IF;

   --TRACE "Parte 2 - verifica existencia en mdt_det_ctr_mandato ";

   SELECT p.id_cat_mandato   ,
          p.cve_mandato
     INTO v_id_cat_mandato , 
          v_cve_mandato
     FROM safre_viv:mdt_solicitud_mandato   s , 
          safre_viv:mdt_cat_mandato         c ,
          safre_viv:mdt_cat_mandato_paquete p
    WHERE s.id_solicitud_mandato = p_id_solicitud_mandato
      AND s.cve_mandato          = p.cve_mandato
      AND p.id_cat_mandato       = c.id_cat_mandato ;
   
   --TRACE "Recupera v_id_cat_mandato - "||v_id_cat_mandato;

   SELECT UNIQUE NVL(estado,0), id_det_ctr_mandato INTO v_estado, v_h_id_det_ctr_mandato
     FROM safre_viv:mdt_det_ctr_mandato
     WHERE id_ctr_mandato = v_id_ctr_mandato 
     AND   id_cat_mandato = v_id_cat_mandato
     AND   cve_mandato    = v_cve_mandato  ;
   
   --TRACE "Recupera v_estado               - "||v_estado;
   --TRACE "Recupera v_h_id_det_ctr_mandato - "||v_h_id_det_ctr_mandato;

   IF v_estado = 0 OR v_estado IS NULL THEN
      UPDATE safre_viv:mdt_solicitud_mandato
         SET estado = 106, diagnostico = '004'
       WHERE id_solicitud_mandato = p_id_solicitud_mandato;
       
       RETURN 106;
   END IF;

   --TRACE "Parte 3 - verifica estado actual de mdt_det_ctr_mandato ";

   IF v_estado = 106 THEN
      UPDATE safre_viv:mdt_solicitud_mandato
         SET estado = 106, diagnostico = '005'
       WHERE id_solicitud_mandato = p_id_solicitud_mandato;
       
       RETURN 106;
   END IF

   IF v_estado = 103 OR v_estado = 104 THEN
      UPDATE safre_viv:mdt_solicitud_mandato
         SET estado = 102
       WHERE id_solicitud_mandato = p_id_solicitud_mandato;

      -- Valida si inserta en mdt_his_mandato 
      SELECT NVL(a.id_cat_dato_actualizado,0)
        INTO v_h_id_cat_dato_actualizado
      FROM safre_viv:mdt_cat_dato_actualizado a
      WHERE a.cve_natural = 'estado';
      
  --TRACE "Recupera v_h_id_cat_dato_actualizado - "||v_h_id_cat_dato_actualizado;
      IF v_h_id_cat_dato_actualizado > 0 THEN
         
         LET v_h_f_modificacion = TODAY;
         
         LET v_h_id_mdt_his_mandato = seq_mdt_his_mandato.NEXTVAL;  

         -- Inserta en mdt_his_mandato
         -- se agrega el parametro de usuario p_v_usuairo SDL JUL12 

         INSERT INTO safre_viv:mdt_his_mandato VALUES (
                                             v_h_id_mdt_his_mandato     ,
                                             v_h_id_det_ctr_mandato     ,
                                             v_h_id_cat_dato_actualizado,
                                             v_h_f_modificacion         ,
                                             v_estado                   ,
                                             106                        ,
                                             p_id_solicitud_mandato   
                                             );
      END IF;
     
      UPDATE safre_viv:mdt_det_ctr_mandato
         SET estado = 106
       WHERE id_det_ctr_mandato     = v_h_id_det_ctr_mandato;
       
   END IF
   
   --TRACE "Fin del SP";
   
   RETURN 102;
   
END PROCEDURE;


