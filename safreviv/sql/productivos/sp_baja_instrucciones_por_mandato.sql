






CREATE PROCEDURE "safreviv".sp_baja_instrucciones_por_mandato(p_id_cat_mandato DECIMAL(10,0))
                             RETURNING INTEGER;
   
   DEFINE v_existe           INTEGER;

   -- arreglo insert mdt_his_mandato
   DEFINE v_h_id_mdt_his_mandato      DECIMAL(9,0);
   DEFINE v_h_id_det_ctr_mandato      DECIMAL(9,0);
   DEFINE v_h_id_cat_dato_actualizado SMALLINT    ;
   DEFINE v_h_f_modificacion          DATE        ;
   DEFINE v_estado                    SMALLINT    ;
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO 'baja_instrucciones_por_mandato.trace';
   --TRACE 'Inicia el store procedure de baja de instrucciones de mandatos para un mandato especifico';

   --TRACE "Parte 1 - verifica existencia de solicitudes de mandato ";
   
   LET v_existe = 0;
   
   FOREACH SELECT s.estado, s.id_det_ctr_mandato INTO v_estado, v_h_id_det_ctr_mandato
             FROM safre_viv:mdt_det_ctr_mandato s, safre_viv:mdt_cat_mandato c
            WHERE c.id_cat_mandato = p_id_cat_mandato
              AND s.id_cat_mandato = c.id_cat_mandato
              AND s.estado in (103,104)
   
      -- Valida si inserta en mdt_his_mandato 
      SELECT NVL(a.id_cat_dato_actualizado,0)
        INTO v_h_id_cat_dato_actualizado
      FROM safre_viv:mdt_cat_dato_actualizado a
      WHERE a.cve_natural = 'estado';
      
      --TRACE "Recupera v_h_id_cat_dato_actualizado - "||v_h_id_cat_dato_actualizado;
      IF v_h_id_cat_dato_actualizado > 0 THEN
         
         LET v_h_f_modificacion = TODAY;
         
         SELECT NVL(MAX(id_mdt_his_mandato),0)+1  
           INTO v_h_id_mdt_his_mandato
           FROM safre_viv:mdt_his_mandato;
           
         -- Inserta en mdt_his_mandato 
         --TRACE "Inserta historia de v_h_id_det_ctr_mandato - "||v_h_id_det_ctr_mandato;
         INSERT INTO safre_viv:mdt_his_mandato VALUES (v_h_id_mdt_his_mandato     ,
                                             v_h_id_det_ctr_mandato     ,
                                             v_h_id_cat_dato_actualizado,
                                             v_h_f_modificacion         ,
                                             v_estado                   ,
                                             106                        ,
                                             NULL -- 20120424 Se agrego folio solicitud
                                             );
      END IF;
     
      --TRACE "Actualiza baja a 106 de v_h_id_det_ctr_mandato - "||v_h_id_det_ctr_mandato;
      UPDATE safre_viv:mdt_det_ctr_mandato
         SET estado = 106
       WHERE id_det_ctr_mandato     = v_h_id_det_ctr_mandato;       
      
      LET v_existe = 1;
   
   END FOREACH

   --TRACE "Fin del SP";
   
   RETURN v_existe;
   
END PROCEDURE;


