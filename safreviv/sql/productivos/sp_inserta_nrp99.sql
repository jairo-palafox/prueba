






CREATE PROCEDURE "safreviv".sp_inserta_nrp99(p_id_derechohabiente decimal(9,0)
                                 ,p_nrp                char(11)
                                 ,p_f_actualiza        date
                                 ,p_ind_actividad      smallint
                                 ,p_ind_origen         smallint
                                  )

   DEFINE v_ok CHAR(2);
   --Se verifica si existe el registro para el id_dechohabiente y el origen
   SELECT FIRST 1 'OK'
   INTO v_ok
   FROM  pag_nrp
   WHERE id_derechohabiente = p_id_derechohabiente
   AND   ind_origen = p_ind_origen;

   IF (v_ok = 'OK') THEN
      --Si encontro el registro se actualiza
      UPDATE pag_nrp
      SET nrp           = p_nrp
         ,f_actualiza   = p_f_actualiza
         ,ind_actividad = p_ind_actividad
      WHERE id_derechohabiente = p_id_derechohabiente
      AND   ind_origen = p_ind_origen ;
   ELSE
      --Si no encuentra el registro lo inserta
      INSERT INTO pag_nrp 
      (id_derechohabiente
      ,nrp      
      ,f_actualiza       
      ,ind_actividad     
      ,ind_origen
      )
      VALUES
      (p_id_derechohabiente
      ,p_nrp      
      ,p_f_actualiza       
      ,p_ind_actividad     
      ,p_ind_origen        
      );
   END IF

END PROCEDURE;


