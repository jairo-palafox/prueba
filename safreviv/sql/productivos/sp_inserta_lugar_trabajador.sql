






CREATE PROCEDURE "safreviv".sp_inserta_lugar_trabajador(p_id_derechohabiente decimal(9,0)
                                            ,p_localizacion       smallint
                                            ,p_f_actualiza        date
                                            ,p_ind_actividad      smallint
                                            ,p_ind_origen         smallint
                                             )
   DEFINE v_ok CHAR(2);
   --Se verifica si existe el registro para el id_dechohabiente y el origen
   SELECT FIRST 1 'OK'
   INTO v_ok
   FROM  pag_lugar_trabajador
   WHERE id_derechohabiente = p_id_derechohabiente
   AND   ind_origen = p_ind_origen;
   
   IF (v_ok = 'OK') THEN
      --Si encontro el registro se actualiza
      UPDATE pag_lugar_trabajador
      SET    localizacion  = p_localizacion
            ,f_actualiza   = p_f_actualiza
            ,ind_actividad = p_ind_actividad
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    ind_origen = p_ind_origen ;
   ELSE
      --Si no encuentra el registro lo inserta
      INSERT INTO pag_lugar_trabajador 
      (id_derechohabiente
      ,localizacion      
      ,f_actualiza       
      ,ind_actividad     
      ,ind_origen         
      )
      VALUES
      (p_id_derechohabiente
      ,p_localizacion      
      ,p_f_actualiza       
      ,p_ind_actividad     
      ,p_ind_origen         
      );
   END IF
   
END PROCEDURE;


