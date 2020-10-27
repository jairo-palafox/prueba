






CREATE PROCEDURE "safreviv".sp_inserta_tpo_aclaracion(p_id_derechohabiente decimal(9,0)
                                          ,p_tpo_archivo        smallint
                                          ,p_origen_pago        smallint
                                          ,p_tpo_afiliacion     smallint
                                          ,p_tpo_aclaracion     smallint
                                          ,p_f_actualiza        date
                                          ,p_origen_pago_aux    smallint)
   DEFINE v_ok CHAR(2);                                    
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_inserta_tpo_aclaracion.trace';
   
   
   --Se verifica si existe el registro para el id_dechohabiente
   SELECT FIRST 1 'OK'
   INTO v_ok
   FROM  pag_ctr_pago
   WHERE id_derechohabiente = p_id_derechohabiente;

   --trace  ("v_ok"||v_ok); 

   IF (v_ok = 'OK') THEN
      --Si existe el registro se actualiza
      UPDATE pag_ctr_pago
      SET tpo_archivo     = p_tpo_archivo
         ,origen_pago     = p_origen_pago
         ,tpo_afiliacion  = p_tpo_afiliacion
         ,tpo_aclaracion  = p_tpo_aclaracion
         ,f_actualiza     = p_f_actualiza
      WHERE id_derechohabiente = p_id_derechohabiente;
   ELSE
      --Si no existe se inserta el registro
      INSERT INTO pag_ctr_pago
                   ( 
                     id_derechohabiente ,
                     tpo_archivo        ,
                     origen_pago        ,
                     tpo_afiliacion     ,
                     tpo_aclaracion     ,
                     f_actualiza
                    )
              VALUES
                    (
                      p_id_derechohabiente  ,
                      p_tpo_archivo         ,
                      p_origen_pago         ,
                      p_tpo_afiliacion      ,
                      p_tpo_aclaracion      ,
                      p_f_actualiza
                    );
   END IF
   --Se inserta el historico de la aclaración
   INSERT INTO pag_his_ctr_pago
                 (
                   id_derechohabiente  ,
                   tpo_archivo         ,
                   origen_pago         ,
                   tpo_afiliacion      ,
                   tpo_aclaracion      ,
                   f_actualiza
                 )
           VALUES
                 (
                   p_id_derechohabiente  ,
                   p_tpo_archivo         ,
                   p_origen_pago_aux     ,
                   p_tpo_afiliacion      ,
                   p_tpo_aclaracion      ,
                   p_f_actualiza
                 );
                           
           --trace  ("final del store");         
                           
END PROCEDURE;


