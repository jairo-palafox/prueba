






CREATE PROCEDURE "safreviv".sp_insert_marca_ws(p_id_derechohabiente decimal(9,0), 
                                    p_id_origen decimal(9,0), 
                                    p_modulo_cod char(3), 
                                    p_tpo_credito smallint, 
                                    p_marca smallint,
                                    p_f_solicita date, 
                                    p_intento smallint,                                     
                                    p_situacion smallint, 
                                    p_num_credito decimal(10,0), 
                                    p_f_infonavit date, 
                                    p_marca_procesar char(2), 
                                    p_folio_archivo decimal(9,0), 
                                    p_usuario char(20))
                                    
   DEFINE p_cod_result_op smallint;
   DEFINE p_diagnostico smallint;
                                    
   LET p_cod_result_op = NULL;
   LET p_diagnostico = NULL;

   -- se inserta el registro en la tabla de de marca ws
   INSERT INTO safre_viv:cta_marca_ws (
               id_derechohabiente,
               id_origen,
               modulo_cod,
               tpo_credito,
               marca,
               f_solicita,
               intento,
               cod_result_op,
               diagnostico,
               situacion,
               num_credito,
               f_infonavit,
               marca_procesar,
               folio_archivo,
               usuario)
       VALUES (p_id_derechohabiente,
               p_id_origen,
               p_modulo_cod,
               p_tpo_credito,
               p_marca,
               p_f_solicita,
               p_intento,
               p_cod_result_op,
               p_diagnostico,
               p_situacion,
               p_num_credito, 
               p_f_infonavit,
               p_marca_procesar,
               p_folio_archivo,
               p_usuario);

END PROCEDURE;


