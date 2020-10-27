






CREATE PROCEDURE "safreviv".sp_mdt_inserta_lote_recurrente(
                p_origen              smallint ,
                p_f_lote              date     ,
                p_lote                integer  ,
                p_f_carga_acr         date     );
               
   DEFINE v_id_lote_mandato       dec(9,0);
   DEFINE v_f_proceso             date    ;
   DEFINE v_estado                smallint;


   INSERT INTO mdt_lote_mandato VALUES 
   (seq_mdt_lote_mandato.NEXTVAL      ,
    p_origen                          ,
    p_f_lote                          ,
    p_lote                            ,
    p_f_carga_acr                     ,
    today                             ,
    101                                );
    

END PROCEDURE;


