






CREATE PROCEDURE "safreviv".sp_dac_inserta_rechazo(
                                        p_rch_folio             DECIMAL(9,0),
                                        p_rch_tipo_registro     SMALLINT    ,
                                        p_rch_id_dac_referencia DECIMAL(9,0),
                                        p_rch_resul_operacion   CHAR(2)     ,
                                        p_rch_diagnostico       SMALLINT    ,
                                        p_rch_campo_valor       CHAR(50)    
                                        )

   INSERT INTO dac_rch_archivo
     (
      id_rechazo      ,
      folio           ,
      tipo_registro   ,
      id_dac_solicitud,
      resul_opera     ,
      diagnostico     ,
      campo_valor      
     )
    VALUES
     (
      seq_dac_rch_archivo.NEXTVAL,
      p_rch_folio                ,
      p_rch_tipo_registro        ,
      p_rch_id_dac_referencia    ,
      p_rch_resul_operacion     ,
      p_rch_diagnostico          ,
      p_rch_campo_valor
     );

END PROCEDURE
;


