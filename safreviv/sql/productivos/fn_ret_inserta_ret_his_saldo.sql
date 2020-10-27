






CREATE PROCEDURE "safreviv".fn_ret_inserta_ret_his_saldo(p_id_solicitud    decimal(9,0)            ,
                                              p_subcuenta       smallint                ,
                                              p_fondo_inversion smallint                ,
                                              p_saldo_acciones  decimal(16,6)           ,
                                              p_saldo_pesos     decimal(12,2)           ,
                                              p_folio           decimal(9,0)            ,
                                              p_f_registro      date                    ,
                                              p_h_registro      datetime hour to second )

   -- se inserta el registro
   INSERT INTO ret_his_saldo (
      id_solicitud   ,
      subcuenta      ,
      fondo_inversion,
      saldo_acciones ,
      saldo_pesos    ,
      folio          ,
      f_registro     ,
      h_registro     
   )
   VALUES (
      p_id_solicitud   ,
      p_subcuenta      ,
      p_fondo_inversion,
      p_saldo_acciones ,
      p_saldo_pesos    ,
      p_folio          ,
      p_f_registro     ,
      p_h_registro     
   );
   
END PROCEDURE;


