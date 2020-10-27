






CREATE PROCEDURE "safreviv".sp_insert_solicitud_ret_det_fondo72(p_sq_ret_solicitud     decimal(9,0)                
                                                    ,p_id_solicitud         decimal(9,0)
                                                    ,p_saldo                decimal(12,2)               
                                                    ,p_tanto_adicional      decimal(12,2)               
                                                    ,p_id_datamart          decimal(9,0)                
                                                    ,p_f_saldo              date                        
                                                    ,p_h_saldo              datetime hour to second     
                                                    ,p_estatus              smallint                    
                                                    ,p_rechazo              smallint                    
                                                    )


  INSERT INTO ret_det_fondo72 VALUES( p_sq_ret_solicitud   --id_afi_fondo72 
                                     ,p_id_solicitud       --id_solicitud
                                     ,p_saldo              --saldo_viv72
                                     ,p_tanto_adicional    --tanto_adicional
                                     ,p_id_datamart        --id_datamart
                                     ,p_f_saldo            --f_saldo
                                     ,p_h_saldo            --h_saldo
                                     ,p_estatus            --estado_detalle
                                     ,p_rechazo            --cod_rechazo
                                     );                                                                                                    
                                                                                                                                           
END PROCEDURE;


