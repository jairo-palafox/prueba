






CREATE PROCEDURE "safreviv".sp_insert_solicitud_ret_ley73(p_id_derechohabiente DECIMAL(9,0)
                                              ,p_grupo_tr           SMALLINT 
                                              ,p_aivs_viv92         DECIMAL(12,2)
                                              ,p_aivs_viv97         DECIMAL(12,2)
                                              ,p_importeviv92       DECIMAL(12,2)
                                              ,p_importeviv97       DECIMAL(12,2)
                                              ,p_cod_ret            SMALLINT
                                              ,p_estado             SMALLINT 
                                              ,p_cod_rec            smallint 
                                             -- ,p_nombre_NRP         CHAR(40)
                                              )
                                              
DEFINE  v_id_nrp smallint;

LET v_id_nrp  = 666;
INSERT INTO ret_ley73 VALUES(
                              safre_viv:seq_ret_solicitud.NEXTVAL             --id_solicitud      
                             ,p_id_derechohabiente        --id_derechohabiente
                             ,p_grupo_tr                  --tipo_proceso      
                             ,TODAY                       --f_solicitud       
                             ,p_estado                    --estado_solicitud  
                             ,0                           --folio             
                             ,p_aivs_viv92                --aivs_viv92        
                             ,p_aivs_viv97                --aivs_viv97        
                             ,p_importeviv92              --importe_viv92     
                             ,p_importeviv97              --importe_viv97     
                             ,p_cod_ret                   --cod_retorno       
                             ,v_id_nrp                    --nrp               
                             ,TODAY                       --f_valuacion       
                             ,TODAY                       --f_captura         
                             ,CURRENT DAY TO SECOND       --h_captura         
                             ,USER                        --usuario           
                             ,666                         --marca_juridico    
                             ,p_cod_rec                   --cod_rechazo       
                             );

END PROCEDURE ;


