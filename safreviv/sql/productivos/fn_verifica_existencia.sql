






CREATE FUNCTION "safreviv".fn_verifica_existencia(p_id_derechohabiente decimal(9,0)
                                       ,p_folio              decimal(9,0)
                                       ,p_nrp                char(11)    
                                       ,p_periodo_pago       char(6)     
                                       ,p_folio_sua          decimal(6,0)
                                       ,p_f_pago_patron      date        
                                       ,p_monto_pesos        DECIMAL (12,2)
                                       ,p_tpo_archivo        smallint
                                       ,p_origen_pago        smallint
                                       ,p_tpo_afiliacion     smallint
                                       ,p_tpo_aclaracion     smallint
                                       ,p_imp_ap_pat         DECIMAL (12,2))
   RETURNING SMALLINT;
   
   DEFINE v_fn_existe_pgo_cta SMALLINT;
   DEFINE v_fn_existe_pgo_his SMALLINT;
   
   --TRACE "fn_verifica_existencia: Paso 1";
   LET v_fn_existe_pgo_cta = fn_existe_pgo_cta(p_id_derechohabiente
                                              ,p_nrp               
                                              ,p_periodo_pago      
                                              ,p_folio_sua         
                                              ,p_f_pago_patron     
                                              ,p_monto_pesos);
   --Se valida el valor obtenido
   IF(v_fn_existe_pgo_cta = 1) THEN
      --TRACE "fn_verifica_existencia: Paso 2";
      EXECUTE PROCEDURE sp_actualiza_origen(p_id_derechohabiente
                                           ,p_tpo_archivo       
                                           ,10  --p_origen_pago 
                                           ,p_tpo_afiliacion    
                                           ,p_tpo_aclaracion    
                                           ,TODAY);
      RETURN 0;
   ELSE
      --TRACE "fn_verifica_existencia: Paso 3";
      LET v_fn_existe_pgo_his = fn_existe_pgo_his(p_id_derechohabiente
                                                  ,p_folio
                                                  ,p_nrp
                                                  ,p_periodo_pago
                                                  ,p_folio_sua
                                                  ,p_f_pago_patron
                                                   ,p_imp_ap_pat);
      IF (v_fn_existe_pgo_his = 1) THEN
         --TRACE "fn_verifica_existencia: Paso 4";
         EXECUTE PROCEDURE sp_actualiza_origen(p_id_derechohabiente
                                              ,p_tpo_archivo       
                                              ,9 --p_origen_pago   
                                              ,p_tpo_afiliacion    
                                              ,p_tpo_aclaracion    
                                              ,TODAY);
         --Ejecutar la preliquidación
         RETURN 1;
      ELSE  
         --TRACE "fn_verifica_existencia: Paso 5";
         EXECUTE PROCEDURE sp_actualiza_origen(p_id_derechohabiente
                                              ,p_tpo_archivo       
                                              ,13 --p_origen_pago  
                                              ,p_tpo_afiliacion    
                                              ,p_tpo_aclaracion    
                                              ,TODAY);
      END IF;
   END IF;
   
   RETURN 0;
   
END FUNCTION;


