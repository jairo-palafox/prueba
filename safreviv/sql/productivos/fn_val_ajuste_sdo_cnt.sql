






CREATE PROCEDURE "safreviv".fn_val_ajuste_sdo_cnt(p_folio       DECIMAL(9,0),  --Folio de liquidación del proceso                                                                    
                                       p_cod_proceso SMALLINT)      --Código Proceso                                  
RETURNING SMALLINT, SMALLINT, CHAR(70);

  --Última modificación 13102015
  --Declaración de variables  
  DEFINE v_estado              SMALLINT;
  DEFINE v_char                CHAR(20);
  
  DEFINE v_status              SMALLINT;
  DEFINE sql_err               INTEGER ;
  DEFINE isam_err              INTEGER ;
  DEFINE error_info            CHAR(70);  
  
  ON EXCEPTION
     SET sql_err, isam_err, error_info
     LET v_status = sql_err;
     RETURN  v_status ,isam_err , error_info;
  END EXCEPTION
  
  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_val_ajuste_sdo_cnt.trace';
  
  SELECT estado
    INTO v_estado
    FROM cnt_transaccion
   WHERE cod_proceso = p_cod_proceso
     AND tpo_transaccion = 0
     AND folio_liquida = p_folio
  GROUP BY estado;

  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_estado = 0;
  END IF  

--   TRACE 'Finaliza fn_val_ajuste_sdo_cnt con valor '||v_bnd_proceso;
   
   LET v_char = "Terminado fn_val_ajuste_sdo_cnt exitosamente";
   RETURN v_estado , 0 , v_char;

END PROCEDURE;


