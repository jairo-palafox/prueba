






CREATE PROCEDURE "safreviv".sp_preliquida_sar92(p_folio DECIMAL(9,0), p_usuario CHAR(20))

RETURNING SMALLINT, INTEGER, VARCHAR(255) 

  --pag_cza_sar92
   --DEFINE v_cza_folio                 DECIMAL(9,0);
   --DEFINE v_cza_folio_sua             CHAR(6);
   DEFINE v_cza_f_envio_lote          DATE;

   --pag_det_sar92
   --DEFINE v_det_folio                 DECIMAL(9,0);
   DEFINE v_det_id_decreto            DECIMAL(9,0);
   DEFINE v_det_aivs_ap               DECIMAL(18,6);
   DEFINE v_det_imp_viv               DECIMAL(12,2);
   DEFINE v_det_id_referencia         DECIMAL(9,0);

   --pag_preliquida
   DEFINE v_preliq_f_liquida          DATE;                        --f_liquida            date
   DEFINE v_preliq_id_decreto         DECIMAL(9,0);                --id_decreto           decimal(9,0)
   DEFINE v_preliq_subcuenta          SMALLINT;                    --subcuenta            smallint
   DEFINE v_preliq_fondo_inversion    SMALLINT;                    --fondo_inversion      smallint
   DEFINE v_preliq_movimiento         SMALLINT;                    --movimiento           smallint
   DEFINE v_preliq_folio_liquida      DECIMAL(9,0);                --folio_liquida        decimal(9,0)
   DEFINE v_preliq_id_referencia      DECIMAL(9,0);                --id_referencia        decimal(9,0)
   DEFINE v_preliq_monto_acciones     DECIMAL(18,6);               --monto_acciones       decimal(22,2)
   DEFINE v_preliq_monto_pesos        DECIMAL(12,2);               --monto_pesos          decimal(22,2)
   DEFINE v_preliq_f_valor            DATE;                        --f_valor              date
   DEFINE v_preliq_f_registro         DATE;                        --f_registro           date
   DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;     --h_registro           datetime hour to second

   DEFINE v_error                     SMALLINT;
   DEFINE isam_err                    INTEGER ;
   DEFINE error_info                  CHAR(70);
   
  -- Control de Excepciones
   DEFINE v_si_resultado              SMALLINT;
   DEFINE err_txt                     VARCHAR(255);   

   ON EXCEPTION
      SET v_error, isam_err, err_txt
      --TRACE 'Ocurrio el error:'||v_error;
      --TRACE isam_err;
      --TRACE error_info;
      RETURN v_error, isam_err, err_txt ;
   END EXCEPTION --WITH RESUME
   
   --SET DEBUG FILE TO 'sp_preliquida_sar92.trace';
   ---SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_preliquida_sar92.trace';
   --TRACE 'Inicia sp_preliquida_sar92 con Folio:' || p_folio;
   
   LET v_error = 0;

   LET v_preliq_subcuenta       = 48;
   LET v_preliq_movimiento      = 71;
   LET v_preliq_fondo_inversion = 11;
   --Se recuperan los datos del encabezado para insertar en pag_preliquida
   FOREACH SELECT f_envio_lote
             INTO v_cza_f_envio_lote
             FROM pag_cza_sar92
            WHERE folio = p_folio
            
          --trace " v_cza_f_envio_lote "  || v_cza_f_envio_lote;

      --Constantes para todos los derechohabintes que pertenecen al folio

      LET v_preliq_folio_liquida = p_folio;

      --por cada derechohabiente
      FOREACH SELECT id_referencia,id_decreto,aivs_ap,imp_viv
                INTO v_det_id_referencia,v_det_id_decreto,v_det_aivs_ap,v_det_imp_viv
                FROM pag_det_sar92
               WHERE folio = p_folio
               
               
              --trace " v_det_imp_viv "  || v_det_imp_viv;   

         IF ( v_det_imp_viv > 0 ) THEN
            LET v_preliq_f_liquida      = TODAY;
            LET v_preliq_f_valor        = TODAY;
            LET v_preliq_id_referencia  = v_det_id_referencia;
            LET v_preliq_id_decreto     = v_det_id_decreto;
            LET v_preliq_monto_pesos    = v_det_imp_viv;
            LET v_preliq_monto_acciones = v_det_aivs_ap;
            LET v_preliq_f_registro     = TODAY; --en caso de realizar la preliquidacion con muchos registros, actualiza cada registro
            LET v_preliq_h_registro     = CURRENT HOUR TO SECOND; --actualiza tiempo para cada registro

            INSERT INTO pag_sar92_preliquida
                            ( 
                              f_liquida        , 
                              id_decreto       , 
                              subcuenta        , 
                              fondo_inversion  ,
                              movimiento       , 
                              folio_liquida    , 
                              id_referencia    , 
                              monto_acciones   ,
                              monto_pesos      , 
                              f_valor          , 
                              f_registro       , 
                              h_registro       ,
                              origen                        
                            )
                     VALUES(
                              v_preliq_f_liquida          ,--f_liquida
                              v_preliq_id_decreto         ,--id_decreto
                              v_preliq_subcuenta          ,--subcuenta
                              v_preliq_fondo_inversion    ,--fondo_inversion
                              v_preliq_movimiento         ,--movimiento
                              v_preliq_folio_liquida      ,--folio_liquida
                              v_preliq_id_referencia      ,--id_referencia
                              v_preliq_monto_acciones     ,--monto_acciones
                              v_preliq_monto_pesos        ,--monto_pesos
                              v_preliq_f_valor            ,--f_valor
                              v_preliq_f_registro         ,--f_registro
                              v_preliq_h_registro         , --h_registro
                              "SAR92"
                            );

         END IF
      END FOREACH
   END FOREACH
   -- se actualiza el estatus del folio a preliquidado
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;
       
   UPDATE STATISTICS FOR TABLE pag_sar92_preliquida;  

   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de preliquidación finalizó correctamente.";

   RETURN v_si_resultado, isam_err, err_txt;
 
END PROCEDURE


;


