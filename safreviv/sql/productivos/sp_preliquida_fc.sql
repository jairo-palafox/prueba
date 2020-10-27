






CREATE PROCEDURE "safreviv".sp_preliquida_fc(p_folio DECIMAL(9,0), p_usuario CHAR(20))
RETURNING SMALLINT, SMALLINT, VARCHAR(255)

   --pag_det_fc
   DEFINE v_pag_det_fc_folio                 DECIMAL(9,0)     ;
   DEFINE v_pag_det_fc_origen_archivo        SMALLINT         ;
   DEFINE v_pag_det_fc_id_referencia         DECIMAL(9,0)     ;
   DEFINE v_pag_det_fc_f_pago                DATE             ;
   DEFINE v_pag_det_fc_id_derechohabiente    DECIMAL(9,0)     ;
   DEFINE v_pag_det_fc_imp_ap_fc             DECIMAL(12,2)    ;
   DEFINE v_pag_det_fc_result_operacion      CHAR(2)          ;


   
   
   DEFINE v_cza_folio                 DECIMAL(9,0);
   DEFINE v_cza_nrp                   CHAR(11);
   DEFINE v_cza_folio_sua             CHAR(6);
   DEFINE v_cza_f_valor_4seguros      DATE;
   --pag_det_trab_sifv
   DEFINE v_folio                     DECIMAL(9,0) ;    --folio
   DEFINE v_id_referencia             DECIMAL(9,0) ;    --id_referencia
   DEFINE v_id_derechohabiente        DECIMAL(9,0) ;    --id_derechohabiente
   DEFINE v_nrp                       CHAR(11)     ;    --nrp
   DEFINE v_periodo_pago              CHAR(6)      ;    --periodo_pago
   DEFINE v_folio_sua                 DECIMAL(6,0) ;    --folio_sua
   DEFINE v_sdi                       DECIMAL(10,2);    --sdi
   DEFINE v_imp_ap_pat                DECIMAL(12,2);    --imp_ap_pat
   DEFINE v_cve_concepto              SMALLINT     ;    --cve_concepto
   DEFINE v_f_pago                    DATE         ;    --f_pago
   DEFINE v_f_proceso                 DATE         ;    --f_proceso
   DEFINE v_num_crd_trab              DECIMAL(10,0);    --num_crd_trab
   DEFINE v_cve_ent_recauda           CHAR(3)      ;    --cve_ent_recauda
   DEFINE v_imp_am_crd                DECIMAL(12,2);    --imp_am_crd
   --pag_preliquida
   DEFINE v_preliq_f_liquida          DATE;                     --f_liquida
   DEFINE v_preliq_id_derechohabiente DECIMAL(9,0);             --id_derechohabiente
   DEFINE v_preliq_subcuenta          SMALLINT;                 --subcuenta
   DEFINE v_preliq_fondo_inversion    SMALLINT;                 --fondo_inversion
   DEFINE v_preliq_movimiento         SMALLINT;                 --movimiento
   DEFINE v_preliq_folio_liquida      DECIMAL(9,0);             --folio_liquida
   DEFINE v_preliq_id_referencia      DECIMAL(9,0);             --id_referencia
   DEFINE v_preliq_monto_acciones     DECIMAL(18,2);            --monto_acciones
   DEFINE v_preliq_f_valor            DATE;                     --f_valor
   DEFINE v_preliq_f_registro         DATE;                     --f_registro
   DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;  --h_registro
   DEFINE v_preliq_usuario            CHAR(20);                 --usuario
   DEFINE v_consecutivo               INTEGER;
   DEFINE v_bandera_nrp               SMALLINT;
   DEFINE v_siafore                   SMALLINT;

  -- Control de Excepciones
  DEFINE sql_err                SMALLINT;
  DEFINE isam_err               SMALLINT;
  DEFINE err_txt                VARCHAR(255);
  DEFINE v_si_resultado         SMALLINT;


  --manejo de excepciones
  ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
  END EXCEPTION


   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_preliquida_fc.trace';
   --TRACE 'Inicia sp_preliquida_fc con Folio:' || p_folio;
   --asignación de variables 
   LET v_consecutivo = 0;
   LET v_bandera_nrp = 0;
   LET v_preliq_f_liquida = TODAY  ;
   
   LET v_preliq_subcuenta = 49       ;
   LET v_preliq_fondo_inversion = 10  ;
   LET v_preliq_movimiento = 211     ;
   LET v_preliq_h_registro  = CURRENT HOUR TO SECOND  ;
   LET v_siafore = 10 ;    
   LET v_preliq_monto_acciones = 0;
   
   --TRACE 'v_preliq_fondo_inversion:' || v_preliq_fondo_inversion;
                        
   --se invoca a función que calcual el monto en acciones 
   --Inicia con la lectura de todos los registros de la tabla pag_det_trab_sifv
   --por cada derechohabiente
   --trace  (" acciones =  "||v_preliq_monto_acciones ) ;
   --TRACE 'Inicia FOREACH :' ;
   FOREACH SELECT folio                  ,
                  origen_archivo         ,
                  id_referencia          ,
                  f_pago                 ,
                  id_derechohabiente     ,
                  imp_ap_fc              ,
                  result_operacion       
             INTO v_pag_det_fc_folio               ,
                  v_pag_det_fc_origen_archivo      ,
                  v_pag_det_fc_id_referencia       ,
                  v_pag_det_fc_f_pago              ,
                  v_pag_det_fc_id_derechohabiente  ,
                  v_pag_det_fc_imp_ap_fc           ,
                  v_pag_det_fc_result_operacion    
            FROM pag_det_fc
           WHERE folio = p_folio
             AND result_operacion = "01" 

         
         LET v_preliq_monto_acciones = fn_consulta_precio_fondo (v_pag_det_fc_imp_ap_fc ,
                                                                 v_preliq_f_liquida  ,
                                                                 v_siafore ) ;
         --Se valida si no encontro el fondo se asiga 0  
         IF(v_preliq_monto_acciones IS NULL)  THEN 
            LET v_preliq_monto_acciones	= 0 ;
         END IF 

         INSERT INTO pag_preliquida
                        ( 
                          f_liquida              ,
                          id_derechohabiente     ,
                          subcuenta              ,
                          fondo_inversion        ,
                          movimiento             ,
                          folio_liquida          ,
                          id_referencia          ,
                          monto_acciones         ,
                          monto_pesos            ,
                          f_valor                ,
                          f_registro             ,
                          h_registro             ,
                          origen                  
                        )
                VALUES  (
                          v_preliq_f_liquida                ,
                          v_pag_det_fc_id_derechohabiente   ,
                          v_preliq_subcuenta                ,
                          v_preliq_fondo_inversion          ,
                          v_preliq_movimiento               ,
                          p_folio                           ,
                          v_pag_det_fc_id_referencia        ,
                          v_preliq_monto_acciones           ,
                          v_pag_det_fc_imp_ap_fc            ,
                          v_pag_det_fc_f_pago               ,
                          TODAY                             ,
                          v_preliq_h_registro               ,
                          "FORTALECIMIENTO"
                        );
   END FOREACH

   -- el folio se actualiza a estatus de preliquidado
   UPDATE glo_folio
     SET status = 1
   WHERE folio = p_folio;

   {
   UPDATE safre_viv:glo_ctr_archivo
      SET estado = 3
    WHERE folio = p_folio;
   }
   -- si no hubo error, se indica que el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de preliquidación de FORTALECIMIENTO DE CRÉDITO terminó correctamente";

   RETURN v_si_resultado, isam_err, err_txt;

END PROCEDURE;


