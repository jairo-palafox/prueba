






CREATE FUNCTION "safreviv".fn_dae_desmarca_cuentas(p_usuario_cod CHAR(20), 
                                        p_folio_liquida DECIMAL(9,0),
                                        p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, 
             INTEGER, 
             CHAR(200)

DEFINE v_acep_id_dae_ref_aceptados DECIMAL(9,0) ;
DEFINE v_acep_id_dae_ref_ajuste    DECIMAL(9,0) ;
DEFINE v_acep_id_dae_referencia    DECIMAL(9,0) ;
DEFINE v_acep_folio_liquida        DECIMAL(9,0) ;
DEFINE v_acep_fecha_liquida        DATE         ;
DEFINE v_acep_monto_pesos          DECIMAL(16,6);
DEFINE v_acep_monto_acciones       DECIMAL(16,6);
DEFINE v_acep_fecha_valor          DATE         ; 
DEFINE v_id_derechohabiente        DECIMAL(9,0) ;
DEFINE v_marca_ajuste_amortizacion SMALLINT;
DEFINE v_si_resultado              SMALLINT;

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  CHAR(200);

    ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
    
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION
 
 --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_dae_desmarca.trace";
 --TRACE ON;
 
 LET v_si_resultado = 0;
 LET isam_err = 0;
 LET v_c_msj = '1';
 
 --trace("Verifica pagos devoluciones completas");
 
 LET v_c_msj = 'Previo a foreach para buscar datos con folio'||p_folio_liquida;

 FOREACH
   SELECT a.id_dae_ref_aceptados, 
          a.id_dae_ref_ajuste   ,
          a.id_dae_referencia   ,
          a.folio_liquida       ,
          a.fecha_liquida       ,
          a.monto_pesos         ,
          a.monto_acciones      ,
          a.fecha_valor         ,
          b.id_derechohabiente 
   INTO   v_acep_id_dae_ref_aceptados , 
          v_acep_id_dae_ref_ajuste    , 
          v_acep_id_dae_referencia    , 
          v_acep_folio_liquida        , 
          v_acep_fecha_liquida        , 
          v_acep_monto_pesos          , 
          v_acep_monto_acciones       , 
          v_acep_fecha_valor          ,
          v_id_derechohabiente
   FROM   dae_aceptados_ajuste a,
          dae_det_ajuste b
   WHERE  a.id_dae_ref_ajuste = b.id_dae_ref_ajuste
   

   LET v_c_msj = 'Dentro de proceso foreach';

   LET v_c_msj = 'Se verifica si se pago completamente la solicitud';

   LET v_c_msj = 'Se pago completamente, se procede a desmarcar';
   
   -- # [indica que ya se cubrió el importe total a devolver]
   -- # [ y se requiere quitar la marca de la cuenta        ]
   LET v_marca_ajuste_amortizacion = 403;

   EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente       ,
                                        v_marca_ajuste_amortizacion,
                                        v_acep_id_dae_ref_ajuste   ,
                                        0                          ,
                                        0                          ,
                                        p_usuario_cod              ,
                                        p_proceso_cod)
      INTO v_si_resultado;

   LET v_c_msj = 'Solicitud desmarcada';
       
 END FOREACH;

 LET v_c_msj = 'proceso terminado exitosamente';
 
 RETURN v_si_resultado, 
        isam_err, 
        v_c_msj;
END FUNCTION
;


