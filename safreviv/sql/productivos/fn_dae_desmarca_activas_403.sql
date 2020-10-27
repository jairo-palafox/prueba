






CREATE FUNCTION "safreviv".fn_dae_desmarca_activas_403()
   RETURNING SMALLINT, 
             INTEGER, 
             CHAR(200),
             INTEGER

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

DEFINE v_usuario_cod   CHAR(20)       ;
DEFINE v_folio_liquida DECIMAL(9,0) ;
DEFINE v_proceso_cod   SMALLINT       ;

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  CHAR(200);
DEFINE v_tot_desmarca INTEGER;

    ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
    
      RETURN v_si_resultado, isam_err, err_txt, v_tot_desmarca;
   END EXCEPTION
 
 --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_dae_desmarca.trace";
 --TRACE ON;
 
 LET v_si_resultado = 0;
 LET isam_err = 0;
 LET v_tot_desmarca = 0;
 LET v_usuario_cod = "safreviv";
 LET v_proceso_cod = 2403;

 FOREACH
   SELECT id_derechohabiente, 
          n_referencia
   INTO   v_id_derechohabiente,
          v_acep_id_dae_ref_ajuste
   FROM   sfr_marca_activa 
   WHERE  marca = 403

   EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente,
                                        403,
                                        v_acep_id_dae_ref_ajuste,
                                        0,
                                        0,
                                        v_usuario_cod,
                                        v_proceso_cod)
   INTO v_si_resultado;

   LET v_tot_desmarca = v_tot_desmarca + 1 ;
       
 END FOREACH;

 LET v_c_msj = 'proceso terminado exitosamente';
 
 RETURN v_si_resultado, 
        isam_err, 
        v_c_msj,
        v_tot_desmarca;
END FUNCTION
;


