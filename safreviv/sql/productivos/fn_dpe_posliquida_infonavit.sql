






CREATE FUNCTION "safreviv".fn_dpe_posliquida_infonavit(p_usuario_cod CHAR(20), 
                                            p_folio_liquida DECIMAL(9,0),
                                            p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, INTEGER, CHAR(200)

DEFINE v_id_dpe_referencia  DECIMAL(9,0);
DEFINE v_folio              DECIMAL(9,0);
DEFINE v_nrp                CHAR(11);
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_periodo_pago       CHAR(6);
DEFINE v_estado_solicitud   SMALLINT;
DEFINE v_aportacion_sol     DECIMAL(9,2);
DEFINE v_amortizacion_sol   DECIMAL(9,2);
-- 
DEFINE v_si_marca_infonavit  SMALLINT;
DEFINE v_si_resultado        SMALLINT;

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  CHAR(200);

    ON EXCEPTION SET sql_err, isam_err, err_txt
      --LET v_si_resultado = -206;
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION
 
 --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.dpe.posliquida_INFONAVIT.txt";
 
 LET v_si_resultado = 0;
 LET isam_err = 0;
 LET v_c_msj = "1";

 --trace "Previo a foreach para buscar datos con folio: "||p_folio_liquida;
 
 LET v_c_msj = "Previo a foreach para buscar datos con folio: "||p_folio_liquida;

 FOREACH
    SELECT id_dpe_referencia,
           folio,
           nrp,
           id_derechohabiente,
           periodo_pago,
           estado_solicitud,
           aportacion_sol,
           amortizacion_sol
      INTO v_id_dpe_referencia,
           v_folio,
           v_nrp,
           v_id_derechohabiente,
           v_periodo_pago,
           v_estado_solicitud,
           v_aportacion_sol,
           v_amortizacion_sol
    FROM dpe_sol_soloinfonavit
   WHERE estado_solicitud = 3  -- solo solicitudes aceptadas y liquidadas
     AND folio = p_folio_liquida

   LET v_c_msj = "Dentro de proceso foreach";
   LET v_c_msj = "Se verifica si se pago completamente la solicitud";
   IF v_aportacion_sol > 0 OR v_amortizacion_sol > 0 THEN
      -- Asegurar que hay importes verficando que sean mayor a cero
      --trace("Verifica v:se cumple la regla y desmarca");
      LET v_c_msj = "Se pago completamente, se procede a desmarcar";
      -- # [indica que ya se cubrió el importe total a devolver]
      -- # [ y se requiere quitar la marca de la cuenta        ]
      LET v_si_marca_infonavit = 402;
      --trace("Verifica v:desmarcar");
      EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                          v_si_marca_infonavit, -- marca de infonavit
                                          v_id_dpe_referencia,
                                          0,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
         INTO v_si_resultado;
      --trace "Verifica v:resultado desmarca.."||v_si_resultado;
      LET v_c_msj = "Solicitud desmarcada";
      
      -- Actualiza el estado de la solicitud a 4 liquidado
      UPDATE dpe_sol_soloinfonavit
         SET estado_solicitud = 4 -- liquidados
       WHERE estado_solicitud = 3 -- Aceptadas y liquidadas
         AND folio = p_folio_liquida;
      
   ELSE
      --trace "Verifica v: no es mayor";
      LET v_c_msj = "Solicitud no desmarcada, saldo no cubierto completamente";
   END IF
   
 END FOREACH;
 
 --trace("termina proceso");
 LET v_c_msj = "proceso terminado exitosamente";
 
 RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION 
;


