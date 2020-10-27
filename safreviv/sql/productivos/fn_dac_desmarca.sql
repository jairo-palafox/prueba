






CREATE FUNCTION "safreviv".fn_dac_desmarca(
                                p_usuario_cod CHAR(20), 
                                p_folio       DECIMAL(9,0),
                                p_proceso_cod SMALLINT
                                )

RETURNING SMALLINT, INTEGER, CHAR(200)

-- Tipos status de pagos
DEFINE v_marca_imsss        SMALLINT;
DEFINE v_si_resultado       SMALLINT;
DEFINE v_id_dac_solicitud   DECIMAL(9,0); 
DEFINE v_id_derechohabiente DECIMAL(9,0); 
DEFINE v_folio_integracion  DECIMAL(9,0); 
DEFINE v_nss                CHAR(11);
DEFINE v_folio_sua          DECIMAL(6,0);

-- Control de Excepciones
DEFINE sql_err  INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  CHAR(200);

ON EXCEPTION SET sql_err, isam_err, err_txt
   LET v_si_resultado = sql_err;

   RETURN v_si_resultado, isam_err, err_txt;
END EXCEPTION
 
--SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_fn_dac_desmarca.trace";
--TRACE ON;

LET v_si_resultado = 0;
LET isam_err = 0;
LET v_c_msj = '1';
LET v_id_dac_solicitud   = 0;
LET v_id_derechohabiente = 0;
LET v_folio_integracion  = 0;
LET v_nss                = "";
LET v_folio_sua          = 0;

   FOREACH      
      SELECT id_dac_solicitud,
             id_derechohabiente,
             folio_integracion,
             nss,
             folio_sua
      INTO   v_id_dac_solicitud,
             v_id_derechohabiente,
             v_folio_integracion,
             v_nss,
             v_folio_sua
      FROM   dac_det_solicitud 
      WHERE  folio_integracion = p_folio
      AND    resul_opera = "01"
      AND    diagnostico = 4
         
      LET v_marca_imsss = 450;
   
      EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente,
                                           v_marca_imsss,
                                           v_folio_sua,
                                           0,
                                           0,
                                           p_usuario_cod,
                                           p_proceso_cod)
     INTO v_si_resultado;
     LET v_c_msj = 'Solicitud desmarcada';       
   END FOREACH;

   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION 
;


