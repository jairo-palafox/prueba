






CREATE FUNCTION "safreviv".fn_dae_actualiza_status_retiro(
                                               p_usuario_cod CHAR(20), 
                                               p_folio       DECIMAL(9,0),
                                               p_proceso_cod SMALLINT,
                                               p_marca       SMALLINT
                                              )
   RETURNING SMALLINT,
             INTEGER,
             VARCHAR(250)

 -- para desmarcar cuentas
 DEFINE v_id_derechohabiente  DECIMAL(9,0);
 DEFINE v_id_referencia       DECIMAL(9,0);
 DEFINE v_f_folio             DATE;
 DEFINE v_ret_ae_proc         INTEGER;
 DEFINE v_rest_ae_proc        INTEGER;
 DEFINE v_rest_rch_ae_proc    INTEGER;
 DEFINE v_folio_ret_rest      CHAR(50);
 DEFINE v_contador            DECIMAL(9,0);
 DEFINE v_ctr_folios_ret_rest CHAR(200);

 
 -- Control de Excepciones
 DEFINE sql_err        INTEGER     ;
 DEFINE isam_err       INTEGER     ;
 DEFINE err_txt        CHAR(200)   ;
 DEFINE v_c_msj        VARCHAR(250);
 DEFINE v_si_resultado SMALLINT    ;

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_actualiza_status_retiro.trace";
   
   -- Retiro de Amoritizaciones Excedentes             1530
   -- Restitución Retiro de Amoritizaciones Excedentes 1536
   -- Restitución Rechazo FICO                         1540
   
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = '1';
   LET v_contador     = 0;

   --Se busca la fecha del folio de retiro para actualizar los movimientos previos a esa fecha
   SELECT f_actualiza,
          proceso_cod
     INTO v_f_folio,
          v_folio_ret_rest
     FROM glo_folio
    WHERE folio = p_folio;

   LET v_folio_ret_rest = p_folio || '-' || TRIM(v_folio_ret_rest) || '|';
   LET v_folio_ret_rest = TRIM(v_folio_ret_rest);
   
   FOREACH 
      SELECT id_derechohabiente,
             id_referencia 
      INTO   v_id_derechohabiente,
             v_id_referencia 
      FROM   cta_movimiento 
      WHERE  folio_liquida = p_folio
      GROUP BY 1,2
      LET v_contador = v_contador + 1;
         -- Valida el proceso para actualizar status
         IF p_proceso_cod = 1530 THEN
            --Si es retiro   
            SELECT FIRST 1 
                   ctr_folios_ret_rest
            INTO   v_ctr_folios_ret_rest
            FROM   dae_det_solicitud
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    status_retiro IN (1,3)
            AND    fecha_liquida <= v_f_folio;
            IF v_ctr_folios_ret_rest IS NULL THEN 
                LET v_ctr_folios_ret_rest = TRIM(v_folio_ret_rest); 
            ELSE 
                LET v_ctr_folios_ret_rest = TRIM(v_ctr_folios_ret_rest) || TRIM(v_folio_ret_rest);
            END IF

            --TRACE("Los folios en proc_cod = 1530 >" || v_ctr_folios_ret_rest || "< ID_DERECHOHABIENTE >" || v_id_derechohabiente || "<");

            UPDATE dae_det_solicitud
            SET    status_retiro      = 4,
                   folio_retiro       = p_folio,
                   id_ret_solicitud   = v_id_referencia,
                   ctr_folios_ret_rest = v_ctr_folios_ret_rest
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    status_retiro IN (1,3)
            AND    fecha_liquida <= v_f_folio;
            LET v_ctr_folios_ret_rest = '';
         ELSE               
            SELECT FIRST 1 
                   ctr_folios_ret_rest
            INTO   v_ctr_folios_ret_rest
            FROM   dae_det_solicitud
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    fecha_liquida <= v_f_folio
            AND    status_retiro = 4;

            --TRACE("El resultado de la consulta para el id_derechohabiente >" || v_id_derechohabiente || " es >" || v_ctr_folios_ret_rest || "< el dato para concatenar es >" || v_folio_ret_rest || "<");
            
            IF v_ctr_folios_ret_rest IS NULL THEN 
                LET v_ctr_folios_ret_rest = TRIM(v_folio_ret_rest); 
            ELSE 
                LET v_ctr_folios_ret_rest = TRIM(v_ctr_folios_ret_rest) || TRIM(v_folio_ret_rest);
            END IF

            --TRACE("Los folios en proc_cod = 1536 >" || v_ctr_folios_ret_rest || "< ID_DERECHOHABIENTE >" || v_id_derechohabiente || "<");

            UPDATE dae_det_solicitud
            SET    status_retiro      = 3,
                   folio_retiro       = p_folio,
                   id_ret_solicitud   = NULL,
                   ctr_folios_ret_rest = v_ctr_folios_ret_rest
            WHERE  id_derechohabiente = v_id_derechohabiente
            AND    fecha_liquida <= v_f_folio
            AND    status_retiro = 4;
            LET v_ctr_folios_ret_rest = '';
         END IF
   END FOREACH
   --TRACE("termina proceso");
   LET v_c_msj = 'Proceso terminado exitosamente';

   -- se devuelve el resultado de la operacion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


