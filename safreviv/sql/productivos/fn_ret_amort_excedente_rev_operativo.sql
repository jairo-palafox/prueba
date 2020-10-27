






CREATE FUNCTION "safreviv".fn_ret_amort_excedente_rev_operativo(p_folio DECIMAL(9,0),
                                                     p_proceso_cod SMALLINT, 
                                                     p_usuario_cod CHAR(20)
                                  ) 
   RETURNING INTEGER, VARCHAR(100)

-- Control de Excepciones
DEFINE v_si_resultado       SMALLINT;
DEFINE v_i_estado_marca     SMALLINT;
DEFINE sql_err              INTEGER;
DEFINE isam_err             INTEGER;
DEFINE err_txt              VARCHAR(250);
DEFINE v_c_msj              VARCHAR(250);
DEFINE v_edo_reversado      SMALLINT;
DEFINE v_edo_autorizada     SMALLINT;
DEFINE v_edo_rch_marca      SMALLINT;
DEFINE v_marca_amort_exced  SMALLINT;
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_id_solicitud       DECIMAL(9,0);
DEFINE v_marca              SMALLINT;
DEFINE v_s_qry              CHAR(500);


    -- se configura el retorno de los valores
    ON EXCEPTION SET sql_err, isam_err, err_txt 
        LET v_si_resultado = sql_err;

        RETURN v_si_resultado, err_txt;
    END EXCEPTION

    -- el reverso operativo no hace nada
    LET v_si_resultado = 0;
    LET v_c_msj        = "Reverso operativo finalizado exitosamente.";

    -- se asigna el estado de reversado por proceso operativo a las solicitudes
    LET v_edo_reversado      = 200;
    LET v_edo_autorizada     = 15;  -- Solicitudes listas para volverse a procesar por reverso operativo
    LET v_edo_rch_marca      = 100; -- estado para identificar las cuentas que no se pudieron marcar
    LET v_id_solicitud       = 0;
    LET v_id_derechohabiente = 0;
    LET v_marca_amort_exced  = 810;
    LET v_i_estado_marca     = 0;
    LET v_marca              = 0;

    -- se cambian las solicitudes a verificadas para reprocesar por restitución
    UPDATE ret_solicitud_generico
    SET    estado_solicitud = v_edo_autorizada,
           cod_rechazo      = 0
    WHERE  folio        = p_folio
    AND    estado_solicitud in (70);

    UPDATE ret_amort_excedente
    SET    estado_solicitud = v_edo_autorizada,
           cod_rechazo      = 0
    WHERE  folio        = p_folio
    AND    estado_solicitud in (70);

    CALL fn_dae_actualiza_status_retiro(p_usuario_cod, p_folio, p_proceso_cod, v_marca) RETURNING v_si_resultado, isam_err, v_c_msj;
    
    UPDATE ret_solicitud_generico
    SET    estado_solicitud = v_edo_reversado,
           cod_rechazo = 0
    WHERE  folio            = p_folio
    AND    estado_solicitud NOT IN (15, 100);
   
    UPDATE ret_amort_excedente
    SET    estado_solicitud = v_edo_reversado,
           cod_rechazo = 0
    WHERE  folio            = p_folio
    AND    estado_solicitud NOT IN (15, 100);

    UPDATE STATISTICS  FOR TABLE ret_solicitud_generico;
    UPDATE STATISTICS  FOR TABLE ret_amort_excedente;
    
    -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, v_c_msj;
END FUNCTION;


