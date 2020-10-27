






CREATE PROCEDURE "safreviv".sp_finaliza_rest_fondo_ahorro(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_proceso_cod SMALLINT)

    -- para desmarcar cuentas
    DEFINE ret_aclara_fa_id_solicitud             DECIMAL(9,0) ;
    DEFINE ret_aclara_fa_id_derechohabiente       DECIMAL(9,0) ;
    DEFINE ret_aclara_fa_tpo_movto                CHAR(2)      ;
 

    DEFINE v_si_resultado                         SMALLINT     ;

    -- para marcar las cuentas
    DEFINE v_i_estado_marca                       INTEGER      ;
    DEFINE v_marca                                INTEGER      ; -- Puede tomar los valores 811, 812, 813 dependiendo del tipo de movimiento
                                                                 -- 811 - CI, 812 - UC y 813 - FA


    -- Control de Excepciones
    DEFINE sql_err                                INTEGER      ;
    DEFINE isam_err                               INTEGER      ;
    DEFINE err_txt                                CHAR(200)    ;
    DEFINE v_c_msj                                VARCHAR(250) ;

--    ON EXCEPTION SET sql_err, isam_err, err_txt
--        LET v_si_resultado = sql_err;
--
--        RETURN v_si_resultado, isam_err, err_txt;
--    END EXCEPTION

    --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_finaliza_aclara_fondo_ahorro.txt";

    LET v_si_resultado = 0;
    LET isam_err       = 0;
    LET v_c_msj        = '1';

    --TRACE("Desmarcando cuentas liquidadas");

    LET v_marca             = 0; 
    LET v_i_estado_marca    = 0;

    -- se actualizan las solicitudes a estado 60 liquidada para el folio dado
    UPDATE ret_rest_fondo_ahorro
    SET    estado_solicitud = 60 -- liquidada
    WHERE  estado_solicitud = 50 -- preliquidada
    AND    folio            = p_folio;

    --TRACE("termina proceso");
    LET v_c_msj = 'proceso terminado exitosamente';

--    RETURN v_si_resultado, isam_err, v_c_msj;
END PROCEDURE;


