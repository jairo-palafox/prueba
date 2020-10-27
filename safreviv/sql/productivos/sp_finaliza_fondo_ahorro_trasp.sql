






CREATE PROCEDURE "safreviv".sp_finaliza_fondo_ahorro_trasp(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_proceso_cod SMALLINT)

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
    UPDATE ret_fondo_ahorro_trasp
    SET    estado_solicitud = 60 -- liquidada
    WHERE  estado_solicitud = 50 -- preliquidada
    AND    folio            = p_folio;

      
    -- se desmarcan todas las cuentas que fueron liquidadas
{    FOREACH
        SELECT a.id_solicitud, b.id_derechohabiente, c.tpo_movto
        INTO   ret_aclara_fa_id_solicitud, ret_aclara_fa_id_derechohabiente, ret_aclara_fa_tpo_movto
        FROM   ret_aclara_accion_fa a, afi_fondo72 b, ret_det_aclara_fondo_ahorro c
        WHERE  a.id_afi_fondo72_cargo = b.id_afi_fondo72 
        AND    a.id_solicitud         = c.id_solicitud
        AND    c.estado_solicitud     = 60 -- liquidadas

        IF ret_aclara_fa_tpo_movto = "CI" THEN 
            LET v_marca = 811;
        END IF 
        IF ret_aclara_fa_tpo_movto = "UC" THEN 
            LET v_marca = 812;
        END IF 
        IF ret_aclara_fa_tpo_movto = "FA" THEN 
            LET v_marca = 811;
        END IF 
        -- se desmarca la cuenta en cuestion
        -- se marca la cuenta
        LET v_i_estado_marca = 0;
        EXECUTE FUNCTION fn_desmarca_cuenta(
                        ret_aclara_fa_id_derechohabiente
                        ,v_marca -- marca de Aclaración Fondo de Ahorro
                        ,ret_aclara_fa_id_solicitud -- identificador de registro de archivo o lote
                        ,0 -- estado marca
                        ,0 -- marca de la causa
                        ,p_usuario_cod
                        ,p_proceso_cod) -- Aclaraciones Fondo de Ahorro
         INTO v_i_estado_marca;

    END FOREACH;
}

    --TRACE("termina proceso");
    LET v_c_msj = 'proceso terminado exitosamente';

--    RETURN v_si_resultado, isam_err, v_c_msj;
END PROCEDURE;


