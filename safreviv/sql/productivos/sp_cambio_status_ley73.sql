






CREATE PROCEDURE "safreviv".sp_cambio_status_ley73(p_id_derechohabiente  DECIMAL(9,0)
                                        ,p_id_solicitud       DECIMAL(9,0)
                                        ,p_estado_solicitud   SMALLINT 
                                        ,p_cod_rechazo        SMALLINT 
                                        ,p_viv97_val          DECIMAL(12,2)
                                        ,p_viv92_val          DECIMAL(12,2)
                                        ,p_aivs97             DECIMAL(12,2)
                                        ,p_aivs92             DECIMAL(12,2)
                                        ,p_mot_cod            CHAR(2)
                                        );

IF p_mot_cod = 'A' THEN
UPDATE ret_ley73 
        Set  estado_solicitud  = p_estado_solicitud
             ,cod_rechazo      = p_cod_rechazo
    WHERE id_solicitud         = p_id_solicitud
      AND id_derechohabiente   = p_id_derechohabiente;
END IF 

IF p_mot_cod = 'M' THEN
UPDATE ret_ley73 
        Set  estado_solicitud    = p_estado_solicitud
             ,cod_rechazo        = p_cod_rechazo
             ,importe_viv97      = p_viv97_val
             ,importe_viv92      = p_viv92_val
             ,aivs_viv97         = p_aivs97
             ,aivs_viv92         = p_aivs92
             ,f_valuacion        = TODAY 
    WHERE id_solicitud         = p_id_solicitud
      AND id_derechohabiente   = p_id_derechohabiente;
END IF 
    
END PROCEDURE;


