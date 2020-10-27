






CREATE PROCEDURE "safreviv".sp_reversa_marca_decreto( p_id_decreto DECIMAL(9,0),
                                   p_marca_entra  SMALLINT,
                                   p_n_referencia DECIMAL(9,0),
                                   p_folio        DECIMAL(9,0)
                                 )
    DEFINE v_fecha_ini DATE;
    DEFINE v_hora_ini  DATETIME HOUR TO SECOND;

    -- -----------------------------------------------------------------------------

    -- Obtiene datos necesarios para buscar en historico
    SELECT f_inicio    
      INTO v_fecha_ini   
      FROM sfr_marca_activa_decreto
     WHERE id_decreto   = p_id_decreto
       AND marca        = p_marca_entra
       AND n_referencia = p_n_referencia 
       AND folio        = p_folio;

    SET LOCK MODE TO WAIT ;

    -- Elimina de marcas activas.
    DELETE
      FROM sfr_marca_activa_decreto
     WHERE id_decreto   = p_id_decreto
       AND marca        = p_marca_entra
       AND n_referencia = p_n_referencia 
       AND f_inicio     = v_fecha_ini;

    -- Elimina la marca del historico
    DELETE
      FROM sfr_marca_historica_decreto
     WHERE id_decreto   = p_id_decreto
       AND marca        = p_marca_entra
       AND n_referencia = p_n_referencia
       AND f_inicio     = v_fecha_ini
       AND folio        = p_folio;

    -- Elimina de historico las marcas que pudieran haber surgido por rechazos
    DELETE
      FROM sfr_marca_historica_decreto
     WHERE id_decreto   = p_id_decreto
       AND marca        = p_marca_entra
       AND n_referencia = p_n_referencia
       AND f_inicio     = v_fecha_ini
       AND folio        = p_folio
       AND rch_cod   > 0;

    -- Elimina las marcas que pudieran surgido solo en histórico
    DELETE
      FROM sfr_marca_historica_decreto
     WHERE id_decreto   = p_id_decreto
       AND marca        = p_marca_entra
       AND n_referencia = p_n_referencia
       AND folio        = p_folio -- Se elimina fecha de inicio 
       AND estado_marca   > 0;

    SET LOCK MODE TO NOT WAIT;

    RETURN ;

END PROCEDURE;


