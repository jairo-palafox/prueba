DATABASE safre_viv
-- cliente de prueba para generar solicitud de retiro fondo ahorro
GLOBALS "RETX20.inc"

MAIN
-- datos de entrada
DEFINE v_nss           CHAR(11),
       v_grupo         CHAR( 4),
       v_fecha_val     CHAR( 8),
       v_estatus_ssv   CHAR( 4),
       v_id_pro        CHAR( 2),
       v_imp_pago_fico CHAR(12),
       v_aivs_92       CHAR(12),
       v_aivs_97       CHAR(12)

  -- datos de respuesta
DEFINE v_nss_respuesta   CHAR(11), -- nss del trabajador
       v_grupo_respuesta CHAR(4) , -- grupo del retiro
       v_saldo_adic      CHAR(12), -- saldo adicional a integrar en futuros casos
       v_saldo_no_tes    CHAR(12), -- Saldo no presente en BT. Revisar contabilidad
       v_ssv97_trans     CHAR(12), -- saldo viv97 transferido al gobierno federal
       v_ssv97_no_trans  CHAR(12), -- saldo viv97 no transferido
       v_aiv_92          CHAR(12), -- AIVs viv92
       v_aiv_97          CHAR(12), -- AIVs viv97
       v_pesos_92        CHAR(12), -- Pesos Viv92
       v_pesos_97        CHAR(12), -- Pesos viv97
       v_tc              CHAR(10), -- tipo de cambio
       v_fecha_tc_resp   CHAR(8) , -- fecha del tipo de cambio AAAAMMDD
       v_codret          CHAR(2) , -- codigo retorno
       v_mensaje         CHAR(50) -- descripcion del codigo de retorno
DEFINE wsstatus SMALLINT

  CLOSE WINDOW screen

  -- se abre la ventana
  OPEN WINDOW w1 WITH FORM "CONSULTA_SALDO_RET01"

  INPUT BY NAME 
     v_nss          , 
     v_grupo        , 
     v_fecha_val    , 
     v_estatus_ssv  , 
     v_id_pro       , 
     v_imp_pago_fico, 
     v_aivs_92      , 
     v_aivs_97       
  ATTRIBUTE (UNBUFFERED, WITHOUT DEFAULTS)
    
     ON ACTION Accept
        CALL fn_saldo_retiro(v_nss, v_grupo, v_fecha_val, v_estatus_ssv, v_id_pro, v_imp_pago_fico, v_aivs_92, v_aivs_97)
              RETURNING wsstatus         , 
                        v_nss_respuesta  ,
                        v_grupo_respuesta,
                        v_saldo_adic     ,
                        v_saldo_no_tes   ,
                        v_ssv97_trans    ,
                        v_ssv97_no_trans ,
                        v_aiv_92         ,
                        v_aiv_97         ,
                        v_pesos_92       ,
                        v_pesos_97       ,
                        v_tc             ,
                        v_fecha_tc_resp  ,
                        v_codret         ,
                        v_mensaje

           DISPLAY BY NAME wsstatus,  
                           v_nss_respuesta ,
                           v_grupo_respuesta,
                           v_saldo_adic     ,
                           v_saldo_no_tes   ,
                           v_ssv97_trans    ,
                           v_ssv97_no_trans ,
                           v_aiv_92         ,
                           v_aiv_97         ,
                           v_pesos_92       ,
                           v_pesos_97       ,
                           v_tc             ,
                           v_fecha_tc_resp  ,
                           v_codret         ,
                           v_mensaje

           CALL ui.interface.refresh()

     ON ACTION Cancel
       EXIT INPUT       
  END INPUT

  CLOSE WINDOW w1
  
END MAIN

