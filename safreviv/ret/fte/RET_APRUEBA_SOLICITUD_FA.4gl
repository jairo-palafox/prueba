-- Cliente de prueba para aprobar/rechazar solicitudes de retiro de fondo
-- de ahorro por parte de CESI

GLOBALS "RETX04.inc"


MAIN
  DEFINE v_nss           char(11)    -- nss
  DEFINE v_rfc           CHAR(13)    -- caso adai
  DEFINE v_causal        SMALLINT    -- Identificador de beneficiario (si aplica)
  DEFINE v_autoriza_pago SMALLINT

  DEFINE wsstatus     INTEGER
  DEFINE r_result     INTEGER 
  
  
  DEFINE r_nss          CHAR(11)
  DEFINE r_rfc          CHAR(13)
  DEFINE r_res_op       SMALLINT
  DEFINE r_cod_rechazo  SMALLINT
  DEFINE v_error      STRING 
  
  CLOSE WINDOW SCREEN
  
  OPEN WINDOW w1 WITH FORM "RET_APRUEBA_SOLICITUD_FA01.4fd" ATTRIBUTE (TEXT="Web Services resolucion demo")
  
    INPUT BY NAME v_nss, v_rfc, v_causal, v_autoriza_pago
    WITHOUT DEFAULTS
    ATTRIBUTE ( UNBUFFERED )

     ON ACTION Accept
        CALL fn_aprueba_solicitud_fa(v_nss, v_rfc, v_causal, v_autoriza_pago) 
             RETURNING wsstatus, r_nss, r_rfc, r_res_op, r_cod_rechazo

        IF ( wsstatus = 0 ) THEN
          --LET r_remaind=NULL
          DISPLAY BY NAME wsstatus, r_res_op,r_cod_rechazo
          DISPLAY "OK" TO msg
        ELSE
          LET v_error = wsError.description," - ", wsError.code ," - ",wsError.codeNS	," - ",wsError.action
          DISPLAY v_error TO msg
          --DISPLAY wsError.description TO msg
        END IF
    
     ON ACTION Cancel
       EXIT INPUT       
    END INPUT    

  CLOSE WINDOW w1
  
END MAIN

