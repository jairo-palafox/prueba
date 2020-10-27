
--GLOBALS "RETWEL02.inc"
MAIN

  DEFINE op1        CHAR(11)    -- NSS DERECHOHABIENTE
  DEFINE op2        SMALLINT    -- RESPUESTA DE OPERACIÛN.  ACEPTADO / RECHAZADO
  DEFINE op3        SMALLINT    -- CÛDIGO DE RECHAZO 
  DEFINE op4        SMALLINT    -- DOCUMENTO FICO
  DEFINE op5        DATE        -- FECHA DE PAGO DEL BANCO

  DEFINE TERMINATE    INTEGER
  DEFINE wsstatus     INTEGER
  DEFINE v_error      STRING  
  
  IF num_args()==1 THEN
    LET AprobficoInfo_AprobficoInfoPortTypeLocation = arg_val(1)
  END IF
  
  CLOSE WINDOW SCREEN
  
  OPEN WINDOW w1 WITH FORM "cliente_fico" ATTRIBUTE (TEXT="Web Services rechazo demo",STYLE="naked")
  
  LET terminate=FALSE
  
  WHILE NOT terminate
    INPUT BY NAME op1, op2,op3, op4,op5  ATTRIBUTE (UNBUFFERED ,WITHOUT DEFAULTS)
      BEFORE INPUT
      DISPLAY "Capturar" TO msg
      
     ON ACTION plus

        CALL fn_aprob_fico_info(op1, op2,op3, op4,op5) RETURNING wsstatus
        IF wsstatus = 0 THEN
          DISPLAY "OK" TO msg
        ELSE
          LET v_error = wsError.description," - ", wsError.code ," - ",wsError.codeNS	," - ",wsError.action
          DISPLAY v_error TO msg
          --DISPLAY wsError.description TO msg
        END IF
    
     ON ACTION close
       LET terminate=true
       EXIT INPUT       
    END INPUT
  END WHILE    

  CLOSE WINDOW w1
  
END MAIN

