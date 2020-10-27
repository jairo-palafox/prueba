
GLOBALS "RETWEL01.inc"
MAIN

  DEFINE op1        CHAR(11)    
  DEFINE op2        SMALLINT    
  DEFINE op3        CHAR(18)    
  DEFINE op4        CHAR(18)    
  DEFINE op5        CHAR(18)
  DEFINE op6        CHAR(18)
  DEFINE op7        DECIMAL(9,0)
  DEFINE op8        SMALLINT
  DEFINE op10       SMALLINT     

  
  DEFINE r_result     INTEGER
  DEFINE r_remaind    INTEGER
  DEFINE TERMINATE    INTEGER
  DEFINE wsstatus     INTEGER
  --DEFINE v_status     VARCHAR(50)
  DEFINE r_nss        CHAR(11)
  DEFINE r_importe    DECIMAL(19,14)
  DEFINE v_error      STRING 
  
  IF num_args()==1 THEN
    LET retirosoloinfonavit_retirosoloinfonavitPortTypeLocation = arg_val(1)
  END IF
  
  CLOSE WINDOW SCREEN
  
  OPEN WINDOW w1 WITH FORM "client" ATTRIBUTE (TEXT="Web Services rechazo demo",STYLE="naked")
  
  LET TERMINATE=FALSE
  WHILE NOT TERMINATE
    INPUT BY NAME op1, op2,op3, op4,op5, op6,op7, op8, op10 ATTRIBUTE (UNBUFFERED ,WITHOUT DEFAULTS)
      BEFORE INPUT
      DISPLAY "Capturar" TO msg
      
     ON ACTION plus

        CALL fn_rechazo(op1, op2,op3, op4,op5, op6,op7, op8, op10) RETURNING wsstatus, r_nss , r_result, r_remaind, r_importe
        IF wsstatus = 0 THEN
          --LET remaind=NULL
          DISPLAY BY NAME r_result,r_remaind
          DISPLAY "OK" TO msg
          IF r_remaind = 99 THEN
            DISPLAY "NSS NO EXISTE"  TO msg1
          ELSE  
             DISPLAY r_result  TO msg1
          END IF  
          {CALL fn_mensaje(result,remaind) RETURNING wsstatus, v_status
          IF wsstatus = 0 THEN
             DISPLAY v_status TO msg1
             DISPLAY v_status 
          END IF} 
        ELSE
          LET v_error = wsError.description," - ", wsError.code ," - ",wsError.codeNS	," - ",wsError.ACTION
          DISPLAY v_error TO msg
        END IF
    
     ON ACTION CLOSE
       LET TERMINATE=TRUE
       EXIT INPUT       
    END INPUT
  END WHILE    

  CLOSE WINDOW w1
  
END MAIN
