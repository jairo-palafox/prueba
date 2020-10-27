
GLOBALS "RETWEL08.inc"


MAIN
  DEFINE op1        CHAR(11)             -- NSS
         ,op2        SMALLINT            -- GRUPO DE TRABAJADOR 
         ,op3        DATE                -- FECHA DE MARACA 
         ,op4        SMALLINT            -- CLAVE DE AFORE
         ,op5        SMALLINT            -- CASA RECHAZO
         
         ,v_g_nss           CHAR(11)     --Número de seguridad social del trabajador
         ,v_cod_ret         SMALLINT     --Código de retorno   Según cátalogo a dos posiciones   Numérico 
         ,v_mensaje_cod_ret CHAR(100)
  
  DEFINE TERMINATE    INTEGER
  DEFINE wsstatus     INTEGER  
  
  IF num_args()==1 THEN
    LET BajaMarca_BajaMarcaPortTypeLocation = arg_val(1)
  END IF
  
  CLOSE WINDOW SCREEN
  
  OPEN WINDOW w1 WITH FORM "client_desmarca_73" ATTRIBUTE (TEXT="Web Services desmarca Ley73 demo",STYLE="naked")
  
  LET TERMINATE=FALSE
  WHILE NOT TERMINATE
    INPUT BY NAME op1,op2,op3,op4,op5 ATTRIBUTE (UNBUFFERED ,WITHOUT DEFAULTS)

     BEFORE INPUT   
     LET op3 = TODAY 

     ON ACTION plus
        CALL fn_baja_marca(op1,op2,op3,op4,op5) RETURNING wsstatus,
                                                    v_g_nss,
                                                    v_cod_ret,
                                                    v_mensaje_cod_ret
        IF wsstatus = 0 THEN
          DISPLAY BY NAME v_g_nss,
                          v_cod_ret,
                          v_mensaje_cod_ret
                          
          DISPLAY "OK" TO msg
          IF wsstatus = 0 THEN
             DISPLAY v_mensaje_cod_ret TO msg1             
          END IF 
        ELSE
          DISPLAY wsError.description TO msg
        END IF
    
     ON ACTION CLOSE
       LET TERMINATE=TRUE
       EXIT INPUT       
    END INPUT
  END WHILE    

  CLOSE WINDOW w1
  
END MAIN

