GLOBALS "RETWEL09.inc"


MAIN
  DEFINE  op1  CHAR(11)           -- NSS DERECHOHABIENTE
         ,op2  SMALLINT           --1 titular, 2 beneficiario                                 
         ,op3  CHAR(40)           --Nombre beneficiario                                       
         ,op4  CHAR(20)           --Apellido paterno beneficiario                             
         ,op5  CHAR(20)           --Apellido materno beneficiario                             
         ,op6  CHAR(18)           --CLABE                                                     
         ,op7  CHAR(18)           --Clave de Banco                                            
         ,op8  CHAR(2)            --Entidad federativa                                        
         ,op9  DATE               --Fecha autorización                                        
         ,op10 SMALLINT           --Tipo de grupo de pensionados                              
         ,op11 DATE               --Fecha de marca                                            
         ,op12 SMALLINT           --Clave de AFORE                                            
         ,op13 SMALLINT           --Número de caso en ADAI                                    
         ,op14 SMALLINT           --Número de laudo (grupo 2, 3)                              
         ,op15 SMALLINT           --Número de junta de conciliación y arbitraje (grupos 2 y 3)
         
         ,v_g_nss           CHAR(11)     --Número de seguridad social del trabajador
         ,v_cod_ret         SMALLINT     --Código de retorno   Según cátalogo a dos posiciones   Numérico 
         ,v_mensaje_cod_ret CHAR(100)
  
  DEFINE TERMINATE    INTEGER
  DEFINE wsstatus     INTEGER  
  
  IF num_args()==1 THEN
    LET AutorizaLey73_AutorizaLey73PortTypeLocation = arg_val(1)
  END IF
  
  CLOSE WINDOW SCREEN
  
  OPEN WINDOW w1 WITH FORM "client_autoriza_73" ATTRIBUTE (TEXT="Web Services Autoriza Ley73 demo",STYLE="naked")
  
  LET TERMINATE=FALSE
  WHILE NOT TERMINATE
    INPUT BY NAME op1,op2,op3,op4,op5,op6,op7,op8,op9,op10,op11,op12,op13,op14,op15 ATTRIBUTE (UNBUFFERED ,WITHOUT DEFAULTS)

     BEFORE INPUT   
     LET op9 = TODAY
     LET op11 = TODAY 

     ON ACTION plus
        CALL fn_autoriza_ley73(op1,op2,op3,op4,op5,op6,op7,op8,op9,op10,op11,op12,op13,op14,op15)
                   RETURNING wsstatus,
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

