#
# FOURJS_START_COPYRIGHT(U,2000)
# PROPERTY OF FOUR JS*
# (C) COPYRIGHT FOUR JS 2000, 2011. ALL RIGHTS RESERVED.
# * TRADEMARK OF FOUR JS DEVELOPMENT TOOLS EUROPE LTD
#   IN THE UNITED STATES AND ELSEWHERE
# 
# FOUR JS AND ITS SUPPLIERS DO NOT WARRANT OR GUARANTEE THAT THESE SAMPLES
# ARE ACCURATE AND SUITABLE FOR YOUR PURPOSES. THEIR INCLUSION IS PURELY 
# FOR INFORMATION PURPOSES ONLY.
# FOURJS_END_COPYRIGHT
#

GLOBALS "ws_retiroLey73.inc"


MAIN
  DEFINE op1        CHAR(11)           -- NSS
  DEFINE op2        SMALLINT           -- GRUPO DE TRABAJADOR 

DEFINE v_g_nss           CHAR(11)     ,--Número de seguridad social del trabajador
       v_nombre_af       CHAR(40)     ,--Nombre  Nombre del trabajador    Texto
       v_paterno_af      CHAR(40)     ,--Apellido paterno  Apellido paterno del trabajador   Texto  
       v_materno_af      CHAR(40)     ,--Apellido materno  Apellido materno del trabajador   Texto             
       v_importeviv97    DECIMAL(12,2),--Importe de Vivienda 97     Numérico
       v_importeviv92    DECIMAL(12,2),--Importe de Vivienda 92     Numérico
       v_cod_ret         SMALLINT     ,--Código de retorno   Según cátalogo a dos posiciones   Numérico 
       v_mensaje_cod_ret CHAR(100)    ,--Mensaje   Mensaje de código de retorno    Texto 
       v_rfc_af          CHAR(13)     ,--RFC   RFC del trabajador   
       v_curp_af         CHAR(18)     ,--CURP  CURP del trabajador   
{       v_nombre_NRP      CHAR(40)     ,--NRP   Número de Registro Patronal 
       v_rfc_pat         CHAR(13)     ,--Nombre   Nombre del NRP  } 
       v_marca_jur       SMALLINT      --Marca Jurídico   Estatús marca en sistema 
  
  DEFINE terminar    INTEGER
  DEFINE wsstatus     INTEGER  
  
  IF num_args()==1 THEN
    LET RetiroLey73_RetiroLey73PortTypeLocation = arg_val(1)
  END IF
   
  OPEN WINDOW w1 WITH FORM "RET_SOLICITUD_LEY73" ATTRIBUTE (TEXT="Genera solicitud Retiro Ley 73")
  
  LET terminar=FALSE

    INPUT BY NAME op1,op2 ATTRIBUTE (UNBUFFERED ,WITHOUT DEFAULTS)

     ON ACTION accept
        CALL fn_genera_solicitud_retiro_ley73(op1, op2) RETURNING wsstatus,
                                                    v_g_nss,
                                                    v_nombre_af,
                                                    v_paterno_af,
                                                    v_materno_af,
                                                    v_importeviv97,
                                                    v_importeviv92,
                                                    v_cod_ret,
                                                    v_mensaje_cod_ret,
                                                    v_rfc_af,
                                                    v_curp_af,
                                                    --v_nombre_nrp,
                                                    --v_rfc_pat,
                                                    v_marca_jur
        IF wsstatus = 0 THEN
          DISPLAY BY NAME v_g_nss,
                          v_nombre_af,
                          v_paterno_af,
                          v_materno_af,
                          v_importeviv97,
                          v_importeviv92,
                          v_cod_ret,
                          v_mensaje_cod_ret,
                          v_rfc_af,
                          v_curp_af,
                          --v_nombre_nrp,
                          --v_rfc_pat,
                          v_marca_jur
                          
          DISPLAY "OK" TO msg
          IF wsstatus = 0 THEN
             DISPLAY v_mensaje_cod_ret TO msg1             
          END IF 
        ELSE
          DISPLAY wsError.description TO msg
        END IF
    
     ON ACTION cancel
       EXIT INPUT       
    END INPUT 

  CLOSE WINDOW w1
  
END MAIN

