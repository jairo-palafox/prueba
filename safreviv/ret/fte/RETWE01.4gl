
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => ret                                                     #
#Programa          => RETP01                                                  #
#Objetivo          => ws Solicitud de retiro solo infonavit vivienda97        #
#Fecha Inicio      => 03-Ene-2012                                             #
###############################################################################

--disparador de ws solicitud de retiro viv97
--httpdispatch -f as_safreviv_ws.xcf
--port  9065
IMPORT FGL WSHelper
IMPORT com

--GLOBALS "RETWEL01.inc"

DATABASE safre_tmp
DATABASE safre_viv
# 
# USER GLOBALS VARIABLES
#
GLOBALS
  DEFINE 
    ret_rechazo RECORD
      g_nss            CHAR(11),     -- NSS DERECHOHABIENTE
      g_ident          SMALLINT,     -- Identificador de beneficiario (si aplica)
      g_nombre         CHAR(18),     -- Nombre del beneficiario 
      g_ape_pat        CHAR(18),     -- Apellido paterno 
      g_ape_mat        CHAR(18),     -- Apellido materno 
      g_cve_bancaria   CHAR(18) , -- CLABE interbancaria
      g_cve_banco      DECIMAL(9,0), -- Clave de Banco 
      g_entidad        SMALLINT,     -- Entidad federativa 
    --  g_fec_aut        DATE,         -- Fecha autorización 
      g_causal         SMALLINT      -- Causal de retiro
       
    END RECORD,

    ret_respuesta RECORD
      r_g_nss          CHAR(11),       -- Número de seguridad social del trabajador
      --r_g_status       SMALLINT,     -- estado de la solicitud // se elimina ya que solo se necesita enviar si procede o no dicha petision
      r_g_res_op       SMALLINT,       -- Respuesta de operación.  Aceptado / Rechazado
      r_g_cod_rechazo  SMALLINT ,      -- Código de rechazo 
      r_g_imp_viv97    DECIMAL(19,14)  -- Importe de vivienda 97 
    END RECORD
    
{modificacion ya que el cliente ya contara con los parametros y rechazos y no necesita validacion
    ,ret_estatus RECORD
      g_estado    SMALLINT ,
      g_rechazo   SMALLINT 
    END RECORD,
    
    ret_desc_estatus RECORD 
      g_desc_estatus varchar(50)       
    END RECORD
 }   
DEFINE g_id_derechohabiente DECIMAL(9,0),
       g_nss                CHAR(11),     -- NSS DERECHOHABIENTE
       g_ident              SMALLINT,     -- Identificador de beneficiario (si aplica)
       g_nombre             CHAR(18),     -- Nombre del beneficiario 
       g_ape_pat            CHAR(18),     -- Apellido paterno 
       g_ape_mat            CHAR(18),     -- Apellido materno 
       g_cve_bancaria       CHAR(18) , -- CLABE interbancaria
       g_cve_banco          DECIMAL(9,0), -- Clave de Banco 
       g_entidad            SMALLINT,     -- Entidad federativa 
       g_fec_aut            DATE,         -- Fecha autorización 
       g_causal             SMALLINT      -- Causal de retiro
DEFINE g_acc_acciones       DECIMAL(16,6)
DEFINE g_acc_pesos          DECIMAL(16,6)
DEFINE g_id_solicitud       SMALLINT 

END GLOBALS

DEFINE serverURL STRING
DEFINE SCREEN    BOOLEAN

&define display_status(status) \
  IF NOT screen THEN \
    DISPLAY status \
  ELSE \
    DISPLAY status TO MSG \
    MENU ON idle 1 EXIT MENU ON ACTION close EXIT PROGRAM END MENU \
  END IF
#
# MAIN
#
MAIN
  DEFINE ret INTEGER  
      
  DEFER INTERRUPT

  LET SCREEN = FALSE
  #
  # Check arguments
  #
  --fglrun RETW01 
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateRechazoService(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET SCREEN = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "RETWE010" ATTRIBUTES(TEXT = "Rechazo Retiro service", STYLE="naked")
      display_status("Rechazo Retiro Service Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create Rechazo Retiro service
  #
  CALL CreateRechazoService(FALSE)
    
  #
  # Start the server
  #
  display_status("Starting server...")
  #
  # Starts the server on the port number specified by the FGLAPPSERVER environment variable
  #  (EX: FGLAPPSERVER=9065)
  # 
  CALL com.WebServiceEngine.Start()
  display_status("The server is listening.")

  IF SCREEN THEN
    MENU
      ON IDLE 1
        LET ret = com.WebServiceEngine.ProcessServices(1)
        CASE ret
          WHEN 0
            DISPLAY "Request processed." TO msg
          WHEN -1
            DISPLAY "No request..." TO msg
          WHEN -2
            DISPLAY "Disconnected from application server." TO msg
            EXIT PROGRAM   # The Application server has closed the connection
          WHEN -3
            DISPLAY "Client Connection lost." TO msg
          WHEN -4
            DISPLAY "Server interrupted with Ctrl-C." TO msg
          WHEN -10
            DISPLAY "Internal server error." TO msg
        END CASE
      ON ACTION CLOSE
        EXIT PROGRAM
    END MENU
  ELSE
    WHILE TRUE
      LET ret = com.WebServiceEngine.ProcessServices(-1)
      CASE ret
        WHEN 0
          DISPLAY "Request processed." 
        WHEN -1
          DISPLAY "Timeout reached."
        WHEN -2
          DISPLAY "Disconnected from application server."
          EXIT PROGRAM   # The Application server has closed the connection
        WHEN -3
          DISPLAY "Client Connection lost."
        WHEN -4
          DISPLAY "Server interrupted with Ctrl-C."
        WHEN -10
          DISPLAY "Internal server error."
      END CASE
      IF INT_FLAG<>0 THEN
        LET INT_FLAG=0
        EXIT WHILE
      END IF     
    END WHILE
    DISPLAY "Server stopped"
  END IF
END MAIN

#
# Create Rechazo Retiro RPC/Literal service
#
FUNCTION CreateRechazoService(generateWSDL)
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  DEFINE serviceNS    STRING
  DEFINE generateWSDL SMALLINT
  DEFINE ret          INTEGER

  LET serviceNS       = "http://localhost/"

  TRY
  
    #
    # Create Rechazo retiro Web Service
    #
    LET serv = com.WebService.CreateWebService("retirosoloinfonavit",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_rechazo 
    LET op = com.WebOperation.CreateRPCStyle("fn_rechazo","fn_rechazo",ret_rechazo,ret_respuesta)
    CALL serv.publishOperation(op,NULL)

   { # fn_mensaje 
    LET op = com.WebOperation.CreateRPCStyle("fn_mensaje","fn_mensaje",ret_estatus,ret_desc_estatus)
    CALL serv.publishOperation(op,NULL)}
    
    IF generateWSDL THEN
      #
      # Generate WSDL
      #
      LET ret = serv.saveWSDL(serverURL)
      IF ret=0 THEN
        DISPLAY "WSDL saved"      
      ELSE
        DISPLAY "ERROR: Unable to save WSDL"
      END IF
    ELSE
      #
      # Register service  
      #
      CALL com.WebServiceEngine.RegisterService(serv)  
      display_status("Rechazo Retiro Service registered")
    END IF
    
  CATCH
    display_status("Unable to create 'Rechazo Retiro' Web Service :"||STATUS)
    EXIT PROGRAM
  END TRY    
END FUNCTION



FUNCTION exitHelp()
  DISPLAY "Usage: "
  DISPLAY "  ", arg_val(0)
  DISPLAY "    Start the server on port defined by FGLAPPSERVER"
  DISPLAY "  ", arg_val(0), " -W serverurl"
  DISPLAY "    Generate the WSDL file for the given url"
  DISPLAY "  ", arg_val(0), " -S port"
  DISPLAY "    Start service in graphical mode and on given port"
  EXIT PROGRAM
END FUNCTION

#
# USER PUBLIC FUNCTIONS
#

FUNCTION fn_rechazo()
   
   LET g_nss            = ret_rechazo.g_nss
   LET g_ident          = ret_rechazo.g_ident
   LET g_nombre         = ret_rechazo.g_nombre
   LET g_ape_pat        = ret_rechazo.g_ape_pat
   LET g_ape_mat        = ret_rechazo.g_ape_mat
   LET g_cve_bancaria   = ret_rechazo.g_cve_bancaria
   LET g_cve_banco      = ret_rechazo.g_cve_banco
   LET g_entidad        = ret_rechazo.g_entidad
   LET g_causal         = ret_rechazo.g_causal

   CALL valida_solicitud()
END FUNCTION

FUNCTION valida_solicitud()
DEFINE r_saldo              DECIMAL(19,6)
DEFINE r_id_transaccion     SMALLINT 
DEFINE v_count_bnd          SMALLINT 

LET v_count_bnd = 0
INITIALIZE g_id_derechohabiente TO NULL 
     --DISPLAY "g_nss,g_cve_bancaria ",g_nss," ",g_cve_bancaria
     --Obtenemos id_derechohabiente según número seguro social
     
     IF LENGTH(g_cve_bancaria) <=0 THEN 
        --ERROR  "La clabe no cuenta con 18 digitos favor de confirmarlo"
           LET ret_respuesta.r_g_res_op       = 100
           LET ret_respuesta.r_g_cod_rechazo  = 66 --ERV
           LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
           RETURN   
     END IF 

      SELECT id_derechohabiente
        INTO g_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = g_nss; 

        IF  g_id_derechohabiente IS NULL OR g_id_derechohabiente = "" THEN
           -- regresa codigo si no existe id_derchohabiente en la tabla           
           LET ret_respuesta.r_g_res_op       = 100
           LET ret_respuesta.r_g_cod_rechazo  = 99 --ERV
           LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
           RETURN 
        END IF

      SELECT COUNT(id_derechohabiente) 
        INTO v_count_bnd
        FROM ret_solo_infonavit 
       WHERE id_derechohabiente  = g_id_derechohabiente
         AND  estado_solicitud IN (10,50,60,70);   

      IF v_count_bnd > 0 THEN
       --se elimina el regreso del estado al cliente
       --LET ret_respuesta.r_g_status = 100
       LET ret_respuesta.r_g_res_op       = 100
       LET ret_respuesta.r_g_cod_rechazo  = 30
       LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
       RETURN 
      END IF

      IF g_causal IS NULL THEN
      --El tipo causal retiro es nulo
           LET ret_respuesta.r_g_res_op       = 100
           LET ret_respuesta.r_g_cod_rechazo  = 88 --ERV
           LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
           RETURN
       END IF  

      SELECT COUNT(*) 
        INTO v_count_bnd
        FROM ret_causal_retiro
       WHERE causal_retiro = g_causal;   

      IF v_count_bnd <=0 THEN
         --se le regresa el cliente ya qe no coincide con la tabla de causal retiro  01-04
         LET ret_respuesta.r_g_res_op       = 100
         LET ret_respuesta.r_g_cod_rechazo  = 77 --ERV
         LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
         RETURN 
      END IF 
 

      --Obtener monto del movimiento del derechohabiente  
      --funcion de calculo de saldo de vivienda 97
      CALL fn_recupera_saldo(g_id_derechohabiente) RETURNING g_acc_pesos,g_acc_acciones
      LET r_saldo = g_acc_pesos
      --DISPLAY   "r_saldo ", r_saldo
      IF r_saldo > 0 THEN          
         --funcion recuperacion de credito
         IF fn_rec_credito() = 0  THEN
         --no se encontro credito
         --funcion que realiza los cambios de status y control de flujo
            CALL fn_solit_acept(r_saldo) 
         ELSE
           --se elimina el regreso del estado al cliente
           --LET ret_respuesta.r_g_status = 100
           LET ret_respuesta.r_g_res_op       = 100
           LET ret_respuesta.r_g_cod_rechazo  = 20
           LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
           --se cambian los saldos a cero por rechazo
           LET g_acc_pesos    = 0
           LET g_acc_acciones = 0
          --se encontro credito
            CALL fn_genera_status(0,                              --id_solicitud
                                  1,                              --solo infonavit
                                  --ret_respuesta.r_g_res_op,     --Se acepta / rechaza la operacion
                                  100,
                                  --ret_respuesta.r_g_status,     --rechazo
                                  ret_respuesta.r_g_cod_rechazo,  --Rechazo por Credito                                  
                                  'A'                             --Alta de registro
                                  )RETURNING r_id_transaccion
        END IF 
      ELSE 
       --se elimina el regreso del estado al cliente
       --LET ret_respuesta.r_g_status = 100
       LET ret_respuesta.r_g_res_op       = 100
       LET ret_respuesta.r_g_cod_rechazo  = 10
       LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
       --se cambian los saldos a cero por rechazo
           LET g_acc_pesos    = 0
           LET g_acc_acciones = 0
         CALL fn_genera_status(0,                              --id_solicitud
                               1,                              --solo infonavit
                               --ret_respuesta.r_g_res_op,     --Se acepta / rechaza la operacion
                               100,
                               --ret_respuesta.r_g_status,     --rechazo
                               ret_respuesta.r_g_cod_rechazo,  --Rechazo por Saldo insuficiente
                               'A'                             --Alta de registro
                               )RETURNING r_id_transaccion
   
      END IF 
--RETURN r_id_transaccion
END FUNCTION 

#Objetivo se genera la solicitud en con estatus 10 rechazo 0
FUNCTION fn_solit_acept(r_saldo)
DEFINE r_saldo              DECIMAL(19,14)
DEFINE r_id_solicitud       DECIMAL(9,0) 

LET r_id_solicitud = 0     
      
        CALL fn_genera_status(0,                        --id_solicitud
                              1,                        --solo infonavit
                              10,                        --Capturado
                              0,                        --sin rechazo                            
                              'A'                       --Alta de registro
                              )RETURNING r_id_solicitud
                              
    IF r_id_solicitud <> 0 THEN   
         --se elimina el regreso del estado al cliente
         --LET ret_respuesta.r_g_status = 10
         LET ret_respuesta.r_g_res_op       = 10
         LET ret_respuesta.r_g_cod_rechazo  = 0
         --CALL fn_aviso_cliente()
    END IF 
    

END FUNCTION

{
FUNCTION fn_aviso_cliente()
   --CALL fn_preliquidacion_retiro()
END FUNCTION 
}
#Objetivo se valida que exista credito en 
FUNCTION fn_rec_credito()
   DEFINE v_credito  SMALLINT 
  -- DISPLAY "busco credito default manda cero por falta de tabla"
   SELECT COUNT(*)
     INTO v_credito
     FROM cta_credito
    WHERE id_derechohabiente  = g_id_derechohabiente
    --AND TODAY BETWEEN f_credito AND  f_credito UNITS YEAR  --ERV
    
RETURN v_credito
END FUNCTION 

FUNCTION fn_recupera_saldo(v_id_derechohabiente)
   
   DEFINE v_precio_fondo                DECIMAL(19,6)
   DEFINE r_saldo_acciones              DECIMAL(19,6)
   DEFINE r_saldo_pesos                 DECIMAL(19,6)
   DEFINE v_id_derechohabiente          DECIMAL(9,0)
   DEFINE v_resultado_consulta          SMALLINT 
   DEFINE v_s_sql                       VARCHAR(200)
   DEFINE v_subcuenta                   SMALLINT
   DEFINE v_fondo                       SMALLINT
   DEFINE v_fec_saldo                   DATE   
   
   LET v_fec_saldo = TODAY
   LET v_subcuenta = 44
   LET v_fondo = 11
   LET v_precio_fondo = 0 

   --se coloca la funcion saldo_dia para calcular el saldo del derechohabiente 
   --conforme ala validacion de preliquidacion 
   --Operacion aplica con calculo de la funcion

   LET v_s_sql = "EXECUTE FUNCTION fn_recupera_saldo_valuado(?,?,?,?,?)" 
   DISPLAY g_nss,v_id_derechohabiente,v_subcuenta,v_fec_saldo
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_saldo_dia FROM v_s_sql
   EXECUTE sid_saldo_dia USING g_nss,v_id_derechohabiente,v_subcuenta,v_fec_saldo,v_fondo
                          --INTO v_resultado_consulta, r_saldo_acciones, r_saldo_pesos
                          INTO  r_saldo_pesos, r_saldo_acciones,v_resultado_consulta

    IF r_saldo_acciones <= 0 THEN
       LET r_saldo_acciones = 0
       LET r_saldo_pesos = 0
    END IF    
  
RETURN r_saldo_pesos,r_saldo_acciones
END FUNCTION  

FUNCTION fn_genera_status(v_id_solicitud,v_tipo_r,v_estatus,v_causa_rec,v_metodo)
DEFINE v_id_solicitud       SMALLINT 
DEFINE v_tipo_r             SMALLINT 
DEFINE v_estatus            SMALLINT 
DEFINE v_causa_rec          SMALLINT 
DEFINE v_metodo             CHAR(1) 
DEFINE v_count_bnd          SMALLINT

LET  v_count_bnd = 0 

      SELECT COUNT(id_derechohabiente) 
        INTO v_count_bnd
        FROM ret_solo_infonavit 
       WHERE id_derechohabiente  = g_id_derechohabiente
         AND  estado_solicitud IN (10,50,60,70); 

      IF v_count_bnd > 0 THEN
         --se elimina el regreso del estado al cliente
         --LET ret_respuesta.r_g_status = 100
         LET ret_respuesta.r_g_res_op       = 100
         LET ret_respuesta.r_g_cod_rechazo  = 30
         LET ret_respuesta.r_g_imp_viv97    = 0 --ERV
         RETURN 0 
      END IF          

      
      --DISPLAY "g_id_derechohabiente, v_id_solicitud ", g_id_derechohabiente," ",v_id_solicitud
      --WHENEVER ERROR CONTINUE  
      PREPARE sp_solicitud FROM "EXECUTE PROCEDURE sp_insert_solicitud_retiro_solo_infonavit(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      EXECUTE sp_solicitud USING g_id_derechohabiente,
                                 v_id_solicitud,
                                 v_tipo_r,
                                 v_causa_rec,
                                 v_estatus,
                                 g_acc_acciones,
                                 g_acc_pesos,
                                 g_cve_bancaria,
                                 g_cve_banco,
                                 g_entidad,
                                 g_causal,
                                 g_ident,       -- Identificador de beneficiario (si aplica)
                                 g_nombre,      -- Nombre del beneficiario 
                                 g_ape_pat,     -- Apellido paterno 
                                 g_ape_mat,     -- Apellido materno 
                                 v_metodo
     -- WHENEVER ERROR STOP
     LET  v_count_bnd = 1
RETURN v_count_bnd

END FUNCTION 

