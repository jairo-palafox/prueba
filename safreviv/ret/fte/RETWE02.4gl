
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => ret                                                     #
#Programa          => RETWE02                                                 #
#Objetivo          => ws Solicitud de retiro solo infonavit                   #
#Fecha Inicio      => 24-May-2012                                             #
###############################################################################

--disparador de ws solicitud de retiro viv97
--httpdispatch -f as_safreviv_ws3.xcf
--port 9188 

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_tmp
DATABASE safre_viv
# 
# USER GLOBALS VARIABLES
#
GLOBALS
  DEFINE 
    ret_aprob_fico RECORD
    g_nss          CHAR(11),      -- NSS DERECHOHABIENTE
    g_res_op       SMALLINT,      -- RESPUESTA DE OPERACIÛN.  ACEPTADO / RECHAZADO
	g_cod_rechazo  SMALLINT,      -- CÛDIGO DE RECHAZO 
	g_doc_fico     SMALLINT,      -- DOCUMENTO FICO
	g_f_pago       DATE           -- FECHA DE PAGO DEL BANCO
    END RECORD 

DEFINE g_id_derechohabiente DECIMAL(9,0),
       g_nss          CHAR(11),      -- NSS DERECHOHABIENTE
       g_res_op       SMALLINT,      -- RESPUESTA DE OPERACIÛN.  ACEPTADO / RECHAZADO
	   g_cod_rechazo  SMALLINT,      -- CÛDIGO DE RECHAZO 
	   g_doc_fico     SMALLINT,      -- DOCUMENTO FICO
	   g_f_pago       DATE           -- FECHA DE PAGO DEL BANCO

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
      CALL CreateFicoService(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET SCREEN = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "RETWE020" ATTRIBUTES(TEXT = "Aprobacion fico info service", STYLE="naked")
      display_status("Aprobacion fico Service Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create Aprobacion fico service
  #
  CALL CreateFicoService(FALSE)
    
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
# Create Aprobacion fico RPC/Literal service
#
FUNCTION CreateFicoService(generateWSDL)
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  DEFINE serviceNS    STRING
  DEFINE generateWSDL SMALLINT
  DEFINE ret          INTEGER

  LET serviceNS       = "http://localhost/"

  TRY  
    #
    # Create Aprobacion fico Web Service
    #
    LET serv = com.WebService.CreateWebService("AprobficoInfo",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_aprob_fico_info 
    LET op = com.WebOperation.CreateRPCStyle("fn_aprob_fico_info","fn_aprob_fico_info",ret_aprob_fico,NULL)
    CALL serv.publishOperation(op,NULL)

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
      display_status("Aprobacion fico Info Service registered")
    END IF
    
  CATCH
    display_status("Unable to create 'Aprobacion fico Info' Web Service :"||STATUS)
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

FUNCTION fn_aprob_fico_info()   
   LET g_nss              = ret_aprob_fico.g_nss
   LET g_res_op           = ret_aprob_fico.g_res_op
   LET g_doc_fico         = ret_aprob_fico.g_doc_fico
   LET g_cod_rechazo      = ret_aprob_fico.g_cod_rechazo
   LET g_f_pago           = ret_aprob_fico.g_f_pago
   
   CALL valida_aprob()
END FUNCTION

FUNCTION valida_aprob()

INITIALIZE g_id_derechohabiente TO NULL
  
 SELECT id_derechohabiente 
   INTO g_id_derechohabiente
   FROM afi_derechohabiente  
  WHERE nss = g_nss

IF fn_valida_estatus() > 0 THEN
  --DISPLAY "solicitud estatus de la solicitud correcta"
  IF fn_valida_movimiento() > 0 THEN
    --DISPLAY "solicitud estatus del movimiento correcta"
    CALL fn_reversa_solicitud()
  ELSE     
    --DISPLAY "solicitud estatus del movimiento error"
  END IF 
ELSE
  --DISPLAY "Estatus de la solicitud erroneo"
END IF 

END FUNCTION

FUNCTION  fn_valida_estatus()
DEFINE  v_count SMALLINT 

 SELECT COUNT(id_derechohabiente)
   INTO  v_count
   FROM ret_solo_infonavit
  WHERE  estado_solicitud   = 70
    AND  id_derechohabiente = g_id_derechohabiente  
RETURN v_count
END FUNCTION

FUNCTION fn_valida_movimiento()    
DEFINE  v_count SMALLINT 

 SELECT COUNT(id_referencia)
   INTO  v_count
   FROM  cta_movimiento
  WHERE  movimiento = 172
    AND  id_derechohabiente = g_id_derechohabiente         
      
RETURN v_count
END FUNCTION

FUNCTION fn_reversa_solicitud()
DEFINE v_s_sql STRING 

LET v_s_sql = "EXECUTE PROCEDURE sp_reversa_solicitud_teso_info(?,?,?)"

            PREPARE sid_reverso_solo FROM v_s_sql
            EXECUTE sid_reverso_solo USING  g_nss,
                                            g_res_op,
                                            g_cod_rechazo

END FUNCTION 