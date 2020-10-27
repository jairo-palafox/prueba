
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => ret                                                     #
#Programa          => RETWE07                                                 #
#Objetivo          => ws marca de tramite judicial                            #
#Fecha Inicio      => 06-Jun-2012                                             #
###############################################################################

--disparador de ws marca ley 73 
--httpdispatch -f as_safreviv_ws6.xcf
--port  9191

IMPORT FGL WSHelper
IMPORT com
IMPORT XML 

DATABASE safre_viv
# 
# USER GLOBALS VARIABLES
#
GLOBALS
  DEFINE 
    ret_generacion_marca RECORD
      g_nss            CHAR(11),     -- NSS DERECHOHABIENTE
      g_grupo_tr       SMALLINT,     -- GRUPO DE TRABAJADOR
      g_f_marca        DATE    ,     -- FECHA DE MARCA 
      g_clave_afore    SMALLINT      -- CLAVE DE AFORE 
       
    END RECORD,

    ret_respuesta RECORD
      r_g_nss           CHAR(11)     ,--Número de seguridad social del trabajador
      r_cod_ret         SMALLINT     ,--Código de retorno   Según cátalogo a dos posiciones   Numérico 
      r_mensaje_cod_ret CHAR(100)     --Mensaje de código de retorno    Texto 
    END RECORD
    
 DEFINE g_id_derechohabiente DECIMAL(9,0)
       ,g_nss                CHAR(11)      -- NSS DERECHOHABIENTE
       ,g_grupo_tr           SMALLINT      -- Grupo de trabajador
       ,g_viv97_val          DECIMAL (12,2)       
       ,g_viv92_val          DECIMAL (12,2) 
       ,g_aivs97             DECIMAL (12,2) 
       ,g_aivs92             DECIMAL (12,2) 
       ,g_f_marca            DATE          -- FECHA DE MARACA 
       ,g_clave_afore        SMALLINT      -- CLAVE DE AFORE 


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
  --fglrun RETWE07
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateRechazoService(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET SCREEN = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "RETWE010" ATTRIBUTES(TEXT = "Marca Ley 73", STYLE="naked")
      display_status(" Retiro Ley 73 Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create  Retiro Ley 73
  #
  CALL CreateRechazoService(FALSE)
    
  #
  # Start the server
  #
  display_status("Starting server...")
  #
  # Starts the server on the port number specified by the FGLAPPSERVER environment variable
  #  (EX: FGLAPPSERVER=9190)
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
    LET serv = com.WebService.CreateWebService("AltaMarca",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_rechazo 
    LET op = com.WebOperation.CreateRPCStyle("fn_alta_marca","fn_alta_marca",ret_generacion_marca,ret_respuesta)
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
      display_status(" Retiro Ley 73 registered")
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

FUNCTION fn_alta_marca()
       
       LET  g_id_derechohabiente = 0
       INITIALIZE g_nss TO NULL 
       INITIALIZE g_grupo_tr TO NULL 
       LET  g_viv97_val = 0
       LET  g_viv92_val = 0
       LET  g_aivs97 = 0 
       LET  g_aivs92 = 0
       
   LET g_nss           = ret_generacion_marca.g_nss
   LET g_grupo_tr      = ret_generacion_marca.g_grupo_tr
   LET g_f_marca       = ret_generacion_marca.g_f_marca
   LET g_clave_afore   = ret_generacion_marca.g_clave_afore       
   
   CALL fn_marcas()   
END FUNCTION

FUNCTION fn_marcas()
DEFINE v_estado               SMALLINT
DEFINE v_i_estado_marca       INTEGER
DEFINE v_marca_Ley73   INTEGER
DEFINE v_script               STRING 
DEFINE v_id_solicitud         DECIMAL(9,0)
DEFINE p_cod_user             char(20)

LET p_cod_user = 'safreviv'

     SELECT id_derechohabiente
         INTO  g_id_derechohabiente
         FROM afi_derechohabiente 
         WHERE nss = g_nss
  
   CALL fn_marcando_nss() RETURNING v_estado   
   IF  v_estado > 0 THEN
       DISPLAY "no se marca la cuenta ya esta marcada"
       LET ret_respuesta.r_g_nss = g_nss
       LET ret_respuesta.r_cod_ret = 666

      IF ret_respuesta.r_cod_ret IS NULL THEN
        LET ret_respuesta.r_mensaje_cod_ret = "Codigo no existente"
      ELSE  
         SELECT desc_larga 
           INTO ret_respuesta.r_mensaje_cod_ret
           FROM   ret_codigo_retorno
          WHERE cod_retorno = ret_respuesta.r_cod_ret

          IF ret_respuesta.r_mensaje_cod_ret IS NULL THEN
             LET ret_respuesta.r_mensaje_cod_ret = "No cargados"
          END IF 
      END IF
   ELSE
       DISPLAY "se marca la cuenta"
         INITIALIZE  v_id_solicitud TO NULL 
         
         DISPLAY g_id_derechohabiente
       SELECT id_solicitud
         INTO v_id_solicitud
         FROM ret_ley73
        WHERE id_derechohabiente = g_id_derechohabiente
          AND estado_solicitud IN (10) 

       -- se inician las variables para marca
       LET v_marca_Ley73          = 803;    -- marca para Ley 73
       LET v_i_estado_marca       = 0; 
  
       LET v_i_estado_marca = 0;
       IF v_id_solicitud IS NULL THEN
           DISPLAY "no existen solicitudes"
           LET ret_respuesta.r_g_nss = g_nss
           LET ret_respuesta.r_cod_ret = 999
 
          IF ret_respuesta.r_cod_ret IS NULL THEN
             LET ret_respuesta.r_mensaje_cod_ret = "Codigo no existente"
          ELSE  
            SELECT desc_larga 
              INTO ret_respuesta.r_mensaje_cod_ret
              FROM   ret_codigo_retorno
             WHERE cod_retorno = ret_respuesta.r_cod_ret

             IF ret_respuesta.r_mensaje_cod_ret IS NULL THEN
                LET ret_respuesta.r_mensaje_cod_ret = "No cargados"
             END IF 
           END IF
       ELSE  
           LET v_script = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
       
           PREPARE pr_marca FROM  v_script
           EXECUTE pr_marca USING g_id_derechohabiente
                                 ,v_marca_Ley73               -- marca de Retiro Ley 73 
                                 ,v_id_solicitud
                                 ,"0"                         -- folio se asignara en la solicitd
                                 ,"0"                         -- estado marca
                                 ,"0"                         -- codigo de rechazo
                                 ,"0"                         -- marca de la causa
                                 ,"TODAY"                     -- fecha de la causa
                                 ,p_cod_user
                                 ,"1506"
                            INTO v_i_estado_marca
        END IF 
      DISPLAY "v_i_estado_marca"
      DISPLAY v_i_estado_marca

      LET ret_respuesta.r_g_nss = g_nss
      LET ret_respuesta.r_cod_ret = 0

      IF ret_respuesta.r_cod_ret IS NULL THEN
        LET ret_respuesta.r_mensaje_cod_ret = "Codigo no existente"
      ELSE  
         SELECT desc_larga 
           INTO ret_respuesta.r_mensaje_cod_ret
           FROM   ret_codigo_retorno
          WHERE cod_retorno = ret_respuesta.r_cod_ret

          IF ret_respuesta.r_mensaje_cod_ret IS NULL THEN
             LET ret_respuesta.r_mensaje_cod_ret = "No cargados"
          END IF 
      END IF
      
      --LET ret_respuesta.r_g_nss = g_nss
   END IF 
END FUNCTION 

FUNCTION fn_marcando_nss()
DEFINE v_marca SMALLINT 
#se marca la cuenta y se valida qe se haya generado correctamente
LET v_marca = 0

  SELECT count(id_derechohabiente)
    INTO  v_marca
    FROM sfr_marca_activa
   WHERE marca = 803
     AND id_derechohabiente = g_id_derechohabiente 
      
  RETURN v_marca
END FUNCTION 
