
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => ret                                                     #
#Programa          => RETWE08                                                 #
#Objetivo          => ws desmarca de tramite judicial                         #
#Fecha Inicio      => 06-Jun-2012                                             #
###############################################################################

--disparador de ws desmarca ley 73 
--httpdispatch -f as_safreviv_ws7.xcf
--port  9192

IMPORT FGL WSHelper
IMPORT com
IMPORT XML 

DATABASE safre_viv
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
GLOBALS
  DEFINE 
    ret_generacion_marca RECORD
      g_nss            CHAR(11),     -- NSS DERECHOHABIENTE
      g_grupo_tr       SMALLINT,     -- GRUPO DE TRABAJADOR
      g_f_marca        DATE    ,     -- FECHA DEdesmarca
      g_clave_afore    SMALLINT ,    -- CLAVE DE AFORE
      g_causa_desmarca SMALLINT      -- Causa de la desmarca
       
    END RECORD,

    ret_respuesta RECORD
      r_g_nss           CHAR(11)     ,--Número de seguridad social del trabajador
      r_cod_ret         SMALLINT     ,--Código de retorno   Según cátalogo a dos posiciones
      r_mensaje_cod_ret CHAR(100)     --Mensaje de código de retorno
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
       ,g_causa_desmarca     SMALLINT


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
  --fglrun RETWE08
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
      display_status(" Desmarca Ley 73 Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create  Desmarca Ley 73
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
    LET serv = com.WebService.CreateWebService("BajaMarca",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_rechazo 

    LET op = com.WebOperation.CreateRPCStyle("fn_baja_marca","fn_baja_marca",ret_generacion_marca,ret_respuesta)
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
      display_status(" Desmarca Ley 73 registered")
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

FUNCTION fn_baja_marca()
       
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
   LET g_causa_desmarca = ret_generacion_marca.g_causa_desmarca
   
   CALL fn_desmarcas()   
END FUNCTION

FUNCTION fn_desmarcas()
DEFINE v_estado               SMALLINT
DEFINE v_i_estado_desmarca    INTEGER
DEFINE v_desmarca_ley73       INTEGER
DEFINE v_script               STRING 
DEFINE v_id_solicitud         DECIMAL(9,0)
DEFINE p_cod_user             char(20)
DEFINE v_proceso_cod          SMALLINT 

LET p_cod_user    = 'safreviv' 
LET v_proceso_cod = g_proceso_cod_ret_ley73_ws

     SELECT id_derechohabiente
         INTO  g_id_derechohabiente
         FROM afi_derechohabiente 
         WHERE nss = g_nss
  
   CALL fn_desmarcando_nss() RETURNING v_estado   
   IF  v_estado <=0 THEN
       --DISPLAY "no se desmarca la cuenta ya esta desmarcada"
       CALL fn_aviso_marca(666)
   ELSE
       --DISPLAY "se desmarca la cuenta"
         LET v_id_solicitud = 0
         
       SELECT NVL(id_solicitud,0)  
         INTO v_id_solicitud  
         FROM ret_ley73
        WHERE id_derechohabiente = g_id_derechohabiente
          AND estado_solicitud IN (10) 

          IF v_id_solicitud = 0 THEN
              --DISPLAY "La solicitud no cuenta con estatus necesario para realizar la operacion" 
              CALL fn_aviso_marca(888)
          END IF 
       -- se inician las variables para desmarca
       LET v_desmarca_ley73     = 803; -- desmarca para Ley 73
       LET v_i_estado_desmarca  = 0  ; 
  
       LET v_i_estado_desmarca  = 0  ;
       LET v_script = "EXECUTE FUNCTION sp_status_retiro_ley73_autorizacion(?,?,?,?,?)"
       
       PREPARE pr_desmarca FROM  v_script
       EXECUTE pr_desmarca USING v_id_solicitud
                                ,g_causa_desmarca --v_causa_rec   SMALLINT,  --causa de rechazo
                                ,"100" --v_estatus     SMALLINT,  --estatus de solicitud
                                ,p_cod_user --v_usuario_cod VARCHAR(20),
                                ,"D" --v_mod_marca   CHAR(1)
                                       
       --LET v_script = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
       --PREPARE pr_desmarca FROM  v_script
       --EXECUTE pr_desmarca USING 
                                --g_id_derechohabiente
                               --,v_desmarca_ley73         -- desmarca de solo infonavit
                               --,v_id_solicitud           -- identificador de registro de archivo o lote
                               --,"0"                      -- estado marca
                               --,"0"                      -- marca de la causa
                               --,p_cod_user               -- se le coloco el usuario con los usos
                               --,v_proceso_cod           --proceso_cod 
                        --INTO v_i_estado_desmarca
      
      CALL fn_aviso_marca(0)      
      
   END IF 
END FUNCTION 

FUNCTION fn_desmarcando_nss()
DEFINE v_desmarca SMALLINT 
#se desmarca la cuenta y se valida qe se haya generado correctamente
LET v_desmarca = 0

  SELECT count(id_derechohabiente)
    INTO  v_desmarca
    FROM sfr_marca_activa
   WHERE marca= 803
     AND id_derechohabiente = g_id_derechohabiente 
      
  RETURN v_desmarca
END FUNCTION 

FUNCTION fn_aviso_marca(v_cod_rechazo)
  DEFINE v_cod_rechazo SMALLINT  

       LET ret_respuesta.r_g_nss   = g_nss
       LET ret_respuesta.r_cod_ret = v_cod_rechazo

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
END FUNCTION