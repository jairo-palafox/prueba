--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWE04                                                  #
#OBJETIVO          => WS SOLICITUD DE RESOLUCION ADAI  FONDO AHORRO           #
#FECHA INICIO      => 03-MAY-2012                                             #
###############################################################################

--disparador de ws solicitud de retiro viv7292
--/opt/fourjs/2.32/gas/. ./envas
--httpdispatch -f as_safreviv_ws2.xcf
--port 9187

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
GLOBALS
    --servicio de consulta al ws
    --############################################
  DEFINE 
    ret_resol_adai RECORD
       g_nss            CHAR(11),
       g_caso_adai      SMALLINT,
       g_autoriza_pago  SMALLINT 
    END RECORD,

    ret_respuesta    RECORD
      r_g_nss          CHAR(11),       -- Número de seguridad social del trabajador
      r_g_res_op       SMALLINT,       -- Respuesta de operación.  Aceptado / Rechazado
      r_g_cod_rechazo  SMALLINT        -- Código de rechazo       
    END RECORD

    DEFINE g_nss                CHAR(11)
    DEFINE g_caso_adai          SMALLINT
    DEFINE g_autoriza_pago      SMALLINT
    DEFINE g_id_derechohabiente DECIMAL(9,0)
    DEFINE g_id_fondo72         DECIMAL(9,0)
    DEFINE g_id_solicitud       DECIMAL(9,0)
    DEFINE g_proceso_cod        SMALLINT  
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
  --fglrun RETW02 -S 
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateRetiroService(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET SCREEN = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "RETWE040" ATTRIBUTES(TEXT = "Resolucion Adai service", STYLE="naked")
      display_status("Retiro Service Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create Retiro service
  #
  CALL CreateRetiroService(FALSE)
  #
  # Start the server
  #
  display_status("Starting server...")
  #
  # Starts the server on the port number specified by the FGLAPPSERVER environment variable
  #  (EX: FGLAPPSERVER=8090)
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
# Create Retiro RPC/Literal service
#
FUNCTION CreateRetiroService(generateWSDL)
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  DEFINE serviceNS    STRING
  DEFINE generateWSDL SMALLINT
  DEFINE ret          INTEGER

  LET serviceNS       = "http://localhost/"

  TRY
    #
    # Create retiro Web Service
    #
    LET serv = com.WebService.CreateWebService("ResolAdai",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_resol_adai 
    LET op = com.WebOperation.CreateRPCStyle("fn_resol_adai","fn_resol_adai",ret_resol_adai,ret_respuesta)
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
      display_status("Resolucion Adai Service registered")
    END IF
    
  CATCH
    display_status("Unable to create 'Resolacion Adai' Web Service :"||STATUS)
    EXIT PROGRAM
  END TRY
END FUNCTION

FUNCTION exitHelp()
  --DISPLAY "Usage: "
  DISPLAY "Uso: "
  DISPLAY "  ", arg_val(0)
  --DISPLAY "    Start the server on port defined by FGLAPPSERVER"
  DISPLAY "    Inicar el servidor en el puerto definido en FGLAPPSERVER"
  DISPLAY "  ", arg_val(0), " -W serverurl"
  --DISPLAY "    Generate the WSDL file for the given url"
  DISPLAY "    Genera el archivo WSDL para la URL dada"
  DISPLAY "  ", arg_val(0), " -S port"
  --DISPLAY "    Start service in graphical mode and on given port"
  DISPLAY "    Iniciar el serviciuo en modo grafico y en el puerto dado"
  EXIT PROGRAM
END FUNCTION

#
# USER PUBLIC FUNCTIONS
#

FUNCTION fn_resol_adai()
   INITIALIZE g_nss TO NULL
   INITIALIZE g_autoriza_pago TO NULL     
   INITIALIZE g_caso_adai TO NULL
   
   LET g_nss           = ret_resol_adai.g_nss
   LET g_autoriza_pago = ret_resol_adai.g_autoriza_pago
   LET g_caso_adai     = ret_resol_adai.g_caso_adai  
   LET g_proceso_cod   = g_proceso_cod_ret_fondo_ahorro
   
   CALL fn_valida_resolucion()
END FUNCTION

FUNCTION fn_valida_resolucion()
DEFINE v_bnd_for   SMALLINT 
      LET v_bnd_for = 0

   #"Se recupera el identificador de afi_fondo72
   DECLARE cur_afi_fondo CURSOR FOR SELECT det.id_afi_fondo72, det.id_solicitud
                                      FROM ret_det_fondo72  det, 
                                           afi_fondo72      afi,
                                           ret_fondo_ahorro ret
                                       WHERE afi.id_afi_fondo72 = det.id_afi_fondo72
                                         AND det.id_solicitud   = ret.id_solicitud
                                         AND estado_solicitud   = 10
                                         AND estado_detalle     = 1  
                                         AND nss                = g_nss
                                          
   #se agrega foreach ya que se peden encontrar varios nss repetidos medida tomada hasta resolucion de uso de rfc                                          
   FOREACH cur_afi_fondo INTO g_id_fondo72, g_id_solicitud --ERV falta checar si esto es definitivo      
      LET v_bnd_for = 1
      SELECT id_derechohabiente 
        INTO g_id_derechohabiente
        FROM afi_fondo72
        WHERE id_afi_fondo72 = g_id_fondo72

     IF fn_valida_caso_adai() > 0 THEN
        --DISPLAY "fn_valida_caso_adai"
        IF fn_valida_estado_solcitud() THEN 
          --DISPLAY "fn_valida_estado_solcitud"
          IF g_autoriza_pago THEN
             --DISPLAY "autoriza"
             CALL fn_cambio_estado("A") --Aprobado Pagar
          ELSE
             --DISPLAY "Rechaza"
             CALL fn_cambio_estado("R") --Rechazado no Pagar
          END IF
        ELSE 
          --DISPLAY "Rechazado caso ya resuelto"
          CALL fn_cambio_estado("X")    --Rechazado caso ya resuelto 
        END IF
     ELSE 
       --DISPLAY "Rechazado no existe caso"
       CALL fn_cambio_estado("X")    --Rechazado no existe caso
     END IF  
  END FOREACH 

  IF v_bnd_for = 0 THEN
         LET ret_respuesta.r_g_nss         = g_nss    
         LET ret_respuesta.r_g_res_op      = 1
         LET ret_respuesta.r_g_cod_rechazo = 666 -- nss incorrecto  
  END IF 
END FUNCTION

FUNCTION  fn_valida_estado_solcitud()
DEFINE v_count_estado_solicitud SMALLINT 

  SELECT COUNT(*) 
    INTO v_count_estado_solicitud
    FROM ret_fondo_ahorro 
    WHERE caso_adai          = g_caso_adai
      AND id_solicitud       = g_id_solicitud  
      AND estado_solicitud   = 10
      
RETURN v_count_estado_solicitud
END FUNCTION  

FUNCTION fn_cambio_estado(v_estado)
  DEFINE v_estado              CHAR(1) 
  DEFINE v_id_solicitud        DECIMAL(9,0)  
  --DEFINE v_s_query             STRING 
  DEFINE v_i_estado_marca      INTEGER
  DEFINE v_marca_fondo_ahorro  INTEGER 
  

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro   = 802; -- marca para fondo ahorro
   LET v_i_estado_marca       = 0;

  SELECT id_solicitud 
    INTO v_id_solicitud
    FROM ret_fondo_ahorro 
   WHERE caso_adai = g_caso_adai
     AND id_solicitud = g_id_solicitud
     AND estado_solicitud= 10
      
  IF v_estado = "A" THEN 
    UPDATE  ret_fondo_ahorro SET estado_solicitud   = 15
                           WHERE id_solicitud       = v_id_solicitud
                             AND estado_solicitud   = 10
                                       
    LET ret_respuesta.r_g_nss         = g_nss    
    LET ret_respuesta.r_g_res_op      = 1
    LET ret_respuesta.r_g_cod_rechazo = 0     
  END IF 
  
  IF v_estado = "R" THEN 
    UPDATE  ret_fondo_ahorro SET estado_solicitud   = 100,
                                 cod_rechazo        = 80
                           WHERE id_solicitud       = v_id_solicitud
                             AND estado_solicitud   = 10
                                       
    LET ret_respuesta.r_g_nss         = g_nss    
    LET ret_respuesta.r_g_res_op      = 2
    LET ret_respuesta.r_g_cod_rechazo = 80 --no pagar 

    
         PREPARE pr_cambio_estatus FROM "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
           EXECUTE  pr_cambio_estatus USING  g_id_derechohabiente
                                            ,v_marca_fondo_ahorro -- desmarca de fondo ahorro
                                            ,v_id_solicitud -- identificador de registro de archivo o lote
                                            ,"40" -- estado marca
                                            ,"0"  -- marca de la caus a
                                            ,"USER"
                                            ,g_proceso_cod 
               
          INTO v_i_estado_marca; 
    
  END IF 

   IF v_estado = "X" THEN
      LET ret_respuesta.r_g_nss         = g_nss    
      LET ret_respuesta.r_g_res_op      = 2
      LET ret_respuesta.r_g_cod_rechazo = 999    --caso rechazado varias causas
   END IF 
   
END FUNCTION 

FUNCTION fn_valida_caso_adai()
DEFINE v_count_caso_adai SMALLINT 

SELECT COUNT(*)
  INTO v_count_caso_adai 
  FROM ret_fondo_ahorro
 WHERE caso_adai = g_caso_adai
   AND id_solicitud = g_id_solicitud 

RETURN v_count_caso_adai
END FUNCTION 