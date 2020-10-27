--------------------------------------------------------------------------------------------------------------
-- Modificación  => WS Carga registro cambia Vit a tabla de detalle
-- Fehca         => 29 de Agosto de 2018.
-- Autor         => 
-- Requerimiento => SISXVIII-16
-- Clave cambio  => sisxviii-16
-- Descripción   => Cambiar tamaño del cambpo marca_cambio_casa de 2 a 4
--============================================================================================================

	IMPORT com    # imports the Genero Web Services Extension package
	IMPORT XML


    GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
      
        
	GLOBALS
          CONSTANT 
            g_proceso_reg_pag_svt       SMALLINT = 1416       

		DEFINE 
			GarantiaPrendaria	RECORD 
				pag_det_nss                CHAR(11),
				pag_det_curp               CHAR(18),
				pag_det_credito_infonavit  DECIMAL(10,0),
				pag_det_cod_identificacion CHAR(10),
				pag_det_marca_cambio_casa  CHAR(4),        --sisxviii-16
				pag_det_numero_caso        DECIMAL(10,0),
				pag_det_f_pago             DATE,
				pag_det_monto_deposito     DECIMAL(15,2)
			END RECORD, 
		 
			GarantiaPrendariaResponse	RECORD
				result_operacion   CHAR(02),
				ind_registro       SMALLINT,
				nss_consultado     CHAR(11)
			END RECORD,

           p_nss                CHAR(11),
           p_curp               CHAR(18),
           p_credito_infonavit  DECIMAL(10,0),
           p_cod_identificacion CHAR(10),
           p_marca_cambio_casa  CHAR(4),          --sisxviii-16
           p_numero_caso        DECIMAL(10,0),
           p_f_pago             DATE,
           p_monto_deposito     DECIMAL(15,2),
           v_result_operacion   CHAR(02),
           v_rechazo            SMALLINT,
           p_pid                DECIMAL(9,0)
 

        
			
	END GLOBALS 

    
    DEFINE serverURL STRING
    DEFINE screen    BOOLEAN   
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
	   DEFINE create_status INTEGER
	   
	   DEFER INTERRUPT

        LET screen = false
  #
  # Check arguments
  #
  IF num_args() = 2 and arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateRegistroDePagosCVit(true)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 and arg_val(1) = "-S" THEN
      LET screen = true
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "server" ATTRIBUTES(TEXT = "CreateRegistroDePagosCVit", STYLE="naked")
      display_status("CreateRegistroDePagosCVit  startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM    
      END IF
    END IF
  END IF
  
  #
  # Create  service
  #
  DISPLAY "CreateRegistroDePagosCVit"
  
  CALL CreateRegistroDePagosCVit(false)
    
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

  IF screen THEN
    MENU
      ON idle 1
        LET create_status = com.WebServiceEngine.ProcessServices(1)
        CASE create_status
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
      ON ACTION close
        EXIT program
    END MENU
  ELSE
    WHILE TRUE
      LET create_status = com.WebServiceEngine.ProcessServices(-1)
      CASE create_status 
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
      IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
      END IF     
    END WHILE
    DISPLAY "Server stopped"
  END IF
END MAIN

#
# Create CreateRegistroDePagosCVit()
#
FUNCTION CreateRegistroDePagosCVit(generateWSDL)
	  DEFINE serv         com.WebService       # WebService
	  DEFINE serviceNS    STRING
	  DEFINE generateWSDL SMALLINT
	  DEFINE ret          INTEGER
      DEFINE operation    com.WebOperation
                         -- Ambito de servicio del docu "ficha de servicio web"
	  LET serviceNS       = "http://service.infonavit.org.mx"

	  TRY
	  
		#
		# Create CreateRegistroDePagosCVit Web Service
		#
		LET serv = com.WebService.CreateWebService("RegistroDePagosCVit",serviceNS)
		--CALL serv.setFeature("Soap1.1",TRUE)
		--CALL serv.setFeature("Soap1.2",TRUE)
	  
		
		
		#
		# Publish the functions
		#
       
        LET operation =com.WebOperation.CreateDOCStyle ("GarantiaPrendaria","GarantiaPrendaria", GarantiaPrendaria, GarantiaPrendariaResponse)
        CALL serv.publishOperation(operation,"GarantiaPrendaria")
		
		
	  
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
		  display_status("RegistroDePagosCVit  registered")
		END IF
		
	  CATCH
		display_status("Unable to create 'RegistroDePagosCVit' Web Service :"||STATUS)
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
	
FUNCTION GarantiaPrendaria ()
        
     LET p_nss                = GarantiaPrendaria.pag_det_nss
     LET p_curp               = GarantiaPrendaria.pag_det_curp
     LET p_credito_infonavit  = GarantiaPrendaria.pag_det_credito_infonavit
     LET p_cod_identificacion = GarantiaPrendaria.pag_det_cod_identificacion
     LET p_marca_cambio_casa  = GarantiaPrendaria.pag_det_marca_cambio_casa
     LET p_numero_caso        = GarantiaPrendaria.pag_det_numero_caso
     LET p_f_pago             = GarantiaPrendaria.pag_det_f_pago
     LET p_monto_deposito     = GarantiaPrendaria.pag_det_monto_deposito  
  
     CALL fn_carga_pago()
--   DISPLAY "v_result_operacion, rechazo y nss: ",v_result_operacion," ",v_rechazo," ",p_nss
    
     LET GarantiaPrendariaResponse.nss_consultado = p_nss
     LET GarantiaPrendariaResponse.result_operacion = v_result_operacion
     LET GarantiaPrendariaResponse.ind_registro = v_rechazo
 
END FUNCTION 



FUNCTION fn_carga_pago()
   
   DEFINE 
     
      v_folio              DECIMAL(9,0),
      v_id_derecho         DECIMAL(9,0)
      

   -- Valida operacion

   -- Genera Folio
   LET v_folio = 0 
       
   -- se genera el pid para el proceso
  

   -- Inicia operacion de integración.
   
   -- Finaliza iniciación de integración.

   LET v_rechazo = 0
   
   IF p_credito_infonavit IS NULL OR p_credito_infonavit = 0 THEN
      LET v_rechazo = 3
      
   END IF
   IF p_f_pago IS NULL THEN
      LET v_rechazo = 7
      
   END IF
   IF p_monto_deposito = 0 OR p_monto_deposito IS NULL THEN
      LET v_rechazo = 8
      
   END IF

   LET v_id_derecho = NULL;

   SELECT FIRST 1 id_derechohabiente
   INTO   v_id_derecho
   FROM   afi_derechohabiente
   WHERE  nss = p_nss

   IF v_id_derecho IS NULL OR v_rechazo <> 0 THEN
      LET v_id_derecho = 0 ;
      LET v_result_operacion    = "02";  -- codigo de rechazo
   ELSE
      LET v_result_operacion    = "01";  -- codigo de aceptado
   END IF

   INSERT INTO pag_det_cvt(
      folio,
      id_referencia,
      id_derechohabiente,
      curp,
      credito_infonavit,
      cod_identificacion,
      marca_cambio_casa,
      num_caso,
      f_pago,
      monto_deposito,
      f_registro,
      ind_registro,
      result_operacion)
   VALUES (
      v_folio,
      1,
      v_id_derecho,
      p_curp,
      p_credito_infonavit,
      p_cod_identificacion,
      p_marca_cambio_casa,
      p_numero_caso,
      p_f_pago,
      p_monto_deposito,
      TODAY,
      v_rechazo,
      v_result_operacion);

   -- Se agrega sentencia de update statics a las tablas temporales
   UPDATE STATISTICS FOR TABLE pag_det_cvt; 

   --Se registra el FIN DE LA OPERACION COMO EXITOSA
  

END FUNCTION
 

