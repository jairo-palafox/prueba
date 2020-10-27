
IMPORT com

#
# USER GLOBALS VARIABLES
#
GLOBALS "AFIWS22.inc"

DATABASE safre_viv
GLOBALS
  DEFINE
    nss_in RECORD
      nss string
     
    END RECORD,
    datos_out RECORD
      curp           STRING,
      nombre         CHAR(40),
      apaterno       CHAR(40),
      amaterno       CHAR(40),
      f_nacimiento   CHAR(10),
      sexo           CHAR(30),
      resul_op       CHAR(02)
    END RECORD
     
  DEFINE
    divide_by_zero String ATTRIBUTES (XMLName="DividedByZero")
   
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
  DEFINE ret INTEGER  
     
  DEFER INTERRUPT

  LET screen = false
  #
  # Check arguments
  #
  IF num_args() = 2 and arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateNssService(true)
      EXIT PROGRAM
  ELSE
    IF num_args() = 2 and arg_val(1) = "-S" THEN
      LET screen = true
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "server" ATTRIBUTES(TEXT = "Nss service", STYLE="naked")
      display_status("Nss service startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM    
      END IF
    END IF
  END IF
 
  #
  # Create Nss service
  #
  CALL CreateNssService(false)
   
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
      ON ACTION close
        EXIT program
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
      IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
      END IF    
    END WHILE
    DISPLAY "Server stopped"
  END IF
END MAIN

#
# Create Nss RPC/Literal service
#
FUNCTION CreateNssService(generateWSDL)
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  DEFINE serviceNS    STRING
  DEFINE generateWSDL SMALLINT
  DEFINE ret          INTEGER

  LET serviceNS       = "http://www.infonavit.org.mx/"

  TRY
 
    # http://10.90.8.199/ConsultaNSSAfiliado?wsdl
    # Create Nss Web Service
    #
    LET serv = com.WebService.CreateWebService("ConsultaNSSAfiliado",serviceNS)
    CALL serv.setFeature("Soap1.1",TRUE)
 
    #
    # Create a Soap Fault
    #
    CALL serv.createFault(divide_by_zero,FALSE)
   
    #
    # Publish the functions
    #
   
    # Nss
    LET op = com.WebOperation.CreateDOCStyle("fn_consulta_nss","ConsultaNSSAfiliado",nss_in,datos_out)
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
      display_status("Nss Service registered")
    END IF
   
  CATCH
    display_status("Unable to create 'Nss' Web Service :"||STATUS)
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
-- funcion que consulta un NSS y regresa los datos Base CURP
FUNCTION fn_consulta_nss()
DEFINE v_sql             VARCHAR(1000)
DEFINE v_curp            VARCHAR(18)
DEFINE v_f_nacimiento    DATE
DEFINE v_caracter_fecha  CHAR(10)
DEFINE v_sexo            CHAR(50)
DEFINE v_nss             CHAR(11)
DEFINE v_nombre          CHAR(40)
DEFINE v_apaterno        CHAR(40)
DEFINE v_amaterno        CHAR(40)
DEFINE v_desc_sexo       CHAR(30)
DEFINE v_resul_op        CHAR(02)

    -- inicio de variables
    -- asignacion de datos
    LET v_nss = nss_in.nss
   
    -- se busca datos del afiliado
    LET v_sql = "SELECT DISTINCT a.curp,a.nombre_af,ap_paterno_af,ap_materno_af, a.f_nacimiento, a.sexo FROM afi_derechohabiente a INNER JOIN cat_genero g",
                "\n ON a.sexo  = g.sexo                                  ",
                "\n WHERE a.nss = ?                                      ",
                "\n and a.ind_estado_cuenta = 0                          " --activo
               
    DISPLAY "Consulta Formada:", v_sql
   
    PREPARE pre_obt_datos FROM v_sql

    EXECUTE pre_obt_datos USING v_nss
                          INTO v_curp   ,
                               v_nombre,
                               v_apaterno,
                               v_amaterno,
                               v_f_nacimiento,
                               v_sexo


    -- se verfica si existe informacion
    IF SQLCA.SQLERRD[3] = 0 THEN
       LET v_resul_op = '02' -- no existe informacion a procesar
    ELSE
       LET v_resul_op = '01'
    END IF
    
    -- se realiza conversión de catálogo
    CASE (v_sexo)
     WHEN 1
        LET v_desc_sexo = 'MASCULINO'    
     WHEN 2
        LET v_desc_sexo = 'FEMENINO'
     OTHERWISE
        LET v_desc_sexo = 'SIN INDICADOR MIGRAD'
    END CASE
   
    -- se le da formato a la fecha de nacimiento
    LET v_caracter_fecha       = v_f_nacimiento USING "dd/mm/yyyy"
    --LET nss_in.nss = "NSS recibido"
    LET datos_out.curp         = v_curp CLIPPED
    LET datos_out.nombre       = v_nombre CLIPPED
    LET datos_out.apaterno     = v_apaterno CLIPPED
    LET datos_out.amaterno     = v_amaterno CLIPPED
    LET datos_out.f_nacimiento = v_caracter_fecha CLIPPED
    LET datos_out.sexo         = v_desc_sexo CLIPPED
    LET datos_out.resul_op     = v_resul_op CLIPPED

    

END FUNCTION
