
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

   
END GLOBALS

DEFINE serverURL STRING
DEFINE screen    BOOLEAN 

MAIN
    DEFINE ret INTEGER

    CALL CreateNssService()
  
    DISPLAY "Starting server..."   

    CALL com.WebServiceEngine.Start()

    DISPLAY "The server is listening."

    WHILE TRUE
      # Process each incoming requests (infinite loop)
      LET ret = com.WebServiceEngine.ProcessServices(-1)

      DISPLAY "Processing request..."

      CASE ret
          WHEN 0
              DISPLAY "Request processed." 
          WHEN -1
              DISPLAY "Timeout reached."
          WHEN -2
              DISPLAY "Disconnected from application server."
              EXIT PROGRAM 
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
END MAIN

# Create Nss RPC/Literal service
#
FUNCTION CreateNssService()
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  

    
  LET serv = com.WebService.CreateWebService("ConsultaNSSAfiliado","http://www.infonavit.org.mx/")
  CALL serv.setFeature("Soap1.1",TRUE)

  LET op = com.WebOperation.CreateDOCStyle("fn_consulta_nss","ConsultaNSSAfiliado",nss_in,datos_out)
  CALL serv.publishOperation(op,NULL)

  CALL com.WebServiceEngine.RegisterService(serv) 

  

  
   
END FUNCTION

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
DEFINE v_valor           CHAR(01)
    -- inicio de variables
    -- asignacion de datos
    LET v_nss = nss_in.nss
   
    -- se busca datos del afiliado
    LET v_sql = "SELECT DISTINCT a.curp,a.nombre_af,ap_paterno_af,ap_materno_af, a.f_nacimiento, a.sexo FROM afi_derechohabiente a ",
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
   
    LET v_valor = v_curp[11]
    
    CASE v_valor
        WHEN "H"
           LET v_desc_sexo = 'MASCULINO' CLIPPED
        WHEN "M"
           LET v_desc_sexo = 'FEMENINO' CLIPPED
        OTHERWISE
            LET v_desc_sexo = 'SIN INDICADOR MIGRADO' CLIPPED
    END CASE
   
    -- se verfica si existe informacion
    IF SQLCA.SQLERRD[3] = 0 THEN
       LET v_resul_op = '02' -- no existe informacion a procesar
    ELSE
       LET v_resul_op = '01'
    END IF
    
   
   
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
