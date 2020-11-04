IMPORT com
GLOBALS "AEXWS04.inc"

#
# USER GLOBALS VARIABLES
#
DATABASE safre_viv
GLOBALS
  DEFINE
    solicitud RECORD
      nss string
     
    END RECORD,
    datos_out RECORD
      nss           string,
      resul_op      CHAR(02)
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

    CALL CreateIndicadoresService()
  
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

#
# Create Nss RPC/Literal service
#


FUNCTION CreateIndicadoresService()
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  

    
  LET serv = com.WebService.CreateWebService("InsertaIndicadores","http://www.infonavit.org.mx/")

  LET op = com.WebOperation.CreateDOCStyle("fn_inserta_indicador","InsertaIndicadores",solicitud,datos_out)
  CALL serv.publishOperation(op,NULL)

  CALL com.WebServiceEngine.RegisterService(serv) 

  
END FUNCTION

#
# USER PUBLIC FUNCTIONS
#
-- funcion que consulta un NSS y regresa los datos Base CURP
FUNCTION fn_inserta_indicador()
DEFINE v_resul_op        CHAR(02)
DEFINE v_count           INTEGER
DEFINE lref_CatIndicador RECORD LIKE aex_afi_identifica.*

    -- inicio de variables
    LET lref_CatIndicador.nss = solicitud.nss
    LET lref_CatIndicador.cod_identifica = 1
    LET lref_CatIndicador.f_actualiza = TODAY
    LET lref_CatIndicador.f_alta = TODAY

    SELECT COUNT(*) INTO v_count
    FROM aex_afi_identifica
    WHERE nss = lref_CatIndicador.nss

    IF v_count > 0 THEN

        WHENEVER ERROR CONTINUE
          UPDATE aex_afi_identifica 
          SET f_actualiza = TODAY
          WHERE nss = lref_CatIndicador.nss
             IF SQLCA.SQLCODE <> 0 THEN
                LET v_resul_op = '02'
             ELSE
                LET v_resul_op = '01'
             END IF
        WHENEVER ERROR STOP

    ELSE

        WHENEVER ERROR CONTINUE
          INSERT INTO aex_afi_identifica VALUES (lref_CatIndicador.*)
             IF SQLCA.SQLCODE <> 0 THEN
                LET v_resul_op = '02'
             ELSE
                LET v_resul_op = '01'
             END IF
        WHENEVER ERROR STOP

    END IF
   
    
    --LET nss_in.nss = "NSS recibido"
    LET datos_out.nss       = solicitud.nss
    LET datos_out.resul_op  = v_resul_op


    

END FUNCTION
