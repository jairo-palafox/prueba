IMPORT com

DEFINE v_sumandos RECORD
          v_sumando1 DECIMAL(12,2),
          v_sumando2 DECIMAL(12,2)
       END RECORD,
       v_resultado RECORD
          v_resultado DECIMAL(12,2)
       END RECORD

MAIN
DEFINE ret INTEGER

   CALL createservice()

   CALL com.WebServiceEngine.Start()

   WHILE TRUE
    # Process each incoming requests (infinite loop)
    LET ret = com.WebServiceEngine.ProcessServices(-1)
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
      DISPLAY "Error"
      LET int_flag=0
      EXIT WHILE
    END IF 
  END WHILE

  DISPLAY "Server stopped"

END MAIN

FUNCTION createservice()
    DEFINE serv  com.WebService    # A WebService
    DEFINE op    com.WebOperation  # Operation of a WebService

   --Create WebService object
   LET serv = com.WebService.CreateWebService("ws_prueba_monitor","http://efp.com.mx/monitor")

   --Create WebOperation object
   LET op = com.WebOperation.CreateRPCStyle("suma", "suma", v_sumandos, v_resultado)

   --Publish the operation, associating it with the WebService object
   CALL serv.publishOperation(op,NULL)

   CALL com.WebServiceEngine.RegisterService(serv)

END FUNCTION

FUNCTION suma()
    LET v_resultado.v_resultado = v_sumandos.v_sumando1 + v_sumandos.v_sumando2 
END FUNCTION