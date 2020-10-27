IMPORT com
IMPORT util
DEFINE mensajeEntradaSolicitud RECORD
          numeroCaso  VARCHAR(10)   ATTRIBUTE(XSDString,XSDMaxLength="10"),--DECIMAL(10,0) ATTRIBUTE(XSDDecimal,XSDTotalDigits="10",XSDFractionDigits="0"),
          idEstatus   CHAR(5)       ATTRIBUTE(XSDString,XSDMaxLength="5"),
          idMotivo    CHAR(3)       ATTRIBUTE(XSDString,XSDMaxLength="4")
       END RECORD,
       mensajeSalidaSolicitud RECORD
          resultadoOperacion CHAR(5) ATTRIBUTE(XSDString,XSDMaxLength="5"),
          mensaje VARCHAR(20) ATTRIBUTE(XSDString,XSDMaxLength="20")
       END RECORD
DEFINE numero SMALLINT
       
MAIN
DEFINE ret INTEGER

   CALL fn_crea_servicio()

   # Puerto 8076
   CALL com.WebServiceEngine.Start()
   DISPLAY "Inicia WS recepción MARCA"

   WHILE TRUE
      # Process each incoming requests (infinite loop)
      LET ret = com.WebServiceEngine.ProcessServices(-1)
      CASE ret
         WHEN 0
            DISPLAY "Request processed. ",CURRENT YEAR TO SECOND 
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM 
         WHEN -3
            DISPLAY "Client Connection lost."
            DISPLAY SQLCA.sqlcode
            DISPLAY SQLCA.sqlerrm
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

FUNCTION fn_crea_servicio()
DEFINE serv  com.WebService    # A WebService
DEFINE op    com.WebOperation  # Operation of a WebService

   LET serv = com.WebService.CreateWebService("recibeMarca","http:www.adai.com/recibe/marca")  
   LET op = com.WebOperation.CreateRPCStyle("recibirResultadoMarca", 
                                            "recibeMarcaCedente", 
                                            mensajeEntradaSolicitud, 
                                            mensajeSalidaSolicitud)

   CALL serv.publishOperation(op,"recibeResultadoMarcaCedente")
   CALL com.WebServiceEngine.RegisterService(serv)


END FUNCTION

FUNCTION recibirResultadoMarca()

   DISPLAY "Caso:",mensajeEntradaSolicitud.numeroCaso
   DISPLAY "Estado:",mensajeEntradaSolicitud.idEstatus
   DISPLAY "Motivo:",mensajeEntradaSolicitud.idMotivo

   LET mensajeSalidaSolicitud.resultadoOperacion = "00"
   LET mensajeSalidaSolicitud.mensaje            = "Actualización realizada correctamente"
   
END FUNCTION