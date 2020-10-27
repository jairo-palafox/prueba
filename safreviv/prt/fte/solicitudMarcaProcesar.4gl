IMPORT com
IMPORT util

TYPE rechazos DYNAMIC ARRAY OF RECORD
      cod_rechazo          VARCHAR(6),
      desc_rechazo         VARCHAR(100)
   END RECORD
   
DEFINE mensajeEntradaSolicitud RECORD
          idssn RECORD
             idSistema    DECIMAL(32,0),
             idEbusiness  DECIMAL(32,0),
             idPortafolio DECIMAL(32,0),
             idServicio   DECIMAL(32,0),
             idCliente    DECIMAL(32,0),
             idCanal      DECIMAL(32,0),
             codoperCliente VARCHAR(50),
             fecha        DATETIME YEAR TO FRACTION(5)
          END RECORD,
          solicitudMarca RECORD
             folioCliente    VARCHAR(50),
             origenSolicitud CHAR(3),
             tipoSolicitud   CHAR(2),
             nss             CHAR(11),
             apellidoPaterno VARCHAR(40),
             apellidoMaterno VARCHAR(40),
             nombre          VARCHAR(40),
             curp            CHAR(18)
          END RECORD
       END RECORD,
       mensajeSalidaSolicitud RECORD
          folioProcesar  VARCHAR(50),
          codOper        CHAR(2),
          codResp        CHAR(2),
          codRespOpr     CHAR(2),
          descResp       VARCHAR(254),
          rechazo        rechazos,
          codOperCliente VARCHAR(50),
          tiempoResp     STRING,     
          fecha          DATETIME YEAR TO FRACTION(5)
       END RECORD
DEFINE numero INTEGER
       
MAIN
DEFINE ret INTEGER

   CALL fn_crea_servicio()

   # Puerto 8077
   CALL com.WebServiceEngine.Start()
   DISPLAY "Inicia WS SERVIDOR recepción MARCA Dummy PROCESAR"

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

   LET serv = com.WebService.CreateWebService("recibeSolicitudMarca","http:www.procesar.com/recibe/marca")  
   LET op = com.WebOperation.CreateDOCStyle("recibirSolicitudMarca", 
                                            "recibeMarcaInfonavit", 
                                            mensajeEntradaSolicitud, 
                                            mensajeSalidaSolicitud)

   CALL serv.publishOperation(op,"recibeSolicitudMarcaInfonavit")
   CALL com.WebServiceEngine.RegisterService(serv)


END FUNCTION

FUNCTION recibirSolicitudMarca()

   DISPLAY "Folio cliente:",mensajeEntradaSolicitud.solicitudMarca.folioCliente
   
   CALL util.Math.rand(100000) RETURNING numero
   
   LET mensajeSalidaSolicitud.folioProcesar = numero
   LET mensajeSalidaSolicitud.codOper    = numero
   LET mensajeSalidaSolicitud.codResp    = "OK"
   LET mensajeSalidaSolicitud.codRespOpr = "AC"
   LET mensajeSalidaSolicitud.descResp   = ""
   -- Sin rechzasos
   --LET mensajeSalidaSolicitud.rechazo.cod_rechazo
   --LET mensajeSalidaSolicitud.rechazo.desc_rechazo
   LET mensajeSalidaSolicitud.codOperCliente = mensajeEntradaSolicitud.idssn.codoperCliente
   LET mensajeSalidaSolicitud.tiempoResp     = "0.0001"
   LET mensajeSalidaSolicitud.fecha          = CURRENT YEAR TO FRACTION(5)
   
END FUNCTION