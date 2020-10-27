IMPORT com
IMPORT util
DATABASE safre_viv
DEFINE mensajeEntradaSolicitud RECORD
          nss       CHAR(11),
          idCredito VARCHAR(10)
       END RECORD,
       mensajeSalidaSolicitud RECORD
          nss                     CHAR(11),
          idCredito               VARCHAR(10),
          diagnosticoCartera      CHAR(2),
          tipoCredito             CHAR(2),
          saldoInsolutoCredito    DECIMAL(10,2),
          montoOriginacion        DECIMAL(10,2),
          fechaOriginacionCredito CHAR(8),
          idMotivo                CHAR(2)
       END RECORD
DEFINE numero SMALLINT
       
MAIN
DEFINE ret INTEGER

   CALL fn_crea_servicio()

   # Puerto 8073
   CALL com.WebServiceEngine.Start()
   DISPLAY "Inicia WS consulta crédito cartera"

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

   LET serv = com.WebService.CreateWebService("consultaCreditoCartera","http:www.catera.com/consulta/credito")  
   LET op = com.WebOperation.CreateRPCStyle("consultarCreditoCartera", 
                                            "consultaCreditoCartera", 
                                            mensajeEntradaSolicitud, 
                                            mensajeSalidaSolicitud)

   CALL serv.publishOperation(op,"consultaCreditoCarteraReceptora")
   CALL com.WebServiceEngine.RegisterService(serv)


END FUNCTION

FUNCTION consultarCreditoCartera()
DEFINE v_consulta STRING,
       v_acreditado RECORD
          v_nss          CHAR(11),
          v_credito      VARCHAR(10),          
          v_saldo        DECIMAL(22,2),
          v_saldo_originacion DECIMAL(22,2),
          v_tipo_credito SMALLINT,
          v_diagnostico  CHAR(2),
          v_motivo       CHAR(2)
       END RECORD

   
   LET v_consulta = "SELECT FIRST 1 nss,
                            credito,
                            saldo,
                            saldo_originacion,
                            tipo_credito,
                            estado_credito,
                            motivo_credito 
                       FROM prt_nss_prueba_cedente 
                      WHERE nss = ?"
   PREPARE prp_cons_nss_prueba_ced FROM v_consulta
   EXECUTE prp_cons_nss_prueba_ced USING mensajeEntradaSolicitud.nss
                                    INTO v_acreditado.*
   IF( v_acreditado.v_diagnostico IS  NULL )THEN
      CALL util.Math.rand(10) RETURNING numero
      DISPLAY "Numero:",numero, " ",CURRENT YEAR TO FRACTION

      IF(numero > 6)THEN
         LET mensajeSalidaSolicitud.nss                     = mensajeEntradaSolicitud.nss
         LET mensajeSalidaSolicitud.idCredito               = 0
         LET mensajeSalidaSolicitud.diagnosticoCartera      = "02" # Improcedente
         LET mensajeSalidaSolicitud.tipoCredito             = 0
         LET mensajeSalidaSolicitud.saldoInsolutoCredito    = 0.0
         LET mensajeSalidaSolicitud.montoOriginacion        = 0.0
         LET mensajeSalidaSolicitud.fechaOriginacionCredito = TODAY USING "yyyymmdd"
         CALL util.Math.rand(3) RETURNING numero
         CASE 
            WHEN numero <= 1
               LET mensajeSalidaSolicitud.idMotivo = "01"
            WHEN numero = 2
               LET mensajeSalidaSolicitud.idMotivo = "02"
            WHEN numero = 3
               LET mensajeSalidaSolicitud.idMotivo = "05"
         END CASE
      ELSE
         LET mensajeSalidaSolicitud.nss                     = mensajeEntradaSolicitud.nss
         LET mensajeSalidaSolicitud.idCredito               = mensajeEntradaSolicitud.idCredito
         LET mensajeSalidaSolicitud.diagnosticoCartera      = "00" # Procedente
         LET mensajeSalidaSolicitud.tipoCredito             = util.Math.rand(1) + 1
         LET mensajeSalidaSolicitud.saldoInsolutoCredito    = util.Math.rand(100000)
         LET mensajeSalidaSolicitud.montoOriginacion        = util.Math.rand(80000)
         LET mensajeSalidaSolicitud.fechaOriginacionCredito = TODAY USING "yyyymmdd"
         LET mensajeSalidaSolicitud.idMotivo = ""
      END IF

   ELSE
      LET mensajeSalidaSolicitud.nss                     = mensajeEntradaSolicitud.nss
      LET mensajeSalidaSolicitud.idCredito               = v_acreditado.v_credito
      LET mensajeSalidaSolicitud.tipoCredito             = v_acreditado.v_tipo_credito
      LET mensajeSalidaSolicitud.saldoInsolutoCredito    = v_acreditado.v_saldo
      LET mensajeSalidaSolicitud.montoOriginacion        = v_acreditado.v_saldo_originacion
      LET mensajeSalidaSolicitud.fechaOriginacionCredito = TODAY USING "yyyymmdd"
      
      IF( v_acreditado.v_diagnostico = "00" )THEN
         LET mensajeSalidaSolicitud.diagnosticoCartera   = v_acreditado.v_diagnostico USING "&&"
         LET mensajeSalidaSolicitud.idMotivo             = ""
      ELSE
         LET mensajeSalidaSolicitud.diagnosticoCartera   = v_acreditado.v_diagnostico USING "&&"
         LET mensajeSalidaSolicitud.idMotivo             = v_acreditado.v_motivo
      END IF  
   END IF
END FUNCTION