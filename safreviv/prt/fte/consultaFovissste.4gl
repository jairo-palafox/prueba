
IMPORT com
IMPORT util
DATABASE safre_viv
DEFINE mensajeEntradaSolicitud RECORD
          nss               CHAR(11),
          curp              CHAR(18),
          idCredito         VARCHAR(10),
          folioConsulta     VARCHAR(10)
       END RECORD,
       mensajeSalidaSolicitud RECORD
          nss                     VARCHAR(11),
          curp                    CHAR(18),
          fechaOtorgamiento       CHAR(8),
          idCredito               VARCHAR(10),
          folioConsulta           VARCHAR(10),
          folioRespuesta          VARCHAR(10),
          saldoInsolutoCredito    DECIMAL(13,2),
          tipoCredito             SMALLINT,
          diagnostico             SMALLINT,
          idMotivo                CHAR(5)
       END RECORD
DEFINE numero SMALLINT
       
MAIN
DEFINE ret INTEGER

   CALL fn_crea_servicio()

   #PUERTO 8072
   CALL com.WebServiceEngine.Start()
   DISPLAY "Inicia WS consulta crédito fovissste"

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
         LET int_flag=0
         EXIT WHILE
      END IF 
   END WHILE
   DISPLAY "Server stopped"

END MAIN

FUNCTION fn_crea_servicio()
DEFINE serv  com.WebService    # A WebService
DEFINE op    com.WebOperation  # Operation of a WebService

   LET serv = com.WebService.CreateWebService("consultaCreditoFovissste","http:www.fovissste.com/consulta/credito")  
   LET op = com.WebOperation.CreateRPCStyle("fn_consulta_credito_fovissste", 
                                            "consultaCreditoFovissste", 
                                            mensajeEntradaSolicitud, 
                                            mensajeSalidaSolicitud)

   CALL serv.publishOperation(op,"consultaCreditoFovisssteCedente")
   CALL com.WebServiceEngine.RegisterService(serv)


END FUNCTION

FUNCTION fn_consulta_credito_fovissste()
DEFINE v_consulta STRING,
       v_acreditado RECORD
          v_nss          CHAR(11),
          v_credito      VARCHAR(10),
          v_saldo        DECIMAL(22,2),
          v_tipo_credito SMALLINT,
          v_diagnostico  SMALLINT,
          v_motivo       CHAR(5)
       END RECORD

   
   LET v_consulta = "SELECT FIRST 1 nss,credito,saldo,tipo_credito,diagnostico,motivo FROM prt_nss_prueba_cedente WHERE nss = ?"
   PREPARE prp_cons_nss_prueba_ced FROM v_consulta
   EXECUTE prp_cons_nss_prueba_ced USING mensajeEntradaSolicitud.nss
                                    INTO v_acreditado.*
                                    
   IF( v_acreditado.v_nss IS  NULL )THEN
      
      CALL util.Math.rand(10) RETURNING numero
      DISPLAY "Numero:",numero," ",CURRENT YEAR TO FRACTION

      IF(numero > 6)THEN
         LET mensajeSalidaSolicitud.nss                     = mensajeEntradaSolicitud.nss
         LET mensajeSalidaSolicitud.curp                    = mensajeEntradaSolicitud.curp
         LET mensajeSalidaSolicitud.fechaOtorgamiento       = "20150217"
         LET mensajeSalidaSolicitud.idCredito               = 0
         LET mensajeSalidaSolicitud.folioConsulta           = mensajeEntradaSolicitud.folioConsulta
         LET mensajeSalidaSolicitud.folioRespuesta          = mensajeEntradaSolicitud.folioConsulta + 1 
         LET mensajeSalidaSolicitud.saldoInsolutoCredito    = 0.0
         CALL util.Math.rand(1) RETURNING numero
         LET mensajeSalidaSolicitud.tipoCredito             = numero + 1
         LET mensajeSalidaSolicitud.diagnostico             = 2 # Rechazado
         CALL util.Math.rand(5) RETURNING numero
         CASE numero
            WHEN 1
               LET mensajeSalidaSolicitud.idMotivo    = "1001"
                        
            WHEN 2
               LET mensajeSalidaSolicitud.idMotivo    = "1003"
                        
            WHEN 3
               LET mensajeSalidaSolicitud.idMotivo    = "1004"
                        
            WHEN 4
               LET mensajeSalidaSolicitud.idMotivo    = "1005"
                        
            WHEN 5
               LET mensajeSalidaSolicitud.idMotivo    = "1007"

            WHEN 6
               LET mensajeSalidaSolicitud.idMotivo    = "1010"

            WHEN 7
               LET mensajeSalidaSolicitud.idMotivo    = "1012"

            WHEN 8
                  LET mensajeSalidaSolicitud.idMotivo    = "1013"
                        
         END CASE
      ELSE
         LET mensajeSalidaSolicitud.nss                     = mensajeEntradaSolicitud.nss
         LET mensajeSalidaSolicitud.curp                    = mensajeEntradaSolicitud.curp
         LET mensajeSalidaSolicitud.fechaOtorgamiento       = "20150217"
         LET mensajeSalidaSolicitud.idCredito               = util.Math.rand(1000000000)
         LET mensajeSalidaSolicitud.folioConsulta           = mensajeEntradaSolicitud.folioConsulta
         LET mensajeSalidaSolicitud.folioRespuesta          = mensajeEntradaSolicitud.folioConsulta + 1 
         LET mensajeSalidaSolicitud.saldoInsolutoCredito    = util.Math.rand(100000)
         LET mensajeSalidaSolicitud.tipoCredito             = util.Math.rand(1) + 1
         LET mensajeSalidaSolicitud.diagnostico             = 1 # Aceptado
         LET mensajeSalidaSolicitud.idMotivo                = 1000
      END IF
   ELSE
      
      LET mensajeSalidaSolicitud.nss                     = mensajeEntradaSolicitud.nss
      LET mensajeSalidaSolicitud.curp                    = mensajeEntradaSolicitud.curp
      LET mensajeSalidaSolicitud.fechaOtorgamiento       = "20150217"
      LET mensajeSalidaSolicitud.idCredito               = v_acreditado.v_credito
      LET mensajeSalidaSolicitud.folioConsulta           = mensajeEntradaSolicitud.folioConsulta
      LET mensajeSalidaSolicitud.folioRespuesta          = mensajeEntradaSolicitud.folioConsulta + 1 
      LET mensajeSalidaSolicitud.saldoInsolutoCredito    = v_acreditado.v_saldo
      LET mensajeSalidaSolicitud.tipoCredito             = v_acreditado.v_tipo_credito
      
      IF( v_acreditado.v_diagnostico IS NOT NULL )THEN
         LET mensajeSalidaSolicitud.diagnostico             = v_acreditado.v_diagnostico
         LET mensajeSalidaSolicitud.idMotivo                = v_acreditado.v_motivo
      ELSE
         LET mensajeSalidaSolicitud.diagnostico             = 2 # Rechazado
         CALL util.Math.rand(5) RETURNING numero
         CASE numero
            WHEN 1
               LET mensajeSalidaSolicitud.idMotivo    = "1001"
                        
            WHEN 2
               LET mensajeSalidaSolicitud.idMotivo    = "1003"
                        
            WHEN 3
               LET mensajeSalidaSolicitud.idMotivo    = "1004"
                        
            WHEN 4
               LET mensajeSalidaSolicitud.idMotivo    = "1005"
                        
            WHEN 5
               LET mensajeSalidaSolicitud.idMotivo    = "1007"

            WHEN 6
               LET mensajeSalidaSolicitud.idMotivo    = "1010"

            WHEN 7
               LET mensajeSalidaSolicitud.idMotivo    = "1012"

            WHEN 8
                  LET mensajeSalidaSolicitud.idMotivo    = "1013"
                        
         END CASE
      END IF  

   END IF
END FUNCTION