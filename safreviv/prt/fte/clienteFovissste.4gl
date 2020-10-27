
GLOBALS "PRTW01.inc"

DEFINE v_mensajeEntradaParaFovissste RECORD
          nss                     CHAR(11),
          curp                    CHAR(18),
          idCredito               VARCHAR(10),
          folioConsulta           VARCHAR(10),
          nombre                  VARCHAR(40),
          apPaterno               VARCHAR(40),
          apMaterno               VARCHAR(40)          
       END RECORD,
       r_respuesta_fovissste RECORD
          nss                     CHAR(11),
          curp                    CHAR(18),
          fechaOtorgamientoCad    CHAR(8),
          fechaOtorgamiento       DATE,
          idCredito               VARCHAR(10),
          folioConsulta           VARCHAR(10),
          folioRespuesta          VARCHAR(10),
          saldoInsolutoCredito    DECIMAL(13,2),
          tpoCredito              SMALLINT,
          diagnostico             SMALLINT,
          idMotivo                CHAR(5)
       END RECORD ,
      r_resultado_invoca_ws      INTEGER,
      v_error smallint
       
MAIN 

LET v_mensajeEntradaParaFovissste.nss           = ARG_VAL(1)
LET v_mensajeEntradaParaFovissste.curp          = ARG_VAL(2)
LET v_mensajeEntradaParaFovissste.idCredito     = ARG_VAL(3)
LET v_mensajeEntradaParaFovissste.folioConsulta = ARG_VAL(4)

CALL fn_consulta_credito_fovissste()

END MAIN

#Objetivo: Invoca función de WS consulta de crédito FOVISSSTE (PRTW01)
FUNCTION fn_consulta_credito_fovissste()
   
   # Asigna secuencia como id del folio hacia fovissste

   INITIALIZE r_respuesta_fovissste.* TO NULL
   # Invocar el servicio de fovissste
   CALL consultaCreditoFovissste(v_mensajeEntradaParaFovissste.nss,
                                 v_mensajeEntradaParaFovissste.curp,
                                 v_mensajeEntradaParaFovissste.idCredito,
                                 v_mensajeEntradaParaFovissste.folioConsulta                                 
                                 ) RETURNING r_resultado_invoca_ws,
                                             r_respuesta_fovissste.nss,
                                             r_respuesta_fovissste.curp,
                                             r_respuesta_fovissste.fechaOtorgamientoCad,
                                             r_respuesta_fovissste.idCredito,
                                             r_respuesta_fovissste.folioConsulta,
                                             r_respuesta_fovissste.folioRespuesta,
                                             r_respuesta_fovissste.saldoInsolutoCredito,
                                             r_respuesta_fovissste.tpoCredito,
                                             r_respuesta_fovissste.diagnostico,
                                             r_respuesta_fovissste.idMotivo


 DISPLAY "resultado ws      : ",r_resultado_invoca_ws
 DISPLAY "nss               : ",r_respuesta_fovissste.nss
 DISPLAY "curp              : ",r_respuesta_fovissste.curp
 DISPLAY "fecha otorgamiento: ",r_respuesta_fovissste.fechaOtorgamientoCad
 DISPLAY "id_credito        : ",r_respuesta_fovissste.idCredito
 DISPLAY "folio consulta    : ",r_respuesta_fovissste.folioConsulta
 DISPLAY "folio resupesta   : ",r_respuesta_fovissste.folioRespuesta
 DISPLAY "saldo insoluto    : ",r_respuesta_fovissste.saldoInsolutoCredito
 DISPLAY "tipo credito      : ",r_respuesta_fovissste.tpoCredito
 DISPLAY "diagnostico       : ",r_respuesta_fovissste.diagnostico
 DISPLAY "id motivo         : ",r_respuesta_fovissste.idMotivo

# Pruebas aceptadas
{LET r_resultado_invoca_ws = 0
LET r_respuesta_fovissste.nss = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
LET r_respuesta_fovissste.curp = mensajeEntradaSolicitud.mensajeEntradaSolicitud.curp
LET r_respuesta_fovissste.fechaOtorgamientoCad = "20010807"
LET r_respuesta_fovissste.idCredito = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCredito
LET r_respuesta_fovissste.folioConsulta = v_id_prt_solicitud
LET r_respuesta_fovissste.folioRespuesta = 1
LET r_respuesta_fovissste.saldoInsolutoCredito = "58320"
LET r_respuesta_fovissste.tpoCredito = "1"
LET r_respuesta_fovissste.diagnostico = 1
LET r_respuesta_fovissste.idMotivo = "1000"}

   IF(r_resultado_invoca_ws <> 0)THEN
      DISPLAY "Error de comunicación con WS FOVISSSTE:"
      DISPLAY "Estado:",r_resultado_invoca_ws
            
      LET v_error = TRUE
   ELSE
      TRY
         LET r_respuesta_fovissste.fechaOtorgamiento = DATE(r_respuesta_fovissste.fechaOtorgamientoCad[5,6]||"/"||
                                                            r_respuesta_fovissste.fechaOtorgamientoCad[7,8]||"/"||
                                                            r_respuesta_fovissste.fechaOtorgamientoCad[1,4])
      CATCH
         DISPLAY "Error en fecha otorgamiento para solicitud: ",v_mensajeEntradaParaFovissste.folioConsulta
         DISPLAY "Fecha recuperada: ",r_respuesta_fovissste.fechaOtorgamientoCad
      END TRY
      LET v_error = FALSE
   END IF
   
END FUNCTION
