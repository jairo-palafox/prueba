GLOBALS

DEFINE marcaNotificacion   RECORD
         nss                  CHAR(11),
         indicador_sms        CHAR(1),
         indicador_correo     CHAR(1),
         ind_bloqueo_sms      CHAR(1),
         ind_bloqueo_correo   CHAR(1),
         fuente_sms           CHAR(20),
         fuente_correo        CHAR(20)
END RECORD

DEFINE marcaNotificacionRespuesta RECORD
         result_opera               CHAR(2)
END RECORD


END GLOBALS