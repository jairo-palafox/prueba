#Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
GLOBALS "CTAW14.inc"

MAIN
   DEFINE v_parametros         solicita_marca
   DEFINE v_respuesta           respuesta_marca
  
   {#Se llenan los parametros a enviar
   LET v_parametros.apeMaterno = 'ALVARADO'
   LET v_parametros.apePaterno = 'HERNANDEZ'
   LET v_parametros.nombres = 'JULIA'
   LET v_parametros.fechaPresentacion = '20130603'--formato AAAAMMDD
   LET v_parametros.nss = '01006401028'
   LET v_parametros.numCreditoInfonavit = '1234567890'
   LET v_parametros.rfc = 'HEAJ640630LX0'
   LET v_parametros.sitCredito = '2'
   LET v_parametros.tipoCredito  = '01'}

   CLOSE WINDOW SCREEN 

   OPEN WINDOW w_1 WITH FORM "PRUEBA"

   INPUT v_parametros.* FROM   materno,
                               paterno,
                               fecha,
                               nombre ,
                               nss    ,
                               numCredito,
                               rfc    ,
                               sitCredito,
                               tpoCredito ATTRIBUTES (UNBUFFERED)


      ON ACTION ACCEPT

         DISPLAY "Parametros a enviar:"
         DISPLAY ""
         DISPLAY "nss                 = ", v_parametros.nss
         DISPLAY "apeMaterno          = ", v_parametros.apeMaterno
         DISPLAY "apePaterno          = ", v_parametros.apePaterno
         DISPLAY "nombres             = ", v_parametros.nombres
         DISPLAY "rfc                 = ", v_parametros.rfc
         DISPLAY "tipoCredito         = ", v_parametros.tipoCredito
         DISPLAY "numCreditoInfonavit = ", v_parametros.numCreditoInfonavit
         DISPLAY "fechaPresentacion   = ", v_parametros.fechaPresentacion
         DISPLAY "sitCredito          = ", v_parametros.sitCredito

         DISPLAY ""

         #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta.*
         #NOTA: Esta funcion se ejecuta por cada registro a notificar
         CALL fn_solicita_marca_procesar(v_parametros.*) RETURNING v_respuesta.*

         #En caso de que se presente algun problema con el WS la funcion regresara los campos:
            #  v_respuesta.diagProceso = "-1"
         #ademas de guardar un registro en la tabla wsv_his_err_cliente
   
         DISPLAY "Resultado Operacion = ", v_respuesta.resultOperacion --Es igual al que se envia
         DISPLAY "Respuesta del Servidor = ", v_respuesta.diagProceso    --Respuesta del servidor
   END INPUT

   CLOSE WINDOW w_1
END MAIN