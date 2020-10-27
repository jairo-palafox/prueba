#Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
GLOBALS "CTAW14.inc"

MAIN
   DEFINE v_parametros        solicita_desmarca
   DEFINE v_respuesta         respuesta_desmarca

   {
   #Se llenan los parametros a enviar
   LET v_parametros.apeMaterno = 'LOPEZ'
   LET v_parametros.apePaterno = 'VILLELA'
   LET v_parametros.nombres = 'ROSA MARIA'
   LET v_parametros.fechaPresentacion = '20150626'--Tiene que tener el formato AAAAMMDD
   LET v_parametros.nss = '01715387005'
   LET v_parametros.numCreditoInfonavit = '0000000000'
   LET v_parametros.rfc = 'VILR5411212Y8'}

   CLOSE WINDOW SCREEN 
   OPEN WINDOW w_desmarca WITH FORM "PRUEBA_DESMARCA"
{apeMaterno              STRING,
      apePaterno              STRING,
      fechaPresentacion       STRING,
      nombres                 STRING,
      nss                     STRING,
      numCreditoInfonavit     STRING,
      rfc                  }
   INPUT v_parametros.* FROM materno,paterno,f_presentacion,nombre,nss,numCredito,rfc ATTRIBUTES (UNBUFFERED)
      ON ACTION ACCEPT
         DISPLAY "Parametros a enviar:"
         DISPLAY ""
         DISPLAY "nss                 = ", v_parametros.nss
         DISPLAY "apeMaterno          = ", v_parametros.apeMaterno
         DISPLAY "apePaterno          = ", v_parametros.apePaterno
         DISPLAY "nombre              = ", v_parametros.nombres
         DISPLAY "rfc                 = ", v_parametros.rfc
         DISPLAY "numCreditoInfonavit = ", v_parametros.numCreditoInfonavit
         DISPLAY "fechaPresentacion   = ", v_parametros.fechaPresentacion
         
         DISPLAY ""
         
         #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta.*
         #NOTA: Esta funcion se ejecuta por cada registro a notificar
         CALL fn_solicita_desmarca_procesar(v_parametros.*) RETURNING v_respuesta.*
         
         #En caso de que se presente algun problema con el WS la funcion regresara los campos:
            #  v_respuesta.diagProceso = "-1"
         #ademas de guardar un registro en la tabla wsv_his_err_cliente

         DISPLAY "Resultado Operación    = ", v_respuesta.resultOperacion --Es igual al que se envia
         DISPLAY "Respuesta del Servidor = ", v_respuesta.diagProceso     --Respuesta del servidor
   END INPUT
   CLOSE WINDOW w_desmarca

END MAIN