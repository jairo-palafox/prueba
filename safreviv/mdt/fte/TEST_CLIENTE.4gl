#Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
GLOBALS "MDTW01.inc"

MAIN
   DEFINE v_parametros notificaMandato #Variable para los parametros que se enviaran al WS
   DEFINE v_respuesta respuestaMandato #Variable para "cachar" la respuesta del WS 

   #Se llenan los parametros a enviar
   LET v_parametros.id_origen         = 1
   LET v_parametros.nss               = "12345678909"
   LET v_parametros.id_credito        = 9000000090
   LET v_parametros.cve_mandato        = "111111111111111111"
   LET v_parametros.tpo_descuento     = 3
   LET v_parametros.valor_descuento   = 123123
   LET v_parametros.f_canales         = TODAY
   LET v_parametros.f_inicio_mandato  = TODAY
   LET v_parametros.f_culmina_mandato = TODAY
   LET v_parametros.referencia        = "sadfsadf"
   LET v_parametros.id_canales        = 1
   LET v_parametros.tipo_operacion    = 3
   LET v_parametros.diagnostico       = "02"
   LET v_parametros.resultado_operacion = "02"
   #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta.*
   #NOTA: Esta funcion se ejecuta por cada registro a notificar
   CALL fn_notifica_instruccion_mdt_can(v_parametros.*) RETURNING v_respuesta.*

   #En caso de que se presente algun problema con el WS la funcion regresara los campos:
      #  v_respuesta.diag_notifica = "-1"
   #ademas de guardar un registro en la tabla wsv_his_err_cliente
   
   DISPLAY "Respuesta = ", v_respuesta.diagnostico                   --Es igual al que se envia
   DISPLAY "Resultado Operacion = ", v_respuesta.resultado_operacion --Es igual al que se envia
   DISPLAY "Respuesta del Servidor = ", v_respuesta.diag_notifica    --Respuesta del servidor
END MAIN