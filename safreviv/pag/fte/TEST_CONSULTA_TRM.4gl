#Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
DATABASE safre_viv

GLOBALS "PAGW01.inc"

MAIN
   DEFINE v_parametros consultaPago                #Variable para los parametros que se enviaran al WS
   DEFINE v_respuesta  respuestaPagosHistoricos    #Variable para "cachar" la respuesta del WS 

   #Se llenan los parametros a enviar, estos valores se cacaron del correo en el que envian la evidencia del WSDL
   LET v_parametros.NSS         = "31968122130"
   LET v_parametros.NRP         = "B2819918106"
   LET v_parametros.folioSUA    = "093743"
   LET v_parametros.fechaPago   = "31.05.2010"

   #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta.*
   #NOTA: Esta funcion se ejecuta por cada registro
   CALL fn_consulta_pago_trm(v_parametros.*) RETURNING v_respuesta.*

   #En caso de que se presente algun problema con el WS la funcion regresara los campos:
      #  v_respuesta.ICodigo        = "-1"
      #  v_respuesta.IDescripcion   = "DESCRIPCION DEL ERROR"
   
   DISPLAY "Respuesta            = ", v_respuesta.ICodigo
   DISPLAY "Descripcion          = ", v_respuesta.IDescripcion
   DISPLAY v_respuesta.NSS

   IF v_respuesta.ICodigo <> '-1' THEN
      #Consulta exitosa
      DISPLAY "fecha Entrada TRM =", v_respuesta.fechaEntradaTRM
      DISPLAY "importe Pago      =", v_respuesta.importePago
   END IF 
END MAIN
