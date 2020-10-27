####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTW02.4gl                                    #
#Objetivo          =>Programa que invoca al cliente del WS         #
#                    de mandatos                                   #
#Fecha inicio      =>09 MARZO 2012                                 #
####################################################################
DATABASE safre_viv

GLOBALS "MDTW03.inc"

FUNCTION fn_notifica_mandato(p_url_servidor, p_usuario, p_password, p_intentos, p_solicita)
   DEFINE p_url_servidor   STRING 
   DEFINE p_usuario        STRING 
   DEFINE p_password       STRING
   DEFINE p_intentos       INTEGER
   DEFINE p_solicita       tns2NotificaInstruccionRequest
   DEFINE v_respuesta      tns2NotificaInstruccionResponse
   DEFINE wsStatus         INTEGER
   DEFINE v_num_intento    INTEGER 

   #Variables para el tratamiento de errores
   DEFINE v_cve_cliente       LIKE wsv_his_err_cliente.cve_cliente
   DEFINE v_f_error           LIKE wsv_his_err_cliente.f_error
   DEFINE v_ws_status         LIKE wsv_his_err_cliente.ws_status
   DEFINE v_cod_error         LIKE wsv_his_err_cliente.cod_error
   DEFINE v_desc_error        LIKE wsv_his_err_cliente.desc_error
   DEFINE v_usuario_ejecuta   LIKE wsv_his_err_cliente.usuario

   LET v_num_intento = 0

   --Se llenan las variables que se enviaran como prametros al cliente del WS de marcas
   LET ns1notifica_instruccion_mdt_can.notificaInstruccionRequest.* = p_solicita.*
   
   WHILE v_num_intento <= p_intentos
      --Se ejecuta la funcion del cliente
      CALL  notificaInstruccion(p_url_servidor, p_usuario, p_password) RETURNING wsStatus

      IF wsStatus == 0 THEN
        --Se ejecuto correctamente el WS
        LET v_respuesta.* = ns1notifica_instruccion_mdt_canResponse.notifica_instruccion_mdt_canReturn.*
        EXIT WHILE
      ELSE
         --El WS no se ejecuto correctamente
         IF v_num_intento = p_intentos THEN
            --Se llena la respuesta
            LET v_respuesta.diag_notifica = "-1"
            
            --Se reporta el error
            LET v_cve_cliente = 2         --Clave del cliente de notificacion de mandatos (wsv_cliente)
            LET v_f_error = TODAY
            LET v_ws_status = wsStatus
            LET v_cod_error = wsError.code
            LET v_desc_error = "Error al notificar el NSS ", p_solicita.nss, ": ", wsError.description
            LET v_usuario_ejecuta = "mandatos"
                                                   
            #Instruccion para ejecutar la funcion que registra los errores de los clientes de WS
            PREPARE exe_fn_error_cliente_ws FROM "EXECUTE PROCEDURE safre_viv:sp_error_cliente_ws(?,?,?,?,?,?,?)"
            LET v_num_intento = v_num_intento + 1
            EXECUTE exe_fn_error_cliente_ws  USING v_cve_cliente,
                                                   v_f_error,
                                                   v_num_intento,
                                                   v_ws_status,
                                                   v_cod_error,
                                                   v_desc_error,
                                                   v_usuario_ejecuta
            EXIT WHILE
         ELSE
            #si aun no se llega al numero maximo de intentos configurados se 
            #incrementa el contador y se reintenta la peticion
            LET v_num_intento = v_num_intento + 1 
         END IF 
      END IF
   END WHILE
   RETURN v_respuesta.*
END FUNCTION 