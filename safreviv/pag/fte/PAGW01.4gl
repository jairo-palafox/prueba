####################################################################
#Modulo            =>BUS                                           #
#Programa          =>CLIENTE_SUMA                                  #
#Objetivo          =>Interface de safre que manda a llamar el WS   #
#                    de TRM                                        #
#Fecha inicio      =>19 NOVIEMBRE 2013                             #
####################################################################
DATABASE safre_viv

GLOBALS "PAGW01.inc"
GLOBALS "PAGW02.inc"

#Parametros de conexion
PRIVATE DEFINE v_url_servidor       LIKE wsv_cliente.ruta_servidor 
PRIVATE DEFINE v_usuario            LIKE wsv_cliente.usuario
PRIVATE DEFINE v_password           LIKE wsv_cliente.password
PRIVATE DEFINE v_intentos           LIKE wsv_cliente.num_reintento

FUNCTION fn_consulta_pago_trm(p_consulta_pago)
   DEFINE p_consulta_pago        consultaPago
   DEFINE v_respuesta_pago       respuestaPagosHistoricos

   DEFINE v_num_intento    INTEGER


   DEFINE wsstatus   INTEGER

   #Primero se asigna la URL del servicio
   CALL fn_configura_ws()
   LET HistoricoPagosServiceWS_HistoricoPagosPortLocation = v_url_servidor CLIPPED
   

   #Se asignan los parametros a enviar
   LET obtenPagosHistoricos.NSS = p_consulta_pago.NSS
   LET obtenPagosHistoricos.NRP = p_consulta_pago.NRP
   LET obtenPagosHistoricos.folioSUA = p_consulta_pago.folioSUA
   LET obtenPagosHistoricos.fechaPago = p_consulta_pago.fechaPago

   INITIALIZE v_respuesta_pago.* TO NULL
   LET v_num_intento = 0
   #Se ejecuta el llamado al servicio
   WHILE v_num_intento <= v_intentos
      --Se ejecuta la funcion del cliente
      CALL obtenPagosHistoricos_g() RETURNING wsstatus
      IF wsStatus == 0 THEN
        --Se ejecuto correctamente el WS, se llena el objeto de salida 
        LET v_respuesta_pago.* = obtenPagosHistoricosResponse.return.*
        EXIT WHILE
      ELSE
         --El WS no se ejecuto correctamente
         IF v_num_intento = v_intentos THEN
            --Se llena la respuesta
            LET v_respuesta_pago.ICodigo = '-1'
            LET v_respuesta_pago.IDescripcion = 'Error de comunicacion con el WS de TRM: ', wsError.description
            EXIT WHILE
         ELSE
            #si aun no se llega al numero maximo de intentos configurados se 
            #incrementa el contador y se reintenta la peticion
            LET v_num_intento = v_num_intento + 1 
         END IF 
      END IF
   END WHILE

   RETURN v_respuesta_pago.*
END FUNCTION

PRIVATE FUNCTION fn_configura_ws()
   DEFINE v_consulta    STRING

   LET v_consulta = "SELECT   ruta_servidor, 
                              usuario, 
                              password, 
                              num_reintento 
                     FROM     wsv_cliente 
                     WHERE    cve_cliente = ?"
   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta USING WS_PAGOS_TRM INTO  v_url_servidor,
                                                   v_usuario,
                                                   v_password,
                                                   v_intentos
   --DISPLAY "Conectando a: ", v_url_servidor
END FUNCTION