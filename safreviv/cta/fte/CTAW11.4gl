####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAW11.4gl                                    #
#Objetivo          =>Programa que invoca al cliente del WS         #
#                    de marca en procesar                          #
#Fecha inicio      =>01 FEBRERO 2012                               #
####################################################################
#IMPORT JAVA com.efp.safre.services.WSProcesar

DATABASE safre_viv

GLOBALS "CTAW12.inc"

{FUNCTION fn_marca_procesar(p_url_servidor, p_usuario, p_password, p_intentos, p_solicita)

   DEFINE p_url_servidor   STRING 
   DEFINE p_usuario        STRING 
   DEFINE p_password       STRING
   DEFINE p_intentos       INTEGER
   DEFINE p_solicita       tSolicMarcaVO
   DEFINE v_respuesta      tSolicMarcaRespVO
   DEFINE v_num_intento    INTEGER 

   DEFINE cliente       WSProcesar
   DEFINE response      STRING
   DEFINE item      base.StringTokenizer

   --Se llenan las variables que se enviaran como prametros al cliente del WS de marcas
   IF p_solicita.rfc IS NULL OR p_solicita.rfc.getLength() = 0 THEN
      LET p_solicita.rfc = " "
   END IF

   IF p_solicita.apeMaterno IS NULL OR p_solicita.apeMaterno.getLength() = 0 THEN
      LET p_solicita.apeMaterno = " "
   END IF
   
   LET p_solicita.numCreditoInfonavit = p_solicita.numCreditoInfonavit USING '&&&&&&&&&&'

   LET v_num_intento = 0

   WHILE v_num_intento <= p_intentos
      --Se ejecuta la funcion del cliente
      LET response = com.efp.safre.services.WSProcesar.fn_marcar(p_solicita.apeMaterno,
                                                                   p_solicita.apePaterno,
                                                                   p_solicita.fechaPresentacion,
                                                                   p_solicita.nombres,
                                                                   p_solicita.nss,
                                                                   p_solicita.numCreditoInfonavit,
                                                                   p_solicita.rfc,
                                                                   p_solicita.sitCredito,
                                                                   p_solicita.tipoCredito,
                                                                   p_url_servidor, 
                                                                   p_usuario, 
                                                                   p_password)
      LET item = base.StringTokenizer.create(response,"|")
      LET v_respuesta.diagProceso = item.nextToken()
      LET v_respuesta.resultOperacion = item.nextToken()

      #DISPLAY "Diagnostico: ", v_respuesta.diagProceso
      #DISPLAY "ResultOperacion: ", v_respuesta.resultOperacion

      IF v_respuesta.diagProceso = "-1" THEN
         IF v_num_intento = p_intentos THEN
            DISPLAY "ERROR al marcar el NSS ", p_solicita.nss
            DISPLAY "Numero de intentos ejecutados: ", v_num_intento
            EXIT WHILE
         ELSE
            #si aun no se llega al numero maximo de intentos configurados se 
            #incrementa el contador y se reintenta la peticion
            LET v_num_intento = v_num_intento + 1 
         END IF
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN v_respuesta.*
   
END FUNCTION

FUNCTION fn_desmarca_procesar(p_url_servidor, p_usuario, p_password, p_intentos, p_solicita)

   DEFINE p_url_servidor   STRING 
   DEFINE p_usuario        STRING 
   DEFINE p_password       STRING
   DEFINE p_intentos       INTEGER
   DEFINE p_solicita       tSolicDesmarcaVO
   DEFINE v_respuesta      tSolicDesmarcaRespVO
   DEFINE v_num_intento    INTEGER 

   DEFINE cliente       WSProcesar
   DEFINE response      STRING
   DEFINE item      base.StringTokenizer

   --Se llenan las variables que se enviaran como prametros al cliente del WS de marcas
   IF p_solicita.rfc IS NULL OR p_solicita.rfc.getLength() = 0 THEN
      LET p_solicita.rfc = " "
   END IF

   IF p_solicita.apeMaterno IS NULL OR p_solicita.apeMaterno.getLength() = 0 THEN
      LET p_solicita.apeMaterno = " "
   END IF

   LET p_solicita.numCreditoInfonavit = p_solicita.numCreditoInfonavit USING '&&&&&&&&&&'

   LET v_num_intento = 0

   WHILE v_num_intento <= p_intentos
      --Se ejecuta la funcion del cliente
      LET response = com.efp.safre.services.WSProcesar.fn_desmarcar(p_solicita.apeMaterno,
                                                                   p_solicita.apePaterno,
                                                                   p_solicita.fechaPresentacion,
                                                                   p_solicita.nombres,
                                                                   p_solicita.nss,
                                                                   p_solicita.numCreditoInfonavit,
                                                                   p_solicita.rfc,
                                                                   p_url_servidor, 
                                                                   p_usuario, 
                                                                   p_password)
      LET item = base.StringTokenizer.create(response,"|")
      LET v_respuesta.diagProceso = item.nextToken()
      LET v_respuesta.resultOperacion = item.nextToken()
      
      #DISPLAY "Diagnostico: ", v_respuesta.diagProceso
      #DISPLAY "ResultOperacion: ", v_respuesta.resultOperacion

      IF v_respuesta.diagProceso = "-1" THEN
         IF v_num_intento = p_intentos THEN
            DISPLAY "ERROR al marcar el NSS ", p_solicita.nss
            DISPLAY "Numero de intentos ejecutados: ", v_num_intento
            EXIT WHILE
         ELSE
            #si aun no se llega al numero maximo de intentos configurados se 
            #incrementa el contador y se reintenta la peticion
            LET v_num_intento = v_num_intento + 1 
         END IF
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN v_respuesta.*
   
END FUNCTION}

FUNCTION fn_solicita_marca(p_url_servidor, p_usuario, p_password, p_intentos, p_solicita)
   DEFINE p_url_servidor        STRING 
   DEFINE p_usuario             STRING 
   DEFINE p_password            STRING
   DEFINE p_intentos            INTEGER
   DEFINE p_solicita            tSolicMarcaVO
   DEFINE v_respuesta           tSolicMarcaRespVO
   DEFINE wsStatus              INTEGER
   DEFINE v_num_intento         INTEGER
   DEFINE soapStatus            INTEGER

   #Variables para el tratamiento de errores
   DEFINE v_cve_cliente         LIKE wsv_his_err_cliente.cve_cliente
   DEFINE v_f_error             LIKE wsv_his_err_cliente.f_error
   DEFINE v_ws_status           LIKE wsv_his_err_cliente.ws_status
   DEFINE v_cod_error           LIKE wsv_his_err_cliente.cod_error
   DEFINE v_desc_error          LIKE wsv_his_err_cliente.desc_error
   DEFINE v_usuario_ejecuta     LIKE wsv_his_err_cliente.usuario

   LET v_num_intento = 0

   --Se llenan las variables que se enviaran como prametros al cliente del WS de marcas
   IF p_solicita.rfc IS NULL OR p_solicita.rfc.getLength() = 0 THEN
      LET p_solicita.rfc = " "
   END IF

   IF p_solicita.apeMaterno IS NULL OR p_solicita.apeMaterno.getLength() = 0 THEN
      LET p_solicita.apeMaterno = " "
   END IF

   LET p_solicita.numCreditoInfonavit = p_solicita.numCreditoInfonavit USING '&&&&&&&&&&'
   
   --LET solicitudMarcaRequest.solicitud.* = p_solicita.*
   
   WHILE v_num_intento <= p_intentos
      --Se ejecuta la funcion del cliente
      #CALL  solicitudMarca(p_url_servidor, p_usuario, p_password) RETURNING wsStatus
      CALL  solicitudMarca( p_url_servidor,
                            p_usuario,
                            p_password,
                            p_solicita.apeMaterno,
                            p_solicita.apePaterno,
                            p_solicita.fechaPresentacion,
                            p_solicita.nombres,
                            p_solicita.nss,
                            p_solicita.numCreditoInfonavit,
                            p_solicita.rfc,
                            p_solicita.sitCredito,
                            p_solicita.tipoCredito )
                            RETURNING soapStatus,
                                      SolicMarcaRespVO.apeMaternoBD,
                                      SolicMarcaRespVO.apeMaternoInfo,
                                      SolicMarcaRespVO.apePaternoBD,
                                      SolicMarcaRespVO.apePaternoInfo,
                                      SolicMarcaRespVO.diagProceso,
                                      SolicMarcaRespVO.nombresBD,
                                      SolicMarcaRespVO.nombresInfo,
                                      SolicMarcaRespVO.nss,
                                      SolicMarcaRespVO.resultOperacion,
                                      SolicMarcaRespVO.rfc,
                                      SolicMarcaRespVO.rfcBD,
                                      SolicMarcaRespVO.tipTrabajador


      --IF wsStatus == 0 THEN
      IF soapStatus == 0 THEN
        --Se ejecuto correctamente el WS
        LET v_respuesta.* = SolicMarcaRespVO.*
        EXIT WHILE
      ELSE
         --El WS no se ejecuto correctamente
         IF v_num_intento = p_intentos THEN
            --Se llena la respuesta
            LET v_respuesta.diagProceso = "-1"
            LET v_respuesta.resultOperacion = "-1"

            --Se reporta el error
            LET v_cve_cliente     = 'cre_1'         --Clave del cliente para marcas (wsv_cliente)
            LET v_f_error         = TODAY
            LET v_ws_status       = wsStatus
            LET v_cod_error       = wsError.code
            LET v_desc_error      = "ERROR al marcar el NSS ", p_solicita.nss, ": ", v_cod_error, " - ", wsError.description
            LET v_usuario_ejecuta = "marcas"

            DISPLAY ""
            DISPLAY v_desc_error
            DISPLAY "Numero de intentos ejecutados: ", v_num_intento
            DISPLAY ""

            {  -- Se comenta por que ya no es necesario insertar los registros de error del WS
            #Instruccion para ejecutar la funcion que registra los errores de los clientes de WS
            PREPARE exe_fn_error_cliente_ws FROM "EXECUTE PROCEDURE safre_viv:sp_error_cliente_ws(?,?,?,?,?,?,?)"
            EXECUTE exe_fn_error_cliente_ws  USING v_cve_cliente,
                                                   v_f_error,
                                                   v_num_intento,
                                                   v_ws_status,
                                                   v_cod_error,
                                                   v_desc_error,
                                                   v_usuario_ejecuta}
            EXIT WHILE
         ELSE
            #si aun no se llega al numero máximo de intentos configurados se
            #incrementa el contador y se reintenta la petición
            LET v_num_intento = v_num_intento + 1 
         END IF 
      END IF
   END WHILE
   RETURN v_respuesta.*

END FUNCTION 

FUNCTION fn_solicita_desmarca(p_url_servidor, p_usuario, p_password, p_intentos, p_solicita)
   DEFINE p_url_servidor        STRING
   DEFINE p_usuario             STRING
   DEFINE p_password            STRING
   DEFINE p_intentos            INTEGER
   DEFINE p_solicita            tSolicDesmarcaVO
   DEFINE v_respuesta           tSolicDesmarcaRespVO
   DEFINE wsStatus              INTEGER
   DEFINE v_num_intento         INTEGER
   DEFINE soapStatus            INTEGER

   #Variables para el tratamiento de errores
   DEFINE v_cve_cliente       LIKE wsv_his_err_cliente.cve_cliente
   DEFINE v_f_error           LIKE wsv_his_err_cliente.f_error
   DEFINE v_ws_status         LIKE wsv_his_err_cliente.ws_status
   DEFINE v_cod_error         LIKE wsv_his_err_cliente.cod_error
   DEFINE v_desc_error        LIKE wsv_his_err_cliente.desc_error
   DEFINE v_usuario_ejecuta   LIKE wsv_his_err_cliente.usuario

   LET v_num_intento = 0

   --Se llenan las variables que se enviaran como prametros al cliente del WS de marcas
   IF p_solicita.rfc IS NULL OR p_solicita.rfc.getLength() = 0 THEN
      LET p_solicita.rfc = " "
   END IF

   IF p_solicita.apeMaterno IS NULL OR p_solicita.apeMaterno.getLength() = 0 THEN
      LET p_solicita.apeMaterno = " "
   END IF

   LET p_solicita.numCreditoInfonavit = p_solicita.numCreditoInfonavit USING '&&&&&&&&&&'

   --LET solicitudDesMarcaRequest.solicitud.* = p_solicita.*
   
   WHILE v_num_intento <= p_intentos
      --Se ejecuta la funcion del cliente
      CALL solicitudDesmarca( p_url_servidor,
                              p_usuario,
                              p_password,
                              p_solicita.apeMaterno,
                              p_solicita.apePaterno,
                              p_solicita.fechaPresentacion,
                              p_solicita.nombres,
                              p_solicita.nss,
                              p_solicita.numCreditoInfonavit,
                              p_solicita.rfc )
                              RETURNING soapStatus,
                                        SolicDesmarcaRespVO.apeMaternoBD,
                                        SolicDesmarcaRespVO.apeMaternoInfo,
                                        SolicDesmarcaRespVO.apePaternoBD,
                                        SolicDesmarcaRespVO.apePaternoInfo,
                                        SolicDesmarcaRespVO.diagProceso,
                                        SolicDesmarcaRespVO.nombresBD,
                                        SolicDesmarcaRespVO.nombresInfo,
                                        SolicDesmarcaRespVO.nss,
                                        SolicDesmarcaRespVO.resultOperacion,
                                        SolicDesmarcaRespVO.rfc,
                                        SolicDesmarcaRespVO.rfcBD,
                                        SolicDesmarcaRespVO.tipCreditDesm

      --IF wsStatus == 0 THEN
      IF soapStatus == 0 THEN
        --Se ejecuto correctamente el WS
        LET v_respuesta.* = SolicDesmarcaRespVO.*
        EXIT WHILE
      ELSE
         --El WS no se ejecuto correctamente
         IF v_num_intento = p_intentos THEN
            --Se llena la respuesta
            LET v_respuesta.diagProceso     = "-1"
            LET v_respuesta.resultOperacion = "-1"
            
            --Se reporta el error
            LET v_cve_cliente     = 'cre_2'         --Clave del cliente para marcas (wsv_cliente)
            LET v_f_error         = TODAY
            LET v_ws_status       = wsStatus
            LET v_cod_error       = wsError.code
            LET v_desc_error      = "ERROR al desmarcar el NSS ", p_solicita.nss, ": ", wsError.description
            LET v_usuario_ejecuta = "marcas"

            DISPLAY ""
            DISPLAY v_desc_error
            DISPLAY "Numero de intentod ejecutados: ", v_num_intento
            DISPLAY ""

            {  -- Se comenta por que se deja de insertar en la tabla de errores
            #Instruccion para ejecutar la funcion que registra los errores de los clientes de WS
            PREPARE exe_fn_error_cliente FROM "EXECUTE PROCEDURE safre_viv:sp_error_cliente_ws(?,?,?,?,?,?,?)"
            EXECUTE exe_fn_error_cliente  USING v_cve_cliente,
                                                v_f_error,
                                                v_num_intento,
                                                v_ws_status,
                                                v_cod_error,
                                                v_desc_error,
                                                v_usuario_ejecuta
            }
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
