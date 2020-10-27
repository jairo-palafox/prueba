####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSW002.4gl                                   #
#Objetivo          =>Programa que invoca al cliente del WS         #
#                    de Procesar                                   #
#Fecha inicio      =>20 NOVIEMBRE 2013                             #
####################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml

DATABASE safre_viv

GLOBALS "BUSW001.inc"
GLOBALS "BUSW002.inc"

PRIVATE DEFINE v_id_mensaje         DECIMAL(9,0)

#Variables de configuracion del cliente
PRIVATE DEFINE v_url                   VARCHAR(255)
PRIVATE DEFINE v_num_reintento         SMALLINT
PRIVATE DEFINE v_intervalo             SMALLINT

#Variables del tramite que se enviaran a procesar
PRIVATE DEFINE v_folio_procesar        VARCHAR(50)
PRIVATE DEFINE v_id_sistema            CHAR(10)
PRIVATE DEFINE v_id_ebusiness          CHAR(10)
PRIVATE DEFINE v_id_portafolio         CHAR(10)
PRIVATE DEFINE v_id_servicio           CHAR(10)
PRIVATE DEFINE v_id_cliente            CHAR(10)
PRIVATE DEFINE v_id_canal              CHAR(10)
PRIVATE DEFINE v_cod_oper_cliente      CHAR(50)
PRIVATE DEFINE v_folio_transaccion     CHAR(50)
PRIVATE DEFINE v_cod_respuesta_opr     CHAR(2)
PRIVATE DEFINE v_id_proceso            VARCHAR(3)
PRIVATE DEFINE v_id_operacion          VARCHAR(4)
PRIVATE DEFINE v_rechazos_solicitud    DYNAMIC ARRAY OF rechazos
PRIVATE DEFINE v_parametros_negocio    DYNAMIC ARRAY OF detalle_request
PRIVATE DEFINE v_nss                   CHAR(11)
PRIVATE DEFINE v_curp                  CHAR(18)

#Variables de salida
PRIVATE DEFINE v_r_folio_Ack           VARCHAR(50)
PRIVATE DEFINE v_r_cod_respuesta       CHAR(2)
PRIVATE DEFINE v_r_cod_respuesta_opr   CHAR(2)
PRIVATE DEFINE v_r_desc_respuesta      VARCHAR(100)
PRIVATE DEFINE v_r_cod_oper_cliente    CHAR(50)
PRIVATE DEFINE v_r_rechazos_respuesta  DYNAMIC ARRAY OF rechazos

#Variables para el manejo de las validaciones
PRIVATE DEFINE v_id_cat_bus_contrato   DECIMAL(9,0)
PRIVATE DEFINE v_id_bus_tramite        DECIMAL(9,0)

#Variables para el manejo de la bitacora
PRIVATE DEFINE v_id_bus_solicitud_tramite        DECIMAL(9,0)

PRIVATE DEFINE v_nombre_archivo        STRING
PRIVATE DEFINE v_archivo_correo        BASE.CHANNEL
PRIVATE DEFINE v_ind_correo            SMALLINT

MAIN
   #Primero se recuperan los parametros
   LET v_id_mensaje     = ARG_VAL(1)
   LET v_folio_procesar = ARG_VAL(2) CLIPPED
   LET v_id_proceso    = ARG_VAL(3)
   LET v_id_operacion  = ARG_VAL(4)
   
   CALL fn_prepara_consulta()

   DISPLAY "Enviando mensaje por cliente generico..."

   CALL fn_envia_solicitud()
END MAIN

FUNCTION fn_prepara_consulta()
   DEFINE v_fn_genera_folio_bus              STRING
   DEFINE v_fn_genera_tramite                STRING
   DEFINE v_fn_inserta_solicitud_tramite     STRING
   DEFINE v_actualiza_nss                    STRING
   DEFINE v_inserta_error_solicitud          STRING
   DEFINE v_seq_bus_detalle_solicitud        STRING
   DEFINE v_inserta_detalle_solicitud        STRING
   DEFINE v_fn_inserta_respuesta_tramite     STRING
   DEFINE v_inserta_error_respuesta          STRING
   DEFINE v_consulta_encabezado              STRING
   DEFINE v_consulta_rechazos                STRING
   DEFINE v_consulta_xml                     STRING
   DEFINE v_valida_arreglo                   STRING
   DEFINE v_consulta_nombre_arreglo          STRING
   DEFINE v_consulta_detalle_bloque          STRING
   DEFINE v_inserta_detalle_bloque           STRING

   DEFINE v_inserta_excepcion_envio          STRING

   LET v_fn_genera_folio_bus = "EXECUTE FUNCTION fn_genera_folio_bus(?,?)"
   PREPARE exe_fn_genera_folio_bus FROM v_fn_genera_folio_bus

   LET v_fn_genera_tramite = "EXECUTE FUNCTION fn_genera_tramite(?,?,?,?)"
   PREPARE exe_fn_genera_tramite FROM v_fn_genera_tramite

   LET v_fn_inserta_solicitud_tramite = "EXECUTE FUNCTION fn_inserta_solicitud_tramite(?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_fn_inserta_solicitud_tramite FROM v_fn_inserta_solicitud_tramite

   LET v_actualiza_nss = "UPDATE bus_solicitud_tramite set nss = ?, curp = ? WHERE id_bus_solicitud_tramite = ?"
   PREPARE exe_actualiza_nss FROM v_actualiza_nss

   LET v_inserta_error_solicitud = "INSERT INTO bus_error_solicitud (id_bus_error_solicitud,id_bus_solicitud_tramite,cod_error,desc_error) ",
                                    "VALUES (seq_bus_error_solicitud.NEXTVAL,?,?,?)"
   PREPARE exe_inserta_error_solicitud FROM v_inserta_error_solicitud

   LET v_seq_bus_detalle_solicitud = "SELECT seq_bus_detalle_solicitud.NEXTVAL from systables WHERE tabid=1"
   PREPARE exe_seq_bus_detalle_solicitud FROM v_seq_bus_detalle_solicitud

   LET v_inserta_detalle_solicitud = "INSERT INTO bus_detalle_solicitud (id_bus_solicitud_detalle,id_bus_solicitud_tramite,nombre_campo,valor) ",
                                    "VALUES (?,?,?,?)"
   PREPARE exe_inserta_detalle_solicitud FROM v_inserta_detalle_solicitud

   LET v_fn_inserta_respuesta_tramite = "EXECUTE FUNCTION fn_inserta_respuesta_tramite(?,?,?,?,?,?,?)"
   PREPARE exe_fn_inserta_respuesta_tramite FROM v_fn_inserta_respuesta_tramite

   LET v_inserta_error_respuesta = "INSERT INTO bus_error_respuesta (id_bus_error_respuesta,id_bus_respuesta_tramite,cod_error,desc_error) ",
                                    "VALUES (seq_bus_error_respuesta.NEXTVAL,?,?,?)"
   PREPARE exe_inserta_error_respuesta FROM v_inserta_error_respuesta

   #LET v_consulta_detalle_contrato = "SELECT cve_natural, tipo_dato FROM cat_bus_detalle_contrato  WHERE id_cat_bus_contrato = ? ORDER BY orden"
   #PREPARE exe_consulta_detalle_contrato FROM v_consulta_detalle_contrato

   LET v_consulta_encabezado =   "SELECT ",
                                 "cza.id_sistema, ",
                                 "cza.id_ebusiness, ",
                                 "cza.id_portafolio, ",
                                 "cza.id_servicio, ",
                                 "cza.id_cliente, ",
                                 "cza.id_canal, ",
                                 "cza.url_servicio, ",
                                 "cza.num_reintento, ",
                                 "cza.intervalo ",
                                 "FROM cat_bus_contrato cntr ",
                                 "INNER JOIN cat_bus_cza_operacion cza ON cza.id_cat_bus_operacion = cntr.id_cat_bus_operacion ",
                                 "WHERE cntr.id_cat_bus_contrato = ?"
   PREPARE exe_consulta_encabezado FROM v_consulta_encabezado

   LET v_consulta_rechazos = "SELECT cod_error, desc_error FROM bus_tmp_error_solicitud WHERE id_bus_tmp_solicitud_detalle = ?"
   PREPARE exe_consulta_rechazos FROM v_consulta_rechazos
   
   LET v_consulta_xml = "SELECT orden, nombre_campo, valor FROM bus_tmp_detalle_solicitud WHERE id_bus_tmp_solicitud_detalle = ? ORDER BY orden ASC"
   PREPARE exe_consulta_xml FROM v_consulta_xml

   LET v_inserta_excepcion_envio = "INSERT INTO bus_excepcion (id_bus_solicitud_tramite,estado, descripcion) VALUES (?,?,?)"
   PREPARE exe_inserta_excepcion_envio FROM v_inserta_excepcion_envio

   LET v_valida_arreglo = "SELECT FIRST 1 ",
                           "blo.id_bus_agrupa_bloque ",
                           "FROM bus_tmp_detalle_bloque blo ",
                           "INNER JOIN bus_tmp_detalle_solicitud det ON (det.id_bus_tmp_solicitud_detalle = blo.id_bus_tmp_solicitud_detalle AND det.orden = blo.consecutivo_padre) ",
                           "WHERE blo.id_bus_tmp_solicitud_detalle = ? ",
                           "AND blo.consecutivo_padre = ?"
   PREPARE exe_valida_arreglo FROM v_valida_arreglo

   LET v_consulta_nombre_arreglo = "SELECT ",
                                    "det.cve_natural, ",
                                    "blo.cve_natural ",
                                    "FROM cat_bus_detalle_contrato det ",
                                    "INNER JOIN cat_bus_bloque blo ON blo.id_cat_bus_detalle_contrato = det.id_cat_bus_detalle_contrato ",
                                    "WHERE id_cat_bus_contrato = ? ",
                                    "AND det.cve_natural = ? "
   PREPARE exe_consulta_nombre_arreglo FROM v_consulta_nombre_arreglo

   LET v_inserta_detalle_bloque = "INSERT INTO bus_detalle_bloque (id_bus_detalle_bloque,id_bus_solicitud_detalle,id_bus_agrupa_bloque,nombre_campo_bloque,valor) ",
                                  "VALUES (seq_bus_detalle_bloque.NEXTVAL,?,?,?,?);"
   PREPARE exe_inserta_detalle_bloque FROM v_inserta_detalle_bloque

   LET v_consulta_detalle_bloque = "SELECT ",
                                    "id_bus_agrupa_bloque, ",
                                    "nombre_campo_bloque, ",
                                    "valor, ",
                                    "orden ",
                                    "FROM bus_tmp_detalle_bloque ",
                                    "WHERE id_bus_tmp_solicitud_detalle = ? ",
                                    "AND consecutivo_padre = ? ",
                                    "ORDER BY id_bus_agrupa_bloque, orden"
   PREPARE exe_consulta_detalle_bloque FROM v_consulta_detalle_bloque 
END FUNCTION

PRIVATE FUNCTION fn_envia_solicitud()
   DEFINE ind_rechazo            SMALLINT
   DEFINE i                      SMALLINT

   DEFINE v_cod_rechazo          VARCHAR(6)
   DEFINE v_desc_rechazo         VARCHAR(100)

   INITIALIZE v_rechazos_solicitud     TO NULL 
   INITIALIZE v_r_rechazos_respuesta   TO NULL 

   INITIALIZE v_id_bus_tramite         TO NULL
   INITIALIZE v_id_cat_bus_contrato    TO NULL

   INITIALIZE v_r_folio_Ack            TO NULL
   INITIALIZE v_r_cod_respuesta        TO NULL
   INITIALIZE v_r_cod_respuesta_opr    TO NULL
   INITIALIZE v_r_desc_respuesta       TO NULL

   WHENEVER ERROR CONTINUE
   #Primero se valida que el proceso y operacion son validos
   DISPLAY "Registrando en bitacora..."
   DISPLAY "v_id_proceso = ", v_id_proceso
   DISPLAY "v_id_operacion = ", v_id_operacion
   DISPLAY "v_folio_procesar = ", v_folio_procesar
   DISPLAY "ORIGEN_AFORE = ", ORIGEN_AFORE 
   
   EXECUTE exe_fn_genera_tramite USING v_id_proceso,
                                       v_id_operacion,
                                       v_folio_procesar,
                                       ORIGEN_AFORE
                                 INTO  v_id_bus_tramite,
                                       v_id_cat_bus_contrato
   IF SQLCA.SQLCODE <> 0 THEN
      DISPLAY "Error al generar el folio..."
      CALL fn_redacta_correo("Ocurrio un error al intentar registrar el tramite en la bitacora del Bus, \n " ||
                              "Favor de consultar con el administrador")
      CALL fn_envia_archivo_correo()
   ELSE     #FIN VALIDACION DE TRAMITE CON ERROR
      #Se valida que el resultado de la funcion que genera el tramite encontro la configuracion de la operacion solicitada
      IF v_id_bus_tramite > 0 AND v_id_cat_bus_contrato > 0 THEN
         #Generamos un folio de transaccion
         EXECUTE exe_fn_genera_folio_bus USING v_id_proceso, v_id_operacion
                                         INTO v_folio_transaccion
         LET v_folio_transaccion = v_folio_transaccion CLIPPED
         LET ns1notificarTramiteRequest.cuerpo.folioDeTransaccion = v_folio_transaccion CLIPPED
         #Llenamos el encabezado del mensaje
         EXECUTE exe_consulta_encabezado  USING v_id_cat_bus_contrato
                                          INTO  v_id_sistema,
                                                v_id_ebusiness,
                                                v_id_portafolio,
                                                v_id_servicio,
                                                v_id_cliente,
                                                v_id_canal,
                                                v_url,
                                                v_num_reintento,
                                                v_intervalo
                                                
         LET ns1notificarTramiteRequest.idssn.idCanal          = v_id_canal
         LET ns1notificarTramiteRequest.idssn.idCliente        = v_id_cliente
         LET ns1notificarTramiteRequest.idssn.idEbusiness      = v_id_ebusiness
         LET ns1notificarTramiteRequest.idssn.idPortafolio     = v_id_portafolio
         LET ns1notificarTramiteRequest.idssn.idServicio       = v_id_servicio
         LET ns1notificarTramiteRequest.idssn.idSistema        = v_id_sistema
         LET ns1notificarTramiteRequest.idssn.codoperCliente   = COD_OPER_CLIENTE CLIPPED
         LET ns1notificarTramiteRequest.idssn.fecha            = CURRENT YEAR TO FRACTION(5)
         
         #Se inserta la solicitud que se enviara a procesar
         EXECUTE exe_fn_inserta_solicitud_tramite USING  v_id_bus_tramite,
                                                         v_id_sistema,
                                                         v_id_ebusiness,    
                                                         v_id_portafolio,   
                                                         v_id_servicio,
                                                         v_id_cliente,
                                                         v_id_canal,
                                                         v_cod_oper_cliente,
                                                         v_folio_transaccion,
                                                         v_cod_respuesta_opr
                                                   INTO  v_id_bus_solicitud_tramite

         #Se llena el cuerpo del mensaje
         LET ns1notificarTramiteRequest.cuerpo.folioDeTramiteProcesar = v_folio_procesar CLIPPED
         LET ns1notificarTramiteRequest.cuerpo.idProceso = v_id_proceso
         LET ns1notificarTramiteRequest.cuerpo.idOperacion = v_id_operacion
         
         #Se buscan posibles rechazos enviados por el negocio, en caso de encontrar rechazos no se genera el campo notificacionXML
         LET ind_rechazo = 0
         DECLARE cur_consulta_rechazos CURSOR FOR exe_consulta_rechazos
         FOREACH cur_consulta_rechazos USING v_id_mensaje INTO v_cod_rechazo, v_desc_rechazo
            IF v_cod_rechazo IS NOT NULL THEN
               CALL v_rechazos_solicitud.appendElement()
               LET v_rechazos_solicitud[1].cod_rechazo = v_cod_rechazo
               LET v_rechazos_solicitud[1].desc_rechazo = v_desc_rechazo
               CALL ns1notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo.appendElement()
               LET ns1notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo[
                     ns1notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo.getLength()
                                                                                 ].idMotivoRechazo = v_cod_rechazo
               LET ns1notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo[
                     ns1notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo.getLength()
                                                                                 ].descripcionRechazo = v_desc_rechazo
               EXECUTE exe_inserta_error_solicitud USING v_id_bus_solicitud_tramite,
                                                         v_cod_rechazo,
                                                         v_desc_rechazo
            END IF
            LET i = i + 1
            LET ind_rechazo = 1
         END FOREACH

         #Si no se encuentran rechazos se arma el mensaje XML
         IF ind_rechazo = 0 THEN
            INITIALIZE v_nss TO NULL
            INITIALIZE v_curp TO NULL
            CALL fn_genera_notificacion_xml()
            IF (v_nss IS NOT NULL OR v_curp IS NOT NULL) THEN
               EXECUTE exe_actualiza_nss USING v_nss, v_curp, v_id_bus_solicitud_tramite
            END IF
            LET ns1notificarTramiteRequest.cuerpo.codRespuestaOpr = COD_OPR_OK
         ELSE
            LET ns1notificarTramiteRequest.cuerpo.codRespuestaOpr = COD_OPR_ERROR
         END IF
         
         #Con los datos llenos en el mensaje y gusrdados en la bitacora se envia el mensaje al bus
         CALL fn_envia_mensaje_bus()
         CALL fn_limpiar_bandeja()
         
      ELSE     #FIN CONFIGURACION DE TRAMITE CORRECTA
         #Aqui se envia correo avisando que el proceso, operacion no son validos
         DISPLAY "Error al registrar en bitacora"
         DISPLAY "Validar la ejecucion de la funcion fn_genera_tramite en BD"
         CALL fn_redacta_correo("Ocurrió un error al intentar registrar el trámite en la bitácora del Bus \n " ||
                              "porque la combinacion de proceso y operacion no son validos")
         CALL fn_redacta_correo("id_proceso: " || v_id_proceso)
         CALL fn_redacta_correo("id_operacion: " || v_id_operacion)
         CALL fn_redacta_correo("Folio Procesar: " || v_folio_procesar)
         CALL fn_envia_archivo_correo()
      END IF   #FIN CONFIGURACION DE TRAMITE INCORRECTA
   END IF   #FIN VALIDACION DE TRAMITE SIN ERROR
   
   WHENEVER ERROR STOP
END FUNCTION

PRIVATE FUNCTION fn_genera_notificacion_xml()
   DEFINE i                            SMALLINT
   DEFINE doc                          xml.DomDocument
   DEFINE elt                          xml.DomNode
   DEFINE v_notificacionXML            STRING
   DEFINE v_arregloXML                 STRING

   DEFINE v_id_bus_solicitud_detalle   DECIMAL(9,0)
   DEFINE v_id_bus_agrupa_bloque       DECIMAL(9,0)
   DEFINE v_ind_arreglo                SMALLINT
   DEFINE v_num_caracter               INTEGER
   DEFINE v_num_campo                  INTEGER
   DEFINE v_nombre_campo               VARCHAR(60)
   DEFINE v_valor                      VARCHAR(100)

   #Se crea el documento con el elemento root 
   LET doc = xml.DomDocument.CreateDocument("map")

   #obtenemos el nodo principal
   LET elt = doc.getDocumentElement()
   LET i = 1
   DECLARE cur_consulta_xml CURSOR FOR exe_consulta_xml
   FOREACH cur_consulta_xml USING v_id_mensaje INTO v_num_campo, v_nombre_campo, v_valor
      IF v_nombre_campo IS NOT NULL THEN
      
         CALL v_parametros_negocio.appendElement()
         LET v_parametros_negocio[i].num_campo = v_num_campo
         LET v_parametros_negocio[i].nombre_campo = v_nombre_campo
         LET v_ind_arreglo = 0

         #Primero se valida si el campo corresponde a un arreglo o a un campo normal
         EXECUTE exe_valida_arreglo USING v_id_mensaje, v_num_campo INTO v_id_bus_agrupa_bloque
         IF v_id_bus_agrupa_bloque IS NOT NULL AND v_id_bus_agrupa_bloque > 0 THEN
            #Se trata de un arreglo
            LET v_valor = "Arreglo de ", v_nombre_campo
            LET v_ind_arreglo = 1
         END IF
         LET v_parametros_negocio[i].valor = v_valor
         

         EXECUTE exe_seq_bus_detalle_solicitud INTO v_id_bus_solicitud_detalle
         
         EXECUTE exe_inserta_detalle_solicitud USING  v_id_bus_solicitud_detalle,
                                                      v_id_bus_solicitud_tramite,
                                                      v_nombre_campo,
                                                      v_valor

         IF v_ind_arreglo = 0 THEN
            #Se agrega un campo normal a la respuesta
            CALL elt.appendChild(fn_genera_nodo(v_nombre_campo,v_valor, doc))
         ELSE
            #Se genera el arreglo
            CALL fn_genera_arreglo(v_id_bus_solicitud_detalle, v_nombre_campo, v_num_campo) RETURNING v_arregloXML
            CALL elt.appendChild(fn_genera_nodo(v_nombre_campo,v_arregloXML, doc))
         END IF
          #Se agrega la separacion del nss y curp
         IF v_nombre_campo = "nss" THEN
            LET v_nss = v_valor
         END IF
         IF v_nombre_campo = "curp" THEN
            LET v_curp = v_valor
         END IF
      END IF
      LET i = i + 1
   END FOREACH

   #obtenemos el xml en una cadena para mandar la respuesta
   CALL doc.saveToString() RETURNING v_notificacionXML
   CALL v_notificacionXML.getIndexOf('?', 5) RETURNING v_num_caracter
   LET v_num_caracter = v_num_caracter + 2
   CALL v_notificacionXML.subString(v_num_caracter, v_notificacionXML.getLength()) RETURNING v_notificacionXML
   LET ns1notificarTramiteRequest.cuerpo.notificacionXML = v_notificacionXML
END FUNCTION

PRIVATE FUNCTION fn_genera_arreglo(p_id_bus_solicitud_detalle, p_nombre_padre, p_num_campo_padre)
   DEFINE p_id_bus_solicitud_detalle   DECIMAL(9,0)
   DEFINE p_nombre_padre               VARCHAR(60)
   DEFINE p_num_campo_padre            INTEGER

   DEFINE v_num_bloque                 SMALLINT
   DEFINE v_nombre_raiz                VARCHAR(60)
   DEFINE v_nombre_elemento            VARCHAR(60)
   DEFINE v_num_elemento               DECIMAL(9,0)

   DEFINE v_id_bus_agrupa_bloque       DECIMAL(9,0)
   DEFINE v_nombre_campo_bloque        VARCHAR(60)
   DEFINE v_valor_bloque               VARCHAR(100)
   DEFINE v_num_campo_bloque           INTEGER
   DEFINE v_arreglo                    STRING

   DEFINE v_doc                        xml.DomDocument
   DEFINE v_nodo_padre                 xml.DomNode   
   DEFINE v_item                       xml.DomNode

   LET v_num_bloque = 1
   
   EXECUTE exe_consulta_nombre_arreglo USING v_id_cat_bus_contrato, p_nombre_padre INTO v_nombre_raiz, v_nombre_elemento
   LET v_nombre_raiz = v_nombre_raiz CLIPPED
   LET v_nombre_elemento = v_nombre_elemento CLIPPED

   EXECUTE exe_inserta_detalle_bloque USING p_id_bus_solicitud_detalle, v_num_bloque, NOMBRE_ARREGLO, v_nombre_raiz
   EXECUTE exe_inserta_detalle_bloque USING p_id_bus_solicitud_detalle, v_num_bloque, NOMBRE_ELEMENTO, v_nombre_elemento

   #Primero se genera la raiz del XML
   LET v_doc = xml.DomDocument.CreateDocument(v_nombre_raiz)
   LET v_nodo_padre = v_doc.getDocumentElement()

   LET v_num_elemento = 0
   DECLARE cur_consulta_detalle_bloque CURSOR FOR exe_consulta_detalle_bloque
   FOREACH cur_consulta_detalle_bloque USING v_id_mensaje, p_num_campo_padre INTO v_id_bus_agrupa_bloque, v_nombre_campo_bloque, v_valor_bloque, v_num_campo_bloque
      IF v_num_elemento <> v_id_bus_agrupa_bloque THEN
         #Significa que se encontro un nuevo bloque
         LET v_num_elemento = v_id_bus_agrupa_bloque
         LET v_num_bloque = v_num_bloque + 1

         #Se genera un nuevo nodo para el elemento
         LET v_item = v_doc.createElement(v_nombre_elemento)
         CALL v_nodo_padre.appendChild(v_item)
      END IF
      #Agregamos el nuevo campo al elemento
      EXECUTE exe_inserta_detalle_bloque USING p_id_bus_solicitud_detalle, v_num_bloque, v_nombre_campo_bloque, v_valor_bloque
     CALL v_item.appendChild(fn_genera_nodo_arreglo(v_nombre_campo_bloque, v_valor_bloque, v_doc))
   END FOREACH
   LET v_arreglo = "<![CDATA[",v_nodo_padre.toString(), "]]>"
   RETURN v_arreglo
END FUNCTION

PRIVATE FUNCTION fn_genera_nodo_arreglo(p_nombre, p_valor, p_doc)
   DEFINE p_nombre      STRING
   DEFINE p_valor       STRING
   DEFINE p_doc         xml.DomDocument
   DEFINE node          xml.DomNode
   DEFINE nodo          STRING

   DEFINE v_valor_especial    STRING
   DEFINE v_especial          INTEGER
   
   TRY
      LET nodo = "<", p_nombre CLIPPED , ">", p_valor CLIPPED ,"</", p_nombre CLIPPED , ">"
      LET node = p_doc.createNode(nodo)
   CATCH
      #Significa que la cadena tiene un caracter especial
      LET v_especial = p_valor.getIndexOf('&', 1)
      IF v_especial IS NOT NULL AND v_especial <> -1 THEN
         LET v_valor_especial = p_valor.subString(1,v_especial - 1)
         LET v_valor_especial = v_valor_especial.append('$')
         LET v_valor_especial = v_valor_especial.append(p_valor.subString(v_especial + 1, p_valor.getLength()))
         RETURN fn_genera_nodo_arreglo(p_nombre, v_valor_especial, p_doc)
      END IF
      
   END TRY
   RETURN node
END FUNCTION

PRIVATE FUNCTION fn_genera_nodo(p_nombre, p_valor, p_doc)
   DEFINE p_nombre      STRING
   DEFINE p_valor       STRING
   DEFINE p_doc         xml.DomDocument
   DEFINE node          xml.DomNode
   DEFINE nodo          STRING
   TRY
      LET p_valor = p_valor CLIPPED
      IF p_valor IS NULL OR p_valor.getLength() = 0 THEN
         DISPLAY "******************************************"
         DISPLAY "Campo Vacio: ", p_nombre
         DISPLAY "******************************************"
         LET nodo = "<entry><key>", p_nombre CLIPPED, "</key><value> </value></entry>"
      ELSE
         LET nodo = "<entry><key>", p_nombre CLIPPED, "</key><value>", p_valor, "</value></entry>"
      END IF
      LET node = p_doc.createNode(nodo)
   CATCH
      #Significa que la cadena tiene un caracter especial
   END TRY
   RETURN node
END FUNCTION

PRIVATE FUNCTION fn_envia_mensaje_bus()
   DEFINE wsStatus            INTEGER
   DEFINE v_num_intento       INTEGER
   DEFINE v_desc_error        VARCHAR(255)

   #Se asigna la URL del servicio a consumir
   IF v_url IS NULL OR v_url CLIPPED == "" THEN
      #Si el URL no es valido es necesario enviar un correo
      DISPLAY "URL nula..."
      CALL fn_redacta_correo("No es posible enviar el mensaje por el Bus de Tramites porque la operación no fue correctamente configurada en el catalogo \n " ||
                             "Favor de comunicarse con el administrador del sistema para solucionar el problema y se pueda solicitar un reenvió del mensaje")
      CALL fn_envia_archivo_correo()
      #Guardamos el registro en la tabla de envios fallidos para tener el control
      LET v_desc_error = "Operación sin URL destino configurada"
      EXECUTE exe_inserta_excepcion_envio USING v_id_bus_solicitud_tramite,
                                                COD_ERROR_ENVIO,
                                                v_desc_error
      RETURN
   END IF
   LET BusTramites_BusTramitesPortTypeLocation = v_url

   LET v_num_intento = 0
   WHILE v_num_intento <= v_num_reintento
      
      CALL notificarTramite_g() RETURNING wsStatus
      IF wsStatus = 0 THEN
        --Se ejecuto correctamente el WS
        CALL fn_procesa_respuesta()
        EXIT WHILE
      ELSE
          --El WS no se ejecuto correctamente
         IF v_num_intento = v_num_reintento THEN

            #Guardamos el registro en la tabla de envios fallidos para tener el control
            LET v_desc_error = wsError.description
            EXECUTE exe_inserta_excepcion_envio USING v_id_bus_solicitud_tramite,
                                                      COD_ERROR_ENVIO,
                                                      v_desc_error
            
            #Si se llega al numero maximos de reintentos se tiene que mandar el correo
            CALL fn_redacta_correo("Ocurrió un error de comunicación con el destinatario del mensaje con folio procesar " || v_folio_procesar)
            CALL fn_redacta_correo("el servicio de notificación genérico no está disponible por lo que no se recibió la respuesta de la transacción \n")
            IF v_intervalo IS NOT NULL AND v_intervalo > 0 THEN
               CALL fn_redacta_correo("El bus de trámites ejecuto " || v_num_reintento || " reintentos con intervalo de " || v_intervalo || " segundo(s) \n\n")
            ELSE
               CALL fn_redacta_correo("El bus de trámites ejecuto " || v_num_reintento || " reintentos consecutivos \n\n")
            END IF
            CALL fn_redacta_correo("Favor de intentar el envío del trámite cuando el proveedor notifique que el servicio esta disponible")
            CALL fn_envia_archivo_correo()
            EXIT WHILE
         ELSE
            #si aun no se llega al numero maximo de intentos configurados se 
            #incrementa el contador y se reintenta la peticion
            LET v_num_intento = v_num_intento + 1
            IF v_intervalo IS NOT NULL AND v_intervalo > 0 THEN
               SLEEP v_intervalo
            END IF
         END IF
      END IF

   END WHILE
END FUNCTION

PRIVATE FUNCTION fn_procesa_respuesta()
   DEFINE i                               SMALLINT
   DEFINE v_id_bus_respuesta_tramite      DECIMAL(9,0)
   DEFINE v_ind_error                     SMALLINT

   LET v_ind_error = 0
   LET v_r_cod_respuesta = ns1notificarTramiteResponse.ssnrop.codRespuesta
   LET v_r_cod_respuesta_opr = ns1notificarTramiteResponse.ssnrop.codRespuestaOpr
   LET v_r_desc_respuesta = ns1notificarTramiteResponse.ssnrop.descRespuesta
   LET v_r_cod_oper_cliente = ns1notificarTramiteResponse.ssnrop.codoper
   LET v_r_folio_Ack = ns1notificarTramiteResponse.objetoRespuesta.folioAck

   #Se inserta la respuesta en la bitacora
   EXECUTE exe_fn_inserta_respuesta_tramite USING  v_id_bus_tramite,
                                                      v_id_bus_solicitud_tramite,
                                                      v_r_cod_respuesta,
                                                      v_r_cod_respuesta_opr,
                                                      v_r_desc_respuesta,
                                                      v_r_cod_oper_cliente,
                                                      v_r_folio_Ack
                                                INTO  v_id_bus_respuesta_tramite

   #Si existen rechazos en el mensaje de respuesta se insertan en la bitacora
   FOR i = 1 TO ns1notificarTramiteResponse.ssnrop.motivos.motivo.getLength()
      LET v_ind_error = 1
      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[i].cod_rechazo = ns1notificarTramiteResponse.ssnrop.motivos.motivo[i].idMotivo
      LET v_r_rechazos_respuesta[i].desc_rechazo = ns1notificarTramiteResponse.ssnrop.motivos.motivo[i].descripcion
      IF v_r_rechazos_respuesta[i].cod_rechazo IS NOT NULL THEN
         EXECUTE exe_inserta_error_respuesta USING v_id_bus_respuesta_tramite, 
                                                   v_r_rechazos_respuesta[i].cod_rechazo, 
                                                   v_r_rechazos_respuesta[i].desc_rechazo
      END IF
   END FOR

   IF v_r_folio_Ack IS NULL OR v_r_folio_Ack CLIPPED == "" THEN
      #Si no llego folio ACK se envia correo avisando al responsable
      #Se envia correo avisando al responsable que el dueño del sevicio regreso rechazos al mensaje enviado
      CALL fn_redacta_correo("El mensaje se envió correctamente pero el destinatario no regreso folio de acuse \n")

      IF v_ind_error = 1 THEN
         CALL fn_redacta_correo("El mensaje de respuesta contiene la siguiente lista de rechazos: \n")
         FOR i = 1 TO v_r_rechazos_respuesta.getLength()
            CALL fn_redacta_correo(v_r_rechazos_respuesta[i].cod_rechazo || " - " || v_r_rechazos_respuesta[i].desc_rechazo)
         END FOR
      END IF
      
      CALL fn_envia_archivo_correo()
   END IF
END FUNCTION

PRIVATE FUNCTION fn_limpiar_bandeja()

   DELETE FROM bus_tmp_detalle_bloque WHERE id_bus_tmp_solicitud_detalle = v_id_mensaje
   
   DELETE FROM bus_tmp_detalle_solicitud WHERE id_bus_tmp_solicitud_detalle = v_id_mensaje

   DELETE FROM bus_tmp_error_solicitud WHERE id_bus_tmp_solicitud_detalle = v_id_mensaje
END FUNCTION

PRIVATE FUNCTION fn_redacta_correo(p_mensaje)
   DEFINE p_mensaje                    STRING

   IF v_ind_correo = 0 THEN
      CALL fn_inicializa_correo()
      LET v_ind_correo = 1
   END IF
   CALL v_archivo_correo.writeLine(p_mensaje)
END FUNCTION

PRIVATE FUNCTION fn_inicializa_correo()
   DEFINE v_desc_proceso_bus           VARCHAR(50)
   DEFINE v_desc_opera_bus             VARCHAR(50)

   DEFINE v_ruta_correo                VARCHAR(255)
   
   SELECT
      proc.desc_proceso_bus,
      opr.desc_opera_bus
   INTO
      v_desc_proceso_bus,
      v_desc_opera_bus
   FROM cat_bus_proceso proc
   INNER JOIN cat_bus_operacion opr ON opr.id_cat_bus_proceso = proc.id_cat_bus_proceso
   WHERE proc.cod_proceso_bus = v_id_proceso
   AND opr.cod_opera_bus = v_id_operacion

   SELECT ruta_listados
   INTO v_ruta_correo
   FROM seg_modulo
   WHERE modulo_cod = 'bus'
   
   #Se asigna el nombre del archivo
   LET v_nombre_archivo = v_ruta_correo CLIPPED, "/CORREO_" , v_folio_procesar CLIPPED, "_", TODAY USING "ddmmyyyy",".txt"

   -- se crea el manejador de archivo
   LET v_archivo_correo = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo_correo.openFile(v_nombre_archivo, "w" )
   CALL v_archivo_correo.setDelimiter("")

   CALL v_archivo_correo.writeLine("")
   CALL v_archivo_correo.writeLine("Recibes este email porque estás registrado en el sistema del Bus de trámites como responsable del siguiente proceso:")
   CALL v_archivo_correo.writeLine("")
   CALL v_archivo_correo.writeLine("Proceso: " || v_desc_proceso_bus)
   CALL v_archivo_correo.writeLine("Operación: "|| v_desc_opera_bus)
   CALL v_archivo_correo.writeLine("")

END FUNCTION

PRIVATE FUNCTION fn_envia_archivo_correo()
   IF v_ind_correo = 1 THEN
      CALL v_archivo_correo.writeLine("")
      CALL v_archivo_correo.writeLine("")
      CALL v_archivo_correo.writeLine("===================================================================================================================")
      CALL v_archivo_correo.writeLine("AVISO DE CONFIDENCIALIDAD Y ALCANCE DE CONTENIDO")
      CALL v_archivo_correo.writeLine("")
      CALL v_archivo_correo.writeLine("La información contenida en este mensaje de correo electrónico es confidencial y restringida")
      CALL v_archivo_correo.writeLine("y esta destinada únicamente para el uso de la o las personas a que esta dirigido,")
      CALL v_archivo_correo.writeLine("por lo que se le notifica que esta estrictamente prohibido cualquier difusión, distribución o copia del mismo.")
      CALL v_archivo_correo.writeLine("")
      CALL v_archivo_correo.writeLine("Si ha recibido este mensaje de correo electrónico por error, debe destruirlo y notificar al remitente por esta misma vía.")
      CALL v_archivo_correo.close()
      CALL fn_envia_correo(v_id_proceso, v_id_operacion, NULL, v_nombre_archivo, NULL)
   END IF
END FUNCTION
