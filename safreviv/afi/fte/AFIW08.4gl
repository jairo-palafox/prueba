####################################################################
#Proyecto          => SACI VIVIENDA                                #
#Propietario       => E.F.P.                                       #
#------------------------------------------------------------------#
#Modulo            => AFI                                          #
#Programa          => AFIW08                                       #
#Objetivo          => Funcion general para consultar la CURP en    #
#                     RENAPO                                       #
#Fecha Inicio      => 07 - Septiembre - 2017                       #
####################################################################
IMPORT com
IMPORT XML

GLOBALS "AFIW07.inc"
GLOBALS "AFIW08.inc"

PUBLIC FUNCTION fn_consulta_curp_renapo(p_curp)
   DEFINE p_curp           CHAR(18)
   DEFINE v_num_intento    INTEGER
   DEFINE wsStatus         INTEGER
   #DEFINE v_respuesta      STRING
   DEFINE v_respuesta       respuesta_renapo

   INITIALIZE ns1consultarPorCurp.datos.* TO NULL

   IF p_curp IS NOT NULL THEN
      LET v_num_intento = 0
   
      LET ns1consultarPorCurp.datos.cveCurp           = p_curp
      LET ns1consultarPorCurp.datos.cveEntidadEmisora = ENTIDAD_EMISORA
      LET ns1consultarPorCurp.datos.direccionIp       = IP_CONSULTA
      LET ns1consultarPorCurp.datos.tipoTransaccion   = TIPO_TRANSACCION
      LET ns1consultarPorCurp.datos.usuario           = USUARIO
      LET ns1consultarPorCurp.datos.password          = PASSWORD

      LET ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint.Address.Uri = URL

      WHILE v_num_intento <= INTENTOS
         CALL consultarPorCurp_g() RETURNING wsStatus

         IF wsStatus == 0 THEN
           --Se ejecuto correctamente el WS

           CALL fn_procesa_respuesta(ns1consultarPorCurpResponse.return) RETURNING v_respuesta.*
           EXIT WHILE
         ELSE
            IF v_num_intento = INTENTOS THEN
               INITIALIZE v_respuesta.* TO NULL
               LET v_respuesta.status_operacion = "ERROR DE COMUNICACIÓN: ", wsError.description
               LET v_respuesta.desc_status = "EL SERVICIO NO SE PUDO CONSUMIR DESPUÉS DE ", v_num_intento, " INTENTOS"
            ELSE
               #si aun no se llega al numero maximo de intentos configurados se
               #incrementa el contador y se reintenta la peticion
               LET v_num_intento = v_num_intento + 1
            END IF
         END IF
      END WHILE
   ELSE
      INITIALIZE v_respuesta.* TO NULL
      LET v_respuesta.status_operacion = "ERROR DE VALIDACIÓN"
      LET v_respuesta.desc_status = "EL CAMPO DE CURP NO PUEDE SER NULO"
   END IF
   
   RETURN v_respuesta.*
END FUNCTION

PRIVATE FUNCTION fn_procesa_respuesta(p_entrada)
   DEFINE p_entrada         STRING
   DEFINE v_respuesta       respuesta_renapo

   DEFINE v_doc                     xml.DomDocument
   DEFINE v_nodo_padre              xml.DomNode
   DEFINE v_item                    xml.DomNode
   #DEFINE v_nodo_xml                xml.DomNode

   DEFINE v_campo                   STRING
   DEFINE v_valor                   STRING
   
   DEFINE v_num_elementos           INTEGER
   DEFINE i                         INTEGER
   DEFINE v_num_item                INTEGER

   TRY 
      #Se carga la respuesta en la libreria XML para que se valide la estructura
      LET v_doc = xml.DomDocument.Create()
      CALL v_doc.loadFromString(p_entrada)
      CALL v_doc.normalize()

      #Se obtiene el elemento padre y se valida que en el XML solo se tenga un elemento raiz
      CALL v_doc.getDocumentNodesCount() RETURNING v_num_elementos
      IF v_num_elementos IS NULL OR v_num_elementos <> 1 THEN
         #ERROR: Llego mas de un elemento en la respuesta
         INITIALIZE v_respuesta.* TO NULL
         LET v_respuesta.status_operacion = "ERROR DE FORMATO"
         LET v_respuesta.desc_status = "OCURRIÓ UN ERROR PORQUE EL XML DE RESPUESTA NO ES VALIDO"
      ELSE
         LET v_nodo_padre = v_doc.getDocumentNodeItem(1)

         #LET v_item =  v_nodo_padre.getAttribute("statusOper")
         LET v_respuesta.status_operacion = v_nodo_padre.getAttribute("statusOper")
         LET v_respuesta.desc_status = v_nodo_padre.getAttribute("message")
         #Obtenemos el numero de hijos para ver si la respuesta contiene datos
         LET v_num_elementos = v_nodo_padre.getChildrenCount()
         IF v_num_elementos IS NULL OR v_num_elementos < 1 THEN
            #El elemento padre no contiene datos, validar parametros por posible error de ejecucion
         ELSE
            #Obtenemos todos los elementos hijos dentro
            LET v_num_item = 1
            FOR i = 1 TO v_num_elementos
               LET v_item = v_nodo_padre.getChildNodeItem(v_num_item)
               IF v_item IS NULL OR v_item.getLocalName() = "#text" THEN
                  #En este caso el elemento se considera basura ya que corresponde a un caracter especial o a un salto de linea
                  CALL v_nodo_padre.removeChild(v_item)
                  LET v_num_item = v_num_item - 1
               ELSE
                  LET v_campo = v_item.getNodeName() CLIPPED
                  IF v_item.getChildrenCount() > 0 THEN
                     CALL v_item.getFirstChild() RETURNING v_item
                     LET v_valor = v_item.toString() CLIPPED
                  ELSE
                     LET v_valor = NULL
                  END IF
                  CALL fn_asigna_valor(v_campo, v_valor, v_respuesta.*) RETURNING v_respuesta.*
               END IF
               LET v_num_item = v_num_item + 1
            END FOR
         END IF
      END IF
   CATCH
      INITIALIZE v_respuesta.* TO NULL
      LET v_respuesta.status_operacion = "ERROR DE FORMATO"
      LET v_respuesta.desc_status = "OCURRIÓ UN ERROR PORQUE EL XML DE RESPUESTA NO ES VALIDO"
   END TRY
   RETURN v_respuesta.*
END FUNCTION

PRIVATE FUNCTION fn_asigna_valor(p_campo, p_valor, p_respuesta)
   DEFINE p_campo       STRING
   DEFINE p_valor       STRING
   DEFINE p_respuesta   respuesta_renapo

   LET p_valor = p_valor CLIPPED
   CASE p_campo
      WHEN "CURP"
         LET p_respuesta.curp = p_valor
      WHEN "apellido1"
         LET p_respuesta.apellido_paterno = p_valor
      WHEN "apellido2"
         LET p_respuesta.apellido_materno = p_valor
      WHEN "nombres"
         LET p_respuesta.nombre = p_valor
      WHEN "sexo"
         LET p_respuesta.sexo = p_valor
      WHEN "fechNac"
         LET p_respuesta.fecha_nacimiento = p_valor
      WHEN "nacionalidad"
         LET p_respuesta.nacionalidad = p_valor
      WHEN "docProbatorio"
         LET p_respuesta.tipo_doc_probatorio = p_valor
      WHEN "anioReg"
         LET p_respuesta.anio_registro = p_valor
      WHEN "foja"
         LET p_respuesta.foja = p_valor
      WHEN "tomo"
         LET p_respuesta.tomo = p_valor
      WHEN "libro"
         LET p_respuesta.libro = p_valor
      WHEN "numActa"
         LET p_respuesta.num_acta = p_valor
      WHEN "CRIP"
         LET p_respuesta.crip = p_valor
      WHEN "numEntidadReg"
         LET p_respuesta.cve_entidad_reg = p_valor
      WHEN "cveMunicipioReg"
         LET p_respuesta.cve_municipio_reg = p_valor
      WHEN "NumRegExtranjeros"
         LET p_respuesta.num_reg_extrangeros = p_valor
      WHEN "FolioCarta"
         LET p_respuesta.filio_carta = p_valor
      WHEN "cveEntidadNac"
         LET p_respuesta.cve_ent_nacimiento = p_valor
      WHEN "cveEntidadEmisora"
         LET p_respuesta.cve_ent_emisora = p_valor
      WHEN "statusCurp"
         LET p_respuesta.status_curp = p_valor
      OTHERWISE
         DISPLAY "ERROR, el campo ", p_campo, " no se encuentra en el contrato"
   END CASE

   RETURN p_respuesta.*
END FUNCTION