####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS12.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         # 
#                    consulta de saldo anterior                    #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml

DATABASE safre_viv

GLOBALS "CTAWS11.inc"
GLOBALS "CTAWS12.inc"

#Variables para el manejo del XML de salida
PRIVATE DEFINE v_tipo_consulta  INTEGER
PRIVATE DEFINE doc              xml.DomDocument
PRIVATE DEFINE raiz             xml.DomNode
PRIVATE DEFINE trabajadores     xml.DomNode
PRIVATE DEFINE trabajador       xml.DomNode

FUNCTION fn_prepara_consulta()
   DEFINE v_conteo_nss        STRING
   DEFINE v_conteo_rfc        STRING
   DEFINE v_conteo_rfc_10     STRING
   DEFINE v_conteo_nss_rfc    STRING
   DEFINE v_consulta_nss      STRING
   DEFINE v_consulta_rfc      STRING
   DEFINE v_consulta_rfc_10   STRING
   DEFINE v_consulta_nss_rfc  STRING

   DATABASE safre_viv
   
   LET v_conteo_nss =   "SELECT ", 
                           "COUNT (id_afi_fondo72) ",
                        "FROM afi_fondo72 ",
                        "WHERE nss = ?"
   PREPARE exe_conteo_nss FROM v_conteo_nss
   
   LET v_conteo_rfc =   "SELECT ",
                           "COUNT (id_afi_fondo72) ",
                        "FROM afi_fondo72 ",
                        "WHERE rfc = ?"
   PREPARE exe_conteo_rfc FROM v_conteo_rfc

   LET v_conteo_rfc_10 =   "SELECT ",
                           "COUNT (id_afi_fondo72) ",
                        "FROM afi_fondo72 ",
                        "WHERE rfc[1,10] = ?"
   PREPARE exe_conteo_rfc_10 FROM v_conteo_rfc_10
   
   LET v_conteo_nss_rfc =  "SELECT ",
                              "COUNT (id_afi_fondo72) ",
                           "FROM afi_fondo72 ",
                           "WHERE nss = ? ",
                           "AND rfc = ?"
   PREPARE exe_conteo_nss_rfc FROM v_conteo_nss_rfc
   
   LET v_consulta_nss = "SELECT FIRST 10 ",
                           "afi.nombre, ",
                           "SUM(cta.importe), ",
                           "afi.rfc, ",
                           "afi.nss ",
                        "FROM afi_fondo72 afi ",
                        "INNER JOIN cta_fondo72 cta ON cta.id_afi_fondo72 = afi.id_afi_fondo72 ",
                        "WHERE afi.nss = ?",
                        "GROUP BY afi.nombre, afi.rfc, afi.nss"
   PREPARE exe_consulta_nss FROM v_consulta_nss
   
   LET v_consulta_rfc = "SELECT FIRST 10 ",
                           "afi.nombre, ",
                           "SUM(cta.importe), ",
                           "afi.rfc, ",
                           "afi.nss ",
                        "FROM afi_fondo72 afi ",
                        "INNER JOIN cta_fondo72 cta ON cta.id_afi_fondo72 = afi.id_afi_fondo72 ",
                        "WHERE afi.rfc = ? ",
                        "GROUP BY afi.nombre, afi.rfc, afi.nss"
   PREPARE exe_consulta_rfc FROM v_consulta_rfc

   LET v_consulta_rfc_10 = "SELECT FIRST 10 ",
                           "afi.nombre, ",
                           "SUM(cta.importe), ",
                           "afi.rfc, ",
                           "afi.nss ",
                        "FROM afi_fondo72 afi ",
                        "INNER JOIN cta_fondo72 cta ON cta.id_afi_fondo72 = afi.id_afi_fondo72 ",
                        "WHERE afi.rfc[1,10] = ? ",
                        "GROUP BY afi.nombre, afi.rfc, afi.nss"
   PREPARE exe_consulta_rfc_10 FROM v_consulta_rfc_10
   
   LET v_consulta_nss_rfc =   "SELECT FIRST 10 ",
                                 "afi.nombre, ",
                                 "SUM(cta.importe), ",
                                 "afi.rfc, ",
                                 "afi.nss ",
                              "FROM afi_fondo72 afi ",
                              "INNER JOIN cta_fondo72 cta ON cta.id_afi_fondo72 = afi.id_afi_fondo72 ",
                              "WHERE afi.nss = ? ",
                              "AND afi.rfc = ? ",
                              "GROUP BY afi.nombre, afi.rfc, afi.nss"
   PREPARE exe_consulta_nss_rfc FROM v_consulta_nss_rfc
END FUNCTION

FUNCTION fn_consultaSaldoAnterior()
   DEFINE doc_request   xml.DomDocument

   DEFINE nss           STRING
   DEFINE rfc           STRING
   
   TRY 
      LET doc_request = xml.DomDocument.Create()
      CALL doc_request.loadFromString(consultaSaldoAnterior.xmlRequest)
   CATCH
      LET consultaSaldoAnteriorResponse.consultaSaldoAnteriorReturn = "El formato de los parametros de entrada no es valido"
      RETURN
   END TRY

   CALL fn_obtener_valor("nss",doc_request)       RETURNING nss
   CALL fn_obtener_valor("rfc",doc_request)       RETURNING rfc

   CASE
      WHEN (nss IS NOT NULL AND nss.getLength() = 11 AND rfc IS NULL)
         LET v_tipo_consulta = CONSULTA_NSS        #Consulta solo con NSS
      WHEN (nss IS NULL AND rfc IS NOT NULL AND rfc.getLength() > 9)
         LET v_tipo_consulta = CONSULTA_RFC        #Consulta solo con RFC
      WHEN (nss IS NOT NULL AND nss.getLength() = 11 AND rfc IS NOT NULL AND rfc.getLength() > 9)
         LET v_tipo_consulta = CONSULTA_AMBOS      #Consulta con NSS y RFC
      OTHERWISE
         CALL fn_genera_error_respuesta(SALDO_ANT_0001_REQ_RFC_NSS) RETURNING consultaSaldoAnteriorResponse.consultaSaldoAnteriorReturn
         RETURN
   END CASE
   
   CALL fn_genera_respuesta(nss, rfc) RETURNING consultaSaldoAnteriorResponse.consultaSaldoAnteriorReturn
END FUNCTION

PRIVATE FUNCTION fn_genera_respuesta(p_nss, p_rfc)
   DEFINE p_nss               CHAR(11)
   DEFINE p_rfc               CHAR(13)
   DEFINE v_resultado        STRING

   CASE v_tipo_consulta
      WHEN CONSULTA_NSS
         CALL fn_solo_nss(p_nss) RETURNING v_resultado
      WHEN CONSULTA_RFC
         CALL fn_solo_rfc(p_rfc, FALSE) RETURNING v_resultado
      WHEN CONSULTA_AMBOS
         CALL fn_ambos(p_nss, p_rfc) RETURNING v_resultado
   END CASE

   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_solo_nss(p_nss)
   DEFINE p_nss            CHAR(11)
   DEFINE v_fondo          saldo_fondo
   DEFINE v_fondo_multiple saldo_multiple
   DEFINE i                INTEGER
   DEFINE v_resultado      STRING

   DEFINE v_num_registros  INTEGER
   #Primero se obtiene el numero de registros para saber el tipo de XML a generar
   EXECUTE exe_conteo_nss USING p_nss INTO v_num_registros
   CASE
      WHEN v_num_registros = 0   #No existen resultados
         CALL fn_genera_error_respuesta(SALDO_ANT_0010_NSS_NOT_FOUND) RETURNING v_resultado

      WHEN v_num_registros = 1   #Existe un resultado unico
         EXECUTE exe_consulta_nss USING p_nss INTO v_fondo.*
         DISPLAY "registro unico: ", v_fondo.*
         CALL fn_genera_respuesta_unica(SALDO_ANT_0000_OK,v_fondo.*) RETURNING v_resultado
         #aqui era 0020 pero con el catalogo actualizado se manda 0000

      WHEN v_num_registros > 1   #Existen multiples registros
         LET v_fondo_multiple.codigo = SALDO_ANT_0030_NSS_MULT
         DECLARE saldos CURSOR FOR exe_consulta_nss

         #Se guarda el resultado en el arreglo para generar el XML
         LET i = 1
         FOREACH saldos USING p_nss INTO v_fondo_multiple.lista_saldo[i].*
            LET i = i + 1
         END FOREACH
         CLOSE saldos
         FREE saldos
   
         CALL fn_genera_respuesta_multiple(v_fondo_multiple.*) RETURNING v_resultado
   END CASE 
   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_solo_rfc(p_rfc, p_ind_nss)
   DEFINE p_rfc            CHAR(13)
   DEFINE p_ind_nss        BOOLEAN
   DEFINE v_fondo          saldo_fondo
   DEFINE v_fondo_multiple saldo_multiple
   DEFINE i                INTEGER
   DEFINE codigo           STRING
   #DEFINE v_rfc_10         LIKE cta_fondo72.rfc
   DEFINE v_resultado      STRING

   DEFINE v_num_registros  INTEGER
   
   #Primero se obtiene el numero de registros para saber el tipo de XML a generar
   IF length(p_rfc) = 10 THEN
      EXECUTE exe_conteo_rfc_10 USING p_rfc INTO v_num_registros
   ELSE
      EXECUTE exe_conteo_rfc USING p_rfc INTO v_num_registros
   END IF
   
   CASE
      WHEN v_num_registros = 0   #No existen resultados
         IF length(p_rfc) = 10 THEN
            #ya se busco con RFC a 10 posiciones por lo que se manda el error
            IF p_ind_nss THEN #Se busco con nss y tampoco se encontro
               LET codigo = SALDO_ANT_0090_NSS_RFC_10_13_NOT_FOUND
            ELSE              #Solo se busco por RFC
               LET codigo = SALDO_ANT_0040_RFC_NOT_FOUND
            END IF
            CALL fn_genera_error_respuesta(codigo) RETURNING v_resultado
         ELSE
            #Se busca con RFC a 10 posiciones
            LET p_rfc = p_rfc[1,10]
            CALL fn_solo_rfc(p_rfc, p_ind_nss) RETURNING v_resultado
         END IF

      WHEN v_num_registros = 1   #Existe un resultado unico
         IF length(p_rfc) = 10 THEN
            EXECUTE exe_consulta_rfc_10 USING p_rfc INTO v_fondo.*
         ELSE
            EXECUTE exe_consulta_rfc USING p_rfc INTO v_fondo.*
         END IF
         
         IF p_ind_nss THEN    #significa que no se encontro el nss
            IF length(p_rfc) = 13 THEN
               LET codigo = SALDO_ANT_0080_RFC_13_DIFF
            ELSE
               LET codigo = SALDO_ANT_0050_RFC_10_DIFF
            END IF
         ELSE                 #Significa que solo se busco por RFC
            LET codigo = SALDO_ANT_0000_OK
            #aqui era 0020 pero con el catalogo actualizado se manda 0000
         END IF
         CALL fn_genera_respuesta_unica(codigo, v_fondo.*) RETURNING v_resultado
      WHEN v_num_registros > 1   #Existen multiples registros
         LET v_fondo_multiple.codigo = SALDO_ANT_0060_RFC_MULT

         #Se guarda el resultado en el arreglo para generar el XML
         LET i = 1
         IF length(p_rfc) = 10 THEN
            DECLARE saldos_rfc_10 CURSOR FOR exe_consulta_rfc_10
            FOREACH saldos_rfc_10 USING p_rfc INTO v_fondo_multiple.lista_saldo[i].*
               LET i = i + 1
            END FOREACH
            CLOSE saldos_rfc_10
            FREE saldos_rfc_10
         ELSE
            DECLARE saldos_rfc CURSOR FOR exe_consulta_rfc
            FOREACH saldos_rfc USING p_rfc INTO v_fondo_multiple.lista_saldo[i].*
               LET i = i + 1
            END FOREACH
            CLOSE saldos_rfc
            FREE saldos_rfc
         END IF
         
         CALL fn_genera_respuesta_multiple(v_fondo_multiple.*) RETURNING v_resultado
   END CASE 

   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_ambos(p_nss, p_rfc)
   DEFINE p_nss               CHAR(11)
   DEFINE p_rfc               CHAR(13)
   DEFINE v_fondo             saldo_fondo
   DEFINE v_fondo_multiple    saldo_multiple
   DEFINE i                   INTEGER
   DEFINE v_resultado         STRING

   DEFINE v_num_registros     INTEGER
   DEFINE v_num_registros_nss INTEGER

   #Primero se obtiene el numero de registros para saber el tipo de XML a generar
   EXECUTE exe_conteo_nss_rfc USING p_nss, p_rfc INTO v_num_registros
   CASE
      WHEN v_num_registros = 0   #No existen resultados buscando por NSS y RFC
         #Primero se busca por NSS
         EXECUTE exe_conteo_nss USING p_nss INTO v_num_registros_nss
         CASE
            WHEN v_num_registros_nss = 0  #No existen resultados buscando por NSS
               #Por ultimo se busca por RFC
               CALL fn_solo_rfc(p_rfc, TRUE) RETURNING v_resultado
               
            WHEN v_num_registros_nss = 1  #Existe un resultado unico con RFC distinto
               EXECUTE exe_consulta_nss USING p_nss INTO v_fondo.*
               CALL fn_genera_respuesta_unica(SALDO_ANT_0070_NSS_DIFF_RFC, v_fondo.*) RETURNING v_resultado

            WHEN v_num_registros_nss > 1  #Existen multiples registros con RFC distinto
               LET v_fondo_multiple.codigo = SALDO_ANT_0030_NSS_MULT
               DECLARE saldos_nss CURSOR FOR exe_consulta_nss

               #Se guarda el resultado en el arreglo para generar el XML
               LET i = 1
               FOREACH saldos_nss USING p_nss INTO v_fondo_multiple.lista_saldo[i].*
                  LET i = i + 1
               END FOREACH
               CALL fn_genera_respuesta_multiple(v_fondo_multiple.*) RETURNING v_resultado
         END CASE
      WHEN v_num_registros = 1   #Existe un resultado unico buscando por NSS y RFC
         EXECUTE exe_consulta_nss_rfc USING p_nss, p_rfc INTO v_fondo.*
         CALL fn_genera_respuesta_unica(SALDO_ANT_0000_OK, v_fondo.*) RETURNING v_resultado
         #aqui era 0020 pero deacuerdo al catalogo actualizado se regresa 0000
         
      WHEN v_num_registros > 1   #Existen multiples registros buscando por NSS y RFC
         LET v_fondo_multiple.codigo = SALDO_ANT_0030_NSS_MULT
         DECLARE saldos_ambos CURSOR FOR exe_consulta_nss_rfc
         #Se guarda el resultado en el arreglo para generar el XML
         LET i = 1
         FOREACH saldos_ambos USING p_nss, p_rfc INTO v_fondo_multiple.lista_saldo[i].*
            LET i = i + 1
         END FOREACH
         CLOSE saldos_ambos
         FREE saldos_ambos
         CALL fn_genera_respuesta_multiple(v_fondo_multiple.*) RETURNING v_resultado
   END CASE

   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_genera_error_respuesta(p_codigo_error)
   DEFINE p_codigo_error         STRING
   DEFINE resultado              STRING

   #Se crea el documento con el elemento root 
   LET doc = xml.DomDocument.CreateDocument("SalidaConsultaSaldoAnterior")
   #obtenemos el nodo principal
   LET raiz = doc.getDocumentElement() 
   #agregamos los nodos del encabezado
   CALL raiz.appendChild(fn_genera_nodo("codigoRespuesta",p_codigo_error))
   CALL raiz.appendChild(fn_genera_nodo("nombre",""))
   CALL raiz.appendChild(fn_genera_nodo("saldo",""))
   CALL raiz.appendChild(fn_genera_nodo("rfc",""))
   CALL raiz.appendChild(fn_genera_nodo("nss",""))

   #creamos el nodo padre que contendra los bimestres
   LET trabajadores = doc.createElement("trabajadores")
   CALL trabajadores.setAttribute("indMul","")
   
   #agregamos los bimestres a la raiz
   CALL raiz.appendChild(trabajadores)

   #obtenemos el xml en una cadena para mandar la respuesta
   CALL doc.saveToString() RETURNING resultado

   RETURN resultado
END FUNCTION

PRIVATE FUNCTION fn_genera_respuesta_unica(p_codigo, p_saldo)
   DEFINE p_saldo          saldo_fondo
   DEFINE p_codigo         STRING
   DEFINE v_resultado     STRING

   #Se crea el documento con el elemento root 
   LET doc = xml.DomDocument.CreateDocument("SalidaConsultaSaldoAnterior")
   #obtenemos el nodo principal
   LET raiz = doc.getDocumentElement() 
   #agregamos los nodos del encabezado
   CALL raiz.appendChild(fn_genera_nodo("codigoRespuesta",p_codigo))
   CALL raiz.appendChild(fn_genera_nodo("nombre",p_saldo.nombre))
   CALL raiz.appendChild(fn_genera_nodo("saldo",p_saldo.saldo))
   CALL raiz.appendChild(fn_genera_nodo("rfc",p_saldo.rfc))
   CALL raiz.appendChild(fn_genera_nodo("nss",p_saldo.nss))

   #creamos el nodo padre que contendra los bimestres
   LET trabajadores = doc.createElement("trabajadores")
   CALL trabajadores.setAttribute("indMul","0")
   
   #agregamos los bimestres a la raiz
   CALL raiz.appendChild(trabajadores)

   #obtenemos el xml en una cadena para mandar la respuesta
   CALL doc.saveToString() RETURNING v_resultado

   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_genera_respuesta_multiple(p_lista_saldo)
   DEFINE p_lista_saldo    saldo_multiple
   DEFINE v_resultado      STRING
   DEFINE i                INTEGER

   #Se crea el documento con el elemento root 
   LET doc = xml.DomDocument.CreateDocument("SalidaConsultaSaldoAnterior")
   #obtenemos el nodo principal
   LET raiz = doc.getDocumentElement() 
   #agregamos los nodos del encabezado
   CALL raiz.appendChild(fn_genera_nodo("codigoRespuesta",p_lista_saldo.codigo))
   CALL raiz.appendChild(fn_genera_nodo("nombre","0"))
   CALL raiz.appendChild(fn_genera_nodo("saldo","0"))
   CALL raiz.appendChild(fn_genera_nodo("rfc","0"))
   CALL raiz.appendChild(fn_genera_nodo("nss","0"))

   #creamos el nodo padre que contendra los bimestres
   LET trabajadores = doc.createElement("trabajadores")
   CALL trabajadores.setAttribute("indMul","1")

   FOR i = 1 TO p_lista_saldo.lista_saldo.getLength()
      IF p_lista_saldo.lista_saldo[i].saldo IS NOT NULL THEN
         LET trabajador = doc.createElement("trabajador")
         CALL trabajador.appendChild(fn_genera_nodo("nombreTra",p_lista_saldo.lista_saldo[i].nombre))
         CALL trabajador.appendChild(fn_genera_nodo("saldoTra",p_lista_saldo.lista_saldo[i].saldo))
         CALL trabajador.appendChild(fn_genera_nodo("rfcTra",p_lista_saldo.lista_saldo[i].rfc))
         CALL trabajador.appendChild(fn_genera_nodo("nssTra",p_lista_saldo.lista_saldo[i].nss))

         #agregamos el bimestre en el bloque de bimestres
         CALL trabajadores.appendChild(trabajador)
      END IF
   END FOR

   #agregamos los bimestres a la raiz
   CALL raiz.appendChild(trabajadores)

   #obtenemos el xml en una cadena para mandar la respuesta
   CALL doc.saveToString() RETURNING v_resultado

   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_genera_nodo(p_nombre, p_valor)
   DEFINE p_nombre            STRING
   DEFINE p_valor             STRING
   DEFINE v_valor_especial    STRING
   DEFINE v_especial          INTEGER

   DEFINE node          xml.DomNode
   DEFINE nodo          STRING

   TRY
      LET nodo = "<", p_nombre CLIPPED , ">", p_valor CLIPPED ,"</", p_nombre CLIPPED , ">"
      LET node = doc.createNode(nodo)
   CATCH
      #Significa que la cadena tiene un caracter especial
      LET v_especial = p_valor.getIndexOf('&', 1)
      IF v_especial IS NOT NULL AND v_especial <> -1 THEN
         LET v_valor_especial = p_valor.subString(1,v_especial - 1)
         LET v_valor_especial = v_valor_especial.append('$')
         LET v_valor_especial = v_valor_especial.append(p_valor.subString(v_especial + 1, p_valor.getLength()))
         RETURN fn_genera_nodo(p_nombre, v_valor_especial)
      END IF
      
   END TRY
   
   RETURN node
END FUNCTION

PRIVATE FUNCTION fn_obtener_valor(p_nombre_elemento, p_documento) 
   DEFINE p_nombre_elemento      STRING
   DEFINE p_documento            xml.DomDocument
   DEFINE nodo                   xml.DomNode
   DEFINE lista                  xml.DomNodeList
   DEFINE valor                  STRING
   TRY
      CALL p_documento.getElementsByTagName(p_nombre_elemento) RETURNING lista
      CALL lista.getItem(1) RETURNING nodo
      CALL nodo.getFirstChild() RETURNING nodo
      CALL nodo.toString() RETURNING valor
   CATCH
      LET valor = NULL
   END TRY
      
   RETURN valor
END FUNCTION