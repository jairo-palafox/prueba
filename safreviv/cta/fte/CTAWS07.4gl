####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS07.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    consulta del detalle del  saldo 9297          #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml

DATABASE safre_viv

GLOBALS "CTAWS06.inc"
GLOBALS "CTAWS07.inc"

PRIVATE DEFINE nss           LIKE afi_decreto.nss
PRIVATE DEFINE origen        STRING
PRIVATE DEFINE id            STRING
PRIVATE DEFINE ctr_interno   STRING
PRIVATE DEFINE seguridad     STRING 

FUNCTION fn_prepara_consulta()
   DEFINE v_consulta_credito     STRING
   DEFINE v_consulta_detalle     STRING

   DATABASE safre_viv

   LET v_consulta_credito = " SELECT ",
                                 "afiliado.id_derechohabiente, ",
                                 "credito.num_credito, ",
                                 "tipo_credito.desc_credito, ",
                                 "credito.edo_credito, ",
                                 "credito.f_otorga, ",
                                 "credito.f_culmina ",
                              "FROM afi_derechohabiente afiliado ",
                              "LEFT JOIN cre_acreditado credito ON credito.id_derechohabiente = afiliado.id_derechohabiente ",
                              "LEFT JOIN cat_tipo_credito tipo_credito ON tipo_credito.tpo_credito = credito.tpo_credito ",
                              "WHERE afiliado.nss = ? "
   PREPARE exe_consulta_credito FROM v_consulta_credito

   LET v_consulta_detalle =   "SELECT FIRST 16 ",
                                 "nrp, ",
                                 "'EMPRESA', ",
                                 "f_pago, ",
                                 "imp_ap_pat, ",
                                 "imp_am_cre, ",
                                 "periodo_pago ",
                              "FROM cta_his_pagos ",
                              "WHERE origen_archivo IN(6,8) ",
                              "AND id_derechohabiente = ? ",
                              "ORDER BY f_pago DESC "
   PREPARE exe_consulta_detalle FROM v_consulta_detalle
END FUNCTION

FUNCTION fn_consultaDetalle9297()
   DEFINE doc        xml.DomDocument

   TRY 
      LET doc = xml.DomDocument.Create()
      CALL doc.loadFromString(consultaDetalle9297.xmlRequest)
   CATCH
      LET consultaDetalle9297Response.consultaDetalle9297Return = "El formato de los parametros de entrada no es valido"
      RETURN
   END TRY

   CALL fn_obtener_valor("nss",doc)       RETURNING nss
   IF nss IS NULL THEN
      LET consultaDetalle9297Response.consultaDetalle9297Return = "No se encontro el campo 'nss' en el XML de entrada"
      RETURN
   END IF
   
   CALL fn_obtener_valor("origen",doc)    RETURNING origen
   CALL fn_obtener_valor("id",doc)        RETURNING id
   CALL fn_obtener_valor("control",doc)   RETURNING ctr_interno
   CALL fn_obtener_valor("seguridad",doc) RETURNING seguridad
   
   CALL fn_genera_respuesta() RETURNING consultaDetalle9297Response.consultaDetalle9297Return
END FUNCTION

PRIVATE FUNCTION fn_genera_respuesta()
   DEFINE doc              xml.DomDocument
   DEFINE raiz             xml.DomNode
   DEFINE bimestres        xml.DomNode
   DEFINE bimestre         xml.DomNode
   DEFINE resultado        STRING

   DEFINE datos_credito    credito
   DEFINE datos_detalle    detalle

   #Se buscan los datos del credito
   EXECUTE exe_consulta_credito USING nss INTO datos_credito.*
   
   #Se crea el documento con el elemento root 
   LET doc = xml.DomDocument.CreateDocument("SalidaConsultaSaldoDetalle")
   #obtenemos el nodo principal
   LET raiz = doc.getDocumentElement()
   #agregamos los nodos del encabezado

   IF datos_credito.num_credito IS NULL THEN
      CALL raiz.appendChild(fn_genera_nodo("numCredito","", doc))
      CALL raiz.appendChild(fn_genera_nodo("tipoCredito","", doc))
      CALL raiz.appendChild(fn_genera_nodo("estatus","", doc))
      CALL raiz.appendChild(fn_genera_nodo("fechaOtorgamiento","", doc))
      CALL raiz.appendChild(fn_genera_nodo("fechaCulminacion","", doc))
   ELSE
      CALL raiz.appendChild(fn_genera_nodo("numCredito",datos_credito.num_credito, doc))
      CALL raiz.appendChild(fn_genera_nodo("tipoCredito",datos_credito.tipo_credito, doc))
      CALL raiz.appendChild(fn_genera_nodo("estatus",datos_credito.estatus, doc))
      CALL raiz.appendChild(fn_genera_nodo("fechaOtorgamiento",datos_credito.fecha_otorgamiento USING "DD-MM-YYYY", doc))
      CALL raiz.appendChild(fn_genera_nodo("fechaCulminacion",datos_credito.fecha_culminacion USING "DD-MM-YYYY", doc))
   END IF
   
   IF datos_credito.id_derechohabiente IS NULL THEN
      CALL raiz.appendChild(fn_genera_nodo("mensaje","0", doc))
   ELSE
      CALL raiz.appendChild(fn_genera_nodo("mensaje","1", doc))

      #creamos el nodo padre que contendra los bimestres
      LET bimestres = doc.createElement("bimestres")

      #Se ejecuta la consulta para obtener los ultimos 16 aportes del afiliado
      DECLARE consulta_detalle CURSOR FOR exe_consulta_detalle

      FOREACH consulta_detalle USING datos_credito.id_derechohabiente INTO datos_detalle.*
         LET bimestre = doc.createElement("bimestre")
         CALL bimestre.setAttribute("numBim",datos_detalle.bimestre)
         CALL bimestre.appendChild(fn_genera_nodo("noRegPatronal",datos_detalle.nrp, doc))
         CALL bimestre.appendChild(fn_genera_nodo("empresa",datos_detalle.empresa, doc))
         CALL bimestre.appendChild(fn_genera_nodo("fechaPago",datos_detalle.fecha_pago USING "DD-MM-YYYY", doc))
         CALL bimestre.appendChild(fn_genera_nodo("aportacion",datos_detalle.aportacion, doc))
         CALL bimestre.appendChild(fn_genera_nodo("amortizacion",datos_detalle.amortizacion, doc))

         #agregamos el bimestre en el bloque de bimestres
         CALL bimestres.appendChild(bimestre)
      END FOREACH

      #agregamos los bimestres a la raiz
      CALL raiz.appendChild(bimestres)
   END IF

   #obtenemos el xml en una cadena para mandar la respuesta
   CALL doc.saveToString() RETURNING resultado

   RETURN resultado
END FUNCTION

PRIVATE FUNCTION fn_genera_nodo(p_nombre, p_valor, p_doc)
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
         RETURN fn_genera_nodo(p_nombre, v_valor_especial, p_doc)
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