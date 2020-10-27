####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS03.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    consulta de saldo total 9297                  #
#Fecha inicio      =>10 FEBRERO 2012                               #
#                                                                  #
####################################################################
#PRODINF-489:                                                      #
#Fecha cambio      =>13 OCTUBRE 2014                               #
#Cambio              Se agrega como parte del saldo la subcuenta   #
#                    de RISS VOLUNTARIO                            #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml

GLOBALS "CTAWS02.inc"
GLOBALS "CTAWS03.inc"

PRIVATE DEFINE nss           CHAR(11)
PRIVATE DEFINE origen        STRING
PRIVATE DEFINE id            STRING
PRIVATE DEFINE ctr_interno   STRING
PRIVATE DEFINE seguridad     STRING 

FUNCTION fn_prepara_consulta()
   DEFINE v_consulta_nss     STRING
   DEFINE v_consulta_saldo   STRING
   DEFINE v_consulta_saldo_bis          STRING
   DEFINE v_consulta_max_pension	STRING
   DEFINE v_consulta_datamart 		STRING   
   
   DATABASE safre_sdo

   LET v_consulta_nss = "SELECT ",
                           "id_derechohabiente, ",
                           "nss, ",
                           "rfc, ",
                           "nombre_af, ",
                           "ap_paterno_af, ",
                           "ap_materno_af, ",
                           "tipo_trabajador ",
                        "FROM safre_viv@vivop_tcp:afi_derechohabiente ",
                        "WHERE nss = ?"
   PREPARE exe_consulta_nss FROM v_consulta_nss

   LET v_consulta_saldo =  "SELECT FIRST 1 ",
                     "sdo.id_derechohabiente, ",
                     "sdo97.monto_pesos, ",
                     "sdo92.monto_pesos ",
                     "FROM cta_saldo_diario sdo ",
                     "LEFT OUTER JOIN( ",
                     "	SELECT id_derechohabiente, SUM(monto_pesos) AS monto_pesos FROM cta_saldo_diario WHERE subcuenta IN (", VIVIENDA_97, ",",
                                                                                                                              SOLO_INFONAVIT_97, ",", 
                                                                                                                              RISS_VOLUNTARIO, ") ",
                     "	AND fondo_inversion = ", FONDO_INV , " ",
                     "	AND id_derechohabiente = ? ",
                     "  GROUP BY id_derechohabiente ",
                     ") sdo97 ON sdo.id_derechohabiente = sdo97.id_derechohabiente ",
                     "LEFT OUTER JOIN( ",
                     "	SELECT id_derechohabiente, monto_pesos FROM cta_saldo_diario WHERE subcuenta IN (", VIVIENDA_92, ",", SOLO_INFONAVIT_92, ") ",
                     "	AND fondo_inversion = ", FONDO_INV , " ",
                     "	AND id_derechohabiente = ? ",
                     ") sdo92 ON sdo.id_derechohabiente = sdo92.id_derechohabiente ",
                     "WHERE sdo.id_derechohabiente = ?"
   PREPARE exe_consulta_saldo FROM v_consulta_saldo

   LET v_consulta_saldo_bis =  "SELECT FIRST 1 ",
                     "sdo.id_derechohabiente, ",
                     "sdo97.monto_pesos, ",
                     "sdo92.monto_pesos ",
                     "FROM cta_saldo_diario_bis sdo ",
                     "LEFT OUTER JOIN( ",
                     "  SELECT id_derechohabiente, SUM(monto_pesos) AS monto_pesos FROM cta_saldo_diario_bis WHERE subcuenta IN ( ", VIVIENDA_97, ",", 
                                                                                                                                    SOLO_INFONAVIT_97, ",",
                                                                                                                                    RISS_VOLUNTARIO, ") ",
                     "  AND fondo_inversion = ", FONDO_INV , " ",
                     "  AND id_derechohabiente = ? ",
                     "  GROUP BY id_derechohabiente ",
                     ") sdo97 ON sdo.id_derechohabiente = sdo97.id_derechohabiente ",
                     "LEFT OUTER JOIN( ",
                     "  SELECT id_derechohabiente, monto_pesos FROM cta_saldo_diario_bis WHERE subcuenta IN (", VIVIENDA_92, ",", SOLO_INFONAVIT_92, ") ",
                     "  AND fondo_inversion = ", FONDO_INV , " ",
                     "  AND id_derechohabiente = ? ",
                     ") sdo92 ON sdo.id_derechohabiente = sdo92.id_derechohabiente ",
                     "WHERE sdo.id_derechohabiente = ?"
   PREPARE exe_consulta_saldo_bis FROM v_consulta_saldo_bis

   LET v_consulta_max_pension = "SELECT MAX(sec_pension) FROM safre_viv@vivop_tcp:ret_datamart WHERE nss = ?"
   PREPARE exe_consulta_max_pension FROM v_consulta_max_pension

   LET v_consulta_datamart =  "SELECT FIRST 1",
                                 "regimen, ",
                                 "tpo_seguro, ",
                                 "tpo_pension, ",
                                 "tpo_prestacion, ",
                                 "diag_registro ",
                              "FROM safre_viv@vivop_tcp:ret_datamart ",
                              "WHERE nss = ? ",
                              "AND sec_pension = ? "
   PREPARE exe_consulta_datamart FROM v_consulta_datamart

END FUNCTION 

FUNCTION fn_consultaTotal9297()
   DEFINE doc        xml.DomDocument
   
   TRY 
      LET doc = xml.DomDocument.Create()
      CALL doc.loadFromString(consultaTotal9297.xmlRequest)
   CATCH
      LET consultaTotal9297Response.consultaTotal9297Return = "El formato de los parametros de entrada no es valido"
      RETURN
   END TRY

   CALL fn_obtener_valor("nss",doc)       RETURNING nss
   IF nss IS NULL THEN
      LET consultaTotal9297Response.consultaTotal9297Return = "No se encontro el campo 'nss' en el XML de entrada"
      RETURN
   END IF 
   
   CALL fn_obtener_valor("origen",doc)    RETURNING origen
   CALL fn_obtener_valor("id",doc)        RETURNING id
   CALL fn_obtener_valor("control",doc)   RETURNING ctr_interno
   CALL fn_obtener_valor("seguridad",doc) RETURNING seguridad
   
   CALL fn_genera_respuesta() RETURNING consultaTotal9297Response.consultaTotal9297Return
END FUNCTION

PRIVATE FUNCTION fn_genera_respuesta()
   DEFINE doc                    xml.DomDocument
   DEFINE elt                    xml.DomNode
   DEFINE v_resultado_consulta   consultaSaldo
   DEFINE resultado              STRING

   DEFINE v_max_pension          SMALLINT
   DEFINE v_regimen              SMALLINT
   DEFINE v_tpo_seguro           CHAR(2)
   DEFINE v_tpo_pension          CHAR(2)
   DEFINE v_tpo_prestacion       CHAR(2)
   DEFINE v_diag_registro        SMALLINT
   DEFINE v_id_datamart          SMALLINT
   DEFINE v_tipo_trabajador      CHAR(1)

   DEFINE v_tabla		 VARCHAR(25)

   #Se crea el documento con el elemento root 
   LET doc = xml.DomDocument.CreateDocument("SalidaConsultaSaldoTotal9297")
   #obtenemos el nodo principal
   LET elt = doc.getDocumentElement()
   
   #Se ejecuta la consulta del nss
   EXECUTE exe_consulta_nss USING nss INTO   v_resultado_consulta.id_derechohabiente,
                                             v_resultado_consulta.nss,
                                             v_resultado_consulta.rfc,
                                             v_resultado_consulta.nombre,
                                             v_resultado_consulta.paterno,
                                             v_resultado_consulta.materno,
                                             v_tipo_trabajador
   #Validamos si el nss esta en el sistema
   IF v_resultado_consulta.nss IS NULL THEN
      #agregamos los nodos
      CALL elt.appendChild(fn_genera_nodo("nombre","", doc))
      CALL elt.appendChild(fn_genera_nodo("apell_pat","", doc))
      CALL elt.appendChild(fn_genera_nodo("apell_mat","", doc))
      CALL elt.appendChild(fn_genera_nodo("saldosar92","", doc))
      CALL elt.appendChild(fn_genera_nodo("saldosar97","", doc))
      CALL elt.appendChild(fn_genera_nodo("rfc","", doc))
      CALL elt.appendChild(fn_genera_nodo("nss","", doc))
   
      CALL elt.appendChild(fn_genera_nodo("chdat","", doc))
      CALL elt.appendChild(fn_genera_nodo("zlog",WS_CONSULTA9297_NSS_NOT_FOUND, doc))
   ELSE  #FIN NSS no existe

      --Se busca cual es la tabla disponible
      SELECT tabla_saldo 
      INTO v_tabla 
      FROM glo_saldo 
      WHERE ind_saldo = 1

      IF v_tabla = 'cta_saldo_diario' THEN
         EXECUTE exe_consulta_saldo USING v_resultado_consulta.id_derechohabiente,
                                          v_resultado_consulta.id_derechohabiente,
                                          v_resultado_consulta.id_derechohabiente
                                    INTO  v_resultado_consulta.id_derechohabiente,
                                          v_resultado_consulta.saldo_97,
                                          v_resultado_consulta.saldo_92
      ELSE
         EXECUTE exe_consulta_saldo_bis USING v_resultado_consulta.id_derechohabiente,
                                          v_resultado_consulta.id_derechohabiente,
                                          v_resultado_consulta.id_derechohabiente
                                    INTO  v_resultado_consulta.id_derechohabiente,
                                          v_resultado_consulta.saldo_97,
                                          v_resultado_consulta.saldo_92
      END IF

      IF v_resultado_consulta.saldo_97 IS NULL OR v_resultado_consulta.saldo_97 < 0 THEN
         LET v_resultado_consulta.saldo_97 = 0.00
      END IF

      IF v_resultado_consulta.saldo_92 IS NULL OR v_resultado_consulta.saldo_92 < 0 THEN
         LET v_resultado_consulta.saldo_92 = 0.00
      END IF

      LET v_resultado_consulta.f_corte = TODAY -1
      
      #agregamos los nodos
      CALL elt.appendChild(fn_genera_nodo("nombre",v_resultado_consulta.nombre, doc))
      CALL elt.appendChild(fn_genera_nodo("apell_pat",v_resultado_consulta.paterno, doc))
      CALL elt.appendChild(fn_genera_nodo("apell_mat",v_resultado_consulta.materno, doc))
      CALL elt.appendChild(fn_genera_nodo("saldosar92",v_resultado_consulta.saldo_92, doc))
      CALL elt.appendChild(fn_genera_nodo("saldosar97",v_resultado_consulta.saldo_97, doc))
      CALL elt.appendChild(fn_genera_nodo("rfc",v_resultado_consulta.rfc, doc))
      CALL elt.appendChild(fn_genera_nodo("nss",v_resultado_consulta.nss, doc))
      CALL elt.appendChild(fn_genera_nodo("chdat",v_resultado_consulta.f_corte USING 'dd-mm-yyyy', doc))

      #Se valida si es un trabajador del estado
      IF v_tipo_trabajador = 'E' THEN
         CALL elt.appendChild(fn_genera_nodo("zlog",WS_CONSULTA9297_ESTADO, doc))
      ELSE
         #Se valida si el trabajador es pensionado
         EXECUTE exe_consulta_max_pension USING nss INTO v_max_pension
         IF v_max_pension IS NOT NULL AND v_max_pension > 0 THEN  #Se encuentra la maxima resolucion en datamart
            #Se busca la resolucion con la maxima secuencia de pencion
            EXECUTE exe_consulta_datamart USING nss, 
                                                v_max_pension
                                          INTO  v_regimen,
                                                v_tpo_seguro,
                                                v_tpo_pension,
                                                v_tpo_prestacion,
                                                v_diag_registro
            IF v_tpo_prestacion <> '03' THEN #que no sea Negativa de pension
               #Registro con pension
               CALL elt.appendChild(fn_genera_nodo("zlog",WS_CONSULTA9297_PENSION_OK, doc))
            ELSE  #Resolucion de Negativa de pension
               CALL elt.appendChild(fn_genera_nodo("zlog",WS_CONSULTA9297_OK, doc))
            END IF
         ELSE  #No se encuentra el NSS en el datamart
            CALL elt.appendChild(fn_genera_nodo("zlog",WS_CONSULTA9297_OK, doc))
         END IF
      END IF
   END IF   #FIN NSS existe
   
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
