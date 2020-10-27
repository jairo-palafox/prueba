###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS15.inc                                             #
#Objetivo          => ARCHIVO DE DEFINICION DE DATOS                          #
#Fecha Inicio      => 08-DICIEMBRE-2017                                       #
###############################################################################


GLOBALS

   CONSTANT CONSULTA_EXITOSA        CHAR(3) = '001'
   CONSTANT SIN_REGISTROS           CHAR(3) = '002'

#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# Global HTTP server incoming variable type definition
#
TYPE tGlobalServerHttpInputVariableType RECORD
	verb	STRING,	# HTTP request VERB
	url	STRING,	# HTTP request URL
	headers	DYNAMIC ARRAY OF RECORD	# HTTP Headers
		name	STRING,
		value	STRING
	END RECORD
END RECORD

#
# Global HTTP server outgoing variable type definition
#
TYPE tGlobalServerHttpOutputVariableType RECORD
	code	INTEGER,	# HTTP status code
	desc	STRING,	# HTTP description
	headers	DYNAMIC ARRAY OF RECORD	# HTTP Headers
		name	STRING,
		value	STRING
	END RECORD
END RECORD

TYPE entidadFederativa RECORD ATTRIBUTE(XMLSequence,XSTypeName="entidadFederativa",XSTypeNamespace="http://vo.service.saci.efp.com")
  cveEntFederativa     INTEGER              ATTRIBUTE(XMLName="cveEntFederativa",XMLNamespace=""),
  descEntFederativa    VARCHAR(50)          ATTRIBUTE(XMLName="descEntFederativa",XMLNamespace="")
END RECORD

TYPE ciudad RECORD ATTRIBUTE(XMLSequence,XSTypeName="ciudad",XSTypeNamespace="http://vo.service.saci.efp.com")
  cveCiudad     INTEGER              ATTRIBUTE(XMLName="cveCiudad",XMLNamespace=""),
  descCiudad    VARCHAR(50)          ATTRIBUTE(XMLName="descCiudad",XMLNamespace="")
END RECORD

TYPE municipio RECORD ATTRIBUTE(XMLSequence,XSTypeName="municipio",XSTypeNamespace="http://vo.service.saci.efp.com")
  cveMunicipio     INTEGER              ATTRIBUTE(XMLName="cveMunicipio",XMLNamespace=""),
  descMunicipio    VARCHAR(50)          ATTRIBUTE(XMLName="descMunicipio",XMLNamespace="")
END RECORD

TYPE colonia RECORD ATTRIBUTE(XMLSequence,XSTypeName="colonia",XSTypeNamespace="http://vo.service.saci.efp.com")
  cveColonia            INTEGER ATTRIBUTE(XMLName="cveColonia",XMLNamespace=""),
  descColonia           VARCHAR(50) ATTRIBUTE(XMLName="descColonia",XMLNamespace="")
END RECORD


TYPE tns1ListaColonias RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaColonias",XSTypeNamespace="http://vo.service.saci.efp.com")
  colonia DYNAMIC ARRAY ATTRIBUTE(XMLList) OF colonia ATTRIBUTE(XMLName="colonia",XMLNamespace="")
END RECORD

#
# TYPE : tns2CPRequest
#
TYPE tns2CPRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="CPRequest",XSTypeNamespace="http://vo.service.saci.efp.com")
   cp   CHAR(5) ATTRIBUTE(XSDMinLength="5",XSDPattern="(\\d{5})?",XMLName="cp")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2CPResponse
#
TYPE tns2CPResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="CPResponse",XSTypeNamespace="http://vo.service.saci.efp.com")
   codRespuesta         CHAR(3)              ATTRIBUTE(XMLName="codRespuesta",XMLNamespace=""),
   cp                   CHAR(5)              ATTRIBUTE(XMLName="cp",XMLNamespace=""),
   entidadFederativa    entidadFederativa    ATTRIBUTE(XMLName="entidadFederativa",XMLNamespace=""),
   ciudad               ciudad               ATTRIBUTE(XMLName="ciudad",XMLNamespace=""),
   municipio            municipio            ATTRIBUTE(XMLName="municipio",XMLNamespace=""),
   listaColonias        tns1ListaColonias    ATTRIBUTE(XMLName="listaColonias",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Service: ConstulaCPService
# Port:    ConstulaCP
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : ConstulaCPHttpIn
DEFINE ConstulaCPHttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : ConstulaCPHttpOut
DEFINE ConstulaCPHttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : consultaRequest
DEFINE consultaRequest RECORD ATTRIBUTE(XMLSequence,XMLName="consultarRequest",XMLNamespace="http://service.saci.efp.com")
         request tns2CPRequest ATTRIBUTE(XMLName="request")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : consultaResponse
DEFINE consultaResponse RECORD ATTRIBUTE(XMLSequence,XMLName="consultarResponse",XMLNamespace="http://service.saci.efp.com")
         consultarReturn tns2CPResponse ATTRIBUTE(XMLName="consultarReturn")
       END RECORD


END GLOBALS
