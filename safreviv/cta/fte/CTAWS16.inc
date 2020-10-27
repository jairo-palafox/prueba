###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAWS16.inc                                             #
#Objetivo          => ARCHIVO DE DEFINICION DE DATOS                          #
#Fecha Inicio      => 05-SEPTIEMBRE-2017                                      #
###############################################################################

GLOBALS

   CONSTANT CONSULTA_EXITOSA        CHAR(3) = '001'
   CONSTANT SIN_REGISTROS           CHAR(3) = '002'
   CONSTANT VARIOS_REGISTROS        CHAR(3) = '003'
   CONSTANT EXITOSA_ROJA            CHAR(3) = '004'
   CONSTANT CUENTA_INACTIVA         CHAR(3) = '005'
   CONSTANT VIVIENDA_97             SMALLINT = 4
   CONSTANT VIVIENDA_92             SMALLINT = 8

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

TYPE trabajador RECORD ATTRIBUTE(XMLSequence,XSTypeName="trabajador",XSTypeNamespace="http://vo.service.saci.efp.com")
   nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
   rfc               CHAR(13)       ATTRIBUTE(XMLName="rfc"),
   curp              CHAR(18)       ATTRIBUTE(XMLName="curp"),
   apellidoPaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoPaterno"),
   apellidoMaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoMaterno"),
   nombre            VARCHAR(40)    ATTRIBUTE(XMLName="nombre"),
   afore             VARCHAR(50)    ATTRIBUTE(XMLName="afore")
END RECORD

TYPE tns1ListaTrabajaroes RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaTrabajaroes",XSTypeNamespace="http://vo.service.saci.efp.com")
  trabajador DYNAMIC ARRAY ATTRIBUTE(XMLList) OF trabajador ATTRIBUTE(XMLName="trabajador",XMLNamespace="")
END RECORD


#
# TYPE : tns2SaldoSACIRequest
#
TYPE tns2SaldoSACIRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="SaldoSACIRequest",XSTypeNamespace="http://vo.service.saci.efp.com")
   nss   STRING ATTRIBUTE(XSDMinLength="0",XSDPattern="(\\d{11})?",XMLName="nss",XMLOptional),
   rfc   STRING ATTRIBUTE(XSDMinLength="0",XMLName="rfc",XMLOptional),
   curp  STRING ATTRIBUTE(XSDMinLength="0", XSDPattern="([A-Z]{4}\\d{6}[A-Z0-9]{8})?", XMLName="curp",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2SaldoSACIResponse
#
TYPE tns2SaldoSACIResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="SaldoSACIResponse",XSTypeNamespace="http://vo.service.saci.efp.com")
   nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
   rfc               CHAR(13)       ATTRIBUTE(XMLName="rfc"),
   curp              CHAR(18)       ATTRIBUTE(XMLName="curp"),
   apellidoPaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoPaterno"),
   apellidoMaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoMaterno"),
   nombre            VARCHAR(40)    ATTRIBUTE(XMLName="nombre"),
   correo            VARCHAR(40)    ATTRIBUTE(XMLName="correo"),
   afore             VARCHAR(50)    ATTRIBUTE(XMLName="afore"),
   saldoViv92        DECIMAL(16,2)  ATTRIBUTE(XMLName="saldoViv92"),
   saldoViv97        DECIMAL(16,2)  ATTRIBUTE(XMLName="saldoViv97"),
   codRespuesta      CHAR(3)        ATTRIBUTE(XMLName="codRespuesta"),
   listaTrabajaroes  tns1ListaTrabajaroes ATTRIBUTE(XMLName="listaTrabajaroes",XMLNamespace="",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: ConstulaSaldoSACIService
# Port:    ConstulaSaldoSACI
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : ConstulaSaldoSACIHttpIn
DEFINE ConstulaSaldoSACIHttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : ConstulaSaldoSACIHttpOut
DEFINE ConstulaSaldoSACIHttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : consultaRequest
DEFINE consultaRequest RECORD ATTRIBUTE(XMLSequence,XMLName="consultarRequest",XMLNamespace="http://service.saci.efp.com")
         request tns2SaldoSACIRequest ATTRIBUTE(XMLName="request")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : consultaResponse
DEFINE consultaResponse RECORD ATTRIBUTE(XMLSequence,XMLName="consultarResponse",XMLNamespace="http://service.saci.efp.com")
         consultarReturn tns2SaldoSACIResponse ATTRIBUTE(XMLName="consultarReturn")
       END RECORD


END GLOBALS
