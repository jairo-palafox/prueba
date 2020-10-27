###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS16.inc                                             #
#Objetivo          => ARCHIVO DE DEFINICION DE DATOS                          #
#Fecha Inicio      => 17-OCTUBRE-2018                                         #
###############################################################################

GLOBALS


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



#
# TYPE : tns1ConsultaRequest
#
TYPE tns1ConsultaRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaRequest",XSTypeNamespace="http://vo.consulta.saci.efp.com")
   nss CHAR(11) ATTRIBUTE(XMLName="nss"),
   curp CHAR(18) ATTRIBUTE(XMLName="curp")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1DatosTrabajador
#
TYPE tns1DatosTrabajador RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosTrabajador",XSTypeNamespace="http://vo.consulta.saci.efp.com")
  nombre VARCHAR(50) ATTRIBUTE(XMLName="nombre"),
  primerApellido VARCHAR(50) ATTRIBUTE(XMLName="primerApellido"),
  segundoApellido VARCHAR(50) ATTRIBUTE(XMLName="segundoApellido"),
  fechaNacimiento CHAR(10) ATTRIBUTE(XMLName="fechaNacimiento"),
  codSexo CHAR(1) ATTRIBUTE(XMLName="genero")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1ConsultaResponse
#
TYPE tns1ConsultaResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaResponse",XSTypeNamespace="http://vo.consulta.saci.efp.com")
  nss CHAR(11) ATTRIBUTE(XMLName="nss"),
  curp CHAR(18) ATTRIBUTE(XMLName="curp"),
  datosTrabajador tns1DatosTrabajador ATTRIBUTE(XMLName="datosTrabajador",XMLOptional),
  diagnostico CHAR(3) ATTRIBUTE(XMLName="diagnostico"),
  descripcion STRING ATTRIBUTE(XMLName="descripcion")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: ConsultaCurpSaciService
# Port:    ConsultaCurpSaci
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : WSValidaCurpHttpIn
DEFINE WSValidaCurpHttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : WSValidaCurpHttpOut
DEFINE WSValidaCurpHttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : ns2consultaRequest
DEFINE ns2consultaRequest tns1ConsultaRequest ATTRIBUTE(XMLName="consultaRequest",XMLNamespace="http://consulta.saci.efp.com")

#-------------------------------------------------------------------------------

# VARIABLE : ns2consultaCurpReturn
DEFINE ns2consultaCurpReturn tns1ConsultaResponse ATTRIBUTE(XMLName="consultaCurpReturn",XMLNamespace="http://consulta.saci.efp.com")


#Definiciones para el negocio

TYPE trabajador RECORD
  id_derechohabiente DECIMAL(9,0),
  nss                CHAR(11),
  curp               CHAR(18),
  nombre             VARCHAR(100),
  primerApellido     VARCHAR(100),
  segundoApellido    VARCHAR(100),
  fechaNacimiento    DATE
END RECORD

END GLOBALS
