###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS13.inc                                             #
#Objetivo          => ARCHIVO DE DEFINICION DE DATOS                          #
#Fecha Inicio      => 29-NOVIEMBRE-2017                                       #
###############################################################################


GLOBALS

   CONSTANT CONSULTA_EXITOSA        CHAR(3) = '001'
   CONSTANT SIN_REGISTROS           CHAR(3) = '002'
   CONSTANT VARIOS_REGISTROS        CHAR(3) = '003'
   CONSTANT EXITOSA_ROJA            CHAR(3) = '004'
   CONSTANT CUENTA_INACTIVA         CHAR(3) = '005'

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

#
# TYPE : domicilioTrabajador
#
TYPE domicilioTrabajador RECORD ATTRIBUTE(XMLSequence,XSTypeName="domicilioTrabajador",XSTypeNamespace="http://vo.service.saci.efp.com")
  calle                 VARCHAR(65) ATTRIBUTE(XMLName="calle",XMLNamespace=""),
  numeroExterior        VARCHAR(25) ATTRIBUTE(XMLName="numeroExterior",XMLNamespace="",XMLOptional),
  numeroInterior        VARCHAR(25) ATTRIBUTE(XMLName="numeroInterior",XMLNamespace="",XMLOptional),
  colonia               VARCHAR(65) ATTRIBUTE(XMLName="colonia",XMLNamespace=""),
  delegacionOMunicipio  VARCHAR(65) ATTRIBUTE(XMLName="delegacionOMunicipio",XMLNamespace=""),
  codigoPostal          CHAR(5)     ATTRIBUTE(XMLName="codigoPostal",XMLNamespace=""),
  entidadFederativa     VARCHAR(65) ATTRIBUTE(XMLName="entidadFederativa",XMLNamespace=""),
  pais                  CHAR(3)     ATTRIBUTE(XMLName="pais",XMLNamespace=""),
  tpo_domicilio         CHAR(1)     ATTRIBUTE(XMLName="tipoDomicilio",XMLNamespace="")
END RECORD

TYPE telefonoTrabajador RECORD ATTRIBUTE(XMLSequence,XSTypeName="telefonoTrabajador",XSTypeNamespace="http://vo.service.saci.efp.com")
  tpo_telefono          INTEGER ATTRIBUTE(XMLName="tipoTelefono",XMLNamespace=""),
  cve_lada              CHAR(3) ATTRIBUTE(XMLName="claveLada",XMLNamespace="",XMLOptional),
  telefono              CHAR(10) ATTRIBUTE(XMLName="telefono",XMLNamespace=""),
  extension             CHAR(10) ATTRIBUTE(XMLName="extension",XMLNamespace="",XMLOptional)
END RECORD

TYPE correoTrabajador RECORD ATTRIBUTE(XMLSequence,XSTypeName="correoTrabajador",XSTypeNamespace="http://vo.service.saci.efp.com")
  tpo_correo            INTEGER ATTRIBUTE(XMLName="tipoCorreo",XMLNamespace=""),
  correo                VARCHAR(60) ATTRIBUTE(XMLName="correo",XMLNamespace="")
END RECORD

TYPE tns1ListaTrabajadores RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaTrabajadores",XSTypeNamespace="http://vo.service.saci.efp.com")
  trabajador DYNAMIC ARRAY ATTRIBUTE(XMLList) OF trabajador ATTRIBUTE(XMLName="trabajador",XMLNamespace="")
END RECORD

TYPE tns1ListaDomicilios RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaDomicilios",XSTypeNamespace="http://vo.service.saci.efp.com")
  domicilio DYNAMIC ARRAY ATTRIBUTE(XMLList) OF domicilioTrabajador ATTRIBUTE(XMLName="domicilio",XMLNamespace="")
END RECORD

TYPE tns1ListaTelefonos RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaTelefonos",XSTypeNamespace="http://vo.service.saci.efp.com")
  telefono DYNAMIC ARRAY ATTRIBUTE(XMLList) OF telefonoTrabajador ATTRIBUTE(XMLName="telefono",XMLNamespace="")
END RECORD

TYPE tns1ListaCorreos RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaCorreos",XSTypeNamespace="http://vo.service.saci.efp.com")
  correo DYNAMIC ARRAY ATTRIBUTE(XMLList) OF correoTrabajador ATTRIBUTE(XMLName="correo",XMLNamespace="")
END RECORD

#
# TYPE : tns2DatosSACIRequest
#
TYPE tns2DatosSACIRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosSACIRequest",XSTypeNamespace="http://vo.service.saci.efp.com")
   nss   STRING ATTRIBUTE(XSDMinLength="0",XSDPattern="(\\d{11})?",XMLName="nss",XMLOptional),
   rfc   STRING ATTRIBUTE(XSDMinLength="0",XMLName="rfc",XMLOptional),
   curp  STRING ATTRIBUTE(XSDMinLength="0", XSDPattern="([A-Z]{4}\\d{6}[A-Z0-9]{8})?", XMLName="curp",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2DatosSACIResponse
#
TYPE tns2DatosSACIResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosSACIResponse",XSTypeNamespace="http://vo.service.saci.efp.com")
   nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
   rfc               CHAR(13)       ATTRIBUTE(XMLName="rfc"),
   curp              CHAR(18)       ATTRIBUTE(XMLName="curp"),
   apellidoPaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoPaterno"),
   apellidoMaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoMaterno"),
   nombre            VARCHAR(40)    ATTRIBUTE(XMLName="nombre"),
   afore             VARCHAR(50)    ATTRIBUTE(XMLName="afore"),
   fNacimiento       DATE           ATTRIBUTE(XMLName="fechaDeNacimiento"),
   genero            CHAR(1)        ATTRIBUTE(XMLName="genero"),
   indFallecido      INTEGER        ATTRIBUTE(XMLName="indFallecido"),
   codRespuesta      CHAR(3)        ATTRIBUTE(XMLName="codRespuesta"),
   listaDomicilios   tns1ListaDomicilios     ATTRIBUTE(XMLName="listaDomicilios",XMLNamespace="",XMLOptional),
   listaTelefonos    tns1ListaTelefonos      ATTRIBUTE(XMLName="listaTelefonos",XMLNamespace="",XMLOptional),
   listaCorreos      tns1ListaCorreos        ATTRIBUTE(XMLName="listaCorreos",XMLNamespace="",XMLOptional),
   listaTrabajadores tns1ListaTrabajadores   ATTRIBUTE(XMLName="listaTrabajadores",XMLNamespace="",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------

TYPE trabajador_saci RECORD
   nss               CHAR(11),
   rfc               CHAR(13),
   curp              CHAR(18),
   apellidoPaterno   VARCHAR(40),
   apellidoMaterno   VARCHAR(40),
   nombre            VARCHAR(40),
   afore             VARCHAR(50),
   fNacimiento       DATE,
   genero            CHAR(1),
   indFallecido      INTEGER
END RECORD


#-------------------------------------------------------------------------------
# Service: ConstulaSaldoSACIService
# Port:    ConstulaSaldoSACI
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : ConstulaDatosSACIHttpIn
DEFINE ConstulaDatosSACIHttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : ConstulaDatosSACIHttpOut
DEFINE ConstulaDatosSACIHttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : consultaRequest
DEFINE consultaRequest RECORD ATTRIBUTE(XMLSequence,XMLName="consultarRequest",XMLNamespace="http://service.saci.efp.com")
         request tns2DatosSACIRequest ATTRIBUTE(XMLName="request")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : consultaResponse
DEFINE consultaResponse RECORD ATTRIBUTE(XMLSequence,XMLName="consultarResponse",XMLNamespace="http://service.saci.efp.com")
         consultarReturn tns2DatosSACIResponse ATTRIBUTE(XMLName="consultarReturn")
       END RECORD


END GLOBALS
