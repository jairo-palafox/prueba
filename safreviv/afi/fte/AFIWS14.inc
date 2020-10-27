###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS14.inc                                             #
#Objetivo          => ARCHIVO DE DEFINICION DE DATOS                          #
#Fecha Inicio      => 29-NOVIEMBRE-2017                                       #
###############################################################################

GLOBALS

   CONSTANT ACTUALIZACION_EXITOSA    CHAR(3) = '001'
   CONSTANT ACTUALIZACION_RECHAZADA  CHAR(3) = '002'
   CONSTANT PERFIL_DATOS_CONTACTO    CHAR(5) = 'ACTIN'
   CONSTANT PERFIL_CONSULTA_ROJA     CHAR(5) = 'CONSR'
   

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

#
# TYPE : telefonoTrabajador
#
TYPE telefonoTrabajador RECORD ATTRIBUTE(XMLSequence,XSTypeName="telefonoTrabajador",XSTypeNamespace="http://vo.service.saci.efp.com")
  tpo_telefono          INTEGER ATTRIBUTE(XMLName="tipoTelefono",XMLNamespace=""),
  cve_lada              CHAR(3) ATTRIBUTE(XMLName="claveLada",XMLNamespace="",XMLOptional),
  telefono              CHAR(10) ATTRIBUTE(XMLName="telefono",XMLNamespace=""),
  extension             CHAR(10) ATTRIBUTE(XMLName="extension",XMLNamespace="",XMLOptional)
END RECORD

#
# TYPE : correoTrabajador
#
TYPE correoTrabajador RECORD ATTRIBUTE(XMLSequence,XSTypeName="correoTrabajador",XSTypeNamespace="http://vo.service.saci.efp.com")
  tpo_correo            INTEGER ATTRIBUTE(XMLName="tipoCorreo",XMLNamespace=""),
  correo                VARCHAR(60) ATTRIBUTE(XMLName="correo",XMLNamespace="")
END RECORD

#
# TYPE : tns1ListaDomicilios
#
TYPE tns1ListaDomicilios RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaDomicilios",XSTypeNamespace="http://vo.service.saci.efp.com")
  domicilio DYNAMIC ARRAY ATTRIBUTE(XMLList) OF domicilioTrabajador ATTRIBUTE(XMLName="domicilio",XMLNamespace="")
END RECORD

#
# TYPE : tns1ListaTelefonos
#
TYPE tns1ListaTelefonos RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaTelefonos",XSTypeNamespace="http://vo.service.saci.efp.com")
  telefono DYNAMIC ARRAY ATTRIBUTE(XMLList) OF telefonoTrabajador ATTRIBUTE(XMLName="telefono",XMLNamespace="")
END RECORD

#
# TYPE : tns1ListaCorreos
#
TYPE tns1ListaCorreos RECORD ATTRIBUTE(XMLSequence,XSTypeName="listaCorreos",XSTypeNamespace="http://vo.service.saci.efp.com")
  correo DYNAMIC ARRAY ATTRIBUTE(XMLList) OF correoTrabajador ATTRIBUTE(XMLName="correo",XMLNamespace="")
END RECORD

#
# TYPE : tns2DatosSACIRequest
#
TYPE tns2DatosSACIRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosSACIRequest",XSTypeNamespace="http://vo.service.saci.efp.com")
   usuarioCod        VARCHAR(40)    ATTRIBUTE(XMLName="usuarioActualiza"),
   nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
   rfc               CHAR(13)       ATTRIBUTE(XMLName="rfc",XMLOptional),
   curp              CHAR(18)       ATTRIBUTE(XMLName="curp",XMLOptional),
   apellidoPaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoPaterno",XMLOptional),
   apellidoMaterno   VARCHAR(40)    ATTRIBUTE(XMLName="apellidoMaterno",XMLOptional),
   nombre            VARCHAR(40)    ATTRIBUTE(XMLName="nombre",XMLOptional),
   afore             VARCHAR(50)    ATTRIBUTE(XMLName="afore",XMLOptional),
   fNacimiento       DATE           ATTRIBUTE(XMLName="fechaDeNacimiento",XMLOptional),
   genero            CHAR(1)        ATTRIBUTE(XMLName="genero",XMLOptional),
   indFallecido      INTEGER        ATTRIBUTE(XMLName="indFallecido",XMLOptional),
   listaDomicilios   tns1ListaDomicilios     ATTRIBUTE(XMLName="listaDomicilios",XMLNamespace="",XMLOptional),
   listaTelefonos    tns1ListaTelefonos      ATTRIBUTE(XMLName="listaTelefonos",XMLNamespace="",XMLOptional),
   listaCorreos      tns1ListaCorreos        ATTRIBUTE(XMLName="listaCorreos",XMLNamespace="",XMLOptional),
   id_origen_cambio  INTEGER        ATTRIBUTE(XMLName="origenCambio")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2DatosSACIResponse
#
TYPE tns2DatosSACIResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosSACIResponse",XSTypeNamespace="http://vo.service.saci.efp.com")
   codRespuesta      CHAR(3)        ATTRIBUTE(XMLName="codRespuesta"),
   descripcion       VARCHAR(200)   ATTRIBUTE(XMLName="descripcion")
END RECORD
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : ActualizaDatosSACIHttpIn
DEFINE ActualizaDatosSACIHttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : ActualizaDatosSACIHttpOut
DEFINE ActualizaDatosSACIHttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : actualizaRequest
DEFINE actualizaRequest RECORD ATTRIBUTE(XMLSequence,XMLName="actualizaRequest",XMLNamespace="http://service.saci.efp.com")
         request tns2DatosSACIRequest ATTRIBUTE(XMLName="request")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : actualizaResponse
DEFINE actualizaResponse RECORD ATTRIBUTE(XMLSequence,XMLName="actualizaResponse",XMLNamespace="http://service.saci.efp.com")
         actualizaReturn tns2DatosSACIResponse ATTRIBUTE(XMLName="consultarReturn")
       END RECORD


END GLOBALS
