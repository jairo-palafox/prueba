#-------------------------------------------------------------------------------
# File: CTAWS17.inc
#-------------------------------------------------------------------------------


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
# TYPE : tns2numericoString
#
TYPE tns2numericoString STRING ATTRIBUTE(XSDPattern="[0-9]{1,2}",XSTypeName="numericoString",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientosTypes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns1nss
#
TYPE tns1nss STRING ATTRIBUTE(XSDMinLength="0",XSDPattern="([0-9]{11})?",XSTypeName="nss",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientosTypes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns2fecha_AAAAMMDD
#
TYPE tns2fecha_AAAAMMDD STRING ATTRIBUTE(XSDPattern="[1,2]{1}[0,9]{1}[0-9]{2}[0,1]{1}[0-9]{1}[0-9]{2}",XSTypeName="fecha_AAAAMMDD",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientosTypes/")
#-------------------------------------------------------------------------------

#
# TYPE : tnsnumerico_9
#
TYPE tns2numerico_9 SMALLINT ATTRIBUTE(XSDPattern="[0-9]{1}",XSTypeName="numerico_9",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientosTypes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns1notificarGeneracionFolioEntrada
#
TYPE tns1solicitaResumenMovimientosEntrada RECORD ATTRIBUTE(XMLSequence,XSTypeName="solicitaResumenMovimientosEntrada",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientos")
  nss tns1nss ATTRIBUTE(XMLName="nss",XMLNamespace=""),
  fInicio tns2fecha_AAAAMMDD ATTRIBUTE(XMLName="fInicio",XMLNamespace=""),
  fFin tns2fecha_AAAAMMDD ATTRIBUTE(XMLName="fFin",XMLNamespace=""),
  tipoDocumento tns2numericoString ATTRIBUTE(XMLName="tipoDocumento",XMLNamespace=""),
  idSistema tns2numerico_9 ATTRIBUTE(XMLName="idSistema",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1notificarGeneracionFolioContrato
#
TYPE tns1solicitaResumenMovimientosContrato RECORD ATTRIBUTE(XMLSequence,XSTypeName="solicitaResumenMovimientosContrato",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientos")
  cuerpo tns1solicitaResumenMovimientosEntrada ATTRIBUTE(XMLName="cuerpo",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------



#
# TYPE : tns2resultadoOp
#
TYPE tns2resultadoOp STRING ATTRIBUTE(XSDEnumeration="01|02",XSTypeName="resultadoOp",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientosTypes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns2alfaNumerico_3
#
TYPE tns2alfaNumerico_3 STRING ATTRIBUTE(XSDMinLength="0",XSDMaxLength="3",XSTypeName="alfaNumerico_3",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientosTypes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns2alfaNumerico_14
#
TYPE tns2alfaNumerico_14 STRING ATTRIBUTE(XSDPattern="[A-Z0-9]{14}",XSTypeName="alfaNumerico_14",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientosTypes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns1solicitaResumenMovimientosSalida
#
TYPE tns1solicitaResumenMovimientosSalida RECORD ATTRIBUTE(XMLSequence,XSTypeName="solicitaResumenMovimientosSalida",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientos")
  resultadoOperacion tns2resultadoOp ATTRIBUTE(XMLName="resultadoOperacion",XMLNamespace=""),
  motivoRechazo tns2alfaNumerico_3 ATTRIBUTE(XMLName="motivoRechazo",XMLNamespace="",XMLOptional),
  ticketAtencion tns2alfaNumerico_14 ATTRIBUTE(XMLName="ticketAtencion",XMLNamespace="",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1solicitaResumenMovimientosRespuesta
#
TYPE tns1solicitaResumenMovimientosRespuesta RECORD ATTRIBUTE(XMLSequence,XSTypeName="solicitaResumenMovimientosRespuesta",XSTypeNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientos")
  objetoRespuesta tns1solicitaResumenMovimientosSalida ATTRIBUTE(XMLName="objetoRespuesta",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Service: SolicitaResumenMovimientosSACI
# Port:    SolicitaResumenMovimientosSACIPort
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : CTAWS17HttpIn
DEFINE CTAWS17HttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : CTAWS17HttpOut
DEFINE CTAWS17HttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : ns1notificarGeneracionFolioRequest
DEFINE ns1solicitaResumenMovimientosRequest tns1solicitaResumenMovimientosContrato ATTRIBUTE(XMLName="solicitaResumenMovimientosRequest",XMLNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientos")

#-------------------------------------------------------------------------------

# VARIABLE : ns1notificarGeneracionFolioResponse
DEFINE ns1solicitaResumenMovimientosResponse tns1solicitaResumenMovimientosRespuesta ATTRIBUTE(XMLName="solicitaResumenMovimientosResponse",XMLNamespace="http://www.infonavit.org.mx/Cuentas/SolicitaResumenMovimientosSACI/solicitaResumenMovimientos")



END GLOBALS