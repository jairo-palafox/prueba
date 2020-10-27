GLOBALS

   CONSTANT ENTIDAD_EMISORA            CHAR(5) = '04001'
   CONSTANT IP_CONSULTA                CHAR(7) = '0.0.0.0'
   CONSTANT TIPO_TRANSACCION           INTEGER = 1
   CONSTANT USUARIO                    CHAR(1) = ' '
   CONSTANT PASSWORD                   CHAR(1) = ' '
   CONSTANT URL                        STRING  = 'alias://consultaCurpRenapo'
   CONSTANT INTENTOS                   INTEGER = 5

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
# TYPE : tns1DatosConsultaCurp
#
TYPE tns1DatosConsultaCurp RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosConsultaCurp",XSTypeNamespace="http://efp.com.mx/saci/xsd")
  curp CHAR(18) ATTRIBUTE(XMLName="curp")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: ConsultaCurpRenapoService
# Port:    ConsultaPorCurpServiceHttpSoap11Endpoint
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : ConsultaPorCurpHttpIn
DEFINE ConsultaPorCurpHttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : ConsultaPorCurpHttpOut
DEFINE ConsultaPorCurpHttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : v_request
DEFINE v_request RECORD ATTRIBUTE(XMLSequence,XMLName="consultarPorCurp",XMLNamespace="http://efp.com.mx/saci")
         datos tns1DatosConsultaCurp ATTRIBUTE(XMLName="datos")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : v_response
DEFINE v_response RECORD ATTRIBUTE(XMLSequence,XMLName="consultarPorCurpResponse",XMLNamespace="http://efp.com.mx/saci")
         return STRING ATTRIBUTE(XMLName="return",XMLOptional)
       END RECORD

#-------------------------------------------------------------------------------
END GLOBALS
