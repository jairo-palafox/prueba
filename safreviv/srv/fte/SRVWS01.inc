####################################################################
#Modulo            =>SRV                                           #
#Programa          =>SRVWS012.inc                                  #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>15 MARZO 2012                                 #
####################################################################



GLOBALS


#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# TYPE : tns1BuscarEdoCuentaRequest
#
TYPE tns1BuscarEdoCuentaRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="BuscarEdoCuentaRequest",XSTypeNamespace="http://vo.edoCuenta.safre.efp.com")
  curp STRING ATTRIBUTE(XMLName="curp"),
  nss STRING ATTRIBUTE(XMLName="nss")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1BuscarEdoCuentaResponse
#
TYPE tns1BuscarEdoCuentaResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="BuscarEdoCuentaResponse",XSTypeNamespace="http://vo.edoCuenta.safre.efp.com")
  diagnostico STRING ATTRIBUTE(XMLName="diagnostico"),
  listaEdoCuenta STRING ATTRIBUTE(XMLName="listaEdoCuenta"),
  nss STRING ATTRIBUTE(XMLName="nss")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1EdoCuentaRequest
#
TYPE tns1EdoCuentaRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="EdoCuentaRequest",XSTypeNamespace="http://vo.edoCuenta.safre.efp.com")
  nombreEdoCuenta STRING ATTRIBUTE(XMLName="nombreEdoCuenta"),
  nss STRING ATTRIBUTE(XMLName="nss")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1EdoCuentaResponse
#
TYPE tns1EdoCuentaResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="EdoCuentaResponse",XSTypeNamespace="http://vo.edoCuenta.safre.efp.com")
  archivo BYTE ATTRIBUTE(XMLName="archivo"),
  diagnostico STRING ATTRIBUTE(XMLName="diagnostico"),
  nss STRING ATTRIBUTE(XMLName="nss")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: EdoCuentaServiceService
# Port:    EdoCuentaService
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : ns2buscarEdoCuentaRequest
DEFINE ns2buscarEdoCuentaRequest tns1BuscarEdoCuentaRequest ATTRIBUTE(XMLName="buscarEdoCuentaRequest",XMLNamespace="http://edoCuenta.safre.efp.com")

#-------------------------------------------------------------------------------

# VARIABLE : ns2buscarEdoCuentaReturn
DEFINE ns2buscarEdoCuentaReturn tns1BuscarEdoCuentaResponse ATTRIBUTE(XMLName="buscarEdoCuentaReturn",XMLNamespace="http://edoCuenta.safre.efp.com")

#-------------------------------------------------------------------------------

# VARIABLE : ns2edoCuentaRequest
DEFINE ns2edoCuentaRequest tns1EdoCuentaRequest ATTRIBUTE(XMLName="edoCuentaRequest",XMLNamespace="http://edoCuenta.safre.efp.com")

#-------------------------------------------------------------------------------

# VARIABLE : ns2obtenerEdoCuentaReturn
DEFINE ns2obtenerEdoCuentaReturn tns1EdoCuentaResponse ATTRIBUTE(XMLName="obtenerEdoCuentaReturn",XMLNamespace="http://edoCuenta.safre.efp.com")


END GLOBALS
