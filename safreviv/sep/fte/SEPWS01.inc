####################################################################
#Modulo            =>SEP                                           #
#Programa          =>"SEPWS01.inc                                  #
#Objetivo          =>Archivo de definicion de tipos de dato        #
#Fecha inicio      =>15 MAYO 2012                                  #
####################################################################


GLOBALS


#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# TYPE : tns2NotificaContactoRequest
#
TYPE tns2NotificaContactoRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="NotificaContactoRequest",XSTypeNamespace="http://vo.separacion.safre.efp.com")
  nssAsociado STRING ATTRIBUTE(XMLName="nssAsociado"),
  nssInvadido STRING ATTRIBUTE(XMLName="nssInvadido"),
  numeroCaso STRING ATTRIBUTE(XMLName="numeroCaso"),
  resultContactoAsociado INTEGER ATTRIBUTE(XMLName="resultContactoAsociado"),
  resultContactoInvadido INTEGER ATTRIBUTE(XMLName="resultContactoInvadido")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2NotificaContactoResponse
#
TYPE tns2NotificaContactoResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="NotificaContactoResponse",XSTypeNamespace="http://vo.separacion.safre.efp.com")
  diagRechazo STRING ATTRIBUTE(XMLName="diagRechazo"),
  nssAsociado STRING ATTRIBUTE(XMLName="nssAsociado"),
  nssInvadido STRING ATTRIBUTE(XMLName="nssInvadido"),
  numeroCaso STRING ATTRIBUTE(XMLName="numeroCaso"),
  resultContactoAsociado INTEGER ATTRIBUTE(XMLName="resultContactoAsociado"),
  resultContactoInvadido INTEGER ATTRIBUTE(XMLName="resultContactoInvadido"),
  resultOperacion STRING ATTRIBUTE(XMLName="resultOperacion")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: RecibeNotificacionContactoService
# Port:    RecibeNotificacionContacto
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : ns1recibeNotificacionContacto
DEFINE ns1recibeNotificacionContacto RECORD ATTRIBUTE(XMLSequence,XMLName="recibeNotificacionContacto",XMLNamespace="http://separacion.safre.efp.com")
         notificaContactoRequest tns2NotificaContactoRequest ATTRIBUTE(XMLName="notificaContactoRequest")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : ns1recibeNotificacionContactoResponse
DEFINE ns1recibeNotificacionContactoResponse RECORD ATTRIBUTE(XMLSequence,XMLName="recibeNotificacionContactoResponse",XMLNamespace="http://separacion.safre.efp.com")
         recibeNotificacionContactoReturn tns2NotificaContactoResponse ATTRIBUTE(XMLName="recibeNotificacionContactoReturn")
       END RECORD


END GLOBALS
