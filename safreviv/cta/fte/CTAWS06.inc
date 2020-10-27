####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS06.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################


GLOBALS

#-------------------------------------------------------------------------------
# Service: ConsultaDetalles9297Service
# Port:    ConsultaDetalles9297
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : consultaDetalle9297
DEFINE consultaDetalle9297 RECORD ATTRIBUTE(XMLSequence,XMLName="consultaDetalle9297",XMLNamespace="http://service.infonavit.org.mx")
         xmlRequest STRING ATTRIBUTE(XMLName="xmlRequest")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : consultaDetalle9297Response
DEFINE consultaDetalle9297Response RECORD ATTRIBUTE(XMLSequence,XMLName="consultaDetalle9297Response",XMLNamespace="http://service.infonavit.org.mx")
         consultaDetalle9297Return STRING ATTRIBUTE(XMLName="consultaDetalle9297Return")
       END RECORD


END GLOBALS
