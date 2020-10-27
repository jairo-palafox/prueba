####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS02.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

GLOBALS

#-------------------------------------------------------------------------------
# Service: ConsultaTotales9297Service
# Port:    ConsultaTotales9297
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : consultaTotal9297
DEFINE consultaTotal9297 RECORD ATTRIBUTE(XMLSequence,XMLName="consultaTotal9297",XMLNamespace="http://service.infonavit.org.mx")
         xmlRequest STRING ATTRIBUTE(XMLName="xmlRequest",XMLNamespace="")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : consultaTotal9297Response
DEFINE consultaTotal9297Response RECORD ATTRIBUTE(XMLSequence,XMLName="consultaTotal9297Response",XMLNamespace="http://service.infonavit.org.mx")
         consultaTotal9297Return STRING ATTRIBUTE(XMLName="consultaTotal9297Return",XMLNamespace="")
       END RECORD


END GLOBALS
