####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS11.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################



GLOBALS

#-------------------------------------------------------------------------------
# Service: ConsultaFondosService
# Port:    ConsultaFondos
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : consultaSaldoAnterior
DEFINE consultaSaldoAnterior RECORD ATTRIBUTE(XMLSequence,XMLName="consultaSaldoAnterior",XMLNamespace="http://service.infonavit.org.mx")
         xmlRequest STRING ATTRIBUTE(XMLName="xmlRequest",XMLNamespace="")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : consultaSaldoAnteriorResponse
DEFINE consultaSaldoAnteriorResponse RECORD ATTRIBUTE(XMLSequence,XMLName="consultaSaldoAnteriorResponse",XMLNamespace="http://service.infonavit.org.mx")
         consultaSaldoAnteriorReturn STRING ATTRIBUTE(XMLName="consultaSaldoAnteriorReturn",XMLNamespace="")
       END RECORD


END GLOBALS
