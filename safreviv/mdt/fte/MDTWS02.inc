####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTWS02.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>05 MARZO 2012                                 #
####################################################################


GLOBALS


#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# TYPE : tns2ValidaInstruccionRequest
#
TYPE tns2ValidaInstruccionRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="ValidaInstruccionRequest",XSTypeNamespace="http://vo.mandatos.safre.efp.com")
  id_origen INTEGER ATTRIBUTE(XMLName="id_origen"),
  nss STRING ATTRIBUTE(XMLName="nss"),
  id_credito DECIMAL(10) ATTRIBUTE(XMLName="id_credito"),
  --id_mandato STRING ATTRIBUTE(XMLName="id_mandato"),
  cve_mandato STRING ATTRIBUTE(XMLName="cve_mandato"),
  tpo_descuento INTEGER ATTRIBUTE(XMLName="tpo_descuento"),
  valor_descuento DECIMAL(24) ATTRIBUTE(XMLName="valor_descuento"),
  f_canales DATE ATTRIBUTE(XMLName="f_canales"),
  f_inicio_mandato DATE ATTRIBUTE(XMLName="f_inicio_mandato"),
  f_culmina_mandato DATE ATTRIBUTE(XMLName="f_culmina_mandato"),
  referencia STRING ATTRIBUTE(XMLName="referencia"),
  id_canales DECIMAL(9) ATTRIBUTE(XMLName="id_canales"),
  tipo_operacion STRING ATTRIBUTE(XMLName="tipo_operacion")
  
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2ValidaInstruccionResponse
#
TYPE tns2ValidaInstruccionResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="ValidaInstruccionResponse",XSTypeNamespace="http://vo.mandatos.safre.efp.com")
  id_origen INTEGER ATTRIBUTE(XMLName="id_origen"),
  nss STRING ATTRIBUTE(XMLName="nss"),
  id_credito DECIMAL(10) ATTRIBUTE(XMLName="id_credito"),
  --id_mandato STRING ATTRIBUTE(XMLName="id_mandato"),
  cve_mandato STRING ATTRIBUTE(XMLName="cve_mandato"),
  tpo_descuento INTEGER ATTRIBUTE(XMLName="tpo_descuento"),
  valor_descuento DECIMAL(24) ATTRIBUTE(XMLName="valor_descuento"),
  f_canales DATE ATTRIBUTE(XMLName="f_canales"),
  f_inicio_mandato DATE ATTRIBUTE(XMLName="f_inicio_mandato"),
  f_culmina_mandato DATE ATTRIBUTE(XMLName="f_culmina_mandato"),
  referencia STRING ATTRIBUTE(XMLName="referencia"),
  id_canales DECIMAL(9) ATTRIBUTE(XMLName="id_canales"),
  tipo_operacion STRING ATTRIBUTE(XMLName="tipo_operacion"),
  resultado_operacion STRING ATTRIBUTE(XMLName="resultado_operacion"),
  diagnostico STRING ATTRIBUTE(XMLName="diagnostico")
  
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: ValidaInstruccionMandatosServiceService
# Port:    ValidaInstruccionMandatosService
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : ns1valida_instruccion_mdt_can
DEFINE ns1valida_instruccion_mdt_can RECORD ATTRIBUTE(XMLSequence,XMLName="valida_instruccion_mdt_can",XMLNamespace="http://mandatos.safre.efp.com")
         validaInstruccionRequest tns2ValidaInstruccionRequest ATTRIBUTE(XMLName="validaInstruccionRequest")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : ns1valida_instruccion_mdt_canResponse
DEFINE ns1valida_instruccion_mdt_canResponse RECORD ATTRIBUTE(XMLSequence,XMLName="valida_instruccion_mdt_canResponse",XMLNamespace="http://mandatos.safre.efp.com")
         valida_instruccion_mdt_canReturn tns2ValidaInstruccionResponse ATTRIBUTE(XMLName="valida_instruccion_mdt_canReturn")
       END RECORD


END GLOBALS
