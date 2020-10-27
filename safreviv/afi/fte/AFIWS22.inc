
GLOBALS

#
# TYPE : tConsultaNSSAfiliadoResponse_nombre
#
TYPE tConsultaNSSAfiliadoResponse_nombre STRING ATTRIBUTE(XSDMaxLength="40",XSTypeName="ConsultaNSSAfiliadoResponse_nombre",XSTypeNamespace="http://www.infonavit.org.mx/")
#-------------------------------------------------------------------------------

#
# TYPE : tConsultaNSSAfiliadoResponse_apaterno
#
TYPE tConsultaNSSAfiliadoResponse_apaterno STRING ATTRIBUTE(XSDMaxLength="40",XSTypeName="ConsultaNSSAfiliadoResponse_apaterno",XSTypeNamespace="http://www.infonavit.org.mx/")
#-------------------------------------------------------------------------------

#
# TYPE : tConsultaNSSAfiliadoResponse_amaterno
#
TYPE tConsultaNSSAfiliadoResponse_amaterno STRING ATTRIBUTE(XSDMaxLength="40",XSTypeName="ConsultaNSSAfiliadoResponse_amaterno",XSTypeNamespace="http://www.infonavit.org.mx/")
#-------------------------------------------------------------------------------

#
# TYPE : tConsultaNSSAfiliadoResponse_f_nacimiento
#
TYPE tConsultaNSSAfiliadoResponse_f_nacimiento STRING ATTRIBUTE(XSDMaxLength="10",XSTypeName="ConsultaNSSAfiliadoResponse_f_nacimiento",XSTypeNamespace="http://www.infonavit.org.mx/")
#-------------------------------------------------------------------------------

#
# TYPE : tConsultaNSSAfiliadoResponse_sexo
#
TYPE tConsultaNSSAfiliadoResponse_sexo STRING ATTRIBUTE(XSDMaxLength="30",XSTypeName="ConsultaNSSAfiliadoResponse_sexo",XSTypeNamespace="http://www.infonavit.org.mx/")
#-------------------------------------------------------------------------------

#
# TYPE : tConsultaNSSAfiliadoResponse_resul_op
#
TYPE tConsultaNSSAfiliadoResponse_resul_op STRING ATTRIBUTE(XSDMaxLength="2",XSTypeName="ConsultaNSSAfiliadoResponse_resul_op",XSTypeNamespace="http://www.infonavit.org.mx/")
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
#
# Operation: ConsultaNSSAfiliado
#
# FUNCTION: ConsultaNSSAfiliado_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ConsultaNSSAfiliado
#   OUTPUT: GLOBAL ConsultaNSSAfiliadoResponse
#
# FUNCTION: ConsultaNSSAfiliado(p_nss)
#   RETURNING: soapStatus ,p_curp ,p_nombre ,p_apaterno ,p_amaterno ,p_f_nacimiento ,p_sexo ,p_resul_op
#
# FUNCTION: ConsultaNSSAfiliadoRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ConsultaNSSAfiliado
#
# FUNCTION: ConsultaNSSAfiliadoResponse_g()
#   RETURNING: soapStatus
#   OUTPUT: GLOBAL ConsultaNSSAfiliadoResponse
#

#-------------------------------------------------------------------------------

# VARIABLE : ConsultaNSSAfiliado
DEFINE ConsultaNSSAfiliado RECORD ATTRIBUTE(XMLSequence,XMLName="ConsultaNSSAfiliado",XMLNamespace="http://www.infonavit.org.mx/")
         nss STRING ATTRIBUTE(XMLName="nss",XMLNamespace="")
       END RECORD


#-------------------------------------------------------------------------------

# VARIABLE : ConsultaNSSAfiliadoResponse
DEFINE ConsultaNSSAfiliadoResponse RECORD ATTRIBUTE(XMLSequence,XMLName="ConsultaNSSAfiliadoResponse",XMLNamespace="http://www.infonavit.org.mx/")
         curp STRING ATTRIBUTE(XMLName="curp",XMLNamespace=""),
         nombre tConsultaNSSAfiliadoResponse_nombre ATTRIBUTE(XMLName="nombre",XMLNamespace=""),
         apaterno tConsultaNSSAfiliadoResponse_apaterno ATTRIBUTE(XMLName="apaterno",XMLNamespace=""),
         amaterno tConsultaNSSAfiliadoResponse_amaterno ATTRIBUTE(XMLName="amaterno",XMLNamespace=""),
         f_nacimiento tConsultaNSSAfiliadoResponse_f_nacimiento ATTRIBUTE(XMLName="f_nacimiento",XMLNamespace=""),
         sexo tConsultaNSSAfiliadoResponse_sexo ATTRIBUTE(XMLName="sexo",XMLNamespace=""),
         resul_op tConsultaNSSAfiliadoResponse_resul_op ATTRIBUTE(XMLName="resul_op",XMLNamespace="")
       END RECORD



END GLOBALS
