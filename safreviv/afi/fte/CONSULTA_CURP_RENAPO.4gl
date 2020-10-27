#Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
####################################################################
#Proyecto          => SACI VIVIENDA                                #
#Propietario       => E.F.P.                                       #
#------------------------------------------------------------------#
#Modulo            => AFI                                          #
#Programa          => CONSULTA_CURP_RENAPO                         #
#Objetivo          => Programa de prueba para consultar la CURP en #
#                     RENAPO                                       #
#Fecha Inicio      => 07 - Septiembre - 2017                       #
####################################################################

GLOBALS "AFIW08.inc"

MAIN
   DEFINE p_curp        CHAR(18) #CURP que se enviaran al WS
   DEFINE v_respuesta   respuesta_renapo   #Variable para "cachar" la respuesta del WS

   #Se llenan los parametros a enviar
   LET p_curp    = ARG_VAL(1)
   
   #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta
   #NOTA: Esta funcion se ejecuta por cada registro a notificar
   CALL fn_consulta_curp_renapo(p_curp) RETURNING v_respuesta.*

   DISPLAY "=============================================================================="
   DISPLAY "=============================RESPUESTA RENAPO================================="
   DISPLAY "Respuesta en negocio = ", v_respuesta.status_operacion
   DISPLAY "Descripcion de respuesta = ", v_respuesta.desc_status
   DISPLAY " "
   DISPLAY "CURP                 : ", v_respuesta.curp
   DISPLAY "Apellido paterno     : ", v_respuesta.apellido_paterno
   DISPLAY "Apellido materno     : ", v_respuesta.apellido_materno
   DISPLAY "Nombres              : ", v_respuesta.nombre
   DISPLAY "Sexo                 : ", v_respuesta.sexo
   DISPLAY "Fecha nacimiento     : ", v_respuesta.fecha_nacimiento
   DISPLAY "Nacionalidad         : ", v_respuesta.nacionalidad
   DISPLAY "Tipo doc probatorio  : ", v_respuesta.tipo_doc_probatorio
   DISPLAY "Año registro         : ", v_respuesta.anio_registro
   DISPLAY "Foja                 : ", v_respuesta.foja
   DISPLAY "Tomo                 : ", v_respuesta.tomo
   DISPLAY "Libro                : ", v_respuesta.libro
   DISPLAY "Num acta             : ", v_respuesta.num_acta
   DISPLAY "CRIP                 : ", v_respuesta.crip
   DISPLAY "Clave ent registro   : ", v_respuesta.cve_entidad_reg
   DISPLAY "Clave mun registro   : ", v_respuesta.cve_municipio_reg
   DISPLAY "Numero reg extrangero: ", v_respuesta.num_reg_extrangeros
   DISPLAY "Folio carta          : ", v_respuesta.filio_carta
   DISPLAY "Clave ent nacimiento : ", v_respuesta.cve_ent_nacimiento
   DISPLAY "Clave ent emisora    : ", v_respuesta.cve_ent_emisora
   DISPLAY "Status CURP          : ", v_respuesta.status_curp
   DISPLAY " "
   DISPLAY "=============================================================================="
   DISPLAY "=============================================================================="
      
END MAIN
