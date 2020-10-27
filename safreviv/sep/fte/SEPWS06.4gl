####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPWS02.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         # 
#                    Consulta de Estado del Expediente             #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

IMPORT FGL WSHelper

GLOBALS "SEPWS05.inc"

FUNCTION fn_prepara_consultas()
   DEFINE v_consulta_caso           STRING
   DEFINE v_consulta_expediente     STRING

   DATABASE safre_viv

   LET v_consulta_caso =   "SELECT FIRST 1 ",
                              "expediente.id_expediente, ",
                              "expediente.estado, ",
                              "cat_expediente.descripcion ",
                           "FROM sep_expediente expediente ",
                           "INNER JOIN sep_estado_expediente cat_expediente ON cat_expediente.estado = expediente.estado ", 
                           "WHERE caso_adai = ? "
   PREPARE exe_consulta_caso FROM v_consulta_caso

   LET v_consulta_expediente =   "SELECT FIRST 1 ",
                                    "id_nss_expediente ",
                                 "FROM sep_nss_expediente ",
                                 "WHERE id_expediente = ? ",
                                 "AND nss = ? ",
                                 "AND tipo_nss = ? "
   PREPARE exe_consulta_expediente FROM v_consulta_expediente
END FUNCTION

FUNCTION fn_consulta_estado_expediente()
   DEFINE v_nssAsociado                CHAR(11)
   DEFINE v_nssInvadido                CHAR(11)
   DEFINE v_numeroCaso                 INTEGER
   DEFINE v_id_expediente              INTEGER
   DEFINE v_estado                     INTEGER
   DEFINE v_desc_estado                VARCHAR(40)
   DEFINE v_tipo_nss                   INTEGER
   DEFINE v_id_nss_expediente_inv      INTEGER
   DEFINE v_id_nss_expediente_asc      INTEGER

   LET v_numeroCaso = ns1consultaEstadoExpediente.estadoExpedienteRequest.numeroCaso
   LET v_nssAsociado = ns1consultaEstadoExpediente.estadoExpedienteRequest.nssAsociado
   LET v_nssInvadido = ns1consultaEstadoExpediente.estadoExpedienteRequest.nssInvadido
   
   #Primero se llenan los valores fijos de la respuesta
   LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.numeroCaso = ns1consultaEstadoExpediente.estadoExpedienteRequest.numeroCaso
   LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.nssAsociado = ns1consultaEstadoExpediente.estadoExpedienteRequest.nssAsociado
   LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.nssInvadido = ns1consultaEstadoExpediente.estadoExpedienteRequest.nssInvadido

   #Se valida que exista el caso en la tabla sep_expediente
   EXECUTE exe_consulta_caso USING v_numeroCaso INTO v_id_expediente, v_estado, v_desc_estado
   IF  v_id_expediente IS NULL OR v_id_expediente = 0 THEN
      LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.diagRechazo = '001'
      LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.resultOperacion = '02'
      RETURN
   END IF

   #Se valida que el NSS invavido se encuentre en la tabla sep_nss_expediente
   LET v_tipo_nss = 1
   EXECUTE exe_consulta_expediente USING v_id_expediente, v_nssInvadido, v_tipo_nss INTO v_id_nss_expediente_inv
   IF v_id_nss_expediente_inv IS NULL OR v_id_nss_expediente_inv = 0 THEN
      LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.diagRechazo = '002'
      LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.resultOperacion = '02'
      RETURN
   END IF

   #Se valida que el NSS asociado se encuentre en la tabla sep_nss_expediente
   LET v_tipo_nss = 2
   EXECUTE exe_consulta_expediente USING v_id_expediente, v_nssAsociado, v_tipo_nss INTO v_id_nss_expediente_asc
   IF v_id_nss_expediente_asc IS NULL OR v_id_nss_expediente_asc = 0 THEN
      LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.diagRechazo = '004'
      LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.resultOperacion = '02'
      RETURN
   END IF

   #En este punto ya se pasaron todas las validaciones
   LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.codigoEstado = v_estado
   LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.descripcion = v_desc_estado
   LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.diagRechazo = '000'
   LET ns1consultaEstadoExpedienteResponse.consultaEstadoExpedienteReturn.resultOperacion = '01'
   
END FUNCTION