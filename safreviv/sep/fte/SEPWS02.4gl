####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPWS02.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         # 
#                    Notificacion de contacto                      #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

IMPORT FGL WSHelper

GLOBALS "SEPWS01.inc"
GLOBALS "SEPWS02.inc"

FUNCTION fn_prepara_consultas()
   DEFINE v_consulta_caso           STRING
   DEFINE v_consulta_expediente     STRING

   DATABASE safre_viv

   LET v_consulta_caso = " SELECT FIRST 1 ",
                          "id_expediente ",
                     "FROM sep_expediente ",
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

FUNCTION fn_recibe_notificacion_contacto()
   DEFINE v_nssAsociado                CHAR(11)
   DEFINE v_nssInvadido                CHAR(11)
   DEFINE v_numeroCaso                 INTEGER
   DEFINE v_id_expediente              INTEGER
   DEFINE v_id_nss_expediente_inv      INTEGER
   DEFINE v_id_nss_expediente_asc      INTEGER
   DEFINE v_tipo_nss                   INTEGER

   LET v_nssAsociado = ns1recibeNotificacionContacto.notificaContactoRequest.nssAsociado
   LET v_nssInvadido = ns1recibeNotificacionContacto.notificaContactoRequest.nssInvadido
   LET v_numeroCaso = ns1recibeNotificacionContacto.notificaContactoRequest.numeroCaso
   

   #Primero se llenan los valores fijos de la respuesta
   LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.nssAsociado = ns1recibeNotificacionContacto.notificaContactoRequest.nssAsociado
   LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.nssInvadido = ns1recibeNotificacionContacto.notificaContactoRequest.nssInvadido
   LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.numeroCaso = ns1recibeNotificacionContacto.notificaContactoRequest.numeroCaso
   LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultContactoAsociado = ns1recibeNotificacionContacto.notificaContactoRequest.resultContactoAsociado
   LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultContactoInvadido = ns1recibeNotificacionContacto.notificaContactoRequest.resultContactoInvadido

   #Se valida que exista el caso en la tabla sep_expediente
   EXECUTE exe_consulta_caso USING v_numeroCaso INTO v_id_expediente
   IF  v_id_expediente IS NULL OR v_id_expediente = 0 THEN
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.diagRechazo = '001'
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultOperacion = '02'
      RETURN
   END IF

   #Se valida que el NSS invavido se encuentre en la tabla sep_nss_expediente
   LET v_tipo_nss = 1
   EXECUTE exe_consulta_expediente USING v_id_expediente, v_nssInvadido, v_tipo_nss INTO v_id_nss_expediente_inv
   IF v_id_nss_expediente_inv IS NULL OR v_id_nss_expediente_inv = 0 THEN
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.diagRechazo = '002'
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultOperacion = '02'
      RETURN
   END IF

   #Se verifica que el valor del resultado de contacto para el invadido sea valido
   IF ns1recibeNotificacionContacto.notificaContactoRequest.resultContactoInvadido != 0 
      AND ns1recibeNotificacionContacto.notificaContactoRequest.resultContactoInvadido != 1 THEN
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.diagRechazo = '003'
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultOperacion = '02'
      RETURN
   END IF

   #Se valida que el NSS asociado se encuentre en la tabla sep_nss_expediente
   LET v_tipo_nss = 2
   EXECUTE exe_consulta_expediente USING v_id_expediente, v_nssAsociado, v_tipo_nss INTO v_id_nss_expediente_asc
   IF v_id_nss_expediente_asc IS NULL OR v_id_nss_expediente_asc = 0 THEN
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.diagRechazo = '004'
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultOperacion = '02'
      RETURN
   END IF

   #Se verifica que el valor del resultado de contacto para el asociado sea valido
   IF ns1recibeNotificacionContacto.notificaContactoRequest.resultContactoAsociado != 0 
      AND ns1recibeNotificacionContacto.notificaContactoRequest.resultContactoAsociado != 1 THEN
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.diagRechazo = '005'
      LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultOperacion = '02'
      RETURN
   END IF

   #En este punto ya se pasaron todas las validaciones
   LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.diagRechazo = '000'
   LET ns1recibeNotificacionContactoResponse.recibeNotificacionContactoReturn.resultOperacion = '01'
END FUNCTION