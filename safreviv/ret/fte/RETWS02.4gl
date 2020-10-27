####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETWS02.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    consulta de elegibilidad para pago masivo de  #
#                    fondo 72                                      #
#Fecha inicio      =>10 JULIO 2013                                 #
####################################################################

GLOBALS "RETWS02.inc"
GLOBALS "RETWS01.inc"

PRIVATE DEFINE v_nss          CHAR(11)

FUNCTION fn_prepara_consulta()
   DEFINE v_consulta_pago     STRING

   DATABASE safre_viv


   LET v_consulta_pago =   "SELECT ",
                              "ret.nss, ",
                              "afi.rfc, ",
                              "afi.nombre, ",
                              "ret.estado_solicitud, ",
                              "ret.importe_viv72, ",
                              "ret.cod_rechazo ",
                           "FROM ret_fondo_ahorro_masivo ret ",
                           "INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72 ",
                           "WHERE ret.nss = ?"
   PREPARE exe_consulta_pago FROM v_consulta_pago
END FUNCTION 

FUNCTION fn_consultaElegibilidad()
   DEFINE v_pago     consultaPago

   LET v_nss = consultaRequest.nssRequest
   EXECUTE exe_consulta_pago USING v_nss
                              INTO v_pago.*

   LET ns1consultaElegibilidadReturn.nss = v_nss
   IF v_pago.nss IS NULL OR v_pago.nss = "" THEN
      #Si el NSS es nulo significa que el nss no se encuentra en el programa
      LET ns1consultaElegibilidadReturn.estatusPago = NO_ESTA_EN_PROGRAMA
      LET ns1consultaElegibilidadReturn.fechaPago = NULL
      LET ns1consultaElegibilidadReturn.codigoRechazo = NULL
   ELSE
      LET ns1consultaElegibilidadReturn.rfc = v_pago.rfc
      LET ns1consultaElegibilidadReturn.nombre = v_pago.nombre
      LET ns1consultaElegibilidadReturn.montoPago = v_pago.monto_pago
      LET ns1consultaElegibilidadReturn.estatusPago = v_pago.estatusPago
      LET ns1consultaElegibilidadReturn.codigoRechazo = v_pago.codigoRechazo
      IF v_pago.estatusPago = 100 THEN
         IF v_pago.codigoRechazo < 300 OR v_pago.codigoRechazo > 399 THEN
            LET ns1consultaElegibilidadReturn.fechaPago = NULL
         ELSE
            CALL fn_calcula_fecha_pago()
         END IF
      ELSE
         LET ns1consultaElegibilidadReturn.estatusPago = v_pago.estatusPago
         LET ns1consultaElegibilidadReturn.codigoRechazo = NULL
         CALL fn_calcula_fecha_pago()
      END IF
   END IF
END FUNCTION

PRIVATE FUNCTION fn_calcula_fecha_pago()
   DEFINE v_num_pago             SMALLINT
   DEFINE v_fPago                DATE

   LET v_num_pago = v_nss[11]
   IF v_num_pago IS NULL OR v_num_pago = 0 THEN
      LET v_num_pago = 10
   END IF
   
   SELECT
      f_pago
   INTO
      v_fPago
   FROM ret_ctr_pago_masivo
   where num_pago = v_num_pago
   
   LET ns1consultaElegibilidadReturn.fechaPago = v_fPago
END FUNCTION