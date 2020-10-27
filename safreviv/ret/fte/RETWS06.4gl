####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETWS06.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    comprobante de pago para pago masivo de       #
#                    fondo 72                                      #
#Fecha inicio      =>10 JULIO 2013                                 #
####################################################################

GLOBALS "RETWS06.inc"
GLOBALS "RETWS05.inc"

PRIVATE DEFINE v_nss          CHAR(11)
PRIVATE DEFINE v_valida       CHAR(11)

FUNCTION fn_prepara_consulta()
   DEFINE v_consulta_pago           STRING
   DEFINE v_inserta_contacto        STRING
   DEFINE v_consulta_contacto       STRING
   DEFINE v_actualiza_contacto      STRING

   DATABASE safre_viv


   LET v_consulta_pago =   "SELECT ",
                           "ret.id_solicitud, ",
                           "ret.id_afi_fondo72, ",
                           "ret.clabe, ",
                           "det.saldo, ",
                           "det.f_aplicacion, ",
                           "ret.estado_solicitud, ",
                           "ret.cod_rechazo ",
                           "FROM ret_fondo_ahorro_masivo ret ",
                           "LEFT JOIN ret_detalle_spei det ON det.id_solicitud = ret.id_solicitud ",
                           "WHERE ret.nss = ?"
   PREPARE exe_consulta_pago FROM v_consulta_pago

   LET v_inserta_contacto = "INSERT INTO ret_comprobante_pago (id_solicitud, nss, tpo_persona, nombre, paterno, materno, calle, num_exterior,
                                                               num_interior, colonia, estado, municipio, codigo_postal, lada, telefono, celular,
                                                               correo_electronico,f_alta) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_inserta_contacto FROM v_inserta_contacto

   LET v_consulta_contacto = "SELECT nss FROM ret_comprobante_pago WHERE id_solicitud = ?"
   PREPARE exe_consulta_contacto FROM v_consulta_contacto

   LET v_actualiza_contacto = "UPDATE ret_comprobante_pago SET  tpo_persona = ?, nombre = ?, paterno = ?, materno = ?, ",
                                                               "calle = ?, num_exterior = ?, num_interior = ?, colonia = ?, ",
                                                               "estado = ?, municipio = ?, codigo_postal = ?, lada = ?, ",
                                                               "telefono = ?, celular = ?, correo_electronico = ?, f_alta = ? ",
                              "WHERE id_solicitud = ?"
   PREPARE exe_actualiza_contacto FROM v_actualiza_contacto
END FUNCTION 

FUNCTION fn_comprobantePago()
   DEFINE v_pago     consultaPago
   DEFINE v_falta    DATETIME YEAR TO SECOND

   LET v_nss = ns2request.nss
   EXECUTE exe_consulta_pago USING v_nss
                              INTO v_pago.*

   LET ns2comprobantePagoReturn.nss = v_nss
   LET ns2comprobantePagoReturn.fPago = NULL
   IF v_pago.id_solicitud IS NULL OR v_pago.id_solicitud = 0 THEN
      #Si el id_solicitud es nulo significa que el nss no se encuentra en el programa
      LET ns2comprobantePagoReturn.estatusPago = NO_ESTA_EN_PROGRAMA
      LET ns2comprobantePagoReturn.codRechazo = NULL
   ELSE
      LET ns2comprobantePagoReturn.ctaCLABE = v_pago.clabe
      LET ns2comprobantePagoReturn.estatusPago = v_pago.estado_solicitud
      LET ns2comprobantePagoReturn.codRechazo = v_pago.cod_rechazo
      IF v_pago.estado_solicitud = 71 THEN
         LET ns2comprobantePagoReturn.montoPago = v_pago.saldo
         LET ns2comprobantePagoReturn.fPago = v_pago.f_aplicacion

         LET v_falta = CURRENT YEAR TO SECOND
         #Se valida si ya se habia guardado el contacto
         LET v_valida = NULL
         EXECUTE exe_consulta_contacto USING v_pago.id_solicitud
                                       INTO v_valida
         DISPLAY v_valida
         IF v_valida IS NULL THEN
            #Insertar solicitud
            EXECUTE exe_inserta_contacto USING  v_pago.id_solicitud,
                                                ns2request.nss,
                                                ns2request.tipoPersona,
                                                ns2request.nombre,
                                                ns2request.paterno,
                                                ns2request.materno,
                                                ns2request.calle,
                                                ns2request.numExterior,
                                                ns2request.numInterior,
                                                ns2request.colonia,
                                                ns2request.estado,
                                                ns2request.municipio,
                                                ns2request.codigoPostal,
                                                ns2request.lada,
                                                ns2request.telefono,
                                                ns2request.celular,
                                                ns2request.correoElectronico,
                                                v_falta
         ELSE
            EXECUTE exe_actualiza_contacto USING   ns2request.tipoPersona,
                                                   ns2request.nombre,
                                                   ns2request.paterno,
                                                   ns2request.materno,
                                                   ns2request.calle,
                                                   ns2request.numExterior,
                                                   ns2request.numInterior,
                                                   ns2request.colonia,
                                                   ns2request.estado,
                                                   ns2request.municipio,
                                                   ns2request.codigoPostal,
                                                   ns2request.lada,
                                                   ns2request.telefono,
                                                   ns2request.celular,
                                                   ns2request.correoElectronico,
                                                   v_falta,
                                                   v_pago.id_solicitud
         END IF
         
         
      END IF
   END IF
END FUNCTION