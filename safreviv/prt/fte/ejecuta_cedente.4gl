
GLOBALS "cliente_cedente.inc"
MAIN
DEFINE nss CHAR(11),
       curp CHAR(18),
       num_credito VARCHAR(10),
       num_caso    DECIMAL(9,0),
       id_estatus  CHAR(5),
       correo      VARCHAR(50),
       telefono    VARCHAR(10),
       ws_error    INTEGER

   LET nss         = ARG_VAL(1)
   LET curp        = ARG_VAL(2)
   LET num_credito = ARG_VAL(3)
   LET num_caso    = ARG_VAL(4)
   LET id_estatus  = ARG_VAL(5)
   LET correo      = ARG_VAL(6)
   LET telefono    = ARG_VAL(7)

   LET ns1solicitaCreditoFovissste.mensajeEntradaSolicitud.nss               = nss
   LET ns1solicitaCreditoFovissste.mensajeEntradaSolicitud.curp              = curp
   LET ns1solicitaCreditoFovissste.mensajeEntradaSolicitud.numeroCredito     = num_credito
   LET ns1solicitaCreditoFovissste.mensajeEntradaSolicitud.numeroCaso        = num_caso
   LET ns1solicitaCreditoFovissste.mensajeEntradaSolicitud.idEstatus         = id_estatus
   LET ns1solicitaCreditoFovissste.mensajeEntradaSolicitud.correoElectronico = correo
   LET ns1solicitaCreditoFovissste.mensajeEntradaSolicitud.telCelular        = telefono

   CALL solicitaCreditoFovissste_g() RETURNING ws_error

  display ws_error
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.numeroCaso
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.nss
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.apPaterno
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.apMaterno
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.nombre
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.tipoPortabilidad
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.idEstatus
  display ns1solicitaCreditoFovisssteResponse.solicitaCreditoFovisssteReturn.diagnostico

END MAIN
