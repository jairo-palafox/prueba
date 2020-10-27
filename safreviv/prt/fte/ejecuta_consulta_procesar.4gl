# Ejecuta cliente consulta cifras control procesar QA (PRTW07)
MAIN
DEFINE p_idssn RECORD
          p_idSistema      INTEGER,
          p_idEbusiness    INTEGER,
          p_idPortafolio   INTEGER,
          p_idServicio     INTEGER,
          p_idCliente      INTEGER,
          p_idCanal        INTEGER,
          p_codoperCliente VARCHAR(50),
          p_fecha          DATETIME YEAR TO FRACTION(5)
       END RECORD,
       p_cuerpo RECORD
          p_entidad       VARCHAR(3),
          p_fechaInicio   VARCHAR(10),
          p_fechaFin      VARCHAR(10),
          p_tipoSolicitud VARCHAR(2),
          p_diagnostico   VARCHAR(3)
       END RECORD,
       r_estatus INTEGER,
       r_mensaje_ws STRING
       
   LET p_idssn.p_idSistema      = ARG_VAL(1)
   LET p_idssn.p_idEbusiness    = ARG_VAL(2)
   LET p_idssn.p_idPortafolio   = ARG_VAL(3)
   LET p_idssn.p_idServicio     = ARG_VAL(4)
   LET p_idssn.p_idCliente      = ARG_VAL(5)
   LET p_idssn.p_idCanal        = ARG_VAL(6)
   LET p_idssn.p_codoperCliente = ARG_VAL(7)
   LET p_idssn.p_fecha          = ARG_VAL(8)

   
   LET p_cuerpo.p_fechaInicio   = ARG_VAL(9)
   LET p_cuerpo.p_fechaFin      = ARG_VAL(10)
   LET p_cuerpo.p_tipoSolicitud = ARG_VAL(11)
   LET p_cuerpo.p_diagnostico   = ARG_VAL(12)
   LET p_cuerpo.p_entidad       = ARG_VAL(13)

   CALL consultaCifrasProcesarMarca(p_idssn.*, p_cuerpo.*) RETURNING r_estatus,r_mensaje_ws
   DISPLAY "r_estatus:",r_estatus
   DISPLAY "r_mensaje_ws:",r_mensaje_ws

END MAIN 