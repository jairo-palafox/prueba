###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#MODULO            => RET                                                     #
#PROGRAMA          => RETM395                                                 #
#OBJETIVO          => Realizar la actualizacion y desmarca de solicitudes en  #
#                     ret_solicitud_generico con f_solicitud igual o mayor a  #
#                     30 dias                                                 #
#FECHA INICIO      => Octubre de 2015                                         #
###############################################################################
DATABASE safre_viv
GLOBALS "ret_saldo_preliminar.inc" -- saldo prelimminar PROCESAR
GLOBALS "RETG01.4gl"

MAIN
DEFINE v_resultado                SMALLINT,
       v_diagnostico              SMALLINT,      -- respuesta del WS, 101 - Aceptado
       v_estatus                  SMALLINT,      -- estatus de la cuenta individual
       v_sql                      STRING,
       v_id_solicitud             DECIMAL(9,0),
       v_nss                      CHAR(11),
       p_proceso_cod              SMALLINT,
       --Variable para realizar el desmarcado de la solicitud
       rec_marca                  RECORD LIKE sfr_marca_activa.*,
       p_usuario                  CHAR(20),
       p_tpo_ejecucion            SMALLINT,
       p_nom_ventana              STRING,
       v_solicitudes_desmarcadas  INTEGER,
       v_mensaje                  STRING

    -- Se obtienen los valores de la ejecucion
    LET p_usuario       = ARG_VAL(1)
    LET p_tpo_ejecucion = ARG_VAL(2)
    LET p_nom_ventana   = ARG_VAL(3)

    LET p_proceso_cod = g_proceso_cod_consulta_pago_fico_ret_generico

    LET v_solicitudes_desmarcadas = 0

    -- Se inicia el log del programa
    CALL STARTLOG (p_usuario CLIPPED||".RETM395.log")

    -- Parametors para realizar la consulta por Web Service
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 60 -- Consultar y desmarcar
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = 30 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 7
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = "INFONAVIT"    
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION
    -- Solo se ocupara el nss, el curp se mandara en blanco
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.curp = ""
    
    DISPLAY "Comunicación con PROCESAR, saldo previo"

    -- Solicitudes mayores a 30 dias a partir de la fecha de ejecucion del programa
    -- marcadas con 815
    LET v_sql = "SELECT rsg.id_solicitud, rsg.nss, sma.id_derechohabiente,"||
                 "sma.marca, sma.n_referencia, sma.marca_causa"||
                 "FROM   ret_solicitud_generico rsg, sfr_marca_activa sma"||
                 "WHERE  rsg.id_solicitud = sma.n_referencia"||
                 "AND    marca = 815"||
                 "AND    (TODAY - f_solicitud) >= 30"
    PREPARE prp_solicitudes FROM v_sql
    DECLARE cur_solicitudes CURSOR FOR prp_solicitudes
    
    FOREACH cur_solicitudes INTO v_id_solicitud, v_nss,
                                 rec_marca.id_derechohabiente, rec_marca.marca,
                                 rec_marca.n_referencia, rec_marca.marca_causa
                                 

       -- La busqueda a traves del WS sera mediante NSS
       LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = v_nss

       -- se invoca el servicio
       DISPLAY "Ejecutando WSSaldoPreliminar..."
       CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado

       -- Si el consumo del WS fue exitoso
       IF v_resultado == 1 THEN

          -- Se recupera los datos de la consulta
          LET v_diagnostico = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
          LET v_estatus     = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual

          -- si la solicitud existe en la AFORE se desmarca aqui también
          IF v_diagnostico = 101 THEN

              DISPLAY "Desmarcando solicitud:   ",v_id_solicitud,". NSS:    ",v_nss

              -- Se prepara la desmarca
              LET v_sql = "EXECUTE FUNCTION fn_desmarca_cuenta(",rec_marca.id_derechohabiente,","
                                                                ,rec_marca.marca,","
                                                                ,rec_marca.n_referencia,","
                                                                ,"0,"
                                                                ,rec_marca.marca_causa,","
                                                                ,'"SAFREVIV",'
                                                                ,p_proceso_cod,")"
              PREPARE prp_desmarca FROM v_sql
              EXECUTE prp_desmarca INTO v_resultado

              -- Se actualiza la solicitud en ret_solicitud_generico
              UPDATE ret_solicitud_generico
              SET    estado_solcitud = 100,
                     cod_rechazo     = 839
              WHERE  id_solicitud    = v_id_solicitud

              LET v_solicitudes_desmarcadas = v_solicitudes_desmarcadas + 1
          END IF -- existe solicitud en la afore
       ELSE
          DISPLAY "id_solicitud:                ",v_id_solicitud
          DISPLAY "Resultado de la ejecucion:   ", v_resultado
          DISPLAY "CODE       :                 ", wsError.code
          DISPLAY "CODENS     :                 ", wsError.codeNS
          DISPLAY "DESCRIPTION:                 ", wsError.description
          DISPLAY "ACTION     :                 ", wsError.action
          DISPLAY ""
       END IF -- resultado = 1

    END FOREACH

    IF v_solicitudes_desmarcadas > 1 THEN
        LET v_mensaje = "Se desmarcaron "||v_solicitudes_desmarcadas||" solicitudes."
    ELSE
        IF v_solicitudes_desmarcadas = 1 THEN
           LET v_mensaje = "Se desmarco "||v_solicitudes_desmarcadas||" solicitud."
        END IF
    END IF

    DISPLAY BY NAME v_mensaje

END MAIN