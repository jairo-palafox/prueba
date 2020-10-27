GLOBALS "ret_saldo_preliminar.inc" -- saldo prelimminar PROCESAR

MAIN
DEFINE v_resultado   SMALLINT,
       v_diagnostico SMALLINT,      -- respuesta del WS, 101 - Aceptado
       v_estatus     SMALLINT,      -- estatus de la cuenta individual
       v_aivs_viv92  DECIMAL(14,6), -- monto en aivs de vivienda 92
       v_pesos_viv92 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_aivs_viv97  DECIMAL(14,6), -- monto en aivs de vivienda 97
       v_pesos_viv97 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_total_aivs  DECIMAL(14,6), -- monto total en aivs
       v_total_pesos DECIMAL(12,2)  -- monto total en pesos

    -- se inicializan las variables de retorno del saldo
    LET v_aivs_viv92 = 0
    LET v_aivs_viv97 = 0

    LET v_pesos_viv92 = 0
    LET v_pesos_viv97 = 0
       
    DISPLAY "Prueba de comunicacion con PROCESAR, saldo previo"
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 49
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = 30 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = "INFONAVIT"    
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION

    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.curp = "LEMR420215HNECNY05"
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = "01004200265"
  
    -- se invoca el servicio
    DISPLAY "Ejecutando WSSaldoPreliminar..."
    CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado


    IF ( v_resultado <> 0 ) THEN      

        DISPLAY "Resultado de la ejecucion: ", v_resultado
        DISPLAY "CODE       : ", wsError.code
        DISPLAY "CODENS     : ", wsError.codeNS
        DISPLAY "DESCRIPTION: ", wsError.description
        DISPLAY "ACTION     : ", wsError.action
        DISPLAY "=========================================================\n"

        -- se devuelve el codigo de error del WS y fecha nula
        RETURN wsError.code, NULL
    ELSE 
        LET v_diagnostico = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
        LET v_estatus     = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual
        LET v_aivs_viv92  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92AIVS
        LET v_pesos_viv92 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92
        LET v_aivs_viv97  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97AIVS
        LET v_pesos_viv97 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97

        DISPLAY "Estatos FOVISSSTE  :", ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusViviendaF
        DISPLAY "Estatus INFONAVIT  :", ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusViviendaI
        IF v_diagnostico = 101 AND v_estatus = 101 THEN -- procede la solicitud sin error se devuelven los saldos de las subcuentas
            DISPLAY "Saldo AIVS  VIV 92 :", v_aivs_viv92
            DISPLAY "Saldo PESOS VIV 92 :", v_pesos_viv92
            DISPLAY "Saldo AIVS  VIV 97 :", v_aivs_viv97
            DISPLAY "Saldo PESOS VIV 97 :", v_pesos_viv92
        ELSE
            DISPLAY "Los saldos se devuelven en Cero (0) ya que la cuenta no esta disponible Diagnóstico:", v_diagnostico , " estatus: ", v_estatus
        END IF 
    END IF

  --DISPLAY "Prueba de comunicacion con PROCESAR, cuenta clabe"


END MAIN