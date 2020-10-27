#########################################################################
#Modulo          => GRT                                                 #
#Programa        => GRTW102                                             #
#Objetivo        => Programa que consume el WS de consulta Saldo        #
#                   preeliminar con PROCESAR                            #
#Autor           => Héctor Fabián Jiménez Lara                          #
#Fecha inicio    => 12 Octubre 2015                                     #
#########################################################################

GLOBALS "GRTW101.inc" -- saldo prelimminar PROCESAR

MAIN
   DEFINE v_resultado   SMALLINT
   DEFINE v_diagnostico SMALLINT       -- respuesta del WS, 101 - Aceptado
   DEFINE v_estatus     SMALLINT       -- estatus de la cuenta individual
   DEFINE v_aivs_viv92  DECIMAL(14,6)  -- monto en aivs de vivienda 92
   DEFINE v_pesos_viv92 DECIMAL(12,2)  -- monto en pesos de vivienda 92
   DEFINE v_aivs_viv97  DECIMAL(14,6)  -- monto en aivs de vivienda 97
   DEFINE v_pesos_viv97 DECIMAL(12,2)  -- monto en pesos de vivienda 92

   -- se inicializan las variables de retorno del saldo
   LET v_aivs_viv92 = 0
   LET v_aivs_viv97 = 0

   LET v_pesos_viv92 = 0
   LET v_pesos_viv97 = 0

   DISPLAY "Prueba de comunicacion con PROCESAR, saldo previo"
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 49
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = 30 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = "INFONAVIT"    
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION

   -- R E V I S A R    C O N    M A U R O
   LET ns2parametersConsultarSaldoPreliminarRequest.cuerpo.curp = "LEMR420215HNECNY05"
   LET ns2parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = "01004200265"
  
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
      LET v_diagnostico = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
      LET v_estatus     = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual
      LET v_aivs_viv92  = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92AIVS
      LET v_pesos_viv92 = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92
      LET v_aivs_viv97  = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97AIVS
      LET v_pesos_viv97 = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97
      DISPLAY "Estatos FOVISSSTE  :", ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusViviendaF
      DISPLAY "Estatus INFONAVIT  :", ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusViviendaI
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