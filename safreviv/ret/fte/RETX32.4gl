IMPORT util
IMPORT SECURITY

SCHEMA safre_viv
{
RETX32

Funcionalidad para el llamado al Servicio de consulta de saldo, marca y desmarca con ProceSAR
para Retiro Ley 73

}
GLOBALS "ret_saldo_preliminar.inc" -- saldo prelimminar PROCESAR
GLOBALS "RETG01.4gl"               -- Constantes para los codigos de error

{
======================================================================
Nombre: fn_consulta_saldo_vivienda_afore
Fecha creacion: Noviembre 25, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el saldo de la subcuenta de vivienda en la AFORE

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_saldo_vivienda_afore(p_nss,p_id_cliente)
DEFINE 
      p_nss       LIKE afi_derechohabiente.nss, -- nss consultado
      p_id_cliente SMALLINT,
      v_aivs_viv92  DECIMAL(14,6), -- monto en aivs de vivienda 92
      v_pesos_viv92 DECIMAL(12,2), -- monto en pesos de vivienda 92
      v_aivs_viv97 DECIMAL(14,6), -- monto en aivs de vivienda 97
      v_pesos_viv97 DECIMAL(12,2), -- monto en pesos de vivienda 92
      v_diagnostico SMALLINT,      -- Diagnostico de la consulta en la Afore
      v_estatus     SMALLINT,      -- Estatus de la Cuenta Individual
      v_resultado   SMALLINT,
      v_probabilidad SMALLINT,
       v_cod_rechazo SMALLINT
   
   --Inicializaciones para la simulacion
   LET v_diagnostico = 101
   LET v_aivs_viv92  = 0
   LET v_aivs_viv97  = 0
   LET v_pesos_viv92 = 0
   LET v_pesos_viv97 = 0
   LET v_estatus     = 0
   
   --Simulo una probabilidad al azar de entre 1 y 39
   CALL util.math.rand(40) RETURNING v_probabilidad

   {
   --Posibilidad total de que no pudo conectarse con PROCESAR
   DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
   DISPLAY "CODE       : ", wsError.code
   DISPLAY "CODENS     : ", wsError.codeNS
   DISPLAY "DESCRIPTION: ", wsError.description
   DISPLAY "ACTION     : ", wsError.action	  
   LET v_diagnostico = 127
   --LET v_diagnostico = 0
   LET v_estatus     = 0
   LET v_aivs_viv92  = 0
   LET v_aivs_viv97  = 0
   LET v_pesos_viv92 = 0
   LET v_pesos_viv97 = 0
   }
            
   
   --Posibilidad total para buscar en SACI
   CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
   CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
   IF ( v_resultado = 0 ) THEN
      LET v_diagnostico = 101
      LET v_estatus     = 101
   ELSE
      DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action	  
      LET v_diagnostico = 127
      --LET v_diagnostico = 0
      LET v_estatus     = 0
      LET v_aivs_viv92  = 0
      LET v_aivs_viv97  = 0
      LET v_pesos_viv92 = 0
      LET v_pesos_viv97 = 0
   END IF
   


   {
   --Posibilidad mayor para que no se revise desde PROCESAR
   CASE
      WHEN v_probabilidad = 1 OR v_probabilidad = 3 OR v_probabilidad = 5 OR v_probabilidad = 7
      OR v_probabilidad = 9 OR v_probabilidad = 11
         --Para las probabilidades nones simulare que SI hubo conexión a Procesar
         --pero intentaré obtener saldo, sino encuentro saldo doy como error al invocar WS
         CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
         CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
         IF ( v_resultado = 0 ) THEN
            LET v_diagnostico = 101
            LET v_estatus     = 101
         ELSE
            DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
            DISPLAY "CODE       : ", wsError.code
            DISPLAY "CODENS     : ", wsError.codeNS
            DISPLAY "DESCRIPTION: ", wsError.description
            DISPLAY "ACTION     : ", wsError.action	  
            LET v_diagnostico = 127
            --LET v_diagnostico = 0
            LET v_estatus     = 0
            LET v_aivs_viv92  = 0
            LET v_aivs_viv97  = 0
            LET v_pesos_viv92 = 0
            LET v_pesos_viv97 = 0
         END IF
         EXIT CASE
      WHEN v_probabilidad = 13 OR v_probabilidad = 15
      OR v_probabilidad = 17 OR v_probabilidad = 19 OR v_probabilidad = 21 OR v_probabilidad = 23
      OR v_probabilidad = 25 OR v_probabilidad = 27 OR v_probabilidad = 29 OR v_probabilidad = 31
      OR v_probabilidad = 33 OR v_probabilidad = 35 OR v_probabilidad = 37
      OR (v_probabilidad >= 39 AND v_probabilidad <= 100)
         DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
         DISPLAY "CODE?????? : ", wsError.code
         DISPLAY "CODENS???? : ", wsError.codeNS
         DISPLAY "DESCRIPTION: ", wsError.description
         DISPLAY "ACTION???? : ", wsError.action
         LET v_diagnostico = 127
         LET v_estatus     = 0
         LET v_aivs_viv92  = 0
         LET v_aivs_viv97  = 0
         LET v_pesos_viv92 = 0
         LET v_pesos_viv97 = 0
         -- se devuelve el codigo de error del WS y fecha nula
         --RETURN wsError.code, NULL
      --Para las probabilidades pares asignaré un estado de error.
      WHEN v_probabilidad = 2
         LET v_estatus = 201     EXIT CASE
      WHEN v_probabilidad = 4
         LET v_estatus = 202     EXIT CASE
      WHEN v_probabilidad = 6
         LET v_estatus = 203     EXIT CASE
      WHEN v_probabilidad = 8
         LET v_estatus = 204     EXIT CASE
      WHEN v_probabilidad = 10
         LET v_estatus = 205     EXIT CASE
      WHEN v_probabilidad = 12
         LET v_estatus = 207     EXIT CASE
      WHEN v_probabilidad = 14
         LET v_estatus = 208     EXIT CASE
      WHEN v_probabilidad = 16
         LET v_estatus = 211     EXIT CASE
      WHEN v_probabilidad = 18
         LET v_estatus = 212     EXIT CASE
      WHEN v_probabilidad = 20
         LET v_estatus = 213     EXIT CASE
      WHEN v_probabilidad = 22
         LET v_estatus = 216     EXIT CASE
      WHEN v_probabilidad = 24
         LET v_estatus = 217     EXIT CASE
      WHEN v_probabilidad = 26
         LET v_estatus = 219     EXIT CASE
      WHEN v_probabilidad = 28
         LET v_estatus = 220     EXIT CASE
      WHEN v_probabilidad = 30
         LET v_estatus = 221     EXIT CASE
      WHEN v_probabilidad = 32
         LET v_estatus = 223     EXIT CASE
      WHEN v_probabilidad = 34
         LET v_estatus = 224     EXIT CASE
      WHEN v_probabilidad = 36
         LET v_estatus = 225     EXIT CASE
      WHEN v_probabilidad = 38
         LET v_estatus = 226     EXIT CASE
   END CASE

   CASE v_estatus
       WHEN 102
           LET v_cod_rechazo = gi_101_102 -- 999, -- Codigo 101 102 NSS no se encuentra registrado en la BDNSAR
       WHEN 201
           LET v_cod_rechazo = gi_101_201 -- 22,  -- La cuenta se encuentra en proceso de 43 BIS
       WHEN 202
           LET v_cod_rechazo = gi_101_202 -- 820, -- En proceso de Traspaso A-A
       WHEN 203
           LET v_cod_rechazo = gi_101_203 -- 597, -- En proceso de Unificación de cuentas
       WHEN 204
           LET v_cod_rechazo = gi_101_204 -- 821, -- En proceso de Fusión de Afore
       WHEN 205
           LET v_cod_rechazo = gi_101_205 -- 201, -- En proceso de separación de cuentas
       WHEN 207
           LET v_cod_rechazo = gi_101_207 -- 822, -- En proceso de transferencias de recursos
       WHEN 208
           LET v_cod_rechazo = gi_101_208 -- 823, -- En proceso de disposición de recursos
       WHEN 211
           LET v_cod_rechazo = gi_101_211 -- 824, -- En proceso de Devolución de pagos efectuados sin Justificación Legal
       WHEN 212
           LET v_cod_rechazo = gi_101_212 -- 825, -- En proceso de retiros parciales
       WHEN 213
           LET v_cod_rechazo = gi_101_213 -- 826, -- En proceso de tramite judicial iniciado por Afore
       WHEN 216
           LET v_cod_rechazo = gi_101_216 -- 827, -- Cuenta en proceso de aclaración por conciliación
       WHEN 217
           LET v_cod_rechazo = gi_101_217 -- 828, -- Cuenta en proceso de selección SIEFORE
       WHEN 219
           LET v_cod_rechazo = gi_101_219 -- 829, -- Cuenta en proceso de modificación
       WHEN 220
           LET v_cod_rechazo = gi_101_220 -- 836, -- Cuenta en proceso de crédito de vivienda
       WHEN 221
           LET v_cod_rechazo = gi_101_221 -- 837, -- Cuenta en proceso de crédito de vivienda 43BIS
       WHEN 223
           LET v_cod_rechazo = gi_101_223 -- 830, -- Cuenta en proceso de saldo previo
       WHEN 224
           LET v_cod_rechazo = gi_101_224 -- 832, -- No se encuentra marcado
       WHEN 225
           LET v_cod_rechazo = gi_101_225 -- 833, -- Existe alguna notificación de pago por Ventanilla INFONAVIT
       WHEN 226
           LET v_cod_rechazo = gi_101_226 -- 834, -- Existe alguna notificación de pago por Ventanilla INFONAVIT (ESTE LO ENVIA LA AF0RE)
       OTHERWISE 
           LET v_cod_rechazo = gi_103_0   -- 835, -- No se encuentra marcado (ESTE LO ENVIA LA AF0RE)
    END CASE
   }
    
   -- se devuelve el saldo total en aivs y pesos
   RETURN v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
   
END FUNCTION

{
FUNCTION fn_consulta_saldo_vivienda_afore(p_nss,p_id_cliente)
DEFINE p_nss   LIKE afi_derechohabiente.nss, -- nss consultado
       p_id_cliente SMALLINT,     
       p_curp  LIKE afi_derechohabiente.curp,  -- curp consultado
       v_aivs_viv92  DECIMAL(14,6), -- monto en aivs de vivienda 92
       v_pesos_viv92 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_aivs_viv97  DECIMAL(14,6), -- monto en aivs de vivienda 97
       v_pesos_viv97 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_total_aivs  DECIMAL(14,6), -- monto total en aivs
       v_total_pesos DECIMAL(12,2),  -- monto total en pesos,
       v_diagnostico SMALLINT,       -- Diagnostico de la consulta en la Afore
       v_estatus     SMALLINT,       -- Estatus de la Cuenta Individual
       v_resultado   SMALLINT,
       v_nss_tmp     STRING,
       v_cod_rechazo SMALLINT  


    -- se inicializan las variables de retorno del saldo
    LET v_aivs_viv92 = 0
    LET v_aivs_viv97 = 0

    LET v_pesos_viv92 = 0
    LET v_pesos_viv97 = 0
    LET v_diagnostico = 0
    LET v_estatus     = 0


    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 49
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = p_id_cliente   -- 30 - Consulta, 44 - marca y consulta, 60 - desmarca 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = "INFONAVIT"    
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION
    
    -- se inicializan las variables del WS con los valores a consultar   
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.curp = p_curp
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = p_nss
  
    -- se invoca el servicio
    -- DISPLAY "Ejecutando WSSaldoPreliminar..."
    CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado       

    -- si el webservice NO se ejecuto correctamente
    IF ( v_resultado = 0 ) THEN      

        LET v_diagnostico = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
        LET v_estatus     = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual

        LET v_aivs_viv92  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92AIVS
        LET v_pesos_viv92 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92
        LET v_aivs_viv97  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97AIVS
        LET v_pesos_viv97 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97
    ELSE 
        DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
        DISPLAY "CODE       : ", wsError.code
        DISPLAY "CODENS     : ", wsError.codeNS
        DISPLAY "DESCRIPTION: ", wsError.description
        DISPLAY "ACTION     : ", wsError.action	  
        LET v_diagnostico = 127
        -- se devuelve el codigo de error del WS y fecha nula
        --RETURN wsError.code, NULL
    END IF
}
    -- se devuelve el saldo total en aivs y pesos
    {
    RETURN v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97
END FUNCTION
}

