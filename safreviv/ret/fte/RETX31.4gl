IMPORT util
IMPORT SECURITY

SCHEMA safre_viv
{
RETX33  

funciones utilizadas para interaccion con CRM

-- Busca Caso CRM
-- Cancela Caso CRM
-- Crea Caso CRM
-- Confirma pago CRM

}
GLOBALS "ret_ws_cons_rel_laboral.inc" -- consulta de pago a FICO
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
    -----------
    -----------
    ---- Este codigo es un simulador del llamado a procesar 
    -----------
    -----------
    LET v_nss_tmp = p_nss
    IF v_nss_tmp.subString(1,4) = "0123" OR 
       v_nss_tmp.subString(1,4) = "1234" OR 
       v_nss_tmp.subString(1,4) = "2345" OR 
       v_nss_tmp.subString(1,4) = "3456" OR 
       v_nss_tmp.subString(1,4) = "4567" OR 
       v_nss_tmp.subString(1,4) = "5678" OR 
       v_nss_tmp.subString(1,4) = "6789" OR 
       v_nss_tmp.subString(1,4) = "7890" OR 
       v_nss_tmp.subString(1,4) = "8901" OR 
       v_nss_tmp.subString(1,4) = "9012" OR 
       v_nss_tmp.subString(1,4) = "1122" OR 
       v_nss_tmp.subString(1,4) = "2233" OR 
       v_nss_tmp.subString(1,4) = "3344" OR 
       v_nss_tmp.subString(1,4) = "4455" OR 
       v_nss_tmp.subString(1,4) = "5566" OR 
       v_nss_tmp.subString(1,4) = "6677" OR 
       v_nss_tmp.subString(1,4) = "7788" OR 
       v_nss_tmp.subString(1,4) = "8899" OR 
       v_nss_tmp.subString(1,4) = "9900" THEN 
        LET v_diagnostico = 101
        LET v_aivs_viv92  = 0
        LET v_aivs_viv97  = 0
        LET v_pesos_viv92 = 0
        LET v_pesos_viv97 = 0
        CASE v_nss_tmp.subString(1,4)
            WHEN "0123"
                LET v_estatus     = 201
            WHEN "1234"
                LET v_estatus     = 202
            WHEN "2345"
                LET v_estatus     = 203
            WHEN "3456"
                LET v_estatus     = 204
            WHEN "4567"
                LET v_estatus     = 205
            WHEN "5678"
                LET v_estatus     = 207
            WHEN "6789"
                LET v_estatus     = 208
            WHEN "7890"
                LET v_estatus     = 211
            WHEN "8901"
                LET v_estatus     = 212
            WHEN "9012"
                LET v_estatus     = 213
            WHEN "1122"
                LET v_estatus     = 216
            WHEN "2233"
                LET v_estatus     = 217
            WHEN "3344"
                LET v_estatus     = 219
            WHEN "4455"
                LET v_estatus     = 220
            WHEN "5566"
                LET v_estatus     = 221
            WHEN "6677"
                LET v_estatus     = 223
            WHEN "7788"
                LET v_estatus     = 224
            WHEN "8899"
                LET v_estatus     = 225
            WHEN "9900"
                LET v_estatus     = 226
        END CASE 
    ELSE 
       
        
        CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97;
        CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92;

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
            LET v_diagnostico = 0
            LET v_estatus     = 0
            LET v_aivs_viv92  = 0
            LET v_aivs_viv97  = 0
            LET v_pesos_viv92 = 0
            LET v_pesos_viv97 = 0
            -- se devuelve el codigo de error del WS y fecha nula
            --RETURN wsError.code, NULL
        END IF
    END IF 
    
    -----------
    -----------
    ---- Aqui termina el código del simulador se debera comentar cuando se necesite liberar con procesar
    -----------
    -----------    

{    

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

{
======================================================================
Nombre: fn_fecha_ultima_relacion_laboral
Fecha creacion: diciembre 11, 2013
Autor: Eneas Armas, EFP
Narrativa del proceso que realiza:
Funcion que invoca la consulta de la ultima relacion laboral mediante
Web service

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_fecha_ultima_relacion_laboral(p_nss)
DEFINE p_nss           LIKE afi_derechohabiente.nss -- nss consultado
       ,v_ws_status    SMALLINT -- estatus de ejecucion de un webservice
       ,v_indice       INTEGER
       ,v_fecha_fin    DATE
       ,v_fecha_f      DATE
       ,v_fecha_c      STRING
       ,v_con_sin_rl   SMALLINT 

   LET ns1consultarTrabajador.nss = p_nss

   -- se invoca la consulta de pago a FICO
   CALL consultarTrabajador_g() RETURNING v_ws_status

   -- si el webservice NO se ejecuto correctamente
   IF ( v_ws_status <> 0 ) THEN      

      DISPLAY "ERROR al invocar webservice de consulta de pago"
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESRIPTION : ", wsError.description
      DISPLAY "ACTION     : ", wsError.action
	  
	  -- se devuelve el codigo de error del WS y fecha nula
      RETURN wsError.code, NULL
   END IF

   {
Cuando la fecha de baja de relación laboral sea: 9999-12-31, quiere decir que la 
relación laboral aún se encuentra vigente.

En los casos que el servicio conteste el campo NRP, como blanco o ceros, y los campos 
de fecha de alta y fecha de baja tengan el formato 1900-01-01, significa que para ese 
NSS no existe relación laboral.
   }
   
   --fecha de referencia inicial para la comparación
   LET v_fecha_fin = DATE ( "01/01/1899" )
   LET v_con_sin_rl = 0   -- Inicializa el indicador 

   FOR v_indice = 1 TO ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab.getLength()
      --se formatea la fecha
	  CALL ERRORLOG("Fecha encontrada en WS de relacion laboral")
      LET v_fecha_c = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja
	  CALL ERRORLOG(ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja)
      LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
      LET v_fecha_f = DATE(v_fecha_c)
      
	  -- se selecciona la fecha mas alta
      IF ( v_fecha_f > v_fecha_fin ) THEN
         LET v_fecha_fin = v_fecha_f
      END IF
   END FOR

   -- si la fecha mayor es 9999-12-31 entonces aun hay relacion laboral
   IF ( v_fecha_fin = "12/31/9999" ) THEN
      -- no se devuelve fecha
      LET v_fecha_fin = NULL
      LET v_con_sin_rl = 1  -- Con Relacion Laboral
   END IF
   
   -- si la fecha es 1900-01-01, entonces no hay relacion laboral
   IF ( v_fecha_fin = "01/01/1900" ) THEN
      LET v_fecha_fin = NULL
      LET v_con_sin_rl = 2   -- Sin Relacion Laboral
   END IF
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_ws_status, v_fecha_fin, v_con_sin_rl
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_detalle_resolucion_spess
Fecha creacion: Agosto 04, 2014
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Detalle el error por el que no se encontro la resolucion en el spess

======================================================================
}
FUNCTION fn_detalle_resolucion_spess(p_nss, p_causal)
DEFINE  v_tiene_resolucion     SMALLINT, -- booleana que indica si el trabajador tiene resolucion en spess
        p_nss                  LIKE afi_derechohabiente.nss, -- nss del trabajador
        p_causal               SMALLINT, -- causal de retiro
        v_sec_pension          LIKE ret_datamart.sec_pension, -- max secuencia de pension
        v_diag_registro        LIKE ret_datamart.diag_registro, -- diagnostico del registro
        v_id_datamart          LIKE ret_datamart.id_datamart, -- clave de la tabla del datamart
		v_tipo_pension         LIKE ret_datamart.tpo_pension, -- tipo de pension
		v_porcentaje_valuacion LIKE ret_datamart.porcentaje_valuacion, -- porcentaje de valuacion
        v_sql                  STRING,
        v_error_det            SMALLINT,
        v_tipo_prestacion      LIKE ret_datamart.tpo_prestacion 
   
   -- se asume que el trabajador no tiene resolucion
   LET v_tiene_resolucion = FALSE
   LET v_error_det        = 91 -- Sin resolucion en el SPESS
   
   DISPLAY "Validando causal: ", p_causal
   
   CASE p_causal

      -- termino de relacion laboral
      WHEN 1
         --LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion ",
                     --"\nFROM  ret_datamart a,                     ",
                     --"\n      ret_matriz_derecho b                ",
                     --"\nWHERE a.nss            = ?                ", -- el trabajador
                     --"\nAND   a.regimen        = b.regimen        ", -- condiciones del retiro validas
                     --"\nAND   a.tpo_seguro     = b.tpo_seguro     ", 
                     --"\nAND   a.tpo_pension    = b.tpo_pension    ", 
                     --"\nAND   a.tpo_prestacion = b.tpo_prestacion ",
                     --"\nGROUP BY a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion    ", 
                     --"\nORDER BY a.id_datamart DESC               " 
         
         LET v_sql = "\nSELECT MAX(a.sec_pension)                 ",
                     "\nFROM  ret_datamart a,                     ",
                     "\n      ret_matriz_derecho b                ",
                     "\nWHERE a.nss            = ?                "
         
         PREPARE detsid_rellab FROM v_sql
         EXECUTE detsid_rellab USING p_nss INTO v_sec_pension
         IF v_sec_pension IS NOT NULL THEN  
             LET v_sql = "\n SELECT FIRST 1 a.id_datamart, a.tpo_pension , a.porcentaje_valuacion, a.tpo_prestacion ",
                         "\n FROM   ret_datamart a ",
                         "\n WHERE  a.nss = ", p_nss,
                         "\n AND    a.sec_pension = ", v_sec_pension
             PREPARE detsid_datamartcausal FROM v_sql
             EXECUTE detsid_datamartcausal INTO v_id_datamart, v_tipo_pension, v_porcentaje_valuacion, v_tipo_prestacion 
         END IF 

         LET v_error_det = 91 -- Sin resolucion en el SPESS
         
         -- si se encontro un registro
         IF ( v_id_datamart IS NOT NULL ) THEN
             IF (v_tipo_prestacion = "03") THEN 
                LET v_tiene_resolucion = FALSE 
                LET v_error_det = 92 -- Resolucion por Negativa de Pension
             ELSE 
                 -- si es tipo de pension IP, debe tener porcentaje valuacion
                IF ( v_tipo_pension = "IP" ) THEN
                   IF ( v_porcentaje_valuacion >= 50 ) THEN
                      -- resolucion valida
                      LET v_tiene_resolucion = TRUE
                      LET v_error_det = 0
                   ELSE
                      -- no tiene resolucion valida
                      LET v_tiene_resolucion = FALSE
                      LET v_error_det = 140 -- El porcentaje de valuacion es menor al 50 %
                   END IF
                ELSE
                   -- es resolucion valida
                   LET v_tiene_resolucion = TRUE
                   LET v_error_det = 0
                END IF
            END IF 
         END IF
     
      -- Resolucion de pension otorgada por el IMSS
      WHEN 2 -- se elimina el cruce con la matriz de derechos 20150702
         --LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion ",
                     --"\nFROM  ret_datamart a,                     ",
                     --"\n      ret_matriz_derecho b                ",
                     --"\nWHERE a.nss            = ?                ", -- el trabajador
                     --"\nAND   a.regimen        = b.regimen        ", -- condiciones del retiro validas
                     --"\nAND   a.tpo_seguro     = b.tpo_seguro     ", 
                     --"\nAND   a.tpo_pension    = b.tpo_pension    ", 
                     --"\nAND   a.tpo_prestacion = b.tpo_prestacion ",
                     --"\nGROUP BY a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion    ", 
                     --"\nORDER BY a.id_datamart DESC                       " 
         LET v_sql = "\nSELECT MAX(a.sec_pension)                 ",
                     "\nFROM  ret_datamart a,                     ",
                     "\n      ret_matriz_derecho b                ",
                     "\nWHERE a.nss            = ?                " -- el trabajador
         
         PREPARE detsid_pensionimss FROM v_sql
         EXECUTE detsid_pensionimss USING p_nss INTO v_sec_pension
         IF v_sec_pension IS NOT NULL THEN  
             LET v_sql = "\n SELECT FIRST 1 a.id_datamart, a.tpo_pension , a.porcentaje_valuacion, a.tpo_prestacion ",
                         "\n FROM   ret_datamart a ",
                         "\n WHERE  a.nss = ", p_nss,
                         "\n AND    a.sec_pension = ", v_sec_pension
             PREPARE detsid_datamart FROM v_sql
             EXECUTE detsid_datamart INTO v_id_datamart, v_tipo_pension, v_porcentaje_valuacion, v_tipo_prestacion 
         END IF 
         LET v_error_det = 91   -- no existe resolucion en el spess

         -- si se encontro un registro
         IF ( v_id_datamart IS NOT NULL ) THEN
		    -- si es tipo de pension IP, debe tener porcentaje valuacion
            IF (v_tipo_prestacion = "03") THEN
                LET v_error_det = 92 -- resolucion por negativa de pension
                LET v_tiene_resolucion = FALSE 
            ELSE 
                IF ( v_tipo_pension = "IP" ) THEN
                   IF ( v_porcentaje_valuacion >= 50 ) THEN
                      -- resolucion valida
                      LET v_tiene_resolucion = TRUE
                      LET v_error_det = 0
                   ELSE
                      LET v_error_det = 140 -- porcentaje de valuacion menor al 50 %
                      -- no tiene resolucion valida
                      LET v_tiene_resolucion = FALSE
                   END IF
                ELSE
                   -- es resolucion valida
                   LET v_tiene_resolucion = TRUE
                   LET v_error_det = 0
                END IF
            END IF 
         END IF
              
      -- plan privado de pension
      WHEN 3
	     -- 24feb2014. No requiere ser validado, se asume que se tiene
		 LET v_tiene_resolucion = TRUE
         LET v_error_det = 0
     
      WHEN 4
         SELECT MAX(a.sec_pension), a.id_datamart
         INTO    v_sec_pension, v_id_datamart
         FROM    ret_datamart a
                ,ret_matriz_derecho b
         WHERE  a.nss = p_nss
         AND    a.regimen        = b.regimen
         AND    a.tpo_seguro     = b.tpo_seguro
         AND    a.tpo_pension    IN ('OR','VI','VO','AS')
         AND    a.tpo_prestacion = b.tpo_prestacion
         GROUP BY a.id_datamart
     
         -- si tiene una secuencia de pension, tiene resolucion de pension que corresponde con una defuncion
         IF ( v_sec_pension IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
            LET v_error_det = 0
         END IF
     
      -- ====================================================================================
      -- BUSQUEDA DE RESOLUCION EN EL SPESS PARA LEY 73
      WHEN 5
         -- Resolucion del spess para ley 73
         LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.diag_registro, a.id_datamart",
                     "\nFROM  ret_datamart a                            ",
                     "\n      ,ret_matriz_derecho b                     ",
                     "\nWHERE a.nss            = ?                      ", -- el trabajador
                     "\nAND   a.regimen        = 73                     ", -- el regimen es forzoso que sea Ley73
                     "\nAND   b.tpo_retiro     = 'E'                    ", -- retiro avalado por el imss
                     "\nAND   a.regimen        = b.regimen              ",                      
                     "\nAND   a.tpo_seguro     = b.tpo_seguro           ", 
                     "\nAND   a.tpo_pension    = b.tpo_pension          ", 
                     "\nAND   a.tpo_prestacion = b.tpo_prestacion       ",
                     "\nGROUP BY a.diag_registro, a.id_datamart         ", 
                     "\nORDER BY a.id_datamart DESC                     " 
         
         PREPARE detsid_ley73 FROM v_sql
         EXECUTE detsid_ley73 USING p_nss INTO v_sec_pension, v_diag_registro, v_id_datamart
         
         -- si se encontro un datamart
         IF ( v_id_datamart IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
            LET v_error_det = 0
         END IF

      
   END CASE
   
   -- se devuelve el resultado de la consulta
   RETURN v_tiene_resolucion, v_id_datamart, v_error_det
END FUNCTION 
{
======================================================================
Clave: 
Nombre: fn_guarda_consulta_ws_vent_afore
Fecha creacion: Diciembre 28, 2015
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Guarda la consulta realizada a Procesar

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_guarda_consulta_ws_vent_afore(p_nss, p_indicador_marca, p_estado_indicador, p_f_intento,
                                          p_h_intento, p_diagnostico, p_estado_cuenta, p_saldo_viv92, 
                                          p_saldo_viv97, p_usuario, p_id_solicitud, p_caso_crm, p_origen)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_indicador_marca  SMALLINT,                -- Indica el tipo de accion 1 - marca, 2 - desmarca y 3 - consulta
         p_estado_indicador SMALLINT,                -- indica el evento en el que se encuentra el indicador de la marca
         p_f_intento        DATE,                    -- Fecha en la que se realiza el intento
         p_h_intento        DATETIME HOUR TO SECOND, -- Hora en la que se realiza el intento
         p_diagnostico      SMALLINT,                -- Diagnostico de la cunsulta, marca y/o desmarca
         p_estado_cuenta    SMALLINT,                -- Estado resultado de la consulta, marca y/o desmarca
         p_saldo_viv92      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 92
         p_saldo_viv97      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 97
         p_usuario          CHAR(20),                -- Usuario que realiza la consulta
         p_id_solicitud     DECIMAL(9,0),            -- Numero de solicitud asociado
         p_caso_crm         CHAR(10),                -- Numero de caso CRM asignado
         p_origen           SMALLINT,                -- Origen de la ejecución
         v_cuenta           INTEGER

    --Reviso dependiendo el indicador marca
    CASE p_indicador_marca
       WHEN 1
          --Reviso en caso de ser Marcar
          LET v_cuenta = 0
          --
          SELECT COUNT(*)
          INTO v_cuenta
          FROM ret_ctr_marca_procesar_maestra
          WHERE nss = p_nss
          AND id_solicitud = p_id_solicitud
          AND estado_indicador IN (1,2)
          --
          IF v_cuenta = 1 THEN
             --Actualizo registro maestro
             UPDATE ret_ctr_marca_procesar_maestra
             SET estado_indicador = p_estado_indicador, origen = p_origen
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador IN (1,2)
          ELSE
             --Inserto registro maestro
             INSERT INTO ret_ctr_marca_procesar_maestra VALUES (p_id_solicitud, p_nss, p_estado_indicador, p_origen)
          END IF
       WHEN 2
          IF p_estado_indicador = 6 THEN
             --Actualizo registro maestro de No marcado a Intentado desmarcar sin ser marcada.
             UPDATE ret_ctr_marca_procesar_maestra
             SET estado_indicador = p_estado_indicador, origen = p_origen
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador = 1
          ELSE
             --Reviso en caso de ser Desmarcar
             LET v_cuenta = 0
             --
             SELECT COUNT(*)
             INTO v_cuenta
             FROM ret_ctr_marca_procesar_maestra
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador IN (4,5)
             --
             IF v_cuenta = 1 THEN
                --Actualizo registro maestro
                UPDATE ret_ctr_marca_procesar_maestra
                SET estado_indicador = p_estado_indicador, origen = p_origen
                WHERE nss = p_nss
                AND id_solicitud = p_id_solicitud
                AND estado_indicador IN (4,5)
             ELSE
                --Inserto registro maestro
                INSERT INTO ret_ctr_marca_procesar_maestra VALUES (p_id_solicitud, p_nss, p_estado_indicador, p_origen)
             END IF
          END IF
       WHEN 3
          --Reviso en caso de ser Consultar
          LET v_cuenta = 0
          --
          IF p_id_solicitud IS NULL THEN
             SELECT COUNT(*)
             INTO v_cuenta
             FROM ret_ctr_marca_procesar_maestra
             WHERE nss = p_nss
             AND id_solicitud IS NULL
             AND estado_indicador IN (3,7)
          ELSE
             SELECT COUNT(*)
             INTO v_cuenta
             FROM ret_ctr_marca_procesar_maestra
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador IN (3,7)
          END IF
          --
          IF v_cuenta = 1 THEN
             IF p_id_solicitud IS NULL THEN
                --Actualizo registro maestro
                UPDATE ret_ctr_marca_procesar_maestra
                SET estado_indicador = p_estado_indicador, origen = p_origen
                WHERE nss = p_nss
                AND id_solicitud IS NULL
                AND estado_indicador IN (3,7)
             ELSE
                --Actualizo registro maestro
                UPDATE ret_ctr_marca_procesar_maestra
                SET estado_indicador = p_estado_indicador, origen = p_origen
                WHERE nss = p_nss
                AND id_solicitud = p_id_solicitud
                AND estado_indicador IN (3,7)
             END IF
          ELSE
             --Inserto registro maestro
             INSERT INTO ret_ctr_marca_procesar_maestra VALUES (p_id_solicitud, p_nss, p_estado_indicador, p_origen)
          END IF
    END CASE

    --Inserto en la tabla histórica ret_ctr_marca_procesar
    INSERT 
    INTO   ret_ctr_marca_procesar (nss, indicador_marca, estado_indicador, f_intento, 
                                    h_intento, diagnostico, estado_cuenta, saldo_viv92,
                                    saldo_viv97, usuario, id_solicitud, caso_crm, origen)
                           VALUES (p_nss, p_indicador_marca, p_estado_indicador, p_f_intento,
                                   p_h_intento, p_diagnostico, p_estado_cuenta, p_saldo_viv92, 
                                   p_saldo_viv97, p_usuario, p_id_solicitud, p_caso_crm, p_origen);

                                   
         

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_recupera_saldo_consulta
Fecha creacion: Diciembre 29, 2015
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Consulta el último saldo devuelto por Procesar

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recupera_saldo_consulta(p_nss)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         v_aivs_viv92       DECIMAL(24,6),
         v_aivs_viv97       DECIMAL(24,6),
         v_pesos_viv92      DECIMAL(22,2),
         v_pesos_viv97      DECIMAL(22,2),
         p_indicador_marca  SMALLINT,                -- Indica el tipo de accion 1 - marca, 2 - desmarca y 3 - consulta
         p_estado_indicador SMALLINT,                -- indica el evento en el que se encuentra el indicador de la marca
         v_f_intento        DATE,                    -- Fecha en la que se realiza el intento
         v_h_intento        DATETIME HOUR TO SECOND, -- Hora en la que se realiza el intento
         p_diagnostico      SMALLINT,                -- Diagnostico de la cunsulta, marca y/o desmarca
         p_estado_cuenta    SMALLINT,                -- Estado resultado de la consulta, marca y/o desmarca
         p_saldo_viv92      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 92
         p_saldo_viv97      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 97
         p_usuario          CHAR(20),                -- Usuario que realiza la consulta
         p_id_solicitud     DECIMAL(9,0),            -- Numero de solicitud asociado
         p_caso_crm         DECIMAL(9,0)             -- Numero de caso CRM asignado

    DISPLAY "Procesa consulta ";


    LET v_aivs_viv92  = 0
    LET v_pesos_viv92 = 0
    LET v_aivs_viv97  = 0
    LET v_pesos_viv97 = 0
    
    SELECT MAX(f_intento)
    INTO   v_f_intento
    FROM   ret_ctr_marca_procesar
    WHERE  nss           = p_nss
    AND    diagnostico   = 101
    AND    estado_cuenta = 101

    IF v_f_intento IS NOT NULL THEN 
        SELECT MAX(h_intento)
        INTO   v_h_intento
        FROM   ret_ctr_marca_procesar
        WHERE  nss           = p_nss
        AND    diagnostico   = 101
        AND    estado_cuenta = 101
        AND    f_intento     = v_f_intento
        IF v_h_intento IS NOT NULL THEN 

            SELECT saldo_viv92, saldo_viv92 * b.precio_fondo, saldo_viv97, saldo_viv97 * b.precio_fondo
            INTO   v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97
            FROM   ret_ctr_marca_procesar a,
                  (SELECT NVL(precio_fondo,0)
                   FROM   glo_valor_fondo
                   WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                                         FROM   (SELECT LIMIT 1 1 
                                                 FROM   systables))
                   AND    fondo = 11) b
            WHERE  indicador_marca = 3
            AND    nss             = p_nss
            AND    f_intento       = v_f_intento
            AND    h_intento       = v_h_intento
            AND    diagnostico     = 101
            AND    estado_cuenta   = 101
        END IF 
    END IF 
    RETURN v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97
         
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_hash
Fecha creacion: Marzo 03, 2016
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
Genera un código HASH (pasado por parámetro) de una cadena de texto STRING
(pasada por parámetro),

códigos HASH permitidos:
  - SHA1 (Recomendado)
  - SHA512
  - SHA384
  - SHA256
  - SHA224
  - MD5

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_hash(toDigest, algo)

   DEFINE toDigest, algo, result STRING
   DEFINE dgst security.Digest

   IF algo IS NULL OR algo = "" THEN
      LET algo = "SHA1" --(Default)
   END IF

   TRY
      LET dgst = security.Digest.CreateDigest(algo)
      CALL dgst.AddStringData(toDigest)
      --LET result = dgst.DoBase64Digest()
      LET result = dgst.DoHexBinaryDigest()
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      EXIT PROGRAM(-1)
   END TRY

   RETURN result
   
END FUNCTION
