#Modulo        => RET                                                          #
#Programa      => RETL407                                                      #
#Objetivo      => Lanzado de notificacion de solicitudes de Retiro Ley 73,     #
#                 Grupo 1.                                                     #
#Fecha inicio  => Diciembre, 2015.                                             #
#Requerimiento =>                                                              #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
IMPORT util 
GLOBALS "NotiClabe.inc"
GLOBALS "RETG01.4gl"

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_nss                      LIKE ret_solicitud_generico.nss

MAIN

    DEFINE v_estado             SMALLINT
    DEFINE r_resultado_opera    INTEGER

    CALL ARG_VAL(1) RETURNING p_nss


    --Encabezado para el archivo de monitoreo
    DISPLAY "*******************************************************************"
    DISPLAY " Notificación a ProceSAR Cuenta CLABE POR NSS : "
    DISPLAY " El NSS a enviar es    : ",p_nss
    DISPLAY " FECHA                 : ",TODAY USING 'dd-mm-yyyy'
    DISPLAY " HORA                  : ",TIME(CURRENT)
    DISPLAY "*******************************************************************"
    DISPLAY ""
    DISPLAY ""
    DISPLAY ""

    CALL fn_notificar_procesar() RETURNING v_estado


    --Encabezado para el archivo de monitoreo
    DISPLAY ""
    DISPLAY ""
    DISPLAY ""
    DISPLAY "Terminó la Notificación a ProceSAR Cuenta CLABE POR NSS "
    DISPLAY "*******************************************************************"
    DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
    DISPLAY " HORA               : ",TIME(CURRENT)
    DISPLAY "*******************************************************************"

END MAIN

FUNCTION fn_notificar_procesar()

    -- Variable regresada
    DEFINE r_estado                   SMALLINT

    -- Variables de control
    DEFINE v_solicitudes_procesadas     INTEGER
    DEFINE v_solicitudes_informadas     INTEGER
    DEFINE v_solicitudes_no_informadas  INTEGER
    DEFINE v_resultado                  INTEGER
    DEFINE v_diagnostico                CHAR(03)

    -- Variables auxiliares
    DEFINE v_sql                      STRING
    DEFINE subcuenta_8                LIKE cat_subcuenta.subcuenta
    DEFINE subcuenta_4                LIKE cat_subcuenta.subcuenta
    DEFINE v_estado_solicitud_sig     LIKE ret_solicitud_generico.estado_solicitud
    DEFINE v_respuesta                SMALLINT
    DEFINE v_string                   base.StringBuffer

    -- Datos para la consulta
    DEFINE v_solicitud_notificacion RECORD
        id_solicitud                LIKE ret_solicitud_generico.id_solicitud,
        nss                         LIKE ret_solicitud_generico.nss,
        rfcTrabajador               LIKE ret_solicitud_generico.rfc,
        curpTrabajador              LIKE afi_derechohabiente.curp,
        grupoTrabajador             CHAR(4),
        estado_solicitud            LIKE ret_solicitud_generico.estado_solicitud
    END RECORD

    DEFINE v_clabe  LIKE ret_pago_spei.cuenta_clabe
    DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
    
    -- Consulta pago fico
    DEFINE v_h_consulta         LIKE ret_ws_consulta_pago_fico.h_consulta
    DEFINE v_rsp_referencia     LIKE ret_ws_consulta_pago_fico.rsp_referencia
    DEFINE v_rsp_f_pago         LIKE ret_ws_consulta_pago_fico.rsp_f_pago
    DEFINE v_rsp_estatus        LIKE ret_ws_consulta_pago_fico.rsp_estatus
    DEFINE v_f_pago             CHAR(25)  --DATETIME YEAR TO fraction(5)

    -- Beneficiario generico
    DEFINE v_nombreBeneficiario          LIKE ret_beneficiario_generico.nombre
    DEFINE v_apellidoPaternoBeneficiario LIKE ret_beneficiario_generico.ap_paterno
    DEFINE v_apellidoMaternoBeneficiario LIKE ret_beneficiario_generico.ap_materno
    DEFINE v_id_entidad_federativa       LIKE ret_beneficiario_generico.id_entidad_federativa

    -- Datos de ret_datamart y ret_matriz_derecho
    DEFINE v_id_datamart                 LIKE ret_datamart.id_datamart
    DEFINE v_secuenciaPension            LIKE ret_datamart.sec_pension
    DEFINE v_regimen                     LIKE ret_datamart.regimen
    DEFINE v_tipoRetiro                  LIKE ret_matriz_derecho.tpo_retiro
    DEFINE v_tipoSeguro                  LIKE ret_datamart.tpo_seguro
    DEFINE v_tipoPension                 LIKE ret_datamart.tpo_pension
    DEFINE v_tipoPrestacion              LIKE ret_datamart.tpo_prestacion
    DEFINE v_semanasCotizadas            LIKE ret_datamart.semanas_cotizadas

    -- Datos de cta_movimiento
    DEFINE v_vivienda92_aivs            DECIMAL(16,6)
    DEFINE v_vivienda97_aivs            DECIMAL(16,6)
    DEFINE v_vivienda92_pesos           DECIMAL(16,2)
    DEFINE v_vivienda97_pesos           DECIMAL(16,2)
    DEFINE v_f_valor                    LIKE cta_movimiento.f_valor

    DEFINE v_rec_notifica               RECORD LIKE ret_ws_notifica_procesar.*

    DEFINE v_marca                      SMALLINT 

    DEFINE v_consec_beneficiario        SMALLINT
    DEFINE v_porcentaje                 SMALLINT 
    DEFINE v_solicitud_consulta_pago    DECIMAL(9,0)
       
    -- Inicializar variables
    LET r_estado                    = 0
    LET v_solicitudes_procesadas    = 0
    LET v_solicitudes_informadas    = 0
    LET v_solicitudes_no_informadas = 0
    LET subcuenta_8                 = 8
    LET subcuenta_4                 = 4 
    LET v_marca                     = 815  --- Variable de trabajo para desmarcar los casos de Ventanilla Afore

    -- Datos fijos para realizar la consulta
    LET ns1notificacionCuentaClabeRequest.idssn.idSistema      = 12 
    LET ns1notificacionCuentaClabeRequest.idssn.idEbusiness    = 13 
    LET ns1notificacionCuentaClabeRequest.idssn.idPortafolio   = 16 
    LET ns1notificacionCuentaClabeRequest.idssn.idServicio     = 125 
    LET ns1notificacionCuentaClabeRequest.idssn.idCliente      = 42 
    LET ns1notificacionCuentaClabeRequest.idssn.idCanal        = 13
    LET ns1notificacionCuentaClabeRequest.idssn.codoperCliente = "INFONAVIT"
    LET ns1notificacionCuentaClabeRequest.idssn.fecha          = YEAR(TODAY) USING "&&&&",'-',MONTH(TODAY) USING "&&",'-',DAY(TODAY) USING "&&",'T',CURRENT HOUR TO SECOND,'.000Z'
--    LET ns1notificacionCuentaClabeRequest.idssn.fecha          = CURRENT YEAR TO FRACTION

    DISPLAY "   " 
    DISPLAY "   "
    DISPLAY "***********************************************************************" 
    DISPLAY "   " 
    DISPLAY "   "
    DISPLAY "LOS DATOS DEL ENCABEZADO SON :"
    DISPLAY "idSistema      >", ns1notificacionCuentaClabeRequest.idssn.idSistema      ,"<"
    DISPLAY "idEbusiness    >", ns1notificacionCuentaClabeRequest.idssn.idEbusiness    ,"<" 
    DISPLAY "idPortafolio   >", ns1notificacionCuentaClabeRequest.idssn.idPortafolio   ,"<" 
    DISPLAY "idServicio     >", ns1notificacionCuentaClabeRequest.idssn.idServicio     ,"<"
    DISPLAY "idCliente      >", ns1notificacionCuentaClabeRequest.idssn.idCliente      ,"<"
    DISPLAY "idCanal        >", ns1notificacionCuentaClabeRequest.idssn.idCanal        ,"<"
    DISPLAY "codoperCliente >", ns1notificacionCuentaClabeRequest.idssn.codoperCliente ,"<"
    DISPLAY "fecha          >", ns1notificacionCuentaClabeRequest.idssn.fecha          ,"<"
    DISPLAY "   " 
    DISPLAY "   "
    DISPLAY "***********************************************************************" 

    
    -- Se declara el cursor sobre las solicitudes tanto Ventanilla Infonavit como Ventanilla Afore.
    LET v_sql = "SELECT rsg.id_solicitud,ad.nss,NVL(ad.rfc,' '),NVL(ad.curp,' '),  "||
                "       '0101',rsg.estado_solicitud, ad.id_derechohabiente      "||
                "FROM   ret_solicitud_generico    rsg,                 "||
                "       ret_ley73_generico        rlg,                 "||
                "       ret_beneficiario_generico rbg,                 "||
                "       afi_derechohabiente       ad                   "||
                -- Join entre tablas
                -- ret_solicitud_generico <-> ret_ley73_generico
                "WHERE  rsg.id_solicitud       = rlg.id_solicitud      "||
                -- ret_solicitud_generico <-> afi_derechohabiente
                "  AND  rsg.id_derechohabiente = ad.id_derechohabiente "||
                "  AND  rsg.id_solicitud       = rbg.id_solicitud      "||
                "  AND  rbg.tpo_beneficiario   = 1                     "||
                -- Solo se notifica grupo 1
                "  AND  rlg.gpo_ley73          = 1                     "||
                -- Pasar de estado 72 a 73 y de 214 a 215, Se quitan los casos 214 Ventanilla Infonavit segun req SACI2018-188
                "  AND  rsg.estado_solicitud   IN (72)                 "||
                "  AND  rsg.nss = " || p_nss                            || 
                "UNION ALL                                             "||
                "SELECT rsg.id_solicitud,ad.nss,NVL(rsad.rfc_s,' '),NVL(rsad.curp_s,' '),        "||
                "       '0201',rsg.estado_solicitud, ad.id_derechohabiente      "||
                "FROM   ret_solicitud_generico    rsg,                 "||
                "       ret_ley73_generico        rlg,                 "||
                "       ret_ws_actualiza_datos_v_a rsad,               "||
                "       afi_derechohabiente       ad                   "||
                "WHERE  rsg.id_solicitud       = rlg.id_solicitud      "||
                "  AND  rsg.id_solicitud       = rsad.id_solicitud     "||
                "  AND  rsg.id_derechohabiente = ad.id_derechohabiente "||
                "  AND  rlg.gpo_ley73          = 1                     "||
                "  AND  rsg.estado_solicitud   IN (71,210)             "||
                "  AND  rsg.nss = " || p_nss 

    DISPLAY "La búsqueda ", v_sql
    PREPARE prp_notificacion_procesar FROM v_sql
    DECLARE cur_notificacion_procesar CURSOR FOR prp_notificacion_procesar

    LET v_sql = "SELECT DISTINCT rsg.id_solicitud, ad.nss,             "||
                "       NVL(ad.rfc,' '), NVL(ad.curp,' '), '0101',     "||
                "       rsg.estado_solicitud, ad.id_derechohabiente    "||
                "FROM   ret_solicitud_generico    rsg,                 "||
                "       ret_ley73_generico        rlg,                 "||
                "       ret_beneficiario_generico rbg,                 "||
                "       afi_derechohabiente       ad                   "||
                -- Join entre tablas
                -- ret_solicitud_generico <-> ret_ley73_generico
                "WHERE  rsg.id_solicitud       = rlg.id_solicitud      "||
                -- ret_solicitud_generico <-> afi_derechohabiente
                "  AND  rsg.id_derechohabiente = ad.id_derechohabiente "||
                "  AND  rsg.id_solicitud       = rbg.id_solicitud      "||
                "  AND  rbg.tpo_beneficiario   = 2                     "||
                -- Solo se notifica grupo 1
                "  AND  rlg.gpo_ley73          = 1                     "||
                -- Pasar de estado 72 a 73 y de 214 a 215, Se quitan los casos 214 Ventanilla Infonavit segun req SACI2018-188
                "  AND  rsg.estado_solicitud   IN (72, 720)            "||
                "  AND  rsg.nss = " || p_nss

    PREPARE prp_notificacion_procesar_benef FROM v_sql
    DECLARE cur_notificacion_procesar_benef CURSOR FOR prp_notificacion_procesar_benef

    -- Datos de ret_pago_spei
    LET v_sql = "SELECT FIRST 1 cuenta_clabe    "||
                "FROM   ret_pago_spei           "||
                "WHERE  id_solicitud = ?        "||
                "AND    consec_beneficiario = ? "
    PREPARE prp_pago_spei FROM v_sql

    -- Obtiene el consecutivo del beneficiario 
    LET v_sql = "SELECT FIRST 1 a.consec_beneficiario                 "||
                "FROM   ret_beneficiario_generico a,                  "||
                "       ret_beneficiario_juridico b                   "||
                "WHERE  a.id_solicitud = b.id_solicitud               "||
                "AND    a.consec_beneficiario = b.consec_beneficiario "||
                "AND    a.id_solicitud = ?                            "||
                "AND    b.estado_solicitud = 72                       "
    PREPARE prp_consec_beneficiario FROM v_sql

    -- Obtiene el consecutivo del beneficiario 
    LET v_sql = "SELECT SUM(a.porcentaje)                             "||
                "FROM   ret_beneficiario_generico a,                  "||
                "       ret_beneficiario_juridico b                   "||
                "WHERE  a.id_solicitud = b.id_solicitud               "||
                "AND    a.consec_beneficiario = b.consec_beneficiario "||
                "AND    a.id_solicitud = ?                            "||
                "AND    b.estado_solicitud = 72                       "
    PREPARE prp_porcentaje_beneficiarios FROM v_sql
    
    -- Datos de ret_ws_consulta_pago_fico
    LET v_sql = "SELECT FIRST 1 MAX(h_consulta),rsp_referencia,               "||
                "       rsp_f_pago,rsp_estatus                                "||
                "FROM   ret_ws_consulta_pago_fico rpf                         "||
                "WHERE  f_consulta = (SELECT MAX(f_consulta)                  "||
                "                     FROM   ret_ws_consulta_pago_fico        "||
                "                     WHERE  id_solicitud = rpf.id_solicitud) "||
                "  AND  id_solicitud = ?                                      "||
                "GROUP  BY rsp_referencia,rsp_f_pago,rsp_estatus              "
    PREPARE prp_consulta_pago_fico FROM v_sql

    -- Datos de ret_beneficiario_generico
    LET v_sql = "SELECT FIRST 1 nombre,ap_paterno,       "||
                "       NVL(ap_materno,' '),id_entidad_federativa "||
                "FROM   ret_beneficiario_generico        "||
                "WHERE  id_solicitud = ?                 "||
                "AND    consec_beneficiario = ?          "
    PREPARE prp_beneficiario_generico FROM v_sql

    -- No se incluye en el query principal porque hay casos de NSS
    -- con la maxima secuencia de pensión repetida
    -- Consulta de la maxima secuencia de pension
    LET v_sql = "SELECT FIRST 1 MAX(sec_pension),id_datamart "||
                "FROM   ret_datamart                         "||
                "WHERE  nss = ?                              "||
                "GROUP  BY id_datamart                       "||
                "ORDER  BY id_datamart DESC                  "
    PREPARE prp_sec_datamart FROM v_sql

    -- Consulta para obtener regimen, tipo retiro, tipo seguro, 
    -- tipo pension, tipo prestacion, semanas cotizadas y tipo retiro
    LET v_sql = "SELECT rd.regimen,'E',rd.tpo_seguro, "||
                "       rd.tpo_pension,rd.tpo_prestacion,                 "||
                "       rd.semanas_cotizadas                              "||
                "FROM   ret_datamart           rd                         "||
                "WHERE  rd.sec_pension       = ?                          "||
                "  AND  rd.id_datamart       = ?                          "
    PREPARE prp_datos_datamart FROM v_sql

    -- Consulta para obtener regimen, tipo retiro, tipo seguro, 
    -- tipo pension, tipo prestacion, semanas cotizadas y secuencia de pension
    -- para los casos de Ventanilla Afore
    LET v_sql = "SELECT DISTINCT rd.regimen,rd.tpo_retiro,rd.tpo_seguro,  "||
                "       rd.tpo_pension,rd.tpo_prestacion,                 "||
                "       rd.sem_cotizadas,rd.sec_pension                   "||
                "FROM   ret_ws_sol_retiro_vent_afore           rd         "||
                "WHERE  rd.id_solicitud  = ?                              "
    PREPARE prp_datos_vent_afore FROM v_sql

        
    -- Consulta de montos
    LET v_sql = "SELECT NVL(SUM(monto_acciones),0),NVL(SUM(monto_pesos),0) "||
                "FROM   ret_preliquida                       "||
                "WHERE  id_referencia = ?                    "||
                "AND    subcuenta     = ?                    "||
                "AND    id_derechohabiente = ?               "
    PREPARE prp_montos_subcuenta FROM v_sql

    -- Consulta la fecha de valor
    LET v_sql = "SELECT FIRST 1 (f_valor - (DAY(f_valor)-1)) "||
                "FROM   ret_preliquida                       "||
                "WHERE  id_referencia = ?                    "||
                "AND    id_derechohabiente = ?               "
    PREPARE prp_fecha_movimiento FROM v_sql

    -- Actualizacion de estado_solicitud en ret_ley73_generico
    LET v_sql = "UPDATE ret_ley73_generico    "||
                "SET    estado_solicitud = ? "||
                "WHERE  id_solicitud     = ?  "
    PREPARE prp_actualiza_ley73 FROM v_sql

    -- Actualizacion de estado_solicitud en ret_solicitud_generico
    LET v_sql = "UPDATE ret_solicitud_generico "||
                "SET    estado_solicitud = ?  "||
                "WHERE  id_solicitud     = ?   "
    PREPARE prp_actualiza_sol_generico FROM v_sql

    -- Actualizacion de estado_solicitud en ret_solicitud_generico
    LET v_sql = "UPDATE ret_beneficiario_juridico "||
                "SET    estado_solicitud = ?      "||
                "WHERE  id_solicitud     = ?      "||
                "AND    estado_solicitud = 72     "
    PREPARE prp_actualiza_benef_juridico_generico FROM v_sql

    -- Insert para dejar evidencia de la respuesta de procesar
    LET v_sql = "INSERT INTO ret_ws_notifica_procesar(
                   id_solicitud,f_notifica,estado_pago,diag_notifica,folio_notificacion,indicador_beneficiario,
                   entidad_federativa,nss,rfc_trabajador,curp_trabajador,cta_clabe,grupo_trabajador,sec_pension,
                   regimen,tpo_retiro,tpo_seguro,tpo_pension,tpo_prestacion,semanas_cotizadas,nombre_beneficiario,
                   paterno_beneficiario,materno_beneficiario,rfc_benficiario,curp_beneficiario,aivs_viv92,aivs_viv97,
                   f_valor_viv,imp_viv92,imp_viv97,otros_imp_vivienda,imp_neto_dep_vivienda,comentarios,f_pago,
                   referencia_pago,observaciones)
                 VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    PREPARE exe_inserta_documento FROM v_sql
    
    FOREACH cur_notificacion_procesar INTO v_solicitud_notificacion.*, v_id_derechohabiente

        DISPLAY ""
        DISPLAY ""
        DISPLAY "Solicitud: ",v_solicitud_notificacion.id_solicitud
        LET v_consec_beneficiario = 1;

        -- pago spei
        INITIALIZE v_clabe TO NULL 
        EXECUTE prp_pago_spei INTO v_clabe USING v_solicitud_notificacion.id_solicitud, v_consec_beneficiario

        -- Consulta pago fico
        INITIALIZE v_h_consulta, v_rsp_referencia, v_rsp_f_pago, v_rsp_estatus TO NULL 
        EXECUTE prp_consulta_pago_fico INTO v_h_consulta,v_rsp_referencia,v_rsp_f_pago,v_rsp_estatus
                                       USING v_solicitud_notificacion.id_solicitud

        -- Beneficiario Generico
        INITIALIZE v_nombreBeneficiario,v_apellidoPaternoBeneficiario,v_apellidoMaternoBeneficiario,v_id_entidad_federativa TO NULL 
        EXECUTE prp_beneficiario_generico INTO v_nombreBeneficiario,v_apellidoPaternoBeneficiario,
                                               v_apellidoMaternoBeneficiario,v_id_entidad_federativa
                                          USING v_solicitud_notificacion.id_solicitud, v_consec_beneficiario
        IF v_solicitud_notificacion.grupoTrabajador = '0101' THEN                                   
           -- Se obtiene la maxima secuencia de pension
           INITIALIZE v_secuenciaPension,v_id_datamart  TO NULL 
           EXECUTE prp_sec_datamart INTO v_secuenciaPension,v_id_datamart USING v_solicitud_notificacion.nss

           -- Se obtienen los datos de datamart
           INITIALIZE v_regimen,v_tipoRetiro,v_tipoSeguro,v_tipoPension,v_tipoPrestacion,v_semanasCotizadas TO NULL
           EXECUTE prp_datos_datamart INTO v_regimen,v_tipoRetiro,v_tipoSeguro,
                                           v_tipoPension,v_tipoPrestacion,v_semanasCotizadas
                                      USING v_secuenciaPension,v_id_datamart
        ELSE 
           -- Se obtienen los datos ventanilla afore
           INITIALIZE v_regimen,v_tipoRetiro,v_tipoSeguro,v_tipoPension,v_tipoPrestacion,v_semanasCotizadas,v_secuenciaPension TO NULL
           EXECUTE prp_datos_vent_afore INTO v_regimen,v_tipoRetiro,v_tipoSeguro,
                                           v_tipoPension,v_tipoPrestacion,v_semanasCotizadas,
                                           v_secuenciaPension
                                      USING v_solicitud_notificacion.id_solicitud 
        END IF  
        IF v_solicitud_notificacion.estado_solicitud = 71 OR 
           v_solicitud_notificacion.estado_solicitud = 72 THEN 
            -- Se obtienen los montos de la subcuenta 8 (vivienda 92)
            INITIALIZE v_vivienda92_aivs,v_vivienda92_pesos TO NULL
            EXECUTE prp_montos_subcuenta INTO v_vivienda92_aivs,v_vivienda92_pesos
                                        USING  v_solicitud_notificacion.id_solicitud,subcuenta_8, v_id_derechohabiente        

           -- Se obtienen los montos de la subcuenta 4 (vivienda 97)
           INITIALIZE v_vivienda97_aivs,v_vivienda97_pesos TO NULL
           EXECUTE prp_montos_subcuenta INTO v_vivienda97_aivs,v_vivienda97_pesos
                                        USING  v_solicitud_notificacion.id_solicitud,subcuenta_4, v_id_derechohabiente        
        ELSE 
            LET v_vivienda92_aivs = 0.0
            LET v_vivienda92_pesos = 0.0
            LET v_vivienda97_aivs = 0.0
            LET v_vivienda97_pesos = 0.0
        END IF 

        INITIALIZE v_f_valor TO NULL
        EXECUTE prp_fecha_movimiento INTO v_f_valor 
                                     USING v_solicitud_notificacion.id_solicitud, v_id_derechohabiente        
        
        DISPLAY "la fecha de pago ", v_rsp_f_pago
        DISPLAY "  año ",v_rsp_f_pago[1,4]
        DISPLAY "  mes ",v_rsp_f_pago[5,6]
        DISPLAY "  dia ",v_rsp_f_pago[7,8]
        DISPLAY " Hora ",v_h_consulta
        LET v_f_pago = v_rsp_f_pago[1,4]||'-'||v_rsp_f_pago[5,6]||'-'||v_rsp_f_pago[7,8]||'T'||v_h_consulta||'.000Z'
        DISPLAY " Fecha de pago concatenada ", v_f_pago
        LET v_diagnostico = "101"
        IF v_solicitud_notificacion.estado_solicitud = 210 THEN 
           LET v_f_pago = v_f_valor USING "yyyy-mm-dd"||"T09:00:00.000Z"
           LET v_rsp_referencia = "000000000000"
           LET v_diagnostico = "998"
        END IF 
        INITIALIZE ns1notificacionCuentaClabeRequest TO NULL
        -- Datos fijos para realizar la consulta
        LET ns1notificacionCuentaClabeRequest.idssn.idSistema      = 12 
        LET ns1notificacionCuentaClabeRequest.idssn.idEbusiness    = 13 
        LET ns1notificacionCuentaClabeRequest.idssn.idPortafolio   = 16 
        LET ns1notificacionCuentaClabeRequest.idssn.idServicio     = 125 
        LET ns1notificacionCuentaClabeRequest.idssn.idCliente      = 42 
        LET ns1notificacionCuentaClabeRequest.idssn.idCanal        = 13
        LET ns1notificacionCuentaClabeRequest.idssn.codoperCliente = "INFONAVIT"
        LET ns1notificacionCuentaClabeRequest.idssn.fecha          = YEAR(TODAY) USING "&&&&",'-',MONTH(TODAY) USING "&&",'-',DAY(TODAY) USING "&&",'T',CURRENT HOUR TO SECOND,'.000Z'

        LET ns1notificacionCuentaClabeRequest.cuerpo.folioOperacion                       = 0
        LET ns1notificacionCuentaClabeRequest.cuerpo.folioNotificacion                    = v_solicitud_notificacion.nss||v_clabe
        LET ns1notificacionCuentaClabeRequest.cuerpo.indicadorBeneficiario                = 1
        LET ns1notificacionCuentaClabeRequest.cuerpo.entidadFederativa                    = v_id_entidad_federativa USING "&&"
        LET ns1notificacionCuentaClabeRequest.cuerpo.nss                                  = v_solicitud_notificacion.nss
        LET ns1notificacionCuentaClabeRequest.cuerpo.rfcTrabajador                        = v_solicitud_notificacion.rfcTrabajador
        LET ns1notificacionCuentaClabeRequest.cuerpo.curpTrabajador                       = v_solicitud_notificacion.curpTrabajador
        LET ns1notificacionCuentaClabeRequest.cuerpo.clabe                                = v_clabe
        LET ns1notificacionCuentaClabeRequest.cuerpo.grupoTrabajador                      = v_solicitud_notificacion.grupoTrabajador
        LET ns1notificacionCuentaClabeRequest.cuerpo.secuenciaPension                     = v_secuenciaPension USING "&&"
        LET ns1notificacionCuentaClabeRequest.cuerpo.regimen                              = v_regimen
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoRetiro                           = v_tipoRetiro
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoSeguro                           = v_tipoSeguro
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoPension                          = v_tipoPension
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoPrestacion                       = v_tipoPrestacion
        LET ns1notificacionCuentaClabeRequest.cuerpo.semanasCotizadas                     = v_semanasCotizadas USING "&&&&"
        LET ns1notificacionCuentaClabeRequest.cuerpo.nombreBeneficiario                   = v_nombreBeneficiario CLIPPED 
        LET ns1notificacionCuentaClabeRequest.cuerpo.apellidoPaternoBeneficiario          = v_apellidoPaternoBeneficiario CLIPPED
        LET ns1notificacionCuentaClabeRequest.cuerpo.apellidoMaternoBeneficiario          = v_apellidoMaternoBeneficiario CLIPPED 
        LET ns1notificacionCuentaClabeRequest.cuerpo.rfcBeneficiario                      = v_solicitud_notificacion.rfcTrabajador -- Por ahora no se tienen
        LET ns1notificacionCuentaClabeRequest.cuerpo.curpBeneficiario                     = v_solicitud_notificacion.curpTrabajador -- estos campos
        -- Constantes --------------------------------------------------------------------------
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore1                        = "02"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore1                = "0.0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore1                = "0.0"
     --   LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore1   = "0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore1   = "0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore2                        = "12"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore2                = "0.0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore2                = "0.0"
     --   LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore2   = "0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore2   = "0.0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoSiefores        = "0.0"
        ----------------------------------------------------------------------------------------
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda92Aivs                       = v_vivienda92_aivs * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda97Aivs                       = v_vivienda97_aivs * (-1)
--        LET ns1notificacionCuentaClabeRequest.cuerpo.fechaValorViviendaMovimientoContable = util.Datetime.toUtc(v_f_valor USING "yyyy-mm-dd"||"T09:00:00.00000")
        LET ns1notificacionCuentaClabeRequest.cuerpo.fechaValorViviendaMovimientoContable = v_f_valor USING "yyyy-mm-dd"||"T09:00:00.000Z" 
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda92                           = v_vivienda92_pesos * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda97                           = v_vivienda97_pesos * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesVivienda                = "0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoVivienda        = (v_vivienda92_pesos + v_vivienda97_pesos) * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.diagnosticoRecepcion                 = v_diagnostico
        LET ns1notificacionCuentaClabeRequest.cuerpo.comentarios                          = ""
        LET ns1notificacionCuentaClabeRequest.cuerpo.fechaPago                            = v_f_pago CLIPPED 
        LET ns1notificacionCuentaClabeRequest.cuerpo.referenciaPago                       = v_rsp_referencia CLIPPED 
        LET ns1notificacionCuentaClabeRequest.cuerpo.observaciones                        = ""

        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "------------------------------------------------------------------------"
        DISPLAY " Los datos a enviar :" 
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "folioOperacion                       >", ns1notificacionCuentaClabeRequest.cuerpo.folioOperacion                       ,"<"
        DISPLAY "folioNotificacion                    >", ns1notificacionCuentaClabeRequest.cuerpo.folioNotificacion                    ,"<"
        DISPLAY "indicadorBeneficiario                >", ns1notificacionCuentaClabeRequest.cuerpo.indicadorBeneficiario                ,"<"
        DISPLAY "entidadFederativa                    >", ns1notificacionCuentaClabeRequest.cuerpo.entidadFederativa                    ,"<"
        DISPLAY "nss                                  >", ns1notificacionCuentaClabeRequest.cuerpo.nss                                  ,"<"
        DISPLAY "rfcTrabajador                        >", ns1notificacionCuentaClabeRequest.cuerpo.rfcTrabajador                        ,"<"
        DISPLAY "curpTrabajador                       >", ns1notificacionCuentaClabeRequest.cuerpo.curpTrabajador                       ,"<"
        DISPLAY "clabe                                >", ns1notificacionCuentaClabeRequest.cuerpo.clabe                                ,"<"
        DISPLAY "grupoTrabajador                      >", ns1notificacionCuentaClabeRequest.cuerpo.grupoTrabajador                      ,"<"
        DISPLAY "secuenciaPension                     >", ns1notificacionCuentaClabeRequest.cuerpo.secuenciaPension                     ,"<"
        DISPLAY "regimen                              >", ns1notificacionCuentaClabeRequest.cuerpo.regimen                              ,"<"
        DISPLAY "tipoRetiro                           >", ns1notificacionCuentaClabeRequest.cuerpo.tipoRetiro                           ,"<"
        DISPLAY "tipoSeguro                           >", ns1notificacionCuentaClabeRequest.cuerpo.tipoSeguro                           ,"<"
        DISPLAY "tipoPension                          >", ns1notificacionCuentaClabeRequest.cuerpo.tipoPension                          ,"<"
        DISPLAY "tipoPrestacion                       >", ns1notificacionCuentaClabeRequest.cuerpo.tipoPrestacion                       ,"<"
        DISPLAY "semanasCotizadas                     >", ns1notificacionCuentaClabeRequest.cuerpo.semanasCotizadas                     ,"<"
        DISPLAY "nombreBeneficiario                   >", ns1notificacionCuentaClabeRequest.cuerpo.nombreBeneficiario                   ,"<"
        DISPLAY "apellidoPaternoBeneficiario          >", ns1notificacionCuentaClabeRequest.cuerpo.apellidoPaternoBeneficiario          ,"<"
        DISPLAY "apellidoMaternoBeneficiario          >", ns1notificacionCuentaClabeRequest.cuerpo.apellidoMaternoBeneficiario          ,"<"
        DISPLAY "rfcBeneficiario                      >", ns1notificacionCuentaClabeRequest.cuerpo.rfcBeneficiario                      ,"<"
        DISPLAY "curpBeneficiario                     >", ns1notificacionCuentaClabeRequest.cuerpo.curpBeneficiario                     ,"<"
        -- Constantes --------------------------------------------------------------------------
        DISPLAY "claveSiefore1                        >", ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore1                        ,"<"
        DISPLAY "claveRetiro92Siefore1                >", ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore1                ,"<"
        DISPLAY "claveRetiro97Siefore1                >", ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore1                ,"<"
  --      DISPLAY "otrosImportesAhorro73ClaveSiefore1   >", ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore1   ,"<"
        DISPLAY "importeNetoDepositadoClaveSiefore1   >", ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore1   ,"<"
        DISPLAY "claveSiefore2                        >", ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore2                        ,"<"
        DISPLAY "claveRetiro92Siefore2                >", ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore2                ,"<"
        DISPLAY "claveRetiro97Siefore2                >", ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore2                ,"<"
  --      DISPLAY "otrosImportesAhorro73ClaveSiefore2   >", ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore2   ,"<"
        DISPLAY "importeNetoDepositadoClaveSiefore2   >", ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore2   ,"<"
        DISPLAY "importeNetoDepositadoSiefores        >", ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoSiefores        ,"<"
        ----------------------------------------------------------------------------------------
        DISPLAY "vivienda92Aivs                       >", ns1notificacionCuentaClabeRequest.cuerpo.vivienda92Aivs                       ,"<"
        DISPLAY "vivienda97Aivs                       >", ns1notificacionCuentaClabeRequest.cuerpo.vivienda97Aivs                       ,"<"
        DISPLAY "fechaValorViviendaMovimientoContable >", ns1notificacionCuentaClabeRequest.cuerpo.fechaValorViviendaMovimientoContable ,"<"
        DISPLAY "vivienda92                           >", ns1notificacionCuentaClabeRequest.cuerpo.vivienda92                           ,"<"
        DISPLAY "vivienda97                           >", ns1notificacionCuentaClabeRequest.cuerpo.vivienda97                           ,"<"
        DISPLAY "otrosImportesVivienda                >", ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesVivienda                ,"<"
        DISPLAY "importeNetoDepositadoVivienda        >", ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoVivienda        ,"<"
        DISPLAY "diagnosticoRecepcion                 >", ns1notificacionCuentaClabeRequest.cuerpo.diagnosticoRecepcion                 ,"<"
        DISPLAY "comentarios                          >", ns1notificacionCuentaClabeRequest.cuerpo.comentarios                          ,"<"
        DISPLAY "fechaPago                            >", ns1notificacionCuentaClabeRequest.cuerpo.fechaPago                            ,"<"
        DISPLAY "referenciaPago                       >", ns1notificacionCuentaClabeRequest.cuerpo.referenciaPago                       ,"<"
        DISPLAY "observaciones                        >", ns1notificacionCuentaClabeRequest.cuerpo.observaciones                        ,"<"
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "------------------------------------------------------------------------"

        INITIALIZE ns1notificacionCuentaClabeResponse TO NULL
        INITIALIZE wsError TO null
        INITIALIZE v_rec_notifica TO NULL 
        -- se ejecuta el WS
        CALL notificacionCuentaClabe_g() RETURNING v_resultado
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "########################################################################"
        DISPLAY " EL resultado de la Notificación "
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "Resultado de la ejecucion  :", v_resultado                                              ,":"
        DISPLAY "CODE                       :", wsError.code                                             ,":"
        DISPLAY "CODENS                     :", wsError.codeNS                                           ,":"
        DISPLAY "DESCRIPTION                :", wsError.description                                      ,":"
        DISPLAY "ACTION                     :", wsError.action                                           ,":"
        DISPLAY "SQLCA.SQLERRM              :", SQLCA.sqlerrm                                            ,":"
        IF v_resultado = 0 THEN 
--        IF ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta = 'OK' THEN 
           DISPLAY "Codigo Respuesta           :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta   ,":"
           DISPLAY "Codigo Respuesta Oper      :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuestaOpr,":"
           DISPLAY "Codigo Operacion           :", ns1notificacionCuentaClabeResponse.ssnrop.codoper        ,":"
           DISPLAY "Codigo Oper Cliente        :", ns1notificacionCuentaClabeResponse.ssnrop.codoperCliente ,":"
           DISPLAY "Desc Respuesta             :", ns1notificacionCuentaClabeResponse.ssnrop.descRespuesta  ,":"
           DISPLAY "Fecha                      :", ns1notificacionCuentaClabeResponse.ssnrop.fecha          ,":"
           DISPLAY "motivoRechazo              :", ns1notificacionCuentaClabeResponse.objetoRespuesta.motivoRechazo ,":"
           DISPLAY "Descripcion diagnostico    :", ns1notificacionCuentaClabeResponse.objetoRespuesta.descripcionDiagnostico ,":"
           DISPLAY "Folio Notificacion         :", ns1notificacionCuentaClabeResponse.objetoRespuesta.folioNotificacion ,":"
           DISPLAY "NSS                        :", ns1notificacionCuentaClabeResponse.objetoRespuesta.nss ,":"
           DISPLAY "Resultado operacion        :", ns1notificacionCuentaClabeResponse.objetoRespuesta.resultadoOperacion ,":"
           DISPLAY "Motivos-base               :", ns1notificacionCuentaClabeResponse.ssnrop.motivos.motivo[1].base ,":"
           DISPLAY "Motivos-descripcion        :", ns1notificacionCuentaClabeResponse.ssnrop.motivos.motivo[1].descripcion ,":"
           DISPLAY "Motivos-id Motivo          :", ns1notificacionCuentaClabeResponse.ssnrop.motivos.motivo[1].idMotivo ,":"
           DISPLAY "   "
           DISPLAY "   "
           DISPLAY "########################################################################"
           INITIALIZE v_rec_notifica TO NULL
           LET v_rec_notifica.id_solicitud  = v_solicitud_notificacion.id_solicitud
           LET v_rec_notifica.f_notifica    = CURRENT YEAR TO SECOND
           LET v_rec_notifica.estado_pago   = ns1notificacionCuentaClabeResponse.objetoRespuesta.motivoRechazo
           LET v_rec_notifica.diag_notifica = ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta
           LET v_rec_notifica.folio_notificacion = ns1notificacionCuentaClabeResponse.objetoRespuesta.folioNotificacion
           LET v_rec_notifica.indicador_beneficiario = ns1notificacionCuentaClabeResponse.objetoRespuesta.indicadorBeneficiario
           LET v_rec_notifica.entidad_federativa     = ns1notificacionCuentaClabeResponse.objetoRespuesta.entidadFederativa
           LET v_rec_notifica.nss                    = ns1notificacionCuentaClabeResponse.objetoRespuesta.nss
           LET v_rec_notifica.rfc_trabajador         = ns1notificacionCuentaClabeResponse.objetoRespuesta.rfcTrabajador
           LET v_rec_notifica.curp_trabajador        = ns1notificacionCuentaClabeResponse.objetoRespuesta.curpTrabajador
           LET v_rec_notifica.cta_clabe              = ns1notificacionCuentaClabeResponse.objetoRespuesta.clabe
           LET v_rec_notifica.grupo_trabajador       = ns1notificacionCuentaClabeResponse.objetoRespuesta.grupoTrabajador
           LET v_rec_notifica.sec_pension            = ns1notificacionCuentaClabeResponse.objetoRespuesta.secuenciaPension
           LET v_rec_notifica.regimen                = ns1notificacionCuentaClabeResponse.objetoRespuesta.regimen
           LET v_rec_notifica.tpo_retiro             = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoRetiro
           LET v_rec_notifica.tpo_seguro             = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoSeguro
           LET v_rec_notifica.tpo_pension            = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoPension
           LET v_rec_notifica.tpo_prestacion         = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoPrestacion
           LET v_rec_notifica.semanas_cotizadas      = ns1notificacionCuentaClabeResponse.objetoRespuesta.semanasCotizadas
           LET v_rec_notifica.nombre_beneficiario    = ns1notificacionCuentaClabeResponse.objetoRespuesta.nombreBeneficiario
           LET v_rec_notifica.paterno_beneficiario   = ns1notificacionCuentaClabeResponse.objetoRespuesta.apellidoPaternoBeneficiario
           LET v_rec_notifica.materno_beneficiario   = ns1notificacionCuentaClabeResponse.objetoRespuesta.apellidoMaternoBeneficiario
           LET v_rec_notifica.rfc_benficiario        = ns1notificacionCuentaClabeResponse.objetoRespuesta.rfcBeneficiario
           LET v_rec_notifica.curp_beneficiario      = ns1notificacionCuentaClabeResponse.objetoRespuesta.curpBeneficiario
           LET v_rec_notifica.aivs_viv92             = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda92Aivs
           LET v_rec_notifica.aivs_viv97             = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda97Aivs
           LET v_rec_notifica.f_valor_viv            = ns1notificacionCuentaClabeResponse.objetoRespuesta.fechaValorViviendaMovimientoContable
           LET v_rec_notifica.imp_viv92              = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda92
           LET v_rec_notifica.imp_viv97              = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda97
           LET v_rec_notifica.otros_imp_vivienda     = ns1notificacionCuentaClabeResponse.objetoRespuesta.otrosImportesVivienda
           LET v_rec_notifica.imp_neto_dep_vivienda  = ns1notificacionCuentaClabeResponse.objetoRespuesta.importeNetoDepositadoVivienda
           LET v_rec_notifica.comentarios            = ns1notificacionCuentaClabeResponse.objetoRespuesta.comentarios
           LET v_rec_notifica.f_pago                 = ns1notificacionCuentaClabeResponse.objetoRespuesta.fechaPago
           LET v_rec_notifica.referencia_pago        = ns1notificacionCuentaClabeResponse.objetoRespuesta.referenciaPago
           LET v_rec_notifica.observaciones          = ns1notificacionCuentaClabeResponse.objetoRespuesta.observaciones
           
           EXECUTE exe_inserta_documento USING
              v_rec_notifica.id_solicitud           ,v_rec_notifica.f_notifica             ,v_rec_notifica.estado_pago            ,
              v_rec_notifica.diag_notifica          ,v_rec_notifica.folio_notificacion     ,v_rec_notifica.indicador_beneficiario ,
              v_rec_notifica.entidad_federativa     ,v_rec_notifica.nss                    ,v_rec_notifica.rfc_trabajador         ,
              v_rec_notifica.curp_trabajador        ,v_rec_notifica.cta_clabe              ,v_rec_notifica.grupo_trabajador       ,
              v_rec_notifica.sec_pension            ,v_rec_notifica.regimen                ,v_rec_notifica.tpo_retiro             ,
              v_rec_notifica.tpo_seguro             ,v_rec_notifica.tpo_pension            ,v_rec_notifica.tpo_prestacion         ,
              v_rec_notifica.semanas_cotizadas      ,v_rec_notifica.nombre_beneficiario    ,v_rec_notifica.paterno_beneficiario   ,
              v_rec_notifica.materno_beneficiario   ,v_rec_notifica.rfc_benficiario        ,v_rec_notifica.curp_beneficiario      ,
              v_rec_notifica.aivs_viv92             ,v_rec_notifica.aivs_viv97             ,v_rec_notifica.f_valor_viv            ,
              v_rec_notifica.imp_viv92              ,v_rec_notifica.imp_viv97              ,v_rec_notifica.otros_imp_vivienda     ,
              v_rec_notifica.imp_neto_dep_vivienda  ,v_rec_notifica.comentarios            ,v_rec_notifica.f_pago                 ,
              v_rec_notifica.referencia_pago        ,v_rec_notifica.observaciones          
        ELSE 
           DISPLAY "Codigo Respuesta           :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta   ,":"
           DISPLAY "Codigo Respuesta Oper      :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuestaOpr,":"
           DISPLAY "Codigo Operacion           :", ns1notificacionCuentaClabeResponse.ssnrop.codoper        ,":"
           DISPLAY "Codigo Oper Cliente        :", ns1notificacionCuentaClabeResponse.ssnrop.codoperCliente ,":"
           DISPLAY "Desc Respuesta             :", ns1notificacionCuentaClabeResponse.ssnrop.descRespuesta  ,":"
           DISPLAY "Fecha                      :", ns1notificacionCuentaClabeResponse.ssnrop.fecha          ,":"
        END IF 
        LET  v_respuesta = ns1notificacionCuentaClabeResponse.objetoRespuesta.motivoRechazo
           
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "Antes de la evaluación "
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY " v_resultado >",v_resultado, "<"
        DISPLAY " v_respuesta >",v_respuesta, "<"

        
        IF v_resultado = 0 AND (v_respuesta = "101" OR v_respuesta = "203") THEN
           --  101 - Solicitud de Notifiación Aceptada
           --  203 - Solicitud Previamente Notificada

            IF v_solicitud_notificacion.estado_solicitud = 72 OR 
               v_solicitud_notificacion.estado_solicitud = 71 THEN
                LET v_estado_solicitud_sig = 73
            END IF

            IF v_solicitud_notificacion.estado_solicitud = 210 THEN
                LET v_estado_solicitud_sig = 214
            END IF
            IF v_solicitud_notificacion.estado_solicitud = 71 OR 
               v_solicitud_notificacion.estado_solicitud = 210 THEN 
                --- Para los casos de Ventanilla Afore se deben desmarcar las cuentas
                CALL fn_ret_generico_desmarca_cuenta(v_id_derechohabiente, 
                                                     v_marca,
                                                     v_solicitud_notificacion.id_solicitud, 
                                                     v_marca,
                                                     'safreviv', 
                                                     g_proceso_cod_ret_ley73_ws)
            END IF             
            DISPLAY "   "
            DISPLAY "   "
            DISPLAY " La solicitud debe quedar en estado >",v_estado_solicitud_sig, "<"

            EXECUTE prp_actualiza_ley73        USING v_estado_solicitud_sig,v_solicitud_notificacion.id_solicitud
            EXECUTE prp_actualiza_sol_generico USING v_estado_solicitud_sig,v_solicitud_notificacion.id_solicitud

            -- Se cuenta el numero de solicitudes actualizadas
            DISPLAY "Actualizada a ",v_estado_solicitud_sig
            LET v_solicitudes_informadas = v_solicitudes_informadas + 1

        ELSE
            DISPLAY "No actualizada"
            LET v_solicitudes_no_informadas = v_solicitudes_no_informadas + 1
        END IF

        -- Se cuenta el numero de solicitudes procesadas
        LET v_solicitudes_procesadas = v_solicitudes_procesadas + 1

    END FOREACH

--- Se procesan las solicitudes de beneficiarios
   DISPLAY " se procesan beneficiarios" 
    INITIALIZE v_solicitud_notificacion, v_id_derechohabiente TO NULL
    FOREACH cur_notificacion_procesar_benef INTO v_solicitud_notificacion.*, v_id_derechohabiente

        DISPLAY ""
        DISPLAY ""
        DISPLAY "Solicitud: ",v_solicitud_notificacion.id_solicitud
        -- Se obtiene el consecutivo de un solo beneficiario que tenga estado de pagado
        INITIALIZE v_consec_beneficiario TO NULL
        EXECUTE prp_consec_beneficiario INTO v_consec_beneficiario USING v_solicitud_notificacion.id_solicitud

        -- Se obtiene el porcentaje de pago para reportar el importe solo de los aceptados
        INITIALIZE v_porcentaje TO NULL
        EXECUTE prp_porcentaje_beneficiarios INTO v_porcentaje USING v_solicitud_notificacion.id_solicitud

        -- pago spei
        INITIALIZE v_clabe TO NULL
        EXECUTE prp_pago_spei INTO v_clabe USING v_solicitud_notificacion.id_solicitud, v_consec_beneficiario

        -- Consulta pago fico
        LET v_solicitud_consulta_pago = (v_solicitud_notificacion.id_solicitud*10)+v_consec_beneficiario
        INITIALIZE v_h_consulta,v_rsp_referencia,v_rsp_f_pago,v_rsp_estatus TO NULL
        EXECUTE prp_consulta_pago_fico INTO v_h_consulta,v_rsp_referencia,v_rsp_f_pago,v_rsp_estatus
                                       USING v_solicitud_consulta_pago

        -- Beneficiario Generico
        INITIALIZE v_nombreBeneficiario,v_apellidoPaternoBeneficiario,v_apellidoMaternoBeneficiario,v_id_entidad_federativa TO NULL
        EXECUTE prp_beneficiario_generico INTO v_nombreBeneficiario,v_apellidoPaternoBeneficiario,
                                               v_apellidoMaternoBeneficiario,v_id_entidad_federativa
                                          USING v_solicitud_notificacion.id_solicitud, v_consec_beneficiario
        IF v_solicitud_notificacion.grupoTrabajador = '0101' THEN                                   
           -- Se obtiene la maxima secuencia de pension
           INITIALIZE v_secuenciaPension,v_id_datamart TO NULL
           EXECUTE prp_sec_datamart INTO v_secuenciaPension,v_id_datamart USING v_solicitud_notificacion.nss

           -- Se obtienen los datos de datamart
           INITIALIZE v_regimen,v_tipoRetiro,v_tipoSeguro,v_tipoPension,v_tipoPrestacion,v_semanasCotizadas TO NULL
           EXECUTE prp_datos_datamart INTO v_regimen,v_tipoRetiro,v_tipoSeguro,
                                           v_tipoPension,v_tipoPrestacion,v_semanasCotizadas
                                      USING v_secuenciaPension,v_id_datamart
        ELSE 
           -- Se obtienen los datos ventanilla afore
           INITIALIZE v_regimen,v_tipoRetiro,v_tipoSeguro,v_tipoPension,v_tipoPrestacion,v_semanasCotizadas,v_secuenciaPension TO NULL
           EXECUTE prp_datos_vent_afore INTO v_regimen,v_tipoRetiro,v_tipoSeguro,
                                           v_tipoPension,v_tipoPrestacion,v_semanasCotizadas,
                                           v_secuenciaPension
                                      USING v_solicitud_notificacion.id_solicitud 
        END IF  
        IF v_solicitud_notificacion.estado_solicitud = 720 OR 
           v_solicitud_notificacion.estado_solicitud = 72 THEN 
           -- Se obtienen los montos de la subcuenta 8 (vivienda 92)
           INITIALIZE v_vivienda92_aivs,v_vivienda92_pesos TO NULL
           EXECUTE prp_montos_subcuenta INTO v_vivienda92_aivs,v_vivienda92_pesos
                                        USING  v_solicitud_notificacion.id_solicitud,subcuenta_8, v_id_derechohabiente

            LET v_vivienda92_aivs  = v_vivienda92_aivs  * (v_porcentaje/100)
            LET v_vivienda92_pesos = v_vivienda92_pesos * (v_porcentaje/100)

           -- Se obtienen los montos de la subcuenta 4 (vivienda 97)
           INITIALIZE v_vivienda97_aivs,v_vivienda97_pesos TO NULL 
           EXECUTE prp_montos_subcuenta INTO v_vivienda97_aivs,v_vivienda97_pesos
                                        USING  v_solicitud_notificacion.id_solicitud,subcuenta_4, v_id_derechohabiente        
            LET v_vivienda97_aivs  = v_vivienda97_aivs  * (v_porcentaje/100)
            LET v_vivienda97_pesos = v_vivienda97_pesos * (v_porcentaje/100)
        ELSE 
            LET v_vivienda92_aivs = 0.0
            LET v_vivienda92_pesos = 0.0
            LET v_vivienda97_aivs = 0.0
            LET v_vivienda97_pesos = 0.0
        END IF 

        INITIALIZE v_f_valor TO NULL
        EXECUTE prp_fecha_movimiento INTO v_f_valor 
                                     USING v_solicitud_notificacion.id_solicitud, v_id_derechohabiente        
                
        DISPLAY "la fecha de pago ", v_rsp_f_pago
        DISPLAY "  año ",v_rsp_f_pago[1,4]
        DISPLAY "  mes ",v_rsp_f_pago[5,6]
        DISPLAY "  dia ",v_rsp_f_pago[7,8]
        DISPLAY " Hora ",v_h_consulta
        LET v_f_pago = v_rsp_f_pago[1,4]||'-'||v_rsp_f_pago[5,6]||'-'||v_rsp_f_pago[7,8]||'T'||v_h_consulta||'.000Z'
        DISPLAY " Fecha de pago concatenada ", v_f_pago
        LET v_diagnostico = "101"
        IF v_solicitud_notificacion.estado_solicitud = 210 THEN 
           LET v_f_pago = v_f_valor USING "yyyy-mm-dd"||"T09:00:00.000Z"
           LET v_rsp_referencia = "000000000000"
           LET v_diagnostico = "998"
        END IF 

        INITIALIZE ns1notificacionCuentaClabeRequest TO NULL
        -- Datos fijos para realizar la consulta
        LET ns1notificacionCuentaClabeRequest.idssn.idSistema      = 12 
        LET ns1notificacionCuentaClabeRequest.idssn.idEbusiness    = 13 
        LET ns1notificacionCuentaClabeRequest.idssn.idPortafolio   = 16 
        LET ns1notificacionCuentaClabeRequest.idssn.idServicio     = 125 
        LET ns1notificacionCuentaClabeRequest.idssn.idCliente      = 42 
        LET ns1notificacionCuentaClabeRequest.idssn.idCanal        = 13
        LET ns1notificacionCuentaClabeRequest.idssn.codoperCliente = "INFONAVIT"
        LET ns1notificacionCuentaClabeRequest.idssn.fecha          = YEAR(TODAY) USING "&&&&",'-',MONTH(TODAY) USING "&&",'-',DAY(TODAY) USING "&&",'T',CURRENT HOUR TO SECOND,'.000Z'
        
        LET ns1notificacionCuentaClabeRequest.cuerpo.folioOperacion                       = 0
        LET ns1notificacionCuentaClabeRequest.cuerpo.folioNotificacion                    = v_solicitud_notificacion.nss||v_clabe
        LET ns1notificacionCuentaClabeRequest.cuerpo.indicadorBeneficiario                = 1
        LET ns1notificacionCuentaClabeRequest.cuerpo.entidadFederativa                    = v_id_entidad_federativa USING "&&"
        LET ns1notificacionCuentaClabeRequest.cuerpo.nss                                  = v_solicitud_notificacion.nss
        LET ns1notificacionCuentaClabeRequest.cuerpo.rfcTrabajador                        = v_solicitud_notificacion.rfcTrabajador
        LET ns1notificacionCuentaClabeRequest.cuerpo.curpTrabajador                       = v_solicitud_notificacion.curpTrabajador
        LET ns1notificacionCuentaClabeRequest.cuerpo.clabe                                = v_clabe
        LET ns1notificacionCuentaClabeRequest.cuerpo.grupoTrabajador                      = v_solicitud_notificacion.grupoTrabajador
        LET ns1notificacionCuentaClabeRequest.cuerpo.secuenciaPension                     = v_secuenciaPension USING "&&"
        LET ns1notificacionCuentaClabeRequest.cuerpo.regimen                              = v_regimen
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoRetiro                           = v_tipoRetiro
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoSeguro                           = v_tipoSeguro
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoPension                          = v_tipoPension
        LET ns1notificacionCuentaClabeRequest.cuerpo.tipoPrestacion                       = v_tipoPrestacion
        LET ns1notificacionCuentaClabeRequest.cuerpo.semanasCotizadas                     = v_semanasCotizadas USING "&&&&"
        LET ns1notificacionCuentaClabeRequest.cuerpo.nombreBeneficiario                   = v_nombreBeneficiario CLIPPED
        LET ns1notificacionCuentaClabeRequest.cuerpo.apellidoPaternoBeneficiario          = v_apellidoPaternoBeneficiario CLIPPED
        LET ns1notificacionCuentaClabeRequest.cuerpo.apellidoMaternoBeneficiario          = v_apellidoMaternoBeneficiario CLIPPED
        LET ns1notificacionCuentaClabeRequest.cuerpo.rfcBeneficiario                      = v_solicitud_notificacion.rfcTrabajador -- Por ahora no se tienen
        LET ns1notificacionCuentaClabeRequest.cuerpo.curpBeneficiario                     = v_solicitud_notificacion.curpTrabajador -- estos campos
        -- Constantes --------------------------------------------------------------------------
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore1                        = "02"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore1                = "0.0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore1                = "0.0"
--        LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore1   = 0
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore1   = "0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveSiefore2                        = "12"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro92Siefore2                = "0.0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.claveRetiro97Siefore2                = "0.0"
--        LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesAhorro73ClaveSiefore2   = 0
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoClaveSiefore2   = "0.0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoSiefores        = "0.0"
        ----------------------------------------------------------------------------------------
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda92Aivs                       = v_vivienda92_aivs * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda97Aivs                       = v_vivienda97_aivs * (-1)
        --LET ns1notificacionCuentaClabeRequest.cuerpo.fechaValorViviendaMovimientoContable = v_f_valor USING "yyyy-mm-dd"||" 09:00:00.00000"
        LET ns1notificacionCuentaClabeRequest.cuerpo.fechaValorViviendaMovimientoContable = v_f_valor USING "yyyy-mm-dd"||"T09:00:00.000Z"  
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda92                           = v_vivienda92_pesos * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.vivienda97                           = v_vivienda97_pesos * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.otrosImportesVivienda                = "0"
        LET ns1notificacionCuentaClabeRequest.cuerpo.importeNetoDepositadoVivienda        = (v_vivienda92_pesos + v_vivienda97_pesos) * (-1)
        LET ns1notificacionCuentaClabeRequest.cuerpo.diagnosticoRecepcion                 = v_diagnostico
        LET ns1notificacionCuentaClabeRequest.cuerpo.comentarios                          = ""
        LET ns1notificacionCuentaClabeRequest.cuerpo.fechaPago                            = v_f_pago CLIPPED 
        LET ns1notificacionCuentaClabeRequest.cuerpo.referenciaPago                       = v_rsp_referencia CLIPPED 
        LET ns1notificacionCuentaClabeRequest.cuerpo.observaciones                        = ""
        -- Limpia el arreglo de respuesta

        INITIALIZE ns1notificacionCuentaClabeResponse TO NULL
        INITIALIZE v_rec_notifica TO NULL
        INITIALIZE wsError TO NULL
        -- se ejecuta el WS
        CALL notificacionCuentaClabe_g() RETURNING v_resultado
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "########################################################################"
        DISPLAY " EL resultado de la Notificación "
        DISPLAY "   "
        DISPLAY "   "
        DISPLAY "Resultado de la ejecucion  :", v_resultado                                              ,":"
        DISPLAY "CODE                       :", wsError.code                                             ,":"
        DISPLAY "CODENS                     :", wsError.codeNS                                           ,":"
        DISPLAY "DESCRIPTION                :", wsError.description                                      ,":"
        DISPLAY "ACTION                     :", wsError.action                                           ,":"
        IF v_resultado = 0 THEN 
--        IF ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta = 'OK' THEN 
           DISPLAY "Codigo Respuesta           :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta   ,":"
           DISPLAY "Codigo Respuesta Oper      :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuestaOpr,":"
           DISPLAY "Codigo Operacion           :", ns1notificacionCuentaClabeResponse.ssnrop.codoper        ,":"
           DISPLAY "Codigo Oper Cliente        :", ns1notificacionCuentaClabeResponse.ssnrop.codoperCliente ,":"
           DISPLAY "Desc Respuesta             :", ns1notificacionCuentaClabeResponse.ssnrop.descRespuesta  ,":"
           DISPLAY "Fecha                      :", ns1notificacionCuentaClabeResponse.ssnrop.fecha          ,":"
           DISPLAY "motivoRechazo              :", ns1notificacionCuentaClabeResponse.objetoRespuesta.motivoRechazo ,":"
           DISPLAY "Descripcion diagnostico    :", ns1notificacionCuentaClabeResponse.objetoRespuesta.descripcionDiagnostico ,":"
           DISPLAY "Folio Notificacion         :", ns1notificacionCuentaClabeResponse.objetoRespuesta.folioNotificacion ,":"
           DISPLAY "NSS                        :", ns1notificacionCuentaClabeResponse.objetoRespuesta.nss ,":"
           DISPLAY "Resultado operacion        :", ns1notificacionCuentaClabeResponse.objetoRespuesta.resultadoOperacion ,":"
           DISPLAY "Motivos-base               :", ns1notificacionCuentaClabeResponse.ssnrop.motivos.motivo[1].base ,":"
           DISPLAY "Motivos-descripcion        :", ns1notificacionCuentaClabeResponse.ssnrop.motivos.motivo[1].descripcion ,":"
           DISPLAY "Motivos-id Motivo          :", ns1notificacionCuentaClabeResponse.ssnrop.motivos.motivo[1].idMotivo ,":"
           DISPLAY "   "
           DISPLAY "   "
           DISPLAY "########################################################################"
           INITIALIZE v_rec_notifica TO NULL
           LET v_rec_notifica.id_solicitud  = v_solicitud_notificacion.id_solicitud
           LET v_rec_notifica.f_notifica    = CURRENT YEAR TO SECOND
           LET v_rec_notifica.estado_pago   = ns1notificacionCuentaClabeResponse.objetoRespuesta.motivoRechazo
           LET v_rec_notifica.diag_notifica = ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta
           LET v_rec_notifica.folio_notificacion = ns1notificacionCuentaClabeResponse.objetoRespuesta.folioNotificacion
           LET v_rec_notifica.indicador_beneficiario = ns1notificacionCuentaClabeResponse.objetoRespuesta.indicadorBeneficiario
           LET v_rec_notifica.entidad_federativa     = ns1notificacionCuentaClabeResponse.objetoRespuesta.entidadFederativa
           LET v_rec_notifica.nss                    = ns1notificacionCuentaClabeResponse.objetoRespuesta.nss
           LET v_rec_notifica.rfc_trabajador         = ns1notificacionCuentaClabeResponse.objetoRespuesta.rfcTrabajador
           LET v_rec_notifica.curp_trabajador        = ns1notificacionCuentaClabeResponse.objetoRespuesta.curpTrabajador
           LET v_rec_notifica.cta_clabe              = ns1notificacionCuentaClabeResponse.objetoRespuesta.clabe
           LET v_rec_notifica.grupo_trabajador       = ns1notificacionCuentaClabeResponse.objetoRespuesta.grupoTrabajador
           LET v_rec_notifica.sec_pension            = ns1notificacionCuentaClabeResponse.objetoRespuesta.secuenciaPension
           LET v_rec_notifica.regimen                = ns1notificacionCuentaClabeResponse.objetoRespuesta.regimen
           LET v_rec_notifica.tpo_retiro             = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoRetiro
           LET v_rec_notifica.tpo_seguro             = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoSeguro
           LET v_rec_notifica.tpo_pension            = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoPension
           LET v_rec_notifica.tpo_prestacion         = ns1notificacionCuentaClabeResponse.objetoRespuesta.tipoPrestacion
           LET v_rec_notifica.semanas_cotizadas      = ns1notificacionCuentaClabeResponse.objetoRespuesta.semanasCotizadas
           LET v_rec_notifica.nombre_beneficiario    = ns1notificacionCuentaClabeResponse.objetoRespuesta.nombreBeneficiario
           LET v_rec_notifica.paterno_beneficiario   = ns1notificacionCuentaClabeResponse.objetoRespuesta.apellidoPaternoBeneficiario
           LET v_rec_notifica.materno_beneficiario   = ns1notificacionCuentaClabeResponse.objetoRespuesta.apellidoMaternoBeneficiario
           LET v_rec_notifica.rfc_benficiario        = ns1notificacionCuentaClabeResponse.objetoRespuesta.rfcBeneficiario
           LET v_rec_notifica.curp_beneficiario      = ns1notificacionCuentaClabeResponse.objetoRespuesta.curpBeneficiario
           LET v_rec_notifica.aivs_viv92             = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda92Aivs
           LET v_rec_notifica.aivs_viv97             = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda97Aivs
           LET v_rec_notifica.f_valor_viv            = ns1notificacionCuentaClabeResponse.objetoRespuesta.fechaValorViviendaMovimientoContable
           LET v_rec_notifica.imp_viv92              = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda92
           LET v_rec_notifica.imp_viv97              = ns1notificacionCuentaClabeResponse.objetoRespuesta.vivienda97
           LET v_rec_notifica.otros_imp_vivienda     = ns1notificacionCuentaClabeResponse.objetoRespuesta.otrosImportesVivienda
           LET v_rec_notifica.imp_neto_dep_vivienda  = ns1notificacionCuentaClabeResponse.objetoRespuesta.importeNetoDepositadoVivienda
           LET v_rec_notifica.comentarios            = ns1notificacionCuentaClabeResponse.objetoRespuesta.comentarios
           LET v_rec_notifica.f_pago                 = ns1notificacionCuentaClabeResponse.objetoRespuesta.fechaPago
           LET v_rec_notifica.referencia_pago        = ns1notificacionCuentaClabeResponse.objetoRespuesta.referenciaPago
           LET v_rec_notifica.observaciones          = ns1notificacionCuentaClabeResponse.objetoRespuesta.observaciones
           
           EXECUTE exe_inserta_documento USING
              v_rec_notifica.id_solicitud           ,v_rec_notifica.f_notifica             ,v_rec_notifica.estado_pago            ,
              v_rec_notifica.diag_notifica          ,v_rec_notifica.folio_notificacion     ,v_rec_notifica.indicador_beneficiario ,
              v_rec_notifica.entidad_federativa     ,v_rec_notifica.nss                    ,v_rec_notifica.rfc_trabajador         ,
              v_rec_notifica.curp_trabajador        ,v_rec_notifica.cta_clabe              ,v_rec_notifica.grupo_trabajador       ,
              v_rec_notifica.sec_pension            ,v_rec_notifica.regimen                ,v_rec_notifica.tpo_retiro             ,
              v_rec_notifica.tpo_seguro             ,v_rec_notifica.tpo_pension            ,v_rec_notifica.tpo_prestacion         ,
              v_rec_notifica.semanas_cotizadas      ,v_rec_notifica.nombre_beneficiario    ,v_rec_notifica.paterno_beneficiario   ,
              v_rec_notifica.materno_beneficiario   ,v_rec_notifica.rfc_benficiario        ,v_rec_notifica.curp_beneficiario      ,
              v_rec_notifica.aivs_viv92             ,v_rec_notifica.aivs_viv97             ,v_rec_notifica.f_valor_viv            ,
              v_rec_notifica.imp_viv92              ,v_rec_notifica.imp_viv97              ,v_rec_notifica.otros_imp_vivienda     ,
              v_rec_notifica.imp_neto_dep_vivienda  ,v_rec_notifica.comentarios            ,v_rec_notifica.f_pago                 ,
              v_rec_notifica.referencia_pago        ,v_rec_notifica.observaciones          
        ELSE 
           DISPLAY "Codigo Respuesta           :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta   ,":"
           DISPLAY "Codigo Respuesta Oper      :", ns1notificacionCuentaClabeResponse.ssnrop.codRespuestaOpr,":"
           DISPLAY "Codigo Operacion           :", ns1notificacionCuentaClabeResponse.ssnrop.codoper        ,":"
           DISPLAY "Codigo Oper Cliente        :", ns1notificacionCuentaClabeResponse.ssnrop.codoperCliente ,":"
           DISPLAY "Desc Respuesta             :", ns1notificacionCuentaClabeResponse.ssnrop.descRespuesta  ,":"
           DISPLAY "Fecha                      :", ns1notificacionCuentaClabeResponse.ssnrop.fecha          ,":"
        END IF 
        LET  v_respuesta = ns1notificacionCuentaClabeResponse.objetoRespuesta.motivoRechazo

 
        -- Se actualizan los estados de solicitud
--        LET  v_string = base.StringBuffer.create()
--        CALL v_string.append(ns1notificacionCuentaClabeResponse.ssnrop.codRespuesta)
--        CALL v_string.replace(" ","",0)

--        LET v_respuesta = v_string.toString()
        
        
        IF v_resultado = 0 AND (v_respuesta = "101" OR v_respuesta = "203") THEN


            IF v_solicitud_notificacion.estado_solicitud = 72 OR 
               v_solicitud_notificacion.estado_solicitud = 720 THEN
                LET v_estado_solicitud_sig = 73
            END IF

            IF v_solicitud_notificacion.estado_solicitud = 210 THEN
                LET v_estado_solicitud_sig = 214
            END IF
            IF v_solicitud_notificacion.estado_solicitud = 71 OR 
               v_solicitud_notificacion.estado_solicitud = 210 THEN 
                --- Para los casos de Ventanilla Afore se deben desmarcar las cuentas
                CALL fn_ret_generico_desmarca_cuenta(v_id_derechohabiente, 
                                                     v_marca,
                                                     v_solicitud_notificacion.id_solicitud, 
                                                     v_marca,
                                                     'safreviv', 
                                                     g_proceso_cod_ret_ley73_ws)
            END IF 

            EXECUTE prp_actualiza_benef_juridico_generico USING v_estado_solicitud_sig,v_solicitud_notificacion.id_solicitud
            -- Las solicitudes parcialmente pagadas se actualizan con el estado 730
            IF v_solicitud_notificacion.estado_solicitud = 720  THEN 
               LET v_estado_solicitud_sig = 730
            END IF 

            EXECUTE prp_actualiza_ley73        USING v_estado_solicitud_sig,v_solicitud_notificacion.id_solicitud
            EXECUTE prp_actualiza_sol_generico USING v_estado_solicitud_sig,v_solicitud_notificacion.id_solicitud

            -- Se cuenta el numero de solicitudes actualizadas
            DISPLAY "Actualizada a ",v_estado_solicitud_sig
            LET v_solicitudes_informadas = v_solicitudes_informadas + 1

        ELSE
            DISPLAY "No actualizada"
            LET v_solicitudes_no_informadas = v_solicitudes_no_informadas + 1
        END IF

        -- Se cuenta el numero de solicitudes procesadas
        LET v_solicitudes_procesadas = v_solicitudes_procesadas + 1

    END FOREACH
    
    DISPLAY ""
    DISPLAY ""
    DISPLAY "               SOLICITUDES"
    DISPLAY "----------------------------------------------"
    DISPLAY "PROCESADAS:            ",v_solicitudes_procesadas
    DISPLAY "ACTUALIZADAS:          ",v_solicitudes_informadas
    DISPLAY "NO ACTUALIZADAS:       ",v_solicitudes_no_informadas
    DISPLAY "----------------------------------------------"

    RETURN r_estado

END FUNCTION