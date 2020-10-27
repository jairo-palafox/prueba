-- funciones, variales y constantes globales del modulo de retiros
DATABASE safre_viv
GLOBALS
   CONSTANT  g_proceso_cod_ret_solo_infonavit                    SMALLINT = 1501, -- retiro solo_infonavit
             g_proceso_cod_ret_disposicion                       SMALLINT = 1502, -- retiro por disposicion de recursos
             g_proceso_cod_ret_fondo_ahorro                      SMALLINT = 1503, -- retiro fondo ahorro
             g_proceso_cod_ret_transferencia                     SMALLINT = 1504, -- retiro por transferencia
             g_proceso_cod_ret_tipo_N                            SMALLINT = 1505, -- retiro tipo N
             g_proceso_cod_ret_ley73_ws                          SMALLINT = 1506, -- retiro LEY 73 Web services
             g_proceso_cod_ret_fortal_credito                    SMALLINT = 1507, -- retiro por fortalecimiento al credito
             g_proceso_cod_con_ret_solo_infonavit                SMALLINT = 1508, -- retiro solo_infonavit consulta fico
             g_proceso_cod_ret_carga_spes                        SMALLINT = 1509, -- carga del spes/datamart
             g_proceso_cod_ret_fondo_ahorro_arch                 SMALLINT = 1515, -- retiro fondo ahorro arch
             g_proceso_cod_ret_fondo_ahorro_comb                 SMALLINT = 1515, -- retiro fondo ahorro archivo combinado PRODINF354
             g_proceso_cod_ret_Ley73_arch                        SMALLINT = 1516, -- retiro ley73 contingente
             g_proceso_cod_ret_solo_inf_arch                     SMALLINT = 1517, -- retiro contingente de solo infonavit
             g_proceso_cod_ret_fondo_ahorro_aj_manual            SMALLINT = 1518, -- retiro contingente de solo infonavit
             g_proceso_cod_ret_disposicion_pmg                   SMALLINT = 1519, -- retiro por disposicion de recursos pmg
             g_proceso_cod_ret_carga_his_ley73                   SMALLINT = 1520, -- historico transferencias ret ley73 Anexo1
             g_proceso_cod_ret_webservice                        SMALLINT = 1521, -- retiro por webservice
             g_proceso_cod_ret_aport_voluntarias                 SMALLINT = 1527, -- retiro de aportaciones voluntarias
             g_proceso_cod_ret_aport_voluntarias_restitucion     SMALLINT = 1528, -- restitucion de retiro de aportaciones voluntarias
             g_proceso_cod_ret_amort_excedentes                  SMALLINT = 1530, -- Retiro de amortizaciones excedentes
             g_proceso_cod_ret_envio_fico                        SMALLINT = 1531, -- generacion de archivo para creacion de cuentas por pagar enviado a FICO
             g_proceso_cod_consulta_pago_fico_ret_generico       SMALLINT = 1532, -- consulta de pagos por FICO de retiro generico
             g_proceso_archivos_respuesta_fico                   SMALLINT = 1533, -- recepcion de respuesta de FICO de creacion de cuentas por pagar
             g_proceso_cod_restitucion_ret_generico_72           SMALLINT = 1534, -- restitucion montos no pagados fondo72 rechazados por BANCO
             g_proceso_cod_restitucion_ret_generico_ley73        SMALLINT = 1535, -- restitucion montos no pagados ley 72 rechazados por banco
             g_proceso_cod_restitucion_ret_generico_amortexc     SMALLINT = 1536, -- restitucion retiro genérico amortización excedente
             g_proceso_cod_restitucion_ret_generico_volunarias   SMALLINT = 1537, -- restitucion retiro genérico voluntarias
             g_proceso_cod_cancelacion_cxp_fico                  SMALLINT = 1538, -- generacion de archivo de cancelacion de cuentas por pagar en FICO
             g_proceso_cod_solicitud_anulacion                   SMALLINT = 1539, -- solicitud de anulación de 
             g_proceso_cod_restitucion_rechazo_fico              SMALLINT = 1540, -- restitucion retiro generico de rechazos fico
             g_proceso_cod_ret_generico_notifica_adai            SMALLINT = 1541, -- notificacion a ADAI
             g_proceso_cod_restitucion_ret_generico_72fico       SMALLINT = 1542, -- restitucion montos no pagados fondo72 rechazados por FICO
             g_proceso_cod_restitucion_ret_generico_ley73fico    SMALLINT = 1543, -- restitución de retiro genérico Ley 73 rechazo por FICO 
             g_proceso_cod_ret_archivo_fico_DAP                  SMALLINT = 1544, -- creacion de archivo de salida para FICO pago por DAP
             g_proceso_cod_ret_resp_archiv_fico_DAP              SMALLINT = 1545, -- Integracion de la respuesta de archivo para FICO pago por DAP
             g_proceso_cod_ret_generico_notifica_procesar        SMALLINT = 1546, -- notificacion a PROCESAR
             g_proceso_cod_ret_archivo_SIAF                      SMALLINT = 1547, -- generación de archivo para SIAF
             g_proceso_cod_tramite_juridico_desmarca             SMALLINT = 1548, -- generación de archivo para SIAF
             g_proceso_cod_ley73_anexo1_cargo_historico          SMALLINT = 1549, -- cargos historicos de Anexo 1 Ley73
             g_proceso_cod_not_daps_vencidos                     SMALLINT = 1550, -- notificacion daps vencidos solo infonavit
             g_proceso_canint                                    SMALLINT = 1552, -- carga de los registros del archivo CANINT
             g_proceso_disp_bp                                   SMALLINT = 1553, -- carga registros de rechazos BP disposicion
             g_proceso_disp_but                                  SMALLINT = 1554, -- carga registros de rechazos BUT disposicion
             g_proceso_pmg_bp                                    SMALLINT = 1555, -- carga registros de rechazos BP pmg
             g_proceso_pmg_but                                   SMALLINT = 1556, -- carga registros de rechazos BUT pmg
             g_proceso_tran_bp                                   SMALLINT = 1557, -- carga registros de rechazos BP transferencia
             g_proceso_tran_but                                  SMALLINT = 1558, -- carga registros de rechazos BUT transferencia
             g_proceso_n_bp                                      SMALLINT = 1559, -- carga registros de rechazos BP tipo N
             g_proceso_n_but                                     SMALLINT = 1560, -- carga registros de rechazos BUT tipo N
             g_proceso_aclara_fa                                 SMALLINT = 1561, -- carga registros de aclaraciones del Fondo de Ahorro
             g_proceso_marca_z                                   SMALLINT = 1562, -- carga registros para marca Z
             g_proceso_consulta_historico_anexo_1                SMALLINT = 1567, -- Consulta Historico Anexo 1
             g_proceso_carga_cuentas_clabe                       SMALLINT = 1568, -- Carga registros con cuentas Clabe
             g_proceso_cargos_tesofe_resp_siaff                  SMALLINT = 1569, -- proceso para realizar cargos a la subcuenta 47, mediante un archivo con layout de la respuesta SIAFF             
             g_proceso_carga_SIAF                                SMALLINT = 1570, -- Carga respuesta SIAFF para actualizar estado de solicitud
             g_proceso_restitucion_rechazo_siaff                 SMALLINT = 1571, -- Restitucion de los rechazos de la respuesta SIAFF
             g_proceso_notificacion_datamart_historico           SMALLINT = 1572, -- Notificacion por SMS o correo de retiro ley 73 de acuerdo a ret_datamart (barrido historico)
             g_proceso_notificacion_datamart_diario              SMALLINT = 1573, -- Notificacion por SMS o correo de retiro ley 73 de acuerdo a ret_datamart (de acuerdo al folio generado)
             g_archivo_solicitud_marca_procesar                  SMALLINT = 1574, -- Consulta de solicitudes marcadas ley 73 grupo 1
             g_archivo_solicitud_desmarca_procesar               SMALLINT = 1575, -- Consulta de solicitudes desmarcadas ley 73 grupo 1
             g_reporte_marcas_desmarcas_notificacion             SMALLINT = 1576, -- Proceso generado para visualizar archivos generados (PRODINF-845)
             g_proceso_carga_multinss                            SMALLINT = 1577, -- Proceso de carga por multinss
             g_notificar_procesar_ley73_grupo1                   SMALLINT = 1578, -- Proceso de notificacion a procesar Ley 73 Grupo 1
             g_proceso_marca_embargo_todas                       SMALLINT = 1579, -- Proceso marcas de embargo (0027,0028,0016,0026,0036)
             g_marca_procesar_indicador_pendiente                SMALLINT = 1580, -- Proceso automatico de marca a PROCESAR de solicitudes con indicador pendiente
             g_desmarca_procesar_indicador_pendient              SMALLINT = 1581, -- Proceso automatico de desmarca a PROCESAR de solicitudes con indicador pendiente
             g_desmarca_procesar_solicitudes_vencidas            SMALLINT = 1582, -- Proceso automatico de desmarca a PROCESAR de solicitudes con indicador pendiente
             g_consulta_procesar_indicador_pendient              SMALLINT = 1583, -- Proceso automatico de desmarca a PROCESAR de solicitudes con indicador pendiente
             g_notificacion_ley73_contigente                     SMALLINT = 1584, -- Proceso de notificacion lanzado posterior a la liquidacion del Contingente de Ley 73
             g_proceso_ley73_anexo_1_contracargos                SMALLINT = 1585, -- Proceso que aplica movimientos contrarios a la carga del anexo 1 por duplicidad de movimientos
             g_proceso_ley73_cargo_ssv_siaff                     SMALLINT = 1586, -- Proceso que aplica movimientos contrarios a la carga del anexo 1 por duplicidad de movimientos
             g_proceso_det_cargo_ssv_consulta                    SMALLINT = 1587, -- Proceso con el que se carga el archivo para consulta de detalle de los cargos SSV
             g_proceso_det_dif_ssv_pensionado                    SMALLINT = 1588, -- Proceso de Diferencias SSV Pensionados
             g_proceso_det_revolvente_siaff                      SMALLINT = 1589, -- Proceso de Indicadores Revolvente SIAFF
             g_proceso_excep_devol_ssv                           SMALLINT = 1590, -- Excepciones de la Devolución del SSV
             g_proceso_excep_devol_ssv_resp_fico                 SMALLINT = 1591, -- Respuesta FICO a las solicitudes de pago de las Excepciones de la Devolucion del SSV
             g_proceso_excep_devol_ssv_notifica                  SMALLINT = 1592, -- Notificación de Pago FICO de las Excepciones de la Devolucion del SSV
             g_proceso_excep_devol_ssv_restitucion               SMALLINT = 1593, -- Restitución de las Excepciones de la Devolucion del SSV
             g_proceso_oficios_rojos                             SMALLINT = 1594, -- Oficios Rojos
             g_proceso_cod_ret_fondo_ahorro_trasp                SMALLINT = 1595, -- Proceso Contingente Traspasos Fondo de Ahorro
             g_proceso_cod_ret_rest_pago_vencido_fondo_ahorro    SMALLINT = 1596, -- Proceso de Restitucion de Pagos Vencidos del Fondo de Ahorro
             g_proceso_cod_ret_notifica_grupo                    SMALLINT = 1598, -- Generación de Notificación de pago de Grupos 2, 3 y 4
             g_proceso_cod_ret_reps_notifica_grupo               SMALLINT = 1599, -- Recepción de Notificación de pago de Grupos 2, 3 y 4
             g_proceso_cod_ret_cruce_fico                        SMALLINT = 1601, -- retiro por disposicion de recursos pmg 
             g_proceso_cod_extraccion                            SMALLINT = 1602, -- extracción de información para ambientar QA.RET259
             g_proceso_extractor_amortizaciones                  SMALLINT = 2602, -- generación de extractor de solicitudes de amortizaciones excedentes
             g_proceso_extractor_amortizaciones_pendientes       SMALLINT = 2603, -- generación de extractor de solicitudes de amortizaciones excedentes pendientes de pago
             g_proceso_pago_fondo_ahorro_masivo                  SMALLINT = 2604, -- generación de extractor de pagos del Fondo de Ahorro Masivo
             g_proceso_marca_embargo                             SMALLINT = 2605, -- carga de registros con marca 0028 y 0029 marca embargo
             g_proceso_cod_ret_envio_fico_ssv                    SMALLINT = 2606, -- generacion de archivo para creacion de cuentas por pagar enviado a FICO de la devolución del saldo de vivienda
             g_proceso_cod_consulta_pago_fico_ssv                SMALLINT = 2607, -- consulta de pagos por FICO de la devolución del saldo de vivienda
             g_proceso_archivos_respuesta_fico_ssv               SMALLINT = 2608, -- recepcion de respuesta de FICO de creacion de cuentas por pagar de la devolución del saldo de vivienda
             g_proceso_cod_ret_notifica_beneficiarios            SMALLINT = 2610, -- Motificación de pagos a Jurídico de los Beneficiario DSSV
             g_proceso_cod_ret_envio_fico_fa                     SMALLINT = 2611, -- Generacion de archivo para creacion de cuentas por pagar enciada a FICO del fonfo de Ahorro
             g_proceso_cod_consulta_pago_fico_fa                 SMALLINT = 2612, -- consulta de pagos por FICO del Fondo de Ahorro
             g_proceso_archivos_respuesta_fico_fa                SMALLINT = 2613, -- recepcion de respuesta de FICO de creacion de cuentas por pagar del Fondo de Ahorro
             g_proceso_cod_notifica_pago_crm                     SMALLINT = 2614, -- Notificación de pago a CRM
             g_proceso_cod_aport_fondo_ahorro                    SMALLINT = 2615, -- Aportaciones por pantalla del Fondo de Ahorro
             g_proceso_cod_envio_correo_fondo_ahorro             SMALLINT = 2616, -- Envío de correos de las cartas de negativa del fondo de ahorro
             g_proceso_cod_envio_correo_cartas_tableta           SMALLINT = 2617, -- Envío de las cartas de aceptación de los retiros tramitados por tableta
             g_proceso_cod_historico_fondo_ahorro                SMALLINT = 2618, -- Carga del archivo histórico de movimientos del fondo de ahorro
             g_proceso_cod_extractor_pagos                       SMALLINT = 2619, -- Extractor de información del Saldo de la subcuenta de vivienda
             g_proceso_cod_extractor_detalle_pagos               SMALLINT = 2620, -- Extractor de información del Saldo de la subcuenta de vivienda detallada
             g_proceso_cod_fondo_ahorro_masivo                   SMALLINT = 2621 -- Carga del Archivo para pago Masivo del Fondo de Ahorro

             -- etapas de los procesos
   CONSTANT  g_opera_cod_ret_disp_carga                          SMALLINT = 1, -- carga de archivo de retiro por disposicion de recursos
             g_opera_cod_ret_disp_integracion                    SMALLINT = 2, -- integracion de retiro por disposicion de recursos
             g_opera_cod_ret_disp_preliquidacion                 SMALLINT = 3, -- preliquidacion de retiro por disposicion de recursos
             g_opera_cod_ret_disp_liquidacion                    SMALLINT = 4, -- liquidacion de retiro por disposicion de recursos
             g_opera_cod_ret_disp_salida_tesoreria               SMALLINT = 5, -- emision del archivo de salida para Tesoreria
             g_opera_extractor_amortizaciones                    SMALLINT = 1, -- operación del archivo de solicitudes de amortizaciones excedentes
             g_opera_extractor_amortizaciones_pendientes         SMALLINT = 1, -- operación del archivo de solicitudes de amortizaciones excedentes pendientes
             g_opera_pago_fondo_ahorro_masivo                    SMALLINT = 1, -- operación del archivo de extractor de pagos del Fondo de Ahorro Masivo

             -- etapas del proceso marca embargo
             g_opera_marca_embargo_carga                         SMALLINT = 1,  -- operación de carga del archivo de marca embargo
             g_opera_marca_embargo_integracion                   SMALLINT = 2,  -- operación de integracion del archivo de marca de embargo

             -- etapas del proceso retiros por transferencia
             g_opera_cod_ret_transf_carga                        SMALLINT = 1, -- carga de archivo de retiro por transferencia
             g_opera_cod_ret_transf_integracion                  SMALLINT = 2, -- integracion de retiro por transferencia
             g_opera_cod_ret_transf_preliquidacion               SMALLINT = 3, -- preliquidacion de retiro por transferencia
             g_opera_cod_ret_transf_liquidacion                  SMALLINT = 4, -- liquidacion de retiro por transferencia
             g_opera_cod_ret_transf_salida_tesoreria             SMALLINT = 5, -- emision del archivo de salida para Tesoreria
             
             -- etapas del proceso retiros por tipo N
             g_opera_cod_ret_tipoN_carga                         SMALLINT = 1, -- carga de archivo de retiro tipo N
             g_opera_cod_ret_tipoN_integracion                   SMALLINT = 2, -- integracion de retiro tipo N
             g_opera_cod_ret_tipoN_preliquidacion                SMALLINT = 3, -- preliquidacion de retiro tipo N
             g_opera_cod_ret_tipoN_liquidacion                   SMALLINT = 4, -- liquidacion de retiro tipo N
             g_opera_cod_ret_tipoN_salida_tesoreria              SMALLINT = 5,  -- emision del archivo de salida para Tesoreria

             -- etapas del proceso retiros por fortalecimiento al credito (FC)
             g_opera_cod_ret_FC_carga                            SMALLINT = 1, -- carga de archivo de retiro FC
             g_opera_cod_ret_FC_integracion                      SMALLINT = 2, -- integracion de retiro FC
             g_opera_cod_ret_FC_preliquidacion                   SMALLINT = 3, -- preliquidacion de retiro FC
             g_opera_cod_ret_FC_liquidacion                      SMALLINT = 4, -- liquidacion de retiro FC
             g_opera_cod_ret_FC_salida_tesoreria                 SMALLINT = 5,  -- emision del archivo de salida para Tesoreria
             
             -- etapas del proceso retiros por solo infonavit
             g_opera_cod_ret_soloInfo_preliquidacion             SMALLINT = 1, -- preliquidacion de retiro solo infonavit
             g_opera_cod_ret_soloInfo_liquidacion                SMALLINT = 2, -- liquidacion de retiro solo infonavit
             
             -- etapas del proceso retiros por solo infonavit consulta fico
             g_opera_cod_ret_soloInfo_salida_tesoreria           SMALLINT = 1, -- emision del archivo de salida para Tesoreria
             
             -- etapas del proceso retiros por fondo ahorro
             g_opera_cod_ret_fondoAho_preliquidacion             SMALLINT = 1, -- preliquidacion de retiro fondo ahorro
             g_opera_cod_ret_fondoAho_liquidacion                SMALLINT = 2, -- liquidacion de retiro fondo ahorro
             g_opera_cod_ret_fondoAho_salida_tesoreria           SMALLINT = 3, -- emision del archivo de salida para Tesoreria

             -- etapas del retiro ley 73 por webservices
             g_opera_cod_ret_ley73_ws_preliquidacion             SMALLINT = 1, -- preliqudiacion de retiro ley 73 ws
             g_opera_cod_ret_ley73_ws_liquidacion                SMALLINT = 2, -- liqudiacion de retiro ley 73 ws

             -- etapas del proceso retiros por trámite judicial
             g_opera_cod_ret_tramJud_preliquidacion              SMALLINT = 1, -- preliquidacion de retiro por trámite judicial
             g_opera_cod_ret_tramJud_liquidacion                 SMALLINT = 2, -- liquidacion de retiro por trámite judicial
             g_opera_cod_ret_tramJud_salida_tesoreria            SMALLINT = 3, -- emision del archivo de salida para Tesoreria

             -- etapas del proceso retiros por trámite judicial contingencia
             g_opera_cod_ret_ley73_carga                         SMALLINT = 1, -- carga de archivo de retiro de retiro por ley73
             g_opera_cod_ret_ley73_integracion                   SMALLINT = 2, -- integracion de retiro de retiro por ley73
             g_opera_cod_ret_ley73_preliquidacion                SMALLINT = 3, -- preliquidacion de retiro por ley73
             g_opera_cod_ret_ley73_liquidacion                   SMALLINT = 4, -- liquidacion de retiro por ley73

             -- etapas del proceso retiros contingente de solo infonavit
             g_opera_cod_ret_conting_solo_inf_carga              SMALLINT = 1, -- carga de archivo de retiro de retiro contingente solo inf
             g_opera_cod_ret_conting_solo_inf_integracion        SMALLINT = 2, -- integracion de retiro de retiro contingente solo inf
             g_opera_cod_ret_conting_solo_inf_preliquidacion     SMALLINT = 3, -- preliquidacion de retiro contingente solo inf
             g_opera_cod_ret_conting_solo_inf_liquidacion        SMALLINT = 4, -- liquidacion de retiro contingente solo inf
          
             -- etapas de la carga inicial del spes/datamart
             g_opera_cod_ret_carga_spes_carga                    SMALLINT = 1, -- operacion de carga de la carga del spes
             g_opera_cod_ret_integra_spes_carga                  SMALLINT = 2, -- operacion de integracion de la carga de spes

             --fondo ahorro por contingencia
             g_opera_cod_ret_fondoahorro_carga                   SMALLINT = 1, -- carga de archivo de retiro por disposicion de recursos
             g_opera_cod_ret_fondoahorro_carga_comb              SMALLINT = 1, -- carga de archivo de retiro de fondo de ahorro conting combinado prodinf-354
             g_opera_cod_ret_fondoahorro_integracion             SMALLINT = 2, -- integracion de retiro por disposicion de recursos
             g_opera_cod_ret_fondoahorro_integracion_comb        SMALLINT = 2, -- integracion de retiro por disposicion de recursos combinado #PRODINF-354
             g_opera_cod_ret_fondoahorro_preliquidacion          SMALLINT = 3, -- preliquidacion de retiro por disposicion de recursos
             g_opera_cod_ret_fondoahorro_preliquidacion_comb     SMALLINT = 3, -- preliquidacion de retiro por disposicion de recursos combinado #PRODINF-354
             g_opera_cod_ret_fondoahorro_liquidacion             SMALLINT = 4, -- liquidacion de retiro por disposicion de recursos
             g_opera_cod_ret_fondoahorro_liquidacion_comb        SMALLINT = 4, -- liquidacion de retiro por disposicion de recursos combinado #PRODINF-354

             --ajuste de fondo ahorro por contingencia manual 
             g_opera_cod_ret_fondoAho_manual_preliquidacion      SMALLINT = 1, -- preliquidacion de retiro fondo ahorro
             g_opera_cod_ret_fondoAho_manual_liquidacion         SMALLINT = 2, -- liquidacion de retiro fondo ahorro

             g_opera_cod_ret_pmg_carga                           SMALLINT = 1, -- carga de archivo de retiro por disposicion de recursos
             g_opera_cod_ret_pmg_integracion                     SMALLINT = 2, -- integracion de retiro por disposicion de recursos
             g_opera_cod_ret_pmg_preliquidacion                  SMALLINT = 3, -- preliquidacion de retiro por disposicion de recursos
             g_opera_cod_ret_pmg_liquidacion                     SMALLINT = 4, -- liquidacion de retiro por disposicion de recursos
             g_opera_cod_ret_pmg_salida_tesoreria                SMALLINT = 5, -- emision del archivo de salida para Tesoreria

             --Solo carga para cruce fico-saci
             g_opera_cod_ret_cruce_carga                         SMALLINT = 1, -- carga de archivo de fico para cruce con saci
             
			 g_opera_cod_ret_Extraccion                             SMALLINT = 1, -- extraccion de información de PROD para ambientar QA
             -- carga historica de transferencias de retiro ley 73
             -- etapas de la carga inicial del spes/datamart
             g_opera_cod_ret_his_ley73_carga                     SMALLINT = 1, -- operacion de carga de historico de ley 73
             g_opera_cod_ret_his_ley73_integracion               SMALLINT = 2, -- operacion de integracion de historico de ley73
             g_opera_cod_ret_his_ley73_preliquidacion            SMALLINT = 3, -- preliquidacion de historico de ley73
             g_opera_cod_ret_his_ley73_liquidacion               SMALLINT = 4, -- liquidacion de historico de ley73
             
             -- retiro por webservices con TRM
             g_opera_cod_ret_ws_preliquidacion                   SMALLINT = 1, -- preliquidacion de retiros por ws
             g_opera_cod_ret_ws_liquidacion                      SMALLINT = 2, -- liquidacion de retiros por ws

             -- retiro por webservices de aportaciones voluntarias
             g_opera_cod_ret_av_preliquidacion                   SMALLINT = 1, -- preliquidacion de retiros aportaciones voluntarias
             g_opera_cod_ret_av_liquidacion                      SMALLINT = 2, -- liquidacion de retiros aportaciones voluntarias
             g_opera_cod_ret_av_notifica_sap_fico                SMALLINT = 3,  -- notificacion a SAP-FICO de retiros realizados
             
             --Retiro de amortizaciones excedentes
             g_opera_cod_ret_amort_ex_preliquidacion             SMALLINT = 1,
             g_opera_cod_ret_amort_ex_liquidacion                SMALLINT = 2,
             
             --Envío de archivos a FICO 
             g_opera_cod_envia_archivos_fico                     SMALLINT = 1,
  
             -- restitucion de retiros aportaciones voluntarias no pagadas
             g_opera_cod_ret_av_restitucion_preliquidacion       SMALLINT = 1,  -- restitucion de montos no pagados preliquidacion
             g_opera_cod_ret_av_restitucion_liquidacion          SMALLINT = 2,  -- restitucion de montos no pagados liquidacion
             
             -- consulta de solicitudes pagadas a FICO
             g_opera_cod_ret_gen_consulta_pagos_fico             SMALLINT = 1, -- consulta de que solicitudes han sido pagadas en FICO
             
             -- operación de validación de archivo de respuesta FICO
             g_opera_cod_valida_respuesta_fico                   SMALLINT = 1,
             g_opera_cod_integra_respuesta_fico                  SMALLINT = 2,
             
             -- restitucion de retiros genericas
             g_opera_cod_restitucion_ret_generico_preliquidacion SMALLINT = 1,  -- restitucion de montos no pagados preliquidacion
             g_opera_cod_restitucion_ret_generico_liquidacion    SMALLINT = 2,   -- restitucion de montos no pagados liquidacion
             
             -- generacion de archivo de cancelacion de cuentas por pagar en fico
             g_opera_cod_archivo_cancelacion_cxp_fico            SMALLINT = 1, -- generacion del archivo de salida de cancelacion de cuentas por pagar en FICO
             
             -- carga e integración de la solicitud de anulación
             g_opera_cod_valida_solicitud_anulacion              SMALLINT = 1,
             g_opera_cod_integra_solicitud_anulacion             SMALLINT = 2,
             
             -- preliquidacion y liquidacion de rechazo fico
             g_opera_cod_valida_rechazo_fico                     SMALLINT = 1,
             g_opera_cod_liquida_rechazo_fico                    SMALLINT = 2,
                                                                  
             -- notificacion de solicitudes aceptadas y rechazadas a ADAI
             g_opera_cod_notifica_adai_ws                        SMALLINT = 1, -- ejecucion de ws de notificacion a ADAI

             -- notificacion de solicitudes aceptadas y rechazadas a PROCESAR
             g_opera_cod_notifica_procesar_ws                    SMALLINT = 1, -- ejecucion de ws de notificacion a PROCESAR
             
             -- Archivo de pago por DAP para FICO
             g_opera_cod_genera_archivo_dap                      SMALLINT = 1,

             -- operación de validación de archivo de respuesta FICO pago por DAP
             g_opera_cod_valida_respuesta_fico_dap               SMALLINT = 1,
             g_opera_cod_integra_respuesta_fico_dap              SMALLINT = 2,
             
             -- generación de archivo para SIAF
             g_opera_cod_genera_archivo_SIAF                     SMALLINT = 1,
             
             -- carga de archivo Ley 73 Anexo 1 Cargos historicos
             g_opera_cod_ret_ley73_a1ch_carga                    SMALLINT = 1, -- carga de archivo de Ley73 Anexo 1 Cargo Historico
             g_opera_cod_ret_ley73_a1ch_integracion              SMALLINT = 2, -- integracion Ley73 Anexo 1 Cargo Historico
             g_opera_cod_ret_ley73_a1ch_preliquidacion           SMALLINT = 3, -- preliquidacion Ley73 Anexo 1 Cargo Historico
             g_opera_cod_ret_ley73_a1ch_liquidacion              SMALLINT = 4, -- liquidacion Ley73 Anexo 1 Cargo Historico

             -- carga de archivo Notificacion de DAPS vencidos
             g_opera_cod_not_dap_vencidos_carga                  SMALLINT = 1, -- carga de archivo de notificacion de DAPS vencidos
             g_opera_cod_not_dap_vencidos_integracion            SMALLINT = 2, -- integracion notificacion de DAPS vencidos
             g_opera_cod_not_dap_vencidos_preliquidacion         SMALLINT = 3, -- preliquidacion notificacion de DAPS vencidos
             g_opera_cod_not_dap_vencidos_liquidacion            SMALLINT = 4,  -- liquidacion notificacion de DAPS vencidos

             -- operación de validación e integración del archivo CANINT
             g_opera_cod_valida_canint                           SMALLINT = 1,
             g_opera_cod_integra_canint                          SMALLINT = 2,

             -- operación de validación e integración de rechazos Disposicion BP
             g_opera_cod_valida_disp_bp                          SMALLINT = 1,
             g_opera_cod_integra_disp_bp                         SMALLINT = 2,

             -- operación de validación e integración de rechazos Disposicion BUT
             g_opera_cod_valida_disp_but                         SMALLINT = 1,
             g_opera_cod_integra_disp_but                        SMALLINT = 2,

             -- operación de validación e integración de rechazos PMG BP
             g_opera_cod_valida_pmg_bp                           SMALLINT = 1,
             g_opera_cod_integra_pmg_bp                          SMALLINT = 2,

             -- operación de validación e integración de rechazos PMG BUT
             g_opera_cod_valida_pmg_but                          SMALLINT = 1,
             g_opera_cod_integra_pmg_but                         SMALLINT = 2,

             -- operación de validación e integración de rechazos Transferencia BP
             g_opera_cod_valida_tran_bp                          SMALLINT = 1,
             g_opera_cod_integra_tran_bp                         SMALLINT = 2,

             -- operación de validación e integración de rechazos Transferencia BUT
             g_opera_cod_valida_tran_but                         SMALLINT = 1,
             g_opera_cod_integra_tran_but                        SMALLINT = 2,

             -- operación de validación e integración de rechazos Tipo N BP
             g_opera_cod_valida_n_bp                             SMALLINT = 1,
             g_opera_cod_integra_n_bp                            SMALLINT = 2,

             -- operación de validación e integración de rechazos Tipo N BUT
             g_opera_cod_valida_n_but                            SMALLINT = 1,
             g_opera_cod_integra_n_but                           SMALLINT = 2,

             -- operación de Aclaraciones del Fondo de Ahorro
             g_opera_cod_valida_aclara_fa                        SMALLINT = 1,
             g_opera_cod_integra_aclara_fa                       SMALLINT = 2,
             g_opera_cod_preliquida_aclara_fa                    SMALLINT = 3,
             g_opera_cod_liquida_aclara_fa                       SMALLINT = 4,

             g_opera_cod_valida_marca_z                          SMALLINT = 1,
             g_opera_cod_integra_marca_z                         SMALLINT = 2,

             -- etapas del proceso Carga de cuentas clabe
             g_opera_cod_carga_cuentas_clabe_carga               SMALLINT = 1,  -- carga de archivo
             g_opera_cod_carga_cuentas_clabe_integracion         SMALLINT = 2,  -- integracion de archivo

             -- etapas del proceso Carga de cuentas clabe
             g_opera_cod_consulta_historico_anexo_1_carga        SMALLINT = 1,  -- carga de archivo
             g_opera_cod_consulta_historico_anexo_1_integracion  SMALLINT = 2,  -- integracion de archivo

             -- operación de carga historica de SIAFF
             g_opera_cod_valida_cargos_tesofe_resp_siaff         SMALLINT = 1,
             g_opera_cod_integra_cargos_tesofe_resp_siaff        SMALLINT = 2,
             g_opera_cod_preliquida_cargos_tesofe_resp_siaff     SMALLINT = 3,
             g_opera_cod_liquida_cargos_tesofe_resp_siaff        SMALLINT = 4,

             -- operación de restitucion de rechazos de la respuesta SIAFF
             g_opera_cod_preliquida_restitucion_rechazo_siaff    SMALLINT = 1,
             g_opera_cod_liquida_restitucion_rechazo_siaff       SMALLINT = 2,

             -- generación de archivo para SIAF
             g_opera_cod_carga_valida_SIAF                       SMALLINT = 1,
             g_opera_cod_carga_integra_SIAF                      SMALLINT = 2,

             -- Notifiacion historica
             g_opera_cod_validacion_datamart_historico           SMALLINT = 1,

             -- Notificacion diaria
             g_opera_cod_validacion_datamart_diario              SMALLINT = 1,

             -- Consulta solicitudes marcadas procesar
             g_archivo_solicitud_marca_procesar_carga            SMALLINT = 1,
             g_archivo_solicitud_marca_procesar_integracion      SMALLINT = 2,

             -- Consulta solicitudes desmarcadas procesar
             g_archivo_solicitud_desmarca_procesar_carga         SMALLINT = 1,
             g_archivo_solicitud_desmarca_procesar_integracion   SMALLINT = 2,

             -- Carga de archivo por MultiNSS
             g_opera_cod_multiNSS_valida                         SMALLINT = 1,
             g_opera_cod_multiNSS_integra                        SMALLINT = 2,

             -- Notificacion a procesar de ley 73 grupo 1
             g_notificar_procesar_ley73_grupo1_notificacion      SMALLINT = 1,

             -- Marcas de embargo (0027,0028,0016,0026,0036)
             g_opera_marca_embargo_todas_carga                   SMALLINT = 1,
             g_opera_marca_embargo_todas_integracion             SMALLINT = 2,

             -- Marca a procesar, indicador pendiente
             g_opera_marca_procesar_indicador_pendiente          SMALLINT = 1,

             -- Desmarca a procesar, indicador pendiente
             g_opera_desmarca_procesar_indicador_pendiente       SMALLINT = 1,

             -- Desmarca de las solicitudes con estado de captura por mas de 30 dias
             g_opera_desmarca_procesar_solicitudes_vencidas      SMALLINT = 1,

             -- Consulta a procesar, indicador pendiente
             g_opera_consulta_procesar_indicador_pendiente       SMALLINT = 1,

             -- Operacion de validacion de solicitudes de ley 73 contingente a ser notificadas posterior a la liquidacion
             g_opera_notificacion_ley73_contigente               SMALLINT = 1,

             -- Operacion de aplicacion de movimientos contrarios al anexo 1 por duplicidad de movimientos
             g_opera_ley73_anexo_1_contracargos_carga            SMALLINT = 1,
             g_opera_ley73_anexo_1_contracargos_integra          SMALLINT = 2,
             g_opera_ley73_anexo_1_contracargos_preliquida       SMALLINT = 3,
             g_opera_ley73_anexo_1_contracargos_liquida          SMALLINT = 4,
             
             -- Operacion de aplicacion de movimientos de cargo a la SSV realizados por la plataforma SIAFF
             g_opera_ley73_cargo_ssv_siaff_carga                 SMALLINT = 1,
             g_opera_ley73_cargo_ssv_siaff_integra               SMALLINT = 2,
             g_opera_ley73_cargo_ssv_siaff_preliquida            SMALLINT = 3,
             g_opera_ley73_cargo_ssv_siaff_liquida               SMALLINT = 4,

             -- Operacion de aplicacion de movimientos de cargo a la SSV realizados por la plataforma SIAFF
             g_opera_det_cargo_ssv_consulta_carga                 SMALLINT = 1,
             g_opera_det_cargo_ssv_consulta_integra               SMALLINT = 2,

             -- Operacion de aplicacion de Diferencias SSV Pensionados
             g_opera_dif_ssv_pensionados_genera                  SMALLINT = 1,
             g_opera_dif_ssv_pensionados_carga                   SMALLINT = 2,
             g_opera_dif_ssv_pensionados_integra                 SMALLINT = 3,
             g_opera_dif_ssv_pensionados_respuesta               SMALLINT = 4,

             -- Operacion de indicadores revolvente siaff
             g_opera_revolvente_siaff_carga                      SMALLINT = 1,
             g_opera_revolvente_siaff_integra                    SMALLINT = 2,

             -- Operacion de Excepciones de la Devolución del SSV
             g_opera_excep_devol_ssv_carga                       SMALLINT = 1,
             g_opera_excep_devol_ssv_integra                     SMALLINT = 2,
             g_opera_excep_devol_ssv_preliquida                  SMALLINT = 3,
             g_opera_excep_devol_ssv_liquida                     SMALLINT = 4,
             g_opera_excep_devol_ssv_respuesta                   SMALLINT = 5,

             -- Operacion de Respuesta de las Excepciones de la Devolución del SSV
             g_opera_resp_excep_devol_ssv_carga                  SMALLINT = 1,
             g_opera_resp_excep_devol_ssv_integra                SMALLINT = 2,

             -- Operacion de Notificación de las Excepciones de la Devolución del SSV
             g_opera_notif_excep_devol_ssv_consulta              SMALLINT = 1,
             
             -- Operacion de Restitución de las Excepciones de la Devolución del SSV
             g_opera_restitucion_excep_devol_ssv_preliquida      SMALLINT = 1,
             g_opera_restitucion_excep_devol_ssv_liquida         SMALLINT = 2,

             -- Operacion de Restitución de las Excepciones de la Devolución del SSV
             g_opera_oficios_rojos_carga                         SMALLINT = 1,
             g_opera_oficios_rojos_integra                       SMALLINT = 2,

             -- Operaciones de Contingente Traspasos Fondo de Ahorro
             g_opera_fondo_ahorro_trasp_carga                    SMALLINT = 1,
             g_opera_fondo_ahorro_trasp_integra                  SMALLINT = 2,
             g_opera_fondo_ahorro_trasp_preliquida               SMALLINT = 3,
             g_opera_fondo_ahorro_trasp_liquida                  SMALLINT = 4,

             g_opera_rest_pago_vencido_fondo_ahorro_carga        SMALLINT = 1,
             g_opera_rest_pago_vencido_fondo_ahorro_integra      SMALLINT = 2,
             g_opera_rest_pago_vencido_fondo_ahorro_preliquida   SMALLINT = 3,
             g_opera_rest_pago_vencido_fondo_ahorro_liquida      SMALLINT = 4,

             g_opera_ret_notifica_grupo_genera                   SMALLINT = 1,

             g_opera_ret_resp_notifica_grupo_carga               SMALLINT = 1,
             g_opera_ret_resp_notifica_grupo_integra             SMALLINT = 2,

             g_opera_ret_notifica_beneficiarios                  SMALLINT = 1,

             g_opera_ret_aport_fondo_ahorro                      SMALLINT = 1,

             g_opera_ret_primera_operacion                       SMALLINT = 1,

             g_opera_extractor_pago                              SMALLINT = 1,

             g_opera_extractor_detale_pago                       SMALLINT = 1,

             g_opera_fondo_ahorro_masivo                         SMALLINT = 1


             
             -- tolerancia maxima para formulario de captura de montos notificados (en pesos)
   CONSTANT  g_tolerancia_max_disposicion                                 = 1,
             g_tolerancia_max_pmg                                         = 1,
             g_tolerancia_max_transferencia                               = 1,
             g_tolerancia_max_tipo_n                                      = 1   

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- CONSTANTES PARA RETIRO GENERICO

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


-- =======================================================
-- constantes para los estados de la solicitud y para los codigos de rechazo de los webservices del retiro generico
CONSTANT  gi_solicitud_aceptada                                  SMALLINT = 10 ,
          gi_solicitud_aceptada_amort_excedentes                 SMALLINT = 15  ,
          gi_solicitud_rechazada                                 SMALLINT = 100,
          -- usados en servicio de marca/desmarca
          gi_estatus_marca_existoso                              SMALLINT = 1,
          gi_estatus_marca_no_exitoso                            SMALLINT = 2
          
-- codigos de rechazo de los webservices
CONSTANT  gi_datos_incompletos                                   SMALLINT =  99, -- no se recibieron los datos necesarios para ejecutar el WS
          gi_nss_es_necesario                                    SMALLINT =  98, -- el nss es necesario para el proceso en turno
          gi_nss_rfc_no_existe                                   SMALLINT = 999, -- no existe el NSS/RFC
          gi_modalidad_invalida                                  SMALLINT = 101, -- la modalidad de retiro es invalida
          gi_causal_retiro_invalido                              SMALLINT =  77, -- la causal de retiro de Fondo de ahorro es invalida
          gi_solicitud_en_tramite                                SMALLINT =  97, -- ya existe una solicitud en tramite
          gi_tiene_credito_vigente                               SMALLINT =  20, -- tiene un credito vigente
          gi_sin_saldo                                           SMALLINT =  10, -- no tiene saldo
          gi_sin_un_ano_relacion_laboral                         SMALLINT =  50, -- no tiene un ano sin relacion laboral
          gi_nrp_no_encontrado                                   SMALLINT =  53, -- numero de registro patronal no existe en el catalogo
          gi_no_existe_rel_laboral                               SMALLINT =  48, -- no existe relacion laboral
          gi_con_rel_laboral_actual                              SMALLINT =  47, -- actualmente cuenta con relacion laboral
          gi_edad_inferior_50_anos                               SMALLINT =  40, -- la edad es inferior a 50 anos
          gi_sin_pension_vigente                                 SMALLINT =  91, -- no tiene pension vigente en el SPESS
          gi_sin_resolucion_spess                                SMALLINT =  90, -- no tiene resolucion en el SPESS
          gi_resolucion_neg_pension                              SMALLINT =  92, -- trabajador con resolucion de negativa de pension
          gi_mas_de_un_registro                                  SMALLINT = 100, -- se encontro mas de un registro para el NSS/RFC dado (FONDO DE AHORRO)
          gi_no_hay_precio_fondo                                 SMALLINT = 200, -- no existe precio de fondo para la fecha de valuacion
          gi_no_disponible_para_retiro                           SMALLINT = 218, -- El saldo no esta disponible para ser retirado por regla del instituto
          gi_indicador_marca_invalido                            SMALLINT = 102, -- valor enviado como indicador de marca invalido
          gi_error_interno_marca                                 SMALLINT = 103, -- error interno al intentar marcar/desmarcar la cuenta
          gi_error_marca_generar_solicitud                       SMALLINT = 104, -- error al generar la solicitud en WS de marca
          gi_error_marca_no_existe_solicitud                     SMALLINT = 105, -- no existe una cuenta para desmarcar
          gi_porcentaje_menor_50                                 SMALLINT = 140, -- porcentaje de pension menor al 50 %
          gi_fecha_inicio_pension_invalida_l73                   SMALLINT = 141, -- fecha de inicio de pension invalida para ley 73
          gi_regimen_diferente_73                                SMALLINT = 145, -- Regimen de resolucion diferente a 73
          gi_subcuenta_invalida                                  SMALLINT = 150, -- Subcuenta invalida para modalidad de retiro (ley 73)
          gi_solicitud_sin_beneficiarios                         SMALLINT = 300, -- la solicitud no tiene beneficiarios
          gi_f_pension_mayor_f_ult_rel_laboral                   SMALLINT = 400, -- la fecha de pension es posterior a la de la ultima relacion laboral
          gi_con_prescripcion                                    SMALLINT = 500, -- el trabajador ha prescrito
          gi_no_existe_solicitud                                 SMALLINT = 600, -- no existe una solicitud con los datos enviados
          gi_solicitud_en_edo_rechazo                            SMALLINT = 650, -- la solicitud esta en estado de rechazo
          gi_solicitud_en_edo_no_reconocido                      SMALLINT = 651, -- la solicitud tiene un estado no recnocido
          gi_esctructura_cta_clabe_incorrecta                    SMALLINT = 652, -- la estructura de la cuenta clabe es incorrecta
          gi_error_marca_no_convive                              SMALLINT = 130, -- la marca no convive
          gi_fecha_resolucion_invalida_l73                       SMALLINT = 700, -- fecha de resolucion invalida para ley 73
          gi_no_corresponde_a_nuevo_pensionado                   SMALLINT = 700, -- No corresponde a un nuevo pensionado. (Se solicitó cambiarlo en el req SACI2018-155)
          gi_fallo_consulta_saldo_afore                          SMALLINT = 127, -- Fallo la consulta al servicio de saldo de Procesar
		    gi_monto_excedido_pago_dap                             SMALLINT = 701, -- el monto excede el maximo permitido por pago DAP
		    gi_modalidad_multiple_sin_CLABE                        SMALLINT = 702, -- se tiene un retiro con modalidad multiple y alguna de estas no tiene cuenta CLABE
		    gi_ws_rel_laboral_no_disponible                        SMALLINT = 703, -- se detecto un error al consultar el WS de ultima relacion laboral
          
          gi_cargo_juridico                                      SMALLINT = 593, -- Tiene la marca 593, Cargo jurídico por instrucción judicial
          gi_pagado_por_cargo_juridico                           SMALLINT = 594, -- Tiene la marca 594, Pagado por cargo jurídico
          gi_pagado_laudo_tramitado                              SMALLINT = 595, -- Tiene la marca 595, Pagado por laudo ya tramitado
          gi_pagado_amparo_tramitado                             SMALLINT = 596, -- Tiene la marca 596, Pagado por amparo ya tramitado
          gi_rechazo_banco_siaff                                 SMALLINT = 604, -- Cuando la respuesta del SIAFF haya tenido un rechazo
          gi_pendiente_envio_clabe                               SMALLINT = 605, -- Pendiente de envio a SIAFF por no contar con cuenta CLABE
          gi_cuenta_marcada_laudo                                SMALLINT = 810, -- La cuenta se encuentra marcada en por Laudo o proceso Jurídico
          gi_cuenta_con_marca_admiva                             SMALLINT = 815, -- La cuenta se encuentra maracada por un proceso administrativo
          gi_cuenta_en_proceso_de_retiro                         SMALLINT = 820, -- La cuenta se encuentra en proceso de retiro
          gi_cuenta_con_marca_procesar                           SMALLINT = 825, -- La cuenta se encuenta marcada por otro proceso en Procesar

          gi_inhabil_por_unificacion_imss                        SMALLINT = 2830, -- Marca 150, la cuenta se encuentra marcada por Unificación IMSS
          gi_inhabil_por_unif_infonavit                          SMALLINT = 831, -- Marca 151, la cuenta se encuentra marcada por Unificación INFONAVIT
          gi_retiro_solo_infonavit                               SMALLINT = 832, -- Marca 801, la cuenta se encuentra en proceso de Retiro Solo Infonavit
          gi_retiro_ley73                                        SMALLINT = 833, -- Marca 803, la cuenta se encuentra en proceso de Retiro Ley 73
          gi_retiro_ley73_va                                     SMALLINT = 834, -- Marca 815, la cuenta se encuentra en proceso de Retiro Ley 73 Ventanilla Afore
          gi_retiro_disposicion                                  SMALLINT = 835, -- Marca 805, la cuenta se encuentra en proceso de Retiro por Disposición de Recursos
          gi_retiro_pmg                                          SMALLINT = 2836, -- Marca 808, la cuenta se encuentra en proceso de Retiro por Disposición de Recursos PMG
          gi_retiro_transferencia                                SMALLINT = 2837, -- Marca 806, la cuenta se encuentra en proceso de Retiro por Transferencia de Recursos 
          gi_retiro_fortalecimiento                              SMALLINT = 838, -- Marca 807, la cuenta se encuentra en proceso de Retiro por Fortalecimiento al Crédito
          gi_unif_imss_unificador                                SMALLINT = 839, -- Marca 501, la cuenta se encuentra en proceso de Unificación IMSS Unificador
          gi_unif_imss_unificado                                 SMALLINT = 840, -- Marca 502, la cuenta se encuentra en proceso de Unificación IMSS Unificado
          gi_sep_ctas_invadido                                   SMALLINT = 841, -- Marca 701, la cuenta se encuentra en proceso de Separación de cuentas Invadido
          gi_sep_ctas_asociado                                   SMALLINT = 842, -- Marca 702, la cuenta se encuentra en proceso de Separación de cuentas Asociado
          gi_sep_ctas                                            SMALLINT = 843, -- Marca 280, la cuenta se encuentra en proceso de Separación de cuentas          

          gi_101_102	                                            SMALLINT = 999, -- Codigo 101 102 NSS no se encuentra registrado en la BDNSAR
          gi_101_201	                                            SMALLINT = 22,  -- La cuenta se encuentra en proceso de 43 BIS
          gi_101_202                                             SMALLINT = 820, -- En proceso de Traspaso A-A
          gi_101_203                                             SMALLINT = 597, -- En proceso de Unificación de cuentas
          gi_101_204                                             SMALLINT = 821, -- En proceso de Fusión de Afore
          gi_101_205                                             SMALLINT = 201, -- En proceso de separación de cuentas
          gi_101_207                                             SMALLINT = 822, -- En proceso de transferencias de recursos
          gi_101_208                                             SMALLINT = 823, -- En proceso de disposición de recursos
          gi_101_211                                             SMALLINT = 824, -- En proceso de Devolución de pagos efectuados sin Justificación Legal
          gi_101_212                                             SMALLINT = 825, -- En proceso de retiros parciales
          gi_101_213                                             SMALLINT = 826, -- En proceso de tramite judicial iniciado por Afore
          gi_101_216                                             SMALLINT = 827, -- Cuenta en proceso de aclaración por conciliación
          gi_101_217                                             SMALLINT = 828, -- Cuenta en proceso de selección SIEFORE
          gi_101_219                                             SMALLINT = 829, -- Cuenta en proceso de modificación
          gi_101_220                                             SMALLINT = 836, -- Cuenta en proceso de crédito de vivienda
          gi_101_221                                             SMALLINT = 837, -- Cuenta en proceso de crédito de vivienda 43BIS
          gi_101_223                                             SMALLINT = 830, -- Cuenta en proceso de saldo previo
          gi_101_224                                             SMALLINT = 832, -- No se encuentra marcado
          gi_101_225                                             SMALLINT = 833, -- Existe alguna notificación de pago por Ventanilla INFONAVIT
          gi_101_226                                             SMALLINT = 834, -- Existe alguna notificación de pago por Ventanilla INFONAVIT (ESTE LO ENVIA LA AF0RE)
          gi_103_0 	                                            SMALLINT = 835, -- No se encuentra marcado (ESTE LO ENVIA LA AF0RE)
          
          -- estados para la restitucion
          gi_estado_restitucion_rechazo                          SMALLINT = 90,  -- restitución de rechazos
          gi_estado_restitucion_ret_generico                     SMALLINT = 91,  -- restitucion de solicitudes de retiros
          gi_estado_preliq_restitucion_ret_generico              SMALLINT = 209, -- preliquidado restitucion de solicitudes de retiros

          -- Estados envío a SIAF
          gi_estado_enviada_tesofe                               SMALLINT = 69,    --Estado enviada correctamente a TESOFE
          gi_estado_pendiente_envio                              SMALLINT = 61,    --Estado pendiente de envio por inconsistencia de información del SIAF

          -- Codigos de rechazo de SIAF
          gi_cod_rech_605                                        SMALLINT = 605,   --Código de rechazo Pendiente de Envío a SIAFF por no contar con cuenta CLABE
          gi_cod_rech_606                                        SMALLINT = 606,   --Código de rechazo Pendiente de Envío a SIAFF por cuenta CLABE duplicada
          gi_cod_rech_607                                        SMALLINT = 607,   --Código de rechazo Pendiente de Envío a SIAFF por NSS duplicado
          gi_no_es_grupo_1                                       SMALLINT = 2001,  -- La solicitud no es grupo 1
          gi_ws_busca_caso_crm_no_disponible                     SMALLINT = 2002, -- Error en la consulta del servicio de busqueda de caso CRM
          gi_ws_cancela_caso_crm_no_disponible                   SMALLINT = 2003, -- El servicio de cancela caso CRM no esta disponible
          gi_ws_crea_caso_crm_no_disponible                      SMALLINT = 2004, -- El servicio de crea caso CRM no esta disponible
          gi_ws_confirma_pago_crm_no_disponible                  SMALLINT = 2005, -- El servicio de confirmación de pago a CRM no esta disponible
          gi_benef_sin_tipo_beneficiario                         SMALLINT = 2006, -- En los beneficiario falta el tipo de beneficiario
          gi_benef_sin_rfc                                       SMALLINT = 2007, -- En los beneficiario falta el RFC
          gi_benef_sin_email                                     SMALLINT = 2008, -- En los beneficiario falta el email
          gi_benef_sin_nombre                                    SMALLINT = 2009, -- En los beneficiario falta el Nombre
          gi_benef_sin_paterno                                   SMALLINT = 2010, -- En los beneficiario falta el Apellido Paterno
          gi_benef_sin_entidad_federativa                        SMALLINT = 2011, -- En los beneficiario falta la entidad federativa
          gi_benef_porcentaje_no_suma_cien                       SMALLINT = 2012, -- La suma de los porcentajes no es 100

          --- NO CONSIDERAR EL 2013 SE ESTA USANDO PARA LA DESMARCA MASIVA DE CUADRE CON PROCESAR 
          gi_dssv_pagada_ant                                     SMALLINT = 2014, -- Código de Rechazo para la Cancelación de Solicitudes 
          gi_nss_duplicado                                       SMALLINT = 2015, -- Código de Rechazo para la Cancelación de Solicitudes 
          gi_cr_con_deudor_abierto                               SMALLINT = 2016, -- Código de Rechazo para la Cancelación de Solicitudes 
          gi_cuenta_clabe_duplicada                              SMALLINT = 2017, -- La cuenta clabe se está utilizando en otro NSS
          gi_rfc_obligatorio                                     SMALLINT = 782  -- El RFC es obligatorio, aplica para el Fondo de Ahorro

-- constantes de los archivos para y desde FICO
CONSTANT  gi_tipo_archivo_envio                                  SMALLINT = 1, -- archivo de salida
          gi_tipo_archivo_respuesta                              SMALLINT = 2  -- archivo de respuesta

-- constantes para ventanilla afore e infonavit
CONSTANT gi_ventanilla_infonavit                                 SMALLINT = 101, -- solicitud iniciada en infonavit
         gi_ventanilla_afore                                     SMALLINT = 201  -- solicitud iniciada en afore
END GLOBALS
