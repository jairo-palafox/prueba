###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
------------------------------------------------------------------------------#
#Modulo            => ret                                                     #
#Programa          => RETG04                                                  #
#Objetivo          => Constantes globales para retiros por webservices        #
#Fecha Inicio      => 06-Jun-2012                                             #
###############################################################################

GLOBALS

  --CONSTANT c1 SMALLINT = 12000
  -- =======================================================
  -- =======================================================
  -- CONSTANTES PARA RETIRO LEY 73
  
  -- codigos de retorno 
    CONSTANT g_ret_ley73_cod_retorno_solicitud_aprobada                  SMALLINT = 5 
    CONSTANT g_ret_ley73_cod_retorno_sin_saldo_de_vivienda               SMALLINT = 19
    CONSTANT g_ret_ley73_cod_retorno_nss_no_existe                       SMALLINT = 23
    CONSTANT g_ret_ley73_cod_retorno_existe_solicitud_previa             SMALLINT = 30
    CONSTANT g_ret_ley73_cod_retorno_existe_tramite_judicial             SMALLINT = 63
    CONSTANT g_ret_ley73_cod_retorno_nss_con_credito_vigente             SMALLINT = 81
    CONSTANT g_ret_ley73_cod_retorno_sin_resolucion_en_spess             SMALLINT = 82
    CONSTANT g_ret_ley73_cod_retorno_f_resolucion_ant_13ene2012          SMALLINT = 83 -- fecha de resolucion es anterior al 13 de enero de 2012
    CONSTANT g_ret_ley73_cod_retorno_todos_los_campos_son_obligatorios   SMALLINT = 99

    CONSTANT g_url_disp_cod_ret_22                                       CHAR(2) = "22" 
    CONSTANT g_url_disp_cod_ret_23                                       CHAR(2) = "23" 
    CONSTANT g_url_disp_cod_ret_40                                       CHAR(2) = "40" 
    CONSTANT g_url_disp_cod_ret_41                                       CHAR(2) = "41" 
    CONSTANT g_url_disp_cod_ret_58                                       CHAR(2) = "58" 
    CONSTANT g_url_disp_cod_ret_59                                       CHAR(2) = "59" 
    CONSTANT g_url_disp_cod_ret_60                                       CHAR(2) = "60" 
    CONSTANT g_url_disp_cod_ret_61                                       CHAR(2) = "61" 
    CONSTANT g_url_disp_cod_ret_62                                       CHAR(2) = "62" 
    CONSTANT g_url_disp_cod_ret_63                                       CHAR(2) = "63" 
    CONSTANT g_url_disp_cod_ret_64                                       CHAR(2) = "64" 
    CONSTANT g_url_disp_cod_ret_65                                       CHAR(2) = "65" 
    CONSTANT g_url_disp_cod_ret_73                                       CHAR(2) = "73" 
    CONSTANT g_url_disp_cod_ret_74                                       CHAR(2) = "74" 
    CONSTANT g_url_disp_cod_ret_75                                       CHAR(2) = "75" 
    CONSTANT g_url_disp_cod_ret_76                                       CHAR(2) = "76" 
    CONSTANT g_url_disp_cod_ret_77                                       CHAR(2) = "77" 
    CONSTANT g_url_disp_cod_ret_78                                       CHAR(2) = "78" 
    CONSTANT g_url_disp_cod_ret_80                                       CHAR(2) = "80" 
    CONSTANT g_url_disp_cod_ret_81                                       CHAR(2) = "81" 
    CONSTANT g_url_disp_cod_ret_82                                       CHAR(2) = "82" 
    CONSTANT g_url_disp_cod_ret_83                                       CHAR(2) = "83" 
    CONSTANT g_url_disp_cod_ret_84                                       CHAR(2) = "84" 
    CONSTANT g_url_disp_cod_ret_85                                       CHAR(2) = "85" 
    CONSTANT g_url_disp_cod_ret_88                                       CHAR(2) = "88" 
    CONSTANT g_url_disp_cod_ret_90                                       CHAR(2) = "90"
    CONSTANT g_url_disp_cod_ret_91                                       CHAR(2) = "91"
    CONSTANT g_url_disp_cod_ret_92                                       CHAR(2) = "92"
    CONSTANT g_url_disp_cod_ret_99                                       CHAR(2) = "99" 

  
    CONSTANT g_ret_ley73_cnt_Solicitud_Portal                      CHAR(2) = "10" -- "0010"
    CONSTANT g_ret_ley73_cnt_En_Proceso                            CHAR(2) = "11" -- "0011"
    CONSTANT g_ret_ley73_cnt_Autorizado                            CHAR(2) = "12" -- "0012"
    CONSTANT g_ret_ley73_cnt_Pago_en_Tramite                       CHAR(2) = "13" -- "0013"
    CONSTANT g_ret_ley73_cnt_Pago_confirmado_Banco                 CHAR(2) = "14" -- "0014"
    CONSTANT g_ret_ley73_cnt_Rechazo_por_Banco                     CHAR(2) = "15" -- "0015"
    CONSTANT g_ret_ley73_cnt_Pagado_Laudo_tramitado_anteriormente  CHAR(2) = "16" -- "0016"
    CONSTANT g_ret_ley73_cnt_Confirmado_Banco_Enviado_ADAI         CHAR(2) = "17" -- "0017"
    CONSTANT g_ret_ley73_cnt_Rechazado_Banco_Enviado_ADAI          CHAR(2) = "18" -- "0018"
    CONSTANT g_ret_ley73_cnt_Confirmado_Banco_Enviar_ADAI          CHAR(2) = "19" -- "0019"
    CONSTANT g_ret_ley73_cnt_RzdoxBanco_Enviar_ADAI                CHAR(2) = "20" -- "0020"
    CONSTANT g_ret_ley73_cnt_Pagado_y_Confirmado                   CHAR(2) = "21" -- "0021"
    CONSTANT g_ret_ley73_cnt_RechazadozdoxBanco_AFORE              CHAR(2) = "22" -- "0022"
    CONSTANT g_ret_ley73_cnt_DevolucionBanco_Cerrado_ADAI          CHAR(2) = "23" -- "0023"
    CONSTANT g_ret_ley73_cnt_DevolucionBanco_Cerrar_ADAI           CHAR(2) = "24" -- "0024"
    CONSTANT g_ret_ley73_cnt_DevolucionBanco                       CHAR(2) = "25" -- "0025"
    CONSTANT g_ret_ley73_cnt_Pagado_Amparo_tramitado_anteriormente CHAR(2) = "26" -- "0026"
    CONSTANT g_ret_ley73_cnt_Ya_existe_Disposicion_Recursos        CHAR(2) = "36" -- "0036"
    CONSTANT g_ret_ley73_cnt_Pendiente_enviar_SAP_FICO             CHAR(2) = "40" -- "0040"
    CONSTANT g_ret_ley73_cnt_En_tremite_Homonimia                  CHAR(2) = "50" -- "0050"


    CONSTANT g_url_disp_desc_cod_ret_22                            CHAR(100) = "El dato que escribiste no corresponde con un NSS"
    CONSTANT g_url_disp_desc_cod_ret_23                            CHAR(100) = "El dato que escribiste no corresponde con un NSS"
    CONSTANT g_url_disp_desc_cod_ret_40                            CHAR(100) = "El pago se hará cuando concluya el proceso de embargo"
    CONSTANT g_url_disp_desc_cod_ret_41                            CHAR(100) = "El SSV está pagado por instrucción judicial"
    CONSTANT g_url_disp_desc_cod_ret_58                            CHAR(100) = "La solicitud de devolución del SSV está en proceso; el depósito se hará en la cuenta proporcionada"
    CONSTANT g_url_disp_desc_cod_ret_59                            CHAR(100) = "La devolución del SSV está autorizada; el depósito se hará en la cuenta proporcionada"
    CONSTANT g_url_disp_desc_cod_ret_60                            CHAR(100) = "La solicitud de devolución del SSV está en proceso; el depósito se hará en la cuenta proporcionada"
    CONSTANT g_url_disp_desc_cod_ret_61                            CHAR(100) = "El depósito del SSV está hecho y confirmado por el banco"
    CONSTANT g_url_disp_desc_cod_ret_62                            CHAR(100) = "No se pudo hacer el depósito del SSV en la cuenta bancaria registrada"
    CONSTANT g_url_disp_desc_cod_ret_63                            CHAR(100) = "El depósito del SSV está hecho y confirmado por el banco"
    CONSTANT g_url_disp_desc_cod_ret_64                            CHAR(100) = "El depósito del SSV está hecho y confirmado por el banco"
    CONSTANT g_url_disp_desc_cod_ret_65                            CHAR(100) = "El depósito del SSV está hecho y confirmado por el banco"
    CONSTANT g_url_disp_desc_cod_ret_73                            CHAR(100) = "Para iniciar el trámite es necesario concluir la aclaración de homonimia"
    CONSTANT g_url_disp_desc_cod_ret_74                            CHAR(100) = "El depósito del SSV está hecho y confirmado por el banco"
    CONSTANT g_url_disp_desc_cod_ret_75                            CHAR(100) = "No se pudo hacer el depósito del SSV en la cuenta bancaria registrada"
    CONSTANT g_url_disp_desc_cod_ret_76                            CHAR(100) = "El depósito del SSV está hecho y confirmado por el banco"
    CONSTANT g_url_disp_desc_cod_ret_77                            CHAR(100) = "No se pudo hacer el depósito del SSV en la cuenta bancaria registrada"
    CONSTANT g_url_disp_desc_cod_ret_78                            CHAR(100) = "La solicitud de devolución del SSV está en proceso"
    CONSTANT g_url_disp_desc_cod_ret_80                            CHAR(100) = "Tiene crédito vigente, no se puede solicitar la devolución Amort Excedente"
    CONSTANT g_url_disp_desc_cod_ret_81                            CHAR(100) = "Tiene crédito en trámite, no se puede solicitar la devolución del SSV"
    CONSTANT g_url_disp_desc_cod_ret_82                            CHAR(100) = "El NSS que escribiste no tiene registro de pensión"
    CONSTANT g_url_disp_desc_cod_ret_83                            CHAR(100) = "Debe solicitar la devolución del SSV en la oficinas del Infonavit"
    CONSTANT g_url_disp_desc_cod_ret_84                            CHAR(100) = "El NSS que escribiste no tiene registro de pensión"
    CONSTANT g_url_disp_desc_cod_ret_85                            CHAR(100) = "El NSS que escribiste no tiene registro de pensión"
    CONSTANT g_url_disp_desc_cod_ret_88                            CHAR(100) = "Ya se inició el trámite en la Afore, debe concluirlo aquí"
    CONSTANT g_url_disp_desc_cod_ret_90                            CHAR(100) = "La cuenta se encuentra en proceso de disposición de recursos"
    CONSTANT g_url_disp_desc_cod_ret_91                            CHAR(100) = "La cuenta se encuentra en proceso de disposición de recursos por una pension minima garantizada"
    CONSTANT g_url_disp_desc_cod_ret_92                            CHAR(100) = "La cuenta se encuentra en proceso de transferencia de recursos"
    CONSTANT g_url_disp_desc_cod_ret_99                            CHAR(100) = "Llena Formulario"

  
  -- =======================================================
END GLOBALS