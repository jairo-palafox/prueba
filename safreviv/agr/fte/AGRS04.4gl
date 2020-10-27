#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRS04                                        #
#Objetivo          => Programa que ejecuta el proceso de generación #
#                     de archivo solicitud de saldos AG             #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 10 Abril 2012                                 #
#                     3 de marzo de 2014                            #
#                     Función para obtención de movimientos         #
#                     liquidados en cta_movimiento_xx               #
#Modifica:         => José Eduardo Ventura                          #
#Fecha modif:      => 9 de julio de 2015                            #
#Adecuación        => Tomar los registros liquidados con remanente  #
#Modifica:         => Mauro Muñiz Caballero                         #
#Fecha modif:      => 9 de noviembre de 2015                        #
#Adecuación        => Eliminación de adelantos                      #
#####################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

GLOBALS

   DEFINE v_s_qryTxt             STRING -- se asigna una sentencia sql a ejecutar

   DEFINE p_v_usuario            LIKE seg_usuario.usuario                -- nombre del usuario
   DEFINE p_d_pid                LIKE bat_ctr_proceso.pid                -- pid
   DEFINE p_i_proceso_cod        LIKE cat_proceso.proceso_cod            -- código del proceso
   DEFINE p_i_opera_cod          LIKE cat_operacion.opera_cod            -- código de la operación de la etapa
   DEFINE p_d_folio              LIKE glo_ctr_archivo.folio              -- número de folio
   DEFINE p_v_arch_proceso       LIKE glo_ctr_archivo.nombre_archivo     -- nombre del archivo a integrar
   DEFINE v_i_tpo_originacion    LIKE cre_acreditado.tpo_originacion     -- tipo de originación
   DEFINE v_c_tpo_transferencia  LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia

   DEFINE v_r_cre_acred RECORD
      id_cre_acred               LIKE cre_acreditado.id_cre_acreditado, -- id cre acreditado
      id_derhabiente             LIKE cre_acreditado.id_derechohabiente, -- id derechohabiente
      num_credito                LIKE cre_acreditado.num_credito, -- número de crédito
      folio_liquida              LIKE cre_acreditado.folio_liquida, -- folio de liquidación
      estado                     LIKE cre_acreditado.estado, -- estado
      edo_procesar               LIKE cre_acreditado.edo_procesar, --estado Procesar
      sdo_deudor                 LIKE cre_acreditado.sdo_deudor -- saldo deudor
   END RECORD

   DEFINE v_r_cre_uso RECORD
      id_cre_uso                 LIKE cre_uso_garantia.id_cre_uso_garantia, -- id cre uso garantia
      folio_liquida              LIKE cre_uso_garantia.folio_liquida, -- folio de liquidación
      estado                     LIKE cre_uso_garantia.estado,        -- estado de la solicitud
      edo_procesar               LIKE cre_uso_garantia.edo_procesar, -- estado procesar
      importe_uso                DECIMAL(12,2)
   END RECORD

   DEFINE r_r_afi_derechohab RECORD -- registro de afi derechohabiente
      curp                       LIKE afi_derechohabiente.curp,
      nss                        LIKE afi_derechohabiente.nss,
      rfc                        LIKE afi_derechohabiente.rfc,
      ap_paterno_af              LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_af              LIKE afi_derechohabiente.ap_materno_af,
      nombre_af                  LIKE afi_derechohabiente.nombre_af,
      nombre_imss                LIKE afi_derechohabiente.nombre_imss
   END RECORD

   DEFINE v_r_encabezado RECORD
      tpo_registro               CHAR(2), -- Tipo de Registro (001-002)
      id_servicio                CHAR(2), -- Identificador de Servicio (003-004)
      id_operacion               CHAR(2), -- Identificador de Operación (005-006)
      tpo_ent_origen             CHAR(2), -- Tipo de entidad origen (007-008)
      cve_ent_origen             CHAR(3), -- Clave de entidad origen (009-011)
      tpo_ent_destino            CHAR(2), -- Tipo de entidad destino (012-013)
      cve_ent_destino            CHAR(3), -- Clave de entidad destino (014-016)
      ent_fed_env_lote           CHAR(3), -- Entidad federativa de envío de lote (017-019)
      fec_presentacion           CHAR(8), -- Fecha de presentación (020-027)
      consec_lote                CHAR(3), -- Consecutivo del lote en el día (028-030)
      cod_resul_opera            CHAR(2), -- Código de resultado de la Operación (033-034)
      mot_rechazo                CHAR(9), -- Motivo de rechazo del lote (035-043)
      filler                     CHAR(689)-- Filler (044-730)
   END RECORD

   DEFINE v_r_detalle RECORD
      tpo_registro               CHAR(2), -- Tipo de Registro (001-002)
      cont_servicio              CHAR(10), -- Contador de Servicio (003-012)
      tpo_ent_recept             CHAR(2), -- Tipo de entidad receptora de la cuenta (013-014)
      cve_ent_recept             CHAR(3), -- Clave de entidad receptora de la cuenta (015-017)
      tpo_ent_cedente            CHAR(2), -- Tipo de entidad cedente de la cuenta (018-019)
      cve_ent_cedente            CHAR(3), -- Clave de entidad ced. de la cuenta (020-022)
      tpo_transferencia          CHAR(2), -- Origen/Tipo de transferencia (023-024)
      f_presentacion             CHAR(8), -- Fecha de presentación (025-032)
      filler1                    CHAR(8), -- Filler1 (033-040)
      curp_trabajador            CHAR(18), -- CURP del trabajador (041-058)
      nss_trab_infonavit         CHAR(11), -- NSS del trabajador según INFONAVIT (059-069)
      filler2                    CHAR(15), -- Filler2 (070-084)
      rfc_trab_infonavit         CHAR(13), -- RFC del trabajador según INFONAVIT (085-097)
      ape_pat_infonavit          CHAR(40), -- Apellido paterno del trabajador en el INFONAVIT (098-137)
      ape_mat_infonavit          CHAR(40), -- Apellido materno del trabajador en el INFONAVIT (138-177)
      nom_trab_infonavit         CHAR(40), -- Nombres del trabajador en el INFONAVIT (178-217)
      filler3                    CHAR(22), -- Filler3 (218-239)
      id_lote_solic              CHAR(16), -- Identificador de lote de la solicitud (240-255)
      filler4                    CHAR(15), -- Filler4 (256-270)
      nss_trab_afore             CHAR(11), -- NSS del trabajador según AFORE/PS. (271-281)
      rfc_trab_afore             CHAR(13), -- RFC del trabajador según AFORE (282-294)
      filler5                    CHAR(30), -- Filler5 (295-324)
      ape_pat_afore              CHAR(40), -- Apellido paterno del trabajador en la AFORE cedente (325-364)
      ape_mat_afore              CHAR(40), -- Apellido materno del trabajador en la AFORE cedente (365-404)
      nom_trab_afore             CHAR(40), -- Nombres del trabajador en la AFORE cedente (405-444)
      filler6                    CHAR(45), -- Filler6 (445-489)
      num_apl_int_viv92          CHAR(15), -- Número de "Aplicaciones de Intereses de Vivienda" 92 de la última aportación (490-504)
      --num_apl_int_viv97          CHAR(15), -- Número de "Aplicaciones de Intereses de Vivienda" 97 de la última aportación (475-489)
      --ult_aport_vivi97           CHAR(15), -- Ultima aportación Vivienda 97 (490-504)
      filler7                    CHAR(60), -- Filler7 (505-564)
      num_apl_int_viv97          CHAR(15), -- Número de "Aplicaciones de Intereses de Vivienda" 97 de la última aportación (465-479)
      filler8                    CHAR(8), -- Filler8 (580-587)
      --cod_result_opera           CHAR(2), -- Código Resultado de la Operación (583-584)
      --diagnostico_proc           CHAR(15), -- Diagnóstico del Proceso (585-599)
      nom_trab_imss              CHAR(50), -- Nombre del Trabajador según IMSS (588-637)
      num_cred_infonavit         CHAR(10), -- Número de Crédito INFONAVIT (638-647)
      cod_result_opera           CHAR(2), -- Código Resultado de la Operación (648-649)
      diagnostico_proc           CHAR(15), -- Diagnóstico del Proceso (550-664)
      filler9                    CHAR(66) -- Filler9 (665-730)
      --periodo_pago               CHAR(6), -- Período de pago (713-718)
      --filler10                   CHAR(12) -- Filler10 (719-730)
   END RECORD

   DEFINE v_r_sumario RECORD
      tpo_registro               CHAR(2), -- tipo de registro (001-002)
      cant_regs_detalle          CHAR(9), -- cantidad de registros de detalle (003-011)
      filler1                    CHAR(45), -- filler1 (012-056)
      sum_numAplInt_viv92        CHAR(18), -- suma del número de "aplicaciones intereses de vivienda" 97 de la última aportación (057-074)
      filler2                    CHAR(60), -- filler1 (075-134)
      sum_numAplInt_viv97        CHAR(18), -- suma de la ultima aportación de vivienda 97 (135-152)
      filler3                    CHAR(578) -- filler2 (153-730)
   END RECORD

   DEFINE v_r_tmp_agr_solic_sdo RECORD
      nss                        CHAR(11),
      id_derechohabiente         DECIMAL(9,0),
      modulo_cod                 CHAR(2),
      f_proceso                  DATE,
      id_referencia              DECIMAL(9,0)
   END RECORD

   -- registro de históricos
   DEFINE v_cre_his_acreditado RECORD LIKE cre_his_acreditado.*
   DEFINE v_det_nss            CHAR(11)

   DEFINE v_d_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_d_id_derhabiente     LIKE cre_acreditado.id_derechohabiente -- id derechohabiente
   DEFINE v_d_num_credito        LIKE cre_uso_garantia.num_credito -- número de crédito
   DEFINE v_edo_procesar_aux     LIKE cre_acreditado.edo_procesar -- estado procesar a actualizar
   DEFINE v_dt_fec_present       DATE -- fecha de presentacion
   DEFINE v_c_fec_present_ax     CHAR(8) -- fecha auxiliar de presentacion con formato YYYYMMDD
   DEFINE v_dt_f_solic_saldos    DATE -- fecha para la generación de solicitud de saldos
   --DEFINE v_v_nom_archivo        LIKE cre_ctr_archivo.nom_archivo -- nombre del archivo de salida
   DEFINE v_v_ruta_nomarch       VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp    VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_c_extension          LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_ch_arch_solTransf    BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_s_registro           STRING -- registro a insertar
   DEFINE v_c_ruta_envio         LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_i_contador_reg       INTEGER -- contrador de registros
   DEFINE v_s_qryTxt             STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_s_comando            STRING -- contiene al comando a correr
   DEFINE v_r_cre_ctr_archivo    RECORD LIKE cre_ctr_archivo.* -- registro de cre ctr archivo
   DEFINE v_c_ruta_listado       LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_ruta_bin             LIKE seg_modulo.ruta_bin
   DEFINE v_manejador_rpt        OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_c_programa_cod       LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_archivo_nom          STRING
   DEFINE v_d_sumtot_aivs_92     DECIMAL(18,6) -- total en aivs de viv92 para el derechohabiente en proceso
   DEFINE v_d_sumtot_aivs_97     DECIMAL(18,6) -- total en aivs de viv97 para el derechohabiente en proceso
   DEFINE v_i_lote               LIKE dse_ctr_archivo.lote -- lote del archivo
   DEFINE v_b_inserta_reg        SMALLINT -- booleana que indica si se debe insertar o no el registro
   DEFINE r_d_tot_aivs_92        DECIMAL(18,6) -- total en aivs de viv92 para el derechohabiente en proceso
   DEFINE r_d_tot_aivs_uso_92    DECIMAL(18,6) -- total en aivs de viv92 para el derechohabiente en proceso
   DEFINE r_d_tot_aivs_97        DECIMAL(18,6) -- total en aivs de viv97 para el derechohabiente en proceso
   DEFINE r_d_tot_aivs_uso_97    DECIMAL(18,6) -- total en aivs de viv97 para el derechohabiente en proceso
   DEFINE r_d_tot_pesos_92       DECIMAL(12,2) --Modificación Emilio Abarca
   DEFINE r_d_tot_pesos_97       DECIMAL(12,2) --Modificación Emilio Abarca
   DEFINE r_b_valida             SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_obt_movs             SMALLINT -- booleana que indica que ya se obtuvieron los movimientos liquidados
   DEFINE v_ejecuta_sh           STRING

   -- Variables para la herencia de marcas
   DEFINE v_id_dh_resp           DECIMAL(9,0)
   DEFINE v_id_cre_ug_resp       DECIMAL(9,0)

   DEFINE v_valor_fondo          DECIMAL(12,6)
   DEFINE v_f_valua              DATE 
   DEFINE v_hoy                  DATE
   DEFINE cre_tpo_credito        SMALLINT

   --Arreglo para inf. del reporte PDF
   DEFINE arr_sol_saldo DYNAMIC ARRAY OF RECORD
      estado_desc    CHAR(50),
      t_registros    INTEGER,
      aivs92         DECIMAL(12,2),
      aivs97         DECIMAL(12,2),
      porcentaje     CHAR(12) 
   END RECORD 
   --Record para el total global de registros procesados
   DEFINE r_total_global RECORD
      t_registros    INTEGER,
      aivs92         DECIMAL(12,2),
      aivs97         DECIMAL(12,2),
      porcentaje     CHAR(12) 
   END RECORD
   DEFINE v_aux_porcentaje DECIMAL(6,2)
   DEFINE v_f_opera_ini    LIKE bat_ctr_operacion.fecha_ini
   DEFINE v_f_opera_fin    LIKE bat_ctr_operacion.fecha_fin
   DEFINE v_arh_salida     STRING
   DEFINE v_f_pivote       DATE
   DEFINE v_aux_aivs92     DECIMAL(16,6)
   DEFINE v_aux_aivs97     DECIMAL(16,6)


END GLOBALS

MAIN

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRS04.log")

   DISPLAY "=INICIA AGRS04="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY ""

   -- se inicializan variables
   LET v_i_tpo_originacion   = 4     -- Anualidades Garantizadas
   LET v_c_tpo_transferencia = "43"  -- Anualidades Garantizadas
   LET v_i_contador_reg      = 0
   LET v_d_sumtot_aivs_92    = 0
   LET v_d_sumtot_aivs_97    = 0
   LET v_valor_fondo         = 0

   LET v_hoy                 = TODAY - 1 units day

   --CALL fn_crea_tabla()

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados,ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado,v_ruta_bin

   -- se crea la fecha. Primer dia del mes
   LET v_dt_fec_present = v_hoy - DAY(v_hoy) + 1

   -- se crea tabla temporal, esta identificará a que proceso corresponde un NSS,
   -- la tabla será propia de No Atendidas y contendrá los registros del proceso anterior
   CALL fn_crea_tmp_solic_sdo_agr2()

   -- se crea tabla temporal la cual identifica de que proceso corresponde un NSS
   CALL fn_crea_tmp_solic_sdo_agr()

   -- se crea tabla temporal la cual contendrá los registros de cta movimiento
   CALL fn_crea_tmp_cta_mov_agrs04()

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_dt_fec_present CLIPPED, "',2)"

   PREPARE prp_obtiene_3habil FROM v_s_qryTxt
   EXECUTE prp_obtiene_3habil INTO v_dt_fec_present

   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_dt_fec_present < v_hoy THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_dt_fec_present = v_hoy - DAY(v_hoy) + 1
      LET v_dt_fec_present = v_dt_fec_present + 1 UNITS MONTH

      PREPARE prp_obtiene_3habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_dt_fec_present CLIPPED||"',2)"
      EXECUTE prp_obtiene_3habil_sig INTO v_dt_fec_present
   END IF

   DISPLAY " Fecha presentación: ",v_dt_fec_present USING "DD/MM/YYYY"
   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fec_present_ax = v_dt_fec_present USING "YYYYMMDD"

   LET v_f_valua = (v_dt_fec_present - DAY(v_dt_fec_present) + 1) + 1 UNITS MONTH

   DISPLAY " Fecha de valuación: ",v_f_valua USING "dd/mm/yyyy"

   SELECT precio_fondo
     INTO v_valor_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_valua
      AND fondo = 11

   DISPLAY " Precio AIV:         ",v_valor_fondo

   -- Obtiene fecha del mes anterior
   LET v_f_pivote = MDY(MONTH(TODAY),1,YEAR(TODAY))  
   LET v_f_pivote = v_f_pivote - 1 UNITS MONTH

   DISPLAY " Fecha mes anterior: ",v_f_pivote USING "dd/mm/yyyy"
 
   -- se obtiene el maximo lote para la fecha de presentación
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_fec_present,"'\n",
                    "    AND id_proceso = ",g_proc_cod_agr_arch_solic

   PREPARE prp_max_lote FROM v_s_qryTxt
   EXECUTE prp_max_lote INTO v_i_lote

   -- se valida el lote
   IF v_i_lote IS NULL THEN
      LET v_i_lote = 1
   ELSE
      LET v_i_lote = v_i_lote + 1
   END IF

   --LET v_r_cre_ctr_archivo.id_cre_ctr_archivo = seq_cre_archivo.NEXTVAL
   LET v_r_cre_ctr_archivo.folio_archivo        = p_d_folio
   LET v_r_cre_ctr_archivo.lote                 = v_i_lote
   LET v_r_cre_ctr_archivo.f_lote               = v_dt_fec_present
   LET v_r_cre_ctr_archivo.id_proceso           = g_proc_cod_agr_arch_solic
   LET v_r_cre_ctr_archivo.operacion            = 0
   LET v_r_cre_ctr_archivo.nom_archivo          = p_v_arch_proceso
   LET v_r_cre_ctr_archivo.tot_registros        = 0
   LET v_r_cre_ctr_archivo.tot_aceptados        = 0
   LET v_r_cre_ctr_archivo.tot_rechazados       = 0
   LET v_r_cre_ctr_archivo.tot_sin_origen       = 0
   LET v_r_cre_ctr_archivo.estado               = 10
   LET v_r_cre_ctr_archivo.f_proceso            = TODAY
   LET v_r_cre_ctr_archivo.usuario              = p_v_usuario

   -- se inserta el registro en la tabla de control
   INSERT INTO cre_ctr_archivo VALUES (seq_cre_archivo.NEXTVAL,
                                       v_r_cre_ctr_archivo.folio_archivo,
                                       v_r_cre_ctr_archivo.lote,
                                       v_r_cre_ctr_archivo.f_lote,
                                       v_r_cre_ctr_archivo.id_proceso,
                                       v_r_cre_ctr_archivo.operacion,
                                       v_r_cre_ctr_archivo.nom_archivo,
                                       v_r_cre_ctr_archivo.tot_registros,
                                       v_r_cre_ctr_archivo.tot_aceptados,
                                       v_r_cre_ctr_archivo.tot_rechazados,
                                       v_r_cre_ctr_archivo.tot_sin_origen,
                                       v_r_cre_ctr_archivo.estado,
                                       v_r_cre_ctr_archivo.f_proceso,
                                       v_r_cre_ctr_archivo.usuario)

   -- se consulta el identificador del archivo recien insertado
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE folio_archivo = ",p_d_folio,"\n",
                    "    AND lote = ",v_i_lote,"\n",
                    "    AND f_lote = '",v_dt_fec_present,"'\n",
                    "    AND id_proceso = ",g_proc_cod_agr_arch_solic,"\n",
                    "    AND operacion = 0"

   PREPARE prp_id_cre_ctr_archivo FROM v_s_qryTxt
   EXECUTE prp_id_cre_ctr_archivo INTO v_d_id_cre_ctr_archivo

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("") 

   -- se asignan los valores del registro encabezado a insertar
   LET v_r_encabezado.tpo_registro     = "01"
   LET v_r_encabezado.id_servicio      = "02"
   LET v_r_encabezado.id_operacion     = "01"
   LET v_r_encabezado.tpo_ent_origen   = "04"
   LET v_r_encabezado.cve_ent_origen   = "002"
   LET v_r_encabezado.tpo_ent_destino  = "03" --"01"
   LET v_r_encabezado.cve_ent_destino  = "001" --"" -- 3 espacios en blanco
   LET v_r_encabezado.ent_fed_env_lote = "009"
   LET v_r_encabezado.fec_presentacion = v_c_fec_present_ax
   LET v_r_encabezado.consec_lote      = "001"
   LET v_r_encabezado.cod_resul_opera  = "" -- 2 espacios en blanco
   LET v_r_encabezado.mot_rechazo      = "" -- 9 espacios en blanco
   LET v_r_encabezado.filler           = "" -- 689 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_encabezado.tpo_registro,
                      v_r_encabezado.id_servicio,
                      v_r_encabezado.id_operacion,
                      v_r_encabezado.tpo_ent_origen,
                      v_r_encabezado.cve_ent_origen,
                      v_r_encabezado.tpo_ent_destino,
                      v_r_encabezado.cve_ent_destino,
                      v_r_encabezado.ent_fed_env_lote,
                      v_r_encabezado.fec_presentacion,
                      v_r_encabezado.consec_lote,
                      v_r_encabezado.cod_resul_opera,
                      v_r_encabezado.mot_rechazo,
                      v_r_encabezado.filler

   -- se escribe el registro (encabezado) en el archivo
   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- se obtiene la fecha del ultimo día del mes anterior de la fecha de presentación
   LET v_dt_f_solic_saldos = v_dt_fec_present - DAY(v_dt_fec_present)
   --DISPLAY " Fecha Solic. Sdos.: ",v_dt_f_solic_saldos USING "dd/mm/yyyy"

   -- se obtienen los registros de la tabla cta movimiento de cre_acreditado y cre_uso_garantia
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_obtiene_mov_liq(?,?)"

   ---display v_s_qryTxt," ", v_i_tpo_originacion," ", v_c_tpo_transferencia

   PREPARE prp_obt_movs FROM v_s_qryTxt
   EXECUTE prp_obt_movs USING v_i_tpo_originacion, v_c_tpo_transferencia
                        INTO r_obt_movs

   CALL fn_crea_indices_agrs04()

   --REGISTROS CON ANUALIDAD GARANTIZADA

   LET v_r_cre_acred.sdo_deudor = 0

   -- propuesta MMC para que no se generen los registros con tipos de crédito que no generan deudor 29/08/2012 07:30:29 p.m.
   LET v_s_qryTxt = " SELECT cre.id_cre_acreditado, cre.id_derechohabiente, cre.num_credito,\n",
                    "        cre.folio_liquida, cre.estado, cre.edo_procesar, cre.sdo_deudor\n",
                    "   FROM cre_acreditado cre,\n",
                    "        cat_tipo_credito tpo\n",
                    "  WHERE cre.estado IN (18, 20, 25, 140, 145)\n",
                    "    AND cre.edo_procesar IN (60, 70)\n",
                    "    AND cre.tpo_originacion = ",v_i_tpo_originacion,"\n",
                    "    AND cre.tpo_originacion = tpo.tpo_originacion\n",
                    "    AND cre.tpo_credito = tpo.tpo_credito\n",
                    "    AND tpo.id_deudor = 1\n",
                    "    AND cre.sdo_deudor > 0"

   PREPARE prp_his_transferencia FROM v_s_qryTxt
   DECLARE cur_his_transferencia CURSOR FOR prp_his_transferencia

   FOREACH cur_his_transferencia INTO v_r_cre_acred.*

      -- se invoca la funcion que obtiene los datos del trabajador
      CALL f_obt_datos_trab(v_r_cre_acred.id_derhabiente) RETURNING r_r_afi_derechohab.*

      LET r_d_tot_aivs_97  = 0
      LET r_d_tot_aivs_92  = 0
      LET r_d_tot_pesos_92 = 0 
      LET r_d_tot_pesos_97 = 0

      IF v_r_cre_acred.sdo_deudor IS NULL THEN
         LET v_r_cre_acred.sdo_deudor = 0
      END IF

      IF v_r_cre_acred.estado = 140 OR v_r_cre_acred.estado = 145 THEN
         -- se valida el folio de liquidación
         IF v_r_cre_acred.folio_liquida > 0 THEN
            -- se obtiene el total en aivs para vivienda 92
            LET r_d_tot_aivs_92 = f_obt_aivs_liq(v_r_cre_acred.id_derhabiente, v_r_cre_acred.folio_liquida, 8)

            -- se obtiene el total en aivs para vivienda 97
            LET r_d_tot_aivs_97 = f_obt_aivs_liq(v_r_cre_acred.id_derhabiente, v_r_cre_acred.folio_liquida, 4)
         ELSE
            -- se asignan los valores de viv92 y viv97
            LET r_d_tot_aivs_92 = 0
            LET r_d_tot_aivs_97 = 0
         END IF
      ELSE
         IF v_r_cre_acred.estado = 20 OR
            v_r_cre_acred.estado = 18 THEN
            IF v_r_cre_acred.sdo_deudor <= 0 OR v_r_cre_acred.sdo_deudor IS NULL THEN
               -- se asignan los valores de viv92 y viv97
               LET r_d_tot_aivs_92 = 0
               LET r_d_tot_aivs_97 = 0
            ELSE
               -- se obtiene el total en aivs para vivienda 92

               LET v_r_cre_acred.sdo_deudor = v_r_cre_acred.sdo_deudor / v_valor_fondo

               LET r_d_tot_aivs_92 = f_obt_aivs_rem(v_r_cre_acred.id_derhabiente, v_r_cre_acred.folio_liquida, 8)

               IF r_d_tot_aivs_92 > 0 THEN
                  IF (v_r_cre_acred.sdo_deudor <= r_d_tot_aivs_92) THEN
                     ---LET r_d_tot_aivs_92 = v_r_cre_acred.sdo_deudor
                     LET v_r_cre_acred.sdo_deudor = 0
                  ELSE
                     LET v_r_cre_acred.sdo_deudor = v_r_cre_acred.sdo_deudor - r_d_tot_aivs_92
                  END IF
               END IF

               IF v_r_cre_acred.sdo_deudor > 0 THEN
                  -- se obtiene el total en aivs para vivienda 97
                  LET r_d_tot_aivs_97 = f_obt_aivs_rem(v_r_cre_acred.id_derhabiente, v_r_cre_acred.folio_liquida, 4)

                  IF r_d_tot_aivs_97 > 0 THEN
                     IF (v_r_cre_acred.sdo_deudor <= r_d_tot_aivs_97) THEN
                        LET r_d_tot_aivs_97 = v_r_cre_acred.sdo_deudor
                     END IF
                  END IF
               END IF
            END IF
         ELSE
            -- se obtiene el total en aivs para vivienda 92
            LET r_d_tot_aivs_92 = f_obt_aivs_rem(v_r_cre_acred.id_derhabiente, v_r_cre_acred.folio_liquida, 8)

           -- se obtiene el total en aivs para vivienda 97
            LET r_d_tot_aivs_97 = f_obt_aivs_rem(v_r_cre_acred.id_derhabiente, v_r_cre_acred.folio_liquida, 4)
         END IF

         LET r_d_tot_aivs_92 = r_d_tot_aivs_92 * -1
         LET r_d_tot_aivs_97 = r_d_tot_aivs_97 * -1
      END IF

      IF r_d_tot_aivs_92 = 0 AND r_d_tot_aivs_97 = 0 THEN
         LET v_r_cre_acred.sdo_deudor = 0
         CONTINUE FOREACH
      END IF

      -- se acumulan los importes
      LET v_d_sumtot_aivs_92 = v_d_sumtot_aivs_92 + r_d_tot_aivs_92
      LET v_d_sumtot_aivs_97 = v_d_sumtot_aivs_97 + r_d_tot_aivs_97

      --Calcula pesos (Modificación Emilio Abarca)
      LET r_d_tot_pesos_92 = (r_d_tot_aivs_92 * v_valor_fondo)
      LET r_d_tot_pesos_97 = (r_d_tot_aivs_97 * v_valor_fondo)

      -- se incrementa el contador de registro
      LET v_i_contador_reg = v_i_contador_reg + 1

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.tpo_registro       = "02"
      LET v_r_detalle.cont_servicio      = v_i_contador_reg USING "&&&&&&&&&&"
      LET v_r_detalle.tpo_ent_recept     = "04"
      LET v_r_detalle.cve_ent_recept     = "002"
      LET v_r_detalle.tpo_ent_cedente    = "01"
      LET v_r_detalle.cve_ent_cedente    = "" -- 3 espacios en blanco
      LET v_r_detalle.tpo_transferencia  = "43" -- 2 espacios en blanco
      LET v_r_detalle.f_presentacion     = v_c_fec_present_ax
      LET v_r_detalle.filler1            = "" -- 8 espacios en blanco
      LET v_r_detalle.curp_trabajador    = r_r_afi_derechohab.curp
      LET v_r_detalle.nss_trab_infonavit = r_r_afi_derechohab.nss
      LET v_r_detalle.filler2            = "" -- 15 espacios en blanco
      LET v_r_detalle.rfc_trab_infonavit = r_r_afi_derechohab.rfc
      LET v_r_detalle.ape_pat_infonavit  = r_r_afi_derechohab.ap_paterno_af
      LET v_r_detalle.ape_mat_infonavit  = r_r_afi_derechohab.ap_materno_af
      LET v_r_detalle.nom_trab_infonavit = r_r_afi_derechohab.nombre_af
      LET v_r_detalle.filler3            = "" -- 22 espacios en blanco
      LET v_r_detalle.id_lote_solic      = "04002" || v_c_fec_present_ax || "001"
      LET v_r_detalle.filler4            = "" -- 15 espacios en blanco
      LET v_r_detalle.nss_trab_afore     = "" -- 1a espacios en blanco. No se sabe el nss afore
      LET v_r_detalle.rfc_trab_afore     = "" -- 13 espacios en blanco. No se sabe el rfc afore
      LET v_r_detalle.filler5            = "" -- 30 espacios en blanco
      LET v_r_detalle.ape_pat_afore      = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.ape_mat_afore      = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.nom_trab_afore     = "" -- 40 espacios en blanco. No se sabe el nombre afore
      LET v_r_detalle.filler6            = "" -- 30 espacios en blanco
      LET v_r_detalle.num_apl_int_viv92  = (r_d_tot_aivs_92 * 1000000) USING "&&&&&&&&&&&&&&&" --"000000000000000"
      LET v_r_detalle.filler7            = "" -- 78 espacios en blanco
      LET v_r_detalle.num_apl_int_viv97  = (r_d_tot_aivs_97 * 1000000) USING "&&&&&&&&&&&&&&&" --"000000000000000"
      LET v_r_detalle.filler8            = "" -- 53 espacios en blanco
      LET v_r_detalle.nom_trab_imss      = r_r_afi_derechohab.nombre_imss
      LET v_r_detalle.num_cred_infonavit = v_r_cre_acred.num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.cod_result_opera   = "" -- 2 espacios en blanco
      LET v_r_detalle.diagnostico_proc   = "" -- 15 espacios en blanco
      LET v_r_detalle.filler9            = "" -- 12 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tpo_registro,
                         v_r_detalle.cont_servicio,
                         v_r_detalle.tpo_ent_recept,
                         v_r_detalle.cve_ent_recept,
                         v_r_detalle.tpo_ent_cedente,
                         v_r_detalle.cve_ent_cedente,
                         v_r_detalle.tpo_transferencia,
                         v_r_detalle.f_presentacion,
                         v_r_detalle.filler1,
                         v_r_detalle.curp_trabajador,
                         v_r_detalle.nss_trab_infonavit,
                         v_r_detalle.filler2,
                         v_r_detalle.rfc_trab_infonavit,
                         v_r_detalle.ape_pat_infonavit,
                         v_r_detalle.ape_mat_infonavit,
                         v_r_detalle.nom_trab_infonavit,
                         v_r_detalle.filler3,
                         v_r_detalle.id_lote_solic,
                         v_r_detalle.filler4,
                         v_r_detalle.nss_trab_afore,
                         v_r_detalle.rfc_trab_afore,
                         v_r_detalle.filler5,
                         v_r_detalle.ape_pat_afore,
                         v_r_detalle.ape_mat_afore,
                         v_r_detalle.nom_trab_afore,
                         v_r_detalle.filler6,
                         v_r_detalle.num_apl_int_viv92,
                         v_r_detalle.filler7,
                         v_r_detalle.num_apl_int_viv97,
                         v_r_detalle.filler8,
                         v_r_detalle.nom_trab_imss,
                         v_r_detalle.num_cred_infonavit,
                         v_r_detalle.cod_result_opera,
                         v_r_detalle.diagnostico_proc,
                         v_r_detalle.filler9

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se inserta el registro en la tabla temporal
      LET v_r_tmp_agr_solic_sdo.nss                = r_r_afi_derechohab.nss
      LET v_r_tmp_agr_solic_sdo.id_derechohabiente = v_r_cre_acred.id_derhabiente
      LET v_r_tmp_agr_solic_sdo.modulo_cod         = "AG"
      LET v_r_tmp_agr_solic_sdo.f_proceso          = TODAY
      LET v_r_tmp_agr_solic_sdo.id_referencia      = v_r_cre_acred.id_cre_acred

      -- se inserta el registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_agr_solic_sdo VALUES (v_r_tmp_agr_solic_sdo.*, r_d_tot_aivs_97, r_d_tot_aivs_92)

      #Guarda la petición en histórico (Modificación Emilio Abarca,EFP)
      INSERT INTO cre_his_solic_sdo(
                     id_referencia      ,
                     id_derechohabiente , 
                     nss                , 
                     modulo_cod         ,
                     aivs92             ,
                     aivs97             ,
                     pesos92            ,
                     pesos97            ,
                     periodo_pago       ,
                     f_proceso) 
            VALUES (v_r_tmp_agr_solic_sdo.id_referencia,  
                     v_r_tmp_agr_solic_sdo.id_derechohabiente ,
                     v_r_tmp_agr_solic_sdo.nss                ,
                     'AG'                                     ,
                     r_d_tot_aivs_92                          , 
                     r_d_tot_aivs_97                          ,
                     r_d_tot_pesos_92                         , 
                     r_d_tot_pesos_97                         ,
                     NULL                                    , -- No aplica
                     TODAY);

      -- se valida el estado procesar
      IF v_r_cre_acred.edo_procesar = 70 THEN
         LET v_edo_procesar_aux = 85
      ELSE
         LET v_edo_procesar_aux = 80
      END IF

      -- se actualiza el registro leído de cre acreditado
      UPDATE cre_acreditado
         SET edo_procesar      = v_edo_procesar_aux
       WHERE id_cre_acreditado = v_r_cre_acred.id_cre_acred

      -- se asigna el registro a en cre his acreditado
      LET v_cre_his_acreditado.id_cre_acreditado  = v_r_cre_acred.id_cre_acred
      LET v_cre_his_acreditado.id_cre_ctr_archivo = v_d_id_cre_ctr_archivo
      LET v_cre_his_acreditado.tpo_transferencia  = v_c_tpo_transferencia
      LET v_cre_his_acreditado.edo_procesar       = v_edo_procesar_aux
      LET v_cre_his_acreditado.diagnostico        = 0
      LET v_cre_his_acreditado.estado             = v_r_cre_acred.estado
      LET v_cre_his_acreditado.nss_afore          = NULL
      LET v_cre_his_acreditado.rfc_afore          = NULL
      LET v_cre_his_acreditado.paterno_afore      = NULL
      LET v_cre_his_acreditado.materno_afore      = NULL
      LET v_cre_his_acreditado.nombre_afore       = NULL
      LET v_cre_his_acreditado.nom_imss           = NULL
      LET v_cre_his_acreditado.f_proceso          = TODAY

      -- se inserta el registro en la tabla de historicos
      INSERT INTO cre_his_acreditado VALUES (v_cre_his_acreditado.*)

      LET v_r_cre_acred.sdo_deudor = 0
   END FOREACH

   --REGISTROS CON USO DE ANUALIDAD CON ADELANTO

   LET v_det_nss = ""

   -- en esta tabla podría haber duplicados, por lo que se agruparán los registros por derechohabiente 
   LET v_s_qryTxt = " SELECT UNIQUE c1.id_derechohabiente, c1.num_credito\n",
                    "   FROM cre_uso_garantia c1\n",
                    "  WHERE c1.estado = 140\n",
                    "    AND c1.edo_procesar IN (10, 70)\n",
                    "    AND c1.tpo_transferencia = '",v_c_tpo_transferencia,"'\n",
                    "    AND c1.id_derechohabiente NOT IN(\n",
                    "        SELECT t1.id_derechohabiente\n",
                    "          FROM safre_tmp:tmp_agr_solic_sdo t1)"

   PREPARE prp_uso_garantia_idder FROM v_s_qryTxt
   DECLARE cur_uso_garantia_idder CURSOR FOR prp_uso_garantia_idder

   FOREACH cur_uso_garantia_idder INTO v_d_id_derhabiente, v_d_num_credito
      -- se inicializa la varible que indica si se inserta o no en el archivo
      LET v_b_inserta_reg = FALSE

      -- se inicializan los importes de vivienda 92 y 97
      LET r_d_tot_aivs_92  = 0
      LET r_d_tot_aivs_97  = 0
      LET r_d_tot_pesos_92 = 0
      LET r_d_tot_pesos_97 = 0

      -- se consultan  todos los registro para el derechohabiente en proceso
      DECLARE cur_uso_garantia CURSOR FOR
       SELECT c1.id_cre_uso_garantia, c1.folio_liquida, c1.estado, c1.edo_procesar, c1.importe_v97, a1.nss
         FROM cre_uso_garantia c1, afi_derechohabiente a1
        WHERE c1.estado = 140
          AND c1.edo_procesar IN (10, 70)
          AND c1.id_derechohabiente = v_d_id_derhabiente
          AND c1.tpo_transferencia  = v_c_tpo_transferencia
          AND c1.id_derechohabiente = a1.id_derechohabiente

      FOREACH cur_uso_garantia INTO v_r_cre_uso.*, v_det_nss
         LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_uso_unificacion(?,?)"

         PREPARE prp_exe_unif FROM v_s_qryTxt
         EXECUTE prp_exe_unif USING v_d_id_derhabiente,
                                    v_r_cre_uso.id_cre_uso INTO v_id_dh_resp,
                                                                v_id_cre_ug_resp

         -- se valida el folio de liquidación
         IF v_r_cre_uso.folio_liquida > 0 THEN
            -- se obtiene el total en aivs para vivienda 92
            LET r_d_tot_aivs_uso_92 = f_obt_aivs_liq(v_id_dh_resp, v_r_cre_uso.folio_liquida, 8)

            -- se obtiene el total en aivs para vivienda 97
            LET r_d_tot_aivs_uso_97 = f_obt_aivs_liq(v_id_dh_resp, v_r_cre_uso.folio_liquida, 4)
         ELSE
            -- se asignan los valores de viv92 y viv97
            LET r_d_tot_aivs_uso_92 = 0
            LET r_d_tot_aivs_uso_97 = 0

            CONTINUE FOREACH
         END IF

         -- si los importes tanto de vivienda 92 como vivienda 97 son cero, no se inserta registro
         IF r_d_tot_aivs_uso_92 = 0 AND r_d_tot_aivs_uso_97 = 0 THEN
            CONTINUE FOREACH
         END IF

         -- se indica que se debe insertar el registro ya que los importes son menores a cero
         LET v_b_inserta_reg = TRUE

         -- se acumulan los importes
         LET r_d_tot_aivs_92 = r_d_tot_aivs_92 + r_d_tot_aivs_uso_92
         LET r_d_tot_aivs_97 = r_d_tot_aivs_97 + r_d_tot_aivs_uso_97

         -- se valida el estado procesar
         IF v_r_cre_uso.edo_procesar = 70 THEN
            LET v_edo_procesar_aux = 85
         ELSE
            LET v_edo_procesar_aux = 80
         END IF

         -- se actualiza el registro en la tabla maestro
         UPDATE cre_uso_garantia
            SET edo_procesar        = v_edo_procesar_aux
          WHERE id_cre_uso_garantia = v_r_cre_uso.id_cre_uso

         -- se inserta el registro en la tabla temporal
         --INSERT INTO safre_tmp:tmp_agr_solic_sdo_ua (nss, id_derechohabiente, modulo_cod, f_proceso, id_referencia, aivs97, aivs92)
         --VALUES (v_det_nss, v_d_id_derhabiente, "UA", TODAY, v_r_cre_uso.id_cre_uso, r_d_tot_aivs_97, r_d_tot_aivs_92)
      END FOREACH

      -- se valida si se actualizó al menos un registro de la tabla maestro
      IF NOT v_b_inserta_reg THEN
         CONTINUE FOREACH
      END IF

      -- se invoca la funcion que obtiene los datos del trabajador
      CALL f_obt_datos_trab(v_d_id_derhabiente) RETURNING r_r_afi_derechohab.*

      -- se incrementa el contador de registro
      LET v_i_contador_reg = v_i_contador_reg + 1

      -- se acumulan los importes
      LET v_d_sumtot_aivs_92 = v_d_sumtot_aivs_92 + r_d_tot_aivs_92
      LET v_d_sumtot_aivs_97 = v_d_sumtot_aivs_97 + r_d_tot_aivs_97

      --Calcula pesos (Modificación Emilio Abarca)
      LET r_d_tot_pesos_92 = (r_d_tot_aivs_92 * v_valor_fondo)
      LET r_d_tot_pesos_97 = (r_d_tot_aivs_97 * v_valor_fondo)

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.tpo_registro        = "02"
      LET v_r_detalle.cont_servicio       = v_i_contador_reg USING "&&&&&&&&&&"
      LET v_r_detalle.tpo_ent_recept      = "04"
      LET v_r_detalle.cve_ent_recept      = "002"
      LET v_r_detalle.tpo_ent_cedente     = "01"
      LET v_r_detalle.cve_ent_cedente     = "" -- 3 espacios en blanco
      LET v_r_detalle.tpo_transferencia   = "43" -- 2 espacios en blanco
      LET v_r_detalle.f_presentacion      = v_c_fec_present_ax
      LET v_r_detalle.filler1             = "" -- 8 espacios en blanco
      LET v_r_detalle.curp_trabajador     = r_r_afi_derechohab.curp
      LET v_r_detalle.nss_trab_infonavit  = r_r_afi_derechohab.nss
      LET v_r_detalle.filler2             = "" -- 15 espacios en blanco
      LET v_r_detalle.rfc_trab_infonavit  = r_r_afi_derechohab.rfc
      LET v_r_detalle.ape_pat_infonavit   = r_r_afi_derechohab.ap_paterno_af
      LET v_r_detalle.ape_mat_infonavit   = r_r_afi_derechohab.ap_materno_af
      LET v_r_detalle.nom_trab_infonavit  = r_r_afi_derechohab.nombre_af
      LET v_r_detalle.filler3             = "" -- 22 espacios en blanco
      LET v_r_detalle.id_lote_solic       = "04002" || v_c_fec_present_ax || "001"
      LET v_r_detalle.filler4             = "" -- 15 espacios en blanco 
      LET v_r_detalle.nss_trab_afore      = "" -- 1a espacios en blanco. No se sabe el nss afore
      LET v_r_detalle.rfc_trab_afore      = "" -- 13 espacios en blanco. No se sabe el rfc afore
      LET v_r_detalle.filler5             = "" -- 30 espacios en blanco
      LET v_r_detalle.ape_pat_afore       = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.ape_mat_afore       = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.nom_trab_afore      = "" -- 40 espacios en blanco. No se sabe el nombre afore
      LET v_r_detalle.filler6             = "" -- 30 espacios en blanco
      LET v_r_detalle.num_apl_int_viv92   = (r_d_tot_aivs_92 * 1000000) USING "&&&&&&&&&&&&&&&" --"000000000000000"
      LET v_r_detalle.filler7             = "" -- 78 espacios en blanco
      LET v_r_detalle.num_apl_int_viv97   = (r_d_tot_aivs_97 * 1000000) USING "&&&&&&&&&&&&&&&" --"000000000000000"
      LET v_r_detalle.filler8             = "" -- 53 espacios en blanco
      LET v_r_detalle.nom_trab_imss       = r_r_afi_derechohab.nombre_imss
      LET v_r_detalle.num_cred_infonavit  = v_d_num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.cod_result_opera    = "" -- 2 espacios en blanco
      LET v_r_detalle.diagnostico_proc    = "" -- 15 espacios en blanco
      LET v_r_detalle.filler9             = "" -- 12 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tpo_registro,
                         v_r_detalle.cont_servicio,
                         v_r_detalle.tpo_ent_recept,
                         v_r_detalle.cve_ent_recept,
                         v_r_detalle.tpo_ent_cedente,
                         v_r_detalle.cve_ent_cedente,
                         v_r_detalle.tpo_transferencia,
                         v_r_detalle.f_presentacion,
                         v_r_detalle.filler1,
                         v_r_detalle.curp_trabajador,
                         v_r_detalle.nss_trab_infonavit,
                         v_r_detalle.filler2,
                         v_r_detalle.rfc_trab_infonavit,
                         v_r_detalle.ape_pat_infonavit,
                         v_r_detalle.ape_mat_infonavit,
                         v_r_detalle.nom_trab_infonavit,
                         v_r_detalle.filler3,
                         v_r_detalle.id_lote_solic,
                         v_r_detalle.filler4,
                         v_r_detalle.nss_trab_afore,
                         v_r_detalle.rfc_trab_afore,
                         v_r_detalle.filler5,
                         v_r_detalle.ape_pat_afore,
                         v_r_detalle.ape_mat_afore,
                         v_r_detalle.nom_trab_afore,
                         v_r_detalle.filler6,
                         v_r_detalle.num_apl_int_viv92,
                         v_r_detalle.filler7,
                         v_r_detalle.num_apl_int_viv97,
                         v_r_detalle.filler8,
                         v_r_detalle.nom_trab_imss,
                         v_r_detalle.num_cred_infonavit,
                         v_r_detalle.cod_result_opera,
                         v_r_detalle.diagnostico_proc,
                         v_r_detalle.filler9

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se inserta el registro en la tabla temporal
      LET v_r_tmp_agr_solic_sdo.nss                = r_r_afi_derechohab.nss
      LET v_r_tmp_agr_solic_sdo.id_derechohabiente = v_d_id_derhabiente
      LET v_r_tmp_agr_solic_sdo.modulo_cod         = "UA"
      LET v_r_tmp_agr_solic_sdo.f_proceso          = TODAY
      LET v_r_tmp_agr_solic_sdo.id_referencia      = v_r_cre_uso.id_cre_uso

      -- se inserta el registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_agr_solic_sdo VALUES (v_r_tmp_agr_solic_sdo.*, r_d_tot_aivs_97, r_d_tot_aivs_92)

      INSERT INTO safre_tmp:tmp_agr_solic_sdo_ua (nss, id_derechohabiente, modulo_cod, f_proceso, id_referencia, aivs97, aivs92)
      VALUES (v_det_nss, v_d_id_derhabiente, "UA", TODAY, v_r_cre_uso.id_cre_uso, r_d_tot_aivs_97, r_d_tot_aivs_92)

      #Guarda la petición en histórico (Modificación Emilio Abarca,EFP)
      INSERT INTO cre_his_solic_sdo(
                     id_referencia      ,
                     id_derechohabiente , 
                     nss                , 
                     modulo_cod         ,
                     aivs92             ,
                     aivs97             ,
                     pesos92            ,
                     pesos97            ,
                     periodo_pago       ,
                     f_proceso) 
            VALUES (v_r_tmp_agr_solic_sdo.id_referencia      ,  
                     v_r_tmp_agr_solic_sdo.id_derechohabiente ,
                     v_r_tmp_agr_solic_sdo.nss                ,
                     'UA'                                     ,
                     r_d_tot_aivs_92                          , 
                     r_d_tot_aivs_97                          ,
                     r_d_tot_pesos_92                         , 
                     r_d_tot_pesos_97                         ,
                     NULL                                     , -- No aplica
                     TODAY);

      LET v_det_nss       = ""
      LET r_d_tot_aivs_92 = 0
      LET r_d_tot_aivs_97 = 0
   END FOREACH

   --REGISTROS CON USO DE ANUALIDAD SIN ADELANTO

   LET v_det_nss = ""

   -- en esta tabla podría haber duplicados, por lo que se agruparán los registros por derechohabiente 
   LET v_s_qryTxt = " SELECT UNIQUE c3.id_derechohabiente, c3.num_credito\n",
                    "   FROM cre_uso_garantia c3\n",
                    "  WHERE c3.estado IN(20, 142)\n",
                    "    AND c3.edo_procesar IN(10,70)\n",
                    "    AND c3.tpo_transferencia = '",v_c_tpo_transferencia,"'\n",
                    "    AND c3.id_derechohabiente NOT IN(\n",
                    "        SELECT t3.id_derechohabiente\n",
                    "          FROM safre_tmp:tmp_agr_solic_sdo t3)"

   PREPARE prp_ua_grp_sin FROM v_s_qryTxt
   DECLARE cur_ua_grp_sin CURSOR FOR prp_ua_grp_sin

   FOREACH cur_ua_grp_sin INTO v_d_id_derhabiente, v_d_num_credito
      -- se inicializa la varible que indica si se inserta o no en el archivo
      LET v_b_inserta_reg  = FALSE
      LET r_d_tot_pesos_92 = 0
      LET r_d_tot_pesos_97 = 0
      LET r_d_tot_aivs_92  = 0
      LET r_d_tot_aivs_97  = 0
      
      -- se consultan  todos los registro para el derechohabiente en proceso
      DECLARE cur_ua_sin CURSOR FOR
       SELECT FIRST 1 c2.id_cre_uso_garantia, c2.folio_liquida, c2.estado, c2.edo_procesar, c2.importe_v97, a2.nss
         FROM cre_uso_garantia c2, afi_derechohabiente a2
        WHERE c2.estado             IN(20, 142)
          AND c2.edo_procesar       IN(10, 70)
          AND c2.id_derechohabiente = v_d_id_derhabiente
          AND c2.tpo_transferencia  = v_c_tpo_transferencia
          AND c2.id_derechohabiente = a2.id_derechohabiente
          AND c2.id_derechohabiente NOT IN(
              SELECT t2.id_derechohabiente
                FROM safre_tmp:tmp_agr_solic_sdo t2
               WHERE t2.modulo_cod = "UA")
         ORDER BY c2.estado

      FOREACH cur_ua_sin INTO v_r_cre_uso.id_cre_uso,
                              v_r_cre_uso.folio_liquida,
                              v_r_cre_uso.estado,
                              v_r_cre_uso.edo_procesar,
                              v_r_cre_uso.importe_uso,
                              v_det_nss

         SELECT FIRST 1 s.marca
           INTO cre_tpo_credito
           FROM sfr_marca_activa s
          WHERE s.id_derechohabiente = v_d_id_derhabiente
            AND s.marca IN(
                SELECT m.marca_inf
                  FROM cat_tipo_credito m
                 WHERE m.tpo_originacion = 4)
          ORDER BY s.f_inicio DESC

         LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_uso_unificacion(?,?)"

         PREPARE prp_exe_unif_sin FROM v_s_qryTxt
         EXECUTE prp_exe_unif_sin USING v_d_id_derhabiente,
                                        v_r_cre_uso.id_cre_uso INTO v_id_dh_resp,
                                                                    v_id_cre_ug_resp

         -- se inicializan los importes de vivienda 92 y 97
         LET r_d_tot_aivs_uso_92 = 0
         LET r_d_tot_aivs_uso_97 = 0

         IF v_r_cre_uso.importe_uso <= 0 THEN
            CONTINUE FOREACH
         ELSE

            LET v_r_cre_uso.importe_uso = v_r_cre_uso.importe_uso / v_valor_fondo

            IF(v_r_cre_uso.estado = 20) THEN
            
               IF cre_tpo_credito = 212 OR cre_tpo_credito = 218 OR cre_tpo_credito = 219 THEN
               
                  LET r_d_tot_aivs_uso_97 = f_obt_aivs_rem(v_id_dh_resp, 0, 4)

                  IF r_d_tot_aivs_uso_97 > 0 THEN
                     IF (v_r_cre_uso.importe_uso <= r_d_tot_aivs_uso_97) THEN
                        LET r_d_tot_aivs_uso_97 = v_r_cre_uso.importe_uso
                     END IF
                  ELSE
                     LET r_d_tot_aivs_uso_97 = 0
                  END IF
               ELSE
                  LET r_d_tot_aivs_uso_92 = f_obt_aivs_rem(v_id_dh_resp, 0, 8)

                  IF r_d_tot_aivs_uso_92 > 0 THEN
                     IF (v_r_cre_uso.importe_uso <= r_d_tot_aivs_uso_92) THEN
                        LET r_d_tot_aivs_uso_92     = v_r_cre_uso.importe_uso
                        LET v_r_cre_uso.importe_uso = 0
                     ELSE
                        LET v_r_cre_uso.importe_uso = v_r_cre_uso.importe_uso - r_d_tot_aivs_uso_92
                     END IF
                  ELSE
                     LET r_d_tot_aivs_uso_92 = 0
                  END IF

                  IF v_r_cre_uso.importe_uso > 0 THEN
                     -- se obtiene el total en aivs para vivienda 97
                     LET r_d_tot_aivs_uso_97 = f_obt_aivs_rem(v_id_dh_resp, 0, 4)

                     IF r_d_tot_aivs_uso_97 > 0 THEN
                        IF (v_r_cre_uso.importe_uso <= r_d_tot_aivs_uso_97) THEN
                           LET r_d_tot_aivs_uso_97 = v_r_cre_uso.importe_uso
                        END IF
                     ELSE
                        LET r_d_tot_aivs_uso_97 = 0
                     END IF
                  END IF
               END IF
            ELSE

               LET v_aux_aivs92     = 0
               LET v_aux_aivs97     = 0

               --~~ Búsca si el registro fué cargado de manera especial durante el mes anterior ~~
               --DISPLAY "id_dh: ",v_d_id_derhabiente

               # Carga solicitud de saldo (ÍTEM 4-SACI2017-1)
               SELECT FIRST 1
                      aivs92,
                      aivs97
                 INTO v_aux_aivs92,
                      v_aux_aivs97
                 FROM cre_solic_sdo
                WHERE id_derechohabiente = v_d_id_derhabiente
                  AND diagnostico    = 0
                  AND marca_procesar = 4
                  AND f_proceso >= v_f_pivote
                  ORDER BY f_proceso DESC;

               IF(v_aux_aivs92 = 0) AND (v_aux_aivs97 = 0) OR 
                 (v_aux_aivs92 IS NULL) AND (v_aux_aivs97 IS NULL) THEN
                 
                  # Búsca Solicitud en conciliación de marcas en la entrada del recurrente Originación AG o 43BIS (ÍTEM 5-SACI2017-1)
                  SELECT FIRST 1
                        aivs92,
                        aivs97
                   INTO v_aux_aivs92,
                        v_aux_aivs97
                   FROM cre_marca_conciliacion
                  WHERE id_derechohabiente = v_d_id_derhabiente
                    AND estado        = 20
                    AND tpo_originar  = 4
                    AND f_proceso >= v_f_pivote
                    ORDER BY f_proceso DESC;
               END IF

               IF(v_aux_aivs92 = 0) AND (v_aux_aivs97 = 0) OR 
                 (v_aux_aivs92 IS NULL) AND (v_aux_aivs97 IS NULL) THEN

                  # Búsca solicitud Actualización de marcas solicitud de saldo (ÍTEM 6-SACI2017-1)
                  SELECT FIRST 1
                        aivs_92,
                        aivs_97
                   INTO v_aux_aivs92,
                        v_aux_aivs97
                   FROM cre_act_marca_sspr
                  WHERE id_derechohabiente = v_d_id_derhabiente
                    AND diagnostico   = 0
                    AND marca_prc     = 4
                    AND f_proceso >= v_f_pivote
                    ORDER BY f_proceso DESC;
               END IF 
               
               IF(v_aux_aivs92 IS NOT NULL) AND (v_aux_aivs97 IS NOT NULL) AND 
                 (v_aux_aivs92 <> 0) OR (v_aux_aivs97 <> 0) THEN
                  LET r_d_tot_aivs_uso_92 = v_aux_aivs92
                  LET r_d_tot_aivs_uso_97 = v_aux_aivs97
               ELSE
                  LET r_d_tot_aivs_uso_92 = 0
                  LET r_d_tot_aivs_uso_97 = v_r_cre_uso.importe_uso  -- Asigna el monto importe_v97 calculado en aivs
               END IF 
            END IF 
         END IF

         -- si los importes tanto de vivienda 92 como vivienda 97 son cero, no se inserta registro
         IF r_d_tot_aivs_uso_92 = 0 AND r_d_tot_aivs_uso_97 = 0 THEN
            CONTINUE FOREACH
         END IF

         -- se indica que se debe insertar el registro ya que los importes son menores a cero
         LET v_b_inserta_reg = TRUE

         -- se acumulan los importes
         LET r_d_tot_aivs_92 = r_d_tot_aivs_92 + r_d_tot_aivs_uso_92
         LET r_d_tot_aivs_97 = r_d_tot_aivs_97 + r_d_tot_aivs_uso_97

         -- se valida el estado procesar
         IF v_r_cre_uso.edo_procesar = 70 THEN
            LET v_edo_procesar_aux = 85
         ELSE
            LET v_edo_procesar_aux = 80
         END IF

         -- se actualiza el registro en la tabla maestro
         UPDATE cre_uso_garantia
            SET edo_procesar        = v_edo_procesar_aux
          WHERE id_cre_uso_garantia = v_r_cre_uso.id_cre_uso

         -- se inserta el registro en la tabla temporal
         --INSERT INTO safre_tmp:tmp_agr_solic_sdo_ua (nss, id_derechohabiente, modulo_cod, f_proceso, id_referencia, aivs97, aivs92)
         --VALUES (v_det_nss, v_d_id_derhabiente, "UA", TODAY, v_r_cre_uso.id_cre_uso, r_d_tot_aivs_97, r_d_tot_aivs_92)

         LET r_d_tot_aivs_uso_92     = 0
         LET r_d_tot_aivs_uso_97     = 0
         LET v_r_cre_uso.importe_uso = 0
      END FOREACH

      -- se valida si se actualizó al menos un registro de la tabla maestro
      IF NOT v_b_inserta_reg THEN
         CONTINUE FOREACH
      END IF

      -- se invoca la funcion que obtiene los datos del trabajador
      CALL f_obt_datos_trab(v_d_id_derhabiente) RETURNING r_r_afi_derechohab.*

      -- se incrementa el contador de registro
      LET v_i_contador_reg = v_i_contador_reg + 1

      -- se acumulan los importes
      LET v_d_sumtot_aivs_92 = v_d_sumtot_aivs_92 + (r_d_tot_aivs_92 * -1)
      LET v_d_sumtot_aivs_97 = v_d_sumtot_aivs_97 + (r_d_tot_aivs_97 * -1)

      --Calcula pesos (Modificación Emilio Abarca)
      LET r_d_tot_pesos_92 = (r_d_tot_aivs_92 * v_valor_fondo)
      LET r_d_tot_pesos_97 = (r_d_tot_aivs_97 * v_valor_fondo)

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.tpo_registro        = "02"
      LET v_r_detalle.cont_servicio       = v_i_contador_reg USING "&&&&&&&&&&"
      LET v_r_detalle.tpo_ent_recept      = "04"
      LET v_r_detalle.cve_ent_recept      = "002"
      LET v_r_detalle.tpo_ent_cedente     = "01"
      LET v_r_detalle.cve_ent_cedente     = "" -- 3 espacios en blanco
      LET v_r_detalle.tpo_transferencia   = "43" -- 2 espacios en blanco
      LET v_r_detalle.f_presentacion      = v_c_fec_present_ax
      LET v_r_detalle.filler1             = "" -- 8 espacios en blanco
      LET v_r_detalle.curp_trabajador     = r_r_afi_derechohab.curp
      LET v_r_detalle.nss_trab_infonavit  = r_r_afi_derechohab.nss
      LET v_r_detalle.filler2             = "" -- 15 espacios en blanco
      LET v_r_detalle.rfc_trab_infonavit  = r_r_afi_derechohab.rfc
      LET v_r_detalle.ape_pat_infonavit   = r_r_afi_derechohab.ap_paterno_af
      LET v_r_detalle.ape_mat_infonavit   = r_r_afi_derechohab.ap_materno_af
      LET v_r_detalle.nom_trab_infonavit  = r_r_afi_derechohab.nombre_af
      LET v_r_detalle.filler3             = "" -- 22 espacios en blanco
      LET v_r_detalle.id_lote_solic       = "04002" || v_c_fec_present_ax || "001"
      LET v_r_detalle.filler4             = "" -- 15 espacios en blanco 
      LET v_r_detalle.nss_trab_afore      = "" -- 1a espacios en blanco. No se sabe el nss afore
      LET v_r_detalle.rfc_trab_afore      = "" -- 13 espacios en blanco. No se sabe el rfc afore
      LET v_r_detalle.filler5             = "" -- 30 espacios en blanco
      LET v_r_detalle.ape_pat_afore       = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.ape_mat_afore       = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.nom_trab_afore      = "" -- 40 espacios en blanco. No se sabe el nombre afore
      LET v_r_detalle.filler6             = "" -- 30 espacios en blanco
      LET v_r_detalle.num_apl_int_viv92   = (r_d_tot_aivs_92 * 1000000) USING "&&&&&&&&&&&&&&&" --"000000000000000"
      LET v_r_detalle.filler7             = "" -- 78 espacios en blanco
      LET v_r_detalle.num_apl_int_viv97   = (r_d_tot_aivs_97 * 1000000) USING "&&&&&&&&&&&&&&&" --"000000000000000"
      LET v_r_detalle.filler8             = "" -- 53 espacios en blanco
      LET v_r_detalle.nom_trab_imss       = r_r_afi_derechohab.nombre_imss
      LET v_r_detalle.num_cred_infonavit  = v_d_num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.cod_result_opera    = "" -- 2 espacios en blanco
      LET v_r_detalle.diagnostico_proc    = "" -- 15 espacios en blanco
      LET v_r_detalle.filler9             = "" -- 12 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tpo_registro,
                         v_r_detalle.cont_servicio,
                         v_r_detalle.tpo_ent_recept,
                         v_r_detalle.cve_ent_recept,
                         v_r_detalle.tpo_ent_cedente,
                         v_r_detalle.cve_ent_cedente,
                         v_r_detalle.tpo_transferencia,
                         v_r_detalle.f_presentacion,
                         v_r_detalle.filler1,
                         v_r_detalle.curp_trabajador,
                         v_r_detalle.nss_trab_infonavit,
                         v_r_detalle.filler2,
                         v_r_detalle.rfc_trab_infonavit,
                         v_r_detalle.ape_pat_infonavit,
                         v_r_detalle.ape_mat_infonavit,
                         v_r_detalle.nom_trab_infonavit,
                         v_r_detalle.filler3,
                         v_r_detalle.id_lote_solic,
                         v_r_detalle.filler4,
                         v_r_detalle.nss_trab_afore,
                         v_r_detalle.rfc_trab_afore,
                         v_r_detalle.filler5,
                         v_r_detalle.ape_pat_afore,
                         v_r_detalle.ape_mat_afore,
                         v_r_detalle.nom_trab_afore,
                         v_r_detalle.filler6,
                         v_r_detalle.num_apl_int_viv92,
                         v_r_detalle.filler7,
                         v_r_detalle.num_apl_int_viv97,
                         v_r_detalle.filler8,
                         v_r_detalle.nom_trab_imss,
                         v_r_detalle.num_cred_infonavit,
                         v_r_detalle.cod_result_opera,
                         v_r_detalle.diagnostico_proc,
                         v_r_detalle.filler9

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se inserta el registro en la tabla temporal
      LET v_r_tmp_agr_solic_sdo.nss                = r_r_afi_derechohab.nss
      LET v_r_tmp_agr_solic_sdo.id_derechohabiente = v_d_id_derhabiente
      LET v_r_tmp_agr_solic_sdo.modulo_cod         = "UA"
      LET v_r_tmp_agr_solic_sdo.f_proceso          = TODAY
      LET v_r_tmp_agr_solic_sdo.id_referencia      = v_r_cre_uso.id_cre_uso

      -- se inserta el registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_agr_solic_sdo VALUES (v_r_tmp_agr_solic_sdo.*, r_d_tot_aivs_97, r_d_tot_aivs_92)

      INSERT INTO safre_tmp:tmp_agr_solic_sdo_ua (nss, id_derechohabiente, modulo_cod, f_proceso, id_referencia, aivs97, aivs92)
      VALUES (v_det_nss, v_d_id_derhabiente, "UA", TODAY, v_r_cre_uso.id_cre_uso, r_d_tot_aivs_97, r_d_tot_aivs_92)

      #Guarda la petición en histórico (Modificación Emilio Abarca,EFP)
      INSERT INTO cre_his_solic_sdo(
                     id_referencia      ,
                     id_derechohabiente , 
                     nss                , 
                     modulo_cod         ,
                     aivs92             ,
                     aivs97             ,
                     pesos92            ,
                     pesos97            ,
                     periodo_pago       ,
                     f_proceso) 
            VALUES (v_r_tmp_agr_solic_sdo.id_referencia      ,  
                     v_r_tmp_agr_solic_sdo.id_derechohabiente ,
                     v_r_tmp_agr_solic_sdo.nss                ,
                     'UA'                                     ,
                     r_d_tot_aivs_92                          , 
                     r_d_tot_aivs_97                          ,
                     r_d_tot_pesos_92                         , 
                     r_d_tot_pesos_97                         ,
                     NULL                                     , -- No aplica
                     TODAY);

      LET v_det_nss       = ""

   END FOREACH

   -- se actualiza el total de registros en la tabla de control
   UPDATE cre_ctr_archivo
      SET tot_registros = v_i_contador_reg
    WHERE id_cre_ctr_archivo = v_d_id_cre_ctr_archivo
      AND folio_archivo = p_d_folio

   -- se asignan los valores del registro sumario
   LET v_r_sumario.tpo_registro        = "09"
   LET v_r_sumario.cant_regs_detalle   = v_i_contador_reg USING "&&&&&&&&&"
   LET v_r_sumario.filler1             = "" -- 45 espacios en blanco
   LET v_r_sumario.sum_numAplInt_viv92 = (v_d_sumtot_aivs_92 * 1000000) USING "&&&&&&&&&&&&&&&&&&" -- "000000000000000"
   LET v_r_sumario.filler2             = "" -- 60 espacios en blanco
   LET v_r_sumario.sum_numAplInt_viv97 = (v_d_sumtot_aivs_97 * 1000000) USING "&&&&&&&&&&&&&&&&&&" -- "000000000000000"
   LET v_r_sumario.filler3             = "" -- 578 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_sumario.tpo_registro,
                      v_r_sumario.cant_regs_detalle,
                      v_r_sumario.filler1,
                      v_r_sumario.sum_numAplInt_viv92,
                      v_r_sumario.filler2,
                      v_r_sumario.sum_numAplInt_viv97,
                      v_r_sumario.filler3

   -- se escribe el registro (sumario) en el archivo
   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = v_c_ruta_envio CLIPPED || "/" || "sol_ag." || v_c_extension

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   DISPLAY ""
   DISPLAY " Total de registros por enviar: ",v_i_contador_reg
   DISPLAY " Suma total de AIVS92: ",v_d_sumtot_aivs_92
   DISPLAY " Suma total de AIVS97: ",v_d_sumtot_aivs_97
   DISPLAY ""

   --DISPLAY " Ejecutando envío interfaz para Procesar"

   --LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/sol_ag.sh"
   --RUN v_ejecuta_sh

   --Ejecuta función que genera PDF y archivo de salida
   CALL inf_sol_reporte()
   DISPLAY ""
   DISPLAY " Genera reporte PDF ...completado"
   DISPLAY ""
   DISPLAY " El archivo detalle de salida podrá ser recuperado en la ruta"
   DISPLAY " /safreviv_int/agr/envio con el siguiente nombre: ",v_arh_salida
   DISPLAY ""

   CALL fn_crea_indices_tmp_agrs()

   DISPLAY "=FIN="

END MAIN 

#Objetivo: Función que recupera información de las peticiones para el PDF
FUNCTION inf_sol_reporte()

   DEFINE r_solicitud RECORD
      nss                 CHAR(11),
      id_derechohabiente  DECIMAL(9,0),
      id_referencia       DECIMAL(9,0),
      aivs92              DECIMAL(12,2),
      aivs97              DECIMAL(12,2),
      estado              SMALLINT,
      edo_procesar        SMALLINT
   END RECORD

   DEFINE r_inf_archivo RECORD
      f_genera      DATE,
      nss           CHAR(11),
      aivs92        CHAR(15),
      aivs97        CHAR(15),
      tipo          CHAR(2),
      num_intento   CHAR(4)
   END RECORD

   DEFINE v_s_qryTxt     STRING
   DEFINE v_reporte_bin  STRING
   DEFINE v_ruta_rpt     STRING
   DEFINE v_ruta_archivo STRING
   DEFINE archivo        base.Channel
   DEFINE v_detalle      STRING
   DEFINE v_aux_dh       DECIMAL(9,0)

   --Inicializa records
   INITIALIZE r_solicitud.* TO NULL
   INITIALIZE r_inf_archivo.* TO NULL

   --Inicializa valores del arreglo
   --Estado = 18
   LET arr_sol_saldo[1].estado_desc = "En Trámite"
   LET arr_sol_saldo[1].t_registros = 0
   LET arr_sol_saldo[1].aivs92      = 0
   LET arr_sol_saldo[1].aivs97      = 0
   --Estado = 20
   LET arr_sol_saldo[2].estado_desc = "Solicitud Nueva UG/Anualidad"
   LET arr_sol_saldo[2].t_registros = 0
   LET arr_sol_saldo[2].aivs92      = 0
   LET arr_sol_saldo[2].aivs97      = 0
   --Estado = 25
   LET arr_sol_saldo[3].estado_desc = "Saldo Remanente"
   LET arr_sol_saldo[3].t_registros = 0
   LET arr_sol_saldo[3].aivs92      = 0
   LET arr_sol_saldo[3].aivs97      = 0
   --Estado = 140
   LET arr_sol_saldo[4].estado_desc = "Conciliación por Adelanto"
   LET arr_sol_saldo[4].t_registros = 0
   LET arr_sol_saldo[4].aivs92      = 0
   LET arr_sol_saldo[4].aivs97      = 0
   --Estado = 142
   LET arr_sol_saldo[5].estado_desc = "Casos Especiales"
   LET arr_sol_saldo[5].t_registros = 0
   LET arr_sol_saldo[5].aivs92      = 0
   LET arr_sol_saldo[5].aivs97      = 0
   --Estado = 145
   LET arr_sol_saldo[6].estado_desc = "Saldo Deudor Liquidado"
   LET arr_sol_saldo[6].t_registros = 0
   LET arr_sol_saldo[6].aivs92      = 0
   LET arr_sol_saldo[6].aivs97      = 0
   --Solisitudes de meses anteriores
   LET arr_sol_saldo[7].estado_desc = "Solicitud Mes Anterior de UG/Anualidad"
   LET arr_sol_saldo[7].t_registros = 0
   LET arr_sol_saldo[7].aivs92      = 0
   LET arr_sol_saldo[7].aivs97      = 0

   --Inicializa record del total global
   LET r_total_global.t_registros = 0
   LET r_total_global.aivs92      = 0
   LET r_total_global.aivs97      = 0

   LET v_arh_salida   = "Adetalle_",TODAY USING "yyyymmdd",".cag"
   LET v_ruta_archivo = v_c_ruta_envio CLIPPED,"/",v_arh_salida

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   DECLARE crs_tipo_AG CURSOR FOR
   SELECT t.nss,
          t.id_derechohabiente,
          t.id_referencia,
          t.aivs92,
          t.aivs97,
          c.estado,
          c.edo_procesar
     FROM safre_tmp:tmp_agr_solic_sdo t,
          cre_acreditado c
    WHERE t.id_referencia      = c.id_cre_acreditado
      AND t.modulo_cod         = 'AG'

   LET r_inf_archivo.f_genera    = TODAY
   LET r_inf_archivo.num_intento = "0001"

   FOREACH crs_tipo_AG INTO r_solicitud.nss,
                            r_solicitud.id_derechohabiente,
                            r_solicitud.id_referencia,
                            r_solicitud.aivs92,
                            r_solicitud.aivs97,
                            r_solicitud.estado,
                            r_solicitud.edo_procesar

      LET r_inf_archivo.tipo = "  "
      LET v_aux_dh = NULL 

      IF(r_solicitud.aivs92 IS NULL) THEN
         LET r_solicitud.aivs92 = 0
      END IF

      IF(r_solicitud.aivs97 IS NULL) THEN
         LET r_solicitud.aivs97 = 0
      END IF

      #Incrementa totales globales
      LET r_total_global.t_registros = r_total_global.t_registros + 1
      LET r_total_global.aivs92      = r_total_global.aivs92  + r_solicitud.aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97  + r_solicitud.aivs97

      --Asigna información para el archivo de salida
      LET r_inf_archivo.nss    = r_solicitud.nss
      LET r_inf_archivo.aivs92 = r_solicitud.aivs92
      LET r_inf_archivo.aivs97 = r_solicitud.aivs97

      #Evalúa si es una solicitud del mes anterior
      SELECT MAX(id_derechohabiente)
        INTO v_aux_dh
        FROM safre_tmp:tmp_agr_solic_sdo2
      WHERE id_derechohabiente = r_solicitud.id_derechohabiente
        AND id_referencia      = r_solicitud.id_referencia
        AND modulo_cod = 'AG'
        
      IF(v_aux_dh IS NOT NULL) THEN
         --Fila registros del mes anterior
         LET arr_sol_saldo[7].t_registros = arr_sol_saldo[7].t_registros + 1
         LET arr_sol_saldo[7].aivs92      = arr_sol_saldo[7].aivs92 + r_solicitud.aivs92
         LET arr_sol_saldo[7].aivs97      = arr_sol_saldo[7].aivs97 + r_solicitud.aivs97

         LET r_inf_archivo.tipo        = "UR"   --Tipo Meses anteriores
         LET r_inf_archivo.num_intento = "0002" --Se entiende que es la segunda vez que se está solicitando
      ELSE
         LET r_inf_archivo.num_intento = "0001"
         #Evalúa el tipo de solicitud del mes actual
         CASE
            --En trámite
            WHEN r_solicitud.estado = 18
               LET arr_sol_saldo[1].t_registros = arr_sol_saldo[1].t_registros + 1
               LET arr_sol_saldo[1].aivs92      = arr_sol_saldo[1].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[1].aivs97      = arr_sol_saldo[1].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "TM"

            --Solicitud nueva
            WHEN r_solicitud.estado = 20
               LET arr_sol_saldo[2].t_registros = arr_sol_saldo[2].t_registros + 1
               LET arr_sol_saldo[2].aivs92      = arr_sol_saldo[2].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[2].aivs97      = arr_sol_saldo[2].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "UA"

            --Saldo remanente
            WHEN r_solicitud.estado = 25
               LET arr_sol_saldo[3].t_registros = arr_sol_saldo[3].t_registros + 1
               LET arr_sol_saldo[3].aivs92      = arr_sol_saldo[3].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[3].aivs97      = arr_sol_saldo[3].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "SR"

            --Conciliación por adelanto
            WHEN r_solicitud.estado = 140
               LET arr_sol_saldo[4].t_registros = arr_sol_saldo[4].t_registros + 1
               LET arr_sol_saldo[4].aivs92      = arr_sol_saldo[4].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[4].aivs97      = arr_sol_saldo[4].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "CA"

            --Saldo deudor liquidado
            WHEN r_solicitud.estado = 145
               LET arr_sol_saldo[6].t_registros = arr_sol_saldo[6].t_registros + 1
               LET arr_sol_saldo[6].aivs92      = arr_sol_saldo[6].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[6].aivs97      = arr_sol_saldo[6].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "DE"

         END CASE
      END IF

      LET v_detalle = r_inf_archivo.f_genera USING "yyyymmdd",
                      r_inf_archivo.nss,
                      r_inf_archivo.aivs92 USING "&&&&&&&&&.&&&&&",
                      r_inf_archivo.aivs97 USING "&&&&&&&&&.&&&&&",
                      r_inf_archivo.tipo,
                      r_inf_archivo.num_intento

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)

   END FOREACH

   DECLARE crs_tipo_UA CURSOR FOR
   SELECT t.nss,
          t.id_derechohabiente,
          t.id_referencia,
          t.aivs92,
          t.aivs97,
          u.estado,
          u.edo_procesar
     FROM safre_tmp:tmp_agr_solic_sdo t,
          cre_uso_garantia u
    WHERE t.id_referencia = u.id_cre_uso_garantia
      AND t.modulo_cod    = 'UA';

   INITIALIZE r_solicitud.* TO NULL

   FOREACH crs_tipo_UA INTO r_solicitud.nss,
                            r_solicitud.id_derechohabiente,
                            r_solicitud.id_referencia,
                            r_solicitud.aivs92,
                            r_solicitud.aivs97,
                            r_solicitud.estado,
                            r_solicitud.edo_procesar
                            
      LET r_inf_archivo.tipo = "  "
      LET v_aux_dh = NULL

      IF(r_solicitud.aivs92 IS NULL) THEN
         LET r_solicitud.aivs92 = 0
      END IF

      IF(r_solicitud.aivs97 IS NULL) THEN
         LET r_solicitud.aivs97 = 0
      END IF

      #Incrementa totales globales
      LET r_total_global.t_registros = r_total_global.t_registros + 1
      LET r_total_global.aivs92      = r_total_global.aivs92  + r_solicitud.aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97  + r_solicitud.aivs97

      --Asigna información para el archivo de salida
      LET r_inf_archivo.nss    = r_solicitud.nss
      LET r_inf_archivo.aivs92 = r_solicitud.aivs92
      LET r_inf_archivo.aivs97 = r_solicitud.aivs97

      #Evalúa si es una solicitud del mes anterior
      SELECT MAX(id_derechohabiente)
        INTO v_aux_dh
        FROM safre_tmp:tmp_agr_solic_sdo2
      WHERE id_derechohabiente = r_solicitud.id_derechohabiente
        AND id_referencia      = r_solicitud.id_referencia
        AND modulo_cod = 'UA'
        
      IF(v_aux_dh IS NOT NULL) THEN
         --Fila registros del mes anterior
         LET arr_sol_saldo[7].t_registros = arr_sol_saldo[7].t_registros + 1
         LET arr_sol_saldo[7].aivs92      = arr_sol_saldo[7].aivs92 + r_solicitud.aivs92
         LET arr_sol_saldo[7].aivs97      = arr_sol_saldo[7].aivs97 + r_solicitud.aivs97

         LET r_inf_archivo.tipo        = "UR"   --Tipo Meses anteriores
         LET r_inf_archivo.num_intento = "0002" --Se entiende que es la segunda vez que se está solicitando
      ELSE
         LET r_inf_archivo.num_intento = "0001"
         #Evalúa el tipo de solicitud del mes actual
         CASE
            --Solicitud nueva
            WHEN r_solicitud.estado = 20
               LET arr_sol_saldo[2].t_registros = arr_sol_saldo[2].t_registros + 1
               LET arr_sol_saldo[2].aivs92      = arr_sol_saldo[2].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[2].aivs97      = arr_sol_saldo[2].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "UA"

            --Conciliación por adelanto
            WHEN r_solicitud.estado = 140
               LET arr_sol_saldo[4].t_registros = arr_sol_saldo[4].t_registros + 1
               LET arr_sol_saldo[4].aivs92      = arr_sol_saldo[4].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[4].aivs97      = arr_sol_saldo[4].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "CA"

            --Caso Especial
            WHEN r_solicitud.estado = 142
               LET arr_sol_saldo[5].t_registros = arr_sol_saldo[5].t_registros + 1
               LET arr_sol_saldo[5].aivs92      = arr_sol_saldo[5].aivs92 + r_solicitud.aivs92
               LET arr_sol_saldo[5].aivs97      = arr_sol_saldo[5].aivs97 + r_solicitud.aivs97

               LET r_inf_archivo.tipo = "CE"

         END CASE
      END IF

      LET v_detalle = r_inf_archivo.f_genera USING "yyyymmdd",
                      r_inf_archivo.nss,
                      r_inf_archivo.aivs92 USING "&&&&&&&&&.&&&&&",
                      r_inf_archivo.aivs97 USING "&&&&&&&&&.&&&&&",
                      r_inf_archivo.tipo,
                      r_inf_archivo.num_intento

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)

   END FOREACH

   CALL archivo.close()

   #Calcula porcentajes %
   LET v_aux_porcentaje = 0
   --Porcentaje total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100
   LET r_total_global.porcentaje = v_aux_porcentaje CLIPPED,"%"

   --finaliza operación
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
   END IF

   SELECT fecha_ini,fecha_fin
     INTO v_f_opera_ini,v_f_opera_fin
     FROM bat_ctr_operacion
    WHERE pid         = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod   = p_i_opera_cod

   #################################################
   #   CONFIGURACION PARA SALIDA DEL REPORTE PDF   #
   #################################################
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRS041.4rp"
   LET v_ruta_rpt    = v_c_ruta_listado CLIPPED,"/",
                       p_v_usuario CLIPPED,"-",v_c_programa_cod CLIPPED,"-",
                       p_d_pid USING "&&&&&","-",
                       p_i_proceso_cod USING "&&&&&","-",
                       p_i_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN

         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF

      END IF
   ELSE
       DISPLAY "ERROR: No fue posible abrir plantilla del reporte"

   END IF

END FUNCTION 

#Objetivo: Funcion que consulta los datos de la tabla afi derechohabiente
FUNCTION f_obt_datos_trab(p_id_derechohabiente)

   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente -- identificador del derechohabiente

   DEFINE v_r_afi_derechohab RECORD -- registro de afi derechohabiente
      curp              LIKE afi_derechohabiente.curp,
      nss               LIKE afi_derechohabiente.nss,
      rfc               LIKE afi_derechohabiente.rfc,
      ap_paterno_af     LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_af     LIKE afi_derechohabiente.ap_materno_af,
      nombre_af         LIKE afi_derechohabiente.nombre_af,
      nombre_imss       LIKE afi_derechohabiente.nombre_imss
   END RECORD

   DEFINE v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene la información del trabajador
   --curp, nss, rfc, ap_paterno_af, ap_materno_af, nombre_af, nombre_imss
   LET v_s_qryTxt = " SELECT curp, nss, rfc, ap_paterno_af, ap_materno_af, nombre_af, nombre_imss\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_id_derechohabiente

   PREPARE prp_afi_derechohabiente FROM v_s_qryTxt
   EXECUTE prp_afi_derechohabiente INTO v_r_afi_derechohab.*

   RETURN v_r_afi_derechohab.*

END FUNCTION

#OBJETIVO: Genera Reporte PDF
REPORT genera_PDF()

   DEFINE v_f_presentacion DATE
   DEFINE v_desc_operacion CHAR(60)
   DEFINE f                INTEGER

   FORMAT
   FIRST PAGE HEADER
      LET v_f_presentacion = TODAY
      LET v_desc_operacion = "Solicitud de Saldos AGR"

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX v_f_presentacion USING "dd/mm/yyyy"

      #RESUMEN
      PRINTX p_v_arch_proceso           --Nombre del archivo
      PRINTX v_f_opera_ini
      PRINTX v_f_opera_fin
      PRINTX v_desc_operacion
      PRINTX r_total_global.t_registros

      IF(r_total_global.aivs92 < 0) THEN
         LET r_total_global.aivs92 = r_total_global.aivs92 * -1 
      END IF 
      
      IF(r_total_global.aivs97 < 0) THEN
         LET r_total_global.aivs97 = r_total_global.aivs97 * -1
      END IF 
      
      PRINTX r_total_global.aivs92
      PRINTX r_total_global.aivs97
      PRINTX r_total_global.porcentaje

   ON EVERY ROW
      --rechazos solicitudes nuevas
      FOR f = 1 TO arr_sol_saldo.getLength()

         PRINTX arr_sol_saldo[f].estado_desc
         PRINTX arr_sol_saldo[f].t_registros

         -- Si el monto aivs92 y aivs97 es negativo se deja como valor absoluto positivo
         IF(arr_sol_saldo[f].aivs92 < 0) THEN
            LET arr_sol_saldo[f].aivs92 = arr_sol_saldo[f].aivs92 * -1 
         END IF 

         IF(arr_sol_saldo[f].aivs97 < 0) THEN
            LET arr_sol_saldo[f].aivs97 = arr_sol_saldo[f].aivs97 * -1 
         END IF 
         
         PRINTX arr_sol_saldo[f].aivs92
         PRINTX arr_sol_saldo[f].aivs97

         --Calcula porcentaje por cada fila
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_sol_saldo[f].t_registros / r_total_global.t_registros) * 100
         LET arr_sol_saldo[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_sol_saldo[f].porcentaje
      END FOR

END REPORT

#Objetivo: Función que obtiene el total de AVIS registros liquidados
FUNCTION f_obt_aivs_liq(v_id_derechohabiente, v_folio_liquida, v_subcta)

   DEFINE v_id_derechohabiente LIKE cre_acreditado.id_derechohabiente -- id derechohabiente
   DEFINE v_folio_liquida      LIKE glo_folio.folio -- folio de liquidación
   DEFINE v_d_monto_aivs       DECIMAL(18,6) -- monto total en aivs
   --DEFINE v_s_qryTxt           STRING -- se asigna una sentencia sql a ejecutar
   DEFINE v_subcta             SMALLINT

   -- se realiza la consulta de las aivs en la tabla temporal
   LET v_s_qryTxt = " SELECT SUM(monto_acciones)\n",
                    "   FROM safre_tmp:tmp_cta_mov_agrs04\n",
                    "  WHERE folio_liquida = ",v_folio_liquida,"\n",
                    "    AND id_derechohabiente = ",v_id_derechohabiente,"\n",
                    "    AND subcuenta = ",v_subcta,"\n",
                    "    AND monto_acciones < 0"

   PREPARE prp_monto_acc_tmp FROM v_s_qryTxt
   EXECUTE prp_monto_acc_tmp INTO v_d_monto_aivs

   IF v_d_monto_aivs IS NULL THEN
      LET v_d_monto_aivs = 0
   END IF

   RETURN v_d_monto_aivs

END FUNCTION

#Objetivo: Función que crea tabla temporal la cual tiene por objetivo diferenciar el módulo
#          correspondiente a cada NSS que llega de Infonavit (para el proceso de No Atendidas)
FUNCTION fn_crea_tmp_solic_sdo_agr2()

   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- Se ejecuta la función que crea las temporales
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_tmp_solic_sdo_agr2()"

   PREPARE prp_sdo_agr2 FROM v_s_qryTxt

   EXECUTE prp_sdo_agr2

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

#Objetivo: Función que crea tabla temporal la cual tiene por objetivo diferenciar el módulo
#          correspondiente a cada NSS que llega de Infonavit
FUNCTION fn_crea_tmp_solic_sdo_agr()
   DEFINE v_ax_envio         LIKE seg_modulo.ruta_envio
   DEFINE v_ruta_resp        STRING

   SELECT ruta_envio
    INTO v_ax_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   LET v_ruta_resp = v_ax_envio CLIPPED || "/" || "tmp_agr_solic_sdo.unl"

   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp
--******************************************************************************
      -- Se ejecuta la función que crea las temporales
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_tmp_solic_sdo_agr()"

   PREPARE prp_sdo_agr FROM v_s_qryTxt
   EXECUTE prp_sdo_agr
--******************************************************************************
   -- Se realiza el respaldo de la tabla tmp_agr_solic_sdo
   UNLOAD TO v_ruta_resp CLIPPED
   SELECT *
     FROM tmp_agr_solic_sdo

   -- se pasa los registros del proceso anterior a la tabla de No atendidas
   INSERT INTO tmp_agr_solic_sdo2
   SELECT *
     FROM tmp_agr_solic_sdo

   LET v_ruta_resp = v_ax_envio CLIPPED || "/" || "tmp_agr_solic_sdo_ua.unl"

   -- Se realiza el respaldo de la tabla tmp_agr_solic_sdo_ua
   UNLOAD TO v_ruta_resp CLIPPED
   SELECT *
     FROM tmp_agr_solic_sdo_ua

   -- se pasa los registros del proceso anterior a la tabla de No atendidas
   INSERT INTO tmp_agr_solic_sdo_ua2
   SELECT *
     FROM tmp_agr_solic_sdo_ua
{
   -- Se ejecuta la función que crea las temporales
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_tmp_solic_sdo_agr()"

   PREPARE prp_sdo_agr FROM v_s_qryTxt
   EXECUTE prp_sdo_agr
}
   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

#Objetivo: Función que crea tabla temporal la cual tiene por objetivo agilizar la consulta
#          a cta movimiento
FUNCTION fn_crea_tmp_cta_mov_agrs04()

   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- Se ejecuta la función que crea las temporales
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_tmp_cta_mov_agrs04()"

   PREPARE prp_mov_agrs4 FROM v_s_qryTxt

   EXECUTE prp_mov_agrs4

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

FUNCTION fn_crea_indices_agrs04()

   DEFINE v_sql_ix STRING

   DATABASE safre_tmp

   -- en caso de error continua
   WHENEVER ERROR CONTINUE
      -- se ejecuta el script para crear indices y estadísticas
      LET v_sql_ix = " CREATE INDEX ix_tmp_agrs041\n",
                     "  ON tmp_cta_mov_agrs04 (folio_liquida,id_derechohabiente,subcuenta) USING btree ;"

      PREPARE prp_index_mov FROM v_sql_ix
      EXECUTE prp_index_mov

      LET v_sql_ix = "UPDATE STATISTICS FOR TABLE tmp_cta_mov_agrs04"

      PREPARE prp_actualiza_est FROM v_sql_ix
      EXECUTE prp_actualiza_est
   WHENEVER ERROR STOP
   -- regresamos a la base de datos safre viv

   DATABASE safre_viv

END FUNCTION

FUNCTION fn_crea_indices_tmp_agrs()

   DEFINE v_sql_ix STRING

   DATABASE safre_tmp

   -- en caso de error continua
   WHENEVER ERROR CONTINUE
      -- se ejecuta el script para crear indices y estadísticas
      LET v_sql_ix = " CREATE INDEX ix_tmp_agr_sol_sdo1\n",
                     "  ON tmp_agr_solic_sdo (nss) USING btree ;"

      PREPARE prp_index_t1 FROM v_sql_ix
      EXECUTE prp_index_t1

      LET v_sql_ix = "UPDATE STATISTICS FOR TABLE tmp_agr_solic_sdo"

      PREPARE prp_actualiza_st_t1 FROM v_sql_ix
      EXECUTE prp_actualiza_st_t1

      LET v_sql_ix = " CREATE INDEX ix_tmp_agr_sol_sdo21\n",
                     "  ON tmp_agr_solic_sdo2 (nss) USING btree ;"

      PREPARE prp_index_t2 FROM v_sql_ix
      EXECUTE prp_index_t2

      LET v_sql_ix = "UPDATE STATISTICS FOR TABLE tmp_agr_solic_sdo2"

      PREPARE prp_actualiza_st_t2 FROM v_sql_ix
      EXECUTE prp_actualiza_st_t2

   WHENEVER ERROR STOP
   -- regresamos a la base de datos safre viv

   DATABASE safre_viv

END FUNCTION

#Objetivo: Función que obtiene el total de AVIS registros por liquidar
FUNCTION f_obt_aivs_rem(v_id_derechohabiente, v_folio_liquida, v_subcta)

   DEFINE v_id_derechohabiente LIKE cre_acreditado.id_derechohabiente -- id derechohabiente
   DEFINE v_folio_liquida      LIKE glo_folio.folio -- folio de liquidación
   DEFINE v_d_monto_aivs       DECIMAL(18,6) -- monto total en aivs
   DEFINE v_subcta             SMALLINT

   LET v_d_monto_aivs = 0

   -- se realiza la consulta de las aivs en la tabla temporal
   LET v_s_qryTxt = " SELECT SUM(monto_acciones)\n",
                    "   FROM cta_movimiento\n",
                    "  WHERE id_derechohabiente = ",v_id_derechohabiente,"\n",
                    "    AND subcuenta = ",v_subcta,"\n",
                    "    AND fondo_inversion = 11"

   PREPARE prp_monto_aivs_rem FROM v_s_qryTxt
   EXECUTE prp_monto_aivs_rem INTO v_d_monto_aivs

   IF v_d_monto_aivs IS NULL OR v_d_monto_aivs < 0 THEN
      LET v_d_monto_aivs = 0
   END IF

   RETURN v_d_monto_aivs

END FUNCTION
{
FUNCTION fn_crea_tabla()

DATABASE safre_tmp
DROP TABLE IF EX    ISTS tmp_agr_solic_sdo

create table tmp_agr_solic_sdo
  (
    nss char(11),
    id_derechohabiente decimal(9,0),
    modulo_cod char(2),
    f_proceso date,
    id_referencia decimal(9,0),
    aivs97 decimal(12,2),
    aivs92 decimal(12,2)
  )
  DATABASE safre_viv

END FUNCTION
}

