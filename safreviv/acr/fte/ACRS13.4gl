--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#####################################################################
#Modulo            => ACR                                           #
#Programa          => ACRS13                                        #
#Objetivo          => Programa que ejecuta el proceso de generación #
#                     de archivo solicitud de saldos, para el       #
#                     módulo de Transferencia de Acreditados        #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 25 ENERO 2012                                 #
#Modificación      => 16 FEBRERO 2012                               #
#                  => Se creó el lanzado del proceso de solicitud   #
#                     de saldos                                     #
#Modifica:         => Mauro Muñiz Caballero                         #
#Fecha modif:      => 9 de noviembre de 2015                        #
#Adecuación        => Eliminación de adelantos                      #
#####################################################################

DATABASE safre_viv

GLOBALS "ACRG10.4gl"

GLOBALS

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso          LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   DEFINE v_i_tpo_originacion       LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_c_tpo_transferencia     LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia

   DEFINE v_r_cre_acred RECORD
      id_cre_acred                  LIKE cre_acreditado.id_cre_acreditado, -- id cre acreditado
      id_derhabiente                LIKE cre_acreditado.id_derechohabiente, -- id derechohabiente
      num_credito                   LIKE cre_acreditado.num_credito, -- número de crédito
      estado                        LIKE cre_acreditado.estado,       -- estado|1
      edo_procesar                  LIKE cre_acreditado.edo_procesar -- estado procesar
   END RECORD

   DEFINE r_r_afi_derechohab RECORD -- registro de afi derechohabiente
      curp                          LIKE afi_derechohabiente.curp,
      nss                           LIKE afi_derechohabiente.nss,
      rfc                           LIKE afi_derechohabiente.rfc,
      ap_paterno_af                 LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_af                 LIKE afi_derechohabiente.ap_materno_af,
      nombre_af                     LIKE afi_derechohabiente.nombre_af,
      nombre_imss                   LIKE afi_derechohabiente.nombre_imss
   END RECORD

   DEFINE v_r_encabezado RECORD
      tpo_registro                  CHAR(2), -- Tipo de Registro (001-002)
      id_servicio                   CHAR(2), -- Identificador de Servicio (003-004)
      id_operacion                  CHAR(2), -- Identificador de Operación (005-006)
      tpo_ent_origen                CHAR(2), -- Tipo de entidad origen (007-008)
      cve_ent_origen                CHAR(3), -- Clave de entidad origen (009-011)
      tpo_ent_destino               CHAR(2), -- Tipo de entidad destino (012-013)
      cve_ent_destino               CHAR(3), -- Clave de entidad destino (014-016)
      ent_fed_env_lote              CHAR(3), -- Entidad federativa de envío de lote (017-019)
      fec_presentacion              CHAR(8), -- Fecha de presentación (020-027)
      consec_lote                   CHAR(3), -- Consecutivo del lote en el día (028-030)
      cve_modal_recep               CHAR(2), -- Clave de modalidad de recepción (031-032)
      cod_resul_opera               CHAR(2), -- Código de resultado de la Operación (033-034)
      mot_rechazo                   CHAR(9), -- Motivo de rechazo del lote (035-043)
      filler                        CHAR(687)-- Filler (044-730)
   END RECORD

   DEFINE v_r_detalle RECORD
      tpo_registro                  CHAR(2), -- Tipo de Registro (001-002)
      cont_servicio                 CHAR(10), -- Contador de Servicio (003-012)
      tpo_ent_recept                CHAR(2), -- Tipo de entidad receptora de la cuenta (013-014)
      cve_ent_recept                CHAR(3), -- Clave de entidad receptora de la cuenta (015-017)
      tpo_ent_cedente               CHAR(2), -- Tipo de entidad cedente de la cuenta (018-019)
      cve_ent_cedente               CHAR(3), -- Clave de entidad ced. de la cuenta (020-022)
      tpo_transferencia             CHAR(2), -- Origen/Tipo de transferencia (023-024)
      f_presentacion                CHAR(8), -- Fecha de presentación (025-032)
      filler1                       CHAR(8), -- Filler1 (033-040)
      curp_trabajador               CHAR(18), -- CURP del trabajador (041-058)
      nss_trab_infonavit            CHAR(11), -- NSS del trabajador según INFONAVIT (059-069)
      filler2                       CHAR(15), -- Filler2 (070-084)
      rfc_trab_infonavit            CHAR(13), -- RFC del trabajador según INFONAVIT (085-097)
      ape_pat_infonavit             CHAR(40), -- Apellido paterno del trabajador en el INFONAVIT (098-137)
      ape_mat_infonavit             CHAR(40), -- Apellido materno del trabajador en el INFONAVIT (138-177)
      nom_trab_infonavit            CHAR(40), -- Nombres del trabajador en el INFONAVIT (178-217)
      filler3                       CHAR(22), -- Filler3 (218-239)
      id_lote_solic                 CHAR(16), -- Identificador de lote de la solicitud (240-255)
      filler4                       CHAR(15), -- Filler4 (256-270)
      nss_trab_afore                CHAR(11), -- NSS del trabajador según AFORE/PS. (271-281)
      rfc_trab_afore                CHAR(13), -- RFC del trabajador según AFORE (282-294)
      filler5                       CHAR(30), -- Filler5 (295-324)
      ape_pat_afore                 CHAR(40), -- Apellido paterno del trabajador en la AFORE cedente (325-364)
      ape_mat_afore                 CHAR(40), -- Apellido materno del trabajador en la AFORE cedente (365-404)
      nom_trab_afore                CHAR(40), -- Nombres del trabajador en la AFORE cedente (405-444)
      filler6                       CHAR(30), -- Filler6 (445-474)
      num_apl_int_viv97             CHAR(15), -- Número de "Aplicaciones de Intereses de Vivienda" 97 de la última aportación (475-489)
      ult_aport_vivi97              CHAR(15), -- Ultima aportación Vivienda 97 (490-504)
      filler7                       CHAR(78), -- Filler7 (505-582)
      cod_result_opera              CHAR(2), -- Código Resultado de la Operación (583-584)
      diagnostico_proc              CHAR(15), -- Diagnóstico del Proceso (585-599)
      nom_trab_imss                 CHAR(50), -- Nombre del Trabajador según IMSS (600-649)
      num_cred_infonavit            CHAR(10), -- Número de Crédito INFONAVIT (650-659)
      filler8                       CHAR(53), -- Filler8 (660-712)
      periodo_pago                  CHAR(6), -- Período de pago (713-718)
      filler9                       CHAR(12) -- Filler9 (719-730)
   END RECORD

   DEFINE v_r_sumario RECORD
      tpo_registro                  CHAR(2), -- tipo de registro (001-002)
      cant_regs_detalle             CHAR(9), -- cantidad de registros de detalle (003-011)
      filler1                       CHAR(30), -- filler1 (012-041)
      sum_numAplInt_viv97           CHAR(15), -- suma del número de "aplicaciones intereses de vivienda" 97 de la última aportación (042-056)
      sum_ult_aport_viv97           CHAR(15), -- suma de la ultima aportación de vivienda 97 (057-071)
      filler2                       CHAR(659) -- filler2 (072-730)
   END RECORD

   DEFINE v_r_tmp_acr_solic_sdo RECORD
      nss                           CHAR(11),
      id_derechohabiente            DECIMAL(9,0),
      id_cre_acreditado             DECIMAL(9,0),
      modulo_cod                    CHAR(2),
      f_proceso                     DATE
   END RECORD

   DEFINE v_cre_his_acreditado      RECORD LIKE cre_his_acreditado.* -- registro de historicos
   DEFINE v_d_id_cre_ctr_archivo    LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_edo_procesar_aux        LIKE cre_acreditado.edo_procesar -- estado procesar a actualizar
   DEFINE v_dt_fec_present          DATE -- fecha de presentacion
   DEFINE v_c_fec_present_ax        CHAR(8) -- fecha ayxiliar de presentacion con formato YYYYMMDD
   DEFINE v_dt_f_solic_saldos       DATE -- fecha para la generación de solicitud de saldos
   DEFINE v_v_ruta_nomarch          VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp       VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_ch_arch_solTransf       BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_i_contrador_reg         INTEGER -- contrador de registros
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_r_cre_ctr_archivo       RECORD LIKE cre_ctr_archivo.* -- regsitro de cre ctr archivo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_manejador_rpt           OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_archivo_nom             STRING
   DEFINE v_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin
   DEFINE v_i_lote                  LIKE dse_ctr_archivo.lote -- lote del archivo
   DEFINE v_i_total_regis_arch      INTEGER -- total de registros en el archivo
   DEFINE v_i_total_regis_sin       INTEGER -- total de registros en el archivo
   DEFINE v_ejecuta_sh              STRING  --automatización de envío de archivo

   --Arreglo para inf. del reporte PDF
   DEFINE arr_sol_saldo DYNAMIC ARRAY OF RECORD
      estado_desc    CHAR(50),
      t_registros    INTEGER,
      aivs92         DECIMAL(16,2),
      aivs97         DECIMAL(16,2),
      porcentaje     CHAR(12) 
   END RECORD 
   --Record para el total global de registros procesados
   DEFINE r_total_global RECORD
      t_registros    INTEGER,
      aivs92         DECIMAL(16,2),
      aivs97         DECIMAL(16,2),
      porcentaje     CHAR(12) 
   END RECORD
   DEFINE v_arh_salida     STRING
   DEFINE v_aux_porcentaje DECIMAL(6,2)
   DEFINE v_f_opera_ini    LIKE bat_ctr_operacion.fecha_ini
   DEFINE v_f_opera_fin    LIKE bat_ctr_operacion.fecha_fin
   DEFINE v_ruta_archivo   STRING

END GLOBALS

MAIN

      -- se recuperan los parámetros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRS13.log")

   DISPLAY "=INICIA ACRS13="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY ""

   -- se inicializan variables
   LET v_i_tpo_originacion   = 1 -- Transferencia de acreditados
   LET v_c_tpo_transferencia = "03" -- Transferencia de acreditados

   -- se crea tabla temporal la cual identifica que registros se procesaron en la generación del archivo

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

   PREPARE prp_slc_ruta_envio FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio INTO v_c_ruta_envio

   -- se crea la fecha. Primer dia del mes
   LET v_dt_fec_present = TODAY - DAY(TODAY) + 1   

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_dt_fec_present CLIPPED, "',2)"

   PREPARE prp_obt_3habil FROM v_s_qryTxt
   EXECUTE prp_obt_3habil INTO v_dt_fec_present

   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_dt_fec_present < TODAY THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_dt_fec_present = TODAY - DAY(TODAY) + 1
      LET v_dt_fec_present = v_dt_fec_present + 1 UNITS MONTH

      PREPARE prp_obt_3habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_dt_fec_present CLIPPED||"',2)"
      EXECUTE prp_obt_3habil_sig INTO v_dt_fec_present
   END IF

   DISPLAY " Fecha presentación: ",v_dt_fec_present
   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fec_present_ax = v_dt_fec_present USING "YYYYMMDD"

   -- se obtiene el maximo lote para la fecha de presentación
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_fec_present,"'\n",
                    "    AND id_proceso = ",g_proc_cod_acr_arch_solic

   PREPARE prp_max_lote FROM v_s_qryTxt
   EXECUTE prp_max_lote INTO v_i_lote

   -- se valida el lote
   IF v_i_lote IS NULL THEN
      LET v_i_lote = 1
   ELSE
      LET v_i_lote = v_i_lote + 1
   END IF

   --agregar nuevos campos de la tabla cre ctr archivo
   LET v_r_cre_ctr_archivo.folio_archivo      = p_d_folio
   LET v_r_cre_ctr_archivo.lote               = v_i_lote
   LET v_r_cre_ctr_archivo.f_lote             = v_dt_fec_present
   LET v_r_cre_ctr_archivo.id_proceso         = g_proc_cod_acr_arch_solic
   LET v_r_cre_ctr_archivo.operacion          = 0
   LET v_r_cre_ctr_archivo.nom_archivo        = p_v_arch_proceso
   LET v_r_cre_ctr_archivo.tot_registros      = 0
   LET v_r_cre_ctr_archivo.tot_aceptados      = 0
   LET v_r_cre_ctr_archivo.tot_rechazados     = 0
   LET v_r_cre_ctr_archivo.tot_sin_origen     = 0
   LET v_r_cre_ctr_archivo.estado             = 10
   LET v_r_cre_ctr_archivo.f_proceso          = TODAY
   LET v_r_cre_ctr_archivo.usuario            = p_v_usuario

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
                    "    AND id_proceso = ",g_proc_cod_acr_arch_solic,"\n",
                    "    AND operacion = 0"
                    
   PREPARE prp_id_cre_ctr_archivo FROM v_s_qryTxt
   EXECUTE prp_id_cre_ctr_archivo INTO v_d_id_cre_ctr_archivo

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso

   -- se inicializa el contador de registros
   LET v_i_contrador_reg    = 0
   LET v_i_total_regis_arch = 0
   LET v_i_total_regis_sin  = 0

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
   LET v_r_encabezado.tpo_ent_destino  = "01"
   LET v_r_encabezado.cve_ent_destino  = "" -- 3 espacios en blanco
   LET v_r_encabezado.ent_fed_env_lote = "009"
   LET v_r_encabezado.fec_presentacion = v_c_fec_present_ax
   LET v_r_encabezado.consec_lote      = "001"
   LET v_r_encabezado.cve_modal_recep  = "02"
   LET v_r_encabezado.cod_resul_opera  = (2 SPACES) -- 2 espacios en blanco
   LET v_r_encabezado.mot_rechazo      = "" -- 9 espacios en blanco
   LET v_r_encabezado.filler           = "" -- 687 espacios en blanco

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
                      v_r_encabezado.cve_modal_recep,
                      v_r_encabezado.cod_resul_opera,
                      v_r_encabezado.mot_rechazo,
                      v_r_encabezado.filler

   -- se escribe el registro (encabezado) en el archivo
   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- se obtiene la fecha del ultimo día del mes anterior de la fecha de presentación
   LET v_dt_f_solic_saldos = v_dt_fec_present - DAY(v_dt_fec_present)
   LET v_dt_f_solic_saldos = TODAY

   -- todos los registro de cre acreditado con edo_procesar = 70 y con estado = 140 (liq) y 145 (remanentes) ,25(adelantos) y 20 sin adelantos
   LET v_s_qryTxt = " SELECT id_cre_acreditado, id_derechohabiente, num_credito, estado, edo_procesar\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado IN(140,145,142,25,20)\n",
                    "    AND edo_procesar = 70\n",
                    "    AND tpo_originacion = ",v_i_tpo_originacion

   PREPARE prp_his_transferencia FROM v_s_qryTxt
   DECLARE cur_his_transferencia CURSOR FOR prp_his_transferencia

   FOREACH cur_his_transferencia INTO v_r_cre_acred.*
      -- se invoca la funcion que obtiene los datos del trabajador
      CALL f_obt_datos_trab(v_r_cre_acred.id_derhabiente) RETURNING r_r_afi_derechohab.*

      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.tpo_registro       = "02"
      LET v_r_detalle.cont_servicio      = v_i_contrador_reg USING "&&&&&&&&&&"
      LET v_r_detalle.tpo_ent_recept     = "04"
      LET v_r_detalle.cve_ent_recept     = "002"
      LET v_r_detalle.tpo_ent_cedente    = "01"
      LET v_r_detalle.cve_ent_cedente    = "002" --"" -- 3 espacios en blanco
      LET v_r_detalle.tpo_transferencia  = "03" -- 2 espacios en blanco
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
      LET v_r_detalle.nss_trab_afore     = "00000000000" --"" -- 1a espacios en blanco. No se sabe el nss afore
      LET v_r_detalle.rfc_trab_afore     = "" -- 13 espacios en blanco. No se sabe el rfc afore
      LET v_r_detalle.filler5            = "" -- 30 espacios en blanco
      LET v_r_detalle.ape_pat_afore      = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.ape_mat_afore      = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.nom_trab_afore     = "" -- 40 espacios en blanco. No se sabe el nombre afore
      LET v_r_detalle.filler6            = "" -- 30 espacios en blanco
      LET v_r_detalle.num_apl_int_viv97  = "000000000000000"
      LET v_r_detalle.ult_aport_vivi97   = "000000000000000"
      LET v_r_detalle.filler7            = "" -- 78 espacios en blanco
      LET v_r_detalle.cod_result_opera   = "" -- 2 espacios en blanco
      LET v_r_detalle.diagnostico_proc   = "" -- 15 espacios en blanco
      LET v_r_detalle.nom_trab_imss      = r_r_afi_derechohab.nombre_imss
      LET v_r_detalle.num_cred_infonavit = v_r_cre_acred.num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.filler8            = "" -- 53 espacios en blanco
      LET v_r_detalle.periodo_pago       = "" -- 6 espacios en blanco
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
                         v_r_detalle.num_apl_int_viv97,
                         v_r_detalle.ult_aport_vivi97,
                         v_r_detalle.filler7,
                         v_r_detalle.cod_result_opera,
                         v_r_detalle.diagnostico_proc,
                         v_r_detalle.nom_trab_imss,
                         v_r_detalle.num_cred_infonavit,
                         v_r_detalle.filler8,
                         v_r_detalle.periodo_pago,
                         v_r_detalle.filler9

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se inserta el registro en la tabla temporal
      LET v_r_tmp_acr_solic_sdo.nss = r_r_afi_derechohab.nss
      LET v_r_tmp_acr_solic_sdo.id_derechohabiente = v_r_cre_acred.id_derhabiente
      LET v_r_tmp_acr_solic_sdo.id_cre_acreditado = v_r_cre_acred.id_cre_acred
      LET v_r_tmp_acr_solic_sdo.modulo_cod = "TA"
      LET v_r_tmp_acr_solic_sdo.f_proceso = TODAY

      -- se inserta el registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_acr_solic_sdo VALUES (v_r_tmp_acr_solic_sdo.*)

      #Guarda la petición de saldo en histórico (Modificación Emilio Abarca,EFP)
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
               VALUES (v_r_cre_acred.id_cre_acred  ,  
                        v_r_cre_acred.id_derhabiente,
                        r_r_afi_derechohab.nss      ,
                        'TA'                        ,
                        0                           ,
                        0                           ,
                        0                           ,
                        0                           ,
                        NULL                        ,
                        TODAY);

      -- se asigna el estado procesar a actualizar
      LET v_edo_procesar_aux = 85

      -- se actualiza el registro leído de cre_acreditado
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

      IF v_r_cre_acred.estado = 20 THEN
         LET v_i_total_regis_sin = v_i_total_regis_sin + 1
      ELSE
         LET v_i_total_regis_arch = v_i_total_regis_arch + 1
      END IF
   END FOREACH

   DISPLAY " Total de registros por enviar: ",v_i_contrador_reg

   -- se actualiza el total de registros en la tabla de control
   UPDATE cre_ctr_archivo
      SET tot_registros = v_i_contrador_reg
    WHERE folio_archivo = p_d_folio
      AND id_cre_ctr_archivo = v_d_id_cre_ctr_archivo

   -- se asignan los valores del registro sumario
   LET v_r_sumario.tpo_registro        = "09"
   LET v_r_sumario.cant_regs_detalle   = v_i_contrador_reg USING "&&&&&&&&&"
   LET v_r_sumario.filler1             = "" -- 30 espacios en blanco
   LET v_r_sumario.sum_numAplInt_viv97 = "000000000000000"
   LET v_r_sumario.sum_ult_aport_viv97 = "000000000000000"
   LET v_r_sumario.filler2             = "" -- 659 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_sumario.tpo_registro,
                      v_r_sumario.cant_regs_detalle,
                      v_r_sumario.filler1,
                      v_r_sumario.sum_numAplInt_viv97,
                      v_r_sumario.sum_ult_aport_viv97,
                      v_r_sumario.filler2

   -- se escribe el registro (sumario) en el archivo
   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = v_c_ruta_envio CLIPPED || "/" || "sol_ta." || v_c_extension

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   DISPLAY ""
   --DISPLAY " Ejecutando envío interfaz para Procesar"

   --LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/sol_ta.sh"
   --RUN v_ejecuta_sh

   --Ejecuta función que genera PDF y archivo de salida
   CALL inf_sol_reporte()
   DISPLAY ""
   DISPLAY " Genera reporte PDF ...completado"
   DISPLAY ""
   DISPLAY " Archivo detalle: ",v_ruta_archivo
   DISPLAY ""

   DISPLAY "=FIN="

END MAIN

#Objetivo: Función que recupera información de las peticiones para el PDF
FUNCTION inf_sol_reporte()

   DEFINE v_reporte_bin  STRING
   DEFINE v_ruta_rpt     STRING
   DEFINE v_hoy        DATE
   DEFINE v_f_pivote     DATE
   DEFINE v_s_qryTxt     STRING

   DEFINE r_solicitud    RECORD
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      folio_liquida      DECIMAL(9,0),
      estado             SMALLINT,
      edo_procesar       SMALLINT
   END RECORD

   DEFINE r_inf_archivo RECORD
      f_genera      DATE,
      nss           CHAR(11),
      aivs92        CHAR(15),
      aivs97        CHAR(15),
      tipo          CHAR(2),
      num_intento   CHAR(4)
   END RECORD
   
   DEFINE archivo        base.Channel
   DEFINE v_detalle      STRING
   DEFINE v_aux_aivs92   DECIMAL(16,2)
   DEFINE v_aux_aivs97   DECIMAL(16,2)
   DEFINE v_tabla        CHAR(20)
   DEFINE v_aux_id_ref   DECIMAL(9,0)

   --Inicializa valores del arreglo
   --Estado = 20
   LET arr_sol_saldo[1].estado_desc = "Solicitud Nueva UG/Anualidad"
   LET arr_sol_saldo[1].t_registros = 0
   LET arr_sol_saldo[1].aivs92      = 0
   LET arr_sol_saldo[1].aivs97      = 0
   --Estado = 25
   LET arr_sol_saldo[2].estado_desc = "Saldo Remanente"
   LET arr_sol_saldo[2].t_registros = 0
   LET arr_sol_saldo[2].aivs92      = 0
   LET arr_sol_saldo[2].aivs97      = 0
   --Estado = 140
   LET arr_sol_saldo[3].estado_desc = "Conciliación por Adelanto"
   LET arr_sol_saldo[3].t_registros = 0
   LET arr_sol_saldo[3].aivs92      = 0
   LET arr_sol_saldo[3].aivs97      = 0
   --Estado = 145
   LET arr_sol_saldo[4].estado_desc = "Saldo Deudor Liquidado"
   LET arr_sol_saldo[4].t_registros = 0
   LET arr_sol_saldo[4].aivs92      = 0
   LET arr_sol_saldo[4].aivs97      = 0
   
   --Meses anteriores
   LET arr_sol_saldo[5].estado_desc = "Solicitud Mes Anterior de UG/Anualidad"
   LET arr_sol_saldo[5].t_registros = 0
   LET arr_sol_saldo[5].aivs92      = 0
   LET arr_sol_saldo[5].aivs97      = 0

   -- Caso especial 142
   LET arr_sol_saldo[6].estado_desc = "Caso especial"
   LET arr_sol_saldo[6].t_registros = 0
   LET arr_sol_saldo[6].aivs92      = 0
   LET arr_sol_saldo[6].aivs97      = 0

   --Inicializa record del total global
   LET r_total_global.t_registros = 0
   LET r_total_global.aivs92      = 0
   LET r_total_global.aivs97      = 0

   #Calcula fecha para solicitudes de meses anteriores
   LET v_hoy = TODAY

   --Obtiene fecha del primer día hábil del mes
   LET v_f_pivote = v_hoy - DAY (v_hoy) + 1

   --Obtiene fecha del mes anterior
   LET v_f_pivote = v_f_pivote -1 UNITS MONTH

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_f_pivote CLIPPED, "',2)"

   PREPARE prp_dia_habil FROM v_s_qryTxt
   EXECUTE prp_dia_habil INTO v_f_pivote

   DISPLAY ""
   DISPLAY " Fecha mes anterior: ",v_f_pivote USING "dd/mm/yyyy"

   LET v_arh_salida   = "Adetalle_",TODAY USING "yyyymmdd",".cta"
   LET v_ruta_archivo = v_c_ruta_envio CLIPPED,"/",v_arh_salida

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   DECLARE crs_tipo_TA CURSOR FOR
   SELECT UNIQUE t.id_cre_acreditado,
          t.id_derechohabiente,
          t.nss,
          c.folio_liquida,
          c.estado,
          c.edo_procesar
     FROM safre_tmp:tmp_acr_solic_sdo t,
          cre_acreditado c
    WHERE t.id_cre_acreditado = c.id_cre_acreditado
      AND t.modulo_cod = 'TA'
      AND t.f_proceso  = TODAY;

   INITIALIZE r_solicitud.* TO NULL
   INITIALIZE r_inf_archivo.* TO NULL

   LET r_inf_archivo.f_genera    = TODAY
   LET r_inf_archivo.num_intento = "0001"
   LET v_aux_aivs92              = 0
   LET v_aux_aivs97              = 0
   LET v_detalle                 = NULL

   FOREACH crs_tipo_TA INTO r_solicitud.id_cre_acreditado,
                            r_solicitud.id_derechohabiente,
                            r_solicitud.nss,
                            r_solicitud.folio_liquida,
                            r_solicitud.estado,
                            r_solicitud.edo_procesar
                            
      LET r_inf_archivo.tipo = "  "
      LET v_aux_aivs92       = 0
      LET v_aux_aivs97       = 0
      LET v_aux_id_ref       = NULL 

      #Obtiene Saldo
      IF (r_solicitud.folio_liquida > 0) THEN
         --Busca tabla movimiento
         LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(0,?,NULL)"

         PREPARE prp_obt_mov FROM v_s_qryTxt
         EXECUTE prp_obt_mov USING r_solicitud.folio_liquida
                              INTO v_tabla

          LET v_s_qryTxt = "SELECT SUM(monto_acciones)
                              FROM ",v_tabla,
                            "WHERE folio_liquida      = ",r_solicitud.folio_liquida,
                            "  AND id_derechohabiente = ",r_solicitud.id_derechohabiente,
                            "  AND subcuenta       = 8",
                            "  AND fondo_inversion = 11;"

         PREPARE prp_92 FROM v_s_qryTxt
         EXECUTE prp_92 INTO v_aux_aivs92

         LET v_s_qryTxt = "SELECT SUM(monto_acciones)
                              FROM ",v_tabla,
                            "WHERE folio_liquida      = ",r_solicitud.folio_liquida,
                            "  AND id_derechohabiente = ",r_solicitud.id_derechohabiente,
                            "  AND subcuenta       = 4",
                            "  AND fondo_inversion = 11;"

         PREPARE prp_97 FROM v_s_qryTxt
         EXECUTE prp_97 INTO v_aux_aivs97

      ELSE
         --Recupera el sdo actual de vivienda 92
         SELECT SUM(monto_acciones)
           INTO v_aux_aivs92
           FROM cta_movimiento
          WHERE id_derechohabiente = r_solicitud.id_derechohabiente
            AND subcuenta       = 8
            AND fondo_inversion = 11
            AND monto_acciones > 0;

         --Recupera el sdo actual de vivienda 92
         SELECT SUM(monto_acciones)
           INTO v_aux_aivs97
           FROM cta_movimiento
          WHERE id_derechohabiente = r_solicitud.id_derechohabiente
            AND subcuenta       = 4
            AND fondo_inversion = 11
            AND monto_acciones > 0;
      END IF


      IF(v_aux_aivs92 IS NULL) THEN
         LET v_aux_aivs92 = 0
      END IF

      IF(v_aux_aivs97 IS NULL) THEN
         LET v_aux_aivs97 = 0
      END IF

      --Asigna valores para el archivo
      LET r_inf_archivo.nss    = r_solicitud.nss
      LET r_inf_archivo.aivs92 = v_aux_aivs92
      LET r_inf_archivo.aivs97 = v_aux_aivs97

      #Incrementa totales globales
      LET r_total_global.t_registros = r_total_global.t_registros + 1
      LET r_total_global.aivs92      = r_total_global.aivs92 + v_aux_aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + v_aux_aivs97

      #Verifica si es un registro solicitado el mes anterior
      SELECT MAX(id_referencia)
        INTO v_aux_id_ref
        FROM cre_his_solic_sdo
       WHERE id_referencia      = r_solicitud.id_cre_acreditado
         AND id_derechohabiente = r_solicitud.id_derechohabiente
         AND modulo_cod = 'TA'
         AND f_proceso  = v_f_pivote
         
      IF(v_aux_id_ref IS NOT NULL) THEN
         LET arr_sol_saldo[5].t_registros = arr_sol_saldo[5].t_registros + 1
         LET arr_sol_saldo[5].aivs92      = arr_sol_saldo[5].aivs92 + v_aux_aivs92
         LET arr_sol_saldo[5].aivs97      = arr_sol_saldo[5].aivs97 + v_aux_aivs97

         LET r_inf_archivo.tipo        = "UR"   --Tipo Meses anteriores
         LET r_inf_archivo.num_intento = "0002" --Se entiende que es la segunda vez que se está solicitando
      ELSE
         LET r_inf_archivo.num_intento = "0001"
         #Evalúa el tipo de solicitud del mes actual
         CASE
            --Solicitud nueva
            WHEN r_solicitud.estado = 20
               LET arr_sol_saldo[1].t_registros = arr_sol_saldo[1].t_registros + 1
               LET arr_sol_saldo[1].aivs92      = arr_sol_saldo[1].aivs92 + v_aux_aivs92
               LET arr_sol_saldo[1].aivs97      = arr_sol_saldo[1].aivs97 + v_aux_aivs97
               LET r_inf_archivo.tipo = "UA"

            --Saldo remanente
            WHEN r_solicitud.estado = 25
               LET arr_sol_saldo[2].t_registros = arr_sol_saldo[2].t_registros + 1
               LET arr_sol_saldo[2].aivs92      = arr_sol_saldo[2].aivs92 + v_aux_aivs92
               LET arr_sol_saldo[2].aivs97      = arr_sol_saldo[2].aivs97 + v_aux_aivs97
               LET r_inf_archivo.tipo = "SR"

            --Conciliación por adelanto
            WHEN r_solicitud.estado = 140
               LET arr_sol_saldo[3].t_registros = arr_sol_saldo[3].t_registros + 1
               LET arr_sol_saldo[3].aivs92      = arr_sol_saldo[3].aivs92 + v_aux_aivs92
               LET arr_sol_saldo[3].aivs97      = arr_sol_saldo[3].aivs97 + v_aux_aivs97
               LET r_inf_archivo.tipo = "CA"

            -- Caso especial
            WHEN r_solicitud.estado = 142
               LET arr_sol_saldo[6].t_registros = arr_sol_saldo[6].t_registros + 1
               LET arr_sol_saldo[6].aivs92      = arr_sol_saldo[6].aivs92 + v_aux_aivs92
               LET arr_sol_saldo[6].aivs97      = arr_sol_saldo[6].aivs97 + v_aux_aivs97
               LET r_inf_archivo.tipo = "CE"

            -- Vigente deudor liquidado
            WHEN r_solicitud.estado = 145
               LET arr_sol_saldo[4].t_registros = arr_sol_saldo[4].t_registros + 1
               LET arr_sol_saldo[4].aivs92      = arr_sol_saldo[4].aivs92 + v_aux_aivs92
               LET arr_sol_saldo[4].aivs97      = arr_sol_saldo[4].aivs97 + v_aux_aivs97
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

   CALL archivo.close()

   #Calcula porcentajes %
   LET v_aux_porcentaje = 0
   --Porcentaje total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100
   LET r_total_global.porcentaje = v_aux_porcentaje CLIPPED,"%"

   --fSe finaliza antes la operación para obtener f_opera_fin
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

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("acr") RETURNING v_c_ruta_bin, v_c_ruta_listados

   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   LET v_reporte_bin = v_c_ruta_bin CLIPPED,"/ACRS131.4rp"
   LET v_ruta_rpt    = v_c_ruta_listados CLIPPED,"/",
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

       LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      
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
      LET v_desc_operacion = "Solicitud de Saldos ACR"

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

      FOR f = 1 TO arr_sol_saldo.getLength()
         PRINTX arr_sol_saldo[f].estado_desc
         PRINTX arr_sol_saldo[f].t_registros

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
