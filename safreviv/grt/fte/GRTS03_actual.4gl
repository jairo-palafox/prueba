--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTS03                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo solicitud de saldos, para el       #
#                    módulo de Uso de Garantía 43 bis              #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>26 Abril 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

MAIN 
   DEFINE p_v_usuario            LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid                LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod        LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod          LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio              LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_arch_proceso       LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
          v_r_cre_uso_garatia    RECORD
             id_derhabiente      LIKE cre_uso_garantia.id_derechohabiente, -- id derechohabiente
             num_credito         LIKE cre_uso_garantia.num_credito, -- número de crédito
             periodo_pago        LIKE cre_uso_garantia.periodo_pago, -- periodo de pago
             tpo_transferencia   LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
          END RECORD,
          v_d_id_cre_uso_garant  LIKE cre_uso_garantia.id_cre_uso_garantia, -- identificar de uso de garantia
          v_si_edo_procesar      LIKE cre_uso_garantia.edo_procesar, -- estado procesar
          v_d_aivs_v97           DECIMAL(18,6), -- importe de vivienda 97
          v_d_importe_v97        DECIMAL(12,2), -- importe de vivienda 97
          v_r_afi_derechohab     RECORD LIKE afi_derechohabiente.*, -- registro de afi derechohabiente
          v_r_encabezado         RECORD
             tpo_registro        CHAR(2), -- Tipo de Registro (001-002)
             id_servicio         CHAR(2), -- Identificador de Servicio (003-004)
             id_operacion        CHAR(2), -- Identificador de Operación (005-006)
             tpo_ent_origen      CHAR(2), -- Tipo de entidad origen (007-008)
             cve_ent_origen      CHAR(3), -- Clave de entidad origen (009-011)
             tpo_ent_destino     CHAR(2), -- Tipo de entidad destino (012-013)
             cve_ent_destino     CHAR(3), -- Clave de entidad destino (014-016)
             ent_fed_env_lote    CHAR(3), -- Entidad federativa de envío de lote (017-019)
             fec_presentacion    CHAR(8), -- Fecha de presentación (020-027)
             consec_lote         CHAR(3), -- Consecutivo del lote en el día (028-030)
             cve_modal_recep     CHAR(2), -- Clave de modalidad de recepción (031-032)
             cod_resul_opera     CHAR(2), -- Código de resultado de la Operación (033-034)
             mot_rechazo         CHAR(9), -- Motivo de rechazo del lote (035-043)
             filler              CHAR(687)-- Filler (044-730)
          END RECORD,
          v_r_detalle            RECORD
             tpo_registro        CHAR(2), -- Tipo de Registro (001-002)
             cont_servicio       CHAR(10), -- Contador de Servicio (003-012)
             tpo_ent_recept      CHAR(2), -- Tipo de entidad receptora de la cuenta (013-014)
             cve_ent_recept      CHAR(3), -- Clave de entidad receptora de la cuenta (015-017)
             tpo_ent_cedente     CHAR(2), -- Tipo de entidad cedente de la cuenta (018-019)
             cve_ent_cedente     CHAR(3), -- Clave de entidad ced. de la cuenta (020-022)
             tpo_transferencia   CHAR(2), -- Origen/Tipo de transferencia (023-024)
             f_presentacion      CHAR(8), -- Fecha de presentación (025-032)
             f_movimiento        CHAR(8), -- fecha de movimiento (033-040)
             curp_trabajador     CHAR(18), -- CURP del trabajador (041-058)
             nss_trab_infonavit  CHAR(11), -- NSS del trabajador según INFONAVIT (059-069)
             filler2             CHAR(15), -- Filler2 (070-084)
             rfc_trab_infonavit  CHAR(13), -- RFC del trabajador según INFONAVIT (085-097)
             ape_pat_infonavit   CHAR(40), -- Apellido paterno del trabajador en el INFONAVIT (098-137)
             ape_mat_infonavit   CHAR(40), -- Apellido materno del trabajador en el INFONAVIT (138-177)
             nom_trab_infonavit  CHAR(40), -- Nombres del trabajador en el INFONAVIT (178-217)
             filler3             CHAR(22), -- Filler3 (218-239)
             id_lote_solic       CHAR(16), -- Identificador de lote de la solicitud (240-255)
             filler4             CHAR(15), -- Filler4 (256-270)
             nss_trab_afore      CHAR(11), -- NSS del trabajador según AFORE/PS. (271-281)
             rfc_trab_afore      CHAR(13), -- RFC del trabajador según AFORE (282-294)
             filler5             CHAR(30), -- Filler5 (295-324)
             ape_pat_afore       CHAR(40), -- Apellido paterno del trabajador en la AFORE cedente (325-364)
             ape_mat_afore       CHAR(40), -- Apellido materno del trabajador en la AFORE cedente (365-404)
             nom_trab_afore      CHAR(40), -- Nombres del trabajador en la AFORE cedente (405-444)
             filler6             CHAR(30), -- Filler6 (445-474)
             num_apl_int_viv97   CHAR(15), -- Número de "Aplicaciones de Intereses de Vivienda" 97 de la última aportación (475-489)
             ult_aport_vivi97    CHAR(15), -- Ultima aportación Vivienda 97 (490-504)
             filler7             CHAR(78), -- Filler7 (505-582)
             cod_result_opera    CHAR(2), -- Código Resultado de la Operación (583-584)
             diagnostico_proc    CHAR(15), -- Diagnóstico del Proceso (585-599)
             nom_trab_imss       CHAR(50), -- Nombre del Trabajador según IMSS (600-649)
             num_cred_infonavit  CHAR(10), -- Número de Crédito INFONAVIT (650-659)
             filler8             CHAR(53), -- Filler8 (660-712)
             periodo_pago        CHAR(6), -- Período de pago (713-718)
             filler9             CHAR(12) -- Filler9 (719-730)
          END RECORD,
          v_r_sumario            RECORD
             tpo_registro        CHAR(2), -- tipo de registro (001-002)
             cant_regs_detalle   CHAR(9), -- cantidad de registros de detalle (003-011)
             filler1             CHAR(30), -- filler1 (012-041)
             sum_numAplInt_viv97 CHAR(15), -- suma del número de "aplicaciones intereses de vivienda" 97 de la última aportación (042-056)
             sum_ult_aport_viv97 CHAR(15), -- suma de la ultima aportación de vivienda 97 (057-071)
             filler2             CHAR(659) -- filler2 (072-730)
          END RECORD,
          v_r_tmp_uso_solic_sdo   RECORD
             nss                  CHAR(11),
             id_derechohabiente   DECIMAL(9,0),
             id_cre_uso_garantia  DECIMAL(9,0),
             modulo_cod           CHAR(2),
             f_proceso            DATE
          END RECORD,
          v_edo_procesar_aux     LIKE cre_uso_garantia.edo_procesar, -- estado procesar a actualizar
          v_dt_fec_present       DATE, -- fecha de presentacion
          v_dt_fec_movimiento    DATE, -- fecha de movimiento
          v_c_fec_present_ax     CHAR(8), -- fecha ayxiliar de presentacion con formato YYYYMMDD
          v_dt_f_solic_saldos    DATE, -- fecha para la generación de solicitud de saldos
          v_v_ruta_nomarch       VARCHAR(100), -- ruta y nombre del archivo de salida
          v_v_ruta_nomarch_cp    VARCHAR(100), -- ruta y nombre del archivo de salida
          v_c_extension          LIKE cat_operacion.extension, -- extensión del archivo
          v_ch_arch_solTransf    BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_s_registro           STRING, -- registro a insertar
          v_c_ruta_envio         LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_i_contrador_reg      INTEGER, -- contrador de registros
          v_c_programa_cod       LIKE cat_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt             STRING, -- guarda una sentencia sql a ejecutar
          v_s_comando            STRING, -- contiene al comando a correr
          v_r_cre_ctr_archivo    RECORD LIKE cre_ctr_archivo.*, -- regsitro de cre ctr archivo
          v_c_ruta_listado       LIKE seg_modulo.ruta_listados, -- ruta donde se colocara el archivo
          v_manejador_rpt        OM.SaxDocumentHandler, -- Contenedor de Documentos para el reporte          
          v_archivo_nom          STRING,
          v_d_sum_importe_v97    DECIMAL(25,2), -- suma del importe de vivienda 97
          v_d_precio_fondo       LIKE glo_valor_fondo.precio_fondo, -- precio de accion
          v_i_lote               LIKE dse_ctr_archivo.lote, -- lote del archivo
          r_b_valida             SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE v_ejecuta_sh             STRING

      -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario           = ARG_VAL(1)
   LET p_d_pid               = ARG_VAL(2)
   LET p_i_proceso_cod       = ARG_VAL(3)
   LET p_i_opera_cod         = ARG_VAL(4)
   LET p_d_folio             = ARG_VAL(5)
   LET p_v_arch_proceso      = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTS03.log")

   DISPLAY "=INICIA GRTS03="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_d_sum_importe_v97 = 0 -- suma del importe de vivienda 97
   LET v_i_contrador_reg = 0 -- contador de registros

   -- se crea tabla temporal la cual identifica que registros se procesaron en la generación del archivo
   CALL fn_crea_tmp_solic_sdo_uso()

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'grt'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado

{
   -- se obtiene el precio de accion para el día de hoy
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE f_valuacion = TODAY\n",
                    "    AND fondo = 11"

   PREPARE prp_precio_accion FROM v_s_qryTxt
   EXECUTE prp_precio_accion INTO v_d_precio_fondo
}

   -- se crea la fecha. Primer dia del mes
   LET v_dt_fec_present = TODAY - DAY(TODAY) + 1

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_dt_fec_present CLIPPED, "',2)" 

   PREPARE prp_obtiene_3habil FROM v_s_qryTxt
   EXECUTE prp_obtiene_3habil INTO v_dt_fec_present
{-- se comenta unicamente para pruebas. DEspués de las pruebas volver a habilitar
   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_dt_fec_present < TODAY THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_dt_fec_present = TODAY - DAY(TODAY) + 1
      LET v_dt_fec_present = v_dt_fec_present + 1 UNITS MONTH

      PREPARE prp_obtiene_3habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_dt_fec_present CLIPPED||"',2)"
      EXECUTE prp_obtiene_3habil_sig INTO v_dt_fec_present
   END IF
}
   LET v_dt_fec_movimiento = v_dt_fec_present - DAY(v_dt_fec_present) + 1
   LET v_dt_fec_movimiento = v_dt_fec_movimiento + 1 UNITS MONTH

   DISPLAY " Fecha presentación: ",v_dt_fec_present USING "DD/MM/YYYY"
   DISPLAY " Fecha movimiento  : ",v_dt_fec_movimiento USING "DD/MM/YYYY"

   -- se obtiene el precio de accion para el día de liquidación
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11",
                    "    AND f_valuacion = '",v_dt_fec_movimiento,"'"

   PREPARE prp_precio_accion FROM v_s_qryTxt
   EXECUTE prp_precio_accion INTO v_d_precio_fondo

   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fec_present_ax = v_dt_fec_present USING "YYYYMMDD"

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   --LET v_v_nom_archivo = "A" || v_c_fec_present_ax || ".usog"
   LET v_v_ruta_nomarch = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- se asignan los valores del registro encabezado a insertar
   LET v_r_encabezado.tpo_registro = "01"
   LET v_r_encabezado.id_servicio = "02"
   LET v_r_encabezado.id_operacion = "01"
   LET v_r_encabezado.tpo_ent_origen = "04"
   LET v_r_encabezado.cve_ent_origen = "002"
   LET v_r_encabezado.tpo_ent_destino = "01"
   LET v_r_encabezado.cve_ent_destino = "" -- 3 espacios en blanco
   LET v_r_encabezado.ent_fed_env_lote = "009"
   LET v_r_encabezado.fec_presentacion = v_c_fec_present_ax
   LET v_r_encabezado.consec_lote = "001"
   LET v_r_encabezado.cve_modal_recep = "" --"02"
   LET v_r_encabezado.cod_resul_opera = (2 SPACES) -- 2 espacios en blanco
   LET v_r_encabezado.mot_rechazo = "" -- 9 espacios en blanco
   LET v_r_encabezado.filler = "" -- 687 espacios en blanco

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
{
   -- se consultan  todos los registro de cre acreditado con edo_procesar = 70 y con estado = 140
   LET v_s_qryTxt = " SELECT cre.id_cre_uso_garantia, cre.id_derechohabiente,\n",
                    "        cre.num_credito, cta.monto_acciones,\n",
                    "        cre.periodo_pago, cre.edo_procesar\n",
                    "        cre.tpo_transferencia\n",
                    "   FROM cre_uso_garantia cre, cta_movimiento cta\n",
                    "  WHERE cre.estado = 140\n",
                    "    AND cre.tpo_transferencia IN ('18','48')\n",
                    "    AND cre.edo_procesar IN (10, 60, 70)\n",
                    "    AND cre.id_derechohabiente = cta.id_derechohabiente\n",
                    "    AND cre.folio_liquida = cta.folio_liquida\n",
                    "    AND cre.f_proceso <= '",v_dt_f_solic_saldos,"'",
                    "    AND cre.periodo_pago = cta.id_referencia\n"

   PREPARE prp_his_transferencia FROM v_s_qryTxt
   DECLARE cur_his_transferencia CURSOR FOR prp_his_transferencia
}
   -- se consultan  todos los registro de cre acreditado con edo_procesar = 70 y con estado = 140
   LET v_s_qryTxt = " SELECT UNIQUE id_derechohabiente, num_credito,\n",
                    "        periodo_pago, tpo_transferencia\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado = 140\n",
                    "    AND edo_procesar IN (10, 70)\n",
                    "    AND tpo_transferencia IN ('18','48')\n",
                    "    AND f_proceso <= '",v_dt_f_solic_saldos,"'"

   PREPARE prp_uniq_cre_uso FROM v_s_qryTxt
   DECLARE cur_uniq_cre_uso CURSOR FOR prp_uniq_cre_uso

   FOREACH cur_uniq_cre_uso INTO v_r_cre_uso_garatia.*
      -- se obtiene el monto en acciones para el derechohabiente en proceso
      SELECT SUM(monto_acciones)
        INTO v_d_aivs_v97
        FROM cta_movimiento
       WHERE folio_liquida IN (
             SELECT UNIQUE folio_liquida
               FROM cre_uso_garantia
              WHERE estado = 140
                AND edo_procesar IN (10, 70)
                AND id_derechohabiente = v_r_cre_uso_garatia.id_derhabiente
                AND tpo_transferencia = v_r_cre_uso_garatia.tpo_transferencia
                AND num_credito = v_r_cre_uso_garatia.num_credito
                AND periodo_pago = v_r_cre_uso_garatia.periodo_pago
                AND f_proceso <= v_dt_f_solic_saldos)
         AND id_referencia = v_r_cre_uso_garatia.periodo_pago
         AND id_derechohabiente = v_r_cre_uso_garatia.id_derhabiente

      -- se valida el importe obtenido
      IF v_d_aivs_v97 IS NULL THEN
         LET v_d_aivs_v97 = 0
      END IF

      -- se obtiene el importe en pesos
      LET v_d_importe_v97 = v_d_aivs_v97 * v_d_precio_fondo

      -- se invoca la funcion que obtiene los datos del trabajador
      CALL f_obt_datos_trab(v_r_cre_uso_garatia.id_derhabiente) RETURNING v_r_afi_derechohab.*

      -- se incrementa el contador de registro y el acumulador del importe
      LET v_i_contrador_reg = v_i_contrador_reg + 1
      LET v_d_sum_importe_v97 = v_d_sum_importe_v97 + v_d_importe_v97

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.tpo_registro = "02"
      LET v_r_detalle.cont_servicio = v_i_contrador_reg USING "&&&&&&&&&&"
      LET v_r_detalle.tpo_ent_recept = "04"
      LET v_r_detalle.cve_ent_recept = "002"
      LET v_r_detalle.tpo_ent_cedente = "01"
      LET v_r_detalle.cve_ent_cedente = "" -- 3 espacios en blanco
      LET v_r_detalle.tpo_transferencia = "18" -- siempre se va "18" v_r_cre_uso_garatia.tpo_transferencia -- 2 espacios en blanco
      LET v_r_detalle.f_presentacion = v_c_fec_present_ax
      LET v_r_detalle.f_movimiento = v_dt_fec_movimiento USING "yyyymmdd"
      LET v_r_detalle.curp_trabajador = v_r_afi_derechohab.curp
      LET v_r_detalle.nss_trab_infonavit = v_r_afi_derechohab.nss
      LET v_r_detalle.filler2 = "" -- 15 espacios en blanco
      LET v_r_detalle.rfc_trab_infonavit = v_r_afi_derechohab.rfc
      LET v_r_detalle.ape_pat_infonavit = v_r_afi_derechohab.ap_paterno_af
      LET v_r_detalle.ape_mat_infonavit = v_r_afi_derechohab.ap_materno_af
      LET v_r_detalle.nom_trab_infonavit = v_r_afi_derechohab.nombre_af
      LET v_r_detalle.filler3 = "" -- 22 espacios en blanco
      LET v_r_detalle.id_lote_solic = "04002" || v_c_fec_present_ax || "001"
      LET v_r_detalle.filler4 = "" -- 15 espacios en blanco 
      LET v_r_detalle.nss_trab_afore = "" -- 1a espacios en blanco. No se sabe el nss afore
      LET v_r_detalle.rfc_trab_afore = "" -- 13 espacios en blanco. No se sabe el rfc afore
      LET v_r_detalle.filler5 = "" -- 30 espacios en blanco
      LET v_r_detalle.ape_pat_afore = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.ape_mat_afore = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
      LET v_r_detalle.nom_trab_afore = "" -- 40 espacios en blanco. No se sabe el nombre afore
      LET v_r_detalle.filler6 = "" -- 30 espacios en blanco
      LET v_r_detalle.num_apl_int_viv97 = "000000000000000"
      --LET v_r_detalle.num_apl_int_viv97 = ((v_d_importe_v97 / v_d_precio_fondo) * 100) USING "&&&&&&&&&&&&&&&"  MMC
      --LET v_r_detalle.ult_aport_vivi97 = "000000000000000"  MMC
      --LET v_r_detalle.num_apl_int_viv97 = ((v_d_importe_v97 / v_d_precio_fondo) * 100) USING "&&&&&&&&&&&&&&&"  --MMC
      LET v_r_detalle.ult_aport_vivi97 = v_d_importe_v97 * 100 USING "&&&&&&&&&&&&&&&"  --MMC
      LET v_r_detalle.filler7 = "" -- 78 espacios en blanco
      LET v_r_detalle.cod_result_opera = "" -- 2 espacios en blanco
      LET v_r_detalle.diagnostico_proc = "" -- 15 espacios en blanco
      LET v_r_detalle.nom_trab_imss = v_r_afi_derechohab.nombre_imss
      LET v_r_detalle.num_cred_infonavit = v_r_cre_uso_garatia.num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.filler8 = "" -- 53 espacios en blanco
      LET v_r_detalle.periodo_pago = v_r_cre_uso_garatia.periodo_pago -- 6 espacios en blanco
      LET v_r_detalle.filler9 = "" -- 12 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tpo_registro,
                         v_r_detalle.cont_servicio,
                         v_r_detalle.tpo_ent_recept,
                         v_r_detalle.cve_ent_recept,
                         v_r_detalle.tpo_ent_cedente,
                         v_r_detalle.cve_ent_cedente,
                         v_r_detalle.tpo_transferencia,
                         v_r_detalle.f_presentacion,
                         v_r_detalle.f_movimiento,
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

      -- se procesar los registros para el derechohabiente en proceso
      DECLARE cur_slct_id_edoPrc CURSOR FOR 
      SELECT id_cre_uso_garantia, edo_procesar
        FROM cre_uso_garantia
       WHERE estado = 140
         AND edo_procesar IN (10, 70)
         AND id_derechohabiente = v_r_cre_uso_garatia.id_derhabiente
         AND tpo_transferencia = v_r_cre_uso_garatia.tpo_transferencia
         AND num_credito = v_r_cre_uso_garatia.num_credito
         AND periodo_pago = v_r_cre_uso_garatia.periodo_pago
         AND f_proceso <= v_dt_f_solic_saldos

      FOREACH cur_slct_id_edoPrc INTO v_d_id_cre_uso_garant, v_si_edo_procesar
         -- se inserta el registro en la tabla temporal
         LET v_r_tmp_uso_solic_sdo.nss = v_r_afi_derechohab.nss
         LET v_r_tmp_uso_solic_sdo.id_derechohabiente = v_r_cre_uso_garatia.id_derhabiente
         LET v_r_tmp_uso_solic_sdo.id_cre_uso_garantia = v_d_id_cre_uso_garant
         LET v_r_tmp_uso_solic_sdo.modulo_cod = "UG"
         LET v_r_tmp_uso_solic_sdo.f_proceso = TODAY

         -- se inserta el registro en la tabla temporal
         INSERT INTO safre_tmp:tmp_uso_solic_sdo VALUES (v_r_tmp_uso_solic_sdo.*)

         -- se valida el estado procesar
         IF v_si_edo_procesar = 70 THEN
            LET v_edo_procesar_aux = 85
         ELSE
            LET v_edo_procesar_aux = 80
         END IF

         -- se actualiza el registro leído de cre acreditado
         UPDATE cre_uso_garantia
            SET edo_procesar = v_edo_procesar_aux
          WHERE id_cre_uso_garantia = v_d_id_cre_uso_garant
            AND estado = 140
            AND edo_procesar = v_si_edo_procesar
            AND id_derechohabiente = v_r_cre_uso_garatia.id_derhabiente
            AND tpo_transferencia = v_r_cre_uso_garatia.tpo_transferencia
            AND num_credito = v_r_cre_uso_garatia.num_credito
            AND periodo_pago = v_r_cre_uso_garatia.periodo_pago
      END FOREACH
   END FOREACH

   -- se asignan los valores del registro sumario
   LET v_r_sumario.tpo_registro = "09"
   LET v_r_sumario.cant_regs_detalle = v_i_contrador_reg USING "&&&&&&&&&"
   LET v_r_sumario.filler1 = "" -- 30 espacios en blanco
   LET v_r_sumario.sum_numAplInt_viv97 = "000000000000000"
   --LET v_r_sumario.sum_ult_aport_viv97 = "000000000000000"
   LET v_r_sumario.sum_ult_aport_viv97 = (v_d_sum_importe_v97 * 100) USING "&&&&&&&&&&&&&&&"
   LET v_r_sumario.filler2 = "" -- 659 espacios en blanco

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
   LET v_v_ruta_nomarch_cp = v_c_ruta_envio CLIPPED || "/" || "sol_ug." || v_c_extension

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   -- se obtiene el maximo lote para la fecha de presentación
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_fec_present,"'\n",
                    "    AND id_proceso = ",g_id_proceso_grt_uso

   PREPARE prp_max_lote FROM v_s_qryTxt
   EXECUTE prp_max_lote INTO v_i_lote

   -- se valida el lote
   IF v_i_lote IS NULL THEN
      LET v_i_lote = 1
   ELSE
      LET v_i_lote = v_i_lote + 1
   END IF

   DISPLAY " Total de registros por enviar: ",v_i_contrador_reg
   DISPLAY " Suma total de saldo viv 97: ",v_d_sum_importe_v97

   DISPLAY ""
   DISPLAY " Ejecutando envío interfaz PROCESAR"

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/sol_ug.sh"
   RUN v_ejecuta_sh

   DISPLAY ""

   --agregar nuevos campos de la tabla cre ctr archivo
   LET v_r_cre_ctr_archivo.folio_archivo      = p_d_folio
   LET v_r_cre_ctr_archivo.lote               = v_i_lote
   LET v_r_cre_ctr_archivo.f_lote             = v_dt_fec_present
   LET v_r_cre_ctr_archivo.id_proceso         = g_id_proceso_grt_uso
   LET v_r_cre_ctr_archivo.operacion          = 0
   LET v_r_cre_ctr_archivo.nom_archivo        = p_v_arch_proceso
   LET v_r_cre_ctr_archivo.tot_registros      = v_i_contrador_reg
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

   --se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "GRTL26"    
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   IF fgl_report_loadCurrentSettings("GRTS031.4rp") THEN  -- if  the file loaded OK
      LET v_archivo_nom = p_v_usuario CLIPPED 
                          ,"-",v_c_programa_cod CLIPPED 
                          ,"-",p_d_pid USING "&&&&&"
                          ,"-", p_i_proceso_cod USING "&&&&&"
                          ,"-", p_i_opera_cod USING "&&&&&"
                          ,".pdf"
       
      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listado CLIPPED||"/"||v_archivo_nom)

      -- sin preview
      CALL fgl_report_selectPreview(0)
      -- se indica que se escriba en archivo
      --CALL fgl_report_setOutputFileName("reporte_escrito_con_nombre_archivo") 

      LET v_manejador_rpt = fgl_report_commitCurrentSettings() -- commit the file settings
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- empieza el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- salida de reporte
   OUTPUT TO REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_proceso,v_r_sumario.sum_ult_aport_viv97, v_i_contrador_reg)

   --Finaliza el reporte
   FINISH REPORT reporte_archivo_salida

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
   END IF

   DISPLAY "=FIN="
END MAIN 

#Objetivo: Funcion que consulta los datos de la tabla afi derechohabiente
FUNCTION f_obt_datos_trab(p_id_derechohabiente)
   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- identificador del derechohabiente 
          v_r_afi_derechohab   RECORD LIKE afi_derechohabiente.*, -- registro de afi derechohabiente
          v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene la información del trabajador
   LET v_s_qryTxt = " SELECT FIRST 1 *\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_id_derechohabiente

   PREPARE prp_afi_derechohabiente FROM v_s_qryTxt
   EXECUTE prp_afi_derechohabiente INTO v_r_afi_derechohab.*

   RETURN v_r_afi_derechohab.*
END FUNCTION

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_v_usuario, 
                              p_d_folio, 
                              p_v_arch_proceso,
                              p_total_deudor, 
                              p_count_reg)
                          
   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod, 
          p_d_folio              INTEGER,
          v_fecha_reporte        DATE,
          v_fecha_present        LIKE dis_sum_avance_pago.f_presentacion,
          p_count_reg            INTEGER,
          p_total_deudor         DECIMAL(10,2),
          p_v_arch_proceso       CHAR(100)

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte
      LET p_total_deudor = p_total_deudor / 100

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_d_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX p_v_usuario
      PRINTX p_total_deudor
      PRINTX p_count_reg
      PRINTX p_v_arch_proceso

END REPORT

#Objetivo: Función que crea tabla temporal la cual tiene por objetivo diferenciar el módulo
#          correspondiente a cada NSS que llega de Infonavit
FUNCTION fn_crea_tmp_solic_sdo_uso()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- en caso de error continua
   WHENEVER ERROR CONTINUE

   -- se elimina la tabla temporal
   DROP TABLE tmp_uso_solic_sdo

   -- al encontrar un error detiene el programa
   WHENEVER ERROR STOP

   -- se ejecuta el script para crear la tabla temporal
   CREATE TABLE tmp_uso_solic_sdo(nss CHAR(11),
                                  id_derechohabiente DECIMAL(9,0),
                                  id_cre_uso_garantia  DECIMAL(9,0),
                                  modulo_cod CHAR(2),
                                  f_proceso DATE)

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION
