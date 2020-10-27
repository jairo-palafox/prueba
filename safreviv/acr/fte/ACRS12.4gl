--===============================================================
-- Version: 1.0.0
-- Fecha última modificación:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRS12                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo Cargo a capital para la fecha de   #
#                    liquidación que ntra como parámetro, para el  #
#                    módulo de Transferecia de Acreditados         #
#Autor             =>Felipe Nava, EFP                              #
#Fecha inicio      =>25 ENERO 2012                                 #
####################################################################

DATABASE safre_viv

MAIN

   DEFINE p_v_usuario              LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                  LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod          LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod            LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio                LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso         VARCHAR(100) -- nombre del archivo a integrar
   DEFINE v_dt_f_liquidacion       DATE -- fecha auxiliar de presentacion
   DEFINE v_c_liquidacion          CHAR(8) -- fecha de presentacion del archivo formato: YYYYMMDD

   DEFINE v_r_reg_inicial RECORD
      tpo_registro              CHAR(1), -- tipo de registro
      fec_archivo               CHAR(8), -- fecha de archivo. Formato: YYYYMMDD
      filler                    CHAR(76) -- filler
   END RECORD

   DEFINE v_r_encabezado RECORD
      tpo_registro              CHAR(1), -- tipo de registro
      transaccion               CHAR(4), -- tranzaccion
      f_genera_archivo          CHAR(8), -- fecha de generacion del archivo
      filler                    CHAR(72) -- filler
   END RECORD

   DEFINE v_r_detalle RECORD
      tpo_registro              CHAR(1),  -- tipo de registro
      nss                       CHAR(11), -- nss
      num_credito               CHAR(10), -- numero de credito
      periodo_pago              CHAR(6),  -- periodo de pago
      fec_pago                  CHAR(8),  -- fecha de pago
      ent_recaudadora           CHAR(3),  -- entidad recaudadora
      nrp                       CHAR(11), -- nrp
      mto_aport_trasp_sar92     CHAR(9),   -- monto de aportacion o monto de traspaso sar 92
      mto_aport_trasp_sar97     CHAR(9),  -- monto de aportacion o monto de traspaso sar 97
      num_folio_pago_sua        CHAR(6),  -- numero folio pago sua
      marca_seguro              CHAR(1),  -- marca que incluye si el pago incluye seguro de danos
      cve_rechazo               CHAR(10)  -- claves de rechazo
   END RECORD

   DEFINE v_r_sumario RECORD
      tpo_registro              CHAR(1),  -- tipo de registro
      transaccion               CHAR(4),  -- transacciones
      tot_regs_transacion       CHAR(10), -- total de registros detalle
      tot_aport_trasp_sar92     CHAR(12), -- total de aportaciÓn Ó monto de traspaso sar 92
      tot_amort_trasp_sar97     CHAR(12), -- total de aportaciÓn Ó monto de traspaso sar 97
      tot_primas_seguro         CHAR(12), -- total del pago de primas del seguro de danos
      sum_aport_amort_prima     CHAR(12), -- suma aportación
      filler                    CHAR(22)  -- filler
   END RECORD

   DEFINE v_r_reg_final RECORD
      tpo_registro              CHAR(1),  -- tipo de registro (4)
      tot_reg_archivo           CHAR(10), -- total de registros contenidos en archivo
      filler1                   CHAR(4),  -- cuatros espacios en blanco
      tot_glob_trasp_sar92      CHAR(12), -- total de traspaso sar 92
      tot_glob_trasp_sar97      CHAR(12), -- total de traspaso sar 97 en registro detalle
      tot_glob_primas_seg       CHAR(12), -- total primas de seguro danio
      sum_aport_amort_prima     CHAR(12), -- sma traspaso sar92 + sar97
      filler2                   CHAR(22)  -- 22 espacios en blanco
   END RECORD

   DEFINE v_c_ruta_envio           LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_v_ruta_nomarch         VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp      VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_c_extension            LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_comando              STRING -- contiene al comando a correr
   DEFINE v_s_registro             STRING -- registro a insertar
   DEFINE v_ch_arch_cargoCapit     BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_i_total_regis_arch     INTEGER -- total de registros en el archivo
   DEFINE v_i_cont_reg_det         INTEGER -- contrador de registros en la sección Detalle
   DEFINE v_id_cre_acreditado      LIKE cre_acreditado.id_cre_acreditado -- identificador del acreditado
   DEFINE v_sum_mto_pesos          DECIMAL(12,2) -- suma de monto en pesos por id derechohabiente
   DEFINE v_c_nss                  LIKE afi_derechohabiente.nss -- numero de seguridad social
   DEFINE v_d_suma_sar_97          DECIMAL(12,2) -- se acumula el monto de sar 97
   DEFINE v_d_num_credito          LIKE cre_acreditado.num_credito -- numero de credito
   DEFINE v_c_programa_cod         LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt               STRING -- guarda una sentencia sql a ejecutar
   DEFINE r_b_valida               SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE v_manejador_rpt          OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_total_deudor           LIKE cta_movimiento.monto_pesos -- monto en pesos
   DEFINE v_archivo_nom            STRING
   DEFINE v_c_ruta_listado         LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo

   DEFINE v_s_Txt                  CHAR(100)
   DEFINE v_nom_fin                CHAR(20)

   DEFINE v_ejecuta_sh             STRING

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRS12.log")

   DISPLAY "=INICIA ACRS12="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

   PREPARE prp_slc_ruta_envio FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio INTO v_c_ruta_envio, v_c_ruta_listado

   -- se obtienen la fecha y folio de liquidación
   LET v_s_qryTxt = " SELECT FIRST 1 f_liquida\n",
                    "   FROM safre_tmp:tmp_liquida_deudor_acr"

   PREPARE prp_fec_folio_liq FROM v_s_qryTxt
   EXECUTE prp_fec_folio_liq INTO v_dt_f_liquidacion

   -- se cambia el formato de la fecha de liquidación a YYYYMMDD
   LET v_c_liquidacion = v_dt_f_liquidacion USING "YYYYMMDD"

   -- se crea el nombre del archivo y se concatena con la ruta
   --LET p_v_arch_proceso = "A" || v_c_liquidacion || ".cct"
   LET v_v_ruta_nomarch = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso CLIPPED

   DISPLAY " Genera archivo: ",p_v_arch_proceso

   -- se crea el manejador de archivo
   LET v_ch_arch_cargoCapit = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_cargoCapit.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_cargoCapit.setDelimiter("")
   
   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "ACRL17"
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   IF fgl_report_loadCurrentSettings("ACRS121.4rp") THEN  -- if  the file loaded OK
       LET v_archivo_nom = p_v_usuario CLIPPED 
                          ,"-",v_c_programa_cod CLIPPED 
                          ,"-",p_d_pid USING "&&&&&"
                          ,"-", p_i_proceso_cod USING "&&&&&"
                          ,"-", p_i_opera_cod USING "&&&&&"
                          ,".pdf"

      DISPLAY " Genera reporte: ",v_archivo_nom
      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listado CLIPPED||"/"||v_archivo_nom)

      -- sin preview
      CALL fgl_report_selectPreview(0)
      -- se indica que se escriba en archivo
      --CALL fgl_report_setOutputFileName("reporte_escrito_con_nombre_archivo") 

      LET v_manejador_rpt = fgl_report_commitCurrentSettings() -- commit the file settings
   ELSE
      DISPLAY "ERROR: No fue posible abrir la plantilla del reporte"

      EXIT PROGRAM
  END IF

   -- se inicializa el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- se inicializan variables
   LET v_i_total_regis_arch = 0
   LET v_i_cont_reg_det = 0
   LET v_d_suma_sar_97 = 0

   -- se armar el registro inicial
   LET v_r_reg_inicial.tpo_registro = '0'
   LET v_r_reg_inicial.fec_archivo  = v_c_liquidacion
   LET v_r_reg_inicial.filler       = "" -- 76 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_reg_inicial.tpo_registro,
                      v_r_reg_inicial.fec_archivo,
                      v_r_reg_inicial.filler

   -- se escribe el registro (registro inicial) en el archivo
   CALL v_ch_arch_cargoCapit.write([v_s_registro])

   -- se incrementa el total de registros continidos en el archivo
   LET v_i_total_regis_arch = v_i_total_regis_arch + 1

   -- se armar registro encabezado
   LET v_r_encabezado.tpo_registro     = '1'
   LET v_r_encabezado.transaccion      = '8839'
   LET v_r_encabezado.f_genera_archivo = v_c_liquidacion
   LET v_r_encabezado.filler           = "" -- 72 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_encabezado.tpo_registro,
                      v_r_encabezado.transaccion,
                      v_r_encabezado.f_genera_archivo,
                      v_r_encabezado.filler

   -- se escribe el registro (registro inicial) en el archivo
   CALL v_ch_arch_cargoCapit.write([v_s_registro])

   -- se incrementa el total de registros en el archivo
   LET v_i_total_regis_arch = v_i_total_regis_arch + 1

   -- se armar consulta que obtiene la información para el registro detalle
   LET v_s_qryTxt = " SELECT id_cre_acreditado, SUM(monto_pesos)\n",
                    "   FROM safre_tmp:tmp_liquida_deudor_acr\n",
                    "  WHERE movimiento = 252\n",
                    "  GROUP BY 1"

   PREPARE prp_tmp_liquidaDeudor FROM v_s_qryTxt
   DECLARE cur_tmp_liquidaDeudor CURSOR FOR prp_tmp_liquidaDeudor

   FOREACH cur_tmp_liquidaDeudor INTO v_id_cre_acreditado, v_sum_mto_pesos
      -- se valida el importe obtenido
      IF v_sum_mto_pesos IS NULL THEN
         LET v_sum_mto_pesos = 0
      END IF

      -- se invoca la funcion que obtiene el nss y el númeo de crédito del trabajador
      CALL fn_Obt_nss_numCred_IdCre(v_id_cre_acreditado) RETURNING v_c_nss, v_d_num_credito

      -- acumula importe de sar_97
      LET v_d_suma_sar_97 = v_d_suma_sar_97 + v_sum_mto_pesos

      -- se incrementa contador de registros de seccion [Detalle]
      LET v_i_cont_reg_det = v_i_cont_reg_det + 1

      -- se armar registro detalle
      LET v_r_detalle.tpo_registro          = '2'
      LET v_r_detalle.nss                 = v_c_nss USING "&&&&&&&&&&&"
      LET v_r_detalle.num_credito           = v_d_num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.periodo_pago          = "000000"
      LET v_r_detalle.fec_pago              = v_c_liquidacion
      LET v_r_detalle.ent_recaudadora       = "207"
      LET v_r_detalle.nrp                 = "" -- 11 espacios en blanco
      LET v_r_detalle.mto_aport_trasp_sar92 = "000000000"
      -- Se múltiplica por 100 para quitarle en punto decimal
      LET v_r_detalle.mto_aport_trasp_sar97 = (v_sum_mto_pesos * 100) USING "&&&&&&&&&"
      LET v_r_detalle.num_folio_pago_sua    = "000000"
      LET v_r_detalle.marca_seguro          = "" -- 1 espacio en blanco
      LET v_r_detalle.cve_rechazo           = "" -- 10 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tpo_registro,
                         v_r_detalle.nss,
                         v_r_detalle.num_credito,
                         v_r_detalle.periodo_pago,
                         v_r_detalle.fec_pago,
                         v_r_detalle.ent_recaudadora,
                         v_r_detalle.nrp,
                         v_r_detalle.mto_aport_trasp_sar92,
                         v_r_detalle.mto_aport_trasp_sar97,
                         v_r_detalle.num_folio_pago_sua,
                         v_r_detalle.marca_seguro,
                         v_r_detalle.cve_rechazo

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_cargoCapit.write([v_s_registro])

      -- actualiza total de registros de archivo
      LET v_i_total_regis_arch = v_i_total_regis_arch + 1
   END FOREACH

   -- se arma el registro sumario
   LET v_r_sumario.tpo_registro          = '3'
   LET v_r_sumario.transaccion           = '8839'
   LET v_r_sumario.tot_regs_transacion   = v_i_cont_reg_det USING "&&&&&&&&&&"
   LET v_r_sumario.tot_aport_trasp_sar92 = "000000000000"
   -- Para quitar los ceros de v_d_suma_sar_97 se multiplica por 100
   LET v_r_sumario.tot_amort_trasp_sar97 = (v_d_suma_sar_97 * 100) USING "&&&&&&&&&&&&"
   LET v_r_sumario.tot_primas_seguro     = "000000000000"
   -- Sar_92 es cero,por lo qe NO se requiere sumar
   -- Para quitar los ceros de v_d_suma_sar_97 se multiplica por 100
   LET v_r_sumario.sum_aport_amort_prima = (v_d_suma_sar_97 * 100) USING "&&&&&&&&&&&&"
   LET v_r_sumario.filler                = "" -- 22 espacios en blanco

   -- se concatenan los datos de registro sumario
   LET v_s_registro = v_r_sumario.tpo_registro,
                      v_r_sumario.transaccion,
                      v_r_sumario.tot_regs_transacion,
                      v_r_sumario.tot_aport_trasp_sar92,
                      v_r_sumario.tot_amort_trasp_sar97,
                      v_r_sumario.tot_primas_seguro,
                      v_r_sumario.sum_aport_amort_prima,
                      v_r_sumario.filler

   -- se escribe el registro (sumario) en el archivo
   CALL v_ch_arch_cargoCapit.write([v_s_registro])

   -- se incrementa el total de registros de archivo a 2 ya que el registro final también se debe considerar
   LET v_i_total_regis_arch = v_i_total_regis_arch + 2

   -- se arma el registro Final
   LET v_r_reg_final.tpo_registro        = "4"  
   LET v_r_reg_final.tot_reg_archivo       = v_i_total_regis_arch USING "&&&&&&&&&&"
   LET v_r_reg_final.filler1               = "0000" -- 4 ceros "" -- 4 espacios en blanco
   LET v_r_reg_final.tot_glob_trasp_sar92  = "000000000000"
   LET v_r_reg_final.tot_glob_trasp_sar97  = (v_d_suma_sar_97 * 100) USING "&&&&&&&&&&&&"
   LET v_r_reg_final.tot_glob_primas_seg   = "000000000000"
   LET v_r_reg_final.sum_aport_amort_prima = (v_d_suma_sar_97 * 100) USING "&&&&&&&&&&&&"
   LET v_r_reg_final.filler2             = "" -- 22 espacios en blanco

   -- se concatena registro Final
   LET v_s_registro = v_r_reg_final.tpo_registro,
                      v_r_reg_final.tot_reg_archivo,
                      v_r_reg_final.filler1,
                      v_r_reg_final.tot_glob_trasp_sar92,
                      v_r_reg_final.tot_glob_trasp_sar97,
                      v_r_reg_final.tot_glob_primas_seg,
                      v_r_reg_final.sum_aport_amort_prima,
                      v_r_reg_final.filler2

   -- se escribe registro de cierre en el archivo
   CALL v_ch_arch_cargoCapit.write([v_s_registro])

   -- se cierra el manejador de lectura
   CALL v_ch_arch_cargoCapit.close()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   LET v_nom_fin = "A_cap_ta." || v_c_extension

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = v_c_ruta_envio CLIPPED || "/" || v_nom_fin CLIPPED

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   --LET v_s_Txt = "unix2dos "||" "||v_c_ruta_envio CLIPPED||" "||v_nom_fin CLIPPED
   --RUN v_s_Txt

   DISPLAY " Nombre del archivo cargo a capital a enviar: ",v_nom_fin
   DISPLAY ""
   DISPLAY " Ejecutando envío interfaz a ALS"

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/A_cap_ta.sh"
   RUN v_ejecuta_sh

   DISPLAY ""

   LET v_total_deudor = v_d_suma_sar_97
   OUTPUT TO REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_proceso, v_total_deudor, v_i_cont_reg_det)
      --Finaliza el reporte
   FINISH REPORT reporte_archivo_salida

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="

END MAIN

#Objetivo: Funcion que obtiene el NSS y numero de credito del trabajador en la tabla
FUNCTION fn_Obt_nss_numCred_IdCre(v_d_id_cre_acreditado)

   DEFINE v_d_id_cre_acreditado LIKE cre_acreditado.id_cre_acreditado -- id del acreditado
   DEFINE v_id_derechohabiente  LIKE cre_acreditado.id_derechohabiente --id del derechohabiente
   DEFINE v_d_num_credito       LIKE cre_acreditado.num_credito -- registro de afi derechohabiente
   DEFINE v_i_nss               LIKE afi_derechohabiente.nss -- numero de seguridad social
   DEFINE v_s_qryTxt            STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene el numero de credito para el id derechohabiente
   LET v_s_qryTxt = " SELECT FIRST 1 id_derechohabiente, num_credito\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_cre_acreditado = ",v_d_id_cre_acreditado,"\n",
                    "  ORDER BY f_otorga DESC"

   PREPARE prp_id_derechohab FROM v_s_qryTxt
   EXECUTE prp_id_derechohab INTO v_id_derechohabiente, v_d_num_credito

   -- se asigna la sentencia que obtiene el numero de credito para el id derechohabiente
   LET v_s_qryTxt = " SELECT FIRST 1 nss\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",v_id_derechohabiente

   PREPARE prp_slcFrst_nssNumCred FROM v_s_qryTxt
   EXECUTE prp_slcFrst_nssNumCred INTO v_i_nss

   RETURN v_i_nss, v_d_num_credito

END FUNCTION

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_proceso, p_total_deudor, p_count_reg)

   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod
   DEFINE p_d_folio              INTEGER
   DEFINE v_fecha_reporte        DATE
   DEFINE v_fecha_present        LIKE dis_sum_avance_pago.f_presentacion
   DEFINE p_count_reg            INTEGER
   DEFINE p_total_deudor         DECIMAL(10,2)
   DEFINE p_v_arch_proceso       CHAR(100)

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_d_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX p_v_usuario
      PRINTX p_total_deudor
      PRINTX p_count_reg
      PRINTX p_v_arch_proceso

END REPORT
