--===============================================================
-- Version: 1.0.0
-- Fecha última modificación:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRS15                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo Amortización Fondo 72 para la      #
#                    fecha de liquidación que entra como parámetro #
#                    para el módulo de Transferencia de Acreditados#
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>03 Julio 2012                                 #
####################################################################

DATABASE safre_viv

MAIN

   DEFINE p_v_usuario              LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                  LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod          LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod            LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio                LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso         VARCHAR(100) -- nombre del archivo a integrar
   DEFINE v_c_liquidacion          CHAR(8) -- fecha de presentacion del archivo formato: YYYYMMDD

   DEFINE v_r_reg_inicial RECORD
      tpo_registro                CHAR(1), -- tipo de registro
      fec_archivo                 CHAR(8), -- fecha de archivo. Formato: YYYYMMDD
      filler                      CHAR(76) -- filler
   END RECORD

   DEFINE v_r_encabezado RECORD
      tpo_registro                CHAR(1), -- tipo de registro
      transaccion                 CHAR(4), -- tranzaccion
      f_genera_archivo            CHAR(8), -- fecha de generacion del archivo
      filler                      CHAR(72) -- filler
   END RECORD

   DEFINE v_r_detalle RECORD
      tpo_registro                CHAR(1),  -- tipo de registro
      nss                         CHAR(11), -- nss
      num_credito                 CHAR(10), -- numero de credito
      periodo_pago                CHAR(6),  -- periodo de pago
      fec_pago                    CHAR(8),  -- fecha de pago
      ent_recaudadora             CHAR(3),  -- entidad recaudadora
      nrp                         CHAR(11), -- nrp
      mto_aport_trasp_sar92       CHAR(9),  -- monto de aportacion o monto de traspaso sar 92
      mto_aport_trasp_sar97       CHAR(9),  -- monto de aportacion o monto de traspaso sar 97
      num_folio_pago_sua          CHAR(6),  -- numero folio pago sua
      marca_seguro                CHAR(1),  -- marca que incluye si el pago incluye seguro de danos
      cve_rechazo                 CHAR(10)  -- claves de rechazo
   END RECORD

   DEFINE v_r_sumario RECORD
      tpo_registro                CHAR(1),  -- tipo de registro
      transaccion                 CHAR(4),  -- transacciones
      tot_regs_transacion         CHAR(10), -- total de registros detalle
      tot_aport_trasp_sar92       CHAR(12), -- total de aportaciÓn Ó monto de traspaso sar 92
      tot_amort_trasp_sar97       CHAR(12), -- total de aportaciÓn Ó monto de traspaso sar 97
      tot_primas_seguro           CHAR(12), -- total del pago de primas del seguro de danos
      sum_aport_amort_prima       CHAR(12), -- suma aportación
      filler                      CHAR(22)  -- filler
   END RECORD

   DEFINE v_r_reg_final RECORD
      tpo_registro                CHAR(1),  -- tipo de registro (4)
      tot_reg_archivo             CHAR(10), -- total de registros contenidos en archivo
      filler1                     CHAR(4),  -- cuatros espacios en blanco
      tot_glob_trasp_sar92        CHAR(12), -- total de traspaso sar 92
      tot_glob_trasp_sar97        CHAR(12), -- total de traspaso sar 97 en registro detalle
      tot_glob_primas_seg         CHAR(12), -- total primas de seguro danio
      sum_aport_amort_prima       CHAR(12), -- sma traspaso sar92 + sar97
      filler2                     CHAR(22)  -- 22 espacios en blanco
   END RECORD

   DEFINE v_r_cta_movimiento RECORD
      id_afi_fondo72              LIKE cta_fondo72.id_afi_fondo72, -- identificador de fondo 72
      subcuenta                   LIKE cta_fondo72.subcuenta, -- subcuenta
      sum_monto_pesos             LIKE cta_fondo72.importe -- suma de monto en pesos
   END RECORD

   DEFINE v_c_ruta_envio           LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_v_ruta_nomarch         VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp      VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_c_extension            LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_comando              STRING -- contiene al comando a correr
   DEFINE v_s_registro             STRING -- registro a insertar
   DEFINE v_ch_arch_amortizacion   BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_i_total_regis_arch     INTEGER -- total de registros en el archivo
   DEFINE v_i_cont_reg_det         INTEGER -- contrador de registros en la sección Detalle
   DEFINE v_c_nss                  LIKE afi_derechohabiente.nss -- numero de seguridad social
   DEFINE v_d_suma_sar_92          LIKE cta_movimiento.monto_pesos -- se acumula el monto de sar 92
   DEFINE v_d_suma_sar_97          LIKE cta_movimiento.monto_pesos -- se acumula el monto de sar 97
   DEFINE v_d_monto_pesos_sar92    LIKE cta_movimiento.monto_pesos -- monto en pesos de sar92
   DEFINE v_d_monto_pesos_sar97    LIKE cta_movimiento.monto_pesos -- monto en pesos de sar97
   DEFINE v_d_num_credito          LIKE cre_acreditado.num_credito
   DEFINE v_c_programa_cod         LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt               STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_manejador_rpt          OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_total_deudor92         LIKE cta_movimiento.monto_pesos -- monto en pesos sar 92
   DEFINE v_total_deudor97         LIKE cta_movimiento.monto_pesos -- monto en pesos sar 97
   DEFINE v_archivo_nom            STRING
   DEFINE v_c_ruta_listado         LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE r_b_valida               SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

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
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRS15.log")

   DISPLAY "=INICIA ACRS15="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_total_deudor92 = 0
   LET v_total_deudor97 = 0

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

   PREPARE prp_slc_ruta_envio FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio INTO v_c_ruta_envio

   -- se obtiene la fecha de liquidación del nombre del archivo
   LET v_c_liquidacion = p_v_arch_proceso[2,9]
   LET v_v_ruta_nomarch = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso CLIPPED

   DISPLAY " Genera archivo: ",p_v_arch_proceso

   -- se crea el manejador de archivo
   LET v_ch_arch_amortizacion = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_amortizacion.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_amortizacion.setDelimiter("")

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "ACRL46"
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   IF fgl_report_loadCurrentSettings("ACRS151.4rp") THEN  -- if  the file loaded OK
      LET v_archivo_nom = p_v_usuario CLIPPED,"-",
                          v_c_programa_cod CLIPPED,"-",
                          p_d_pid USING "&&&&&","-",
                          p_i_proceso_cod USING "&&&&&","-",
                          p_i_opera_cod USING "&&&&&",".pdf"

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

   -- se inicia el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- se inicializan variables
   LET v_i_total_regis_arch = 0
   LET v_i_cont_reg_det = 0
   LET v_d_suma_sar_97 = 0
   LET v_d_suma_sar_92 = 0

   -- se armar el registro inicial
   LET v_r_reg_inicial.tpo_registro = '0'
   LET v_r_reg_inicial.fec_archivo  = v_c_liquidacion
   LET v_r_reg_inicial.filler       = "" -- 76 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_reg_inicial.tpo_registro,
                      v_r_reg_inicial.fec_archivo,
                      v_r_reg_inicial.filler

   -- se escribe el registro (registro inicial) en el archivo
   CALL v_ch_arch_amortizacion.write([v_s_registro])

   -- se incrementa el total de registros contenidos en el archivo
   LET v_i_total_regis_arch = v_i_total_regis_arch + 1

   -- se armar registro encabezado
   LET v_r_encabezado.tpo_registro     = '1'
   LET v_r_encabezado.transaccion      = '7191'
   LET v_r_encabezado.f_genera_archivo = v_c_liquidacion
   LET v_r_encabezado.filler           = "" -- 72 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_encabezado.tpo_registro,
                      v_r_encabezado.transaccion,
                      v_r_encabezado.f_genera_archivo,
                      v_r_encabezado.filler

   -- se escribe el registro (registro inicial) en el archivo
   CALL v_ch_arch_amortizacion.write([v_s_registro])

   -- se incrementa el total de registros en el archivo
   LET v_i_total_regis_arch = v_i_total_regis_arch + 1

   -- se armar la consulta que obtiene los registros necesario para el registro detalle
   LET v_s_qryTxt = " SELECT id_afi_fondo72, subcuenta, SUM(importe)\n",
                    "   FROM cta_fondo72\n",
                    "  WHERE folio_liquida = ",p_d_folio,"\n",
                    "    AND movimiento = 452\n",
                    "  GROUP BY 1, 2"

   PREPARE prp_cta_movimiento FROM v_s_qryTxt
   DECLARE cur_cta_movimiento CURSOR FOR prp_cta_movimiento

   FOREACH cur_cta_movimiento INTO v_r_cta_movimiento.*
      -- se invoca la funcion que obtiene el nss y el númeo de crédito del trabajador
      CALL fn_obt_nss_numCredito(v_r_cta_movimiento.id_afi_fondo72) RETURNING v_c_nss, v_d_num_credito

      -- se verifica la subcuenta
      IF v_r_cta_movimiento.subcuenta = 8 THEN
         -- pertenece a movimiento sar 92. Se acumula el importe correspondiente
         LET v_d_suma_sar_92 = v_d_suma_sar_92 + v_r_cta_movimiento.sum_monto_pesos

         -- se asigna los montos que se insertarán en el registro
         LET v_d_monto_pesos_sar92 = v_r_cta_movimiento.sum_monto_pesos
         LET v_d_monto_pesos_sar97 = 0
      ELSE
         -- pertenece a movimiento sar 97. Se acumula el importe correspondiente
         LET v_d_suma_sar_97 = v_d_suma_sar_97 + v_r_cta_movimiento.sum_monto_pesos

         -- se asigna los montos que se insertarán en el registro
         LET v_d_monto_pesos_sar92 = 0
         LET v_d_monto_pesos_sar97 = v_r_cta_movimiento.sum_monto_pesos
      END IF

      -- se incrementa contador de registros de seccion [Detalle]
      LET v_i_cont_reg_det = v_i_cont_reg_det + 1

      -- se armar registro detalle
      LET v_r_detalle.tpo_registro          = '2'
      LET v_r_detalle.nss                 = v_c_nss USING "&&&&&&&&&&&"
      LET v_r_detalle.num_credito           = v_d_num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.periodo_pago          = "000000"
      LET v_r_detalle.fec_pago              = v_c_liquidacion
      LET v_r_detalle.ent_recaudadora       = "170"
      LET v_r_detalle.nrp                 = "" -- 11 espacios en blanco
      LET v_r_detalle.mto_aport_trasp_sar92 = (v_d_monto_pesos_sar92 * 100) USING "&&&&&&&&&"
      LET v_r_detalle.mto_aport_trasp_sar97 = (v_d_monto_pesos_sar97 * 100) USING "&&&&&&&&&"
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
      CALL v_ch_arch_amortizacion.write([v_s_registro])

      -- actualiza total de registros de archivo
      LET v_i_total_regis_arch = v_i_total_regis_arch + 1
   END FOREACH

   LET v_total_deudor92 = v_d_suma_sar_92
   LET v_total_deudor97 = v_d_suma_sar_97

   OUTPUT TO REPORT reporte_archivo_salida(p_v_usuario, p_d_folio,p_v_arch_proceso, v_total_deudor92,v_total_deudor97, v_i_total_regis_arch)
   --Finaliza el reporte
   FINISH REPORT reporte_archivo_salida

   -- se arma el registro sumario
   LET v_r_sumario.tpo_registro          = '3'
   LET v_r_sumario.transaccion           = '7191'
   LET v_r_sumario.tot_regs_transacion   = v_i_cont_reg_det USING "&&&&&&&&&&"
   LET v_r_sumario.tot_aport_trasp_sar92 = (v_d_suma_sar_92 * 100) USING "&&&&&&&&&&&&"
   LET v_r_sumario.tot_amort_trasp_sar97 = (v_d_suma_sar_97 * 100) USING "&&&&&&&&&&&&"
   LET v_r_sumario.tot_primas_seguro     = "000000000000"
   LET v_r_sumario.sum_aport_amort_prima = ((v_d_suma_sar_92 + v_d_suma_sar_97) * 100) USING "&&&&&&&&&&&&"
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
   CALL v_ch_arch_amortizacion.write([v_s_registro])

   -- se incrementa el total de registros de archivo a 2 ya que el registro final también se debe considerar
   LET v_i_total_regis_arch = v_i_total_regis_arch + 2

   -- se arma el registro Final
   LET v_r_reg_final.tpo_registro        = "4"
   LET v_r_reg_final.tot_reg_archivo       = v_i_total_regis_arch USING "&&&&&&&&&&"
   LET v_r_reg_final.filler1               = "0000" -- 4 ceros "" -- 4 espacios en blanco
   LET v_r_reg_final.tot_glob_trasp_sar92  = (v_d_suma_sar_92 * 100) USING "&&&&&&&&&&&&"
   LET v_r_reg_final.tot_glob_trasp_sar97  = (v_d_suma_sar_97 * 100) USING "&&&&&&&&&&&&"
   LET v_r_reg_final.tot_glob_primas_seg   = "000000000000"
   LET v_r_reg_final.sum_aport_amort_prima = ((v_d_suma_sar_92 + v_d_suma_sar_97) * 100) USING "&&&&&&&&&&&&"
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
   CALL v_ch_arch_amortizacion.write([v_s_registro])

   -- se cierra el manejador de lectura
   CALL v_ch_arch_amortizacion.close()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   LET v_nom_fin = "A_cred_f_ta." || v_c_extension

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = v_c_ruta_envio CLIPPED || "/" || v_nom_fin CLIPPED

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   --LET v_s_Txt = "unix2dos "||" "||v_c_ruta_envio CLIPPED||" "||v_nom_fin CLIPPED
   --RUN v_s_Txt

   DISPLAY " Nombre del archivo amortización a enviar: ",v_nom_fin
   DISPLAY ""
   DISPLAY " Ejecutando envío interfaz a ALS"

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/A_cred_f_ta.sh"
   RUN v_ejecuta_sh

   DISPLAY ""

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

#Objetivo: Funcion que obtiene el numero de credito del trabajador en la tabla
FUNCTION fn_obt_nss_numCredito(p_id_afi_fondo72)

   DEFINE p_id_afi_fondo72     LIKE afi_fondo72.id_afi_fondo72 --id del fondo72
   DEFINE v_d_num_credito      LIKE cre_acreditado.num_credito -- registro de afi derechohabiente
   DEFINE v_i_nss              LIKE afi_derechohabiente.nss -- numero de seguridad social
   DEFINE v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene el nss apartir del id fondo 72
   LET v_s_qryTxt = " SELECT FIRST 1 nss\n",
                    "   FROM afi_fondo72\n",
                    "  WHERE id_afi_fondo72 = ",p_id_afi_fondo72

   PREPARE prp_slcFrst_nss FROM v_s_qryTxt
   EXECUTE prp_slcFrst_nss INTO v_i_nss

   -- se asigna la sentencia que obtiene el numero de credito para el nss obtenido
   LET v_s_qryTxt = " SELECT FIRST 1 num_credito\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_derechohabiente IN (\n",
                    "         SELECT id_derechohabiente\n",
                    "           FROM afi_derechohabiente\n",
                    "          WHERE nss = '",v_i_nss CLIPPED,"')\n",
                    "  ORDER BY f_otorga DESC"

   PREPARE prp_slcFrst_numCred FROM v_s_qryTxt
   EXECUTE prp_slcFrst_numCred INTO v_d_num_credito

   -- si no se obtuvo numero de crédito se asigna cero
   IF v_d_num_credito IS NULL THEN
      LET v_d_num_credito = 0
   END IF

   RETURN v_i_nss, v_d_num_credito

END FUNCTION

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_proceso, p_total_deudor92, p_total_deudor97, p_count_reg)

   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod
   DEFINE p_d_folio              INTEGER
   DEFINE v_fecha_reporte        DATE
   DEFINE v_fecha_present        LIKE dis_sum_avance_pago.f_presentacion
   DEFINE p_count_reg            INTEGER
   DEFINE p_total_deudor92       DECIMAL(10,2)
   DEFINE p_total_deudor97       DECIMAL(10,2)
   DEFINE p_v_arch_proceso       CHAR(100)

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte

      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_d_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX p_v_usuario
      PRINTX p_total_deudor92
      PRINTX p_total_deudor97
      PRINTX p_count_reg
      PRINTX p_v_arch_proceso

END REPORT
