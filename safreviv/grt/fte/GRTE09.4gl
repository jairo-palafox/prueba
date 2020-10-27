--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Modulo            => GRT                                           #
#Programa          => GRTE09                                        #
#Objetivo          => Programa para integrar el archivo de Saldos   #
#                     Transferidos que ha sido validado para el     #
#                     módulo de Uso de Garantía 43 bis              #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 26 Abril 2012                                 #
#Modifica:         => Mauro Muñiz Caballero                         #
#Fecha modif:      => 23 de marzo de 2016                           #
#Adecuación        => Eliminación de adelantos                      #
#####################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl"

#Objetivo: Función que realiza la integración para el archivo que entra como parámetro
MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operación
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar
   DEFINE v_d_id_cre_ctr_arch       LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                 LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados                 LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
      tot_rechazados                LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
      tot_sin_origen                LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_operacion             LIKE cre_ctr_archivo.operacion -- operación
   DEFINE v_dt_f_lote               LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_si_lote                 LIKE cre_ctr_archivo.lote -- lote
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_isam_err                INTEGER
   DEFINE r_c_msj                   VARCHAR(250)
   DEFINE r_c_nss                   LIKE afi_derechohabiente.nss

   -- se recuperan los parámetros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE09.log")

   DISPLAY "=INICIA GRTE09="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 9 -- Saldos Transferidos
   LET v_si_id_proceso = g_id_proceso_grt_uso -- Uso de Garantia 43 bis

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la función que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_sdos_transf()

   -- se busca numero de lote y fecha correspondiente al archivo
   LET v_s_qryTxt = " SELECT f_presentacion, cons_lote\n",
                    "   FROM safre_tmp:tmp_sdo_transf_enc_uso"

   PREPARE prp_fec_lote FROM v_s_qryTxt
   EXECUTE prp_fec_lote INTO v_dt_f_lote, v_si_lote

   -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
   LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE lote = ",v_si_lote,"\n",
                    "    AND f_lote = '",v_dt_f_lote,"'\n",
                    "    AND id_proceso = ",v_si_id_proceso,"\n",
                    "    AND operacion = ",v_i_operacion,"\n",
                    "    AND estado = 10\n",
                    "  ORDER BY id_cre_ctr_archivo DESC"

   PREPARE prp_id_creCtrArchivo FROM v_s_qryTxt
   EXECUTE prp_id_creCtrArchivo INTO v_d_id_cre_ctr_arch

   -- se verifica si fue posible obtener el identificador del archivo
   IF v_d_id_cre_ctr_arch IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   END IF

   DISPLAY " INTEGRA SALDOS"
   -- se crea la sentencia que ejecuta el procedure que integra archivo de saldos transferidos
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_uso_integra_sdos_transf(?,?,?,?)"

   PREPARE prp_integra_sdos_transf FROM v_s_qryTxt
   EXECUTE prp_integra_sdos_transf USING p_v_usuario,
                                         p_v_arch_proceso,
                                         p_d_folio,
                                         v_d_id_cre_ctr_arch
                                    INTO r_b_valida,
                                         r_isam_err,
                                         r_c_msj,
                                         r_c_nss   

     -- verifica si ocurrió un error en la integración
   IF r_b_valida <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE INTEGRACIÓN"
      DISPLAY "CÓD. ERROR : ",r_b_valida
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY " TOTAL REGISTROS : ",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOTAL ACEPTADOS : ",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS: ",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " TOTAL SIN ORIGEN: ",v_r_cre_ctr_archivo.tot_sin_origen

   -- se invoca la función que genera el reporte (texto plano)
   CALL fn_genera_rep_proc(v_d_id_cre_ctr_arch, p_i_proceso_cod, p_i_opera_cod)

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   DISPLAY " EJECUTA CONCILIACIÓN"
   -- se crea el comando que ejecuta la conciliación de Rechazo de Saldos
   LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/GRTP13 ",
                                           p_v_usuario, " ",
                                           p_d_pid, " ",
                                           p_i_proceso_cod, " ",
                                           p_i_opera_cod, " ",
                                           p_d_folio, " ",
                                           p_v_arch_proceso,
                                           " 1>> ",v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",
                                           p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&",
                                           " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando

END MAIN

#Objetivo: Función que crea la tabla temporal de la integración de saldos transferidos
FUNCTION fn_crea_tmp_sdos_transf()

   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_deudor_saldo_grt

   WHENEVER ERROR STOP

   CREATE TABLE tmp_deudor_saldo_grt(id_cre_acreditado DECIMAL(9,0),
                                     id_derechohabiente DECIMAL(9,0),
                                     nss CHAR(11))

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION

#Objetivo: Función que genera el reporte de los registros integrados
FUNCTION fn_genera_rep_proc(p_id_cre_ctr_archivo, p_i_proceso_cod, p_i_opera_cod)

   DEFINE p_id_cre_ctr_archivo      LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de archivo
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE v_v_arch_salida           VARCHAR(100) -- nombre del archivo de salida
   DEFINE v_v_ruta_archivo          VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_ch_arch_reporte         BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_c_nss                   CHAR(11) -- NSS del trabajador
   DEFINE v_c_periodo_pago          CHAR(6) -- periodo de pago
   DEFINE v_id_cre_uso_garantia     DECIMAL(9,0)

   DEFINE v_r_rpt_fechas_liq RECORD
      nss                           CHAR(11),
      fec_liquida                   CHAR(8),
      fec_concilia                  CHAR(8)
   END RECORD

   DEFINE v_d_folio_liquida         DECIMAL(9,0) -- folio de liquidación
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_i_contrador_reg         INTEGER -- contrador de registros
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_c_ruta_listado          LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo

   -- se inicializan variables
   LET v_c_fec_hoy       = TODAY USING "yyyymmdd"
   LET v_i_contrador_reg = 0 -- contador de registros

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_arch_salida = "Grt" || v_c_fec_hoy || "." || v_c_extension CLIPPED
   DISPLAY " REPORTE SALIDA (FECHAS LIQUIDACIÓN): ", v_v_arch_salida

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado

   -- se crea el manejador de archivo
   LET v_ch_arch_reporte = base.Channel.create()

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT nss, per_pago\n",
                    "   FROM safre_tmp:tmp_sdo_transf_det_uso"

   PREPARE prp_tmp_solic_sdo FROM v_s_qryTxt
   DECLARE cur_tmp_solic_sdo CURSOR FOR prp_tmp_solic_sdo

   FOREACH cur_tmp_solic_sdo INTO v_c_nss, v_c_periodo_pago
      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se obtiene el id derechohabiente para el nss en proceso
      LET v_s_qryTxt = " SELECT id_cre_uso_garantia\n",
                       "   FROM safre_tmp:tmp_uso_solic_sdo\n",
                       "  WHERE nss = '",v_c_nss CLIPPED,"'",
                       "    AND periodo_pago = '",v_c_periodo_pago CLIPPED,"'"

      PREPARE prp_id_cre_ug FROM v_s_qryTxt
      DECLARE cur_id_cre_ug CURSOR FOR prp_id_cre_ug

      FOREACH cur_id_cre_ug INTO v_id_cre_uso_garantia
         -- se obtiene el folio de liquidación y el identificador del derechohabiente
         LET v_s_qryTxt = " SELECT folio_liquida\n",
                          "   FROM cre_uso_garantia\n",
                          "  WHERE id_cre_uso_garantia =\n",v_id_cre_uso_garantia,
                          "    AND folio_liquida > 0"

         PREPARE prp_slct_folLiq FROM v_s_qryTxt
         EXECUTE prp_slct_folLiq INTO v_d_folio_liquida

         IF v_d_folio_liquida IS NOT NULL AND v_d_folio_liquida > 0 THEN
            -- se asignan los valores al registro a insertar en archivo (fechas liquidación)
            LET v_r_rpt_fechas_liq.nss          = v_c_nss
            LET v_r_rpt_fechas_liq.fec_liquida  = fn_obtn_fch_lqdcn(v_d_folio_liquida)
            LET v_r_rpt_fechas_liq.fec_concilia = TODAY USING "yyyymmdd"
            
            -- se concatenan los campos a insertar
            LET v_s_registro = v_r_rpt_fechas_liq.nss,
                               v_r_rpt_fechas_liq.fec_liquida,
                               v_r_rpt_fechas_liq.fec_concilia
            
            -- se escribe el registro (montos iguales) en el archivo
            CALL v_ch_arch_reporte.write([v_s_registro])
         END IF
      END FOREACH
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte.close()

END FUNCTION

#Objetivo: Función que selecciona y regresa la fecha de liquidación del folio de liquidación
#          que entra como parámentro
FUNCTION fn_obtn_fch_lqdcn(p_d_folio_liquida)

   DEFINE p_d_folio_liquida         LIKE cta_movimiento.folio_liquida
   DEFINE p_d_f_liquida             LIKE cta_movimiento.f_liquida
   DEFINE v_s_sqlQry                STRING
   DEFINE v_criterio                SMALLINT
   DEFINE v_s_qryTxt                STRING
   DEFINE v_fecha                   DATE
   DEFINE v_tabla                   CHAR(20)
   DEFINE v_c_f_liquida             CHAR(8)

   LET v_criterio = 0
   LET v_fecha    = ""

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

   PREPARE prp_obt_fmov FROM v_s_qryTxt
   EXECUTE prp_obt_fmov USING v_criterio,
                             p_d_folio_liquida,
                             v_fecha
                        INTO v_tabla

   -- se selecciona la descripción del tipo de credito
   LET v_s_sqlQry = " SELECT FIRST 1 f_liquida\n",
                    "   FROM ",v_tabla,"\n",
                    "  WHERE folio_liquida = ",p_d_folio_liquida,"\n",
                    "    AND f_liquida IS NOT NULL"

display v_s_sqlQry

   PREPARE prp_slct_fLiquida FROM v_s_sqlQry
   EXECUTE prp_slct_fLiquida INTO p_d_f_liquida

   LET v_c_f_liquida = p_d_f_liquida USING "ddmmyyyy"

   RETURN v_c_f_liquida

END FUNCTION
