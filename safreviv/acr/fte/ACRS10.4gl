--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRS10                                        #
#Objetivo          =>Programa que genera el archivo de salida de   #
#                    liquidacion para el módulo de Transferencia   #
#                    de Acreditados                                #
#Autor             =>Ivan Vega, EFP                                #
####################################################################
DATABASE safre_viv

MAIN

   DEFINE p_v_usuario          LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid              LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod      LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod        LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio            LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso     VARCHAR(100) -- nombre del archivo
   DEFINE v_v_ruta_nomarch     VARCHAR(150) -- nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp  VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_c_extension        LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_comando          STRING -- contiene al comando a correr
   DEFINE v_ch_arch_solTransf  BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_folio_liquidacion  LIKE cta_movimiento.folio_liquida
   DEFINE v_f_liquida          LIKE cta_movimiento.f_liquida -- fecha de liquidacion

   DEFINE v_r_detalle RECORD
      --CAMPO  LONG. POSC. DE  POSC. A TIPO TRANSACCION  REQUERIDO OBSERVACIONES
      filler        CHAR(5), -- Filler (1-5)
      num_credito   CHAR(10), -- Número de crédito (6-16)
      f_proceso     CHAR(8), -- Fecha de proceso (17-24) formato: DDMMAAAA
      nss           CHAR(11), -- NSS (25-35)
      deudor        CHAR(10), -- Deudor (36-45)
      punto_decimal CHAR(1), -- Puto decimal (46-46)
      decimales     CHAR(2), -- Decimales (47-48)
      entidad       CHAR(4) -- Entidad (49-52)
   END RECORD

   DEFINE v_id_cre_acreditado       LIKE cre_acreditado.id_cre_acreditado -- ID del derechohabiente
   DEFINE v_pesos                   LIKE cta_movimiento.monto_pesos -- monto en pesos
   DEFINE v_deudor_txt              CHAR(13) -- para obtener cifras del saldo
   --DEFINE v_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_nss                     LIKE afi_derechohabiente.nss -- NSS del derechohabiente
   DEFINE v_num_credito             LIKE cre_acreditado.num_credito -- numero del credito
   DEFINE v_si_tpo_originacion      LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_c_ruta_listado          LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_i_contrador_reg         INTEGER -- contrador de registros
   DEFINE v_c_programa_cod          LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_manejador_rpt           OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_total_deudor            LIKE cta_movimiento.monto_pesos -- monto en pesos
   DEFINE v_archivo_nom             STRING

   DEFINE v_s_Txt                   CHAR(100)
   DEFINE v_nom_fin                 CHAR(20)
   DEFINE v_edo_liq                 CHAR(15) 

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRS10.log")

   DISPLAY "=INICIA ACRS10="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_folio_liquidacion = p_d_folio
   --LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_si_tpo_originacion = 1 -- Transferencia de Acreditados
   LET v_total_deudor = 0 

   LET v_edo_liq = "140, 170"

   -- Valida que exista información para el folio catpurado
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_saldo_deudor\n",
                    "  WHERE id_cre_acreditado IN (\n",
                    "        SELECT UNIQUE id_cre_acreditado\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE folio_liquida = ",v_folio_liquidacion,"\n",
                    "           AND estado IN(",v_edo_liq,")\n",
                    "           AND tpo_originacion = ",v_si_tpo_originacion,")\n",
                    "    AND movimiento = 181\n",
                    "    AND monto_pesos > 0"

   PREPARE prp_cuenta_regs_liquida FROM v_s_qryTxt
   EXECUTE prp_cuenta_regs_liquida INTO v_i_contrador_reg

   IF v_i_contrador_reg = 0 THEN
      -- se invoca la función que deja la operación en estado Finalizado
      LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)

         EXIT PROGRAM
      END IF
   END IF

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'acr'"

   PREPARE prp_slc_ruta_envio FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio INTO v_c_ruta_envio, v_c_ruta_listado

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   --LET p_v_arch_proceso = "A" || v_c_fec_hoy || ".lqt"
   LET v_v_ruta_nomarch = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso CLIPPED

   DISPLAY " Archivo a generar: ", p_v_arch_proceso

   -- se inicializa el contador de registros
   LET v_i_contrador_reg = 0

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("") 

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "ACRL15"
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   IF fgl_report_loadCurrentSettings("ACRS101.4rp") THEN  -- if  the file loaded OK
       LET v_archivo_nom = p_v_usuario CLIPPED 
                          ,"-",v_c_programa_cod CLIPPED 
                          ,"-",p_d_pid USING "&&&&&"
                          ,"-", p_i_proceso_cod USING "&&&&&"
                          ,"-", p_i_opera_cod USING "&&&&&"
                          ,".pdf"

      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listado CLIPPED||"/"||v_archivo_nom)

      -- se indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings() -- commit the file settings
   ELSE
      DISPLAY "No fue posible abrir plantilla del reporte"

      EXIT PROGRAM
   END IF

   -- Inicia el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- se asignan los valores generales a todos los registros
   LET v_r_detalle.filler        = "00000" -- CHAR(5) ,-- Filler  5 1 5 ceros
   LET v_r_detalle.punto_decimal = "."     -- CHAR(1) ,-- Puto Decimal 1 46  46        punto

   DISPLAY " Crea detalle del archivo "
   -- se consultan los datos que componen el cuerpo del archivo de salida
   -- del archivo de liquidacion de deudor

   -- inicia el reporte de registros con rechazo
   LET v_s_qryTxt = " SELECT id_cre_acreditado, f_movimiento, monto_pesos\n",
                    "   FROM cre_saldo_deudor\n",
                    "  WHERE id_cre_acreditado IN (\n",
                    "        SELECT UNIQUE id_cre_acreditado\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE folio_liquida = ",v_folio_liquidacion,"\n",
                    "           AND estado IN(",v_edo_liq,")\n",
                    "           AND tpo_originacion = ",v_si_tpo_originacion,")\n",
                    "    AND movimiento = 181\n",
                    "    AND monto_pesos > 0"

   PREPARE sid_liquidadeudor FROM v_s_qryTxt
   DECLARE cur_liquidadeudor CURSOR FOR sid_liquidadeudor

   FOREACH cur_liquidadeudor INTO v_id_cre_acreditado, v_f_liquida, v_pesos
      -- se valida el importe obtenido
      IF v_pesos IS NULL OR v_pesos = 0 THEN
         CONTINUE FOREACH
      END IF

      -- se obtiene el NSS y el numero de credito del derechohabiente
      SELECT a.nss, b.num_credito
        INTO v_nss, v_num_credito
        FROM afi_derechohabiente a, cre_acreditado b
       WHERE b.id_cre_acreditado  = v_id_cre_acreditado
         AND a.id_derechohabiente = b.id_derechohabiente
         AND b.tpo_originacion    = v_si_tpo_originacion
         AND b.folio_liquida      = v_folio_liquidacion

      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se transforma el saldo a caracteres
      LET v_deudor_txt = v_pesos USING "&&&&&&&&&&.&&" --"#########&.&&"

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.num_credito   = v_num_credito USING "&&&&&&&&&&" -- CHAR(10),-- No Credito  10  6 16        num_credito
      LET v_r_detalle.f_proceso     = v_f_liquida USING "ddmmyyyy" -- CHAR(8) ,-- Fecha De Proceso  8 17  24  DD MM AAAA  FECHA DE ENVÍO A  HS - CARTERA    f_liquidacion
      LET v_r_detalle.nss           = v_nss -- CHAR(11),-- NSS  11  25  35        nss de afi_derechohabiente
      LET v_r_detalle.deudor        = v_deudor_txt[1,10] -- CHAR(10),-- Deudor  10  36  45        entero del sdo_deudor (var_pesos)
      LET v_r_detalle.decimales     = v_deudor_txt[12,13] -- CHAR(2),-- Decimales 2 47  48        decimales del sdo_deudor  (var_pesos)
      LET v_r_detalle.entidad       = "00" || v_r_detalle.num_credito[1,2] --v_num_credito -- CHAR(4)-- Entidad 4 49  52  00 y primeras dos posiciones del numero de credito

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.filler       ,
                         v_r_detalle.num_credito  ,
                         v_r_detalle.f_proceso    ,
                         v_r_detalle.nss          ,
                         v_r_detalle.deudor       ,
                         v_r_detalle.punto_decimal,
                         v_r_detalle.decimales    ,
                         v_r_detalle.entidad
                         
      LET v_total_deudor = v_total_deudor + v_pesos

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   LET v_nom_fin = "liq_deud_ta." || v_c_extension

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = v_c_ruta_envio CLIPPED || "/" || v_nom_fin CLIPPED

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   LET v_s_Txt = "unix2dos "||" "||v_c_ruta_envio CLIPPED||" "||v_nom_fin CLIPPED
   RUN v_s_Txt

   DISPLAY " Nombre del archivo liquidación deudor a enviar: ",v_nom_fin

   DISPLAY "Total Saldo Deudor: ",v_total_deudor
   -- Salida del reporte
   OUTPUT TO REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_proceso, v_total_deudor, v_i_contrador_reg)

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

#Objetivo: Funcion que obtiene el numero de credito del trabajador en la tabla
FUNCTION f_obt_num_credito(p_id_derechohabiente)

   DEFINE p_id_derechohabiente LIKE cre_acreditado.id_derechohabiente
   DEFINE v_d_num_credito      LIKE cre_acreditado.num_credito -- registro de afi derechohabiente
   DEFINE v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene el numero de credito para el id derechohabiente
   LET v_s_qryTxt = " SELECT FIRST 1 num_credito\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_derechohabiente = ",p_id_derechohabiente,"\n",
                    "  ORDER BY f_otorga DESC"

   PREPARE prp_slcUniq_numCred FROM v_s_qryTxt
   EXECUTE prp_slcUniq_numCred INTO v_d_num_credito

   RETURN v_d_num_credito

END FUNCTION

#Objetivo: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_v_usuario, p_d_folio, p_v_arch_proceso,p_total_deudor, p_count_reg)

   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod
   DEFINE p_d_folio              INTEGER
   DEFINE v_fecha_reporte        DATE
   DEFINE v_fecha_present        LIKE dis_sum_avance_pago.f_presentacion
   DEFINE p_count_reg            INTEGER
   DEFINE p_total_deudor         LIKE cta_movimiento.monto_pesos
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

