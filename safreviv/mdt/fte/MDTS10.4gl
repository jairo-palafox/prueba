--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--==============================================================================

################################################################################
#Modulo            =>MDTS                                                      #
#Programa          =>MDTS10                                                    #
#Objetivo          =>Programa que genera el archivo de salida de               # 
#                    mandatos                                                  #
#Autor             =>Alexandro Hollmann, EFP                                   #
#Fecha inicio      =>14 FEBRERO 2012                                           #
################################################################################
DATABASE safre_viv

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid              LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_arch_proceso     VARCHAR(100), -- nombre del archivo a integrar
          v_v_nom_archivo      VARCHAR(80), -- nombre del archivo de salida
          v_ch_arch_solTransf  BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_folio_liquidacion  LIKE cta_movimiento.folio_liquida,
          v_f_liquida          LIKE cta_movimiento.f_liquida, -- fecha de liquidacion
          v_r_detalle          RECORD
             --CAMPO	LONG.	POSC. DE	POSC. A	TIPO TRANSACCION	REQUERIDO	OBSERVACIONES
             id_mdt           CHAR(7)  , -- 7   1   a 7   TTCCCCC Tpo mandato y Consecutivo
             tpo_operacion    CHAR(1)  , -- 1   8   a 8   A, B y M (Altas, bajas y modificaciones)
             nivel1           CHAR(100), -- 100 9   a 108 descripcion nivel 1
             nivel2           CHAR(100), -- 100 109 a 208 filler
             nivel3           CHAR(100), -- 100 209 a 308 filler
             num_cuenta       CHAR(40) , -- 40  309 a 348
             convenio         CHAR(40) , -- 40  349 a 388
             referencia       CHAR(40) , -- 40  389 a 428
             cta_clabe        CHAR(18) , -- 18  429 a 446 ???
             fecha_captura    CHAR(8)  , -- 8   447 a 454 AAAAMMDD
             res_operacion    CHAR(2)  , -- 2   455 a 456 01 Aceptado 02 Rechazado
             diagostico       CHAR(3)    -- 3   457 a 459 ???
          END RECORD,
          v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- ID del derechohabiente
          v_pesos              LIKE cta_movimiento.monto_pesos, -- monto en pesos
          v_deudor_txt         CHAR(13), -- para obtener cifras del saldo
          v_nss                LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
          --v_num_credito        LIKE acr_transferencia.num_credito, -- numero del credito
          v_s_registro         STRING, -- registro a insertar
          v_c_ruta_env_mdt     LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_i_contrador_reg    INTEGER, -- contrador de registros
          v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_r_mandato          RECORD LIKE mdt_notifica_mandato.* 

   DEFINE tot_altas               INTEGER,
          tot_bajas               INTEGER,
          tot_modificaciones      INTEGER
   DEFINE tot_altas_proc          INTEGER,
          tot_bajas_proc          INTEGER,
          tot_modificaciones_proc INTEGER
   DEFINE p_fec_ejecucion         DATE
   
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_d_folio = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   LET p_fec_ejecucion = DATE
   
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".MDTS10.log")
   
   DISPLAY "Iniciando Generación de Archivo"
   DISPLAY "Fecha de Proceso ",today

   LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                    "   FROM mdt_notifica_mandato ",
                    "  WHERE estado = 100 ",
                    "  AND tipo_operacion = ? "
                    
   PREPARE EnuTotReg FROM v_s_qryTxt
   EXECUTE EnuTotReg USING "A" INTO tot_altas
   EXECUTE EnuTotReg USING "B" INTO tot_bajas
   EXECUTE EnuTotReg USING "M" INTO tot_modificaciones

   DISPLAY "TOTAL ALTAS: ",tot_altas,", TOTAL BAJAS: ",tot_bajas,", TOTAL MODIFICACIONES: ",tot_modificaciones

   -- asigna el folio en la variable de folio liquidación
   LET v_folio_liquidacion = p_d_folio
   LET v_c_programa_cod = "MDTS10"

   -- se invoca la función que deja la operación en estado Procesando
   {LET r_b_valida = fn_actualiza_opera_ini(p_d_pid, p_i_proceso_cod, p_i_opera_cod,
                                            p_d_folio, v_c_programa_cod,
                                            p_v_arch_proceso, p_v_usuario)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_actualiza_opera_ini"
      EXIT PROGRAM
   END IF}

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'mdt'"

   PREPARE prp_slc_rutasAcr1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutasAcr1 INTO v_c_ruta_env_mdt

   -- se crea el nombre del archivo
   LET v_v_nom_archivo = v_c_ruta_env_mdt CLIPPED || "/S" || v_folio_liquidacion || "mandato.mdt"

   -- se inicializa el contador de registros
   LET v_i_contrador_reg = 0

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   DISPLAY "Archivo generado: ", v_v_nom_archivo

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_nom_archivo, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("") 

   -- se consultan los datos que componen el cuerpo del archivo de salida
   -- del archivo de liquidacion de deudor
   LET v_s_qryTxt = "SELECT *                               \n",
                    "FROM mdt_notifica_mandato              \n",
                    "WHERE estado = 100   "
   --DISPLAY v_s_qryTxt
                    
   PREPARE sid_mandato       FROM v_s_qryTxt
   DECLARE cur_mandato       CURSOR FOR sid_mandato
   
   LET tot_altas_proc          = 0
   LET tot_bajas_proc          = 0
   LET tot_modificaciones_proc = 0
   
   FOREACH cur_mandato INTO v_r_mandato.*
      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se asignan los valores en el registro detalle
      -- LET v_r_detalle.id_mdt        = v_r_mandato.id_tpo_mandato USING "&&",v_i_contrador_reg USING "&&&&&"  -- Ajuste 20120410 Campo nuevo
      LET v_r_detalle.id_mdt        = v_r_mandato.cve_mandato -- Ajuste 20120410 Campo nuevo
      LET v_r_detalle.tpo_operacion = v_r_mandato.tipo_operacion
      LET v_r_detalle.nivel1        = v_r_mandato.descripcion1
      LET v_r_detalle.nivel2        = ""
      LET v_r_detalle.nivel3        = ""
      LET v_r_detalle.num_cuenta    = v_r_mandato.n_cuenta_bancaria
      LET v_r_detalle.convenio      = v_r_mandato.n_convenio       
      LET v_r_detalle.referencia    = v_r_mandato.n_referencia     
      LET v_r_detalle.cta_clabe     = v_r_mandato.cta_clabe
      LET v_r_detalle.fecha_captura = v_r_mandato.f_creacion USING "yyyymmdd"
      LET v_r_detalle.res_operacion = v_r_mandato.resultado_operacion
      LET v_r_detalle.diagostico    = v_r_mandato.diagnostico
      
      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.id_mdt       ,
                         v_r_detalle.tpo_operacion,
                         v_r_detalle.nivel1       ,
                         v_r_detalle.nivel2       ,
                         v_r_detalle.nivel3       ,
                         v_r_detalle.num_cuenta   ,
                         v_r_detalle.convenio     ,
                         v_r_detalle.referencia   ,
                         v_r_detalle.cta_clabe    ,
                         v_r_detalle.fecha_captura,
                         v_r_detalle.res_operacion,
                         v_r_detalle.diagostico   
      DISPLAY v_s_registro

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- Actualiza
      UPDATE mdt_notifica_mandato
         SET estado = 110
       WHERE id_mdt_notifica = v_r_mandato.id_mdt_notifica

      IF v_r_mandato.tipo_operacion = 'A' THEN
         LET tot_altas_proc          = tot_altas_proc + 1
      END IF
      IF v_r_mandato.tipo_operacion = 'B' THEN
         LET tot_bajas_proc          = tot_bajas_proc + 1
      END IF
      IF v_r_mandato.tipo_operacion = 'M' THEN
         LET tot_modificaciones_proc = tot_modificaciones_proc + 1
      END IF
      
   END FOREACH

   DISPLAY "TOTAL ALTAS INCLUIDAS: ",tot_altas_proc,", TOTAL BAJAS INCLUIDAS: ",tot_bajas_proc,", TOTAL MODIFICACIONES INCLUIDAS: ",tot_modificaciones_proc

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_actualiza_opera_fin"
      --EXIT PROGRAM
   ELSE
      -- Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(p_d_pid, 
                             p_i_proceso_cod, 
                             p_i_opera_cod, 
                             '', -- TMP AHM adjunto ?
                             'Generación de archivo a hipotecaria social',
                             'ID Proceso   : '||p_d_pid||
                             'Proceso      : '||p_i_proceso_cod||
                             'Operacion    : '||p_i_opera_cod||
                             'Fecha Inicio : '||p_fec_ejecucion||
                             'Fecha Fin    : '||TODAY
                             )
   END IF
END MAIN