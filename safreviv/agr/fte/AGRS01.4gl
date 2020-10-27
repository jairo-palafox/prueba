######################################################################
#Módulo            => AGR                                            #
#Programa          => AGRS01                                         #
#Objetivo          => Programa que genera el archivo de salida de    #
#                     liquidación para el módulo de Anualidades      #
#                     Garantizadas                                   #
#Autor             => Daniel Buendia, EFP                            #
#Fecha inicio      => 28 Marzo 2012                                  #
#Modifica:         => Mauro Muñiz Caballero                          #
#Fecha modif:      => 24 de mayo de 2016                             #
#Adecuación        => Liquidación proyectada al 1 día siguiente mes  #
#                     Generación de archivos de deudor y liquidación #
#                     de usos de anualidad desde la preliquidación   #
#Modifica          => Emilio Abarca, EFP.                            #
#Fecha modifica    => 12/Noviembre/2018                              #
#Adecuación        => Adecuación a los archivos de salida y PDF.     #
######################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

   DEFINE m_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE m_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE m_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE m_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE m_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE m_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar
   DEFINE m_tpo_liq                 SMALLINT
   DEFINE v_ejecuta_sh              STRING
   DEFINE v_criterio                SMALLINT
   DEFINE v_tabla                   CHAR(20)
   DEFINE v_f_pivote                DATE

   #Array para el reporte
   DEFINE arr_archivos              DYNAMIC ARRAY OF RECORD
       nombre_archivo   CHAR(20),
       total            INTEGER,
       aivs92           DECIMAL(16,6),
       aivs97           DECIMAL(16,6),
       pesos            DECIMAL(16,2)
   END RECORD
   DEFINE r_saldo          RECORD
      aivs92     DECIMAL(16,6),  
      pesos92    DECIMAL(12,2),
      aivs97     DECIMAL(16,6),
      pesos97    DECIMAL(12,2)
   END RECORD
   DEFINE arr_inf_lqa  DYNAMIC ARRAY OF RECORD
      detalle      CHAR(15),
      total        INTEGER,
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      pesos        DECIMAL(16,2),
      porcentaje   CHAR(12)
   END RECORD 
    #Causal rechazos saci
   DEFINE arr_rch_saci DYNAMIC ARRAY OF RECORD
      total       INTEGER,
      aivs92      DECIMAL(16,6),
      pesos92     DECIMAL(12,2),
      aivs97      DECIMAL(16,6),
      pesos97    DECIMAL(12,2),
      total_pesos DECIMAL(16,2),
      estado      SMALLINT,
      causal_desc CHAR(40)
   END RECORD 
   --Causal rechazos procesar
   DEFINE arr_rch_procesar DYNAMIC ARRAY OF RECORD
      total       INTEGER,
      aivs92      DECIMAL(16,6),
      pesos92     DECIMAL(12,2),
      aivs97      DECIMAL(16,6),
      pesos97     DECIMAL(12,2),
      total_pesos DECIMAL(16,2),
      diagnostico CHAR(3),
      causal_desc CHAR(100)
   END RECORD
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_c_ruta_listado LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_c_programa_cod LIKE cat_operacion.programa_cod
   DEFINE v_arh_rechazos   STRING   --Archivo de rechazos
   
MAIN

   DEFINE v_c_ruta_env_agr          LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_si_tpo_originacion      LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_c_ruta_listado          LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE r_b_existe_error          SMALLINT -- booleana que indica si ocurrió un error durante el proceso
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_hoy                     DATE
   DEFINE v_t_mes_ant               INTEGER

   -- se recuperan los parÁmetros que envia el programa lanzador
   LET m_v_usuario      = ARG_VAL(1)
   LET m_d_pid          = ARG_VAL(2)
   LET m_i_proceso_cod  = ARG_VAL(3)
   LET m_i_opera_cod    = ARG_VAL(4)
   LET m_d_folio        = ARG_VAL(5)
   LET m_v_arch_proceso = ARG_VAL(6)
   LET m_tpo_liq        = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(m_v_usuario CLIPPED|| ".AGRS01.log")

   DISPLAY "=INICIA AGRS01="
   DISPLAY " USUARIO       : ",m_v_usuario
   DISPLAY " PID           : ",m_d_pid
   DISPLAY " FOLIO         : ",m_d_folio USING "#########&"

   -- se inicializan variables
   LET v_si_tpo_originacion = 4 -- Anualidades Garantizadas
   LET v_c_fec_hoy          = TODAY USING "yyyymmdd" -- se asigna la fecha de hoy para nombrar los archivos

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados,ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_rutasAgr FROM v_s_qryTxt
   EXECUTE prp_slc_rutasAgr INTO v_c_ruta_env_agr, v_c_ruta_listado,v_ruta_bin

   --Nombre del programa
   LET v_c_programa_cod = fn_obten_nom_programa(m_i_proceso_cod , m_i_opera_cod) --Programa AGRL13

   --crea temporal
   CALL crea_temporal()

   DISPLAY " Genera archivo de Uso de Anualidad"
   -- se ejecuta la función que genera el archivo de salida de USO ANUALIDAD
   LET r_b_existe_error = fn_gen_archivo_uso_anualidad(v_c_ruta_env_agr, v_c_fec_hoy)

   -- verifica si existió error en el proceso
   IF r_b_existe_error THEN
      DISPLAY "No se generó archivo de liquidación de uso de anulidad y uso de garantía AG"
   END IF

   DISPLAY ""
   DISPLAY " Genera archivo de Liquidación de Deudor"

   IF m_tpo_liq IS NULL OR m_tpo_liq = "" THEN
     LET m_tpo_liq = 2
   END IF

   -- se ejecuta la función que genera el archivo de salida de LIQUIDACIÓN AGR
   LET r_b_existe_error = fn_gen_archivo_liquidacion_agr(v_c_ruta_env_agr, v_c_fec_hoy, v_si_tpo_originacion, m_tpo_liq)

   -- verifica si existió error en el proceso
   IF r_b_existe_error THEN
      -- se marca como ERROR el proceso
      LET r_b_valida = fn_error_opera(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

      IF r_b_valida <> 0 THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- Genera archivo de registros liquidados de meses anteriores.
   LET v_hoy = TODAY 
   LET v_f_pivote = v_hoy - DAY(v_hoy) + 1

   DISPLAY "Genera archivo meses anteriores"
   DISPLAY "Fecha pivote reg. meses ant.: ",v_f_pivote
   DISPLAY ""

   SELECT COUNT(*)
      INTO v_t_mes_ant
      FROM cre_uso_garantia
     WHERE folio_liquida = m_d_folio
       AND importe_v97 > 0
       AND f_proceso < v_f_pivote

   IF(v_t_mes_ant = 0) THEN
      DISPLAY " "
      DISPLAY " ADVERTENCIA: No se encontró información correspondiente a meses anteriores "
   ELSE
      DISPLAY ""
      DISPLAY " Genera archivo Uso Anualidades y Garantía de meses anteriores"
      CALL fn_gen_archivo_uso_mes_ant(v_c_ruta_env_agr)
   END IF 

   #Genera archivo de respuesta Procesar (Rechazos, Devoluciones, No Atendidad)
   CALL fn_gen_archivo_respuesta_prcr(v_c_ruta_env_agr)
   
   # GENERA PDF
   DISPLAY ""
   CALL informacion_reporte()
   DISPLAY " Genera reporte PDF ...Completado"

   IF m_tpo_liq = 2 THEN
      -- se invoca la función que deja la operación en estado Finalizado
      LET r_b_valida = fn_actualiza_opera_fin(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)

         --EXIT PROGRAM
      END IF
   END IF

   DISPLAY "=FIN="

END MAIN

#Objetivo: Se genera el archivo de liquidación de Uso de la Anualidad
FUNCTION fn_gen_archivo_uso_anualidad(p_c_ruta_envio, p_c_fec_hoy)

   DEFINE p_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE p_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_r_cre_uso_garantia      RECORD LIKE cre_uso_garantia.* -- registro de cre uso garantia

   DEFINE v_r_detalle RECORD
      tipo_registro                 CHAR(2), -- tipo de registro de detalle (1-2)
      nss                           CHAR(11), -- número de seguro social (3-13)
      num_credito                   CHAR(10), -- número de crédito del trabajador (14-23)
      fec_envio                     CHAR(8), -- fecha de envío de archivo (24-31)
      marca                         CHAR(1), -- tipo de marca que tiene registrada (32-32)
      monto_recuperado              CHAR(15), -- monto de la anualidad o uso de garantía (pesos) (33-47)
      fikey_kk                      CHAR(16), -- clave de reconciliación para contabilidad principal (48-63)
      estatus                       CHAR(1), -- estatus de la solicitud (64-64)
      codigo_rechazo                CHAR(2), -- código de rechazo (65-66)
      causal_rechazo                CHAR(3) -- Causal de rechazo (67-69)
   END RECORD

   DEFINE v_r_sumario RECORD
      tipo_registro                 CHAR(2), -- tipo de registro de sumario
      tot_registros                 CHAR(9), -- total de registros de detalle
      tot_montos                    CHAR(15), -- suma de los montos recuperados
      filler                        CHAR(43) -- filler para completar largo de registro
   END RECORD

   DEFINE v_d_suma_montos           DECIMAL(13,2) -- suma de los montos recuperados
   DEFINE v_c_tpo_transferencia     LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
   DEFINE v_d_id_cre_ctr_archivo    LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_r_cre_ctr_archivo       RECORD LIKE cre_ctr_archivo.* -- regsitro de cre ctr archivo
   DEFINE v_dt_fec_present          DATE -- fecha de presentacion
   DEFINE v_c_fec_present_ax        CHAR(8) -- fecha auxiliar de presentacion con formato YYYYMMDD
   DEFINE v_v_ruta_nomarch          VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp       VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_ch_arch_solTransf       BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_i_num_registros         INTEGER -- numero de registros
   DEFINE v_i_contrador_reg         INTEGER -- contrador de registros
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_b_existe_error          SMALLINT -- booleana que indica si ocurrió un error durante el proceso        
   DEFINE r_c_nss                   LIKE afi_derechohabiente.nss -- registro de afi derechohabiente
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE v_s_Txt                   CHAR(100)
   DEFINE v_nom_fin                 CHAR(20)

   -- se inicializan variables
   LET v_c_tpo_transferencia = "43" -- Anualidades Garantizadas
   LET v_b_existe_error      = FALSE -- se asume que no existirá error en el proceso
   LET v_i_num_registros     = 0 -- numero de registros a procesar
   LET v_d_suma_montos       = 0 -- suma del monto
   LET v_dt_fec_present      = TODAY - DAY(TODAY) + 1 -- se crea la fecha. Primer dia del mes   

   {
   IF m_tpo_liq = 1 THEN
      LET v_dt_fec_present = v_dt_fec_present + 1 UNITS MONTH
   END IF
   }

   -- se obtiene el identificador del archivo
   LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE folio_liquida = ",m_d_folio

---display v_s_qryTxt

   PREPARE prp_id_ctr_archivo FROM v_s_qryTxt
   EXECUTE prp_id_ctr_archivo INTO v_d_id_cre_ctr_archivo

   -- verifica si se encontró identificador de archivo
   IF v_d_id_cre_ctr_archivo IS NULL THEN
      DISPLAY " MENSAJE: No se encontró información correspondiente a Uso de Anualidad"

      RETURN v_b_existe_error
   END IF

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_dt_fec_present CLIPPED, "',2)" 

   PREPARE prp_fn_habil_siguiente FROM v_s_qryTxt
   EXECUTE prp_fn_habil_siguiente INTO v_dt_fec_present

   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fec_present_ax = v_dt_fec_present USING "YYYYMMDD"

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   DISPLAY " Nombre del archivo: ",m_v_arch_proceso

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch = p_c_ruta_envio CLIPPED || "/" || m_v_arch_proceso CLIPPED

   -- se inicializa el contador de registros
   LET v_i_contrador_reg = 0

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("") 

   -- se consultan  todos los registro de cre acreditado con edo_procesar = 70 y con estado = 140
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE folio_liquida = ",m_d_folio,
                    "    AND importe_v97 > 0"

   PREPARE prp_his_transferencia FROM v_s_qryTxt
   DECLARE cur_his_transferencia CURSOR FOR prp_his_transferencia

   LET  r_saldo.aivs92  = 0    
   LET  r_saldo.pesos92 = 0   
   LET  r_saldo.aivs97  = 0 
   LET  r_saldo.pesos97 = 0

   FOREACH cur_his_transferencia INTO v_r_cre_uso_garantia.*
      -- se invoca la funcion que obtiene los datos del trabajador
      CALL f_obt_datos_trab(v_r_cre_uso_garantia.id_derechohabiente) RETURNING r_c_nss

      -- se asigna el estatus correspondiente según el estado del registro en proceso
      -- RECHAZOS SACI
      IF v_r_cre_uso_garantia.estado = 150 OR
         v_r_cre_uso_garantia.estado = 240 OR 
         -- RECHAZOS PROCESAR
         v_r_cre_uso_garantia.edo_procesar = 90  OR
         v_r_cre_uso_garantia.edo_procesar = 100 OR 
         v_r_cre_uso_garantia.edo_procesar = 110 THEN
         
         LET v_r_detalle.estatus = 2
         LET v_r_detalle.codigo_rechazo = "02"
         LET v_r_detalle.causal_rechazo = v_r_cre_uso_garantia.diagnostico

         # CAUSALES DE RECHAZO
         IF v_r_cre_uso_garantia.estado = 150 OR
            v_r_cre_uso_garantia.estado = 240 THEN
            LET v_r_detalle.causal_rechazo = v_r_cre_uso_garantia.estado
         ELSE 
           --Cuando es rechazado por PROCESAR
            CASE 
               WHEN (v_r_cre_uso_garantia.edo_procesar = 90) OR ( v_r_cre_uso_garantia.edo_procesar = 100)
               LET v_r_detalle.causal_rechazo = v_r_cre_uso_garantia.diagnostico

               WHEN (v_r_cre_uso_garantia.edo_procesar = 110)
               LET v_r_detalle.causal_rechazo = "0NA"
                  
            END CASE 
         END IF
         
      ELSE
         LET v_r_detalle.estatus = 1
         LET v_r_detalle.codigo_rechazo = ""
         LET v_r_detalle.causal_rechazo = ""
      END IF

       #Obtiene saldo ssv
      CALL obtiene_ssv(v_r_cre_uso_garantia.id_derechohabiente,m_d_folio) RETURNING r_saldo.* 
      
      -- se asignan los valores en el registro detalle
      LET v_r_detalle.tipo_registro     = "02"
      LET v_r_detalle.nss               = r_c_nss
      LET v_r_detalle.num_credito       = v_r_cre_uso_garantia.num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.fec_envio         = TODAY USING "YYYYMMDD"
      LET v_r_detalle.marca             = 4 -- marca que se envia a procesar
      LET v_r_detalle.monto_recuperado  = fn_monto_recuperado(v_r_cre_uso_garantia.id_derechohabiente, v_r_detalle.codigo_rechazo, v_r_cre_uso_garantia.id_cre_uso_garantia, m_tpo_liq) -- se recupera el monto liquidado en pesos
      LET v_r_detalle.fikey_kk          = "" -- 16 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tipo_registro,
                         v_r_detalle.nss,
                         v_r_detalle.num_credito,
                         v_r_detalle.fec_envio,
                         v_r_detalle.marca,
                         v_r_detalle.monto_recuperado,
                         v_r_detalle.fikey_kk,
                         v_r_detalle.estatus,
                         v_r_detalle.codigo_rechazo,
                         v_r_detalle.causal_rechazo

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se acumula el monto recuperado
      LET v_d_suma_montos = v_d_suma_montos + (v_r_detalle.monto_recuperado / 100)

      --Guarda en temporal para el reporte
      INSERT INTO safre_tmp:tmp_liq_ag(
                        id_referencia, 
                        id_derechohabiente, 
                        estado       ,  
                        edo_procesar ,   
                        diagnostico  ,  
                        aivs92       ,
                        pesos92      ,
                        aivs97       ,
                        pesos97      ,
                        tpo_arh) 
               VALUES (v_r_cre_uso_garantia.id_cre_uso_garantia,
                       v_r_cre_uso_garantia.id_derechohabiente,
                       v_r_cre_uso_garantia.estado,
                       v_r_cre_uso_garantia.edo_procesar,
                       v_r_cre_uso_garantia.diagnostico,
                       r_saldo.aivs92,
                       r_saldo.pesos92,
                       r_saldo.aivs97,
                       r_saldo.pesos97,
                       "lqa");
   END FOREACH

   -- se asignan los valores del registro sumario
   LET v_r_sumario.tipo_registro = "09"
   LET v_r_sumario.tot_registros = v_i_contrador_reg USING "&&&&&&&&&"
   --LET v_r_sumario.tot_montos    = (v_d_suma_montos * 100) USING "&&&&&&&&&&&&&&&"
   LET v_r_sumario.tot_montos    = v_d_suma_montos USING "&&&&&&&&&&&&&&&"
   LET v_r_sumario.filler        = "" -- 33 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_sumario.tipo_registro,
                      v_r_sumario.tot_registros,
                      v_r_sumario.tot_montos,
                      v_r_sumario.filler

   -- se escribe el registro (sumario) en el archivo
   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(m_i_proceso_cod, m_i_opera_cod)

   LET v_nom_fin = "Liq_ag." || v_c_extension

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = p_c_ruta_envio CLIPPED || "/" || v_nom_fin CLIPPED

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   --LET v_s_Txt = "unix2dos "||" "||p_c_ruta_envio CLIPPED||" "||v_nom_fin CLIPPED
   --RUN v_s_Txt

   DISPLAY " Nombre del archivo UA a enviar: ",v_nom_fin
   DISPLAY ""
   --DISPLAY " Ejecutando envío interfaz SAS"

   --LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/liq_ag.sh"
   --RUN v_ejecuta_sh

   DISPLAY ""

   -- Salida del reporte
   --OUTPUT TO REPORT reporte_archivo_salida(m_v_arch_proceso,v_r_sumario.tot_montos, v_i_contrador_reg)

   RETURN v_b_existe_error

END FUNCTION

#Objetivo: Se genera el archivo de liquidación
FUNCTION fn_gen_archivo_liquidacion_agr(p_c_ruta_envio, p_c_fec_hoy, p_si_tpo_originacion, p_tpo_liq)

   DEFINE p_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE p_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE p_si_tpo_originacion      LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_v_ruta_nomarch          VARCHAR(150) -- nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp       VARCHAR(100) -- ruta y nombre del archivo de salida
   --DEFINE v_c_extension              LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_ch_arch_solTransf       BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_f_liquida               LIKE cta_movimiento.f_liquida -- fecha de liquidacion
   DEFINE p_tpo_liq                 SMALlINT

   DEFINE v_r_detalle RECORD
      filler                        CHAR(5), -- Filler       5   01-05
      num_credito                   CHAR(10),-- No Credito  10   06-16
      f_proceso                     CHAR(8) ,-- F Proceso    8   17-24
      nss                           CHAR(11),-- NSS         11   25-35
      deudor                        CHAR(10),-- Deudor      10   36-45
      punto_decimal                 CHAR(1), -- Puto Decimal 1   46-46
      decimales                     CHAR(2), -- Decimales    2   47-48
      entidad                       CHAR(4)  -- Entidad      4   49-52
   END RECORD

   DEFINE v_d_sum_tot_deudor        DECIMAL(16,2) -- suma total del deudor
   DEFINE v_id_cre_acreditado       LIKE cre_acreditado.id_cre_acreditado -- ID del derechohabiente
   DEFINE v_d_monto_pesos           LIKE cta_movimiento.monto_pesos -- monto en pesos
   DEFINE v_nss                     LIKE afi_derechohabiente.nss -- NSS del derechohabiente
   DEFINE v_num_credito             LIKE cre_acreditado.num_credito -- numero del credito
   DEFINE v_deudor_txt              CHAR(13) -- para obtener cifras del saldo
   DEFINE v_i_num_registros         INTEGER -- numero de registros
   DEFINE v_i_contrador_reg         INTEGER -- contrador de registros
   DEFINE v_b_existe_error          SMALLINT -- booleana que indica si ocurrió un error durante el proceso
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_aux_id_derechohabiente  DECIMAL(9,0)

   DEFINE v_s_Txt                   CHAR(100)
   DEFINE v_nom_fin                 CHAR(20)
   DEFINE v_edo_liq                 CHAR(15)

   -- se inicializan variables
   LET v_b_existe_error   = FALSE -- se asume que no existirá error en el proceso
   LET v_d_sum_tot_deudor = 0
   LET v_i_contrador_reg  = 0

   IF p_tpo_liq = 2 THEN
      LET v_edo_liq = "140, 148, 170"
   ELSE
      LET v_edo_liq = "130, 138, 137, 330"
   END IF

   -- se valida que exista información para generar el archivo
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_saldo_deudor\n",
                    "  WHERE id_cre_acreditado IN (\n",
                    "        SELECT id_cre_acreditado\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE folio_liquida = ", m_d_folio, "\n",
                    "           AND estado IN(",v_edo_liq,")\n",
                    "           AND tpo_originacion = ",p_si_tpo_originacion,")\n",
                    "    AND movimiento = 181\n",
                    "    AND monto_pesos > 0"

---display v_s_qryTxt

   PREPARE prp_cuenta_regs_ctamov FROM v_s_qryTxt
   EXECUTE prp_cuenta_regs_ctamov INTO v_i_num_registros

   -- verifica si se encontró registros a procesar
   IF v_i_num_registros IS NULL OR v_i_num_registros = 0 THEN
      -- no continua con el proceso y regresa estatus de error
      DISPLAY " ADVERTENCIA: No se encontró información correspondiente a Liquidación AGR"

      RETURN v_b_existe_error
   END IF

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET m_v_arch_proceso = "A" || p_c_fec_hoy || ".alq"

   DISPLAY " Nombre del archivo: ",m_v_arch_proceso

   LET v_v_ruta_nomarch = p_c_ruta_envio CLIPPED || "/" || m_v_arch_proceso CLIPPED

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("") 

   -- se asignan los valores generales a todos los registros
   LET v_r_detalle.filler        = "00000" -- CHAR(5) ,-- Filler  5 1 5 ceros     
   LET v_r_detalle.punto_decimal = "."     -- CHAR(1),-- Puto Decimal 1 46  46        punto

   LET v_criterio = 0
   LET v_f_liquida = "12/31/1899"

   IF p_tpo_liq = 2 THEN
      LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

      PREPARE prp_obt_tab FROM v_s_qryTxt
      EXECUTE prp_obt_tab USING v_criterio,
                                m_d_folio,
                                v_f_liquida
                           INTO v_tabla
   ELSE
      SELECT nombre_tabla
        INTO v_tabla
        FROM cat_preliquida
       WHERE proceso_cod = 312
         AND opera_cod   = 1
   END IF

--display v_tabla

   -- se busca la fecha de liquidación
   LET v_s_qryTxt = " SELECT UNIQUE f_liquida\n",
                    "   FROM ",v_tabla,"\n",
                    "  WHERE folio_liquida = ",m_d_folio

   PREPARE prp_slc_fLiquida FROM v_s_qryTxt
   EXECUTE prp_slc_fLiquida INTO v_f_liquida

   -- se obtiene la información de saldo deudor para generar el archivo de Liquidación
   LET v_s_qryTxt = " SELECT id_cre_acreditado,  monto_pesos\n",
                    "   FROM cre_saldo_deudor\n",
                    "  WHERE id_cre_acreditado IN(\n",
                    "        SELECT id_cre_acreditado\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE folio_liquida = ", m_d_folio, "\n",
                    "           AND estado IN(",v_edo_liq,")\n",
                    "           AND tpo_originacion = ",p_si_tpo_originacion,")\n",
                    "    AND movimiento = 181\n",
                    "    AND monto_pesos > 0"

   PREPARE sid_liquidadeudor FROM v_s_qryTxt
   DECLARE cur_liquidadeudor CURSOR FOR sid_liquidadeudor

   LET  r_saldo.aivs92  = 0    
   LET  r_saldo.pesos92 = 0   
   LET  r_saldo.aivs97  = 0 
   LET  r_saldo.pesos97 = 0

   FOREACH cur_liquidadeudor INTO v_id_cre_acreditado, v_d_monto_pesos
      -- se obtiene el NSS y el numero de credito del derechohabiente
      SELECT UNIQUE a.nss, b.num_credito,b.id_derechohabiente
        INTO v_nss, v_num_credito,v_aux_id_derechohabiente
        FROM afi_derechohabiente a, cre_acreditado b
       WHERE b.id_cre_acreditado  = v_id_cre_acreditado
         AND b.id_derechohabiente = a.id_derechohabiente
         AND b.tpo_originacion    = p_si_tpo_originacion

      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- Obtienes saldo ssv 92 y 97
      CALL obtiene_ssv(v_aux_id_derechohabiente,m_d_folio) RETURNING r_saldo.*

      -- se acumula el monto obtenido
      LET v_d_sum_tot_deudor = v_d_sum_tot_deudor + v_d_monto_pesos

      -- se transforma el saldo a caracteres
      LET v_deudor_txt = v_d_monto_pesos USING "&&&&&&&&&&.&&"

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.num_credito   = v_num_credito USING "&&&&&&&&&&" -- CHAR(10),-- No Credito  10  6 16        num_credito
      LET v_r_detalle.f_proceso     = v_f_liquida USING "ddmmyyyy" -- CHAR(8) ,-- Fecha De Proceso  8 17  24  DD MM AAAA  FECHA DE ENVÍO A  HS - CARTERA    f_liquidacion
      LET v_r_detalle.nss           = v_nss -- CHAR(11),-- NSS  11  25  35        nss de afi_derechohabiente
      LET v_r_detalle.deudor        = v_deudor_txt[1,10] -- CHAR(10),-- Deudor  10  36  45        entero del sdo_deudor (var_pesos)
      LET v_r_detalle.decimales     = v_deudor_txt[12,13] -- CHAR(2),-- Decimales 2 47  48        decimales del sdo_deudor  (var_pesos)
      LET v_r_detalle.entidad       = "00" || v_r_detalle.num_credito[1,2] --v_num_credito -- CHAR(4)-- Entidad 4 49  52  00 y primeras dos posiciones del número de credito

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.filler,
                         v_r_detalle.num_credito,
                         v_r_detalle.f_proceso,
                         v_r_detalle.nss,
                         v_r_detalle.deudor,
                         v_r_detalle.punto_decimal,
                         v_r_detalle.decimales,
                         v_r_detalle.entidad

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

       --Guarda en temporal para el reporte
      INSERT INTO safre_tmp:tmp_liq_ag(
                        id_referencia,
                        id_derechohabiente,
                      --  estado       ,  
                      --  edo_procesar ,   
                      --  diagnostico  ,  
                        aivs92       ,
                        pesos92      ,
                        aivs97       ,
                        pesos97      ,
                        tpo_arh) 
               VALUES (v_id_cre_acreditado,
                        v_aux_id_derechohabiente,
                        r_saldo.aivs92,
                        r_saldo.pesos92,
                        r_saldo.aivs97,
                        r_saldo.pesos97,
                        "alq");
                        
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   LET v_nom_fin = "A_Liq_deud_ag.alq"

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = p_c_ruta_envio CLIPPED || "/" || v_nom_fin CLIPPED

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   ---LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   ---RUN v_s_comando

   ---LET v_s_Txt = "unix2dos "||" "||p_c_ruta_envio CLIPPED||" "||v_nom_fin CLIPPED
   ---RUN v_s_Txt

   DISPLAY " Nombre del archivo AG a enviar: ",v_nom_fin

   -- Salida del reporte
   --OUTPUT TO REPORT reporte_archivo_salida(m_v_arch_proceso,v_d_sum_tot_deudor, v_i_contrador_reg)

   RETURN v_b_existe_error

END FUNCTION

FUNCTION fn_gen_archivo_uso_mes_ant(p_ruta_envio)

   DEFINE p_ruta_envio      CHAR(40)
   DEFINE v_arh_cag         STRING
   DEFINE archivo           base.channel
   DEFINE v_arh_salida      STRING
   DEFINE r_detalle         RECORD
      id_cre_uso_garantia DECIMAL(9,0),
      id_derechohabiente  DECIMAL(9,0),
      nss                 CHAR(11),
      num_credito         CHAR(10),
      edo_procesar        SMALLINT,
      diagnostico         CHAR(3),
      estado              SMALLINT,
      tpo_registro        CHAR(2),
      fec_envio           CHAR(8),
      marca               CHAR(1),
      monto_recuperado    CHAR(15),
      fikey_kk            CHAR(16),
      estatus             CHAR(1),
      codigo_rechazo      CHAR(2), 
      causal_rechazo      CHAR(3),
      f_proceso           DATE,
      tpo_credito         CHAR(3)
   END RECORD
   DEFINE v_detalle         STRING
   
   LET v_arh_cag = "A",TODAY USING "yyyymmdd","_ant.cag"
   LET v_arh_salida = p_ruta_envio CLIPPED,"/",v_arh_cag

   DISPLAY " Nombre del archivo: ",v_arh_cag

   DECLARE crs_uso_mes_ant CURSOR FOR
   SELECT u.id_cre_uso_garantia,
           u.id_derechohabiente,
           u.num_credito,
           u.importe_v97,
           u.edo_procesar,
           u.diagnostico,
           u.estado,
           a.f_proceso
      FROM cre_uso_garantia u,
           cre_ctr_archivo a
     WHERE u.folio_liquida = m_d_folio
       AND u.importe_v97 > 0
       AND u.id_cre_ctr_archivo = a.id_cre_Ctr_archivo
       AND u.f_proceso < v_f_pivote

   INITIALIZE r_detalle.* TO NULL 
   LET v_detalle = NULL 
   
   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_arh_salida,"w")

   --Agisna valores
   LET r_detalle.tpo_registro = "02"
   LET r_detalle.fec_envio    = TODAY USING "YYYYMMDD"
   LET r_detalle.marca        = 4 -- marca que se envia a procesar
   LET r_detalle.fikey_kk     = ""

   --inicializa variables de saldo
   LET  r_saldo.aivs92  = 0    
   LET  r_saldo.pesos92 = 0   
   LET  r_saldo.aivs97  = 0 
   LET  r_saldo.pesos97 = 0
   
   FOREACH crs_uso_mes_ant INTO r_detalle.id_cre_uso_garantia,
                                r_detalle.id_derechohabiente,
                                r_detalle.num_credito,
                                r_detalle.monto_recuperado,
                                r_detalle.edo_procesar,
                                r_detalle.diagnostico,
                                r_detalle.estado,
                                r_detalle.f_proceso

      CALL f_obt_datos_trab(r_detalle.id_derechohabiente) RETURNING r_detalle.nss

      IF r_detalle.estado = 150 OR
         r_detalle.estado = 240 OR 
         --edo_procesar rechazos PROCESAR 
         r_detalle.edo_procesar = 90  OR
         r_detalle.edo_procesar = 100 OR 
         r_detalle.edo_procesar = 110 THEN
         
         LET r_detalle.estatus = 2
         LET r_detalle.codigo_rechazo = "02"

         #Causal de rechazo por SACI
         IF r_detalle.estado = 150 OR
            r_detalle.estado = 240 THEN
            LET r_detalle.causal_rechazo = r_detalle.estado
         ELSE 
           --Cuando es rechazado por PROCESAR
            CASE 
               WHEN (r_detalle.edo_procesar = 90) OR ( r_detalle.edo_procesar = 100)
               LET r_detalle.causal_rechazo = r_detalle.diagnostico

               WHEN (r_detalle.edo_procesar = 110)
               LET r_detalle.causal_rechazo = "0NA"
            END CASE 
         END IF
      ELSE 
         LET r_detalle.estatus        = 1
         LET r_detalle.codigo_rechazo = ""
         LET r_detalle.causal_rechazo = ""
      END IF
 
      LET r_detalle.monto_recuperado = fn_monto_recuperado(r_detalle.id_derechohabiente, r_detalle.codigo_rechazo, r_detalle.id_cre_uso_garantia, m_tpo_liq) -- se recupera el monto liquidado en pesos 

      -- Obtienes saldo ssv 92 y 97
      CALL obtiene_ssv(r_detalle.id_derechohabiente,m_d_folio) RETURNING r_saldo.*

      --Búsca el tipo de crédito
      SELECT MAX(c.tpo_credito)
        INTO r_detalle.tpo_credito
        FROM cre_acreditado c,
             cat_maq_credito q
      WHERE c.id_derechohabiente = r_detalle.id_derechohabiente
        AND c.tpo_originacion = 4
        AND c.estado = q.estado
        AND q.entidad IN (1,2)
      --  ORDER BY q.entidad,c.f_otorga DESC

      IF(r_detalle.tpo_credito IS NULL) THEN
         LET r_detalle.tpo_credito = "10"
      END IF 
         
      LET v_detalle = r_detalle.tpo_registro,
                      r_detalle.nss,
                      r_detalle.num_credito USING "&&&&&&&&&&",
                      r_detalle.fec_envio,
                      r_detalle.marca,
                      r_detalle.monto_recuperado, 
                      r_detalle.fikey_kk,
                      r_detalle.estatus,
                      r_detalle.codigo_rechazo,
                      r_detalle.causal_rechazo,
                      r_detalle.f_proceso USING "yyyymmdd",
                      r_detalle.tpo_credito USING "&&&"

      CALL archivo.writeLine(v_detalle)

      --Guarda en temporal para el reporte
      INSERT INTO safre_tmp:tmp_liq_ag(
                        id_referencia,
                        id_derechohabiente,
                        estado       ,  
                        edo_procesar ,   
                        diagnostico  ,  
                        aivs92       ,
                        pesos92      ,
                        aivs97       ,
                        pesos97      ,
                        tpo_arh) 
               VALUES (r_detalle.id_cre_uso_garantia,
                       r_detalle.id_derechohabiente,
                       r_detalle.estado,
                       r_detalle.edo_procesar,
                       r_detalle.diagnostico,
                       r_saldo.aivs92,
                       r_saldo.pesos92,
                       r_saldo.aivs97,
                       r_saldo.pesos97,
                       "cag");
   END FOREACH 

   CALL archivo.close()
   
END FUNCTION

FUNCTION fn_gen_archivo_respuesta_prcr(p_ruta_envio_rch)

   DEFINE v_f_proceso_solicitud     DATE
   DEFINE v_arh_rch_salida          STRING
   DEFINE p_ruta_envio_rch          CHAR(40)
   DEFINE archivo_rch               base.channel
   DEFINE v_detalle                 STRING
   DEFINE v_k                       INTEGER
   DEFINE v_aux_aivs92              DECIMAL(12,2)
   DEFINE v_aux_aivs97              DECIMAL(12,2)
   DEFINE v_suma_aivs               DECIMAL(12,2)
   DEFINE v_valor_fondo             DECIMAL(12,6)
   DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0)

   DEFINE r_detalle_rch             RECORD
      id_cre_uso_garantia DECIMAL(9,0),
      id_derechohabiente  DECIMAL(9,0),
      nss                 CHAR(11),
      num_credito         CHAR(10),
      edo_procesar        SMALLINT,
      estado              SMALLINT,
      tpo_registro        CHAR(2),
      fec_envio           CHAR(8),
      marca               CHAR(1),
      monto_recuperado    CHAR(15),
      fikey_kk            CHAR(16),
      estatus             CHAR(1),
      codigo_rechazo      CHAR(2), 
      causal_rechazo      CHAR(3)
   END RECORD

   LET v_arh_rechazos   = "A", TODAY USING "yyyymmdd",".rch"
   LET v_arh_rch_salida = p_ruta_envio_rch CLIPPED,"/",v_arh_rechazos

   LET archivo_rch = base.Channel.create()
   CALL archivo_rch.openFile(v_arh_rch_salida,"w")

   INITIALIZE r_detalle_rch.* TO NULL

   # RECHAZOS SACI

   --Búsca la última fecha del proceso de Recepción Uso Anualidad Garantizada
   SELECT FIRST 1 id_cre_ctr_archivo
      INTO v_id_cre_ctr_archivo
      FROM cre_ctr_archivo
     WHERE id_proceso = 302
       AND operacion  = 43
       AND nom_archivo MATCHES '*.uag'
       ORDER BY f_proceso DESC;
       --AND f_proceso = '06/20/2018';  ---NO OLVIDAR QUITAR ESTA CONDICIÓN!!

   DECLARE crs_rch_recepcion CURSOR FOR
   SELECT id_cre_uso_garantia,
          id_derechohabiente,
          num_credito,
          importe_v97,
          edo_procesar,
          diagnostico,
          estado
     FROM cre_uso_garantia
    WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo
      AND estado IN (150,240);

   LET r_detalle_rch.tpo_registro   = "02"
   LET r_detalle_rch.fec_envio      = TODAY USING "YYYYMMDD"
   LET r_detalle_rch.marca          = 4 -- marca que se envia a procesar
   LET r_detalle_rch.estatus        = 2 --Rechazo
   LET r_detalle_rch.codigo_rechazo = "02"
   LET r_detalle_rch.fikey_kk       = ""
   LET v_detalle                    = NULL

   LET v_k = 1 --contador

   FOREACH crs_rch_recepcion INTO r_detalle_rch.id_cre_uso_garantia,
                                  r_detalle_rch.id_derechohabiente,
                                  r_detalle_rch.num_credito,
                                  r_detalle_rch.monto_recuperado,
                                  r_detalle_rch.edo_procesar,
                                  r_detalle_rch.causal_rechazo,
                                  r_detalle_rch.estado

       CALL f_obt_datos_trab(r_detalle_rch.id_derechohabiente) RETURNING r_detalle_rch.nss

       LET v_detalle = r_detalle_rch.tpo_registro,
                       r_detalle_rch.nss,
                       r_detalle_rch.num_credito USING "&&&&&&&&&&",
                       r_detalle_rch.fec_envio,
                       r_detalle_rch.marca,
                       r_detalle_rch.monto_recuperado USING "&&&&&&&&&.&&&&&",
                       r_detalle_rch.fikey_kk,
                       r_detalle_rch.estatus,
                       r_detalle_rch.codigo_rechazo,
                       r_detalle_rch.causal_rechazo USING "&&&"

      --Escribe
      CALL archivo_rch.writeLine(v_detalle)

      LET v_k = v_k + 1

   END FOREACH

   # RESPUESTAS PROCESAR

   #Para obtener la respuesta de procesar primero se obtiene la f_proceso de la solicitud de saldos más reciente
   SELECT FIRST 1 f_proceso
      INTO v_f_proceso_solicitud
     FROM cre_ctr_archivo
    WHERE id_proceso = 310  --Generación archivo de solicitud saldos AGR
      AND nom_archivo MATCHES 'A*.agr'
     -- AND f_proceso = '06262018'  --Se pone fecha para simular la respuesta...No olvidar descomentar!!
     ORDER BY f_proceso DESC;

   --cálcula valor de la acción
   SELECT precio_fondo
     INTO v_valor_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_proceso_solicitud
      AND fondo = 11

   ########################################
   ###### RECHAZOS Y DEVOLUCIONES #########
   ########################################

   --Registros tipo AG
   DECLARE crs_rechazos_ag CURSOR FOR
   SELECT h.id_cre_acreditado,
          c.id_derechohabiente,
          t.nss,
          c.num_credito,
          t.aivs92,
          t.aivs97,
          h.edo_procesar,
          h.diagnostico,
          h.estado
     FROM cre_his_acreditado h,
          cre_acreditado c,
          safre_tmp:tmp_agr_solic_sdo t
    WHERE h.edo_procesar IN (90,100)
      AND h.id_cre_acreditado  = c.id_cre_acreditado
      AND c.id_cre_acreditado  = t.id_referencia
      AND t.modulo_cod = 'AG'
      AND h.f_proceso >= v_f_proceso_solicitud

    {IN (SELECT MAX(f_proceso)
          FROM cre_ctr_archivo
         WHERE id_proceso = 301
           AND operacion = 01
           AND nom_archivo MATCHES '*.43o01'
           AND f_proceso > v_f_proceso_solicitud)}

   LET v_aux_aivs92 = 0
   LET v_aux_aivs97 = 0
   LET v_suma_aivs  = 0

   --LET v_k = 1 --contador

   FOREACH crs_rechazos_ag INTO r_detalle_rch.id_cre_uso_garantia,
                                r_detalle_rch.id_derechohabiente,
                                r_detalle_rch.nss,
                                r_detalle_rch.num_credito,
                                v_aux_aivs92,
                                v_aux_aivs97,
                                r_detalle_rch.edo_procesar,
                                r_detalle_rch.causal_rechazo,
                                r_detalle_rch.estado

      LET v_suma_aivs= 0

      IF(v_aux_aivs92 IS NULL) THEN
         LET v_aux_aivs92 = 0
      END IF

      IF(v_aux_aivs97 IS NULL) THEN
         LET v_aux_aivs97 = 0
      END IF

      LET v_suma_aivs = v_aux_aivs92 + v_aux_aivs97

      --monto pesos recuperado
      LET r_detalle_rch.monto_recuperado = (v_suma_aivs * v_valor_fondo )

      LET v_detalle = r_detalle_rch.tpo_registro,
                      r_detalle_rch.nss,
                      r_detalle_rch.num_credito USING "&&&&&&&&&&",
                      r_detalle_rch.fec_envio,
                      r_detalle_rch.marca,
                      r_detalle_rch.monto_recuperado USING "&&&&&&&&&.&&&&&",
                      r_detalle_rch.fikey_kk,
                      r_detalle_rch.estatus,
                      r_detalle_rch.codigo_rechazo,
                      r_detalle_rch.causal_rechazo USING "&&&"

      --Escribe
      CALL archivo_rch.writeLine(v_detalle)

      LET v_k = v_k + 1

   END FOREACH

   #Registros tipo UA
   DECLARE crs_rechazos_ua CURSOR FOR
   SELECT id_cre_uso_garantia,
          id_derechohabiente,
          num_credito,
          importe_v97,
          edo_procesar,
          diagnostico,
          estado
     FROM cre_uso_garantia
    WHERE edo_procesar IN (90,100,110) --Rechazo, Devolución, No atendidas
      AND f_proceso >= v_f_proceso_solicitud


    {IN (SELECT MAX(f_proceso)
                             FROM cre_ctr_archivo
                            WHERE id_proceso = 301
                              AND operacion  = 01
                              AND nom_archivo MATCHES '*.43o01'
                              AND f_proceso > v_f_proceso_solicitud);}

   LET v_detalle = NULL
   
   FOREACH crs_rechazos_ua INTO r_detalle_rch.id_cre_uso_garantia,
                                r_detalle_rch.id_derechohabiente,
                                r_detalle_rch.num_credito,
                                r_detalle_rch.monto_recuperado,
                                r_detalle_rch.edo_procesar,
                                r_detalle_rch.causal_rechazo,
                                r_detalle_rch.estado

       CALL f_obt_datos_trab(r_detalle_rch.id_derechohabiente) RETURNING r_detalle_rch.nss

       LET v_detalle = r_detalle_rch.tpo_registro,
                       r_detalle_rch.nss,
                       r_detalle_rch.num_credito USING "&&&&&&&&&&",
                       r_detalle_rch.fec_envio,
                       r_detalle_rch.marca,
                       r_detalle_rch.monto_recuperado USING "&&&&&&&&&.&&&&&",
                       r_detalle_rch.fikey_kk,
                       r_detalle_rch.estatus,
                       r_detalle_rch.codigo_rechazo,
                       r_detalle_rch.causal_rechazo USING "&&&"

      --Escribe
      CALL archivo_rch.writeLine(v_detalle)

      LET v_k = v_k + 1

   END FOREACH

   ########################################
   ############# NO ATENDIDAS #############
   ########################################

   --Anualidades Garantizadas tipo AG
   DECLARE crs_no_atendidas_ag CURSOR FOR
   SELECT h.id_cre_acreditado,
          c.id_derechohabiente,
          t.nss,
          c.num_credito,
          t.aivs92,
          t.aivs97,
          h.edo_procesar,
          h.diagnostico,
          h.estado
     FROM cre_his_acreditado h,
          cre_acreditado c,
          safre_tmp:tmp_agr_solic_sdo2 t
    WHERE h.edo_procesar = 110 --No atendidas
      AND h.id_cre_acreditado  = c.id_cre_acreditado
      AND c.id_cre_acreditado  = t.id_referencia
      AND h.tpo_transferencia  = '43'
      AND h.f_proceso >= v_f_proceso_solicitud

   LET v_aux_aivs92 = 0
   LET v_aux_aivs97 = 0
   LET v_suma_aivs  = 0
   LET v_detalle    = NULL

   FOREACH crs_no_atendidas_ag INTO r_detalle_rch.id_cre_uso_garantia,
                                    r_detalle_rch.id_derechohabiente,
                                    r_detalle_rch.nss,
                                    r_detalle_rch.num_credito,
                                    v_aux_aivs92,
                                    v_aux_aivs97,
                                    r_detalle_rch.edo_procesar,
                                    r_detalle_rch.causal_rechazo,
                                    r_detalle_rch.estado

      LET v_suma_aivs= 0

      IF(v_aux_aivs92 IS NULL) THEN
         LET v_aux_aivs92 = 0
      END IF

      IF(v_aux_aivs97 IS NULL) THEN
         LET v_aux_aivs97 = 0
      END IF

      LET v_suma_aivs = v_aux_aivs92 + v_aux_aivs97

      --monto pesos recuperado
      LET r_detalle_rch.monto_recuperado = (v_suma_aivs * v_valor_fondo )

      LET v_detalle = r_detalle_rch.tpo_registro,
                      r_detalle_rch.nss,
                      r_detalle_rch.num_credito USING "&&&&&&&&&&",
                      r_detalle_rch.fec_envio,
                      r_detalle_rch.marca,
                      r_detalle_rch.monto_recuperado USING "&&&&&&&&&.&&&&&",
                      r_detalle_rch.fikey_kk,
                      r_detalle_rch.estatus,
                      r_detalle_rch.codigo_rechazo,
                      r_detalle_rch.causal_rechazo USING "&&&"

      --Escribe
      CALL archivo_rch.writeLine(v_detalle)

      LET v_k = v_k + 1

   END FOREACH

   CALL archivo_rch.close()

END FUNCTION

#Objetivo : Calcula la información para el PDF
FUNCTION informacion_reporte()

   DEFINE k           INTEGER --contador
   DEFINE r_bloque1   RECORD
       tpo_arh   CHAR(3),
       aivs92    DECIMAL(16,6),  
       pesos92   DECIMAL(12,2),
       aivs97    DECIMAL(16,6),
       pesos97   DECIMAL(12,2),
       total     INTEGER 
   END RECORD 
   DEFINE r_detalle_lqa   RECORD
       estado    SMALLINT,
       aivs92    DECIMAL(16,6),  
       pesos92   DECIMAL(12,2),
       aivs97    DECIMAL(16,6),
       pesos97   DECIMAL(12,2),
       total     INTEGER 
   END RECORD 
   DEFINE v_aux_porcentaje  DECIMAL(6,2)
   DEFINE v_total_glo       INTEGER 
   DEFINE v_reporte_bin     STRING 
   DEFINE v_ruta_rpt        STRING 
   DEFINE v_manejador_rpt   om.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_entidad         CHAR(3)
   DEFINE v_estado_procesar SMALLINT 
   
   --Inicializa arreglo para el rpt
   --archivo alq
   LET arr_archivos[1].nombre_archivo = "A",TODAY USING "yyyymmdd",".alq"
   LET arr_archivos[1].total  = 0
   LET arr_archivos[1].aivs92 = 0
   LET arr_archivos[1].aivs97 = 0
   LET arr_archivos[1].pesos  = 0 
   --archivo lqa
   LET arr_archivos[2].nombre_archivo = m_v_arch_proceso CLIPPED
   LET arr_archivos[2].total  = 0
   LET arr_archivos[2].aivs92 = 0
   LET arr_archivos[2].aivs97 = 0
   LET arr_archivos[2].pesos  = 0
   --archivo cag
   LET arr_archivos[3].nombre_archivo = "A",TODAY USING "yyyymmdd","_ant.cag"
   LET arr_archivos[3].total  = 0
   LET arr_archivos[3].aivs92 = 0
   LET arr_archivos[3].aivs97 = 0
   LET arr_archivos[3].pesos  = 0
   
   DECLARE crs_bloque1 CURSOR FOR 
   SELECT tpo_arh,
           SUM (aivs92),
           SUM (pesos92),
           SUM (aivs97),
           SUM (pesos97),
           COUNT(*)
     FROM safre_tmp:tmp_liq_ag
     GROUP BY 1;

   INITIALIZE r_bloque1.* TO NULL
   
   LET k = 1

   FOREACH crs_bloque1 INTO r_bloque1.tpo_arh,
                             r_bloque1.aivs92,
                             r_bloque1.pesos92,
                             r_bloque1.aivs97,
                             r_bloque1.pesos97,
                             r_bloque1.total

      IF(r_bloque1.aivs92 IS NULL) THEN 
         LET r_bloque1.aivs92 = 0
      END IF 

      IF( r_bloque1.pesos92 IS NULL) THEN 
         LET  r_bloque1.pesos92 = 0
      END IF
      
      IF(r_bloque1.aivs97 IS NULL) THEN 
         LET r_bloque1.aivs97 = 0
      END IF
      
       IF( r_bloque1.pesos97 IS NULL) THEN 
         LET  r_bloque1.pesos97 = 0
      END IF 
      
      --identifica por tipo de archivo
      CASE 
         WHEN r_bloque1.tpo_arh = "alq"
            LET arr_archivos[1].total  = r_bloque1.total
            LET arr_archivos[1].aivs92 = r_bloque1.aivs92
            LET arr_archivos[1].aivs97 = r_bloque1.aivs97
            LET arr_archivos[1].pesos  = r_bloque1.pesos92 +  r_bloque1.pesos97
   
         WHEN r_bloque1.tpo_arh = "lqa"
            LET arr_archivos[2].total  = r_bloque1.total
            LET arr_archivos[2].aivs92 = r_bloque1.aivs92
            LET arr_archivos[2].aivs97 = r_bloque1.aivs97
            LET arr_archivos[2].pesos  = r_bloque1.pesos92 +  r_bloque1.pesos97
            --DISPLAY "pesos lqa ",arr_archivos[2].pesos
            
         WHEN r_bloque1.tpo_arh = "cag"
            LET arr_archivos[3].total  = r_bloque1.total
            LET arr_archivos[3].aivs92 = r_bloque1.aivs92
            LET arr_archivos[3].aivs97 = r_bloque1.aivs97
            LET arr_archivos[3].pesos  = r_bloque1.pesos92 +  r_bloque1.pesos97
            
      END CASE 
      
      LET k = k + 1
      
   END FOREACH

   --Inicializa arreglo para aceptados y rechazados del archivo .lqa
   LET arr_inf_lqa[1].detalle    = "Aceptados"
   LET arr_inf_lqa[1].total      = 0
   LET arr_inf_lqa[1].aivs92     = 0
   LET arr_inf_lqa[1].aivs97     = 0
   LET arr_inf_lqa[1].pesos      = 0
   LET arr_inf_lqa[2].detalle    = "Rechazados"
   LET arr_inf_lqa[2].total      = 0
   LET arr_inf_lqa[2].aivs92     = 0
   LET arr_inf_lqa[2].aivs97     = 0
   LET arr_inf_lqa[2].pesos      = 0
   
   DECLARE crs_detalle_lqa CURSOR FOR 
   SELECT estado,
          SUM(aivs92),
          SUM(pesos92),
          SUM(aivs97),
          SUM(pesos97),
          COUNT(*)
     FROM safre_tmp:tmp_liq_ag
     GROUP BY 1;

   LET k = 1
   
   INITIALIZE r_detalle_lqa.* TO NULL
   LET v_total_glo = 0 

   
   FOREACH crs_detalle_lqa INTO r_detalle_lqa.estado,
                                r_detalle_lqa.aivs92,
                                r_detalle_lqa.pesos92,
                                r_detalle_lqa.aivs97,
                                r_detalle_lqa.pesos97,
                                r_detalle_lqa.total

      LET v_total_glo = v_total_glo +   r_detalle_lqa.total

      IF( r_detalle_lqa.aivs92 IS NULL) THEN 
         LET  r_detalle_lqa.aivs92 = 0
      END IF 

      IF(  r_detalle_lqa.pesos92 IS NULL) THEN 
         LET   r_detalle_lqa.pesos92 = 0
      END IF
      
      IF(r_detalle_lqa.aivs97 IS NULL) THEN 
         LET r_detalle_lqa.aivs97 = 0
      END IF
      
       IF( r_detalle_lqa.pesos97 IS NULL) THEN 
         LET  r_detalle_lqa.pesos97 = 0
      END IF 
      
      
      --aceptados
      IF(r_detalle_lqa.estado <> 150) AND 
        (r_detalle_lqa.estado <> 240 )THEN
         LET arr_inf_lqa[1].total  = arr_inf_lqa[1].total  + r_detalle_lqa.total
         LET arr_inf_lqa[1].aivs92 = arr_inf_lqa[1].aivs92 + r_detalle_lqa.aivs92
         LET arr_inf_lqa[1].aivs97 = arr_inf_lqa[1].aivs97 + r_detalle_lqa.aivs97
         LET arr_inf_lqa[1].pesos  = arr_inf_lqa[1].pesos  +  r_detalle_lqa.pesos92 + r_detalle_lqa.pesos97
      ELSE 
         --rechazados
         LET arr_inf_lqa[2].total  = arr_inf_lqa[2].total  + r_detalle_lqa.total
         LET arr_inf_lqa[2].aivs92 = arr_inf_lqa[2].aivs92 + r_detalle_lqa.aivs92
         LET arr_inf_lqa[2].aivs97 = arr_inf_lqa[2].aivs97 + r_detalle_lqa.aivs97
         LET arr_inf_lqa[2].pesos  = arr_inf_lqa[2].pesos  + r_detalle_lqa.pesos92 + r_detalle_lqa.pesos97
      END IF  

      LET k = k + 1
      
   END FOREACH 

   -- Calcula porcentaje aceptados
   LET v_aux_porcentaje = 0
   LET v_aux_porcentaje = (arr_inf_lqa[1].total / v_total_glo) * 100
   LET arr_inf_lqa[1].porcentaje = v_aux_porcentaje CLIPPED,"%"
   -- Calcula porcentaje rechazados
   LET v_aux_porcentaje = (arr_inf_lqa[2].total / v_total_glo) * 100
   LET arr_inf_lqa[2].porcentaje = v_aux_porcentaje CLIPPED,"%"

   # DETALLE RECHAZOS
   CALL arr_rch_saci.clear()
   CALL arr_rch_procesar.clear()

   --rechazos saci
   DECLARE crs_rch_saci CURSOR FOR 
   SELECT estado,
          SUM(aivs92),       
          SUM(pesos92),         
          SUM(aivs97),          
          SUM(pesos97),
          COUNT(*)
    FROM safre_tmp:tmp_liq_ag
   WHERE estado IN (150,240)
     AND tpo_arh = "lqa"
    GROUP BY 1;

   LET k = 1

   FOREACH crs_rch_saci INTO arr_rch_saci[k].estado,
                              arr_rch_saci[k].aivs92,
                              arr_rch_saci[k].pesos92,
                              arr_rch_saci[k].aivs97,
                              arr_rch_saci[k].pesos97,
                              arr_rch_saci[k].total

      --suma total pesos
      LET arr_rch_saci[k].total_pesos = arr_rch_saci[k].pesos92 + arr_rch_saci[k].pesos97

      --Descripción del rechazo
      SELECT desc_estado
        INTO arr_rch_saci[k].causal_desc
        FROM cat_rch_acreditado
       WHERE estado = arr_rch_saci[k].estado

      IF(arr_rch_saci[k].causal_desc IS NULL) THEN
         LET arr_rch_saci[k].causal_desc = "Descripción no encontrada"
      END IF 
      
      LET k =  k + 1
      
   END FOREACH 

   --Elimina fila en balnco del arreglo
   IF(arr_rch_saci[arr_rch_saci.getLength()].estado IS NULL) THEN
      CALL arr_rch_saci.deleteElement(arr_rch_saci.getLength())
   END IF

   --rechazos procesar
   DECLARE crs_rch_procesar CURSOR FOR 
   SELECT edo_procesar,
          diagnostico,
          SUM(aivs92),       
          SUM(pesos92),         
          SUM(aivs97),          
          SUM(pesos97),
          COUNT(*)
    FROM safre_tmp:tmp_liq_ag
   WHERE edo_procesar IN (90,100,110) 
     AND estado NOT IN (150,240)
     AND diagnostico IS NOT NULL 
     AND tpo_arh = "lqa"
    GROUP BY 1,2;

   LET k = 1

   FOREACH crs_rch_procesar INTO v_estado_procesar,
                                 arr_rch_procesar[k].diagnostico,
                                 arr_rch_procesar[k].aivs92,
                                 arr_rch_procesar[k].pesos92,
                                 arr_rch_procesar[k].aivs97,
                                 arr_rch_procesar[k].pesos97,
                                 arr_rch_procesar[k].total
                                  
      --suma total pesos
      LET arr_rch_procesar[k].total_pesos = arr_rch_procesar[k].pesos92 + arr_rch_procesar[k].pesos97

      IF(v_estado_procesar = 90) THEN
         LET v_entidad = 'PRC'
      ELSE 
         LET v_entidad = 'AFO'
      END IF 
      
      --Busca descripción rechazo
      SELECT desc_causal
        INTO arr_rch_procesar[k].causal_desc
        FROM cat_rechazo_causal
       WHERE causal  = arr_rch_procesar[k].diagnostico
         AND entidad = v_entidad

      IF(arr_rch_procesar[k].causal_desc IS NULL) THEN
         LET arr_rch_procesar[k].causal_desc = "Descripción no encontrada" 
      END IF 

      LET k = k + 1
      
   END FOREACH 

   --Elimina fila en balnco del arreglo
   IF(arr_rch_procesar[arr_rch_procesar.getLength()].diagnostico IS NULL) THEN
      CALL arr_rch_procesar.deleteElement(arr_rch_procesar.getLength())
   END IF

   #################################################
   #   CONFIGURACION PARA SALIDA DEL REPORTE PDF   #
   #################################################

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRS011.4rp"
   LET v_ruta_rpt    = v_c_ruta_listado CLIPPED,"/",
                       m_v_usuario CLIPPED,"-",v_c_programa_cod CLIPPED,"-",
                       m_d_pid USING "&&&&&","-",
                       m_i_proceso_cod USING "&&&&&","-",
                       m_i_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN

         START REPORT reporte_pdf TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT reporte_pdf()

         FINISH REPORT reporte_pdf

      END IF
   ELSE
       DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
       EXIT PROGRAM 
   END IF

     
END FUNCTION

REPORT reporte_pdf()

   DEFINE v_fecha_present           LIKE dis_sum_avance_pago.f_presentacion
   DEFINE f                         INTEGER 

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_present = TODAY

      PRINTX m_d_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX m_v_usuario

   ON EVERY ROW
      --Imprime bloque 1 archivos
      FOR f = 1 TO arr_archivos.getLength()
         PRINTX arr_archivos[f].nombre_archivo
         PRINTX arr_archivos[f].total
         PRINTX arr_archivos[f].aivs92
         PRINTX arr_archivos[f].aivs97
         PRINTX arr_archivos[f].pesos
      END FOR 

      --Detalle archivo .lqa
      FOR f = 1 TO arr_inf_lqa.getLength()
         PRINTX arr_inf_lqa[f].detalle
         PRINTX arr_inf_lqa[f].total
         PRINTX arr_inf_lqa[f].aivs92
         PRINTX arr_inf_lqa[f].aivs97
         PRINTX arr_inf_lqa[f].pesos
         PRINTX arr_inf_lqa[f].porcentaje
      END FOR 

      --Detalle causal rechazo saci
      FOR f = 1 TO arr_rch_saci.getLength()
         PRINTX arr_rch_saci[f].total
         PRINTX arr_rch_saci[f].aivs92
         PRINTX arr_rch_saci[f].aivs97
         PRINTX arr_rch_saci[f].total_pesos
         PRINTX arr_rch_saci[f].estado
         PRINTX arr_rch_saci[f].causal_desc
      END FOR 

      --Detalle causal rechazo procesar
      FOR f = 1 TO arr_rch_procesar.getLength()
         PRINTX arr_rch_procesar[f].total
         PRINTX arr_rch_procesar[f].aivs92
         PRINTX arr_rch_procesar[f].aivs97
         PRINTX arr_rch_procesar[f].total_pesos
         PRINTX arr_rch_procesar[f].diagnostico
         PRINTX arr_rch_procesar[f].causal_desc
      END FOR 

END REPORT


#Objetivo: Función que consulta los datos de la tabla afi derechohabiente
FUNCTION f_obt_datos_trab(p_id_derechohabiente)

   DEFINE p_id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente -- identificador del derechohabiente 
   DEFINE v_c_nss                   LIKE afi_derechohabiente.nss -- registro de afi derechohabiente
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene la información del trabajador
   LET v_s_qryTxt = " SELECT nss\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_id_derechohabiente

   PREPARE prp_afi_derechohabiente FROM v_s_qryTxt
   EXECUTE prp_afi_derechohabiente INTO v_c_nss

   RETURN v_c_nss

END FUNCTION

#Objetivo: Función que obtiene el número de crédito del trabajador en la tabla

FUNCTION f_obt_num_credito(p_id_derechohabiente)

   DEFINE p_id_derechohabiente      LIKE cre_acreditado.id_derechohabiente
   DEFINE v_d_num_credito           LIKE cre_acreditado.num_credito -- registro de afi derechohabiente
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene el numero de credito para el id derechohabiente
   LET v_s_qryTxt = " SELECT FIRST 1 num_credito\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_derechohabiente = ",p_id_derechohabiente,"\n",
                    "  ORDER BY f_otorga DESC"

   PREPARE prp_slcUniq_numCred FROM v_s_qryTxt
   EXECUTE prp_slcUniq_numCred INTO v_d_num_credito

   RETURN v_d_num_credito

END FUNCTION

#Objetivo: FunciÓn que obtiene el monto recuperado (liquidado)
FUNCTION fn_monto_recuperado(p_d_id_derechohabiente, p_c_codigo_rechazo,p_d_id_cre_uso_garantia, p_tpo_liq)

   DEFINE p_d_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente -- identificador del derechohabiente
   DEFINE p_d_id_cre_uso_garantia LIKE cre_uso_garantia.id_cre_uso_garantia -- identificador del registro en el histórico
   DEFINE p_c_codigo_rechazo      CHAR(2) -- código de rechazo
   DEFINE v_d_monto_pesos         LIKE cta_movimiento.monto_pesos -- monto en pesos recuperado
   DEFINE v_c_monto_pesos         CHAR(15) -- monto recuperado con formato de 15 posiciones
   DEFINE v_s_qryTxt              STRING
   DEFINE v_f_liquida             DATE
   DEFINE p_tpo_liq               SMALLINT

   -- se inicializan variables
   LET v_d_monto_pesos = 0 -- monto recuperado

   LET v_criterio = 0
   LET v_f_liquida = "12/31/1899"

   -- se verifica el codigo de rechazo
   IF p_c_codigo_rechazo IS NULL THEN
      IF p_tpo_liq = 2 THEN
         LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

         PREPARE prp_obt_rec FROM v_s_qryTxt
         EXECUTE prp_obt_rec USING v_criterio,
                                   m_d_folio,
                                   v_f_liquida
                              INTO v_tabla
      ELSE
         SELECT nombre_tabla
           INTO v_tabla
           FROM cat_preliquida
          WHERE proceso_cod = 312
            AND opera_cod   = 1
      END IF

      -- se consulta el monto de cta movimiento
      LET v_s_qryTxt = " SELECT SUM(monto_pesos)\n",
                       "   FROM ",v_tabla,"\n",
                       "  WHERE folio_liquida = ",m_d_folio,"\n",
                       "    AND id_referencia = ",p_d_id_cre_uso_garantia,"\n",
                       "    AND id_derechohabiente = ",p_d_id_derechohabiente,"\n",
                       "    AND movimiento NOT IN (492,82,152,162)"

      PREPARE prp_mto_cta_mov FROM v_s_qryTxt
      EXECUTE prp_mto_cta_mov INTO v_d_monto_pesos
   ELSE
      -- se consulta el monto de la uso de garantia
      LET v_s_qryTxt = " SELECT SUM(importe_v97)\n",
                       "   FROM cre_uso_garantia\n",
                       "  WHERE folio_liquida = ",m_d_folio,"\n",
                       "    AND id_cre_uso_garantia = ",p_d_id_cre_uso_garantia,"\n",
                       "    AND id_derechohabiente = ",p_d_id_derechohabiente

      PREPARE prp_mto_cre_uso FROM v_s_qryTxt
      EXECUTE prp_mto_cre_uso INTO v_d_monto_pesos
   END IF

   -- se valida el monto obtenido
   IF v_d_monto_pesos IS NULL THEN
      LET v_d_monto_pesos = 0
   END IF

   -- se asigna el monto con formato "&&&&&&&&&&&&&&&" 15 posiciones
   LET v_c_monto_pesos = (v_d_monto_pesos * 100) USING "&&&&&&&&&&&&&&&"

   RETURN v_c_monto_pesos

END FUNCTION

#Objetivo: Obtiene saldo subcuenta 92 y 97
FUNCTION obtiene_ssv(p_id_derechohabiente,p_folio)

   DEFINE p_id_referencia          DECIMAL(9,0)
   DEFINE p_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente
   DEFINE p_folio                  LIKE glo_ctr_archivo.folio 
   DEFINE return_saldo     RECORD
      aivs92     DECIMAL(16,6),  
      pesos92    DECIMAL(12,2),
      aivs97     DECIMAL(16,6),
      pesos97    DECIMAL(12,2)
   END RECORD 

   LET return_saldo.aivs92  = 0
   LET return_saldo.pesos92 = 0
   LET return_saldo.aivs97  = 0
   LET return_saldo.pesos97 = 0
     
   --Query SALDO 92
   SELECT SUM(monto_acciones), -- AIVS 92
           SUM(monto_pesos)     --PESOS 92
      INTO return_saldo.aivs92,
           return_saldo.pesos92
      FROM cta_movimiento
     WHERE folio_liquida      = p_folio
       AND id_derechohabiente = p_id_derechohabiente
       AND subcuenta IN (8,42);

   --Query SALDO 97
   SELECT SUM(monto_acciones), -- AIVS 97
           SUM(monto_pesos)     --PESOS 97
      INTO return_saldo.aivs97,
           return_saldo.pesos97
      FROM cta_movimiento
     WHERE folio_liquida      = p_folio
       AND id_derechohabiente = p_id_derechohabiente
       AND subcuenta IN (4,44);

   #Retorna valores
   RETURN return_saldo.*
        
END FUNCTION 


#OBJETIVO: Genera el reporte de Rechazos
{REPORT reporte_archivo_salida(p_v_arch_proceso, p_total_deudor, p_count_reg)

   DEFINE p_v_arch_proceso          CHAR(100)
   DEFINE p_total_deudor            DECIMAL(16,2)
   DEFINE p_count_reg               INTEGER
   DEFINE v_fecha_present           LIKE dis_sum_avance_pago.f_presentacion
   DEFINE v_sum_total_deudor        DECIMAL(18,2)
   DEFINE v_sum_count_reg           INTEGER

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_present = TODAY

      PRINTX m_d_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX m_v_usuario

   ON EVERY ROW
      PRINTX p_v_arch_proceso
      PRINTX p_total_deudor
      PRINTX p_count_reg

   ON LAST ROW
      LET v_sum_total_deudor = SUM(p_total_deudor)
      LET v_sum_count_reg = SUM(p_count_reg)

      PRINTX v_sum_total_deudor
      PRINTX v_sum_count_reg

END REPORT}


FUNCTION crea_temporal()

   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_liq_ag;

   WHENEVER ERROR STOP
      CREATE TABLE tmp_liq_ag(
                        id_referencia      DECIMAL(9,0),
                        id_derechohabiente DECIMAL(9,0),
                        estado             SMALLINT,
                        edo_procesar       SMALLINT,
                        diagnostico        CHAR(3),
                        aivs92             DECIMAL(16,6),  
                        pesos92            DECIMAL(12,2),
                        aivs97             DECIMAL(16,6),
                        pesos97            DECIMAL(12,2),
                        tpo_arh            CHAR(3))
 
   DATABASE safre_viv
   
END FUNCTION 
