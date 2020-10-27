--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación: 26/04/2012
--===============================================================

####################################################################
#Módulo            =>GRT                                           #
#Programa          =>GRTS05                                        #
#Objetivo          =>Programa que genera el archivo de salida de   #
#                    Rechazos para el módulo de Solicitud de       #
#                    Saldos en Garantía                            #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>27 Septiembre 2012                            #
####################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl"

   DEFINE m_v_usuario              LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE m_d_pid                  LIKE bat_ctr_proceso.pid -- pid
   DEFINE m_i_proceso_cod          LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE m_i_opera_cod            LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE m_d_folio                LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE m_v_arch_salida          VARCHAR(100) -- nombre del archivo de salida
   DEFINE v_s_Txt                  CHAR(100)
   DEFINE v_nom_fin                CHAR(20)
   DEFINE v_ejecuta_sh             STRING

MAIN

   DEFINE v_v_arch_entrada         VARCHAR(100) -- nombre del archivo de entrada
   DEFINE v_id_cre_ctr_archivo     LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de archivo
   DEFINE v_c_ruta_envio           LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_c_programa_cod         LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt               STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_c_ruta_listado         LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_manejador_rpt          OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_archivo_nom            STRING
   DEFINE r_b_valida               SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET m_v_usuario      = ARG_VAL(1)
   LET m_d_pid          = ARG_VAL(2)
   LET m_i_proceso_cod  = ARG_VAL(3)
   LET m_i_opera_cod    = ARG_VAL(4)
   LET m_d_folio        = ARG_VAL(5)
   LET m_v_arch_salida  = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(m_v_usuario CLIPPED|| ".GRTS05.log")

   DISPLAY "=INICIA GRTS05="
   DISPLAY " USUARIO       : ",m_v_usuario
   DISPLAY " PID           : ",m_d_pid
   DISPLAY " FOLIO         : ",m_d_folio USING "#########&"

   -- se obtienen la ruta envio del módulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'grt'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado

   -- se obtiene la ruta envio del módulo
   LET v_s_qryTxt = " SELECT id_cre_ctr_archivo, nom_archivo\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE folio_archivo = ",m_d_folio,"\n",
                    "    AND id_proceso = ",g_id_proceso_grt_uso,"\n",
                    "    AND operacion = 18\n", -- 18 - Uso de Garantía
                    "    AND estado = 20"

   PREPARE prp_id_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_id_cre_ctr_arch INTO v_id_cre_ctr_archivo, v_v_arch_entrada

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(m_i_proceso_cod , m_i_opera_cod)

   -- se intenta cargar la plantilla del reporte
   IF fgl_report_loadCurrentSettings("GRTS051.4rp") THEN
      -- se crea el nombre del reporte
      LET v_archivo_nom = m_v_usuario CLIPPED, "-",
                          v_c_programa_cod CLIPPED, "-",
                          m_d_pid USING "&&&&&", "-",
                          m_i_proceso_cod USING "&&&&&", "-",
                          m_i_opera_cod USING "&&&&&", ".pdf"

      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listado CLIPPED||"/"||v_archivo_nom)

      -- se indica que no será necesario la vista previa
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración al manejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
      LET r_b_valida = fn_error_opera(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- empieza el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- se invoca la función que genera el archivo de liquidación para los tipo de transferencia 18
   CALL fn_gen_arch_rch_18(v_v_arch_entrada, v_id_cre_ctr_archivo, v_c_ruta_envio)

   -- se invoca la función que genera el archivo de liquidación para los tipo de transferencia 48
   CALL fn_gen_arch_rch_48(v_v_arch_entrada, v_id_cre_ctr_archivo, v_c_ruta_envio)

   -- finaliza el reporte
   FINISH REPORT reporte_archivo_salida

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   DISPLAY "=FIN="

END MAIN

# Objetivo: Función que genera el archivo de Rechazos para el tipo de transferencia 18
FUNCTION fn_gen_arch_rch_18(p_v_arch_entrada, p_id_cre_ctr_archivo, p_c_ruta_envio)

   DEFINE p_v_arch_entrada      VARCHAR(100) -- nombre del archivo de entrada
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de archivo
   DEFINE p_c_ruta_envio        LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_ch_arch_solTransf   BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_v_ruta_nomarch      VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp   VARCHAR(100) -- ruta y nombre del archivo de salida

   DEFINE v_r_uso_garant RECORD
      id_derechohabiente        LIKE afi_derechohabiente.id_derechohabiente, -- ID del derechohabiente
      periodo_pago              LIKE cre_uso_garantia.periodo_pago, -- periodo de pago
      diagnostico               LIKE cre_uso_garantia.diagnostico, -- diagnostico del rechazo
      pesos                     DECIMAL(20,2), -- monto en pesos
      nss                       LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
      nombre_af                 LIKE afi_derechohabiente.nombre_af, -- nombre afore
      ap_paterno_af             LIKE afi_derechohabiente.ap_paterno_af, -- apellido paterno afore
      ap_materno_af             LIKE afi_derechohabiente.ap_paterno_af, -- apellido materno afore
      f_liquida                 LIKE cre_uso_garantia.f_movimiento -- fecha de movimiento
   END RECORD

   DEFINE v_r_detalle RECORD
      v_nss                     CHAR(11), -- número de seguridad social(1-11)
      v_nombre                  CHAR(30), -- nombre (12-41)
      v_periodo                 CHAR(6), -- periodo (42-47) formato aaaabb
      v_aportacion              CHAR(10), -- aportación (48-57)
      v_folio_sua               CHAR(6), -- folio sua (58-63)
      v_fec_aplic_safre         CHAR(8), -- fecha aplicaciones safre (64-71) formato aaaammdd
      v_fec_entrega             CHAR(8), -- fecha de entrega (72-79) formato aaaammdd
      v_interface               CHAR(2), -- inteface (80-81)
      v_rechazo                 CHAR(3) -- código de rechazo
   END RECORD

   DEFINE v_c_tpo_transf        CHAR(2) -- tipo de transferencia
   DEFINE v_s_registro          STRING -- registro a insertar
   DEFINE v_c_extension         LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_d_total_pesos       DECIMAL(20,2) -- monto total en pesos
   DEFINE v_i_contrador_reg     INTEGER -- contrador de registros
   DEFINE v_s_qryTxt            STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_s_comando           STRING -- contiene al comando a correr

   -- se inicializan variables
   LET v_i_contrador_reg = 0
   LET v_d_total_pesos = 0
   LET v_c_tpo_transf = "18"

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_nomarch = p_c_ruta_envio CLIPPED || "/" || m_v_arch_salida CLIPPED
   DISPLAY " ARCHIVO A GENERAR (18): ", m_v_arch_salida

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT cre.id_derechohabiente, cre.periodo_pago, cre.diagnostico, SUM(cre.importe_v97),\n",
                    "        afi.nss, afi.nombre_af, afi.ap_paterno_af, afi.ap_materno_af,\n",
                    "        cre.f_movimiento\n",
                    "   FROM cre_uso_garantia cre, afi_derechohabiente afi\n",
                    "  WHERE cre.estado IN (240, 150)\n",
                    "    AND cre.id_cre_ctr_archivo = ",p_id_cre_ctr_archivo,"\n",
                    "    AND cre.id_derechohabiente = afi.id_derechohabiente\n",
                    "    AND cre.tpo_transferencia = '18'\n",
                    "  GROUP BY 1, 2, 3, 5, 6, 7, 8, 9"

   PREPARE prp_rch_uso_garant18 FROM v_s_qryTxt
   DECLARE cur_rch_uso_garant18 CURSOR FOR prp_rch_uso_garant18

   FOREACH cur_rch_uso_garant18 INTO v_r_uso_garant.*
      -- se valida el importe obtenido
      IF v_r_uso_garant.pesos IS NULL THEN
         LET v_r_uso_garant.pesos = 0
      END IF

      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.v_nss             = v_r_uso_garant.nss
      LET v_r_detalle.v_nombre          = v_r_uso_garant.ap_paterno_af CLIPPED, " ", v_r_uso_garant.ap_materno_af CLIPPED, " ", v_r_uso_garant.nombre_af CLIPPED
      LET v_r_detalle.v_periodo         = v_r_uso_garant.periodo_pago USING "&&&&&&"
      LET v_r_detalle.v_aportacion      = ( v_r_uso_garant.pesos * 100) USING "&&&&&&&&&&"
      LET v_r_detalle.v_folio_sua       = "" -- 6 espacios en blanco 
      LET v_r_detalle.v_fec_aplic_safre = v_r_uso_garant.f_liquida USING "yyyymmdd"
      LET v_r_detalle.v_fec_entrega     = TODAY USING "yyyymmdd"
      LET v_r_detalle.v_interface       = "UG"
      LET v_r_detalle.v_rechazo         = v_r_uso_garant.diagnostico USING "&&&"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.v_nss,
                         v_r_detalle.v_nombre,
                         v_r_detalle.v_periodo,
                         v_r_detalle.v_aportacion,
                         v_r_detalle.v_folio_sua,
                         v_r_detalle.v_fec_aplic_safre,
                         v_r_detalle.v_fec_entrega,
                         v_r_detalle.v_interface,
                         v_r_detalle.v_rechazo

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se acumula el monto del crédito
      LET v_d_total_pesos = v_d_total_pesos  + v_r_uso_garant.pesos
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(m_i_proceso_cod, m_i_opera_cod)

   LET v_nom_fin = "A_rch_43bis." || v_c_extension

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = p_c_ruta_envio CLIPPED || "/" || v_nom_fin CLIPPED

    -- para tpo transf 48: A_rch_48ua.rchua

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   --LET v_s_Txt = "unix2dos "||" "||p_c_ruta_envio CLIPPED||" "||v_nom_fin CLIPPED
   --RUN v_s_Txt

   DISPLAY " Nombre del archivo rechazos de uso de garantía de 43 bis a enviar: ",v_nom_fin
   DISPLAY ""
   DISPLAY " Ejecutando envío interfaz ADS"

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/A_rch_43bis.sh"
   RUN v_ejecuta_sh

   DISPLAY ""

   -- salida de reporte
   OUTPUT TO REPORT reporte_archivo_salida(p_v_arch_entrada, m_v_arch_salida, v_c_tpo_transf, v_d_total_pesos, v_i_contrador_reg)

END FUNCTION

# Objetivo: Función que genera el archivo de Rechazos para el tipo de transferencia 48
FUNCTION fn_gen_arch_rch_48(p_v_arch_entrada, p_id_cre_ctr_archivo, p_c_ruta_envio)

   DEFINE p_v_arch_entrada      VARCHAR(100) -- nombre del archivo de entrada
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de archivo
   DEFINE p_c_ruta_envio        LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_ch_arch_solTransf   BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_v_ruta_nomarch      VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp   VARCHAR(100) -- ruta y nombre del archivo de salida

   DEFINE v_r_uso_garant RECORD
      id_derechohabiente        LIKE afi_derechohabiente.id_derechohabiente, -- ID del derechohabiente
      periodo_pago              LIKE cre_uso_garantia.periodo_pago, -- periodo de pago
      diagnostico               LIKE cre_uso_garantia.diagnostico, -- diagnostico del rechazo
      pesos                     DECIMAL(20,2), -- monto en pesos
      nss                       LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
      nombre_af                 LIKE afi_derechohabiente.nombre_af, -- nombre afore
      ap_paterno_af             LIKE afi_derechohabiente.ap_paterno_af, -- apellido paterno afore
      ap_materno_af             LIKE afi_derechohabiente.ap_paterno_af, -- apellido materno afore
      f_liquida                 LIKE cre_uso_garantia.f_movimiento -- fecha de movimiento
   END RECORD

   DEFINE v_r_detalle RECORD
      v_nss                     CHAR(11), -- número de seguridad social(1-11)
      v_nombre                  CHAR(30), -- nombre (12-41)
      v_periodo                 CHAR(6), -- periodo (42-47) formato aaaabb
      v_aportacion              CHAR(10), -- aportación (48-57)
      v_folio_sua               CHAR(6), -- folio sua (58-63)
      v_fec_aplic_safre         CHAR(8), -- fecha aplicaciones safre (64-71) formato aaaammdd
      v_fec_entrega             CHAR(8), -- fecha de entrega (72-79) formato aaaammdd
      v_interface               CHAR(2), -- inteface (80-81)
      v_rechazo                 CHAR(3) -- código de rechazo
   END RECORD

   DEFINE v_c_tpo_transf        CHAR(2) -- tipo de transferencia
   DEFINE v_v_arch_proceso      VARCHAR(100) -- nombre del archivo
   DEFINE v_c_fec_hoy           CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_s_registro          STRING -- registro a insertar
   DEFINE v_c_extension         LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_d_total_pesos       DECIMAL(20,2) -- monto total en pesos
   DEFINE v_i_contrador_reg     INTEGER -- contrador de registros
   DEFINE v_s_qryTxt            STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_s_comando           STRING -- contiene al comando a correr

   -- se inicializan variables
   LET v_i_contrador_reg = 0
   LET v_d_total_pesos = 0
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_c_extension = "rchua"
   LET v_c_tpo_transf = "48"

   -- se asigna el nombre del archivo
   LET v_v_arch_proceso = "A" || v_c_fec_hoy || "." || v_c_extension CLIPPED

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_nomarch = p_c_ruta_envio CLIPPED || "/" || v_v_arch_proceso CLIPPED
   DISPLAY " ARCHIVO A GENERAR (48): ", v_v_arch_proceso

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT cre.id_derechohabiente, cre.periodo_pago, cre.diagnostico, SUM(cre.importe_v97),\n",
                    "        afi.nss, afi.nombre_af, afi.ap_paterno_af, afi.ap_materno_af,\n",
                    "        cre.f_movimiento\n",
                    "   FROM cre_uso_garantia cre, afi_derechohabiente afi\n",
                    "  WHERE cre.estado IN (240, 150)\n",
                    "    AND cre.id_cre_ctr_archivo = ",p_id_cre_ctr_archivo,"\n",
                    "    AND cre.id_derechohabiente = afi.id_derechohabiente\n",
                    "    AND cre.tpo_transferencia = '48'\n",
                    "  GROUP BY 1, 2, 3, 5, 6, 7, 8, 9"

   PREPARE prp_rch_uso_garant48 FROM v_s_qryTxt
   DECLARE cur_rch_uso_garant48 CURSOR FOR prp_rch_uso_garant48

   FOREACH cur_rch_uso_garant48 INTO v_r_uso_garant.*
      -- se valida el importe obtenido
      IF v_r_uso_garant.pesos IS NULL THEN
         LET v_r_uso_garant.pesos = 0
      END IF

      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.v_nss             = v_r_uso_garant.nss
      LET v_r_detalle.v_nombre          = v_r_uso_garant.ap_paterno_af CLIPPED, " ", v_r_uso_garant.ap_materno_af CLIPPED, " ", v_r_uso_garant.nombre_af CLIPPED
      LET v_r_detalle.v_periodo         = v_r_uso_garant.periodo_pago USING "&&&&&&"
      LET v_r_detalle.v_aportacion      = ( v_r_uso_garant.pesos * 100) USING "&&&&&&&&&&"
      LET v_r_detalle.v_folio_sua       = "" -- 6 espacios en blanco 
      LET v_r_detalle.v_fec_aplic_safre = v_r_uso_garant.f_liquida USING "yyyymmdd"
      LET v_r_detalle.v_fec_entrega     = TODAY USING "yyyymmdd"
      LET v_r_detalle.v_interface       = "UG"
      LET v_r_detalle.v_rechazo         = v_r_uso_garant.diagnostico USING "&&&"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.v_nss,
                         v_r_detalle.v_nombre,
                         v_r_detalle.v_periodo,
                         v_r_detalle.v_aportacion,
                         v_r_detalle.v_folio_sua,
                         v_r_detalle.v_fec_aplic_safre,
                         v_r_detalle.v_fec_entrega,
                         v_r_detalle.v_interface,
                         v_r_detalle.v_rechazo

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se acumula el monto del crédito
      LET v_d_total_pesos = v_d_total_pesos  + v_r_uso_garant.pesos
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   LET v_nom_fin = "A_rch_48ua." || v_c_extension

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = p_c_ruta_envio CLIPPED || "/" || v_nom_fin CLIPPED

    -- para tpo transf 48: A_rch_48ua.rchua

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   --LET v_s_Txt = "unix2dos "||" "||p_c_ruta_envio CLIPPED||" "||v_nom_fin CLIPPED
   --RUN v_s_Txt

   DISPLAY " Nombre del archivo rechazos de uso de garantía de AG a enviar: ",v_nom_fin
   DISPLAY ""
   DISPLAY " Ejecutando envío interfaz SAS"

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/A_Rch_48ua.sh"
   RUN v_ejecuta_sh

   DISPLAY ""

   -- salida de reporte
   OUTPUT TO REPORT reporte_archivo_salida(p_v_arch_entrada, v_v_arch_proceso, v_c_tpo_transf, v_d_total_pesos, v_i_contrador_reg)

END FUNCTION

#Objetivo: Funcion que consulta los datos de la tabla afi derechohabiente
FUNCTION f_obt_datos_trab(p_id_derechohabiente)

   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente -- identificador del derechohabiente 
   DEFINE v_r_afi_derechohab   RECORD LIKE afi_derechohabiente.* -- registro de afi derechohabiente
   DEFINE v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene la información del trabajador
   LET v_s_qryTxt = " SELECT FIRST 1 *\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_id_derechohabiente

   PREPARE prp_afi_derechohabiente FROM v_s_qryTxt
   EXECUTE prp_afi_derechohabiente INTO v_r_afi_derechohab.*

   RETURN v_r_afi_derechohab.*

END FUNCTION

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_v_arch_entrada, p_v_arch_salida, p_c_tpo_transf, p_v_aportacion, p_count_reg)

   DEFINE p_v_arch_entrada CHAR(100)
   DEFINE p_v_arch_salida  CHAR(100)
   DEFINE p_c_tpo_transf   CHAR(2) -- tipo de transferencia
   DEFINE p_v_aportacion   DECIMAL(20,2)
   DEFINE p_count_reg      INTEGER
   DEFINE v_d_total_aport  DECIMAL(12,2)
   DEFINE v_i_total_regs   INTEGER
   DEFINE v_fecha_reporte  DATE
   DEFINE v_fecha_present  LIKE dis_sum_avance_pago.f_presentacion

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte      

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX m_d_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX m_v_usuario

   ON EVERY ROW
      PRINTX p_v_arch_entrada
      PRINTX p_v_arch_salida
      PRINTX p_c_tpo_transf
      PRINTX p_v_aportacion
      PRINTX p_count_reg

   ON LAST ROW
      LET v_d_total_aport = SUM(p_v_aportacion)
      LET v_i_total_regs = SUM(p_count_reg)

      PRINTX v_d_total_aport
      PRINTX v_i_total_regs

END REPORT