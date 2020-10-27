--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRE13                                        #
#Objetivo          =>Programa que realiza la integración del       #
#                    archivo de Uso de Garantía Estados y Mcpios   #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>08 Julio 2013                                 #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
       p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
       p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion
       p_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
       p_v_arch_proceso    VARCHAR(100) -- nombre del archivo a integrar

#Objetivo: Funcion que realiza la integracion del archivo de solicitud de saldo
MAIN
   DEFINE v_d_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          v_r_cre_ctr_archivo RECORD
             tot_registros    LIKE cre_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados    LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados   LIKE cre_ctr_archivo.tot_rechazados -- total rechazados
          END RECORD,
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          v_i_operacion       LIKE cre_ctr_archivo.operacion, -- operacion
          v_si_id_proceso     LIKE cre_ctr_archivo.id_proceso, -- identificador del proceso
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          r_isam_err          INTEGER, 
          r_c_msj             VARCHAR(250)

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRE13.log")

   DISPLAY "=INICIA AGRE13="
   DISPLAY " USUARIO        : ",p_v_usuario
   DISPLAY " PID            : ",p_d_pid
   DISPLAY " ARCHIVO ENTRADA: ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 25 -- operacion del proceso (25 -- Carga Uso Garantía Estados y Municipios)
   LET v_si_id_proceso = g_id_proceso_agr -- Anualidades Garantizadas

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO          : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_uso_edo_mcpio()

   -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
   LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",v_si_id_proceso,"\n",
                    "    AND operacion = ",v_i_operacion,"\n",
                    "    AND estado = 10\n",
                    "  ORDER BY id_cre_ctr_archivo DESC"

   PREPARE prp_id_creCtrArchivo FROM v_s_qryTxt
   EXECUTE prp_id_creCtrArchivo INTO v_d_cre_ctr_archivo

   -- se verifica si fue posible obtener el identificador del archivo
   IF v_d_cre_ctr_archivo IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   END IF

   DISPLAY " INTEGRA SOLICITUD SALDO"
   -- se crea la sentencia que ejecuta el procedure que realiza la integracion de solicitud de saldo
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_agr_integra_uso_edo_mcpio(?,?,?,?)"

   PREPARE prp_integra_solic_saldo FROM v_s_qryTxt
   EXECUTE prp_integra_solic_saldo USING p_v_usuario, p_v_arch_proceso, p_d_folio, v_d_cre_ctr_archivo
                                    INTO r_b_valida, r_isam_err, r_c_msj

   IF r_b_valida <> 0 THEN
      DISPLAY " Ocurrió un error durante el proceso de Integración"
      DISPLAY " CÓDIGO ERR : ",r_b_valida
      DISPLAY " ISAM ERR   : ",r_isam_err
      DISPLAY " MENSAJE ERR: ",r_c_msj

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se realiza consulta de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_aceptados, tot_rechazados\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_cre_ctr_archivo

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY " TOT_REGISTROS :",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOT_ACEPTADOS :",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOT_RECHAZADOS:",v_r_cre_ctr_archivo.tot_rechazados

   DISPLAY " GENERA ARCHIVO SALIDA (RESPUESTA DE USO DE GARANTÍAS ESTADOS MUNICIPIOS)"
   CALL fn_genera_arch_solic_saldo(v_d_cre_ctr_archivo)

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- se marca como ERRONEO el proceso
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible marcar como ERROR la operacion
      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      --EXIT PROGRAM
   END IF
   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que crea la tabla temporal de la integración de Uso Gtía Edo y Mcpio
FUNCTION fn_crea_tmp_uso_edo_mcpio()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- en caso de error continua
   WHENEVER ERROR CONTINUE

   -- se elimina la tabla temporal
   DROP TABLE tmp_resp_uso_edo_mcpio

   -- al encontrar un error detiene el programa
   WHENEVER ERROR STOP

   CREATE TABLE tmp_resp_uso_edo_mcpio(nss           CHAR(11),
                                       fec_envio     DATE,
                                       mto_recup     DECIMAL(13,2),
                                       edo_reg       CHAR(1),
                                       fdo_inver     SMALLINT,
                                       cod_rech      CHAR(1),
                                       causal_rech   SMALLINT,
                                       folio_liquida DECIMAL(9,0))

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION

FUNCTION fn_genera_arch_solic_saldo(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador de archivo
          v_v_arch_salida       VARCHAR(100), -- nombre del archivo de salida
          v_v_ruta_archivo      VARCHAR(150), -- ruta y nombre del archivo de salida
          v_v_ruta_reporte      VARCHAR(150), -- ruta y nombre del reporte
          v_c_fec_hoy           CHAR(8), -- fecha con formato "yyyymmdd"
          v_c_extension         LIKE cat_operacion.extension, -- extensión del archivo
          v_ch_arch_solic_sdo   BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_r_tmp_uso_edo_mcpio RECORD
             nss                CHAR(11),
             fec_envio          DATE,
             mto_recup          DECIMAL(15,2),
             edo_reg            CHAR(1),
             fdo_inver          SMALLINT,
             cod_rech           CHAR(1),
             causal_rech        SMALLINT,
             folio_liquida      DECIMAL(9,0)
          END RECORD,
          v_r_detalle           RECORD
             nss                CHAR(11),
             fec_envio          CHAR(8),
             mto_recup          CHAR(15),
             edo_reg            CHAR(1),
             fdo_inver          CHAR(5),
             cod_rech           CHAR(1),
             causal_rech        CHAR(3)
          END RECORD,
          v_r_sumario           RECORD
             tpo_registro       CHAR(1),
             tot_registros      CHAR(9),
             tot_sdo_viv        CHAR(15),
             filler             CHAR(30)
          END RECORD,
          v_d_tot_mto_pesos     DECIMAL(20,2), -- monto total en saldo vivienda 92
          v_s_registro          STRING, -- registro a insertar
          v_i_contrador_reg     INTEGER, -- contrador de registros
          v_c_programa_cod      LIKE cat_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt            STRING, -- guarda una sentencia sql a ejecutar
          v_c_ruta_bin          LIKE seg_modulo.ruta_bin, -- ruta donde se ejecuta el archivo
          v_c_ruta_envio        LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_c_ruta_listado      LIKE seg_modulo.ruta_listados, -- ruta donde se colocara el archivo
          v_manejador_rpt       OM.SaxDocumentHandler, -- Contenedor de Documentos para el reporte          
          v_archivo_nom         STRING, 
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_i_contrador_reg = 0 -- contador de registros
   LET v_d_tot_mto_pesos = 0 -- saldo vivienda 92

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo
   LET v_v_arch_salida = "Resp_uso_edo_mcpio." || v_c_extension CLIPPED
   DISPLAY " ARCHIVO SALIDA : ", v_v_arch_salida

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_bin, ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_bin, v_c_ruta_envio, v_c_ruta_listado

   -- se concatena el nombre del archivo con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida CLIPPED

   -- se crea el manejador de archivo
   LET v_ch_arch_solic_sdo = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solic_sdo.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_solic_sdo.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM safre_tmp:tmp_resp_uso_edo_mcpio"

   PREPARE prp_tmp_solic_sdo FROM v_s_qryTxt
   DECLARE cur_tmp_solic_sdo CURSOR FOR prp_tmp_solic_sdo

   FOREACH cur_tmp_solic_sdo INTO v_r_tmp_uso_edo_mcpio.*
      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.nss = v_r_tmp_uso_edo_mcpio.nss
      LET v_r_detalle.fec_envio = v_r_tmp_uso_edo_mcpio.fec_envio USING "yyyymmdd"
      LET v_r_detalle.mto_recup = (v_r_tmp_uso_edo_mcpio.mto_recup * 100) USING "&&&&&&&&&&&&&&&"
      LET v_r_detalle.edo_reg = v_r_tmp_uso_edo_mcpio.edo_reg
      LET v_r_detalle.fdo_inver = v_r_tmp_uso_edo_mcpio.fdo_inver USING "&&&&&"
      LET v_r_detalle.cod_rech = v_r_tmp_uso_edo_mcpio.cod_rech
      LET v_r_detalle.causal_rech = v_r_tmp_uso_edo_mcpio.causal_rech USING "&&&"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.nss,
                         v_r_detalle.fec_envio,
                         v_r_detalle.mto_recup,
                         v_r_detalle.edo_reg,
                         v_r_detalle.fdo_inver,
                         v_r_detalle.cod_rech,
                         v_r_detalle.causal_rech

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solic_sdo.write([v_s_registro])

      -- se acumula el monto del crédito
      LET v_d_tot_mto_pesos = v_d_tot_mto_pesos + v_r_tmp_uso_edo_mcpio.mto_recup
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solic_sdo.close()

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)
{
   -- se asigna el nombre y ruta del reporte
   LET v_v_ruta_reporte = v_c_ruta_bin CLIPPED || "/AGRE131.4rp"
}
   -- se carga la configuración del reporte
   IF fgl_report_loadCurrentSettings("AGRE131.4rp") THEN
      LET v_archivo_nom = p_v_usuario CLIPPED ,"-",
                          v_c_programa_cod CLIPPED,"-",
                          p_d_pid USING "&&&&&","-",
                          p_i_proceso_cod USING "&&&&&","-",
                          p_i_opera_cod USING "&&&&&"

      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listado CLIPPED||"/"||v_archivo_nom)

      -- sin vista previa
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración al manejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte\n ",v_v_ruta_reporte
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- empieza el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- salida de reporte
   OUTPUT TO REPORT reporte_archivo_salida(v_v_arch_salida, v_d_tot_mto_pesos, v_i_contrador_reg)

   -- finaliza el reporte
   FINISH REPORT reporte_archivo_salida
END FUNCTION

#Objetivo: Genera el reporte de Uso de Garantía Estados y Municipios
REPORT reporte_archivo_salida(p_v_arch_salida,
                              p_d_mto_tot_pss,
                              p_count_reg)
   DEFINE p_v_arch_salida  CHAR(100),
          p_d_mto_tot_pss  DECIMAL(20,2),
          p_count_reg      INTEGER,
          v_fecha_reporte  DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY

      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_v_usuario
      PRINTX p_d_folio
      PRINTX p_v_arch_proceso
      PRINTX p_v_arch_salida
      PRINTX p_d_mto_tot_pss
      PRINTX p_count_reg
END REPORT
