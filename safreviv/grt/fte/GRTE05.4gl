--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTE05                                        #
#Objetivo          =>Programa para integrar el archivo de          #
#                    Solicitudes no Atentidas para el módulo de    #
#                    Solicitud de Saldo en Garantía                #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>23 Abril 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

#Objetivo: Funcion que realiza la integracion del archivo de solicitudes no atendidas
MAIN
   DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion
          p_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_arch_proceso    VARCHAR(100), -- nombre del archivo a integrar
          v_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_r_cre_ctr_archivo RECORD
             tot_registros    LIKE cre_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados    LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados   LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
             tot_sin_origen   LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
          END RECORD,
          v_i_operacion       LIKE cre_ctr_archivo.operacion, -- operacion
          v_dt_f_lote         LIKE cre_ctr_archivo.f_lote, -- fecha del lote
          v_si_lote           LIKE cre_ctr_archivo.lote, -- lote
          v_si_id_proceso     LIKE cre_ctr_archivo.id_proceso, -- identificador del proceso
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          v_s_comando         STRING, -- se asigna un comando a ejecutar
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados del módulo 
          r_si_cod_error      SMALLINT, -- indica si el proceso puede continuar o no (contiene el código de error)
          r_isam_err          INTEGER, 
          r_c_msj             VARCHAR(250),
          r_c_nss             LIKE afi_derechohabiente.nss

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE05.log")

   DISPLAY "=INICIA GRTE05="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 14 -- Solicitudes no atendidas
   LET v_si_id_proceso = g_id_proceso_grt -- Solicitud de Saldos en Garantia 43 bis

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_no_aten()

   -- se busca numero de lote y fecha correspondiente al archivo
   LET v_s_qryTxt = " SELECT f_presentacion, cons_lote\n",
                    "   FROM safre_tmp:tmp_no_atend_enc_grt"

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

   DISPLAY " INTEGRA NO ATENDIDAS"
   -- se crea la sentencia que ejecuta el procedure que realiza la integracion de solicitudes no atendidas
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_grt_integra_no_aten(?,?,?,?)"

   PREPARE prp_integra_solic_no_aten FROM v_s_qryTxt
   EXECUTE prp_integra_solic_no_aten USING p_v_usuario,
                                           p_v_arch_proceso,
                                           p_d_folio,
                                           v_d_id_cre_ctr_arch
                                      INTO r_si_cod_error,
                                           r_isam_err,
                                           r_c_msj,
                                           r_c_nss

   -- verifica si ocurrió un error en la integración
   IF r_si_cod_error <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE INTEGRACIÓN"
      DISPLAY "CÓD. ERROR : ",r_si_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- ocurrió un error por lo que se marca como rechazada la operación
      LET r_si_cod_error = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      IF r_si_cod_error <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_si_cod_error)
      END IF

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY "TOT_REGISTROS TOT_ACEPTADOS TOT_RECHAZADOS TOT_SIN_ORIGEN"
   DISPLAY v_r_cre_ctr_archivo.*

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   DISPLAY " EJECUTA CONCILIACIÓN DE USO DE GARANTIA"
   -- se crea el comando que ejecuta la conciliación de Rechazo de Saldos
   LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/GRTP17 ",
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

#Objetivo: Función que crea la tabla temporal de la integración de no atendidas
FUNCTION fn_crea_tmp_no_aten()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_deudor_no_aten_grt

   WHENEVER ERROR STOP

   CREATE TABLE tmp_deudor_no_aten_grt(id_cre_acreditado DECIMAL(9,0),
                                       id_derechohabiente DECIMAL(9,0),
                                       nss CHAR(11))

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION
