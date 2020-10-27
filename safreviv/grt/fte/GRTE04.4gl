--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

########################################################################
#Modulo            =>GRT                                               #
#Programa          =>GRTE04                                            #
#Objetivo          =>Programa que ejecuta la integracion de Devolución #
#                    de Solicitudes para el módulo de Solicitud de     #
#                    Saldo en Garantía (Éste no genera conciliación)   #
#Autor             =>Daniel Buendia, EFP                               #
#Fecha inicio      =>23 Abril 2012                                     #
########################################################################
DATABASE safre_viv
GLOBALS "GRTG01.4gl"

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid              LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion
          p_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_arch_proceso     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo a integrar
          v_d_id_cre_ctr_arch  LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          v_r_cre_ctr_archivo  RECORD
             tot_registros     LIKE cre_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados     LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados    LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
             tot_sin_origen    LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
          END RECORD,
          v_s_qryTxt           STRING, -- guarda una sentencia SQL a ejecutar
          v_i_operacion        LIKE cre_ctr_archivo.operacion, -- operacion
          v_dt_f_lote          LIKE cre_ctr_archivo.f_lote, -- fecha del lote
          v_si_lote            LIKE cre_ctr_archivo.lote, -- lote
          v_si_id_proceso      LIKE cre_ctr_archivo.id_proceso, -- identificador del proceso
          r_c_ruta_bin         LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          r_c_ruta_listados    LIKE seg_modulo.ruta_listados, -- ruta listados del módulo 
          r_b_valida           SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          r_isam_err           INTEGER,
          r_c_msj              VARCHAR(250),
          r_c_nss              LIKE afi_derechohabiente.nss

    -- se recuperan los parametros que envia el programa lanzador
    LET p_v_usuario         = ARG_VAL(1)
    LET p_d_pid             = ARG_VAL(2)
    LET p_i_proceso_cod     = ARG_VAL(3)
    LET p_i_opera_cod       = ARG_VAL(4)
    LET p_d_folio           = ARG_VAL(5)
    LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE04.log")

   DISPLAY "=INICIA GRTE04="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 6 -- Devolución de solicitudes
   LET v_si_id_proceso = g_id_proceso_grt -- Solicitud de Saldos en Garantía 43 bis

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se busca numero de lote y fecha correspondiente al archivo
   LET v_s_qryTxt = " SELECT f_presentacion, cons_lote\n",
                    "   FROM safre_tmp:tmp_devoluc_enc_grt"

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

   DISPLAY " INTEGRA DEVOLUCIONES"
   -- se crea la sentencia que ejecuta la función que realiza la integracion de devoluciones
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_grt_integra_devolucion(?,?,?,?)"

   PREPARE prp_integra_devoluciones FROM v_s_qryTxt
   EXECUTE prp_integra_devoluciones USING p_v_usuario,
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

   DISPLAY "TOT_REGISTROS TOT_ACEPTADOS TOT_RECHAZADOS TOT_SIN_ORIGEN"
   DISPLAY v_r_cre_ctr_archivo.*

      -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF
END MAIN
