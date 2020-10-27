--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

######################################################################
#Módulo             => OCG                                           #
#Programa           => OCGE92                                        #
#Objetivo           => Programa que permite integrar el archivo de   #
#                      migración de detalles de Créditos en Garantía #
#Autor              => Mauro Muñiz Caballero, EFP                    #
#Fecha inicio       => 7 de agosto de 2016                           #
######################################################################

DATABASE safre_viv

#Objetivo: Funcion que realiza la integracion del archivo de rechazo saldo
MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar

   DEFINE v_d_cre_ctr_archivo       DECIMAL(10,0)

   DEFINE v_tot_reg_rch             DECIMAL(10,0)
   DEFINE v_tot_sp1_rch             DECIMAL(10,0)
   DEFINE v_tot_sp2_rch             DECIMAL(10,0)
   DEFINE v_tot_sp3_rch             DECIMAL(10,0)
   DEFINE v_tot_sp4_rch             DECIMAL(10,0)
   DEFINE v_tot_sp5_rch             DECIMAL(10,0)

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                 DECIMAL(10,0),
      tot_sp1                       DECIMAL(10,0),
      tot_sp2                       DECIMAL(10,0),
      tot_sp3                       DECIMAL(10,0),
      tot_sp4                       DECIMAL(10,0),
      tot_sp5                       DECIMAL(10,0)
   END RECORD

   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_operacion             LIKE cre_ctr_archivo.operacion -- operacion
   DEFINE v_dt_f_lote               LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_si_lote                 LIKE cre_ctr_archivo.lote -- lote
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE r_cod_error               SMALLINT -- contiene el codigo de error en caso de ocurrir
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_isam_err                INTEGER 
   DEFINE r_c_msj                   VARCHAR(250)
   DEFINE r_c_nss                   LIKE afi_derechohabiente.nss

   -- se recuperan los parámetros que envía el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".OCGE92.log")

   DISPLAY "=INICIA OCGE92="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion   = 91 -- Detalles
   LET v_si_id_proceso = 3991

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)

   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
   LET v_s_qryTxt = " SELECT FIRST 1 id_ocg_ctr_archivo, f_lote, 1\n",
                    "   FROM ocg_ctr_archivo\n",
                    "  WHERE id_proceso = ",v_si_id_proceso,"\n",
                    "    AND operacion = ",v_i_operacion,"\n",
                    "    AND estatus = 10\n",
                    "  ORDER BY id_ocg_ctr_archivo DESC"

   PREPARE prp_id_creCtrArchivo FROM v_s_qryTxt
   EXECUTE prp_id_creCtrArchivo INTO v_d_cre_ctr_archivo, v_dt_f_lote, v_si_lote

   -- se verifica si fue posible obtener el identificador del archivo
   IF v_d_cre_ctr_archivo IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   END IF

   ---CALL fn_crea_ix_tmp()

   DISPLAY " INTEGRA ARCHIVO DE MIGRACIÓN DE DETALLES SP001"

   DATABASE safre_tmp

   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_integra_detalle()"

   PREPARE prp_obt_id FROM v_s_qryTxt
   EXECUTE prp_obt_id

   -- se crea la sentencia que ejecuta el procedure que realiza la integracion de rechazo de saldos
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_ocg_integra_detalles001(?,?,?)"

   PREPARE prp_integra_det1 FROM v_s_qryTxt
   EXECUTE prp_integra_det1 USING p_v_usuario,
                                  p_d_folio,
                                  v_d_cre_ctr_archivo
                             INTO r_cod_error,
                                  r_isam_err,
                                  r_c_msj,
                                  r_c_nss,
                                  v_tot_reg_rch,
                                  v_tot_sp1_rch,
                                  v_tot_sp2_rch,
                                  v_tot_sp3_rch,
                                  v_tot_sp4_rch,
                                  v_tot_sp5_rch

   DISPLAY ""
   DISPLAY " INTEGRA ARCHIVO DE MIGRACIÓN DE DETALLES SP002"

   -- se crea la sentencia que ejecuta el procedure que realiza la integracion de rechazo de saldos
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_ocg_integra_detalles002(?,?,?)"

   PREPARE prp_integra_det2 FROM v_s_qryTxt
   EXECUTE prp_integra_det2 USING p_v_usuario,
                                  p_d_folio,
                                  v_d_cre_ctr_archivo
                             INTO r_cod_error,
                                  r_isam_err,
                                  r_c_msj,
                                  r_c_nss,
                                  v_tot_reg_rch,
                                  v_tot_sp1_rch,
                                  v_tot_sp2_rch,
                                  v_tot_sp3_rch,
                                  v_tot_sp4_rch,
                                  v_tot_sp5_rch


   DATABASE safre_viv

   IF r_cod_error <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE INTEGRACIÓN DEL ARCHIVO DE DETALLES"
      DISPLAY "CÓD. ERROR : ",r_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- ocurrió un error y se marca como rechazado la operación
      LET r_cod_error = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("ocg") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros,tot_sp1, tot_sp2, tot_sp3, tot_sp4, tot_sp5\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_cre_ctr_archivo

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY "TOTALES"

   DISPLAY "Registros           : ",v_r_cre_ctr_archivo.tot_registros
   DISPLAY "Registros sp1       : ",v_r_cre_ctr_archivo.tot_sp1
   DISPLAY "Registros sp2       : ",v_r_cre_ctr_archivo.tot_sp2
   DISPLAY "Registros sp3       : ",v_r_cre_ctr_archivo.tot_sp3
   DISPLAY "Registros sp4       : ",v_r_cre_ctr_archivo.tot_sp4
   DISPLAY "Registros sp5       : ",v_r_cre_ctr_archivo.tot_sp5

   DISPLAY "Registros rechazados: ",v_tot_reg_rch
   DISPLAY "Rechazos sp1        : ",v_tot_sp1_rch
   DISPLAY "Rechazos sp2        : ",v_tot_sp2_rch
   DISPLAY "Rechazos sp3        : ",v_tot_sp3_rch
   DISPLAY "Rechazos sp4        : ",v_tot_sp4_rch
   DISPLAY "Rechazos sp5        : ",v_tot_sp5_rch

      -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF
END MAIN

{
FUNCTION fn_crea_ix_tmp()
#------------------------

   DEFINE v_sql_ix STRING

   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      LET v_sql_ix = " DROP INDEX ID EXISTS ix_tmp_detalle1";

      PREPARE prp_index_dmv FROM v_sql_ix
      EXECUTE prp_index_dmv


      -- se ejecuta el script para crear índices y estadísticas
      LET v_sql_ix = " CREATE INDEX ix_tmp_detalle1\n",
                     "  ON tmp_detalle_mig (nss) USING btree ;"

      PREPARE prp_index_mov FROM v_sql_ix
      EXECUTE prp_index_mov

      LET v_sql_ix = "UPDATE STATISTICS FOR TABLE tmp_detalle_mig"

      PREPARE prp_actualiza_est FROM v_sql_ix
      EXECUTE prp_actualiza_est
   WHENEVER ERROR STOP

   ---DATABASE safre_viv

END FUNCTION
}