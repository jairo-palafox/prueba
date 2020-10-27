--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-07-2013 
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSR35                                                   #
#Objetivo          => Batch reverso de liquidacion pago mandatos               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 12 Julio 2013                                            #
#Modificado        =>                                                          #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # Usuario
       p_pid_rev         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod_rev LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod_rev   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio           LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       v_pid             LIKE bat_ctr_proceso.pid,     # identificador de proceso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # Código del proceso
       v_opera_cod       LIKE cat_operacion.opera_cod # Código de la operacion

MAIN
DEFINE v_proceso_desc   LIKE cat_proceso.proceso_desc,
       v_opera_desc     LIKE cat_operacion.opera_desc,
       v_consulta       STRING,
       v_ind            SMALLINT,
       v_diag           CHAR(3),
       v_sql_error      INTEGER,
       v_isam_error     SMALLINT,
       v_msg_error      CHAR(100),
       v_tot_reversados INTEGER,
       v_mensaje        STRING,
       r_res_opera      SMALLINT,
       v_bnd_rev_cnt    SMALLINT

   # Se recuperan los parámetros
   LET p_usuario_cod     = ARG_VAL(1)
   LET p_pid_rev         = ARG_VAL(2)
   LET p_proceso_cod_rev = ARG_VAL(3)
   LET p_opera_cod_rev   = ARG_VAL(4)
   LET p_folio           = ARG_VAL(5)
   LET p_nom_archivo     = ARG_VAL(6)

   # proceso y operaciones iniciales
   LET v_proceso_cod = g_proceso_cod_pago_mandatos # Pago mandatos
   LET v_opera_cod   = g_opera_cod_integracion     # liquidacion pago mandatos
   
   # Valida si se puede ejecutar reverso para registro contable
   --CALL fn_reverso_reg_cnt(p_folio) RETURNING v_bnd_rev_cnt
   LET v_bnd_rev_cnt = 0
   IF( v_bnd_rev_cnt )THEN
      DISPLAY "Reverso no procedente: registro contable realizado"
      CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      IF( r_res_opera <> 0 )THEN
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF
   ELSE

      # Recuepra descripcion de proceso y operacion de reverso
      CALL fn_proceso_cod_desc(p_proceso_cod_rev) RETURNING v_proceso_desc
      CALL fn_opera_cod_desc(p_proceso_cod_rev, p_opera_cod_rev) RETURNING v_opera_desc
      DISPLAY "\n"
      DISPLAY "PROCESO      :",v_proceso_desc
      DISPLAY "OPERACIÓN    :",v_opera_desc
      DISPLAY "FOLIO        :",p_folio
      DISPLAY "\n"

      # recupera el pid que le corresponde al proceso y folio
      SELECT pid
        INTO v_pid
        FROM bat_ctr_operacion
       WHERE proceso_cod = v_proceso_cod
         AND opera_cod   = v_opera_cod
         AND folio       = p_folio

      LET v_consulta = "EXECUTE FUNCTION fn_hps_r_liquidar_pagos(?,?,?,?,?)"
      PREPARE prp_reverso_integracion FROM v_consulta
      EXECUTE prp_reverso_integracion USING p_folio,
                                            v_pid,
                                            v_proceso_cod,
                                            v_opera_cod,
                                            p_usuario_cod
                                       INTO v_ind,
                                            v_diag,
                                            v_sql_error,
                                            v_isam_error,
                                            v_msg_error,
                                            v_tot_reversados
      
      IF( v_sql_error <> 0 )THEN
         DISPLAY "\nError en ejecución de SP (Codigo): ",v_sql_error
         DISPLAY "Error en SP (Mensaje):",v_msg_error,"\n"
         LET v_mensaje = "Error al realizar el reverso. Código: ",v_sql_error,", mensaje: ",v_msg_error
         CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
         IF( r_res_opera <> 0 )THEN
            CALL fn_desplega_inc_operacion(r_res_opera)
         END IF
      ELSE
         IF( v_ind <> 0 )THEN
            LET v_mensaje = "Error al realizar el reverso. "
            # Muestra las incosistencias      
            DISPLAY "\nError en ejecución de SP: "
            CASE v_diag
               WHEN "001" 
                 DISPLAY v_msg_error," \n"
                 LET v_mensaje = v_msg_error
               WHEN "002" 
                 DISPLAY v_msg_error," \n"
                 LET v_mensaje = v_msg_error
            END CASE
            CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
            IF ( r_res_opera <> 0 ) THEN
               CALL fn_desplega_inc_operacion(r_res_opera)
            END IF
         ELSE
            LET v_mensaje = "Operación finalzada correctamente"
            # imprime totales reversados
            DISPLAY "\nTOTAL REGISTROS REVERSADOS: ",v_tot_reversados
            DISPLAY "\n"
            # finaliza operacion de reverso
            CALL fn_actualiza_opera_fin(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
               IF ( r_res_opera  <> 0 ) THEN
                  # Imprime el mensaje de inconsistencia en consola
                  CALL fn_desplega_inc_operacion(r_res_opera)
                  # trata de establecer erronea la operacion
                  CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
                  IF ( r_res_opera  <> 0 ) THEN
                     # Imprime el mensaje de inconsistencia en consola
                     CALL fn_desplega_inc_operacion(r_res_opera)
                  END IF
               END IF
         END IF
      END IF
   END IF
   CALL fn_correo_proceso(p_pid_rev, 
                          p_proceso_cod_rev, 
                          p_opera_cod_rev, 
                          '', # Archivo adjunto
                          'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reverso de liquidación pagos servicios hps',
                          v_mensaje
                          )

END MAIN
