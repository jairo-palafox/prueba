--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23-04-2012
--==============================================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL12                                                        #
#Objetivo     => Programa lanzado del reverso de validacion de instrucciones   # 
#                con origen recurrente                                         #
#Fecha inicio => Abril 23, 2012                                                #
#Autor        => Hugo César Ramírez García                                     #
################################################################################
DATABASE safre_viv

DEFINE r_usuario_cod  LIKE seg_usuario.usuario_cod,
       r_pid          LIKE bat_ctr_operacion.pid,
       r_proceso_cod  LIKE cat_proceso.proceso_cod,
       r_opera_cod    LIKE cat_operacion.opera_cod,
       r_folio         LIKE mdt_lote_mandato.folio,
       r_f_proceso       LIKE mdt_lote_mandato.f_proceso,
       v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_mensaje      STRING
       --r_pid_reverso LIKE cat_proceso.proceso_cod

MAIN
DEFINE v_continua BOOLEAN,
       v_consulta STRING,
       v_ind_rev   SMALLINT,
       v_diag      SMALLINT,
       v_tot_rev_a INTEGER,
       v_tot_rev_m INTEGER,
       v_tot_rev_r INTEGER,
       v_tot_rev_b INTEGER,
       v_tot_rev   INTEGER

   WHENEVER ERROR CONTINUE 
   
   LET r_usuario_cod = ARG_VAL(1)
   LET r_pid         = ARG_VAL(2)
   LET r_proceso_cod = ARG_VAL(3)
   LET r_opera_cod   = ARG_VAL(4)
   LET r_folio        = ARG_VAL(5)
   LET r_f_proceso      = ARG_VAL(6)
   --LET r_pid_reverso = ARG_VAL(7)
   
   --CALL fn_valida_operacion(r_pid,r_proceso_cod,r_opera_cod) RETURNING v_continua
   --IF(v_continua <> 0)THEN
      # Imprime el mensaje de inconsistencia en consola
   --   CALL fn_desplega_inc_operacion(v_continua)
   --ELSE
      {CALL fn_actualiza_opera_ini(r_pid,r_proceso_cod,r_opera_cod,r_folio,"MDTR12","",r_usuario_cod)
                 RETURNING v_continua
      IF(v_continua  <> 0)THEN
         # Imprime el mensaje de inconsistencia en consola
         CALL fn_desplega_inc_operacion(v_continua)
      ELSE}
         LET v_consulta = "EXECUTE FUNCTION safre_viv:fn_mdt_rev_val_recurrente(?,?)"
         PREPARE prp_ejecuta_reverso_val_recurrente FROM v_consulta
         EXECUTE prp_ejecuta_reverso_val_recurrente USING r_folio,r_f_proceso
                                                    INTO v_ind_rev, v_diag, v_tot_rev_a,
                                                         v_tot_rev_m, v_tot_rev_r, v_tot_rev_b,
                                                         v_tot_rev
         IF(SQLCA.SQLCODE <> 0)THEN
            LET v_mensaje = "Error ejecucion SP"
            DISPLAY "\nError ejecucion SP (Codigo): ",SQLCA.SQLCODE
            DISPLAY "Error en SP (Mensaje):",SQLCA.SQLERRM,"\n"
            CALL fn_error_opera(r_pid,r_proceso_cod,r_opera_cod) RETURNING v_continua
            IF(v_continua  <> 0)THEN
               # Imprime el mensaje de inconsistencia en consola
               CALL fn_desplega_inc_operacion(v_continua)
            END IF
            RETURN 0  # Termina ejecucion
         ELSE
            CALL fn_actualiza_opera_fin(r_pid,r_proceso_cod,r_opera_cod) RETURNING v_continua
            IF(v_continua  <> 0)THEN
               # Imprime el mensaje de inconsistencia en consola
               CALL fn_desplega_inc_operacion(v_continua)
               CALL fn_error_opera(r_pid,r_proceso_cod,r_opera_cod) RETURNING v_continua
               IF(v_continua  <> 0)THEN
                  # Imprime el mensaje de inconsistencia en consola
                  CALL fn_desplega_inc_operacion(v_continua)
               END IF
               RETURN 0  # Termina ejecucion
            END IF
         END IF
         IF(v_ind_rev = 0 AND v_diag = 0)THEN
            LET v_mensaje = "TOTAL DE ALTAS REVERSADAS: ",v_tot_rev_a,
                            "\nTOTAL DE MODIFICACIÓNES REVERSADAS: ",v_tot_rev_m,
                            "\nTOTAL DE REACTIVACIÓNES REVERSADAS: ",v_tot_rev_r,
                            "\nTOTAL DE BAJAS REVERSADAS: ",v_tot_rev_b,
                            "\nTOTAL DE REGISTROS REVERSADAS: ",v_tot_rev
            # Se mandan a log las cifras de reverso
            DISPLAY "TOTAL DE ALTAS REVERSADAS: ",v_tot_rev_a
            DISPLAY "TOTAL DE MODIFICACIÓNES REVERSADAS: ",v_tot_rev_m
            DISPLAY "TOTAL DE REACTIVACIÓNES REVERSADAS: ",v_tot_rev_r
            DISPLAY "TOTAL DE BAJAS REVERSADAS: ",v_tot_rev_b
            DISPLAY "TOTAL DE REGISTROS REVERSADAS: ",v_tot_rev
         ELSE
            LET v_mensaje = "INDICADOR DE REVERSO: ",v_ind_rev,
                            "\nDIAGNOSTICO: ",v_diag,
                            "\nTOTAL DE ALTAS REVERSADAS: ",v_tot_rev_a,
                            "\nTOTAL DE MODIFICACIÓNES REVERSADAS: ",v_tot_rev_m,
                            "\nTOTAL DE REACTIVACIÓNES REVERSADAS: ",v_tot_rev_r,
                            "\nTOTAL DE BAJAS REVERSADAS: ",v_tot_rev_b,
                            "\nTOTAL DE REGISTROS REVERSADAS: ",v_tot_rev
            # Se muestra indicadores derechazo de reverso  
            DISPLAY "INDICADOR DE REVERSO: ",v_ind_rev
            DISPLAY "DIAGNOSTICO: ",v_diag
            DISPLAY "TOTAL DE ALTAS REVERSADAS: ",v_tot_rev_a
            DISPLAY "TOTAL DE MODIFICACIÓNES REVERSADAS: ",v_tot_rev_m
            DISPLAY "TOTAL DE REACTIVACIÓNES REVERSADAS: ",v_tot_rev_r
            DISPLAY "TOTAL DE BAJAS REVERSADAS: ",v_tot_rev_b
            DISPLAY "TOTAL DE REGISTROS REVERSADAS: ",v_tot_rev
         END IF
      --END IF
   --END IF
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = r_proceso_cod
   CALL fn_correo_proceso(r_pid, 
                          r_proceso_cod, 
                          r_opera_cod, 
                          '', # Archivo adjunto
                          'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reverso de validación de recurrentes',
                          v_mensaje
                          )
END MAIN
