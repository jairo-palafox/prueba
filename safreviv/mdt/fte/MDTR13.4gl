--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17-07-2012
--===============================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL13                                                        #
#Objetivo     => Programa lanzado del reverso de integración de instrucciones  #
#                con origen recurrente                                         #
#Fecha inicio => Julio 17, 2012                                                #
#Autor        => Hugo César Ramírez García                                     #
################################################################################
DATABASE safre_viv

DEFINE r_usuario_cod  LIKE seg_usuario.usuario_cod,
       r_pid          LIKE bat_ctr_operacion.pid,
       r_proceso_cod  LIKE cat_proceso.proceso_cod,
       r_opera_cod    LIKE cat_operacion.opera_cod,
       r_folio        LIKE mdt_lote_mandato.folio,
       r_f_proceso    LIKE mdt_lote_mandato.f_proceso,
       v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_mensaje      STRING,
       v_pid_rev          LIKE bat_ctr_operacion.pid,
       v_proceso_cod_rev  LIKE cat_proceso.proceso_cod,
       v_opera_cod_rev    LIKE cat_operacion.opera_cod
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
       v_tot_rev   INTEGER,
       r_bandera   BOOLEAN

   WHENEVER ERROR CONTINUE 
   
   LET r_usuario_cod = ARG_VAL(1)
   LET r_pid         = ARG_VAL(2)
   LET r_proceso_cod = ARG_VAL(3)
   LET r_opera_cod   = ARG_VAL(4)
   LET r_folio       = ARG_VAL(5)
   LET r_f_proceso   = ARG_VAL(6)
   --LET r_pid_reverso = ARG_VAL(7)
   
   --CALL fn_valida_operacion(r_pid,r_proceso_cod,r_opera_cod) RETURNING v_continua
   --IF(v_continua <> 0)THEN
      # Imprime el mensaje de inconsistencia en consola
   --   CALL fn_desplega_inc_operacion(v_continua)
   --ELSE
      {CALL fn_actualiza_opera_ini(r_pid,r_proceso_cod,r_opera_cod,r_folio,"MDTR13","",r_usuario_cod)
                 RETURNING v_continua
      IF(v_continua  <> 0)THEN
         # Imprime el mensaje de inconsistencia en consola
         CALL fn_desplega_inc_operacion(v_continua)
      ELSE}
         LET v_consulta = "EXECUTE FUNCTION safre_viv:fn_mdt_rev_inserta_inst(?)"
         PREPARE prp_ejecuta_reverso_val_recurrente FROM v_consulta
         EXECUTE prp_ejecuta_reverso_val_recurrente USING r_folio
                                                    INTO v_ind_rev, 
                                                         v_tot_rev
         IF(SQLCA.SQLCODE <> 0)THEN
            LET v_mensaje = "Error ejecucion fn_mdt_rev_inserta_inst"
            DISPLAY "\nError ejecucion SP (Codigo): ",SQLCA.SQLCODE
            DISPLAY "Error en SP (Mensaje):",SQLCA.SQLERRM,"\n"
            CALL fn_error_opera(r_pid,r_proceso_cod,r_opera_cod) RETURNING v_continua
            IF(v_continua  <> 0)THEN
               # Imprime el mensaje de inconsistencia en consola
               CALL fn_desplega_inc_operacion(v_continua)
            END IF
            RETURN 0  # Termina ejecucion
         ELSE
            LET v_proceso_cod_rev = 1303 # proceso a reversar
            LET v_opera_cod_rev   = 2    # operacion a reversar
            CALL fn_max_pid(v_proceso_cod_rev,v_opera_cod_rev)RETURNING v_pid_rev
 
            CALL fn_reversa_operacion(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev)
            RETURNING r_bandera
            IF(r_bandera <> 0)THEN
               CALL fn_desplega_inc_operacion(r_bandera)
            END IF
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
         IF(v_ind_rev = 0)THEN
            UPDATE glo_ctr_archivo
               SET estado = 1,
                   folio = NULL
             WHERE proceso_cod = v_proceso_cod_rev
               AND folio = r_folio 
            LET v_mensaje = "\nTOTAL DE REGISTROS REVERSADOS: ",v_tot_rev
            # Se mandan a log las cifras de reverso
            DISPLAY "TOTAL DE REGISTROS REVERSADAS: ",v_tot_rev
         ELSE
            LET v_mensaje = "INDICADOR DE REVERSO: ",v_ind_rev,
                            "\nTOTAL DE REGISTROS REVERSADOS: ",v_tot_rev
            # Se muestra indicadores derechazo de reverso  
            DISPLAY "INDICADOR DE REVERSO: ",v_ind_rev
            DISPLAY "TOTAL DE REGISTROS REVERSADOS: ",v_tot_rev
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
                          'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reverso de integración de recurrentes',
                          v_mensaje
                          )
END MAIN
