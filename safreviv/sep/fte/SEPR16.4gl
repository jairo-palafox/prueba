--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 25-07-2012 
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPR43                                                   #
#Objetivo          => Batch reverso de validación diagnóstico operación 27     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Julio, 2012                                           #
#Modificado        =>                                                          #
################################################################################

DATABASE safre_viv

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
DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_opera_desc   LIKE cat_operacion.opera_desc,
       v_consulta     STRING,
       v_ind          SMALLINT,
       v_diag         CHAR(3),
       v_sql_error    INTEGER,
       v_isam_error   SMALLINT,
       v_msg_error    CHAR(100),
       v_tot_02       INTEGER,
       v_tot_03       INTEGER,
       v_tot_05       INTEGER,
       v_tot_06       INTEGER,
       v_mensaje      STRING,
       v_msg_opera    VARCHAR(100),
       r_res_opera    SMALLINT

   # Se recuperan los parámetros
   LET p_usuario_cod     = ARG_VAL(1)
   LET p_pid_rev         = ARG_VAL(2)
   LET p_proceso_cod_rev = ARG_VAL(3)
   LET p_opera_cod_rev   = ARG_VAL(4)
   LET p_folio           = ARG_VAL(5)
   LET p_nom_archivo     = ARG_VAL(6)


   # Recuepra descripcion de proceso y operacion de reverso
   CALL fn_proceso_cod_desc(p_proceso_cod_rev) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(p_proceso_cod_rev, p_opera_cod_rev) RETURNING v_opera_desc
   DISPLAY "PROCESO        :",v_proceso_desc
   DISPLAY "OPERACIÓN      :",v_opera_desc
   --DISPLAY "FOLIO          :",p_folio USING "############"
   DISPLAY "\n"

   LET v_proceso_cod = 2204 # diagnóstico operacion 27
   LET v_opera_cod   = 1    # validacion diagnóstico operacion 27

   # recupera el pid que le corresponde al proceso y folio
   SELECT MAX(pid)
     INTO v_pid
     FROM bat_ctr_operacion
    WHERE proceso_cod = v_proceso_cod
      AND opera_cod = v_opera_cod
      AND nom_archivo = p_nom_archivo

   DISPLAY "PID A REVERSAR     :",v_pid
   DISPLAY "ARCHIVO A REVERSAR :",p_nom_archivo
   DISPLAY "\n"

   # Elimina el registro del archivo
   DELETE 
     FROM glo_ctr_archivo
    WHERE nombre_archivo = p_nom_archivo
      AND proceso_cod = v_proceso_cod

   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "Ocurrió un error al eliminar archivo registrado, código: ",SQLCA.SQLCODE
      DISPLAY "Mensjae: ",SQLCA.sqlerrm
      CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      IF(r_res_opera <> 0)THEN
         # Imprime el mensaje de inconsistencia en consola
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF
      EXIT PROGRAM
   END IF

   # Reversa diagnóstico operacion 27
   CALL fn_reversa_operacion(v_pid,v_proceso_cod,v_opera_cod) RETURNING r_res_opera
   IF(r_res_opera <> 0)THEN
      SELECT descripcion
        INTO v_msg_opera
        FROM cat_bat_parametro_salida
       WHERE cod_salida = r_res_opera
      LET v_mensaje = "Ocurrió un error al realizar el reverso: ",v_msg_opera
      # Imprime el mensaje de inconsistencia en consola
      CALL fn_desplega_inc_operacion(r_res_opera)
      # establece en erronea la operacion de reverso
      CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      IF(r_res_opera <> 0)THEN
         # Imprime el mensaje de inconsistencia en consola
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF
   ELSE
      # finaliza operacion de reverso
      CALL fn_actualiza_opera_fin(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      IF(r_res_opera  <> 0)THEN
         SELECT descripcion
           INTO v_msg_opera
           FROM cat_bat_parametro_salida
          WHERE cod_salida = r_res_opera
         LET v_mensaje = "Ocurrió un error al finalizar el reverso: ",v_msg_opera
         # Imprime el mensaje de inconsistencia en consola
         CALL fn_desplega_inc_operacion(r_res_opera)
         # trata de establecer erronea la operacion
         CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
         IF(r_res_opera  <> 0)THEN
            # Imprime el mensaje de inconsistencia en consola
            CALL fn_desplega_inc_operacion(r_res_opera)
         END IF
      ELSE
         # mensaje para el correo, en el flujo principal sin errores
         LET v_mensaje = "Operación finalzada correctamente"
      END IF
   END IF
   DISPLAY v_mensaje||"\n"
   CALL fn_correo_proceso(p_pid_rev, 
                          p_proceso_cod_rev, 
                          p_opera_cod_rev, 
                          '', # Archivo adjunto
                          'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reverso de validación Op 29',
                          v_mensaje
                          )

END MAIN