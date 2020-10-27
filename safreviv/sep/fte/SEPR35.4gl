--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20-07-2012 
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPR36                                                   #
#Objetivo          => Batch reverso de integracion Operacion 28                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Julio, 2012                                           #
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
DEFINE v_proceso_desc   LIKE cat_proceso.descripcion,
       v_opera_desc     LIKE cat_operacion.descripcion,
       v_consulta       STRING,
       v_ind            SMALLINT,
       v_diag           CHAR(3),
       v_sql_error      INTEGER,
       v_isam_error     SMALLINT,
       v_msg_error      CHAR(100),
       v_tot_reversados INTEGER,
       v_mensaje        STRING,
       r_res_opera      SMALLINT

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
   DISPLAY "PROCESO      :",v_proceso_desc
   DISPLAY "OPERACIÓN    :",v_opera_desc
   DISPLAY "FOLIO        :",p_folio
   DISPLAY "\n"

   -- proceso y operaciones iniciales
   LET v_proceso_cod = 2202 # operacion 28
   LET v_opera_cod   = 1    # carga de archivo de operacion 28

   -- Restaura datos afectados por carga de archivo
   CALL fn_corrige_reg_carga_archivo(p_nom_archivo, v_proceso_cod, v_opera_cod)
   DISPLAY "El reverso se realizó con éxito"

   -- se obtiene el pid de la operacion
   SELECT max(pid)
   INTO   v_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = v_proceso_cod

   -- Reversa operación
   LET r_res_opera = 0
   CALL fn_reversa_operacion(v_pid,v_proceso_cod,v_opera_cod)
                             RETURNING r_res_opera
         
   IF ( r_res_opera = 0 ) THEN
      -- se indica que el reverso se realizo con exito
      DISPLAY "Operación lista para volver a generarse."
      -- se finaliza el proceso de reverso
      CALL fn_actualiza_opera_fin(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      
      IF ( r_res_opera <> 0 ) THEN
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF

   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_res_opera)
      
      -- se marca la operacion en error
      CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev)
                          RETURNING r_res_opera
                          
      IF ( r_res_opera <> 0 ) THEN
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF
   END IF
   
   CALL fn_correo_proceso(p_pid_rev, 
                          p_proceso_cod_rev, 
                          p_opera_cod_rev, 
                          '', # Archivo adjunto
                          'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reverso de carga Op 29',
                          v_mensaje
                          )

END MAIN

{
   Funcion : fn_corrige_reg_carga_archivo
   Descrip : borra el registro del archivo cargado
}
FUNCTION fn_corrige_reg_carga_archivo(p_nombre_archivo, p_proceso_cod, p_opera_cod)
  DEFINE 
   p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   ,p_proceso_cod    LIKE cat_operacion.proceso_cod
   ,p_opera_cod      LIKE cat_operacion.opera_cod
   ,v_s_qry          STRING

   LET v_s_qry = "DELETE FROM glo_ctr_archivo"
                 ,"\n WHERE nombre_archivo = ?"
                 ,"\n   AND proceso_cod = ", v_proceso_cod
                 ,"\n   AND estado = 1"
   DISPLAY v_s_qry 
   PREPARE Prpr_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_LimpiaCtrArchvo USING  p_nombre_archivo

END FUNCTION -- fn_corrige_reg_carga_archivo
