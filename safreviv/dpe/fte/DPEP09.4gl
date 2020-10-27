--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/03/2013
--===============================================================

################################################################################
#Modulo      => DPE                                                            #
#Programa    => DPEP09                                                         #
#Objetivo    => Programa lanzado que hace la validación de la carga del archivo#
#               para verificar si se ejecuto el shell que agrega el FOLIO SUA  #
#Fecha inicio=> 04/Marzo/2013                                                  #
################################################################################

GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid,          -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod,  -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod,    -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod,        -- clave del usuario firmado
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       p_titulo         STRING,                              -- titulo del mensaje enviado en el correo
       p_mensaje        STRING,                              -- cuerpo del mensaje enviado
       v_layout         LIKE cat_operacion.layout_cod,
       v_ruta_rescate   STRING,
       v_usuario        LIKE seg_modulo.usuario,
       v_proceso_desc   LIKE cat_proceso.proceso_desc,
       v_extension      LIKE cat_operacion.extension,
       v_opera_desc     LIKE cat_operacion.opera_desc,
       v_ruta_listados  LIKE seg_modulo.ruta_listados,
       v_total_folios   INTEGER,
       r_bnd_fin_oper   SMALLINT,
       v_tiempo         CHAR(23),
       v_ruta_listado_nohup LIKE seg_modulo.ruta_listados,
       v_ind_tipo_ejecucion LIKE bat_ctr_operacion.ind_tipo_ejecucion,
       v_archivo_nohup      STRING,
       v_archivo_errnohup   STRING,
       v_comando            STRING,
       v_nom_archivo        CHAR(40)
       
   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario

   -- se asigna proceso y operacion
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
   
   --AQUI VA LA VALIDACION 
   SELECT COUNT(folio_sua_det)
   INTO   v_total_folios
   FROM   safre_tmp:tmp_det_devolucion_dpe
   WHERE  folio_sua_det IS NOT NULL 
   AND    folio_sua_det <> 0
  
   -- si no ocurrio error al integrar
   IF ( v_total_folios > 0 ) THEN
      DISPLAY "   La carga del archivo se terminó completamente."
      DISPLAY " "
      DISPLAY "   Carga realizada con exito"
      DISPLAY " "
      DISPLAY "   Total registros :", v_total_folios
      
      LET p_mensaje = "   La carga se terminó completamente.","\n",
                      "   ","\n",
                      "   Carga realizada con exito","\n",
                      "   ","\n",
                      "   Total registros :", v_total_folios

   ELSE
      DISPLAY " "
      DISPLAY "  No se puede continuar con la Integración"
      DISPLAY " "
      DISPLAY "  No se ha ejecutado el archivo de las referencias "
      DISPLAY " "

      LET v_tiempo = CURRENT YEAR TO SECOND;
   
      # actualiza los estado del proceso y operacion de manera manual, ya que
      # no se puede utilizar fn_error_opera cuando se ha finalizado la operacion
       UPDATE bat_ctr_operacion
       SET    fecha_fin   = v_tiempo,
              estado_cod  = 3
       WHERE  pid         = g_pid
       AND    proceso_cod = g_proceso_cod
       AND    opera_cod   = g_opera_cod;

       UPDATE bat_ctr_proceso
       SET    fecha_fin   = v_tiempo,
              estado_cod  = 3
       WHERE  pid         = g_pid
       AND    proceso_cod = g_proceso_cod;

       SELECT ruta_listados
       INTO   v_ruta_listado_nohup
       FROM   seg_modulo
       WHERE  modulo_cod = 'bat'

      # consulta para determinar el tipo de ejecucion, 0 = manual  1 = batch
      SELECT ind_tipo_ejecucion
      INTO   v_ind_tipo_ejecucion
      FROM   bat_ctr_operacion
      WHERE  pid = g_pid
      AND    proceso_cod = g_proceso_cod
      AND    opera_cod = g_opera_cod

      SELECT nom_archivo
      INTO   v_nom_archivo
      FROM   bat_ctr_operacion
      WHERE  pid = g_pid
      AND    proceso_cod = g_proceso_cod
      AND    opera_cod = g_opera_cod
         
      IF(v_ind_tipo_ejecucion = 0)THEN # para el caso de ejecucion batch, solo se imprime la leyenda para que realice la tarea por si sola
         
         # se cambia el log de la operacion, para que la funcion de correo lo tome correctamente
         # y no provoque error
         LET v_archivo_nohup = v_ruta_listado_nohup CLIPPED, "/finnohup:",
                                g_pid         USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod   USING "&&&&&"

         LET v_archivo_errnohup = v_ruta_listado_nohup CLIPPED, "/errnohup:",
                                  g_pid         USING "&&&&&",":",
                                  g_proceso_cod USING "&&&&&",":",
                                  g_opera_cod   USING "&&&&&" 

         LET v_comando = "mv "||v_archivo_nohup||" "||v_archivo_errnohup
         RUN v_comando
      ELSE
         DISPLAY "Program stopped"
      END IF

      DELETE FROM glo_ctr_archivo
      WHERE nombre_archivo = v_nom_archivo
      AND   proceso_cod    = g_proceso_cod
      AND   opera_cod      = g_opera_cod

   END IF   
END MAIN
