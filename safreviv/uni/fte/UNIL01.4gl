################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIL01                                                        #
#Objetivo     => Lanzador para la validación del archivo de solicitudes de     #
#                unificación de cuentas IMSS.                                  #
#Fecha inicio => 21/05/2012                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
-- 19/11/2014 Se adecúa para ejecución de unificación manual 
--==============================================================================
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       --
       r_bnd_fin_oper   SMALLINT,
       v_rest_valida    SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2309 --  Solicitud de Unificación IMSS 
   LET g_opera_cod   = 1    -- Carga de archivo
   
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING v_rest_valida
   
   IF ( v_rest_valida = 0 ) THEN
      IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"UNIL01","",
          p_usuario_cod, TRUE) = TRUE)THEN
      END IF
   ELSE
      CALL fn_mensaje("Atención", fn_recupera_inconsis_opera(r_bnd_fin_oper), "stop")
   END IF

END MAIN
