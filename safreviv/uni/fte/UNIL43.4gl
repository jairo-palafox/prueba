--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14/11/2013
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIL43                                                        #
#Objetivo     => Lanzador Validación Unificación Complementaria Recurrente     # 
#Fecha inicio => 06/11/2013                                                    #
################################################################################

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       r_bnd_opera_ini  SMALLINT,
       r_bnd_valida_op  SMALLINT,
       v_descripcion    CHAR(150)

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET r_bnd_opera_ini = 0
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2314 -- Unificación de cuentas recurrente
   LET g_opera_cod   = 1 -- Carga de archivo
   
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bnd_valida_op
   
   IF ( r_bnd_valida_op = 0 ) THEN
      IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"UNIL43","",p_usuario, TRUE) = TRUE)THEN
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_valida_op)
      RETURNING v_descripcion 
      --Muestra el mensaje encontrado
      CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")
   END IF

END MAIN