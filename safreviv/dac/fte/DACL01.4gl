--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/03/2014
--===============================================================

################################################################################
#Modulo       => DAC                                                           #
#Programa     => DPEL01                                                        #
#Objetivo     => Lanzador Integración Devolución de Amortización Mejora tu Casa#
#Fecha inicio => 03/03/2014                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       v_rest_valida  SMALLINT,
       r_bnd_incons   SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2601
   LET g_opera_cod   = 1 -- Carga de archivo

   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING v_rest_valida
   
   IF ( v_rest_valida = 0 ) THEN
      IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"DACL01","",p_usuario_cod, TRUE) = TRUE)THEN
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_rest_valida)
      RETURNING r_bnd_incons
   END IF

END MAIN