--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08/Nov/2016
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEL11                                                        #
#Objetivo     => Invoca la carga del Archivo que se recibe de PROCESAR         #
#Fecha inicio => Noviembre 11, 2016                                            #
################################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT,
       p_s_titulo       STRING,
       r_bnd_fin_oper   SMALLINT,
       v_rest_valida    SMALLINT,
       v_mensaje        STRING,
       v_folio          DECIMAL(9,0) 
       
   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET g_proceso_cod = 1001
   LET g_opera_cod   = 3

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
        RETURNING v_rest_valida

   IF ( v_rest_valida = 0 ) THEN                         
      IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"DPEL11",
                              "",p_usuario_cod, FALSE) = TRUE)THEN
         SELECT folio
         INTO   v_folio 
         FROM   bat_ctr_operacion
         WHERE  pid = g_pid
         AND    opera_cod = 2

         UPDATE bat_ctr_operacion
         SET    folio = v_folio
         WHERE  pid = g_pid
         AND    opera_cod = g_opera_cod
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_mensaje
      CALL fn_mensaje("Atención",v_mensaje,"stop")
  END IF 
END MAIN
