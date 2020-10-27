--=============================================================================
-- Version: 1.0.0
-- Fecha �ltima modificaci�n:
--=============================================================================
#########################################################################################
#M�dulo       => AFI                                                                    #
#Programa     => AFIE02                                                                 #
#Objetivo     => Invoca la carga de datos para movimientos afiliatorios SOLO INFONAVIT  #
#Fecha inicio => 22 de febrero de 2012                                                  #
#########################################################################################

DATABASE safre_viv

GLOBALS "AFIG01.4gl"

GLOBALS

   DEFINE g_pid         LIKE bat_ctr_proceso.pid     --  ID del proceso
   DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion

END GLOBALS

MAIN

   DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_tipo_ejecucion SMALLINT  -- forma como ejecutara el programa
   DEFINE p_s_titulo       STRING    -- titulo de la ventana
   DEFINE r_bnd_fin_oper   SMALLINT
   DEFINE v_rest_valida    SMALLINT
   DEFINE v_mensaje        STRING

   -- se recupera la clave de usuario desde par�metro
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   -- si se obtuvo el t�tulo, se pone como t�tulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se asigna proceso y operaci�n
   LET g_proceso_cod = g_proceso_cod_afi_movimientos_sinf -- movimientos afiliatorios
   LET g_opera_cod   = g_opera_cod_afi_movimientos_carga_sinf -- recepcion de archivo

   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
   IF ( v_rest_valida = 0 ) THEN
      -- Inicia proceso de carga de archivo mismo que inicia la cadena de proceso
      CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod, 2, "AFIE02",
                           "NA",p_usuario_cod, TRUE) RETURNING v_rest_valida
   ELSE
      CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_mensaje

      CALL fn_mensaje("Atenci�n", v_mensaje, "stop")
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
   END IF

END MAIN
