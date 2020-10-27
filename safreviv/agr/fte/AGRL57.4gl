--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
#######################################################################################
#M�dulo       => AGR                                                                  #
#Programa     => AGRLL57                                                              #
#Objetivo     => Invoca y valida la carga masiva para conuslta de saldos y marcas     # 
#Autor        => Jos� Eduardo Ventura                                                 #
#Fecha inicio => 08 de junio de 2016                                                  #
#######################################################################################

DATABASE safre_viv


GLOBALS

   DEFINE g_pid                     LIKE bat_ctr_proceso.pid     -- ID del proceso
   DEFINE g_proceso_cod             LIKE cat_proceso.proceso_cod -- c�digo del proceso
   DEFINE g_opera_cod               LIKE cat_operacion.opera_cod -- c�digo de operaci�n

   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_tipo_ejecucion          SMALLINT                     -- forma como ejecutar� el programa
   DEFINE p_s_titulo                STRING                       -- t�tulo de la ventana
   DEFINE r_bnd_fin_oper            SMALLINT
   DEFINE v_rest_valida             SMALLINT
   DEFINE v_mensaje                 STRING

END GLOBALS

MAIN

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

   CLOSE WINDOW SCREEN

   -- se asigna proceso y operacion
   LET g_proceso_cod = 337
   LET g_opera_cod   = 1

   -- Valida operaci�n para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida

   IF ( v_rest_valida = 0 ) THEN
      -- Inicia proceso de carga de archivo mismo que inicia la cadena de proceso
      CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod, 2, "AGRL57",
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