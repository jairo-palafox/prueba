--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETE03                                                                 #
#Objetivo     => Invoca la carga de datos para retiros tipo N                           #
#Fecha inicio => Marzo 08, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       r_bnd_fin_oper   SMALLINT,
       v_rest_valida    SMALLINT,
       v_mensaje        STRING

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
   LET g_proceso_cod = g_proceso_cod_ret_tipo_N -- Retiro tipo N
   LET g_opera_cod   = g_opera_cod_ret_tipoN_carga -- recepcion de archivo
   
   
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
   IF ( v_rest_valida = 0 ) THEN
         
         ## > -- se obtiene el ID del proceso
         --CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
         --     RETURNING g_pid

         
         ## > Inicializa proceso porque es la primera operacion
         --CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"RETE03","",
         --                     p_usuario_cod) RETURNING v_rest_valida 
         --IF ( v_rest_valida = 0)THEN
            -- Inicia proceso de carga de archivo y se solicita que genere el arbol de proceso
            CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod, 2, "RETE03",
                                 "NA",p_usuario_cod, TRUE) RETURNING v_rest_valida               
         --END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_mensaje

      CALL fn_mensaje("Atención", v_mensaje, "stop")
      MENU
         COMMAND "Salir"
            EXIT MENU
      END MENU
   END IF

END MAIN
