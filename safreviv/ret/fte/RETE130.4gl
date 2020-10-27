--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07 de Septiembre de 2015
-- Modificación: Se valida que el campo de proceso cargado tenga cualquiera de
--               los siguientes valores: 0101, 0201, 0114,0102, 0103, 0124, 
--               0401, 0402, 0403, 0404, 0411, 0412, 0413, 0414
--=============================================================================
################################################################################
#Modulo       => RET                                                           #
#Programa     => RETE130                                                       #
#Objetivo     => Invoca la carga de datos para retiros de Ley 73               #
#Fecha inicio => Febrero 22, 2012                                              #
################################################################################
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
   LET g_proceso_cod = g_proceso_cod_ret_Ley73_arch -- Retiro por fondo ahorro 
   LET g_opera_cod   = g_opera_cod_ret_ley73_carga -- recepcion de archivo
   
   
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
   IF ( v_rest_valida = 0 ) THEN         
      -- Inicia proceso de carga de archivo mismo que inicia la cadena de proceso
      CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod, 2, "RETE130",
                            "NA",p_usuario_cod, TRUE) RETURNING v_rest_valida      
                                
   ELSE
      CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_mensaje

      CALL fn_mensaje("Atención", v_mensaje, "stop")
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
   END IF

END MAIN