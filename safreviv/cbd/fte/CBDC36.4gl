###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CBD                                                     #
#Programa          => CBDC36                                                  #
#Objetivo          => CONSULTA DE PRELIQUIDACION DE AJUSTE SALDO              #
#Fecha Inicio      => 24/09/2015                                              #
###############################################################################
DATABASE safre_viv


GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se asignan los valores a las variables de control
   LET g_proceso_cod = 2116 -- Liquidacion de pagos
   LET g_opera_cod   = 3 -- PRELIQUIDACION AJUSTE OPERATIVO

   CALL fn_consulta_preliq(p_usuario_cod, g_proceso_cod, g_opera_cod)
END MAIN