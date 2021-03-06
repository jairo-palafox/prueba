#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDL33                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                del ajuste                                                             #
#Fecha inicio => 22/10/2014                                                             #
#########################################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_operacion    SMALLINT -- 1 CONSULTA; 2 LIQUIDA
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

   -- se asigna proceso y operacion
   LET g_proceso_cod = 2111 -- REVERSO OPERATIVO
   LET g_opera_cod   = 4 -- LIQUIDACION
   LET g_operacion   = 2 -- liquidar

   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, g_operacion)

END MAIN
