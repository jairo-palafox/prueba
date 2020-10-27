--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15-10-2013
--==============================================================================

################################################################################
#Modulo            =>                                                          #
#Programa          => RETL292                                                  #
#Objetivo          => Programa lanzador para le preliquidacion de restitución  #
#                     de retiros genéricos fondo ahorro 72                     #
#Autor             =>                                                          #
#Fecha inicio      => 15 Octubre 2013                                          #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

DEFINE g_pid          LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, # codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod  # codigo de operacion


MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # clave usuario firmado
       p_tipo_ejecucion  SMALLINT, # forma como ejecutara el programa
       p_titulo_vtna     STRING,   # titulo de la ventana
       p_operacion       SMALLINT,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   # Obtiene las rutas de modulo a ejecutar
   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo 
    WHERE modulo_cod = 'ret'
   
   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo_vtna IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF
   
   LET g_proceso_cod = g_proceso_cod_restitucion_ret_generico_72        # Restitucion retiro genérico fondo ahorro 72
   LET g_opera_cod   = g_opera_cod_restitucion_ret_generico_liquidacion # Liquidación
   LET p_operacion   = 2 #  2 --> ejecutar liquidacion   1--> ejecutar consulta

   # se invoca la funcion para enviar la liquidacion
   CALL fn_liquida_fondo72(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)
   
END MAIN
