--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================

################################################################################
#Modulo            =>                                                          #
#Programa          => RETL394                                                  #
#Objetivo          => Programa lanzador para la liquidación de restitución     #
#                     de retiros genéricos Ley 73 rechazados por SIAFF         #
#Autor             =>                                                          #
#Fecha inicio      => Septiembre 2015                                          #
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
DEFINE v_rest_valida    SMALLINT,
       v_mensaje        STRING

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
   
   LET g_proceso_cod = g_proceso_restitucion_rechazo_siaff  # Restitucion retiro genérico amortización excedentes
   LET g_opera_cod   = g_opera_cod_liquida_restitucion_rechazo_siaff # Liquidación
   LET p_operacion   = 2 #  2 --> ejecutar liquidacion   1--> ejecutar consulta

   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   DISPLAY g_pid

   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
   IF ( v_rest_valida = 0 ) THEN
         
      # se invoca la funcion para enviar la liquidacion
      CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)
      -- Actualiza el estado de las solicitudes a Liquidado (210)
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = 210   -- Liquidado
      WHERE  id_solicitud IN (SELECT id_referencia
                              FROM   cta_movimiento
                              WHERE  folio_liquida IN (SELECT DISTINCT folio
                                                       FROM   bat_ctr_operacion
                                                       WHERE  pid = g_pid))
      AND    estado_solicitud = 209   -- Preliquidado
      AND    cod_rechazo = 66

      UPDATE ret_ley73_generico
      SET    estado_solicitud = 210   -- Liquidado
      WHERE  id_solicitud IN (SELECT id_referencia
                              FROM   cta_movimiento
                              WHERE  folio_liquida IN (SELECT DISTINCT folio
                                                       FROM   bat_ctr_operacion
                                                       WHERE  pid = g_pid))
      AND    estado_solicitud = 209   -- Preliquidado
      AND    cod_rechazo = 66

      UPDATE ret_beneficiario_juridico
      SET    estado_solicitud = 210   -- Liquidado
      WHERE  id_solicitud IN (SELECT id_referencia
                              FROM   cta_movimiento
                              WHERE  folio_liquida IN (SELECT DISTINCT folio
                                                       FROM   bat_ctr_operacion
                                                       WHERE  pid = g_pid))
      AND    estado_solicitud = 209   -- Preliquidado
      AND    cod_rechazo = 66
   ELSE
      CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_mensaje

      CALL fn_mensaje("Atención", v_mensaje, "stop")
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
   END IF
   
END MAIN
