--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================

################################################################################
#Modulo            =>                                                          #
#Programa          => RETL458                                                  #
#Objetivo          => Programa lanzador para le liquidacion de la restitución  #
#                     de las excepciones de la devolución del SSV              #
#Autor             =>                                                          #
#Fecha inicio      => 13 Septiembre 2017                                       #
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
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_id_derechohabiente DECIMAL(9,0),
       v_id_solicitud       DECIMAL(9,0),
       v_query              STRING,
       v_marca_excep_ssv    SMALLINT,
       v_estado_marca       SMALLINT,
       v_marca_causa        SMALLINT,
       v_i_estado_marca     SMALLINT


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
   
   LET g_proceso_cod = g_proceso_excep_devol_ssv_restitucion  # Restitucion excepciones de la devolución del SSV
   LET g_opera_cod   = g_opera_restitucion_excep_devol_ssv_liquida # Liquidación
   LET p_operacion   = 2 #  2 --> ejecutar liquidacion   1--> ejecutar consulta
   LET v_estado_marca = 0
   LET v_marca_causa = 0
   LET v_marca_excep_ssv = 820

   # se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)

   --- Se desmarcan las cuentas
   
   LET v_query = " SELECT a.id_derechohabiente, b.id_solicitud ",
                 " FROM   afi_derechohabiente a, ret_excep_devol_ssv b ",
                 " WHERE  a.nss = b.nss ",
                 " AND    b.estado_solicitud = 209 "

   PREPARE prp_solicitudes FROM v_query
   DECLARE cur_solicitudes CURSOR FOR prp_solicitudes

   FOREACH cur_solicitudes INTO v_id_derechohabiente, v_id_solicitud

      LET v_query = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
      PREPARE prp_desmarca FROM v_query
      EXECUTE prp_desmarca USING 
                    v_id_derechohabiente
                   ,v_marca_excep_ssv
                   ,v_id_solicitud
                   ,v_estado_marca
                   ,v_marca_causa
                   ,p_usuario_cod
                   ,g_proceso_cod
               INTO v_i_estado_marca;
               
      -- Actualiza las solicitudes liquidadas

      UPDATE ret_excep_devol_ssv
      SET    estado_solicitud = 210   -- Saldo Restituido
      WHERE  id_solicitud     = v_id_solicitud
      AND    estado_solicitud = 209;
      
   END FOREACH   


   
END MAIN
