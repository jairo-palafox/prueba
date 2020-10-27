--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP450                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                de las excepciones de la devolución del SSV                            #
#Fecha inicio => Marzo 28, 2018                                                         #
#                                                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       v_s_sql                STRING, -- cadena con una instruccion SQL
       v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper        SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,v_s_comando           STRING -- PARA EJECUTAR ARCHIVO DE RECHAZADOS
       ,p_programa_cod        VARCHAR(10)
       ,v_error_isam          INTEGER
       ,v_mensaje             VARCHAR(250)
       ,p_marca               SMALLINT 
       ,p_movimiento          SMALLINT -- movimiento de retiro por disposicion de recursos
       ,p_movimiento_sobregiro SMALLINT -- movimiento de sobregiro del retiro por disposicion de recursos
       ,p_max_aivs_sobregiro   DECIMAL(20,6) -- num maximo de aivs de diferencia entre saldo y monto solicitado para sobregiro
       ,v_id_solicitud_error    DECIMAL(9,0)

   
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod          = ARG_VAL(1)
   LET p_pid                  = ARG_VAL(2)
   LET p_proceso_cod          = ARG_VAL(3)
   LET p_opera_cod            = ARG_VAL(4)
   LET p_folio                = ARG_VAL(5)
   LET p_nombre_archivo       = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- retiros por disposicion de recursos
   LET g_opera_cod   = p_opera_cod -- preliquidacion
           
   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "PRELIQUIDACION")

   DISPLAY "Realizando preliquidación de las excepciones de la devoluciones del SSV"
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION sp_ret_preliquida_excep_devol_ssv(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_folio, p_proceso_cod, p_opera_cod, p_usuario_cod, g_pid
      INTO v_i_resultado, v_error_isam, v_mensaje, v_id_solicitud_error

   DISPLAY "Estatus de preliquidación:",v_i_resultado

   
   -- si se termino correctamente 
   IF ( v_i_resultado = 0 )THEN
      DISPLAY "Preliquidación realizada con éxito"
      DISPLAY "Ya se puede continuar con la Liquidación."
      -- se obtiene el codigo de programa
      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = p_proceso_cod
      AND    opera_cod   = p_opera_cod

      CALL fn_reporte_liquidacion(p_folio, "ret_preliquida",
                                  p_usuario_cod, p_pid,
                                  p_proceso_cod, p_opera_cod,
                                  p_programa_cod, FALSE)
      
      -- se complementa el mensaje
      LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING r_bnd_fin_oper
   ELSE
      -- se despliega el error
      DISPLAY "Error SQL : ", v_i_resultado
      DISPLAY "Error ISAM: ", v_error_isam
      DISPLAY "Mensaje   : ", v_mensaje
      DISPLAY "ID_Solicitud ERROR: ", v_id_solicitud_error
      -- se indica en el mensaje que la preliquidacion no termino correctamente
      LET p_mensaje = "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación. ", v_mensaje
   
      DISPLAY "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación."
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado
   END IF

   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - EXCEPCIONES DE LA DEVOLUCIÓN DEL SSV - PRELIQUIDACION"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "PRELIQUIDACIÓN")


END MAIN
