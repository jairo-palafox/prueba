--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR44                                                                 #
#Objetivo     => Programa que ejecuta el rutna de reverso de la generacion de archivo   #
#                de salida de retiros por transferencia para Tesoreria                  #
#Fecha inicio => Marzo 01, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid              LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod        LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_i_folio         LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera         SMALLINT
       ,v_s_sql           STRING -- cadena con un enunciado SQL

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR44.log")

   DISPLAY "Inicia proceso de reverso de generación archivo de salida para Tesorería."

   -- se cambia el estado de las solicitudes a 60 (liquidadas)
   UPDATE ret_transferencia
      SET estado_solicitud = 60 -- liquidadas
    WHERE folio            = p_i_folio
      AND estado_solicitud = 70 -- notificada a tesoreria

   -- se cambia el estado de las solicitudes a 60 (liquidadas)
   UPDATE ret_transferencia
      SET estado_solicitud = 100 -- rechazadas
    WHERE folio            = p_i_folio
      AND estado_solicitud = 101 -- rechazadas notificad a tesoreria 

    LET v_s_sql = "\n UPDATE glo_folio",
                 "\n SET status = 2",         
                 "\n WHERE proceso_cod = ",g_proceso_cod, -- retiros por disposicion de recursos
                 "\n AND status = 3 ", --tesoreria
                 "\n AND folio  = ", p_i_folio
   DISPLAY   v_s_sql               
    PREPARE pr_folio  FROM v_s_sql
    EXECUTE pr_folio 
      
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bandera

   IF ( r_bandera = 0 ) THEN
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
    
END MAIN