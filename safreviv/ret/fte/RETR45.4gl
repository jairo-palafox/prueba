--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR45                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de retiros por transferencia #
#                por causa de inconsistencia entre montos notificados por               #
#                correo vs montos encontrados en archivo de carga                       #
#Fecha inicio => Febrero 28, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_d_folio         LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera         SMALLINT
       ,v_sql             STRING -- cadena con una instruccion SQL

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR45.log")

   -- se inicia el reverso de los registros integrados
   LET g_proceso_cod = g_proceso_cod_ret_transferencia -- retiros por transferencia
   LET g_opera_cod   = g_opera_cod_ret_transf_integracion  -- integracion

   DISPLAY "Se inicia el reverso del proceso de Retiros por Transferencia por inconsistencia de\n",
           "montos encontrados en archivo vs montos notificados por correo electrónico.\n"
   
   DISPLAY "Aplicando reverso a registros integrados..."


   WHENEVER ERROR CONTINUE
   -- se ejecuta el SP que realiza el reverso de la integracion y carga por causa
   -- de inconsistencia entre montos notificados contra montos encontrados en archivo
   LET v_sql = "EXECUTE FUNCTION fn_ret_transferencia_reverso_inconsistencia(?, ?, ?)"
   
   PREPARE sid_spreversoinconsistencia FROM v_sql
   EXECUTE sid_spreversoinconsistencia INTO r_bandera USING p_usuario_cod, p_d_folio, g_pid

   WHENEVER ERROR STOP
   -- si ejecuto correctamente se marcan como erroneos los procesos
   IF ( r_bandera = 0 ) THEN
      DISPLAY "El proceso de reverso ha finalizado correctamente."


      DISPLAY "Marcando error en proceso"
      
      -- se marca como erronea la integracion y la carga
      UPDATE bat_ctr_operacion
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

      -- se marca como erroneo el proceso
      UPDATE bat_ctr_proceso
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

   ELSE
      DISPLAY "El proceso de reverso terminó pero con errores."

      -- se marca como erronea la integracion y la carga
      UPDATE bat_ctr_operacion
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

      -- se marca como erroneo el proceso
      UPDATE bat_ctr_proceso
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

   END IF
   
END MAIN
