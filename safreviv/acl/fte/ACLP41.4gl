--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>ACL                                                                     #
#PROGRAMA          =>ACLP41                                                                  #
#OBJETIVO          =>Programa que busca las aclaraciones que no tienen causal con respecto   #
#                    al archivo de actualizacion cargado por el instituto                    #
#                                                                                            #
##############################################################################################

DATABASE safre_viv
GLOBALS "ACLG02.4gl"
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
DEFINE p_usuario_cod                 LIKE seg_usuario.usuario_cod, -- nombre del usuario
       p_folio                       LIKE glo_folio.folio, -- numero de folio
       v_bandera                     SMALLINT,
       v_conteo                      INTEGER, -- contador de registros
       p_titulo                      STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                     STRING, -- cuerpo del mensaje enviado       
       v_sql                         STRING, -- cadena con enunciado SQL
       v_contador                    SMALLINT, -- contador de registros
       v_ws_status                   SMALLINT, -- estatus de ejecucion de un webservice
       v_cadena                      STRING, -- cadena auxiliar
       v_estatus_fico                SMALLINT, -- estatus de pago en fico en formato numerico
       v_cambio_cuenta               SMALLINT, -- booleana para ver si hubo cambio dee stado de la cuenta
       v_proceso_cod                 LIKE cat_proceso.proceso_cod, -- codigo de proceso
	   v_resultado                   INTEGER, -- resultado de ejecucion del SP
	   v_error_isam                  INTEGER,
	   v_mensaje_error               VARCHAR(255),
	   v_nss_error                   CHAR(11)
       
   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".ACLP41.log")
          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "BUSQUEDA")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando rutina de busqueda de aclaraciones sin causal"

   -- 16Dic2013. Se verifica si hay datos para consulta de pago FICO
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   safre_tmp:tmp_aclaraciones_sin_causal
   
   -- si no hay registros 
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Actualización de aclaraciones sin causal"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : ACTUALIZACIÓN DE ACLARACIONES SIN CAUSAL\n",
                      "Operación    : BUSQUEDA\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen registros para actualizar.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)

                       
   ELSE -- se ejcuta el SP que busca las aclaraciones
   
      CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
           RETURNING p_folio
   
	  LET v_cadena = "EXECUTE FUNCTION fn_acl_busca_aclara_sin_causal(?,?,?)"
	  
	  PREPARE sid_actualiza FROM v_cadena
	  EXECUTE sid_actualiza USING p_folio, g_pid, g_proceso_cod
	  INTO    v_resultado, v_error_isam, v_mensaje_error, v_nss_error
	  
	  DISPLAY v_cadena
	  
	  -- si el SP se ejecuto correctamente
	  IF ( v_resultado = 0 ) THEN
	     DISPLAY "Proceso finalizado correctamente."

         -- se finaliza la operacion
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING v_bandera
	  ELSE
	     DISPLAY "Ocurrión un error al ejecutar el proceso: "
		 DISPLAY "Error (SQL)    : ", v_resultado
		 DISPLAY "Error(ISAM)    : ", v_error_isam
		 DISPLAY "Error (Mensaje): ", v_mensaje_error
		 DISPLAY "Error (NSS)    : ", v_nss_error
	  
         -- se finaliza la operacion en error
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING v_bandera
	  END IF
         
      -- se complementa el mensaje
      LET p_mensaje = "ACTUALIZACIÓN ACLARACIONES SIN CAUSAL."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - ACTUALIZACIÓN ACLARACIONES SIN CAUSAL"
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "BUSQUEDA")
END MAIN