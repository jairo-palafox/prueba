 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP359                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para las aclaraciones del fondo de ahorro                              #
#Fecha inicio => Abril 10, 2015                                                         #
#Fecha modificacion =>                                                                  #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod,  -- codigo de operacion
       g_folio       LIKE ret_preliquida.folio_liquida --folio liquidacion
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid,         -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod,   -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod,       -- clave del usuario firmado
       p_folio          LIKE ret_preliquida.folio_liquida,
       v_s_sql          STRING,                             -- cadena con una instruccion SQL
       v_i_resultado    INTEGER                             -- resultado del proceso
       ,r_bnd_fin_oper  SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_doc_cod       VARCHAR(20)
       ,p_titulo        STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje       STRING -- cuerpo del mensaje enviado
       ,p_programa_cod  VARCHAR(10)
       ,v_id_solicitud  DECIMAL(9,0)
       ,v_error_isam    INTEGER
       ,v_mensaje       VARCHAR(250)
       ,v_s_comando     STRING

     ##Ejecuta prevalidación de saldos
   ## se recuperan los parametros la clave de usuario desde parametro 
   ## argumento con indice 1
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 
   
   ## RECIBIR LOS OTROS DOS PARAMETROS
   ## se asigna proceso y operacion
   LET g_pid         = p_pid
   LET g_folio       = p_folio    
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion

   --CALL fn_genera_folio(g_proceso_cod, g_opera_cod,p_usuario_cod) RETURNING g_folio 

   -- Inicia proceso de carga de archivo.
   -- se asume que el proceso termina correctamente
   LET v_i_resultado = 0
   LET v_si_correcto_integra = 0
                                
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_insert_preliquida_aclara_fondo_ahorro(?,?,?,?,?)"
            
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integraret FROM  v_s_sql
   EXECUTE sid_integraret USING g_folio,
                                g_proceso_cod,
                                g_opera_cod,
                                p_usuario_cod,
                                g_pid
                     INTO v_i_resultado, v_error_isam, v_mensaje, v_id_solicitud
                 
   --DISPLAY v_i_resultado
   --Se finaliza aunque existan errores
   IF ( v_i_resultado = 0 ) THEN
      -- Cierra la operación
      DISPLAY "La preliquidacion se terminó completamente."
      DISPLAY "Estatus de preliquidacion:",v_i_resultado
      
       LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : ACLARACIONES DEL FONDO DE AHORRO\n",
                      "Operación    : PRELIQUIDACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n"


      -- si se termino correctamente
      DISPLAY v_mensaje
      DISPLAY "Ya se puede continuar con la Liquidación" , v_error_isam       
      
         DISPLAY "Preliquidacion realizada con exito"
         LET p_mensaje = p_mensaje || "Preliquidacion realizada con éxito\n.Ya se puede continuar con la Liquidación"
         LET p_titulo = "Preliquidación Aclaraciones del Fondo de Ahorro "
         
      -- se obtiene el codigo de programa
      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = g_proceso_cod
      AND    opera_cod   = g_opera_cod
                      
      CALL fn_reporte_liquidacion(g_folio, "ret_preliquida_afa",
                                  p_usuario_cod, g_pid,
                                  g_proceso_cod, g_opera_cod,
                                  p_programa_cod, FALSE)                      

      
      DISPLAY "Ya se puede Continuar con la Liquidación"
      DISPLAY "\n"
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper
   
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación."                                 
      DISPLAY p_mensaje

      DISPLAY "\nError (SQL): ", v_i_resultado
      DISPLAY "Error (ISAM) : ", v_error_isam
      DISPLAY "Mensaje      : ", v_mensaje
      DISPLAY "ID Solicitud : ", v_id_solicitud
       
      DISPLAY "Preliquidación realizada pero con errores de validación"
      LET p_mensaje = p_mensaje || "El proceso de Preliquidacion ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación."
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado
      -- Cancela la operacion para q se pueda iniciar nuevamente
   END IF
     -- END IF

END MAIN



