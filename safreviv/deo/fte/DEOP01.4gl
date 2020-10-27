--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOP01                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                para la devolucion por errores de operacion                            #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                   LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod           LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod             LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod           LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_folio                 LIKE deo_preliquida.folio_liquida,
       v_s_sql                 STRING, -- cadena con una instruccion SQL
       p_nombre_archivo        LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_i_resultado           INTEGER -- resultado del proceso
       ,r_bnd_fin_oper         SMALLINT
       ,v_si_correcto_integra  SMALLINT
       ,p_titulo               STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje              STRING -- cuerpo del mensaje enviado
       ,v_si_error_isam        SMALLINT -- error ISAM
       ,v_mensaje_error        VARCHAR(255) -- mensaje devuelto por el SP
       ,v_afores_integradas    SMALLINT -- numero de afores integradas
       ,v_afores_rechazadas    SMALLINT -- numero de afores rechazadas

   
   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se asigna proceso y operacion
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion

   CALL STARTLOG(p_usuario_cod CLIPPED||".DEOP01.log")
   
   -- se solicita el numero de folio asociado a la operacion
   -- parametros: proceso, operacion, usuario
   -- proceso 16, operacion 2, usuario que esta ejecutando la operacion
   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
     RETURNING v_folio
   
   -- Ejecuta prevalidación de encabezados y sumarios
   LET v_i_resultado = 0
   LET v_s_sql = "EXECUTE FUNCTION fn_deo_pre_integra_op98(?,?,?) "
   PREPARE Prpr_ValidaEncabezados FROM v_s_sql CLIPPED
   EXECUTE Prpr_ValidaEncabezados USING p_usuario_cod, g_pid, v_folio
      INTO v_i_resultado, v_si_error_isam, v_mensaje_error

   IF ( v_i_resultado <> 0 ) THEN
      -- Error en prevalidación, no se puede continuar con integracion
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                         RETURNING r_bnd_fin_oper
      DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
               
      DISPLAY "Error en prevalidación de Encabezados de archivo"
      DISPLAY v_mensaje_error
      DISPLAY v_i_resultado
      DISPLAY "No se puede continuar con la Integración"
               
      RETURN
   END IF
                       
   -- se asume que el proceso termina correctamente
   LET v_i_resultado = 0
   LET v_si_correcto_integra = 0
            
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_deo_integra_op98(?,?,?,?)"
            
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql
            
   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_usuario_cod, g_pid, v_folio, p_nombre_archivo
      INTO v_i_resultado, v_si_error_isam, v_mensaje_error, v_afores_integradas, v_afores_rechazadas
            
   -- Cierra la operación
   DISPLAY "El proceso de integración ha finalizado."
   DISPLAY "Estatus de integración:",v_i_resultado
               
   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : DEVOLUCIÓN POR  ERRORES OPERATIVOS\n",
                   "Operación    : INTEGRACIÓN\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n\n"

   -- el proceso termino sin errores
   IF ( v_i_resultado = 0 ) THEN
      -- si no se integro al menos una afore, es un error
      IF ( v_afores_integradas < 1 ) THEN
         DISPLAY "No se integró ninguna Afore. No se puede continuar con la preliquidación."
         DISPLAY "Mensaje: ", v_mensaje_error

         -- se indica en el mensaje para el correo electronico que el proceso termino con errores
         LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación:"
         LET p_mensaje = p_mensaje || v_mensaje_error || "\n\n"
         LET p_mensaje = p_mensaje || "No se puede continuar con el proceso de Preliquidación."

         -- se marca la operacion en error
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_fin_oper
         DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
      ELSE
         IF ( v_afores_rechazadas > 0 ) THEN
            DISPLAY "Se realizó la integración pero se encontraron Afores con Error."
            LET p_mensaje = p_mensaje || "Se realizó la integración pero se encontraron Afores con Error. Ya se puede continuar con la Preliquidación"
         ELSE
            DISPLAY "Integración realizada con éxito"
            LET p_mensaje = p_mensaje || "Integración realizada con éxito. Ya se puede continuar con la Preliquidación"
         END IF

         -- se marca la operacion como finalizada
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_fin_oper
         DISPLAY "Ya se puede Continuar con la Preliquidación"

      END IF
      
                  
   ELSE
      DISPLAY "Integración realizada pero con errores de validación"
      DISPLAY "Error - ", v_i_resultado
      DISPLAY "Mensaje: ", v_mensaje_error

      -- se indica en el mensaje para el correo electronico que el proceso termino con errores
      LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación:"
      LET p_mensaje = p_mensaje || v_mensaje_error || "\n\n"
      LET p_mensaje = p_mensaje || "No se puede continuar con el proceso de Preliquidación."

      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bnd_fin_oper
      DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
   END IF
                              
   LET p_titulo = "Finalización de operación - DEVOLUCIÓN POR ERRORES OPERATIVOS - INTEGRACION"
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END MAIN


FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   RETURN v_descripcion CLIPPED

END FUNCTION -- fn_mues_desc_valida