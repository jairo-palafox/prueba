 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP190                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para retiro fondo de ahorro 72-92                                      #
#Fecha inicio => Febrero 23, 2012                                                       #
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
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,p_programa_cod  VARCHAR(10)

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
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion
   LET g_folio       = p_folio

    
    -- Inicio operacion.
    --IF (fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETP190",
                                 --p_doc_cod,p_usuario_cod) = 0) THEN
            -- Inicia proceso de carga de archivo.
            -- se asume que el proceso termina correctamente
            LET v_i_resultado = 0
            LET v_si_correcto_integra = 0
                                         
            -- se contruye el enuncionado SQL
            LET v_s_sql = "EXECUTE FUNCTION fn_insert_preliquidacion_retiro_fondo_ahorro_cont_manual(?,?,?,?,?)"
                     
            -- se prepara la ejecucion del stored procedure para la integracion
            PREPARE sid_integraret FROM  v_s_sql
            EXECUTE sid_integraret USING g_folio,
                                         g_proceso_cod,
                                         g_opera_cod,
                                         p_usuario_cod,
                                         g_pid
                          INTO v_i_resultado
                          
            DISPLAY v_i_resultado
            --Se finaliza aunque existan errores
            IF ( v_i_resultado = 0 ) THEN
               -- Cierra la operación
               DISPLAY "La preliquidacion se terminó completamente."
               DISPLAY "Estatus de preliquidacion:",v_i_resultado
               
                LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                               "Proceso      : RETIRO FONDO DE AHORRO\n",
                               "Operación    : PRELIQUIDACIÓN\n",
                               "Fecha Inicio : ", TODAY, "\n",
                               "Fecha Fin    : ", TODAY, "\n\n"
                               
                -- si se termino correctamente                                
               
                  DISPLAY "Preliquidacion realizada con exito"
                  LET p_mensaje = p_mensaje || "Preliquidacion realizada con éxito\n.Ya se puede continuar con la Liquidación"
                  LET p_titulo = "Preliquidación Retiro Fondo de Ahorro "
                  DISPLAY p_proceso_cod,p_opera_cod
                  
                 SELECT programa_cod
                   INTO p_programa_cod
                   FROM cat_operacion
                  WHERE proceso_cod = p_proceso_cod
                    AND opera_cod   = p_opera_cod
                       
                    --CALL fn_reporte_liquidacion(p_folio, "ret_preliquida72", 
                                                --p_usuario_cod, p_pid, p_proceso_cod, 
                                                --p_opera_cod, p_programa_cod, 
                                                --FALSE)

               
               DISPLAY "Ya se puede Continuar con la Liquidación"
               DISPLAY "\n\n"
               
               CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                           RETURNING r_bnd_fin_oper
                 DISPLAY r_bnd_fin_oper
               CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                                      NULL, --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                                      p_titulo,
                                      p_mensaje)
            ELSE
                  DISPLAY "Preliquidación realizada pero con errores de validación"
                  LET p_mensaje = p_mensaje || "El proceso de Preliquidacion ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación."
               CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado
               -- Cancela la operacion para q se pueda iniciar nuevamente
            END IF
     -- END IF

END MAIN



