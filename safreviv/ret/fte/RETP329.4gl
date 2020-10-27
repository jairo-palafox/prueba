 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP329                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para retiro fondo de ahorro contingente de pagos vencido               #
#Fecha inicio => Enero 19, 2015                                                         #
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
       ,v_id_solicitud  DECIMAL(10,0)
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
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_preliquida_rest_pago_vencido_fondo_ahorro(?,?,?,?,?)"
            
   -- se prepara la ejecucion del stored procedure para la integracion
   DISPLAY "Se manda llamar la funcion con los parámetros :"
   DISPLAY "Folio ", g_folio
   DISPLAY "Proceso ", g_proceso_cod
   DISPLAY "Operación ", g_opera_cod
   DISPLAY "Usuario ", p_usuario_cod
   DISPLAY "Pid ", g_pid
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
                      "Proceso      : RESTITUCIÓN DE PAGOS VENCIDOS DEL FONDO DE AHORRO\n",
                      "Operación    : PRELIQUIDACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n"
                      
       -- si se termino correctamente
      DISPLAY v_mensaje
      DISPLAY "Ya se puede continuar con la Liquidación" , v_error_isam       
      
         DISPLAY "Preliquidacion realizada con exito"
         LET p_mensaje = p_mensaje || "Preliquidacion realizada con éxito\n.Ya se puede continuar con la Liquidación"
         LET p_titulo = "Preliquidación Restitución de pagos vencidos del Fondo de Ahorro"

      --Se invoca la función para generar archivos de rechazos y aceptados
      -- Se actualizan los registros que no se preliquidaron para reportarlos en los archivos correspondientes
      UPDATE  ret_rest_fondo_ahorro 
      SET     estado_solicitud   = 100,
              cod_rechazo        = 49
      WHERE   folio              = p_folio
      AND     estado_solicitud   = 15;   --- Estos no se preliquidaron por inconsistencia en la informacion
      
      DISPLAY "Generando archivos con registros aceptados y rechazados: "
      CALL fn_exporta_archivo(p_folio) RETURNING v_i_resultado
      IF v_i_resultado = 10 THEN 
         CALL fn_mensaje("Atención","No se encontraron registros para exportar", "information")
      END IF 
      -- parametros folio, usuario, incluir_rechazos, es_previo
--      LET v_s_comando = "fglrun RETS110.42r ",p_folio ," ", p_usuario_cod, " 0 "
--      RUN v_s_comando
      

         
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
      DISPLAY "\n"
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper
   
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE
      -- se complementa el mensaje
--      LET p_mensaje = p_mensaje || "El proceso de Preliquidacion  ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Preliquidación."                                 

      DISPLAY "\nError (SQL): ", v_i_resultado
      DISPLAY "Error (ISAM) : ", v_error_isam
      DISPLAY "Mensaje      : ", v_mensaje
      DISPLAY "ID Solicitud : ", v_id_solicitud
       
      DISPLAY "Preliquidación realizada pero con errores de validación"
      LET p_mensaje = p_mensaje || "El proceso de Preliquidacion ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación."
      DISPLAY p_mensaje
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado
      -- Cancela la operacion para q se pueda iniciar nuevamente
   END IF
     -- END IF

END MAIN

--Función que exporta datos a un archivo
FUNCTION fn_exporta_archivo(p_l_folio)
DEFINE p_l_folio          DECIMAL(9,0)

DEFINE v_cve_referencia   CHAR(11)

DEFINE p_query             STRING 
DEFINE v_s_sql             STRING 
DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_v_ruta_nomarch2       STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_cuenta                INTEGER,
      v_solicitud             DECIMAL(9,0),
      v_tipo                  CHAR(1),
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_contador              INTEGER,
      v_query                 STRING,
      v_cve_referencia_paso   CHAR(12),
      v_suma_aivs_97          DECIMAL(18,6),
      v_i                     INTEGER,
      v_regs_a_procesar       INTEGER,
      v_error                 SMALLINT 

   LET v_regresa = FALSE
   LET v_i       = 0
   LET v_cve_referencia   = NULL
   LET v_regs_a_procesar = 0


   DISPLAY "Se valida que existan registros para exportar"
   SELECT COUNT(*) 
   INTO   v_regs_a_procesar
   FROM   ret_rest_fondo_ahorro a 
   WHERE  a.folio =  p_l_folio
   AND    a.indicador = 'V'
   IF v_regs_a_procesar = 0 THEN 
      CALL fn_mensaje("Atención", "No existen registros para exportar", "information")
      RETURN 10
   END IF 


   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

--   DISPLAY "el folio a procesar es >", p_folio, "<"
   SELECT COUNT(*) 
   INTO   v_regs_a_procesar
   FROM   ret_rest_fondo_ahorro a 
   WHERE  a.folio =  p_l_folio
   AND    a.estado_solicitud <> 100
   AND    a.indicador = 'V' 
   IF v_regs_a_procesar > 0 THEN 
      -- las extensiones del archivo son csv para el detalle
      LET v_extension_txt = ".venliq"
      LET v_hora = CURRENT HOUR TO SECOND

      -- Se genera el nombre del archivo de Aceptados
      LET v_nom_archivo = TODAY USING "yyyymmdd"
      LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

      -- El archivo con ruta destino que contiene el detalle 
      LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

      -- Se muestra mensaje
      LET v_mensaje_archivo = "Se generará el archivo de aceptados :\n\n\t", v_v_ruta_nomarch
      DISPLAY "Se generará el archivo de aceptados :", v_v_ruta_nomarch
      
      
      --CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
      -- nombre de archivo generado

      -- Se crea el manejador de archivo
      LET v_ch_arch_ret_generico = base.Channel.create()
      CALL v_ch_arch_ret_generico.setDelimiter(NULL)

      -- Se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
      -- Escribe el encabezado del archivo de Aceptados
      
      LET v_s_detalle = "Referencia|Estado|"

      CALL v_ch_arch_ret_generico.write(v_s_detalle)

      LET p_query = "   SELECT cve_referencia ", 
                    "\n FROM   ret_rest_fondo_ahorro ",
                    "\n WHERE  folio = ", p_l_folio,
                    "\n AND    estado_solicitud <> 100",
                    "\n AND    indicador = 'V'"

      DISPLAY "La consulta de aceptados es >", p_query, "<"
      -- se llena el arreglo 
      PREPARE s_acep_query FROM p_query
      DECLARE cur_acep_query CURSOR FOR s_acep_query

      FOREACH cur_acep_query INTO v_cve_referencia
         
         LET v_s_detalle = v_cve_referencia,                 "|ACEPTADO|"
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         LET v_cve_referencia   = NULL
      END FOREACH
    
                              
      -- Se cierra el archivo de Aceptados
      CALL v_ch_arch_ret_generico.close()
      LET v_mensaje_archivo = "El archivo fue generado exitosamente:\n\n\t", v_v_ruta_nomarch

      DISPLAY "Se generó el archivo de Aceptados exitosamente", v_v_ruta_nomarch
   ELSE 
      CALL fn_mensaje("Atención", "No existen registros Aceptados para exportar", "information")
   END IF 

   
   SELECT COUNT(*) 
   INTO   v_regs_a_procesar
   FROM   ret_rest_fondo_ahorro a 
   WHERE  a.folio =  p_l_folio
   AND    a.estado_solicitud = 100 
   AND    a.indicador = 'V'
   IF v_regs_a_procesar > 0 THEN 

      LET v_extension_txt = ".venrech"

      -- Se genera el nombre del archivo de Rechazados
      LET v_nom_archivo = TODAY USING "yyyymmdd"
      LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

      -- El archivo con ruta destino que contiene el detalle 
      LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

      -- Se muestra mensaje
      LET v_mensaje_archivo = "Se generará el archivo de rechazados :\n\n\t", v_v_ruta_nomarch
      DISPLAY "Se generará el archivo de rechazados :", v_v_ruta_nomarch
      
      
      --CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
      -- nombre de archivo generado

      -- Se crea el manejador de archivo
      LET v_ch_arch_ret_generico = base.Channel.create()
      CALL v_ch_arch_ret_generico.setDelimiter(NULL)

      -- Se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
      -- Escribe el encabezado del archivo de Rechazados
      
      LET v_s_detalle = "Referencia|Estado|"

      CALL v_ch_arch_ret_generico.write(v_s_detalle)

      LET p_query = "   SELECT cve_referencia ", 
                    "\n FROM   ret_rest_fondo_ahorro ",
                    "\n WHERE  folio = ", p_l_folio,
                    "\n AND    estado_solicitud = 100",
                    "\n AND    indicador = 'V'"

      DISPLAY "La consulta de rechazados es >", p_query, "<"
      -- se llena el arreglo 
      PREPARE s_rch_query FROM p_query
      DECLARE cur_rch_query CURSOR FOR s_rch_query

      FOREACH cur_rch_query INTO v_cve_referencia
         
         LET v_s_detalle = v_cve_referencia,                 "|RECHAZADO|"
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         LET v_cve_referencia   = NULL
      END FOREACH
    
                              
      -- Se cierra el archivo de Rechazados
      CALL v_ch_arch_ret_generico.close()
      
      --CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   ELSE 
      CALL fn_mensaje("Atención", "No existen registros Rechazados para exportar", "information")
   END IF 

   RETURN v_regresa
   
END FUNCTION

