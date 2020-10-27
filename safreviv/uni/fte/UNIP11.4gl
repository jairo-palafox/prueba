--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07/01/2014
-- Modificació: Se actualizo el Proceso Vacio para que finalice el proceso
--              cuando no existan datos a preliquidas. AG. 
--===============================================================

#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIP11                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para los complementarios                                               #
#Fecha inicio => 16/08/2012                                                             #
#########################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_i_resultado    INTEGER -- resultado del proceso
       ,r_bnd_fin_oper SMALLINT
      --
       ,v_i_total_registros_insertados INTEGER
       ,p_titulo            STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje           STRING -- cuerpo del mensaje enviado
       ,v_layout            LIKE cat_operacion.layout_cod
       ,v_ruta_rescate      STRING
       ,v_usuario           LIKE seg_modulo.usuario
       ,v_proceso_desc      LIKE cat_proceso.proceso_desc
       ,v_extension         LIKE cat_operacion.extension
       ,v_opera_desc        LIKE cat_operacion.opera_desc
       ,v_ruta_listados     LIKE seg_modulo.ruta_listados
       ,v_folio_liquida     LIKE dpe_preliquida.folio_liquida -- folio de la liquidación
       ,p_programa_cod      VARCHAR(10)
       ,v_error_isam        INTEGER
       ,v_mensaje_error     VARCHAR(255)
       ,p_operacion         SMALLINT
       ,v_unificacion_compl INTEGER
       ,v_bnd_opera_fin     SMALLINT
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario

   CALL STARTLOG(p_usuario_cod CLIPPED|| ".UNIP11.log")

   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = g_proceso_cod_uni_complementario -- COMPLEMENTARIO
   LET g_opera_cod   = g_opera_cod_uni_batch -- ejecucion en batch
   LET p_operacion   = 2 -- ejecutar liquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
   
   --validar si hay registro en el primer cursor
   SELECT COUNT(*)
   INTO   v_unificacion_compl
   FROM   uni_det_unificador
   WHERE  estado_unificacion = 1  -- solo solicitudes confrontado
   AND    estado_familia = 1
   AND    folio_liquidacion > 0

   IF v_unificacion_compl < = 0 THEN
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "# \n",
                      "#    PROCESO FINALIZADO. \n",
                      "# \n",
                      "#    No se encontraron registros para ""\n",
                      "#    ejecutar la unificación complementaria ""\n",
                      "# \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY p_mensaje 
   
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod ,g_opera_cod)
      RETURNING v_bnd_opera_fin
   ELSE 
      -- se asume que el proceso termina correctamente
      LET v_i_resultado = 0
      -- se contruye el enuncionado SQL
      LET v_s_sql = "EXECUTE FUNCTION fn_uni_preliq_complementario(?, ?, ?, ?)"
      -- se prepara la ejecucion del stored procedure para la preliquidacion
      PREPARE sid_preliquidauni_com FROM v_s_sql
      -- se ejecuta el stored procedure
      EXECUTE sid_preliquidauni_com USING p_folio, 
                                          p_usuario_cod, 
                                          g_pid, 
                                          p_proceso_cod
                                     INTO v_i_resultado, 
                                          v_i_total_registros_insertados, 
                                          v_folio_liquida, 
                                          v_error_isam, 
                                          v_mensaje_error
      
      IF ( v_i_resultado = 0 ) THEN
         IF v_i_total_registros_insertados > 0 THEN 
            DISPLAY "#    Preliquidación realizada completamente"
            LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                            "# \n",
                            "#    La preliquidación se terminó completamente. \n",
                            "# \n",
                            "#    El folio preliqudiación: "||v_folio_liquida,"\n",
                            "# \n",
                            "# # # # # # # # # # # # # # # # # # # # # # # # # #"
            DISPLAY p_mensaje
      
            UPDATE bat_ctr_operacion
            SET    folio = v_folio_liquida
            WHERE  proceso_cod = g_proceso_cod
            AND    opera_cod = 1
            AND    pid = g_pid
         
            -- se invoca la finalizacion de la operacion
            CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
                                        g_proceso_cod, --- Clave del proceso
                                        g_opera_cod) --- Clave de la operación
            RETURNING r_bnd_fin_oper
         ELSE
            LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                            "Proceso      : UNIFICACIÓN CUENTAS COMPLEMENTARIO \n",
                            "Operación    : PRELIQUIDACIÓN \n",
                            "Fecha Inicio : ", TODAY, "\n",
                            "Fecha Fin    : ", TODAY, "\n\n",
                            "\n__________________________________________________________________",
                            "\nNo se tienen solicitudes para la preliquidación.\nNo es necesario ejecutar esta etapa.",
                            "\nProceso Vacio"                         
         END IF 
      ELSE
         -- Indica que ocurrio
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper

         DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
         DISPLAY "#    Error al procesar la Preliquidación"
         DISPLAY "#    El status de resultado es: ", v_i_resultado
         DISPLAY "#    Mensaje: ", v_mensaje_error
         DISPLAY "#    Sin saldo disponible para las solicitudes aceptadas"
         DISPLAY "#    Total registros encontrados: ", v_i_total_registros_insertados
         -- se complementa el mensaje
         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de Preliquidación no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n",
                         " Proceso Vacio"        
      END IF
        
      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = g_proceso_cod
      AND    opera_cod   = g_opera_cod

      CALL fn_reporte_liquidacion(v_folio_liquida, "uni_preliquida", 
                                  p_usuario_cod, g_pid, g_proceso_cod, 
                                  g_opera_cod, p_programa_cod,FALSE)
      
      LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION COMPLEMENTARIA"
      
      
      CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   END IF 
END MAIN

#OBJETIVO: Obtener la descripcion del error y mostrar el mensaje al usuario.
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT,
         v_descripcion     LIKE cat_bat_parametro_salida.descripcion
   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
   INTO   v_descripcion
   FROM   cat_bat_parametro_salida
   WHERE  cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   DISPLAY "Atención ",v_descripcion CLIPPED
   
   RETURN v_descripcion CLIPPED
END FUNCTION 