--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 13/01/2016
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP24                                                        #
#Objetivo     => Lanzador de la preliquidación Unificación Complementaria      #
#Fecha inicio => Enero 13, 2015                                                #
################################################################################

GLOBALS "UNIG01.4gl"
GLOBALS
   DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
          g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN
   DEFINE p_pid               LIKE bat_ctr_operacion.pid,
          p_proceso_cod       LIKE bat_ctr_operacion.proceso_cod,
          p_opera_cod         LIKE bat_ctr_operacion.opera_cod,
          p_usuario_cod       LIKE seg_usuario.usuario_cod,
          p_folio_unificacion LIKE deo_preliquida.folio_liquida,
          p_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo,
          v_s_sql             STRING,
          v_i_resultado       INTEGER,
          r_bnd_fin_oper      SMALLINT,
          v_i_total_registros INTEGER,
          p_titulo            STRING,
          p_mensaje           STRING,
          v_layout            SMALLINT,
          v_ruta_rescate      STRING,
          v_usuario           LIKE seg_modulo.usuario,
          v_proceso_desc      LIKE cat_proceso.proceso_desc,
          v_extension         LIKE cat_operacion.extension,
          v_opera_desc        LIKE cat_operacion.opera_desc,
          v_ruta_listados     LIKE seg_modulo.ruta_listados,
          v_folio_liquida     LIKE dpe_preliquida.folio_liquida,
          p_programa_cod      VARCHAR(10),
          v_error_isam        INTEGER,
          v_mensaje_error     VARCHAR(255)


   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| "UNIP24.log")

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario_cod       = ARG_VAL(1)
   LET g_pid               = ARG_VAL(2)
   LET g_proceso_cod       = ARG_VAL(3)
   LET g_opera_cod         = ARG_VAL(4)
   LET p_folio_unificacion = ARG_VAL(5)
   LET p_nombre_archivo    = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
        RETURNING v_proceso_desc,v_extension, 
                  v_opera_desc,v_layout, 
                  v_ruta_rescate,v_ruta_listados,
                  v_usuario

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- Se prepara la función de preliquidación
   LET v_s_sql = "EXECUTE FUNCTION fn_uni_preliquida_complementaria(?, ?, ?, ?)"

   PREPARE sid_preliquidauni FROM v_s_sql
   EXECUTE sid_preliquidauni USING p_folio_unificacion, 
                                   p_usuario_cod, 
                                   g_pid, 
                                   g_proceso_cod 
           INTO v_i_resultado, 
                v_i_total_registros,
                v_folio_liquida, 
                v_error_isam, 
                v_mensaje_error

   UPDATE glo_folio
   SET    folio_referencia = v_folio_liquida,
          status = 1
   WHERE  folio = p_folio_unificacion

   UPDATE bat_ctr_operacion
   SET    folio = v_folio_liquida
   WHERE  pid = g_pid
   AND    opera_cod = 3

   -- Se finaliza operacion aunque no se termine correctamente el error
   DISPLAY "    La preliquidación se terminó completamente."
   DISPLAY " "
   DISPLAY "    El folio Complementario                : "||p_folio_unificacion
   DISPLAY " "
   DISPLAY "    El folio Preliquidación                : "||v_folio_liquida
   DISPLAY " "

   IF ( v_i_resultado = 0 ) THEN
      DISPLAY "    Preliquidación realizada completamente"
      -- se invoca la finalizaon de la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bnd_fin_oper
      LET p_mensaje = "    La preliquidación se terminó completamente. \n",
                      " \n",
                      "    El folio Complementario: "||p_folio_unificacion,"\n",
                      " \n",
                      "    El folio Preliquidación: "||v_folio_liquida,"\n",
                      " \n"

      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = g_proceso_cod
      AND    opera_cod   = g_opera_cod
         
      CALL fn_reporte_liquidacion(v_folio_liquida, 
                                  "uni_preliquida",
                                  p_usuario_cod, 
                                  g_pid, 
                                  g_proceso_cod, 
                                  g_opera_cod, 
                                  p_programa_cod,
                                  FALSE);
   ELSE
      -- Indica que error ocurrio
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bnd_fin_oper
      DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
      DISPLAY "    Error al procesar la Preliquidación"
      DISPLAY "    El status de resultado es: ", v_i_resultado
      DISPLAY "    Mensaje: ", v_mensaje_error
      DISPLAY "    Sin saldo disponible para las solicitudes aceptadas"
      DISPLAY "    Total registros encontrados: ",v_i_total_registros
      
      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END IF
      
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION"

   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)

END MAIN

#OBJETIVO: Obtener la descripcion del error de la validacion y mostrar al usuario. 
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   --CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")
   DISPLAY "Atención ",v_descripcion CLIPPED
   
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida
