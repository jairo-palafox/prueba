--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOP02                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
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
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_i_resultado    INTEGER -- resultado del proceso
       ,r_bnd_fin_oper SMALLINT
       ,p_titulo               STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje              STRING -- cuerpo del mensaje enviado
       ,p_programa_cod  VARCHAR(10)
       ,v_si_error_isam        SMALLINT -- error ISAM
       ,v_mensaje_error        VARCHAR(255) -- mensaje devuelto por el SP
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)   
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO g_pid
   FROM
    bat_ctr_proceso
   WHERE
    proceso_cod = g_proceso_cod    

   {-- se mueve al programa lanzador 25 Mayo 2012
      -- Inicio operacion.
      IF (fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"DEOP02",
                                 "",p_usuario_cod) = 0) THEN
   }
   -- se asume que el proceso termina correctamente
   LET v_i_resultado = 0
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_deo_preliq_op98(?, ?, ?)"
   
   -- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidadeo FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_preliquidadeo USING p_folio, p_usuario_cod, g_pid 
      INTO v_i_resultado, v_si_error_isam, v_mensaje_error 
   CASE
      WHEN (v_i_resultado = 0)
       LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                       "Proceso      : DEVOLUCIÓN POR ERRORES OPERATIVOS\n",
                       "Operación    : PRELIQUIDACIÓN\n",
                       "Fecha Inicio : ", TODAY, "\n",
                       "Fecha Fin    : ", TODAY, "\n\n"

         -- se obtiene el codigo de programa
         SELECT programa_cod
         INTO   p_programa_cod
         FROM   cat_operacion
         WHERE  proceso_cod = p_proceso_cod
         AND    opera_cod   = p_opera_cod

                       
         CALL fn_reporte_liquidacion(p_folio, "deo_preliquida", 
                                     p_usuario_cod, p_pid, p_proceso_cod, 
                                     p_opera_cod, p_programa_cod, 
                                     FALSE)
         IF ( v_i_resultado = 0 ) THEN
         	
           -- se complementa el mensaje
           LET p_mensaje = p_mensaje || "Preliquidación realizada con éxito\n.Ya se puede continuar con la Liquidación"
         
         	
         	
            -- se invoca la finalizacion de la operacion
            CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
                                    g_proceso_cod, --- Clave del proceso
                                    g_opera_cod) --- Clave de la operación
                           RETURNING r_bnd_fin_oper
         ELSE
         	 -- se complementa el mensaje
         	 LET p_mensaje = p_mensaje || "El proceso de Preliquidación ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Liquidación."
         	
            CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
               RETURNING r_bnd_fin_oper
            DISPLAY "Ocurrió error al procesar preliquidación"
            DISPLAY "#  Error. No se preliquidó ninguna solicitud"
            DISPLAY "v_i_resultado:",v_i_resultado
         END IF
      
      LET p_titulo = "Finalización de operación - DEVOLUCIÓN POR ERRORES OPERATIVOS - PRELIQUIDACIÓN"
      
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   

      WHEN (SQLCA.SQLCODE = NOTFOUND)
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_fin_oper
         DISPLAY "NOT FOUND. No existen datos procesados"
         DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
         DISPLAY "#  Error. No se preliquidó ninguna solicitud"
      
      WHEN (SQLCA.SQLCODE < 0)
         DISPLAY SQLERRMESSAGE
         DISPLAY "Codigo Error SQL:",SQLCA.SQLCODE
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_fin_oper
         
         DISPLAY "Error al procesar la preliquidación"
         DISPLAY "No se puede continuar..."
   END CASE
        

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