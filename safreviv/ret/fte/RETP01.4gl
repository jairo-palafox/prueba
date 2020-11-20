--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETP01                                                                 #
#OBJETIVO     => PROGRAMA QUE EJECUTA EL STORED PROCEDURE QUE REALIZA LA PRELIQUIDACION #
#                PARA RETIRO SOLO INFONAVIT                                             #
#FECHA INICIO => FEBRERO 17, 2012                                                       #
#FECHA MODIFICACION =>                                                                  #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,         --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod,     -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod,     -- codigo de operacion
       g_folio       LIKE ret_preliquida.folio_liquida --folio liquidacion
END GLOBALS

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid,         -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod,   -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod,       -- clave del usuario firmado
       p_folio                LIKE ret_preliquida.folio_liquida,
       v_s_sql                STRING,                             -- cadena con una instruccion SQL
       v_i_resultado          INTEGER                             -- resultado del proceso
      ,r_bnd_fin_oper         SMALLINT
      ,v_si_correcto_integra  SMALLINT
      ,p_doc_cod              VARCHAR(20)
      ,p_titulo               STRING -- titulo del mensaje enviado en el correo
      ,p_mensaje              STRING -- cuerpo del mensaje enviado
      ,p_programa_cod         VARCHAR(10)
      ,v_error_isam          INTEGER
      ,v_mensaje             VARCHAR(250)
      ,v_subcuenta        LIKE ret_preliquida.subcuenta
      ,v_subcuenta_desc   LIKE cat_subcuenta.subcuenta_desc
      ,v_movimiento       LIKE ret_preliquida.movimiento
      ,v_movimiento_desc  LIKE cat_movimiento.movimiento_desc
      ,v_sum_acciones     LIKE ret_preliquida.monto_acciones
      ,v_sum_pesos        LIKE ret_preliquida.monto_pesos

   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 

   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion
   LET g_folio       = p_folio
   
   -- Inicio operacion
  IF ( fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETP01",
                                 p_doc_cod,p_usuario_cod) = 0 ) THEN
     -- se asume que el proceso termina correctamente
     LET v_i_resultado = 0
     LET v_si_correcto_integra = 0
     
     -- se contruye el enuncionado SQL
     LET v_s_sql = "EXECUTE FUNCTION fn_ret_preliquida_solo_infonavit_ws(?,?,?,?,?)"

     -- se prepara la ejecucion del stored procedure para la preliquidacion
     PREPARE sid_ret_solo FROM v_s_sql
     EXECUTE sid_ret_solo USING   g_folio,
                                  g_proceso_cod,
                                  g_opera_cod,
                                  p_usuario_cod,
                                  g_pid
                   INTO v_i_resultado, v_error_isam, v_mensaje
     
     -- Se finaliza aunque existan errores
     DISPLAY "El proceso de preliquidación ha finalizado.\n"
    
     LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                     "Proceso      : RETIRO SÓLO INFONAVIT WS\n",
                     "Operación    : PRELIQUIDACIÓN\n",
                     "Fecha Inicio : ", TODAY, "\n",
                     "Fecha Fin    : ", TODAY, "\n\n"
    
    -- si se termino correctamente  
    IF ( v_i_resultado = 0 ) THEN
       DISPLAY "Preliquidación realizada con éxito."
       LET p_mensaje = p_mensaje || "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación"
       LET p_titulo = "Preliquidación Retiro sólo Infonavit WS"
    
       SELECT programa_cod
         INTO p_programa_cod
         FROM cat_operacion
        WHERE proceso_cod = p_proceso_cod
          AND opera_cod   = p_opera_cod

      
       -- reporte en log de cifras preliquidadas
       DISPLAY "= CIFRAS PRELIQUIDADAS ="
       
       DECLARE cur_cifras_control CURSOR FOR
       SELECT pre.subcuenta
              ,cs.subcuenta_desc
              ,pre.movimiento
              ,cm.movimiento_desc
              ,SUM(pre.monto_acciones)
              ,SUM(pre.monto_pesos)
       FROM ret_preliquida pre
       INNER JOIN cat_subcuenta cs ON pre.subcuenta = cs.subcuenta
       INNER JOIN cat_movimiento cm ON pre.movimiento = cm.movimiento
       WHERE folio_liquida = p_folio
       GROUP BY pre.subcuenta, cs.subcuenta_desc, pre.movimiento, cm.movimiento_desc
       ORDER BY pre.subcuenta
       
       FOREACH cur_cifras_control INTO v_subcuenta, v_subcuenta_desc, v_movimiento, v_movimiento_desc,
                                       v_sum_acciones, v_sum_pesos
          DISPLAY "SUBCUENTA           ", v_subcuenta, " ", v_subcuenta_desc
          DISPLAY "MOVIMIENTO          ", v_movimiento, " ", v_movimiento_desc
          DISPLAY "MONTO ACCIONES      ", v_sum_acciones
          DISPLAY "MONTO PESOS         ", v_sum_pesos
       END FOREACH
       
       FREE cur_cifras_control
       
       DISPLAY "\n\n"
       
       -- se invoca el reporte generico de preliquidacion/liquidacion
       CALL fn_reporte_liquidacion(p_folio, "ret_preliquida", 
                                   p_usuario_cod, p_pid, p_proceso_cod, 
                                   p_opera_cod, p_programa_cod, 
                                   FALSE)
                                   
       CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                              RETURNING r_bnd_fin_oper
    ELSE
       -- si fue error de base de datos, o que no preliquido ningun registro, no se puede continuar
       DISPLAY "La preliquidación finalizó con errores: "
       DISPLAY "Código : ", v_i_resultado
       DISPLAY "ISAM   : ", v_error_isam
       DISPLAY "Mensaje: ", v_mensaje
       LET p_mensaje = p_mensaje || "\n\nPreliquidacion finalizó con errores.\nNo es posible continuar con la Liquidación"
       LET p_mensaje = p_mensaje || "\nCódigo : ", v_i_resultado
       LET p_mensaje = p_mensaje || "\nISAM   : ", v_error_isam
       LET p_mensaje = p_mensaje || "\nMensaje: ", v_mensaje
       
       CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                           RETURNING v_i_resultado
    END IF
    
    -- se envia correo al usuario    
    CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                           NULL, --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                           p_titulo,
                           p_mensaje)
  END IF

END MAIN