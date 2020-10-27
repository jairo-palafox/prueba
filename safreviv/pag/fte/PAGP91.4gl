-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGP91
-- Objetivo      => Progama de preliquidación de registro de pagos CambiVit
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 28 de Mayo de 2018
-- Requerimiento => plasgac-43
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS
   DEFINE
      g_pid                      LIKE bat_ctr_proceso.pid,     --  ID del proceso
      g_proceso_cod              LIKE cat_proceso.proceso_cod, -- codigo del proceso
      g_opera_cod_carga          LIKE cat_operacion.opera_cod, -- codigo de operacion
      p_usuario_cod              LIKE seg_usuario.usuario_cod, -- Clave de usuario
      g_opera_cod_preliquidacion LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN
   DEFINE 
      p_num_folio     DECIMAL(9),
      p_usuario_cod   CHAR(20),
      p_nom_archivo   STRING,
      v_estatus       SMALLINT,
      v_ruta_listados LIKE seg_modulo.ruta_listados,
      v_ruta_vacia    LIKE seg_modulo.ruta_bin,
      p_titulo        STRING,      -- titulo del mensaje enviado en el correo
      p_mensaje       STRING,      -- cuerpo del mensaje enviado   
      p_programa_cod  VARCHAR(10)

   IF NUM_ARGS() > 0 THEN

      LET p_usuario_cod = ARG_VAL(1)
      LET g_pid         = ARG_VAL(2)
      LET g_proceso_cod = ARG_VAL(3)
      LET g_opera_cod_preliquidacion = ARG_VAL(4)
      LET p_num_folio   = ARG_VAL(5)
      LET p_nom_archivo = ARG_VAL(6)

      CALL fn_rutas('bat') RETURNING v_ruta_vacia, v_ruta_listados          
      CALL STARTLOG(v_ruta_listados||
                    "/nohup:"||
                    g_pid USING "&&&&&"||
                    g_proceso_cod USING "&&&&&"||
                    g_opera_cod_preliquidacion USING "&&&&&"
                   )
      
         --Se ejecutan los displays
         CALL fn_display_proceso(0,"PRELIQUIDACIÓN")
         #Llamada a ejecución de procedimiento almacenado
         CALL fn_ejecuta_preliquidacion(p_num_folio,p_usuario_cod) RETURNING v_estatus

         IF v_estatus = 0 THEN
            SELECT programa_cod
            INTO   p_programa_cod
            FROM   cat_operacion
            WHERE  proceso_cod = g_proceso_cod
            AND    opera_cod = g_opera_cod_preliquidacion
               
            --Se manda llamar la función que ejecuta el reporte de liquidación
            CALL fn_reporte_liquidacion(p_num_folio, "pag_preliquida_cvt",
                                        p_usuario_cod, g_pid, g_proceso_cod,
                                        g_opera_cod_preliquidacion, p_programa_cod,
                                        FALSE)

            CALL fn_actualiza_opera_fin(g_pid
                                       ,g_proceso_cod
                                       ,g_opera_cod_preliquidacion)
                                       RETURNING v_estatus
            LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
         ELSE
            --Si ocurrio un error se actualiza el estatus como erroneo
            CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_preliquidacion)  RETURNING v_estatus
            LET p_mensaje = "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación."
         END IF

         --Se ejecutan los displays
         CALL fn_display_proceso(1,"PRELIQUIDACIÓN")
         LET p_titulo = "Finalización de operación - CambiaVit - Preliquidación"
         CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                             g_opera_cod_preliquidacion, 
                             NULL, p_titulo,p_mensaje)
   END IF
   
END MAIN


FUNCTION fn_ejecuta_preliquidacion(p_folio_sua,p_usuario)
   DEFINE 
      p_folio_sua          LIKE pag_cza_pag_patronal.folio_sua,
      p_usuario            CHAR(20),
      v_sql_procedure      STRING,
      v_estatus            SMALLINT,
	  v_error              SMALLINT,
	  isam_err             INTEGER ,
      err_txt              VARCHAR(255),
      v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente

   -- se prepara la ejecuicion del SP
   LET v_estatus = 0 --El cero indica que se jecuto con exito
   
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_cvt(?,?)"
   
   PREPARE prp_sqlPreliquidacion FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacion INTO v_error, isam_err, err_txt, v_id_derechohabiente
      USING p_folio_sua,p_usuario   
   
  IF v_error = 0 THEN
   	 DISPLAY v_error
   	 DISPLAY isam_err
     DISPLAY err_txt
     # Ejecucion sin error
     RETURN FALSE
  ELSE
     DISPLAY "\nError ejecucion sp_preliquida_cvt (ISAM): ", isam_err
     DISPLAY "\nError ejecucion sp_preliquida_cvt (SLQ) : ", v_error
     DISPLAY "\nMensaje                                 : ", err_txt
     DISPLAY "\nID Derechohabiente                      : ", v_id_derechohabiente
     RETURN TRUE
  END IF

END FUNCTION