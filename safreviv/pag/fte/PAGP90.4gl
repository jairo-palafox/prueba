-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGP90
-- Objetivo      => Integración en el histórico de CambiaVit 
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
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_integracion     LIKE cat_operacion.opera_cod, -- codigo de operacion
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  -- codigo de operacion

END GLOBALS

#Objetivo:
MAIN
DEFINE
   v_bnd_fin_proceso  SMALLINT,
   p_num_folio        DECIMAL(9,0),
   p_nom_archivo      LIKE glo_ctr_archivo.nombre_archivo,
   v_estatus          SMALLINT,
   p_titulo           STRING,  -- titulo del mensaje enviado en el correo
   p_mensaje          STRING   -- cuerpo del mensaje enviado

DEFINE v_reg INTEGER
   
   #Si se ha recibido parámetros se continua    
   #Primer parámetro
   LET p_usuario_cod = ARG_VAL(1)
   #Segundo parámetro
   LET g_pid         = ARG_VAL(2)
   #Tercer parámetro
   LET g_proceso_cod = ARG_VAL(3)
   #Cuarto parámetro
   LET g_opera_cod_integracion = ARG_VAL(4)  --Paso de información a las 
                                             --tablas históricas
   #Quinto parámetro
   LET p_num_folio = ARG_VAL(5)
   #Segundo parámetro
   LET p_nom_archivo = ARG_VAL(6)
      
   --Inicializacion de variables
   LET v_bnd_fin_proceso = 0
   LET v_reg = 0
     
   LET p_num_folio = fn_genera_folio(g_proceso_cod, g_opera_cod_integracion, p_usuario_cod)

   LET p_nom_archivo = fn_recupera_arch_cargado(g_proceso_cod,1)

   --Se ejecutan los displays
   CALL fn_display_proceso(0,"INTEGRACIÓN")
                              
   #Llamada a ejecución de procedimiento almacenado
   CALL fn_guarda_historicos_pag(p_num_folio) RETURNING v_estatus

   IF v_estatus = 0 THEN
      --Se actualiza el archivo como integrado
      CALL fn_act_edo_archivo(p_nom_archivo, p_num_folio, 2, p_usuario_cod ) RETURNING v_estatus
      --Se registra el FIN DE LA OPERACION COMO EXITOSA
      CALL fn_actualiza_opera_fin(g_pid,
                                  g_proceso_cod,
                                  g_opera_cod_integracion)
                                 RETURNING v_estatus
      LET p_mensaje = "Integración realizada con éxito.\nYa se puede continuar con la Preliquidación."
      --Mensaje si se puede prelquidar
      SELECT COUNT(*)
      INTO   v_reg
      FROM   pag_det_cvt
      WHERE  folio = p_num_folio
      AND    result_operacion = "01"
      IF v_reg = 0 THEN
         LET p_mensaje = "No se puede continuar con la preliquidación.\nYa que todos los registros fueron rechazados."
      END IF 
   ELSE
      LET p_mensaje = "El proceso de Integración ha finalizado pero con errores.\nNo se puede continuar con el proceso de Preliquidación."
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_integracion)  RETURNING v_estatus
   END IF
   --Se ejecutan los displays
   CALL fn_display_proceso(1,"INTEGRACIÓN")

   LET p_titulo = "Finalización de operación - CambiaVit - Integración"
   CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                          g_opera_cod_integracion, 
                          NULL, p_titulo,p_mensaje)
      
END MAIN

FUNCTION fn_guarda_historicos_pag(p_folio)
   DEFINE p_folio          DECIMAL(9),
          v_sql_procedure  STRING,
          v_estatus        SMALLINT,
          v_cod_error      SMALLINT,
          v_error_isam     INTEGER,
          v_mensaje_error  VARCHAR(255),
          v_det_trabajador_nss CHAR(11)
       
   --WHENEVER ERROR STOP
   LET v_estatus = 0 --El cero indica que se jecuto con exito
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_historico_pag_cvt(?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_historicoPag FROM v_sql_procedure
   EXECUTE prp_historicoPag USING p_folio ,g_pid,g_proceso_cod
   INTO v_cod_error, v_error_isam, v_mensaje_error, v_det_trabajador_nss

   IF (v_cod_error = 0) THEN
      # Ejecucion sin error
      DISPLAY v_mensaje_error
      DISPLAY v_cod_error
      --DISPLAY "@ v_det_trabajador_nss: ", v_det_trabajador_nss
      RETURN FALSE
   ELSE 
      LET v_estatus = 1  --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "\nError de ejecución en 'sp_registro_historicos_cvt' (Código): ",v_cod_error
      DISPLAY "Error en 'sp_registro_historicos_cvt' (Mensaje):",v_mensaje_error,"\n"
      DISPLAY "Error en 'sp_registro_historicos_cvt' (Mensaje):",v_error_isam,"\n"
      DISPLAY "NSS en curso: ", v_det_trabajador_nss
       RETURN TRUE
   END IF

   RETURN v_estatus
   
END FUNCTION