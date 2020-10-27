--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#############################################################################
#Módulo          => PAG                                                     #
#Programa        => PAGP13.4gl                                              #
#Objetivo        => Programa de preliquidación de recaudación SAR 92        #
#Fecha Inicio    => 11 ENERO 2012                                           #
#############################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_opera_cod_carga       LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_opera_cod_integracion LIKE cat_operacion.opera_cod  -- codigo de operacion
   
END GLOBALS

#Objetivo:
MAIN
DEFINE p_folio                     LIKE glo_folio.folio,
       p_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
       p_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
       p_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_nom_archivo               STRING,
       r_resultado_opera SMALLINT,
       p_titulo              STRING, -- titulo del mensaje enviado en el correo
       p_mensaje             STRING, -- cuerpo del mensaje enviado
       v_descripcion         CHAR(150),
       p_programa_cod        VARCHAR(10)

   #Si se ha recibido parámetros se continua    
   IF(NUM_ARGS() > 0)THEN
      
      LET p_usuario_cod              = ARG_VAL(1)
      LET p_pid                      = ARG_VAL(2)
      LET p_proceso_cod              = ARG_VAL(3)
      LET p_opera_cod_preliquidacion = ARG_VAL(4)
      LET p_folio                    = ARG_VAL(5)
      LET p_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(p_usuario_cod CLIPPED||".PAGP13.log")

      CALL fn_display_proceso(0,"PRELIQUIDACIÓN SAR92")
      
      	 -- se lleva esta funcion al lanzador 25 Mayo 2012         
         --Se registra el inicio de la operacion
         {# Se registra el inicio de la operacion
         CALL fn_actualiza_opera_ini(p_pid,
                                     p_proceso_cod,
                                     p_opera_cod_preliquidacion,
                                     p_folio,
                                     "PAGP13",
                                     p_nom_archivo,
                                     p_usuario_cod)
          RETURNING r_resultado_opera
         # Valida si ocurrió una inconsistencia 
         IF(r_resultado_opera)THEN
            # Muestra el mensaje de la inconsistencia
            CALL fn_desplega_inc_operacion(r_resultado_opera)
            
            LET p_titulo = "Error de operación - SAR92 - Preliquidación"
            SELECT descripcion
              INTO v_descripcion 
              FROM cat_bat_parametro_salida
             WHERE cod_salida = r_resultado_opera
            LET p_mensaje = v_descripcion
            
         ELSE
         }
            #Llamada a ejecución de procedimiento almacenado
            CALL fn_ejecuta_preliquidacion_sar(p_folio,p_usuario_cod)
                             RETURNING r_resultado_opera
            # cambia a estado errone si no se ejecutó correctamente el SP
            IF(r_resultado_opera)THEN
               LET p_titulo = "Error de operación - SAR92 - Preliquidación"
               SELECT descripcion
                 INTO v_descripcion 
                 FROM cat_bat_parametro_salida
                WHERE cod_salida = r_resultado_opera
               LET p_mensaje = v_descripcion
               
               CALL fn_error_opera(p_pid,p_proceso_cod,g_opera_cod_integracion) 
                 RETURNING r_resultado_opera 
            ELSE
               CALL fn_display_proceso(1,"PRELIQUIDACIÓN SAR92")
               # Se finaliza la carga de registros para preliquidacion
               CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                                  RETURNING r_resultado_opera
               
               # Valida si ocurrió una inconsistencia 
               IF(r_resultado_opera)THEN
                  LET p_titulo = "Error de operación - SAR92 - Preliquidación"
                  SELECT descripcion
                    INTO v_descripcion 
                    FROM cat_bat_parametro_salida
                   WHERE cod_salida = r_resultado_opera
                  LET p_mensaje = v_descripcion
                  # Muestra el mensaje de la inconsistencia
                  CALL fn_desplega_inc_operacion(r_resultado_opera)
                  CALL fn_error_opera(p_pid,p_proceso_cod,g_opera_cod_integracion) 
                                    RETURNING r_resultado_opera
               ELSE
                  --se obtiene el codigo de programa
                  SELECT programa_cod
                    INTO p_programa_cod
                    FROM cat_operacion
                   WHERE proceso_cod = p_proceso_cod
                     AND opera_cod = p_opera_cod_preliquidacion
                     
                  --Se manda llamar la función que ejecuta el reporte de liquidación
                  CALL fn_reporte_liquidacion(p_folio, "pag_sar92_preliquida",
                                              p_usuario_cod, p_pid, p_proceso_cod,
                                              p_opera_cod_preliquidacion, p_programa_cod,
                                              FALSE)
               
                  LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
                  LET p_titulo = "Finalización de operación - SAR92 - Preliquidación"
               END IF
            END IF
         --END IF
         CALL fn_correo_proceso(p_pid, p_proceso_cod, 
                                p_opera_cod_preliquidacion, 
                                NULL, p_titulo,p_mensaje)     
   END IF
   --LABEL salir:
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_sar
Nombre: fn_ejecuta_preliquidacion_sar
Fecha creacion: 11 de Enero de 2012
Autor: Hugo César Ramírez García
Narrativa del proceso que realiza:
 Esta función executa el store procedure que almacena la información 
 de la preliquidación para el módulo de "Recaudación SAR 92"
 Parametros de Entrada:
 -
 Parámetros de salida;
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_ejecuta_preliquidacion_sar(p_folio,p_usuario)
DEFINE p_folio         LIKE glo_folio.folio,
       p_usuario       CHAR(20),
       v_sql_procedure STRING

-- variables control de excepciones
DEFINE v_cod_error      SMALLINT,
       v_error_isam     INTEGER,
       v_mensaje_error  VARCHAR(255)
       
   --WHENEVER ERROR CONTINUE
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_sar92(?,?)"
   PREPARE prp_sqlPreliquidacionSAR FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacionSAR USING p_folio,p_usuario
   INTO v_cod_error, v_error_isam, v_mensaje_error

   IF (v_cod_error = 0) THEN
      DISPLAY v_mensaje_error
      DISPLAY v_cod_error
      # Ejecucion sin error
      RETURN FALSE

   ELSE 
      DISPLAY "\nError de ejecución en 'sp_preliquida_sar92.sql' (Código): ",v_cod_error
      DISPLAY "Error en 'sp_preliquida_sar92.sql' (Mensaje):",v_mensaje_error,"\n"
      DISPLAY "Error en 'sp_preliquida_sar92.sql' (Mensaje):",v_error_isam,"\n"
       RETURN TRUE
   END IF
   
   IF(SQLCA.SQLCODE = 0)THEN
      RETURN FALSE
   ELSE
      DISPLAY "\nError en sp_preliquida_sar92 (Codigo):",SQLCA.SQLCODE
      DISPLAY "Error en sp_preliquida_sar92 (Codigo):",SQLCA.SQLERRM,"\n"
      RETURN TRUE
   END IF
END FUNCTION