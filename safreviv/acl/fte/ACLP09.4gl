--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#############################################################################
#Módulo          => ACL                                                     #
#Programa        => ACLP06.4gl                                              #
#Objetivo        => Programa de preliquidación de Aclaracion ENACLARA       #
#Fecha Inicio    => 15 FEBRERO 2012                                         #
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
       r_resultado_opera     SMALLINT,
       p_titulo              STRING, -- titulo del mensaje enviado en el correo
       p_mensaje             STRING, -- cuerpo del mensaje enviado
       v_descripcion CHAR(150)   

   #Si se ha recibido parámetros se continua    
   IF(NUM_ARGS() > 0)THEN
      
      LET p_usuario_cod              = ARG_VAL(1)
      LET p_pid                      = ARG_VAL(2)
      LET p_proceso_cod              = ARG_VAL(3)
      LET p_opera_cod_preliquidacion = ARG_VAL(4)
      LET p_folio                    = ARG_VAL(5)
      LET p_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(p_usuario_cod CLIPPED||".ACLP09.log")

      CALL fn_display_proceso(0,"Preliquidación aclaracion 1402")
      
      # Valida si se puede actualizar la operaion
      CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                            RETURNING r_resultado_opera
      # Valida si se puede iniciar la operacion
      IF(r_resultado_opera)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
         # Solo muestra el mensaje y sale de la arga
      ELSE
         # Se registra el inicio de la operacion
         CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod_preliquidacion,p_folio,
                                     "ACLP09",p_nom_archivo,p_usuario_cod)
                            RETURNING r_resultado_opera
         # Valida si ocurrió una inconsistencia 
         IF(r_resultado_opera)THEN
            # Muestra el mensaje de la inconsistencia
            CALL fn_desplega_inc_operacion(r_resultado_opera)
            LET p_titulo = "Error de operación - Aclaraciones ACL-1402 - Preliquidación"
            SELECT descripcion
              INTO v_descripcion 
              FROM cat_bat_parametro_salida
             WHERE cod_salida = r_resultado_opera
            LET p_mensaje = v_descripcion
         ELSE
      
            #Llamada a ejecución de procedimiento almacenado
            CALL fn_ejecuta_preliquidacion_enaclara(p_folio,p_usuario_cod)
                             RETURNING r_resultado_opera
            # cambia a estado erroneo si no se ejecutó correctamente el SP
            IF(r_resultado_opera)THEN
               LET p_titulo = "Error de operación - Aclaraciones ACL-1402 - Preliquidación"
               SELECT descripcion
                 INTO v_descripcion 
                 FROM cat_bat_parametro_salida
                WHERE cod_salida = r_resultado_opera
               LET p_mensaje = v_descripcion
               
               CALL fn_error_opera(p_pid,p_proceso_cod,g_opera_cod_integracion) 
                 RETURNING r_resultado_opera 
               
            ELSE            
               CALL fn_display_proceso(1,"Preliquidación aclaraciones 1402")
               # Se finaliza la carga de registros para preliquidacion
               CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                                  RETURNING r_resultado_opera
               
               # Valida si ocurrió una inconsistencia 
               IF(r_resultado_opera)THEN
                  LET p_titulo = "Error de operación - Aclaraciones ACL-1402 - Preliquidación"
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
                  LET p_titulo = "Finalización de operación - Aclaraciones ACL-1402 - Preliquidación"
                  LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
               END IF
            END IF
         END IF
         CALL fn_correo_proceso(p_pid, p_proceso_cod, 
                             p_opera_cod_preliquidacion, 
                             NULL, p_titulo,p_mensaje)
      END IF
   END IF
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_enaclara
Nombre: fn_ejecuta_preliquidacion_enaclara
Fecha creacion: 28 de Febrero de 2012
Autor: Francisco López
Narrativa del proceso que realiza:
 Esta función executa el store procedure que almacena la información 
 de la preliquidación para el módulo de "Aclaracion ENACLARA NSS"
 Parametros de Entrada:
 -
 Parámetros de salida;
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_ejecuta_preliquidacion_enaclara(p_folio,p_usuario)
DEFINE p_folio         LIKE glo_folio.folio,
       p_usuario       CHAR(20),
       v_sql_procedure STRING

   WHENEVER ERROR CONTINUE
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_aclaracion1402(?,?)"
   PREPARE prp_sqlPreliquidacionEnaclara FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacionEnaclara USING p_folio,p_usuario
   IF(SQLCA.SQLCODE = 0)THEN
      RETURN FALSE
   ELSE
      DISPLAY "\nError en sp_preliquida_aclaracion1402 (Codigo):",SQLCA.SQLCODE
      DISPLAY "Error en sp_preliquida_aclaracion1402 (Codigo):",SQLCA.SQLERRM,"\n"
      RETURN TRUE
   END IF
END FUNCTION