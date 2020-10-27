--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#############################################################################
#Módulo          => ACL                                                     #
#Programa        => ACLP04.4gl                                              #
#Objetivo        => Programa de preliquidación de Aclaracion con cambio NSS #
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
       p_opera_cod_liquidacion  LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_nom_archivo               STRING,
       r_resultado_opera           SMALLINT,
       p_titulo                    STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                   STRING, -- cuerpo del mensaje enviado
       v_descripcion               CHAR(150),
       p_programa_cod              VARCHAR (10),
       v_num_reg             INTEGER,
       v_sql_procedure STRING,
       v_respuesta SMALLINT,
       v_mensaje_resp VARCHAR(255)

   #Si se ha recibido parámetros se continua    
   IF(NUM_ARGS() > 0)THEN
      
      LET p_usuario_cod              = ARG_VAL(1)
      LET p_pid                      = ARG_VAL(2)
      LET p_proceso_cod              = ARG_VAL(3)
      LET p_opera_cod_preliquidacion = ARG_VAL(4)
      LET p_folio                    = ARG_VAL(5)
      LET p_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(p_usuario_cod CLIPPED||".ACLP04.log")
      --Se imprimen los displays para el log
      CALL fn_display_proceso(0,"Preliquidación de aclaración con cambio ")
      
         {-- se mueve el inicio de la operacion al programa lanzador 25 Mayo 2012
         # Se registra el inicio de la operacion
         CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod_preliquidacion,p_folio,
                                     "ACLP04",p_nom_archivo,p_usuario_cod)
                            RETURNING r_resultado_opera
         
         # Valida si ocurrió una inconsistencia 
         IF(r_resultado_opera)THEN
            # Muestra el mensaje de la inconsistencia
            CALL fn_desplega_inc_operacion(r_resultado_opera)
            
            LET p_titulo = "Error de operación - Aclaraciones Con Cambios - Preliquidación"
            SELECT descripcion
              INTO v_descripcion 
              FROM cat_bat_parametro_salida
             WHERE cod_salida = r_resultado_opera
            LET p_mensaje = v_descripcion
         ELSE
         }
      
            #Llamada a ejecución de procedimiento almacenado
            CALL fn_ejecuta_preliquidacion_conCambio(p_folio,p_usuario_cod)
                             RETURNING r_resultado_opera
            # cambia a estado errone si no se ejecutó correctamente el SP
            IF(r_resultado_opera)THEN
               LET p_titulo = "Error de operación - Aclaraciones Con Cambios - Preliquidación"
               SELECT descripcion
                 INTO v_descripcion 
                 FROM cat_bat_parametro_salida
                WHERE cod_salida = r_resultado_opera
               LET p_mensaje = v_descripcion
               
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                 RETURNING r_resultado_opera
            ELSE
               CALL fn_display_proceso(1,"Preliquidación de aclaración con cambio ")
               # Se finaliza la carga de registros para preliquidacion
               CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                                  RETURNING r_resultado_opera
               
               # Valida si ocurrió una inconsistencia 
               IF(r_resultado_opera)THEN
                  LET p_titulo = "Error de operación - Aclaraciones Con Cambios - Preliquidación"
                  SELECT descripcion
                    INTO v_descripcion 
                    FROM cat_bat_parametro_salida
                   WHERE cod_salida = r_resultado_opera
                  LET p_mensaje = v_descripcion
                  # Muestra el mensaje de la inconsistencia
                  CALL fn_desplega_inc_operacion(r_resultado_opera)
                  CALL fn_error_opera(p_pid,p_proceso_cod,g_opera_cod_integracion) 
                                    RETURNING r_resultado_opera 
               ELSE--CABC se agrega código para continuar con el proceso aunque todos los registros hayan sido rechazados
                
               SELECT COUNT(*) 
                   INTO v_num_reg
                   FROM acl_preliquida
                   WHERE folio_liquida=p_folio--CABC se agrega para continuar aunque todos los registros hayan sido rechazados


                   IF (v_num_reg = 0) OR (v_num_reg IS NULL) THEN 
                        LET p_titulo = "Se continúa con el proceso para dar seguimiento,\n ya que no existen valores para ser liquidados."
                        LET p_mensaje = "Se continúa con el proceso para dar seguimiento,\n ya que no existen valores para ser liquidados.."
                                            
                        LET p_opera_cod_liquidacion=4
                        CALL fn_ejecuta_liquidacion(p_folio,p_proceso_cod,p_opera_cod_liquidacion,"acl_preliquida",p_usuario_cod)

                        #Se agregan lineas para ejecutar el procedure que inhabilita el folio
                        LET v_sql_procedure= "EXECUTE PROCEDURE sp_inhabilita_indicador(?)"

                        PREPARE prp_stm_inhabilita_indicador FROM v_sql_procedure

                        EXECUTE prp_stm_inhabilita_indicador USING p_folio
                        INTO v_respuesta,v_mensaje_resp

                        IF v_respuesta<>0 THEN
                              DISPLAY "\nError ejecucion sp_inhabilita_indicador (Codigo): ",v_respuesta      
                              DISPLAY "Error en sp_inhabilita_indicador (Mensaje):",v_mensaje_resp,"\n"
                        END IF
                ELSE
                   
                   --se obtiene el codigo de programa
                   SELECT programa_cod
                     INTO p_programa_cod
                     FROM cat_operacion
                    WHERE proceso_cod = p_proceso_cod
                      AND opera_cod = p_opera_cod_preliquidacion

                   --Se manda llamar la función que ejecuta el reporte de liquidación
                   CALL fn_reporte_liquidacion(p_folio, "acl_preliquida",
                                        p_usuario_cod, p_pid, p_proceso_cod,
                                        p_opera_cod_preliquidacion, p_programa_cod,
                                        FALSE)
               
                  LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
                  LET p_titulo = "Finalización de operación - Aclaraciones Con Cambios - Preliquidación"
                  END IF
               END IF
            END IF
         --END IF
         CALL fn_correo_proceso(p_pid, p_proceso_cod, 
                             p_opera_cod_preliquidacion, 
                             NULL, p_titulo,p_mensaje)
   END IF   
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_conCambio
Nombre: fn_ejecuta_preliquidacion_conCambio
Fecha creacion: 15 de Febrero de 2012
Autor: Hugo César Ramírez García
Narrativa del proceso que realiza:
 Esta función executa el store procedure que almacena la información 
 de la preliquidación para el módulo de "Aclaracion con cambio NSS"
 Parametros de Entrada:
 -
 Parámetros de salida;
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_ejecuta_preliquidacion_conCambio(p_folio,p_usuario)
DEFINE p_folio         LIKE glo_folio.folio,
       p_usuario       CHAR(20),
       v_sql_procedure STRING,
       v_error_sql     SMALLINT,
       v_error_isam    SMALLINT,
       v_mensaje_error VARCHAR(255),
       v_origen_archivo SMALLINT

   --WHENEVER ERROR CONTINUE
   
   LET v_origen_archivo = 6 -- se asigna el valor original que es 6 para cambio de NSS
   
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_con_cambio_nss(?,?,?)"
   
   PREPARE prp_sqlPreliquidacionconCambio FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacionconCambio USING p_folio, p_usuario, v_origen_archivo
  INTO v_error_sql,v_error_isam,v_mensaje_error
   
   IF(v_error_sql = 0)THEN
      RETURN FALSE
   ELSE
      DISPLAY "\nError ejecucion sp_preliquida_con_cambio_nss (Codigo): ",v_error_sql      
      DISPLAY "Error en sp_preliquida_con_cambio_nss (Mensaje):",v_error_isam,"\n"
      DISPLAY "Error en sp_preliquida_con_cambio_nss (Mensaje):",v_mensaje_error,"\n"
      RETURN TRUE
   END IF
END FUNCTION