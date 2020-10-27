--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#############################################################################
#M�dulo          => ACL                                                     #
#Programa        => ACLP02.4gl                                              #
#Objetivo        => Programa de preliquidaci�n de Aclaracion si cambio NSS  #
#Fecha Inicio    => 15 FEBRERO 2012                                         #
#############################################################################
DATABASE safre_viv
{
GLOBALS
DEFINE g_opera_cod_carga       LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_opera_cod_integracion LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS
}

#Objetivo:
MAIN
DEFINE p_folio                     LIKE glo_folio.folio,
       p_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
       p_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
       p_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod, -- codigo de operacion
        p_opera_cod_liquidacion  LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_nom_archivo               STRING,
       r_resultado_opera     SMALLINT,
       p_titulo              STRING, -- titulo del mensaje enviado en el correo
       p_mensaje             STRING, -- cuerpo del mensaje enviado
       v_descripcion         CHAR(150),
       p_programa_cod        VARCHAR (10),
       v_num_reg             INTEGER,
       v_sql_procedure STRING,
       v_respuesta SMALLINT,
       v_mensaje_resp VARCHAR(255)

   #Si se ha recibido par�metros se continua    
   IF(NUM_ARGS() > 0)THEN
      
      LET p_usuario_cod              = ARG_VAL(1)
      LET p_pid                      = ARG_VAL(2)
      LET p_proceso_cod              = ARG_VAL(3)
      LET p_opera_cod_preliquidacion = ARG_VAL(4)
      LET p_folio                    = ARG_VAL(5)
      LET p_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(p_usuario_cod CLIPPED||".ACLP02.log")
      CALL fn_display_proceso(0,"Aclaraciones Sin Cambio")
      
               {-- se mueve al lanzador 25 Mayo 2012
         # Se registra el inicio de la operacion
         CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod_preliquidacion,p_folio,
                                     "ACLP02",p_nom_archivo,p_usuario_cod)
                            RETURNING r_resultado_opera
         # Valida si ocurri� una inconsistencia 
         IF(r_resultado_opera)THEN
         
            # Muestra el mensaje de la inconsistencia
            CALL fn_desplega_inc_operacion(r_resultado_opera)
            
            LET p_titulo = "Error de operaci�n - Aclaraciones Sin Cambio - Preliquidaci�n"
            SELECT descripcion
              INTO v_descripcion 
              FROM cat_bat_parametro_salida
             WHERE cod_salida = r_resultado_opera
            LET p_mensaje = v_descripcion
         ELSE
         }
            LET p_titulo = "Finalizaci�n de operaci�n - Aclaraciones Sin Cambio - Preliquidaci�n"
            #Llamada a ejecuci�n de procedimiento almacenado
            CALL fn_ejecuta_preliquidacion_sinCambio(p_folio,p_usuario_cod)
                             RETURNING r_resultado_opera
                             
            # cambia a estado errone si no se ejecut� correctamente el SP
            IF ( r_resultado_opera ) THEN
               
               LET p_titulo = "Error de operaci�n - Aclaraciones Sin Cambio - Preliquidaci�n"
               SELECT descripcion
                 INTO v_descripcion 
                 FROM cat_bat_parametro_salida
                WHERE cod_salida = r_resultado_opera
               LET p_mensaje = v_descripcion
               
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                 RETURNING r_resultado_opera 
            ELSE
               CALL fn_display_proceso(1,"Aclaraciones Sin Cambio")
               # Se finaliza la carga de registros para preliquidacion
               CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                                  RETURNING r_resultado_opera
               
               # Valida si ocurri� una inconsistencia 
               IF(r_resultado_opera)THEN
                  LET p_titulo = "Error de operaci�n - Aclaraciones Sin Cambio - Preliquidaci�n"
                  SELECT descripcion
                    INTO v_descripcion 
                    FROM cat_bat_parametro_salida
                   WHERE cod_salida = r_resultado_opera
                  LET p_mensaje = v_descripcion
                  
                  # Muestra el mensaje de la inconsistencia
                  CALL fn_desplega_inc_operacion(r_resultado_opera)
                  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                                    RETURNING r_resultado_opera 
               ELSE--CABC se agrega para continuar aunque todos los registros hayan sido rechazados
                 SELECT COUNT(*) 
                   INTO v_num_reg
                   FROM acl_preliquida
                   WHERE folio_liquida=p_folio--CABC se agrega para continuar aunque todos los registros hayan sido rechazados

                 IF (v_num_reg = 0) OR (v_num_reg IS NULL) THEN 
                    LET p_titulo = "Se contin�a con el proceso para dar seguimiento,\n ya que no existen valores para ser liquidados."
                    LET p_mensaje = "Se contin�a con el proceso para dar seguimiento,\n ya que no existen valores para ser liquidados."
                    
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

                   --Se manda llamar la funci�n que ejecuta el reporte de liquidaci�n
                   CALL fn_reporte_liquidacion(p_folio, "acl_preliquida",
                                        p_usuario_cod, p_pid, p_proceso_cod,
                                        p_opera_cod_preliquidacion, p_programa_cod,
                                        FALSE)
                  LET p_mensaje = "Preliquidaci�n realizada con �xito.\nYa se puede continuar con la Liquidaci�n."
                END IF 
               END IF 
            END IF
         --END IF

         CALL fn_correo_proceso(p_pid, p_proceso_cod, 
                             p_opera_cod_preliquidacion, 
                             NULL, p_titulo,p_mensaje)
   END IF
   
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidaci�n
{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_sinCambio
Nombre: fn_ejecuta_preliquidacion_sinCambio
Fecha creacion: 15 de Febrero de 2012
Autor: Hugo C�sar Ram�rez Garc�a
Narrativa del proceso que realiza:
 Esta funci�n executa el store procedure que almacena la informaci�n 
 de la preliquidaci�n para el m�dulo de "Aclaracion sin cambio NSS"
 Parametros de Entrada:
 -
 Par�metros de salida;
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_ejecuta_preliquidacion_sinCambio(p_folio,p_usuario)
DEFINE p_folio         LIKE glo_folio.folio,
       p_usuario       CHAR(20),
       v_sql_procedure STRING,
       v_error_sql     SMALLINT,
       v_error_isam    INTEGER,
       v_mensaje_error VARCHAR(255)

   --WHENEVER ERROR CONTINUE}
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_sin_cambio_nss(?,?)"

   PREPARE prp_sqlPreliquidacionSinCambio FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacionSinCambio USING p_folio,p_usuario
   INTO v_error_sql,v_error_isam,v_mensaje_error
   
   IF(v_error_sql = 0)THEN
--      DISPLAY "1: ", v_error_sql
--      DISPLAY "2: ",v_error_isam
--      DISPLAY "3: ",v_mensaje_error
      --ejecucion sin error
      RETURN FALSE
   ELSE
      --DISPLAY "\nError en sp_preliquida_sin_cambio_nss (Codigo):",SQLCA.SQLCODE
      --DISPLAY "Error en sp_preliquida_sin_cambio_nss (Codigo):",SQLCA.SQLERRM,"\n"
      DISPLAY "\nError ejecucion sp_preliquida_sin_cambio_nss (Codigo): ",v_error_sql
      DISPLAY "Error en sp_preliquida_sin_cambio_nss (Mensaje):",v_mensaje_error,"\n"
      DISPLAY "Error en sp_preliquida_sin_cambio_nss (Mensaje):",v_error_isam,"\n"
      RETURN TRUE
   END IF
END FUNCTION