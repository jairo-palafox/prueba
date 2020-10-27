--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#############################################################################
#M�dulo          => ACL                                                     #
#Programa        => ACLP06.4gl                                              #
#Objetivo        => Programa de preliquidaci�n de Aclaracion ENACLARA       #
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
       r_resultado_opera           SMALLINT,
       p_titulo                    STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                   STRING, -- cuerpo del mensaje enviado
       v_descripcion               CHAR(150),    
       p_programa_cod              VARCHAR(10)
       
       
   #Si se ha recibido par�metros se continua    
   IF(NUM_ARGS() > 0)THEN
      
      LET p_usuario_cod              = ARG_VAL(1)
      LET p_pid                      = ARG_VAL(2)
      LET p_proceso_cod              = ARG_VAL(3)
      LET p_opera_cod_preliquidacion = ARG_VAL(4)
      LET p_folio                    = ARG_VAL(5)
      LET p_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(p_usuario_cod CLIPPED||".ACLP06.log")

      CALL fn_display_proceso(0,"Preliquidaci�n enaclara")
      
         {-- se mueve el inicio de la operacion al programa lanzador 25 Mayo 2012
         # Se registra el inicio de la operacion
         CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod_preliquidacion,p_folio,
                                     "ACLP06",p_nom_archivo,p_usuario_cod)
                            RETURNING r_resultado_opera
         # Valida si ocurri� una inconsistencia 
         IF(r_resultado_opera)THEN
            # Muestra el mensaje de la inconsistencia
            CALL fn_desplega_inc_operacion(r_resultado_opera)
            LET p_titulo = "Error de operaci�n - Aclaraciones Enaclara - Preliquidaci�n"
            SELECT descripcion
              INTO v_descripcion 
              FROM cat_bat_parametro_salida
             WHERE cod_salida = r_resultado_opera
            LET p_mensaje = v_descripcion
         ELSE
         }
      
            #Llamada a ejecuci�n de procedimiento almacenado
            CALL fn_ejecuta_preliquidacion_enaclara(p_folio,p_usuario_cod)
                             RETURNING r_resultado_opera
                             
            --display " fn_ejecuta_preliquidacion_enaclara r_resultado_opera = " ,r_resultado_opera
            # cambia a estado errone si no se ejecut� correctamente el SP
            IF(r_resultado_opera)THEN
            	 --display " en el if  r_resultado_opera"
               LET p_titulo = "Error de operaci�n - Aclaraciones Enaclara - Preliquidaci�n"
               SELECT descripcion
                 INTO v_descripcion 
                 FROM cat_bat_parametro_salida
                WHERE cod_salida = r_resultado_opera
               LET p_mensaje = v_descripcion
               
               CALL fn_error_opera(p_pid,p_proceso_cod,g_opera_cod_integracion) 
                 RETURNING r_resultado_opera
            ELSE
            
               --display " en el else  r_resultado_opera"
               CALL fn_display_proceso(1,"Preliquidaci�n enaclara")
               # Se finaliza la carga de registros para preliquidacion
               CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
                                  RETURNING r_resultado_opera
                --display " fn_actualiza_opera_fin r_resultado_opera = " ,r_resultado_opera
               # Valida si ocurri� una inconsistencia 
               IF(r_resultado_opera)THEN
               	  --display " IF DE fn_actualiza_opera_fin r_resultado_opera"
                  LET p_titulo = "Error de operaci�n - Aclaraciones Enaclara - Preliquidaci�n"
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

                   --Se manda llamar la funci�n que ejecuta el reporte de liquidaci�n
                   CALL fn_reporte_liquidacion(p_folio, "acl_enaclara_preliquida",
                                        p_usuario_cod, p_pid, p_proceso_cod,
                                        p_opera_cod_preliquidacion, p_programa_cod,
                                        FALSE)
               
               	  --display " Else fn_actualiza_opera_fin r_resultado_opera"
                  LET p_titulo = "Finalizaci�n de operaci�n - Aclaraciones Enaclara - Preliquidaci�n"
                  LET p_mensaje = "Preliquidaci�n realizada con �xito.\nYa se puede continuar con la Liquidaci�n."
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
Clave:  fn_ejecuta_preliquidacion_enaclara
Nombre: fn_ejecuta_preliquidacion_enaclara
Fecha creacion: 28 de Febrero de 2012
Autor: Francisco L�pez
Narrativa del proceso que realiza:
 Esta funci�n executa el store procedure que almacena la informaci�n 
 de la preliquidaci�n para el m�dulo de "Aclaracion ENACLARA NSS"
 Parametros de Entrada:
 -
 Par�metros de salida;
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_ejecuta_preliquidacion_enaclara(p_folio,p_usuario)
DEFINE p_folio            LIKE glo_folio.folio,
       p_usuario          CHAR(20),
       v_sql_procedure    STRING,
       v_cod_error        SMALLINT,
       v_error_isam       INTEGER,
       v_mensaje_error    VARCHAR(255),
       v_id_derechohabiente_error  LIKE afi_derechohabiente.id_derechohabiente

   --WHENEVER ERROR CONTINUE
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_enaclara_nss(?,?)"
   PREPARE prp_sqlPreliquidacionEnaclara FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacionEnaclara USING p_folio,p_usuario
   INTO v_cod_error, v_error_isam, v_mensaje_error, v_id_derechohabiente_error

   IF(v_cod_error = 0)THEN
   	  DISPLAY v_mensaje_error
      DISPLAY v_cod_error
      # Ejecucion sin error
      RETURN FALSE
   ELSE
   	  DISPLAY "Error de ejecuci�n en 'sp_preliquida_enaclara_nss' (C�digo): ", v_cod_error
      DISPLAY "Error en 'sp_preliquida_enaclara_nss' (Mensaje):", v_mensaje_error
      DISPLAY "Error en 'sp_preliquida_enaclara_nss' (ISAM)   :", v_error_isam
      DISPLAY "ID_DERECHOHABIENTE                             :", v_id_derechohabiente_error
      RETURN TRUE
   END IF
END FUNCTION