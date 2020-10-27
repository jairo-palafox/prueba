--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#############################################################################
#M�dulo          => PAG                                                     #
#Programa        => PAGP12.4gl                                              #
#Objetivo        => Programa de registro de informaci�n hist�rica para el   #
#                   m�dulo de  "Recaudaci�n SAR 92"                         #
#Fecha Inicio    => 11 ENERO 2012                                           #
#############################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, # C�digo del proceso
       g_opera_cod_integracion     LIKE cat_operacion.opera_cod # C�digo de operaci�n
       --g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  # C�digo de operaci�n
       
END GLOBALS

#Objetivo:
MAIN
DEFINE p_opera_cod_carga LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_usuario_cod     LIKE seg_usuario.usuario_cod, -- Clave de usuario
       p_nom_archivo     STRING,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_listados   LIKE seg_modulo.ruta_listados,
       r_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT,
       r_ruta_vacia      LIKE seg_modulo.ruta_bin,
       r_bnd_estado      SMALLINT,
       p_titulo              STRING, -- titulo del mensaje enviado en el correo
       p_mensaje             STRING, -- cuerpo del mensaje enviado
       v_descripcion CHAR(150)

   --WHENEVER ERROR CONTINUE
   LET r_bnd_estado = 0 # Inicializa bandera
   
   #Si se ha recibido par�metros se continua    
   #Primer par�metro
   LET p_usuario_cod = ARG_VAL(1)
   #Segundo par�metro
   LET g_pid         = ARG_VAL(2)
   #Tercer par�metro
   LET g_proceso_cod = ARG_VAL(3)
   #Cuarto par�metro
   LET g_opera_cod_integracion = ARG_VAL(4) # Paso de informaci�n a las tablas hist�ricas
   #Quinto par�metro
   LET r_folio       = ARG_VAL(5)
   #Segundo par�metro
   LET p_nom_archivo = ARG_VAL(6)
   # Indicamos operacion de carga
   LET p_opera_cod_carga = 1
   # Archivo de bitacora
   CALL STARTLOG(p_usuario_cod CLIPPED||".PAGP12.log")   
   CALL fn_display_proceso(0,"INTEGRACI�N SAR92")
   # Genera folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod_integracion, p_usuario_cod)
                        RETURNING r_folio
   # Recupera el nombre del archivo cargado
   CALL fn_recupera_arch_cargado(g_proceso_cod,p_opera_cod_carga)
                     RETURNING p_nom_archivo

   # Valida si se puede actualizar la operaion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod_integracion)
                          RETURNING r_resultado_opera

      LET r_resultado_opera = 0
      {# Se registra el inicio de la operacion
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod_integracion,r_folio,
                                  "PAGP12",p_nom_archivo,p_usuario_cod) 
                     RETURNING r_resultado_opera
      # Valida si ocurri� una inconsistencia 
      IF(r_resultado_opera)THEN
         # Muestra el mensaje de la inconsistencia
         CALL fn_desplega_inc_operacion(r_resultado_opera)
         LET p_titulo = "Error de operaci�n - SAR92 - Integraci�n"
         SELECT descripcion
           INTO v_descripcion 
           FROM cat_bat_parametro_salida
          WHERE cod_salida = r_resultado_opera
         LET p_mensaje = v_descripcion
      ELSE
      }

         # Llamada a ejecuci�n de procedimiento almacenado
         CALL fn_guarda_historicos_sar92(r_folio) RETURNING r_resultado_opera         
        
         # cambia a estado errone si no se ejecut� correctamente el SP
         IF(r_resultado_opera)THEN
         	
            LET p_titulo = "Error de operaci�n - SAR92 - Integraci�n"
            SELECT descripcion
              INTO v_descripcion 
              FROM cat_bat_parametro_salida
             WHERE cod_salida = r_resultado_opera
            LET p_mensaje = v_descripcion
            
            CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod_integracion) 
                     RETURNING r_resultado_opera 
            IF(r_resultado_opera)THEN
            	
               # Muestra el mensaje de la inconsistencia
               CALL fn_desplega_inc_operacion(r_resultado_opera)
               IF(r_resultado_opera)THEN
                  # Muestra el mensaje de la inconsistencia
                  CALL fn_desplega_inc_operacion(r_resultado_opera)
               --ELSE
               --   # actualiza el estado del archivo a reversado, para poder cargar el mismo
               --   CALL fn_act_edo_archivo(p_nom_archivo,r_folio,3,p_usuario_cod)
               --                        RETURNING r_bnd_estado 
               END IF 
            END IF
            
         ELSE
            LET r_resultado_opera = 0
            CALL fn_display_proceso(1,"INTEGRACI�N SAR92")
            
             # Actualiza el estado del archivo en glo_ctr_archivo a integrado
               # 2 = integrado
               CALL fn_act_edo_archivo(p_nom_archivo,r_folio,2,p_usuario_cod)
                                     RETURNING r_resultado_opera
            
            # Se finaliza la carga de registros historicos
            CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod_integracion) 
                               RETURNING r_resultado_opera
                               
            # Valida si ocurri� una inconsistencia 
            IF(r_resultado_opera)THEN
               LET p_titulo = "Error de operaci�n - SAR92 - Integraci�n"
               SELECT descripcion
                 INTO v_descripcion 
                 FROM cat_bat_parametro_salida
                WHERE cod_salida = r_resultado_opera
               LET p_mensaje = v_descripcion
               
               # Muestra el mensaje de la inconsistencia
               CALL fn_desplega_inc_operacion(r_resultado_opera)
               CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod_integracion) 
                         RETURNING r_resultado_opera

               IF(r_resultado_opera)THEN
                  # Muestra el mensaje de la inconsistencia
                  CALL fn_desplega_inc_operacion(r_resultado_opera)
               --ELSE
               --   # actualiza el estado del archivo a reversado, para poder cargar el mismo
               --   CALL fn_act_edo_archivo(p_nom_archivo,r_folio,3,p_usuario_cod)
               --                           RETURNING r_bnd_estado 
               END IF
            ELSE
            
               LET p_mensaje = "Integraci�n realizada con �xito.\nYa se puede continuar con la Preliquidaci�n."
               # Se obtienen las variables para invocar el procedure 
               # que almacena la inf. en tablas hist�ricas
               CALL fn_rutas("pag") RETURNING r_ruta_ejecutable, r_ruta_vacia
               CALL fn_rutas("bat") RETURNING r_ruta_vacia, r_ruta_listados

               # Valida si ocurri� una inconsistencia 
               IF(r_resultado_opera)THEN
                  LET p_titulo = "Error de operaci�n - SAR92 - Integraci�n"
                  SELECT descripcion
                    INTO v_descripcion 
                    FROM cat_bat_parametro_salida
                   WHERE cod_salida = r_resultado_opera
                  LET p_mensaje = v_descripcion
                  
                  # Muestra el mensaje de la inconsistencia
                  CALL fn_desplega_inc_operacion(r_resultado_opera)

                  CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod_integracion) 
                            RETURNING r_resultado_opera 
                            
                              DISPLAY " fn_error_opera =   ",r_resultado_opera
                  LET p_mensaje = "El proceso de Integraci�n ha finalizado pero con errores.\nNo se puede continuar con el proceso de Preliquidaci�n."
                  IF(r_resultado_opera)THEN
                     # Muestra el mensaje de la inconsistencia
                     CALL fn_desplega_inc_operacion(r_resultado_opera)
                  ELSE
                     # actualiza el estado del archivo a reversado, para poder cargar el mismo
                     CALL fn_act_edo_archivo(p_nom_archivo,r_folio,3,p_usuario_cod)
                                             RETURNING r_bnd_estado 
                  END IF
                  
               ELSE
                 LET p_titulo = "Finalizaci�n de operaci�n - SAR92 - Integraci�n"
               END IF
            END IF
         END IF
      --END IF
      CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                             g_opera_cod_integracion, 
                             NULL, p_titulo,p_mensaje)
            
     
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidaci�n
{ ==========================================================================
Clave:  fn_guarda_historicos_sar92
Nombre: fn_guarda_historicos_sar92
Fecha creacion: 10 de Enero de 2012
Autor: Hugo C�sar Ram�rez Garc�a
Narrativa del proceso que realiza:
 Esta funci�n ejecuta el store procedure que almacena la informaci�n 
 de los registros hist�ricos para el m�dulo de "Recaudaci�n SAR 92"
 Parametros de Entrada:
 -
 Par�metros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_guarda_historicos_sar92(p_folio)
DEFINE p_folio          LIKE glo_folio.folio,
       v_sql_procedure  STRING,
       v_cod_error      SMALLINT,
       v_error_isam     INTEGER,
       v_mensaje_error  VARCHAR(255),
       v_det_id_unico   DECIMAL(11,0)
       

   LET v_sql_procedure = "EXECUTE PROCEDURE sp_registro_historicos_sar92(?,?,?)"
   --WHENEVER ERROR CONTINUE
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_historicoSAR FROM v_sql_procedure
   EXECUTE prp_historicoSAR USING p_folio,g_proceso_cod, g_pid
   INTO v_cod_error, v_error_isam, v_mensaje_error, v_det_id_unico
   
   IF(v_cod_error = 0)THEN
      # Ejecucion sin error
      DISPLAY v_cod_error
   	  DISPLAY v_error_isam
      DISPLAY v_mensaje_error
      --DISPLAY "Id_unico: ", v_det_id_unico
      RETURN FALSE
   ELSE
      DISPLAY "\nError ejecucion sp_registro_historicos_sar92 (Codigo): ",v_error_isam
      DISPLAY "\nError ejecucion sp_registro_historicos_sar92 (Codigo): ",v_cod_error
      DISPLAY "Error en 'sp_registro_historicos_pag' (Mensaje): ",v_mensaje_error,"\n"
      DISPLAY "ID_UNICO: ", v_det_id_unico
      RETURN TRUE
   END IF
   
END FUNCTION