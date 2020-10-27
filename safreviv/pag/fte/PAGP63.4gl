--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/04/2012
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGP63                                                        #
#Objetivo     => Programa batch del proceso de pleliquidacion de registro de   #
#                pagos de aportacion voluntaria                                #
#Fecha inicio => 10 Abril de 2013                                              #
################################################################################

DATABASE safre_viv

DEFINE g_pid                       LIKE bat_ctr_proceso.pid,     #  ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, # codigo del proceso
       g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  # codigo de operacion

MAIN
DEFINE p_num_folio     DECIMAL(9),
       p_usuario_cod   CHAR(20),
       p_nom_archivo   STRING  ,
       v_error         SMALLINT,
       v_ruta_listados LIKE seg_modulo.ruta_listados,
       v_ruta_vacia    LIKE seg_modulo.ruta_bin,
       p_titulo        STRING, # titulo del mensaje enviado en el correo
       p_mensaje       STRING, # cuerpo del mensaje enviado   
       p_programa_cod  VARCHAR(10)

   # Si se ha recibido parámetros se continua    
   IF ( NUM_ARGS() > 0 ) THEN
    
      LET p_usuario_cod              = ARG_VAL(1)
      LET g_pid                      = ARG_VAL(2)
      LET g_proceso_cod              = ARG_VAL(3)
      LET g_opera_cod_preliquidacion = ARG_VAL(4)
      LET p_num_folio                = ARG_VAL(5)
      LET p_nom_archivo              = ARG_VAL(6)

      CALL fn_rutas('bat') RETURNING v_ruta_vacia, v_ruta_listados          

      {CALL STARTLOG(v_ruta_listados||
                    "/nohup:"||
                    g_pid USING "&&&&&"||
                    g_proceso_cod USING "&&&&&"||
                    g_opera_cod_preliquidacion USING "&&&&&"
                   )}
      
      # Se ejecutan los displays
      CALL fn_display_proceso(0,"PRELIQUIDACIÓN")

      # Llamada a ejecución de procedimiento almacenado
      CALL fn_ejecuta_preliquidacion_aportacion_voluntaria(p_num_folio,p_usuario_cod) RETURNING v_error

      IF ( v_error = 0 ) THEN
         	
         # se obtiene el codigo de programa
         SELECT programa_cod
           INTO p_programa_cod
           FROM cat_operacion
          WHERE proceso_cod = g_proceso_cod
            AND opera_cod = g_opera_cod_preliquidacion
               
         # Se manda llamar la función que ejecuta el reporte de liquidación
         CALL fn_reporte_liquidacion(p_num_folio, 
                                     "pag_preliquida",
                                     p_usuario_cod, 
                                     g_pid, 
                                     g_proceso_cod,
                                     g_opera_cod_preliquidacion, 
                                     p_programa_cod,
                                     FALSE)
                                     
         # Se registra el FIN DE LA OPERACION COMO EXITOSA
         CALL fn_actualiza_opera_fin(g_pid,
                                     g_proceso_cod,
                                     g_opera_cod_preliquidacion) RETURNING v_error
         LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
      ELSE
         # Si ocurrio un error se actualiza el estatus como erroneo
         CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_preliquidacion)  RETURNING v_error
         LET p_mensaje = "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación."
      END IF
      
      # Se ejecutan los displays
      CALL fn_display_proceso(1,"PRELIQUIDACIÓN")
      
      LET p_titulo = "Finalización de operación - FONDO ANTERIOR - Preliquidación"

      CALL fn_correo_proceso(g_pid, 
                             g_proceso_cod, 
                             g_opera_cod_preliquidacion, 
                             NULL, 
                             p_titulo,
                             p_mensaje)
   END IF
   
END MAIN

{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_aportacion_voluntaria
Nombre: fn_ejecuta_preliquidacion_aportacion_voluntaria
Fecha creacion: 11 Abril 2013     
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
Ejecuta el Stored Procedure que realiza el proceso de preliquidacion de
Registro de Pagos de aportaciones voluntarias

Parametros de Entrada:
Parámetros de salida:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_ejecuta_preliquidacion_aportacion_voluntaria(p_folio,p_usuario)
DEFINE p_folio         LIKE glo_folio.folio,
       p_usuario       CHAR(20),
       v_consulta      STRING,
       v_error         SMALLINT,
       v_cod_error     SMALLINT,
       v_error_isam    INTEGER,
       v_mensaje_error VARCHAR(255)
       
   # se asume que se finaliza sin errores
   LET v_error = 0 
   
   LET v_consulta = "EXECUTE PROCEDURE sp_preliquida_vol(?,?)"
   PREPARE prp_sqlPreliquidacion FROM v_consulta
   EXECUTE prp_sqlPreliquidacion USING p_folio,
                                       p_usuario
                                       
                                  INTO v_cod_error, 
                                       v_error_isam, 
                                       v_mensaje_error 

   # no hubo error
   IF (v_cod_error = 0) THEN
   	  DISPLAY v_mensaje_error
   ELSE 	
      # hubo un error en la preliquidacion
      LET v_error = 1  #El uno indca que ocurrio un error al ejecutarse
      DISPLAY "Error (SQL)    : ", v_cod_error
      DISPLAY "Error (ISAM)   : ", v_error_isam
      DISPLAY "Error (Mensaje): ", v_mensaje_error
   END IF

   # se devuelve el resultado de la ejecucion
   RETURN v_error
END FUNCTION