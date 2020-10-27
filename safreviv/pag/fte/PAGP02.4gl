--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/06/2012
--===============================================================

#############################################################################
#Módulo          => PAG                                                     #
#Programa        => PAGP02.4gl                                              #
#Objetivo        => Programa de preliquidación de FORTALECIMIENTO DE CRÉDITO         #
#Fecha Inicio    => 19 de JUNIO 2012                                           #
#############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   g_opera_cod_carga           LIKE cat_operacion.opera_cod, -- codigo de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   --g_opera_cod_integracion     LIKE cat_operacion.opera_cod, -- codigo de operacion
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

#Objetivo:
MAIN
DEFINE p_num_folio       DECIMAL(9),
       p_usuario_cod     CHAR(20),
       p_nom_archivo     STRING  ,
       v_estatus         SMALLINT,
       --V_QUERY           STRING,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ruta_vacia      LIKE seg_modulo.ruta_bin,
       p_titulo          STRING, -- titulo del mensaje enviado en el correo
       p_mensaje         STRING,-- cuerpo del mensaje enviado   
       p_programa_cod    VARCHAR(10)

   #Si se ha recibido parámetros se continua    
   IF(NUM_ARGS() > 0)THEN
      #Primer parámetro
      LET p_usuario_cod = ARG_VAL(1)
      #Segundo parámetro
      LET g_pid         = ARG_VAL(2)
      #Tercer parámetro
      LET g_proceso_cod = ARG_VAL(3)
      #Cuarto parámetro
      LET g_opera_cod_preliquidacion = ARG_VAL(4)  --Preliquidación 
                                                
      #Quinto parámetro
      LET p_num_folio = ARG_VAL(5)
      #Segundo parámetro
      LET p_nom_archivo = ARG_VAL(6)
      CALL fn_rutas('bat') RETURNING v_ruta_vacia, v_ruta_listados          
      CALL STARTLOG(v_ruta_listados||
                    "/nohup:"||
                    g_pid USING "&&&&&"||
                    g_proceso_cod USING "&&&&&"||
                    g_opera_cod_preliquidacion USING "&&&&&"
                   )
      
      -- se registra el inicio de la operacion
      --Inicia la Preliquidacion
      LET g_opera_cod_preliquidacion = 3
      --Se registra el inicio de la operacion

         --Se ejecutan los displays
         CALL fn_display_proceso(0,"PRELIQUIDACIÓN")
         #Llamada a ejecución de procedimiento almacenado
         CALL fn_ejecuta_preliquidacion_fc(p_num_folio,p_usuario_cod) RETURNING v_estatus

         IF v_estatus = 0 THEN
         	
         	  --DISPLAY "entra despues de la  preliquidació Error en 'sp_preliquida_lqinfo' (Mensaje):",v_estatus
         	  --se obtiene el codigo de programa
            SELECT programa_cod
              INTO p_programa_cod
              FROM cat_operacion
             WHERE proceso_cod = g_proceso_cod
               AND opera_cod = g_opera_cod_preliquidacion
               
            --DISPLAY "llamad a función de rewporte general :"
            --Se manda llamar la función que ejecuta el reporte de liquidación
            CALL fn_reporte_liquidacion(p_num_folio, "pag_preliquida",
                                        p_usuario_cod, g_pid, g_proceso_cod,
                                        g_opera_cod_preliquidacion, p_programa_cod,
                                        FALSE)
             --Se registra el FIN DE LA OPERACION COMO EXITOSA

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
         LET p_titulo = "Finalización de operación - FORTALECIMIENTO DE CRÉDITO - Preliquidación"
         CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                             g_opera_cod_preliquidacion, 
                             NULL, p_titulo,p_mensaje)
   END IF
   
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_fc
Nombre: fn_ejecuta_preliquidacion_fc
Fecha creacion: 19  de Junio de 2012
Autor: Rubén Haro Castro
Narrativa del proceso que realiza:
Esta función executa el store procedure que almacena la información 
de los registros históricos para el módulo de "Registro de pagos - Fortalecimiento de Crédito"

Parametros de Entrada:
Parámetros de salida;-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_ejecuta_preliquidacion_fc(p_folio,p_usuario)
DEFINE p_folio          LIKE pag_cza_pag_patronal.folio_sua,
       p_usuario        CHAR(20),
       v_sql_procedure  STRING,
       v_estatus        SMALLINT,
       v_cod_error      SMALLINT,
       v_error_isam     INTEGER,
       v_mensaje_error  VARCHAR(255)
       

   --WHENEVER ERROR CONTINUE
   LET v_estatus = 0 --El cero indica que se jecuto con exito
   
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_fc(?,?)"
   PREPARE prp_sqlPreliquidacion FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacion USING p_folio,p_usuario  
   INTO v_cod_error, v_error_isam, v_mensaje_error 
   
   IF (v_cod_error = 0) THEN
   	  DISPLAY v_mensaje_error
      # Ejecucion sin error
      RETURN FALSE
   ELSE 	
      LET v_estatus = 1  --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "\nError de ejecución en 'sp_preliquida_fc' (Código): ",v_cod_error
      DISPLAY "Error en 'sp_preliquida_fc' (Mensaje):",v_mensaje_error,"\n"
      DISPLAY "Error en 'sp_preliquida_fc' (Error):",v_error_isam,"\n"
   END IF
   --WHENEVER ERROR STOP
   RETURN v_estatus
END FUNCTION