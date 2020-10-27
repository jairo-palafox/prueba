--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#############################################################################
#Módulo          => PAG                                                     #
#Programa        => PAGP01.4gl                                              #
#Objetivo        => Programa de preliquidación de registro de pagos         #
#Fecha Inicio    => 09 ENERO 2012                                           #
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
      LET g_opera_cod_preliquidacion = ARG_VAL(4)  -- Preliquidación 
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
      --LET g_opera_cod_preliquidacion = 3
      --Se registra el inicio de la operacion

      
         -- se lleva esta funcion al lanzador 25 Mayo 2012         
         --Se registra el inicio de la operacion
         {CALL fn_actualiza_opera_ini(g_pid
                                    ,g_proceso_cod
                                    ,g_opera_cod_preliquidacion
                                    ,p_num_folio
                                    ,"PAGP11"
                                    ,p_nom_archivo
                                    ,p_usuario_cod)
                                    RETURNING v_estatus
          }

         --Se ejecutan los displays
         CALL fn_display_proceso(0,"PRELIQUIDACIÓN")
         #Llamada a ejecución de procedimiento almacenado
         CALL fn_ejecuta_preliquidacion(p_num_folio,p_usuario_cod) RETURNING v_estatus

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
            CALL fn_reporte_liquidacion(p_num_folio, "pag_lqinfo_preliquida",
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
         LET p_titulo = "Finalización de operación - LQINFO - Preliquidación"
         CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                             g_opera_cod_preliquidacion, 
                             NULL, p_titulo,p_mensaje)
   END IF
   
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion
Nombre: fn_ejecuta_preliquidacion
Fecha creacion: 10 de Enero de 2012
Autor: Hugo César Ramirez Garcia
Narrativa del proceso que realiza:
Esta función executa el store procedure que almacena la información 
de los registros históricos para el módulo de "Registro de pagos"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     23 Sept 2013            - Se agrega el ID derechohabiente a los
                                        valores de regreso de ejecucion del SP
============================================================================}
FUNCTION fn_ejecuta_preliquidacion(p_folio_sua,p_usuario)
DEFINE p_folio_sua          LIKE pag_cza_pag_patronal.folio_sua,
       p_usuario            CHAR(20),
       v_sql_procedure      STRING,
       v_estatus            SMALLINT,
	     v_error              SMALLINT,
	     isam_err             INTEGER ,
       err_txt              VARCHAR(255),
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente

   -- se prepara la ejecuicion del SP
   LET v_estatus = 0 --El cero indica que se jecuto con exito
   
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_lqinfo(?,?)"
   
   PREPARE prp_sqlPreliquidacion FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacion 
   INTO v_error, isam_err, err_txt, v_id_derechohabiente
   USING p_folio_sua,p_usuario   
   
  IF(v_error = 0)THEN
   	  DISPLAY v_error
   	  DISPLAY isam_err
      DISPLAY err_txt
      # Ejecucion sin error
      RETURN FALSE
   ELSE
      DISPLAY "\nError ejecucion sp_preliquida_lqinfo (ISAM): ", isam_err
      DISPLAY "\nError ejecucion sp_preliquida_lqinfo (SLQ) : ", v_error
      DISPLAY "\nMensaje                                    : ", err_txt
      DISPLAY "\nID Derechohabiente                         : ", v_id_derechohabiente
      RETURN TRUE
   END IF

END FUNCTION