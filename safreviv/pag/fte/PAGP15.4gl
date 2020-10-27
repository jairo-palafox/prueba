--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#############################################################################
#Módulo          => PAG                                                     #
#Programa        => PAGP15                                                  #
#Objetivo        => Programa de preliquidación de para el módulo de         #
#                   "Sólo infonavit"                                        #
#Fecha Inicio    => 11 ENERO 2012                                           #
#############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   g_opera_cod_carga           LIKE cat_operacion.opera_cod, -- codigo de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

#Objetivo:
MAIN
DEFINE p_folio        DECIMAL(9),
       p_nom_archivo  STRING,
       v_usuario      CHAR(20),
       v_estatus      SMALLINT,
       p_titulo       STRING, -- titulo del mensaje enviado en el correo
       p_mensaje      STRING, -- cuerpo del mensaje enviado
       p_programa_cod VARCHAR(10)

   #Si se ha recibido parámetros se continua    
   IF(NUM_ARGS() > 0)THEN
      #Primer parámetro
      LET p_usuario_cod = ARG_VAL(1)
      #Segundo parámetro
      LET g_pid         = ARG_VAL(2)
      #Tercer parámetro
      LET g_proceso_cod = ARG_VAL(3)
      #Tercer parámetro
      LET g_opera_cod_preliquidacion = ARG_VAL(4)
      #Quinto parámetro
      LET p_folio = ARG_VAL(5)
      #Segundo parámetro
      LET p_nom_archivo = ARG_VAL(6)

      -- se registra el inicio de la operacion
      --Inicia la Preliquidacion
      --LET g_opera_cod_preliquidacion = 3 --Preliquidacion
      
         -- se lleva esta funcion al lanzador 25 Mayo 2012         
         --Se registra el inicio de la operacion
         --Se inicia la operación en el monitor
         {CALL fn_actualiza_opera_ini(g_pid
                                    ,g_proceso_cod
                                    ,g_opera_cod_preliquidacion
                                    ,p_folio
                                    ,"PAGP15"
                                    ,p_nom_archivo
                                    ,p_usuario_cod
                                    ) RETURNING v_estatus
         }   
         CALL fn_display_proceso(0,"PRELIQUIDACIÓN")
         
         #Llamada a ejecución de procedimiento almacenado
         CALL fn_sp_preliquida_sinf(p_folio,v_usuario) RETURNING v_estatus

         IF v_estatus = 0 THEN
         
            SELECT programa_cod
              INTO p_programa_cod
              FROM cat_operacion
             WHERE proceso_cod = g_proceso_cod
               AND opera_cod = g_opera_cod_preliquidacion
               
            --DISPLAY "llamad a función de rewporte general :"
            --Se manda llamar la función que ejecuta el reporte de liquidación
            CALL fn_reporte_liquidacion(p_folio, "pag_preliquida",
                                        p_usuario_cod, g_pid, g_proceso_cod,
                                        g_opera_cod_preliquidacion, p_programa_cod,
                                        FALSE)

         
            --Se registra el FIN DE LA OPERACION COMO EXITOSA
            -- se invoca la finalizacion de la operacion
            CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
                                        g_proceso_cod, --- Clave del proceso
                                        g_opera_cod_preliquidacion) --- Clave de la operación
                                        RETURNING v_estatus
            LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
         ELSE
            --Si ocurrio un error se actualiza el estatus como erroneo
            CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_preliquidacion)  RETURNING v_estatus
            LET p_mensaje = "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Preliquidación." 
         END IF
         --Se ejecutan los display
         CALL fn_display_proceso(1,"PRELIQUIDACIÓN") 
         LET p_titulo = "Finalización de operación - SOLO-INFONAVIT - Preliquidación"
         CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                             g_opera_cod_preliquidacion, 
                             NULL, p_titulo,p_mensaje)
      ELSE
         CALL fn_desplega_inc_operacion(v_estatus)
      END IF
   
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Nombre: fn_sp_preliquida_sinf
Fecha creacion: 11 de Enero de 2012
Narrativa del proceso que realiza:
Esta función ejecuta el store procedure que lleva a cabo la 
preliquidacion para el módulo de "Solo INFONAVIT"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_sp_preliquida_sinf(p_folio,p_usuario)
DEFINE p_folio          DECIMAL(9),
       p_usuario        CHAR(20),
       v_sql_procedure  STRING,
       v_estatus        SMALLINT,
       v_resultado      SMALLINT,
       v_error_isam     SMALLINT,
       v_mensaje_error  VARCHAR(255)

       
   WHENEVER SQLERROR CONTINUE
   
   LET v_estatus = 0 --El cero indica que se jecuto con exito
   
   LET v_sql_procedure = "EXECUTE PROCEDURE safre_viv:sp_preliquida_sinf(?,?)"
   PREPARE prp_sp_preliquida_sinf FROM v_sql_procedure
   EXECUTE prp_sp_preliquida_sinf USING p_folio,p_usuario
   INTO v_resultado, v_error_isam, v_mensaje_error
    
   -- se verifica el resultado de la consulta
   IF ( v_resultado <> 0 ) THEN
      LET v_estatus = 1  --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "Error de ejecución en 'sp_preliquida_sinf' (Código): ", v_resultado
      DISPLAY "Error en 'sp_preliquida_sinf' (Mensaje):", v_mensaje_error
   END IF
   
   WHENEVER SQLERROR STOP

   -- se devuelve el resultado de la ejecucion
   RETURN v_estatus
END FUNCTION