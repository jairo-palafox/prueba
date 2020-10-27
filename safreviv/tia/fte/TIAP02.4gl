--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#############################################################################
#Módulo          => TIA                                                     #
#Programa        => TIAP02.4gl                                              #
#Objetivo        => Programa de preliquidación de Traspasos I-A             #
#Fecha Inicio    => 26 Marzo 2012                                           #
#############################################################################
DATABASE safre_viv

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
       v_descripcion         CHAR(150) ,
       p_programa_cod        VARCHAR(10)

   -- se reciben los parametros del programa
   LET p_usuario_cod              = ARG_VAL(1)
   LET p_pid                      = ARG_VAL(2)
   LET p_proceso_cod              = ARG_VAL(3)
   LET p_opera_cod_preliquidacion = ARG_VAL(4)
   LET p_folio                    = ARG_VAL(5)
   LET p_nom_archivo              = ARG_VAL(6)
   
   -- se inicia el log del proceso 
   CALL STARTLOG(p_usuario_cod CLIPPED||".TIAP02.log")
   
   -- se muestra la informacion del proceso
   CALL fn_display_proceso(0,"Traspasos I-A")
   
   -- se invoca la ejecucion de la preliquidacion
   CALL fn_ejecuta_preliquidacion_traspasos(p_folio,p_usuario_cod)
        RETURNING r_resultado_opera
   
   -- se hubo error
   IF ( r_resultado_opera ) THEN
      LET p_titulo = "Error de operación - Traspasos I-A - Preliquidación"

      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
           RETURNING r_resultado_opera 
   ELSE
   
      -- el proceso de preliquidacion termino correctamente
      CALL fn_display_proceso(1,"Traspasos I-A")
      
      -- Se finaliza la carga de registros para preliquidacion
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
           RETURNING r_resultado_opera
            
      -- verifica si hubo error al cerrar la operacion
      IF ( r_resultado_opera ) THEN
         DISPLAY "Ocurrió un error al finalizar la operacion."
   
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod_preliquidacion) 
              RETURNING r_resultado_opera 
      ELSE
        -- se obtiene el codigo de programa
        SELECT programa_cod
        INTO   p_programa_cod
        FROM   cat_operacion
        WHERE  proceso_cod = p_proceso_cod
        AND    opera_cod = p_opera_cod_preliquidacion
                  
        -- Se manda llamar la función que ejecuta el reporte de liquidación
        CALL fn_reporte_liquidacion(p_folio, "tia_preliquida",
                                  p_usuario_cod, p_pid, p_proceso_cod,
                                  p_opera_cod_preliquidacion, p_programa_cod,
                                  FALSE)      
         LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
         LET p_titulo = "Finalización de operación - Traspasos I-A - Preliquidación"
      END IF
      
      -- se envia correo de finalizacion de proceso
      CALL fn_correo_proceso(p_pid, p_proceso_cod, 
                          p_opera_cod_preliquidacion, 
                          NULL, p_titulo,p_mensaje)
   END IF     

END MAIN

{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_traspasos
Nombre: fn_ejecuta_preliquidacion_traspasos
Fecha creacion: 26 de Marzo de 2012
Autor: Ilhuitemoc Ricardo Ortiz
Narrativa del proceso que realiza:
 Esta función executa el store procedure que almacena la información 
 de la preliquidación para el módulo de Traspasos I-A
 Parametros de Entrada:
 -
 Parámetros de salida;
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_ejecuta_preliquidacion_traspasos(p_folio,p_usuario)
DEFINE p_folio              LIKE glo_folio.folio,
       p_usuario            CHAR(20),
       v_sql_procedure      STRING,
       v_s_SqlQry           STRING ,
       v_error_code         INTEGER,
       v_error_isam         INTEGER,
       v_mensaje            VARCHAR(250),
       v_i_contador_gral    INTEGER, --para el conteo de datos generales
       v_i_contador_excp    INTEGER, --para el conteo de datos en esXcpeción
       v_i_contador_total   INTEGER, --para el conteo de datos en exec´pción - general

       v_s_comando STRING,                         #COMANDO PARA EJECUTAR EL PROGRAMA TIAP04
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin

          SELECT ruta_bin
            INTO v_ruta_ejecutable
            FROM seg_modulo
           WHERE modulo_cod = 'tia'

   --se asigann los contadores a 0    
   LET v_i_contador_gral  = 0
   LET v_i_contador_excp  = 0
   LET v_i_contador_total = 0
  
   LET v_s_SqlQry = "\n SELECT COUNT(*)",
                    "\n FROM tia_det_traspaso",
                    "\n WHERE  folio = ",p_folio
   
   --DISPLAY " v_s_SqlQry =  ",v_s_SqlQry
   PREPARE prp_conteo_gral FROM v_s_SqlQry
   EXECUTE prp_conteo_gral INTO v_i_contador_gral

   LET v_s_SqlQry = "\n SELECT COUNT(*)",
                    "\n   FROM tia_det_traspaso  ",
                    "\n  WHERE folio = ",p_folio ,
                    "\n    AND result_operacion  = '02' "

   --DISPLAY " v_s_SqlQry =  ",v_s_SqlQry
   PREPARE prp_conteo_excep FROM v_s_SqlQry
   EXECUTE prp_conteo_excep INTO v_i_contador_excp

   LET v_i_contador_total = v_i_contador_gral - v_i_contador_excp

   IF ( v_i_contador_total = 0 ) THEN 
     	DISPLAY "No es posible ejecutar la preliquidación. Todos los registros del folio ", p_folio, " están rechazados."
      -- se actualiza el folio a preliquidado
      LET v_s_SqlQry = "UPDATE glo_folio SET status = 1 WHERE folio = ", p_folio
      --DISPLAY v_s_SqlQry, p_folio
      EXECUTE IMMEDIATE v_s_SqlQry
     	RETURN TRUE
   ELSE
      -- se prepara y ejecuta el SP de preliquidacion de TIA
      LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_compara_tia(?,?)"
    	
      PREPARE prp_sqlPreliquidacionTraspasos FROM v_sql_procedure
      
      EXECUTE prp_sqlPreliquidacionTraspasos USING p_folio,p_usuario
              INTO v_error_code, v_error_isam, v_mensaje
    
      IF ( v_error_code = 0 ) THEN
         DISPLAY "Preliquidación finalizada correctamente."


   LET v_s_comando = "fglrun ",v_ruta_ejecutable CLIPPED, #COMANDO PARA EJECUTAR EL PROGRAMA TIAP04
                    "/TIAP04 ", p_usuario," ", p_folio
   RUN v_s_comando

         DISPLAY "Se genero el archivo de Excepciones."   #MENSAJE DE QUE SE EJECUTO BIEN EL PROBRAMA TIAP04
         
         RETURN FALSE
      ELSE
         DISPLAY "Error en sp_preliquida_compara_tia (Codigo) :" , v_error_code
         DISPLAY "Error en sp_preliquida_compara_tia (ISAM)   :" , v_error_isam
         DISPLAY "Error en sp_preliquida_compara_tia (Mensaje):" , v_mensaje
         RETURN TRUE
      END IF
   END IF  
END FUNCTION