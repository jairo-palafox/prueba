--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/11/2013
--===============================================================

#############################################################################
#Módulo          => PAG                                                     #
#Programa        => PAGP75.4gl                                              #
#Objetivo        => Programa que genera reporte de preliquidacion lqinfo    #
#Fecha Inicio    => 28 Noviembre 2013                                       #
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
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ruta_vacia      LIKE seg_modulo.ruta_bin,
       p_programa_cod    VARCHAR(10)

      #Primer parámetro
      LET p_usuario_cod = "safreviv"
      #Segundo parámetro
      LET g_pid         = 11262
      #Tercer parámetro
      LET g_proceso_cod = 1401
      #Cuarto parámetro
      LET g_opera_cod_preliquidacion = 3  -- Preliquidación 
      #Quinto parámetro
      LET p_num_folio = 19300
      #Segundo parámetro
      LET p_nom_archivo = "20131121C841.lqinfo"

      SELECT ruta_listados,
             ruta_bin
      INTO   v_ruta_listados,
             v_ruta_vacia
      FROM   seg_modulo
      where modulo_cod = "pag"
      
      CALL fn_rutas('bat') RETURNING v_ruta_vacia, v_ruta_listados          
      CALL STARTLOG(v_ruta_listados||
                    "/nohup:"||
                    g_pid USING "&&&&&"||
                    g_proceso_cod USING "&&&&&"||
                    g_opera_cod_preliquidacion USING "&&&&&"
                   )
      
         	
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

END MAIN

