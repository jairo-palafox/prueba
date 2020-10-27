--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/04/2013
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEP02                                                   #
#Objetivo          => Programa que ejecuta el stored procedure que realiza la  #
#                     preliquidación de Devolución de Amortizaciones Excedentes#
#Fecha inicio      => 19/04/2012                                               #
################################################################################

--LANZADOR: DAEL03

DATABASE safre_viv
GLOBALS "DAEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_i_resultado    INTEGER, -- resultado del proceso
       r_bnd_fin_oper SMALLINT,
      --
       v_i_total_registros_encontrados INTEGER,
       p_titulo            STRING,
       p_mensaje           STRING,
       v_layout            LIKE cat_operacion.layout_cod,
       v_ruta_rescate      STRING,
       v_usuario           LIKE seg_modulo.usuario,
       v_proceso_desc      LIKE cat_proceso.proceso_desc,
       v_extension         LIKE cat_operacion.extension,
       v_opera_desc        LIKE cat_operacion.opera_desc,
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_folio_liquida     LIKE dpe_preliquida.folio_liquida, -- folio de la liquidación
       v_ejecuta           SMALLINT,
       p_folio_dictamen    DECIMAL(9,0),
       v_folio_dictamen    DECIMAL(9,0)
   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_folio_dictamen = ARG_VAL(7)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,
                                         v_extension, 
                                         v_opera_desc,
                                         v_layout, 
                                         v_ruta_rescate,
                                         v_ruta_listados,
                                         v_usuario
      
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = 2402
   LET g_opera_cod   = 2
   LET v_ejecuta     = 1
   LET v_folio_dictamen = p_folio_dictamen
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_dae_preliquida_dictamen(?, ?, ?, ?, ?)"
   
   -- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidadeo FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_preliquidadeo USING p_folio, 
                                   p_usuario_cod, 
                                   g_pid,
                                   v_ejecuta,
                                   v_folio_dictamen 
                              INTO v_i_resultado, 
                                   v_i_total_registros_encontrados, 
                                   v_folio_liquida

   -- Se finaliza operacion aunque no se termine correctamente el error
   DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   DISPLAY "#    La preliquidación se terminó completamente."
   DISPLAY "#    "
   DISPLAY "#    El folio Dictamen: "||v_folio_dictamen,"\n"
   DISPLAY "#    "
   DISPLAY "#    El folio preliqudiación: "||v_folio_liquida

   IF ( v_i_resultado = 0 ) THEN
      DISPLAY "#    Preliquidación realizada completamente"
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
         RETURNING r_bnd_fin_oper

         LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                         "# ","\n",
                         "#    La preliquidación se terminó completamente. \n",
                         "# ","\n",
                         "#    El folio Dictamen: "||v_folio_dictamen,"\n",
                         "# ","\n",
                         "#    El folio preliquidación: "||v_folio_liquida,"\n",
                         "# ","\n",
                         "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   ELSE
      -- Indica que ocurrio
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
         
      DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
      DISPLAY "#    Error al procesar la Preliquidación"
      DISPLAY "#    El status de resultado es: ", v_i_resultado
      DISPLAY "#    "
      DISPLAY "#    Total registros encontrados: ",v_i_total_registros_encontrados
      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END IF
   
   DISPLAY "#    "

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION"    
      
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END MAIN

#OBJETIVO: Obtener la descripción del error de la validacion
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT,
         v_descripcion LIKE cat_bat_parametro_salida.descripcion

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
   INTO   v_descripcion
   FROM   cat_bat_parametro_salida
   WHERE  cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   DISPLAY "Atención "||v_descripcion CLIPPED
   --CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")
   RETURN v_descripcion CLIPPED
END FUNCTION