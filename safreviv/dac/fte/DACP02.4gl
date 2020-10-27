--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/03/2014
--===============================================================

################################################################################
#Modulo       => DAC                                                           #
#Programa     => DACL03                                                        #
#Objetivo     => Lanzador Preliquidación Devolución de Amortización Mejora     #
#                tu Casa                                                       #
#Fecha inicio => 04/03/2014                                                    #
################################################################################
DATABASE safre_viv

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
       v_resultado      SMALLINT, -- resultado del proceso
       r_bnd_fin_oper   SMALLINT,
       v_tot_registros  INTEGER
      --
       ,v_i_total_registros_encontrados INTEGER
       ,p_titulo            STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje           STRING -- cuerpo del mensaje enviado
       ,v_layout            LIKE cat_operacion.layout_cod
       ,v_ruta_rescate      STRING
       ,v_usuario           LIKE seg_modulo.usuario
       ,v_proceso_desc      LIKE cat_proceso.proceso_desc
       ,v_extension         LIKE cat_operacion.extension
       ,v_opera_desc        LIKE cat_operacion.opera_desc
       ,v_ruta_listados     LIKE seg_modulo.ruta_listados
       ,v_folio_liquidacion     LIKE dpe_preliquida.folio_liquida -- folio de la liquidación
       
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario

   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion solo INFONAVIT
   LET g_opera_cod   = p_opera_cod -- preliquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se asume que el proceso termina correctamente
   LET v_resultado = 0
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION safre_viv:fn_dac_preliquida(?, ?, ?, ?, ?)"

   PREPARE sid_preliquidadeo FROM v_s_sql
   EXECUTE sid_preliquidadeo USING p_folio      ,
                                   g_proceso_cod,
                                   g_opera_cod  ,
                                   p_usuario_cod,
                                   g_pid
                             INTO  v_resultado,
                                   v_tot_registros,
                                   v_folio_liquidacion

   -- Se finaliza operacion aunque no se termine correctamente el error
   --DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   --DISPLAY "#    La preliquidación se terminó completamente."
   --DISPLAY "#    "
   --DISPLAY "#    El folio preliqudiación: "||v_folio_liquidacion

   IF ( v_resultado = 0 ) THEN
      DISPLAY "#    Preliquidación realizada completamente"
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
           g_proceso_cod, --- Clave del proceso
           g_opera_cod) --- Clave de la operación
         RETURNING r_bnd_fin_oper

         IF v_tot_registros = 0 THEN
            CALL fn_finaliza_proceso(p_usuario_cod)
         END IF 
         
         LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                         "# \n",
                         "#    La preliquidación se terminó completamente. \n",
                         "# \n",
                         "#    El folio Lote: "||p_folio,"\n",
                         "# \n",
                         "#    El folio preliqudiación: "||v_folio_liquidacion,"\n",
                         "# \n",
                         "# # # # # # # # # # # # # # # # # # # # # # # # # #"
                         
                         
                         
   ELSE
      -- Indica que ocurrio
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
         DISPLAY fn_recupera_inconsis_opera(r_bnd_fin_oper) 
      DISPLAY "#    Error al procesar la Preliquidación"
      DISPLAY "#    El status de resultado es: ", v_resultado
      DISPLAY "#    "
      DISPLAY "#    Total registros encontrados: ",v_tot_registros
      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END IF
      LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION"
      
      
      CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END MAIN

#OBJETIVO: Marcar todo el proceso como finalizado, aún cuando no haya registros a liquidar.
FUNCTION fn_finaliza_proceso(p_usuario_cod)
DEFINE r_bnd_fin_oper SMALLINT,
       p_usuario_cod  CHAR(20)

   DISPLAY "#    Preliquidación no se termino completamente "
   DISPLAY "#    "
   DISPLAY "#    No se preliquido ningún registro "

   --Finaliza la PRELIQUIDACION
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               g_opera_cod)   --- Clave de la operación
                     RETURNING r_bnd_fin_oper
   --Inicia la LIQUIDACIÓN
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               4,--g_opera_cod,
                               0,
                               "DACL04",
                               "",
                               p_usuario_cod)
                     RETURNING r_bnd_fin_oper

   --Finaliza la LIQUIDACION
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               4)   --- Clave de la operación
                     RETURNING r_bnd_fin_oper

   --Inicia la GENERAR ARCHIVO
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               5,--g_opera_cod,
                               0,
                               "DACL05",
                               "",
                               p_usuario_cod)
                     RETURNING r_bnd_fin_oper
   --Finaliza GENERAR ARCHIVO
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               5)   --- Clave de la operación
                     RETURNING r_bnd_fin_oper     
END FUNCTION