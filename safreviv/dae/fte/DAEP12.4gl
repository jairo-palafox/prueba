--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/Mar/2016
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => DAE                                                           #
#Programa     => DAEP10                                                        #
#Objetivo     => Lanzado de preliquidación de Ajuste Individual Amortizaciones #
#                Excedentes                                                    #
#Fecha inicio => 09/Mar/2016                                                   #
################################################################################

--LANZADOR: DAEL18

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
       p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
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
       v_ejecuta           SMALLINT
       
   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

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
   LET g_proceso_cod = 2407 --Ajuste Amortización Excedente Individual
   LET g_opera_cod   = 2 -- Preliquidación
   LET v_ejecuta     = 0
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_dae_preliquida_ajuste_ind(?, ?, ?, ?)"
   
   -- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidadeo FROM v_s_sql
   -- se ejecuta el stored procedure
   EXECUTE sid_preliquidadeo USING p_folio, 
                                   p_usuario, 
                                   g_pid,
                                   p_proceso_cod 
                              INTO v_i_resultado, 
                                   v_i_total_registros_encontrados, 
                                   v_folio_liquida

   IF ( v_i_resultado = 0 ) THEN
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
         RETURNING r_bnd_fin_oper

         LET p_mensaje = " \n",
                         "   La preliquidación se terminó completamente. \n",
                         " \n",
                         "   El folio Lote: "||p_folio,"\n",
                         " \n",
                         "   El folio preliquidación: "||v_folio_liquida,"\n",
                         " \n"
   ELSE
      -- Indica que ocurrio
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
      DISPLAY fn_recupera_inconsis_opera(r_bnd_fin_oper) 
      DISPLAY "   Error al procesar la Preliquidación"
      DISPLAY "   El status de resultado es: ", v_i_resultado
      DISPLAY " "
      DISPLAY "   Total registros encontrados: ",v_i_total_registros_encontrados
      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END IF

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION"    
      
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)

END MAIN
