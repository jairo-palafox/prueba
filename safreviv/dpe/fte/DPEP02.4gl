--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22/Noviembre/2016
--==============================================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEP02                                                        #
#Objetivo     => Programa lanzado que ejecuta la preliquidacion de Devolución  #
#                de Pagos Indebidos o en Exceso                                #
#Fecha inicio => Noviembre 22, 2016                                            #
################################################################################

GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio_integra  LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_resultado      INTEGER -- resultado del proceso
       ,r_bnd_fin_oper  SMALLINT
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
       ,v_folio_liquida     LIKE dpe_preliquida.folio_liquida -- folio de la liquidación
       ,v_isam_err          INTEGER
       ,err_txt             CHAR(200)
       ,v_regs_insertados   INTEGER

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio_integra  = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
        RETURNING v_folio_liquida

   DISPLAY "   Folio Liquidación ", v_folio_liquida

   LET v_s_sql = "EXECUTE FUNCTION fn_dpe_preliquida (?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE prp_preliquida FROM v_s_sql
   EXECUTE prp_preliquida USING p_usuario_cod, 
                                p_folio_integra, 
                                v_folio_liquida,
                                g_pid
                          INTO  v_resultado,
                                v_isam_err,
                                err_txt,
                                v_regs_insertados;

   IF ( v_resultado = 0 ) THEN
      DISPLAY "   Preliquidación realizada completamente"
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bnd_fin_oper
         
         LET p_mensaje = "   La preliquidación se terminó completamente. \n",
                         "   El folio Lote: "||p_folio_integra,"\n",
                         "   El folio preliqudiación: "||v_folio_liquida,"\n"

   ELSE
      -- Indica que ocurrio
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper

      DISPLAY fn_recupera_inconsis_opera(r_bnd_fin_oper)
      DISPLAY "   Error al procesar la Preliquidación"
      DISPLAY "   El status de resultado es: ", v_resultado
      DISPLAY "   ISAM  : ",v_isam_err
      DISPLAY "   Error : ",err_txt
      DISPLAY " "
      DISPLAY "   Total registros encontrados: ", 
              v_i_total_registros_encontrados
      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END IF

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,"",p_titulo,p_mensaje)

END MAIN