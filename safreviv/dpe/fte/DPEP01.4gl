--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 31/Oct/2016
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEP01                                                        #
#Objetivo     => Programa lanzado de la integración de Devolucion de Pagos     #
#                Indebidos o en Exceso                                         #
#Fecha inicio => Octubre 31, 2016                                              #
################################################################################

GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                      LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod ,-- codigo del proceso
       p_opera_cod                LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod              LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_s_sql                    STRING ,-- cadena con una instruccion SQL
       v_i_resultado              INTEGER ,-- resultado del proceso
       r_bnd_fin_oper             SMALLINT,
       v_si_correcto_integra      SMALLINT,
       p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo ,-- nombre dle archivo
       v_si_solicitudes_totales   SMALLINT,
       v_si_solicitudes_aceptadas SMALLINT,
       v_msj_sql                  CHAR(200),
       v_folio                    LIKE deo_preliquida.folio_liquida,
       v_si_status_detalle_trabaj SMALLINT,
       v_si_status_sumario_patron SMALLINT,
       v_si_status_sumario_exceso SMALLINT,
       v_c_status_proc            CHAR(200),
       v_si_total_trabaja         SMALLINT,
       p_titulo                   STRING ,-- titulo del mensaje enviado en el correo
       p_mensaje                  STRING ,-- cuerpo del mensaje enviado
       v_layout                   LIKE cat_operacion.layout_cod,
       v_ruta_rescate             STRING,
       v_usuario                  LIKE seg_modulo.usuario,
       v_proceso_desc             LIKE cat_proceso.proceso_desc,
       v_extension                LIKE cat_operacion.extension,
       v_opera_desc               LIKE cat_operacion.opera_desc,
       v_ruta_listados            LIKE seg_modulo.ruta_listados

   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
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
        RETURNING v_folio
DISPLAY "   Folio Integración", v_folio
   -- Ejecuta prevalidación de encabezados y sumarios
   LET v_i_resultado = 0
   LET v_s_sql = "EXECUTE FUNCTION fn_dpe_pre_integra(?, ?, ?)"
   PREPARE Prpr_ValidaEncabezados FROM v_s_sql CLIPPED
   EXECUTE Prpr_ValidaEncabezados USING p_usuario_cod, 
                                        g_pid, 
                                        v_folio
                                   INTO v_i_resultado, 
                                        v_c_status_proc
   
   CASE
      WHEN (SQLCA.SQLCODE = 0)
         IF(v_i_resultado <> 0)THEN
            -- Error en prevalidación, no se puede continuar con integracion
            DISPLAY "   Error. Error en prevalidación de Encabezados de archivo"
            DISPLAY "   Error. v_c_status_proc:",v_c_status_proc
            DISPLAY "   Error. Continua proceso para integración completa...\n"
         ELSE
            DISPLAY "   Validación Encabezados completa..."
         END IF

      WHEN (SQLCA.SQLCODE = NOTFOUND)
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_fin_oper
         DISPLAY "NOT FOUND"
         DISPLAY fn_recupera_inconsis_opera(r_bnd_fin_oper) 
         DISPLAY "   Error. No se integró ninguna solicitud"
         LET p_mensaje = " --- ERROR ---\n",
                          "   El proceso de Integración no terminó correctamente.\n",
                          "   Código de error : ", r_bnd_fin_oper,"\n ",
                          "   FECHA           : ",TODAY,"\n",
                          "   HORA            : ",CURRENT HOUR TO SECOND,"\n"
          
      WHEN (SQLCA.SQLCODE < 0)
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_fin_oper
         DISPLAY "   Codigo Error SQL:",SQLCA.SQLCODE
         DISPLAY "   Error al procesar la integración"
         DISPLAY "   No se puede continuar..."
         LET p_mensaje = " --- ERROR ---\n",
                         "   El proceso de Integración no terminó correctamente.\n",
                         "   Código de error : ", r_bnd_fin_oper,"\n ",
                         "   FECHA           : ",TODAY,"\n",
                         "   HORA            : ",CURRENT HOUR TO SECOND,"\n"
    END CASE
    
    -- se asume que el proceso termina correctamente
    LET v_i_resultado = 0
    LET v_si_correcto_integra = 0
   
    -- se contruye el enuncionado SQL
    LET v_s_sql = "EXECUTE FUNCTION fn_dpe_integra_det_nuevo_modelo(?, ?, ?, ?, ?) "
   
    -- se prepara la ejecucion del stored procedure para la integracion
    PREPARE sid_integradeo FROM v_s_sql
    EXECUTE sid_integradeo USING p_usuario_cod, 
                                 g_pid, 
                                 p_nombre_archivo, 
                                 v_folio, g_proceso_cod
                            INTO v_i_resultado, 
                                 v_msj_sql, 
                                 v_si_correcto_integra, 
                                 v_si_solicitudes_totales,
                                 v_si_solicitudes_aceptadas,
                                 v_si_status_detalle_trabaj ,
                                 v_si_status_sumario_patron,
                                 v_si_status_sumario_exceso, 
                                 v_si_total_trabaja
                                
    CASE
       WHEN (SQLCA.SQLCODE = 0)
          DISPLAY "   La integración se terminó completamente."
          DISPLAY " "
          DISPLAY "   RESULTADO INTEGRACION",v_i_resultado       
          IF(v_i_resultado=0 AND 
             (v_si_solicitudes_totales = v_si_solicitudes_aceptadas) AND
             v_si_solicitudes_totales <> 0)THEN
             DISPLAY "   Integración realizada con exito"
          ELSE
             DISPLAY "   Integración realizada pero con errores de validación"
          END IF

          DISPLAY "   ------ TOTALES ------"
          DISPLAY "   Total de patrones    : ",v_si_total_trabaja
          DISPLAY "   Total de solicitudes : ",v_si_solicitudes_totales
          DISPLAY "   Total de aceptadas   : ",v_si_solicitudes_aceptadas
          DISPLAY "   Total de rechazadas  : ",(v_si_solicitudes_totales-
                                               v_si_solicitudes_aceptadas)
          DISPLAY "   Estatus Resultado :",v_i_resultado
          
          IF(v_si_correcto_integra= 0 AND v_si_solicitudes_aceptadas >0)THEN
             CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                RETURNING r_bnd_fin_oper

             DISPLAY "   Ya se puede Continuar con la Validación de la Respuesta de PROCESAR"
       
             -- Genera cifras control por registro de patron.
             CALL fn_obtiene_cifras_control(v_folio)                  
             
             LET p_mensaje = "   La integración se terminó completamente.","\n",
                             "   ","",
                             "   Integración realizada con exito","\n",
                             "   ","\n",
                             "   Folio lote o de integración : ",v_folio,"\n",
                             "   ","\n",
                             "   Total de patrones    : ",v_si_total_trabaja,"\n",
                             "   Total de solicitudes : ",v_si_solicitudes_totales,"\n",
                             "   Total de aceptadas   : ",v_si_solicitudes_aceptadas,"\n",
                             "   Total de rechazadas  : ",(v_si_solicitudes_totales-
                                                           v_si_solicitudes_aceptadas),"\n",
                             "   Estatus Resultado :",v_i_resultado,"\n",
                             "   Ya se puede Continuar con la Preliquidación","\n"

          ELSE
             CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                RETURNING r_bnd_fin_oper
                LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de Preliquidación no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"
             DISPLAY fn_recupera_inconsis_opera(r_bnd_fin_oper) 
             IF(v_i_resultado <> 100)THEN
                DISPLAY "   Error. No se integró ninguna solicitud"
             ELSE
                DISPLAY "   ",fn_status_secciones_integradas(v_si_status_detalle_trabaj 
                        ,v_si_status_sumario_patron,v_si_status_sumario_exceso)
             END IF
             LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de Preliquidación no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"           
          END IF
      
       WHEN (SQLCA.SQLCODE = NOTFOUND)
          DISPLAY "NOT FOUND"
          CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
             RETURNING r_bnd_fin_oper
             
          DISPLAY  fn_recupera_inconsis_opera(r_bnd_fin_oper) 

          DISPLAY "   Error. No se integró ninguna solicitud"
          LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de Preliquidación no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"
       WHEN (SQLCA.SQLCODE < 0)
          DISPLAY SQLERRMESSAGE
          CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
             RETURNING r_bnd_fin_oper
          DISPLAY "Codigo Error SQL:",SQLCA.SQLCODE
          DISPLAY "Error al procesar la integración"
          DISPLAY "No se puede continuar..."
          LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de Preliquidación no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"
    END CASE
   
   DISPLAY "Estado detalle            :",v_si_status_detalle_trabaj 
   DISPLAY "Estado sumario patron     :",v_si_status_sumario_patron
   DISPLAY "Estado sumario encabezadp :",v_si_status_sumario_exceso
   DISPLAY "v_msj_sql:",v_msj_sql

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
      
   
   WHENEVER ERROR STOP
END MAIN

#OBJETIVO: Obtener cifras por patron para mostrar en monitor de procesos
FUNCTION fn_obtiene_cifras_control(p_folio)
   DEFINE 
    p_folio  LIKE deo_preliquida.folio_liquida
   --
   ,v_reg_patranal           CHAR(11)
   ,v_total_solicitudes      INTEGER
   ,v_suma_importes_patronal DECIMAL(18,6)
   --
   ,v_s_qry                  STRING
 
    LET v_s_qry =
       "SELECT reg_patronal_imss, SUM(imp_viv_dev), COUNT(*)"
       ,"\n   FROM safre_viv:dpe_sol_trabajador"
       ,"\n  WHERE folio = ?"
       ,"\n  GROUP BY 1"
       ,"\n  ORDER BY 1"
   PREPARE Prpr_ObtDatosCbzaCtrl FROM v_s_qry CLIPPED
   DECLARE Curr_ObtDatosCbzaCtrl CURSOR FOR Prpr_ObtDatosCbzaCtrl 

   DISPLAY " "
   DISPLAY "-------------  CIFRAS GLOBALES POR PATRON  -------------"
   DISPLAY " "
   DISPLAY "   FOLIO :",p_folio
   DISPLAY " "
   
   
   FOREACH Curr_ObtDatosCbzaCtrl USING p_folio
      INTO v_reg_patranal, v_suma_importes_patronal, v_total_solicitudes
      DISPLAY " "
      DISPLAY "   REG. PATRONAL: ",v_reg_patranal
      DISPLAY "   TOTAL PESOS  : ",v_suma_importes_patronal
      DISPLAY "   SOLICITUDES  :", v_total_solicitudes
   END FOREACH
   DISPLAY "--------------------------------------------------------"
   
   FREE Curr_ObtDatosCbzaCtrl 

END FUNCTION -- fn_obtiene_cifras_control
  
FUNCTION fn_status_secciones_integradas(v_si_detalle, 
      v_si_sumario_patron, v_si_sumario_exceso)
   DEFINE 
     v_si_detalle        SMALLINT
    ,v_si_sumario_patron SMALLINT
    ,v_si_sumario_exceso SMALLINT
    --
    ,v_c_mensaje CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]" 
   END IF

   IF(v_si_sumario_patron = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Sumario Patron ]" 
   END IF

   IF(v_si_sumario_exceso = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Sumario Exceso ]"  
   END IF

    RETURN v_c_mensaje
   
END FUNCTION -- 