--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/03/2014
--===============================================================

################################################################################
#Modulo       => DAC                                                           #
#Programa     => DACP01                                                        #
#Objetivo     => Lanzador Integración Devolución de Amortización Mejora tu Casa#
#Fecha inicio => 03/03/2014                                                    #
################################################################################

DATABASE safre_viv

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid               LIKE bat_ctr_operacion.pid,
       p_proceso_cod       LIKE bat_ctr_operacion.proceso_cod,
       p_opera_cod         LIKE bat_ctr_operacion.opera_cod,
       p_usuario_cod       LIKE seg_usuario.usuario_cod,
       v_s_sql             STRING ,
       v_i_resultado       INTEGER,
       r_bnd_fin_oper      SMALLINT,
       p_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo,
       v_folio_integra     DECIMAL(9,0),
       v_st_det_trabajador SMALLINT,
       p_titulo            STRING,
       p_mensaje           STRING,
       v_layout            LIKE cat_operacion.layout_cod,
       v_ruta_rescate      STRING,
       v_usuario           LIKE seg_modulo.usuario,
       v_proceso_desc      LIKE cat_proceso.proceso_desc,
       v_extension         LIKE cat_operacion.extension,
       v_opera_desc        LIKE cat_operacion.opera_desc,
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_resultado         SMALLINT,
       v_tot_aceptadas     INTEGER,
       v_tot_rechazadas    INTEGER,  
       v_tot_solicitudes   INTEGER,
       v_isam              INTEGER,
       v_mensaje           CHAR(40),
       r_bnd_error_opera   SMALLINT       

   LET p_usuario_cod    = ARG_VAL(1)   
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio_integra  = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
   RETURNING v_proceso_desc,v_extension,v_opera_desc,v_layout,v_ruta_rescate,
             v_ruta_listados,v_usuario

   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod --2601
   LET g_opera_cod   = p_opera_cod   --2
   
   WHENEVER ERROR CONTINUE

   --Se ejecuta función de validación
   LET v_s_sql = "EXECUTE FUNCTION fn_dac_integra_mtc(?, ?, ?, ?) "

   --DISPLAY p_usuario_cod, g_pid, v_folio_integra, g_proceso_cod
   PREPARE sid_integradeo FROM v_s_sql
   EXECUTE sid_integradeo USING p_usuario_cod, 
                                g_pid, 
                                v_folio_integra, 
                                g_proceso_cod
                           INTO v_i_resultado,
                                v_isam, 
                                v_mensaje,
                                v_tot_solicitudes, 
                                v_tot_aceptadas,
                                v_tot_rechazadas;
                                
   CASE
   WHEN (v_i_resultado = 0)
      -- Cierra la operación
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    La integración se terminó completamente."
      DISPLAY "#    "
      --IF(v_i_resultado=0 AND (v_tot_solicitudes = v_tot_aceptadas) AND
      --   v_tot_solicitudes <> 0)THEN
      DISPLAY "#  Integración realizada con exito"
      --ELSE
      --   DISPLAY "#  Integración realizada pero con errores de validación"
      --END IF
      DISPLAY "#  Folio lote o de integración : ",v_folio_integra,"\n"     
      DISPLAY "#  Total de solicitudes : ",v_tot_solicitudes
      DISPLAY "#  Total de aceptadas   : ",v_tot_aceptadas
      DISPLAY "#  Total de rechazadas  : ",v_tot_rechazadas
      DISPLAY "#  Estatus Resultado    :",v_i_resultado
      IF v_tot_aceptadas > 0 THEN
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
         DISPLAY "#  Ya se puede Continuar con la Preliquidación"
         DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"

         -- Genera cifras control por registro de patron.
         --CALL fn_obtiene_cifras_control(v_folio_integra)                  

         LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # #","\n",
                         "#  La integración se terminó completamente.","\n",
                         "#  ","\n",
                         "#  Integración realizada con exito","\n",
                         "#  ","\n",
                         "#  Folio lote o de integración : ",v_folio_integra,"\n",
                         "#  ","\n",
                         "#  Total de solicitudes : ",v_tot_solicitudes,"\n",
                         "#  Total de aceptadas   : ",v_tot_aceptadas,"\n",
                         "#  Total de rechazadas  : ",v_tot_rechazadas,"\n",
                         "#  Estatus Resultado :",v_i_resultado,"\n",
                         "#  Ya se puede Continuar con la Preliquidación","\n",
                         "# # # # # # # # # # # # # # # # # # # # # # # # # #"
                         
      ELSE
         CALL fn_finaliza_proceso (v_folio_integra,
                                   p_pid, 
                                   p_proceso_cod,
                                   p_opera_cod)        
      END IF
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      
   WHEN ( v_i_resultado < 0 )
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bnd_error_opera
      DISPLAY "Codigo Error SQL:",v_i_resultado
      DISPLAY "Error al procesar la integración"
      DISPLAY "No se puede continuar..."
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"

   WHEN ( SQLCA.SQLCODE = NOTFOUND )
      DISPLAY "NOT FOUND"
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bnd_error_opera
      
      DISPLAY fn_recupera_inconsis_opera(r_bnd_error_opera)
      
      DISPLAY "#  Error. No se integró ninguna solicitud"
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   WHEN ( SQLCA.SQLCODE < 0 )
      DISPLAY SQLERRMESSAGE
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bnd_error_opera
      DISPLAY "Codigo Error SQL:",SQLCA.SQLCODE
      DISPLAY "Error al procesar la integración"
      DISPLAY "No se puede continuar..."
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"

       
   WHEN ( v_i_resultado <> 100 )
      DISPLAY "#  Error. No se integró ninguna solicitud"
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", v_i_resultado,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
                      
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bnd_error_opera                      
   END CASE

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
   
{   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                       "/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                       p_titulo,
                       p_mensaje)}
   
   WHENEVER ERROR STOP
END MAIN

#OBJETIVO: Recuperar las cifras control de totales
FUNCTION fn_obtiene_cifras_control(p_folio)
DEFINE p_folio  DECIMAL(9,0),
       v_total_solicitudes          INTEGER,
       v_suma_aportacion_registrada DECIMAL(11,2),
       v_suma_amortiza_registrada   DECIMAL(11,2),
       v_suma_aportacion_solicitada DECIMAL(11,2),
       v_suma_amortiza_solicitada   DECIMAL(11,2),
       v_s_qry                      STRING

   LET v_s_qry = "\n SELECT SUM(aportacion_reg), SUM(amortizacion_reg),",
                 "\n        SUM(aportacion_sol), SUM(amortizacion_sol),"
   
   PREPARE Prpr_ObtDatosCbzaCtrl FROM v_s_qry CLIPPED
   DECLARE Curr_ObtDatosCbzaCtrl CURSOR FOR Prpr_ObtDatosCbzaCtrl 
   
   DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
   DISPLAY "# # # #        CIFRAS GLOBALES            # # # # # # # # # #"
   DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
   DISPLAY "#         "
   DISPLAY "#  FOLIO :",p_folio
   DISPLAY "#         "
   FOREACH Curr_ObtDatosCbzaCtrl USING p_folio
      INTO v_suma_aportacion_registrada,v_suma_amortiza_registrada,
           v_suma_aportacion_solicitada,v_suma_amortiza_solicitada,
           v_total_solicitudes
      DISPLAY "#  "
      DISPLAY "#  TOTAL AMORTIZACION REGISTRADA  : ",v_suma_amortiza_registrada
      DISPLAY "#  TOTAL APORTACION   SOLICITADA  : ",v_suma_aportacion_solicitada
      DISPLAY "#  TOTAL AMORTIZACION SOLICITADA  : ",v_suma_amortiza_solicitada
      DISPLAY "#  "
      DISPLAY "#  SOLICITUDES  :", v_total_solicitudes
   END FOREACH
   DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"

   FREE Curr_ObtDatosCbzaCtrl 

END FUNCTION -- fn_obtiene_cifras_control


#OBJETIVO: Marcar todo el proceso como finalizado, aún cuando no haya registros a liquidar.
FUNCTION fn_finaliza_proceso(p_folio_integracion,
                             p_pid, 
                             p_proceso_cod,
                             p_opera_cod)
DEFINE r_bnd_fin_oper      SMALLINT,
       p_resultado         SMALLINT,
       p_regs_sin_saldo    INTEGER,
       p_folio_integracion DECIMAL(9,0),
       v_query             STRING,
       p_pid               DECIMAL(9,0),
       p_proceso_cod       SMALLINT,
       p_opera_cod         SMALLINT,
       p_usuario_cod       LIKE seg_usuario.usuario_cod

   DISPLAY "#    No hay registros aceptados para preliquidar"

   --Finaliza la INTEGRACION
   LET p_proceso_cod = 2601
   LET p_opera_cod = 2
   
   LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
   PREPARE prp_fn_actualiza_opera_fin_int FROM v_query
   EXECUTE prp_fn_actualiza_opera_fin_int USING p_pid,
                                                p_proceso_cod,
                                                p_opera_cod
                                           INTO r_bnd_fin_oper   
   --Inicia la PRELIQUIDACION
   LET p_proceso_cod = 2601
   LET p_opera_cod = 3
   
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               3,--g_opera_cod,
                               0,
                               "DACL03",
                               "",
                               p_usuario_cod)
                     RETURNING r_bnd_fin_oper
   --Finaliza la PRELIQUIDACION
   LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
   PREPARE prp_fn_actualiza_opera_fin_pre FROM v_query
   EXECUTE prp_fn_actualiza_opera_fin_pre USING p_pid,
                                                p_proceso_cod,
                                                p_opera_cod
                                           INTO r_bnd_fin_oper                        
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
   LET p_proceso_cod = 2601
   LET p_opera_cod = 4

   LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
   PREPARE prp_fn_actualiza_opera_fin_liq FROM v_query
   EXECUTE prp_fn_actualiza_opera_fin_liq USING p_pid,
                                                p_proceso_cod,
                                                p_opera_cod
                                           INTO r_bnd_fin_oper    

   --Se actualiza STATUS del folio a liquidado
   UPDATE glo_folio
   SET status = 2
   WHERE folio = p_folio_integracion;
END FUNCTION