--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07/04/2015
--==============================================================================
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTL10                                                   #
#Descripcion       => Preliquidación de traspasos de saldos cedente            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 07 Abril 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"
GLOBALS "PRTWS02.inc"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       g_pid             LIKE glo_pid.pid,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_resultado_opera SMALLINT

MAIN
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   # Recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   CALL fn_inicializa_consultas()
   
   CALL fn_recupera_folio_traspasos_receptora()

END MAIN

# Descripción: Inicializa consultas sql
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   {LET v_consulta = " SELECT CASE trs.tpo_operacion WHEN '01' THEN 'TRANSFERENCIA DE SALDO TOTAL' WHEN '02' THEN 'TRANSFERENCIA PARCIAL DE SALDO' ELSE 'NO ESPECIFICADO' END CASE,",
                    "        SUM(NVL(trs.mto_aivs_infonavit97_afo,0)),",
                    "        SUM(NVL(trs.mto_pesos_infonavit97_afo,0)),",
                    "        SUM(NVL(trs.mto_aivs_infonavit97_recalculado,0)),",
                    "        SUM(NVL(trs.mto_pesos_infonavit97_recalculado,0)),",
                    "        SUM(NVL(trs.mto_aivs_infonavit97_cedido,0)),",
                    "        SUM(NVL(trs.mto_pesos_infonavit97_cedido,0))",
                    "   FROM prt_traspaso_cedente trs JOIN prt_solicitud_cedente sol",
                    "     ON sol.id_prt_solicitud_cedente = trs.id_prt_solicitud_cedente",
                    "  WHERE trs.estado = ?",
                    "    AND sol.tipo_portabilidad = ?",
                    "    AND sol.estado IN (?,?)",
                    "  GROUP BY trs.tpo_operacion"}
   {LET v_consulta = " SELECT COUNT(cta.id_derechohabiente),",
                    "        SUM(cta.monto_acciones),",
                    "        SUM(cta.monto_pesos)",
                    "   FROM cta_movimiento  cta JOIN ",
                    "        TABLE(MULTISET(",
                    "        SELECT id_derechohabiente",
                    "          FROM prt_solicitud_cedente",
                    "         WHERE estado IN(?,?)",
                    "           AND tipo_portabilidad(?,?))) sol",
                    "     ON cta.id_derechohabiente = id_derechohabiente",
                    "  WHERE cta.subcuenta = ?",
                    "    AND cta.movimiento IN (?,?)"}
   LET v_consulta = "EXECUTE FUNCTION fn_prt_rec_subsec_pendientes()"
   PREPARE prp_rec_traspasos FROM v_consulta

END FUNCTION

# Descripción: Recupera folio a preliquidar
FUNCTION fn_recupera_folio_traspasos_receptora()
DEFINE r_valida   SMALLINT,
       r_error    BOOLEAN,
       r_confirma BOOLEAN,
       v_traspasos DYNAMIC ARRAY OF RECORD
          v_registros DECIMAL(9,0),
          v_mto_aivs   LIKE prt_traspaso_cedente.mto_aivs_infonavit97,
          v_mto_pesos  LIKE prt_traspaso_cedente.mto_pesos_infonavit97
       END RECORD

   OPEN WINDOW vtna_preliq_trasp_subsec_cedente WITH FORM v_ruta_ejecutable CLIPPED||"/PRTL121"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      DISPLAY ARRAY v_traspasos TO sr_traspasos.*  ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY
            LET g_pid = 0
            CALL fn_valida_operacion(g_pid,
                                     C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE,
                                     C_OPERA_COD_PRELIQ_TANS_CED) RETURNING r_valida
            # Se verifica si la operacion es valida
            IF(r_valida <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_valida)
               EXIT DISPLAY
            END IF               
            # Se recuperan los archivos pendientes
            CALL fn_recupera_traspasos() RETURNING v_traspasos
            IF(v_traspasos[1].v_mto_aivs = 0)THEN
               CALL fn_mensaje(p_cad_ventana,"No hay información para preliquidar","about")
               EXIT DISPLAY
            END IF 

         ON ACTION preliquidar
            CALL fn_ventana_confirma(p_cad_ventana,"¿Preliquidar traspasos subsecuentes?","question") RETURNING r_confirma
            IF( r_confirma )THEN
               CALL fn_valida_operacion(g_pid,
                                        C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE,
                                        C_OPERA_COD_PRELIQ_TANS_CED) RETURNING r_resultado_opera
               IF( r_resultado_opera <> 0 )THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
               ELSE
                  CALL fn_ejecuta_preliquidacion_traspaso_cedente() RETURNING r_error
                  IF( r_error )THEN
                     CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la preliquidación","information")
                     CONTINUE DISPLAY
                  ELSE
                     CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","information")
                     ACCEPT DISPLAY
                  END IF
               END IF
            END IF
         
         ON ACTION cancelar
            EXIT DISPLAY
            
      END DISPLAY
   CLOSE WINDOW vtna_preliq_trasp_subsec_cedente

END FUNCTION

# Descripción: Recupera traspasos pendientes de preliquidar
FUNCTION fn_recupera_traspasos()
DEFINE v_indice   SMALLINT,
       {v_traspaso RECORD
          v_tpo_operacion LIKE prt_traspaso_cedente.tpo_operacion,
          v_aivs_afore    LIKE prt_traspaso_cedente.mto_aivs_infonavit97_afo,
          v_pesos_afore   LIKE prt_traspaso_cedente.mto_pesos_infonavit97_afo,
          v_aivs_inf_reca  LIKE prt_traspaso_cedente.mto_aivs_infonavit97_recalculado,
          v_pesos_inf_reca LIKE prt_traspaso_cedente.mto_pesos_infonavit97_recalculado,
          v_aivs_cedido   LIKE prt_traspaso_cedente.mto_aivs_infonavit97,
          v_pesos_cedido  LIKE prt_traspaso_cedente.mto_pesos_infonavit97
       END RECORD,
       {v_traspasos DYNAMIC ARRAY OF RECORD
          v_tpo_operacion LIKE prt_traspaso_cedente.tpo_operacion,
          v_aivs_afore    LIKE prt_traspaso_cedente.mto_aivs_infonavit97_afo,
          v_pesos_afore   LIKE prt_traspaso_cedente.mto_pesos_infonavit97_afo,
          v_aivs_inf_reca  LIKE prt_traspaso_cedente.mto_aivs_infonavit97_recalculado,
          v_pesos_inf_reca LIKE prt_traspaso_cedente.mto_pesos_infonavit97_recalculado,
          v_aivs_cedido   LIKE prt_traspaso_cedente.mto_aivs_infonavit97,
          v_pesos_cedido  LIKE prt_traspaso_cedente.mto_pesos_infonavit97
       END RECORD}
       v_traspaso RECORD
          v_registros DECIMAL(9,0),
          v_mto_aivs   LIKE prt_traspaso_cedente.mto_aivs_infonavit97,
          v_mto_pesos  LIKE prt_traspaso_cedente.mto_pesos_infonavit97
       END RECORD,
       v_traspasos DYNAMIC ARRAY OF RECORD
          v_registros DECIMAL(9,0),
          v_mto_aivs   LIKE prt_traspaso_cedente.mto_aivs_infonavit97,
          v_mto_pesos  LIKE prt_traspaso_cedente.mto_pesos_infonavit97
       END RECORD

   LET v_indice = 1
   DECLARE cur_recupera_traspasos CURSOR FOR prp_rec_traspasos

   FOREACH cur_recupera_traspasos INTO v_traspaso.*
      LET v_traspasos[v_indice].v_registros  = v_traspaso.v_registros
      LET v_traspasos[v_indice].v_mto_aivs     = v_traspaso.v_mto_aivs
      LET v_traspasos[v_indice].v_mto_pesos    = v_traspaso.v_mto_pesos
      
      LET v_indice = v_indice + 1
   END FOREACH
   
   FREE cur_recupera_traspasos

   RETURN v_traspasos
END FUNCTION

# Descripción: Ejecuta el programa que realiza la preliquidación
FUNCTION fn_ejecuta_preliquidacion_traspaso_cedente()
DEFINE v_comando          STRING,
       v_ruta_listados    LIKE seg_modulo.ruta_listados,
       v_ruta_bin_aux     LIKE seg_modulo.ruta_bin,
       v_error            BOOLEAN

   LET v_error = FALSE
   CALL fn_genera_pid(C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE,
                      C_OPERA_COD_PRELIQ_TANS_CED,
                      p_usuario_cod) RETURNING g_pid

   CALL fn_inicializa_proceso(g_pid,
                              C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE,
                              C_OPERA_COD_PRELIQ_TANS_CED,
                              0, # Folio
                              "PRTL12",
                              "NA", # Archivo
                              p_usuario_cod) RETURNING r_resultado_opera
   IF( r_resultado_opera <> 0 )THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      LET v_error = TRUE
      RETURN v_error
   END IF
                                    
   CALL fn_actualiza_opera_ini(g_pid,
                               C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE,
                               C_OPERA_COD_PRELIQ_TANS_CED,
                               0,
                               "PRTL12",
                               "NA",
                               p_usuario_cod) RETURNING r_resultado_opera
   IF( r_resultado_opera <> 0 )THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      LET v_error = TRUE
      RETURN v_error
   ELSE
      CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
   
      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PRTE12.42r ",p_usuario_cod, " ",
                                                                               g_pid, " ",
                                                                               C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE," ",
                                                                               C_OPERA_COD_PRELIQ_TANS_CED," ",
                                                                               "0 ",
                                                                               "NA",
                      " 1>", v_ruta_listados CLIPPED,"/nohup:",g_pid USING "&&&&&",":",
                                                               C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE USING "&&&&&",":",
                                                               C_OPERA_COD_PRELIQ_TANS_CED USING "&&&&&",
                      " 2>&1 &"
      RUN v_comando
      IF( STATUS )THEN
         LET v_error = TRUE
      END IF
   END IF

   RETURN v_error
END FUNCTION