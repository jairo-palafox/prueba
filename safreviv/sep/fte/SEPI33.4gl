--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11-06-2012
--===============================================================

################################################################################
#Modulo            => SEPI                                                     #
#Programa          => SEPI33                                                   #
#Objetivo          => Programa de preliquidacion de operacion 28               # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 11, 2012                                           #
################################################################################
DATABASE safre_viv
DEFINE p_pid                  LIKE bat_ctr_proceso.pid,     
       p_proceso_cod          LIKE cat_proceso.proceso_cod, 
       p_opera_preliquidacion LIKE cat_operacion.opera_cod, 
       p_usuario_cod          LIKE seg_usuario.usuario_cod, 
       p_folio                LIKE glo_folio.folio,
       p_nom_archivo          LIKE glo_ctr_archivo.nombre_archivo,
       r_ruta_lst             LIKE seg_modulo.ruta_listados,
       r_ruta_ejecutable      LIKE seg_modulo.ruta_bin,
       v_nom_reporte          STRING,
       v_manejador_rpt        OM.SaxDocumentHandler,

       v_encabezado RECORD 
         v_fecha_actual CHAR(10),
         v_nom_archivo  LIKE glo_ctr_archivo.nombre_archivo,
         v_folio        LIKE glo_folio.folio,
         v_total_procesadas  INTEGER,
         v_total_diferencias INTEGER,
         v_total_expediente  INTEGER
       END RECORD,
       v_montos_procesar  ARRAY[4] OF RECORD
         v_clasificacion  LIKE sep_det_02_op28.clasifica_separacion,
         v_invadido_sar92 LIKE sep_det_02_op28.saldo_viv_92,
         v_asociado_sar92 LIKE sep_det_03_op28.saldo_viv_92,
         v_invadido_viv97 LIKE sep_det_02_op28.saldo_viv_97,
         v_asociado_viv97 LIKE sep_det_03_op28.saldo_viv_97,
         v_total_sar92    LIKE sep_det_02_op28.saldo_viv_92,
         v_total_viv97    LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas  INTEGER
       END RECORD,
       v_sum_mtos_procesar_rpt RECORD
         v_invadido_sar92 LIKE sep_det_02_op28.saldo_viv_92,
         v_asociado_sar92 LIKE sep_det_03_op28.saldo_viv_92,
         v_invadido_viv97 LIKE sep_det_02_op28.saldo_viv_97,
         v_asociado_viv97 LIKE sep_det_03_op28.saldo_viv_97,
         v_total_sar92    LIKE sep_det_02_op28.saldo_viv_92,
         v_total_viv97    LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas  INTEGER
       END RECORD,
       v_montos_procesar_aux  RECORD
         v_clasificacion  LIKE sep_det_02_op28.clasifica_separacion,
         v_invadido_sar92 LIKE sep_det_02_op28.saldo_viv_92,
         v_asociado_sar92 LIKE sep_det_03_op28.saldo_viv_92,
         v_invadido_viv97 LIKE sep_det_02_op28.saldo_viv_97,
         v_asociado_viv97 LIKE sep_det_03_op28.saldo_viv_97,
         v_total_sar92    LIKE sep_det_02_op28.saldo_viv_92,
         v_total_viv97    LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas  INTEGER
       END RECORD,
       v_his_preliquidacion_aux RECORD
          v_id_his_prel  LIKE sep_his_preliquida_op28.id_his_preliquida_op28,
          v_tipo_nss_inv     VARCHAR(1),
          v_nss_inv          LIKE sep_his_preliquida_op28.invadido,
          v_sdo_sar92_inv    LIKE sep_his_preliquida_op28.sdo_origen_sar92_invadido,
          v_op28_sar92_inv   LIKE sep_his_preliquida_op28.op28_sar92_invadido,
          v_mov_sar92_inv    LIKE sep_his_preliquida_op28.cargo_sar92_invadido,
          v_sdof_sar92_inv   LIKE sep_his_preliquida_op28.sdo_final_sar92_invadido,
          v_dif_sar92_inv    LIKE sep_his_preliquida_op28.dif_sar92_invadido,
          v_sdo_viv97_inv    LIKE sep_his_preliquida_op28.sdo_origen_viv97_invadido,
          v_op28_viv97_inv   LIKE sep_his_preliquida_op28.op28_viv97_invadido,
          v_mov_viv97_inv    LIKE sep_his_preliquida_op28.cargo_viv97_invadido,
          v_sdof_viv97_inv   LIKE sep_his_preliquida_op28.sdo_final_viv97_invadido,
          v_dif_viv97_inv    LIKE sep_his_preliquida_op28.dif_viv97_invadido,

          v_tipo_nss_asoc     VARCHAR(1),
          v_nss_asoc          LIKE sep_his_preliquida_op28.asociado,
          v_sdo_sar92_asoc    LIKE sep_his_preliquida_op28.sdo_origen_sar92_asociado,
          v_op28_sar92_asoc   LIKE sep_his_preliquida_op28.op28_sar92_asociado,
          v_mov_sar92_asoc    LIKE sep_his_preliquida_op28.abono_sar92_asociado,
          v_sdof_sar92_asoc   LIKE sep_his_preliquida_op28.sdo_final_sar92_asociado,
          v_dif_sar92_asoc    LIKE sep_his_preliquida_op28.dif_sar92_asociado,
          v_sdo_viv97_asoc    LIKE sep_his_preliquida_op28.sdo_origen_viv97_asociado,
          v_op28_viv97_asoc   LIKE sep_his_preliquida_op28.op28_viv97_asociado,
          v_mov_viv97_asoc    LIKE sep_his_preliquida_op28.abono_viv97_asociado,
          v_sdof_viv97_asoc   LIKE sep_his_preliquida_op28.sdo_final_viv97_asociado,
          v_dif_viv97_asoc    LIKE sep_his_preliquida_op28.dif_viv97_asociado
       END RECORD,
       v_global DYNAMIC ARRAY OF RECORD
          v_indicador_tabla SMALLINT,
          v_id_his_prel  LIKE sep_his_preliquida_op28.id_his_preliquida_op28,
          v_tipo_nss     VARCHAR(1),
          v_nss          LIKE sep_his_preliquida_op28.invadido,
          v_sdo_sar92    LIKE sep_his_preliquida_op28.sdo_origen_sar92_invadido,
          v_op28_sar92   LIKE sep_his_preliquida_op28.op28_sar92_invadido,
          v_mov_sar92    LIKE sep_his_preliquida_op28.cargo_sar92_invadido,
          v_sdof_sar92   LIKE sep_his_preliquida_op28.sdo_final_sar92_invadido,
          v_dif_sar92    LIKE sep_his_preliquida_op28.dif_sar92_invadido,
          v_sdo_viv97    LIKE sep_his_preliquida_op28.sdo_origen_viv97_invadido,
          v_op28_viv97   LIKE sep_his_preliquida_op28.op28_viv97_invadido,
          v_mov_viv97    LIKE sep_his_preliquida_op28.cargo_viv97_invadido,
          v_sdof_viv97   LIKE sep_his_preliquida_op28.sdo_final_viv97_invadido,
          v_dif_viv97    LIKE sep_his_preliquida_op28.dif_viv97_invadido
       END RECORD

MAIN
DEFINE v_consulta STRING,
       v_indice   INTEGER

   LET p_usuario_cod          = ARG_VAL(1)
   LET p_pid                  = ARG_VAL(2)
   LET p_proceso_cod          = ARG_VAL(3)
   LET p_opera_preliquidacion = ARG_VAL(4)
   LET p_folio                = ARG_VAL(5)
   LET p_nom_archivo          = ARG_VAL(6)

   # INICIALIZA ARREGLO
   CALL v_global.clear()
   
   # Información del encabezado

   LET v_encabezado.v_fecha_actual = MONTH(TODAY) USING "&&","-",DAY(TODAY) USING "&&","-",YEAR(TODAY) USING "&&&&"
   LET v_encabezado.v_nom_archivo = p_nom_archivo
   LET v_encabezado.v_folio = p_folio

   # DATOS DE ENCABEZADO
   # Total procesadas   
   SELECT COUNT(*)
     INTO v_encabezado.v_total_procesadas
     FROM sep_his_preliquida_op28
    WHERE folio = p_folio
   # Total parejas con diferencias
   SELECT COUNT(*)
     INTO v_encabezado.v_total_diferencias
     FROM sep_his_preliquida_op28
    WHERE folio = p_folio
      AND (dif_sar92_invadido <> 0
       OR dif_viv97_invadido <> 0
       OR dif_sar92_asociado <> 0
       OR dif_viv97_asociado <> 0)
   # Total parejas con expediente
   SELECT COUNT(*)
     INTO v_encabezado.v_total_expediente
     FROM sep_his_preliquida_op28
    WHERE folio = p_folio
      AND (id_expediente IS NOT NULL OR id_expediente = 0) 


   # SECCIÓN MONTOS A PROCESAR
   LET v_sum_mtos_procesar_rpt.v_invadido_sar92 = 0
   LET v_sum_mtos_procesar_rpt.v_asociado_sar92 = 0
   LET v_sum_mtos_procesar_rpt.v_invadido_viv97 = 0
   LET v_sum_mtos_procesar_rpt.v_asociado_viv97 = 0
   LET v_sum_mtos_procesar_rpt.v_total_sar92 = 0
   LET v_sum_mtos_procesar_rpt.v_total_viv97 = 0
   LET v_sum_mtos_procesar_rpt.v_total_parejas = 0
   LET v_indice = 1
   LET v_consulta = "\n SELECT det2.clasifica_separacion, SUM(det2.saldo_viv_92),",
                    "\n        SUM(det3.saldo_viv_92),SUM(det2.saldo_viv_97),",
                    "\n        SUM(det3.saldo_viv_97),SUM(det2.saldo_viv_92+det3.saldo_viv_92),",
                    "\n        SUM(det3.saldo_viv_97+det2.saldo_viv_97),COUNT(det2.id_det_02_op28)",
                    "\n   FROM sep_det_02_op28 det2 JOIN sep_det_03_op28 det3",
                    "\n     ON det3.id_det_02_op28 = det2.id_det_02_op28",
                    "\n  WHERE det2.folio = ?",
                    "\n  GROUP BY 1"
   --DISPLAY v_consulta
   PREPARE prp_montos_procesar FROM v_consulta
   DECLARE cur_montos_procesar CURSOR FOR prp_montos_procesar 
   FOREACH cur_montos_procesar USING p_folio
                                INTO v_montos_procesar_aux.*
      LET v_montos_procesar[v_indice].v_clasificacion  = v_montos_procesar_aux.v_clasificacion
      LET v_montos_procesar[v_indice].v_invadido_sar92 = v_montos_procesar_aux.v_invadido_sar92 --USING "-###,###,##&.&&"
      LET v_montos_procesar[v_indice].v_asociado_sar92 = v_montos_procesar_aux.v_asociado_sar92 --USING "-###,###,##&.&&"
      LET v_montos_procesar[v_indice].v_invadido_viv97 = v_montos_procesar_aux.v_invadido_viv97 --USING "-###,###,##&.&&"
      LET v_montos_procesar[v_indice].v_asociado_viv97 = v_montos_procesar_aux.v_asociado_viv97 --USING "-###,###,##&.&&"
      LET v_montos_procesar[v_indice].v_total_sar92    = v_montos_procesar_aux.v_total_sar92    --USING "-###,###,##&.&&"
      LET v_montos_procesar[v_indice].v_total_viv97    = v_montos_procesar_aux.v_total_viv97    --USING "-###,###,##&.&&"
      LET v_montos_procesar[v_indice].v_total_parejas  = v_montos_procesar_aux.v_total_parejas

      LET v_sum_mtos_procesar_rpt.v_invadido_sar92 = v_sum_mtos_procesar_rpt.v_invadido_sar92 
                                                   + v_montos_procesar[v_indice].v_invadido_sar92 --USING "-###,###,##&.&&"
      LET v_sum_mtos_procesar_rpt.v_asociado_sar92 = v_sum_mtos_procesar_rpt.v_asociado_sar92
                                                   + v_montos_procesar[v_indice].v_asociado_sar92 --USING "-###,###,##&.&&"
      LET v_sum_mtos_procesar_rpt.v_invadido_viv97 = v_sum_mtos_procesar_rpt.v_invadido_viv97
                                                   + v_montos_procesar[v_indice].v_invadido_viv97 --USING "-###,###,##&.&&"
      LET v_sum_mtos_procesar_rpt.v_asociado_viv97 = v_sum_mtos_procesar_rpt.v_asociado_viv97
                                                   + v_montos_procesar[v_indice].v_asociado_viv97 --USING "-###,###,##&.&&"
      LET v_sum_mtos_procesar_rpt.v_total_sar92 = v_sum_mtos_procesar_rpt.v_total_sar92
                                                + v_montos_procesar[v_indice].v_total_sar92 --USING "-###,###,##&.&&"
      LET v_sum_mtos_procesar_rpt.v_total_viv97 = v_sum_mtos_procesar_rpt.v_total_viv97
                                                + v_montos_procesar[v_indice].v_total_viv97 --USING "-###,###,##&.&&"
      LET v_sum_mtos_procesar_rpt.v_total_parejas = v_sum_mtos_procesar_rpt.v_total_parejas
                                                  + v_montos_procesar[v_indice].v_total_parejas
      {DISPLAY v_sum_mtos_procesar_rpt.v_total_viv97
      DISPLAY v_sum_mtos_procesar_rpt.v_total_parejas
      DISPLAY "v_total_parejas ",v_montos_procesar_aux.v_total_parejas}
      
      LET v_indice = v_indice + 1
      # si hay más de 4 calsificaciones, se omiten las demas
      IF(v_indice > 4)THEN
         EXIT FOREACH
      END IF
   END FOREACH
   FREE cur_montos_procesar


   # SECCIÓN CON DIFERENCIA SIN EXPEDIENTE
   LET v_indice = 1
   {LET v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                    "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                    "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                    "\n        dif_sar92_invadido,",
                    "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                    "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                    "\n        dif_viv97_invadido",
                    "\n   FROM sep_his_preliquida_op28",
                    "\n  WHERE folio = ?",
                    "\n    AND (dif_sar92_invadido <> 0",
                    "\n     OR dif_viv97_invadido <> 0)",
                    "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)",
                    "\n UNION ALL",
                    "\n SELECT id_his_preliquida_op28,'A',asociado,",
                    "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                    "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                    "\n        dif_sar92_asociado,",
                    "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                    "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                    "\n        dif_viv97_asociado",
                    "\n   FROM sep_his_preliquida_op28",
                    "\n  WHERE folio = ?",
                    "\n    AND (dif_sar92_asociado <> 0",
                    "\n     OR dif_viv97_asociado <> 0)",
                    "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)"}

   LET v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                    "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                    "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                    "\n        dif_sar92_invadido,",
                    "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                    "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                    "\n        dif_viv97_invadido,",
                    "\n        'A',asociado,",
                    "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                    "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                    "\n        dif_sar92_asociado,",
                    "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                    "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                    "\n        dif_viv97_asociado",
                    "\n        ",
                    "\n   FROM sep_his_preliquida_op28",
                    "\n  WHERE folio = ?",
                    "\n    AND (dif_sar92_invadido <> 0",
                    "\n     OR dif_viv97_invadido <> 0",
                    "\n     OR dif_sar92_asociado <> 0",
                    "\n     OR dif_viv97_asociado <> 0)",
                    "\n    AND (id_expediente IS NULL OR id_expediente = 0)"
    
   PREPARE prp_con_dif_sin_exp FROM v_consulta
   DECLARE cur_con_dif_sin_exp CURSOR FOR prp_con_dif_sin_exp 
   FOREACH cur_con_dif_sin_exp USING p_folio
                                INTO v_his_preliquidacion_aux.*

      LET v_global[v_indice].v_indicador_tabla = 1
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_inv
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_inv
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_inv  --USING "-###,###,##&.&&"

      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_inv  --USING "-###,###,##&.&&"



      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 1
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_asoc
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_asoc
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_asoc  --USING "-###,###,##&.&&"

      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_asoc  --USING "-###,###,##&.&&"

      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 1
      
      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_con_dif_sin_exp

   # elimina el ultimo registro, el ultimo registro es nulo
   --CALL v_global.deleteElement(v_global.getLength())


   # SECCIÓN SIN DIFERENCIA SIN EXPEDIENTE
   LET v_indice = v_global.getLength() + 1
   {LET  v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                     "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                     "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                     "\n        dif_sar92_invadido,",
                     "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                     "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                     "\n        dif_viv97_invadido",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_invadido = 0",
                     "\n     OR dif_viv97_invadido = 0)",
                     "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)",
                     "\n UNION ALL",
                     "\n SELECT id_his_preliquida_op28,'A',asociado,",
                     "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                     "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                     "\n        dif_sar92_asociado,",
                     "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                     "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                     "\n        dif_viv97_asociado",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_asociado = 0",
                     "\n     OR dif_viv97_asociado = 0)",
                     "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)"}

   LET  v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                     "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                     "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                     "\n        dif_sar92_invadido,",
                     "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                     "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                     "\n        dif_viv97_invadido,",
                     "\n        'A',asociado,",
                     "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                     "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                     "\n        dif_sar92_asociado,",
                     "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                     "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                     "\n        dif_viv97_asociado",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_invadido = 0",
                     "\n     AND dif_viv97_invadido = 0",
                     "\n     AND dif_sar92_asociado = 0",
                     "\n     AND dif_viv97_asociado = 0)",
                     --"\n     OR dif_viv97_invadido = 0",
                     --"\n     OR dif_sar92_asociado = 0",
                     --"\n     OR dif_viv97_asociado = 0)",
                     "\n    AND (id_expediente IS NULL OR id_expediente = 0)"
   PREPARE prp_sin_dif_sin_exp FROM v_consulta
   DECLARE cur_sin_dif_sin_exp CURSOR FOR prp_sin_dif_sin_exp 
   FOREACH cur_sin_dif_sin_exp USING p_folio
                                INTO v_his_preliquidacion_aux.*
      
      LET v_global[v_indice].v_indicador_tabla = 2
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_inv
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_inv
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_inv  --USING "-###,###,##&.&&"

      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_inv  --USING "-###,###,##&.&&"



      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 2
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_asoc
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_asoc
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_asoc  --USING "-###,###,##&.&&"

      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_asoc  --USING "-###,###,##&.&&"

      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 2

      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_sin_dif_sin_exp

   # elimina el ultimo registro, el ultimo registro es nulo
   --CALL v_global.deleteElement(v_global.getLength())

   # SECCIÓN CON DIFERENCIAS CON EXPEDIENTE
   LET v_indice = v_global.getLength() + 1
   {LET  v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                     "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                     "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                     "\n        dif_sar92_invadido,",
                     "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                     "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                     "\n        dif_viv97_invadido",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_invadido <> 0",
                     "\n     OR dif_viv97_invadido <> 0)",
                     "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)",
                     "\n UNION ALL",
                     "\n SELECT id_his_preliquida_op28,'A',asociado,",
                     "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                     "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                     "\n        dif_sar92_asociado,",
                     "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                     "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                     "\n        dif_viv97_asociado",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_asociado <> 0",
                     "\n     OR dif_viv97_asociado <> 0)",
                     "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)"}

   LET  v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                     "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                     "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                     "\n        dif_sar92_invadido,",
                     "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                     "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                     "\n        dif_viv97_invadido,",
                     "\n        'A',asociado,",
                     "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                     "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                     "\n        dif_sar92_asociado,",
                     "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                     "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                     "\n        dif_viv97_asociado",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_invadido <> 0",
                     "\n     OR dif_viv97_invadido <> 0",
                     "\n     OR dif_sar92_asociado <> 0",
                     "\n     OR dif_viv97_asociado <> 0)",
                    -- "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)"  jdym
                     "\n    AND (id_expediente IS NOT NULL AND id_expediente > 0)" 
   PREPARE prp_con_dif_con_exp FROM v_consulta
   DECLARE cur_con_dif_con_exp CURSOR FOR prp_con_dif_con_exp 
   FOREACH cur_con_dif_con_exp USING p_folio
                                INTO v_his_preliquidacion_aux.*

      LET v_global[v_indice].v_indicador_tabla = 3
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_inv
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_inv
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_inv  --USING "-###,###,##&.&&"

      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 3
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_asoc
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_asoc
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_asoc  --USING "-####,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 3

      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_con_dif_con_exp

   # elimina el ultimo registro, el ultimo registro es nulo
   --CALL v_global.deleteElement(v_global.getLength())

   # SECCIÓN SIN DIFERENCIAS CON EXPEDIENTE
   LET v_indice = v_global.getLength() + 1
   {LET  v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                     "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                     "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                     "\n        dif_sar92_invadido,",
                     "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                     "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                     "\n        dif_viv97_invadido",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_invadido = 0",
                     "\n     OR dif_viv97_invadido = 0)",
                     "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)",
                     "\n UNION ALL",
                     "\n SELECT id_his_preliquida_op28,'A',asociado,",
                     "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                     "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                     "\n        dif_sar92_asociado,",
                     "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                     "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                     "\n        dif_viv97_asociado",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_asociado = 0",
                     "\n     OR dif_viv97_asociado = 0)",
                     "\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)"}

   LET  v_consulta = "\n SELECT id_his_preliquida_op28,'I',invadido,",
                     "\n        sdo_origen_sar92_invadido,op28_sar92_invadido,",
                     "\n        cargo_sar92_invadido,sdo_final_sar92_invadido,",
                     "\n        dif_sar92_invadido,",
                     "\n        sdo_origen_viv97_invadido,op28_viv97_invadido,",
                     "\n        cargo_viv97_invadido,sdo_final_viv97_invadido,",
                     "\n        dif_viv97_invadido,",
                     "\n        'A',asociado,",
                     "\n        sdo_origen_sar92_asociado,op28_sar92_asociado,",
                     "\n        abono_sar92_asociado,sdo_final_sar92_asociado,",
                     "\n        dif_sar92_asociado,",
                     "\n        sdo_origen_viv97_asociado,op28_viv97_asociado,",
                     "\n        abono_viv97_asociado,sdo_final_viv97_asociado,",
                     "\n        dif_viv97_asociado",
                     "\n   FROM sep_his_preliquida_op28",
                     "\n  WHERE folio = ?",
                     "\n    AND (dif_sar92_invadido = 0",
                     --"\n     OR dif_viv97_invadido = 0",
                     --"\n     OR dif_sar92_asociado = 0",
                     --"\n     OR dif_viv97_asociado = 0)",
                     "\n     AND dif_viv97_invadido = 0",
                     "\n     AND dif_sar92_asociado = 0",
                     "\n     AND dif_viv97_asociado = 0)",
                     --"\n    AND (id_expediente IS NOT NULL OR id_expediente = 0)" jdym
                     "\n    AND (id_expediente IS NOT NULL AND id_expediente > 0)" 
                     
   PREPARE prp_sin_dif_con_exp FROM v_consulta
   DECLARE cur_sin_dif_con_exp CURSOR FOR prp_sin_dif_con_exp 
   FOREACH cur_sin_dif_con_exp USING p_folio
                                INTO v_his_preliquidacion_aux.*

      LET v_global[v_indice].v_indicador_tabla = 4
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_inv
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_inv
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_inv  --USING "-###,###,##&.&&"

      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_inv  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_inv --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_inv  --USING "-###,###,##&.&&"



      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 4
      LET v_global[v_indice].v_id_his_prel = v_his_preliquidacion_aux.v_id_his_prel
      LET v_global[v_indice].v_tipo_nss    = v_his_preliquidacion_aux.v_tipo_nss_asoc
      LET v_global[v_indice].v_nss         = v_his_preliquidacion_aux.v_nss_asoc
      
      LET v_global[v_indice].v_sdo_sar92   = v_his_preliquidacion_aux.v_sdo_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_sar92  = v_his_preliquidacion_aux.v_op28_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_sar92   = v_his_preliquidacion_aux.v_mov_sar92_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_sar92  = v_his_preliquidacion_aux.v_sdof_sar92_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_sar92   = v_his_preliquidacion_aux.v_dif_sar92_asoc  --USING "-###,###,##&.&&"

      LET v_global[v_indice].v_sdo_viv97   = v_his_preliquidacion_aux.v_sdo_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_op28_viv97  = v_his_preliquidacion_aux.v_op28_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_mov_viv97   = v_his_preliquidacion_aux.v_mov_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_sdof_viv97  = v_his_preliquidacion_aux.v_sdof_viv97_asoc --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_global[v_indice].v_dif_viv97   = v_his_preliquidacion_aux.v_dif_viv97_asoc  --USING "-###,###,##&.&&"
      LET v_indice = v_indice + 1

      LET v_global[v_indice].v_indicador_tabla = 4

      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_sin_dif_con_exp

   # elimina el ultimo registro, el ultimo registro es nulo
   --CALL v_global.deleteElement(v_global.getLength())

   # Si no hay información, termina sin crear el programa
   IF(v_global.getLength() = 0)THEN
      DISPLAY "NO FUÉ POSIBLE GENERAR REPORTE PRELIQUIDACIÓN OP 28"
      EXIT PROGRAM
   END IF

   # CREACIÓN DE REPORTE
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPI331.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario_cod CLIPPED, "-SEPL33-", 
                          p_pid USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_preliquidacion USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT fn_rpt_preliquidacion_op28 TO XML HANDLER v_manejador_rpt
         
         FOR v_indice = 1 TO v_global.getLength()
            OUTPUT TO REPORT fn_rpt_preliquidacion_op28(v_global[v_indice].*)
         END FOR
      FINISH REPORT fn_rpt_preliquidacion_op28
      
   ELSE
      DISPLAY "No fue posible generar el reporte"
      --CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
      --                   RETURNING r_b_valida
         
      --IF(r_b_valida <> 0)THEN
      --  # En caso de error se muestra un mensaje a usuario y no continua
      --  CALL fn_desplega_inc_operacion(r_b_valida)
      --  EXIT PROGRAM
      --END IF
   END IF

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI33                                                   #
#Descripcion       => Reporte de Preliquidación de op28                        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 12, 2012                                           #
################################################################################
REPORT fn_rpt_preliquidacion_op28(v_global_rpt)
DEFINE v_pagina SMALLINT,
       v_global_rpt RECORD
          v_indicador_tabla SMALLINT,
          v_id_his_prel  INTEGER,--LIKE sep_his_preliquida_op28.id_his_preliquida_op28,
          v_tipo_nss     VARCHAR(8),
          v_nss          LIKE sep_his_preliquida_op28.invadido,
          v_sdo_sar92    LIKE sep_his_preliquida_op28.sdo_origen_sar92_invadido,
          v_op28_sar92   LIKE sep_his_preliquida_op28.op28_sar92_invadido,
          v_mov_sar92    LIKE sep_his_preliquida_op28.cargo_sar92_invadido,
          v_sdof_sar92   LIKE sep_his_preliquida_op28.sdo_final_sar92_invadido,
          v_dif_sar92    LIKE sep_his_preliquida_op28.dif_sar92_invadido,
          v_sdo_viv97    LIKE sep_his_preliquida_op28.sdo_origen_viv97_invadido,
          v_op28_viv97   LIKE sep_his_preliquida_op28.op28_viv97_invadido,
          v_mov_viv97    LIKE sep_his_preliquida_op28.cargo_viv97_invadido,
          v_sdof_viv97   LIKE sep_his_preliquida_op28.sdo_final_viv97_invadido,
          v_dif_viv97    LIKE sep_his_preliquida_op28.dif_viv97_invadido
       END RECORD,
       v_montos_procesar_1 RECORD
         v_clasificacion  LIKE sep_det_02_op28.clasifica_separacion,
         v_invadido_sar92 LIKE sep_det_02_op28.saldo_viv_92,
         v_asociado_sar92 LIKE sep_det_03_op28.saldo_viv_92,
         v_invadido_viv97 LIKE sep_det_02_op28.saldo_viv_97,
         v_asociado_viv97 LIKE sep_det_03_op28.saldo_viv_97,
         v_total_sar92    LIKE sep_det_02_op28.saldo_viv_92,
         v_total_viv97    LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas  INTEGER
       END RECORD,
       v_montos_procesar_2 RECORD
         v_clasificacion  LIKE sep_det_02_op28.clasifica_separacion,
         v_invadido_sar92 LIKE sep_det_02_op28.saldo_viv_92,
         v_asociado_sar92 LIKE sep_det_03_op28.saldo_viv_92,
         v_invadido_viv97 LIKE sep_det_02_op28.saldo_viv_97,
         v_asociado_viv97 LIKE sep_det_03_op28.saldo_viv_97,
         v_total_sar92    LIKE sep_det_02_op28.saldo_viv_92,
         v_total_viv97    LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas  INTEGER
       END RECORD,
       v_montos_procesar_3 RECORD
         v_clasificacion  LIKE sep_det_02_op28.clasifica_separacion,
         v_invadido_sar92 LIKE sep_det_02_op28.saldo_viv_92,
         v_asociado_sar92 LIKE sep_det_03_op28.saldo_viv_92,
         v_invadido_viv97 LIKE sep_det_02_op28.saldo_viv_97,
         v_asociado_viv97 LIKE sep_det_03_op28.saldo_viv_97,
         v_total_sar92    LIKE sep_det_02_op28.saldo_viv_92,
         v_total_viv97    LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas  INTEGER
       END RECORD,
       v_montos_procesar_4 RECORD
         v_clasificacion  LIKE sep_det_02_op28.clasifica_separacion,
         v_invadido_sar92 LIKE sep_det_02_op28.saldo_viv_92,
         v_asociado_sar92 LIKE sep_det_03_op28.saldo_viv_92,
         v_invadido_viv97 LIKE sep_det_02_op28.saldo_viv_97,
         v_asociado_viv97 LIKE sep_det_03_op28.saldo_viv_97,
         v_total_sar92    LIKE sep_det_02_op28.saldo_viv_92,
         v_total_viv97    LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas  INTEGER
       END RECORD
       
   FORMAT

      FIRST PAGE HEADER
         PRINTX v_encabezado.*
         LET v_montos_procesar_1.* = v_montos_procesar[1].* 
         PRINTX v_montos_procesar_1.*
         
         LET v_montos_procesar_2.* = v_montos_procesar[2].*
         PRINTX v_montos_procesar_2.*

         LET v_montos_procesar_3.* = v_montos_procesar[3].*
         PRINTX v_montos_procesar_3.*

         LET v_montos_procesar_4.* = v_montos_procesar[4].*
         PRINTX v_montos_procesar_4.*

         PRINTX v_sum_mtos_procesar_rpt.*
      {BEFORE GROUP OF v_montos_procesar_rpt.v_clasificacion
         PRINTX v_montos_procesar_rpt.*}
         
      
      BEFORE GROUP OF v_global_rpt.v_indicador_tabla
         
      ON EVERY ROW
         PRINTX v_global_rpt.*
         
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT
