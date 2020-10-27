--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11-07-2013
--==============================================================================

################################################################################
#Modulo            => PRTI                                                     #
#Programa          => PRTI31                                                   #
#Objetivo          => Reporte de pago de mandatos                              # 
#Autor             => Jesus Yañez                                              #
#Fecha inicio      => 11 Julio 2013                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTWS02.inc"

FUNCTION fn_rpt_preliquidacion_trasp_ced(p_folio,
                                         p_estado,
                                         p_usuario_cod,
                                         p_pid,
                                         p_proceso_cod,
                                         p_opera_preliquidacion,
                                         p_archivo_batch)
                                   
DEFINE p_folio                LIKE glo_folio.folio,
       p_estado               LIKE prt_solicitud_cedente.estado,
       p_usuario_cod          LIKE seg_usuario.usuario_cod,
       p_pid                  LIKE bat_ctr_operacion.pid,
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod,
       p_opera_preliquidacion LIKE bat_ctr_operacion.opera_cod,
       v_f_proceso            DATE,
       p_archivo_batch        STRING,       
       v_registros  DYNAMIC ARRAY OF RECORD
         v_tpo_traspaso          LIKE prt_tipo_traspaso.tipo_traspaso           ,
         v_tpo_traspaso_desc     LIKE prt_tipo_traspaso.tipo_traspaso_desc      ,
         v_tpo_portabilidad      LIKE prt_tipo_portabilidad.tipo_portabilidad   ,
         v_tpo_portabilidad_desc LIKE prt_tipo_portabilidad.descripcion         ,
         v_subcuenta             LIKE cat_subcuenta.subcuenta                   ,
         v_subcuenta_desc        LIKE cat_subcuenta.subcuenta_desc              ,
         v_movimiento            LIKE cat_movimiento.movimiento                 ,
         v_movimiento_desc       LIKE cat_movimiento.movimiento_desc            ,
         v_monto_pesos           LIKE prt_preliquida.monto_pesos
       END RECORD,       
       v_ruta_ejecutable  LIKE seg_modulo.ruta_bin,
       v_ruta_listados    LIKE seg_modulo.ruta_listados,
       v_indice           SMALLINT,
       v_nombre_reporte   STRING,
       v_manejador_rpt    OM.SaxDocumentHandler,
       v_consulta         STRING,
       v_sum_saldo        LIKE prt_preliquida.monto_pesos     ,
       v_sum_subsecuentes LIKE prt_preliquida.monto_pesos     ,
       v_sum_servicios    LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_texto_rpt        STRING
       
   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "prt"

   IF(fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED||"/PRTI101.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nombre_reporte = p_usuario_cod CLIPPED, "-",
                             p_archivo_batch,"-",
                             p_pid USING "&&&&&", "-", 
                             p_proceso_cod USING "&&&&&", "-", 
                             p_opera_preliquidacion USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nombre_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      LET v_indice = 1
      LET v_sum_saldo     = 0
      LET v_sum_subsecuentes = 0
      LET v_sum_servicios     = 0
      
      LET v_consulta = " SELECT cza.tipo_traspaso , ",
                              " ttr.tipo_traspaso_desc   , ",
                              " scd.tipo_portabilidad    , ",
                              " tpr.descripcion          , ",
                              " mto.subcuenta            , ",
                              " sct.subcuenta_desc       , ",
                              " mto.movimiento           , ",
                              " mov.movimiento_desc      , ",
                              " SUM(mto.monto_pesos)       ",
                       " FROM prt_preliquida         mto    , ",
                            " prt_traspaso_cedente   pcd    , ",
                            " prt_cza_cedente        cza    , ",
                            " prt_solicitud_cedente  scd    , ",
                            " prt_tipo_traspaso      ttr    , ",
                            " prt_tipo_portabilidad  tpr    , ",
                            " cat_subcuenta          sct    , ",
                            " cat_movimiento         mov ",
                       " WHERE mto.folio_liquida            = ? ",
                         " AND mto.id_referencia            = pcd.id_prt_traspaso_cedente ",
                         " AND pcd.folio_liquida            = cza.folio_liquida ",
                         " AND cza.tipo_traspaso            = ttr.tipo_traspaso ",
                         " AND pcd.id_prt_solicitud_cedente = scd.id_prt_solicitud_cedente ",
                         " AND scd.tipo_portabilidad        = tpr.tipo_portabilidad ",
                         " AND mto.subcuenta                = sct.subcuenta ",
                         " AND mto.movimiento               = mov.movimiento ",
                       " GROUP BY 1,2,3,4,5,6,7,8 ",
                       " ORDER BY 1,3,5 "                               
            
      PREPARE prp_rec_datos_rpt_abonos FROM v_consulta
      DECLARE cur_rec_datos_rpt_abonos CURSOR FOR prp_rec_datos_rpt_abonos
      FOREACH cur_rec_datos_rpt_abonos USING p_folio
                                        INTO v_registros[v_indice].*
 
         CASE v_registros[v_indice].v_tpo_traspaso

            WHEN "01" # SALDO
               LET v_sum_saldo        = v_sum_saldo + v_registros[v_indice].v_monto_pesos      

            WHEN "02" # SUBSECUENTES
               LET v_sum_subsecuentes = v_sum_subsecuentes + v_registros[v_indice].v_monto_pesos      

         END CASE
         LET v_indice = v_indice + 1      

      END FOREACH
      FREE cur_rec_datos_rpt_abonos
            
      CASE p_estado
         WHEN C_ESTADO_SDO_PRELIQUIDADO_CED # preliquidacion traspaso
            LET v_texto_rpt = "PRELIQUIDACIÓN PORTABILIDAD CREDITO FOVISSSTE "

         WHEN C_ESTADO_SDO_LIQUIDADO_CED # Saldos liquidados cedente
            LET v_texto_rpt = "PRELIQUIDACIÓN SUBSECUENTES PORTABILIDAD"
         
      END CASE
      
      IF( v_registros[v_registros.getLength()].v_tpo_traspaso IS NULL )THEN
         CALL v_registros.deleteElement(v_registros.getLength())
      END IF
      LET v_f_proceso = TODAY
      IF( v_registros.getLength() > 0 )THEN
         START REPORT fn_rpt_traspasos TO XML HANDLER v_manejador_rpt
            FOR v_indice = 1 TO v_registros.getLength() 
               OUTPUT TO REPORT fn_rpt_traspasos(v_texto_rpt,
                                                 v_registros[v_indice].*,
                                                 p_folio,
                                                 v_f_proceso,
                                                 v_sum_saldo,
                                                 v_sum_subsecuentes)
            END FOR
         FINISH REPORT fn_rpt_traspasos
      ELSE
         DISPLAY "\n"
         DISPLAY "NO SE ENCONTRARON REGISTROS PARA EL FOLIO ",p_folio
         DISPLAY "\n"
         RETURN
      END IF
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF
   
END FUNCTION

# Descripción: Genera reporte de traspasos cedente
REPORT fn_rpt_traspasos(p_texto_rpt,
                        p_registros,
                        p_folio,
                        p_fecha,
                        p_sum_saldo,
                        p_sum_subsecuentes)
DEFINE p_texto_rpt  STRING,
       {p_registros RECORD
         v_tpo_mandato         LIKE mdt_tpo_mandato.tpo_mandato,
         v_tpo_mandato_desc    LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_ent_federativa      LIKE mdt_det_aplica_pago_mandato.ent_federativa,
         v_ent_federativa_desc LIKE cat_entidad_federativa.entidad_desc_larga,
         v_municipio           LIKE mdt_det_aplica_pago_mandato.municipio,
         v_municipio_desc      LIKE cat_municipio.municipio_desc,
         v_mandato             LIKE mdt_det_aplica_monto.id_cat_mandato,
         v_mandato_desc        LIKE mdt_cat_mandato.desc_mandato,
         v_monto_pesos         LIKE mdt_det_aplica_pago_mandato.monto_pesos
       END RECORD,}
       p_registros RECORD
         v_tpo_traspaso          LIKE prt_tipo_traspaso.tipo_traspaso           ,
         v_tpo_traspaso_desc     LIKE prt_tipo_traspaso.tipo_traspaso_desc      ,
         v_tpo_portabilidad      LIKE prt_tipo_portabilidad.tipo_portabilidad   ,
         v_tpo_portabilidad_desc LIKE prt_tipo_portabilidad.descripcion         ,
         v_subcuenta             LIKE cat_subcuenta.subcuenta                   ,
         v_subcuenta_desc        LIKE cat_subcuenta.subcuenta_desc              ,
         v_movimiento            LIKE cat_movimiento.movimiento                 ,
         v_movimiento_desc       LIKE cat_movimiento.movimiento_desc            ,
         v_monto_pesos           LIKE prt_preliquida.monto_pesos
       END RECORD,
       p_folio            LIKE glo_folio.folio,
       p_fecha            DATE,
       p_sum_saldo        LIKE prt_preliquida.monto_pesos,
       p_sum_subsecuentes LIKE prt_preliquida.monto_pesos,
       v_sum_tpo_traspaso LIKE prt_preliquida.monto_pesos,
       v_sum_tpo_prt      LIKE prt_preliquida.monto_pesos,
       v_sum_subcuenta    LIKE prt_preliquida.monto_pesos,
       v_suma_total       LIKE prt_preliquida.monto_pesos,
       v_pagina           SMALLINT
       
   FORMAT

      FIRST PAGE HEADER
         LET v_suma_total = p_sum_saldo + p_sum_subsecuentes
         PRINTX p_texto_rpt,
                p_folio,
                p_fecha USING "dd-mm-yyyy",
                p_sum_saldo,
                p_sum_subsecuentes,
                v_suma_total

      BEFORE GROUP OF p_registros.v_tpo_traspaso
         PRINTX p_registros.v_tpo_traspaso_desc

      AFTER GROUP OF p_registros.v_tpo_traspaso
         LET v_sum_tpo_traspaso = GROUP SUM(p_registros.v_monto_pesos)         
         PRINTX v_sum_tpo_traspaso

      BEFORE GROUP OF p_registros.v_tpo_portabilidad
         PRINTX p_registros.v_tpo_portabilidad,
                p_registros.v_tpo_portabilidad_desc

      AFTER GROUP OF p_registros.v_tpo_portabilidad
         LET v_sum_tpo_prt = GROUP SUM(p_registros.v_monto_pesos)
         PRINTX v_sum_tpo_prt

      BEFORE GROUP OF p_registros.v_subcuenta
         PRINTX p_registros.v_subcuenta,
                p_registros.v_subcuenta_desc

      AFTER GROUP OF p_registros.v_subcuenta
         LET v_sum_subcuenta = GROUP SUM(p_registros.v_monto_pesos)
         PRINTX v_sum_subcuenta
                
      ON EVERY ROW
         PRINTX p_registros.v_movimiento_desc,
                p_registros.v_monto_pesos


      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina         

      ON LAST ROW         
         START REPORT rpt_detalle
            OUTPUT TO REPORT rpt_detalle(p_folio)
         FINISH REPORT rpt_detalle 
                
END REPORT

REPORT rpt_detalle(p_folio)

DEFINE p_folio dec(10,0)
DEFINE r_detalle RECORD 
       nss char(11), 
       nombre char(120),
       curp char(18),
       id_credito_fovissste LIKE prt_solicitud_cedente.id_credito_fovissste,       
       aivs_viv92 LIKE cta_movimiento.monto_acciones,
       pesos_viv92 LIKE cta_movimiento.monto_pesos,
       aivs_viv97 LIKE cta_movimiento.monto_acciones,
       pesos_viv97 LIKE cta_movimiento.monto_pesos,
       pesos_portabilidad LIKE cta_movimiento.monto_pesos
       
END RECORD

FORMAT 

ON EVERY ROW 

   DECLARE cur_1 CURSOR FOR 
   select a.nss ,
          trim(e.paterno)||" "||trim(e.materno)||" "||trim(e.nombre) nombre,
          e.curp,
          e.id_credito_Fovissste,
          d.monto_acciones aivs_viv92,
          d.monto_pesos pesos_viv92 ,
          b.monto_acciones aivs_viv97,
          b.monto_pesos pesos_viv97,
          abs(c.monto_pesos) portabilidad
   from afi_derechohabiente a ,
        cta_movimiento b  ,
        cta_movimiento c  ,
        cta_movimiento d  ,
        prt_solicitud_cedente e
   where b.folio_liquida = p_folio
   and   a.id_derechohabiente = b.id_derechohabiente
   and   b.id_derechohabiente = c.id_derechohabiente
   and   b.folio_liquida = c.folio_liquida
   and   b.movimiento = 1608
   and   c.movimiento = 1610
   and   b.subcuenta  = 4
   and   c.subcuenta = 60
   and   c.folio_liquida = d.folio_liquida
   and   c.id_derechohabiente = d.id_derechohabiente
   and   d.subcuenta = 8
and a.nss = e.nss

   
  FOREACH cur_1 INTO  r_detalle.*
  PRINTX r_detalle.*
  
  END FOREACH
END REPORT


