--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11-07-2013
--==============================================================================

################################################################################
#Modulo            => HPSI                                                     #
#Programa          => HPSI31                                                   #
#Objetivo          => Reporte de pago de mandatos                              # 
#Autor             => Jesus Yañez                                              #
#Fecha inicio      => 11 Julio 2013                                            #
################################################################################
DATABASE safre_viv

FUNCTION fn_mdt_rpt_aplicacion_mdt(p_folio,
                                   p_estado,
                                   p_usuario_cod,
                                   p_pid,
                                   p_proceso_cod,
                                   p_opera_preliquidacion,
                                   p_f_proceso,
                                   p_archivo_batch)
                                   
DEFINE p_folio                LIKE glo_folio.folio,
       p_estado               LIKE mdt_det_aplica_monto.estado,
       p_usuario_cod          LIKE mdt_ctr_aplica_pago_mandato.usuario,
       p_pid                  LIKE bat_ctr_operacion.pid,
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod,
       p_opera_preliquidacion LIKE bat_ctr_operacion.opera_cod,
       p_f_proceso            DATE,
       p_archivo_batch        STRING,
       
       v_registros  DYNAMIC ARRAY OF RECORD
         v_tpo_mandato         LIKE mdt_tpo_mandato.tpo_mandato,
         v_tpo_mandato_desc    LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_ent_federativa      LIKE mdt_det_aplica_pago_mandato.ent_federativa,
         v_ent_federativa_desc LIKE cat_entidad_federativa.entidad_desc_larga,
         v_municipio           LIKE mdt_det_aplica_pago_mandato.municipio,
         v_municipio_desc      LIKE cat_municipio.municipio_desc,
         v_mandato             LIKE mdt_det_aplica_monto.id_cat_mandato,
         v_mandato_desc        LIKE mdt_cat_mandato.desc_mandato,
         v_monto_pesos         LIKE mdt_det_aplica_pago_mandato.monto_pesos
       END RECORD,
       
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_indice          SMALLINT,
       v_nombre_reporte  STRING,
       v_manejador_rpt   OM.SaxDocumentHandler,
       v_consulta        STRING,
       v_sum_prediales     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_mantenimiento LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_servicios     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_texto_rpt         STRING
       
   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   IF(fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED||"/HPSI311.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nombre_reporte = p_usuario_cod CLIPPED, "-",p_archivo_batch,"-",--"-HPSP01-", 
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
      LET v_sum_prediales     = 0
      LET v_sum_mantenimiento = 0
      LET v_sum_servicios     = 0

      
      CASE p_estado
      
         WHEN 50 # liquidacion traspaso fondo servicios

            LET v_texto_rpt = "PRELIQUIDACIÓN TRASPASO SCTA SERVICIOS"
            LET v_consulta = " SELECT tpo.tpo_mandato,",
                             " tpo.desc_tpo_mandato, ",
                             " ent.entidad_federativa, ",
                             " ent.valor_etiqueta, ",
                             " mun.municipio, ",
                             " mun.valor_etiqueta, ",
                             " cat.id_cat_mandato, ",
                             " cat.desc_mandato, ",
                             " SUM(mto.mto_pesos) ",
                        " FROM mdt_tpo_mandato tpo LEFT OUTER JOIN mdt_cat_mandato cat ",
                          " ON cat.tpo_mandato = tpo.tpo_mandato ",
                             " JOIN TABLE(MULTISET(SELECT id_cat_mandato, ",
                                                        " id_ctr_aplica_servicio, ",
                                                        " mto_pesos ",
                                                   " FROM hps_det_aplica_servicio  ",
                                                  " WHERE estado = ? ",
                                                  " )) mto ",                                                  
                          " ON mto.id_cat_mandato = cat.id_cat_mandato ",
                             " JOIN hps_ctr_aplica_servicio apctr ",
                          " ON mto.id_ctr_aplica_servicio = apctr.id_ctr_aplica_servicio ",
                             " JOIN ",
                             " TABLE(MULTISET(SELECT nvl.id_cat_mandato, ",
                                                   " ent.entidad_federativa, ",
                                                   " ins.valor_etiqueta ",
                                              " FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins ",
                                                " ON ins.id_atr_nivel = nvl.id_atr_nivel ",
                                                   " JOIN mdt_cat_gpo_etiqueta etq ",
                                                " ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta ",
                                                   " JOIN cat_entidad_federativa ent ",
                                                " ON ent.entidad_desc_larga = ins.valor_etiqueta ",
                                             " WHERE etq.etiqueta = 'ENTIDAD FEDERATIVA')) ent ",
                          " ON ent.id_cat_mandato = cat.id_cat_mandato ",
                             " JOIN  ",
                             " TABLE(MULTISET(SELECT mun.municipio,  ",
                                                   " nvl.id_cat_mandato, ",
                                                   " mun.entidad_federativa, ",
                                                   " ins.valor_etiqueta ",
                                              " FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins ",
                                                " ON ins.id_atr_nivel = nvl.id_atr_nivel ",
                                                   " JOIN mdt_cat_gpo_etiqueta etq ",
                                                " ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta ",
                                                   " JOIN cat_municipio mun ",
                                                " ON municipio_desc = ins.valor_etiqueta ",
                                             " WHERE etq.etiqueta = 'MUNICIPIO')) mun ",
                          " ON mun.id_cat_mandato = cat.id_cat_mandato ",
                         " AND mun.entidad_federativa = ent.entidad_federativa ",
                       " WHERE apctr.folio_aplica_servicio = ? ",
                       " GROUP BY 1,2,3,4,5,6,7,8 ",
                       " ORDER BY 1,3,5 "

            PREPARE prp_rec_datos_rpt_abonos FROM v_consulta
            DECLARE cur_rec_datos_rpt_abonos CURSOR FOR prp_rec_datos_rpt_abonos
            FOREACH cur_rec_datos_rpt_abonos USING p_estado,
                                                   p_folio
                                              INTO v_registros[v_indice].*
 
               CASE v_registros[v_indice].v_tpo_mandato

                  WHEN 1 # Prediales
                     LET v_sum_prediales     = v_sum_prediales + v_registros[v_indice].v_monto_pesos      

                  WHEN 2 # Mantenimiento
                     LET v_sum_mantenimiento = v_sum_mantenimiento + v_registros[v_indice].v_monto_pesos      

                  WHEN 3 # Servicios
                     LET v_sum_servicios     = v_sum_servicios + v_registros[v_indice].v_monto_pesos

               END CASE
               LET v_indice = v_indice + 1      

            END FOREACH
            FREE cur_rec_datos_rpt_abonos

         WHEN 100 # abonado
            LET v_texto_rpt = "ABONOS MANDATOS"
            LET v_consulta = "\n SELECT tpo.tpo_mandato,",
                             "\n        tpo.desc_tpo_mandato,",
                             "\n        ent.entidad_federativa,",
                             "\n        ent.valor_etiqueta,",
                             "\n        mun.municipio,",
                             "\n        mun.valor_etiqueta,",
                             "\n        cat.id_cat_mandato,",
                             "\n        cat.desc_mandato,",
                             "\n        SUM(mto.monto_pesos)",
                             "\n   FROM mdt_tpo_mandato tpo LEFT OUTER JOIN mdt_cat_mandato cat",
                             "\n     ON cat.tpo_mandato = tpo.tpo_mandato",
                             "\n        JOIN TABLE(MULTISET(SELECT id_cat_mandato,",
                             "\n                                  id_det_aplica_mandato,",
                             "\n                                  monto_pesos",
                             "\n                             FROM mdt_det_aplica_monto",
                             "\n                            WHERE estado = ?",
                             "\n                            GROUP BY 1,2,3)) mto",
                             "\n     ON mto.id_cat_mandato = cat.id_cat_mandato",
                             "\n        JOIN mdt_det_aplica_mandato apl",
                             "\n     ON apl.id_det_aplica_mandato = mto.id_det_aplica_mandato",
                             "\n        JOIN",
                             "\n        TABLE(MULTISET(SELECT nvl.id_cat_mandato,",
                             "\n                              ent.entidad_federativa,",
                             "\n                              ins.valor_etiqueta",
                             "\n                         FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins",
                             "\n                           ON ins.id_atr_nivel = nvl.id_atr_nivel",
                             "\n                              JOIN mdt_cat_gpo_etiqueta etq",
                             "\n                           ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                             "\n                              JOIN cat_entidad_federativa ent",
                             "\n                           ON ent.entidad_desc_larga = ins.valor_etiqueta",
                             "\n                        WHERE etq.etiqueta = 'ENTIDAD FEDERATIVA')) ent",
                             "\n     ON ent.id_cat_mandato = cat.id_cat_mandato",
                             "\n        JOIN ",
                             "\n        TABLE(MULTISET(SELECT mun.municipio, ",
                             "\n                              nvl.id_cat_mandato,",
                             "\n                              mun.entidad_federativa,",
                             "\n                              ins.valor_etiqueta",
                             "\n                         FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins",
                             "\n                           ON ins.id_atr_nivel = nvl.id_atr_nivel",
                             "\n                              JOIN mdt_cat_gpo_etiqueta etq",
                             "\n                           ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                             "\n                              JOIN cat_municipio mun",
                             "\n                           ON municipio_desc = ins.valor_etiqueta",
                             "\n                        WHERE etq.etiqueta = 'MUNICIPIO')) mun",
                             "\n     ON mun.id_cat_mandato = cat.id_cat_mandato",
                             "\n    AND mun.entidad_federativa = ent.entidad_federativa",
                             "\n  WHERE apl.folio_dispersion = ?",
                             "\n  GROUP BY 1,2,3,4,5,6,7,8",
                             "\n  ORDER BY 1,3,5"
            PREPARE prp_rec_datos_rpt_abonado FROM v_consulta
            DECLARE cur_rec_datos_rpt_abonado CURSOR FOR prp_rec_datos_rpt_abonado
            FOREACH cur_rec_datos_rpt_abonado USING p_estado,
                                                  p_folio
                                             INTO v_registros[v_indice].*

               CASE v_registros[v_indice].v_tpo_mandato

                  WHEN 1 # Prediales
                     LET v_sum_prediales     = v_sum_prediales + v_registros[v_indice].v_monto_pesos      

                  WHEN 2 # Mantenimiento
                     LET v_sum_mantenimiento = v_sum_mantenimiento + v_registros[v_indice].v_monto_pesos      

                  WHEN 3 # Servicios
                     LET v_sum_servicios     = v_sum_servicios + v_registros[v_indice].v_monto_pesos

               END CASE
               LET v_indice = v_indice + 1      

            END FOREACH
            FREE cur_rec_datos_rpt_abonado
            
         WHEN 102 # preliquidado pagos
            LET v_texto_rpt = "PRELIQUIDACIÓN PAGO MANDATOS"
            LET v_consulta = "\n SELECT tpo.tpo_mandato,",
                             "\n        tpo.desc_tpo_mandato,",
                             "\n        pgo.ent_federativa,",
                             "\n        ent.entidad_desc_larga,",
                             "\n        pgo.municipio,",
                             "\n        mun.municipio_desc,",
                             "\n        cat.id_cat_mandato,",
                             "\n        cat.desc_mandato,",
                             "\n        SUM(pgo.monto_pesos)",
                             "\n   FROM mdt_tpo_mandato tpo LEFT OUTER JOIN mdt_cat_mandato cat",
                             "\n     ON cat.tpo_mandato = tpo.tpo_mandato",
                             "\n        JOIN TABLE(MULTISET(SELECT id_cat_mandato,",
                             "\n                                  id_det_aplica_pago_servicio,",
                             "\n                                  estado",
                             "\n                             FROM hps_det_aplica_monto",
                             "\n                            WHERE 1 = 1",
                             "\n                            GROUP BY 1,2,3)) mto",
                             "\n     ON mto.id_cat_mandato = cat.id_cat_mandato",
                             "\n        JOIN hps_det_aplica_pago_servicio pgo",
                             "\n     ON mto.id_det_aplica_pago_servicio = pgo.id_det_aplica_pago_servicio",
                             "\n        JOIN hps_ctr_aplica_pago_servicio ctr",
                             "\n     ON pgo.id_ctr_aplica_pago_servicio = ctr.id_ctr_aplica_pago_servicio",
                             "\n        LEFT OUTER JOIN cat_entidad_federativa ent",
                             "\n     ON ent.entidad_federativa = pgo.ent_federativa",
                             "\n        LEFT OUTER JOIN cat_municipio mun",
                             "\n     ON mun.municipio = pgo.municipio",
                             "\n    AND mun.entidad_federativa = pgo.ent_federativa",
                             "\n  WHERE mto.estado = ?", # preliquidado
                             "\n    AND ctr.folio_pago_servicio = ?",
                             "\n  GROUP BY 1,2,3,4,5,6,7,8",
                             "\n  ORDER BY 1,3,5"
            PREPARE prp_rec_datos_rpt FROM v_consulta
            DECLARE cur_rec_datos_rpt CURSOR FOR prp_rec_datos_rpt
            FOREACH cur_rec_datos_rpt USING p_estado,
                                            p_folio
                                       INTO v_registros[v_indice].*

               CASE v_registros[v_indice].v_tpo_mandato

                  WHEN 1 # Prediales
                     LET v_sum_prediales     = v_sum_prediales + v_registros[v_indice].v_monto_pesos      

                  WHEN 2 # Mantenimiento
                     LET v_sum_mantenimiento = v_sum_mantenimiento + v_registros[v_indice].v_monto_pesos      

                  WHEN 3 # Servicios
                     LET v_sum_servicios     = v_sum_servicios + v_registros[v_indice].v_monto_pesos

               END CASE
               LET v_indice = v_indice + 1      

            END FOREACH
            FREE cur_rec_datos_rpt

      END CASE
      
      
      IF(v_registros[v_registros.getLength()].v_tpo_mandato IS NULL)THEN
         CALL v_registros.deleteElement(v_registros.getLength())
      END IF
      IF( v_registros.getLength() > 0 )THEN
         START REPORT fn_rpt_pago_mandatos TO XML HANDLER v_manejador_rpt
            FOR v_indice = 1 TO v_registros.getLength() 
               OUTPUT TO REPORT fn_rpt_pago_mandatos(v_texto_rpt,
                                                     v_registros[v_indice].*,
                                                     p_folio,
                                                     p_f_proceso,
                                                     v_sum_prediales,
                                                     v_sum_mantenimiento,
                                                     v_sum_servicios)
            END FOR
         FINISH REPORT fn_rpt_pago_mandatos
      ELSE
         DISPLAY "\n"
         DISPLAY "NO SE ENCONTRARON REGISTROS PARA EL FOLIO ",p_folio
         DISPLAY "\n"
         RETURN v_registros.getLength()
      END IF
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF
   RETURN v_registros.getLength()
END FUNCTION

{===============================================================================
Nombre: fn_rpt_pago_mandatos
Fecha creacion: 05 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para 
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
REPORT fn_rpt_pago_mandatos(p_texto_rpt,p_registros,p_folio,p_fecha,p_sum_prediales,p_sum_mantenimiento,p_sum_servicios)
DEFINE p_texto_rpt  STRING,
       p_registros RECORD
         v_tpo_mandato         LIKE mdt_tpo_mandato.tpo_mandato,
         v_tpo_mandato_desc    LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_ent_federativa      LIKE mdt_det_aplica_pago_mandato.ent_federativa,
         v_ent_federativa_desc LIKE cat_entidad_federativa.entidad_desc_larga,
         v_municipio           LIKE mdt_det_aplica_pago_mandato.municipio,
         v_municipio_desc      LIKE cat_municipio.municipio_desc,
         v_mandato             LIKE mdt_det_aplica_monto.id_cat_mandato,
         v_mandato_desc        LIKE mdt_cat_mandato.desc_mandato,
         v_monto_pesos         LIKE mdt_det_aplica_pago_mandato.monto_pesos
       END RECORD,
       p_folio                LIKE glo_folio.folio,
       p_fecha                DATE,
       p_sum_prediales     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       p_sum_mantenimiento LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       p_sum_servicios     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_tpo_mandato   LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_entidad       LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_municipio     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_suma_total        LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_pagina            SMALLINT
       
   FORMAT

      FIRST PAGE HEADER
         LET v_suma_total = p_sum_prediales + p_sum_mantenimiento + p_sum_servicios
         PRINTX p_texto_rpt,
                p_folio,
                p_fecha USING "dd-mm-yyyy",
                p_sum_prediales,
                p_sum_mantenimiento,
                p_sum_servicios, 
                v_suma_total

      BEFORE GROUP OF p_registros.v_tpo_mandato
         PRINTX p_registros.v_tpo_mandato_desc

      AFTER GROUP OF p_registros.v_tpo_mandato
         LET v_sum_tpo_mandato = GROUP SUM(p_registros.v_monto_pesos)         
         PRINTX v_sum_tpo_mandato

      BEFORE GROUP OF p_registros.v_ent_federativa
         PRINTX p_registros.v_ent_federativa,
                p_registros.v_ent_federativa_desc

      AFTER GROUP OF p_registros.v_ent_federativa
         LET v_sum_entidad = GROUP SUM(p_registros.v_monto_pesos)
         PRINTX v_sum_entidad

      BEFORE GROUP OF p_registros.v_municipio
         PRINTX p_registros.v_municipio,
                p_registros.v_municipio_desc

      AFTER GROUP OF p_registros.v_municipio
         LET v_sum_municipio = GROUP SUM(p_registros.v_monto_pesos)
         PRINTX v_sum_municipio
                
      ON EVERY ROW
         PRINTX p_registros.v_mandato_desc,
                p_registros.v_monto_pesos

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina         

END REPORT
