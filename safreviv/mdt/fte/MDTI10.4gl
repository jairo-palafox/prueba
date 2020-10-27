--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11-07-2013
--==============================================================================

################################################################################
#Modulo            => MDTI                                                     #
#Programa          => MDTI31                                                   #
#Objetivo          => Reporte de pago de mandatos                              # 
#Autor             => Hugo Ramírez                                             #
#Fecha inicio      => 11 Julio 2013                                            #
################################################################################
DATABASE safre_viv

{===============================================================================
Nombre: fn_consulta_individual
Fecha creacion: 16 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar y generar reporte de consulta individual de movimientos
 de mandatos
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_consulta_individual(p_filtro,p_id_derechohabiente,p_id_cat_mandato,p_abonos,p_pagos, p_tpo_consulta)
DEFINE p_filtro             STRING,
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_cat_mandato     LIKE mdt_cat_mandato.id_cat_mandato,
       p_abonos             STRING,
       p_pagos              STRING,
       p_tpo_consulta       STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_nss             LIKE afi_derechohabiente.nss,
       v_nombre_completo VARCHAR(50),
       v_nombre          LIKE afi_derechohabiente.nombre_af,
       v_ap_paterno      LIKE afi_derechohabiente.ap_paterno_af,
       v_ap_materno      LIKE afi_derechohabiente.ap_materno_af,
       v_indice          SMALLINT,
       v_nombre_reporte  STRING,
       v_manejador_rpt   OM.SaxDocumentHandler,
       v_consulta        STRING,
       v_movimientos     DYNAMIC ARRAY OF RECORD
         v_desc_mandato    LIKE mdt_cat_mandato.desc_mandato,
         v_f_liquida       LIKE cta_movimiento.f_liquida,
         v_movimiento      LIKE cta_movimiento.movimiento,
         v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
         v_periodo_pago    LIKE mdt_det_aplica_mandato.periodo_pago,
         v_folio_liquida   LIKE cta_movimiento.folio_liquida,
         v_monto_pesos     LIKE cta_movimiento.monto_pesos,
         v_estado          LIKE mdt_det_aplica_monto.estado
       END RECORD,
       v_cadena_aux        STRING

   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "mdt"

   LET v_indice = 1
   CALL v_movimientos.clear()
   IF( fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED||"/MDTI101.4rp") )THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nombre_reporte = v_ruta_listados CLIPPED ,"/consulta_individual"
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_nombre_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(1)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      SELECT nss,
             nombre_af,
             ap_paterno_af,
             ap_materno_af
        INTO v_nss,
             v_nombre,
             v_ap_paterno,
             v_ap_materno
        FROM afi_derechohabiente
       WHERE id_derechohabiente = p_id_derechohabiente

      LET v_nombre_completo = v_nombre CLIPPED," ", v_ap_paterno CLIPPED," ", v_ap_materno CLIPPED
      
      CASE p_id_cat_mandato # determina si es consulta por mandato
         WHEN 0 # Consulta Individual
            LET v_cadena_aux = " "
         OTHERWISE # Consulta por mandatos
            LET v_cadena_aux = "\n    AND mto.id_cat_mandato = ?"
      END CASE
      LET v_consulta = "\n SELECT *",
                       "\n   FROM TABLE(MULTISET(",
                       "\n SELECT cat.desc_mandato,",
                       "\n        cta.f_liquida,",
                       "\n        cta.movimiento,",
                       "\n        mov.movimiento_desc,",
--                       "\n        mto.periodo_pago,",
                       "\n        '',",
                       "\n        cta.folio_liquida,",
                       "\n        cta.monto_pesos,",
                       "\n        mto.estado",
                       "\n   FROM afi_derechohabiente afi",
                       "\n        JOIN TABLE(MULTISET(",
                       "\n                       SELECT f_liquida,",
                       "\n                              id_derechohabiente,",
                       "\n                              subcuenta,",
                       "\n                              movimiento,",
                       "\n                              id_referencia,",
                       "\n                              folio_liquida,",
                       "\n                              monto_pesos,",
                       "\n                              origen",
                       "\n                         FROM cta_movimiento",
                       "\n                        WHERE movimiento IN (",p_abonos,"))) cta",
                       "\n     ON afi.id_derechohabiente = cta.id_derechohabiente",
                       "\n        JOIN cat_subcuenta sub",
                       "\n     ON sub.subcuenta = cta.subcuenta",
                       "\n        JOIN cat_movimiento mov",
                       "\n     ON mov.movimiento = cta.movimiento",
                       "\n        JOIN hps_det_aplica_servicio mto",
                       "\n     ON cta.id_referencia = mto.id_det_aplica_servicio",
                       "\n        JOIN mdt_cat_mandato cat",
                       "\n     ON cat.id_cat_mandato = mto.id_cat_mandato",
                       --"\n        JOIN mdt_cat_mandato_paquete paq",
                       --"\n     ON paq.cve_mandato = mdt.cve_mandato",
                       
                       --"\n        JOIN hps_det_aplica_servicio apl",
                       --"\n     ON apl.id_det_aplica_servicio = mto.id_det_aplica_servicio",
                       
                       "\n  WHERE ",p_filtro,
                       "\n    AND cta.id_derechohabiente = ?",
                       --"\n    AND mto.id_cat_mandato = ?",
                       v_cadena_aux, # determina si es consulta por mandato
                       
                       "\n UNION ALL",
                    
                       "\n SELECT cat.desc_mandato,",
                       "\n        cta.f_liquida,",
                       "\n        cta.movimiento,",
                       "\n        mov.movimiento_desc,",
                       "\n        '',",
                       "\n        cta.folio_liquida,",
                       "\n        cta.monto_pesos,",
                       "\n        0",
                       "\n   FROM afi_derechohabiente afi",
                       "\n        JOIN TABLE(MULTISET(",
                       "\n                       SELECT f_liquida,",
                       "\n                              id_derechohabiente,",
                       "\n                              subcuenta,",
                       "\n                              movimiento,",
                       "\n                              id_referencia,",
                       "\n                              folio_liquida,",
                       "\n                              monto_pesos,",
                       "\n                              origen",
                       "\n                         FROM cta_movimiento",
                       "\n                        WHERE movimiento IN (",p_pagos,"))) cta",
                       "\n     ON afi.id_derechohabiente = cta.id_derechohabiente",
                       "\n        JOIN cat_subcuenta sub",
                       "\n     ON sub.subcuenta = cta.subcuenta",
                       "\n        JOIN cat_movimiento mov",
                       "\n     ON mov.movimiento = cta.movimiento",
                       "\n        JOIN hps_det_aplica_pago_servicio pag",
                       "\n     ON cta.id_referencia = pag.id_det_aplica_pago_servicio",
                       "\n        JOIN TABLE(MULTISET(SELECT id_derechohabiente,",
                       "\n                                   id_det_aplica_pago_servicio,",
                       "\n                                   id_cat_mandato",
                       "\n                              FROM hps_det_aplica_monto",
                       "\n                             WHERE 1 = 1",
                       "\n                             GROUP BY 1,2,3)) mto",
                       "\n     ON mto.id_det_aplica_pago_servicio = pag.id_det_aplica_pago_servicio",
                       "\n        JOIN mdt_cat_mandato cat",
                       "\n     ON cat.id_cat_mandato = mto.id_cat_mandato",
                       "\n  WHERE ",p_filtro,
                       "\n    AND cta.id_derechohabiente = ?",
                       --"\n    AND mto.id_cat_mandato = ?"
                       v_cadena_aux, # determina si es consulta por mandato
                       "\n ))",
                       "\n ORDER BY 1,2"
                       
      PREPARE prp_rec_movimientos FROM v_consulta
      DECLARE cur_rec_movimientos CURSOR FOR prp_rec_movimientos
      CASE p_id_cat_mandato # determina si es consulta por mandato o individual
         WHEN 0 
            FOREACH cur_rec_movimientos USING p_id_derechohabiente,
                                              p_id_derechohabiente
                                         INTO v_movimientos[v_indice].*
               LET v_indice = v_indice + 1

            END FOREACH 
            FREE cur_rec_movimientos
            IF(v_movimientos[v_movimientos.getLength()].v_desc_mandato IS NULL)THEN
               CALL v_movimientos.deleteElement(v_movimientos.getLength())
            END IF
         OTHERWISE 
            FOREACH cur_rec_movimientos USING p_id_derechohabiente,
                                              p_id_cat_mandato,
                                              p_id_derechohabiente,
                                              p_id_cat_mandato
                                         INTO v_movimientos[v_indice].*
               LET v_indice = v_indice + 1

            END FOREACH 
            FREE cur_rec_movimientos
            IF(v_movimientos[v_movimientos.getLength()].v_desc_mandato IS NULL)THEN
               CALL v_movimientos.deleteElement(v_movimientos.getLength())
            END IF
      END CASE 
      

      IF( v_movimientos.getLength() > 0 )THEN
         START REPORT fn_rpt_consulta_inividual TO XML HANDLER v_manejador_rpt
            FOR v_indice = 1 TO v_movimientos.getLength() 
               OUTPUT TO REPORT fn_rpt_consulta_inividual(p_tpo_consulta,
                                                          v_movimientos[v_indice].*,
                                                          v_nss,
                                                          v_nombre_completo,
                                                          p_abonos,
                                                          p_pagos)
            END FOR
         FINISH REPORT fn_rpt_consulta_inividual
      ELSE
         DISPLAY "\n"
         DISPLAY "NO SE ENCONTRARON REGISTROS"
         DISPLAY "\n"
         RETURN
      END IF

   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF

END FUNCTION

{===============================================================================
Nombre: fn_recupera_mandatos
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
REPORT fn_rpt_consulta_inividual(p_tpo_consulta,p_movimientos,p_nss,p_nombre_completo,p_abonos,p_pagos)
DEFINE p_tpo_consulta      STRING,
       p_movimientos       RECORD
         v_desc_mandato    LIKE mdt_cat_mandato.desc_mandato,
         v_f_liquida       LIKE cta_movimiento.f_liquida,
         v_movimiento      LIKE cta_movimiento.movimiento,
         v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
         v_periodo_pago    LIKE mdt_det_aplica_mandato.periodo_pago,
         v_folio_liquida   LIKE cta_movimiento.folio_liquida,
         v_monto_pesos     LIKE cta_movimiento.monto_pesos,
         v_estado          LIKE mdt_det_aplica_monto.estado
       END RECORD,
       p_nss             LIKE afi_derechohabiente.nss,
       p_nombre_completo VARCHAR(50),
       p_abonos          STRING,
       p_pagos           STRING,
       v_pagina          SMALLINT,
       v_abonado         LIKE cta_movimiento.monto_pesos,
       v_pagado          LIKE cta_movimiento.monto_pesos,
       v_saldo           LIKE cta_movimiento.monto_pesos,
       v_token           base.StringTokenizer,
       v_indice          SMALLINT,
       v_mov_pagos       ARRAY[3] OF SMALLINT,
       v_mov_abonos      ARRAY[3] OF SMALLINT

   --ORDER BY p_movimientos.v_desc_mandato
   FORMAT

      FIRST PAGE HEADER
         # separa los movimientos de abonos
         LET v_token = base.StringTokenizer.create(p_abonos,",")
         LET v_indice = 1
         WHILE v_token.hasMoreTokens()
            LET v_mov_abonos[v_indice] = v_token.nextToken()
            LET v_indice = v_indice + 1
         END WHILE
         # separa los movimientos de pagos
         LET v_token = base.StringTokenizer.create(p_pagos,",")
         LET v_indice = 1
         WHILE v_token.hasMoreTokens()
            LET v_mov_pagos[v_indice] = v_token.nextToken()
            LET v_indice = v_indice + 1
         END WHILE
         
         PRINTX p_tpo_consulta,
                p_nss,
                p_nombre_completo

      BEFORE GROUP OF p_movimientos.v_desc_mandato
         LET v_abonado = 0
         LET v_pagado  = 0
         LET v_saldo   = 0
         PRINTX p_movimientos.v_desc_mandato

      AFTER GROUP OF p_movimientos.v_desc_mandato
         LET v_saldo = v_abonado + v_pagado
         PRINTX v_abonado,
                v_pagado,
                v_saldo

      ON EVERY ROW
         FOR v_indice = 1 TO v_mov_pagos.getLength()
            IF(p_movimientos.v_movimiento = v_mov_pagos[v_indice])THEN
               LET v_pagado = v_pagado + p_movimientos.v_monto_pesos
            END IF

         END FOR
         FOR v_indice = 1 TO v_mov_abonos.getLength()
            IF(p_movimientos.v_movimiento = v_mov_abonos[v_indice])THEN
               LET v_abonado = v_abonado + p_movimientos.v_monto_pesos
            END IF
         END FOR
         
         PRINTX p_movimientos.v_f_liquida,
                p_movimientos.v_movimiento_desc,
                p_movimientos.v_periodo_pago,
                p_movimientos.v_folio_liquida,
                p_movimientos.v_monto_pesos,
                p_movimientos.v_estado

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina
END REPORT