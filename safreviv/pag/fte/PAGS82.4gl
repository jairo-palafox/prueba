--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/09/2013
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGS80                                                                 #
#Objetivo     => Genera archivo plano de extracción de pagos para la preca opción menu  #
#Fecha inicio => 19 Septiembre de 2014                                                  #
#########################################################################################
DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS
   DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod,
          g_pid          LIKE bat_ctr_proceso.pid,       --  ID del proceso
          g_proceso_cod  LIKE cat_proceso.proceso_cod,
          g_opera_cod    LIKE cat_operacion.opera_cod,
          p_folio        LIKE glo_folio.folio, 
          g_nom_archivo  STRING,
          g_programa     LIKE cat_operacion.programa_cod

END GLOBALS

MAIN

   -- se reciben los parametros del programa
   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET g_nom_archivo = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PAGS80.log")

   DISPLAY "Iniciando generación de extracción de pagos para preca"

   CALL fn_genera_extractor()

END MAIN

{ ==========================================================================
Clave:  fn_genera_extractor
Nombre: fn_genera_extractor
Fecha creacion: 29 de Diciembre de 2011
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Esta función carga un archivo del proceso de "Registro de Pagos"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_genera_extractor()

   DEFINE r_resultado_opera SMALLINT
   DEFINE v_mensaje STRING
   DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc

   SELECT proceso_desc
   INTO   v_proceso_desc
   FROM   cat_proceso
   WHERE  proceso_cod = g_proceso_cod   

      CALL fn_display_proceso(0,"INICIO EXTRACCIÓN")

      CALL fn_genera_extraccion()

      CALL fn_display_proceso(1,"FIN EXTRACCIÓN")

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
      LET v_mensaje = "El proceso de extracción ha finalizado correctamente"

      CALL fn_correo_proceso(g_pid,
                             g_proceso_cod,
                             g_opera_cod,
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - EXTRACTOR PRECA',
                              v_mensaje
                             )
END FUNCTION

FUNCTION fn_genera_extraccion()

   DEFINE v_sql STRING

   DEFINE reg_extractor RECORD
      origen_archivo LIKE cta_his_pagos.origen_archivo,
      f_proceso      LIKE cta_his_pagos.f_proceso,
      folio          DECIMAL(12,0),   ---LIKE cta_his_pagos.folio,
      nss            LIKE afi_derechohabiente.nss,
      nrp            LIKE cta_his_pagos.nrp,
      folio_sua      LIKE cta_his_pagos.folio_sua,
      f_pago         LIKE cta_his_pagos.f_pago,
      bim_pago       LIKE cta_his_pagos.periodo_pago,
      imp_ap_pat     LIKE cta_his_pagos.imp_ap_pat,
      f_ejecucion    LIKE cta_his_pagos.f_proceso,
      id_pago        SMALLINT
   END RECORD

   DEFINE v_ruta_envio LIKE seg_modulo.ruta_envio
   DEFINE v_archivo  STRING
   DEFINE v_contador DECIMAL(9,0)
   DEFINE v_cuantos  DECIMAL(9,0)
   DEFINE v_cuantos2 DECIMAL(9,0)
   DEFINE v_id_datos SMALLINT
   DEFINE v_comando  STRING
   DEFINE v_origen   SMALLINT
   DEFINE s_upd      STRING    

   SELECT ruta_envio
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = "pag"

   LET s_upd = "UPDATE STATISTICS FOR TABLE pag_extractor_preca "
   PREPARE exe_upd FROM s_upd
   EXECUTE exe_upd

   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS,
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
   --         status = 2 significa que esta liquidado
   LET v_sql = "\n SELECT glo.folio folio, ",
               "\n        glo.proceso_cod proceso_cod,  ",
               "\n        DATE(bat.fecha_fin) fecha_fin ",
               "\n FROM   glo_folio glo, bat_ctr_operacion bat ",
               "\n WHERE  glo.folio    = bat.folio ",
               "\n AND    DATE(bat.fecha_fin) = TODAY - 1 ",
--g-               "\n AND    DATE(bat.fecha_fin) = '08/21/2014' ",
               "\n AND    glo.status = 2 ", 
               "\n AND    glo.proceso_cod IN (101,102,103,107,1401,1403) ", 
               "\n AND    glo.proceso_cod = bat.proceso_cod ",
               "\n AND    bat.opera_cod   = 4 ",
               "\n INTO TEMP tmp_folios_preca "

   PREPARE exe_consulta_01 FROM v_sql
   EXECUTE exe_consulta_01             

   LET v_archivo = v_ruta_envio CLIPPED,"/extrac_pagos_",TODAY USING "YYYYMMDD",".preca"

   SELECT count(*)
   INTO   v_cuantos
   FROM   tmp_folios_preca

   IF v_cuantos IS NULL THEN
      LET v_cuantos = 0 
   END IF

   DISPLAY "Registros SAFRE:", v_cuantos

   IF v_cuantos > 0 THEN
      -- SI EXISTEN FOLIOS SACI A EXTRAER
      -- EXTRACCIÓN DE DATOS DE PAGOS DEL HISTORICO

      LET v_id_datos = 1

      LET v_sql = "\n SELECT det.origen_archivo,                             ",
--                  "\n        mov.f_liquida    f_proceso,                     ",
                  "\n        tmp.fecha_fin    f_proceso,                     ",
                  "\n        det.folio        folio,                         ",
                  "\n        afi.nss          nss,                           ",
                  "\n        det.nrp          nrp,                           ",
                  "\n        CASE det.folio_sua                              ",
                  "\n           WHEN 0 THEN det.folio                        ",
                  "\n           ELSE det.folio_sua                           ",
                  "\n        END              folio_sua,                     ",
                  "\n        det.f_pago       f_pago,                        ",
                  "\n        CASE det.origen_archivo                         ",
                  "\n           WHEN 3 THEN det.periodo_pago                 ",
                  "\n           ELSE fn_bimestre_pago(det.periodo_pago)      ",
                  "\n        END              bimestre,                      ",
                  "\n        det.imp_ap_pat   imp_ap_pat,                    ",
                  "\n        today            hoy,                           ",
                  "\n        '2'              cons                           ",
                  "\n FROM   cta_his_pagos det,                              ",
                  "\n        cta_pag_complemento com,                        ",
                  "\n        cta_movimiento mov,                             ",
                  "\n        afi_derechohabiente afi,                        ",
                  "\n        tmp_folios_preca tmp                            ",
                  "\n WHERE  det.folio = tmp.folio                           ",
                  "\n AND    com.folio = det.folio                           ",
                  "\n AND    com.id_referencia = det.id_referencia           ",
                  "\n AND    det.folio = mov.folio_liquida                   ",
                  "\n AND    det.id_referencia = mov.id_referencia           ",
                  "\n AND    afi.id_derechohabiente = com.id_derhab_nuevo    ",   -- se busca con nss nuevo de acuerdo a acl ccnss
                  "\n AND    mov.subcuenta in (4,44)                         ",                 -- sub 4 viv97, 44 Viv SoloINF
                  "\n AND    mov.movimiento in (1,61,221,41,51,81,451,681)   ",   -- mov 1=lq, 61 CcNss, 221 CcNom, 41 Acl, 51 SinCnss, 81 SoloINF, 451 ACL Ade, 681 RISS 
                  "\n AND    tmp.proceso_cod = 103                           ",
                  "\n AND    det.imp_ap_pat > 0                              ",
                  "\n UNION   ALL                                            ",
                  "\n SELECT det.origen_archivo,                             ",
--                  "\n        mov.f_liquida    f_proceso,                     ",                  
                  "\n        tmp.fecha_fin    f_proceso,                     ",                  
                  "\n        det.folio        folio,                         ",
                  "\n        afi.nss          nss,                           ",
                  "\n        det.nrp          nrp,                           ",
                  "\n        CASE det.folio_sua                              ",
                  "\n           WHEN 0 THEN det.folio                        ",
                  "\n           ELSE det.folio_sua                           ",
                  "\n        END              folio_sua,                     ",
                  "\n        det.f_pago       f_pago,                        ",
                  "\n        CASE det.origen_archivo                         ",
                  "\n           WHEN 3 THEN det.periodo_pago                 ",
                  "\n           ELSE fn_bimestre_pago(det.periodo_pago)      ",
                  "\n        END              bimestre,                      ",
                  "\n        det.imp_ap_pat   imp_ap_pat,                    ",
                  "\n        today            hoy,                           ",
                  "\n        '2'              cons                           ",
                  "\n FROM   cta_his_pagos det,                              ",
                  "\n        cta_movimiento mov,                             ",
                  "\n        afi_derechohabiente afi,                        ",
                  "\n        tmp_folios_preca tmp                            ",
                  "\n WHERE  det.folio = tmp.folio                           ",
                  "\n AND    tmp.proceso_cod <> 103                          ",
                  "\n AND    det.folio = mov.folio_liquida                   ",
                  "\n AND    det.id_referencia = mov.id_referencia           ",
                  "\n AND    afi.id_derechohabiente = det.id_derechohabiente ",
                  "\n AND    mov.subcuenta in (4,44)                         ",                 -- sub 4 viv97, 44 Viv SoloINF
                  "\n AND    mov.movimiento in (1,61,221,41,51,81,451,681)   ",   -- mov 1=lq, 61 CcNss, 221 CcNom, 41 Acl, 51 SinCnss, 81 SoloINF, 451 ACL Ade, 681 RISS 
                  "\n AND    det.imp_ap_pat > 0                              ",
                  "\n UNION   ALL                                            ",
                  "\n SELECT 0,                                              ",
                  "\n        pag.f_consulta   f_proceso,                     ",
                  "\n        pag.folio        folio,                         ",
                  "\n        pag.nss          nss,                           ",
                  "\n        pag.nrp          nrp,                           ",
                  "\n        CASE pag.folio_sua                              ",
                  "\n           WHEN 0 THEN pag.folio                        ",
                  "\n           ELSE pag.folio_sua                           ",
                  "\n        END              folio_sua,                     ",
                  "\n        pag.f_pago       f_pago,                        ",
                  "\n        pag.periodo_pago,                               ",
                  "\n        pag.imp_pago     imp_ap_pat,                    ",
                  "\n        today        hoy,                               ",
                  "\n        '2'          cons                               ",
                  "\n FROM   pag_extractor_preca  pag                        ",
                  "\n WHERE  pag.f_consulta = TODAY -1                       ",
--g-                  "\n WHERE  pag.f_consulta = '08/21/2014'                     ",
                  "\n AND    pag.cod_estado = 2                              ",
                  "\n AND    id_codigo  = '00'                               "

      PREPARE exe_consulta_extractor FROM v_sql
      PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
      EXECUTE eje_prio
      DECLARE cur_extractor CURSOR FOR exe_consulta_extractor

      LET v_contador = 0

      START REPORT rpt_extractor TO v_archivo
      FOREACH cur_extractor INTO reg_extractor.*
         OUTPUT TO REPORT rpt_extractor(reg_extractor.*,v_id_datos)
          LET v_contador = v_contador + 1
      END FOREACH
      FINISH REPORT rpt_extractor

      LET v_origen = 1    -- origen extracción de pagos de SACI
      CALL fn_control_folio(v_origen)

      -- ACTUALIZA cod_estado=2 para que no se genere nuevamente
      UPDATE pag_extractor_preca
      SET    cod_estado = 2
      WHERE  f_consulta = TODAY - 1
      AND    cod_estado = 1
      AND    id_codigo  = "00"

   ELSE
      LET v_id_datos = 1

      SELECT COUNT(*)
      INTO   v_cuantos2
      FROM   pag_extractor_preca
      WHERE  f_consulta = TODAY - 1
      AND    cod_estado = 2
      AND    id_codigo  = "00"

      IF v_cuantos2 IS NULL THEN
         LET v_cuantos = 0 
      END IF

      IF v_cuantos2 > 0 THEN
         LET v_sql = "\n SELECT 0,                           ",
                     "\n        pag.f_consulta   f_proceso,                     ",
                     "\n        pag.folio        folio,                         ",
                     "\n        pag.nss          nss,                           ",
                     "\n        pag.nrp          nrp,                           ",
                     "\n        CASE pag.folio_sua                              ",
                     "\n           WHEN 0 THEN pag.folio                        ",
                     "\n           ELSE pag.folio_sua                           ",
                     "\n        END              folio_sua,                     ",
                     "\n        pag.f_pago       f_pago,                        ",
                     "\n        pag.periodo_pago,                               ",
                     "\n        pag.imp_pago     imp_ap_pat,                    ",
                     "\n        today        hoy,                               ",
                     "\n        '2'          cons                               ",
                     "\n FROM   pag_extractor_preca  pag                        ",
                     "\n WHERE  pag.f_consulta = TODAY -1                       ",
--g-                     "\n WHERE  pag.f_consulta = '08/21/2014'                       ",
                     "\n AND    pag.cod_estado = 2                              ",
                     "\n AND    id_codigo  = '00'                               "
                  
         PREPARE exe_consulta_extractor2 FROM v_sql
         DECLARE cur_extractor2 CURSOR FOR exe_consulta_extractor2

         LET v_contador = 1

         START REPORT rpt_extractor TO v_archivo
         FOREACH cur_extractor2 INTO reg_extractor.*
            OUTPUT TO REPORT rpt_extractor(reg_extractor.*,v_id_datos)
            LET v_contador = v_contador + 1
         END FOREACH
         FINISH REPORT rpt_extractor

        LET v_origen = 2        -- origen pantalla de captura de pagos

        CALL fn_control_folio(v_origen)

         -- ACTUALIZA cod_estado=2 para que no se genere nuevamente
         UPDATE pag_extractor_preca
         SET    cod_estado = 2
         WHERE  f_consulta = TODAY - 1
         AND    cod_estado = 1
         AND    id_codigo  = "00"  

      ELSE
      -- SIN EXTRACCIÓN DE DATOS, POR LO TANTO SE GENERA SOLO ENCABEZADO
         LET v_id_datos = 2
         START REPORT rpt_extractor2 TO v_archivo
            OUTPUT TO REPORT rpt_extractor2(v_id_datos)
         FINISH REPORT rpt_extractor2
      END IF
   END IF

   LET v_comando = "cp ",v_ruta_envio CLIPPED,"/","extrac_pagos_",TODAY USING "YYYYMMDD",".preca"," ",
                         v_ruta_envio CLIPPED,"/","extrac_pagos.preca"

   RUN v_comando
END FUNCTION

REPORT rpt_extractor(reg_extractor,v_id_datos)

   DEFINE reg_extractor RECORD
      origen_archivo LIKE cta_his_pagos.origen_archivo,
      f_proceso      LIKE cta_his_pagos.f_proceso,
      folio          DECIMAL(12,0),    --LIKE cta_his_pagos.folio,
      nss            LIKE afi_derechohabiente.nss,
      nrp            LIKE cta_his_pagos.nrp,
      folio_sua      LIKE cta_his_pagos.folio_sua,
      f_pago         LIKE cta_his_pagos.f_pago,
      bim_pago       LIKE cta_his_pagos.periodo_pago,
      imp_ap_pat     LIKE cta_his_pagos.imp_ap_pat,
      f_ejecucion    LIKE cta_his_pagos.f_proceso,
      id_pago        SMALLINT
   END RECORD

   DEFINE v_imp_ap_pat LIKE cta_his_pagos.imp_ap_pat
   DEFINE v_id_datos   SMALLINT
   DEFINE v_folio_sua  LIKE cta_his_pagos.folio_sua
   DEFINE v_nrp        LIKE cta_his_pagos.nrp
   DEFINE v_f_proceso  DATE
   DEFINE v_sql        STRING
   DEFINE v_riss       SMALLINT
   
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT

   FIRST PAGE HEADER

      LET v_sql = " SELECT FIRST 1 ris.nrp ",
                  " FROM   afi_riss ris, afi_derechohabiente afi ",
                  " WHERE  afi.nss = ? ",
                  " AND    afi.id_derechohabiente = ris.id_derechohabiente ",
                  " AND    ris.id_riss = ? ",     --1=Obligatorio  4=Voluntario
                  " AND    f_proceso = ? "
      PREPARE cla_ris FROM v_sql
   
      PRINT COLUMN 1,"1",reg_extractor.f_ejecucion USING "YYYYMMDD",v_id_datos USING "&", 75 SPACE

   ON EVERY ROW

      LET v_imp_ap_pat = reg_extractor.imp_ap_pat*100

      IF reg_extractor.origen_archivo = 3 THEN
         LET v_folio_sua = reg_extractor.folio
      ELSE
         LET v_folio_sua = reg_extractor.folio_sua
      END IF

      LET v_nrp  = NULL
      LET v_riss = 0

      IF reg_extractor.nrp = "B0799991101" THEN --RISS SHCP OBLIGATORIO
         LET v_riss = 1
      END IF

      IF reg_extractor.nrp = "B0799994105" THEN --RISS SHCP VOLUNTARIO 
         LET v_riss = 4
      END IF
      
      IF reg_extractor.nrp = "B0799991101" OR reg_extractor.nrp = "B0799994105" THEN

         SELECT MAX(ris.f_proceso)
         INTO   v_f_proceso
         FROM   afi_riss ris,
                afi_derechohabiente afi
         WHERE  afi.nss                = reg_extractor.nss
         AND    afi.id_derechohabiente = ris.id_derechohabiente
         AND    ris.id_riss            = v_riss

         EXECUTE cla_ris USING reg_extractor.nss,v_riss,v_f_proceso INTO v_nrp

         IF v_nrp IS NULL OR v_nrp ='' THEN
            LET v_nrp = reg_extractor.nrp
         END IF

      ELSE
         LET v_nrp = reg_extractor.nrp
      END IF

      PRINT COLUMN 1, "2",
             reg_extractor.f_proceso      USING "YYYYMMDD",
             reg_extractor.folio          USING "&&&&&&&&&&&&",
             reg_extractor.nss,
             v_nrp,
--             reg_extractor.nrp,
             v_folio_sua                  USING "&&&&&&",
             reg_extractor.f_pago         USING "YYYYMMDD",
             reg_extractor.bim_pago,
             v_imp_ap_pat                 USING "&&&&&&&&&&&&&",
             reg_extractor.f_ejecucion    USING "YYYYMMDD",
             reg_extractor.id_pago        USING "&"

END REPORT

REPORT rpt_extractor2(v_id_datos)
   
   DEFINE v_id_datos SMALLINT

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT

      FIRST PAGE HEADER

      PRINT COLUMN 1,"1",TODAY USING "YYYYMMDD",v_id_datos USING "&", 75 SPACE

END REPORT

FUNCTION fn_control_folio(v_origen)

   DEFINE v_origen SMALLINT
   DEFINE v_folio  DECIMAL(9,0)
   DEFINE v_sql    STRING
   DEFINE v_estado SMALLINT

   LET v_estado = 1  ---- estado procesado (extraido) 

   IF v_origen = 1 THEN  -- origen extracción cta_his_pagos

      LET v_sql = "\n SELECT folio             ",
                  "\n FROM   tmp_folios_preca  "

      PREPARE cla_folio FROM v_sql
      DECLARE cur_folio CURSOR FOR cla_folio

      FOREACH cur_folio INTO v_folio
         INSERT INTO pag_folio_preca VALUES (v_folio,today,v_origen,v_estado)
      END FOREACH

   ELSE
      LET v_sql = "\n SELECT folio                  ",
                  "\n FROM   pag_extractor_preca    ",
                  "\n WHERE  f_consulta = TODAY - 1 ",
                  "\n AND    origen_cod = 1         ",      -- origen pago consulta extractor saci
                  "\n GROUP  BY folio               "
       
      PREPARE cla_folio2 FROM v_sql
      DECLARE cur_folio2 CURSOR FOR cla_folio2

      FOREACH cur_folio2 INTO v_folio
         INSERT INTO pag_folio_preca VALUES(v_folio,today,v_origen,v_estado)
      END FOREACH

   END IF

END FUNCTION
