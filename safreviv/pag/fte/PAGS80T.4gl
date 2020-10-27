--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/02/2013
--===============================================================

--========================================================================================
-- Modulo        => PAG                                                                    
-- Programa      => PAGS80T                                                                
-- Objetivo      => Genera archivo plano de extracción de pagos para la preca para un 
--               => día en especifíco
-- Fecha inicio  => 14 Febrero de 2019
-- Clave cambio  => sisxix-2
--========================================================================================
-- Fecha cambio  => 27 Marzo de 2019
-- Descripción   => Generar fecha 15-ENE-2019
-- Calve cambio  => sisxix-2-2

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
   LET p_usuario_cod = "safreviv"    --sisxix-2
   LET g_pid         = 99397         --sisxix-2-2
   LET g_proceso_cod = 1409          --sisxix-2
   LET g_opera_cod   = 1             --sisxix-2
   LET p_folio       = 0             --sisxix-2
   LET g_nom_archivo = " "           --sisxix-2
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PAGS80T.log")   --sisxix-2

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
GERARDO VEGA    07-oct-2015       Se parametrizó subcuenta y movimientos con las tablas
                                  cat_sub_extractor y cat_mov_extractor

============================================================================}
FUNCTION fn_genera_extractor()

   DEFINE r_resultado_opera SMALLINT
   DEFINE v_mensaje STRING
   DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc
   DEFINE v_fecha_inicio  DATE
   
   SELECT proceso_desc
   INTO   v_proceso_desc
   FROM   cat_proceso
   WHERE  proceso_cod = g_proceso_cod   

      CALL fn_display_proceso(0,"INICIO EXTRACCIÓN")

--g-
      --Lee fecha de inicio de proceso 
      --12 diciembre 2014
--sisxix-2      INITIALIZE v_fecha_inicio TO NULL 
--sisxix-2      CALL fn_obtener_fec_ini_extraccion()
--sisxix-2      RETURNING v_fecha_inicio

--      LET v_fecha_inicio = "11/14/2018"   --sisxix-2
      LET v_fecha_inicio = "01/14/2019"   --sisxix-2-2

      DISPLAY "fn_genera_extraccion: ", v_fecha_inicio
      CALL fn_genera_extraccion(v_fecha_inicio)

--sisxix-2      CALL fn_display_proceso(1,"FIN EXTRACCIÓN")

      --DISPLAY "fn_actualiza_opera_fin"
--sisxix-2      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
--sisxix-2      LET v_mensaje = "El proceso de extracción ha finalizado correctamente"

--sisxix-2      --DISPLAY "fn_correo_proceso"
--sisxix-2      CALL fn_correo_proceso(g_pid,
--sisxix-2                             g_proceso_cod,
--sisxix-2                             g_opera_cod,
--sisxix-2                             '', # Archivo adjunto
--sisxix-2                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - EXTRACTOR PRECA',
--sisxix-2                              v_mensaje
--sisxix-2                             )

      --DISPLAY "termina fn_genera_extractor"
END FUNCTION

FUNCTION fn_genera_extraccion(p_fecha_inicio)

   DEFINE v_sql STRING
   DEFINE p_fecha_inicio DATE

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
   DEFINE v_fecha_aux DATE

   
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
               "\n FROM   glo_folio glo, bat_his_ctr_operacion bat ",   --sisxix-2
--               "\n FROM   glo_folio glo, bat_ctr_operacion bat ",   --sisxix-2-2
               "\n WHERE  glo.folio    = bat.folio ",
--sisxix-2                   "\n AND    DATE(bat.fecha_fin) BETWEEN '",p_fecha_inicio,"' AND TODAY - 1 ",
               "\n AND    DATE(bat.fecha_fin) = ","'",p_fecha_inicio,"'",
               "\n AND    glo.status = 2 ", 
               "\n AND    glo.proceso_cod IN (101,102,103,107,1401,1403) ", 
               "\n AND    glo.proceso_cod = bat.proceso_cod ",
               "\n AND    bat.opera_cod   = 4 ",
               "\n INTO TEMP tmp_folios_preca "

   DISPLAY v_sql

   PREPARE exe_consulta_01 FROM v_sql
   EXECUTE exe_consulta_01             

--   LET v_fecha_aux = "11/15/2018"    --sisxix-2
   LET v_fecha_aux = "01/15/2019"    --sisxix-2-2
   LET v_archivo = v_ruta_envio CLIPPED,"/extrac_pagos_",v_fecha_aux USING "YYYYMMDD",".preca"

   SELECT count(*)
   INTO   v_cuantos
   FROM   tmp_folios_preca

   IF v_cuantos IS NULL THEN
      LET v_cuantos = 0 
   END IF

   DISPLAY "Registros SAFRE:", v_cuantos

   IF v_cuantos > 0 THEN

      LET v_id_datos = 1

      LET v_sql = "\n SELECT det.origen_archivo,                             ",
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
                  "\n AND    det.ind_liquidacion <> -1                       ",
                  "\n AND    afi.id_derechohabiente = com.id_derhab_nuevo    ",   -- se busca con nss nuevo de acuerdo a acl ccnss
                  "\n AND    mov.subcuenta in  (SELECT subcuenta  FROM cat_sub_extractor where vigencia_cod =1) ", 
                  "\n AND    mov.movimiento in (SELECT movimiento FROM cat_mov_extractor where vigencia_cod =1) ",
                  "\n AND    tmp.proceso_cod = 103                           ",
                  "\n AND    det.imp_ap_pat > 0                              ",
                  "\n UNION   ALL                                            ",
                  "\n SELECT det.origen_archivo,                             ",
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
                  "\n AND    det.ind_liquidacion <> -1                       ",                  
                  "\n AND    afi.id_derechohabiente = det.id_derechohabiente ",
                  "\n AND    mov.subcuenta  in (SELECT subcuenta  FROM cat_sub_extractor where vigencia_cod =1) ", 
                  "\n AND    mov.movimiento in (SELECT movimiento FROM cat_mov_extractor where vigencia_cod =1) ",
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
                  "\n  WHERE pag.f_consulta = ","'",p_fecha_inicio,"'",            --sisxix-2
                  "\n AND    pag.cod_estado = 2                              ",
                  "\n AND    id_codigo  = '00'                               "

      --DISPLAY v_sql
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
--      CALL fn_control_folio(v_origen)

   END IF

   LET v_comando = "cp ",v_ruta_envio CLIPPED,"/","extrac_pagos_",v_fecha_aux USING "YYYYMMDD",".preca"," ",
                         v_ruta_envio CLIPPED,"/","extrac_pagos.preca"
                         
--sisxix-2   RUN v_comando  --sisxix-2 preguntar a Alex para que ellos copien el archivo generado a "extrac_pagos.preca"

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
   DEFINE v_registros  DECIMAL(9,0)
   
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

      LET v_registros = 0

   ON EVERY ROW

      LET v_imp_ap_pat = reg_extractor.imp_ap_pat*100

      LET v_registros = v_registros + 1

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
             v_folio_sua                  USING "&&&&&&",
             reg_extractor.f_pago         USING "YYYYMMDD",
             reg_extractor.bim_pago,
             v_imp_ap_pat                 USING "&&&&&&&&&&&&&",
             reg_extractor.f_ejecucion    USING "YYYYMMDD",
             reg_extractor.id_pago        USING "&"

   ON LAST ROW

      PRINT COLUMN 1,"9", v_registros USING "&&&&&&&&&&"   

END REPORT

FUNCTION fn_control_folio(v_origen)

   DEFINE v_origen SMALLINT
   DEFINE v_folio  DECIMAL(9,0)
   DEFINE v_sql    STRING
   DEFINE v_estado SMALLINT
   DEFINE v_fecha_control DATE --sisxix-2

   LET v_estado = 1  ---- estado procesado (extraido) 

   IF v_origen = 1 THEN  -- origen extracción cta_his_pagos

      LET v_sql = "\n SELECT folio             ",
                  "\n FROM   tmp_folios_preca  "

--      LET v_fecha_control = "11/15/2018"  --sisxix-2
      LET v_fecha_control = "01/15/2019"  --sisxix-2-2
      
      PREPARE cla_folio FROM v_sql
      DECLARE cur_folio CURSOR FOR cla_folio

      FOREACH cur_folio INTO v_folio
         INSERT INTO pag_folio_preca VALUES (v_folio,v_fecha_control,v_origen,v_estado)
      END FOREACH

   END IF

END FUNCTION
