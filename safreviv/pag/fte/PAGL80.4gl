--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/02/2013
--===============================================================
----- ESTE PROGRAMA NO SE UTILIZA
-----  23-JUN-14
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL80                                                                 #
#Objetivo     => Programa lanzador de extracción de pagos para la preca                 #
#Fecha inicio => 05 Febrero de 2014 
#########################################################################################
DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid,       --  ID del proceso
       g_opera_cod_integracion     LIKE cat_operacion.opera_cod,   -- codigo de operacion
       g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod,   -- codigo de operacion
       g_num_folio                 DECIMAL(9)
END GLOBALS

MAIN

DEFINE p_usuario       LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion INTEGER,
       p_cad_ventana   STRING
   
   {
    Se recuperan los parametros recibidos
   Clave de usuario
   Tipo de ejecucion (en línea o batch)
   Cadena que identifica al programa (lo que aparecería como título de la ventana)
   }
   
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_genera_extractor(p_usuario, p_tpo_ejecucion, p_cad_ventana)
   
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
GERARDO VEGA    23-jun-14        Se agrego movimiento 681 para aceptar pagos RISS

============================================================================}
FUNCTION fn_genera_extractor(p_usuario, p_tpo_ejecucion, p_cad_ventana)

DEFINE p_usuario         LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tpo_ejecucion   INTEGER,   --Tipo de ejecucion
                               -- 1 En linea
                               -- 2 Batch
       p_cad_ventana     STRING,    --Cadena de la ventana
       lsi_tpoProceso    SMALLINT,
       lsi_tpoOperacion  SMALLINT,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_nom_archivo     STRING,
       v_cadena_pid      VARCHAR(5),
       v_cadena_proc     VARCHAR(5),
       --v_cadena_opera    VARCHAR(5)
       v_estatus         SMALLINT,
       v_ruta_vacia      LIKE seg_modulo.ruta_listados
       
   DEFINE r_resultado_opera SMALLINT

   DEFINE g_proceso  LIKE cat_proceso.proceso_cod,
          g_opera    LIKE cat_operacion.opera_cod,
          g_programa LIKE cat_operacion.programa_cod


   DEFINE v_bnd_ejecuta BOOLEAN
   DEFINE v_mensaje STRING
   DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc
          
   --Inicializacion de variables
   INITIALIZE lsi_tpoProceso, lsi_tpoOperacion TO NULL

   -- se dan las variables de proceso y operacion
   LET g_proceso = 1409      -- extractor de pagos para preca
   LET g_opera   = 1         -- extractor de pagos para preca
   LET g_programa = "PAGL80"
   LET v_nom_archivo = "NA"  

   SELECT proceso_desc
   INTO   v_proceso_desc
   FROM   cat_proceso
   WHERE  proceso_cod = g_proceso

   --Se asigna el tçitulo de la ventana
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

   --Se genera el pid
   CALL fn_genera_pid(g_proceso,
                      g_opera,
                      p_usuario)
                      RETURNING g_pid
   
   -- se verifica si ya se termino el proceso anterior, si no,
   -- no se permite capturar nuevos saldos
   LET v_estatus = fn_valida_operacion(g_pid,g_proceso,g_opera)
   
   IF ( v_estatus = 0 ) THEN

      CALL fn_inicializa_proceso(g_pid,g_proceso,g_opera,0,
                                 g_programa,v_nom_archivo,p_usuario)
                        RETURNING r_resultado_opera
      
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         LET v_bnd_ejecuta = FALSE
         EXIT PROGRAM
      END IF  
      
      # Inicia operación
      CALL fn_actualiza_opera_ini(g_pid,g_proceso,g_opera,0,"PAGL80",
           v_nom_archivo,p_usuario) RETURNING r_resultado_opera
      
      # En el caso de que exista una inconsistencia al iniciar el proceso, se
      # Muestra un mensaje con la descripcion
      IF(r_resultado_opera)THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         #No se mostraran las cifras control
         LET v_bnd_ejecuta = FALSE
         EXIT PROGRAM
      END IF

      --sE OBTIENEN las rutas de los ejecutables
      CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_vacia
      CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_listados

      LET g_num_folio = 0
      
      -- se crean las cadenas para el nombre del archivo log
      LET v_cadena_pid   = g_pid    USING "&&&&&"
      LET v_cadena_proc  = g_proceso USING "&&&&&"
      
      CALL fn_display_proceso(0,"INICIO EXTRACCIÓN")
      
      CALL fn_genera_extraccion()

      CALL fn_display_proceso(1,"FIN EXTRACCIÓN")
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso,g_opera) RETURNING r_resultado_opera
      LET v_mensaje = "El proceso de carga ha finalizado correctamente"

--      CALL fn_correo_proceso(g_pid,
--                             g_proceso,
--                             g_opera,
--                             '', # Archivo adjunto
--                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - EXTRACTOR PRECA',
--                              v_mensaje
--                             )      
       
   ELSE
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF

END FUNCTION

FUNCTION fn_genera_extraccion()
   
   DEFINE v_sql STRING

   DEFINE reg_extractor RECORD
      f_proceso   LIKE cta_his_pagos.f_proceso,
      folio       LIKE cta_his_pagos.folio,
      nss         LIKE afi_derechohabiente.nss,
      nrp         LIKE cta_his_pagos.nrp,
      folio_sua   LIKE cta_his_pagos.folio_sua,
      f_pago      LIKE cta_his_pagos.f_pago,
      bim_pago    LIKE cta_his_pagos.periodo_pago,
      imp_ap_pat  LIKE cta_his_pagos.imp_ap_pat,
      f_ejecucion LIKE cta_his_pagos.f_proceso,
      id_pago     SMALLINT
   END RECORD
   
   DEFINE v_ruta_envio LIKE seg_modulo.ruta_envio 
   DEFINE v_archivo  STRING
   DEFINE v_contador DECIMAL(9,0)
   DEFINE v_cuantos  DECIMAL(9,0)
   DEFINE v_cuantos2 DECIMAL(9,0) 
   DEFINE v_id_datos SMALLINT
   DEFINE v_comando  STRING

   SELECT ruta_envio
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = "pag"

   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS, 
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
   --         status = 2 significa que esta liquidado
   LET v_sql = "\n SELECT glo_folio.folio folio ",
               "\n FROM   glo_folio ",
               "\n WHERE  glo_folio.proceso_cod IN (101,102,103,107,1401,1403) ", 
               "\n AND    glo_folio.status = 2 ", 
--               "\n AND    glo_folio.f_actualiza = TODAY - 1 ",
               "\n AND    glo_folio.f_actualiza = '03/08/2013' ",
               "\n INTO TEMP tmp_folios_preca "
 
     PREPARE exe_consulta_01 FROM v_sql
     EXECUTE exe_consulta_01             
   
   LET v_archivo = v_ruta_envio CLIPPED,"/extrac_pagos_",TODAY USING "YYYYMMDD",".preca"

   SELECT count(*)
   INTO   v_cuantos
   FROM   tmp_folios_preca

   IF v_cuantos > 0 THEN
      -- EXTRACCIÓN DE DATOS DE PAGOS DEL HISTORICO
      LET v_id_datos = 1
      LET v_sql = "\n SELECT det.f_proceso  f_proceso,   ",
                  "\n        det.folio      folio,       ",
                  "\n        afi.nss        nss,         ",
                  "\n        det.nrp        nrp,         ",
                  "\n        det.folio_sua  folio_sua,   ",
                  "\n        det.f_pago     f_pago,      ",
                  "\n        fn_bimestre_pago(det.periodo_pago) bim_pago, ",
                  "\n        det.imp_ap_pat imp_ap_pat,  ",
                  "\n        today          hoy,         ",
                  "\n        '1'            cons         ",
                  "\n FROM   cta_his_pagos det, afi_derechohabiente afi ",
                  "\n WHERE  afi.id_derechohabiente = det.id_derechohabiente ",
                  "\n AND    det.folio IN (SELECT folio FROM tmp_folios_preca ) ",
                  "\n UNION                   ",
                  "\n SELECT f_proceso   f_prceso,   ",
                  "\n        0           folio,      ",
                  "\n        nss         nss,        ",
                  "\n        nrp         nrp,        ",
                  "\n        folio_sua   folio_sua,  ",
                  "\n        f_pago      f_pago,     ",
                  "\n        fn_bimestre_pago(periodo_pago) bim_pago,  ",
                  "\n        imp_pago    imp_ap_pat, ",
                  "\n        today       hoy,        ",
                  "\n        '1'         cons        ",
                  "\n FROM   pag_extractor_preca     ",
                  "\n WHERE   f_proceso = TODAY -1    "

      PREPARE exe_consulta_extractor FROM v_sql
      DECLARE cur_extractor CURSOR FOR exe_consulta_extractor
      
      LET v_contador = 1
      
      START REPORT rpt_extractor TO v_archivo
      FOREACH cur_extractor INTO reg_extractor.*
         OUTPUT TO REPORT rpt_extractor(reg_extractor.*,v_id_datos)
         LET v_contador = v_contador + 1
      END FOREACH
      FINISH REPORT rpt_extractor
   
   ELSE             

      SELECT count(*) 
      INTO   v_cuantos2
      FROM   pag_extractor_preca
      WHERE  f_proceso = TODAY -1
      --   WHERE f_proceso = "01/17/2014"

      IF v_cuantos2 > 0 THEN
         LET v_sql = "\n SELECT f_proceso   f_prceso,   ",
                     "\n        0           folio,      ",
                     "\n        nss         nss,        ",
                     "\n        nrp         nrp,        ",
                     "\n        folio_sua   folio_sua,  ",
                     "\n        f_pago      f_pago,     ",
                     "\n        fn_bimestre_pago(periodo_pago) bim_pago,  ",
                     "\n        imp_pago    imp_ap_pat, ",
                     "\n        today       hoy,        ",
                     "\n        '1'         cons        ",
                     "\n FROM   pag_extractor_preca     ",
                     "\n WHERE   f_proceso = TODAY -1    "

         PREPARE exe_consulta_extractor2 FROM v_sql
         DECLARE cur_extractor2 CURSOR FOR exe_consulta_extractor2
      
         LET v_contador = 1
      
         START REPORT rpt_extractor TO v_archivo
         FOREACH cur_extractor2 INTO reg_extractor.*
            OUTPUT TO REPORT rpt_extractor(reg_extractor.*,v_id_datos)
            LET v_contador = v_contador + 1
         END FOREACH
         FINISH REPORT rpt_extractor

                     
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
      f_proceso   LIKE cta_his_pagos.f_proceso,
      folio       LIKE cta_his_pagos.folio,
      nss         LIKE afi_derechohabiente.nss,
      nrp         LIKE cta_his_pagos.nrp,
      folio_sua   LIKE cta_his_pagos.folio_sua,
      f_pago      LIKE cta_his_pagos.f_pago,
      bim_pago    LIKE cta_his_pagos.periodo_pago,
      imp_ap_pat  LIKE cta_his_pagos.imp_ap_pat,
      f_ejecucion LIKE cta_his_pagos.f_proceso,
      id_pago     SMALLINT
   END RECORD

   DEFINE v_imp_ap_pat LIKE cta_his_pagos.imp_ap_pat
   DEFINE v_id_datos SMALLINT

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1
   
   FORMAT 
   
      FIRST PAGE HEADER
   
      PRINT COLUMN 1,"1",reg_extractor.f_ejecucion USING "YYYYMMDD",v_id_datos USING "&", 75 SPACE
      
   ON EVERY ROW

      LET v_imp_ap_pat = reg_extractor.imp_ap_pat*100
   
      PRINT COLUMN 1, "2",
             reg_extractor.f_proceso      USING "YYYYMMDD",
             reg_extractor.folio          USING "&&&&&&&&&&&&",
             reg_extractor.nss,
             reg_extractor.nrp,
             reg_extractor.folio_sua      USING "&&&&&&",
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

