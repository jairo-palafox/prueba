--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

-----------------------------------------------------------------------------------------
-- Modulo       => ACL                                                                   
-- Programa     => ACLS11                                                                
-- Objetivo     => Reporte Cifras rechazos aclaratorio DICCNCNO (cambio nombre)
-- Fecha inicio => 16 de diciembre de 2015
-- Autor        => GERARDO ALFONSO VEGA PAREDES
-- Modificacion => 16 de febrero de 2016
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
   DEFINE g_pid         LIKE bat_ctr_proceso.pid,      --  ID del proceso
          g_proceso_cod LIKE cat_proceso.proceso_cod,  -- codigo del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod   -- codigo de operacion

   DEFINE g_reg_modulo  RECORD
          ruta_exp      CHAR(40),
          ruta_rescate  CHAR(40),
          ruta_listados CHAR(40)
   END RECORD
   
   DEFINE seg_modulo_bat RECORD
      ruta_listados CHAR(40)
   END RECORD
   
   DEFINE g_usuario_cod LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE g_nom_archivo LIKE bat_ctr_operacion.nom_archivo
   
END GLOBALS

MAIN

   DEFINE p_tipo_ejecucion SMALLINT,              -- forma como ejecutara el programa
          p_s_titulo       STRING,                -- titulo de la ventana      
          v_folio          LIKE glo_folio.folio,  -- folio de consulta
          v_folio_busqueda LIKE glo_folio.folio   -- folio de busqueda
       
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "ACLS111"
   
   -- se inicia el folio en null
   LET v_folio = NULL
   
   INPUT v_folio WITHOUT DEFAULTS
     FROM folio
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_folio IS NOT NULL ) THEN

             -- se verifica si el folio corresponde a un folio del proceso de aclaraciones con cambio
             SELECT folio
             INTO   v_folio_busqueda
             FROM   glo_folio
             WHERE  proceso_cod = g_proceso_cod_acl_reg_pag_cambio_nombre -- aclaraciones sin cambio de NSS
             AND    folio = v_folio

            -- si no se encontro el folio, entonces no es un folio de ese proceso
            IF v_folio_busqueda IS NULL THEN
               CALL fn_mensaje("Atención","El folio no corresponde con un proceso de Aclaraciones con cambio nombre","stop")
               CONTINUE INPUT
            END IF
         
            CALL Consulta_acl(v_folio)
         ELSE
            CALL fn_mensaje("Atención","Debe capturar un folio","stop")
            CONTINUE INPUT
         END IF
      
      ON ACTION cancel
         EXIT INPUT
   
   END INPUT   
   CLOSE WINDOW w_consulta

END MAIN

FUNCTION Consulta_acl(v_folio)
   
   DEFINE v_folio DECIMAL(9,0)
   
   DEFINE v_cifras RECORD
      folio     DECIMAL(9,0),
      registros DECIMAL(9,0),
      rechazo   SMALLINT,
      concepto  CHAR(45),
      ap_pat    DECIMAL(12,2),
      aiv_pat   DECIMAL(22,6),
      am_cre    DECIMAL(12,2),
      int_extem DECIMAL(12,2),
      aiv_int   DECIMAL(22,6)
   END RECORD

   DEFINE v_arr_cifras DYNAMIC ARRAY OF RECORD
      folio     DECIMAL(9,0),
      registros DECIMAL(9,0),
      rechazo   SMALLINT,
      concepto  CHAR(45),
      ap_pat    DECIMAL(12,2),
      aiv_pat   DECIMAL(22,6),
      am_cre    DECIMAL(12,2),
      int_extem DECIMAL(12,2),
      aiv_int   DECIMAL(22,6)
   END RECORD

   DEFINE v_handler       om.SaxDocumentHandler -- handler para el reporte
   DEFINE v_manejador_rpt OM.SaxDocumentHandler
          
   DEFINE v_contador      INTEGER
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE v_v_nom_reporte VARCHAR(80)
   DEFINE v_existe        SMALLINT
   DEFINE v_sql           STRING
   DEFINE v_s_qry         STRING
   DEFINE p_folio         DECIMAL(9,0)
   DEFINE p_usuario       CHAR(20)
   DEFINE p_pid           DECIMAL(9,0)
   DEFINE p_proceso_cod   SMALLINT
   DEFINE p_opera_cod     SMALLINT
   
   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS, 
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
--===============================================================================

    LET v_v_nom_reporte = g_usuario_cod CLIPPED,"-ACLS11-",
                          TODAY USING 'YYYYMMDD','DISCCNOM'

    CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

    -- Ejecución de archivo de rechazo 
    LET p_pid = 0
    LET p_proceso_cod = 107
    LET g_opera_cod = 0
    LET p_folio = v_folio

    SELECT USER
    INTO   p_usuario 
    FROM   cat_proceso
    WHERE  proceso_cod = p_proceso_cod
    
    LET v_s_qry = " fglrun ",r_ruta_bin CLIPPED,"/ACLS04 ",
                            p_usuario, " ",
                            p_pid, " ",
                            p_proceso_cod, " ",
                            p_opera_cod, " ",
                            p_folio, " ",
                            "NA"
    RUN v_s_qry
    
    CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/ACLS11.4rp") RETURNING v_existe

    -- se indica la salida del reporte
    CALL fgl_report_selectDevice("PDF")

    CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)

    -- sin indica que no es necesario el preview
    CALL fgl_report_selectPreview(1)

    -- se asigna la configuración en el menejo del reporte
    LET v_manejador_rpt = fgl_report_commitCurrentSettings()

--===============================================================================

   SELECT nombre_archivo
   INTO   g_nom_archivo
   FROM   glo_ctr_archivo
   WHERE  folio = v_folio

   LET v_sql = "\n SELECT folio, ",
               "\n        count(*)            registros,            ",
               "\n        result_operacion    tipo_rechazo,         ",
               "\n        CASE result_operacion                     ",
               "\n           WHEN  2 THEN 'SIN HISTORICO'           ",
               "\n           ELSE 'SIN NSS DESTINO'                 ",
               "\n        END Concepto,                             ",
               "\n        sum(det.imp_ap_pat) imp_ap_pat,           ",
               "\n        sum(det.aiv_ap_pat) aiv_ap_pat,           ",
               "\n        sum(det.imp_am_cre) imp_am_cre,           ",
               "\n        sum(det.int_gen_pgo_ext) int_gen_pgo_ext, ",
               "\n        sum(det.aiv_gen_pgo_ext) aiv_gen_pgo_ext  ",
               "\n FROM   cta_his_pagos det                         ",
               "\n WHERE  det.folio = ",v_folio,
               "\n AND    det.result_operacion in (2,3)             ",
               "\n GROUP  BY 1,3 "

   PREPARE cla_reg02 FROM v_sql
   DECLARE cur_reg02 CURSOR FOR cla_reg02
   
   LET v_contador = 1

   FOREACH cur_reg02 INTO v_cifras.*
     LET v_arr_cifras[v_contador].* = v_cifras.*
     LET v_contador = v_contador + 1
   END FOREACH

   LET v_sql = "\n SELECT det.folio, ",
               "\n        count(*)             registros,           ",
               "\n        det.result_operacion tipo_rechazo,        ",
               "\n        'REG.CONFIR. PREV. ADELANTADOS' Concepto,       ",
               "\n        sum(det.imp_ap_pat) imp_ap_pat,           ",
               "\n        sum(det.aiv_ap_pat) aiv_ap_pat,           ",
               "\n        sum(det.imp_am_cre) imp_am_cre,           ",
               "\n        sum(det.int_gen_pgo_ext) int_gen_pgo_ext, ",
               "\n        sum(det.aiv_gen_pgo_ext) aiv_gen_pgo_ext  ",
               "\n FROM   cta_his_pagos det                         ",
               "\n WHERE  det.folio = ",v_folio,
               "\n AND    det.ind_liquidacion = 4                   ",
               "\n AND    det.result_operacion = 1                  ",
               "\n GROUP  BY 1,3 "
                              
   PREPARE cla_reg01 FROM v_sql
   DECLARE cur_reg01 CURSOR FOR cla_reg01
   
   FOREACH cur_reg01 INTO v_cifras.*
     LET v_arr_cifras[v_contador].* = v_cifras.*
     LET v_contador = v_contador + 1
   END FOREACH

   START REPORT rpt_consulta_lqinfo TO XML HANDLER v_manejador_rpt
   
   FOR v_contador = 1 TO v_arr_cifras.getLength()
      OUTPUT TO REPORT rpt_consulta_lqinfo(v_arr_cifras[v_contador].*)
   END FOR

   FINISH REPORT rpt_consulta_lqinfo
   
END FUNCTION

REPORT rpt_consulta_lqinfo(v_cifras)

   DEFINE v_cifras RECORD
      folio     DECIMAL(9,0),
      registros DECIMAL(9,0),
      rechazo   SMALLINT,
      concepto  CHAR(45),
      ap_pat    DECIMAL(12,2),
      aiv_pat   DECIMAL(22,6),
      am_cre    DECIMAL(12,2),
      int_extem DECIMAL(12,2),
      aiv_int   DECIMAL(22,6)
   END RECORD

   DEFINE v_total RECORD
      ap_pat    DECIMAL(12,2),
      aiv_pat   DECIMAL(22,6),
      am_cre    DECIMAL(12,2),
      int_extem DECIMAL(12,2),
      aiv_int   DECIMAL(22,6)   
   END RECORD

   DEFINE v_fecha_consulta DATE
   DEFINE v_fecha STRING
   DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod 
   DEFINE v_nombre_usuario VARCHAR(100)
         
   FORMAT

      FIRST PAGE HEADER
         
         -- se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         LET v_fecha_consulta = v_fecha
         
         -- se obtiene el nombre del usuario
         SELECT USER
         INTO   g_usuario_cod
         FROM   seg_modulo
         WHERE  modulo_cod = "acl"
        
         SELECT usuario_desc
         INTO   v_nombre_usuario
         FROM   seg_usuario
         WHERE  usuario_cod = g_usuario_cod
         
         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         LET v_total.ap_pat    = 0
         LET v_total.aiv_pat   = 0
         LET v_total.am_cre    = 0
         LET v_total.int_extem = 0
         LET v_total.aiv_int   = 0
         
         PRINTX g_usuario_cod, v_nombre_usuario, g_nom_archivo, v_fecha
         
      ON EVERY ROW
         LET v_total.ap_pat    = v_total.ap_pat    + v_cifras.ap_pat
         LET v_total.aiv_pat   = v_total.aiv_pat   + v_cifras.aiv_pat
         LET v_total.am_cre    = v_total.am_cre    + v_cifras.am_cre
         LET v_total.int_extem = v_total.int_extem + v_cifras.int_extem
         LET v_total.aiv_int   = v_total.aiv_int   + v_cifras.aiv_int
      
         PRINTX v_cifras.*

      ON LAST ROW
         PRINTX v_total.*
         
             
END REPORT