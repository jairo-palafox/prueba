--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION: 
--===============================================================

--------------------------------------------------------------------------------
-- Modulo       => ACL
-- Programa     => ACLC31
-- Objetivo     => Consulta causales rechazadas, Input (rango nrp, fecha pago,
--              => Causal de rechazo, folio liquidacion) 
-- Fecha inicio => 29 de JUNIO de 2015
-- Autor        => GERARDO ALFONSO VEGA PAREDES
-------------------------------------------------------------------------------- 

DATABASE safre_viv

GLOBALS 
   DEFINE p_registros RECORD
      nss            LIKE afi_derechohabiente.nss,
      paterno        LIKE afi_derechohabiente.ap_paterno_af,
      materno        LIKE afi_derechohabiente.ap_materno_af,
      nombres        LIKE afi_derechohabiente.nombre_af,
      nrp            LIKE cta_his_pagos.nrp,
      f_pago         LIKE cta_his_pagos.f_pago,
      periodo_pago   LIKE cta_his_pagos.periodo_pago,
      folio_sua      LIKE cta_his_pagos.folio_sua,
      imp_ap_pat     LIKE cta_his_pagos.imp_ap_pat,
      imp_am_cre     LIKE cta_his_pagos.imp_am_cre,
      tpo_aclaracion LIKE cta_his_pagos.tpo_aclaracion,
      edo_pago       CHAR(70)
--      rechazo        CHAR(36)      
   END RECORD
   
   DEFINE g_ruta_envio     LIKE seg_modulo.ruta_envio,
          g_nom_archivo    VARCHAR(50),
          g_origen_archivo SMALLINT

  DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
          p_s_titulo       STRING  

    DEFINE ch_reporte    base.Channel
    DEFINE v_ruta_envio     STRING

          
END GLOBALS

MAIN
--   DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
--          p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
--          p_s_titulo       STRING                        -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_origen_archivo = 5
   
   -- consulta de informacion Reporte de Inconsistencias 
   CALL fn_ini_rep_texto()
   CALL fh_reporte_rechazo(p_usuario_cod, p_s_titulo)
   
   
END MAIN

FUNCTION fh_reporte_rechazo(p_usuario_cod, p_s_titulo)

   DEFINE v_construct STRING,
          v_continua SMALLINT,
          manejador_rpt     om.SaxDocumentHandler
          
   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod, -- clave del usuario
          p_s_titulo                STRING,                       -- titulo de la ventana
          v_query                   STRING,
          v_registros               INTEGER


   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN 
   --se abre la ventana de  forma de captura de parametros de busqueda
   
   OPEN WINDOW ventana_rep WITH FORM "ACLC311"

   CONSTRUCT v_construct ON a.nrp, a.f_pago, a.tpo_aclaracion, a.folio
      FROM nrp, f_pago, tpo_aclaracion, folio
   
      ON ACTION CANCEL
         LET INT_FLAG = TRUE
         LET v_continua = 0
         EXIT CONSTRUCT
   
      ON ACTION ACCEPT
         LET v_continua = 1
         ACCEPT CONSTRUCT
   
   END CONSTRUCT

   IF NOT INT_FLAG THEN           
   	
      LET v_query = "\n SELECT COUNT (*)",
                    "\n FROM   cta_his_pagos a, afi_derechohabiente c ",
                    "\n WHERE ",v_construct, 
                    "\n AND    a.id_derechohabiente = c.id_derechohabiente ",
--                    "\n AND a.tpo_aclaracion <> ''                         ",
                    "\n AND    a.ind_liquidacion = 1 "

      PREPARE prp_conteo FROM v_query
      EXECUTE prp_conteo INTO v_registros
      
      --valida que se econtrarón registros
      IF v_registros > 0 THEN

         IF fgl_report_loadCurrentSettings("ACLC31.4rp") THEN
            CALL fgl_report_selectDevice ("PDF")
            LET manejador_rpt = fgl_report_commitCurrentSettings()
         
            LET v_query = "\n SELECT c.nss,             ",
                          "\n        c.ap_paterno_af,   ",
                          "\n        c.ap_materno_af,   ",
                          "\n        c.nombre_af,       ",
                          "\n        a.nrp,             ",
                          "\n        a.f_pago,          ",                         
                          "\n        a.periodo_pago,    ",                         
                          "\n        a.folio_sua,       ",
                          "\n        a.imp_ap_pat,      ",
                          "\n        a.imp_am_cre,      ",
                          "\n        a.tpo_aclaracion,  ",
                          "\n        f.aclaracion_descripcion ",
--                          "\n        CASE a.result_operacion ",
--                          "\n           WHEN 2 THEN 'RECHAZO POR NO ENCONTRAR HISTORICO', ",
--                          "\n           WHEN 3 THEN 'RECHAZO POR NO TRAER NSS EN disccnss' ",  
--                          "\n        END CASE ",
                          "\n FROM cta_his_pagos a, afi_derechohabiente c, ",
                          "\n      outer pag_tpo_aclaracion f ",
                          "\n WHERE ",v_construct,
                          "\n   AND a.id_derechohabiente = c.id_derechohabiente",
                          "\n   AND a.tpo_aclaracion     = f.aclaracion_cod    ",
--                          "\n   AND a.tpo_aclaracion <> ''                     ",
                          "\n   AND a.result_operacion = 2                      "

            PREPARE prp_cur_folio FROM v_query
            DECLARE cur_folio CURSOR FOR prp_cur_folio
         
            START REPORT rpt_aclsc_inconsistenca TO XML HANDLER manejador_rpt    
            FOREACH cur_folio INTO p_registros.*
               OUTPUT TO REPORT rpt_aclsc_inconsistenca(p_registros.*)
               CALL fn_reporte_texto()--CABC Se llama función que escribe en archivo txt
            END FOREACH
            CALL ch_reporte.close()--CABC Se cierra channel al término del foreach
            FINISH REPORT rpt_aclsc_inconsistenca
         ELSE
         	CALL fn_mensaje("Atención","No se puede abrir la plantilla ACLC31.4rp, No esposible generar el reporte","stop")
         END IF
      ELSE
         CALL fn_mensaje("Consulta","No existen registros con los criterios dados.","about")   
      END IF --IF v_registros > 0 THEN

   END IF  --IF NOT INT_FLAG THEN

   MENU 
      ON ACTION CANCEL --EXIT --CLOSE
         EXIT menu
   END MENU
   
   CLOSE WINDOW ventana_rep

END FUNCTION
##############AGREGADO POR CABC PARA GENERAR REPORTE TXT#############################
FUNCTION fn_ini_rep_texto()
    DEFINE v_fecha STRING
    DEFINE v_ruta_envio_char LIKE seg_modulo.ruta_envio;

    SELECT ruta_envio
    INTO v_ruta_envio_char
    FROM SEG_MODULO
    WHERE MODULO_COD='acl'

    
    LET v_fecha=TODAY USING "yyyymmdd"
    LET v_ruta_envio=v_ruta_envio_char CLIPPED||"/"||v_fecha||"_causales_rech_acl.txt"
    DISPLAY v_ruta_envio
    LET ch_reporte=base.Channel.create()
    CALL ch_reporte.openFile(v_ruta_envio,"w")
    CALL ch_reporte.setDelimiter("")

END FUNCTION

FUNCTION fn_reporte_texto()
    
    DEFINE v_cadena STRING

    LET v_cadena=p_registros.nss||"|",
                               p_registros.nombres CLIPPED||" "||p_registros.paterno CLIPPED||" "||p_registros.materno CLIPPED||"|",
                               p_registros.periodo_pago||"|",
                               p_registros.f_pago||"|",
                               p_registros.imp_ap_pat||"|",
                               p_registros.imp_am_cre||"|",
                               p_registros.nrp||"|",
                               p_registros.folio_sua||"|",
                               p_registros.tpo_aclaracion||"|",
                               p_registros.edo_pago||"|"
--                               p_registros.rechazo||"|"

    CALL ch_reporte.write(v_cadena)

END FUNCTION
#####################################################################################

REPORT rpt_aclsc_inconsistenca(p_registros)
   DEFINE p_registros RECORD
      nss            LIKE afi_derechohabiente.nss,
      paterno        LIKE afi_derechohabiente.ap_paterno_af,
      materno        LIKE afi_derechohabiente.ap_materno_af,
      nombres        LIKE afi_derechohabiente.nombre_af,
      nrp            LIKE cta_his_pagos.nrp,
      f_pago         LIKE cta_his_pagos.f_pago,
      periodo_pago   LIKE cta_his_pagos.periodo_pago,
      folio_sua      LIKE cta_his_pagos.folio_sua,
      imp_ap_pat     LIKE cta_his_pagos.imp_ap_pat,
      imp_am_cre     LIKE cta_his_pagos.imp_am_cre,
      tpo_aclaracion LIKE cta_his_pagos.tpo_aclaracion,
      edo_pago       CHAR(70)
--      rechazo        CHAR(36)
   END RECORD

   DEFINE v_fecha DATE
   DEFINE v_nombre_usuario VARCHAR(100)
                                                                                                                                                                                          
   FORMAT                                                                                        
      FIRST PAGE HEADER
         LET v_fecha = TODAY 

         SELECT usuario_desc
         INTO v_nombre_usuario
         FROM seg_usuario
         WHERE usuario_cod = p_usuario_cod

         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         PRINTX v_fecha USING 'dd-mm-yyyy', v_nombre_usuario, p_usuario_cod

      ON EVERY ROW
         PRINTX p_registros.*
                                                                                           
END REPORT          
