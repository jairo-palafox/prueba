--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC81                                                                 #
#Objetivo     => Consulta de Extractor de precalificacion para monitor de procesos      #
#Autor        => GERARDO ALFONSO VEGA PAREDES                                           #
#Fecha inicio => 24 de Febrero de 2014                                                  #
#########################################################################################

DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS

   DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod,
          g_pid          LIKE bat_ctr_proceso.pid,
          g_proceso_cod  LIKE cat_proceso.proceso_cod,
          g_opera_cod    LIKE cat_operacion.opera_cod,
          p_folio        LIKE glo_folio.folio, 
          g_nom_archivo  STRING

   DEFINE seg_modulo_bat RECORD
      ruta_listados CHAR(40)
   END RECORD
   
   DEFINE g_usuario_cod LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          v_sql         STRING                        -- cadena con instruccion SQL
          
END GLOBALS

MAIN

   DEFINE v_fecha_consulta DATE
   DEFINE v_folio_temp     DECIMAL(9,0)
   DEFINE v_dato           CHAR(01)

   DEFINE r_resultado_opera SMALLINT
   DEFINE v_mensaje STRING
   DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc

   -- se reciben los parametros del programa
   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET g_nom_archivo = ARG_VAL(6)



   
   LET v_fecha_consulta = TODAY - 1

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PAGC81.log")

   DISPLAY "Iniciando generación de Reporte de extracción de pagos para preca"

   LET v_sql = "\n SELECT glo.folio         ",
               "\n FROM   glo_folio glo, bat_ctr_operacion bat ",
               "\n WHERE  date(bat.fecha_fin) = ","'",v_fecha_consulta,"'",
               "\n AND    glo.folio  = bat.folio ",
               "\n AND    glo.status = 2 ",
               "\n AND    glo.proceso_cod IN (101,102,103,107,1401,1403) ", 
               "\n AND    glo.proceso_cod = bat.proceso_cod ",
               "\n AND    bat.opera_cod   = 4 "
               
   PREPARE consulta_existe_fecha FROM v_sql
   DECLARE cur_consulta_fecha CURSOR FOR consulta_existe_fecha
   
   FOREACH cur_consulta_fecha INTO v_folio_temp
      EXIT FOREACH
   END FOREACH 

   LET v_dato = NULL
   SELECT "X" dato
   INTO   v_dato
   FROM   pag_extractor_preca
   WHERE  f_consulta = v_fecha_consulta
   GROUP BY dato

   IF (v_folio_temp IS NULL OR 0) AND v_dato IS NULL THEN
      DISPLAY "No existen datos con la fecha de hoy"
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera

   ELSE 

      SELECT proceso_desc
      INTO   v_proceso_desc
      FROM   cat_proceso
      WHERE  proceso_cod = g_proceso_cod  
   
   
      CALL fn_display_proceso(0,"INICIO REPORTE EXTRACCIÓN")  
     
      CALL f_consulta_fecha(v_fecha_consulta)
     
      CALL fn_display_proceso(1,"FIN EXTRACCIÓN")

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
      LET v_mensaje = "El reporte de extracción ha finalizado correctamente"

      CALL fn_correo_proceso(g_pid,
                             g_proceso_cod,
                             g_opera_cod,
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - REPORTE EXTRACTOR PRECA',
                              v_mensaje
                             )
     
   END IF
   
END MAIN

{ ======================================================================
Clave: PAGC81
Nombre: f_consulta_fecha
Fecha creacion: 24 Febrero 2014
Autor: Gerardo Vega
Narrativa del proceso que realiza:
Realiza una consulta sobre los datos de extracción para precalificación y 
emitir el resultado de la misma en un reporte

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
GERARDO VEGA    23-jun-14        Se agrego movimiento 681 para aceptar pagos RISS
GERARDO VEGA    07-oct-2015       Se parametrizó subcuenta y movimientos con las tablas
                                  cat_sub_extractor y cat_mov_extractor
======================================================================
}

FUNCTION f_consulta_fecha(v_fecha_consulta)
   
   DEFINE v_fecha_consulta DATE
   
   DEFINE v_cifras RECORD
      origen    CHAR(08),
      registros DECIMAL(9,0),
      importe   DECIMAL(16,2)
   END RECORD

   DEFINE v_handler       om.SaxDocumentHandler -- handler para el reporte
   DEFINE v_manejador_rpt OM.SaxDocumentHandler
          
   DEFINE v_contador DECIMAL(9,0)
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE v_v_nom_reporte VARCHAR(80)
   DEFINE v_existe SMALLINT
   
   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS, 
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
   --         status = 2 significa que esta liquidado

   
--===============================================================================

    LET v_v_nom_reporte = p_usuario_cod CLIPPED,"-PAGC81-",g_pid USING "&&&&&", "-",g_proceso_cod USING "&&&&&",
                          "-",g_opera_cod USING "&&&&&"

    CALL fn_rutas("pag") RETURNING r_ruta_bin, r_ruta_listados

    CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/PAGI81.4rp") RETURNING v_existe

    -- se indica la salida del reporte
    CALL fgl_report_selectDevice("PDF")

    CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)

    -- sin indica que no es necesario el preview
    CALL fgl_report_selectPreview(0)

    -- se asigna la configuración en el menejo del reporte
    LET v_manejador_rpt = fgl_report_commitCurrentSettings()

--===============================================================================
   
   WHENEVER ERROR CONTINUE
   DROP TABLE tmp_folios_preca
   WHENEVER ERROR STOP

   LET v_sql = "CREATE TEMP TABLE tmp_folios_preca (folio DECIMAL(9,0))"
   PREPARE exe_crea_folio FROM v_sql
   EXECUTE exe_crea_folio
   
   LET v_sql = "\n INSERT INTO tmp_folios_preca ",
               "\n SELECT glo.folio folio ",
               "\n FROM   glo_folio glo, bat_ctr_operacion bat ",
               "\n WHERE  date(bat.fecha_fin) = ","'",v_fecha_consulta,"'",
               "\n AND    glo.folio  = bat.folio ",
               "\n AND    glo.status = 2 ", 
               "\n AND    glo.proceso_cod IN (101,102,103,107,1401,1403) ",
               "\n AND    glo.proceso_cod = bat.proceso_cod ",
               "\n AND    bat.opera_cod   = 4 " 

   PREPARE exe_consulta_01 FROM v_sql
   EXECUTE exe_consulta_01             
   
   -- EXTRACCIÓN DE DATOS DE PAGOS DEL HISTORICO

   LET v_sql = "\n SELECT 'SACI'              origen,                     ",
               "\n        count(*)            registros,                  ",
               "\n        sum(det.imp_ap_pat) imp_ap_pat                  ",
               "\n FROM   cta_his_pagos det,                              ",
               "\n        cta_movimiento mov,                             ", 
               "\n        afi_derechohabiente afi,                        ",
               "\n        tmp_folios_preca glo                            ",
               "\n WHERE  det.folio = glo.folio                           ",
               "\n AND    det.folio = mov.folio_liquida                   ",
               "\n AND    det.id_referencia = mov.id_referencia           ",               
               "\n AND    det.ind_liquidacion <> -1                       ",
               "\n AND    afi.id_derechohabiente = det.id_derechohabiente ",
               "\n AND    mov.subcuenta  in (SELECT subcuenta FROM cat_sub_extractor where vigencia_cod =1) ", 
               "\n AND    mov.movimiento in (SELECT movimiento FROM cat_mov_extractor where vigencia_cod =1) ",
               "\n AND    det.imp_ap_pat > 0                              ",
               "\n UNION ALL                                              ",
               "\n SELECT 'CONSULTA'    origen,                           ",
               "\n        count(*)      registros,                        ",
               "\n        sum(imp_pago) imp_ap_pat                        ",
               "\n FROM   pag_extractor_preca                             ",
               "\n WHERE  f_consulta = ","'",v_fecha_consulta,"'",
               "\n AND    cod_estado = 2                                  ",
               "\n AND    id_codigo = '00'                                "
              
   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_fecha2 FROM v_sql
   DECLARE cur_consulta_fecha2 CURSOR FOR sid_consulta_fecha2
   
   -- se inicia el contador
   LET v_contador = 0

   START REPORT rpt_consulta_lqinfo TO XML HANDLER v_manejador_rpt   
   FOREACH cur_consulta_fecha2 INTO v_cifras.*
      OUTPUT TO REPORT rpt_consulta_lqinfo(v_cifras.*,v_fecha_consulta)
      LET v_contador = v_contador + 1
   END FOREACH
   FINISH REPORT rpt_consulta_lqinfo
   
END FUNCTION

REPORT rpt_consulta_lqinfo(v_cifras,v_fecha_consulta)

   DEFINE v_cifras RECORD
      origen    CHAR(08),
      registros DECIMAL(9,0),
      importe   DECIMAL(16,2)
   END RECORD
   
   DEFINE v_fecha_consulta DATE
   DEFINE v_fecha STRING
   DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod 
   DEFINE v_nombre_usuario VARCHAR(100)
         
   FORMAT

      FIRST PAGE HEADER
         
         -- se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         
         -- se obtiene el nombre del usuario
         SELECT USER
         INTO   g_usuario_cod
         FROM   seg_modulo
         WHERE  modulo_cod = "pag"
        
         SELECT usuario_desc
         INTO   v_nombre_usuario
         FROM   seg_usuario
         WHERE  usuario_cod = g_usuario_cod
         
         LET v_nombre_usuario = v_nombre_usuario CLIPPED
         
         PRINTX v_fecha_consulta, g_usuario_cod, v_nombre_usuario, v_fecha
         
    ON EVERY ROW

      IF v_cifras.importe IS NULL THEN
         LET v_cifras.importe = 0
      END IF

      IF v_cifras.registros IS NULL THEN
         LET v_cifras.registros = 0
      END IF
      
      PRINTX v_cifras.*
             
END REPORT   
             
             