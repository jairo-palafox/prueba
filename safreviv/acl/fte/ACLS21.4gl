--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:  
--=============================================================================
-----------------------------------------------------------------------------------------
-- Modulo       => ACL                                                                    
-- Programa     => ACLS07                                                                 
-- Objetivo     => Reporte cifras de rechazo de ACL con cambio de nss en automatico al liquidar
-- Autor        => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio => 14 de Diciembre de 2015                                                  
-----------------------------------------------------------------------------------------
-- Modificación => Quitar causal de"SIN HISTORICO", "CONFIRMADOS" y "SIN NSS".
-- Fehca        => 29 de Noviembre de 2016.
-- Autor        => GERARDO ALFONSO VEGA PAREDES.
-- Clave cambio => xvi-141-01
-----------------------------------------------------------------------------------------
-- Modificación => Cambiar de lugar amort por AIVS y agregar folio
-- Fehca        => 5 de Diciembre de 2016.
-- Autor        => GERARDO ALFONSO VEGA PAREDES.
-- Clave cambio => xvi-141-02
---------------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                                          
-- Fec Mod.     => 17 de septiembre de 2018.                                              
-- Modificación => Adecuar funcionalidad para que lea nueva tabla de rechazos cta_rechazo_acl
-- Clave cambio => saci2018-67                                                            
---------------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod,
          g_pid          LIKE bat_ctr_proceso.pid,
          g_proceso_cod  LIKE cat_proceso.proceso_cod,
          g_opera_cod    LIKE cat_operacion.opera_cod,
          p_folio        LIKE glo_folio.folio, 
          g_nom_archivo  LIKE bat_ctr_operacion.nom_archivo --STRING

   DEFINE seg_modulo_bat RECORD
      ruta_listados CHAR(40)
   END RECORD

   DEFINE g_arr_causales_rechazo DYNAMIC ARRAY OF RECORD
      causal_rechazo VARCHAR(60),
      total_registros INTEGER,
      imp_ap_pat decimal(12,2),
      imp_am_cre decimal(12,2),
      aiv_ap_pat decimal(18,6)
   END RECORD

   DEFINE g_total_causales RECORD
      total_registros INTEGER,
      imp_ap_pat DECIMAL(12,2),
      imp_am_cre DECIMAL(12,2),
      aiv_ap_pat DECIMAL(18,6)
   END RECORD

   DEFINE g_usuario_cod LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          v_sql         STRING                        -- cadena con instruccion SQL
          
END GLOBALS

MAIN

   DEFINE r_resultado_opera SMALLINT
   DEFINE v_mensaje         STRING
   DEFINE v_proceso_desc    LIKE cat_proceso.proceso_desc
   DEFINE v_registros_02    DECIMAL(9,0)
   DEFINE v_fecha_fin       LIKE bat_ctr_operacion.fecha_ini

   -- se reciben los parametros del programa
   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET g_nom_archivo = ARG_VAL(6)
  
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".ACLS07.log")
  
   DISPLAY "Iniciando generación de Reporte de cifras de rechazos disscnss"

   SELECT FIRST 1 nombre_archivo
   INTO   g_nom_archivo
   FROM   glo_ctr_archivo
   WHERE  folio = p_folio
   
   LET v_fecha_fin = CURRENT
   
   UPDATE bat_ctr_proceso
   SET    estado_cod  = 2
   WHERE  pid         = g_pid
   AND    proceso_cod = g_proceso_cod;   

   UPDATE bat_ctr_operacion
   SET    folio       = p_folio  ,
          fecha_ini   = v_fecha_fin,   --CURRENT YEAR TO SECOND ,
          nom_archivo = g_nom_archivo,
          estado_cod  = 2,
          usuario     = p_usuario_cod
   WHERE  pid         = g_pid
   AND    proceso_cod = g_proceso_cod
   AND   opera_cod    = 5;    

   LET r_resultado_opera = 0 
        
   IF r_resultado_opera <> 0 THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   END IF
   
   SELECT COUNT(*)
   INTO   v_registros_02
--   FROM   cta_his_pagos       --saci2018-67
   FROM   cta_rechazos_acl      --saci2018-67
   WHERE  folio = p_folio
   AND    result_operacion = 2

   IF v_registros_02 < 1 THEN
      DISPLAY "No existe registros rechazados. No se genera archivo de rechazos"
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,5) RETURNING r_resultado_opera
      EXIT PROGRAM

   ELSE 

      SELECT proceso_desc
      INTO   v_proceso_desc
      FROM   cat_proceso
      WHERE  proceso_cod = g_proceso_cod  
      
      CALL fn_display_proceso(0,"INICIO REPORTE RECHAZOS DISCCNSS")  
     
      CALL fn_crea_reporte()
     
      CALL fn_display_proceso(1,"FIN REPORTE")

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,5) RETURNING r_resultado_opera
      LET v_mensaje = "El reporte de cifras de rechazo de disccnss ha finalizado correctamente"

      CALL fn_correo_proceso(g_pid,
                             g_proceso_cod,
                             5,
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - REPORTE RECHAZOS DISCCNSS',
                              v_mensaje
                             )
     
   END IF
   
END MAIN

FUNCTION fn_crea_reporte()
  
   DEFINE v_manejador_rpt OM.SaxDocumentHandler
          
   DEFINE v_contador      INTEGER
   DEFINE v_contador_causales_reporte      INTEGER
   DEFINE v_contador_causales INTEGER
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE v_v_nom_reporte VARCHAR(80)
   DEFINE v_existe        SMALLINT
   
   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS, 
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
--===============================================================================

    LET v_v_nom_reporte = p_usuario_cod CLIPPED,"-ACLS07-",
                          g_pid USING "&&&&&", "-",
                          g_proceso_cod USING "&&&&&","-",
                          5 USING "&&&&&"

    CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

    CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/ACLS07.4rp") RETURNING v_existe

    -- se indica la salida del reporte
    CALL fgl_report_selectDevice("PDF")

    CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)

    -- sin indica que no es necesario el preview
    CALL fgl_report_selectPreview(0)

    -- se asigna la configuración en el menejo del reporte
    LET v_manejador_rpt = fgl_report_commitCurrentSettings()

   #query que  obtiene las causales de aclaracion agrupadas
   LET v_sql="SELECT cat.codigo_rechazo||'-'||TRIM(cat.descripcion), ",
                   " COUNT(*), ",
                   " NVL(sum(cta.imp_ap_pat),0),",
                   " NVL(sum(cta.imp_am_cre),0),",
                   " NVL(sum(cta.aiv_ap_pat),0) ",
--             " FROM cta_his_pagos cta ",           --saci2018-67
             " FROM cta_rechazos_acl cta ",          --saci2018-67
                     " LEFT OUTER JOIN acl_pag_rechazo acl ",
                     "    ON (cta.folio = acl.folio AND cta.id_referencia = acl.id_referencia) ",
                     " LEFT OUTER JOIN acl_cat_rechazo cat ",
                     "    ON (acl.codigo_rechazo = cat.codigo_rechazo) ",
             " WHERE cta.folio=",p_folio,
             " AND   cta.result_operacion in (2,3) ",
             " GROUP BY 1 ORDER BY 1 ASC"

   DISPLAY v_sql

   PREPARE prp_stm_obtiene_causales FROM v_sql
   DECLARE cur_obtiene_causales CURSOR FOR prp_stm_obtiene_causales

   LET v_contador_causales=1

   DISPLAY "ANTES DE ENTRAR folio:",p_folio

   FOREACH cur_obtiene_causales INTO g_arr_causales_rechazo[v_contador_causales].*
      LET v_contador_causales=v_contador_causales+1
   END FOREACH
 
   LET g_total_causales.total_registros=0
   LET g_total_causales.imp_ap_pat=0
   LET g_total_causales.imp_am_cre=0
   LET g_total_causales.aiv_ap_pat=0

   FOR v_contador_causales_reporte = 1 TO g_arr_causales_rechazo.getLength()-1
      LET g_total_causales.total_registros=g_total_causales.total_registros + g_arr_causales_rechazo[v_contador_causales_reporte].total_registros
      LET g_total_causales.imp_ap_pat=g_total_causales.imp_ap_pat + g_arr_causales_rechazo[v_contador_causales_reporte].imp_ap_pat
      LET g_total_causales.imp_am_cre=g_total_causales.imp_am_cre + g_arr_causales_rechazo[v_contador_causales_reporte].imp_am_cre
      LET g_total_causales.aiv_ap_pat=g_total_causales.aiv_ap_pat + g_arr_causales_rechazo[v_contador_causales_reporte].aiv_ap_pat
   END FOR 

 
   #se genera el reporte
   START REPORT rpt_consulta_lqinfo TO XML HANDLER v_manejador_rpt
   FOR v_contador = 1 TO g_arr_causales_rechazo.getLength()-1
      OUTPUT TO REPORT rpt_consulta_lqinfo(g_arr_causales_rechazo[v_contador].*,p_folio) --xvi-141-02
   END FOR
   FINISH REPORT rpt_consulta_lqinfo
   
END FUNCTION

REPORT rpt_consulta_lqinfo(v_arr_causales_rechazo,v_folio) --xvi-141-01  --xvi-141-02

   DEFINE v_arr_causales_rechazo RECORD
      causal_rechazo  VARCHAR(60),
      total_registros INTEGER,
      imp_ap_pat      decimal(12,2),
      imp_am_cre      decimal(12,2),
      aiv_ap_pat      decimal(18,6)
   END RECORD

   DEFINE v_folio DECIMAL(9,0)  --xvi-141-02
   
   #para causales
   DEFINE v_total_causales RECORD
      total_registros INTEGER,
      imp_ap_pat      DECIMAL(12,2),
      imp_am_cre      DECIMAL(12,2),
      aiv_ap_pat      DECIMAL(18,6)
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
          
         PRINTX g_usuario_cod, v_nombre_usuario, v_folio, g_nom_archivo, v_fecha  --xvi-141-02
         
      ON EVERY ROW
         IF v_arr_causales_rechazo.causal_rechazo IS NULL THEN          --xvi-141-01
            LET v_arr_causales_rechazo.causal_rechazo = "13-SIN LQINFO Y HAY SALIDA"
         END IF
         PRINTX v_arr_causales_rechazo.*
           
      ON LAST ROW 
         LET v_total_causales.*=g_total_causales.*
         PRINTX v_total_causales.*

END REPORT   
             
             