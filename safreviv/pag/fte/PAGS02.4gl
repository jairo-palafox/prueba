-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGS02                                                                 
-- Objetivo      => Reporte cifras globales de archivo LQINFO en automático al Cargar archivo
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 12 de Abril de 2018
-- Requerimiento => saci2018-13
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

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
      total_registros decimal(12,0),
      imp_ap_pat decimal(16,2),
      imp_am_cre decimal(16,2),
      aiv_ap_pat decimal(18,6),
      imp_total  decimal(16,2)
   END RECORD

   DEFINE g_usuario_cod LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          v_sql         STRING                        -- cadena con instruccion SQL

END GLOBALS

MAIN

   DEFINE r_resultado_opera SMALLINT
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PAGS02.log")
  
   DISPLAY "Iniciando generación de Reporte de cifras globales LQINFO"

   SELECT FIRST 1 nombre_archivo  --saci2018-13
   INTO   g_nom_archivo           --saci2018-13
   FROM   glo_ctr_archivo         --saci2018-13
   WHERE  proceso_cod = 1401      --saci2018-13
   AND    estado = 1              --saci2018-13

   SELECT MAX(pid)                --saci2018-13            
   INTO   g_pid                   --saci2018-13
   FROM   glo_pid                 --saci2018-13
   WHERE  proceso_cod = 1401      --saci2018-13
   AND    f_asigna    = TODAY     --saci2018-13

   LET g_proceso_cod = 1401       --saci2018-13 
   LET g_opera_cod   = 1          --saci2018-13

   DISPLAY "p_usuario_cod ",p_usuario_cod
   DISPLAY "g_pid         ",g_pid
   DISPLAY "g_proceso_cod ",g_proceso_cod
   DISPLAY "g_opera_cod   ",g_opera_cod
   DISPLAY "p_folio       ",p_folio
   DISPLAY "g_nom_archivo ",g_nom_archivo

   LET v_fecha_fin = CURRENT
   
   LET r_resultado_opera = 0 
        
   IF r_resultado_opera <> 0 THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   END IF
   
   SELECT COUNT(*)
   INTO   v_registros_02
   FROM   tmp_det_trabajador

   IF v_registros_02 < 1 THEN
      DISPLAY "No existe registros. No se genera reporte cifras globales lqinfo"
      EXIT PROGRAM

   ELSE 

      CALL fn_display_proceso(0,"INICIO REPORTE CIFRAS GLOBALES LQ")  
     
      CALL fn_crea_reporte()
     
      CALL fn_display_proceso(1,"FIN REPORTE")

   END IF
   
END MAIN

FUNCTION fn_crea_reporte()
  
   DEFINE v_manejador_rpt OM.SaxDocumentHandler
          
   DEFINE v_contador      INTEGER
   DEFINE v_contador_causales INTEGER
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE v_v_nom_reporte VARCHAR(80)
   DEFINE v_existe        SMALLINT
   
   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS, 
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
--===============================================================================

    LET v_v_nom_reporte = p_usuario_cod CLIPPED,"-GLOE02-",
                          g_pid         USING "&&&&&", "-",
                          g_proceso_cod USING "&&&&&","-",
                          1             USING "&&&&&"

    CALL fn_rutas("pag") RETURNING r_ruta_bin, r_ruta_listados

    CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/PAGS02.4rp") RETURNING v_existe

    -- se indica la salida del reporte
    CALL fgl_report_selectDevice("PDF")

    CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)

    -- 0 = No sale reporte en PDF (sale en monitor). 1 = sale reporte en PDF
    CALL fgl_report_selectPreview(0)

    -- se asigna la configuración en el menejo del reporte
    LET v_manejador_rpt = fgl_report_commitCurrentSettings()

   #query que  obtiene las causales de aclaracion agrupadas
   LET v_sql="SELECT COUNT(*), ",
                   " NVL(sum(cta.imp_ap_pat)/100,0),",
                   " NVL(sum(cta.imp_am_cre)/100,0),",
                   " NVL(sum(cta.aiv_ap_pat)/1000000,0),",
                   " NVL(sum(cta.imp_ap_pat + cta.imp_am_cre)/100,0)", 
             " FROM tmp_det_trabajador cta "

   --DISPLAY v_sql

   PREPARE prp_stm_obtiene_causales FROM v_sql
   DECLARE cur_obtiene_causales CURSOR FOR prp_stm_obtiene_causales

   LET v_contador_causales=1

   DISPLAY "ANTES DE ENTRAR folio:",p_folio

   FOREACH cur_obtiene_causales INTO g_arr_causales_rechazo[v_contador_causales].*
      LET v_contador_causales=v_contador_causales+1
   END FOREACH
 
   #se genera el reporte
   START REPORT rpt_consulta_lqinfo TO XML HANDLER v_manejador_rpt
   FOR v_contador = 1 TO g_arr_causales_rechazo.getLength()-1
      OUTPUT TO REPORT rpt_consulta_lqinfo(g_arr_causales_rechazo[v_contador].*)
   END FOR
   FINISH REPORT rpt_consulta_lqinfo
   
END FUNCTION

REPORT rpt_consulta_lqinfo(v_arr_causales_rechazo) --xvi-141-01  --xvi-141-02

   DEFINE v_arr_causales_rechazo RECORD
      total_registros decimal(12,0),
      imp_ap_pat      decimal(16,2),
      imp_am_cre      decimal(16,2),
      aiv_ap_pat      decimal(18,6),
      imp_total       decimal(16,2)      
   END RECORD

   DEFINE v_fecha_consulta  DATE
   DEFINE v_fecha           STRING
   DEFINE g_usuario_cod     LIKE seg_usuario.usuario_cod 
   DEFINE v_nombre_usuario  VARCHAR(100)
   DEFINE v_fecha_ejecucion STRING
   DEFINE v_fecha_eje       DATE

   FORMAT

      FIRST PAGE HEADER
         
         -- se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         LET v_fecha_consulta = v_fecha
         
         -- se obtiene el nombre del usuario
--         SELECT USER
--         INTO   g_usuario_cod
--         FROM   seg_modulo
--         WHERE  modulo_cod = "acl"

         LET g_usuario_cod = p_usuario_cod 
        
         SELECT usuario_desc
         INTO   v_nombre_usuario
         FROM   seg_usuario
         WHERE  usuario_cod = g_usuario_cod

         SELECT f_actualiza
         INTO   v_fecha_eje
         FROM   glo_ctr_archivo
         WHERE  proceso_cod = 1401
         AND    nombre_archivo = g_nom_archivo
         AND    estado = 1

         LET v_fecha_ejecucion = v_fecha_eje USING "dd-mm-yyyy"
         
         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         PRINTX g_usuario_cod, v_nombre_usuario, v_fecha_ejecucion, g_nom_archivo, v_fecha  --xvi-141-02
         
      ON EVERY ROW
         PRINTX v_arr_causales_rechazo.*
           
END REPORT   
             
             