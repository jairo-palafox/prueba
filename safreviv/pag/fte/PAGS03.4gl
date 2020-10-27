-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGS03                                                                 
-- Objetivo      => Reporte registros duplicados en pag_cza_pag_patronal
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 26 de Junio de 2018
-- Requerimiento => saci2018-62
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

   DEFINE g_arr_dups DYNAMIC ARRAY OF RECORD
      folio         DECIMAL(9,0),
      periodo_pago  CHAR(06),
      nrp           CHAR(11),
      folio_sua     CHAR(06),
      cve_ent_recep CHAR(03),
      f_pago        DATE,
      cuantos       INTEGER
   END RECORD

   DEFINE g_usuario_cod LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE v_sql         STRING                        -- cadena con instruccion SQL

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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PAGS03.log")
  
   DISPLAY "Iniciando generación de reporte de pagos patronales duplicados"

--   SELECT FIRST 1 nombre_archivo
--   INTO   g_nom_archivo
--   FROM   glo_ctr_archivo
--   WHERE  proceso_cod = 1401
--   AND    estado = 2         -- Integrado

--   SELECT MAX(pid)
--   INTO   g_pid
--   FROM   glo_pid
--   WHERE  proceso_cod = 1401
--   AND    f_asigna    = TODAY

--   LET g_proceso_cod = 1401

     LET g_opera_cod   = 2

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
      DISPLAY "No existe registros. No se genera reporte de pagos patronales duplicados"
      EXIT PROGRAM

   ELSE 

      CALL fn_display_proceso(0,"INICIO REPORTE DE PAGOS PATRONALES DUPLICADOS ")  
     
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

    LET v_v_nom_reporte = p_usuario_cod CLIPPED,"-PAGP10-",
                          g_pid         USING "&&&&&", "-",
                          g_proceso_cod USING "&&&&&","-",
                          2             USING "&&&&&"

    CALL fn_rutas("pag") RETURNING r_ruta_bin, r_ruta_listados

    CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/PAGS03.4rp") RETURNING v_existe

    -- se indica la salida del reporte
    CALL fgl_report_selectDevice("PDF")

    CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)

    -- 0 = No sale reporte en PDF (sale en monitor). 1 = sale reporte en PDF
    CALL fgl_report_selectPreview(0)

    -- se asigna la configuración en el menejo del reporte
    LET v_manejador_rpt = fgl_report_commitCurrentSettings()

   #query principal
   LET v_sql = " SELECT folio, ",
                      " periodo_pago,",
                      " nrp,",
                      " folio_sua,",
                      " cve_ent_recep,",
                      " f_pago,",
                      " count(*) ",
               " FROM   pag_cza_pag_patronal ",
               " WHERE  folio = ",p_folio,
               " GROUP  BY 1,2,3,4,5,6 ",
               " HAVING COUNT(*) > 1 ",
               " ORDER  BY 2,3,4,5,6 "

   PREPARE prp_stm_obtiene_causales FROM v_sql
   DECLARE cur_obtiene_causales CURSOR FOR prp_stm_obtiene_causales

   LET v_contador_causales = 1

   DISPLAY "ANTES DE ENTRAR folio:",p_folio

   FOREACH cur_obtiene_causales INTO g_arr_dups[v_contador_causales].*
      LET v_contador_causales=v_contador_causales+1
   END FOREACH
 
   #se genera el reporte
   START REPORT rpt_consulta_lqinfo TO XML HANDLER v_manejador_rpt
   FOR v_contador = 1 TO g_arr_dups.getLength()-1
      OUTPUT TO REPORT rpt_consulta_lqinfo(g_arr_dups[v_contador].*)
   END FOR
   FINISH REPORT rpt_consulta_lqinfo
   
END FUNCTION

REPORT rpt_consulta_lqinfo(v_arr_dups) --xvi-141-01  --xvi-141-02

   DEFINE v_arr_dups RECORD
      folio         DECIMAL(9,0),
      periodo_pago  CHAR(06),
      nrp           CHAR(11),
      folio_sua     CHAR(06),
      cve_ent_recep CHAR(03),
      f_pago        DATE,
      cuantos       INTEGER
   END RECORD

   DEFINE v_fecha_consulta  DATE
   DEFINE v_fecha           STRING
   DEFINE g_usuario_cod     LIKE seg_usuario.usuario_cod 
   DEFINE v_nombre_usuario  VARCHAR(100)
   DEFINE v_fecha_ejecucion STRING
   DEFINE v_fecha_eje       DATE
   DEFINE v_fecha_pago      STRING

   FORMAT

      FIRST PAGE HEADER
         
         -- se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         LET v_fecha_consulta = v_fecha
         
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
         AND    estado = 2       -- Integrado

         LET v_fecha_ejecucion = v_fecha_eje USING "dd-mm-yyyy"
         
         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         PRINTX g_usuario_cod, v_nombre_usuario, v_fecha_ejecucion, g_nom_archivo, v_fecha
         
      ON EVERY ROW
         LET v_fecha_pago = v_arr_dups.f_pago USING "dd-mm-yyyy"

         PRINTX v_arr_dups.folio,
                v_arr_dups.periodo_pago,
                v_arr_dups.nrp,
                v_arr_dups.folio_sua,
                v_arr_dups.cve_ent_recep,
                v_fecha_pago,
                v_arr_dups.cuantos

END REPORT   
