-----------------------------------------------------------------------------------------
-- Modulo        => PAG
-- Programa      => PAGS04
-- Objetivo      => Reporte de causales sin liquidar incluyendo cauales en blanco
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 01 de Septiembre de 2018.
-- Requerimiento => saci2018-67
-----------------------------------------------------------------------------------------
-- Modificación =>
-- Fehca        =>
-- Autor        =>
-- Clave cambio =>
-- Descripción  =>
-----------------------------------------------------------------------------------------
-- Modificación =>
-- Fehca        =>
-- Autor        =>
-- Clave cambio =>
-- Descripción  =>
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS

   DEFINE
      p_fecha_corte DATE,
      p_usuario     CHAR(20),
      p_pid         SMALLINT,
      p_proceso_cod SMALLINT
      
   DEFINE  
      v_ventana ui.Window,
      v_forma   ui.form,
      p_ventana STRING, 
      g_usuario_cod LIKE seg_usuario.usuario_cod,
      v_i_cont_registros INTEGER
        
   DEFINE v_arr_causales DYNAMIC ARRAY OF RECORD
      v_causal           LIKE cta_his_pagos.tpo_aclaracion,
      v_causal_des       LIKE pag_tpo_aclaracion.aclaracion_descripcion,
      v_tot_causal       INTEGER,
      v_imp_ap           DECIMAL (22,2), 
      v_aiv_ap_pat       DECIMAL (18,6),
      v_amp_am           DECIMAL (22,2),
      v_int_gen_pgo_ext  DECIMAL (22,2),
      v_aiv_gen_pgo_ext  DECIMAL (18,6),
      v_imp_ap_am        DECIMAL (22,2),
      v_porcentaje       DECIMAL (16,4)
   END RECORD
   
   DEFINE v_arr_totales RECORD
      v_tot_causales_gral   DECIMAL(9,0),
      v_tot_imp_ap          DECIMAL(22,2),
      v_tot_aiv_ap_pat      DECIMAL(18,6),
      v_tot_imp_am          DECIMAL(22,2),
      v_tot_int_gen_pgo_ext DECIMAL(22,2),
      v_tot_aiv_gen_pgo_ext DECIMAL(18,6),
      v_tot_imp_apam        DECIMAL(22,2),
      v_tot_porcentajes     DECIMAL(16,4),
      v_aux                 DECIMAL
   END RECORD

END GLOBALS

MAIN

   LET p_fecha_corte = ARG_VAL(1)

   SELECT USER              
   INTO   p_usuario        
   FROM   seg_modulo           
   WHERE  modulo_cod = "pag"

   LET p_pid = 111
   LET p_proceso_cod = 100

   CALL genera_reporte()

END MAIN

FUNCTION genera_reporte()

   DEFINE
      aux  INTEGER, -- Auxiliar para los ciclos 
      mes  CHAR(2), -- Mes de busqueda
      anio CHAR(4)  -- Año de busqueda

   DEFINE 
      v_consulta_folio STRING,   -- Consulta de folios
      v_consulta_causa STRING    -- Consulta de causales
      
   DEFINE v_arr_folios DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para los folios encontrados
      v_folio LIKE glo_folio.folio
   END RECORD
   
--============================      
   LET v_i_cont_registros  = 1  --SE INICIALIZA EL CONMTADOR DE REGISTROS PARA CTA_HIS_PAGOS

   -- Busqueda de existencia de registros con los criterios recibidos
   -- Se utilizan los codigos de proceso definidos en el requerimiento   
   LET v_consulta_folio = "\n SELECT folio ",
                          "\n FROM glo_folio",
                          "\n WHERE f_actualiza < ? " ,
                          "\n AND proceso_cod IN (1401, 101,102,103,105,107,110)"

    PREPARE prp_busca_folios FROM v_consulta_folio
    DECLARE cur_busca_folios CURSOR FOR prp_busca_folios
    DISPLAY v_consulta_folio

    -- Inicializa en 0 el valor de de la bandera   
    LET aux = 1
    FOREACH cur_busca_folios USING p_fecha_corte  INTO v_arr_folios[aux].*
      LET aux = aux + 1
    END FOREACH
    FREE cur_busca_folios
    -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
    CALL v_arr_folios.deleteElement(aux)
    DISPLAY v_arr_folios.getLength()

    -- Se evalua el resultado de la validacion
   IF v_arr_folios.getLength() > 0  THEN
      -- Se realiza la buqueda de cuentas causales con los folios encontrados
      LET v_consulta_causa = " SELECT cta.tpo_aclaracion,tpa.aclaracion_descripcion,count(cta.tpo_aclaracion) ",
                                    " as no ,SUM(cta.imp_ap_pat) as pat,                                             ",
                                    " SUM(cta.aiv_ap_pat) as aiv,                                                    ",
                                    " SUM(cta.imp_am_cre) as cre ,                                                   ",
                                    " SUM(cta.int_gen_pgo_ext) as ext,                                               ",
                                    " SUM(cta.aiv_gen_pgo_ext) as aiv_ext,                                           ",
                                    " SUM(cta.aiv_ap_pat + cta.aiv_gen_pgo_ext ) as suma , 0                         ",
                             " FROM   cta_his_pagos cta, OUTER pag_tpo_aclaracion tpa ",
                             " WHERE  cta.folio IN (SELECT folio FROM glo_folio WHERE f_actualiza < ?  ",
                                                  " AND    proceso_cod IN (1401, 101,102,103,105,107,110)) ",
                             " AND    cta.tpo_aclaracion = tpa.aclaracion_cod ",
                             " AND    cta.ind_liquidacion = 1                                                    ",
                             --" AND (cta.tpo_aclaracion <> '' OR cta.tpo_aclaracion IS NOT NULL)               ",   --saci2018-67
                             " GROUP  by cta.tpo_aclaracion, tpa.aclaracion_descripcion                        ",
                             " HAVING count(*)>0 ",
                             " ORDER  BY 1"
    
      DISPLAY v_consulta_causa   
      PREPARE prp_busca_causales FROM v_consulta_causa
      
      DECLARE cur_busca_causales CURSOR FOR prp_busca_causales

      -- Inicializa en 1 el valor de de la bandera   
      LET v_i_cont_registros = 1
      LET v_arr_totales.v_tot_imp_apam = 0
      LET v_arr_totales.v_tot_causales_gral = 0
      LET v_arr_totales.v_tot_imp_ap = 0
      LET v_arr_totales.v_tot_imp_am = 0
      LET v_arr_totales.v_tot_porcentajes = 45.6245
      LET v_arr_totales.v_tot_aiv_ap_pat = 0
      LET v_arr_totales.v_tot_int_gen_pgo_ext = 0
      LET v_arr_totales.v_tot_aiv_gen_pgo_ext = 0
      
      FOREACH cur_busca_causales USING p_fecha_corte INTO v_arr_causales[v_i_cont_registros].*

         LET v_arr_totales.v_tot_causales_gral   = v_arr_totales.v_tot_causales_gral   + v_arr_causales[v_i_cont_registros].v_tot_causal
         LET v_arr_totales.v_tot_imp_ap          = v_arr_totales.v_tot_imp_ap          + v_arr_causales[v_i_cont_registros].v_imp_ap
         LET v_arr_totales.v_tot_aiv_ap_pat      = v_arr_totales.v_tot_aiv_ap_pat      + v_arr_causales[v_i_cont_registros].v_aiv_ap_pat
         LET v_arr_totales.v_tot_imp_am          = v_arr_totales.v_tot_imp_am          + v_arr_causales[v_i_cont_registros].v_amp_am
         LET v_arr_totales.v_tot_int_gen_pgo_ext = v_arr_totales.v_tot_int_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_int_gen_pgo_ext
         LET v_arr_totales.v_tot_aiv_gen_pgo_ext = v_arr_totales.v_tot_aiv_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_aiv_gen_pgo_ext
         LET v_arr_totales.v_tot_imp_apam        = v_arr_totales.v_tot_imp_apam        + v_arr_causales[v_i_cont_registros].v_imp_ap_am
         
         LET v_i_cont_registros = v_i_cont_registros + 1 

      END FOREACH
      FREE cur_busca_causales
      
      -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
      LET v_i_cont_registros = v_i_cont_registros  - 1
      
      CALL v_arr_causales.deleteElement(v_arr_causales.getLength())
      
      IF v_arr_causales.getLength()>0 THEN  
         -- Calculo de porcentajes
         LET v_arr_totales.v_tot_porcentajes = 0
         
         FOR aux = 1 TO v_arr_causales.getLength() STEP +1
            --DISPLAY v_arr_causales[aux].v_porcentaje," ", v_arr_causales[aux].v_imp_ap_am ," ",  v_arr_totales.v_tot_imp_apam     
            
            LET v_arr_causales[aux].v_porcentaje = (v_arr_causales[aux].v_imp_ap_am / v_arr_totales.v_tot_imp_apam) * 100    
            LET v_arr_totales.v_tot_porcentajes  = v_arr_totales.v_tot_porcentajes + v_arr_causales[aux].v_porcentaje
            
            --DISPLAY v_arr_causales[aux].v_porcentaje ," GRAL ",v_arr_totales.v_tot_porcentajes
         END FOR  
         
         CALL fn_reporte_carga_archivo( TRUE, mes, anio)

      ELSE
          -- Se notifica al usuario que no se encontro informacion con los criterios
          CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
      END IF 
   ELSE
      -- Se notifica al usuario que no se encontro informacion con los criterios
      CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
   END IF
       
END FUNCTION

FUNCTION fn_reporte_carga_archivo(p_b_despliegue_pantalla, v_mes, v_anio)
   
   DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
   DEFINE manejador_rpt om.SaxDocumentHandler,
          v_i_inicio_for INTEGER
   
   DEFINE v_mes      CHAR(2)-- Mes de busqueda
   DEFINE v_anio     CHAR(4)-- Año de busqueda
   DEFINE v_mes_desc CHAR(12)

   --DEFINE manejador_rpt      om.SaxDocumentHandler
   DEFINE v_genero_reporte_pdf BOOLEAN
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte   VARCHAR(80)
   DEFINE v_existe        SMALLINT

   LET v_i_inicio_for = 1

   LET v_nom_reporte = p_usuario     CLIPPED,"-PAGS04-",
                       p_pid         USING "&&&&&", "-",
                       p_proceso_cod USING "&&&&&","-",
                       1             USING "&&&&&"

   CALL fn_rutas("pag") RETURNING r_ruta_bin, r_ruta_listados
   LET r_ruta_listados = "/safreviv/pag/tra/"

   LET v_genero_reporte_pdf= FALSE

   CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/PAGS04.4rp") RETURNING v_existe

   IF v_existe THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)
      CALL fgl_report_selectPreview(0)       -- 1=Genera reporte de inmediato 0=Genera reporte en ruta
      LET manejador_rpt = fgl_report_commitCurrentSettings()
      LET v_genero_reporte_pdf = TRUE
   ELSE
      RETURN v_genero_reporte_pdf
   END IF

   -- se asigna descripción al mes
   CASE v_mes <> 0
       WHEN v_mes = 1
           LET v_mes_desc = 'Enero'      EXIT CASE 
       WHEN v_mes = 2  
           LET v_mes_desc = 'Febrero'    EXIT CASE  
       WHEN v_mes = 3  
           LET v_mes_desc = 'Marzo'      EXIT CASE 
       WHEN v_mes = 4  
           LET v_mes_desc = 'Abril'      EXIT CASE  
       WHEN v_mes = 5  
           LET v_mes_desc = 'Mayo'       EXIT CASE  
       WHEN v_mes = 6  
           LET v_mes_desc = 'Junio'      EXIT CASE 
       WHEN v_mes = 7 
           LET v_mes_desc = 'Julio'      EXIT CASE  
       WHEN v_mes = 8  
           LET v_mes_desc = 'Agosto'     EXIT CASE 
       WHEN v_mes = 9  
           LET v_mes_desc = 'Septiembre' EXIT CASE  
       WHEN v_mes = 10 
           LET v_mes_desc = 'Octubre'    EXIT CASE  
       WHEN v_mes = 11 
           LET v_mes_desc = 'Noviembre'  EXIT CASE  
       WHEN v_mes = 12 
           LET v_mes_desc = 'Diciembre'  EXIT CASE  
   END CASE  
    
   --Inicia el reporte de registros con rechazo
   START REPORT rpt_causales_aclaracion_lqinfo TO XML HANDLER manejador_rpt
   FOR v_i_inicio_for = 1 TO v_i_cont_registros
      OUTPUT TO REPORT rpt_causales_aclaracion_lqinfo(v_arr_causales[v_i_inicio_for].*, v_mes_desc, v_anio)
   END FOR  
   FINISH REPORT rpt_causales_aclaracion_lqinfo

END FUNCTION

REPORT rpt_causales_aclaracion_lqinfo(v_arr_causales, p_mes_desc, p_anio)
   
   DEFINE v_arr_causales    RECORD -- Arreglo dinamico para las cuenstas causales encontradas
      v_causal          LIKE cta_his_pagos.tpo_aclaracion,
      v_causal_des      LIKE pag_tpo_aclaracion.aclaracion_descripcion,
      v_tot_causal      INTEGER,
      v_imp_ap          DECIMAL (22,2),
      v_aiv_ap_pat      DECIMAL (18,6),
      v_amp_am          DECIMAL (22,2),
      v_int_gen_pgo_ext DECIMAL (22,2),
      v_aiv_gen_pgo_ext DECIMAL (18,6),
      v_imp_ap_am       DECIMAL (22,2),
      v_porcentaje      DECIMAL (16,4)
   END RECORD
   
   DEFINE
      v_fecha_reporte    DATE,
      v_nombre_usuario   VARCHAR(100),
      p_mes_desc         CHAR(12),
      p_anio             CHAR(4)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER    
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = g_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_arr_totales.*,
             v_nombre_usuario,
             g_usuario_cod ,
             p_mes_desc ,
             p_anio
                                                                                              
   ON EVERY ROW
      PRINTX v_arr_causales.*
                                                                                           
END REPORT  

                  