-----------------------------------------------------------------------------------------
-- Modulo        => PAG
-- Programa      => PAGC09
-- Objetivo      => Reporte de causales sin liquidar.
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 12 de Junio de 2015.
-----------------------------------------------------------------------------------------
-- Modificación => Eliminar pagos con causal en blanco.
-- Fehca        => 10 de Mayo de 2018.
-- Autor        => GERARDO ALFONSO VEGA PAREDES.
-- Clave cambio => saci2018-51 
-- Descripción  => Se agregó tpo_aclaracion is not null
-----------------------------------------------------------------------------------------
-- Modificación => Quitar dependencia de usuario y pasar a batch
-- Fehca        => 05 de Julio de 2018.
-- Autor        => GERARDO ALFONSO VEGA PAREDES.
-- Clave cambio => saci2018-68  
---------------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS

   DEFINE
      p_usuario     LIKE seg_usuario.usuario_cod,
      p_pid         DECIMAL (9,0),
      p_proceso_cod LIKE cat_proceso.proceso_cod,
      p_opera_cod   LIKE cat_operacion.opera_cod,
      p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
      r_bandera     SMALLINT,
      p_mes         INTEGER,
      p_anio        INTEGER

   DEFINE  
      v_ventana ui.Window,
      v_forma   ui.form,
      p_ventana STRING, 
      g_usuario_cod LIKE seg_usuario.usuario_cod,
      v_i_cont_registros INTEGER
        
   DEFINE v_arr_causales DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para las cuenstas causales encontradas
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
      v_tot_causales_gral   DECIMAL (9,0),
      v_tot_imp_ap          DECIMAL (22,2),
      v_tot_aiv_ap_pat      DECIMAL (18,6),
      v_tot_imp_am          DECIMAL (22,2),
      v_tot_int_gen_pgo_ext DECIMAL (22,2),
      v_tot_aiv_gen_pgo_ext DECIMAL (18,6),
      v_tot_imp_apam        DECIMAL (22,2),
      v_tot_porcentajes     DECIMAL(16,4),
      v_aux                 DECIMAL
   END RECORD

END GLOBALS

MAIN
   DEFINE p_tipo_ejecucion SMALLINT
   
   -- se recuperan los argumentos de la linea de comandos
--   LET g_usuario_cod    = ARG_VAL(1)
--   LET p_tipo_ejecucion = ARG_VAL(2)
--   LET p_ventana        = ARG_VAL(3)
   
   --Asignación de parametros generales
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_nombre_archivo = ARG_VAL(5)
   LET p_mes         = ARG_VAL(6)
   LET p_anio        = ARG_VAL(7)   

   DISPLAY "p_usuario ",p_usuario 
   DISPLAY "p_pid ",p_pid         
   DISPLAY "p_proceso_cod ", p_proceso_cod
   DISPLAY "p_opera_cod   ", p_opera_cod
   DISPLAY "p_nombre_archivo ",p_nombre_archivo
   DISPLAY "p_mes ",p_mes         
   DISPLAY "p_anio ",p_anio

   CALL f_prepara_info_reporte(p_mes,p_anio)

   --Finaliza la operación
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
   RETURNING r_bandera
  
   IF r_bandera = 0 THEN 
      DISPLAY "Se ha realizado la generación del archivo."
      EXIT PROGRAM 
   ELSE --Si ocurrió error 
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
      CALL fn_desplega_inc_operacion(r_bandera)
      EXIT PROGRAM 
   END IF

END MAIN

FUNCTION f_prepara_info_reporte(mes,anio)
   DEFINE 
      v_cb_mes  ui.ComboBox, -- Combo para los meses
      v_cb_anio ui.ComboBox, -- Combo para los años
      aux     INTEGER,       -- Auxiliar para los ciclos 
      año_aux INTEGER,       -- Auxiliar para generar los años del combo
      mes     CHAR(2),       -- Mes de busqueda
      anio    CHAR(4)        -- Año de busqueda

   DEFINE 
--      mes              INTEGER,  -- Mes
--      anio             INTEGER,  -- Año
--      aux              INTEGER,  -- Auxiliar para los ciclos
      v_mes_aux        SMALLINT, -- para calcular fecha de consulta
      v_ano_aux        SMALLINT, -- para calcular fecha de consulta
      v_consulta_folio STRING,   -- Consulta de folios
      v_consulta_causa STRING    -- Consulta de causales
      
   DEFINE v_arr_folios     DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para los folios encontrados
      v_folio LIKE glo_folio.folio
   END RECORD
   
   DEFINE
      v_fecha_consulta DATE,       -- fecha usada para consultar los folios
      v_fecha_texto    VARCHAR(10)

   DEFINE manejador_rpt      om.SaxDocumentHandler
   DEFINE v_genero_reporte_pdf BOOLEAN
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte   VARCHAR(80)
   DEFINE v_existe        SMALLINT

{   LET v_nom_reporte = p_usuario     CLIPPED,"-PAGC09-",
                       p_pid         USING "&&&&&", "-",
                       p_proceso_cod USING "&&&&&","-",
                       1             USING "&&&&&"

--   CALL fn_rutas("pag") RETURNING r_ruta_bin, r_ruta_listados
   CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

   LET v_genero_reporte_pdf= FALSE

   CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/PAGC09.4rp") RETURNING v_existe

   IF v_existe THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)
      CALL fgl_report_selectPreview(0)  -- 1=Genera reporte de inmediato 0=Genera reporte en ruta
      LET manejador_rpt = fgl_report_commitCurrentSettings()
      LET v_genero_reporte_pdf = TRUE
   ELSE
      RETURN v_genero_reporte_pdf
   END IF
}

--============================      
   LET v_i_cont_registros  = 1  --SE INICIALIZA EL CONMTADOR DE REGISTROS PARA CTA_HIS_PAGOS

   -- se construye la fecha, obteniendo el dia primero del mes siguiente al elegido
   LET v_mes_aux = mes
   LET v_ano_aux = anio
   
   IF ( v_mes_aux = 12 ) THEN
      -- si el mes elegido es diciembre, entonces el mes siguiente es enero
      LET v_mes_aux = 1
      -- se incrementa el ano
      LET v_ano_aux = v_ano_aux + 1
   ELSE
      -- se incrementa el mes
      LET v_mes_aux = v_mes_aux + 1
   END IF
   
   -- la fecha es
   LET v_fecha_texto = v_mes_aux USING "&&", "/01/", v_ano_aux
   
   -- se construye la fecha
   LET v_fecha_consulta = DATE(v_fecha_texto)
   
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
    FOREACH cur_busca_folios USING v_fecha_consulta  INTO v_arr_folios[aux].*
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
              " FROM cta_his_pagos cta, pag_tpo_aclaracion tpa ",                                   --saci2018-51
              " WHERE cta.folio IN (SELECT folio FROM glo_folio WHERE f_actualiza < ?  ",
              " AND proceso_cod IN (1401, 101,102,103,105,107,110)) ", -- se agregan los proceso cod de aclaraciones
              " AND cta.tpo_aclaracion = tpa.aclaracion_cod ",                                      --saci2018-51
              " AND cta.ind_liquidacion = 1                                                    ",
              -- siguiente linea por j514 09-jul-2015
              " AND (cta.tpo_aclaracion <> '' OR cta.tpo_aclaracion IS NOT NULL)               ",   --saci2018-51
              " GROUP by cta.tpo_aclaracion, tpa.aclaracion_descripcion                        ",
              " HAVING count(*)>0 ",
              " ORDER BY 1"
    
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
      
      FOREACH cur_busca_causales USING v_fecha_consulta INTO v_arr_causales[v_i_cont_registros].*
         LET v_arr_totales.v_tot_causales_gral = v_arr_totales.v_tot_causales_gral + v_arr_causales[v_i_cont_registros].v_tot_causal
         LET v_arr_totales.v_tot_imp_ap = v_arr_totales.v_tot_imp_ap + v_arr_causales[v_i_cont_registros].v_imp_ap
         LET v_arr_totales.v_tot_aiv_ap_pat = v_arr_totales.v_tot_aiv_ap_pat + v_arr_causales[v_i_cont_registros].v_aiv_ap_pat
         LET v_arr_totales.v_tot_imp_am = v_arr_totales.v_tot_imp_am + v_arr_causales[v_i_cont_registros].v_amp_am
         LET v_arr_totales.v_tot_int_gen_pgo_ext = v_arr_totales.v_tot_int_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_int_gen_pgo_ext
         LET v_arr_totales.v_tot_aiv_gen_pgo_ext = v_arr_totales.v_tot_aiv_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_aiv_gen_pgo_ext
         LET v_arr_totales.v_tot_imp_apam = v_arr_totales.v_tot_imp_apam + v_arr_causales[v_i_cont_registros].v_imp_ap_am
         LET v_i_cont_registros = v_i_cont_registros + 1 
      END FOREACH
      FREE cur_busca_causales
      
      -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
      LET v_i_cont_registros = v_i_cont_registros  - 1
      CALL v_arr_causales.deleteElement(v_arr_causales.getLength())
      IF(v_arr_causales.getLength()>0)THEN  
         -- Calculo de porcentajes
         LET v_arr_totales.v_tot_porcentajes = 0
         FOR aux = 1 TO v_arr_causales.getLength() STEP +1
            --DISPLAY v_arr_causales[aux].v_porcentaje," ", v_arr_causales[aux].v_imp_ap_am ," ",  v_arr_totales.v_tot_imp_apam     
            LET v_arr_causales[aux].v_porcentaje = (v_arr_causales[aux].v_imp_ap_am / v_arr_totales.v_tot_imp_apam) * 100    
            LET v_arr_totales.v_tot_porcentajes = v_arr_totales.v_tot_porcentajes + v_arr_causales[aux].v_porcentaje
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
   DEFINE manejador_rpt         om.SaxDocumentHandler,
          v_i_inicio_for    INTEGER
   
   DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
   DEFINE v_ruta_listados      STRING -- ruta de los listados
   DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
   DEFINE v_mes                CHAR(2)-- Mes de busqueda
   DEFINE v_anio               CHAR(4)-- Año de busqueda
   DEFINE v_mes_desc            CHAR(12)

   --DEFINE manejador_rpt      om.SaxDocumentHandler
   DEFINE v_genero_reporte_pdf BOOLEAN
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte   VARCHAR(80)
   DEFINE v_existe        SMALLINT

   LET v_i_inicio_for = 1

{
   # Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                               --r_ruta_bin, r_ruta_listados

   --Se asigna la plantilla para generar el reporte
   IF fgl_report_loadCurrentSettings("PAGC09.4rp") THEN 
       CALL fgl_report_selectDevice ("PDF")
                   
       LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","PAGC09"
       CALL fgl_report_setOutputFileName(v_ruta_reporte)
       CALL fgl_report_selectPreview(0)
       LET manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE         
       DISPLAY "no fue posible generar el reporte"
       EXIT PROGRAM 
   END IF   
}

   LET v_nom_reporte = p_usuario     CLIPPED,"-PAGC09-",
                       p_pid         USING "&&&&&", "-",
                       p_proceso_cod USING "&&&&&","-",
                       1             USING "&&&&&"

   CALL fn_rutas("pag") RETURNING r_ruta_bin, r_ruta_listados
--   CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

   LET v_genero_reporte_pdf= FALSE

   CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/PAGC09.4rp") RETURNING v_existe

   IF v_existe THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)
      CALL fgl_report_selectPreview(0)  -- 1=Genera reporte de inmediato 0=Genera reporte en ruta
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
   -- Asigna el titulo del reporte
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
      v_folio            LIKE glo_folio.folio, -- folio
      v_nombre_usuario   VARCHAR(100),
      v_tipo_registro    VARCHAR(20),
      v_nombre_archivo   VARCHAR(40),
      v_ruta_envio       VARCHAR(40),
      p_mes_desc         CHAR(12),        -- Mes de corte
      p_anio             CHAR(4),          -- Año de corte
      v_archivo_rechazos VARCHAR(100),
      v_nombre_archivo_rechazos STRING
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER    
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = g_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED

      -- se obtiene el nombre del archivo
{
      SELECT nombre_archivo
      INTO   v_nombre_archivo
      FROM   glo_ctr_archivo
      WHERE  folio = v_folio

      -- si no se encuentra el nombre del archivo indicamos error
      IF ( v_nombre_archivo IS NULL ) THEN
         LET v_nombre_archivo = "No se encuentra nombre archivo"
      END IF
}
                                                                                              
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

                  