-- ==================================================================
-- PAGC40.4gl
-- CONSULTA DE PAGOS ADELANTADOS NO CONFIRMADOS
-- Fecha: 12 Febrero 2013
 -- ==================================================================
 
DATABASE safre_viv
GLOBALS
DEFINE  v_ventana            ui.Window,
        v_forma              ui.form,
        p_ventana            STRING, 
        v_arr_causales       DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para las cuenstas causales encontradas
           v_causal           LIKE cta_his_pagos.tpo_aclaracion,
           v_causal_des       LIKE pag_tpo_aclaracion.aclaracion_descripcion,
           v_ind_liquidacion  SMALLINT, -- indicador de liquidacion
           v_tot_causal       INTEGER,
           v_imp_ap           DECIMAL (22,2), 
           v_aiv_ap_pat       DECIMAL (18,6),
           v_amp_am           DECIMAL (22,2),
           v_int_gen_pgo_ext  DECIMAL (22,2),
           v_aiv_gen_pgo_ext  DECIMAL (18,6),
           v_imp_ap_am        DECIMAL (22,2),
           v_porcentaje       DECIMAL (16,4)
        END RECORD,
        v_arr_totales           RECORD
           v_tot_causales_gral   DECIMAL (9,0),
           v_tot_imp_ap          DECIMAL (22,2),
           v_tot_aiv_ap_pat      DECIMAL (18,6),
           v_tot_imp_am          DECIMAL (22,2),
           v_tot_int_gen_pgo_ext DECIMAL (22,2),
           v_tot_aiv_gen_pgo_ext DECIMAL (18,6),
           v_tot_imp_apam        DECIMAL (22,2),
           v_tot_porcentajes     DECIMAL(16,4),
           v_aux                 DECIMAL
           END RECORD,
        v_i_cont_registros     INTEGER,
        g_usuario_cod          LIKE seg_usuario.usuario_cod -- clave del usuario firmado
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion     SMALLINT
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_ventana       = ARG_VAL(3)

   -- consulta de pagos adelantados no confirmados
   CALL f_reporte_pagos_adelantados_no_confirmados()
END MAIN

{===========================================================================
Nombre: f_reporte_pagos_adelantados_no_confirmados
Fecha creacion: 12 Febrero 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
 Consulta los pagos adelantados no confirmados de la tabla cta_his_pagos
 por periodo

 Parametros de Entrada:
 - Ninguno
Parámetros de salida:
 - Ninguno

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION f_reporte_pagos_adelantados_no_confirmados()
DEFINE v_cb_mes  ui.ComboBox, -- Combo para los meses
       v_cb_ano ui.ComboBox, -- Combo para los años
       aux       INTEGER,     -- Auxiliar para los ciclos 
       año_aux   INTEGER,     -- Auxiliar para generar los años del combo
       mes       CHAR(2),     -- Mes de busqueda
       ano      CHAR(4)      -- Año de busqueda

      
   CLOSE WINDOW SCREEN
   -- Se abre la ventana de la consulta que contiene comboboxs para mes y año
   -- El combobox de meses es estatico y esta definido en la forma
   -- El combobox del año se genera del año actual hasta 5 años atras  
   OPEN WINDOW w_consulta WITH FORM "PAGC401"

   -- Se asigna el titulo de la ventana
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma = v_ventana.getForm() 
      
   IF ( p_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_ventana)         
      CALL v_ventana.setText(p_ventana)
   END IF

   -- Inicar combos  
   LET v_cb_mes = ui.ComboBox.forName("v_cb_mes")
   LET v_cb_ano = ui.ComboBox.forName("v_cb_ano")

   -- Limpiar combo anos
   CALL v_cb_ano.clear()

   -- Ciclo de carga del combo de años, se obtiene el año actual
   -- y se decrementa en 5 años
   LET año_aux = YEAR(TODAY)  
   FOR aux = 1 TO 5
      CALL v_cb_ano.addItem(año_aux,año_aux)
      LET año_aux = YEAR(TODAY) - aux
   END FOR
      

   INPUT mes, ano WITHOUT DEFAULTS
   FROM v_cb_mes, v_cb_ano    ATTRIBUTES (UNBUFFERED ,ACCEPT=FALSE)

      -- Se pre-seleccionan los combos con Enero y el año en curso
      -- Se evita seleccion en nulo de los criterio con la forma
      BEFORE INPUT
        LET mes = "01"
        LET ano = YEAR(TODAY) 

      -- Al cambio de seleccion de los combos obtengo sus valores seleccionados  
      ON CHANGE v_cb_mes
        LET mes = GET_FLDBUF(v_cb_mes)
      ON CHANGE v_cb_ano
        LET ano = GET_FLDBUF(v_cb_ano)
      -- Al aceptar se llama a la funcion que valida si hay informacion con los criterios
      ON ACTION reporte
        DISPLAY mes," ", ano
        CALL f_prepara_info_reporte(mes,ano)
    
      ON ACTION CANCEL
        EXIT INPUT

   END INPUT

CLOSE WINDOW w_consulta

END FUNCTION

{===========================================================================
Nombre: f_prepara_info_reporte
Fecha creacion: 12 Febrero 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
 Prepara el reporte en formato impreso de la consulta de pagos adelantados
 no confirmados
 
Parametros de Entrada:
 - Ninguno  
Parámetros de salida:
 - Ninguno
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION f_prepara_info_reporte(mes, ano)
# Funcion que reliza las consultas de validacion de datos para ejecutar el reporte
DEFINE mes              INTEGER, -- Mes
       ano             INTEGER, -- Año
       aux              INTEGER, -- Auxiliar para los ciclos
       v_mes_aux        SMALLINT, -- para calcular fecha de consulta
       v_ano_aux        SMALLINT, -- para calcular fecha de consulta
       v_consulta_folio STRING,  -- Consulta de folios
       v_consulta_causa STRING,  -- Consulta de causales 
       v_arr_folios     DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para los folios encontrados
          v_folio       LIKE glo_folio.folio
       END RECORD,
       v_fecha_consulta DATE, -- fecha usada para consultar los folios
       v_fecha_texto    VARCHAR(10)
       
   LET v_i_cont_registros = 1  --SE INICIALIZA EL CONMTADOR DE REGISTROS PARA CTA_HIS_PAGOS

   -- se construye la fecha, obteniendo el dia primero del mes siguiente al elegido
   LET v_mes_aux = mes
   LET v_ano_aux = ano
       
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
   LET v_consulta_folio = "\nSELECT folio ",
                          "\nFROM   glo_folio",
                          "\nWHERE  f_actualiza < ? " ,
--                          "\nAND proceso_cod IN (1401, 101,102,103,105,107,110)"
                          "\nAND    proceso_cod IN (1401,110)"

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

   -- Se evalua el resultado de la validacion
   IF( v_arr_folios.getLength() > 0) THEN
      -- Se realiza la busqueda de cuentas causales con los folios encontrados
      LET v_consulta_causa = "\n SELECT                                                          ",
                             "\n   cta.tpo_aclaracion                                 ,          ",
                             "\n   tpa.aclaracion_descripcion                         ,          ",
                             "\n   cta.ind_liquidacion                                ,          ",
                             "\n   count(cta.tpo_aclaracion) as no                    ,          ",
                             "\n   SUM(cta.imp_ap_pat) as pat                         ,          ",
                             "\n   SUM(cta.aiv_ap_pat) as aiv                         ,          ",
                             "\n   SUM(cta.imp_am_cre) as cre                         ,          ",
                             "\n   SUM(cta.int_gen_pgo_ext) as ext                    ,          ",
                             "\n   SUM(cta.aiv_gen_pgo_ext) as aiv_ext                ,          ",
                             "\n   SUM(cta.aiv_ap_pat + cta.aiv_gen_pgo_ext ) as suma , 0        ",
                             "\n FROM cta_his_pagos cta LEFT OUTER JOIN pag_tpo_aclaracion tpa ON",
                             "\n cta.tpo_aclaracion = tpa.aclaracion_cod                         ",
                             "\n WHERE cta.origen_archivo IN (1,4)                               ", -- lqinfo 1, Carga inicial 4
                             "\n AND   cta.f_proceso < ?                                        ", -- de la fecha elegida hacia atras
                             "\n AND   cta.ind_liquidacion IN (2,3)                              ", -- adelantados no confirmados
--                             "\n AND   cta.tpo_aclaracion in (13,17)                             ", -- nuevo 27-ag8-2013 seQuitaPque salgan ade q no tiene tpoacl
--------                             "\n AND   cta.folio = 13944                                         ",   --- temporal para prueba con Hamir
                             "\n GROUP by cta.tpo_aclaracion, tpa.aclaracion_descripcion,        ",
                             "\n       cta.ind_liquidacion                                       ",
                             "\n HAVING count(*)>0",
                             "\n ORDER BY 3,1"
      DISPLAY v_consulta_causa   
      PREPARE prp_busca_causales FROM v_consulta_causa
          
      DECLARE cur_busca_causales CURSOR FOR prp_busca_causales

      -- Inicializa en 1 el valor de de la bandera   
      LET v_i_cont_registros                  = 1
      LET v_arr_totales.v_tot_imp_apam        = 0
      LET v_arr_totales.v_tot_causales_gral   = 0
      LET v_arr_totales.v_tot_imp_ap          = 0
      LET v_arr_totales.v_tot_imp_am          = 0
      LET v_arr_totales.v_tot_porcentajes     = 0
      LET v_arr_totales.v_tot_aiv_ap_pat      = 0
      LET v_arr_totales.v_tot_int_gen_pgo_ext = 0
      LET v_arr_totales.v_tot_aiv_gen_pgo_ext = 0
      
      -- se transfieren los datos
      FOREACH cur_busca_causales USING v_fecha_consulta INTO v_arr_causales[v_i_cont_registros].*
         -- arreglo de totales
         LET v_arr_totales.v_tot_causales_gral = v_arr_totales.v_tot_causales_gral + v_arr_causales[v_i_cont_registros].v_tot_causal
         LET v_arr_totales.v_tot_imp_ap = v_arr_totales.v_tot_imp_ap + v_arr_causales[v_i_cont_registros].v_imp_ap
         LET v_arr_totales.v_tot_aiv_ap_pat = v_arr_totales.v_tot_aiv_ap_pat + v_arr_causales[v_i_cont_registros].v_aiv_ap_pat
         LET v_arr_totales.v_tot_imp_am = v_arr_totales.v_tot_imp_am + v_arr_causales[v_i_cont_registros].v_amp_am
         LET v_arr_totales.v_tot_int_gen_pgo_ext = v_arr_totales.v_tot_int_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_int_gen_pgo_ext
         LET v_arr_totales.v_tot_aiv_gen_pgo_ext = v_arr_totales.v_tot_aiv_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_aiv_gen_pgo_ext
         LET v_arr_totales.v_tot_imp_apam = v_arr_totales.v_tot_imp_apam + v_arr_causales[v_i_cont_registros].v_imp_ap_am
         LET v_i_cont_registros = v_i_cont_registros + 1 
      END FOREACH
      -- se libera el cursor
      FREE cur_busca_causales
      
      -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
      LET v_i_cont_registros = v_i_cont_registros - 1
      CALL v_arr_causales.deleteElement(v_arr_causales.getLength())
      
      IF ( v_arr_causales.getLength()>0 ) THEN
         -- Calculo de porcentajes
         LET v_arr_totales.v_tot_porcentajes = 0
         
         FOR aux = 1 TO v_arr_causales.getLength() STEP +1
            LET v_arr_causales[aux].v_porcentaje = (v_arr_causales[aux].v_imp_ap_am / v_arr_totales.v_tot_imp_apam) * 100    
            LET v_arr_totales.v_tot_porcentajes = v_arr_totales.v_tot_porcentajes + v_arr_causales[aux].v_porcentaje
         END FOR
         
         CALL fn_reporte_adelantados_no_confirmados( TRUE, mes, ano)
      ELSE
         -- Se notifica al usuario que no se encontro informacion con los criterios
         CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
      END IF 
   ELSE
      -- Se notifica al usuario que no se encontro informacion con los criterios
      CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
   END IF
       
    
END FUNCTION

{===========================================================================
Nombre: fn_reporte_adelantados_no_confirmados
Fecha creacion: 12 Febrero 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
 Emite el reporte de pagos adelantados no confirmados
 
Parametros de Entrada:
 p_b_despliegue_pantalla - indica si se mostrara en pantalla
 v_mes  - mes de consulta
 v_ano - ano de consulta
Parámetros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_reporte_adelantados_no_confirmados(p_b_despliegue_pantalla, v_mes, v_ano)
DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
DEFINE manejador_rpt      om.SaxDocumentHandler,
       v_i_inicio_for     INTEGER , 
       v_ruta_reporte     STRING  , -- ruta del archivo del reporte
       v_ruta_listados    STRING  , -- ruta de los listados
       v_ruta_ejecutable  STRING  , -- ruta del ejecutable
       v_mes              CHAR(2) , -- Mes de busqueda
       v_ano             CHAR(4) , -- Año de busqueda
       v_mes_desc         CHAR(12)

   LET v_i_inicio_for = 1
 
   -- Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                               --r_ruta_bin, r_ruta_listados
   --Se asigna la plantilla para generar el reporte
   IF ( fgl_report_loadCurrentSettings("PAGC40.4rp") ) THEN 
       CALL fgl_report_selectDevice ("PDF")
                   
       LET v_ruta_reporte = v_ruta_listados CLIPPED, "/PAGC40"
       CALL fgl_report_setOutputFileName(v_ruta_reporte)
       CALL fgl_report_selectPreview(1)
       LET manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE         
       CALL fn_mensaje("Atención", "No fue posible generar el reporte", "stop")
       RETURN
   END IF   

   -- se asigna descripción al mes
   CASE v_mes <> 0
       WHEN v_mes = 1
           LET v_mes_desc = 'Enero'      
       WHEN v_mes = 2  
           LET v_mes_desc = 'Febrero'    
       WHEN v_mes = 3  
           LET v_mes_desc = 'Marzo'      
       WHEN v_mes = 4  
           LET v_mes_desc = 'Abril'      
       WHEN v_mes = 5  
           LET v_mes_desc = 'Mayo'       
       WHEN v_mes = 6  
           LET v_mes_desc = 'Junio'      
       WHEN v_mes = 7 
           LET v_mes_desc = 'Julio'      
       WHEN v_mes = 8  
           LET v_mes_desc = 'Agosto'     
       WHEN v_mes = 9  
           LET v_mes_desc = 'Septiembre' 
       WHEN v_mes = 10 
           LET v_mes_desc = 'Octubre'    
       WHEN v_mes = 11 
           LET v_mes_desc = 'Noviembre'  
       WHEN v_mes = 12 
           LET v_mes_desc = 'Diciembre'  
   END CASE  
   
   -- Inicia el reporte de registros con rechazo
   START REPORT rpt_pagos_adelantados_no_confirmados TO XML HANDLER manejador_rpt
   
   -- Asigna el titulo del reporte
   FOR v_i_inicio_for = 1 TO v_i_cont_registros
      OUTPUT TO REPORT rpt_pagos_adelantados_no_confirmados(v_arr_causales[v_i_inicio_for].*, v_mes_desc, v_ano)
   END FOR  

   FINISH REPORT rpt_pagos_adelantados_no_confirmados
END FUNCTION

{===========================================================================
Nombre: rpt_pagos_adelantados_no_confirmados
Fecha creacion: 12 Febrero 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
 Genera el reporte en formato para imprimir de pagos adelantados no confirmados
 
Parametros de Entrada:
 v_arr_causales - Arreglo de causales
 p_mes_desc - Mes en texto
 p_ano - Ano de consulta
Parámetros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
REPORT rpt_pagos_adelantados_no_confirmados(v_arr_causales, p_mes_desc, p_ano)
  DEFINE  v_arr_causales    RECORD -- Arreglo dinamico para las cuenstas causales encontradas
          v_causal          LIKE cta_his_pagos.tpo_aclaracion,
          v_causal_des      LIKE pag_tpo_aclaracion.aclaracion_descripcion,
          v_ind_liquidacion SMALLINT,
          v_tot_causal      INTEGER,
          v_imp_ap          DECIMAL (22,2),
          v_aiv_ap_pat      DECIMAL (18,6),
          v_amp_am          DECIMAL (22,2),
          v_int_gen_pgo_ext DECIMAL (22,2),
          v_aiv_gen_pgo_ext DECIMAL (18,6),
          v_imp_ap_am       DECIMAL (22,2),
          v_porcentaje      DECIMAL (16,4)
          END RECORD,
          r_subtotales           RECORD
           v_tot_causales_gral    DECIMAL (9,0),
           v_tot_imp_ap           DECIMAL (22,2),
           v_tot_aiv_ap_pat       DECIMAL (18,6),
           v_tot_imp_am           DECIMAL (22,2),
           v_tot_int_gen_pgo_ext  DECIMAL (22,2),
           v_tot_aiv_gen_pgo_ext  DECIMAL (18,6),
           v_tot_imp_apam         DECIMAL (22,2),
           v_tot_porcentajes      DECIMAL(16,4)
          END RECORD,
          v_fecha_reporte  DATE,
          v_folio                    LIKE glo_folio.folio, -- folio
          v_nombre_usuario           VARCHAR(100),
          v_tipo_registro            VARCHAR(20),
          v_nombre_archivo           VARCHAR(40),
          v_nombre_archivo_rechazos  STRING,
          v_ruta_envio               VARCHAR(40),
          v_archivo_rechazos         VARCHAR(100),
          p_mes_desc                 CHAR(12),    -- Mes de corte
          p_ano                     CHAR(4),      -- Año de corte
          v_descripcion     STRING
          

                                                                                                                                                                                          
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
      PRINTX v_nombre_usuario,
             g_usuario_cod ,
             p_mes_desc ,
             p_ano
      
   BEFORE GROUP OF v_arr_causales.v_ind_liquidacion
      -- se verifica el tipo de liquidacion para hacer el titulo de subseccion
      IF ( v_arr_causales.v_ind_liquidacion = 3 ) THEN
         LET v_descripcion = "Pagos pendientes de confirmar de IMSS (adelantos por causales 13 o 17)"
      END IF

      IF ( v_arr_causales.v_ind_liquidacion = 2 ) THEN
         LET v_descripcion = "Pagos pendientes de confirmar Solo Infonavit "
      END IF

      -- se inicia el registro de subtotales
      LET r_subtotales.v_tot_causales_gral    = 0
      LET r_subtotales.v_tot_imp_ap           = 0
      LET r_subtotales.v_tot_aiv_ap_pat       = 0
      LET r_subtotales.v_tot_imp_am           = 0
      LET r_subtotales.v_tot_int_gen_pgo_ext  = 0
      LET r_subtotales.v_tot_aiv_gen_pgo_ext  = 0
      LET r_subtotales.v_tot_imp_apam         = 0
      LET r_subtotales.v_tot_porcentajes      = 0
      
      -- se despliega la descripcion
      PRINTX v_descripcion
                                                                                              
   ON EVERY ROW                                                                               
      PRINTX v_arr_causales.*
      
      -- se incrementan los montos v_arr_causales
      LET r_subtotales.v_tot_causales_gral    = r_subtotales.v_tot_causales_gral   + v_arr_causales.v_tot_causal     
      LET r_subtotales.v_tot_imp_ap           = r_subtotales.v_tot_imp_ap          + v_arr_causales.v_imp_ap         
      LET r_subtotales.v_tot_aiv_ap_pat       = r_subtotales.v_tot_aiv_ap_pat      + v_arr_causales.v_aiv_ap_pat     
      LET r_subtotales.v_tot_imp_am           = r_subtotales.v_tot_imp_am          + v_arr_causales.v_amp_am         
      LET r_subtotales.v_tot_int_gen_pgo_ext  = r_subtotales.v_tot_int_gen_pgo_ext + v_arr_causales.v_int_gen_pgo_ext
      LET r_subtotales.v_tot_aiv_gen_pgo_ext  = r_subtotales.v_tot_aiv_gen_pgo_ext + v_arr_causales.v_aiv_gen_pgo_ext
      LET r_subtotales.v_tot_imp_apam         = r_subtotales.v_tot_imp_apam        + v_arr_causales.v_imp_ap_am      
      LET r_subtotales.v_tot_porcentajes      = r_subtotales.v_tot_porcentajes     + v_arr_causales.v_porcentaje     

   AFTER GROUP OF v_arr_causales.v_ind_liquidacion
      -- se imprimen los subtotales
      PRINTX r_subtotales.*
      
   ON LAST ROW
      -- se imprimen los totales finales
      PRINTX v_arr_totales.*
                                                                                           
END REPORT  

                  