--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:29/06/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLC14-1                                                               #
#Objetivo     => Consulta, lanzador de reporte Causales de aclaracion                   #
#Fecha inicio => Junio 29, 2012                                                         # 
#Autor        => SINUHE DIAZ LUNA                                                       #
#########################################################################################

DATABASE safre_viv
GLOBALS
DEFINE  v_ventana            ui.Window,
        v_forma              ui.form,
        p_ventana            STRING, 
        --v_arr_causales   RECORD -- Arreglo dinamico para las cuenstas causales encontradas
        v_arr_causales DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para las cuenstas causales encontradas
           v_causal           LIKE cta_his_pagos.tpo_aclaracion,
           v_causal_des       LIKE pag_tpo_aclaracion.aclaracion_descripcion,
           v_tot_causal       INTEGER,
           v_imp_ap           LIKE cta_his_pagos.imp_ap_pat,
           v_aiv_ap_pat       LIKE cta_his_pagos.aiv_ap_pat,
           v_amp_am           LIKE cta_his_pagos.imp_am_cre,
           v_int_gen_pgo_ext  LIKE cta_his_pagos.int_gen_pgo_ext,
           v_aiv_gen_pgo_ext  LIKE cta_his_pagos.aiv_gen_pgo_ext,
           v_imp_ap_am        LIKE cta_his_pagos.imp_ap_pat,
           v_porcentaje       DECIMAL(16,4)
        END RECORD,
        v_arr_totales  RECORD
           v_tot_causales_gral   INTEGER,
           v_tot_imp_ap          DECIMAL,
           v_tot_aiv_ap_pat      DECIMAL,
           v_tot_imp_am          DECIMAL,
           v_tot_int_gen_pgo_ext DECIMAL,
           v_tot_aiv_gen_pgo_ext DECIMAL,
           v_tot_imp_apam        DECIMAL,
           v_tot_porcentajes     DECIMAL(16,4),
           v_aux                 DECIMAL
           END RECORD,
        v_i_cont_registros     INTEGER
END GLOBALS

MAIN
   -- se llama a la funcion que abre la vantana de criterios de busqueda
    # Se recupera la clave de usuario desde parámetro 
   LET p_ventana = ARG_VAL(3)
   CALL f_reporte_causales_aclaracion()

END MAIN

         
FUNCTION f_reporte_causales_aclaracion()
# funcion que despliega la ventana de criterios para lanzar el reporte
# de causales de aclaracion
DEFINE v_cb_mes ui.ComboBox, -- Combo para los meses
       v_cb_anio ui.ComboBox, -- Combo para los años
       aux      INTEGER,     -- Auxiliar para los ciclos 
       año_aux  INTEGER,     -- Auxiliar para generar los años del combo
       mes      CHAR(2),     -- Mes de busqueda
       anio      CHAR(4)      -- Año de busqueda

      
   CLOSE WINDOW SCREEN
   -- Se abre la ventana de la consulta que contiene combobox's para mes y año
   -- El combobox de meses es estatico y esta definido en la forma
   -- El combobox del año se genera del año actual hasta 5 años atras  
   OPEN WINDOW ACLC14 WITH FORM "ACLC141"

   -- Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF ( p_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_ventana)         
         CALL v_ventana.setText(p_ventana)
      END IF

      -- Inicar combos  
      LET v_cb_mes = ui.ComboBox.forName("v_cb_mes")
      LET v_cb_anio = ui.ComboBox.forName("v_cb_anio")
      -- Limiar combo años
      CALL v_cb_anio.clear()

      -- Ciclo de carga del combo de años, se obtiene el año actual
      -- y se decrementa en 5 años
      LET año_aux = YEAR(TODAY)  
      FOR aux = 1 TO  5 STEP +1
        CALL v_cb_anio.addItem(año_aux,año_aux)
        LET año_aux = YEAR(TODAY) - aux
        --DISPLAY año_aux
      END FOR
      

    INPUT mes, anio WITHOUT DEFAULTS
      FROM v_cb_mes, v_cb_anio    ATTRIBUTES (UNBUFFERED ,WITHOUT DEFAULTS,ACCEPT=FALSE)

      -- Se pre-seleccionan los combos con Enero y el año en curso
      -- Se evita seleccion en nulo de los criterio con la forma
      BEFORE INPUT
        LET mes = "01"
        LET anio = YEAR(TODAY) 

      -- Al cambio de seleccion de los combos obtengo sus valores seleccionados  
      ON CHANGE v_cb_mes
        LET mes = GET_FLDBUF(v_cb_mes)
      ON CHANGE v_cb_anio
        LET anio = GET_FLDBUF(v_cb_anio)
      -- Al aceptar se llama a la funcion que valida si hay informacion con los criterios
      ON ACTION reporte
        DISPLAY mes," ", anio
        CALL f_prepara_info_reporte(mes,anio)
    
      ON ACTION CANCEL
        EXIT INPUT

    END INPUT

CLOSE WINDOW ACLC14

END FUNCTION

FUNCTION f_prepara_info_reporte(mes,anio)
# Funcion que reliza las consultas de validacion de datos para ejecutar el reporte
DEFINE mes              INTEGER, -- Mes
       anio             INTEGER, -- Año
       aux              INTEGER, -- Auxiliar para los ciclos
       v_consulta_folio STRING,  -- Consulta de folios
       v_consulta_causa STRING,  -- Consulta de causales 
       v_arr_folios     DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para los folios encontrados
          v_folio       LIKE glo_folio.folio
          END RECORD
       

       LET v_i_cont_registros  = 1  --SE INICIALIZA EL CONMTADOR DE REGISTROS PARA CTA_HIS_PAGOS

    
       -- Busqueda de existencia de registros con los criterios recibidos
       -- Se utilizan los codigos de proceso definidos en el requerimiento   
       LET v_consulta_folio = "\n SELECT folio ",
                              "\n FROM glo_folio",
                              "\n WHERE month(f_actualiza) <= ? " ,
                              "\n AND year(f_actualiza) <= ? ",
                              "\n AND proceso_cod = 1401"
        PREPARE prp_busca_folios FROM v_consulta_folio
        DECLARE cur_busca_folios CURSOR FOR prp_busca_folios
        --DISPLAY v_consulta_folio

        -- Inicializa en 0 el valor de de la bandera   
        LET aux = 1
        FOREACH cur_busca_folios USING mes,anio  INTO v_arr_folios[aux].*
          LET aux = aux + 1
        END FOREACH
        FREE cur_busca_folios
        -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
        CALL v_arr_folios.deleteElement(aux)
        DISPLAY v_arr_folios.getLength()

        -- Se evalua el resultado de la validacion
       IF( v_arr_folios.getLength() > 0) THEN
          -- Se realiza la buqueda de cuentas causales con los folios encontrados
          LET v_consulta_causa = "\n SELECT cta.tpo_aclaracion,tpa.aclaracion_descripcion,count(cta.tpo_aclaracion) ",
                  "\n as no ,SUM(cta.imp_ap_pat) as pat,                                             ",
                  "\n SUM(cta.aiv_ap_pat) as aiv,                                                    ",
                  "\n SUM(cta.imp_am_cre) as cre ,                                                   ",
                  "\n SUM(cta.int_gen_pgo_ext) as ext,                                               ",
                  "\n SUM(cta.aiv_gen_pgo_ext) as aiv_ext,                                           ",
                  "\n SUM(cta.aiv_ap_pat + cta.aiv_gen_pgo_ext ) as suma , 0                         ",
                  "\n FROM cta_his_pagos cta JOIN pag_tpo_aclaracion tpa ON                          ",
                  "\n cta.tpo_aclaracion = tpa.aclaracion_cod                                        ",
                  "\n WHERE cta.folio IN (SELECT folio FROM glo_folio WHERE month(f_actualiza) = ?   ",
                  "\n AND year(f_actualiza) = ? AND proceso_cod = 1401)                              ",
                  "\n GROUP by cta.tpo_aclaracion, tpa.aclaracion_descripcion                        ",
                  "\n HAVING count(*)>0",
                  "\n ORDER BY 1"
           
          PREPARE prp_busca_causales FROM v_consulta_causa
          --DISPLAY v_consulta_causa  
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
          
          FOREACH cur_busca_causales USING mes,anio INTO v_arr_causales[v_i_cont_registros].*
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
            CALL fn_reporte_carga_archivo( TRUE)
           ELSE
              -- Se notifica al usuario que no se encontro informacion con los criterios
              CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
           END IF 
       ELSE
          -- Se notifica al usuario que no se encontro informacion con los criterios
          CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
       END IF
       
    
END FUNCTION

--FUNCTION fn_reporte_carga_archivo(v_arr_causales ,total_apam, p_b_despliegue_pantalla)
FUNCTION fn_reporte_carga_archivo(p_b_despliegue_pantalla)
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE manejador_rpt         om.SaxDocumentHandler,
           v_i_inicio_for    INTEGER
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable

    LET v_i_inicio_for = 1
 
    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("rpt_causales_aclaracion.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","rpt_causales_aclaracion"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
           
          --Inicia el reporte de registros con rechazo
            START REPORT rpt_causales_aclaracion TO XML HANDLER manejador_rpt
            -- Asigna el titulo del reporte
            FOR v_i_inicio_for = 1 TO v_i_cont_registros
               OUTPUT TO REPORT rpt_causales_aclaracion(v_arr_causales[v_i_inicio_for].*)
            END FOR  
            FINISH REPORT rpt_causales_aclaracion 
END FUNCTION

REPORT rpt_causales_aclaracion(v_arr_causales)
  DEFINE  v_arr_causales   RECORD -- Arreglo dinamico para las cuenstas causales encontradas
          v_causal         LIKE cta_his_pagos.tpo_aclaracion,
          v_causal_des     LIKE pag_tpo_aclaracion.aclaracion_descripcion,
          v_tot_causal     INTEGER,
          v_imp_ap         LIKE cta_his_pagos.imp_ap_pat,
          v_aiv_ap_pat     LIKE cta_his_pagos.aiv_ap_pat,
          v_amp_am         LIKE cta_his_pagos.imp_am_cre,
          v_int_gen_pgo_ext LIKE cta_his_pagos.int_gen_pgo_ext,
          v_aiv_gen_pgo_ext LIKE cta_his_pagos.aiv_gen_pgo_ext,
          v_imp_ap_am      LIKE cta_his_pagos.imp_ap_pat,
          v_porcentaje     DECIMAL(16,4)
          END RECORD,
          v_fecha_reporte  DATE
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER                                                                          
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"  
      PRINTX v_arr_totales.*                                                             
                                                                                              
   ON EVERY ROW                                                                               
    PRINTX v_arr_causales.*
                                                                                           
END REPORT  
