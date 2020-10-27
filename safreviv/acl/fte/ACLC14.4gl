--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:29/06/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLC14-1                                                               #
#Objetivo     => Consulta, lanzador de reporte Causales de aclaracion                   #
#Fecha inicio => Junio 29, 2012                                                         # 
#Autor        => SINUHE DIAZ LUNA                                                        #
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
           v_imp_ap           DECIMAL (22,2),    
           v_aiv_ap_pat       DECIMAL (18,6),    
           v_amp_am           DECIMAL (22,2),    
           v_int_gen_pgo_ext  DECIMAL (22,2),    
           v_aiv_gen_pgo_ext  DECIMAL (18,6),    
           v_imp_ap_am        DECIMAL (22,2),    
           v_porcentaje       DECIMAL(16,4)
        END RECORD,
        v_arr_totales  RECORD
           v_tot_causales_gral   DECIMAL (9,0),
           v_tot_imp_ap          DECIMAL (22,2),
           v_tot_aiv_ap_pat      DECIMAL (18,6),
           v_tot_imp_am          DECIMAL (22,2),
           v_tot_int_gen_pgo_ext DECIMAL (22,2),
           v_tot_aiv_gen_pgo_ext DECIMAL (18,6),
           v_tot_imp_apam        DECIMAL (22,2),
           v_tot_porcentajes     DECIMAL (16,4),
           v_aux                 DECIMAL
        END RECORD,
        v_i_cont_registros     INTEGER,
        g_usuario_cod          LIKE seg_usuario.usuario_cod -- clave del usuario firmado        
END GLOBALS

MAIN
   -- se llama a la funcion que abre la vantana de criterios de busqueda
DEFINE p_tipo_ejecucion     SMALLINT
    # Se recupera la clave de usuario desde par�metro 

   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_ventana        = ARG_VAL(3)

   CALL f_reporte_causales_aclaracion()

END MAIN

         
FUNCTION f_reporte_causales_aclaracion()
# funcion que despliega la ventana de criterios para lanzar el reporte
# de causales de aclaracion
DEFINE v_cb_mes ui.ComboBox, -- Combo para los meses
       v_cb_anio ui.ComboBox, -- Combo para los a�os
       aux      INTEGER,     -- Auxiliar para los ciclos 
       a�o_aux  INTEGER,     -- Auxiliar para generar los a�os del combo
       mes      CHAR(2),     -- Mes de busqueda
       anio      CHAR(4)      -- A�o de busqueda

      
   CLOSE WINDOW SCREEN
   -- Se abre la ventana de la consulta que contiene combobox's para mes y a�o
   -- El combobox de meses es estatico y esta definido en la forma
   -- El combobox del a�o se genera del a�o actual hasta 5 a�os atras  
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
      -- Limiar combo a�os
      CALL v_cb_anio.clear()

      -- Ciclo de carga del combo de a�os, se obtiene el a�o actual
      -- y se decrementa en 5 a�os
      LET a�o_aux = YEAR(TODAY)  
      FOR aux = 1 TO  5 STEP +1
        CALL v_cb_anio.addItem(a�o_aux,a�o_aux)
        LET a�o_aux = YEAR(TODAY) - aux
        --DISPLAY a�o_aux
      END FOR
      

    INPUT mes, anio WITHOUT DEFAULTS
      FROM v_cb_mes, v_cb_anio    ATTRIBUTES (UNBUFFERED, ACCEPT=FALSE)

      -- Se pre-seleccionan los combos con Enero y el a�o en curso
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
        DISPLAY "@ este no es... @"
        CALL f_prepara_info_reporte(mes,anio)
    
      ON ACTION CANCEL
        EXIT INPUT

    END INPUT

CLOSE WINDOW ACLC14

END FUNCTION

FUNCTION f_prepara_info_reporte(mes,anio)
# Funcion que reliza las consultas de validacion de datos para ejecutar el reporte
DEFINE mes              INTEGER, -- Mes
       anio             INTEGER, -- A�o
       v_ano_aux        SMALLINT, -- para calcular fecha de consulta
       v_mes_aux        SMALLINT, -- para calcular fecha de consulta
       aux              INTEGER, -- Auxiliar para los ciclos
       v_consulta_folio STRING,  -- Consulta de folios
       v_consulta_causa STRING,  -- Consulta de causales 
       v_arr_folios     DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para los folios encontrados
          v_folio       LIKE glo_folio.folio
       END RECORD,
       v_fecha_consulta DATE, -- fecha de consulta, el dia primero del mes siguiente
       v_fecha_texto    VARCHAR(10) -- fecha calculada

       -- se calcula la fecha de consulta, que sera el dia primero del mes siguiente al elegido
       LET v_ano_aux = anio
       LET v_mes_aux = mes

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


       LET v_i_cont_registros  = 1  --SE INICIALIZA EL CONMTADOR DE REGISTROS PARA CTA_HIS_PAGOS
    
       -- Busqueda de existencia de registros con los criterios recibidos
       -- Se utilizan los codigos de proceso definidos en el requerimiento   
       LET v_consulta_folio = "\n SELECT folio ",
                              "\n FROM glo_folio",
                              "\n WHERE month(f_actualiza) = ? " ,
                              "\n AND year(f_actualiza) = ? ",
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
                  "\n FROM cta_his_pagos cta LEFT OUTER JOIN pag_tpo_aclaracion tpa ON                          ",
                  "\n cta.tpo_aclaracion = tpa.aclaracion_cod                                        ",
                  "\n WHERE cta.folio IN (SELECT folio FROM glo_folio WHERE month(f_actualiza) = ?   ",
                  #03/08/2016 se agrega la condici�n de que el destino_ap_viv sea 1 o 2 o 3
                  #debido a que no cuadraba el reporte generado por este programa y la suma de registros de los LQINFO
                  "\n AND year(f_actualiza) = ? AND proceso_cod = 1401)  AND cta.destino_ap_viv in (1,2,3)  ",
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

--FUNCTION fn_reporte_carga_archivo(v_arr_causales ,total_apam, p_b_despliegue_pantalla)
FUNCTION fn_reporte_carga_archivo(p_b_despliegue_pantalla, v_mes, v_anio)
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE manejador_rpt         om.SaxDocumentHandler,
           v_i_inicio_for    INTEGER
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE v_mes                CHAR(2)-- Mes de busqueda
    DEFINE v_anio               CHAR(4)-- A�o de busqueda
    DEFINE v_mes_desc           CHAR(12)
    
    LET v_i_inicio_for = 1
 
    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("ACLC14.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","ACLC14"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   

    -- se asigna descripci�n al mes
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
            START REPORT rpt_causales_aclaracion TO XML HANDLER manejador_rpt
            -- Asigna el titulo del reporte
            FOR v_i_inicio_for = 1 TO v_i_cont_registros
               OUTPUT TO REPORT rpt_causales_aclaracion(v_arr_causales[v_i_inicio_for].*, v_mes_desc, v_anio)
            END FOR  
            FINISH REPORT rpt_causales_aclaracion 
END FUNCTION

REPORT rpt_causales_aclaracion(v_arr_causales, p_mes_desc, p_anio)
  DEFINE  v_arr_causales    RECORD -- Arreglo dinamico para las cuenstas causales encontradas
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
          END RECORD,
          v_fecha_reporte  DATE ,
          v_nombre_usuario           VARCHAR(100),
          p_mes_desc       CHAR(12),    -- Mes de corte
          p_anio           CHAR(4)      -- A�o de corte          
                                                                                                                                                                                          
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
