###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#Modulo            =>                                                         #
#Programa          =>                                                         #
#Objetivo          => CONSULTA DE PRELIQUIDACION DE AGR                       #
#Fecha Inicio      => 11 Abril 2012                                           #
###############################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "AGRG01.4gl"

GLOBALS
DEFINE g_pid              LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_arr_arbol        DYNAMIC ARRAY OF RECORD
          subcuenta_desc     CHAR(30)     ,
          siefore            SMALLINT     ,
          monto_pesos        DECIMAL(28,6),
          monto_acciones     DECIMAL(28,6),
          subcuenta          SMALLINT     ,
          padre_id           STRING       ,
          id                 STRING       ,
          nivel              SMALLINT
       END RECORD,          
       arr_folios            DYNAMIC ARRAY OF RECORD
          folio              DECIMAL(9,0),
          fecha_liquidacion  DATE,
          fecha_proceso      DATE
       END RECORD,  
       g_consulta         RECORD
          folio_liquida      DECIMAL(10,0),
          f_liquida          DATE,
          f_registro         DATE
       END RECORD
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recuperan los valores que vienen como parámetro
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRC02.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- Se inicializan variables
   LET g_proceso_cod = g_proc_cod_agr_liquidacion -- liquidación anualidad garantizada
   LET g_opera_cod   = 1 -- operacion 2
   LET g_pid = "00000"

   -- se invoca la funcion general de consulta de preliquidacion
   -- es necesario cargar en cat_preliquida el proceso y el nombre de la tabla
   -- que contiene los datos de la preliquidacion
   CALL fn_consulta_preliquidacion()
END MAIN

FUNCTION fn_consulta_preliquidacion()
   DEFINE w               ui.Window
   DEFINE f               ui.Form            
   DEFINE v_condicion     STRING
   DEFINE ls_SqlQry       STRING
   DEFINE cb              ui.ComboBox # Variable de Combobox
   DEFINE folio_liquida   DECIMAL(9,0)

   CALL g_arr_arbol.clear()
   CALL arr_folios.clear()

   OPEN WINDOW vent_folios WITH FORM "AGRC021"

   LET cb = ui.ComboBox.forName("formonly.folio_liquida") #Asignación del combo a la forma	 
   CALL cb.clear()#se limpia el combo    
   
   LET w = ui.Window.getCurrent() -- asigna a "w" la ventana activa
   LET f = w.getForm()

   CALL f.setElementHidden("group2",1)
   CALL f.setElementHidden("group3",1)
{
   CONSTRUCT BY NAME v_condicion ON folio_liquida, f_registro, f_liquida                 
      BEFORE CONSTRUCT
         CALL fn_despliega_desc(g_proceso_cod, g_opera_cod)

      AFTER FIELD f_liquida
         NEXT FIELD folio_liquida
            
      ON ACTION ACCEPT
         LET g_consulta.f_liquida     = GET_FLDBUF(f_liquida)
         LET g_consulta.f_registro    = GET_FLDBUF(f_registro)
         LET g_consulta.folio_liquida = GET_FLDBUF(folio_liquida)
            
         IF g_consulta.f_liquida IS NULL AND
            g_consulta.f_registro IS NULL AND
            g_consulta.folio_liquida IS NULL THEN

            CALL fn_mensaje("Consulta", "Debe de ingresar un campo de búsqueda", "about")
         ELSE
            ACCEPT CONSTRUCT
         END IF
            
      ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT CONSTRUCT
   END CONSTRUCT
}
   INPUT BY NAME g_consulta.folio_liquida, 
                 g_consulta.f_registro,
                 g_consulta.f_liquida
   WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
      BEFORE INPUT
      #se asigna la variable para la consulta de folios asociados  
      LET ls_SqlQry = 
      "SELECT folio \n",
      "  FROM glo_folio \n",
      " WHERE proceso_cod = ",g_proceso_cod,"\n",
      "   AND status IN (1,2) \n",
      " ORDER BY 1 DESC"    
                    
      -- las etiquetas se cambian para referir a la preliquidacion
      CALL f.setElementText("group3","Montos preliquidados")
      CALL f.setElementText("formonly.fecha_liquida_tabla","Fecha\npreliquidación")
      CALL f.setElementText("lb34","Fecha preliquidación")
              
      
      #se prepara el STATEMENT 
      PREPARE con_folios FROM ls_SqlQry      
      #se declara el cursor
      DECLARE cur_folios CURSOR FOR con_folios                  
      ##se ejecuta la sentencia SQL que busca los folos asosiados mediante un ciclo
      FOREACH cur_folios INTO folio_liquida      	
      	#se asignan los folios al combo
      	CALL cb.addItem(folio_liquida ,folio_liquida)       	    
      END FOREACH  
      FREE cur_folios
      
      CALL fn_despliega_desc(g_proceso_cod, g_opera_cod)

      -- se inician las variables de captura
      LET g_consulta.f_liquida     = NULL
      LET g_consulta.f_registro    = NULL
      LET g_consulta.folio_liquida = NULL

      AFTER FIELD f_liquida
         CALL FGL_DIALOG_GETBUFFER( ) RETURNING g_consulta.f_liquida
         NEXT FIELD folio_liquida
              
      ON ACTION ACCEPT
{
         DISPLAY "Valores capturados"
         DISPLAY g_consulta.f_liquida     
         DISPLAY g_consulta.f_registro    
         DISPLAY g_consulta.folio_liquida 
         DISPLAY v_condicion
 }           
         IF ( ( g_consulta.f_liquida IS NULL AND
              g_consulta.f_registro IS NULL ) AND
              g_consulta.folio_liquida IS NULL ) THEN
            CALL fn_mensaje("Consulta",
                            "Debe de ingresar un campo de búsqueda",
                            "about")
            
         ELSE
            -- se construye la cadena de condicion
            LET v_condicion = "1=1\n"
            IF ( g_consulta.f_liquida IS NOT NULL ) THEN
               LET v_condicion = v_condicion || "AND f_liquida = '" || g_consulta.f_liquida || "'\n"
            END IF

            IF ( g_consulta.f_registro IS NOT NULL ) THEN
               LET v_condicion = v_condicion || "AND f_registro = '" || g_consulta.f_registro || "'\n"
            END IF

            IF ( g_consulta.folio_liquida IS NOT NULL ) THEN
               LET v_condicion = v_condicion || "AND folio_liquida = '" || g_consulta.folio_liquida || "'\n"
            END IF
            EXIT INPUT
         END IF
            
      ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT INPUT
   END INPUT
   
   IF NOT INT_FLAG THEN       
      IF fn_valida_folio_preliq(v_condicion) THEN
         CALL fn_folios_consulta_preliq()
      ELSE  
         CALL fn_mensaje("Consulta", "No existen folios con los criterios dados.", "about")
      END IF
   END IF                                              

   CLOSE WINDOW vent_folios
END FUNCTION

FUNCTION fn_despliega_desc_preliq()
   DEFINE v_proceso_desc      CHAR(40)
   DEFINE v_opera_desc        CHAR(40)
            
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = g_proceso_cod
            
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod   = g_opera_cod
            
   DISPLAY BY NAME v_proceso_desc
   DISPLAY BY NAME v_opera_desc
END FUNCTION

FUNCTION fn_valida_folio_preliq(v_condicion)
   DEFINE v_condicion        STRING
   DEFINE v_registros        SMALLINT
   DEFINE v_query            STRING
            
   CREATE TEMP TABLE tmp_folios_consulta (
               folio         DECIMAL (9,0),
               fecha_liq     DATE,
               fecha_proceso DATE)
            
   LET v_query = "INSERT INTO tmp_folios_consulta ",
                 "SELECT folio_liquida, f_liquida, f_registro ",
                 "  FROM cre_ag_preliquida",
                 " WHERE ",v_condicion.trim(),
                 " GROUP BY 1,2,3"
                 
   DISPLAY "v_query: ",v_query
   PREPARE prp_folio_valida FROM v_query
   EXECUTE prp_folio_valida
            
   SELECT COUNT(*)
     INTO v_registros
     FROM tmp_folios_consulta a, glo_folio b
    WHERE a.folio = b.folio
      AND b.proceso_cod = g_proceso_cod
            
   RETURN v_registros
END FUNCTION

FUNCTION fn_folios_consulta_preliq()
   DEFINE v_folio            DECIMAL(9,0)
   DEFINE i                  SMALLINT
   DEFINE w                  ui.Window
   DEFINE f                  ui.Form 
   DEFINE v_s_comando        STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt         STRING -- guarda una sentencia SQL a ejecutar 
   DEFINE v_c_ruta_bin       LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE v_c_ruta_rescate   LIKE seg_modulo.ruta_rescate -- ruta rescate del módulo
   DEFINE v_c_ruta_listados  LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE f_w                ui.form   
   DEFINE v_existe_archivo   INTEGER
   DEFINE v_c_programa_cod   LIKE cat_operacion.programa_cod
   DEFINE v_nom_reporte      VARCHAR(80) -- nombre del reporte   
    
   DECLARE cur_folio CURSOR FOR SELECT a.*
                                  FROM tmp_folios_consulta a, 
                                       glo_folio b
                                 WHERE a.folio = b.folio
                                   AND b.proceso_cod = g_proceso_cod

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "AGRC02"                                   
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   LET i = 1
   FOREACH cur_folio INTO arr_folios[i].*
      LET i = i + 1
   END FOREACH
   
   -- se obtienen las rutas de control del modulo
   LET v_s_qryTxt = " SELECT ruta_bin, ruta_rescate, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_rutasModulo FROM v_s_qryTxt
   EXECUTE prp_slc_rutasModulo INTO v_c_ruta_bin, v_c_ruta_rescate, v_c_ruta_listados
   LET v_nom_reporte = g_usuario CLIPPED || "-",v_c_programa_cod CLIPPED,"-",g_pid USING "&&&&&","-",g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"
   LET w = ui.Window.getCurrent() -- asigna a "w" la ventana activa
   LET f = w.getForm()
   LET f_w = w.getForm()    
   
   CALL f.setElementHidden("group2",0)
   CALL f.setElementHidden("group3",0)
            
   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY arr_folios TO arr_folios.* 
         BEFORE ROW
            LET i = ARR_CURR()
            LET v_folio = arr_folios[i].folio
            CALL fn_llena_arbol_montos_agr(v_folio)
            CALL ui.Interface.refresh()
      END DISPLAY
            
      DISPLAY ARRAY g_arr_arbol TO scr1.*
            
      END DISPLAY
            
      ON ACTION ACCEPT
         EXIT DIALOG
            
      ON ACTION cancelar
         EXIT DIALOG
            
      ON ACTION reporte
         -- se crea el comando que ejecuta el modulo que reliza la integracion del archivo
         LET v_s_comando = "fglrun ",v_c_ruta_bin CLIPPED,"/AGRP16 ",
                                     g_usuario, " ",                                                 
                                     g_pid, " ",
                                     g_proceso_cod, " ",
                                     g_opera_cod, " ",
                                     v_folio, " ",
                                     "cre_ag_preliquida ",
                                     v_c_programa_cod, " "                                                 
                                     --"/nohup:",g_proceso_cod USING "&&&&&",":",
                                     --g_opera_cod USING "&&&&&",
                                     --" 2>&1 &"
         --DISPLAY " v_s_comando ", v_s_comando
         RUN v_s_comando

        LET v_existe_archivo = 1

        IF(LENGTH(v_c_ruta_listados) > 0)THEN
           # se revisa si existe el archivo en la ruta de listados
           CALL os.Path.exists(v_c_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
        END IF

        # si no existe el archivo, se oculta la imagen link que visualiza el pdf
        IF NOT(v_existe_archivo)THEN
           CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
        ELSE
           CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
        END IF

                              
        # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
        DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
        CALL ui.Interface.refresh()    
         CALL fn_mensaje("Consulta", "Se genero el reporte de preliquidación", "information")
   END DIALOG
END FUNCTION

FUNCTION fn_llena_arbol_montos_agr(p_folio)
    DEFINE p_folio        INTEGER
    DEFINE qry_string     STRING
    DEFINE i              INTEGER
    DEFINE j              INTEGER
    DEFINE k              INTEGER
    DEFINE cont_arbol     INTEGER
            
    DEFINE arr_nivel1 DYNAMIC ARRAY OF RECORD
        grupo_regimen           SMALLINT,
        desc_larga              CHAR(40),
        monto_pesos          DECIMAL(28,6),
        monto_acciones       DECIMAL(28,6)
    END RECORD
            
    DEFINE arr_nivel2 DYNAMIC ARRAY OF RECORD
        subcuenta               SMALLINT,
        descripcion             CHAR(40),
        siefore                 SMALLINT,
        pesos                   DECIMAL(28,6),
        acciones                DECIMAL(28,6)
    END RECORD
            
    DEFINE arr_nivel3 DYNAMIC ARRAY OF RECORD
        movimiento         SMALLINT,
        movimiento_desc             CHAR(40),
        pesos                   DECIMAL(28,6),
        acciones                DECIMAL(28,6)
    END RECORD
            
    LET qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga, ",
                            " sum(dp.monto_pesos), ",
                            " sum(dp.monto_acciones)",
                       " FROM cre_ag_preliquida dp, ",
                            " cat_grp_subcta_regimen tasr, ",
                            " cat_grupo_regimen tgr ",
                      " WHERE dp.folio_liquida = ? ",
                        " AND tasr.subcuenta = dp.subcuenta ",
                        " AND tgr.grupo_regimen = tasr.grupo_regimen ",
                      " GROUP BY 1,2 ",
                      " ORDER BY 1 "
    PREPARE prp_nivel1 FROM qry_string
    DECLARE cur_nivel1 CURSOR FOR prp_nivel1
            
    LET qry_string = " SELECT ts.subcuenta, ts.subcuenta||' '||ts.subcuenta_desc, ",
                            " dp.fondo_inversion, sum(dp.monto_pesos), ",
                            " sum(dp.monto_acciones) ",
                       " FROM cre_ag_preliquida dp, ",
                            " cat_grp_subcta_regimen tasr, ",
                            " cat_subcuenta ts ",
                      " WHERE dp.folio_liquida = ? ",
                        " AND tasr.subcuenta = dp.subcuenta ",
                        " AND tasr.grupo_regimen = ? ", 
                        " AND ts.subcuenta = tasr.subcuenta ",
                      " GROUP BY 1,2,3 ",
                      " ORDER BY 1,3"
    PREPARE prp_nivel2 FROM qry_string
    DECLARE cur_nivel2 CURSOR FOR prp_nivel2
            
    LET qry_string = " SELECT tm.movimiento, tm.movimiento||' '||tm.movimiento_desc, ",
                            " sum(dp.monto_pesos), ",
                            " sum(dp.monto_acciones) ",
                       " FROM cre_ag_preliquida dp, ",
                            " cat_movimiento tm ",
                      " WHERE dp.folio_liquida = ? " ,
                        " AND dp.subcuenta = ?",
                        " AND dp.fondo_inversion = ? ",
                        " AND tm.movimiento = dp.movimiento ",
                      " GROUP BY 1,2 ",
                      " ORDER BY 1 "
    PREPARE prp_nivel3 FROM qry_string
    DECLARE cur_nivel3 CURSOR FOR prp_nivel3
            
    LET i          = 1
    LET j          = 1
    LET k          = 1
    LET cont_arbol = 1
            
    CALL g_arr_arbol.clear()
            
    FOREACH cur_nivel1 USING p_folio
                        INTO arr_nivel1[i].*
        LET g_arr_arbol[cont_arbol].subcuenta      = arr_nivel1[i].grupo_regimen
        LET g_arr_arbol[cont_arbol].subcuenta_desc = arr_nivel1[i].desc_larga
        LET g_arr_arbol[cont_arbol].monto_pesos    = arr_nivel1[i].monto_pesos
        LET g_arr_arbol[cont_arbol].monto_acciones = arr_nivel1[i].monto_acciones
        LET g_arr_arbol[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING "<<"
        LET g_arr_arbol[cont_arbol].nivel          = 1
        LET g_arr_arbol[cont_arbol].padre_id       = ""
        LET g_arr_arbol[cont_arbol].siefore        = ""
        LET cont_arbol = cont_arbol + 1
            
        FOREACH cur_nivel2 USING p_folio,
                                 arr_nivel1[i].grupo_regimen
                            INTO arr_nivel2[j].*
            
            LET g_arr_arbol[cont_arbol].subcuenta      = arr_nivel2[j].subcuenta
            LET g_arr_arbol[cont_arbol].subcuenta_desc = arr_nivel2[j].descripcion
            LET g_arr_arbol[cont_arbol].monto_pesos    = arr_nivel2[j].pesos
            LET g_arr_arbol[cont_arbol].monto_acciones = arr_nivel2[j].acciones
            LET g_arr_arbol[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING"<<",".",
                                                       arr_nivel2[j].subcuenta USING"<<"
            LET g_arr_arbol[cont_arbol].nivel          = 2
            LET g_arr_arbol[cont_arbol].padre_id       = arr_nivel1[i].grupo_regimen USING"<<"
            LET g_arr_arbol[cont_arbol].siefore        = arr_nivel2[j].siefore
            LET cont_arbol = cont_arbol + 1
            
            
            FOREACH cur_nivel3 USING p_folio,
                                     arr_nivel2[j].subcuenta,
                                     arr_nivel2[j].siefore
                                INTO arr_nivel3[k].*
                LET g_arr_arbol[cont_arbol].subcuenta      = arr_nivel3[k].movimiento
                LET g_arr_arbol[cont_arbol].subcuenta_desc = arr_nivel3[k].movimiento_desc
                LET g_arr_arbol[cont_arbol].monto_pesos    = arr_nivel3[k].pesos
                LET g_arr_arbol[cont_arbol].monto_acciones = arr_nivel3[k].acciones
                LET g_arr_arbol[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING "<<",".",
                                                           arr_nivel2[j].subcuenta USING "<<",".",
                                                           arr_nivel3[k].movimiento USING" <<"
                LET g_arr_arbol[cont_arbol].nivel          = 3
                LET g_arr_arbol[cont_arbol].padre_id       = arr_nivel1[i].grupo_regimen USING "<<",".",
                                                           arr_nivel2[j].subcuenta USING "<<"
                LET g_arr_arbol[cont_arbol].siefore        = arr_nivel2[j].siefore
                LET cont_arbol = cont_arbol + 1
            
                LET k = k + 1
            END FOREACH
            CLOSE cur_nivel3
            LET j = j + 1
        END FOREACH
        CLOSE cur_nivel2
        LET i = i + 1
    END FOREACH
    CLOSE cur_nivel1
END FUNCTION