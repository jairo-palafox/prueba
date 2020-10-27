--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => GRT                                                     #
#Programa          => GRTC04                                                  #
#Objetivo          => CONSULTA DE LIQUIDACION DE ACR                          #
#Autor             => Mauricio Sanchez, EFP                                   #
#Fecha Inicio      => 2/Mayo/2012                                             #
###############################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "GRTG01.4gl"

GLOBALS

   DEFINE g_pid              LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE g_proceso_cod      LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod        LIKE cat_operacion.opera_cod -- codigo de operacion

   DEFINE arr_arbol DYNAMIC ARRAY OF RECORD
      subcuenta_desc         CHAR(30)     ,
      siefore                SMALLINT     ,
      monto_pesos            DECIMAL(28,6),
      monto_acciones         DECIMAL(28,6),
      subcuenta              SMALLINT     ,
      padre_id               STRING       ,
      id                     STRING       ,
      nivel                  SMALLINT
   END RECORD

   DEFINE arr_folios DYNAMIC ARRAY OF RECORD
      folio                  DECIMAL(9,0),
      fecha_liquidacion      DATE,
      fecha_proceso          DATE
   END RECORD

   DEFINE g_consulta RECORD
      folio_liquida          DECIMAL(10,0),
      f_liquida              DATE,
      f_registro             DATE
   END RECORD

END GLOBALS

MAIN

DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_pid = "00000"

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTC04.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET g_proceso_cod = g_proc_cod_grt_uso_liquida -- liquidacion de uso garantia
   LET g_opera_cod   = 2 -- liquidacion de uso garantia

   -- se invoca la funcion general de consulta de liquidacion
   CALL fn_consulta_folios_liquida_grt(p_usuario_cod, g_proceso_cod, g_opera_cod)

END MAIN

FUNCTION fn_consulta_folios_liquida_grt(p_usuario, p_proceso_cod, p_opera_cod)

   DEFINE p_usuario             CHAR(20)
   DEFINE p_proceso_cod         SMALLINT
   DEFINE p_opera_cod           SMALLINT
   DEFINE v_tabla               CHAR(30)
   DEFINE w                     ui.Window
   DEFINE f                     ui.Form
   DEFINE v_condicion           STRING
   DEFINE ls_SqlQry             STRING
   DEFINE cb                    ui.ComboBox # Variable de Combobox
   DEFINE folio_liquida         DECIMAL(9,0)
   DEFINE v_criterio            SMALLINT
   DEFINE v_s_qryTxt            STRING

   CALL arr_arbol.clear()
   CALL arr_folios.clear()

   OPEN WINDOW vent_folios WITH FORM "GRTC041"

   LET cb = ui.ComboBox.forName("formonly.folio_liquida") #Asignación del combo a la forma   
   CALL cb.clear()#se limpia el combo 

   LET w = ui.Window.getCurrent() -- asigna a "w" la ventana activa
   LET f = w.getForm()

   CALL f.setElementHidden("group2",1)
   CALL f.setElementHidden("group3",1)

   INPUT BY NAME g_consulta.folio_liquida,
                 g_consulta.f_liquida

   WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )

      BEFORE INPUT
      #se asigna la variable para la consulta de folios asociados
      LET ls_SqlQry = 
      "SELECT folio \n",
      "  FROM glo_folio \n",
      " WHERE proceso_cod = ",g_proceso_cod,"\n",
      "   AND status = 2 \n",
      " ORDER BY 1 DESC"

      -- las etiquetas se cambian para referir a liquidacion
      CALL f.setElementText("group3","Montos liquidados")
      CALL f.setElementText("formonly.fecha_liquida_tabla","Fecha\nliquidación")
      CALL f.setElementText("lb34","Fecha liquidación")

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

      CALL fn_despliega_desc(p_proceso_cod, p_opera_cod)

      -- se inician las variables de captura
      LET g_consulta.f_liquida     = NULL
      LET g_consulta.f_registro    = NULL
      LET g_consulta.folio_liquida = NULL

      AFTER FIELD f_liquida
         CALL FGL_DIALOG_GETBUFFER( ) RETURNING g_consulta.f_liquida
         NEXT FIELD folio_liquida

      ON ACTION ACCEPT

         IF ( ( g_consulta.f_liquida IS NULL AND
              g_consulta.f_registro IS NULL ) AND
              g_consulta.folio_liquida IS NULL ) THEN
            CALL fn_mensaje("Consulta",
                            "Debe de ingresar un campo de búsqueda",
                            "about")

         ELSE
            -- se construye la cadena de condicion
            LET v_condicion = "1=1\n"

            IF ( g_consulta.folio_liquida IS NOT NULL ) THEN
               LET v_condicion = v_condicion || "AND folio_liquida = '" || g_consulta.folio_liquida || "'\n"
               LET v_criterio = 0
            ELSE
               LET v_condicion = v_condicion || "AND f_liquida = '" || g_consulta.f_liquida || "'\n"
            END IF

            LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

            PREPARE prp_obt_mov FROM v_s_qryTxt
            EXECUTE prp_obt_mov USING v_criterio,
                                      g_consulta.folio_liquida,
                                      g_consulta.f_liquida
                                 INTO v_tabla

            EXIT INPUT
         END IF
 
      ON ACTION cancel
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF NOT INT_FLAG THEN
      IF fn_valida_folio_liquida_grt(v_condicion,v_tabla, p_proceso_cod, p_opera_cod) THEN
         CALL fn_folios_consulta_liquida_grt(v_tabla, p_proceso_cod, p_opera_cod, p_usuario)
      ELSE  
         CALL fn_mensaje("Consulta",
                         "No existen folios con los criterios dados.",
                         "about")
      END IF
   END IF

   CLOSE WINDOW vent_folios
END FUNCTION

FUNCTION fn_despliega_desc_liquida_grt(p_proceso_cod, p_opera_cod)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_proceso_desc      CHAR(40)
   DEFINE v_opera_desc        CHAR(40)

   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   DISPLAY BY NAME v_proceso_desc
   DISPLAY BY NAME v_opera_desc
END FUNCTION

FUNCTION fn_valida_folio_liquida_grt(v_condicion, v_tabla, p_proceso_cod, p_opera_cod)
   DEFINE v_condicion        STRING
   DEFINE v_tabla            STRING
   DEFINE p_proceso_cod      SMALLINT
   DEFINE p_opera_cod        SMALLINT
   DEFINE v_registros        SMALLINT
   DEFINE v_query            STRING

   CREATE TEMP TABLE tmp_folios_consulta (
               folio         DECIMAL (9,0),
               fecha_liq     DATE,
               fecha_proceso DATE)

   LET v_query = "INSERT INTO tmp_folios_consulta ",
                 "SELECT folio_liquida, f_liquida, f_registro ",
                 "  FROM ",v_tabla.trim(),
                 " WHERE ",v_condicion.trim(),
                 " GROUP BY 1,2,3"

   DISPLAY "v_query: ",v_query
   PREPARE prp_folio_valida FROM v_query
   EXECUTE prp_folio_valida

   SELECT COUNT(*)
     INTO v_registros
     FROM tmp_folios_consulta a, glo_folio b
    WHERE a.folio = b.folio
      AND b.proceso_cod = p_proceso_cod
 
   RETURN v_registros

END FUNCTION

FUNCTION fn_folios_consulta_liquida_grt(v_tabla, p_proceso_cod, p_opera_cod, p_usuario_cod)
   DEFINE v_tabla            STRING
   DEFINE p_proceso_cod      SMALLINT
   DEFINE p_opera_cod        SMALLINT
   DEFINE p_usuario_cod      CHAR(20)
   DEFINE v_folio            DECIMAL(9,0)
   DEFINE i                  SMALLINT
   DEFINE w                  ui.Window
   DEFINE f                  ui.Form 
   DEFINE v_s_comando        STRING -- contiene al comando a correr
   DEFINE v_c_programa_cod   LIKE cat_operacion.programa_cod -- nomnre del programa
   DEFINE r_c_ruta_bin       LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE r_c_ruta_listados  LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE f_w                ui.form   
   DEFINE v_existe_archivo   INTEGER      
   DEFINE v_nom_reporte      VARCHAR(80) -- nombre del reporte
    
   DECLARE cur_folio CURSOR FOR SELECT a.*
                                  FROM tmp_folios_consulta a, 
                                       glo_folio b
                                 WHERE a.folio = b.folio
                                   AND b.proceso_cod = p_proceso_cod

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "GRTC04"
   LET v_c_programa_cod = fn_obten_nom_programa(p_proceso_cod , p_opera_cod)

   LET i = 1
   FOREACH cur_folio INTO arr_folios[i].*
      LET i = i + 1
   END FOREACH
   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   LET w = ui.Window.getCurrent() -- asigna a "w" la ventana activa
   LET f = w.getForm()
   LET f_w = w.getForm() 

   CALL f.setElementHidden("group2",0)
   CALL f.setElementHidden("group3",0)

   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY arr_folios TO arr_folios.* 
         BEFORE ROW
            LET i       = ARR_CURR()
            LET v_folio = arr_folios[i].folio
            CALL fn_llena_arbol_montos_grt(v_folio, v_tabla)
            CALL ui.Interface.refresh()
      END DISPLAY

      DISPLAY ARRAY arr_arbol TO scr1.*

      END DISPLAY

      ON ACTION ACCEPT
         EXIT DIALOG

      ON ACTION cancelar
         EXIT DIALOG

      ON ACTION reporte
         LET v_nom_reporte = p_usuario_cod CLIPPED || "-",v_c_programa_cod CLIPPED,"-",g_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&", "-", p_opera_cod USING "&&&&&"        
         --CALL fn_reporte_liquidacion(v_folio, v_tabla, p_usuario_cod, TRUE)
         -- se crea el comando que ejecuta el modulo que reliza la integracion del archivo
         LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/GRTP19 ",
                                                 p_usuario_cod, " ",
                                                 p_proceso_cod, " ",
                                                 p_opera_cod, " ",
                                                 v_folio, " ",
                                                 v_tabla, " ", 
                                                 g_pid, " ",
                                                 v_c_programa_cod, " "
                                                 --"/nohup:",p_proceso_cod USING "&&&&&",":",
                                                 --p_opera_cod USING "&&&&&",
                                                 --" 2>&1 &"
         DISPLAY " v_s_comando ", v_s_comando
         RUN v_s_comando
         LET v_existe_archivo = 1

         IF(LENGTH(r_c_ruta_listados) > 0)THEN
            # se revisa si existe el archivo en la ruta de listados
            CALL os.Path.exists(r_c_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
         END IF

         # si no existe el archivo, se oculta la imagen link que visualiza el pdf
         IF NOT(v_existe_archivo)THEN
            CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
         ELSE
            CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
         END IF

         # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
         DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','grt')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
         CALL ui.Interface.refresh()
         CALL fn_mensaje("Consulta",
                         "Se genero el reporte de liquidación",
                         "info")
   END DIALOG
END FUNCTION

FUNCTION fn_llena_arbol_montos_grt(p_folio, v_tabla)
    DEFINE p_folio        INTEGER
    DEFINE v_tabla        STRING
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
                       " FROM ",v_tabla.trim()," dp, ",
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
                       " FROM ",v_tabla.trim()," dp, ",
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
                       " FROM ",v_tabla.trim()," dp, ",
                            " cat_movimiento tm ",
                      " WHERE dp.folio_liquida = ? " ,
                        " AND dp.subcuenta = ?",
                        " AND dp.movimiento = tm.movimiento ",
                        " AND dp.fondo_inversion = ? ",
                      " GROUP BY 1,2 ",
                      " ORDER BY 1 "
    PREPARE prp_nivel3 FROM qry_string
    DECLARE cur_nivel3 CURSOR FOR prp_nivel3

    LET i          = 1
    LET j          = 1
    LET k          = 1
    LET cont_arbol = 1

    CALL arr_arbol.clear()

    FOREACH cur_nivel1 USING p_folio
                        INTO arr_nivel1[i].*
        LET arr_arbol[cont_arbol].subcuenta      = arr_nivel1[i].grupo_regimen
        LET arr_arbol[cont_arbol].subcuenta_desc = arr_nivel1[i].desc_larga
        LET arr_arbol[cont_arbol].monto_pesos    = arr_nivel1[i].monto_pesos
        LET arr_arbol[cont_arbol].monto_acciones = arr_nivel1[i].monto_acciones
        LET arr_arbol[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING "<<"
        LET arr_arbol[cont_arbol].nivel          = 1
        LET arr_arbol[cont_arbol].padre_id       = ""
        LET arr_arbol[cont_arbol].siefore        = ""
        LET cont_arbol = cont_arbol + 1

        FOREACH cur_nivel2 USING p_folio,
                                 arr_nivel1[i].grupo_regimen
                            INTO arr_nivel2[j].*

            LET arr_arbol[cont_arbol].subcuenta      = arr_nivel2[j].subcuenta
            LET arr_arbol[cont_arbol].subcuenta_desc = arr_nivel2[j].descripcion
            LET arr_arbol[cont_arbol].monto_pesos    = arr_nivel2[j].pesos
            LET arr_arbol[cont_arbol].monto_acciones = arr_nivel2[j].acciones
            LET arr_arbol[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING"<<",".",
                                                       arr_nivel2[j].subcuenta USING"<<"
            LET arr_arbol[cont_arbol].nivel          = 2
            LET arr_arbol[cont_arbol].padre_id       = arr_nivel1[i].grupo_regimen USING"<<"
            LET arr_arbol[cont_arbol].siefore        = arr_nivel2[j].siefore
            LET cont_arbol = cont_arbol + 1

            FOREACH cur_nivel3 USING p_folio,
                                     arr_nivel2[j].subcuenta,
                                     arr_nivel2[j].siefore
                                INTO arr_nivel3[k].*
                LET arr_arbol[cont_arbol].subcuenta      = arr_nivel3[k].movimiento
                LET arr_arbol[cont_arbol].subcuenta_desc = arr_nivel3[k].movimiento_desc
                LET arr_arbol[cont_arbol].monto_pesos    = arr_nivel3[k].pesos
                LET arr_arbol[cont_arbol].monto_acciones = arr_nivel3[k].acciones
                LET arr_arbol[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING "<<",".",
                                                           arr_nivel2[j].subcuenta USING "<<",".",
                                                           arr_nivel3[k].movimiento USING" <<"
                LET arr_arbol[cont_arbol].nivel          = 3
                LET arr_arbol[cont_arbol].padre_id       = arr_nivel1[i].grupo_regimen USING "<<",".",
                                                           arr_nivel2[j].subcuenta USING "<<"
                LET arr_arbol[cont_arbol].siefore        = arr_nivel2[j].siefore
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