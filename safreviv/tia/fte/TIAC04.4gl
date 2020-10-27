--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--=============================================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => TIA                                                     #
#Programa          => TIAC04                                                  #
#Objetivo          => CONSULTA DE LIQUIDACION  DECRETO   Trsapasos     I-A    #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv
GLOBALS "TIAG01.4gl" --archivo de variables globales proceso_cod
GLOBALS
    DEFINE arr_arbol          DYNAMIC ARRAY OF RECORD
          subcuenta_desc     CHAR(30)     ,
          siefore            SMALLINT     ,
          monto_pesos        DECIMAL(28,6),
          monto_acciones     DECIMAL(28,6),
          subcuenta          SMALLINT     ,
          padre_id           STRING       ,
          id                 STRING       ,
          nivel              SMALLINT
END RECORD
    
    DEFINE arr_arbol          DYNAMIC ARRAY OF RECORD
          subcuenta_desc     CHAR(30)     ,
          siefore            SMALLINT     ,
          monto_pesos        DECIMAL(28,6),
          monto_acciones     DECIMAL(28,6),
          subcuenta          SMALLINT     ,
          padre_id           STRING       ,
          id                 STRING       ,
          nivel              SMALLINT
END RECORD
DEFINE arr_folios         DYNAMIC ARRAY OF RECORD
          folio              DECIMAL(9,0),
          fecha_liquidacion  DATE,
          fecha_proceso      DATE
END RECORD  

DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
DEFINE w               ui.Window
DEFINE f               ui.Form
DEFINE g_consulta         RECORD
          folio_liquida      DECIMAL(10,0),
          f_liquida          DATE,
          f_registro         DATE
END RECORD

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --LET g_proceso_cod = 1701
   --LET g_opera_cod   = 4  --3

    CALL fn_consulta(p_usuario_cod, g_proceso_cod_tia, g_opera_cod_tia_liquidacion,'cta_decreto')
    --CALL fn_consulta_liquida_decreto(p_usuario_cod, g_proceso_cod_tia, g_opera_cod_tia_liquidacion,'cta_decreto')

   -- se invoca la funcion general de consulta de liquidacion
   --CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod,1)
END MAIN
{
FUNCTION fn_consulta_liquida_decreto(p_usuario, p_proceso_cod, p_opera_cod, v_tabla)
   DEFINE p_usuario           CHAR(20)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_tabla             CHAR(30)
   DEFINE v_ruta_bin          CHAR(40)            
   DEFINE v_condicion         STRING
   --DEFINE v_v_nombre_combo    VARCHAR(20) -- nombre del combobox
   DEFINE cb                  ui.ComboBox # Variable de Combobox
   DEFINE folio_liquida       DECIMAL(9,0)
   DEFINE ls_SqlQry           STRING 
               
   CALL arr_arbol.clear()
   CALL arr_folios.clear()
            
   SELECT ruta_bin
     INTO v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'glo'
                     
   OPEN WINDOW w_liquia_decreto WITH FORM  "TIAC07"
   LET cb = ui.ComboBox.forName("formonly.folio_liquida") #Asignación del combo a la forma	 
   CALL cb.clear()#se limpia el combo 
   
   LET w = ui.Window.getCurrent() -- asigna a "w" la ventana activa
   LET f = w.getForm()
            
   CALL f.setElementHidden("group2",1)
   CALL f.setElementHidden("group3",1)
      
   INPUT BY NAME g_consulta.folio_liquida, 
                 g_consulta.f_registro,
                 g_consulta.f_liquida
   WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
      BEFORE INPUT
      #se asigna la variable para la consulta de folios asosiados  
      LET ls_SqlQry = 
      "SELECT folio \n",
      "  FROM glo_folio \n",
      " WHERE proceso_cod = ",p_proceso_cod,"\n",
      "   AND status = 2"

      -- =========================================================
      --                   L I Q U I D A C I O N
      -- =========================================================
      -- las etiquetas se cambian para referir a liquidacion
      CALL f.setElementText("group3","Montos liquidados")
      CALL f.setElementText("formonly.fecha_liquida_tabla","Fecha\nliquidación")
      CALL f.setElementText("lb34","Fecha liquidación")      

      #se prepara el ESTEATEMENT 
      --DISPLAY " ls_SqlQry     ",ls_SqlQry      
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
            CALL fn_mensaje("Consulta","Debe de ingresar un campo de búsqueda","about")
         ELSE
            -- se construye la cadena de condicion
            LET v_condicion = "movimiento = 362\n"
            IF ( g_consulta.f_liquida IS NOT NULL ) THEN
               LET v_condicion = v_condicion,"AND f_liquida = '",g_consulta.f_liquida,"'\n"
            END IF

            IF ( g_consulta.f_registro IS NOT NULL ) THEN
               LET v_condicion = v_condicion,"AND f_registro = '", g_consulta.f_registro,"'\n"
            END IF

            IF ( g_consulta.folio_liquida IS NOT NULL ) THEN
               LET v_condicion = v_condicion,"AND folio_liquida = ",g_consulta.folio_liquida,"\n"
            END IF
            EXIT INPUT
         END IF
            
      ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT INPUT
   END INPUT
            
   IF NOT INT_FLAG THEN       
      IF fn_valida_folio_liquida_decreto(v_condicion,v_tabla, p_proceso_cod, p_opera_cod) THEN
         CALL fn_folios_consulta_liquida_decreto(v_tabla, p_proceso_cod, p_opera_cod, p_usuario)
      ELSE  
         CALL fn_mensaje("Consulta",
                         "No existen datos con los criterios dados.",
                         "about")
      END IF
   END IF                                              
            
   CLOSE WINDOW w_liquia_decreto
END FUNCTION
            
FUNCTION fn_valida_folio_liquida_decreto(v_condicion, v_tabla, p_proceso_cod, p_opera_cod)
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
   DISPLAY "v_query  " ,v_query                         
   PREPARE prp_folio_valida FROM v_query
   EXECUTE prp_folio_valida
            
   SELECT COUNT(*)
     INTO v_registros
     FROM tmp_folios_consulta a, glo_folio b
    WHERE a.folio = b.folio
      AND b.proceso_cod = p_proceso_cod
            
   RETURN v_registros
END FUNCTION

            
FUNCTION fn_folios_consulta_liquida_decreto(v_tabla, p_proceso_cod, p_opera_cod, p_usuario_cod)
   DEFINE v_tabla            STRING
   DEFINE p_proceso_cod      SMALLINT
   DEFINE p_opera_cod        SMALLINT
   DEFINE p_usuario_cod      CHAR(20)
   DEFINE v_folio            DECIMAL(9,0)
   DEFINE v_si_contador      SMALLINT
            
   DECLARE cur_folio CURSOR FOR SELECT a.*
                                  FROM tmp_folios_consulta a, 
                                       glo_folio b
                                 WHERE a.folio = b.folio
                                   AND b.proceso_cod = p_proceso_cod
                                   ORDER BY b.folio
            
   LET v_si_contador = 1
   FOREACH cur_folio INTO arr_folios[v_si_contador].*
      LET v_si_contador = v_si_contador + 1
   END FOREACH
            
   CALL f.setElementHidden("group2",0)
   CALL f.setElementHidden("group3",0)
            
   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY arr_folios TO arr_folios.* 
         BEFORE ROW
            LET v_si_contador       = ARR_CURR()
            LET v_folio = arr_folios[v_si_contador].folio
            CALL fn_llena_arbol_montos(v_folio, v_tabla)
            CALL ui.Interface.refresh()
      END DISPLAY
            
      DISPLAY ARRAY arr_arbol TO scr1.*
            
      END DISPLAY
            
      ON ACTION ACCEPT
         EXIT DIALOG
            
      ON ACTION CANCELAR 
         EXIT DIALOG
            
      ON ACTION reporte
         CALL fn_reporte_liquidacion_decreto(v_folio, v_tabla, p_usuario_cod, TRUE)
   END DIALOG
END FUNCTION
            
            
            
-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de preliquidacion y liquidacion
-- Ivan Vega        02Abr2014         - Se modifica encabezado de reporte para que incluya leyenda de que proceso
--                                      se trata, es decir, que diga REPORTE DE LIQUIDACION DE RETIRO DE... en lugar de solo
--                                      Liquidacion/preliquidacion
FUNCTION fn_reporte_liquidacion_decreto(p_folio, p_tabla, p_usuario_cod, p_b_despliegue_pantalla)
    DEFINE p_folio                 INTEGER
    DEFINE p_tabla                 STRING -- nombre de la tabla en cadena
    DEFINE p_usuario_cod           VARCHAR(20)  -- usuario que emite el reporte
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE v_qry_string            STRING
    DEFINE v_i                     INTEGER
    DEFINE v_j                     INTEGER
    DEFINE v_k                     INTEGER
    DEFINE v_contador              SMALLINT
    DEFINE cont_arbol              INTEGER
    DEFINE v_origen_datos          STRING
    DEFINE v_nombre_usuario        VARCHAR(50)
    


    DEFINE arr_grupo DYNAMIC ARRAY OF RECORD
        grupo_regimen           SMALLINT,
        desc_larga              CHAR(40),
        fondo_inversion         SMALLINT,
        monto_pesos          DECIMAL(28,6),
        monto_acciones       DECIMAL(28,6)
    END RECORD

    DEFINE arr_subcuenta DYNAMIC ARRAY OF RECORD
        subcuenta               SMALLINT,
        descripcion             CHAR(40),
        siefore                 SMALLINT,
        pesos                   DECIMAL(28,6),
        acciones                DECIMAL(28,6)
    END RECORD

    DEFINE arr_movimiento DYNAMIC ARRAY OF RECORD
        movimiento              SMALLINT,
        movimiento_desc         CHAR(40),
        pesos                   DECIMAL(28,6),
        acciones                DECIMAL(28,6)
    END RECORD

    DEFINE p_r_encabezado    RECORD
        p_folio               INTEGER,
        p_usuario_cod         STRING,
        p_fecha               DATE -- fecha de liquidacion/preliduidacion
    END RECORD
    
    DEFINE arr_datos      DYNAMIC ARRAY OF RECORD
        grupo_regimen           SMALLINT,
        desc_regimen            VARCHAR(40),
        subcuenta               SMALLINT,
        desc_subcuenta          VARCHAR(40),
        siefore                 SMALLINT,
        movimiento              SMALLINT,
        desc_movimiento         VARCHAR(40),
        pesos                   DECIMAL(28,6),
        aivs                    DECIMAL(28,6)
    END RECORD
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE report_handler       om.SaxDocumentHandler -- handler para el reporte en PDF
    
    -- se obtienen los datos del grupo de subcuenta
    LET v_qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga, \n",
                            " dp.fondo_inversion ,\n",
                            " sum(dp.monto_pesos),  \n",
                            " sum(dp.monto_acciones) \n",
                       " FROM ",p_tabla.trim()," dp,  \n",
                            " cat_grp_subcta_regimen tasr,  \n",
                            " cat_grupo_regimen tgr  \n",
                      " WHERE dp.folio_liquida = ?  \n",
                      "   AND tasr.subcuenta = dp.subcuenta  \n",
                      "   AND tgr.grupo_regimen = tasr.grupo_regimen \n",
                      "   AND dp.movimiento in ( 362, 592)   \n",
                      " GROUP BY 1,2,3  \n",
                      " ORDER BY 1 "
    --DISPLAY "v_qry_string 1fn_reporte_liquidacion_decreto ", v_qry_string
    PREPARE sid_grupo FROM v_qry_string
    DECLARE cur_grupo CURSOR FOR sid_grupo

    -- se obtienen las subcuentas del grupo
    LET v_qry_string = " SELECT ts.subcuenta, ts.subcuenta||' '||ts.subcuenta_desc, \n",
                            " dp.fondo_inversion, sum(dp.monto_pesos),  \n",
                            " sum(dp.monto_acciones)  \n",
                       " FROM ",p_tabla.trim()," dp, \n ",
                            " cat_grp_subcta_regimen tasr, \n ",
                            " cat_subcuenta ts  \n",
                      " WHERE dp.folio_liquida = ? ",
                        " AND tasr.subcuenta = dp.subcuenta  \n",
                        " AND tasr.grupo_regimen = ?  \n", 
                        " AND ts.subcuenta = tasr.subcuenta  \n",
                        " AND dp.movimiento in ( 362, 592)   \n",
                      " GROUP BY 1,2,3  \n",
                      " ORDER BY 1,3"
    --DISPLAY "v_qry_string 2 fn_reporte_liquidacion_decreto", v_qry_string    
    PREPARE sid_subcuenta FROM v_qry_string
    DECLARE cur_subcuenta CURSOR FOR sid_subcuenta

    -- se consturye la consulta de movimientos de la subcuenta
    LET v_qry_string = " SELECT tm.movimiento, tm.movimiento||' '||tm.movimiento_desc,\n ",
                            " sum(dp.monto_pesos),  \n",
                            " sum(dp.monto_acciones)  \n",
                       " FROM ",p_tabla.trim()," dp,  \n",
                            " cat_movimiento tm  \n",
                      " WHERE dp.folio_liquida = ?  \n" ,
                        " AND dp.subcuenta = ?  \n",
                        " AND dp.fondo_inversion = ?  \n",
                        " AND tm.movimiento = dp.movimiento  \n",
                        " AND dp.movimiento in ( 362, 592)   \n",
                      " GROUP BY 1,2 \n",
                      " ORDER BY 1 "
    --DISPLAY "v_qry_string 3 fn_reporte_liquidacion_decreto", v_qry_string
    PREPARE sid_movimiento FROM v_qry_string
    DECLARE cur_movimiento CURSOR FOR sid_movimiento

    LET v_i          = 1
    LET v_j          = 1
    LET v_k          = 1
    LET cont_arbol = 1

    LET v_contador = 0 -- contador para el arreglo de datos
    
    CALL arr_arbol.clear()

    FOREACH cur_grupo USING p_folio
                        INTO arr_grupo[v_i].*
        LET arr_arbol[cont_arbol].subcuenta      = arr_grupo[v_i].grupo_regimen
        LET arr_arbol[cont_arbol].subcuenta_desc = arr_grupo[v_i].desc_larga
        LET arr_arbol[cont_arbol].monto_pesos    = arr_grupo[v_i].monto_pesos
        LET arr_arbol[cont_arbol].monto_acciones = arr_grupo[v_i].monto_acciones
        LET arr_arbol[cont_arbol].id             = arr_grupo[v_i].grupo_regimen USING "<<"
        LET arr_arbol[cont_arbol].nivel          = 1
        LET arr_arbol[cont_arbol].padre_id       = ""
        LET arr_arbol[cont_arbol].siefore        = arr_grupo[v_i].fondo_inversion
        LET cont_arbol = cont_arbol + 1

        FOREACH cur_subcuenta USING p_folio,
                                 arr_grupo[v_i].grupo_regimen
                            INTO arr_subcuenta[v_j].*
                            
            LET arr_arbol[cont_arbol].subcuenta      = arr_subcuenta[v_j].subcuenta
            LET arr_arbol[cont_arbol].subcuenta_desc = arr_subcuenta[v_j].descripcion
            LET arr_arbol[cont_arbol].monto_pesos    = arr_subcuenta[v_j].pesos
            LET arr_arbol[cont_arbol].monto_acciones = arr_subcuenta[v_j].acciones
            LET arr_arbol[cont_arbol].id             = arr_grupo[v_i].grupo_regimen USING"<<",".",
                                                       arr_subcuenta[v_j].subcuenta USING"<<"
            LET arr_arbol[cont_arbol].nivel          = 2
            LET arr_arbol[cont_arbol].padre_id       = arr_grupo[v_i].grupo_regimen USING"<<"
            LET arr_arbol[cont_arbol].siefore        = arr_subcuenta[v_j].siefore
            LET cont_arbol = cont_arbol + 1
          
            FOREACH cur_movimiento USING p_folio,
                                     arr_subcuenta[v_j].subcuenta,
                                     arr_subcuenta[v_j].siefore
                                INTO arr_movimiento[v_k].*
                LET arr_arbol[cont_arbol].subcuenta      = arr_movimiento[v_k].movimiento
                LET arr_arbol[cont_arbol].subcuenta_desc = arr_movimiento[v_k].movimiento_desc
                LET arr_arbol[cont_arbol].monto_pesos    = arr_movimiento[v_k].pesos
                LET arr_arbol[cont_arbol].monto_acciones = arr_movimiento[v_k].acciones
                LET arr_arbol[cont_arbol].id             = arr_grupo[v_i].grupo_regimen USING "<<",".",
                                                           arr_subcuenta[v_j].subcuenta USING "<<",".",
                                                           arr_movimiento[v_k].movimiento USING" <<"
                LET arr_arbol[cont_arbol].nivel          = 3
                LET arr_arbol[cont_arbol].padre_id       = arr_grupo[v_i].grupo_regimen USING "<<",".",
                                                           arr_subcuenta[v_j].subcuenta USING "<<"
                LET arr_arbol[cont_arbol].siefore        = arr_subcuenta[v_j].siefore
                LET cont_arbol = cont_arbol + 1

                -- se incrementa el contador de registros del arreglo de datos
                LET v_contador = v_contador + 1

                -- se asignan los datos de la subcuenta del grupo al arreglo de datos 
                LET arr_datos[v_contador].grupo_regimen     = arr_grupo[v_i].grupo_regimen
                LET arr_datos[v_contador].desc_regimen      = arr_grupo[v_i].desc_larga CLIPPED
                LET arr_datos[v_contador].subcuenta         = arr_subcuenta[v_j].subcuenta
                LET arr_datos[v_contador].desc_subcuenta    = arr_subcuenta[v_j].descripcion
                LET arr_datos[v_contador].siefore           = arr_subcuenta[v_j].siefore
                LET arr_datos[v_contador].movimiento        = arr_movimiento[v_k].movimiento
                LET arr_datos[v_contador].desc_movimiento   = arr_movimiento[v_k].movimiento_desc CLIPPED
                LET arr_datos[v_contador].pesos             = arr_movimiento[v_k].pesos
                LET arr_datos[v_contador].aivs              = arr_movimiento[v_k].acciones

                
                LET v_k = v_k + 1
            END FOREACH
            CLOSE cur_movimiento
            LET v_j = v_j + 1
        END FOREACH
        CLOSE cur_subcuenta
        LET v_i = v_i + 1
    END FOREACH
    CLOSE cur_grupo

    --DISPLAY "tabla ", p_tabla 
    -- se determina el origen de los datos
    IF (( p_tabla.trim() = "cta_movimiento" ) OR (p_tabla.trim() = "cta_decreto" )) THEN
       LET v_origen_datos = "Liquidación"
    ELSE
       LET v_origen_datos = "Preliquidación"
    END IF

    --DISPLAY "Origen de los datos: ", v_origen_datos
    
    -- se obtiene la ruta de los listados
    CALL fn_rutas("bat") RETURNING v_ruta_ejecutable, v_ruta_listados

      --DISPLAY " v_ruta_ejecutable", v_ruta_ejecutable
      --DISPLAY " v_ruta_listados", v_ruta_listados
    -- se construye la ruta del archivo
    LET v_ruta_reporte = v_ruta_listados.trim() , "/" , v_origen_datos.trim() ,
                         "_" , p_folio USING "&&&&&&", ".pdf"

    -- se indica que el reporte usara la plantilla creada
    IF ( fgl_report_loadCurrentSettings("../../glo/bin/GLOI01.4rp") ) THEN  -- if  the file loaded OK

       -- si no se pidio el reporte en pantalla
       IF ( NOT p_b_despliegue_pantalla ) THEN
          -- sin preview
          CALL fgl_report_selectPreview(0)
          -- se indica que se escriba en archivo
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
       END IF
       

       LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
    ELSE
       --DISPLAY "no funciono"
       EXIT PROGRAM
    END IF

    
    -- se inicia el reporte
    START REPORT rpt_liquidacion TO XML HANDLER report_handler
    --START REPORT rpt_liquidacion TO screen

    LET p_r_encabezado.p_folio       = p_folio
    LET p_r_encabezado.p_usuario_cod = p_usuario_cod

    -- se obtiene la fecha de liquidacion/preliquidacion
    LET v_qry_string = " SELECT DISTINCT f_liquida\n",
                       " FROM ",p_tabla.trim()," \n",
                      " WHERE folio_liquida = ? \n"

    --DISPLAY v_qry_string
    --DISPLAY "folio = ", p_folio
    PREPARE sid_fliquida FROM v_qry_string
    EXECUTE sid_fliquida USING p_folio
    INTO p_r_encabezado.p_fecha

    -- se obtiene el nombre del usuario
    SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
     WHERE usuario_cod = p_usuario_cod


    --DISPLAY " v_contador",v_contador
    FOR v_contador = 1 TO arr_datos.getLength()
       DISPLAY arr_datos[v_contador].*
       --display " v_origen_datos ",v_origen_datos
       OUTPUT TO REPORT rpt_liquidacion(v_origen_datos, p_r_encabezado.*, arr_datos[v_contador].*, p_usuario_cod)
    END FOR

    FINISH REPORT rpt_liquidacion
    
END FUNCTION

FUNCTION fn_despliega_desc(p_proceso_cod, p_opera_cod)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_proceso_desc      CHAR(40)
   DEFINE v_opera_desc      CHAR(40)
            
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


FUNCTION fn_llena_arbol_montos(p_folio, v_tabla)
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
        fondo_inversion      SMALLINT,
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

    LET qry_string = "SET PDQPRIORITY HIGH"
    PREPARE prp_high_pdq FROM qry_string 

    LET qry_string = "SET PDQPRIORITY DEFAULT"
    PREPARE prp_default_pdq FROM qry_string 
    
    LET qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga, \n",
                            " dp.fondo_inversion ,\n",
                            " sum(dp.monto_pesos), \n",
                            " sum(dp.monto_acciones) \n",
                       " FROM ",v_tabla.trim()," dp,  \n",
                            " cat_grp_subcta_regimen tasr, \n ",
                            " cat_grupo_regimen tgr  \n",
                      " WHERE dp.folio_liquida = ?  \n",
                      "   AND tasr.subcuenta = dp.subcuenta \n ",
                      "   AND tgr.grupo_regimen = tasr.grupo_regimen   \n",
                      "   AND dp.movimiento in ( 362, 592)   \n",
                      " GROUP BY 1,2,3  \n",
                      " ORDER BY 1 "      
    --DISPLAY " qry_string 1 =  ",qry_string
    PREPARE prp_nivel1 FROM qry_string
    DECLARE cur_nivel1 CURSOR FOR prp_nivel1

    LET qry_string = " SELECT ts.subcuenta, ts.subcuenta||'-'||ts.subcuenta_desc, \n",
                            " dp.fondo_inversion, sum(dp.monto_pesos),   \n",
                            " sum(dp.monto_acciones)   \n",
                       " FROM ",v_tabla.trim()," dp,   \n",
                            " cat_grp_subcta_regimen tasr,  \n",
                            " cat_subcuenta ts   \n",
                      " WHERE dp.folio_liquida = ?   \n",
                        " AND tasr.subcuenta = dp.subcuenta  \n ",
                        " AND tasr.grupo_regimen = ?   \n", 
                        " AND ts.subcuenta = tasr.subcuenta   \n",
                        " AND dp.movimiento in ( 362, 592)   \n",
                      " GROUP BY 1,2,3   \n",
                      " ORDER BY 1,3"
    --DISPLAY " qry_string 2 =  ",qry_string
    PREPARE prp_nivel2 FROM qry_string
    DECLARE cur_nivel2 CURSOR FOR prp_nivel2
            
    LET qry_string = " SELECT tm.movimiento, tm.movimiento||'-'||tm.movimiento_desc, ",
                            " sum(dp.monto_pesos), ",
                            " sum(dp.monto_acciones) ",
                       " FROM ",v_tabla.trim()," dp, ",
                            " cat_movimiento tm ",
                      " WHERE dp.folio_liquida = ? " ,
                        " AND dp.subcuenta = ?",
                        " AND dp.fondo_inversion = ? ",
                        " AND tm.movimiento = dp.movimiento ",   
                        " AND dp.movimiento in ( 362, 592)   \n",
                      " GROUP BY 1,2 ",
                      " ORDER BY 1 "   
    --DISPLAY " qry_string 3 =  ",qry_string                 
    PREPARE prp_nivel3 FROM qry_string
    DECLARE cur_nivel3 CURSOR FOR prp_nivel3
            
    LET i          = 1
    LET j          = 1
    LET k          = 1
    LET cont_arbol = 1
            
    CALL arr_arbol.clear()

    EXECUTE prp_high_pdq   
                              
    FOREACH cur_nivel1 USING p_folio
                        INTO arr_nivel1[i].*
        LET arr_arbol[cont_arbol].subcuenta      = arr_nivel1[i].grupo_regimen
        LET arr_arbol[cont_arbol].subcuenta_desc = arr_nivel1[i].desc_larga
        LET arr_arbol[cont_arbol].monto_pesos    = arr_nivel1[i].monto_pesos
        LET arr_arbol[cont_arbol].monto_acciones = arr_nivel1[i].monto_acciones
        LET arr_arbol[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING "<<"
        LET arr_arbol[cont_arbol].nivel          = 1
        LET arr_arbol[cont_arbol].padre_id       = ""
        LET arr_arbol[cont_arbol].siefore        = arr_nivel1[i].fondo_inversion
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
    EXECUTE prp_default_pdq
END FUNCTION
}