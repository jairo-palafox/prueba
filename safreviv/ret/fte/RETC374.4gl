###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            =>                                                         #
#Programa          =>                                                         #
#Objetivo          => CONSULTA DE LIQUIDACION                                 #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv

PRIVATE DEFINE v_proceso_cod				SMALLINT
PRIVATE DEFINE v_opera_cod					SMALLINT
PRIVATE DEFINE v_funcion					SMALLINT
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana
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
          fecha_proceso      DATE,
          numero_registros   INTEGER 
END RECORD  
DEFINE w               ui.Window
DEFINE f               ui.Form
DEFINE g_consulta         RECORD
          folio_liquida      DECIMAL(10,0),
          f_liquida          DATE,
          f_registro         DATE
END RECORD

MAIN

   DEFINE v_tabla             CHAR(30)
   DEFINE v_ruta_bin          CHAR(40)            
   DEFINE v_condicion         STRING
   DEFINE v_v_nombre_combo    VARCHAR(20) -- nombre del combobox
   DEFINE cb                  ui.ComboBox # Variable de Combobox
   DEFINE folio_liquida       DECIMAL(9,0)
   DEFINE ls_SqlQry           STRING 

   DEFINE v_proceso_paso      SMALLINT
   
   CALL arr_arbol.clear()
   CALL arr_folios.clear()
       
   -- se recupera la clave de usuario desde parametro
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET v_proceso_cod = 1520		#Reversos operativos
   LET v_opera_cod   = 4		#Liquidacion
   LET v_funcion     = 1		#Consulta de liquidacion

   -- se invoca la funcion general de consulta de liquidacion
   --CALL fn_liquida(p_usuario_cod, v_proceso_cod, v_opera_cod, v_funcion)


   OPEN WINDOW retc3741 WITH FORM "RETC3741"
   LET cb = ui.ComboBox.forName("formonly.folio_liquida") #Asignación del combo a la forma	 
   CALL cb.clear()#se limpia el combo 
  
   LET w = ui.Window.getCurrent() -- asigna a "w" la ventana activa
   LET f = w.getForm()
            
   CALL f.setElementHidden("group2",1)
   CALL f.setElementHidden("group3",1)
      
   INPUT BY NAME g_consulta.folio_liquida
   WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
      BEFORE INPUT
      #se asigna la variable para la consulta de folios asosiados  
      LET ls_SqlQry =  "SELECT folio \n",
                       "  FROM glo_folio \n",
                       " WHERE proceso_cod = ",v_proceso_cod,"\n"
      #valida si es preliquidada o liquidada
      LET ls_SqlQry = ls_SqlQry,
      "AND status >= 2 \n",
      " ORDER BY 1 DESC"

      -- =========================================================
      --                   L I Q U I D A C I O N
      -- =========================================================
      -- las etiquetas se cambian para referir a liquidacion
      CALL f.setElementText("group3","Montos liquidados")
      CALL f.setElementText("formonly.fecha_liquida_tabla","Fecha\nliquidación")
      CALL f.setElementText("lb34","Fecha liquidación")
      
      #se prepara el ESTEATEMENT 
      PREPARE con_folios FROM ls_SqlQry      
      #se declara el cursor
      DECLARE cur_folios CURSOR FOR con_folios                  
      ##se ejecuta la sentencia SQL que busca los folos asosiados mediante un ciclo
      FOREACH cur_folios INTO folio_liquida      	
      	#se asignan los folios al combo
      	CALL cb.addItem(folio_liquida ,folio_liquida)       	    
      END FOREACH  
      FREE cur_folios
      CALL fn_despliega_desc(v_proceso_cod, v_opera_cod)
      
      -- se inician las variables de captura
      LET g_consulta.folio_liquida = NULL

      ON CHANGE folio_liquida
         IF v_proceso_cod = 2501 THEN
            SELECT proceso_cod
              INTO v_proceso_paso
              FROM aop_ctr_ajuste
             WHERE folio = g_consulta.folio_liquida

            IF v_proceso_paso = 1402 OR -- SAR92 cta_decreto
               v_proceso_paso = 1505 THEN -- retiro N cta_decreto
               LET v_tabla = "cta_decreto"
            END IF
         END IF
      ON ACTION ACCEPT
         LET v_tabla = "cta_movimiento"
         IF ( g_consulta.folio_liquida IS NULL ) THEN
            CALL fn_mensaje("Consulta",
                            "Debe de ingresar el folio de búsqueda",
                            "about")
            
         ELSE
            -- se construye la cadena de condicion
            LET v_condicion = "1=1\n"

            IF ( g_consulta.folio_liquida IS NOT NULL ) THEN
               LET v_condicion = v_condicion || "AND folio_liquida = '" || g_consulta.folio_liquida || "'\n"
               SELECT tabla
               INTO   v_tabla
               FROM   glo_his_folio
               WHERE  folio = g_consulta.folio_liquida
               IF v_tabla IS NULL THEN 
                   LET v_tabla = "cta_movimiento"
               END IF 
            END IF
            EXIT INPUT
         END IF

      ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT INPUT
   END INPUT

 {  
         DISPLAY "Valores capturados"
         display "ls_SqlQry  ",ls_SqlQry
         DISPLAY "f_liquida  ",g_consulta.f_liquida     
         DISPLAY "f_registro ",g_consulta.f_registro    
         DISPLAY "folio_liq  ",g_consulta.folio_liquida 
         DISPLAY "condicion  ",v_condicion
         DISPLAY "int_flag   ",INT_FLAG
}

   
   IF NOT INT_FLAG THEN       
      IF fn_valida_folio_local(v_condicion,v_tabla, v_proceso_cod, v_opera_cod) THEN
         CALL fn_folios_consulta_local(v_tabla, v_proceso_cod, v_opera_cod, p_usuario_cod)
      ELSE  
         CALL fn_mensaje("Consulta",
                         "No existen datos con los criterios dados.",
                         "about")
      END IF
   END IF                                              
            
   CLOSE WINDOW retc3741
   
END MAIN

FUNCTION fn_valida_folio_local(v_condicion, v_tabla, p_proceso_cod, p_opera_cod)
   DEFINE v_condicion        STRING
   DEFINE v_tabla            STRING
   DEFINE v_tabla_c          CHAR(20)
   DEFINE p_proceso_cod      SMALLINT
   DEFINE p_opera_cod        SMALLINT
   DEFINE v_registros        SMALLINT
   DEFINE v_query            STRING

   CREATE TEMP TABLE tmp_folios_consulta (
               folio         DECIMAL (9,0),
               fecha_liq     DATE,
               fecha_proceso DATE)

   
         
   IF v_tabla = "cta_movimiento" THEN
                    
     
      DECLARE c_tab CURSOR FOR 
        SELECT tabla 
          FROM cat_tab_movimiento
      FOREACH c_tab INTO v_tabla_c
         LET v_query = v_query.trim()," SELECT folio_liquida, f_liquida, f_registro ",
                       "  FROM ", v_tabla_c,
                       " WHERE ",v_condicion.trim(),
                       " GROUP BY 1,2,3 UNION ALL"
      END FOREACH
      LET v_query = v_query.trim(),
                    " SELECT folio_liquida, f_liquida, f_registro ",
                    "  FROM cta_movimiento ",
                    " WHERE ",v_condicion.trim(),
                    " GROUP BY 1,2,3 INTO TEMP tmp_folio_his"
      CLOSE c_tab
      FREE c_tab
      DISPLAY v_query
      PREPARE prp_folio_his FROM v_query
      EXECUTE prp_folio_his

      LET v_query = "INSERT INTO tmp_folios_consulta SELECT * FROM tmp_folio_his "

   ELSE
      LET v_query = "INSERT INTO tmp_folios_consulta ",
                    "SELECT folio_liquida, f_liquida, f_registro ",
                    "  FROM ",v_tabla.trim(),
                    " WHERE ",v_condicion.trim(),
                    " GROUP BY 1,2,3 "
                    
   END IF   

   DISPLAY "sentencia:", v_query
   
   PREPARE prp_folio_valida FROM v_query
   EXECUTE prp_folio_valida
            
   SELECT COUNT(*)
     INTO v_registros
     FROM tmp_folios_consulta a, glo_folio b
    WHERE a.folio = b.folio
      AND b.proceso_cod = p_proceso_cod
   DISPLAY "Registros insertados en la tabla temporal :", v_registros         
   RETURN v_registros
END FUNCTION

FUNCTION fn_folios_consulta_local (v_tabla, p_proceso_cod, p_opera_cod, p_usuario_cod)
   DEFINE v_tabla            STRING
   DEFINE p_proceso_cod      SMALLINT
   DEFINE p_opera_cod        SMALLINT
   DEFINE p_usuario_cod      CHAR(20)
   DEFINE v_folio            DECIMAL(9,0)
   DEFINE i                  SMALLINT
   DEFINE p_programa_cod     VARCHAR(10)
   DEFINE p_pid              DECIMAL(9,0)
   DEFINE v_num_registros    INTEGER 

   -- se obtienen los folios            
   DECLARE cur_folio CURSOR FOR 
   SELECT a.*, c.num_reg
   FROM   tmp_folios_consulta a, 
          glo_folio b,
          (SELECT COUNT(*) AS num_reg, folio
           FROM   ret_his_anexo1
           GROUP BY folio) c
   WHERE  a.folio = b.folio
   AND    c.folio = b.folio
   AND    b.proceso_cod = p_proceso_cod
   ORDER BY b.folio
            
   -- se transfieren los folios al arreglo de folios
   LET i = 1
   FOREACH cur_folio INTO arr_folios[i].*
      DISPLAY "Folio en el ciclo:", arr_folios[i].folio
      LET i = i + 1
   END FOREACH
   DISPLAY "Indice :", i         
   CALL f.setElementHidden("group2",0)
   CALL f.setElementHidden("group3",0)

   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY arr_folios TO arr_folios.* 
         BEFORE ROW
            LET i       = ARR_CURR()
            LET v_folio = arr_folios[i].folio
            LET v_num_registros = arr_folios[i].numero_registros
            DISPLAY "Folio :", arr_folios[i].folio
            DISPLAY "Tabla :", v_tabla
            CALL fn_llena_arbol_montos_local(v_folio, v_tabla)
            CALL ui.Interface.refresh()
      END DISPLAY
            
      DISPLAY ARRAY arr_arbol TO scr1.*
            
      END DISPLAY
            
      ON ACTION ACCEPT
         EXIT DIALOG
            
      ON ACTION CANCELAR 
         EXIT DIALOG
            
      ON ACTION reporte
         -- se obtiene el codigo de programa
         SELECT programa_cod
         INTO   p_programa_cod
         FROM   cat_operacion
         WHERE  proceso_cod = p_proceso_cod
         AND    opera_cod   = p_opera_cod
         
         -- se obtiene el pid asociado al folio
         SELECT pid
         INTO   p_pid
         FROM   bat_ctr_proceso
         WHERE  folio = v_folio
         AND    proceso_cod = p_proceso_cod
         LET v_tabla = "cta_movimiento"
         CALL fn_reporte_liquidacion_local(v_folio, v_num_registros, v_tabla, p_usuario_cod, p_pid, p_proceso_cod, p_opera_cod, p_programa_cod, TRUE)
   END DIALOG
END FUNCTION

FUNCTION fn_llena_arbol_montos_local(p_folio, v_tabla)
    DEFINE v_montos       RECORD
          subcuenta          SMALLINT     ,
          subcuenta_desc     CHAR(50)     ,
          fondo_inversion    SMALLINT     ,
          padre_id           CHAR(30)     ,
          id                 CHAR(30)     ,
          nivel              SMALLINT     ,
          monto_pesos        DECIMAL(28,6),
          monto_acciones     DECIMAL(28,6)
    END RECORD
    
    DEFINE p_folio        INTEGER
    DEFINE v_tabla        CHAR(30)
    DEFINE qry_string     STRING
    DEFINE i              INTEGER
    DEFINE j              INTEGER
    DEFINE k              INTEGER
    DEFINE cont_arbol     INTEGER
    
    LET qry_string = "EXECUTE PROCEDURE sp_montos_folio(?,?)"
    PREPARE prp_mts_folio FROM qry_string
    EXECUTE prp_mts_folio USING p_folio, v_tabla

    DISPLAY "Después del procedure:" ,p_folio, v_tabla
    
    CALL arr_arbol.clear()
    LET cont_arbol = 1
    
    LET qry_string =  "SELECT *
                         FROM tmp_montos_liq
                        ORDER BY id "
    --DISPLAY "qry_string", qry_string
    PREPARE  con_sql FROM qry_string
    DECLARE  cur_arbol CURSOR FOR  con_sql

    --DECLARE cur_arbol CURSOR FOR SELECT *
     --                              FROM tmp_montos_liq
      --                            ORDER BY id
    
    FOREACH cur_arbol INTO v_montos.*
        LET arr_arbol[cont_arbol].subcuenta      = v_montos.subcuenta
        LET arr_arbol[cont_arbol].subcuenta_desc = v_montos.subcuenta_desc
        LET arr_arbol[cont_arbol].monto_pesos    = v_montos.monto_pesos
        LET arr_arbol[cont_arbol].monto_acciones = v_montos.monto_acciones
        LET arr_arbol[cont_arbol].id             = v_montos.id
        LET arr_arbol[cont_arbol].nivel          = v_montos.nivel
        LET arr_arbol[cont_arbol].padre_id       = v_montos.padre_id
        LET arr_arbol[cont_arbol].siefore        = v_montos.fondo_inversion
        DISPLAY "Los datos del arreglo son:"
        DISPLAY "arr_arbol[cont_arbol].subcuenta      = ", arr_arbol[cont_arbol].subcuenta
        DISPLAY "arr_arbol[cont_arbol].subcuenta_desc = ", arr_arbol[cont_arbol].subcuenta_desc
        DISPLAY "arr_arbol[cont_arbol].monto_pesos    = ", arr_arbol[cont_arbol].monto_pesos
        DISPLAY "arr_arbol[cont_arbol].monto_acciones = ", arr_arbol[cont_arbol].monto_acciones
        DISPLAY "arr_arbol[cont_arbol].id             = ", arr_arbol[cont_arbol].id
        DISPLAY "arr_arbol[cont_arbol].nivel          = ", arr_arbol[cont_arbol].nivel
        DISPLAY "arr_arbol[cont_arbol].padre_id       = ", arr_arbol[cont_arbol].padre_id
        DISPLAY "arr_arbol[cont_arbol].siefore        = ", arr_arbol[cont_arbol].siefore

        LET cont_arbol = cont_arbol + 1

    END FOREACH
    DISPLAY "Se insertaron en el arreglo :", cont_arbol
    CLOSE cur_arbol
    FREE cur_arbol
END FUNCTION


FUNCTION fn_reporte_liquidacion_local(p_folio, p_num_registros, p_tabla, p_usuario_cod, p_pid, p_proceso_cod, p_opera_cod, p_programa_cod, p_b_despliegue_pantalla)
    DEFINE p_folio                 INTEGER
    DEFINE p_num_registros         INTEGER 
    DEFINE p_tabla                 STRING   -- nombre de la tabla en cadena
    DEFINE p_usuario_cod           STRING   -- usuario que emite el reporte
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE v_qry_string            STRING
    DEFINE v_tabla                 CHAR(30)
    DEFINE v_i                     INTEGER
    DEFINE v_j                     INTEGER
    DEFINE v_k                     INTEGER
    DEFINE v_contador              SMALLINT
    DEFINE cont_arbol              INTEGER
    DEFINE v_origen_datos          STRING
    DEFINE p_pid                   DECIMAL(9,0) -- pid del proceso
    DEFINE p_proceso_cod           SMALLINT
    DEFINE p_opera_cod             SMALLINT
    DEFINE p_programa_cod          VARCHAR(10)
    DEFINE p_modulo_cod            VARCHAR(3)

    DEFINE arr_grupo DYNAMIC ARRAY OF RECORD
        grupo_regimen           SMALLINT,
        desc_larga              CHAR(40),
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
        p_fecha               DATE, -- fecha de liquidacion/preliduidacion
        p_cant_registros      INTEGER 
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
    DEFINE v_nombre_usuario     CHAR (40)
    DEFINE v_usuario_cod_temp   CHAR (20)
    DEFINE v_ruta_bin           CHAR(40) -- ruta del directorio glo/bin
    DEFINE v_ruta_4rp           STRING   -- ruta donde se encuentra el archivop 4rp

    -- variables para formato de encabezado en reporte
    DEFINE v_proceso_desc       CHAR(40) -- descripcion del proceso
    DEFINE v_titulo_reporte     STRING
    
    -- se obtienen los parametros
    LET v_usuario_cod_temp = p_usuario_cod

    SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
     WHERE usuario_cod = v_usuario_cod_temp


    IF p_tabla.trim() = "cta_movimiento" THEN
       SELECT fn_tab_movimiento(0,p_folio,'')
         INTO v_tabla
         FROM cat_proceso
        WHERE proceso_cod = 901
      
       LET p_tabla = v_tabla

    END IF
    	 -- se obtienen los datos del grupo de subcuenta
       LET v_qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga, ",
                               " sum(dp.monto_pesos), ",
                               " sum(dp.monto_acciones)",
                          " FROM ",p_tabla.trim()," dp, ",
                               " cat_grp_subcta_regimen tasr, ",
                               " cat_grupo_regimen tgr ",
                         " WHERE dp.folio_liquida = ? ",
                           " AND tasr.subcuenta = dp.subcuenta ",
                           " AND tgr.grupo_regimen = tasr.grupo_regimen ",
                         " GROUP BY 1,2 ",
                         " ORDER BY 1 "
       --DISPLAY "v_qry_string 1 ", v_qry_string
       PREPARE sid_grupo FROM v_qry_string
       DECLARE cur_grupo CURSOR FOR sid_grupo
       
       -- se obtienen las subcuentas del grupo
       LET v_qry_string = " SELECT ts.subcuenta, ts.subcuenta||' '||ts.subcuenta_desc, ",
                               " dp.fondo_inversion, sum(dp.monto_pesos), ",
                               " sum(dp.monto_acciones) ",
                          " FROM ",p_tabla.trim()," dp, ",
                               " cat_grp_subcta_regimen tasr, ",
                               " cat_subcuenta ts ",
                         " WHERE dp.folio_liquida = ? ",
                           " AND tasr.subcuenta = dp.subcuenta ",
                           " AND tasr.grupo_regimen = ? ", 
                           " AND ts.subcuenta = tasr.subcuenta ",
                         " GROUP BY 1,2,3 ",
                         " ORDER BY 1,3"
       --DISPLAY "v_qry_string 2 ", v_qry_string    
       PREPARE sid_subcuenta FROM v_qry_string
       DECLARE cur_subcuenta CURSOR FOR sid_subcuenta
       
       -- se consturye la consulta de movimientos de la subcuenta
       LET v_qry_string = " SELECT tm.movimiento, tm.movimiento||' '||tm.movimiento_desc, ",
                               " sum(dp.monto_pesos), ",
                               " sum(dp.monto_acciones) ",
                          " FROM ",p_tabla.trim()," dp, ",
                               " cat_movimiento tm ",
                         " WHERE dp.folio_liquida = ? " ,
                           " AND dp.subcuenta = ?",
                           " AND dp.fondo_inversion = ? ",
                           " AND tm.movimiento = dp.movimiento ",
                         " GROUP BY 1,2 ",
                         " ORDER BY 1 "
       --DISPLAY "v_qry_string 3", v_qry_string
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
           LET arr_arbol[cont_arbol].siefore        = ""
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

    -- se obtiene la descripcion del proceso
    SELECT proceso_desc
    INTO   v_proceso_desc
    FROM   cat_proceso
    WHERE  proceso_cod = p_proceso_cod

    -- se conforma el titulo del reporte
    LET v_titulo_reporte = "REPORTE DE "
    
    --DISPLAY "tabla ", p_tabla 
    -- se determina el origen de los datos
    LET v_titulo_reporte = v_titulo_reporte, "LIQUIDACIÓN "
    LET v_origen_datos = "Liquidación"

    -- se complementa con la descripcion del proceso
    LET v_titulo_reporte = v_titulo_reporte, v_proceso_desc CLIPPED

    -- origen de los datos
    DISPLAY "Titulo de reporte: ", v_titulo_reporte
    
    -- se obtiene el modulo del proceso
    SELECT modulo_cod
      INTO p_modulo_cod
      FROM cat_proceso
     WHERE proceso_cod = p_proceso_cod
    
    -- se obtiene la ruta de los listados
    CALL fn_rutas(p_modulo_cod) RETURNING v_ruta_ejecutable, v_ruta_listados

    -- se escribe la ruta del reporte    
    LET v_ruta_reporte = v_ruta_listados.trim() , "/" ,
                         p_usuario_cod CLIPPED , "-", -- usuario
                         p_programa_cod CLIPPED, "-", -- programa
                         p_pid USING "&&&&&","-", -- PID
                         p_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                         p_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación
    DISPLAY "Ruta del reporte:", v_ruta_reporte
    -- se obtiene la ruta bin de glo
    SELECT ruta_bin
    INTO   v_ruta_bin
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
    
    -- se conforma la ruta del archivo 4rp
    LET v_ruta_4rp = v_ruta_bin CLIPPED, "/RETC374.4rp"
    DISPLAY "Ruta de la plantilla: ", v_ruta_4rp
    
    -- se indica que el reporte usara la plantilla creada
    IF ( fgl_report_loadCurrentSettings(v_ruta_4rp) ) THEN  -- if  the file loaded OK
       -- si no se pidio el reporte en pantalla
       IF (NOT p_b_despliegue_pantalla ) THEN
          -- sin preview
          CALL fgl_report_selectPreview(0)
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
       ELSE
          CALL fgl_report_selectPreview(1)
       END IF
       -- se indica que se escriba en archivo
       LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
    ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_ruta_4rp
       EXIT PROGRAM
    END IF
    DISPLAY "Inicial el reporte con la tabla :", p_tabla
    -- se inicia el reporte
    START REPORT rpt_liquidacion_local TO XML HANDLER report_handler
    --START REPORT rpt_liquidacion_local TO screen
       LET p_r_encabezado.p_folio       = p_folio
       LET p_r_encabezado.p_usuario_cod = p_usuario_cod
       LET p_r_encabezado.p_cant_registros = p_num_registros
       DISPLAY "Valores pasados a la estructura del encabezado:",p_r_encabezado.*
       -- se obtiene la fecha de liquidacion/preliquidacion
       LET v_qry_string = " SELECT DISTINCT f_liquida\n",
                          " FROM ",p_tabla.trim()," \n",
                          " WHERE folio_liquida = ? \n"

       PREPARE sid_fliquida FROM v_qry_string
       EXECUTE sid_fliquida USING p_folio
                             INTO p_r_encabezado.p_fecha

       DISPLAY " v_contador",v_contador
       --DISPLAY ":1:"

          FOR v_contador = 1 TO arr_datos.getLength()
             DISPLAY "Arreglo de datos :", arr_datos[v_contador].*
             --display "  ",v_origen_datos
             OUTPUT TO REPORT rpt_liquidacion_local(v_titulo_reporte, v_origen_datos, p_r_encabezado.*, arr_datos[v_contador].*, v_nombre_usuario)
          END FOR
       
       --DISPLAY ":2:"
    FINISH REPORT rpt_liquidacion_local
    --DISPLAY ":3:"
    
END FUNCTION

REPORT rpt_liquidacion_local(p_titulo_reporte, p_origen_datos, p_r_encabezado, p_r_datos, p_nombre_usuario)
DEFINE p_titulo_reporte  STRING -- preliquidacion, liquidacion
DEFINE p_origen_datos    STRING -- preliquidacion, liquidacion
DEFINE p_r_encabezado    RECORD
        p_folio               INTEGER,
        p_usuario_cod         STRING,
        p_fecha               DATE, -- fecha de liquidacion/preliquidacion
        p_num_registros       INTEGER 
    END RECORD
    
    DEFINE p_r_datos           RECORD
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
    DEFINE  v_subtotal_cuenta_pesos       DECIMAL(28,6)
    DEFINE  v_subtotal_cuenta_aivs        DECIMAL(28,6)
    DEFINE  v_subtotal_grupo_pesos        DECIMAL(28,6)
    DEFINE  v_subtotal_grupo_aivs         DECIMAL(28,6)
    DEFINE  v_subtotal_movimientos_pesos  DECIMAL(28,6)
    DEFINE  v_subtotal_movimientos_aivs   DECIMAL(28,6)
    DEFINE  v_grupo_desc                  STRING
    DEFINE  v_subcuenta_desc              STRING
    DEFINE  v_siefore                     STRING
    DEFINE  v_conteo                      SMALLINT
    DEFINE  p_nombre_usuario              CHAR (40)
    
   FORMAT

      FIRST PAGE HEADER
         DISPLAY "Origen datos en reporte: ", p_origen_datos
         DISPLAY "Encabezado             : ", p_r_encabezado.*
         DISPLAY "Titulo                 : ", p_titulo_reporte
         -- se despliegan los datos del encabezado
         PRINTX p_titulo_reporte, p_origen_datos, p_r_encabezado.*
         LET v_subtotal_cuenta_pesos      = 0
         LET v_subtotal_cuenta_aivs       = 0
         LET v_subtotal_grupo_pesos       = 0
         LET v_subtotal_grupo_aivs        = 0
         LET v_subtotal_movimientos_pesos = 0
         LET v_subtotal_movimientos_aivs  = 0
         LET v_conteo                     = 0 -- conteo de grupos
         PRINTX p_nombre_usuario

      AFTER GROUP OF p_r_datos.grupo_regimen
         DISPLAY "Datos grupo desc           :", v_grupo_desc
         DISPLAY "Datos subtotal grupo pesos :", v_subtotal_grupo_pesos
         DISPLAY "Datos subtotal grupo aivs  :", v_subtotal_grupo_aivs
         PRINTX v_grupo_desc, v_subtotal_grupo_pesos, v_subtotal_grupo_aivs

      AFTER GROUP OF p_r_datos.subcuenta
         LET v_subtotal_grupo_pesos  = v_subtotal_grupo_pesos + v_subtotal_cuenta_pesos
         LET v_subtotal_grupo_aivs   = v_subtotal_grupo_aivs  + v_subtotal_cuenta_aivs
         DISPLAY "Datos Subcuenta desc        :", v_subcuenta_desc
         DISPLAY "Datos Siefore               :", v_siefore
         DISPLAY "Datos subtotal cuenta pesos :", v_subtotal_cuenta_pesos
         DISPLAY "Datos subtotal cuenta aivs  :", v_subtotal_cuenta_aivs
         PRINTX v_subcuenta_desc, v_siefore, v_subtotal_cuenta_pesos, v_subtotal_cuenta_aivs
         LET v_subtotal_cuenta_pesos = 0
         LET v_subtotal_cuenta_aivs  = 0

         LET v_conteo = v_conteo + 1
         
      ON EVERY ROW
         LET v_subtotal_cuenta_pesos = v_subtotal_cuenta_pesos + p_r_datos.pesos
         LET v_subtotal_cuenta_aivs  = v_subtotal_cuenta_aivs  + p_r_datos.aivs
         LET v_grupo_desc            = p_r_datos.desc_regimen
         LET v_subcuenta_desc        = p_r_datos.desc_subcuenta
         LET v_siefore               = p_r_datos.siefore
         DISPLAY "Detalle  :", p_r_datos.*
         PRINTX p_r_datos.*
    
END REPORT

