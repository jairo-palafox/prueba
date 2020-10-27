DATABASE safre_viv

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

MAIN  
   DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
   DEFINE p_folio                 INTEGER
   DEFINE p_tabla                 STRING   -- nombre de la tabla en cadena
   DEFINE p_usuario_cod           STRING   -- usuario que emite el reporte
   DEFINE p_pid                   DECIMAL(9,0) -- pid del proceso
   DEFINE p_proceso_cod           SMALLINT
   DEFINE p_opera_cod             SMALLINT
   DEFINE p_programa_cod          VARCHAR(10)

   
   LET p_folio          = ARG_VAL(1)
   LET p_tabla          = ARG_VAL(2)
   LET p_usuario_cod    = ARG_VAL(3)
   LET p_pid            = ARG_VAL(4)
   LET p_proceso_cod    = ARG_VAL(5)
   LET p_opera_cod      = ARG_VAL(6)
   LET p_programa_cod   = ARG_VAL(7)

   CALL fn_reporte_liquidacion(p_folio, p_tabla, p_usuario_cod, p_pid, p_proceso_cod, p_opera_cod, p_programa_cod, FALSE)

END MAIN 

-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de preliquidacion y liquidacion
FUNCTION fn_reporte_liquidacion(p_folio, p_tabla, p_usuario_cod, p_pid, p_proceso_cod, p_opera_cod, p_programa_cod, p_b_despliegue_pantalla)
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE p_folio                 INTEGER
    DEFINE p_tabla                 STRING   -- nombre de la tabla en cadena
    DEFINE p_usuario_cod           STRING   -- usuario que emite el reporte
    DEFINE p_pid                   DECIMAL(9,0) -- pid del proceso
    DEFINE p_proceso_cod           SMALLINT
    DEFINE p_opera_cod             SMALLINT
    DEFINE p_programa_cod          VARCHAR(10)

    DEFINE v_qry_string            STRING
    DEFINE v_i                     INTEGER
    DEFINE v_j                     INTEGER
    DEFINE v_k                     INTEGER
    DEFINE v_contador              SMALLINT
    DEFINE cont_arbol              INTEGER
    DEFINE v_origen_datos          STRING
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
    DEFINE v_nombre_usuario     CHAR (40)
    DEFINE v_usuario_cod_temp   CHAR (20)
    DEFINE v_ruta_bin           CHAR(40) -- ruta del directorio glo/bin
    DEFINE v_ruta_4rp           STRING   -- ruta donde se encuentra el archivop 4rp

    LET v_usuario_cod_temp = p_usuario_cod

    SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
     WHERE usuario_cod = v_usuario_cod_temp

    -- para los casos en que la tabla es del tipo de fondo72
    IF ( (p_proceso_cod = 1503) OR -- retiro Fondo ahorro WS
         (p_proceso_cod = 1406) OR  -- registro de pagos fondo anterior
         (p_proceso_cod = 1515) OR  -- Retiro Fondo de ahorro contingente
         (p_proceso_cod = 2235) OR  -- Retiro Fondo de ahorro contingente
         (p_proceso_cod = 1810) OR  -- Aclaraciones datos derechohabientes Fondo72
         (p_proceso_cod = 2308) OR  -- Unificación Fondo 72
         (p_proceso_cod = 1523) OR  --retiro masivo fondo 72
         (p_proceso_cod = 1525) OR  --rechazo SIAFF
         (p_proceso_cod = 1534) OR  --restitución de fondo ahorro rechazo banco
         (p_proceso_cod = 1542)     --restitución de fondo ahorro rechazo FICO
          
       ) THEN 
       -- se obtienen los datos del grupo de subcuenta
       LET v_qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga, ",
                               " sum(dp.importe), ",
                               " 0",
                          " FROM ",p_tabla.trim()," dp, ",
                               " cat_grp_subcta_regimen tasr, ",
                               " cat_grupo_regimen tgr ",
                         " WHERE dp.folio_liquida = ? ",
                           " AND tasr.subcuenta = dp.subcuenta ",
                           " AND tgr.grupo_regimen = tasr.grupo_regimen ",
                         " GROUP BY 1,2 ",
                         " ORDER BY 1 "
       --DISPLAY "v_qry_string 1 ", v_qry_string
       PREPARE sid_grupo1 FROM v_qry_string
       DECLARE cur_grupo1 CURSOR FOR sid_grupo1
       
       -- se obtienen las subcuentas del grupo
       LET v_qry_string = " SELECT ts.subcuenta, ts.subcuenta||' '||ts.subcuenta_desc, ",
                               " 11, sum(dp.importe), ",
                               " 0 ",
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
       PREPARE sid_subcuenta1 FROM v_qry_string
       DECLARE cur_subcuenta1 CURSOR FOR sid_subcuenta1
       
       -- se consturye la consulta de movimientos de la subcuenta
       LET v_qry_string = " SELECT tm.movimiento, tm.movimiento||' '||tm.movimiento_desc, ",
                               " sum(dp.importe), ",
                               " 0 ",
                          " FROM ",p_tabla.trim()," dp, ",
                               " cat_movimiento tm ",
                         " WHERE dp.folio_liquida = ? " ,
                           " AND dp.subcuenta = ?",
                       --    " AND dp.fondo_inversion = ? ",
                           " AND tm.movimiento = dp.movimiento ",
                         " GROUP BY 1,2 ",
                         " ORDER BY 1 "
       --DISPLAY "v_qry_string 3", v_qry_string
       PREPARE sid_movimiento1 FROM v_qry_string
       DECLARE cur_movimiento1 CURSOR FOR sid_movimiento1
    ELSE 
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
    END IF 
    

    LET v_i          = 1
    LET v_j          = 1
    LET v_k          = 1
    LET cont_arbol = 1

    LET v_contador = 0 -- contador para el arreglo de datos
    
    CALL arr_arbol.clear()
    -- procesos que no tienen acciones y usan fondo72
    IF ( p_proceso_cod = 1503 OR 
         p_proceso_cod = 1406 OR 
         p_proceso_cod = 2308 OR 
         p_proceso_cod = 2235 OR 
         p_proceso_cod = 1515 OR 
         p_proceso_cod = 1810 OR
         p_proceso_cod = 1523 OR
         p_proceso_cod = 1525 OR
         p_proceso_cod = 1534 OR  
         p_proceso_cod = 1542 ) THEN 
       FOREACH cur_grupo1 USING p_folio
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
       
           FOREACH cur_subcuenta1 USING p_folio,
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
             
               FOREACH cur_movimiento1 USING p_folio,
                                        arr_subcuenta[v_j].subcuenta
                                      --  ,arr_subcuenta[v_j].siefore
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
               CLOSE cur_movimiento1
               LET v_j = v_j + 1
           END FOREACH
           CLOSE cur_subcuenta1
           LET v_i = v_i + 1
       END FOREACH
       CLOSE cur_grupo1
    ELSE
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
    END IF 

    DISPLAY "tabla ", p_tabla 
    -- se determina el origen de los datos
    IF (( p_tabla.trim() = "cta_movimiento" ) 
    OR (p_tabla.trim() = "cta_decreto" ) 
    OR (p_tabla.trim() = "cta_fondo72") 
    OR (p_tabla.trim() = "safre_tmp:tmp_uni_150_02_mov_final") ) THEN
       LET v_origen_datos = "Liquidación"
    ELSE
       LET v_origen_datos = "Preliquidación"
    END IF

    -- origen de los datos
    --DISPLAY "Origen de los datos: ", v_origen_datos
    
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

    -- se obtiene la ruta bin de glo
    SELECT ruta_bin
    INTO   v_ruta_bin
    FROM   seg_modulo
    WHERE  modulo_cod = "uni"
    
    -- se conforma la ruta del archivo 4rp
    LET v_ruta_4rp = v_ruta_bin CLIPPED, "/GLOI01.4rp"
    --DISPLAY "Ruta 4rp: ", v_ruta_4rp
    
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
    -- se inicia el reporte
    START REPORT rpt_liquidacion TO XML HANDLER report_handler
    --START REPORT rpt_liquidacion TO screen
       LET p_r_encabezado.p_folio       = p_folio
       LET p_r_encabezado.p_usuario_cod = p_usuario_cod

       -- se obtiene la fecha de liquidacion/preliquidacion
       LET v_qry_string = " SELECT DISTINCT f_liquida\n",
                          " FROM ",p_tabla.trim()," \n",
                          " WHERE folio_liquida = ? \n"

       PREPARE sid_fliquida FROM v_qry_string
       EXECUTE sid_fliquida USING p_folio
                             INTO p_r_encabezado.p_fecha

       --DISPLAY " v_contador",v_contador
       DISPLAY ":1:"
       IF p_proceso_cod = 2314 THEN
          FOR v_contador = 1 TO arr_datos.getLength()
          OUTPUT TO REPORT rpt_liquidacion(v_origen_datos, p_r_encabezado.*, arr_datos[v_contador].*, v_nombre_usuario)
          END FOR
       ELSE
          --DISPLAY "arr_datos.getLength():",arr_datos.getLength()
          FOR v_contador = 1 TO arr_datos.getLength()
             --DISPLAY arr_datos[v_contador].*
             display "  ",v_origen_datos
             OUTPUT TO REPORT rpt_liquidacion(v_origen_datos, p_r_encabezado.*, arr_datos[v_contador].*, v_nombre_usuario)
          END FOR
       END IF
       
       DISPLAY ":2:"
    FINISH REPORT rpt_liquidacion
    DISPLAY ":3:"
    
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_liquidacion(p_origen_datos, p_r_encabezado, p_r_datos, p_nombre_usuario)
DEFINE p_origen_datos            STRING -- preliquidacion, liquidacion
DEFINE p_r_encabezado    RECORD
        p_folio               INTEGER,
        p_usuario_cod         STRING,
        p_fecha               DATE -- fecha de liquidacion/preliquidacion
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
         --DISPLAY "Origen datos en reporte: ", p_origen_datos
         -- se despliegan los datos del encabezado
         PRINTX p_origen_datos, p_r_encabezado.*
         LET v_subtotal_cuenta_pesos      = 0
         LET v_subtotal_cuenta_aivs       = 0
         LET v_subtotal_grupo_pesos       = 0
         LET v_subtotal_grupo_aivs        = 0
         LET v_subtotal_movimientos_pesos = 0
         LET v_subtotal_movimientos_aivs  = 0
         LET v_conteo                     = 0 -- conteo de grupos
         PRINTX p_nombre_usuario

      AFTER GROUP OF p_r_datos.grupo_regimen
         PRINTX v_grupo_desc, v_subtotal_grupo_pesos, v_subtotal_grupo_aivs

      AFTER GROUP OF p_r_datos.subcuenta
         LET v_subtotal_grupo_pesos  = v_subtotal_grupo_pesos + v_subtotal_cuenta_pesos
         LET v_subtotal_grupo_aivs   = v_subtotal_grupo_aivs  + v_subtotal_cuenta_aivs

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
         PRINTX p_r_datos.*
    
END REPORT
