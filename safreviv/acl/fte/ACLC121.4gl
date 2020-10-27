
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

-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de preliquidacion y liquidacion
FUNCTION fn_reporte_liquidacion_acl_manual(p_folio, p_id_referencia, p_tabla, p_usuario_cod, p_b_despliegue_pantalla)
    DEFINE p_folio                 INTEGER
    DEFINE p_id_referencia    DECIMAL(9,0)
    DEFINE p_tabla                 STRING -- nombre de la tabla en cadena
    DEFINE p_usuario_cod           STRING  -- usuario que emite el reporte
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE v_qry_string            STRING
    DEFINE v_i                     INTEGER
    DEFINE v_j                     INTEGER
    DEFINE v_k                     INTEGER
    DEFINE v_contador              SMALLINT
    DEFINE cont_arbol              INTEGER
    DEFINE v_origen_datos          STRING
    

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

    DISPLAY "*******"
    DISPLAY p_folio
    DISPLAY p_id_referencia
    DISPLAY "*******"
    -- se obtienen los datos del grupo de subcuenta
    LET v_qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga, ",
                            " sum(dp.monto_pesos), ",
                            " sum(dp.monto_acciones)",
                       " FROM ",p_tabla.trim()," dp, ",
                            " cat_grp_subcta_regimen tasr, ",
                            " cat_grupo_regimen tgr ",
                      " WHERE dp.folio_liquida = ? ",
                      "   AND dp.id_referencia = ? ",
                        " AND tasr.subcuenta = dp.subcuenta ",
                        " AND tgr.grupo_regimen = tasr.grupo_regimen ",
                      " GROUP BY 1,2 ",
                      " ORDER BY 1 "
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
                      "   AND dp.id_referencia = ? ",
                        " AND tasr.subcuenta = dp.subcuenta ",
                        " AND tasr.grupo_regimen = ? ", 
                        " AND ts.subcuenta = tasr.subcuenta ",
                      " GROUP BY 1,2,3 ",
                      " ORDER BY 1,3"
    PREPARE sid_subcuenta FROM v_qry_string
    DECLARE cur_subcuenta CURSOR FOR sid_subcuenta

    -- se consturye la consulta de movimientos de la subcuenta
    LET v_qry_string = " SELECT tm.movimiento, tm.movimiento||' '||tm.movimiento_desc, ",
                            " sum(dp.monto_pesos), ",
                            " sum(dp.monto_acciones) ",
                       " FROM ",p_tabla.trim()," dp, ",
                            " cat_movimiento tm ",
                      " WHERE dp.folio_liquida = ? " ,
                      "   AND dp.id_referencia = ? ",
                        " AND dp.subcuenta = ?",
                        " AND dp.fondo_inversion = ? ",
                        " AND tm.movimiento = dp.movimiento ",
                      " GROUP BY 1,2 ",
                      " ORDER BY 1 "
    PREPARE sid_movimiento FROM v_qry_string
    DECLARE cur_movimiento CURSOR FOR sid_movimiento

    LET v_i          = 1
    LET v_j          = 1
    LET v_k          = 1
    LET cont_arbol = 1

    LET v_contador = 0 -- contador para el arreglo de datos
    
    CALL arr_arbol.clear()

    FOREACH cur_grupo USING p_folio, p_id_referencia
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

        FOREACH cur_subcuenta USING p_folio, p_id_referencia,
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

            FOREACH cur_movimiento USING p_folio, p_id_referencia,
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

    -- se determina el origen de los datos
    IF ( p_tabla.trim() = "cta_movimiento" OR p_tabla.trim() = "cta_decreto" ) THEN
       LET v_origen_datos = "Liquidación"
    ELSE
       LET v_origen_datos = "Preliquidación"
    END IF

    DISPLAY "Origen de los datos: ", v_origen_datos
    
    -- se obtiene la ruta de los listados
    CALL fn_rutas("bat") RETURNING v_ruta_ejecutable, v_ruta_listados

    -- se construye la ruta del archivo
    LET v_ruta_reporte = v_ruta_listados.trim() , "/" , v_origen_datos.trim() ,
                         "_" , p_folio USING "&&&&&&", ".pdf"

    -- se indica que el reporte usara la plantilla creada
    IF ( fgl_report_loadCurrentSettings("rpt_liquida_acl_manual.4rp") ) THEN  -- if  the file loaded OK

       -- si no se pidio el reporte en pantalla
       IF ( NOT p_b_despliegue_pantalla ) THEN
          -- sin preview
          CALL fgl_report_selectPreview(0)
          -- se indica que se escriba en archivo
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
       END IF
       

       LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
    ELSE
       DISPLAY "no funciono"
       EXIT PROGRAM
    END IF

    
    -- se inicia el reporte
    START REPORT rpt_liquidacion_acl_manual TO XML HANDLER report_handler
    --START REPORT rpt_liquidacion_acl_manual TO screen

    LET p_r_encabezado.p_folio       = p_folio
    LET p_r_encabezado.p_usuario_cod = p_usuario_cod

    -- se obtiene la fecha de liquidacion/preliquidacion
    {
    LET v_qry_string = " SELECT DISTINCT f_liquida\n",
                       " FROM ",p_tabla.trim()," \n",
                      " WHERE folio_liquida = ? \n"

    DISPLAY v_qry_string
                      
    PREPARE sid_fliquida FROM v_qry_string
    EXECUTE sid_fliquida USING p_folio
    INTO p_r_encabezado.p_fecha
    }
    LET p_r_encabezado.p_fecha = TODAY
    
    DISPLAY p_folio
    DISPLAY p_r_encabezado.p_fecha
    FOR v_contador = 1 TO arr_datos.getLength()
       DISPLAY "1: ",v_origen_datos
       DISPLAY "2: ",p_r_encabezado.*
       DISPLAY "3: ",arr_datos[v_contador].*
       
       OUTPUT TO REPORT rpt_liquidacion_acl_manual(v_origen_datos, p_r_encabezado.*, arr_datos[v_contador].*)
    END FOR

    FINISH REPORT rpt_liquidacion_acl_manual
    
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_liquidacion_acl_manual(p_origen_datos, p_r_encabezado, p_r_datos)
    {DEFINE p_folio                   INTEGER
    DEFINE v_tabla                   STRING
    DEFINE p_usuario_cod             STRING  -- usuario que emite el reporte
    DEFINE p_b_despliegue_pantalla   SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE v_qry_string              STRING
    DEFINE v_i                       INTEGER
    DEFINE v_j                       INTEGER
    DEFINE v_k                       INTEGER}
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
    
   FORMAT

      FIRST PAGE HEADER
         DISPLAY "Origen datos en reporte: ", p_origen_datos
         -- se despliegan los datos del encabezado
         PRINTX p_origen_datos, p_r_encabezado.*
         LET v_subtotal_cuenta_pesos      = 0
         LET v_subtotal_cuenta_aivs       = 0
         LET v_subtotal_grupo_pesos       = 0
         LET v_subtotal_grupo_aivs        = 0
         LET v_subtotal_movimientos_pesos = 0
         LET v_subtotal_movimientos_aivs  = 0
         LET v_conteo                     = 0 -- conteo de grupos

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
