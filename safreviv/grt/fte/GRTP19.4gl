--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:26/04/2012
--===============================================================

############################################################################
#Modulo            =>GRT                                                   #
#Programa          =>GRTP19                                                #
#Objetivo          =>Programa que genera el reporte de preliquidación  y   #
#                    liquidacion de uso en garantía                        # 
############################################################################
DATABASE safre_viv

DEFINE p_v_usuario           LIKE seg_usuario.usuario, -- nombre del usuario          
       p_i_proceso_cod       LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_i_opera_cod         LIKE cat_operacion.opera_cod, -- codigo de la operacion
       p_d_folio             LIKE glo_ctr_archivo.folio, -- numero de folio
       p_s_tabla             STRING,
       p_v_pid               LIKE bat_ctr_proceso.pid, --  ID del proceso
       v_c_programa_cod      LIKE cat_operacion.programa_cod --nombre del programa que lanza el reporte

    -- Variables para cifras de control de pago a EF
    TYPE rec_g_pago             RECORD
       monto_facturado     DECIMAL(12,2),
       total_facturado     INTEGER,
       monto_devolucion    DECIMAL(12,2),
       total_devolucion    INTEGER,
       suma_monto          DECIMAL(16,2),
       suma_total          INTEGER
    END RECORD
    DEFINE r_pago_ap       rec_g_pago
    DEFINE r_pago_ug       rec_g_pago
    DEFINE r_total_global       RECORD
       monto_total         DECIMAL(16,2),
       total_registros     INTEGER
    END RECORD

# Objetivo: Conciliar la información de confirmacion de devolucion de saldos
MAIN
   DEFINE v_v_nom_reporte       VARCHAR(80), -- nombre del reporte
          v_s_mens_correo       STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo     STRING, -- contiene el titulo del correo
          v_s_archivo_correo    STRING, -- ruta y nombre del archivo adjunto en el correo
          v_c_proceso_desc      LIKE cat_proceso.proceso_desc, -- descripción del proceso
          v_c_opera_desc        LIKE cat_operacion.opera_desc, -- descripción de la operacion
          r_ruta_ejecutable     STRING, -- ruta del ejecutable
          r_ruta_listados       STRING -- ruta de los listados

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_v_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)  
   LET p_s_tabla        = ARG_VAL(6)
   --LET p_v_pid          = ARG_VAL(6)
   LET v_c_programa_cod = ARG_VAL(7)

   -- se obtienen las descripciones del proceso y la operacion
   LET v_c_proceso_desc = fn_proceso_cod_desc(p_i_proceso_cod)
   LET v_c_opera_desc   = fn_opera_cod_desc(p_i_proceso_cod, p_i_opera_cod)

   -- se obtiene la ruta de los listados
   CALL fn_rutas("grt") RETURNING r_ruta_ejecutable, r_ruta_listados

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_v_usuario CLIPPED || "-",v_c_programa_cod CLIPPED,"-",p_v_pid USING "&&&&&","-",p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   -- se invoca la función del reporte
   CALL fn_reporte_preliquidacion_grt(FALSE, r_ruta_listados, v_v_nom_reporte, v_c_opera_desc)

   -- Genera archivo de salida
   DISPLAY "GENERA ARCHIVOS DE SALIDA"
   CALL fn_archivo_salida_preliquidacion()

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: ", v_c_proceso_desc

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_v_pid,"\n",
                          "Proceso      : ",v_c_proceso_desc CLIPPED,"\n",
                          "Operacion    : ",v_c_opera_desc CLIPPED,"\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(p_v_pid,
                          p_i_proceso_cod,
                          p_i_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)

END MAIN

-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de preliquidacion y liquidacion
FUNCTION fn_reporte_preliquidacion_grt(p_b_despliegue_pantalla, p_ruta_listados, p_nom_reporte, p_c_opera_desc)
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE p_ruta_listados         STRING -- ruta de los listados
    DEFINE p_nom_reporte           VARCHAR(80) -- nombre del reporte
    DEFINE p_c_opera_desc          LIKE cat_operacion.opera_desc-- descripción del proceso
    DEFINE v_qry_string            STRING
    DEFINE v_i                     INTEGER
    DEFINE v_j                     INTEGER
    DEFINE v_k                     INTEGER
    DEFINE v_contador              SMALLINT
    DEFINE cont_arbol              INTEGER

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
    DEFINE report_handler       om.SaxDocumentHandler -- handler para el reporte en PDF
    DEFINE v_d_valor_accion     DECIMAL(28,6)  
    DEFINE v_d_monto_pesos      DECIMAL(28,6)
    DEFINE v_d_monto_aivs       DECIMAL(28,6)
    DEFINE v_tpo_originacion    SMALLINT
    DEFINE arr_nivel2 DYNAMIC ARRAY OF RECORD
        movimiento             SMALLINT,
        descripcion             CHAR(40),
        siefore                 SMALLINT,
        pesos                   DECIMAL(28,6),
        acciones                DECIMAL(28,6)
    END RECORD
    DEFINE i                    INTEGER
    DEFINE j                    INTEGER 
    DEFINE v_movimiento         LIKE cre_saldo_deudor.movimiento
    DEFINE v_r_aux_pago          RECORD
       tpo_concepto     SMALLINT,
       importe          DECIMAL(12,2),
       total            INTEGER
    END RECORD 

    LET v_tpo_originacion = 2 -- Creditos en Garantia 43 bis
    LET v_d_valor_accion = 0 

    SELECT precio_fondo
      INTO v_d_valor_accion
      FROM glo_valor_fondo
     WHERE fondo = 11
       AND f_valuacion = TODAY
   
    -- se obtienen los datos del grupo de subcuentas
    LET v_qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga,",
                       "        SUM(dp.monto_pesos), ",
                       "        SUM(dp.monto_acciones)",
                       "   FROM ",p_s_tabla.trim()," dp, ",
                       "        cat_grp_subcta_regimen tasr, ",
                       "        cat_grupo_regimen tgr ",
                       "  WHERE dp.folio_liquida = ? ",
                       "    AND tasr.subcuenta = dp.subcuenta ",
                       "    AND tgr.grupo_regimen = tasr.grupo_regimen ",
                       "  GROUP BY 1,2 ",
                       "  ORDER BY 1 "

                       DISPLAY "consulta : ",v_qry_string
    PREPARE sid_grupo FROM v_qry_string
    DECLARE cur_grupo CURSOR FOR sid_grupo

    -- se obtienen las subcuentas del grupo
    LET v_qry_string = " SELECT ts.subcuenta, ts.subcuenta||' '||ts.subcuenta_desc, ",
                            " dp.fondo_inversion, sum(dp.monto_pesos), ",
                            " sum(dp.monto_acciones) ",
                       " FROM ",p_s_tabla.trim()," dp, ",
                            " cat_grp_subcta_regimen tasr, ",
                            " cat_subcuenta ts ",
                      " WHERE dp.folio_liquida = ? ",
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
                       " FROM ",p_s_tabla.trim()," dp, ",
                            " cat_movimiento tm ",
                      " WHERE dp.folio_liquida = ? " ,
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
    LET v_d_monto_pesos = 0
    LET v_d_monto_aivs = 0

    LET v_contador = 0 -- contador para el arreglo de datos
    
    CALL arr_arbol.clear()

    FOREACH cur_grupo USING p_d_folio
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

        
        FOREACH cur_subcuenta USING p_d_folio,
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

            
            FOREACH cur_movimiento USING p_d_folio,
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

    ##############################
    #####    SALDO DEUDOR    #####
    ##############################
    LET i          = 1
    LET j          = 1

    -- se obtienen los movimientos y las descripciones de saldo deudor
     LET v_qry_string =" SELECT UNIQUE sd.movimiento, sd.movimiento|| ' '||mov.movimiento_desc ",
                       "   FROM cre_saldo_deudor sd, cat_movimiento mov ",
                       "  WHERE sd.movimiento = mov.movimiento ",
                       "    AND sd.id_cre_acreditado IN ( ",
                       "        SELECT id_cre_acreditado ",
                       "          FROM cre_acreditado ",
                       "         WHERE folio_liquida = ",p_d_folio,") ",
                       " ORDER BY sd.movimiento"

    PREPARE prp_nivel2 FROM v_qry_string
    DECLARE cur_nivel2 CURSOR FOR prp_nivel2 

    LET arr_arbol[cont_arbol].subcuenta      = "X"
    LET arr_arbol[cont_arbol].subcuenta_desc = "DEUDOR"
    LET arr_arbol[cont_arbol].monto_pesos    = 0
    LET arr_arbol[cont_arbol].monto_acciones = 0
    LET arr_arbol[cont_arbol].id             = "X" USING "<<"
    LET arr_arbol[cont_arbol].nivel          = 1
    LET arr_arbol[cont_arbol].padre_id       = ""
    LET arr_arbol[cont_arbol].siefore        = ""
    LET cont_arbol = cont_arbol + 1
        
    FOREACH cur_nivel2 INTO arr_nivel2[j].movimiento, arr_nivel2[j].descripcion
       LET v_qry_string = " SELECT SUM(monto_aivs), SUM(monto_pesos)\n",
                          "   FROM cre_saldo_deudor\n",
                          "  WHERE id_cre_acreditado IN (\n",
                          "        SELECT id_cre_acreditado\n",
                          "          FROM cre_acreditado\n",
                          "         WHERE folio_liquida = ",p_d_folio,")\n",
                          "    AND movimiento = ",arr_nivel2[j].movimiento,"\n"

       PREPARE prp_cons_mtos FROM v_qry_string
       EXECUTE prp_cons_mtos INTO v_d_monto_aivs, v_d_monto_pesos

       DISPLAY "MONTO AIVS ",v_d_monto_aivs       
       DISPLAY "MONTO PESOS ",v_d_monto_pesos
       LET arr_arbol[cont_arbol].subcuenta      = arr_nivel2[j].movimiento
       LET arr_arbol[cont_arbol].subcuenta_desc = arr_nivel2[j].descripcion
       LET arr_arbol[cont_arbol].monto_pesos    = v_d_monto_pesos
       LET arr_arbol[cont_arbol].monto_acciones = v_d_monto_aivs
       LET arr_arbol[cont_arbol].id             = "",
                                                  arr_nivel2[j].movimiento USING"<<"
       LET arr_arbol[cont_arbol].nivel          = 2
       LET arr_arbol[cont_arbol].padre_id       = ""
       LET arr_arbol[cont_arbol].siefore        = 11
       -- se incrementa el contador de registros del arreglo de datos
          LET v_contador = v_contador + 1
          -- se asignan los datos de la subcuenta del grupo al arreglo de datos       
          LET arr_datos[v_contador].grupo_regimen     = "X"
          LET arr_datos[v_contador].desc_regimen      = "SALDO DEUDOR"
          LET arr_datos[v_contador].subcuenta         = ""--arr_arbol[cont_arbol].subcuenta
          LET arr_datos[v_contador].desc_subcuenta    = ""--arr_arbol[cont_arbol].subcuenta_desc
          LET arr_datos[v_contador].siefore           = 11
          LET arr_datos[v_contador].movimiento        = arr_arbol[cont_arbol].subcuenta 
          LET arr_datos[v_contador].desc_movimiento   = arr_arbol[cont_arbol].subcuenta_desc
          LET arr_datos[v_contador].pesos             = arr_arbol[cont_arbol].monto_pesos
          LET arr_datos[v_contador].aivs              = arr_arbol[cont_arbol].monto_acciones
            LET cont_arbol = cont_arbol + 1              
       LET j = j + 1
    END FOREACH
    CLOSE cur_nivel2
    LET i = i + 1    

    # Obtiene cifras control del pago a EF

    -- Inicializa records
    LET r_pago_ap.monto_facturado  = 0
    LET r_pago_ap.total_facturado  = 0
    LET r_pago_ap.monto_devolucion = 0
    LET r_pago_ap.total_devolucion = 0
    LET r_pago_ap.suma_monto       = 0
    LET r_pago_ap.suma_total       = 0
    LET r_pago_ug.monto_facturado  = 0
    LET r_pago_ug.total_facturado  = 0
    LET r_pago_ug.monto_devolucion = 0
    LET r_pago_ug.total_devolucion = 0
    LET r_pago_ug.suma_monto       = 0
    LET r_pago_ug.suma_total       = 0
    LET r_total_global.monto_total = 0
    LET r_total_global.total_registros = 0

    INITIALIZE v_r_aux_pago.* TO NULL

    DECLARE crs_cifras_pago CURSOR FOR
    SELECT tpo_concepto,
           importe,
           total
      FROM tmp_cifras_ef
     WHERE tpo_registro = 4;

    FOREACH crs_cifras_pago INTO v_r_aux_pago.tpo_concepto,
                                 v_r_aux_pago.importe,
                                 v_r_aux_pago.total
       -- Evalúa tipo concepto                                                        -- Devolución
       IF(v_r_aux_pago.tpo_concepto = 307) OR (v_r_aux_pago.tpo_concepto = 317) OR (v_r_aux_pago.tpo_concepto = 308) THEN

          CASE
             WHEN v_r_aux_pago.tpo_concepto = 307 OR v_r_aux_pago.tpo_concepto = 317
                LET r_pago_ap.monto_facturado = r_pago_ap.monto_facturado + v_r_aux_pago.importe
                LET r_pago_ap.total_facturado = r_pago_ap.total_facturado + v_r_aux_pago.total
             WHEN v_r_aux_pago.tpo_concepto = 308
                LET r_pago_ap.monto_devolucion = r_pago_ap.monto_devolucion + v_r_aux_pago.importe
                LET r_pago_ap.total_devolucion = r_pago_ap.total_devolucion + v_r_aux_pago.total
          END CASE

          -- Incrementa total global aportación
          LET r_pago_ap.suma_monto = r_pago_ap.suma_monto + v_r_aux_pago.importe
          LET r_pago_ap.suma_total = r_pago_ap.suma_total + v_r_aux_pago.total
       ELSE                                                                            -- Devolución
          IF(v_r_aux_pago.tpo_concepto = 407) OR (v_r_aux_pago.tpo_concepto = 417) OR (v_r_aux_pago.tpo_concepto = 408) THEN

             CASE
                WHEN v_r_aux_pago.tpo_concepto = 407 OR v_r_aux_pago.tpo_concepto = 417
                   LET r_pago_ug.monto_facturado = r_pago_ug.monto_facturado + v_r_aux_pago.importe
                   LET r_pago_ug.total_facturado = r_pago_ug.total_facturado + v_r_aux_pago.total
                WHEN v_r_aux_pago.tpo_concepto = 408
                   LET r_pago_ug.monto_devolucion = r_pago_ug.monto_devolucion + v_r_aux_pago.importe
                   LET r_pago_ug.total_devolucion = r_pago_ug.total_devolucion + v_r_aux_pago.total
             END CASE

             -- Incremente total global UG
             LET r_pago_ug.suma_monto = r_pago_ug.suma_monto + v_r_aux_pago.importe
             LET r_pago_ug.suma_total = r_pago_ug.suma_total + v_r_aux_pago.total
          END IF
       END IF
    END FOREACH

    LET r_total_global.monto_total = r_pago_ap.suma_monto + r_pago_ug.suma_monto
    LET r_total_global.total_registros = r_pago_ap.suma_total + r_pago_ug.suma_total

    -- se indica que el reporte usara la plantilla creada
    IF ( fgl_report_loadCurrentSettings("GRTP191.4rp") ) THEN  -- if  the file loaded OK

       -- si no se pidio el reporte en pantalla
       IF ( NOT p_b_despliegue_pantalla ) THEN
          -- sin preview
          CALL fgl_report_selectPreview(0)
          -- se indica que se escriba en archivo
          CALL fgl_report_setOutputFileName(p_ruta_listados CLIPPED||"/"||p_nom_reporte CLIPPED)
       ELSE 
          CALL fgl_report_selectPreview(1)
       CALL fgl_report_setOutputFileName(p_ruta_listados CLIPPED||"/"||p_nom_reporte CLIPPED)        
       END IF       

       LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
    ELSE
       DISPLAY "NO SE PUDO LEER LA PLANTILLA DEL REPORTE"
       EXIT PROGRAM
    END IF

    
    -- se inicia el reporte
    START REPORT rpt_liquidacion_acr TO XML HANDLER report_handler
    --START REPORT rpt_liquidacion TO screen

    LET p_r_encabezado.p_folio       = p_d_folio
    LET p_r_encabezado.p_usuario_cod = p_v_usuario

    -- se obtiene la fecha de liquidacion/preliquidacion
    LET v_qry_string = " SELECT DISTINCT f_liquida\n",
                       "   FROM ",p_s_tabla.trim()," \n",
                       "  WHERE folio_liquida = ? \n"

    PREPARE sid_fliquida FROM v_qry_string
    EXECUTE sid_fliquida USING p_d_folio INTO p_r_encabezado.p_fecha
    
    FOR v_contador = 1 TO arr_datos.getLength()
       --DISPLAY arr_datos[v_contador].*
       OUTPUT TO REPORT rpt_liquidacion_acr(p_c_opera_desc, p_r_encabezado.*, arr_datos[v_contador].*)
    END FOR

    FINISH REPORT rpt_liquidacion_acr
    
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_liquidacion_acr(p_c_opera_desc, p_r_encabezado, p_r_datos)
    DEFINE p_c_opera_desc    LIKE cat_operacion.opera_desc -- descripción del proceso
    DEFINE p_r_encabezado    RECORD
           p_folio           INTEGER,
           p_usuario_cod     STRING,
           p_fecha           DATE -- fecha de liquidacion/preliquidacion
    END RECORD
    DEFINE v_origen_datos    STRING -- preliquidacion, liquidacion
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
    DEFINE  v_subtotal_movimientos_pesos  DECIMAL(28,6)
    DEFINE  v_subtotal_movimientos_aivs   DECIMAL(28,6)
    DEFINE  v_grupo_desc                  STRING
    DEFINE  v_subcuenta_desc              STRING
    DEFINE  v_siefore                     STRING
    DEFINE  v_conteo                      SMALLINT
    DEFINE  v_total_pesos                 DECIMAL(28,6)
    DEFINE  v_total_aivs                  DECIMAL(28,6)
    DEFINE  v_titulo_reporte              STRING
    DEFINE  v_tipo_registro               STRING

   FORMAT   

      FIRST PAGE HEADER
         LET v_total_pesos = 0
         LET v_total_aivs = 0
         LET v_titulo_reporte = "REPORTE DE ",p_c_opera_desc
         IF p_i_opera_cod = 1 THEN
            LET v_origen_datos = "Preliquidación"
         ELSE
            LET v_origen_datos = "Liquidación"
         END IF
         
         -- se despliegan los datos del encabezado
         PRINTX v_titulo_reporte
         PRINTX v_origen_datos
         PRINTX p_r_encabezado.*
         LET v_subtotal_cuenta_pesos      = 0
         LET v_subtotal_cuenta_aivs       = 0
         LET v_subtotal_movimientos_pesos = 0
         LET v_subtotal_movimientos_aivs  = 0
         LET v_conteo                     = 0 -- conteo de grupos
         -- Cifras del Pago a EF
         PRINTX r_pago_ap.monto_facturado
         PRINTX r_pago_ap.total_facturado
         PRINTX r_pago_ap.monto_devolucion
         PRINTX r_pago_ap.total_devolucion
         PRINTX r_pago_ap.suma_monto
         PRINTX r_pago_ap.suma_total
         PRINTX r_pago_ug.monto_facturado
         PRINTX r_pago_ug.total_facturado
         PRINTX r_pago_ug.monto_devolucion
         PRINTX r_pago_ug.total_devolucion
         PRINTX r_pago_ug.suma_monto
         PRINTX r_pago_ug.suma_total
         PRINTX r_total_global.monto_total
         PRINTX r_total_global.total_registros

      AFTER GROUP OF p_r_datos.grupo_regimen
         LET v_total_pesos = GROUP SUM(p_r_datos.pesos)
         LET v_total_aivs = GROUP SUM(p_r_datos.aivs)
         PRINTX v_total_pesos,v_total_aivs
         PRINTX v_grupo_desc

      BEFORE GROUP OF p_r_datos.subcuenta
         IF p_r_datos.desc_regimen = "SALDO DEUDOR" THEN 
            LET v_tipo_registro = "Movimiento"
         ELSE 
            LET v_tipo_registro = "Subcuenta"
         END IF

      AFTER GROUP OF p_r_datos.subcuenta
         PRINTX v_subcuenta_desc, v_siefore, v_subtotal_cuenta_pesos, v_subtotal_cuenta_aivs,v_tipo_registro
         LET v_subtotal_cuenta_pesos = 0
         LET v_subtotal_cuenta_aivs  = 0
         --LET v_total_pesos = 0
         --LET v_total_aivs = 0

         LET v_conteo = v_conteo + 1
         
      ON EVERY ROW         
         LET v_subtotal_cuenta_pesos = v_subtotal_cuenta_pesos + p_r_datos.pesos
         LET v_subtotal_cuenta_aivs  = v_subtotal_cuenta_aivs  + p_r_datos.aivs
         LET v_grupo_desc            = p_r_datos.desc_regimen
         LET v_subcuenta_desc        = p_r_datos.desc_subcuenta
         LET v_siefore               = p_r_datos.siefore
         PRINTX p_r_datos.*

     --ON LAST ROW 
     --    PRINTX v_total_pesos,v_total_aivs
        
END REPORT

FUNCTION fn_archivo_salida_preliquidacion()

   DEFINE v_ruta_envio       CHAR(40)
   DEFINE v_arh_devolucion   STRING
   DEFINE v_arh_cartera      STRING
   DEFINE v_s_qry            STRING
   DEFINE obj_dev            base.channel
   DEFINE obj_cartera        base.channel
   DEFINE r_devolucion       RECORD
     nss            CHAR(11),
     periodo_pago   CHAR(6),
     importe        CHAR(15) 
   END RECORD
   DEFINE r_cartera           RECORD
      nss          CHAR(11),
      periodo_pago CHAR(6),
      pesos        CHAR(25)
   END RECORD 
   DEFINE v_cadena            STRING

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'grt'

   --> Genera archivo de Devoluciones
   
   LET v_arh_devolucion = v_ruta_envio CLIPPED,"/dev_preliq_43bis_",TODAY USING "yyyymmdd",".txt"

   LET obj_dev = base.Channel.create()
   CALL obj_dev.openFile(v_arh_devolucion,"w")
   CALL obj_dev.setDelimiter("|")
   
   LET v_s_qry = "SELECT nss,
                      \n periodo_pago,
                      \n importe
                 \n FROM tmp_reg_dev_ug" 

   INITIALIZE r_devolucion.* TO NULL
   LET v_cadena = NULL

   PREPARE prp_devoluciones FROM v_s_qry
   DECLARE crs_devoluciones CURSOR FOR prp_devoluciones

   FOREACH crs_devoluciones INTO r_devolucion.nss,
                                 r_devolucion.periodo_pago,
                                 r_devolucion.importe

      LET v_cadena = r_devolucion.nss CLIPPED,
                     r_devolucion.periodo_pago CLIPPED,
                     r_devolucion.importe CLIPPED

      CALL obj_dev.writeLine(v_cadena)

   END FOREACH

   FREE crs_devoluciones
   CALL obj_dev.close()

   DISPLAY ""
   DISPLAY "Archivo detalle de Devoluciones: ",v_arh_devolucion

   --> Genera archivo Cartera
   LET v_arh_cartera = v_ruta_envio CLIPPED,"/cartera_preliq_43bis_",TODAY USING "YYYYmmdd",".txt"

   LET obj_cartera = base.Channel.create()
   CALL obj_cartera.openFile(v_arh_cartera,"w")
   CALL obj_cartera.setDelimiter("|")
   
   LET v_s_qry = "SELECT f.nss,
                      \n c.periodo_pago,
                      \n m.monto_pesos
                 \n FROM cre_ug_preliquida m,
                      \n cre_uso_garantia c,
                      \n afi_derechohabiente f
                \n WHERE m.folio_liquida      = c.folio_liquida
                  \n AND m.id_derechohabiente = c.id_derechohabiente
                  \n AND c.id_derechohabiente = f.id_derechohabiente
                  \n AND m.id_referencia      = c.periodo_pago
                  \n AND c.tpo_transferencia  = '48'"

   INITIALIZE r_cartera.* TO NULL
   LET v_cadena = NULL

   PREPARE prp_cartera FROM v_s_qry
   DECLARE crs_cartera CURSOR FOR prp_cartera

   FOREACH crs_cartera INTO r_cartera.nss,
                            r_cartera.periodo_pago,
                            r_cartera.pesos

      LET v_cadena = r_cartera.nss CLIPPED,
                     r_cartera.periodo_pago CLIPPED,
                     r_cartera.pesos CLIPPED

      CALL obj_cartera.writeLine(v_cadena)

   END FOREACH

   FREE crs_cartera
   CALL obj_cartera.close()

   DISPLAY "Archivo detalle a Cartera: ",v_arh_cartera
   DISPLAY ""

END FUNCTION
