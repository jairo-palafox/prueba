            
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
            
DEFINE w               ui.Window
DEFINE f               ui.Form

FUNCTION fn_liquida_fondo72fa(p_usuario, p_proceso_cod, p_opera_cod, p_operacion)
   DEFINE p_usuario           CHAR(20)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_tabla_preliq      CHAR(30)
   DEFINE p_operacion         SMALLINT --1 CONSULTA; 2 LIQUIDA
            
   DATABASE safre_viv
   SELECT nombre_tabla
     INTO v_tabla_preliq
     FROM cat_preliquida
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod
            
      IF(p_proceso_cod  = 1503 OR p_proceso_cod  = 1515 OR p_proceso_cod  = 1518) THEN
         CALL fn_folio_ahorro_liquidar(p_usuario, p_proceso_cod, p_opera_cod, v_tabla_preliq)
      ELSE
         CALL fn_folio_liquidar(p_usuario, p_proceso_cod, p_opera_cod, v_tabla_preliq)
      END IF 
   
END FUNCTION

FUNCTION fn_folio_ahorro_liquidar(p_usuario, p_proceso_cod, p_opera_cod, v_tabla)
   DEFINE p_usuario           CHAR(20)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_bandera           SMALLINT
   DEFINE v_tabla             STRING
   DEFINE v_query             STRING
   DEFINE i                   SMALLINT
   DEFINE v_folio             DECIMAL(9,0)
   DEFINE v_ruta_bin          CHAR(40)
   DEFINE arr_folios_liquidar DYNAMIC ARRAY OF RECORD
             folio              DECIMAL(9,0),
             fecha_liquidacion  DATE,
             fecha_proceso      DATE,
             activa             SMALLINT
   END RECORD
   DEFINE v_r_liquida         RECORD
      folio_liquida              DECIMAL(9,0), 
      f_liquida                  DATE        ,
      f_registro                 DATE
   END RECORD
            
   CALL arr_arbol.clear()
   CALL arr_folios.clear()
            
   CREATE TEMP TABLE tmp_folios_consulta (
               folio         DECIMAL (9,0),
               fecha_liq     DATE,
               fecha_proceso DATE)
            

   DECLARE cur_glo_folio CURSOR FOR SELECT a.folio
                                      FROM glo_folio a
                                     WHERE a.proceso_cod = p_proceso_cod
                                     --  AND a.status      = 0
                                     AND a.status      = 1
   LET v_query = "SELECT a.folio_liquida, a.f_liquida, a.f_registro ",
                 "  FROM ", v_tabla.trim() , " a ",
                 " WHERE a.folio_liquida = ? "

   --DISPLAY v_query
                 
   PREPARE prp_folio_preliq FROM v_query
   DECLARE cur_folio_preliq CURSOR FOR prp_folio_preliq 

   FOREACH cur_glo_folio INTO v_folio
      FOREACH cur_folio_preliq USING v_folio INTO v_r_liquida.*
          INSERT INTO tmp_folios_consulta VALUES(v_r_liquida.*)
          EXIT FOREACH
      END FOREACH
      CLOSE cur_folio_preliq
   END FOREACH
   FREE cur_folio_preliq
   CLOSE cur_glo_folio
   FREE cur_glo_folio
  
   DECLARE cur_folio_liquidar CURSOR FOR SELECT a.*
                                  FROM tmp_folios_consulta a, 
                                       glo_folio b
                                 WHERE a.folio = b.folio
                                   AND b.proceso_cod = p_proceso_cod
            
   LET i = 1
   FOREACH cur_folio_liquidar INTO arr_folios_liquidar[i].*
      LET arr_folios_liquidar[i].activa = 0
      LET i = i + 1
   END FOREACH
   FREE cur_folio_liquidar
            
   IF arr_folios_liquidar[arr_folios_liquidar.getLength()].folio IS NULL THEN
      CALL arr_folios_liquidar.deleteElement(arr_folios_liquidar.getLength())
   END IF   
            
   SELECT ruta_bin
     INTO v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'glo'
            
   OPEN WINDOW retg022 WITH FORM v_ruta_bin CLIPPED|| "/GLOG022"
            
   IF i > 1 THEN
      LET w = ui.Window.getCurrent() -- asigna a "w" la ventana activa
      LET f = w.getForm()
            
      LET INT_FLAG = FALSE
            
      DIALOG ATTRIBUTES(UNBUFFERED)
         INPUT ARRAY arr_folios_liquidar FROM arr_folios.* ATTRIBUTES(
                                                APPEND ROW = FALSE,
                                                INSERT ROW = FALSE,
                                                DELETE ROW = FALSE,
                                                AUTO APPEND= FALSE)
            BEFORE INPUT
               CALL DIALOG.setactionhidden("close",1)
               CALL fn_despliega_desc(p_proceso_cod, p_opera_cod)
               
            BEFORE ROW
               LET i       = ARR_CURR()
               LET v_folio = arr_folios_liquidar[i].folio
               --DISPLAY "BEFORE ROW ",v_folio," ", v_tabla
               --CALL fn_llena_arbol_montos(v_folio, v_tabla)
               CALL fn_llena_arbol_montos_fondo72fa(v_folio, v_tabla)
               CALL ui.Interface.refresh()
               DISPLAY arr_folios_liquidar[i].folio TO folio_liquida
               DISPLAY arr_folios_liquidar[i].fecha_proceso TO f_registro
               DISPLAY arr_folios_liquidar[i].fecha_liquidacion TO f_liquida
               
         END INPUT
            
         DISPLAY ARRAY arr_arbol TO scr1.*
            
         END DISPLAY
            
         ON ACTION ACCEPT
            LET INT_FLAG = FALSE
            ACCEPT DIALOG
            
         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            EXIT DIALOG
      END DIALOG
    
      IF NOT INT_FLAG THEN
         FOR i = 1 TO arr_folios_liquidar.getLength() 
            IF arr_folios_liquidar[i].folio IS NOT NULL AND
               arr_folios_liquidar[i].activa = 1 THEN
               CALL fn_ventana_confirma("Liquidación",
                               "Desea liquidar el folio: "||arr_folios_liquidar[i].folio ,
                               "") RETURNING v_bandera
               IF v_bandera THEN
                  CALL fn_ejecuta_liquidacion_fondo72(arr_folios_liquidar[i].folio, 
                                              p_proceso_cod,
                                              p_opera_cod,
                                              v_tabla,
                                              p_usuario)
               ELSE
                  CALL fn_mensaje("Liquidación",
                               "Se cancelo la liquidación del folio: "||arr_folios_liquidar[i].folio ,
                               "")
               END IF
            END IF
         END FOR
      END IF
   ELSE     
      CALL fn_mensaje("Liquidación",
                      "No existen folios a liquidar.",
                      "")
            
   END IF   
   CLOSE WINDOW retg022
END FUNCTION

FUNCTION fn_ejecuta_liquidacion_fondo72(p_folio, p_proceso_cod, p_opera_cod, v_tabla, p_usuario)
   DEFINE p_folio             DECIMAL(9,0)
   DEFINE p_usuario           CHAR(20)
   DEFINE v_pid                 DECIMAL(9,0)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_tabla             STRING
   DEFINE v_comando           STRING
   DEFINE l_bat_ruta_listado  CHAR(40)
   DEFINE v_ruta_glo          CHAR(40)
            
            
   SELECT pid
     INTO v_pid
     FROM bat_ctr_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod - 1
      AND folio = p_folio
            
   IF v_pid IS NULL OR v_pid = 0 THEN
      SELECT MAX(pid)
        INTO v_pid
        FROM bat_ctr_proceso
       WHERE proceso_cod = p_proceso_cod
         AND estado_cod  = 2
   END IF   
            
   SELECT ruta_listados
     INTO l_bat_ruta_listado
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
            
   SELECT ruta_bin
     INTO v_ruta_glo
     FROM seg_modulo
    WHERE modulo_cod = 'ret'
            
   CALL fn_inserta_operacion(v_pid,p_proceso_cod,p_opera_cod,p_folio,
                             "GLOG03","",p_usuario)
                              
   LET v_comando = "nohup fglrun ",v_ruta_glo CLIPPED,"/RETG03 ",
                    p_usuario CLIPPED," ",
                    v_pid, " ",
                    p_proceso_cod, " ",
                    p_opera_cod, " ",
                    p_folio, " ",
                    v_tabla CLIPPED,
                    " 1>", l_bat_ruta_listado CLIPPED ,
                    "/nohup:",v_pid         USING "&&&&&",":",
                              p_proceso_cod USING "&&&&&",":",
                              p_opera_cod   USING "&&&&&" ,
                              " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
            
   CALL fn_mensaje("Atención","Se ha enviado la liquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","")
            
END FUNCTION

FUNCTION fn_llena_arbol_montos_fondo72fa(p_folio, v_tabla)
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

    LET qry_string = "SET PDQPRIORITY HIGH"
    PREPARE prp_high_pdq FROM qry_string 

    LET qry_string = "SET PDQPRIORITY DEFAULT"
    PREPARE prp_default_pdq FROM qry_string 
    
    LET qry_string = " SELECT tgr.grupo_regimen, tgr.desc_larga, ",
                            " sum(dp.importe), ",
                            "0", --" sum(dp.monto_acciones)",
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
            
    LET qry_string = " SELECT ts.subcuenta, ts.subcuenta||'-'||ts.subcuenta_desc, ",
                            " 11, sum(dp.importe), ",
                            "0", --" sum(dp.monto_acciones) ",
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
            
    LET qry_string = " SELECT tm.movimiento, tm.movimiento||'-'||tm.movimiento_desc, ",
                            " sum(dp.importe), ",
                            "0", --" sum(dp.monto_acciones) ",
                       " FROM ",v_tabla.trim()," dp, ",
                            " cat_movimiento tm ",
                      " WHERE dp.folio_liquida = ? " ,
                        " AND dp.subcuenta = ?",
                        --" AND dp.fondo_inversion = 11 ",
                        " AND tm.movimiento = dp.movimiento ",
                      " GROUP BY 1,2 ",
                      " ORDER BY 1 "
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
                                     arr_nivel2[j].subcuenta
                                --     arr_nivel2[j].siefore
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

#Objetivo: Realiza el reverso de la liquidacion fondo72
FUNCTION fn_reverso_liquidacion_fondo72(p_folio)
DEFINE p_folio       DECIMAL(9,0)
      ,r_sql_reverso SMALLINT
      ,r_rev_cnt     SMALLINT 

   LET r_sql_reverso = 0
   # Ejecuta el SP que realiza el reverso
   PREPARE prp_fn_reverso_liquidacion FROM "EXECUTE FUNCTION fn_reverso_liquidacion_fondo72(?)"
   EXECUTE prp_fn_reverso_liquidacion USING p_folio
                                       INTO r_sql_reverso

   CALL fn_reverso_reg_cnt(p_folio) RETURNING r_rev_cnt

   RETURN r_sql_reverso 
END FUNCTION

FUNCTION fn_valida_reverso_fondo72(p_pid,p_proceso_cod,p_opera_cod)
DEFINE 
   p_pid         DECIMAL(9,0),
   p_proceso_cod SMALLINT    ,
   p_opera_cod   SMALLINT    , 
   r_bandera     SMALLINT    ,
   v_query       STRING
   --Se prepara la funcion para ser ejecutada
   LET v_query = "EXECUTE FUNCTION fn_valida_reverso_fondo72(?,?,?)"
   PREPARE prp_fn_valida_reverso FROM v_query
   EXECUTE prp_fn_valida_reverso USING p_pid
                                      ,p_proceso_cod
                                      ,p_opera_cod
                                 INTO r_bandera
   RETURN r_bandera
END FUNCTION