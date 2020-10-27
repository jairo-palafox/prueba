################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 19/03/2020                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CTA                                                      #
#Programa          => CTAC013                                                  #
#Objetivo          => Servicio web para la publicacion del detalle de          #
#                     movimientos                                              #
#Fecha inicio      => MARZO 2020                                               #
################################################################################

DATABASE safre_viv

DEFINE v_nss_prueba    CHAR(11)
DEFINE v_id_derechohabiente  DECIMAL(9,0)
DEFINE tabla_patron VARCHAR(20)


GLOBALS "CTAC013.inc"

PRIVATE DEFINE v_ruta_pdf    STRING 
PRIVATE DEFINE v_arr_mov     DYNAMIC ARRAY OF movimientos
PRIVATE DEFINE arr_cargos_y_abonos       DYNAMIC ARRAY OF t_movimientos
PRIVATE DEFINE arr_aclaratorio           DYNAMIC ARRAY OF t_aclaratorio 
PRIVATE DEFINE arr_cargos_y_abonos_final        DYNAMIC ARRAY OF t_aclaratorio_final
PRIVATE DEFINE arr_mov_aclara_final DYNAMIC ARRAY OF RECORD
               v_bimestre           VARCHAR(7),
               v_reg_patronal       VARCHAR(11),
               v_nombre_patron      VARCHAR(40),
               v_f_pago             DATE,
               v_aportacion         DECIMAL(16,2),
               v_amortizacion       DECIMAL(16,2)
END RECORD 

DEFINE movimientos_final DYNAMIC ARRAY OF RECORD
      fLiquidacion DATE,
      movimiento CHAR(5),
      movimiento_desc STRING,
      subcuenta STRING,
      abono DECIMAL(16,2),
      cargo DECIMAL(16,2)
    END RECORD

FUNCTION datos_reportes(v_opcion, v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER 
    DEFINE v_opcion     SMALLINT
    DEFINE v_id_derechohabiente DECIMAL(9,0)
    DEFINE v_fecha_ini  CHAR(6)
    DEFINE v_fecha_fin  CHAR(6) 
    

    CASE v_opcion
      WHEN 1 --Seleccionar Todo
         CALL fn_consulta_movimientos(v_id_derechohabiente)--Entrada: Recibe id_derechohabiente  
         CALL fn_genera_rpt_aclara(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
         RETURN movimientos_final, arr_mov_aclara_final
      WHEN 2 --Movimientos que incrementaron tu ahorro
        CALL fn_genera_rpt_comp(v_id_derechohabiente)
        RETURN movimientos_final, arr_mov_aclara_final
         RETURN arr_cargos_y_abonos, arr_cargos_y_abonos_final
      WHEN 3 --Aportaciones y amortizaciones
         CALL fn_genera_rpt_amo_apo(v_id_derechohabiente) --Entrada: Recibe id_derechohabiente 
         RETURN movimientos_final, arr_mov_aclara_final
      WHEN 4 --Movimientos que disminuyeron tu ahorró
         CALL fn_genera_rpt_comp2(v_id_derechohabiente)
         RETURN movimientos_final, arr_mov_aclara_final
         RETURN arr_cargos_y_abonos, arr_cargos_y_abonos_final
      WHEN 5 --Pagos en aclaración
        CALL fn_genera_rpt_aclara(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
        RETURN movimientos_final, arr_mov_aclara_final

      OTHERWISE
         CALL fn_consulta_movimientos(v_id_derechohabiente)

   END CASE
END FUNCTION 


FUNCTION fn_genera_rpt_comp(v_id_derechohabiente) --Reporte2: MOVIMIENTOS QUE AUMENTARON
    DEFINE p_trabajador             t_datos_personales
    DEFINE v_query                  STRING 
    DEFINE v_cont                   INTEGER 
    DEFINE v_rfc10                  CHAR(10)
    DEFINE tabla_patron             VARCHAR(20)
    DEFINE v_patron                 CHAR(50)
    DEFINE v_id_derechohabiente  DECIMAL(9,0)

    --LET v_id_derechohabiente = 29852

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_aumento_dism FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_aumento_dism USING v_id_derechohabiente
   
   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "subc.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "'' ,",
                        "0 ,", 
                        "'' ,",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento AND 
                            cat.movimiento IN (SELECT cat_mov.movimiento FROM cat_movimiento_categoria cat_mov
                            INNER JOIN cat_categoria cat ON cat.categoria = cat_mov.categoria and cat.categoria IN(1)) ", --categoria = 1 Aumentaron tu ahorro
                     "INNER JOIN cat_subcuenta_preca spre ON spre.subcuenta = mov.subcuenta ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = spre.id_subcuenta ",
                     "WHERE mov.id_derechohabiente = ? ",
                     "AND mov.subcuenta IN (4,8,42,44,55) ",
                     "AND mov.movimiento IN (SELECT mmov.movimiento FROM cat_muestra_mov mmov) ",
                     "AND mov.f_liquida >= MDY(11,1,2012) ",
                     "GROUP BY 1,2,3,4,5,6,7,12 ",
                     "UNION ",
                     "SELECT ",
                        "f_pago, ", 
                        "estado, ",
                        "'', ",
                        "-1, ",
                        "CASE estado WHEN 30 THEN 'Pago pendiente de realizar por parte del empleador' ",
                        "         when 70 then 'Pago cancelado, no realizado por el empleador' ",
                        "END CASE, ",
                        "72, ",		--Valor fijo para identificar los adelantos,
                        "'VIVIENDA 97 AMORTIZACION', ",
                        "(monto_aportacion + monto_amortizacion), ",
                        "pag.nrp ,",
                        "pag.num_credito,",
                        "pag.periodo_pago, ", 
                        "0 ", 
                     "FROM dis_det_avance_pago pag ",
                     "WHERE id_derechohabiente = ? ",
                     "AND estado IN (30,70) ",
                     "AND f_pago > MDY(9,1,2005) ",
                     "ORDER BY 1 DESC "
        
      PREPARE exe_consulta_aumen_dis FROM v_query
      DECLARE cur_consulta_aumen_dis CURSOR FOR exe_consulta_aumen_dis

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL

      

      
      FOREACH cur_consulta_aumen_dis USING v_id_derechohabiente,
                                 v_id_derechohabiente 
                           INTO  v_arr_mov[v_cont].*
                           
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta_aumen_dis
      FREE cur_consulta_aumen_dis
      
      --CALL fn_load_out()--Función que carga el arreglo de salida 
      CALL fn_load_out()-- RETURNING arr_cargos_y_abonos_final
   --   LET ns2consultarMovimientosReturn.nss = p_trabajador.nss
   
      DROP TABLE tmp_movimiento 

END FUNCTION 

FUNCTION fn_genera_rpt_comp2(v_id_derechohabiente) --Reporte2: MOVIMIENTOS QUE AUMENTARON Y DISMINUYERON TU AHORRO
    DEFINE p_trabajador             t_datos_personales
    DEFINE v_query                  STRING 
    DEFINE v_cont                   INTEGER 
    DEFINE v_rfc10                  CHAR(10)
    DEFINE tabla_patron             VARCHAR(20)
    DEFINE v_patron                 CHAR(50)
    DEFINE v_id_derechohabiente  DECIMAL(9,0)

    DISPLAY v_id_derechohabiente

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_dism FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_dism USING v_id_derechohabiente
   
   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "subc.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "'' ,",
                        "0 ,", 
                        "'' ,",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento AND 
                            cat.movimiento IN (SELECT cat_mov.movimiento FROM cat_movimiento_categoria cat_mov
                            INNER JOIN cat_categoria cat ON cat.categoria = cat_mov.categoria and cat.categoria IN(2)) ", --categoria = 2 Disminuyeron tu ahorro"
                     "INNER JOIN cat_subcuenta_preca spre ON spre.subcuenta = mov.subcuenta ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = spre.id_subcuenta ",
                     "WHERE mov.id_derechohabiente = ? ",
                     "AND mov.subcuenta IN (4,8,42,44,55) ",
                     "AND mov.movimiento IN (SELECT mmov.movimiento FROM cat_muestra_mov mmov) ",
                     "AND mov.f_liquida >= MDY(11,1,2012) ",
                     "GROUP BY 1,2,3,4,5,6,7,12 ",
                     "UNION ",
                     "SELECT ",
                        "f_pago, ", 
                        "estado, ",
                        "'', ",
                        "-1, ",
                        "CASE estado WHEN 30 THEN 'Pago pendiente de realizar por parte del empleador' ",
                        "         when 70 then 'Pago cancelado, no realizado por el empleador' ",
                        "END CASE, ",
                        "72, ",		--Valor fijo para identificar los adelantos,
                        "'VIVIENDA 97 AMORTIZACION', ",
                        "(monto_aportacion + monto_amortizacion), ",
                        "pag.nrp ,",
                        "pag.num_credito,",
                        "pag.periodo_pago, ", 
                        "0 ", 
                     "FROM dis_det_avance_pago pag ",
                     "WHERE id_derechohabiente = ? ",
                     "AND estado IN (30,70) ",
                     "AND f_pago > MDY(9,1,2005) ",
                     "ORDER BY 1 DESC "
        
      PREPARE exe_consulta_dis FROM v_query
      DECLARE cur_consulta_dis CURSOR FOR exe_consulta_dis

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL

      

      
      FOREACH cur_consulta_dis USING v_id_derechohabiente,
                                 v_id_derechohabiente 
                           INTO  v_arr_mov[v_cont].*
                           
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta_dis
      FREE cur_consulta_dis
      
      --CALL fn_load_out()--Función que carga el arreglo de salida 
      CALL fn_load_out()-- RETURNING arr_cargos_y_abonos_final
      
      DROP TABLE tmp_movimiento 

END FUNCTION 

FUNCTION fn_genera_rpt_amo_apo(v_id_derechohabiente) --Reporte3: APORTACIONES Y/O AMORTIZACIONES ENVIADAS A TU CRÉDITO
    DEFINE p_trabajador             t_datos_personales
    DEFINE v_query                  STRING 
    DEFINE v_cont                   INTEGER 
    DEFINE v_rfc10                  CHAR(10)
    DEFINE tabla_patron             VARCHAR(20)
    DEFINE v_patron                 CHAR(50)
    DEFINE v_id_derechohabiente  DECIMAL(9,0)

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_apo_y_amo FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_apo_y_amo USING v_id_derechohabiente 
   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "subc.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "'' ,",
                        "0 ,", 
                        "'' ,",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento AND 
                            cat.movimiento IN (SELECT cat_mov.movimiento FROM cat_movimiento_categoria cat_mov
                            INNER JOIN cat_categoria cat ON cat.categoria = cat_mov.categoria and cat.categoria = 3) ", --categoria = 3 es "Aportaciones y/o amortizaciones enviadas a tu crédito"
                     "INNER JOIN cat_subcuenta_preca spre ON spre.subcuenta = mov.subcuenta ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = spre.id_subcuenta ",
                     "WHERE mov.id_derechohabiente = ? ",
                     "AND mov.subcuenta IN (4,8,42,44,55) ",
                     "AND mov.movimiento IN (SELECT mmov.movimiento FROM cat_muestra_mov mmov) ",
                     "AND mov.f_liquida >= MDY(11,1,2012) ",
                     "GROUP BY 1,2,3,4,5,6,7,12 ",
                     "UNION ",
                     "SELECT ",
                        "f_pago, ", 
                        "estado, ",
                        "'', ",
                        "-1, ",
                        "CASE estado WHEN 30 THEN 'Pago pendiente de realizar por parte del empleador' ",
                        "         when 70 then 'Pago cancelado, no realizado por el empleador' ",
                        "END CASE, ",
                        "72, ",		--Valor fijo para identificar los adelantos,
                        "'VIVIENDA 97 AMORTIZACION', ",
                        "(monto_aportacion + monto_amortizacion), ",
                        "pag.nrp ,",
                        "pag.num_credito,",
                        "pag.periodo_pago, ", 
                        "0 ", 
                     "FROM dis_det_avance_pago pag ",
                     "WHERE id_derechohabiente = ? ",
                     "AND estado IN (30,70) ",
                     "AND f_pago > MDY(9,1,2005) ",
                     "ORDER BY 1 DESC "
        
      PREPARE exe_consulta_a FROM v_query
      DECLARE cur_consulta_apo_y_amo CURSOR FOR exe_consulta_a

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL

      

      
      FOREACH cur_consulta_apo_y_amo USING v_id_derechohabiente,
                                 v_id_derechohabiente 
                           INTO  v_arr_mov[v_cont].*
                           
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta_apo_y_amo
      FREE cur_consulta_apo_y_amo
      
      --CALL fn_load_out()--Función que carga el arreglo de salida 
      CALL fn_load_out() --RETURNING arr_cargos_y_abonos_final
      
      DROP TABLE tmp_movimiento 

END FUNCTION 


FUNCTION fn_genera_rpt_aclara(v_id_derechohabiente,v_fecha_ini,v_fecha_fin) --Reporte4: ACLARATORIO
    DEFINE v_id_derechohabiente DECIMAL(9,0)
    DEFINE v_fecha_ini  CHAR(6)
    DEFINE v_fecha_fin  CHAR(6) 
    DEFINE v_cont       INTEGER 
    DEFINE v_query      STRING
    LET v_cont = 1    
    
    LET v_query = "SELECT a.nrp      ,", --Número de registro Patronal  ---> Con esto se saca el nombre del patrón
                    "fn_bimestre_pago (a.periodo_pago), ",--"a.periodo_pago  ,", -- Periodo de pago 
                    "a.f_pago        ,", --> Fecha de Pago 
                    "a.tpo_aclaracion,", ----Aclaratorio 
                    "a.imp_ap_pat    ,",--aportación
                    "a.imp_am_cre     ",--Amortización 
                "FROM  cta_his_pagos a, ", 
                "pag_tpo_archivo b, ",
                "cta_pag_complemento d, OUTER pag_tpo_aclaracion c ", 
                "WHERE  a.origen_archivo = b.archivo_cod ",        
                   "AND  a.tpo_aclaracion = c.aclaracion_cod ",        
                   "AND  d.id_derechohabiente = a.id_derechohabiente ",
                   "AND  a.folio = d.folio ",
                   "AND  a.id_referencia = d.id_referencia ",
                   "AND  a.id_derechohabiente = ? ",--,--v_id_derechohabiente
                   "AND  a.periodo_pago between ? AND ? " --AND fn_bimestre_pago (a.periodo_pago) between ? AND ?


    PREPARE exe_consulta_aclaratorio FROM v_query
    DECLARE cur_aclaratorio CURSOR FOR exe_consulta_aclaratorio

    SELECT tabla
    INTO tabla_patron  ---Obtenemos la tabla activa actual
    FROM pag_ctr_patron   
    
    LET v_query = "SELECT raz FROM ",tabla_patron, " WHERE nrp = ? "
    PREPARE nom_patron_aclaratorio FROM v_query
    
    FOREACH cur_aclaratorio USING v_id_derechohabiente,
                                  v_fecha_ini,
                                  v_fecha_fin
                             INTO  arr_aclaratorio[v_cont].* 

        EXECUTE nom_patron_aclaratorio USING arr_aclaratorio[v_cont].nrp 
                                       INTO arr_aclaratorio[v_cont].nom_patron
                                       
        LET arr_cargos_y_abonos_final[v_cont].year_bim = arr_aclaratorio[v_cont].periodo_pago[1,4],"-",arr_aclaratorio[v_cont].periodo_pago[5,6]
        LET arr_cargos_y_abonos_final[v_cont].nrp = arr_aclaratorio[v_cont].nrp 
        LET arr_cargos_y_abonos_final[v_cont].nom_patron = arr_aclaratorio[v_cont].nom_patron
        LET arr_cargos_y_abonos_final[v_cont].f_pago =arr_aclaratorio[v_cont].f_pago --MDY(arr_aclaratorio[v_cont].f_pago[4,5],arr_aclaratorio[v_cont].f_pago[1,2],arr_aclaratorio[v_cont].f_pago[7,10])
        LET arr_cargos_y_abonos_final[v_cont].aportacion = arr_aclaratorio[v_cont].aportacion      
        LET arr_cargos_y_abonos_final[v_cont].amortizacion = arr_aclaratorio[v_cont].amortizacion
        
        LET arr_mov_aclara_final[v_cont].v_bimestre = arr_cargos_y_abonos_final[v_cont].year_bim
        LET arr_mov_aclara_final[v_cont].v_reg_patronal = arr_cargos_y_abonos_final[v_cont].nrp
        LET arr_mov_aclara_final[v_cont].v_nombre_patron = arr_cargos_y_abonos_final[v_cont].nom_patron
        LET arr_mov_aclara_final[v_cont].v_f_pago = arr_cargos_y_abonos_final[v_cont].f_pago
        LET arr_mov_aclara_final[v_cont].v_aportacion = arr_cargos_y_abonos_final[v_cont].aportacion
        LET arr_mov_aclara_final[v_cont].v_amortizacion = arr_cargos_y_abonos_final[v_cont].amortizacion
        LET v_cont = v_cont + 1
    END FOREACH 

    
END FUNCTION 

FUNCTION fn_consulta_movimientos(v_id_derechohabiente)--Funcion que carga el record que serà regreasado por el servicio
   DEFINE p_trabajador             t_datos_personales
   DEFINE v_query                  STRING 
   DEFINE v_cont                   INTEGER 
   DEFINE v_rfc10                  CHAR(10)
   DEFINE tabla_patron             VARCHAR(20)
   DEFINE v_patron                 CHAR(50)
   DEFINE v_id_derechohabiente     DECIMAL(9,0)

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_movimientos FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_movimientos USING v_id_derechohabiente
   
   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "subc.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "'' ,",
                        "0 ,", 
                        "'' ,",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento ",
                     "INNER JOIN cat_subcuenta_preca spre ON spre.subcuenta = mov.subcuenta ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = spre.id_subcuenta ",
                     "WHERE mov.id_derechohabiente = ? ",
                     "AND mov.subcuenta IN (4,8,42,44,55) ",
                     "AND mov.movimiento IN (SELECT mmov.movimiento FROM cat_muestra_mov mmov) ",
                     "AND mov.f_liquida >= MDY(11,1,2012) ",
                     "GROUP BY 1,2,3,4,5,6,7,12 ",
                     "UNION ",
                     "SELECT ",
                        "f_pago, ", 
                        "estado, ",
                        "'', ",
                        "-1, ",
                        "CASE estado WHEN 30 THEN 'Pago pendiente de realizar por parte del empleador' ",
                        "         when 70 then 'Pago cancelado, no realizado por el empleador' ",
                        "END CASE, ",
                        "72, ",		--Valor fijo para identificar los adelantos,
                        "'VIVIENDA 97 AMORTIZACION', ",
                        "(monto_aportacion + monto_amortizacion), ",
                        "pag.nrp ,",
                        "pag.num_credito,",
                        "pag.periodo_pago, ", 
                        "0 ", 
                     "FROM dis_det_avance_pago pag ",
                     "WHERE id_derechohabiente = ? ",
                     "AND estado IN (30,70) ",
                     "AND f_pago > MDY(9,1,2005) ",
                     "ORDER BY 1 DESC "
        
      PREPARE exe_consulta FROM v_query
      DECLARE cur_consulta CURSOR FOR exe_consulta

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL

      

      
      FOREACH cur_consulta USING v_id_derechohabiente,
                                 v_id_derechohabiente 
                           INTO  v_arr_mov[v_cont].*
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta
      FREE cur_consulta
      
      CALL fn_load_out() --RETURNING arr_cargos_y_abonos_final--Función que carga el arreglo de salida 
      
      DROP TABLE tmp_movimiento 

END FUNCTION 

FUNCTION fn_load_out()--Función que carga el arreglo de salida evaluando el campo desc_ciudadana 
                      --si desc_ciudadana es null se utilizara el campo movimiento_desc la salida
    DEFINE v_cont             SMALLINT 
    DEFINE v_indice           SMALLINT
    DEFINE v_cargo_mov_desc   VARCHAR(60)
    DEFINE v_cargo_mov_desc_pen VARCHAR(60)
    DEFINE v_query            STRING 
    DEFINE bandera           SMALLINT
    
    
 --   CALL ns2consultarMovimientosReturn.movimientos.item.clear()

    #Se consulta la descripcion del cargo informativo para los movimientos del historico TRM
    SELECT
      desc_ciudadana
    INTO
      v_cargo_mov_desc
    FROM cat_movimiento
    WHERE movimiento = CARGO_INFORMATIVO 

    #Se consulta la descripcion del cargo informativo para los movimientos del historico PENSIONES
    SELECT
      desc_ciudadana
    INTO
      v_cargo_mov_desc_pen
    FROM cat_movimiento
    WHERE movimiento = CARGO_INFORMATIVO_PEN 

    LET v_indice = 1
    
    SELECT tabla
    INTO tabla_patron  ---Obtenemos la tabla activa actual
    FROM pag_ctr_patron   

    LET v_query = "SELECT raz FROM ",tabla_patron, " WHERE nrp = ? "
    PREPARE nom_patron FROM v_query
    
    FOR v_cont=1 TO v_arr_mov.getLength()
      LET bandera = 0
      IF v_arr_mov[v_cont].f_liquida IS NOT NULL THEN --if 1
      
         IF v_arr_mov[v_cont].desc_ciudadana IS NULL THEN --if 2
            LET arr_cargos_y_abonos[v_indice].v_desc_concepto = v_arr_mov[v_cont].movimiento_desc 
         ELSE 
            LET arr_cargos_y_abonos[v_indice].v_desc_concepto = v_arr_mov[v_cont].desc_ciudadana 
         END IF         --FIN if 2
         
         LET arr_cargos_y_abonos[v_indice].v_f_aplicacion = v_arr_mov[v_cont].f_liquida
         LET arr_cargos_y_abonos[v_indice].v_cve_movimiento = v_arr_mov[v_cont].movimiento USING '##&&'
         #LET arr_cargos_y_abonos[v_indice].movimiento_desc = v_arr_mov[v_cont].movimiento_desc
         LET arr_cargos_y_abonos[v_indice].v_tpo_movimiento = v_arr_mov[v_cont].subcuenta_desc 

         IF v_arr_mov[v_cont].tipo < 0 THEN--if 3
            IF v_arr_mov[v_cont].monto_pesos < 0 THEN--if 4
               LET arr_cargos_y_abonos[v_indice].v_cargo = v_arr_mov[v_cont].monto_pesos * -1
            ELSE
               LET arr_cargos_y_abonos[v_indice].v_cargo = v_arr_mov[v_cont].monto_pesos
            END IF--FIN if 4
            
            LET arr_cargos_y_abonos[v_indice].v_abono = NULL
         ELSE
            LET arr_cargos_y_abonos[v_indice].v_abono = v_arr_mov[v_cont].monto_pesos
            LET arr_cargos_y_abonos[v_indice].v_cargo = NULL
         END IF --FIN if 3

         IF v_arr_mov[v_cont].subcuenta = 72 THEN  --if 5
         
            SELECT periodo_pago, nrp 
            INTO v_arr_mov[v_cont].periodo_pago, v_arr_mov[v_cont].nrp
            FROM cta_his_pagos
            WHERE id_derechohabiente = v_id_derechohabiente
            AND 	 id_referencia = v_arr_mov[v_cont].id_referencia;

            LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto, "\n",
                                                                v_arr_mov[v_cont].periodo_pago,"- NRP ",v_arr_mov[v_cont].nrp
            LET bandera = 1
        END IF  --FIN if 5
        --Obtenemos el número de crédito-----------------------------------------------------
        IF v_arr_mov[v_cont].movimiento = 502 THEN --if 6

            SELECT num_credito 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_det_avance_pago
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_det_avance_pago = v_arr_mov[v_cont].id_referencia

            IF bandera = 1 THEN  --if 7
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto," ",v_arr_mov[v_cont].num_credito
            ELSE 
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF  --FIN if 7
        END IF --FIN if 6

        IF v_arr_mov[v_cont].movimiento = 872 OR  v_arr_mov[v_cont].movimiento = 892 THEN --if 8

            SELECT num_crd_ifv 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_interface_ef
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_interface_ef = v_arr_mov[v_cont].id_referencia

            IF bandera = 1 THEN --if 9
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto," ",v_arr_mov[v_cont].num_credito
            ELSE 
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF --FIN if 9
            
        END IF --FIN if 8

        IF v_arr_mov[v_cont].movimiento = 632 --if 10
        OR  v_arr_mov[v_cont].movimiento = 42
        OR  v_arr_mov[v_cont].movimiento = 72 
        OR  v_arr_mov[v_cont].movimiento = 942 
        OR  v_arr_mov[v_cont].movimiento = 1532 
        OR  v_arr_mov[v_cont].movimiento = 1542 
        OR  v_arr_mov[v_cont].movimiento = 1752 
        OR  v_arr_mov[v_cont].movimiento = 1772 THEN 

            SELECT num_crd_ifv 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_interface_hs
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_interface_hs = v_arr_mov[v_cont].id_referencia

            IF bandera = 1 THEN --if 11
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto," ",v_arr_mov[v_cont].num_credito
            ELSE 
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF --FIN if 11

        END IF --FIN if 10

    END IF --Fin If 1

    --Agregamos el nombre del Patron
    IF v_arr_mov[v_cont].movimiento = 1  --If 11
    OR  v_arr_mov[v_cont].movimiento = 41
    OR  v_arr_mov[v_cont].movimiento = 51 
    OR  v_arr_mov[v_cont].movimiento = 61 
    OR  v_arr_mov[v_cont].movimiento = 81
    OR  v_arr_mov[v_cont].movimiento = 211 
    OR  v_arr_mov[v_cont].movimiento = 221 
    OR  v_arr_mov[v_cont].movimiento = 451 THEN 

        EXECUTE nom_patron USING v_arr_mov[v_cont].nrp 
                                   INTO v_arr_mov[v_cont].nom_patron
        
        LET v_arr_mov[v_cont].nom_patron = v_arr_mov[v_cont].nom_patron CLIPPED
        
        IF v_arr_mov[v_cont].nom_patron = "" OR v_arr_mov[v_cont].nom_patron IS NULL THEN 
          --  LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto, "\n########"
        ELSE 
            LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto, "\n",v_arr_mov[v_cont].nom_patron
        END IF 
    END IF --FIN if 11
    
    LET movimientos_final[v_cont].fLiquidacion = arr_cargos_y_abonos[v_cont].v_f_aplicacion
    LET movimientos_final[v_cont].movimiento = arr_cargos_y_abonos[v_cont].v_cve_movimiento
    LET movimientos_final[v_cont].movimiento_desc = arr_cargos_y_abonos[v_cont].v_desc_concepto
    LET movimientos_final[v_cont].subcuenta = arr_cargos_y_abonos[v_cont].v_tpo_movimiento
    LET movimientos_final[v_cont].abono = arr_cargos_y_abonos[v_cont].v_abono
    LET movimientos_final[v_cont].cargo = arr_cargos_y_abonos[v_cont].v_cargo


    LET v_indice = v_indice + 1
         
    END FOR 
    CALL arr_cargos_y_abonos.deleteElement(v_indice)

   -- RETURN arr_cargos_y_abonos
    
END FUNCTION 