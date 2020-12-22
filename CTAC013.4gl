
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


DEFINE v_id_derechohabiente  DECIMAL(9,0)
DEFINE tabla_patron VARCHAR(20)



GLOBALS "CTAC013.inc"

PRIVATE DEFINE v_arr_mov     DYNAMIC ARRAY OF movimientos
PRIVATE DEFINE arr_cargos_y_abonos       DYNAMIC ARRAY OF t_movimientos
PRIVATE DEFINE arr_aclaratorio           DYNAMIC ARRAY OF t_aclaratorio 
PRIVATE DEFINE arr_cargos_y_abonos_final        DYNAMIC ARRAY OF t_aclaratorio_final
PRIVATE DEFINE arr_mov_aclara_final DYNAMIC ARRAY OF RECORD
               v_bimestre           VARCHAR(7),
               v_reg_patronal       VARCHAR(11),
               v_nombre_patron      VARCHAR(160),
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

FUNCTION datos_reportes(v_opcion, p_id_derechohabiente,v_fecha_ini,v_fecha_fin)
    
    DEFINE v_opcion     SMALLINT
    DEFINE p_id_derechohabiente DECIMAL(9,0)
    DEFINE v_fecha_ini  DATE 
    DEFINE v_fecha_ini_acl DATE
    DEFINE v_fecha_fin  DATE 
    DEFINE v_historico  SMALLINT
    DEFINE v_f_ini  CHAR(6)
    DEFINE v_f_fin  CHAR(6)
    DEFINE mes      CHAR(2)
    DEFINE v_year   CHAR(4)

    LET v_id_derechohabiente = p_id_derechohabiente
    LET v_year = YEAR(v_fecha_ini)
    LET mes = MONTH(v_fecha_ini)USING "&&"
    LET v_f_ini = v_year,mes

    LET v_year = YEAR(v_fecha_fin)
    LET mes = MONTH(v_fecha_fin)USING "&&"
    LET v_f_fin = v_year,mes
    LET v_historico = 0 


###CASO 7 DEFECTO 465, CONSIDERAR FECHA INICIO DE MOVIMIENTOS EN SACI 1-OCT-2012    
    IF v_fecha_ini <= MDY(9,30,2012)  THEN
        LET v_historico = 1
        LET v_fecha_ini_acl = v_fecha_ini
        DISPLAY "Fecha_ini:", v_fecha_ini_acl
        LET v_fecha_ini = MDY(10,1,2012)
    END IF
   { DISPLAY "====== Valores recibidos en la función ======="
    DISPLAY "v_opcion: ",             v_opcion
    DISPLAY "v_id_derechohabiente: ", v_id_derechohabiente
    DISPLAY "v_fecha_ini: ",          v_fecha_ini
    DISPLAY "v_fecha_fin: ",          v_fecha_fin
     DISPLAY "v_f_ini: ",             v_f_ini
    DISPLAY "v_f_fin: ",              v_f_fin
    DISPLAY "=============================================="}
    
    CASE v_opcion
      WHEN 1 --Seleccionar Todo
         IF v_fecha_fin > "09-30-2012" THEN
            CALL fn_consulta_movimientos(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)--Entrada: Recibe id_derechohabiente
         END IF
         --CALL fn_genera_rpt_aclara(v_id_derechohabiente,v_f_ini,v_f_fin)
         IF v_historico = 1 THEN
            LET v_fecha_ini = v_fecha_ini_acl
         END IF 
         CALL fn_genera_rpt_aclara(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
         
         RETURN movimientos_final, arr_mov_aclara_final
      WHEN 2 --Movimientos que incrementaron tu ahorro
        CALL fn_genera_rpt_comp(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
        RETURN movimientos_final, arr_mov_aclara_final 
      WHEN 3 --Aportaciones y amortizaciones
         CALL fn_genera_rpt_amo_apo(v_id_derechohabiente,v_fecha_ini,v_fecha_fin) --Entrada: Recibe id_derechohabiente 
         RETURN movimientos_final, arr_mov_aclara_final
      WHEN 4 --Movimientos que disminuyeron tu ahorró
         CALL fn_genera_rpt_comp2(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
         RETURN movimientos_final, arr_mov_aclara_final 
      WHEN 5 --Pagos en aclaración
        IF v_historico = 1 THEN
           LET v_fecha_ini = v_fecha_ini_acl
        END IF 
        CALL fn_genera_rpt_aclara(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
        RETURN movimientos_final, arr_mov_aclara_final
      WHEN 6 --Movimientos que incrementaron y movimientos que disminuyeron tu ahorro
        CALL fn_genera_rpt_comp3(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)
        RETURN movimientos_final, arr_mov_aclara_final

      OTHERWISE
         CALL fn_consulta_movimientos(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)

   END CASE
END FUNCTION 


FUNCTION fn_genera_rpt_comp(v_id_derechohabiente,v_fecha_ini,v_fecha_fin) --Reporte2: MOVIMIENTOS QUE AUMENTARON
    
    DEFINE v_query                  STRING 
    DEFINE v_cont                   INTEGER 
    
    DEFINE v_id_derechohabiente  DECIMAL(9,0)
    DEFINE v_fecha_ini              DATE 
    DEFINE v_fecha_fin              DATE 
      

    DISPLAY "Fechas recibidas en función R2"
    DISPLAY "v_fecha_ini: ",v_fecha_ini
    DISPLAY "v_fecha_fin: ",v_fecha_fin

    --LET v_id_derechohabiente = 29852

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_aumento FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_aumento USING v_id_derechohabiente

  LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "'','',0,",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov, ",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria catmov,",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,42,44,55) ",  
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento NOT IN (1,41, 51, 61,81,211,221, 451, 691,731,741,751,761,771,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ",      
                     "AND catmov.movimiento = cat.movimiento ",
                     "AND catmov.categoria = 1 ",           
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",    
                     "UNION ALL " ,
                     "SELECT ", 
                        "mov.f_liquida, ",
                       "fn_periodo_bimestre(cta.periodo_pago,cta.origen_archivo), cta.nrp, cta.num_crd_ifv, ",
                         "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                      "FROM tmp_movimiento mov, cta_his_pagos cta, ",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria ccm,",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,42,44,55) ",  
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento IN (1,41, 51, 61,81,211,221, 451, 691,731,741,751,761,771,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ", 
                     "AND ccm.movimiento  = cat.movimiento ", 
                     "AND cta.folio = mov.folio_liquida ",
                     "AND cta.id_referencia = mov.id_referencia "  , 
                     --"AND cta.id_derechohabiente = ? " , 
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",      
                     "ORDER BY 1 DESC,8,2 DESC "
   
         
      PREPARE exe_consulta_aumen FROM v_query
      DECLARE cur_consulta_aumen CURSOR FOR exe_consulta_aumen

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL

      

      
      FOREACH cur_consulta_aumen USING  --v_id_derechohabiente,
                                            v_fecha_ini,
                                            v_fecha_fin,
                                            v_fecha_ini,
                                            v_fecha_fin --,
                                            --v_id_derechohabiente 
                                            
                           INTO  v_arr_mov[v_cont].*
                           
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta_aumen
      FREE cur_consulta_aumen

      --CALL fn_load_out() 
      CALL fn_load_out_reporte2(1)
 
   --   LET ns2consultarMovimientosReturn.nss = p_trabajador.nss
 
      
      DROP TABLE tmp_movimiento 
      

END FUNCTION 

FUNCTION fn_genera_rpt_comp2(v_id_derechohabiente,v_fecha_ini,v_fecha_fin) --Reporte2: MOVIMIENTOS QUE DISMINUYERON TU AHORRO
    DEFINE v_query                  STRING  
    DEFINE v_cont                   INTEGER 
    DEFINE v_id_derechohabiente     DECIMAL(9,0)
    DEFINE v_fecha_ini              DATE 
    DEFINE v_fecha_fin              DATE 
    
    DISPLAY "Fechas recibidas en función R4"
    DISPLAY "v_fecha_ini: ",v_fecha_ini
    DISPLAY "v_fecha_fin: ",v_fecha_fin

    
 
      LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
      PREPARE exe_consulta_dism FROM v_query               --por el derechohabiente
      EXECUTE exe_consulta_dism USING v_id_derechohabiente
   
   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                         "'','',0,",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento AND 
                            cat.movimiento IN (SELECT cat_mov.movimiento FROM cat_movimiento_categoria cat_mov
                            INNER JOIN cat_categoria cat ON cat.categoria = cat_mov.categoria and cat.categoria IN(2)) ", --categoria = 2 Disminuyeron tu ahorro"
                     "INNER JOIN cat_subcuenta_mov spre ON spre.subcuenta = mov.subcuenta ",
                     "WHERE mov.id_derechohabiente = ? ",
                     "AND mov.subcuenta IN (4,8,42,44,55) ", 
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento IN (SELECT mmov.movimiento FROM cat_muestra_mov mmov) ",
                     "AND mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin 
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",

                     "ORDER BY 1 DESC, 8, 2 DESC "
        
      PREPARE exe_consulta_dis FROM v_query
      DECLARE cur_consulta_dis CURSOR FOR exe_consulta_dis

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL



      
      FOREACH cur_consulta_dis USING v_id_derechohabiente,
                                     v_fecha_ini,
                                     v_fecha_fin
                                     #v_id_derechohabiente,
                                     #v_fecha_ini,
                                     #v_fecha_fin
                           INTO  v_arr_mov[v_cont].*
                           
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta_dis
      FREE cur_consulta_dis

      CALL fn_load_out(2)
      
      DROP TABLE tmp_movimiento 

END FUNCTION 



FUNCTION fn_genera_rpt_comp3(v_id_derechohabiente,v_fecha_ini,v_fecha_fin) --Reporte2: MOVIMIENTOS QUE AUMENTARON
    
    DEFINE v_query                  STRING 
    DEFINE v_cont                   INTEGER 
    
    DEFINE v_id_derechohabiente  DECIMAL(9,0)
    DEFINE v_fecha_ini              DATE 
    DEFINE v_fecha_fin              DATE 

    DISPLAY "Fechas recibidas en función R2"
    DISPLAY "v_fecha_ini: ",v_fecha_ini
    DISPLAY "v_fecha_fin: ",v_fecha_fin

    --LET v_id_derechohabiente = 29852

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_aumento_dism FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_aumento_dism USING v_id_derechohabiente

   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "'','',0,",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov, ",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria catmov,",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,42,44,55) ",  
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento NOT IN (1,41, 51, 61,81,211,221, 451, 691,731,741,751,761,771,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ",      
                     "AND catmov.movimiento = cat.movimiento ",
                     "AND catmov.categoria = 1 ",           
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",    
                     "UNION ALL " ,
                     "SELECT ", 
                        "mov.f_liquida, ",
                       "fn_periodo_bimestre(cta.periodo_pago,cta.origen_archivo), cta.nrp, cta.num_crd_ifv, ",
                         "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                      "FROM tmp_movimiento mov, cta_his_pagos cta, ",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria ccm,",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,42,44,55) ",  
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento IN (1,41, 51, 61,81,211,221, 451, 691,731,741,751,761,771,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ", 
                     "AND ccm.movimiento  = cat.movimiento ", 
                     "AND cta.folio = mov.folio_liquida ",
                     "AND cta.id_referencia = mov.id_referencia "  , 
                     --"AND cta.id_derechohabiente = ? " , 
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ", 
                     "UNION ALL ",
                     "SELECT ", 
                        "mov.f_liquida, ",
                        --"TO_CHAR(f_liquida,'%Y%m'),'',''",
                        "'','',0,",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento AND 
                            cat.movimiento IN (SELECT cat_mov.movimiento FROM cat_movimiento_categoria cat_mov
                            INNER JOIN cat_categoria cat ON cat.categoria = cat_mov.categoria and cat.categoria IN(2)) ", --categoria = 2 Disminuyeron tu ahorro"
                     "INNER JOIN cat_subcuenta_mov spre ON spre.subcuenta = mov.subcuenta ",
                     "WHERE mov.id_derechohabiente = ? ",
                     "AND mov.subcuenta IN (4,8,42,44,55) ", 
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento IN (SELECT mmov.movimiento FROM cat_muestra_mov mmov) ",
                     "AND mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin 
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",     
                     "ORDER BY 1 DESC,8, 2 DESC "
   
         
      PREPARE exe_consulta_aumen_dis FROM v_query
      DECLARE cur_consulta_aumen_dis CURSOR FOR exe_consulta_aumen_dis

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL    
      
      FOREACH cur_consulta_aumen_dis USING  --v_id_derechohabiente,
                                            v_fecha_ini,
                                            v_fecha_fin,
                                            v_fecha_ini,
                                            v_fecha_fin,
                                            --v_id_derechohabiente,
                                            v_id_derechohabiente,
                                            v_fecha_ini,
                                            v_fecha_fin
                                            
                           INTO  v_arr_mov[v_cont].*                                      
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta_aumen_dis
      FREE cur_consulta_aumen_dis


      CALL fn_load_out_reporte2(6)
 
 
      DROP TABLE tmp_movimiento 
      

END FUNCTION 

FUNCTION fn_genera_rpt_amo_apo(v_id_derechohabiente,v_fecha_ini,v_fecha_fin) --Reporte3: APORTACIONES Y/O AMORTIZACIONES ENVIADAS A TU CRÉDITO
    
    DEFINE v_query                  STRING 
    DEFINE v_cont                   INTEGER 
    DEFINE v_id_derechohabiente  DECIMAL(9,0)
    DEFINE v_fecha_ini              DATE 
    DEFINE v_fecha_fin              DATE 

    DISPLAY "Fechas recibidas en función R3"
    DISPLAY "v_fecha_ini: ",v_fecha_ini
    DISPLAY "v_fecha_fin: ",v_fecha_fin

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_apo_y_amo FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_apo_y_amo USING v_id_derechohabiente 
   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "'','',0,",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                     "FROM tmp_movimiento mov, ",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria catmov,",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,41,42,44,55) ",  -- SE CONSIDERA SUBCUENTA 41
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento NOT IN (1,41, 51, 61,81,221, 451, 691,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ",      
                     "AND catmov.movimiento = cat.movimiento ",
                     "AND catmov.categoria = 3 ",           
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",    
                     "UNION " ,
                     "SELECT ", 
                        "mov.f_liquida, ",
                        "fn_periodo_bimestre(cta.periodo_pago,cta.origen_archivo), cta.nrp, cta.num_crd_ifv, ",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                      "FROM tmp_movimiento mov, cta_his_pagos cta,",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria ccm, ",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,41,42,44,55) ",  -- SE CONSIDERA SUBCUENTA 41
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento IN (1,41, 51, 61,81,221, 451, 691,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ",   
                     "AND ccm.movimiento  = cat.movimiento ", 
                     "AND cta.folio = mov.folio_liquida ",
                     "AND cta.id_referencia = mov.id_referencia " , 
                     --"AND cta.id_derechohabiente = ? " ,     
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",      
                     "ORDER BY 1 DESC, 8, 2 DESC"

     --DISPLAY "consulta:", v_query
                          
      PREPARE exe_consulta_a FROM v_query
      DECLARE cur_consulta_apo_y_amo CURSOR FOR exe_consulta_a

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL

      

      
      FOREACH cur_consulta_apo_y_amo USING --v_id_derechohabiente,
                                         v_fecha_ini,
                                         v_fecha_fin,
                                         v_fecha_ini,
                                         v_fecha_fin --,
                                         --v_id_derechohabiente
                           INTO  v_arr_mov[v_cont].* 
                           
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta_apo_y_amo
      FREE cur_consulta_apo_y_amo
      
      --CALL fn_load_out()
      CALL fn_load_out_reporte2(3)
      
      DROP TABLE tmp_movimiento 

END FUNCTION 

FUNCTION fn_genera_rpt_aclara(v_id_derechohabiente,v_fecha_ini,v_fecha_fin) --Reporte4: ACLARATORIO
    DEFINE v_id_derechohabiente DECIMAL(9,0)
    DEFINE v_fecha_ini  DATE
    DEFINE v_fecha_fin  DATE 
    
    DEFINE v_cont       INTEGER
    DEFINE v_cont_aux   INTEGER
    DEFINE v_query      STRING
    LET v_cont = 1   
    LET v_cont_aux = 1 
    
    DISPLAY "Fechas recibidas en función R5"
    DISPLAY "v_fecha_ini: ",v_fecha_ini
    DISPLAY "v_fecha_fin: ",v_fecha_fin
    --CALL fn_mensaje("","v_fecha_ini: "||v_fecha_ini||"\nv_fecha_fin: "||v_fecha_fin||"\nv_id_derechohabiente: "||v_id_derechohabiente,"")
    
    LET v_query = "SELECT a.nrp      ,", --Número de registro Patronal  ---> Con esto se saca el nombre del patrón
                    "fn_bimestre_pago (a.periodo_pago), ",--"a.periodo_pago  ,", -- Periodo de pago 
                    "a.f_pago        ,", --> Fecha de Pago 
                    "a.tpo_aclaracion,", ----Aclaratorio 
                    "a.imp_ap_pat    ,",--aportación
                    "a.imp_am_cre     ",--Amortización  
                 "FROM cta_his_pagos a, ", 
                 "pag_tpo_archivo b, ",
                 "pag_tpo_aclaracion c, ",
                 "cta_pag_complemento d, ", 
                 "cat_muestra_acl acl ",
                 "WHERE  a.id_derechohabiente = ? ",--,--v_id_derechohabiente
                   "AND  d.folio = a.folio ",
                   "AND  d.id_referencia = a.id_referencia ",     
                   "AND  d.id_derechohabiente = a.id_derechohabiente ",
                   --"AND  a.origen_archivo IN (5,6,7) ",      
                   "AND  b.archivo_cod = a.origen_archivo ",
                   "AND  c.aclaracion_cod = a.tpo_aclaracion ",  
                   "AND  acl.aclaracion_cod = c.aclaracion_cod ",
                   "AND  a.ind_liquidacion = 1 ",--NOT IN (5,6) ",--Quitamos los pagos de Salida Normal ACL

                   "AND  a.f_pago between ? AND ? ", --AND fn_bimestre_pago (a.periodo_pago) between ? AND ?
                   "ORDER BY 2 DESC, 1 DESC "

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
                            INTO  arr_aclaratorio[v_cont].*--, v_ind_liquidacion
        
       -- CALL fn_mensaje("","periodo_pago: "||arr_aclaratorio[v_cont].periodo_pago,"")                      
        EXECUTE nom_patron_aclaratorio USING arr_aclaratorio[v_cont].nrp 
                                       INTO arr_aclaratorio[v_cont].nom_patron

        SELECT "X" FROM cat_muestra_acl
        WHERE aclaracion_cod = arr_aclaratorio[v_cont].tpo_aclaracion 
       -- CALL fn_mensaje("","tpo_aclaracion "||arr_aclaratorio[v_cont].tpo_aclaracion,"")

        IF SQLCA.SQLCODE = 0 THEN
            
            LET arr_cargos_y_abonos_final[v_cont].nrp = arr_aclaratorio[v_cont].nrp 
            LET arr_cargos_y_abonos_final[v_cont].nom_patron = arr_aclaratorio[v_cont].nom_patron

                SELECT "X" FROM cat_riss_nrp            #pagos RISS voluntarios sin el nombre del patrón ni el NRP
                WHERE nrp = arr_aclaratorio[v_cont].nrp

                IF SQLCA.SQLCODE = 0 THEN
                    LET arr_cargos_y_abonos_final[v_cont].nrp ="           "
                    LET arr_cargos_y_abonos_final[v_cont].nom_patron =" "
                END IF                               
           # END IF 
            
            LET arr_cargos_y_abonos_final[v_cont].year_bim = arr_aclaratorio[v_cont].periodo_pago[1,4],"-",arr_aclaratorio[v_cont].periodo_pago[5,6]
           
            LET arr_cargos_y_abonos_final[v_cont].f_pago =arr_aclaratorio[v_cont].f_pago --MDY(arr_aclaratorio[v_cont].f_pago[4,5],arr_aclaratorio[v_cont].f_pago[1,2],arr_aclaratorio[v_cont].f_pago[7,10])
            LET arr_cargos_y_abonos_final[v_cont].aportacion = arr_aclaratorio[v_cont].aportacion      
            LET arr_cargos_y_abonos_final[v_cont].amortizacion = arr_aclaratorio[v_cont].amortizacion
            
            LET arr_mov_aclara_final[v_cont_aux].v_bimestre = arr_cargos_y_abonos_final[v_cont].year_bim
            LET arr_mov_aclara_final[v_cont_aux].v_reg_patronal = arr_cargos_y_abonos_final[v_cont].nrp
            LET arr_mov_aclara_final[v_cont_aux].v_nombre_patron = arr_cargos_y_abonos_final[v_cont].nom_patron
            LET arr_mov_aclara_final[v_cont_aux].v_f_pago = arr_cargos_y_abonos_final[v_cont].f_pago
            LET arr_mov_aclara_final[v_cont_aux].v_aportacion = arr_cargos_y_abonos_final[v_cont].aportacion
            LET arr_mov_aclara_final[v_cont_aux].v_amortizacion = arr_cargos_y_abonos_final[v_cont].amortizacion

            LET v_cont_aux = v_cont_aux + 1 
        END IF 
        
        LET v_cont = v_cont + 1
    END FOREACH 
    	
END FUNCTION 

FUNCTION fn_consulta_movimientos(v_id_derechohabiente,v_fecha_ini,v_fecha_fin)--Funcion que carga el record que serà regreasado por el servicio
   DEFINE v_query                  STRING 
   DEFINE v_cont                   INTEGER 
   DEFINE v_id_derechohabiente     DECIMAL(9,0)
   DEFINE v_fecha_ini              DATE 
   DEFINE v_fecha_fin              DATE 
   
   DISPLAY "Fechas recibidas en función"
   DISPLAY "v_fecha_ini: ",v_fecha_ini
   DISPLAY "v_fecha_fin: ",v_fecha_fin

   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_movimientos FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_movimientos USING v_id_derechohabiente
   
   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "'','',0,",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                    "FROM tmp_movimiento mov, ",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria ccm, ",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,41,42,44,55) ",  -- SE CONSIDERA SUBCUENTA 41
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento NOT IN (1,41,51,61,81,211,221,451,691,731,741,751,761,771,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ",
                     "AND ccm.movimiento  = cat.movimiento ",   
                   "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",      
                     "UNION ",
                "SELECT ", 
                        "mov.f_liquida, ",
                        "fn_periodo_bimestre(periodo_pago,cta.origen_archivo), nrp, num_crd_ifv, ",
                        "folio_liquida,",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "spre.subcuenta_desc, ",
                        "SUM(mov.monto_pesos), ",
                        "mov.id_referencia ",
                    "FROM tmp_movimiento mov, cta_his_pagos cta, ",
                         " cat_muestra_mov mmov, ",
                         " cat_movimiento cat,",
                         " cat_movimiento_categoria ccm, ",
                         " cat_subcuenta_mov spre ",
                    "WHERE mov.f_liquida >= MDY(10,1,2012) ",
                     "AND mov.f_liquida >= ? ", --Fecha Inicio
                     "AND mov.f_liquida <= ? ", --Fecha Fin    
                     "AND mov.subcuenta IN (4,8,41,42,44,55) ",  -- SE CONSIDERA SUBCUENTA 41
                     "AND spre.subcuenta = mov.subcuenta ",
                     "AND mov.fondo_inversion <> 0 ",
                     "AND mov.movimiento IN (1,41,51,61,81,211,221,451,691,731,741,751,761,771,1001) ",
                     "AND mmov.movimiento = mov.movimiento ", 
                     "AND cat.movimiento  = mmov.movimiento ",  
                     "AND ccm.movimiento  = cat.movimiento ", 
                     "AND cta.folio = mov.folio_liquida ",
                     "AND cta.id_referencia = mov.id_referencia " ,
                     --"AND cta.id_derechohabiente = ? " ,            
                     "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,13 ",      
                     "ORDER BY 1 DESC, 8,2 DESC "
                            
      PREPARE exe_consulta FROM v_query
      DECLARE cur_consulta CURSOR FOR exe_consulta

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL
      
      FOREACH cur_consulta USING --v_id_derechohabiente,
                                 v_fecha_ini,
                                 v_fecha_fin,
                                 v_fecha_ini,
                                 v_fecha_fin --,
                                 --v_id_derechohabiente
                           INTO  v_arr_mov[v_cont].*
            
            LET v_cont = v_cont + 1  
      END FOREACH 

      CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

      CLOSE cur_consulta
      FREE cur_consulta
      
      CALL fn_load_out(4)
      --CALL fn_load_out_reporte2()
      
 END FUNCTION 

FUNCTION fn_load_out(p_categoria)--Función que carga el arreglo de salida evaluando el campo desc_ciudadana 
                      --si desc_ciudadana es null se utilizara el campo movimiento_desc la salida
    DEFINE v_cont               INTEGER 
    DEFINE v_indice             INTEGER
    DEFINE v_cargo_mov_desc     VARCHAR(60)
    DEFINE v_cargo_mov_desc_pen VARCHAR(60)
    DEFINE v_query              STRING 
    DEFINE bandera              SMALLINT
    DEFINE p_categoria          SMALLINT
    
    
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
    PREPARE nom_patron1 FROM v_query
   -- DISPLAY "=====================ARR FINAL=================================="
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


         --IF v_arr_mov[v_cont].tipo = 1 THEN -- No es para todos los abonos, sino los  movimientos especificados abajo.
        IF v_arr_mov[v_cont].movimiento = 1  --IF de movimientos de registro de pagos
         OR  v_arr_mov[v_cont].movimiento = 41
         OR  v_arr_mov[v_cont].movimiento = 51 
         OR  v_arr_mov[v_cont].movimiento = 61 
         OR  v_arr_mov[v_cont].movimiento = 81
         OR  v_arr_mov[v_cont].movimiento = 211 
         OR  v_arr_mov[v_cont].movimiento = 221 
         OR  v_arr_mov[v_cont].movimiento = 451 
         OR  v_arr_mov[v_cont].movimiento = 691
         OR  v_arr_mov[v_cont].movimiento = 1001 
         OR  v_arr_mov[v_cont].movimiento = 731
         OR  v_arr_mov[v_cont].movimiento = 741
         OR  v_arr_mov[v_cont].movimiento = 751
         OR  v_arr_mov[v_cont].movimiento = 761 
         OR  v_arr_mov[v_cont].movimiento = 771 THEN 
            
            IF v_arr_mov[v_cont].subcuenta = 41 AND v_arr_mov[v_cont].tipo = 1 THEN
        	     LET arr_cargos_y_abonos[v_indice].v_desc_concepto  = "Retención salarial"    	
            END IF 
            --Verifica,ps que exista el perioro de pago 
            IF v_arr_mov[v_cont].periodo_pago = " " OR v_arr_mov[v_cont].periodo_pago IS NULL OR v_arr_mov[v_cont].movimiento = 1001 THEN 
            ELSE  
                 IF v_arr_mov[v_cont].subcuenta = 55 OR v_arr_mov[v_cont].movimiento = 691 THEN --pagos a voluntarias RISS
                    LET v_arr_mov[v_cont].nrp = v_arr_mov[v_cont].nrp CLIPPED
                    
                    LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto.trim(), "\n",
                                                                            "Año-Bimestre: ",v_arr_mov[v_cont].periodo_pago
                    
                ELSE 
               
                    LET v_arr_mov[v_cont].nrp = v_arr_mov[v_cont].nrp CLIPPED
                    IF v_arr_mov[v_cont].nrp =" " OR v_arr_mov[v_cont].nrp IS NULL THEN 
                        LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto.trim(), "\n",
                                                                            "Año-Bimestre: ",v_arr_mov[v_cont].periodo_pago --," - NRP " 
                    ELSE 
                        LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto.trim(), "\n",
                                                                        "Año-Bimestre: ",v_arr_mov[v_cont].periodo_pago," - NRP ",v_arr_mov[v_cont].nrp 
                    END IF 
                END IF 
            END IF 
            

        END IF -- If 11 Movimientos con bimestre y NRP en su caso
            
        IF v_arr_mov[v_cont].movimiento = 1  --   Movimientos a agregar el nombre del patrón
        OR  v_arr_mov[v_cont].movimiento = 41
        OR  v_arr_mov[v_cont].movimiento = 51 
        OR  v_arr_mov[v_cont].movimiento = 61 
        OR  v_arr_mov[v_cont].movimiento = 81
        OR  v_arr_mov[v_cont].movimiento = 211 
        OR  v_arr_mov[v_cont].movimiento = 221 
        OR  v_arr_mov[v_cont].movimiento = 451 THEN
        
            EXECUTE nom_patron1 USING v_arr_mov[v_cont].nrp 
                                          INTO v_arr_mov[v_cont].nom_patron
               
            LET v_arr_mov[v_cont].nom_patron = v_arr_mov[v_cont].nom_patron CLIPPED
               
            IF v_arr_mov[v_cont].nom_patron = "" AND v_arr_mov[v_cont].nom_patron IS NULL THEN 
            ELSE 
               LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto,
                                                                        "\n",v_arr_mov[v_cont].nom_patron
            END IF           
        END IF -- IF Movimientos con nombre del patrón
        
        IF v_arr_mov[v_cont].subcuenta = 41 AND v_arr_mov[v_cont].tipo = 1 THEN
        	 LET arr_cargos_y_abonos[v_indice].v_cve_movimiento = 0 USING '##&&' 	
        END IF 
         
        --Obtenemos el número de crédito-----------------------------------------------------
        IF v_arr_mov[v_cont].movimiento = 502 THEN --if 6

            SELECT num_credito 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_det_avance_pago
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_det_avance_pago = v_arr_mov[v_cont].id_referencia
            
            IF v_arr_mov[v_cont].num_credito = "" OR v_arr_mov[v_cont].num_credito IS NULL OR v_arr_mov[v_cont].num_credito=0 THEN

            ELSE             
               LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF
        END IF --FIN if 6

        IF v_arr_mov[v_cont].movimiento = 2 OR v_arr_mov[v_cont].movimiento = 72 OR  v_arr_mov[v_cont].movimiento = 892 THEN --if 8

            SELECT num_crd_ifv 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_interface_ef
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_interface_ef = v_arr_mov[v_cont].id_referencia

            IF v_arr_mov[v_cont].num_credito = "" OR v_arr_mov[v_cont].num_credito IS NULL OR v_arr_mov[v_cont].num_credito=0 THEN 

            ELSE 
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF 

            
              
         END IF --FIN if 8

        IF v_arr_mov[v_cont].movimiento = 632 --if 10
        OR  v_arr_mov[v_cont].movimiento = 42
        OR  v_arr_mov[v_cont].movimiento = 52
        OR  v_arr_mov[v_cont].movimiento = 872 
        OR  v_arr_mov[v_cont].movimiento = 942 
        OR  v_arr_mov[v_cont].movimiento = 1492 
        OR  v_arr_mov[v_cont].movimiento = 1532 
        OR  v_arr_mov[v_cont].movimiento = 1542 
        OR  v_arr_mov[v_cont].movimiento = 1752 
        OR  v_arr_mov[v_cont].movimiento = 1762 
        OR  v_arr_mov[v_cont].movimiento = 1772 THEN 

            SELECT FIRST 1 num_crd_ifv 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_interface_hs
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_interface_hs = v_arr_mov[v_cont].id_referencia

           IF v_arr_mov[v_cont].num_credito = "" OR v_arr_mov[v_cont].num_credito IS NULL OR v_arr_mov[v_cont].num_credito=0 THEN

            ELSE 
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF 

        END IF --FIN if 10

    END IF --Fin If 1

    
    LET movimientos_final[v_cont].fLiquidacion = arr_cargos_y_abonos[v_cont].v_f_aplicacion
    LET movimientos_final[v_cont].movimiento = arr_cargos_y_abonos[v_cont].v_cve_movimiento
    LET movimientos_final[v_cont].movimiento_desc = arr_cargos_y_abonos[v_cont].v_desc_concepto
    LET movimientos_final[v_cont].subcuenta = arr_cargos_y_abonos[v_cont].v_tpo_movimiento
    LET movimientos_final[v_cont].abono = arr_cargos_y_abonos[v_cont].v_abono
    LET movimientos_final[v_cont].cargo = arr_cargos_y_abonos[v_cont].v_cargo
    
    {DISPLAY "fLiquidacion: ",movimientos_final[v_cont].fLiquidacion
    DISPLAY "movimiento: ",movimientos_final[v_cont].movimiento
    DISPLAY "movimiento_desc: ",movimientos_final[v_cont].movimiento_desc
    DISPLAY "subcuenta: ",movimientos_final[v_cont].subcuenta
    DISPLAY "abono: ",movimientos_final[v_cont].abono
    DISPLAY "cargo: ",movimientos_final[v_cont].cargo
    DISPLAY "============================================================="}
    LET v_indice = v_indice + 1
         
    END FOR 
    CALL arr_cargos_y_abonos.deleteElement(v_indice)

   -- RETURN arr_cargos_y_abonos
END FUNCTION 


FUNCTION fn_load_out_reporte2(p_categoria)--Función que carga el arreglo de salida evaluando el campo desc_ciudadana 
                      --si desc_ciudadana es null se utilizara el campo movimiento_desc la salida
    DEFINE v_cont             INTEGER
    DEFINE v_count            INTEGER
    DEFINE v_indice           INTEGER
    DEFINE v_cargo_mov_desc   VARCHAR(60)
    DEFINE v_cargo_mov_desc_pen VARCHAR(60)
    DEFINE v_query            STRING 
    DEFINE bandera            SMALLINT
    DEFINE p_categoria        SMALLINT
    DEFINE v_id_dis_interface DECIMAL(10,0)
    DEFINE v_folio_liquida    DECIMAL(9,0)
    
    
    
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
    PREPARE nom_patron2 FROM v_query
   -- DISPLAY "=====================ARR FINAL=================================="
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
           
        
 
            --    CALL fn_mensaje("","v_id_derechohabiente: "||v_id_derechohabiente||"\nid_referencia"||v_arr_mov[v_cont].id_referencia,"")
            
        --IF v_arr_mov[v_cont].tipo = 1 THEN -- Solo es para los movimientos por registro de pagos.
         IF v_arr_mov[v_cont].movimiento = 1  --If  de Movimientos de Registros de pagos.
         OR  v_arr_mov[v_cont].movimiento = 41
         OR  v_arr_mov[v_cont].movimiento = 51 
         OR  v_arr_mov[v_cont].movimiento = 61 
         OR  v_arr_mov[v_cont].movimiento = 81
         OR  v_arr_mov[v_cont].movimiento = 211 
         OR  v_arr_mov[v_cont].movimiento = 221 
         OR  v_arr_mov[v_cont].movimiento = 451 
         OR  v_arr_mov[v_cont].movimiento = 691
         OR  v_arr_mov[v_cont].movimiento = 1001 
         OR  v_arr_mov[v_cont].movimiento = 731
         OR  v_arr_mov[v_cont].movimiento = 741
         OR  v_arr_mov[v_cont].movimiento = 751
         OR  v_arr_mov[v_cont].movimiento = 761 
         OR  v_arr_mov[v_cont].movimiento = 771 THEN 
         	
            
            IF v_arr_mov[v_cont].movimiento = 1  -- IF  para solo movimientos con contraparte
               OR  v_arr_mov[v_cont].movimiento = 41
               OR  v_arr_mov[v_cont].movimiento = 51 
               OR  v_arr_mov[v_cont].movimiento = 61 
               OR  v_arr_mov[v_cont].movimiento = 81
               OR  v_arr_mov[v_cont].movimiento = 211 
               OR  v_arr_mov[v_cont].movimiento = 221 
               OR  v_arr_mov[v_cont].movimiento = 451 
               OR  v_arr_mov[v_cont].movimiento = 691
               OR  v_arr_mov[v_cont].movimiento = 1001 THEN 
             
            	   IF v_arr_mov[v_cont].num_credito IS NOT NULL AND v_arr_mov[v_cont].num_credito > 0 THEN
            	 	  IF p_categoria = 1 OR p_categoria = 6 THEN -- Para verificar si el abono no tuvo un cargo por crédito y no sería parte de los movs que aumentaron tu ahorro
            	 	     CALL arr_cargos_y_abonos.deleteElement(v_indice)
            	 	     CONTINUE FOR
            	 	  END IF
            	 ELSE
                  LET v_id_dis_interface = NULL
                  LET v_folio_liquida = NULL
                  
                  SELECT FIRST 1 hs.id_dis_interface_hs, hs.folio_liquida, hs.num_crd_ifv
                    INTO v_id_dis_interface, v_folio_liquida, v_arr_mov[v_cont].num_credito
                    FROM dis_interface_hs hs, glo_folio g
                   WHERE g.folio_referencia = v_arr_mov[v_cont].folio
                     AND hs.folio_liquida   = g.folio
                     AND id_derechohabiente = v_id_derechohabiente

                  IF SQLCA.sqlcode <> NOTFOUND THEN -- IF del NOTFOUND
                     IF p_categoria = 1 OR p_categoria = 6 THEN -- Para verificar si el abono no tuvo un cargo por crédito y no sería parte de los movs que aumentaron tu ahorro
                  	   CALL arr_cargos_y_abonos.deleteElement(v_indice)
                  	   CONTINUE FOR
                     ELSE
                        SELECT COUNT(*)
                          INTO v_count
                          FROM tmp_movimiento mov,
                               cat_muestra_mov mmov
                         WHERE mov.folio_liquida = v_folio_liquida
                           AND mov.id_referencia = v_id_dis_interface
                           AND mmov.movimiento = mov.movimiento

                        IF v_count = 0 THEN   
                           CALL arr_cargos_y_abonos.deleteElement(v_indice)
                  	       CONTINUE FOR
                        END IF
                  	 END IF
                  ELSE
                  LET v_id_dis_interface = NULL
                  LET v_folio_liquida = NULL
                  SELECT FIRST 1 ef.id_dis_interface_ef, ef.folio_liquida,ef.num_crd_ifv 
                    INTO v_id_dis_interface, v_folio_liquida,v_arr_mov[v_cont].num_credito
                    FROM dis_interface_ef ef, glo_folio g
                   WHERE g.folio_referencia = v_arr_mov[v_cont].folio
                     AND ef.folio_liquida   = g.folio
                     AND id_derechohabiente = v_id_derechohabiente
                                         
                  IF SQLCA.sqlcode <> NOTFOUND THEN --v_arr_mov[v_cont].num_credito IS NOT NULL AND v_arr_mov[v_cont].num_credito > 0 THEN -- IF 2 del crédito
                  	IF p_categoria = 1 OR p_categoria = 6 THEN -- Para verificar si el abono no tuvo un cargo por crédito y no sería parte de los movs que aumentaron tu ahorro
                  	   CALL arr_cargos_y_abonos.deleteElement(v_indice)
                  	   CONTINUE FOR
                    ELSE
                        SELECT COUNT(*)
                          INTO v_count
                          FROM tmp_movimiento mov,
                               cat_muestra_mov mmov
                         WHERE mov.folio_liquida = v_folio_liquida
                           AND mov.id_referencia = v_id_dis_interface
                           AND mmov.movimiento = mov.movimiento
                           
                        IF v_count = 0 THEN
                           CALL arr_cargos_y_abonos.deleteElement(v_indice)
                  	       CONTINUE FOR
                        END IF
                  	END IF
                  ELSE
                  	    IF p_categoria = 3 THEN
                  	    	 CALL arr_cargos_y_abonos.deleteElement(v_indice)
                  	         CONTINUE FOR
                  	    END IF
                     --END IF -- IF 3 del crédito
                  END IF -- IF 2 del crédito

                END IF -- IF del NOTFOUND
                
               END IF -- IF 1 del crédito
               
               
            END IF -- IF solo movimientos con contraparte
          
            
            IF v_arr_mov[v_cont].subcuenta = 41 AND v_arr_mov[v_cont].tipo = 1 THEN
        	     LET arr_cargos_y_abonos[v_indice].v_desc_concepto  = "Retención salarial"    	
            END IF 
            
            IF v_arr_mov[v_cont].periodo_pago = " " OR v_arr_mov[v_cont].periodo_pago IS NULL OR v_arr_mov[v_cont].movimiento = 1001 THEN 
            ELSE  
                IF v_arr_mov[v_cont].subcuenta = 55 OR v_arr_mov[v_cont].movimiento = 691 THEN --pagos a voluntarias RISS
                    LET v_arr_mov[v_cont].nrp = v_arr_mov[v_cont].nrp CLIPPED
                    
                    LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto.trim(), "\n",
                                                                            "Año-Bimestre: ",v_arr_mov[v_cont].periodo_pago
                    
                ELSE 
               
                    LET v_arr_mov[v_cont].nrp = v_arr_mov[v_cont].nrp CLIPPED
                    IF v_arr_mov[v_cont].nrp =" " OR v_arr_mov[v_cont].nrp IS NULL THEN 
                        LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto.trim(), "\n",
                                                                            "Año-Bimestre: ",v_arr_mov[v_cont].periodo_pago --," - NRP " 
                    ELSE 
                        LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto.trim(), "\n",
                                                                        "Año-Bimestre: ",v_arr_mov[v_cont].periodo_pago," - NRP ",v_arr_mov[v_cont].nrp 
                    END IF 
                END IF -- IF  Movimientos de RISS, solo bimestre
            END IF  -- IF para verificar si tiene periodo de pago.
 
            IF v_arr_mov[v_cont].movimiento = 1  OR --If 11
               v_arr_mov[v_cont].movimiento = 41 OR
               v_arr_mov[v_cont].movimiento = 51 OR
               v_arr_mov[v_cont].movimiento = 61 OR
               v_arr_mov[v_cont].movimiento = 81 OR
               v_arr_mov[v_cont].movimiento = 211 OR
               v_arr_mov[v_cont].movimiento = 221 OR
               v_arr_mov[v_cont].movimiento = 451 THEN
               
               
                   --Obtenemos el nombre del Patron
               EXECUTE nom_patron2 USING v_arr_mov[v_cont].nrp 
                                          INTO v_arr_mov[v_cont].nom_patron
               
               LET v_arr_mov[v_cont].nom_patron = v_arr_mov[v_cont].nom_patron CLIPPED
               
               IF v_arr_mov[v_cont].nom_patron = "" AND v_arr_mov[v_cont].nom_patron IS NULL  THEN
               ELSE 
                     LET arr_cargos_y_abonos[v_indice].v_desc_concepto = arr_cargos_y_abonos[v_indice].v_desc_concepto,
                                                                        "\n",v_arr_mov[v_cont].nom_patron
               END IF 
                
            END IF --IF de movimientos de solo patrón
         
           IF v_arr_mov[v_cont].subcuenta = 41 AND v_arr_mov[v_cont].tipo = 1 THEN
        	   LET arr_cargos_y_abonos[v_indice].v_cve_movimiento = 0 USING '##&&'
           END IF       
         END IF -- IF de movimientos de registro de pagos.

         
        --Obtenemos el número de crédito-----------------------------------------------------
        IF v_arr_mov[v_cont].movimiento = 502 THEN --if 6

            SELECT num_credito 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_det_avance_pago
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_det_avance_pago = v_arr_mov[v_cont].id_referencia
            
            IF v_arr_mov[v_cont].num_credito = "" OR v_arr_mov[v_cont].num_credito IS NULL OR v_arr_mov[v_cont].num_credito=0 THEN

            ELSE             
               LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF
        END IF --FIN if 6

        IF v_arr_mov[v_cont].movimiento = 2 OR v_arr_mov[v_cont].movimiento = 72 OR  v_arr_mov[v_cont].movimiento = 892 THEN --if 8

            SELECT num_crd_ifv 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_interface_ef
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_interface_ef = v_arr_mov[v_cont].id_referencia

            IF v_arr_mov[v_cont].num_credito = "" OR v_arr_mov[v_cont].num_credito IS NULL OR v_arr_mov[v_cont].num_credito=0 THEN 

            ELSE 
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF 

            
              
         END IF --FIN if 8

        IF v_arr_mov[v_cont].movimiento = 632 --if 10
        OR  v_arr_mov[v_cont].movimiento = 42
        OR  v_arr_mov[v_cont].movimiento = 52
        OR  v_arr_mov[v_cont].movimiento = 872 
        OR  v_arr_mov[v_cont].movimiento = 942 
        OR  v_arr_mov[v_cont].movimiento = 1492
        OR  v_arr_mov[v_cont].movimiento = 1532 
        OR  v_arr_mov[v_cont].movimiento = 1542 
        OR  v_arr_mov[v_cont].movimiento = 1752 
        OR  v_arr_mov[v_cont].movimiento = 1762 
        OR  v_arr_mov[v_cont].movimiento = 1772 THEN 

            SELECT FIRST 1 num_crd_ifv 
            INTO v_arr_mov[v_cont].num_credito
            FROM dis_interface_hs
            WHERE id_derechohabiente = v_id_derechohabiente
            AND id_dis_interface_hs = v_arr_mov[v_cont].id_referencia

           IF v_arr_mov[v_cont].num_credito = "" OR v_arr_mov[v_cont].num_credito IS NULL OR v_arr_mov[v_cont].num_credito=0 THEN

            ELSE 
                LET arr_cargos_y_abonos[v_indice].v_desc_concepto =  arr_cargos_y_abonos[v_indice].v_desc_concepto,"\n",v_arr_mov[v_cont].num_credito
            END IF 

        END IF --FIN if 10

    END IF --Fin If 1

    
    LET movimientos_final[v_indice].fLiquidacion = arr_cargos_y_abonos[v_indice].v_f_aplicacion
    LET movimientos_final[v_indice].movimiento = arr_cargos_y_abonos[v_indice].v_cve_movimiento
    LET movimientos_final[v_indice].movimiento_desc = arr_cargos_y_abonos[v_indice].v_desc_concepto
    LET movimientos_final[v_indice].subcuenta = arr_cargos_y_abonos[v_indice].v_tpo_movimiento
    LET movimientos_final[v_indice].abono = arr_cargos_y_abonos[v_indice].v_abono
    LET movimientos_final[v_indice].cargo = arr_cargos_y_abonos[v_indice].v_cargo
    
    {DISPLAY "fLiquidacion: ",movimientos_final[v_cont].fLiquidacion
    DISPLAY "movimiento: ",movimientos_final[v_cont].movimiento
    DISPLAY "movimiento_desc: ",movimientos_final[v_cont].movimiento_desc
    DISPLAY "subcuenta: ",movimientos_final[v_cont].subcuenta
    DISPLAY "abono: ",movimientos_final[v_cont].abono
    DISPLAY "cargo: ",movimientos_final[v_cont].cargo
    DISPLAY "============================================================="}
    LET v_indice = v_indice + 1
         
    END FOR 
    
    
    CALL arr_cargos_y_abonos.deleteElement(v_indice)
    
END FUNCTION 

