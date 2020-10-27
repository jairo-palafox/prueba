################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 19/03/2020                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CTA                                                      #
#Programa          => CTAC012                                                  #
#Objetivo          => Servicio web para la publicacion del detalle de          #
#                     movimientos                                              #
#Fecha inicio      => MARZO 2020                                               #
################################################################################
DATABASE safre_viv

GLOBALS "CTAC012.inc"
PRIVATE DEFINE v_arr_mov     DYNAMIC ARRAY OF movimientos
PRIVATE DEFINE movimientos_gral DYNAMIC ARRAY OF movimiento_otros
PRIVATE DEFINE v_cont        INTEGER
PRIVATE DEFINE v_query       STRING
PRIVATE DEFINE v_querynrp    STRING
PRIVATE DEFINE v_querypatron STRING
PRIVATE DEFINE tabla_patron  VARCHAR(40)


FUNCTION fn_consulta_hist(p_trabajador,p_nss,p_opcion,p_f_ini,p_f_fin)--Funcion que carga el record que serà regreasado por el servicio
   DEFINE p_trabajador      trabajador,
          p_nss             VARCHAR(11),
          p_opcion          SMALLINT,
          p_f_ini           DATE,
          p_f_fin           DATE 

   LET g_nss      = p_nss
   LET g_opcion   = p_opcion
   LET g_finicio  = p_f_ini
   LET g_ffin     = p_f_fin

   SELECT tabla
   INTO tabla_patron  ---Obtenemos la tabla activa actual
   FROM pag_ctr_patron   
          
   LET v_querynrp = "SELECT raz FROM ",tabla_patron, " WHERE nrp = ? "
   PREPARE prepnom_patron FROM v_querynrp

   LET v_querypatron = "SELECT nrp FROM ",tabla_patron, " WHERE raz = ? "
   PREPARE prep_nrp FROM v_querypatron

   LET v_cont = 1
  
   CASE g_opcion
      WHEN 1
         CALL fn_movimientos_aumenta(p_trabajador.*)
         CALL fn_movimientos_otros(p_trabajador.*)
      WHEN 2
         CALL fn_movimientos_aumenta(p_trabajador.*)
      WHEN 3
         CALL fn_movimientos_otros(p_trabajador.*)
   END CASE

 
   CALL fn_ordena_movimientos() 

   RETURN movimientos_gral
    
END FUNCTION

FUNCTION fn_movimientos_aumenta(p_trabajador)
   DEFINE p_trabajador             trabajador
   DEFINE v_rfc10                  CHAR(10)
      
      #Se agregan los registros del historico de movimientos TRM

      LET v_query =  "SELECT ",
                        "trm.f_pago, ",
                        "2, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "69, ",                    #Valor fijo para identificar los movimientos de TRM
                        "subc.subcuenta_desc, ",
                        "trm.aportacion, ",
                        "trm.periodo_pago, ",
                        "trm.nrp, ",
                        "'' ",
                     "FROM safre_his@vivht_tcp:his_pag_bim_trm trm ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                     "WHERE trm.nss = ? ",
                     "AND trm.tpo_aclaracion = 99 ",
                     "AND ((trm.cve_aportacion =  '' AND  trm.aportacion IS NOT NULL) ",
                            "OR trm.cve_aportacion = 2)",
                     "AND trm.f_pago > ? ",
                     "AND trm.f_pago <= MDY(10,31,2012) ",
                     "ORDER BY trm.f_pago DESC "

      PREPARE exe_consulta_trm_aum FROM v_query
      DECLARE cur_consulta_trm_aum CURSOR FOR exe_consulta_trm_aum

      FOREACH cur_consulta_trm_aum USING p_trabajador.nss, g_finicio INTO v_arr_mov[v_cont].*
         IF v_arr_mov[v_cont].bimestre[1] = '9' THEN
            LET v_arr_mov[v_cont].bimestre = '19',v_arr_mov[v_cont].bimestre
         ELSE
            LET v_arr_mov[v_cont].bimestre = '20',v_arr_mov[v_cont].bimestre
         END IF
             
         EXECUTE prepnom_patron USING v_arr_mov[v_cont].nrp INTO v_arr_mov[v_cont].nombre_patron
             
         LET v_cont = v_cont + 1  
      END FOREACH

      IF g_finicio <= MDY(9,1,2005) THEN

          LET v_rfc10 = p_trabajador.rfc
          #Se agregan los registros del historico de movimientos de Pensiones ADS 1997 - 2005
          LET v_query =  "SELECT ",
                            "pen.f_pago, ",
                            "pen.clave, ",
                            "cat.movimiento_desc, ",
                            "cat.tipo, ",
                            "cat.desc_ciudadana, ",
                            "70, ",         --Valor fijo para identificar los movimientos de pensiones
                            "subc.subcuenta_desc, ",
                            "pen.aportacion, ",
                            "pen.bimestre, ",
                            "pen.nrp, ",
                            "pen.patron ",
                         "FROM safre_his@vivht_tcp:his_pag_pen_ads pen ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE (pen.nss = '",p_trabajador.nss CLIPPED, "' OR pen.rfc[1,10] = '",v_rfc10 CLIPPED,"' OR pen.rfc = '",p_trabajador.rfc CLIPPED,"') ",
                         "AND pen.clave IN ('105','115','125','135','155') ",
                         "AND pen.f_pago >= ? ",
                         "AND pen.f_pago <= MDY(8,31,2005) ",
                         "ORDER BY pen.f_pago DESC "
          PREPARE exe_consulta_pen_aum FROM v_query
          DECLARE cur_consulta_pen_aum CURSOR FOR exe_consulta_pen_aum
          FOREACH cur_consulta_pen_aum USING g_finicio INTO  v_arr_mov[v_cont].*
             LET v_cont = v_cont + 1  
          END FOREACH 
      END IF

      IF g_finicio <= MDY(9,1,1997) THEN
          #Se agregan los registros del historico de movimientos de ADS 1992 - 1997
          LET v_query =  "SELECT ",
                            "ads.f_pago, ",
                            "ads.clave, ",
                            "cat.movimiento_desc, ",
                            "cat.tipo, ",
                            "cat.desc_ciudadana, ",
                            "71, ",                    --Valor fijo para identificar los movimientos de ADS Bimestral
                            "subc.subcuenta_desc, ",
                            "ads.importe, ",
                            "trim(ads.ano_pago)||ads.bimestre, ",
                            "'', ",
                            "ads.patron ",
                         "FROM safre_his@vivht_tcp:his_pag_bim_ads ads ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 8 ",
                         "WHERE (ads.nss = '",p_trabajador.nss CLIPPED, "' OR ads.rfc[1,10] = '",v_rfc10 CLIPPED,"' OR ads.rfc = '",p_trabajador.rfc CLIPPED,"') ",
                         "AND ads.clave IN ('105','115','125','135','155') ",
                         "AND ads.f_pago >= ? ",
                         "AND ads.f_pago <= MDY(8,31,1997) ",
                         "ORDER BY ads.f_pago DESC "
                         
          PREPARE exe_consulta_ads_otro FROM v_query
          DECLARE cur_consulta_ads_otro CURSOR FOR exe_consulta_ads_otro
             
          FOREACH cur_consulta_ads_otro USING g_finicio INTO  v_arr_mov[v_cont].*
             IF v_arr_mov[v_cont].bimestre[1] = '9' THEN
                LET v_arr_mov[v_cont].bimestre = '19',v_arr_mov[v_cont].bimestre
             ELSE
                LET v_arr_mov[v_cont].bimestre = '20',v_arr_mov[v_cont].bimestre
             END IF
             
             EXECUTE prep_nrp USING v_arr_mov[v_cont].nombre_patron INTO v_arr_mov[v_cont].nrp
             IF v_arr_mov[v_cont].nrp IS NULL THEN
                LET v_arr_mov[v_cont].nrp = '###########'
             END IF
             
             LET v_cont = v_cont + 1  
          END FOREACH
      END IF
      
END FUNCTION

FUNCTION fn_movimientos_otros(p_trabajador)
   DEFINE p_trabajador             trabajador
   DEFINE v_rfc10                  CHAR(10)   
      
      #Se agregan los registros del historico de movimientos TRM

      LET v_query =  "SELECT ",
                        "trm.f_pago, ",
                        "trm.cve_aportacion, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "69, ",                    #Valor fijo para identificar los movimientos de TRM
                        "subc.subcuenta_desc, ",
                        "CASE ", 
                           "WHEN trm.cve_aportacion = 1 THEN trm.aportacion ",
                           "ELSE trm.amortizacion ",
                        "END ",
                        "as aportacion, ",
                        "trm.periodo_pago, ",
                        "trm.nrp, ",
                        "'' ",
                     "FROM safre_his@vivht_tcp:his_pag_bim_trm trm ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                     "WHERE trm.nss = ? ",
                     "AND trm.tpo_aclaracion = 99 ",
                     "AND ((trm.cve_aportacion =  '' AND  trm.amortizacion IS NOT NULL) ",
                            "OR trm.cve_aportacion = 1)",
                     "AND trm.f_pago > ? ",
                     "AND trm.f_pago <= MDY(10,31,2012) ",
                     "ORDER BY trm.f_pago DESC "

      PREPARE exe_consulta_trm_otro FROM v_query
      DECLARE cur_consulta_trm_otro CURSOR FOR exe_consulta_trm_otro

      FOREACH cur_consulta_trm_otro USING p_trabajador.nss, g_finicio INTO v_arr_mov[v_cont].*
         IF v_arr_mov[v_cont].bimestre[1] = '9' THEN
            LET v_arr_mov[v_cont].bimestre = '19',v_arr_mov[v_cont].bimestre
         ELSE
            LET v_arr_mov[v_cont].bimestre = '20',v_arr_mov[v_cont].bimestre
         END IF
             
         EXECUTE prepnom_patron USING v_arr_mov[v_cont].nrp INTO v_arr_mov[v_cont].nombre_patron
             
         LET v_cont = v_cont + 1  
      END FOREACH

      IF g_finicio <= MDY(9,1,2005) THEN

          LET v_rfc10 = p_trabajador.rfc
          #Se agregan los registros del historico de movimientos de Pensiones ADS 1997 - 2005
          LET v_query =  "SELECT ",
                            "pen.f_pago, ",
                            "pen.clave, ",
                            "cat.movimiento_desc, ",
                            "cat.tipo, ",
                            "cat.desc_ciudadana, ",
                            "70, ",         --Valor fijo para identificar los movimientos de pensiones
                            "subc.subcuenta_desc, ",
                            "CASE ", 
                               "WHEN pen.clave = '102' THEN pen.amortizacion ",
                               "ELSE pen.aportacion ",
                            "END ",
                            "as aportacion, ",
                            "pen.bimestre, ",
                            "pen.nrp, ",
                            "pen.patron ",
                         "FROM safre_his@vivht_tcp:his_pag_pen_ads pen ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE (pen.nss = '",p_trabajador.nss CLIPPED, "' OR pen.rfc[1,10] = '",v_rfc10 CLIPPED,"' OR pen.rfc = '",p_trabajador.rfc CLIPPED,"') ",
                         "AND (pen.clave IN ('106','116','126','136','156') OR ",
                         "(pen.clave = '102' AND (pen.amortizacion IS NOT NULL OR pen.amortizacion != 0))) ",
                         "AND pen.f_pago >= ? ",
                         "AND pen.f_pago <= MDY(8,31,2005) ",
                         "ORDER BY pen.f_pago DESC "
          PREPARE exe_consulta_pen_otro FROM v_query
          DECLARE cur_consulta_pen_otro CURSOR FOR exe_consulta_pen_otro
          FOREACH cur_consulta_pen_otro USING g_finicio INTO  v_arr_mov[v_cont].*
             LET v_cont = v_cont + 1  
          END FOREACH 
      END IF
      
END FUNCTION

FUNCTION fn_ordena_movimientos()--Función que carga el arreglo de salida evaluando el campo desc_ciudadana 
                                --si desc_ciudadana es null se utilizara el campo movimiento_desc la salida
    DEFINE v_cont             SMALLINT 
    DEFINE v_indice           SMALLINT
    DEFINE v_cargo_mov_desc   VARCHAR(60)
    DEFINE v_cargo_mov_desc_pen VARCHAR(60)
    
    CALL movimientos_gral.clear()

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
    FOR v_cont = 1 TO v_arr_mov.getLength()
      IF v_arr_mov[v_cont].f_liquida IS NOT NULL THEN
         IF v_arr_mov[v_cont].desc_ciudadana IS NULL THEN 
            LET movimientos_gral[v_indice].movimiento_desc = v_arr_mov[v_cont].movimiento_desc
         ELSE 
            LET movimientos_gral[v_indice].movimiento_desc = v_arr_mov[v_cont].desc_ciudadana
         END IF
         
         LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
         LET movimientos_gral[v_indice].movimiento = v_arr_mov[v_cont].movimiento USING '##&&'
         #LET movimientos_gral[v_indice].movimiento_desc = v_arr_mov[v_cont].movimiento_desc
         LET movimientos_gral[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc 

         IF v_arr_mov[v_cont].tipo < 0 THEN
            IF v_arr_mov[v_cont].monto_pesos < 0 THEN
               LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos * -1
            ELSE
               LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos
            END IF
            
            LET movimientos_gral[v_indice].abono = NULL
         ELSE
            LET movimientos_gral[v_indice].abono = v_arr_mov[v_cont].monto_pesos
            LET movimientos_gral[v_indice].cargo = NULL
         END IF
         LET v_indice = v_indice + 1

         #Tratamiento especial para los movimientos historicos TRM
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_TRM THEN
            LET movimientos_gral[v_indice - 1].movimiento = '01' USING '##&&'
            IF v_arr_mov[v_cont].movimiento = 1 THEN
               #Se genera un movimiento por el mismo monto de la aportacion
               LET movimientos_gral[v_indice].cargo = NULL 
               LET movimientos_gral[v_indice].abono = v_arr_mov[v_cont].monto_pesos
               LET movimientos_gral[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].movimiento = '01' USING '##&&'
               LET movimientos_gral[v_indice].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice - 1].cargo = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice - 1].abono = NULL
               LET movimientos_gral[v_indice - 1].movimiento = CARGO_INFORMATIVO USING '##&&'
               LET movimientos_gral[v_indice - 1].movimiento_desc = v_cargo_mov_desc
               
               LET v_indice = v_indice + 1
            END IF
            IF v_arr_mov[v_cont].movimiento = 0 THEN
               #Se genera un movimiento por el mismo monto de la aportacion
               LET movimientos_gral[v_indice].abono = NULL 
               LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos
               LET movimientos_gral[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].movimiento = '00' USING '##&&'
               LET movimientos_gral[v_indice].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice - 1].abono = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice - 1].cargo = NULL
               LET movimientos_gral[v_indice - 1].movimiento = CARGO_AMORTIZACION USING '##&&'
               LET movimientos_gral[v_indice - 1].movimiento_desc = v_cargo_mov_desc
               
               LET v_indice = v_indice + 1
            END IF
            IF movimientos_gral[v_indice - 1].movimiento = 01 THEN
               LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                v_arr_mov[v_cont].bimestre, " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
            END IF
            
         END IF

         #Tratamiento especial para los movimientos historicos PENSIONES
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_PEN THEN
            LET movimientos_gral[v_indice - 1].movimiento = '01' USING '##&&'
            IF v_arr_mov[v_cont].movimiento = 106 
            OR v_arr_mov[v_cont].movimiento = 116
            OR v_arr_mov[v_cont].movimiento = 126
            OR v_arr_mov[v_cont].movimiento = 136
            OR v_arr_mov[v_cont].movimiento = 156 THEN
               #Se genera un movimiento por el mismo monto de la aportacion
               LET movimientos_gral[v_indice].cargo = NULL 
               LET movimientos_gral[v_indice].abono = v_arr_mov[v_cont].monto_pesos
               LET movimientos_gral[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].movimiento = '01' USING '##&&'
               LET movimientos_gral[v_indice].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice - 1].cargo = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice - 1].abono = NULL
               LET movimientos_gral[v_indice - 1].movimiento = CARGO_INFORMATIVO_PEN USING '##&&'
               LET movimientos_gral[v_indice - 1].movimiento_desc = v_cargo_mov_desc_pen
               
               LET v_indice = v_indice + 1
            END IF
            IF v_arr_mov[v_cont].movimiento = 102 THEN
               #Se genera un movimiento por el mismo monto de la aportacion
               LET movimientos_gral[v_indice].abono = NULL 
               LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos
               LET movimientos_gral[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].movimiento = '00' USING '##&&'
               LET movimientos_gral[v_indice].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice - 1].abono = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice - 1].cargo = NULL
               LET movimientos_gral[v_indice - 1].movimiento = CARGO_AMORTIZACION_PEN USING '##&&'
               LET movimientos_gral[v_indice - 1].movimiento_desc = v_cargo_mov_desc_pen
               
               LET v_indice = v_indice + 1
            END IF
            IF movimientos_gral[v_indice - 1].movimiento = 01 THEN
               LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                v_arr_mov[v_cont].bimestre, " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
            END IF
         END IF
         #Tratamiento especial para los movimientos historicos BIMESTRAL ADS
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_BIM_ADS THEN
            LET movimientos_gral[v_indice - 1].movimiento = '01' USING '##&&'
            LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                v_arr_mov[v_cont].bimestre, " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
         END IF
      END IF
    END FOR
END FUNCTION 
