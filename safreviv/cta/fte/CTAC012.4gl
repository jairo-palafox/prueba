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
PRIVATE DEFINE v_arr_ads     DYNAMIC ARRAY OF movimientos_ads
PRIVATE DEFINE movimientos_hist DYNAMIC ARRAY OF movimiento_final
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
   
   DEFINE   v_contador           INTEGER
   DEFINE   aux_desc_concepto,
            aux_tpo_movimiento   VARCHAR (255)

   LET g_nss      = p_nss
   LET g_opcion   = p_opcion
   LET g_finicio  = p_f_ini
   LET g_ffin     = p_f_fin

   IF g_ffin > "09-30-2012" THEN
      LET g_ffin = "09-30-2012"
   END IF

   SELECT tabla
   INTO tabla_patron  ---Obtenemos la tabla activa actual
   FROM pag_ctr_patron   
          
   LET v_querynrp = "SELECT raz FROM ",tabla_patron, " WHERE nrp = ? "
   PREPARE prepnom_patron FROM v_querynrp

   LET v_querypatron = "SELECT FIRST 1 nrp FROM ",tabla_patron, " WHERE raz = ? "
   PREPARE prep_nrp FROM v_querypatron

   LET v_cont = 1

   CALL v_arr_mov.clear()
   CALL v_arr_ads.clear()
   CALL movimientos_gral.clear()
   CALL movimientos_hist.clear()
  
   CASE g_opcion
      WHEN 1
         CALL fn_movimientos_aumenta(p_trabajador.*)
         CALL fn_movimientos_otros(p_trabajador.*)
      WHEN 2
         CALL fn_movimientos_aumenta(p_trabajador.*)
      WHEN 3
         CALL fn_movimientos_otros(p_trabajador.*)
      WHEN 6
         CALL fn_movimientos_aumenta(p_trabajador.*)
   END CASE

   CALL fn_ordena_movimientos()

   IF movimientos_gral.getLength() > 0 THEN

      WHENEVER ERROR CONTINUE
         DROP TABLE tmp_resumen_movimientos
      WHENEVER ERROR STOP

      CREATE TEMP TABLE  tmp_resumen_movimientos(      
                    f_aplicacion      DATE,
                    bimestre          CHAR(6),
                    cve_movimiento    CHAR(5),
                    desc_concepto     VARCHAR(255),
                    tpo_movimiento    VARCHAR(255),
                    abono             DECIMAL(16,2),
                    cargo             DECIMAL(16,2),
                    no_movimiento     SMALLINT,
                    id_movimiento     INTEGER
                )
                
      LET v_contador = 1
     
      FOR v_contador = 1 TO movimientos_gral.getLength()
        {DISPLAY movimientos_gral[v_contador].fLiquidacion
        DISPLAY movimientos_gral[v_contador].bimestre
        DISPLAY movimientos_gral[v_contador].movimiento
        DISPLAY movimientos_gral[v_contador].movimiento_desc
        DISPLAY movimientos_gral[v_contador].subcuenta
        DISPLAY movimientos_gral[v_contador].abono
        DISPLAY movimientos_gral[v_contador].cargo
        DISPLAY movimientos_gral[v_contador].no_movimiento
        DISPLAY movimientos_gral[v_contador].id_movimiento}


      
         INSERT INTO tmp_resumen_movimientos VALUES (movimientos_gral[v_contador].*)
      END FOR

      --UNLOAD TO "movimientos.unl" SELECT * FROM tmp_resumen_movimientos

      DECLARE cur_res_mov CURSOR WITH HOLD FOR
         SELECT f_aplicacion   ,
                cve_movimiento ,
                desc_concepto  ,
                tpo_movimiento ,
                abono          ,
                cargo          ,
                no_movimiento  
         FROM   tmp_resumen_movimientos
         ORDER BY f_aplicacion DESC, bimestre DESC, no_movimiento, id_movimiento, cve_movimiento DESC
      
      LET v_contador = 1
  
      FOREACH cur_res_mov INTO movimientos_hist[v_contador].fLiquidacion, movimientos_hist[v_contador].movimiento,
                               aux_desc_concepto, aux_tpo_movimiento,movimientos_hist[v_contador].abono, movimientos_hist[v_contador].cargo
         LET movimientos_hist[v_contador].movimiento_desc = aux_desc_concepto
         LET movimientos_hist[v_contador].subcuenta = aux_tpo_movimiento       
         LET v_contador = v_contador + 1
         LET aux_desc_concepto  = ""
         LET aux_tpo_movimiento = ""    
      END FOREACH

      CALL movimientos_hist.deleteElement(movimientos_hist.getLength())
   END IF

   RETURN movimientos_hist
    
END FUNCTION

FUNCTION fn_movimientos_aumenta(p_trabajador)
   DEFINE p_trabajador             trabajador
   DEFINE v_rfc10                  CHAR(10)
   DEFINE v_cont_ini_ads,           
          v_cont_fin_ads,           
          v_cont_nss,
          v_cont_ads,
          v_cont_rfc               INTEGER
   DEFINE v_arr_nss                DYNAMIC ARRAY OF RECORD
          nss                      CHAR(11)
          END RECORD
      
      #Se agregan los registros del historico de movimientos TRM

      IF g_ffin >= MDY(9,1,2005) THEN

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
                            "'' , ",
                            "trm.id_his_pag_bim_trm ",
                         "FROM safre_his@vivht_tcp:his_pag_bim_trm trm ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE trm.nss = ? ",
                         "AND trm.tpo_aclaracion = 99 ",
                         "AND ((trm.cve_aportacion =  '' AND  trm.aportacion IS NOT NULL AND (trm.amortizacion IS NULL  OR trm.amortizacion = 0)) ",
                                "OR trm.cve_aportacion = 2)",
                         "AND trm.f_pago > ? ",
                         "AND trm.f_pago <= MDY(10,31,2012) ",
                         "AND trm.f_pago <= ? ",
                         "AND trm.aportacion IS NOT NULL ",
                         "ORDER BY trm.f_pago DESC, trm.periodo_pago DESC "

          PREPARE exe_consulta_trm_aum FROM v_query
          DECLARE cur_consulta_trm_aum CURSOR FOR exe_consulta_trm_aum

          FOREACH cur_consulta_trm_aum USING p_trabajador.nss, g_finicio,g_ffin INTO v_arr_mov[v_cont].*
             IF v_arr_mov[v_cont].bimestre[1] = '9' THEN
                LET v_arr_mov[v_cont].bimestre = '19',v_arr_mov[v_cont].bimestre
             ELSE
                LET v_arr_mov[v_cont].bimestre = '20',v_arr_mov[v_cont].bimestre
             END IF
                 
             EXECUTE prepnom_patron USING v_arr_mov[v_cont].nrp INTO v_arr_mov[v_cont].nombre_patron
                 
             LET v_cont = v_cont + 1  
          END FOREACH
      END IF

      IF g_ffin >= MDY(9,1,1997) AND g_finicio <= MDY(9,1,2005) THEN

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
                            "pen.patron, ",
                            "1 ",
                         "FROM safre_his@vivht_tcp:his_pag_pen_ads pen ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE pen.nss = '",p_trabajador.nss CLIPPED, "' ",
                         "AND pen.clave IN ('105','115','125','135','155') ",
                         "AND pen.f_pago >= ? ",
                         "AND (pen.f_pago <= MDY(8,31,2005) ",
                         "     AND pen.f_pago <= ?) ",
                         "AND pen.aportacion IS NOT NULL ", --Se solicitó no hacer búsqueda por RFC para este periodo 
                         "ORDER BY 1 DESC , 9 DESC"
          PREPARE exe_consulta_pen_aum FROM v_query
          DECLARE cur_consulta_pen_aum CURSOR FOR exe_consulta_pen_aum
          FOREACH cur_consulta_pen_aum USING g_finicio,g_ffin INTO  v_arr_mov[v_cont].*
             LET v_cont = v_cont + 1  
          END FOREACH 
      END IF

     
      IF g_finicio <= MDY(9,1,1997) THEN        

          LET v_rfc10 = p_trabajador.rfc

          IF v_arr_mov[v_arr_mov.getLength()].subcuenta IS NULL THEN
             CALL v_arr_mov.deleteElement(v_arr_mov.getLength())
          END IF

          LET v_cont_ini_ads = v_arr_mov.getLength()

          DISPLAY "CONTADOR INICIAL: ", v_cont_ini_ads
          
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
                            "ads.patron, ",
                            "2 ",
                         "FROM safre_his@vivht_tcp:his_pag_bim_ads ads ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 8 ",
                         "WHERE ads.nss = ? ",
                         "AND ads.clave IN ('105','115','125','135','155') ",
                         "AND ads.f_pago >= ? ",
                         "AND (ads.f_pago <= MDY(8,31,1997) ",
                         "   AND   ads.f_pago <= ?) ",
                         "AND ads.importe IS NOT NULL ",
                         "ORDER BY 1 DESC, 9 DESC "
                         
          PREPARE exe_consulta_ads_otro FROM v_query

          LET v_cont_nss = 1
          CALL v_arr_nss.clear()

          CALL f_unificacion (p_trabajador.nss) RETURNING v_arr_nss
          
          DECLARE cur_consulta_ads_otro CURSOR FOR exe_consulta_ads_otro

          FOR v_cont_nss = 1 TO v_arr_nss.getLength()
             
             FOREACH cur_consulta_ads_otro USING v_arr_nss[v_cont_nss].nss , g_finicio,g_ffin INTO  v_arr_mov[v_cont].*
                IF v_arr_mov[v_cont].bimestre[1] = '9' THEN
                   LET v_arr_mov[v_cont].bimestre = '19',v_arr_mov[v_cont].bimestre
                ELSE
                  LET v_arr_mov[v_cont].bimestre = '20',v_arr_mov[v_cont].bimestre
                END IF
             
                EXECUTE prep_nrp USING v_arr_mov[v_cont].nombre_patron INTO v_arr_mov[v_cont].nrp
                IF v_arr_mov[v_cont].nrp IS NULL THEN
                   LET v_arr_mov[v_cont].nrp = '           '
                END IF
             
                LET v_cont = v_cont + 1  
             END FOREACH
          END FOR

          CALL v_arr_mov.deleteElement(v_arr_mov.getLength())

          LET v_cont_fin_ads = v_arr_mov.getLength()

          {

          DISPLAY "CONTADOR FINAL: ", v_cont_fin_ads

          #Se valida que existan movimientos, de no existir se buscan por el NSS Unificado
          --IF v_cont_ini_ads = v_cont_fin_ads THEN
          DISPLAY "NO TIENE MOVIMIENTOS EL NSS: ", p_trabajador.nss
          DISPLAY "INICIA BUSQUEDA DE UNIFICADOS..."

            LET v_cont_nss = 1
            CALL v_arr_nss.clear()
            
            LET v_query =  "SELECT ",
                           "TRIM(dor.nss_unificador) ",
                           "FROM uni_det_unificador dor ",
                           "INNER JOIN uni_det_unificado ado ON ado.id_unificador = dor.id_unificador ",
                           "WHERE ado.nsscta1 = '",p_trabajador.nss CLIPPED,"' ",
                           "AND dor.estado_familia = 1 ",
                           "UNION ",
                           "SELECT ",
                           "TRIM(ado.nsscta1) ",
                           "FROM uni_det_unificador dor ",
                           "INNER JOIN uni_det_unificado ado ON ado.id_unificador = dor.id_unificador ",
                           "WHERE dor.nss_unificador = '",p_trabajador.nss CLIPPED,"' ",
                           "AND dor.estado_familia = 1 "

            PREPARE exe_consulta_nss FROM v_query
            DECLARE cur_consulta_nss CURSOR FOR exe_consulta_nss

            FOREACH cur_consulta_nss INTO v_arr_nss[v_cont_nss].nss
               FOREACH cur_consulta_ads_otro USING v_arr_nss[v_cont_nss].nss, g_finicio,g_ffin INTO  v_arr_mov[v_cont].*
                  IF v_arr_mov[v_cont].bimestre[1] = '9' THEN
                     LET v_arr_mov[v_cont].bimestre = '19',v_arr_mov[v_cont].bimestre
                  ELSE
                     LET v_arr_mov[v_cont].bimestre = '20',v_arr_mov[v_cont].bimestre
                  END IF
                 
                  EXECUTE prep_nrp USING v_arr_mov[v_cont].nombre_patron INTO v_arr_mov[v_cont].nrp
                  IF v_arr_mov[v_cont].nrp IS NULL THEN
                     LET v_arr_mov[v_cont].nrp = '           '
                  END IF
                 
                  LET v_cont = v_cont + 1  
               END FOREACH
               LET v_cont_nss = v_cont_nss + 1  
            END FOREACH
          --END IF

          }
          CALL v_arr_ads.clear()
          LET v_cont_ads = 1

          #Se realiza la consulta unicamente por RFC a 13 Posiciones

          IF p_trabajador.rfc IS NOT NULL AND p_trabajador.rfc[1] <> ' ' THEN
          LET v_query =  "SELECT ",
                            "ads.nss, ",
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
                            "ads.patron, ",
                            "2 ",
                         "FROM safre_his@vivht_tcp:his_pag_bim_ads ads ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 8 ",
                         "WHERE ads.rfc = '",p_trabajador.rfc CLIPPED,"' ",
                         "AND ads.clave IN ('105','115','125','135','155') ",
                         "AND ads.f_pago >= ? ",
                         "AND (ads.f_pago <= MDY(8,31,1997) ",
                         "   AND ads.f_pago <= ?) ",
                         "AND ads.importe IS NOT NULL ",
                         "ORDER BY 2 DESC ,10 DESC"
                         
          PREPARE exe_consulta_ads_otro_rfc FROM v_query
          DECLARE cur_consulta_ads_otro_rfc CURSOR FOR exe_consulta_ads_otro_rfc
             
          FOREACH cur_consulta_ads_otro_rfc USING g_finicio,g_ffin INTO v_arr_ads[v_cont_ads].*
             IF v_arr_ads[v_cont_ads].bimestre[1] = '9' THEN
                LET v_arr_ads[v_cont_ads].bimestre = '19',v_arr_ads[v_cont_ads].bimestre
             ELSE
                LET v_arr_ads[v_cont_ads].bimestre = '20',v_arr_ads[v_cont_ads].bimestre
             END IF
            
             EXECUTE prep_nrp USING v_arr_ads[v_cont_ads].nombre_patron INTO v_arr_ads[v_cont_ads].nrp
             IF v_arr_ads[v_cont_ads].nrp IS NULL THEN
                LET v_arr_ads[v_cont_ads].nrp = '           '
             END IF
              
             LET v_cont_ads = v_cont_ads + 1  
          END FOREACH
          
          CALL v_arr_ads.deleteElement(v_arr_ads.getLength())

          DISPLAY "CONTADOR RFC: ", v_arr_ads.getLength()

          FOR v_cont_rfc = 1 TO v_arr_ads.getLength()
             IF v_arr_ads[v_cont_rfc].nss = '00000000000' OR LENGTH( v_arr_ads[v_cont_rfc].nss CLIPPED) <= 10 THEN
                LET v_arr_mov[v_cont].f_liquida         = v_arr_ads[v_cont_rfc].f_liquida 
                LET v_arr_mov[v_cont].movimiento        = v_arr_ads[v_cont_rfc].movimiento 
                LET v_arr_mov[v_cont].movimiento_desc   = v_arr_ads[v_cont_rfc].movimiento_desc
                LET v_arr_mov[v_cont].tipo              = v_arr_ads[v_cont_rfc].tipo
                LET v_arr_mov[v_cont].desc_ciudadana    = v_arr_ads[v_cont_rfc].desc_ciudadana
                LET v_arr_mov[v_cont].subcuenta         = v_arr_ads[v_cont_rfc].subcuenta
                LET v_arr_mov[v_cont].subcuenta_desc    = v_arr_ads[v_cont_rfc].subcuenta_desc
                LET v_arr_mov[v_cont].monto_pesos       = v_arr_ads[v_cont_rfc].monto_pesos 
                LET v_arr_mov[v_cont].bimestre          = v_arr_ads[v_cont_rfc].bimestre
                LET v_arr_mov[v_cont].nrp               = v_arr_ads[v_cont_rfc].nrp
                LET v_arr_mov[v_cont].nombre_patron     = v_arr_ads[v_cont_rfc].nombre_patron
                LET v_arr_mov[v_cont].id_movimiento     = v_arr_ads[v_cont_rfc].id_movimiento

                LET v_cont = v_cont + 1 
            END IF
          END FOR
          END IF -- VALIDA QUE EL RFC no esté NULO o con espacios en blanco
      END IF
      
END FUNCTION

FUNCTION fn_movimientos_otros(p_trabajador)
   DEFINE p_trabajador             trabajador
   DEFINE v_rfc10                  CHAR(10)   
      
      #Se agregan los registros del historico de movimientos TRM
      IF g_ffin >= MDY(9,1,2005) THEN

          LET v_query =  "SELECT ",
                            "trm.f_pago, ",
                            "1, ",
                            "cat.movimiento_desc, ",
                            "cat.tipo, ",
                            "cat.desc_ciudadana, ",
                            "69, ",                    #Valor fijo para identificar los movimientos de TRM APORTACION
                            "subc.subcuenta_desc, ",
                            "trm.aportacion, ",
                            "trm.periodo_pago, ",
                            "trm.nrp, ",
                            "'' ,",
                            "trm.id_his_pag_bim_trm ",
                         "FROM safre_his@vivht_tcp:his_pag_bim_trm trm ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE trm.nss = '",p_trabajador.nss CLIPPED, "' ",
                         "AND trm.tpo_aclaracion = 99 ",
                         "AND ((trm.cve_aportacion =  '' AND trm.amortizacion IS NOT NULL AND trm.amortizacion > 0 ", 
                               "AND trm.aportacion IS NOT NULL ) OR trm.cve_aportacion = 1) ",
                         "AND trm.f_pago > ? ",
                         "AND trm.f_pago <= MDY(10,31,2012) ",
                         "AND trm.f_pago <= ? ",
                         "UNION ",
                         "SELECT ",
                            "trm.f_pago, ",
                            "1, ",
                            "cat.movimiento_desc, ",
                            "cat.tipo, ",
                            "cat.desc_ciudadana, ",
                            "73, ",                    #Valor fijo para identificar los movimientos de TRM AMORTIZACION
                            "subc.subcuenta_desc, ",
                            "trm.amortizacion, ",
                            "trm.periodo_pago, ",
                            "trm.nrp, ",
                            "'', ",
                            "trm.id_his_pag_bim_trm ",
                         "FROM safre_his@vivht_tcp:his_pag_bim_trm trm ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE trm.nss = '",p_trabajador.nss CLIPPED, "' ",
                         "AND trm.tpo_aclaracion = 99 ",
                         "AND ((trm.cve_aportacion =  '' AND  trm.amortizacion IS NOT NULL AND trm.amortizacion > 0) ",
                                "OR trm.cve_aportacion = 1) ",
                         "AND trm.f_pago > ? ",
                         "AND trm.f_pago <= MDY(10,31,2012) ",
                         "AND trm.f_pago <= ? ",
                         "AND trm.amortizacion IS NOT NULL  ",
                         "ORDER BY 1 DESC, 9 DESC "

          PREPARE exe_consulta_trm_otro FROM v_query
          DECLARE cur_consulta_trm_otro CURSOR FOR exe_consulta_trm_otro

          FOREACH cur_consulta_trm_otro USING g_finicio,g_ffin,g_finicio,g_ffin INTO v_arr_mov[v_cont].*
             IF v_arr_mov[v_cont].bimestre[1] = '9' THEN
                LET v_arr_mov[v_cont].bimestre = '19',v_arr_mov[v_cont].bimestre
             ELSE
                LET v_arr_mov[v_cont].bimestre = '20',v_arr_mov[v_cont].bimestre
             END IF
                 
             EXECUTE prepnom_patron USING v_arr_mov[v_cont].nrp INTO v_arr_mov[v_cont].nombre_patron
                 
             LET v_cont = v_cont + 1  
          END FOREACH

      END IF

      IF g_ffin >= MDY(9,1,1997) AND g_finicio <= MDY(9,1,2005) THEN

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
                            "pen.patron, ",
                            "1 ",
                         "FROM safre_his@vivht_tcp:his_pag_pen_ads pen ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE pen.nss = '",p_trabajador.nss CLIPPED, "' ",
                         "AND pen.clave IN ('106','116','126','136','156') ",
                         "AND pen.f_pago >= ? ",
                         "AND (pen.f_pago <= MDY(8,31,2005) ",
                         "     AND pen.f_pago <= ?) ",
                         "AND pen.aportacion IS NOT NULL ", --Se solicitó no hacer busqueda por RFC para este periodo

                         "UNION ",
                         "SELECT ",
                            "pen.f_pago, ",
                            "pen.clave, ",
                            "cat.movimiento_desc, ",
                            "cat.tipo, ",
                            "cat.desc_ciudadana, ",
                            "72, ",         --Valor fijo para identificar los movimientos de pensiones
                            "subc.subcuenta_desc, ",
                            "pen.amortizacion, ",
                            "pen.bimestre, ",
                            "pen.nrp, ",
                            "pen.patron, ",
                            "3 ",
                         "FROM safre_his@vivht_tcp:his_pag_pen_ads pen ",
                         "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                         "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                         "WHERE pen.nss = '",p_trabajador.nss CLIPPED, "' ",
                         "AND (pen.clave IN ('106','116','126','136','156') OR ",
                         "(pen.clave = '102' AND (pen.amortizacion IS NOT NULL OR pen.amortizacion != 0))) ",
                         "AND pen.f_pago >= ? ",
                         "AND (pen.f_pago <= MDY(8,31,2005) ",
                         "     AND pen.f_pago <= ?) ", -- Se solicitó no hacer búsqueda por RFC para este periodo
  
                         "ORDER BY 1 DESC, 9 DESC "
          PREPARE exe_consulta_pen_otro FROM v_query
          DECLARE cur_consulta_pen_otro CURSOR FOR exe_consulta_pen_otro
          
          FOREACH cur_consulta_pen_otro USING g_finicio,g_ffin,g_finicio,g_ffin INTO  v_arr_mov[v_cont].*
             LET v_cont = v_cont + 1  
          END FOREACH
      END IF
      
END FUNCTION

FUNCTION fn_ordena_movimientos()--Función que carga el arreglo de salida evaluando el campo desc_ciudadana 
                                --si desc_ciudadana es null se utilizara el campo movimiento_desc la salida
    DEFINE v_cont             INTEGER 
    DEFINE v_indice           INTEGER
    DEFINE v_cargo_mov_desc   VARCHAR(60)
    DEFINE v_cargo_amor_desc  VARCHAR(60)
    DEFINE v_cargo_mov_desc_pen VARCHAR(60)
    DEFINE v_cargo_amor_desc_pen VARCHAR(60)
    
    CALL movimientos_gral.clear()

    #Se consulta la descripcion del cargo informativo para los movimientos del historico TRM
    SELECT
      desc_ciudadana
    INTO
      v_cargo_mov_desc
    FROM cat_movimiento
    WHERE movimiento = CARGO_INFORMATIVO 

    SELECT
      desc_ciudadana
    INTO
      v_cargo_amor_desc
    FROM cat_movimiento
    WHERE movimiento = CARGO_AMORTIZACION

    #Se consulta la descripcion del cargo informativo para los movimientos del historico PENSIONES
    SELECT
      desc_ciudadana
    INTO
      v_cargo_mov_desc_pen
    FROM cat_movimiento
    WHERE movimiento = CARGO_INFORMATIVO_PEN 

    SELECT
      desc_ciudadana
    INTO
      v_cargo_amor_desc_pen
    FROM cat_movimiento
    WHERE movimiento = CARGO_AMORTIZACION_PEN

    LET v_indice = 1
    FOR v_cont = 1 TO v_arr_mov.getLength()
      IF v_arr_mov[v_cont].f_liquida IS NOT NULL THEN
         IF v_arr_mov[v_cont].desc_ciudadana IS NULL THEN 
            LET movimientos_gral[v_indice].movimiento_desc = v_arr_mov[v_cont].movimiento_desc
         ELSE 
            LET movimientos_gral[v_indice].movimiento_desc = v_arr_mov[v_cont].desc_ciudadana
         END IF
         
         LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
         LET movimientos_gral[v_indice].bimestre     = v_arr_mov[v_cont].bimestre
         LET movimientos_gral[v_indice].movimiento   = v_arr_mov[v_cont].movimiento USING '##&&'
         #LET movimientos_gral[v_indice].movimiento_desc = v_arr_mov[v_cont].movimiento_desc
         LET movimientos_gral[v_indice].subcuenta    = v_arr_mov[v_cont].subcuenta_desc
         LET movimientos_gral[v_indice].no_movimiento = v_arr_mov[v_cont].subcuenta 
         LET movimientos_gral[v_indice].id_movimiento = v_arr_mov[v_cont].id_movimiento 
         
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

         #Tratamiento especial para los movimientos historicos BIMESTRAL ADS
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_BIM_ADS THEN
            LET movimientos_gral[v_indice].movimiento = '01' USING '##&&'
            LET movimientos_gral[v_indice].bimestre     = v_arr_mov[v_cont].bimestre
            LET movimientos_gral[v_indice].movimiento_desc = movimientos_gral[v_indice].movimiento_desc, "\n",
                "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
         END IF

         
         LET v_indice = v_indice + 1

         #Tratamiento especial para los movimientos historicos TRM APORTACION
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_TRM THEN
            LET movimientos_gral[v_indice-1].movimiento = '01' USING '##&&'
            IF v_arr_mov[v_cont].movimiento = 1 THEN
                #Se genera un movimiento por el mismo monto de la aportacion
                LET movimientos_gral[v_indice-1].cargo = NULL 
                LET movimientos_gral[v_indice-1].abono = v_arr_mov[v_cont].monto_pesos                               
                LET movimientos_gral[v_indice-1].movimiento = '01' USING '##&&'
                LET movimientos_gral[v_indice-1].no_movimiento = 69 
                LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
                LET movimientos_gral[v_indice-1].id_movimiento= v_arr_mov[v_cont].id_movimiento

                --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
                LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos 
                LET movimientos_gral[v_indice].abono = NULL
                LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
                LET movimientos_gral[v_indice].bimestre     = v_arr_mov[v_cont].bimestre
                LET movimientos_gral[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
                LET movimientos_gral[v_indice].movimiento = CARGO_INFORMATIVO USING '##&&'
                LET movimientos_gral[v_indice].movimiento_desc = v_cargo_mov_desc
                --LET movimientos_gral[v_indice].no_movimiento = 73
                LET movimientos_gral[v_indice].no_movimiento = 69
                LET movimientos_gral[v_indice].id_movimiento= v_arr_mov[v_cont].id_movimiento

                IF movimientos_gral[v_indice - 1].movimiento = 01 THEN
                   LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                   "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
                   LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
                END IF   
                
                LET v_indice = v_indice + 1
            END IF
            
            IF movimientos_gral[v_indice - 1].movimiento = 01 THEN
               LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
                LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
            END IF 
            
         END IF

         #Tratamiento especial para los movimientos historicos TRM AMORTIZACION
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_TRM_AMOR THEN
         
            
            #Se genera un movimiento por el mismo monto de la amortizacion
               LET movimientos_gral[v_indice-1].cargo = NULL 
               LET movimientos_gral[v_indice-1].abono = v_arr_mov[v_cont].monto_pesos
               LET movimientos_gral[v_indice-1].subcuenta = DESC_AMORTIZACION#v_arr_mov[v_cont].subcuenta_desc
               
               LET movimientos_gral[v_indice-1].movimiento = '00' USING '##&&'
               LET movimientos_gral[v_indice-1].movimiento_desc = "Retención Salarial \n",
                   "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
               --LET movimientos_gral[v_indice-1].no_movimiento = 73 
               LET movimientos_gral[v_indice-1].no_movimiento = 70 
               LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice-1].id_movimiento= v_arr_mov[v_cont].id_movimiento

            --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice ].cargo = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice ].abono = NULL
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice ].movimiento = CARGO_AMORTIZACION USING '##&&'
               LET movimientos_gral[v_indice ].subcuenta = DESC_AMORTIZACION
               LET movimientos_gral[v_indice ].movimiento_desc = v_cargo_amor_desc
               --LET movimientos_gral[v_indice ].no_movimiento = 69
               LET movimientos_gral[v_indice ].no_movimiento = 70
               LET movimientos_gral[v_indice].id_movimiento= v_arr_mov[v_cont].id_movimiento
           
               LET v_indice = v_indice + 1
                                
         END IF

         #Tratamiento especial para los movimientos historicos PENSIONES APORTACIONES
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_PEN_APOR THEN
            LET movimientos_gral[v_indice - 1].movimiento = '01' USING '##&&'
            IF v_arr_mov[v_cont].movimiento = 106 
            OR v_arr_mov[v_cont].movimiento = 116
            OR v_arr_mov[v_cont].movimiento = 126
            OR v_arr_mov[v_cont].movimiento = 136
            OR v_arr_mov[v_cont].movimiento = 156 THEN
               #Se genera un movimiento por el mismo monto de la aportacion
               LET movimientos_gral[v_indice-1].cargo = NULL 
               LET movimientos_gral[v_indice-1].abono = v_arr_mov[v_cont].monto_pesos
 
               LET movimientos_gral[v_indice-1].movimiento = '01' USING '##&&'
               LET movimientos_gral[v_indice-1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc
               --LET movimientos_gral[v_indice-1].no_movimiento = 70
               LET movimientos_gral[v_indice-1].no_movimiento = 71
               LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice-1].id_movimiento= v_arr_mov[v_cont].id_movimiento

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice].abono = NULL
               LET movimientos_gral[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice].movimiento = CARGO_INFORMATIVO_PEN USING '##&&'
               LET movimientos_gral[v_indice].movimiento_desc = v_cargo_mov_desc_pen
               --LET movimientos_gral[v_indice].no_movimiento = 72
               LET movimientos_gral[v_indice].no_movimiento = 71
               LET movimientos_gral[v_indice].id_movimiento= v_arr_mov[v_cont].id_movimiento
               
               IF movimientos_gral[v_indice - 1].movimiento = 01 THEN
                  LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
                LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
               END IF               
               LET v_indice = v_indice + 1
            END IF
            
            IF movimientos_gral[v_indice - 1].movimiento = 01 THEN
               LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
                LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
            END IF
         END IF

         #Tratamiento especial para los movimientos historicos PENSIONES AMORTIZACIONES
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_PEN_AMOR THEN
            LET movimientos_gral[v_indice - 1].movimiento = '00' USING '##&&'
            IF v_arr_mov[v_cont].movimiento = 106 
            OR v_arr_mov[v_cont].movimiento = 116
            OR v_arr_mov[v_cont].movimiento = 126
            OR v_arr_mov[v_cont].movimiento = 136
            OR v_arr_mov[v_cont].movimiento = 156 THEN
            
               #Se genera un movimiento por el mismo monto de la amortizacion
               LET movimientos_gral[v_indice-1].cargo = NULL 
               LET movimientos_gral[v_indice-1].abono = v_arr_mov[v_cont].monto_pesos
               LET movimientos_gral[v_indice-1].subcuenta = DESC_AMORTIZACION#v_arr_mov[v_cont].subcuenta_desc
               
               LET movimientos_gral[v_indice-1].movimiento = '00' USING '##&&'
               LET movimientos_gral[v_indice-1].movimiento_desc = "Retención Salarial \n",
               "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
               LET movimientos_gral[v_indice-1].no_movimiento = 72
               LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice-1].id_movimiento= v_arr_mov[v_cont].id_movimiento

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice].abono = NULL
               LET movimientos_gral[v_indice].movimiento = CARGO_AMORTIZACION_PEN USING '##&&'
               LET movimientos_gral[v_indice].subcuenta = DESC_AMORTIZACION
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice].movimiento_desc = v_cargo_amor_desc_pen
               --LET movimientos_gral[v_indice].no_movimiento = 70
               LET movimientos_gral[v_indice].no_movimiento = 72
               LET movimientos_gral[v_indice].id_movimiento= v_arr_mov[v_cont].id_movimiento
               
               LET v_indice = v_indice + 1
            END IF
            
            IF v_arr_mov[v_cont].movimiento = 102 THEN
            
               #Se genera un movimiento por el mismo monto de la amortizacion
               LET movimientos_gral[v_indice-1].abono = v_arr_mov[v_cont].monto_pesos
               LET movimientos_gral[v_indice-1].cargo = NULL
               LET movimientos_gral[v_indice-1].subcuenta = DESC_AMORTIZACION#v_arr_mov[v_cont].subcuenta_desc
               
               LET movimientos_gral[v_indice-1].movimiento = '00' USING '##&&'
               LET movimientos_gral[v_indice-1].movimiento_desc = "Retención Salarial \n",
               "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
               LET movimientos_gral[v_indice-1].no_movimiento = 72 
               LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice-1].id_movimiento= v_arr_mov[v_cont].id_movimiento

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET movimientos_gral[v_indice].abono = NULL
               LET movimientos_gral[v_indice].cargo = v_arr_mov[v_cont].monto_pesos 
               LET movimientos_gral[v_indice].movimiento = CARGO_AMORTIZACION_PEN USING '##&&'
               LET movimientos_gral[v_indice].subcuenta = DESC_AMORTIZACION
               LET movimientos_gral[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET movimientos_gral[v_indice].bimestre     = v_arr_mov[v_cont].bimestre
               LET movimientos_gral[v_indice].movimiento_desc = v_cargo_amor_desc_pen
               --LET movimientos_gral[v_indice].no_movimiento = 70
               LET movimientos_gral[v_indice].no_movimiento = 72
               LET movimientos_gral[v_indice].id_movimiento= v_arr_mov[v_cont].id_movimiento
                             
               LET v_indice = v_indice + 1
            END IF
            
            IF movimientos_gral[v_indice - 1].movimiento = 01 THEN
               LET movimientos_gral[v_indice - 1].movimiento_desc = movimientos_gral[v_indice - 1].movimiento_desc, "\n",
                "Año-Bimestre: ",v_arr_mov[v_cont].bimestre[1,4],v_arr_mov[v_cont].bimestre[5,6], " - NRP ",v_arr_mov[v_cont].nrp, "\n",v_arr_mov[v_cont].nombre_patron
               LET movimientos_gral[v_indice-1].bimestre     = v_arr_mov[v_cont].bimestre
            END IF
         END IF
         

      END IF
    END FOR
END FUNCTION 

FUNCTION f_unificacion(p_nss)
   DEFINE p_nss                     CHAR(11)
   DEFINE v_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_u                       SMALLINT
   DEFINE v_uni      DYNAMIC ARRAY OF RECORD
          nss_dor CHAR(11),
          nss_ado CHAR(11)
   END RECORD
   DEFINE v_uni_nss      DYNAMIC ARRAY OF RECORD
          nss CHAR(11)
   END RECORD

   LET v_u = 1

   SELECT afi.id_derechohabiente INTO v_id_derechohabiente 
     FROM afi_derechohabiente afi
    WHERE  afi.nss = p_nss

   DROP TABLE IF EXISTS tmp_uni_familia   
   CREATE TEMP TABLE tmp_uni_familia
   ( nss CHAR(11))
    
   DECLARE cur_unificador CURSOR FOR
   SELECT r1.nss_unificador, d1.nsscta1
     FROM uni_det_unificador r1, uni_det_unificado d1
    WHERE r1.id_derechohabiente = v_id_derechohabiente
      AND r1.id_unificador = d1.id_unificador
   ORDER BY r1.id_unificador DESC

   FOREACH cur_unificador INTO v_uni[v_u].nss_dor,
                               v_uni[v_u].nss_ado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_unificador
   FREE cur_unificador

   DECLARE cur_unificado CURSOR FOR
   SELECT r2.nss_unificador, d2.nsscta1
     FROM uni_det_unificador r2, uni_det_unificado d2
    WHERE d2.id_unificador IN(SELECT n.id_unificador
                                FROM uni_det_unificado n
                               WHERE n.id_derechohabiente = v_id_derechohabiente)
      AND r2.id_unificador = d2.id_unificador
   ORDER BY r2.id_unificador DESC

   FOREACH cur_unificado INTO v_uni[v_u].nss_dor,
                               v_uni[v_u].nss_ado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_unificado
   FREE cur_unificado

   DECLARE cur_inf_unificador CURSOR FOR
   SELECT UNIQUE r3.nss nss_unificador,
          d3.nss nss_unificado
     FROM uni_inf_unificador r3,
          uni_inf_unificado d3
    WHERE r3.id_derechohabiente = v_id_derechohabiente
      AND r3.id_inf_unificador = d3.id_unificador

   FOREACH cur_inf_unificador INTO v_uni[v_u].nss_dor,
                               v_uni[v_u].nss_ado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_inf_unificador
   FREE cur_inf_unificador

   DECLARE cur_inf_unificado CURSOR FOR
   SELECT UNIQUE r4.nss nss_unificador,
          d4.nss nss_unificado
     FROM uni_inf_unificador r4, uni_inf_unificado d4
    WHERE d4.id_unificador IN(SELECT n.id_unificador
                                FROM uni_inf_unificado n
                               WHERE n.id_derechohabiente = v_id_derechohabiente)
      AND r4.id_inf_unificador = d4.id_unificador

   FOREACH cur_inf_unificado INTO v_uni[v_u].nss_dor,
                                  v_uni[v_u].nss_ado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_inf_unificado
   FREE cur_inf_unificado

   CALL v_uni.deleteelement(v_uni.getlength())

   INSERT INTO tmp_uni_familia VALUES (p_nss)

   FOR v_u = 1 TO v_uni.getlength()
       INSERT INTO tmp_uni_familia VALUES ( v_uni[v_u].nss_ado);
       INSERT INTO tmp_uni_familia VALUES ( v_uni[v_u].nss_dor);
   END FOR

   LET v_u = 1
   
   DECLARE c_uni CURSOR FOR SELECT nss FROM tmp_uni_familia GROUP BY nss
   
      FOREACH c_uni INTO v_uni_nss[v_u].nss
         LET v_u = v_u + 1
      END FOREACH
      
      CALL v_uni_nss.deleteelement(v_uni_nss.getlength())
      
   RETURN v_uni_nss

END FUNCTION
