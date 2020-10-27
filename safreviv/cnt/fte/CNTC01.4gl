###################################################################################
#Version                    => 1.0.0                                              #
#Fecha ultima modificacion  => 12/10/2018                                         #
###################################################################################
###################################################################################
#Proyecto          => SAFRE VIVIENDA                                              #
#Propietario       => E.F.P.                                                      #
-----------------------------------------------------------------------------------
#Modulo            => CNT                                                         #
#Programa          => CNTC01                                                      #
#Objetivo          => Consultar y generar el reporte de la liquidación            #
#                     de avances de pago                                          #
#Fecha inicio      => 15/05/2012                                                  #
###################################################################################
#Registro de modificaciones:                                                      #
#Autor           Fecha         Descrip. cambio                                    #
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la           #
#                              Preliquidación de Dispersión de Pagos ejecutándose #
#Janneth Saldaña 28/07/2015    Se comenta la funcionalidad de Compensación Avance # 
#                              Liquidación y se agrega el Porcentaje Compensación #
#                              Avance                                             #
###################################################################################
DATABASE safre_viv
GLOBALS "CNTG01.4gl"

GLOBALS 
  DEFINE arr_detalles_av_pag DYNAMIC ARRAY OF RECORD 
    v_periodo_pago           CHAR (6),
    v_ds_tot_aportacion      DECIMAL(22,2), 
    v_ds_tot_amortizacion    DECIMAL(22,2),
    v_can_aportacion         DECIMAL(22,2),
    v_can_amortizacion       DECIMAL(22,2),
    v_dc_pago_aportacion     DECIMAL(22,2),
    v_dc_pago_amortizacion   DECIMAL(22,2),
    v_res_aportacion         DECIMAL(22,2),
    v_res_amortizacion       DECIMAL(22,2),
    v_pca_aportacion         DECIMAL(5,2), --Porcentaje de compensación de avance Aportación 
    v_pca_amortizacion       DECIMAL(5,2) --Porcentaje de compensación de avance Amortización
    {v_cal_aportacion        DECIMAL(22,2),--Se agregan campos para obtener la compensación avance de la liquidación
    v_cal_amortizacion       DECIMAL(22,2)}
  END RECORD 

  DEFINE arr_detalles_av_pag1 DYNAMIC ARRAY OF RECORD 
    v_periodo_pago           CHAR (6),
    v_ds_tot_aportacion      DECIMAL(22,2), 
    v_ds_tot_amortizacion    DECIMAL(22,2),
    v_can_aportacion         DECIMAL(22,2),
    v_can_amortizacion       DECIMAL(22,2),
    v_dc_pago_aportacion     DECIMAL(22,2),
    v_dc_pago_amortizacion   DECIMAL(22,2),
    v_res_aportacion         DECIMAL(22,2),
    v_res_amortizacion       DECIMAL(22,2)
  END RECORD 

  DEFINE
    v_sum_tot_aportacion     DECIMAL(22,2),
    v_sum_tot_amortizacion   DECIMAL(22,2),
    v_sum_can_aportacion     DECIMAL(22,2),
    v_sum_can_amortizacion   DECIMAL(22,2),
    v_sum_pago_aportacion    DECIMAL(22,2),
    v_sum_pago_amortizacion  DECIMAL(22,2),
    v_sum_aportacion         DECIMAL(22,2),
    v_sum_amortizacion       DECIMAL(22,2),
    v_sum_pca_aportacion     DECIMAL(5,2),
    v_sum_pca_amortizacion   DECIMAL(5,2)
    {v_sum_cal_aportacion     DECIMAL(22,2),--Variables para la suma de la compensación avance de liquidación
    v_sum_cal_amortizacion   DECIMAL(22,2)}
  
  DEFINE
    periodo_ini              CHAR(6),
    periodo_fin              CHAR(6)

  DEFINE 
    v_pca_aportacion         CHAR(10),
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING,
    p_proceso_cod            SMALLINT
END GLOBALS 

MAIN
  DEFINE 
    p_programa               CHAR(10),
    Qry_Txt                  STRING, 
    v_indice                 INTEGER,
    v_tipo_consulta          SMALLINT, -- 0 = sin parámetros
    v_tot_reg                INTEGER,
    v_qwery_ibx              STRING
       
  --Inicializa variables de totales
  LET v_sum_tot_aportacion    = 0.00
  LET v_sum_tot_amortizacion  = 0.00
  LET v_sum_can_aportacion    = 0.00
  LET v_sum_can_amortizacion  = 0.00
  LET v_sum_pago_aportacion   = 0.00
  LET v_sum_pago_amortizacion = 0.00
  LET v_sum_aportacion        = 0.00
  LET v_sum_amortizacion      = 0.00
  LET v_sum_pca_aportacion    = 0.00
  LET v_sum_pca_amortizacion  = 0.00
  {LET v_sum_cal_aportacion    = 0.00
  LET v_sum_cal_amortizacion  = 0.00}

  LET v_tipo_consulta         = 0

  LET p_programa     = "CNTC01"
  LET g_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
  LET g_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
  LET g_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa
  LET p_proceso_cod  = 902

  --Se invoca la función que asigna el titulo a la ventana
  CALL ui.Interface.setText(g_nom_prog)

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####

  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
        
  CLOSE WINDOW SCREEN

  OPEN WINDOW vtn_CNTC01 WITH FORM "CNTC011"
    --CALL fn_crea_tabla_tem()

    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME periodo_ini, periodo_fin
        ON ACTION cancelar
           EXIT DIALOG 

        ON ACTION ACCEPT
           DISPLAY "periodo_ini: ",periodo_ini
           DISPLAY "LENGTH(periodo_ini): ", LENGTH(periodo_ini)
           DISPLAY "periodo_fin: ", periodo_fin
           DISPLAY "LENGTH(periodo_fin): ", LENGTH(periodo_fin)

           --Consulta información sin parametros de periodo  
           IF periodo_ini IS NULL AND periodo_fin IS NULL THEN          
              LET v_tipo_consulta = 0
              DISPLAY " # Nulos"
           ELSE
              IF LENGTH(periodo_ini) >=0 AND LENGTH(periodo_ini) < 6 THEN 
                 CALL fn_mensaje("Atención", "Los periodos deben ser en formato AAAABB. Verifique ", "stop")
                 NEXT FIELD periodo_ini   
              END IF

              IF LENGTH(periodo_fin) >=0 AND LENGTH(periodo_fin) < 6 THEN 
                 CALL fn_mensaje("Atención", "Los periodos deben ser en formato AAAABB. Verifique ", "stop")
                 NEXT FIELD periodo_fin   
              END IF

              IF LENGTH(periodo_ini) =6 AND LENGTH(periodo_fin) = 6 THEN
                 DISPLAY " # Uno"
                 LET v_tipo_consulta = 1  
              END IF  
           END IF

           WHENEVER ERROR CONTINUE 
           DROP TABLE tmp_liq_ctr_ava;

           DISPLAY "Tipo: ", v_tipo_consulta

           IF v_tipo_consulta = 1 THEN --Con parámetros
              SELECT dd.periodo_pago, 
                     SUM(dd.monto_aportacion) ava_apo, 
                     SUM(dd.monto_amortizacion) ava_amo,  
                     0.00 can_apo, 
                     0.00 can_amo, 
                     --NVL(sum(dc.monto_apo_avance),0) comp_apo, 
                     --NVL(sum(dc.monto_amo_avance),0) comp_amo, 
                     0.00 comp_apo, 
                     0.00 comp_amo,
                     0.00 AS Res_Aportacion,
                     0.00 AS Res_Amortizacion,
                     NVL(sum(dc.monto_apo_avance),0) cal_apo, 
                     NVL(sum(dc.monto_amo_avance),0) cal_amo    
              FROM   dis_det_avance_pago dd,
              OUTER  dis_compensa_avance dc 
              WHERE  dd.id_dis_det_avance_pago = dc.id_dis_det_avance_pago
              AND    dd.periodo_pago BETWEEN periodo_ini AND periodo_fin
              AND    dd.periodo_pago           = dc.periodo_pago                             
              AND    dd.tpo_avance             = 181
              GROUP BY 1 
              UNION ALL 
              SELECT da.periodo_pago, 
                     0.00 ava_apo, 
                     0.00 ava_amo, 
                     SUM(da.monto_aportacion * -1) can_apo, 
                     SUM(da.monto_amortizacion * -1) can_amo, 
                     0.00 comp_apo, 
                     0.00 comp_amo, 
                     0.00 Res_Aportacion, 
                     0.00 Res_Amortizacion,
                     0.00 cal_apo,
                     0.00 cal_amo 
              FROM   dis_det_avance_pago da 
              WHERE  da.tpo_avance IN (1811,1812,1815) 
              AND    da.periodo_pago BETWEEN periodo_ini AND periodo_fin
              GROUP BY 1 
              INTO TEMP tmp_liq_ctr_ava;
           ELSE --Sin parámetros  
              SELECT dd.periodo_pago, 
                     SUM(dd.monto_aportacion) ava_apo, 
                     SUM(dd.monto_amortizacion) ava_amo,  
                     0.00 can_apo, 
                     0.00 can_amo, 
                     --NVL(sum(dc.monto_apo_avance),0) comp_apo, 
                     --NVL(sum(dc.monto_amo_avance),0) comp_amo,
                     0.00 comp_apo, 
                     0.00 comp_amo, 
                     0.00 AS Res_Aportacion,
                     0.00 AS Res_Amortizacion,
                     NVL(sum(dc.monto_apo_avance),0) cal_apo, 
                     NVL(sum(dc.monto_amo_avance),0) cal_amo  
              FROM   dis_det_avance_pago dd,
              OUTER  dis_compensa_avance dc 
              WHERE  dd.id_dis_det_avance_pago = dc.id_dis_det_avance_pago
              AND    dd.periodo_pago           = dc.periodo_pago                             
              AND    dd.tpo_avance             = 181
              GROUP BY 1 
              UNION ALL 
              SELECT da.periodo_pago, 
                     0.00 ava_apo, 
                     0.00 ava_amo, 
                     SUM(da.monto_aportacion * -1) can_apo, 
                     SUM(da.monto_amortizacion * -1) can_amo, 
                     0.00 comp_apo, 
                     0.00 comp_amo, 
                     0.00 Res_Aportacion, 
                     0.00 Res_Amortizacion,
                     0.00 cal_apo,
                     0.00 cal_amo
              FROM   dis_det_avance_pago da 
              WHERE  da.tpo_avance IN (1811,1812.1815) 
              GROUP BY 1 
              INTO TEMP tmp_liq_ctr_ava;
           END IF 

           UPDATE STATISTICS FOR TABLE tmp_liq_ctr_ava;
         
           LET v_indice = 1
         
           {DECLARE cur_det_av_pago CURSOR FOR
           SELECT  periodo_pago,
                   SUM(ava_apo), 
                   SUM(ava_amo),  
                   SUM(can_apo), 
                   SUM(can_amo), 
                   SUM(comp_apo), 
                   SUM(comp_amo), 
                   SUM(ava_apo - can_apo - comp_apo), 
                   SUM(ava_amo - can_amo - comp_amo) 
           FROM tmp_liq_ctr_ava 
           GROUP BY 1 
           ORDER BY 1 DESC
           FOREACH cur_det_av_pago INTO arr_detalles_av_pag[v_indice].*}

           LET Qry_Txt = "\n SELECT periodo_pago, ",
                         "\n        SUM(ava_apo), ",
                         "\n        SUM(ava_amo), ", 
                         "\n        SUM(can_apo), ",
                         "\n        SUM(can_amo), ",
                         "\n        SUM(comp_apo), ",
                         "\n        SUM(comp_amo), ",
                         "\n        0.00,",--Se agrega 0.00 la obtención de este importe se realiza en el recorrido del arreglo
                         "\n        0.00,",
                         --"\n        SUM(ava_apo - can_apo - comp_apo), ",
                         --"\n        SUM(ava_amo - can_amo - comp_amo), ",
                         --"\n        SUM(cal_apo),   ",
                         --"\n        SUM(cal_amo)    ",
                         "\n        0.00,",
                         "\n        0.00",
                         "\n  FROM tmp_liq_ctr_ava ",
                         --"\n  WHERE periodo_pago BETWEEN '",periodo_ini,"' ",
                         --"\n  AND '",periodo_fin,"' ",
                         "\n  GROUP BY 1 ",
                         "\n  ORDER BY 1 DESC"
                
           PREPARE qry_detalles FROM Qry_Txt
           DECLARE cur_det_av_pago CURSOR FOR qry_detalles

           LET v_indice = 1

           FOREACH cur_det_av_pago INTO arr_detalles_av_pag[v_indice].*
             --Se agrega selección de suma de importes para composición avance
             LET Qry_Txt="   SELECT NVL(SUM(monto_aportacion),0),NVL(SUM(monto_amortizacion),0)",
                         "\n FROM   dis_det_avance_pago",
                         "\n WHERE  estado       = 50 ",
                         "\n AND    periodo_pago = ",arr_detalles_av_pag[v_indice].v_periodo_pago
                       
             PREPARE stm_liquidacion FROM Qry_Txt           
             EXECUTE stm_liquidacion INTO arr_detalles_av_pag[v_indice].v_dc_pago_aportacion, 
                                          arr_detalles_av_pag[v_indice].v_dc_pago_amortizacion            
                                         
             --Se integra suma para la obtención de avance abierto-aportaciones
             LET  arr_detalles_av_pag[v_indice].v_res_aportacion = arr_detalles_av_pag[v_indice].v_ds_tot_aportacion-
                                                                   arr_detalles_av_pag[v_indice].v_can_aportacion-
                                                                   arr_detalles_av_pag[v_indice].v_dc_pago_aportacion
                                                                
             --Se integra suma para la obtención de avance abierto-aportaciones                                                  
             LET  arr_detalles_av_pag[v_indice].v_res_amortizacion = arr_detalles_av_pag[v_indice].v_ds_tot_amortizacion-
                                                                     arr_detalles_av_pag[v_indice].v_can_amortizacion-
                                                                     arr_detalles_av_pag[v_indice].v_dc_pago_amortizacion

             -- Porcentaje = Compensación de Avance / Avance de Pago * 100
             LET arr_detalles_av_pag[v_indice].v_pca_aportacion = FGL_DECIMAL_TRUNCATE((arr_detalles_av_pag[v_indice].v_dc_pago_aportacion / arr_detalles_av_pag[v_indice].v_ds_tot_aportacion * 100),2)
  	         LET arr_detalles_av_pag[v_indice].v_pca_amortizacion = FGL_DECIMAL_TRUNCATE((arr_detalles_av_pag[v_indice].v_dc_pago_amortizacion / arr_detalles_av_pag[v_indice].v_ds_tot_amortizacion * 100),2)             
                                                                     
             --FOREACH cur_det_av_pago INTO arr_detalles_av_pag[v_indice].* 
               --LET v_indice = v_indice + 1
             --END FOREACH
               
             --  FOREACH cur_det_av_pago INTO arr_detalles_av_pag[v_indice].*

             --Total 1
             LET v_sum_tot_aportacion    = v_sum_tot_aportacion + arr_detalles_av_pag[v_indice].v_ds_tot_aportacion

             --Total 2
             LET v_sum_tot_amortizacion  = v_sum_tot_amortizacion + arr_detalles_av_pag[v_indice].v_ds_tot_amortizacion

             LET v_sum_can_aportacion    = v_sum_can_aportacion   + arr_detalles_av_pag[v_indice].v_can_aportacion
             LET v_sum_can_amortizacion  = v_sum_can_amortizacion + arr_detalles_av_pag[v_indice].v_can_amortizacion

             --Total 3
             LET v_sum_pago_aportacion   = v_sum_pago_aportacion + arr_detalles_av_pag[v_indice].v_dc_pago_aportacion

             --Total 4
             LET v_sum_pago_amortizacion = v_sum_pago_amortizacion + arr_detalles_av_pag[v_indice].v_dc_pago_amortizacion

             --Total 5
             LET v_sum_aportacion        = v_sum_aportacion + arr_detalles_av_pag[v_indice].v_res_aportacion

             --Total 6
             LET v_sum_amortizacion      = v_sum_amortizacion + arr_detalles_av_pag[v_indice].v_res_amortizacion
             
             --Total de compensacion avance de la liquidación
             {LET v_sum_cal_aportacion   = v_sum_cal_aportacion+arr_detalles_av_pag[v_indice].v_cal_aportacion
             LET v_sum_cal_amortizacion = v_sum_cal_amortizacion+arr_detalles_av_pag[v_indice].v_cal_amortizacion}   
                   
             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_ds_tot_aportacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_ds_tot_aportacion = (arr_detalles_av_pag[v_indice].v_ds_tot_aportacion * -1)
             END IF 

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_ds_tot_amortizacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_ds_tot_amortizacion = (arr_detalles_av_pag[v_indice].v_ds_tot_amortizacion * -1)
             END IF

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_can_aportacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_can_aportacion      = (arr_detalles_av_pag[v_indice].v_can_aportacion * -1)
             END IF

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_can_amortizacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_can_amortizacion    = (arr_detalles_av_pag[v_indice].v_can_amortizacion * -1)
             END IF

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_dc_pago_aportacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_dc_pago_aportacion = (arr_detalles_av_pag[v_indice].v_dc_pago_aportacion * -1)
             END IF

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_dc_pago_amortizacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_dc_pago_amortizacion = (arr_detalles_av_pag[v_indice].v_dc_pago_amortizacion * -1)
             END IF

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_res_aportacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_res_aportacion = (arr_detalles_av_pag[v_indice].v_res_aportacion * -1)
             END IF

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_res_amortizacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_res_amortizacion = (arr_detalles_av_pag[v_indice].v_res_amortizacion * -1)
             END IF
              
             --Si la suma de la compensación avance de la liquidación es negativo, se obtiene valor absoluto
             {IF arr_detalles_av_pag[v_indice].v_cal_aportacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_cal_aportacion = (arr_detalles_av_pag[v_indice].v_cal_aportacion * -1)
             END IF

             --Si es negativo, obtiene el valor absoluto
             IF arr_detalles_av_pag[v_indice].v_cal_amortizacion < 0 THEN 
                LET arr_detalles_av_pag[v_indice].v_cal_amortizacion = (arr_detalles_av_pag[v_indice].v_cal_amortizacion * -1)
             END IF}             

             LET v_indice = v_indice + 1
           END FOREACH           
                           
           --Borra elemento vacío del arreglo
           CALL arr_detalles_av_pag.deleteElement(v_indice)

           --Asigna total de registros
           LET v_tot_reg = arr_detalles_av_pag.getLength() 
                
           --DISPLAY "indice de algo ", v_indice
           IF v_indice <=  1 THEN
              CALL fn_mensaje("Información","No existe información para desplegar","about")
              EXIT DIALOG
           ELSE 
              --CALL DIALOG.setActionHidden("cancelar", 0) --Muestra el botón "Cancelar"(1 Oculta, 0 Muestra)
              --CALL DIALOG.setActionHidden("reporte", 0) --Muestra el botón "Reporte"(1 Oculta, 0 Muestra)

              {--Obtiene los montos totales 
              LET Qry_Txt = "\n SELECT SUM(monto_apo_avance), SUM(monto_amo_avance),             ",   
                            "\n        SUM(monto_apo_pag), SUM(monto_amo_pag),                   ",
                            "\n        SUM(monto_apo_pag - monto_apo_avance) AS Res_Aportacion,  ",
                            "\n        SUM(monto_amo_pag - monto_amo_avance) AS Res_Amortizacion ",
                            "\n   FROM dis_compensa_avance dc                                    ",
                            "\n  WHERE 1=1                                                       "

              IF periodo_ini IS NOT NULL AND periodo_fin IS NOT NULL THEN
                 LET Qry_Txt = Qry_Txt || "\n    AND periodo_pago BETWEEN '",periodo_ini,"'", 
                                          "\n    AND '",periodo_fin,"'"                        
              END IF         
                
              DISPLAY Qry_Txt
       
              PREPARE prp_totales FROM  Qry_Txt
              EXECUTE prp_totales INTO v_sum_tot_aportacion, v_sum_tot_amortizacion,
                                       v_sum_pago_aportacion, v_sum_pago_amortizacion,
                                       v_sum_aportacion, v_sum_amortizacion}

              --Si es negativo, obtiene el valor absoluto
              IF v_sum_tot_aportacion < 0 THEN 
                 LET v_sum_tot_aportacion = (v_sum_tot_aportacion * -1)
              END IF

              --Si es negativo, obtiene el valor absoluto
              IF v_sum_tot_amortizacion < 0 THEN 
                 LET v_sum_tot_amortizacion = (v_sum_tot_amortizacion * -1)
              END IF               

              --Si es negativo, obtiene el valor absoluto
              IF v_sum_can_aportacion < 0 THEN 
                 LET v_sum_can_aportacion = (v_sum_can_aportacion * -1)
              END IF

              --Si es negativo, obtiene el valor absoluto
              IF v_sum_can_amortizacion < 0 THEN 
                 LET v_sum_can_amortizacion = (v_sum_can_amortizacion * -1)
              END IF               
               
              --Si es negativo, obtiene el valor absoluto
              IF v_sum_pago_aportacion < 0 THEN 
                 LET v_sum_pago_aportacion = (v_sum_pago_aportacion * -1)
              END IF               

              --Si es negativo, obtiene el valor absoluto
              IF v_sum_pago_amortizacion < 0 THEN 
                 LET v_sum_pago_amortizacion = (v_sum_pago_amortizacion * -1)
              END IF               
               
              --Si es negativo, obtiene el valor absoluto
              IF v_sum_aportacion < 0 THEN 
                 LET v_sum_aportacion = (v_sum_aportacion * -1)
              END IF

              --Si es negativo, obtiene el valor absoluto
              IF v_sum_amortizacion < 0 THEN 
                 LET v_sum_amortizacion = (v_sum_amortizacion * -1)
              END IF
              
               --Si es negativo el valor de la suma de la compensación de avance de la liquidación(aportacion), obtiene el valor absoluto
              {IF v_sum_cal_aportacion < 0 THEN 
                 LET v_sum_cal_aportacion = (v_sum_cal_aportacion * -1)
              END IF
                                             
                --Si es negativo el valor de la suma de la compensación de avance de la liquidación(aportacion), obtiene el valor absoluto
              IF v_sum_cal_aportacion < 0 THEN 
                 LET v_sum_cal_aportacion = (v_sum_cal_aportacion * -1)   
              END IF}
              
              --Porcentaje = Compensación avance / Avance pago * 100              
              LET v_sum_pca_aportacion   = FGL_DECIMAL_TRUNCATE((v_sum_pago_aportacion / v_sum_tot_aportacion * 100),2)              
              LET v_sum_pca_amortizacion = FGL_DECIMAL_TRUNCATE((v_sum_pago_amortizacion / v_sum_tot_amortizacion * 100),2)        
              
              --Escritura de totales
              DISPLAY v_sum_tot_aportacion    TO tot_av_aport
              DISPLAY v_sum_tot_amortizacion  TO tot_av_amort
              DISPLAY v_sum_can_aportacion    TO tot_can_aport
              DISPLAY v_sum_can_amortizacion  TO tot_can_amort
              DISPLAY v_sum_pago_aportacion   TO tot_pag_aport
              DISPLAY v_sum_pago_amortizacion TO tot_pag_amort
              DISPLAY v_sum_aportacion        TO tot_res_aport
              DISPLAY v_sum_amortizacion      TO tot_res_amort
              DISPLAY v_sum_pca_aportacion    TO tot_pca_aport
              DISPLAY v_sum_pca_amortizacion  TO tot_pca_amort
              {DISPLAY v_sum_cal_aportacion    TO tot_cal_aport
              DISPLAY v_sum_cal_amortizacion  TO tot_cal_amort}
              
              DISPLAY ARRAY arr_detalles_av_pag TO scr_det_liquida.* ATTRIBUTES (ACCEPT = FALSE )
                ON ACTION CANCEL 
                   EXIT PROGRAM  
                     
                ON ACTION reporte
                   CALL fn_rpt_detalle_av_pago(g_usuario,
                                               v_sum_tot_aportacion,
                                               v_sum_tot_amortizacion,
                                               v_sum_can_aportacion,
                                               v_sum_can_amortizacion,
                                               v_sum_pago_aportacion,
                                               v_sum_pago_amortizacion,
                                               v_sum_aportacion,
                                               v_sum_amortizacion,
                                               v_sum_pca_aportacion, 
                                               v_sum_pca_amortizacion,
                                               {v_sum_cal_aportacion,
                                               v_sum_cal_amortizacion,}
                                               v_tot_reg)

                ON ACTION archivo
                   CALL fn_genera_archivo_liquidacion()
                
              END DISPLAY  
           END IF 
      END INPUT
    END DIALOG
     
  CLOSE WINDOW vtn_CNTC01 
END MAIN 


--Genera un archivo de salida en texto plano con la información de la liquidación de avances de pago
FUNCTION fn_genera_archivo_liquidacion()
  DEFINE 
    v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
    v_ruta_envio_cnt         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_recorre_arreglo        INTEGER,
    v_archivo_copia          VARCHAR (50),
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_leyenda                STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".cnt"

  LET v_nom_archivo   = "/liquidacion_ava_pag_", v_hora

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_cnt
  FROM   seg_modulo
  WHERE  modulo_cod = "cnt"

  LET v_ruta_nomarch = v_ruta_envio_cnt CLIPPED || v_nom_archivo CLIPPED 
  --DISPLAY "Ruta: ",v_ruta_nomarch

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  --periodo_ini
  LET v_encabezado = "Periodo de Pago: ",periodo_ini,"-",periodo_fin,"|"
  CALL v_ch_arch_salida.write([v_encabezado])
  --LET v_encabezado = " |AVANCE| |CANCELACION-RECHAZO AVANCE| |COMPENSACION AVANCE| |AVANCE ABIERTO| |COMPENSACIÓN AVANCE-LIQUIDACION|"
  --LET v_encabezado = " |AVANCE| |CANCELACION-RECHAZO AVANCE| |COMPENSACION AVANCE| |AVANCE ABIERTO| |PORCENTAJE - COMPENSACIÓN AVANCE|"
  LET v_encabezado = " |AVANCE| |CANCELACION-RECHAZO AVANCE| |RECUPERACIÓN DE AVANCES| |AVANCE POR RECUPERAR| |PORCENTAJE RECUPERACIÓN DE AVANCES|"
  CALL v_ch_arch_salida.write([v_encabezado])
  LET v_encabezado = "Periodo Pago|Aportaciones|Amortizaciones|Aportaciones|Amortizaciones|Aportaciones|Amortizaciones|Aportaciones|Amortizaciones|Aportaciones|Amortizaciones"
  CALL v_ch_arch_salida.write([v_encabezado])
   
  FOR v_recorre_arreglo = 1 TO arr_detalles_av_pag.getLength()  	  
  	
      LET v_detalle = arr_detalles_av_pag[v_recorre_arreglo].v_periodo_pago, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_ds_tot_aportacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_ds_tot_amortizacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_can_aportacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_can_amortizacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_dc_pago_aportacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_dc_pago_amortizacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_res_aportacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_res_amortizacion, "|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_pca_aportacion, "%|",
                      arr_detalles_av_pag[v_recorre_arreglo].v_pca_amortizacion, "%"
                      {arr_detalles_av_pag[v_recorre_arreglo].v_cal_aportacion,"|",   --Se agregan los campos para incluir la compensación avance de liquidación
                      arr_detalles_av_pag[v_recorre_arreglo].v_cal_amortizacion}
                      
      CALL v_ch_arch_salida.write([v_detalle])

  END FOR

  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_sum_tot_aportacion,"|",
                  v_sum_tot_amortizacion, "|",
                  v_sum_can_aportacion,  "|",
                  v_sum_can_amortizacion, "|",
                  v_sum_pago_aportacion, "|",
                  v_sum_pago_amortizacion, "|",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion,"|",
                  v_sum_pca_aportacion, "%|", 
                  v_sum_pca_amortizacion, "%"
                  {v_sum_cal_aportacion,"|",
                   v_sum_cal_amortizacion}

  CALL v_ch_arch_salida.write([v_sumario])

  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Liquidación de Avances de Pago\n en la ruta"||v_ruta_nomarch,"information")
  
END FUNCTION 

#OBJETIVO: Generar el reporte de la Consulta
FUNCTION fn_rpt_detalle_av_pago(g_usuario, 
                                p_sum_tot_aportacion,
                                p_sum_tot_amortizacion,
                                p_sum_can_aportacion,
                                p_sum_can_amortizacion,
                                p_sum_pago_aportacion,
                                p_sum_pago_amortizacion,
                                p_sum_aportacion,
                                p_sum_amortizacion,
                                p_sum_pca_aportacion,
                                p_sum_pca_amortizacion,
                                {p_sum_cal_aportacion,
                                p_sum_cal_amortizacion,}
                                v_total_reg)
   {DEFINE arr_detalles_aux_pag DYNAMIC ARRAY OF RECORD ---se agrega arreglo para envío a reporte
    v_periodo_pago           CHAR (6),
    v_ds_tot_aportacion      DECIMAL(22,2), 
    v_ds_tot_amortizacion    DECIMAL(22,2),
    v_can_aportacion         DECIMAL(22,2),
    v_can_amortizacion       DECIMAL(22,2),
    v_dc_pago_aportacion     DECIMAL(22,2),
    v_dc_pago_amortizacion   DECIMAL(22,2),
    v_res_aportacion         DECIMAL(22,2),
    v_res_amortizacion       DECIMAL(22,2)
   END RECORD}
   
  DEFINE 
    g_usuario                CHAR(20),
    p_sum_tot_aportacion     DECIMAL(22,2), 
    p_sum_tot_amortizacion   DECIMAL(22,2), 
    p_sum_can_aportacion     DECIMAL(22,2), 
    p_sum_can_amortizacion   DECIMAL(22,2), 
    p_sum_pago_aportacion    DECIMAL(22,2),
    p_sum_pago_amortizacion  DECIMAL(22,2),
    p_sum_aportacion         DECIMAL(22,2),
    p_sum_amortizacion       DECIMAL(22,2),
    p_sum_pca_aportacion     DECIMAL(5,2), 
    p_sum_pca_amortizacion   DECIMAL(5,2),
    {p_sum_cal_aportacion     DECIMAL(22,2),--Se integra totales para el reporte
    p_sum_cal_amortizacion   DECIMAL(22,2),}
    v_total_reg              INTEGER,
    v_reg                    INTEGER,
    manejador_rpt            om.SaxDocumentHandler  -- Contenedor documentos reporte

  LET v_reg = 1 

  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("CNTC011.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte de registros con rechazo
  START REPORT rpt_detalle_av_pago TO XML HANDLER manejador_rpt
     FOR v_reg = 1 TO v_total_reg
         #DEBUG
         {LET arr_detalles_aux_pag[v_reg].v_periodo_pago=arr_detalles_av_pag[v_reg].v_periodo_pago
         LET arr_detalles_aux_pag[v_reg].v_ds_tot_aportacion=arr_detalles_av_pag[v_reg].v_ds_tot_aportacion
         LET arr_detalles_aux_pag[v_reg].v_ds_tot_amortizacion=arr_detalles_av_pag[v_reg].v_ds_tot_amortizacion
         LET arr_detalles_aux_pag[v_reg].v_can_aportacion=arr_detalles_av_pag[v_reg].v_can_aportacion
         LET arr_detalles_aux_pag[v_reg].v_can_amortizacion=arr_detalles_av_pag[v_reg].v_can_amortizacion
         LET arr_detalles_aux_pag[v_reg].v_dc_pago_aportacion=arr_detalles_av_pag[v_reg].v_dc_pago_aportacion
         LET arr_detalles_aux_pag[v_reg].v_dc_pago_amortizacion=arr_detalles_av_pag[v_reg].v_dc_pago_amortizacion
         LET arr_detalles_aux_pag[v_reg].v_res_aportacion=arr_detalles_av_pag[v_reg].v_res_aportacion
         LET arr_detalles_aux_pag[v_reg].v_res_aportacion=arr_detalles_av_pag[v_reg].v_res_aportacion
         LET arr_detalles_aux_pag[v_reg].v_res_amortizacion=arr_detalles_av_pag[v_reg].v_res_amortizacion}
         #END DEBUG                 
        
         OUTPUT TO REPORT rpt_detalle_av_pago(arr_detalles_av_pag[v_reg].*,
                                              g_usuario,
                                              p_sum_tot_aportacion, 
                                              p_sum_tot_amortizacion,
                                              p_sum_can_aportacion, 
                                              p_sum_can_amortizacion,
                                              p_sum_pago_aportacion, 
                                              p_sum_pago_amortizacion,
                                              p_sum_aportacion, 
                                              p_sum_amortizacion,
                                              {p_sum_cal_aportacion,
                                              p_sum_cal_amortizacion}
                                              p_sum_pca_aportacion, 
                                              p_sum_pca_amortizacion)
     END FOR 
  FINISH REPORT rpt_detalle_av_pago
END FUNCTION

#OBJETIVO: Se arma la estructura del reporte 
REPORT rpt_detalle_av_pago(arr_detalles_av_p, g_usuario,
                           v_sum_tot_aportacion, 
                           v_sum_tot_amortizacion,
                           v_sum_can_aportacion, 
                           v_sum_can_amortizacion,
                           v_sum_pago_aportacion, 
                           v_sum_pago_amortizacion,
                           v_sum_aportacion, 
                           v_sum_amortizacion,
                           {v_sum_cal_aportacion,
                           v_sum_cal_amortizacion}
                           v_sum_pca_aportacion, 
                           v_sum_pca_amortizacion)

  DEFINE arr_detalles_av_p   RECORD 
    v_periodo_pago           CHAR(6),
    v_ds_tot_aportacion      DECIMAL(22,2), 
    v_ds_tot_amortizacion    DECIMAL(22,2), 
    v_can_aportacion         DECIMAL(22,2),
    v_can_amortizacion       DECIMAL(22,2),
    v_dc_pago_aportacion     DECIMAL(22,2),
    v_dc_pago_amortizacion   DECIMAL(22,2),
    v_res_aportacion         DECIMAL(22,2),
    v_res_amortizacion       DECIMAL(22,2),
    v_pca_aportacion         DECIMAL(5,2), 
    v_pca_amortizacion       DECIMAL(5,2)
    {v_cal_aportacion         DECIMAL(22,2),--Se  integra la suma de la compensación avance liquidación
    v_cal_amortizacion       DECIMAL(22,2)}
  END RECORD

  DEFINE 
    v_fecha_reporte          DATE,
    g_usuario                CHAR(20),
    v_sum_tot_aportacion     DECIMAL(22,2), 
    v_sum_tot_amortizacion   DECIMAL(22,2), 
    v_sum_can_aportacion     DECIMAL(22,2),
    v_sum_can_amortizacion   DECIMAL(22,2),
    v_sum_pago_aportacion    DECIMAL(22,2),
    v_sum_pago_amortizacion  DECIMAL(22,2),
    v_sum_aportacion         DECIMAL(22,2), 
    v_sum_amortizacion       DECIMAL(22,2),
    v_sum_pca_aportacion     DECIMAL(5,2),
    v_sum_pca_amortizacion   DECIMAL(5,2)
    {v_sum_cal_aportacion     DECIMAL(22,2),
    v_sum_cal_amortizacion   DECIMAL(22,2)}

  FORMAT
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY 

      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX g_usuario

    ON EVERY ROW 
      PRINTX arr_detalles_av_p.*

    ON LAST ROW
      PRINTX v_sum_tot_aportacion
      PRINTX v_sum_tot_amortizacion
      PRINTX v_sum_can_aportacion
      PRINTX v_sum_can_amortizacion
      PRINTX v_sum_pago_aportacion
      PRINTX v_sum_pago_amortizacion
      PRINTX v_sum_aportacion
      PRINTX v_sum_amortizacion
      PRINTX v_sum_pca_aportacion
      PRINTX v_sum_pca_amortizacion
      {PRINTX v_sum_cal_aportacion
      PRINTX v_sum_cal_amortizacion}
     
END REPORT

#Objetivo: Se crea tabla temporal para la consulta y reporte de Liquidación Avances de Pago
FUNCTION fn_crea_tabla_tem()
  WHENEVER ERROR CONTINUE 
    DROP TABLE tmp_liq_ctr_ava;

  CREATE TABLE tmp_liq_ctr_ava (periodo_pago     CHAR(6),
                                ava_apo          DECIMAL(22,2),
                                ava_amo          DECIMAL(22,2),
                                can_apo          DECIMAL(22,2),
                                can_amo          DECIMAL(22,2),
                                comp_apo         DECIMAL(22,2),
                                comp_amo         DECIMAL(22,2),
                                Res_aportacion   DECIMAL(22,2),
                                Res_amortizacion DECIMAL(22,2));

  CREATE INDEX xpktmp_liq_ctr_ava ON tmp_liq_ctr_ava(periodo_pago);

END FUNCTION