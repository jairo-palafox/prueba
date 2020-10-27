DATABASE safre_viv
GLOBALS 
    DEFINE v_tabla              SMALLINT
    DEFINE v_estado             SMALLINT
    DEFINE v_modalidad          SMALLINT 
    DEFINE v_con_ini            DATE
    DEFINE v_rechazo            SMALLINT 
    DEFINE v_con_fin            DATE
    DEFINE v_tpo_retiro         CHAR(1)
    DEFINE v_folio              DECIMAL(9,0)
    DEFINE v_nss                CHAR(20)
    DEFINE v_id_derechohabiente CHAR(18)
    DEFINE v_ind_consistencia   SMALLINT   

  DEFINE g_ar_solo_infonavit
    DYNAMIC ARRAY OF RECORD    --LIKE ret_solo_infonavit 
            id_solicitud       DECIMAL(9,0) ,
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_solo_infonavit.f_solicitud,
            clabe              CHAR(18),
            acc_viv97          DECIMAL (19,6),
            pes_viv97          DECIMAL (19,6),
            f_valuacion        DATE ,
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18)
    END RECORD
    
  DEFINE g_ar_combinado
    DYNAMIC ARRAY OF RECORD
            id_tipo_retiro     SMALLINT,
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_fondo_ahorro.f_solicitud,
            pes_viv72          DECIMAL (19,6),
            pes_viv49          DECIMAL (19,6),
            aivs_viv92         DECIMAL (19,6),
            aivs_viv97         DECIMAL (19,6),
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18)
        END RECORD 

 DEFINE g_ar_ley73
    DYNAMIC ARRAY OF RECORD  --LIKE ret_fondo_ahorro
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        DATE ,
            tpo_proceso        SMALLINT  ,
            folio              DECIMAL(9,0),
            importe_viv92      DECIMAL(14,2),
            importe_viv97      DECIMAL(14,2),
            aivs_viv92         DECIMAL(19,6),
            aivs_viv97         DECIMAL(19,6),
            estado_solicitud   CHAR(18),
            cod_retorno        SMALLINT,  
            cod_rechazo        CHAR(18)            
        END RECORD 

 DEFINE g_ar_fondo_ahorro
    DYNAMIC ARRAY OF RECORD  --LIKE ret_fondo_ahorro
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_fondo_ahorro.f_solicitud,
            cve_referencia     LIKE ret_fondo_ahorro.cve_referencia,
            folio              LIKE ret_fondo_ahorro.folio,
            importe_viv72      DECIMAL(14,2),
            id_datamart        VARCHAR(50),
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18),
            id_causal          VARCHAR(50)
        END RECORD 

  DEFINE g_ar_fortalecimiento
   DYNAMIC ARRAY OF RECORD
           id_solicitud       DECIMAL(9,0),
           id_derechohabiente DECIMAL(9,0),
           nss                CHAR(20),
           folio              LIKE ret_fortalecimiento_credito.folio,
           f_solicitud        LIKE ret_fortalecimiento_credito.f_solicitud,
           --h_solicitud        DATETIME HOUR TO SECOND,
           importe_viv        DECIMAL (19,6),
           estado_solicitud   CHAR(18),
           cod_rechazo        CHAR(18)
    END RECORD
    
  DEFINE g_ar_totales
   DYNAMIC ARRAY OF RECORD
           id                  SMALLINT ,
           tipo_retiro         VARCHAR(50),
           movimiento          SMALLINT,
           importe             DECIMAL (19,6),
           importe_49          DECIMAL (19,6),
           aivs92              DECIMAL (19,6),
           aivs97              DECIMAL (19,6),
           id_tipo_retiro      SMALLINT 
    END RECORD

  DEFINE g_ar_retiro
   DYNAMIC ARRAY OF RECORD
           id                  SMALLINT,      --modulo
           desc_tipo_retiro    VARCHAR(50),   --desc
           movimiento          SMALLINT,      --movimiento
           importe72           DECIMAL(14,6), --importe
           importe49           DECIMAL(14,6), --importe fortalecimiento
           aivs92              DECIMAL(14,6), --aivs92
           aivs97              DECIMAL(14,6), --aivs97
           tipo_retiro         CHAR(1),       -- tipo de retiro = 'A'
           id_matriz_derecho   SMALLINT       --identificador para la tabla matriz derecho 
    END RECORD  

   DEFINE g_ar_preliquida
    DYNAMIC ARRAY OF RECORD    --LIKE ret_preliquida
           id_derechohabiente  LIKE ret_preliquida.id_derechohabiente,
           nss                 CHAR(20),
           movimiento          LIKE ret_preliquida.movimiento,
           f_liquida           LIKE ret_preliquida.f_liquida,
           id_referencia       LIKE ret_preliquida.id_referencia,
           folio_liquida       LIKE ret_preliquida.folio_liquida,
           monto_pesos         LIKE ret_preliquida72.importe
        END RECORD

DEFINE g_ar_liquida
DYNAMIC ARRAY OF RECORD        --LIKE cta_movimiento
           id_derechohabiente  LIKE cta_movimiento.id_derechohabiente,
           nss                 CHAR(20),
           movimiento          LIKE cta_movimiento.movimiento,
           f_liquida           LIKE cta_movimiento.f_liquida,
           id_referencia       LIKE cta_movimiento.id_referencia,
           folio_liquida       LIKE cta_movimiento.folio_liquida,
           monto_pesos         LIKE cta_movimiento.monto_pesos
        END RECORD

DEFINE g_ar_disposicion
   DYNAMIC ARRAY OF RECORD        --LIKE ret_disposicion.*
            id_solicitud          DECIMAL(9,0),
            id_derechohabiente    DECIMAL(9,0),
            nss                   CHAR(20),
            f_solicitud           LIKE ret_disposicion.f_solicitud,            
            folio                 LIKE ret_disposicion.folio,
            importe_viv72         LIKE ret_disposicion.importe_viv72,
            importe_viv92         LIKE ret_disposicion.aivs_viv92,
            importe_viv97         LIKE ret_disposicion.aivs_viv97,
            id_datamart           VARCHAR(50),
            estado_solicitud      CHAR(18),
            cod_rechazo           CHAR(18),
            id_causal             VARCHAR(50),
            id_ret_matriz_derecho SMALLINT
        END RECORD

DEFINE g_ar_transferencia
   DYNAMIC ARRAY OF RECORD        --LIKE ret_transferencia.*
            id_solicitud          DECIMAL(9,0),
            id_derechohabiente    DECIMAL(9,0),
            nss                   CHAR(20),
            f_solicitud           LIKE ret_transferencia.f_resolucion,
            folio                 LIKE ret_transferencia.folio,
            aivs_viv97             LIKE ret_transferencia.aivs_viv97,
            id_datamart           VARCHAR(50),
            estado_solicitud      CHAR(18),
            cod_rechazo           CHAR(18),
            f_valuacion           DATE ,
            id_ret_matriz_derecho SMALLINT
        END RECORD  

        DEFINE g_ar_tipo_n
   DYNAMIC ARRAY OF RECORD        --LIKE ret_transferencia.*
            id_solicitud          DECIMAL(9,0),
            id_derechohabiente    DECIMAL(9,0),
            nss                   CHAR(20),
            f_solicitud           LIKE ret_transferencia.f_resolucion,    
            folio                 LIKE ret_transferencia.folio,
            aivs_viv92            LIKE ret_transferencia.aivs_viv97,
            id_datamart           VARCHAR(50),
            estado_solicitud      CHAR(18),
            cod_rechazo           CHAR(18),
            f_valuacion           DATE ,
            id_ret_matriz_derecho SMALLINT 
        END RECORD  
 
END GLOBALS 

--genera datos para el resumen general Liquidacion 
FUNCTION fn_consulta_liquidacion(p_cont)
   DEFINE p_cont SMALLINT
   DEFINE v_total_pesos DECIMAL(19,6) 
   DEFINE v_c           SMALLINT 

   IF p_cont <= 0 THEN 
     RETURN
   END IF

    LET v_total_pesos = 0
    FOR v_c = 1 TO p_cont
        IF  g_ar_liquida[v_c].id_derechohabiente IS NOT NULL THEN 
           LET v_total_pesos = v_total_pesos + g_ar_liquida[v_c].monto_pesos
        END IF 
    END FOR

   OPEN WINDOW w_consulta_liquida WITH FORM "RETF024"
   DISPLAY ARRAY  g_ar_liquida TO t_liquida.*
        ATTRIBUTE (ACCEPT = FALSE ,CANCEL  = FALSE )
   
    BEFORE DISPLAY 
     --CALL g_ar_liquida.deleteElement(p_cont)
     DISPLAY "Liquidacion" TO ff_desc_consul
     DISPLAY "Total Registros: "||p_cont -2   TO ff_cont_reg
     DISPLAY v_total_pesos TO ff_total
     IF p_cont <= 0 THEN
        CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
        EXIT DISPLAY  
     END IF 
     
   ON ACTION Regresar 
      EXIT DISPLAY 
      
   END DISPLAY 
   CLOSE WINDOW w_consulta_liquida
END FUNCTION  

--genera la consulta para la parte de liquidacion
FUNCTION fn_liquidacion(p_con_ini,p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_query          VARCHAR(2000)
 DEFINE v_cont           SMALLINT
 DEFINE v_cadena         VARCHAR(200)
 DEFINE v_pesos_total    LIKE cta_fondo72.importe
 
 --DISPLAY "RETX00" ,v_modalidad
 CALL g_ar_liquida.clear( )

 LET v_cont = 1

 LET v_query =  "\n SELECT id_derechohabiente,' ',",
                "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                "\n FROM cta_movimiento",
                "\n WHERE f_liquida between ? and ?",
                "\n   AND (id_derechohabiente = ?",
                "\n    or ? is null or ? =  '')",
                "\n   AND (folio_liquida = ?",
                "\n    or ? is null or ? = '')"

          --habilita la liquidacion para todos los retiros 
          IF v_modalidad = 0 THEN 
             LET v_query =   "\n SELECT id_derechohabiente,' ', ",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                             "\n FROM cta_movimiento            ",
                             "\n WHERE f_liquida between ? and ?",
                             "\n   AND (id_derechohabiente = ?  ",
                             "\n    or ? is null or ? =  '')    ",
                             "\n   AND (folio_liquida = ?       ",
                             "\n    or ? is null or ? = '')     ",
                             "\n   AND movimiento in (172,212,222,462)",
                             "\n UNION ALL                      ",
                             "\n SELECT 0,' ' ,                 ",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                             "\n FROM cta_fondo72               ",
                             "\n WHERE f_liquida between ? and ?",
                             "\n   AND (folio_liquida = ?      ",
                             "\n    or ? is null or ? = '')    ",
                             "\n   AND movimiento in (182)     ",
                             "\n UNION ALL                     ",
                             "\n SELECT 0,afi.nss ,            ",
                             "\n cta.movimiento,cta.f_liquida,cta.id_referencia,cta.folio_liquida,cta.monto_pesos",
                             "\n FROM cta_decreto cta,         ",
                             "\n      afi_decreto afi          ",
                             "\n WHERE cta.f_liquida between ? and ? ",
                             "\n   AND cta.id_decreto =  afi.id_decreto ",
                             "\n   AND (afi.nss = ?            ",
                             "\n    or ? is null or ? =  '')   ",
                             "\n   AND (cta.folio_liquida = ?  ",
                             "\n    or ? is null or ? = '')    "             
                             
              LET v_cadena = "\n   AND movimiento in (202)"              
          END IF

          --habilita la liquidacion para solo infonavit
          IF v_modalidad = 1 THEN 
             LET v_cadena = "\n   AND movimiento in (172)"
          END IF 

          --habilita la liquidacion para fondo ahorro
          IF v_modalidad = 2 THEN 
             LET v_query =  "\n SELECT 0, '' ,",
                            "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                            "\n FROM cta_fondo72",
                            "\n WHERE f_liquida between ? and ?",
                            "\n   AND (folio_liquida = ?",
                            "\n    or ? is null or ? = '')"
                            
             LET v_cadena = "\n   AND movimiento in (182)"
          END IF

            --habilita la liquidacion para Ley73
          IF v_modalidad = 3 THEN 
             LET v_cadena = "\n   AND movimiento in (192)"
          END IF

          --habilita la liquidacion para tipo_n
          IF v_modalidad = 4 THEN 
             LET v_query =  "\n SELECT 0,afi.nss ,",
                            "\n cta.movimiento,cta.f_liquida,cta.id_referencia,cta.folio_liquida,cta.monto_pesos",
                            "\n FROM cta_decreto cta,",
                            "\n     afi_decreto afi",
                            "\n WHERE cta.f_liquida between ? and ?",
                            "\n   AND cta.id_decreto =  afi.id_decreto",
                            "\n   AND (afi.nss = ?",
                            "\n    or ? is null or ? =  '')",
                            "\n   AND (cta.folio_liquida = ?",
                            "\n    or ? is null or ? = '')"
             
             LET v_cadena = "\n   AND movimiento in (202)"
          END IF

          --habilita la liquidacion para disposicion
          IF v_modalidad = 5 THEN 
             LET v_cadena = "\n   AND movimiento in (212)"
          END IF 

          --habilita la liquidacion para transferenicia
          IF v_modalidad = 6 THEN 
             LET v_cadena = "\n   AND movimiento in (222)"
          END IF 

                    --habilita la preliquidacion para fortalecimiento
          IF v_modalidad = 7 THEN 
             LET v_cadena = "\n   AND movimiento in (462)"
          END IF 

          LET v_query  = v_query CLIPPED || v_cadena CLIPPED             
                           
          DISPLAY  v_query
          LET v_pesos_total = 0
           
          PREPARE pr_liquida FROM v_query
          DECLARE cur_liquida CURSOR FOR pr_liquida

          CASE v_modalidad
          
           WHEN 0
             
             CALL fn_liquida_modulo_0(p_con_ini , p_con_fin)
             RETURNING v_cont ,v_pesos_total
             
           WHEN 2
             CALL fn_liquida_modulo_2(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total
             
           OTHERWISE
             CALL fn_liquida_modulo_1(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total
              
          END CASE 
          --LET v_pesos_total = 0
           
          LET g_ar_liquida[v_cont].nss  = "TOTAL"
          LET g_ar_liquida[v_cont].monto_pesos  = v_pesos_total
          --DISPLAY v_pesos_total
          
          CALL fn_consulta_liquidacion(v_cont)
END FUNCTION

FUNCTION fn_liquida_modulo_0(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE cta_fondo72.importe
 LET v_cont = 1
 LET v_pesos_total = 0
        FOREACH cur_liquida USING  p_con_ini , p_con_fin
                                   , v_id_derechohabiente ,v_id_derechohabiente
                                   , v_id_derechohabiente ,v_folio,v_folio,v_folio 
                                   , p_con_ini , p_con_fin
                                   --, v_nss ,v_nss, v_nss  --cta_fondo72 no tiene nss 
                                   ,v_folio,v_folio,v_folio
                                   , p_con_ini , p_con_fin
                                   , v_nss ,v_nss
                                   , v_nss ,v_folio,v_folio,v_folio
                                INTO g_ar_liquida[v_cont].*
          
          IF g_ar_liquida[v_cont].id_derechohabiente  = 0.00 THEN 
            SELECT id_derechohabiente
               INTO g_ar_liquida[v_cont].id_derechohabiente 
               FROM afi_derechohabiente
              WHERE nss = g_ar_liquida[v_cont].nss
         ELSE
                 SELECT nss
               INTO g_ar_liquida[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_liquida[v_cont].id_derechohabiente
         END IF 
         
              --construye los encabezados y totales de pagina
              IF g_ar_liquida[v_cont].movimiento <> v_bnd_movimiento THEN
                 IF (v_cont - 1) <= 0 THEN
                    LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont].monto_pesos 
                    LET g_ar_liquida[v_cont+1].* = g_ar_liquida[v_cont].*
                    INITIALIZE g_ar_liquida[v_cont].* TO NULL 
                    LET g_ar_liquida[v_cont].nss  = "Movimiento - ",g_ar_liquida[v_cont+1].movimiento
                    LET v_bnd_movimiento = g_ar_liquida[v_cont+1].movimiento
                    LET v_cont = v_cont + 2
                  ELSE
                    LET g_ar_liquida[v_cont+2].* = g_ar_liquida[v_cont].*
                    INITIALIZE g_ar_liquida[v_cont].* TO NULL
                    LET g_ar_liquida[v_cont].nss  = "Total"
                    LET g_ar_liquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total = 0
                    INITIALIZE g_ar_liquida[v_cont+1].* TO NULL 
                    LET g_ar_liquida[v_cont+1].nss  = "Movimiento - ",g_ar_liquida[v_cont+2].movimiento
                    LET v_bnd_movimiento = g_ar_liquida[v_cont+2].movimiento
                    LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont+2].monto_pesos
                    LET v_cont = v_cont + 3                    
                  END IF 
              ELSE
                 LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont].monto_pesos
                 LET v_cont = v_cont + 1 
              END IF               
          END FOREACH
RETURN v_cont ,v_pesos_total
END FUNCTION 

FUNCTION fn_liquida_modulo_1(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida.monto_pesos

 LET v_cont = 1
 LET v_pesos_total = 0

             FOREACH cur_liquida USING p_con_ini , p_con_fin
                 , v_id_derechohabiente ,v_id_derechohabiente
                 , v_id_derechohabiente ,v_folio,v_folio,v_folio 
                                  INTO g_ar_liquida[v_cont].*
             SELECT nss
               INTO g_ar_liquida[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_liquida[v_cont].id_derechohabiente
              
              --construye los encabezados y totales de pagina
              IF g_ar_liquida[v_cont].movimiento <> v_bnd_movimiento THEN
                 IF (v_cont - 1) <= 0 THEN
                    LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont].monto_pesos 
                    LET g_ar_liquida[v_cont+1].* = g_ar_liquida[v_cont].*
                    INITIALIZE g_ar_liquida[v_cont].* TO NULL 
                    LET g_ar_liquida[v_cont].nss  = "Movimiento - ",g_ar_liquida[v_cont+1].movimiento
                    LET v_bnd_movimiento = g_ar_liquida[v_cont+1].movimiento
                    LET v_cont = v_cont + 2
                  ELSE
                    LET g_ar_liquida[v_cont+2].* = g_ar_liquida[v_cont].*
                    INITIALIZE g_ar_liquida[v_cont].* TO NULL
                    LET g_ar_liquida[v_cont].nss  = "Total"
                    LET g_ar_liquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total = 0
                    INITIALIZE g_ar_liquida[v_cont+1].* TO NULL 
                    LET g_ar_liquida[v_cont+1].nss  = "Movimiento - ",g_ar_liquida[v_cont+2].movimiento
                    LET v_bnd_movimiento = g_ar_liquida[v_cont+2].movimiento
                    LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont+2].monto_pesos
                    LET v_cont = v_cont + 3
                    
                  END IF 
              ELSE
                 LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont].monto_pesos
                 LET v_cont = v_cont + 1 
              END IF               
          END FOREACH
RETURN v_cont ,v_pesos_total
END FUNCTION 

FUNCTION fn_liquida_modulo_2(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE cta_movimiento.movimiento
 DEFINE v_pesos_total    LIKE cta_fondo72.importe

 LET v_cont = 1
 LET v_pesos_total = 0

      FOREACH cur_liquida USING p_con_ini , p_con_fin
                               ,v_folio,v_folio,v_folio 
                              INTO g_ar_liquida[v_cont].*
          --DISPLAY  v_cont                     
             SELECT id_derechohabiente
               INTO g_ar_liquida[v_cont].id_derechohabiente 
               FROM afi_derechohabiente
              WHERE nss = g_ar_liquida[v_cont].nss
              
              --construye los encabezados y totales de pagina
              IF g_ar_liquida[v_cont].movimiento <> v_bnd_movimiento THEN
                 IF (v_cont - 1) <= 0 THEN
                    LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont].monto_pesos 
                    LET g_ar_liquida[v_cont+1].* = g_ar_liquida[v_cont].*
                    INITIALIZE g_ar_liquida[v_cont].* TO NULL 
                    LET g_ar_liquida[v_cont].nss  = "Movimiento - ",g_ar_liquida[v_cont+1].movimiento
                    LET v_bnd_movimiento = g_ar_liquida[v_cont+1].movimiento
                    LET v_cont = v_cont + 2
                  ELSE
                    LET g_ar_liquida[v_cont+2].* = g_ar_liquida[v_cont].*
                    INITIALIZE g_ar_liquida[v_cont].* TO NULL
                    LET g_ar_liquida[v_cont].nss  = "Total"
                    LET g_ar_liquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total = 0
                    INITIALIZE g_ar_liquida[v_cont+1].* TO NULL 
                    LET g_ar_liquida[v_cont+1].nss  = "Movimiento - ",g_ar_liquida[v_cont+2].movimiento
                    LET v_bnd_movimiento = g_ar_liquida[v_cont+2].movimiento
                    LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont+2].monto_pesos
                    LET v_cont = v_cont + 3
                    
                  END IF 
              ELSE
                 LET v_pesos_total = v_pesos_total + g_ar_liquida[v_cont].monto_pesos
                 LET v_cont = v_cont + 1 
              END IF               
          END FOREACH
RETURN v_cont ,v_pesos_total
END FUNCTION 

--genera datos para el resumen general preliquidacion
FUNCTION fn_consulta_preliquidacion(p_cont)
   DEFINE p_cont        SMALLINT
   DEFINE v_total_pesos DECIMAL(14,6) 
   DEFINE v_c           SMALLINT 

   IF p_cont <= 0 THEN 
     RETURN
   END IF

    LET v_total_pesos = 0
    FOR v_c = 1 TO p_cont 
        IF g_ar_preliquida[v_c].id_derechohabiente IS NOT NULL THEN
           LET v_total_pesos = v_total_pesos + g_ar_preliquida[v_c].monto_pesos
        END IF 
    END FOR

   OPEN WINDOW w_consulta_preliquida WITH FORM "RETF023"
   DISPLAY ARRAY  g_ar_preliquida TO t_preliquida.*
        ATTRIBUTE (ACCEPT = FALSE ,CANCEL  = FALSE )
   
    BEFORE DISPLAY 
     --CALL g_ar_preliquida.deleteElement(p_cont)
     DISPLAY "Preliquidacion" TO ff_desc_consul
     DISPLAY "Total Registros: "||p_cont -2  TO ff_cont_reg
     DISPLAY v_total_pesos TO ff_total
     IF p_cont <= 0 THEN
       CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
       EXIT DISPLAY  
     END IF 
     
   ON ACTION regresar 
      EXIT DISPLAY 
      
   END DISPLAY 
   CLOSE WINDOW w_consulta_preliquida
END FUNCTION

--genera la consulta para la parte de preliquidacion
FUNCTION fn_preliquidacion(p_con_ini,p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_query          VARCHAR(2000)
 DEFINE v_cont           SMALLINT
 DEFINE v_cadena         VARCHAR(200) 
 DEFINE v_pesos_total    LIKE ret_preliquida72.importe 

 --DISPLAY "RETX00" ,v_modalidad
 CALL g_ar_preliquida.clear( )
      
 LET v_cont = 1

 LET v_query =  "\n SELECT id_derechohabiente,' ',",
                "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                "\n FROM ret_preliquida",
                "\n WHERE f_liquida between ? and ?",
                "\n   AND (id_derechohabiente = ?",
                "\n    or ? is null or ? =  '')",
                "\n   AND (folio_liquida = ?",
                "\n    or ? is null or ? = '')"

          --habilita la preliquidacion para todos los retiros 
          IF v_modalidad = 0 THEN 
             LET v_query =   "\n SELECT id_derechohabiente,' ',",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                             "\n FROM ret_preliquida",
                             "\n WHERE f_liquida between ? and ?",
                             "\n   AND (id_derechohabiente = ?",
                             "\n    or ? is null or ? =  '')",
                             "\n   AND (folio_liquida = ?",
                             "\n    or ? is null or ? = '')",
                             "\n   AND movimiento in (172,212,222,202,462)",
                             "\n UNION ALL                 ",
                             "\n SELECT 0,nss ,",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                             "\n FROM ret_preliquida72 ret,",
                             "\n      afi_fondo72      afi ",
                             "\n WHERE ret.f_liquida between ? and ?",
                             "\n   AND ret.id_afi_fondo72 = afi.id_afi_fondo72",
                             "\n   AND (afi.nss = ?",
                             "\n    or ? is null or ? =  '')",
                             "\n   AND (ret.folio_liquida = ?",
                             "\n    or ? is null or ? = '')"                             
                             
              LET v_cadena = "\n   AND movimiento in (182)"
          END IF

          --habilita la preliquidacion para solo infonavit
          IF v_modalidad = 1 THEN 
             LET v_cadena = "\n   AND movimiento in (172)"
          END IF 

          --habilita la preliquidacion para fondo ahorro
          IF v_modalidad = 2 THEN 
             LET v_query =  "\n SELECT 0,nss ,",
                            "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                            "\n FROM ret_preliquida72 ret,",
                            "\n      afi_fondo72      afi ",
                            "\n WHERE ret.f_liquida between ? and ?",
                            "\n   AND ret.id_afi_fondo72 = afi.id_afi_fondo72",
                            "\n   AND (afi.nss = ?",
                            "\n    or ? is null or ? =  '')",
                            "\n   AND (ret.folio_liquida = ?",
                            "\n    or ? is null or ? = '')"
                            
             LET v_cadena = "\n   AND movimiento in (182)"

          END IF

           --habilita la preliquidacion para Ley73
          IF v_modalidad = 3 THEN 
             LET v_cadena = "\n   AND movimiento in (192)"
          END IF

          --habilita la preliquidacion para tipo_n
          IF v_modalidad = 4 THEN 

          LET v_query =  "\n SELECT 0,afi.nss ,",
                            "\n cta.movimiento,cta.f_liquida,cta.id_referencia,cta.folio_liquida,cta.monto_pesos",
                            "\n FROM cta_decreto cta,",
                            "\n     afi_decreto afi",
                            "\n WHERE cta.f_liquida between ? and ?",
                            "\n   AND cta.id_decreto =  afi.id_decreto",
                            "\n   AND (afi.nss = ?",
                            "\n    or ? is null or ? =  '')",
                            "\n   AND (cta.folio_liquida = ?",
                            "\n    or ? is null or ? = '')"
             
             LET v_cadena = "\n   AND movimiento in (202)"
          END IF

          --habilita la preliquidacion para disposicion
          IF v_modalidad = 5 THEN 
             LET v_cadena = "\n   AND movimiento in (212)"
          END IF 

          --habilita la preliquidacion para transferenicia
          IF v_modalidad = 6 THEN 
             LET v_cadena = "\n   AND movimiento in (222)"
          END IF 

          --habilita la preliquidacion para fortalecimiento
          IF v_modalidad = 7 THEN 
             LET v_cadena = "\n   AND movimiento in (462)"
          END IF 

          LET v_query  = v_query CLIPPED || v_cadena CLIPPED             
                           
          --DISPLAY   v_query
          --DISPLAY   v_cadena
          LET v_pesos_total = 0 
          PREPARE pr_preliquida FROM v_query
          DECLARE cur_preliquida CURSOR FOR pr_preliquida

          CASE v_modalidad
          
           WHEN 0
           --DISPLAY "si entro "
            --foreach con las instrucciones
             CALL fn_preliquida_modulo_0(p_con_ini , p_con_fin)
             RETURNING v_cont ,v_pesos_total
             
           WHEN 2
             CALL fn_preliquida_modulo_2(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total
             
           OTHERWISE
             CALL fn_preliquida_modulo_1(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total
              
          END CASE 
          --LET v_pesos_total = 0
           
          LET g_ar_preliquida[v_cont].nss  = "TOTAL"
          LET g_ar_preliquida[v_cont].monto_pesos  = v_pesos_total
          --DISPLAY v_pesos_total
          
          CALL fn_consulta_preliquidacion(v_cont)
END FUNCTION

FUNCTION fn_preliquida_modulo_0(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida72.importe

 LET v_cont = 1
 LET v_pesos_total = 0

        FOREACH cur_preliquida USING p_con_ini , p_con_fin
                                   , v_id_derechohabiente ,v_id_derechohabiente
                                   , v_id_derechohabiente ,v_folio,v_folio,v_folio 
                                   , p_con_ini , p_con_fin
                                   , v_nss ,v_nss
                                   , v_nss ,v_folio,v_folio,v_folio
                                INTO g_ar_preliquida[v_cont].*
          
          IF g_ar_preliquida[v_cont].id_derechohabiente  = 0.00 THEN 
            SELECT id_derechohabiente
               INTO g_ar_preliquida[v_cont].id_derechohabiente 
               FROM afi_derechohabiente
              WHERE nss = g_ar_preliquida[v_cont].nss
         ELSE
                 SELECT nss
               INTO g_ar_preliquida[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_preliquida[v_cont].id_derechohabiente
         END IF 
         
              --construye los encabezados y totales de pagina
              IF g_ar_preliquida[v_cont].movimiento <> v_bnd_movimiento THEN
                 IF (v_cont - 1) <= 0 THEN
                    LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos 
                    LET g_ar_preliquida[v_cont+1].* = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL 
                    LET g_ar_preliquida[v_cont].nss  = "Movimiento - ",g_ar_preliquida[v_cont+1].movimiento
                    LET v_bnd_movimiento = g_ar_preliquida[v_cont+1].movimiento
                    LET v_cont = v_cont + 2
                  ELSE
                    LET g_ar_preliquida[v_cont+2].* = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL
                    LET g_ar_preliquida[v_cont].nss  = "Total"
                    LET g_ar_preliquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total = 0
                    INITIALIZE g_ar_preliquida[v_cont+1].* TO NULL 
                    LET g_ar_preliquida[v_cont+1].nss  = "Movimiento - ",g_ar_preliquida[v_cont+2].movimiento
                    LET v_bnd_movimiento = g_ar_preliquida[v_cont+2].movimiento
                    LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont+2].monto_pesos
                    LET v_cont = v_cont + 3                    
                  END IF 
              ELSE
                 LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos
                 LET v_cont = v_cont + 1 
              END IF               
          END FOREACH
RETURN v_cont ,v_pesos_total
END FUNCTION 

FUNCTION fn_preliquida_modulo_1(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida.monto_pesos

 LET v_cont = 1
 LET v_pesos_total = 0

             FOREACH cur_preliquida USING p_con_ini , p_con_fin
                 , v_id_derechohabiente ,v_id_derechohabiente
                 , v_id_derechohabiente ,v_folio,v_folio,v_folio 
                                  INTO g_ar_preliquida[v_cont].*
             SELECT nss
               INTO g_ar_preliquida[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_preliquida[v_cont].id_derechohabiente
              
              --construye los encabezados y totales de pagina
              IF g_ar_preliquida[v_cont].movimiento <> v_bnd_movimiento THEN
                 IF (v_cont - 1) <= 0 THEN
                    LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos 
                    LET g_ar_preliquida[v_cont+1].* = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL 
                    LET g_ar_preliquida[v_cont].nss  = "Movimiento - ",g_ar_preliquida[v_cont+1].movimiento
                    LET v_bnd_movimiento = g_ar_preliquida[v_cont+1].movimiento
                    LET v_cont = v_cont + 2
                  ELSE
                    LET g_ar_preliquida[v_cont+2].* = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL
                    LET g_ar_preliquida[v_cont].nss  = "Total"
                    LET g_ar_preliquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total = 0
                    INITIALIZE g_ar_preliquida[v_cont+1].* TO NULL 
                    LET g_ar_preliquida[v_cont+1].nss  = "Movimiento - ",g_ar_preliquida[v_cont+2].movimiento
                    LET v_bnd_movimiento = g_ar_preliquida[v_cont+2].movimiento
                    LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont+2].monto_pesos
                    LET v_cont = v_cont + 3
                    
                  END IF 
              ELSE
                 LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos
                 LET v_cont = v_cont + 1 
              END IF               
          END FOREACH
RETURN v_cont ,v_pesos_total
END FUNCTION 

FUNCTION fn_preliquida_modulo_2(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE ret_preliquida72.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida72.importe

 LET v_cont = 1
 LET v_pesos_total = 0

      FOREACH cur_preliquida USING p_con_ini , p_con_fin
                                 , v_nss ,v_nss, v_nss 
                                 ,v_folio,v_folio,v_folio 
                              INTO g_ar_preliquida[v_cont].*
          --DISPLAY  v_cont                     
             SELECT id_derechohabiente
               INTO g_ar_preliquida[v_cont].id_derechohabiente 
               FROM afi_derechohabiente
              WHERE nss = g_ar_preliquida[v_cont].nss
              
              --construye los encabezados y totales de pagina
              IF g_ar_preliquida[v_cont].movimiento <> v_bnd_movimiento THEN
                 IF (v_cont - 1) <= 0 THEN
                    LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos 
                    LET g_ar_preliquida[v_cont+1].* = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL 
                    LET g_ar_preliquida[v_cont].nss  = "Movimiento - ",g_ar_preliquida[v_cont+1].movimiento
                    LET v_bnd_movimiento = g_ar_preliquida[v_cont+1].movimiento
                    LET v_cont = v_cont + 2
                  ELSE
                    LET g_ar_preliquida[v_cont+2].* = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL
                    LET g_ar_preliquida[v_cont].nss  = "Total"
                    LET g_ar_preliquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total = 0
                    INITIALIZE g_ar_preliquida[v_cont+1].* TO NULL 
                    LET g_ar_preliquida[v_cont+1].nss  = "Movimiento - ",g_ar_preliquida[v_cont+2].movimiento
                    LET v_bnd_movimiento = g_ar_preliquida[v_cont+2].movimiento
                    LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont+2].monto_pesos
                    LET v_cont = v_cont + 3
                    
                  END IF 
              ELSE
                 LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos
                 LET v_cont = v_cont + 1 
              END IF               
          END FOREACH
RETURN v_cont ,v_pesos_total
END FUNCTION 

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_tipo_n(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini     DATE
 DEFINE p_con_fin     DATE  
 DEFINE p_estado      SMALLINT
 DEFINE p_rechazo     SMALLINT
 DEFINE v_cont        SMALLINT
 DEFINE v_query       STRING 
 DEFINE v_total_pesos  DECIMAL(19,6)
 DEFINE v_total_aivs92 DECIMAL(19,6)
 
 DEFINE v_c           SMALLINT 
 DEFINE v_con_tot     SMALLINT
 DEFINE v_desc_estado  CHAR(18)
 DEFINE v_desc_rechazo  CHAR(18) 
 
 CALL g_ar_tipo_n.clear( )

 LET v_query =  "\n SELECT rn.id_solicitud,rn.id_decreto,' ',",
                "\n rcn.f_carga,rn.folio,rn.aivs_viv92,",
                "\n ' ',rn.estado_solicitud,rn.cod_rechazo,today,rn.id_ret_matriz_derecho",
                "\n FROM ret_tipo_n rn,      ",
                "\n      ret_cza_tipo_n rcn ",
                "\n WHERE (rn.folio = ?",
                "\n    or ? is null or ? = '')",
                "\n   AND rn.folio = rcn.folio",
                "\n   AND (rn.estado_solicitud = ? or '0' = ?)",
                "\n   AND (rn.cod_rechazo = ? or '0' = ?)",
                "\n   AND rcn.f_carga between ? and ?",
                "\n   AND (rn.id_decreto = ?",
                "\n    or ? is null or ? =  '')",
                "\n    order by rn.id_ret_matriz_derecho"

 LET v_cont = 1 
 DISPLAY v_query
 LET g_ar_tipo_n[v_cont].nss = "4-Tipo N"
 LET g_ar_tipo_n[v_cont].aivs_viv92 = 0
 LET g_ar_tipo_n[v_cont].f_solicitud = "------------"
 LET v_cont = v_cont + 1

 PREPARE pr_tipo_n FROM v_query
 DECLARE cur_tpo_n CURSOR FOR pr_tipo_n

 --se suprimio la validacion rechazo
 --FOREACH cur_tpo_n USING p_estado,p_estado, p_rechazo ,p_rechazo ,p_con_ini , p_con_fin  
 FOREACH cur_tpo_n USING v_folio, v_folio, v_folio,p_estado,p_estado, p_rechazo ,p_rechazo,p_con_ini , p_con_fin  
      ,v_id_derechohabiente ,v_id_derechohabiente, v_id_derechohabiente      
    --  ,v_ind_consistencia ,v_ind_consistencia, v_ind_consistencia
       INTO g_ar_tipo_n[v_cont].*

     SELECT nss
       INTO g_ar_tipo_n[v_cont].nss 
       FROM afi_derechohabiente
      WHERE id_derechohabiente = g_ar_tipo_n[v_cont].id_derechohabiente

      INITIALIZE v_desc_estado TO NULL 
     SELECT  des_corta
       INTO v_desc_estado
       FROM ret_estado_solicitud
      WHERE estado_solicitud = g_ar_tipo_n[v_cont].estado_solicitud

     LET g_ar_tipo_n[v_cont].estado_solicitud = g_ar_tipo_n[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED

     INITIALIZE v_desc_rechazo TO NULL 
     SELECT  des_corta
       INTO v_desc_rechazo
       FROM ret_rechazo
      WHERE cod_rechazo = g_ar_tipo_n[v_cont].cod_rechazo

     LET g_ar_tipo_n[v_cont].cod_rechazo = g_ar_tipo_n[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
     LET v_cont = v_cont + 1
 END FOREACH  

 LET v_query =  "\n SELECT rn.id_solicitud,rn.id_decreto,' ',",
                "\n rcn.f_carga,rn.folio,rn.aivs_viv92,",
                "\n ' ',rn.estado_solicitud,rn.cod_rechazo_1,today,rn.id_ret_matriz_derecho",
                "\n FROM ret_tipo_n_rch rn,      ",
                "\n      ret_cza_tipo_n_rch rcn ",
                "\n WHERE (rn.folio = ?",
                "\n    or ? is null or ? = '')",
                "\n   AND rn.folio = rcn.folio",
                "\n   AND (rn.estado_solicitud = ? or '0' = ?)",
                "\n   AND (rn.cod_rechazo_1 = ? OR '0' = ?",
                "\n   OR rn.cod_rechazo_2 = ? ",
                "\n   OR rn.cod_rechazo_3 = ? )",
                "\n   AND rcn.f_carga between ? and ?",
                "\n   AND (rn.id_decreto = ?",
                "\n    or ? is null or ? =  '')",
                "\n    order by rn.id_ret_matriz_derecho"
                
 DISPLAY  v_query
 PREPARE pr_tipo_n_rch FROM v_query
 DECLARE cur_tpo_n_rch CURSOR FOR pr_tipo_n_rch

 --se suprimio la validacion rechazo 
 FOREACH cur_tpo_n_rch USING v_folio, v_folio, v_folio
      ,p_estado,p_estado
      , p_rechazo,p_rechazo,p_rechazo,p_rechazo
      ,p_con_ini , p_con_fin  
      ,v_id_derechohabiente ,v_id_derechohabiente, v_id_derechohabiente
       INTO g_ar_tipo_n[v_cont].*

       DISPLAY g_ar_tipo_n[v_cont].*
     SELECT nss
       INTO g_ar_tipo_n[v_cont].nss 
       FROM afi_derechohabiente
      WHERE id_derechohabiente = g_ar_tipo_n[v_cont].id_derechohabiente

      INITIALIZE v_desc_estado TO NULL 
     SELECT  des_corta
       INTO v_desc_estado
       FROM ret_estado_solicitud
      WHERE estado_solicitud = g_ar_tipo_n[v_cont].estado_solicitud

     LET g_ar_tipo_n[v_cont].estado_solicitud = g_ar_tipo_n[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED

     INITIALIZE v_desc_rechazo TO NULL 
     SELECT  des_corta
       INTO v_desc_rechazo
       FROM ret_rechazo
      WHERE cod_rechazo = g_ar_tipo_n[v_cont].cod_rechazo

     LET g_ar_tipo_n[v_cont].cod_rechazo = g_ar_tipo_n[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
     LET v_cont = v_cont + 1
 END FOREACH  
 
 LET v_total_pesos = 0
 LET v_total_aivs92 = 0
 
 FOR v_c = 1 TO v_cont -1
    --LET v_total_pesos = g_ar_tipo_n[v_c].aivs_viv92 + v_total_pesos
    LET v_total_aivs92 = g_ar_tipo_n[v_c].aivs_viv92 + v_total_aivs92
 END FOR 

 IF v_total_aivs92 > 0 OR v_cont - 3 > 0 THEN 
    LET v_con_tot  = g_ar_totales.getLength() + 1
    LET g_ar_totales[v_con_tot].id             = 0
    LET g_ar_totales[v_con_tot].tipo_retiro    = "4 Retiro Tipo n"
    LET g_ar_totales[v_con_tot].movimiento     =  202 
    LET g_ar_totales[v_con_tot].importe        = v_total_pesos
    LET g_ar_totales[v_con_tot].importe_49     = 0
    LET g_ar_totales[v_con_tot].aivs92         = v_total_aivs92
    LET g_ar_totales[v_con_tot].aivs97         = 0
    LET g_ar_totales[v_con_tot].id_tipo_retiro = 4
    LET g_ar_tipo_n[v_cont].f_solicitud        = "------------"
    LET g_ar_tipo_n[v_cont].nss                = "Totales "
    LET g_ar_tipo_n[v_cont].aivs_viv92         = v_total_aivs92
    LET v_cont = v_cont + 1

    CALL g_ar_tipo_n.deleteElement(v_cont)
 END IF
          
          RETURN v_total_pesos,v_total_aivs92
END FUNCTION

