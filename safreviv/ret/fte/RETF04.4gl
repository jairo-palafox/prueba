--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETP02                                                                 #
#OBJETIVO     => PROGRAMA DE CONSULTA GENERAL PARA EL MODULO DE RETIROS                 #
 
#Fecha inicio => Marzo 1, 2012                                                          #
#Modificacion => Marzo 29, 2012                                                          #
#########################################################################################
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


  DEFINE g_ar_combinado
    DYNAMIC ARRAY OF RECORD
            id_tipo_retiro     SMALLINT,
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_fondo_ahorro.f_solicitud,
            pes_viv72          DECIMAL (19,6),
            aivs_viv92         DECIMAL (19,6),
            aivs_viv97         DECIMAL (19,6),
            estado_solicitud   CHAR(18),
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


  DEFINE g_ar_totales
   DYNAMIC ARRAY OF RECORD
           id             SMALLINT ,
           tipo_retiro    VARCHAR(50),
           movimiento     SMALLINT,
           importe        DECIMAL (19,6),
           aivs92         DECIMAL (19,6),
           aivs97         DECIMAL (19,6),
           id_tipo_retiro SMALLINT 
    END RECORD

  DEFINE g_ar_retiro
   DYNAMIC ARRAY OF RECORD
           id                  SMALLINT,      --modulo
           desc_tipo_retiro    VARCHAR(50),   --desc
           movimiento          SMALLINT,      --movimiento
           importe72           DECIMAL(14,6), --importe
           aivs92              DECIMAL(14,6), --aivs92
           aivs97              DECIMAL(14,6), --aivs97
           tipo_retiro         CHAR(1),       -- tipo de retiro = 'A'
           id_matriz_derecho   SMALLINT       --identificador para la tabla matriz derecho 
    END RECORD  

   DEFINE g_ar_preliquida
    DYNAMIC ARRAY OF RECORD  --LIKE ret_preliquida
           id_derechohabiente LIKE ret_preliquida.id_derechohabiente,
           nss                CHAR(20),
           movimiento         LIKE ret_preliquida.movimiento,
           f_liquida          LIKE ret_preliquida.f_liquida,
           id_referencia      LIKE ret_preliquida.id_referencia,
           folio_liquida      LIKE ret_preliquida.folio_liquida,
           monto_pesos        LIKE ret_preliquida72.importe
        END RECORD

DEFINE g_ar_liquida
DYNAMIC ARRAY OF RECORD  --LIKE cta_movimiento
           id_derechohabiente LIKE cta_movimiento.id_derechohabiente,
           nss                CHAR(20),
           movimiento         LIKE cta_movimiento.movimiento,
           f_liquida          LIKE cta_movimiento.f_liquida,
           id_referencia      LIKE cta_movimiento.id_referencia,
           folio_liquida      LIKE cta_movimiento.folio_liquida,
           monto_pesos        LIKE cta_movimiento.monto_pesos
        END RECORD

 
END GLOBALS 

MAIN 
 DEFINE cb_tpo_retiro ui.ComboBox
 DEFINE cb_estado ui.ComboBox
 DEFINE cb_rechazo ui.ComboBox
 DEFINE cb_modalidad ui.ComboBox
 DEFINE v_nss_id CHAR(18)
 
 
 DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING   -- titulo de la ventana 
 
 
 DEFINE v_cont               SMALLINT
 DEFINE v_c                  SMALLINT 
 
 DEFINE t_totalgral_pesos    DECIMAL(19,6)
 DEFINE t_totalgral_aivs92   DECIMAL(19,6)
 DEFINE t_totalgral_aivs97   DECIMAL(19,6)
 
 DEFINE r_total_pesos        DECIMAL(19,6)
 DEFINE r_total_avis92       DECIMAL(19,6)
 DEFINE r_total_avis97       DECIMAL(19,6)

 DEFINE ar_ret_modalidad_retiro 
           RECORD LIKE ret_modalidad_retiro.*
           
 DEFINE ar_ret_tipo_retiro
           RECORD LIKE ret_tipo_retiro.*
    
 DEFINE ar_ret_estado_solicitud
           RECORD LIKE ret_estado_solicitud.*
          
 DEFINE ar_ret_rechazo
           RECORD LIKE ret_rechazo.*
 
   DEFINE w ui.Window
   DEFINE f ui.Form

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

 CLOSE WINDOW SCREEN 
 OPEN WINDOW w_consulta WITH FORM "RETF020"

 LET cb_modalidad     = ui.ComboBox.forName("formonly.cb_modalidad") 
 LET cb_tpo_retiro    = ui.ComboBox.forName("formonly.cb_tpo_retiro")
 LET cb_estado        = ui.ComboBox.forName("formonly.cb_estado")
 LET cb_rechazo       = ui.ComboBox.forName("formonly.cb_rechazo")

   CALL cb_modalidad.clear()
   CALL cb_tpo_retiro.clear()
   CALL cb_estado.clear()
   CALL cb_rechazo.clear()

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   
  INPUT v_tabla,
        v_con_ini,
        v_con_fin,
        v_modalidad,
        v_tpo_retiro,
        v_estado,
        v_rechazo,
        v_nss,
        v_id_derechohabiente,
        v_folio
       FROM  
             rg_estados,
             d_ini,
             d_fin,
             cb_modalidad,
             cb_tpo_retiro,
             cb_estado,
             cb_rechazo,
             e_nss,
             e_dere,
             e_folio
       ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE )

       
    BEFORE INPUT 
       LET v_modalidad = 0
       CALL cb_tpo_retiro.clear()
      DECLARE  c_cb_tpo_retiro CURSOR FOR  
                                    SELECT tpo_retiro, des_corta
                                      FROM ret_tipo_retiro
                                      WHERE (modalidad_retiro = v_modalidad 
                                         OR  0 = v_modalidad) 
                                         ORDER BY tpo_retiro
        LET v_c = 1
        DISPLAY v_modalidad
        CALL cb_tpo_retiro.addItem('0' ,"TODOS")
        FOREACH c_cb_tpo_retiro INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
         CALL cb_tpo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
        END FOREACH
       INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL  
        LET v_tpo_retiro = 0 
       
      DECLARE  c_cb_modalidad CURSOR FOR  
                                 SELECT * 
                                   FROM ret_modalidad_retiro

       CALL cb_modalidad.addItem(0 ,"TODOS") 
       FOREACH c_cb_modalidad INTO ar_ret_modalidad_retiro.*
          CALL cb_modalidad.addItem(ar_ret_modalidad_retiro.modalidad_retiro ,ar_ret_modalidad_retiro.modalidad_retiro||" - "||ar_ret_modalidad_retiro.des_corta)
       END FOREACH 
       INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
       
      DECLARE  c_cb_estado CURSOR FOR  
                                 SELECT * 
                                   FROM ret_estado_solicitud

       CALL cb_estado.addItem(0 ,"TODOS") 
       FOREACH c_cb_estado INTO ar_ret_estado_solicitud.*
          CALL cb_estado.addItem(ar_ret_estado_solicitud.estado_solicitud ,ar_ret_estado_solicitud.estado_solicitud||" - "||ar_ret_estado_solicitud.des_corta)
       END FOREACH
       INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
       
      DECLARE  c_cb_rechazo CURSOR FOR  
                                 SELECT * 
                                   FROM ret_rechazo

       CALL cb_rechazo.addItem(0 ,"TODOS")                            
       FOREACH c_cb_rechazo INTO ar_ret_rechazo.*
          CALL cb_rechazo.addItem(ar_ret_rechazo.cod_rechazo,ar_ret_rechazo.cod_rechazo||" - "||ar_ret_rechazo.des_corta)
       END FOREACH
       INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL

       LET v_modalidad  = 0  --tipos de retiro
       LET v_tabla      = 1  --solicitud - peliquidacion -liquidacion
       LET v_estado     = 0  --estado de la solicitud
       LET v_rechazo    = 0  --estado de la solicitud
       LET v_con_ini    = TODAY
       LET v_con_fin    = TODAY
       LET t_totalgral_pesos = 0
       LET t_totalgral_aivs92 = 0
       LET t_totalgral_aivs97 = 0
       
    ON CHANGE cb_modalidad
      CALL cb_tpo_retiro.clear()

        LET v_c = 1
        DISPLAY v_modalidad
        CALL cb_tpo_retiro.addItem("0" ,"TODOS") 
        FOREACH c_cb_tpo_retiro INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
         CALL cb_tpo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
       END FOREACH 
       INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL

    ON CHANGE rg_estados
      IF v_tabla = 1 THEN
         CALL  f.setElementHidden("lb_estado",0)
         CALL  f.setElementHidden("lb_rechazo",0)
         CALL  f.setElementHidden("formonly.cb_rechazo",0)
         CALL  f.setElementHidden("formonly.cb_estado",0)
      ELSE
         CALL  f.setElementHidden("lb_estado",1)
         CALL  f.setElementHidden("lb_rechazo",1) 
         CALL  f.setElementHidden("formonly.cb_rechazo",1)
         CALL  f.setElementHidden("formonly.cb_estado",1)
      END IF 

    ON ACTION consultar
      IF v_nss IS NOT NULL THEN      
          IF v_id_derechohabiente IS NULL THEN
             SELECT  id_derechohabiente
               INTO  v_id_derechohabiente
               FROM  afi_derechohabiente
              WHERE  nss = v_nss
          ELSE 
                SELECT  id_derechohabiente
                  INTO  v_nss_id
                  FROM  afi_derechohabiente
                 WHERE  nss = v_nss
                 
             IF v_id_derechohabiente <> v_nss_id THEN
                CALL fn_mensaje("Aviso","El nss debe de estar en blanco o ser igual al id derechohabiente","exclamation")
                --ERROR "El nss debe de estar en blanco o ser igual al id derechohabiente" 
                CONTINUE INPUT  
             END IF 
          END IF 
      END IF 
    
      CASE v_tabla 
        WHEN 1     --solicitudes 
         LET v_cont = 0 
            LET t_totalgral_pesos  = 0
            LET t_totalgral_aivs92 = 0
            LET t_totalgral_aivs97 = 0
         
            --fondo-ahorro
         IF v_modalidad = 2 OR v_modalidad = 0 THEN
             LET r_total_avis92 = 0 
             LET r_total_avis97 = 0 
             LET r_total_pesos  = 0 
            CALL fn_solicitud_fondo_ahorro(v_estado, v_rechazo ,v_con_ini , v_con_fin)
             RETURNING r_total_pesos  
             
            LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
            --LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
            --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
         END IF

         -- muestra el resumen de modalidades 
         --agregar los criterios de la busqueda.
         CALL fn_consulta_modalidad(t_totalgral_pesos, t_totalgral_aivs92, t_totalgral_aivs97) 
         
        WHEN 2     --preliquidacion
           CALL fn_preliquidacion(v_con_ini , v_con_fin)

        WHEN 3     --liquidacion
           CALL fn_liquidacion(v_con_ini , v_con_fin)   
           
      END CASE 

    AFTER FIELD e_folio
      NEXT FIELD e_nss
      
    ON ACTION CANCEL 
     IF cb_tpo_retiro IS NULL THEN
       CALL fn_mensaje("Aviso","Form field not found in current form","exclamation")
       --ERROR "Form field not found in current form"
       EXIT PROGRAM
     END IF
     EXIT INPUT

  END INPUT
  CLOSE WINDOW w_consulta
END MAIN

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_fondo_ahorro(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini     DATE
 DEFINE p_con_fin     DATE  
 DEFINE p_estado      SMALLINT
 DEFINE p_rechazo     SMALLINT
 DEFINE v_cont        SMALLINT
 DEFINE v_query       VARCHAR(500)
 DEFINE v_total_pesos DECIMAL(14,6)
 DEFINE v_c           SMALLINT 
 DEFINE v_con_tot     SMALLINT 
 DEFINE v_desc_estado CHAR(18)
 DEFINE v_desc_rechazo CHAR(18)

 CALL g_ar_fondo_ahorro.clear( )


    LET v_query =  "\n SELECT id_solicitud,id_derechohabiente,' ',",
                           "\n f_solicitud,cve_referencia,folio,saldo_viv72,",
                           "\n id_datamart,estado_solicitud,cod_rechazo,causal_retiro",
                           "\n FROM ret_fondo_ahorro",
                           "\n WHERE (estado_solicitud = ? or '0' = ?)",
                           "\n   AND (cod_rechazo = ? or '0' = ?)",
                           "\n   AND f_solicitud between ? and ?" ,
                           "\n   AND (id_derechohabiente = ?",
                           "\n    or ? is null or ? = '')",
                           "\n   AND (folio = ?",
                           "\n    or ? is null or ? = '')"
                           
          LET v_cont = 1 
          LET g_ar_fondo_ahorro[v_cont].nss = "2-Fondo Ahorro"
          LET g_ar_fondo_ahorro[v_cont].importe_viv72 = 0
          LET g_ar_fondo_ahorro[v_cont].cve_referencia = "------------"
          LET v_cont = v_cont + 1

          PREPARE pr_fondo_ahorro FROM v_query
          DECLARE cur_fondo_ahorro CURSOR FOR pr_fondo_ahorro
          FOREACH cur_fondo_ahorro USING p_estado,p_estado, p_rechazo ,p_rechazo,p_con_ini , p_con_fin 
           ,v_id_derechohabiente ,v_id_derechohabiente, v_id_derechohabiente, 
            v_folio, v_folio, v_folio INTO g_ar_fondo_ahorro[v_cont].*
             SELECT nss
               INTO g_ar_fondo_ahorro[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_fondo_ahorro[v_cont].id_derechohabiente

              SELECT  des_corta
              INTO v_desc_estado
              FROM ret_estado_solicitud
              WHERE estado_solicitud = g_ar_fondo_ahorro[v_cont].estado_solicitud

              LET g_ar_fondo_ahorro[v_cont].estado_solicitud = g_ar_fondo_ahorro[v_cont].estado_solicitud CLIPPED ||"-"|| v_desc_estado CLIPPED

              INITIALIZE v_desc_rechazo TO NULL  
              SELECT  des_corta
                INTO v_desc_rechazo
                FROM ret_rechazo
               WHERE cod_rechazo = g_ar_fondo_ahorro[v_cont].cod_rechazo

              LET g_ar_fondo_ahorro[v_cont].cod_rechazo = g_ar_fondo_ahorro[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
              LET v_cont = v_cont + 1
          END FOREACH  
          LET v_total_pesos = 0

         IF g_ar_fondo_ahorro[v_cont].id_derechohabiente IS NULL  THEN
            CALL g_ar_fondo_ahorro.deleteElement(v_cont)
          END IF 

           FOR v_c = 1 TO v_cont 
            IF g_ar_fondo_ahorro[v_c].id_derechohabiente IS NOT NULL THEN
              LET v_total_pesos = g_ar_fondo_ahorro[v_c].importe_viv72 + v_total_pesos
            END IF 
           END FOR 

          IF v_total_pesos > 0 OR v_cont - 2 > 0 THEN 
          
             LET v_con_tot  = g_ar_totales.getLength() + 1
             LET g_ar_totales[v_con_tot].id  = 0
             LET g_ar_totales[v_con_tot].tipo_retiro  = "2 Retiro Fondo Ahorro"
             LET g_ar_totales[v_con_tot].movimiento  = 182          
             LET g_ar_totales[v_con_tot].importe  = v_total_pesos
             LET g_ar_totales[v_con_tot].aivs92   = 0
             LET g_ar_totales[v_con_tot].aivs97   = 0
             LET g_ar_totales[v_con_tot].id_tipo_retiro = 2
             
             LET g_ar_fondo_ahorro[v_cont].cve_referencia = "------------"
             LET g_ar_fondo_ahorro[v_cont].nss = "Totales "
             LET g_ar_fondo_ahorro[v_cont].importe_viv72 = v_total_pesos
             LET v_cont = v_cont + 1 

             CALL g_ar_fondo_ahorro.deleteElement(v_cont)
         END IF 

          RETURN v_total_pesos
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

--genera la consulta para la parte de preliquidacion
FUNCTION fn_preliquidacion(p_con_ini,p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_query          VARCHAR(2000)
 DEFINE v_cont           SMALLINT
 DEFINE v_cadena         VARCHAR(200)
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida72.importe
 
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
                             "\n   AND movimiento in (172,212,222,202)",
                             "\n UNION ALL                 ",
                             "\n SELECT 0,nss ,",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                             "\n FROM ret_preliquida72",
                             "\n WHERE f_liquida between ? and ?",
                             "\n   AND (nss = ?",
                             "\n    or ? is null or ? =  '')",
                             "\n   AND (folio_liquida = ?",
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
                            "\n FROM ret_preliquida72",
                            "\n WHERE f_liquida between ? and ?",
                            "\n   AND (nss = ?",
                            "\n    or ? is null or ? =  '')",
                            "\n   AND (folio_liquida = ?",
                            "\n    or ? is null or ? = '')"
                            
             LET v_cadena = "\n   AND movimiento in (182)"
          END IF

           --habilita la preliquidacion para Ley73
          IF v_modalidad = 3 THEN 
             LET v_cadena = "\n   AND movimiento in (192)"
          END IF

          --habilita la preliquidacion para tipo_n
          IF v_modalidad = 4 THEN 
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
             LET v_cadena = "\n   AND movimiento in (202)"
          END IF 

          LET v_query  = v_query CLIPPED || v_cadena CLIPPED             
                           
          DISPLAY   v_query
          DISPLAY   v_cadena
          LET v_pesos_total = 0 
          PREPARE pr_preliquida FROM v_query
          DECLARE cur_preliquida CURSOR FOR pr_preliquida

          CASE v_modalidad
          
           WHEN 0
           
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
          DISPLAY v_pesos_total
          
          CALL fn_consulta_preliquidacion(v_cont)
END FUNCTION

--genera la consulta para la parte de liquidacion
FUNCTION fn_liquidacion(p_con_ini,p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_query          VARCHAR(2000)
 DEFINE v_cont           SMALLINT
 DEFINE v_cadena         VARCHAR(200)
 DEFINE v_pesos_total    LIKE cta_fondo72.importe
 
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
                             "\n   AND movimiento in (172,212,222)",
                             "\n UNION ALL                      ",
                             "\n SELECT 0,nss ,                 ",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                             "\n FROM cta_fondo72               ",
                             "\n WHERE f_liquida between ? and ?",
                             "\n   AND (nss = ?                ",
                             "\n    or ? is null or ? =  '')   ",
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
             LET v_query =  "\n SELECT 0,nss ,",
                            "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                            "\n FROM cta_fondo72",
                            "\n WHERE f_liquida between ? and ?",
                            "\n   AND (nss = ?",
                            "\n    or ? is null or ? =  '')",
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
             LET v_cadena = "\n   AND movimiento in (202)"
          END IF 

          LET v_query  = v_query CLIPPED || v_cadena CLIPPED             
                           
          --DISPLAY  v_query
          LET v_pesos_total = 0
           
          PREPARE pr_liquida FROM v_query
          DECLARE cur_liquida CURSOR FOR pr_liquida

          CASE v_modalidad
          
           WHEN 0
           --DISPLAY "si entro "
            --foreach con las instrucciones
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
          DISPLAY v_pesos_total
          
          CALL fn_consulta_liquidacion(v_cont)
END FUNCTION

--pantalla del resumen general por modalidad
FUNCTION fn_consulta_modalidad(p_total_pesos,p_total_aivs92,p_total_aivs97)
   DEFINE p_total_pesos  DECIMAL(19,6)
   DEFINE p_total_aivs92 DECIMAL(19,6)
   DEFINE p_total_aivs97 DECIMAL(19,6)
   DEFINE v_c            SMALLINT 
   DEFINE v_cont_ar      SMALLINT 
   
   OPEN WINDOW w_consulta_totales WITH FORM "RETF025"
   INPUT ARRAY  g_ar_totales FROM t_totales.*
      ATTRIBUTE (UNBUFFERED ,WITHOUT DEFAULTS ,APPEND ROW = FALSE 
                ,DELETE ROW = FALSE , INSERT ROW = FALSE ,ACCEPT = FALSE,CANCEL = FALSE)
   
   BEFORE INPUT

    FOR v_c = 1 TO g_ar_totales.getLength()      
    END FOR
    
    IF v_c <= 0 THEN
    --IF NOT (p_total_pesos >0 OR p_total_aivs92 > 0 OR  p_total_aivs97 >0 ) THEN
       CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")  
          EXIT INPUT 
    END IF    
    DISPLAY p_total_pesos  TO ff_total
    DISPLAY p_total_aivs92 TO ff_total1
    DISPLAY p_total_aivs97 TO ff_total2

    ON ACTION recdet
       LET v_cont_ar = 0
       FOR v_c = 1 TO g_ar_totales.getLength()
         IF g_ar_totales[v_c].id = 1 THEN
            LET v_cont_ar = v_cont_ar + 1
         END IF 
       END FOR  
       IF v_cont_ar = 1 THEN 
          FOR v_c = 1 TO g_ar_totales.getLength()
            IF g_ar_totales[v_c].id = 1 THEN
            --seleccion de modulo cuando solo un modulo esta seleccionado
             CASE g_ar_totales[v_c].id_tipo_retiro

                 WHEN 2
                 --fondo_ahorro 
                   CALL fn_consulta_fondo_ahorro(g_ar_fondo_ahorro.getLength())
  
                 OTHERWISE
                   EXIT FOR

              END CASE
            END IF 
          END FOR
        ELSE
           IF v_cont_ar > 0 THEN
           --si existe mas de un seleccionado entra a esta opcion 
              CALL  fn_consulta_combinado_modulos()
           END IF 
        END IF

    ON ACTION Regresar
      EXIT INPUT  

   END INPUT
   CALL g_ar_totales.clear()
   CLOSE WINDOW w_consulta_totales
   
END FUNCTION  

--el input de consulta modalidad
--funcion muestra consulta de modalidad en seleccion multiple 
FUNCTION fn_consulta_combinado_modulos()
   DEFINE p_cont             SMALLINT
   DEFINE v_c                SMALLINT 
   DEFINE v_cont             SMALLINT 
   DEFINE v_cont_ctrl        SMALLINT 
   DEFINE v_total_pesos      DECIMAL(14,6)
   DEFINE v_total_aivs92     DECIMAL(14,6)
   DEFINE v_total_aivs97     DECIMAL(14,6)
   DEFINE v_cont_encabezados SMALLINT
   DEFINE v_cont_1           SMALLINT 

   
   LET p_cont = 0
   LET v_cont_ctrl = 1
   LET v_cont_1 = 1

   FOR v_c = 1 TO g_ar_totales.getLength()
            IF g_ar_totales[v_c].id = 1 THEN
              CASE g_ar_totales[v_c].id_tipo_retiro
                
                 WHEN 2
                 LET v_cont_encabezados = v_cont_encabezados +1 
                   LET p_cont = p_cont + g_ar_fondo_ahorro.getLength()
                   FOR v_cont = 1 TO g_ar_fondo_ahorro.getLength()
                      IF g_ar_fondo_ahorro[v_cont].id_derechohabiente IS NOT NULL THEN 
                         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                         LET v_cont_1 = v_cont_1 + 1
                      ELSE
                         IF g_ar_fondo_ahorro[v_cont].importe_viv72 = 0 THEN
                           LET g_ar_fondo_ahorro[v_cont].importe_viv72 = NULL
                         END IF
                      END IF
                      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_fondo_ahorro[v_cont].id_derechohabiente
                      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_fondo_ahorro[v_cont].id_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_fondo_ahorro[v_cont].f_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_fondo_ahorro[v_cont].nss
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = g_ar_fondo_ahorro[v_cont].importe_viv72
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_fondo_ahorro[v_cont].estado_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_fondo_ahorro[v_cont].cod_rechazo
                      LET v_cont_ctrl = v_cont_ctrl + 1
                  END FOR 
                  
                 OTHERWISE
                   EXIT FOR
              END CASE
            END IF 
          END FOR

          LET v_total_pesos = 0
          LET v_total_aivs92 = 0
          LET v_total_aivs97 = 0
          FOR v_c = 1 TO p_cont
            IF g_ar_combinado[v_c].id_tipo_retiro IS NOT NULL OR  g_ar_combinado[v_c].id_tipo_retiro <> "" THEN
               LET v_total_pesos = v_total_pesos + g_ar_combinado[v_c].pes_viv72
               LET v_total_aivs92 = v_total_aivs92 + g_ar_combinado[v_c].aivs_viv92
               LET v_total_aivs97 = v_total_aivs97 + g_ar_combinado[v_c].aivs_viv97
            END IF 
          END FOR 

   OPEN WINDOW w_consulta_detalle WITH FORM "RETF041"
   DISPLAY ARRAY  g_ar_combinado TO t_detalle_gral.*
         ATTRIBUTE (ACCEPT = FALSE ,CANCEL = FALSE )
   
    BEFORE DISPLAY     
       LET v_cont_encabezados = v_cont_encabezados * 2 
       DISPLAY "Retiro Detalle General" TO ff_desc_consul
       DISPLAY "Total Registros: "||p_cont - v_cont_encabezados TO ff_cont_reg
       DISPLAY v_total_pesos  TO ff_total
       DISPLAY v_total_aivs92 TO ff_total1
       DISPLAY v_total_aivs97 TO ff_total2
       IF p_cont - v_cont_encabezados <= 0 THEN
          CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
          EXIT DISPLAY  
       END IF 
     
   ON ACTION Regresar 
      EXIT DISPLAY 
      
   END DISPLAY 
   CLOSE WINDOW w_consulta_detalle
   CALL g_ar_combinado.clear()
END FUNCTION 


FUNCTION fn_consulta_fondo_ahorro(p_cont)
   DEFINE p_cont SMALLINT
   DEFINE v_total_pesos DECIMAL(14,6)
   DEFINE v_c    SMALLINT 

   IF p_cont <= 0 THEN 
     RETURN 
   END IF 

    LET v_total_pesos = 0
    FOR v_c = 1 TO p_cont
       IF g_ar_fondo_ahorro[v_c].id_derechohabiente IS NOT NULL THEN 
          LET v_total_pesos = v_total_pesos + g_ar_fondo_ahorro[v_c].importe_viv72
       ELSE
         IF g_ar_fondo_ahorro[v_c].importe_viv72 = 0 THEN 
            LET g_ar_fondo_ahorro[v_c].importe_viv72 = NULL
         END IF  
       END IF  
    END FOR 

   OPEN WINDOW w_consulta_fondo_ahorro WITH FORM "RETF022"
   DISPLAY ARRAY  g_ar_fondo_ahorro TO t_fondo_ahorro.*
   ATTRIBUTE (ACCEPT = FALSE ,CANCEL  = FALSE )

    BEFORE DISPLAY 
     DISPLAY "Retiro Fondo Ahorro" TO ff_desc_consul
     DISPLAY "Total Registros: "||p_cont -2 TO ff_cont_reg
     DISPLAY v_total_pesos TO ff_total

     IF p_cont <= 0 THEN
       CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
       EXIT DISPLAY  
     END IF 

   ON ACTION regresar 
      EXIT DISPLAY 

   END DISPLAY 
   CLOSE WINDOW w_consulta_fondo_ahorro
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
                    LET v_pesos_total                = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos 
                    LET g_ar_preliquida[v_cont+1].*  = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL 
                    LET g_ar_preliquida[v_cont].nss  = "Movimiento - ",g_ar_preliquida[v_cont+1].movimiento
                    LET v_bnd_movimiento             = g_ar_preliquida[v_cont+1].movimiento
                    LET v_cont                       = v_cont + 2
                  ELSE
                    LET g_ar_preliquida[v_cont+2].*          = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL
                    LET g_ar_preliquida[v_cont].nss          = "Total"
                    LET g_ar_preliquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total = 0
                    INITIALIZE g_ar_preliquida[v_cont+1].* TO NULL 
                    LET g_ar_preliquida[v_cont+1].nss        = "Movimiento - ",g_ar_preliquida[v_cont+2].movimiento
                    LET v_bnd_movimiento                     = g_ar_preliquida[v_cont+2].movimiento
                    LET v_pesos_total                        = v_pesos_total + g_ar_preliquida[v_cont+2].monto_pesos
                    LET v_cont                               = v_cont + 3                    
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
                    LET v_pesos_total                = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos 
                    LET g_ar_preliquida[v_cont+1].*  = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL 
                    LET g_ar_preliquida[v_cont].nss  = "Movimiento - ",g_ar_preliquida[v_cont+1].movimiento
                    LET v_bnd_movimiento             = g_ar_preliquida[v_cont+1].movimiento
                    LET v_cont                       = v_cont + 2
                  ELSE
                    LET g_ar_preliquida[v_cont+2].*          = g_ar_preliquida[v_cont].*
                    INITIALIZE g_ar_preliquida[v_cont].* TO NULL
                    LET g_ar_preliquida[v_cont].nss          = "Total"
                    LET g_ar_preliquida[v_cont].monto_pesos  = v_pesos_total
                    LET v_pesos_total                        = 0
                    INITIALIZE g_ar_preliquida[v_cont+1].* TO NULL 
                    LET g_ar_preliquida[v_cont+1].nss        = "Movimiento - ",g_ar_preliquida[v_cont+2].movimiento
                    LET v_bnd_movimiento                     = g_ar_preliquida[v_cont+2].movimiento
                    LET v_pesos_total                        = v_pesos_total + g_ar_preliquida[v_cont+2].monto_pesos
                    LET v_cont                               = v_cont + 3
                    
                  END IF 
              ELSE
                 LET v_pesos_total = v_pesos_total + g_ar_preliquida[v_cont].monto_pesos
                 LET v_cont        = v_cont + 1 
              END IF               
          END FOREACH
RETURN v_cont ,v_pesos_total
END FUNCTION 

FUNCTION fn_preliquida_modulo_2(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida72.importe

 LET v_cont = 1
 LET v_pesos_total = 0

      FOREACH cur_preliquida USING p_con_ini , p_con_fin
                                 , v_nss ,v_nss, v_nss 
                                 ,v_folio,v_folio,v_folio 
                              INTO g_ar_preliquida[v_cont].*
          DISPLAY  v_cont                     
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

FUNCTION fn_liquida_modulo_0(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           SMALLINT
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE cta_fondo72.importe
 LET v_cont = 1
 LET v_pesos_total = 0

        FOREACH cur_liquida USING p_con_ini , p_con_fin
                                   , v_id_derechohabiente ,v_id_derechohabiente
                                   , v_id_derechohabiente ,v_folio,v_folio,v_folio 
                                   , p_con_ini , p_con_fin
                                   , v_nss ,v_nss
                                   , v_nss ,v_folio,v_folio,v_folio
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
                                 , v_nss ,v_nss, v_nss 
                                 ,v_folio,v_folio,v_folio 
                              INTO g_ar_liquida[v_cont].*
          DISPLAY  v_cont                     
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
