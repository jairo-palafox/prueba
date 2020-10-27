--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC04                                                                 #
#OBJETIVO     => PROGRAMA DE CONSULTA GENERAL TRANSFERENCIA                                #

#Fecha inicio => Octubre 31, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETX00.4gl"

MAIN 
   DEFINE cb_tpo_retiro         ui.ComboBox
   DEFINE cb_estado             ui.ComboBox
   DEFINE cb_rechazo            ui.ComboBox
   DEFINE cb_modalidad          ui.ComboBox
   DEFINE v_nss_id              CHAR(18)
   DEFINE cb_ind_consistencia   ui.ComboBox  
   
   
   DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
         ,p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa
         ,p_s_titulo            STRING   -- titulo de la ventana 
   
   
   DEFINE v_cont                SMALLINT
   DEFINE v_c                   SMALLINT 
                                
   DEFINE t_totalgral_pesos     DECIMAL(19,6)
   DEFINE t_totalgral_pesos49   DECIMAL(19,6)
   DEFINE t_totalgral_aivs92    DECIMAL(19,6)
   DEFINE t_totalgral_aivs97    DECIMAL(19,6)
   
   DEFINE r_total_pesos         DECIMAL(19,6)
   DEFINE r_total_avis92        DECIMAL(19,6)
   DEFINE r_total_avis97        DECIMAL(19,6)
   
   DEFINE ar_ret_modalidad_retiro 
             RECORD LIKE ret_modalidad_retiro.*
             
   DEFINE ar_ret_tipo_retiro
             RECORD LIKE ret_tipo_retiro.*
   
   DEFINE ar_ret_afi_decreto
             RECORD 
              ind_consistencia SMALLINT 
             END RECORD 
      
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
      OPEN WINDOW w_consulta WITH FORM "RETC030"
      
      --LET cb_modalidad         = ui.ComboBox.forName("formonly.cb_modalidad") 
      LET cb_tpo_retiro        = ui.ComboBox.forName("formonly.cb_tpo_retiro")
      LET cb_estado            = ui.ComboBox.forName("formonly.cb_estado")
      LET cb_rechazo           = ui.ComboBox.forName("formonly.cb_rechazo")
      LET cb_ind_consistencia  = ui.ComboBox.forName("formonly.cb_ind_consistencia")
      
      --CALL cb_modalidad.clear()
      CALL cb_tpo_retiro.clear()
      CALL cb_estado.clear()
      CALL cb_rechazo.clear()
      CALL cb_ind_consistencia.clear()
      
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      
      INPUT v_tabla,
            v_con_ini,
            v_con_fin,
            --v_modalidad,
            v_tpo_retiro,
            v_estado,
            v_rechazo,
            v_nss,
            v_id_derechohabiente,
            v_ind_consistencia,
            v_folio
           FROM  rg_estados,
                 d_ini,
                 d_fin,
                -- cb_modalidad,
                 cb_tpo_retiro,
                 cb_estado,
                 cb_rechazo,
                 e_nss,
                 e_dere,
                 cb_ind_consistencia,
                 e_folio
           ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE  )
      
         BEFORE INPUT 
         	LET v_modalidad = 4 
               --LET v_modalidad = 0
               --IF v_modalidad = 4 THEN
               --     CALL  f.setElementHidden("formonly.cb_ind_consistencia",0)
               --     CALL  f.setElementHidden("lb_ind_consistencia",0)
               --ELSE 
               --     CALL  f.setElementHidden("formonly.cb_ind_consistencia",1)
               --     CALL  f.setElementHidden("lb_ind_consistencia",1)
               --END IF 
         
              CALL cb_ind_consistencia.clear()
              DECLARE  c_cb_ind_consistencia CURSOR FOR  
                                                 SELECT ind_consistencia
                                                   FROM afi_decreto
                                                   WHERE NOT ind_consistencia IS NULL 
                                                  GROUP BY ind_consistencia
                                                  
                
                CALL cb_ind_consistencia.addItem('0' ,"TODOS")
                FOREACH c_cb_ind_consistencia INTO ar_ret_afi_decreto.ind_consistencia
                 CALL cb_ind_consistencia.addItem(ar_ret_afi_decreto.ind_consistencia ,ar_ret_afi_decreto.ind_consistencia)
                END FOREACH
               --INITIALIZE ar_ret_afi_decreto.ind_consistencia TO NULL
               
               CALL cb_tpo_retiro.clear()
               DECLARE  c_cb_tpo_retiro CURSOR FOR  
                                            SELECT tpo_retiro, des_corta
                                              FROM ret_tipo_retiro
                                              WHERE (modalidad_retiro = v_modalidad 
                                                 OR  0 = v_modalidad) 
                                                 ORDER BY tpo_retiro
                LET v_c = 1
                --DISPLAY v_modalidad
                CALL cb_tpo_retiro.addItem('0' ,"TODOS")
                FOREACH c_cb_tpo_retiro INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
                 CALL cb_tpo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
                END FOREACH
               INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL  
                LET v_tpo_retiro = 0 
               
              --DECLARE  c_cb_modalidad CURSOR FOR  
                                         --SELECT * 
                                           --FROM ret_modalidad_retiro
                                           --where modalidad = 5
                                           --ORDER BY modalidad_retiro
         
               --CALL cb_modalidad.addItem(0 ,"TODOS") 
               --FOREACH c_cb_modalidad INTO ar_ret_modalidad_retiro.*
                  --CALL cb_modalidad.addItem(ar_ret_modalidad_retiro.modalidad_retiro ,ar_ret_modalidad_retiro.modalidad_retiro||" - "||ar_ret_modalidad_retiro.des_corta)
               --END FOREACH 
               --INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
               
              DECLARE  c_cb_estado CURSOR FOR  
                                         SELECT * 
                                           FROM ret_estado_solicitud
                                           ORDER BY estado_solicitud 
         
               CALL cb_estado.addItem(0 ,"TODOS") 
               FOREACH c_cb_estado INTO ar_ret_estado_solicitud.*
                  CALL cb_estado.addItem(ar_ret_estado_solicitud.estado_solicitud ,ar_ret_estado_solicitud.estado_solicitud||" - "||ar_ret_estado_solicitud.des_corta)
               END FOREACH
               INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
               
              DECLARE  c_cb_rechazo CURSOR FOR  
                                         SELECT * 
                                           FROM ret_rechazo
                                           ORDER BY cod_rechazo 
         
               CALL cb_rechazo.addItem(0 ,"TODOS")                            
               FOREACH c_cb_rechazo INTO ar_ret_rechazo.*
                  CALL cb_rechazo.addItem(ar_ret_rechazo.cod_rechazo,ar_ret_rechazo.cod_rechazo||" - "||ar_ret_rechazo.des_corta)
               END FOREACH
              INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
         
               --LET v_modalidad           = 0  --tipos de retiro
               LET v_tabla               = 1  --solicitud - peliquidacion -liquidacion
               LET v_estado              = 0  --estado de la solicitud
               LET v_rechazo             = 0  --codigo de rechazo
               LET v_ind_consistencia    = 0  --indicador consistencia
               LET v_con_ini    = TODAY
               LET v_con_fin    = TODAY
               LET t_totalgral_pesos = 0
               LET t_totalgral_pesos49 = 0
               LET t_totalgral_aivs92 = 0
               LET t_totalgral_aivs97 = 0       
               
     
         ON CHANGE rg_estados
           IF v_tabla = 1 THEN
              CALL  f.setElementHidden("lb_estado",0)
              CALL  f.setElementHidden("lb_rechazo",0)
              CALL  f.setElementHidden("formonly.cb_rechazo",0)
              CALL  f.setElementHidden("formonly.cb_estado",0)            
              
              IF v_modalidad = 4 THEN
                 CALL  f.setElementHidden("formonly.cb_ind_consistencia",0)
                 CALL  f.setElementHidden("lb_ind_consistencia",0)
                 
              ELSE 
                 CALL  f.setElementHidden("formonly.cb_ind_consistencia",1)
                 CALL  f.setElementHidden("lb_ind_consistencia",1)
              END IF           
           ELSE
              CALL  f.setElementHidden("lb_estado",1)
              CALL  f.setElementHidden("lb_rechazo",1) 
              CALL  f.setElementHidden("formonly.cb_rechazo",1)
              CALL  f.setElementHidden("formonly.cb_estado",1) 
              CALL  f.setElementHidden("formonly.cb_ind_consistencia",1)    
              CALL  f.setElementHidden("lb_ind_consistencia",1)    
           END IF 
           
         ON ACTION consultar
            IF v_nss IS NOT NULL THEN     
                IF v_id_derechohabiente IS NULL THEN  
                	   IF  v_modalidad = 5 then                	   	
                      {IF v_modalidad = 1 OR v_modalidad = 3 OR v_modalidad = 5 OR 
                      v_modalidad = 6 OR v_modalidad = 7  THEN  }
                      SELECT  id_derechohabiente
                        INTO  v_id_derechohabiente
                        FROM  afi_derechohabiente
                       WHERE  nss = v_nss
                    END IF 
                    
                    --IF v_modalidad = 0 THEN 
                    --  SELECT  id_derechohabiente
                    --    INTO  v_id_derechohabiente
                    --    FROM  afi_derechohabiente
                    --   WHERE  nss = v_nss
                    --
                    --   IF v_id_derechohabiente IS NULL THEN
                    --     SELECT  id_afi_fondo72
                    --       INTO  v_id_derechohabiente
                    --       FROM  afi_fondo72
                    --      WHERE  nss = v_nss 
                    --   END IF
                    --
                    --  IF v_id_derechohabiente IS NULL THEN
                    --     SELECT  id_decreto
                    --       INTO  v_id_derechohabiente
                    --       FROM  afi_decreto
                    --      WHERE  nss = v_nss
                    --   END IF
                    --END IF 
            
                    --IF v_modalidad = 2 THEN  
                    --  SELECT  id_afi_fondo72
                    --    INTO  v_id_derechohabiente
                    --    FROM  afi_fondo72
                    --   WHERE  nss = v_nss
                    --END IF
                    --
                    --IF v_modalidad = 4 THEN
                    --  SELECT  id_decreto
                    --    INTO  v_id_derechohabiente
                    --    FROM  afi_decreto
                    --   WHERE  nss = v_nss
                    --END IF              
                ELSE 
                	  IF  v_modalidad = 5 then 
                      --IF v_modalidad = 1 OR v_modalidad = 3 OR v_modalidad = 5 OR 
                      --   v_modalidad = 6 OR v_modalidad = 7  THEN                  
                      SELECT  id_derechohabiente
                        INTO  v_nss_id
                        FROM  afi_derechohabiente
                       WHERE  nss = v_nss
                   END IF
                   
                   --IF v_modalidad = 0 THEN
                   --   SELECT  id_derechohabiente
                   --     INTO  v_nss_id
                   --     FROM  afi_derechohabiente
                   --    WHERE  nss = v_nss
                   --
                   --    IF v_nss_id IS NULL THEN
                   --        SELECT  id_afi_fondo72
                   --          INTO  v_nss_id
                   --          FROM  afi_fondo72
                   --         WHERE  nss = v_nss 
                   --    END IF
                   --    IF v_nss_id IS NULL THEN
                   --        SELECT  id_decreto
                   --          INTO  v_nss_id
                   --          FROM  afi_decreto
                   --         WHERE  nss = v_nss 
                   --    END IF 
                   --END IF 
            
                  --IF v_modalidad = 2 THEN 
                  --    SELECT  id_afi_fondo72
                  --      INTO  v_nss_id
                  --      FROM  afi_fondo72
                  --     WHERE  nss = v_nss
                  -- END IF  
            
                   --IF v_modalidad = 4 THEN 
                   --   SELECT  id_decreto
                   --     INTO  v_nss_id
                   --     FROM  afi_decreto
                   --    WHERE  nss = v_nss
                   --END IF 
                       
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
                  LET t_totalgral_pesos    = 0
                  LET t_totalgral_pesos49  = 0
                  LET t_totalgral_aivs92   = 0
                  LET t_totalgral_aivs97   = 0
               
                  --solo infonavit
               --IF v_modalidad = 1 OR v_modalidad = 0  THEN 
               --    LET r_total_pesos  = 0 
               --    LET r_total_avis92 = 0 
               --    LET r_total_avis97 = 0
               --
               --   CALL fn_solicitud_solo_infonavit(v_estado, v_rechazo ,v_con_ini , v_con_fin) 
               --   RETURNING r_total_pesos , r_total_avis97
               --  -- LET t_totalgral_pesos  = t_totalgral_pesos + r_total_pesos
               --  -- LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --   LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
               --END IF
               
                  --fondo-ahorro
               --IF v_modalidad = 2 OR v_modalidad = 0 THEN
               --    LET r_total_avis92 = 0 
               --    LET r_total_avis97 = 0 
               --    LET r_total_pesos  = 0 
               --   CALL fn_solicitud_fondo_ahorro(v_estado, v_rechazo ,v_con_ini , v_con_fin)
               --    RETURNING r_total_pesos  
               --    
               --   LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               --   --LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --   --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
               --END IF
               
                  --ley 73
               --IF v_modalidad = 3 OR v_modalidad = 0 THEN
               --     LET r_total_avis92 = 0 
               --     LET r_total_avis97 = 0 
               --     LET r_total_pesos = 0
               --    CALL fn_solicitud_ley73(v_estado, v_rechazo ,v_con_ini , v_con_fin)
               --     RETURNING r_total_pesos,r_total_avis92,r_total_avis97
               --
               --    LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               --    LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --    LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
               --END IF
            
                  --tipo_n
               IF v_modalidad = 4 OR v_modalidad = 0 THEN
                   LET r_total_avis92 = 0 
                   LET r_total_avis97 = 0 
                   LET r_total_pesos = 0
                  CALL fn_solicitud_tipo_n(v_estado, v_rechazo ,v_con_ini , v_con_fin)
                   RETURNING r_total_pesos,r_total_avis92
               
                  LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
                  LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
                  --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
               END IF
                   --disposicion
               --IF v_modalidad = 5 OR v_modalidad = 0 THEN
                   --LET r_total_avis92 = 0 
                   --LET r_total_avis97 = 0 
                   --LET r_total_pesos  = 0 
                   --CALL fn_solicitud_disposicion(v_estado, v_rechazo ,v_con_ini , v_con_fin)            
                       --RETURNING r_total_pesos, r_total_avis92, r_total_avis97
                   --
                  --LET t_totalgral_pesos  = t_totalgral_pesos  + r_total_pesos
                  --LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
                  --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
               --END IF
                  --transferencia
               --IF v_modalidad = 6 OR v_modalidad = 0 THEN
               --    LET r_total_avis92 = 0 
               --    LET r_total_avis97 = 0 
               --    LET r_total_pesos = 0 
               --   CALL fn_solicitud_transferencia(v_estado, v_rechazo ,v_con_ini , v_con_fin)
               --    RETURNING r_total_pesos, r_total_avis97
               --   LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               --   LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --   LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
               --END IF
               --
               --   --fortalecimiento
               --IF v_modalidad = 7 OR v_modalidad = 0 THEN
               --    LET r_total_avis92 = 0 
               --    LET r_total_avis97 = 0 
               --    LET r_total_pesos = 0 
               --   CALL fn_solicitud_fortalecimiento(v_estado, v_rechazo ,v_con_ini , v_con_fin)
               --    RETURNING r_total_pesos
               --    --RETURNING r_total_pesos, r_total_avis97
               --   --LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               --   LET t_totalgral_pesos49 = t_totalgral_pesos49 + r_total_pesos
               --   --LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --   --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
               --END IF
            
               -- muestra el resumen de modalidades si existen modlos distintos 
               --agregar los registros encontrados por retiro con criterios de la solicitud .
               CALL fn_consulta_modalidad(t_totalgral_pesos,t_totalgral_pesos49, t_totalgral_aivs92, t_totalgral_aivs97) 
               
              --WHEN 2     --preliquidacion
              --   CALL fn_preliquidacion(v_con_ini , v_con_fin)
              --
              --WHEN 3     --liquidacion
              --   CALL fn_liquidacion(v_con_ini , v_con_fin)   
                 
            END CASE 
         AFTER FIELD e_folio
           NEXT FIELD e_nss  
      
         AFTER FIELD d_ini
           IF v_con_ini IS NULL THEN
             CALL fn_mensaje("Aviso","El valor del campo no debe ser nulo","exclamation")
             LET v_con_ini = TODAY 
             NEXT FIELD d_ini
           END IF
      
         AFTER FIELD d_fin
           IF v_con_fin IS NULL THEN
             CALL fn_mensaje("Aviso","El valor del campo no debe ser nulo","exclamation")
             LET v_con_fin = TODAY 
             NEXT FIELD d_fin
         END IF         
           
         ON ACTION cancelar 
          IF cb_tpo_retiro IS NULL THEN
            CALL fn_mensaje("Aviso","El tipo retiro no puede ser nulo","exclamation")       
            EXIT PROGRAM
          END IF
          EXIT INPUT
      
      END INPUT
      CLOSE WINDOW w_consulta
END MAIN 

--pantalla del resumen general por modalidad
FUNCTION fn_consulta_modalidad(p_total_pesos,p_total_pesos49,p_total_aivs92,p_total_aivs97)
   DEFINE p_total_pesos  DECIMAL(19,6)
   DEFINE p_total_pesos49  DECIMAL(19,6)
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
    DISPLAY p_total_pesos    TO ff_total
    DISPLAY p_total_pesos49  TO ff_total3
    DISPLAY p_total_aivs92   TO ff_total1
    DISPLAY p_total_aivs97   TO ff_total2

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
                   -- WHEN 1
                   --  --solo infonavit  
                   --   CALL fn_consulta_solo_infonavit(g_ar_solo_infonavit.getLength())
                   --   
                   -- WHEN 2
                   -- --fondo_ahorro 
                   --   CALL fn_consulta_fondo_ahorro(g_ar_fondo_ahorro.getLength())
                   --   
                   -- WHEN 3
                   --  --Ley73
                   --   CALL fn_consulta_ley73(g_ar_ley73.getLength())
                   --   
                   -- WHEN 4 
                   -- --tipo_n
                   --   CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)
                   --
                   WHEN 5
                   --disposicion 
                     CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)
                     
                   --WHEN 6
                   ----transferencia
                   --  CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)
                   
                   --WHEN 7
                   -- --fortalecimiento
                   --   CALL fn_consulta_fortalecimiento(g_ar_fortalecimiento.getLength())
                
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

--funcion genera consulta de retiros por modalidad 
--para seleccion multiple en los casos de tipo n, disposicion y transferencia 
FUNCTION fn_consulta_retiro(id_tipo_retiro)
   DEFINE p_total_pesos     DECIMAL(14,6)
   DEFINE p_total_pesos49   DECIMAL(14,6)
   DEFINE p_total_aivs92    DECIMAL(14,6)
   DEFINE p_total_aivs97    DECIMAL(14,6)
   DEFINE v_c               SMALLINT 
   DEFINE v_cont_ar         SMALLINT
   DEFINE v_cu              SMALLINT
   DEFINE v_bnd_tipo_retiro CHAR(1)
   DEFINE id_tipo_retiro    SMALLINT 
   DEFINE v_count_retiro    SMALLINT
   DEFINE v_id_retiro       SMALLINT  
   DEFINE v_query           CHAR(200) 
   
   CREATE TEMP TABLE tmp_retiro(id                  SMALLINT     ,      --modulo
                                desc_tipo_retiro    VARCHAR(50)  ,
                                movimiento          SMALLINT     ,
                                importe72           DECIMAL(14,6),
                                importe49           DECIMAL(14,6),
                                aivs92              DECIMAL(14,6),
                                aivs97              DECIMAL(14,6),
                                tipo_retiro         CHAR(1),
                                id_matriz_derecho   SMALLINT)

   LET v_cu = FALSE 
   LET v_id_retiro = 0

   LET v_query =  " SELECT COUNT(UNIQUE tpo_retiro) ",
                  "\n FROM ret_matriz_derecho ",
                  "\n WHERE (modalidad_retiro = ",id_tipo_retiro,
                  "\n OR '0' = ",id_tipo_retiro,")"
                  
   PREPARE  c_cuenta_tipo_retiros FROM v_query
   EXECUTE c_cuenta_tipo_retiros INTO  v_count_retiro   
   DECLARE c_ret_matriz_derecho CURSOR FOR 
                                       SELECT id_ret_matriz_derecho,tpo_retiro,modalidad_retiro
                                        FROM ret_matriz_derecho
                                       WHERE (modalidad_retiro = id_tipo_retiro
                                          OR '0' = id_tipo_retiro)
                                         AND (tpo_retiro = v_tpo_retiro 
                                          OR "0" = v_tpo_retiro)
   LET v_cont_ar = 1  
   LET v_bnd_tipo_retiro = "Ñ"   
   
   FOREACH c_ret_matriz_derecho INTO g_ar_retiro[v_cont_ar].id_matriz_derecho,g_ar_retiro[v_cont_ar].tipo_retiro,g_ar_retiro[v_cont_ar].id
      DECLARE c_ret_tipo_retiro CURSOR FOR 
                                     SELECT des_corta 
                                       FROM ret_tipo_retiro
                                      WHERE (modalidad_retiro = g_ar_retiro[v_cont_ar].id
                                         OR '0' = id_tipo_retiro)
                                        AND tpo_retiro = g_ar_retiro[v_cont_ar].tipo_retiro
                                     
      FOREACH c_ret_tipo_retiro INTO g_ar_retiro[v_cont_ar].desc_tipo_retiro
        LET g_ar_retiro[v_cont_ar].desc_tipo_retiro = g_ar_retiro[v_cont_ar].tipo_retiro,"-",g_ar_retiro[v_cont_ar].desc_tipo_retiro
      END FOREACH
      
      --IF id_tipo_retiro = 4 OR id_tipo_retiro = 0 THEN 
      --   FOR v_c = 1 TO g_ar_tipo_n.getLength()
      --      IF g_ar_tipo_n[v_c].id_ret_matriz_derecho = g_ar_retiro[v_cont_ar].id_matriz_derecho THEN
      --
      --          INSERT INTO tmp_retiro VALUES (0, --g_ar_retiro[v_cont_ar].id,
      --                                         g_ar_retiro[v_cont_ar].desc_tipo_retiro,
      --                                         202,
      --                                         0,                                    --importe72
      --                                         0,
      --                                         g_ar_tipo_n[v_c].aivs_viv92,          --aivs92
      --                                         0,                                    --aivs97
      --                                         g_ar_retiro[v_cont_ar].tipo_retiro,
      --                                         g_ar_tipo_n[v_c].id_ret_matriz_derecho)
      --     END IF 
      --   END FOR
      --END IF
      
      IF id_tipo_retiro = 5 OR id_tipo_retiro = 0 THEN 
         FOR v_c = 1 TO g_ar_disposicion.getLength()
            IF g_ar_disposicion[v_c].id_ret_matriz_derecho = g_ar_retiro[v_cont_ar].id_matriz_derecho THEN
                INSERT INTO tmp_retiro VALUES (0, --g_ar_retiro[v_cont_ar].id,
                                               g_ar_retiro[v_cont_ar].desc_tipo_retiro,
                                               212,
                                               g_ar_disposicion[v_c].importe_viv72,
                                               0,
                                               g_ar_disposicion[v_c].importe_viv92, 
                                               g_ar_disposicion[v_c].importe_viv97,
                                               g_ar_retiro[v_cont_ar].tipo_retiro,
                                               g_ar_disposicion[v_c].id_ret_matriz_derecho)
            END IF 
         END FOR 
      END IF
      
      --IF id_tipo_retiro = 6 OR id_tipo_retiro = 0 THEN 
      --   FOR v_c = 1 TO g_ar_transferencia.getLength()
      --      IF g_ar_transferencia[v_c].id_ret_matriz_derecho = g_ar_retiro[v_cont_ar].id_matriz_derecho THEN
      --          INSERT INTO tmp_retiro VALUES (0, --g_ar_retiro[v_cont_ar].id,
      --                                         g_ar_retiro[v_cont_ar].desc_tipo_retiro,
      --                                         222,
      --                                         0,
      --                                         0,
      --                                         0,
      --                                         g_ar_transferencia[v_c].aivs_viv97,
      --                                         g_ar_retiro[v_cont_ar].tipo_retiro,
      --                                         g_ar_transferencia[v_c].id_ret_matriz_derecho)
      --     END IF 
      --   END FOR
      --END IF 
      
      CLOSE c_ret_tipo_retiro
      FREE c_ret_tipo_retiro
      LET v_cont_ar = v_cont_ar + 1 
   END FOREACH                     
   DECLARE c_retiro CURSOR FOR SELECT 0,
                                      desc_tipo_retiro,
                                      SUM(importe72),
                                      SUM(importe49),
                                      SUM(aivs92),
                                      SUM(aivs97),
                                      tipo_retiro,
                                      movimiento
                           FROM tmp_retiro
                           GROUP BY tipo_retiro,desc_tipo_retiro,movimiento
                           ORDER BY tipo_retiro,desc_tipo_retiro,movimiento
                           
    CALL g_ar_retiro.clear()
    LET v_cont_ar       = 1
    LET p_total_pesos   = 0.0
    LET p_total_pesos49 = 0.0
    LET p_total_aivs92  = 0.0
    LET p_total_aivs97  = 0.0
    
   FOREACH c_retiro INTO g_ar_retiro[v_cont_ar].id,
                         g_ar_retiro[v_cont_ar].desc_tipo_retiro,
                         g_ar_retiro[v_cont_ar].importe72,
                         g_ar_retiro[v_cont_ar].importe49,
                         g_ar_retiro[v_cont_ar].aivs92,
                         g_ar_retiro[v_cont_ar].aivs97,
                         g_ar_retiro[v_cont_ar].tipo_retiro ,
                         g_ar_retiro[v_cont_ar].movimiento
                         
      IF v_count_retiro = 1 THEN 
         LET g_ar_retiro[v_cont_ar].id = 1
      ELSE 
         LET g_ar_retiro[v_cont_ar].id = 0
      END IF
      LET p_total_pesos    = p_total_pesos   + g_ar_retiro[v_cont_ar].importe72
      LET p_total_pesos49  = p_total_pesos49 + g_ar_retiro[v_cont_ar].importe49
      LET p_total_aivs92   = p_total_aivs92  + g_ar_retiro[v_cont_ar].aivs92
      LET p_total_aivs97   = p_total_aivs97  + g_ar_retiro[v_cont_ar].aivs97
      --DISPLAY p_total_pesos ,g_ar_retiro[v_cont_ar].importe
      LET v_cont_ar = v_cont_ar +1
           
   END FOREACH 
   CALL g_ar_retiro.deleteElement(v_cont_ar)
   LET v_cont_ar = v_cont_ar - 1  
 
   OPEN WINDOW w_consulta_retiro WITH FORM "RETF029"
   
   INPUT ARRAY  g_ar_retiro FROM t_retiro.*
      ATTRIBUTE (UNBUFFERED ,WITHOUT DEFAULTS ,APPEND ROW = FALSE 
                ,DELETE ROW = FALSE , INSERT ROW = FALSE ,ACCEPT = FALSE,CANCEL = FALSE)
   
   BEFORE INPUT 
      IF v_count_retiro = 1 THEN 
         CALL fn_consulta_combinado_retiro(id_tipo_retiro)
         EXIT INPUT 
      END IF  
      
         DISPLAY p_total_pesos    TO ff_total
         DISPLAY p_total_pesos49  TO ff_total3
         DISPLAY p_total_aivs92   TO ff_total1
         DISPLAY p_total_aivs97   TO ff_total2
      IF v_cont_ar <= 0 THEN
         EXIT INPUT 
      END IF 

   ON ACTION recdet     
      IF v_cont_ar > 0 THEN  
         CALL  fn_consulta_combinado_retiro(id_tipo_retiro)
      END IF

   ON ACTION Regresar
      EXIT INPUT

   END INPUT
   CLOSE WINDOW w_consulta_retiro
   CALL g_ar_retiro.clear()
   DROP TABLE tmp_retiro
END FUNCTION

FUNCTION fn_consulta_combinado_retiro(id_tipo_retiro)
   DEFINE p_cont         SMALLINT
   DEFINE v_c            SMALLINT
   DEFINE v_cont         SMALLINT
   DEFINE v_cont_ctrl    SMALLINT
   DEFINE v_total_pesos  DECIMAL(14,6)
   DEFINE v_total_pesos49  DECIMAL(14,6)
   DEFINE v_total_aivs92 DECIMAL(14,6)
   DEFINE v_total_aivs97 DECIMAL(14,6)
   DEFINE id_tipo_retiro SMALLINT
   DEFINE re_tmp_retiro RECORD 
                    id                  SMALLINT,
                    desc_tipo_retiro    VARCHAR(50),
                    movimiento          SMALLINT,
                    importe72           DECIMAL(14,6),
                    importe49           DECIMAL(14,6),
                    aivs92              DECIMAL(14,6),
                    aivs97              DECIMAL(14,6),
                    tipo_retiro         CHAR(1),
                    id_matriz_derecho   SMALLINT
                END RECORD 
  DEFINE v_ya_paso                      SMALLINT
  DEFINE v_cont_1                       SMALLINT
  DEFINE v_contx                        SMALLINT
  DEFINE v_cont_control                 SMALLINT
  DEFINE v_encabezado                   SMALLINT
  DEFINE v_cont_mostrar                 SMALLINT
  DEFINE v_totales                      SMALLINT
  DEFINE v_ya_nss_paso                  CHAR(20)
  DEFINE v_desc_encabezados             CHAR(30) 

   LET p_cont             = 0
   LET v_cont_ctrl        = 1
   LET v_cont_1           = 1
   LET v_cont_mostrar     = 0
   LET v_ya_paso          = 0
   LET v_ya_nss_paso      = 0
   LET v_totales          = FALSE
   LET v_total_pesos      = 0
   LET v_total_pesos49    = 0
   LET v_total_aivs92     = 0
   LET v_total_aivs97     = 0
   LET v_desc_encabezados = ""

   FOR v_cont_control = 1 TO g_ar_retiro.getLength()
      IF g_ar_retiro[v_cont_control].id = 1 THEN

         ----  WHEN 4 tipo_n
         --LET v_encabezado = TRUE    
         --FOR v_cont = 1 TO g_ar_tipo_n.getLength()
         --   DECLARE c_tmp_retiro CURSOR FOR 
         --                            SELECT *
         --                              FROM tmp_retiro
         --                             WHERE tipo_retiro = g_ar_retiro[v_cont_control].tipo_retiro
         --                             ORDER BY 1
         --                             
         --   --recorre todos los registros comparando los retiros que coinciden con los del corte en turno
         --   FOREACH c_tmp_retiro INTO re_tmp_retiro.*
         --      IF re_tmp_retiro.id_matriz_derecho = g_ar_tipo_n[v_cont].id_ret_matriz_derecho THEN
         --         IF g_ar_tipo_n[v_cont].id_ret_matriz_derecho <> v_ya_paso THEN
         --
         --              --muestra corte con el tipo de retiro 
         --               IF v_encabezado =TRUE THEN
         --                  --muestra totales por tipo de retiro
         --                  IF  v_totales = TRUE THEN
         --                     LET g_ar_combinado[v_cont_ctrl].nss = "Totales "
         --                     LET g_ar_combinado[v_cont_ctrl].pes_viv72  = v_total_pesos
         --                     LET g_ar_combinado[v_cont_ctrl].pes_viv49  = v_total_pesos49
         --                     LET g_ar_combinado[v_cont_ctrl].aivs_viv92 = v_total_aivs92
         --                     LET g_ar_combinado[v_cont_ctrl].aivs_viv97 = v_total_aivs97
         --                     LET v_cont_ctrl = v_cont_ctrl + 1   
         --                     LET v_cont_mostrar = v_cont_mostrar + 1 
         --                     LET v_total_pesos = 0
         --                     LET v_total_pesos49 = 0
         --                     LET v_total_aivs92 = 0
         --                     LET v_total_aivs97 = 0
         --                  END IF 
         --                  
         --                  LET g_ar_combinado[v_cont_ctrl].nss = re_tmp_retiro.desc_tipo_retiro
         --                  LET v_desc_encabezados = re_tmp_retiro.desc_tipo_retiro
         --                  LET v_cont_ctrl = v_cont_ctrl + 1
         --                  LET v_cont_mostrar = v_cont_mostrar + 1
         --                  LET v_encabezado = FALSE
         --                  LET v_totales = TRUE
         --               END IF 
         --               
         --            FOR v_contx = v_cont TO g_ar_tipo_n.getLength()
         --               IF g_ar_tipo_n[v_contx].id_ret_matriz_derecho = g_ar_tipo_n[v_cont].id_ret_matriz_derecho THEN
         --                  IF g_ar_tipo_n[v_cont].id_derechohabiente IS NOT NULL  THEN 
         --                     LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
         --                     LET v_cont_1 = v_cont_1 + 1
         --                  ELSE
         --                     IF g_ar_tipo_n[v_cont].aivs_viv92 = 0 THEN 
         --                         LET g_ar_tipo_n[v_cont].aivs_viv92 = NULL
         --                     END IF  
         --                  END IF  
         --                  --se carga el array para mostrar con los regirtros seleccionados ´para mostrar
         --                  LET g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_tipo_n[v_contx].id_derechohabiente
         --                  LET g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_tipo_n[v_contx].id_solicitud
         --                  LET g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_tipo_n[v_contx].f_solicitud
         --                  LET g_ar_combinado[v_cont_ctrl].nss                = g_ar_tipo_n[v_contx].nss
         --                  LET g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
         --                  LET g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
         --                  LET g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_tipo_n[v_contx].aivs_viv92
         --                  LET g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
         --                  LET g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_tipo_n[v_contx].estado_solicitud
         --                  LET g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_tipo_n[v_contx].cod_rechazo
         --                  LET v_total_pesos  = v_total_pesos  + g_ar_combinado[v_cont_ctrl].pes_viv72
         --                  LET v_total_pesos49  = v_total_pesos49  + g_ar_combinado[v_cont_ctrl].pes_viv49
         --                  LET v_total_aivs92 = v_total_aivs92 + g_ar_combinado[v_cont_ctrl].aivs_viv92
         --                  LET v_total_aivs97 = v_total_aivs97 + g_ar_combinado[v_cont_ctrl].aivs_viv97
         --                  { DISPLAY 'v_total_pesos', ' v_total_aivs92', ' v_total_aivs97'
         --                  DISPLAY v_total_pesos,' ', v_total_aivs92,' ', v_total_aivs97}
         --                  LET v_cont_ctrl = v_cont_ctrl + 1
         --               END IF 
         --            END FOR 
         --        END IF
         --        LET v_ya_paso     = re_tmp_retiro.id_matriz_derecho
         --      END IF
         --   END FOREACH
         --END FOR

         --  WHEN 5 disposicion
         LET v_encabezado = TRUE
         FOR v_cont = 1 TO g_ar_disposicion.getLength()
            DECLARE c_tmp_retiron CURSOR FOR 
                                     SELECT *
                                       FROM tmp_retiro
                                      WHERE tipo_retiro = g_ar_retiro[v_cont_control].tipo_retiro
                                      ORDER BY 1

            --recorre todos los registros comparando los retiros que coinciden con los del corte en turno
            FOREACH c_tmp_retiron INTO re_tmp_retiro.*
               IF re_tmp_retiro.id_matriz_derecho = g_ar_disposicion[v_cont].id_ret_matriz_derecho THEN
                  IF g_ar_disposicion[v_cont].id_ret_matriz_derecho <> v_ya_paso THEN
                     
                     --muestra corte con el tipo de retiro 
                     IF v_encabezado =TRUE THEN
                        --muestra totales por tipo de retiro
                        IF  v_totales = TRUE THEN
                           LET g_ar_combinado[v_cont_ctrl].nss = "Totales "
                           LET g_ar_combinado[v_cont_ctrl].pes_viv72  = v_total_pesos
                           LET g_ar_combinado[v_cont_ctrl].pes_viv49  = v_total_pesos49
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv92 = v_total_aivs92
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv97 = v_total_aivs97
                           LET v_cont_ctrl = v_cont_ctrl + 1   
                           LET v_cont_mostrar = v_cont_mostrar + 1 
                           LET v_total_pesos = 0
                           LET v_total_pesos49 = 0
                           LET v_total_aivs92 = 0
                           LET v_total_aivs97 = 0
                        END IF 

                        LET g_ar_combinado[v_cont_ctrl].nss = re_tmp_retiro.desc_tipo_retiro
                        LET v_desc_encabezados = re_tmp_retiro.desc_tipo_retiro
                        LET v_cont_ctrl = v_cont_ctrl + 1
                        LET v_cont_mostrar = v_cont_mostrar + 1
                        LET v_encabezado = FALSE
                        LET v_totales = TRUE
                     END IF 
                     FOR v_contx = v_cont TO g_ar_disposicion.getLength()
                        IF g_ar_disposicion[v_contx].id_ret_matriz_derecho = g_ar_disposicion[v_cont].id_ret_matriz_derecho THEN
                           IF g_ar_disposicion[v_cont].id_derechohabiente IS NOT NULL  THEN --ERV 
                              LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                              LET v_cont_1 = v_cont_1 + 1
                           ELSE
                              IF g_ar_disposicion[v_cont].importe_viv72 = 0 THEN 
                                  LET g_ar_disposicion[v_cont].importe_viv72 = NULL
                              END IF
                              IF g_ar_disposicion[v_cont].importe_viv92 = 0 THEN 
                                  LET g_ar_disposicion[v_cont].importe_viv92 = NULL
                              END IF  
                              IF g_ar_disposicion[v_cont].importe_viv97 = 0 THEN 
                                  LET g_ar_disposicion[v_cont].importe_viv97 = NULL
                              END IF    
                           END IF  
                           --se carga el array para mostrar con los regirtros seleccionados ´para mostrar
                           LET g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_disposicion[v_contx].id_derechohabiente
                           LET g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_disposicion[v_contx].id_solicitud
                           LET g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_disposicion[v_contx].f_solicitud
                           LET g_ar_combinado[v_cont_ctrl].nss                = g_ar_disposicion[v_contx].nss
                           LET g_ar_combinado[v_cont_ctrl].pes_viv72          = g_ar_disposicion[v_contx].importe_viv72
                           LET g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_disposicion[v_contx].importe_viv92
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_disposicion[v_contx].importe_viv97
                           LET g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_disposicion[v_contx].estado_solicitud
                           LET g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_disposicion[v_contx].cod_rechazo
                           LET v_total_pesos  = v_total_pesos  + g_ar_combinado[v_cont_ctrl].pes_viv72
                           LET v_total_pesos49  = v_total_pesos49  + g_ar_combinado[v_cont_ctrl].pes_viv49
                           LET v_total_aivs92 = v_total_aivs92 + g_ar_combinado[v_cont_ctrl].aivs_viv92
                           LET v_total_aivs97 = v_total_aivs97 + g_ar_combinado[v_cont_ctrl].aivs_viv97
                          { DISPLAY 'v_total_pesos', ' v_total_aivs92', ' v_total_aivs97'
                           DISPLAY v_total_pesos,' ', v_total_aivs92,' ', v_total_aivs97}
                           LET v_cont_ctrl = v_cont_ctrl + 1
                        END IF
                     END FOR 
                  END IF
                 LET v_ya_paso = re_tmp_retiro.id_matriz_derecho
               END IF
            END FOREACH
         END FOR

         --WHEN 6    transferencia 
         LET v_encabezado   = TRUE 
         FOR v_cont = 1 TO g_ar_transferencia.getLength() 
            DECLARE c_tmp_retiroi CURSOR FOR 
                                      SELECT * 
                                        FROM tmp_retiro
                                       WHERE tipo_retiro = g_ar_retiro[v_cont_control].tipo_retiro
                                       ORDER BY id_matriz_derecho

            FOREACH c_tmp_retiroi INTO re_tmp_retiro.*
               IF re_tmp_retiro.id_matriz_derecho = g_ar_transferencia[v_cont].id_ret_matriz_derecho THEN
                  IF g_ar_transferencia[v_cont].id_ret_matriz_derecho <> v_ya_paso THEN
                     IF v_encabezado =TRUE THEN
                        --muestra totales por tipo de retiro
                        IF  v_totales = TRUE THEN
                           LET g_ar_combinado[v_cont_ctrl].nss = "Totales "
                           LET g_ar_combinado[v_cont_ctrl].pes_viv72  = v_total_pesos
                           LET g_ar_combinado[v_cont_ctrl].pes_viv49  = v_total_pesos49
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv92 = v_total_aivs92
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv97 = v_total_aivs97
                           LET v_cont_ctrl      = v_cont_ctrl + 1   
                           LET v_cont_mostrar   = v_cont_mostrar + 1 
                           LET v_total_pesos    = 0
                           LET v_total_pesos49  = 0
                           LET v_total_aivs92   = 0
                           LET v_total_aivs97   = 0
                        END IF 

                        LET g_ar_combinado[v_cont_ctrl].nss = re_tmp_retiro.desc_tipo_retiro
                        LET v_desc_encabezados = re_tmp_retiro.desc_tipo_retiro
                        LET v_cont_ctrl        = v_cont_ctrl    + 1
                        LET v_cont_mostrar     = v_cont_mostrar + 1
                        LET v_encabezado       = FALSE
                        LET v_totales          = TRUE
                     END IF
                     FOR v_contx = v_cont TO g_ar_transferencia.getLength()
                        IF g_ar_transferencia[v_contx].id_ret_matriz_derecho = g_ar_transferencia[v_cont].id_ret_matriz_derecho THEN  
                           IF g_ar_transferencia[v_cont].id_derechohabiente IS NOT NULL  THEN 
                              LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                              LET v_cont_1 = v_cont_1 + 1
                           ELSE
                             IF g_ar_transferencia[v_cont].aivs_viv97 = 0 THEN 
                                LET g_ar_transferencia[v_cont].aivs_viv97 = NULL
                             END IF  
                           END IF
                           LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_transferencia[v_contx].id_derechohabiente
                           LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_transferencia[v_contx].id_solicitud
                           LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_transferencia[v_contx].f_solicitud
                           LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_transferencia[v_contx].nss
                           LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                           LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                           LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                           LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_transferencia[v_contx].aivs_viv97
                           LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_transferencia[v_contx].estado_solicitud
                           LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_transferencia[v_contx].cod_rechazo
                           LET v_total_pesos   = v_total_pesos   + g_ar_combinado[v_cont_ctrl].pes_viv72
                           LET v_total_pesos49 = v_total_pesos49 + g_ar_combinado[v_cont_ctrl].pes_viv49
                           LET v_total_aivs92  = v_total_aivs92  + g_ar_combinado[v_cont_ctrl].aivs_viv92
                           LET v_total_aivs97  = v_total_aivs97  + g_ar_combinado[v_cont_ctrl].aivs_viv97
                           LET v_cont_ctrl     = v_cont_ctrl + 1
                        END IF
                     END FOR 
                  END IF
                  LET v_ya_paso = re_tmp_retiro.id_matriz_derecho
               END IF
            END FOREACH
         END FOR

         IF  v_totales = TRUE THEN 
            LET g_ar_combinado[v_cont_ctrl].nss = "Totales "
            LET g_ar_combinado[v_cont_ctrl].pes_viv72  = v_total_pesos
            LET g_ar_combinado[v_cont_ctrl].pes_viv49  = v_total_pesos49
            LET g_ar_combinado[v_cont_ctrl].aivs_viv92 = v_total_aivs92
            LET g_ar_combinado[v_cont_ctrl].aivs_viv97 = v_total_aivs97
            LET v_cont_ctrl                            = v_cont_ctrl + 1  
            LET v_cont_mostrar                         = v_cont_mostrar + 1 
            LET v_totales                              = FALSE
            LET v_total_pesos                          = 0
            LET v_total_pesos49                        = 0
            LET v_total_aivs92                         = 0
            LET v_total_aivs97                         = 0
            --elimina ultima linea que genera el foreach
         END IF
      END IF 
   END FOR

   LET v_cont_ctrl      = v_cont_ctrl 
   LET v_total_pesos    = 0
   LET v_total_pesos49  = 0
   LET v_total_aivs92   = 0
   LET v_total_aivs97   = 0

   FOR v_c = 1 TO v_cont_ctrl
      IF g_ar_combinado[v_c].id_derechohabiente IS NOT NULL  THEN
         LET v_total_pesos   = v_total_pesos  + g_ar_combinado[v_c].pes_viv72
         LET v_total_pesos49 = v_total_pesos  + g_ar_combinado[v_c].pes_viv49
         LET v_total_aivs92  = v_total_aivs92 + g_ar_combinado[v_c].aivs_viv92
         LET v_total_aivs97  = v_total_aivs97 + g_ar_combinado[v_c].aivs_viv97
      END IF 
   END FOR

   CALL g_ar_combinado.deleteElement(v_cont_ctrl)
   LET v_cont_mostrar = (v_cont_mostrar )

   OPEN WINDOW w_consulta_detalle WITH FORM "RETF026"
   DISPLAY ARRAY  g_ar_combinado TO t_detalle_gral.*
         ATTRIBUTE (ACCEPT = FALSE ,CANCEL = FALSE )
   
    BEFORE DISPLAY
       IF v_cont_mostrar = 2 THEN
          DISPLAY v_desc_encabezados CLIPPED  TO ff_desc_consul
       ELSE
          DISPLAY "Retiro Detalle General" TO ff_desc_consul
       END IF
       
       DISPLAY "Total Registros: "||(v_cont_ctrl - 1 ) - (v_cont_mostrar)   TO ff_cont_reg
       DISPLAY v_total_pesos   TO ff_total
       DISPLAY v_total_pesos49 TO ff_total3
       DISPLAY v_total_aivs92  TO ff_total1
       DISPLAY v_total_aivs97  TO ff_total2
       IF (v_cont_ctrl-1 ) - v_cont_mostrar <= 0 THEN 
          CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
          EXIT DISPLAY  
       END IF 

   ON ACTION Regresar 
      EXIT DISPLAY 

   END DISPLAY 
   CALL g_ar_combinado.clear()
   CLOSE WINDOW w_consulta_detalle

END FUNCTION 

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_disposicion(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini      DATE
 DEFINE p_con_fin      DATE  
 DEFINE p_estado       SMALLINT
 DEFINE p_rechazo      SMALLINT
 DEFINE v_cont         SMALLINT
 DEFINE v_query        STRING 
 DEFINE v_total_pesos  DECIMAL(14,6)
 DEFINE v_total_aivs92 DECIMAL(14,6)
 DEFINE v_total_aivs97 DECIMAL(14,6)
 DEFINE v_total_aivs   DECIMAL(14,6)
 DEFINE v_c            SMALLINT 
 DEFINE v_con_tot      SMALLINT 
 DEFINE v_desc_estado  CHAR(18)
 DEFINE v_desc_rechazo CHAR(18)
 
 CALL g_ar_disposicion.clear( )

    --LET p_estado = 0

    LET v_query =  "\n SELECT rd.id_solicitud,rd.id_derechohabiente,' ',",
                           "\n rcd.f_carga,rd.folio,importe_viv72,rd.aivs_viv92,rd.aivs_viv97,",
                           "\n ' ',rd.estado_solicitud,rd.cod_rechazo,' ',rd.id_ret_matriz_derecho",
                           "\n FROM ret_disposicion  rd,",
                           "\n      ret_cza_disposicion rcd",
                           "\n WHERE (rd.estado_solicitud = ? or '0' = ?)",
                           "\n   AND rd.folio = rcd.folio",
                           "\n   AND (rd.cod_rechazo = ? or '0' = ?)",
                           "\n   AND rcd.f_carga between ? and ?" , 
                           "\n   AND (rd.id_derechohabiente = ?",
                           "\n    or ? is null or ? = '')",
                           "\n   AND (rd.folio = ?",
                           "\n    or ? is null or ? = '')",
                           "\n    order by rd.id_ret_matriz_derecho"
          LET v_cont = 1 
          
          LET g_ar_disposicion[v_cont].nss = "5-disposición"
          LET g_ar_disposicion[v_cont].importe_viv72 = 0
          LET g_ar_disposicion[v_cont].importe_viv92 = 0
          LET g_ar_disposicion[v_cont].importe_viv97 = 0
          LET v_cont = v_cont + 1

         DISPLAY v_query
         DISPLAY 
              p_estado,'- \n'
            , p_estado,'- \n'
            , p_rechazo ,'- \n'
            , p_rechazo,'- \n'
            , p_con_ini ,'- \n'
            , p_con_fin,'- \n'
            , v_id_derechohabiente ,'- \n'
            , v_id_derechohabiente,'- \n'
            , v_id_derechohabiente,'- \n'
            , v_folio,'- \n'
            , v_folio,'- \n'
            , v_folio

          PREPARE pr_disposicion FROM v_query
          DECLARE cur_disposicion CURSOR FOR pr_disposicion
          FOREACH cur_disposicion USING p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin 
                                        ,v_id_derechohabiente ,v_id_derechohabiente, v_id_derechohabiente, 
                                        v_folio, v_folio, v_folio 
                                   INTO g_ar_disposicion[v_cont].*
             SELECT nss
               INTO g_ar_disposicion[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_disposicion[v_cont].id_derechohabiente

              INITIALIZE v_desc_estado TO NULL 
              SELECT  des_corta
                INTO v_desc_estado
                FROM ret_estado_solicitud
               WHERE estado_solicitud = g_ar_disposicion[v_cont].estado_solicitud

              LET g_ar_disposicion[v_cont].estado_solicitud = g_ar_disposicion[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED 

              INITIALIZE v_desc_rechazo TO NULL
              SELECT  des_corta
                INTO v_desc_rechazo
                FROM ret_rechazo
                WHERE cod_rechazo = g_ar_disposicion[v_cont].cod_rechazo

              LET g_ar_disposicion[v_cont].cod_rechazo = g_ar_disposicion[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
              LET v_cont = v_cont + 1
          END FOREACH  
          LET v_total_pesos = 0
          LET v_total_aivs  = 0
          LET v_total_aivs92  = 0
          LET v_total_aivs97  = 0

           FOR v_c = 1 TO v_cont
              IF  g_ar_disposicion[v_c].id_derechohabiente IS NOT NULL THEN
                 LET v_total_pesos  = v_total_pesos  + g_ar_disposicion[v_c].importe_viv72
                 LET v_total_aivs92 = v_total_aivs92 + g_ar_disposicion[v_c].importe_viv92
                 LET v_total_aivs97 = v_total_aivs97 + g_ar_disposicion[v_c].importe_viv97
                 LET v_total_aivs   = v_total_aivs   + g_ar_disposicion[v_c].importe_viv92 + g_ar_disposicion[v_c].importe_viv97
              END IF  
           END FOR 

          IF v_total_pesos > 0 OR v_total_aivs > 0 OR v_cont - 3 > 0 THEN 
             LET v_con_tot  = g_ar_totales.getLength() + 1
             LET g_ar_totales[v_con_tot].id  = 0
             LET g_ar_totales[v_con_tot].id_tipo_retiro = 5
             LET g_ar_totales[v_con_tot].tipo_retiro  = "5 Retiro Disposicion de Recursos"
             LET g_ar_totales[v_con_tot].movimiento   =  212 
             LET g_ar_totales[v_con_tot].importe      = v_total_pesos
             LET g_ar_totales[v_con_tot].importe_49   = 0
             LET g_ar_totales[v_con_tot].aivs92       = v_total_aivs92
             LET g_ar_totales[v_con_tot].aivs97       = v_total_aivs97

             LET g_ar_disposicion[v_cont].nss = "Totales "
             LET g_ar_disposicion[v_cont].importe_viv72 = v_total_pesos
             LET g_ar_disposicion[v_cont].importe_viv92 = v_total_aivs92
             LET g_ar_disposicion[v_cont].importe_viv97 = v_total_aivs97

             DISPLAY  g_ar_disposicion[v_cont].*
             LET v_cont = v_cont + 1
             
             CALL g_ar_disposicion.deleteElement(v_cont)
         END IF
          RETURN v_total_pesos ,v_total_aivs92 ,v_total_aivs97
END FUNCTION

--el input de consulta modalidad
--funcion muestra consulta de modalidad en seleccion multiple para el retiro
FUNCTION fn_consulta_combinado_modulos()
   DEFINE p_cont             SMALLINT
   DEFINE v_c                SMALLINT 
   DEFINE v_cont             SMALLINT 
   DEFINE v_cont_ctrl        SMALLINT 
   DEFINE v_total_pesos      DECIMAL(14,6)
   DEFINE v_total_pesos49    DECIMAL(14,6)
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
                 --WHEN 1                 
                 --  LET v_cont_encabezados = v_cont_encabezados + 1 
                 --  LET p_cont = p_cont + g_ar_solo_infonavit.getLength()
                 --  FOR v_cont = 1 TO g_ar_solo_infonavit.getLength()
                 --     IF g_ar_solo_infonavit[v_cont].id_derechohabiente IS NOT NULL  THEN 
                 --        LET  g_ar_combinado[v_cont].id_tipo_retiro = v_cont_1
                 --        LET  g_ar_combinado[v_cont].pes_viv72          = 0
                 --        LET  g_ar_combinado[v_cont].pes_viv49          = 0
                 --        LET  g_ar_combinado[v_cont].aivs_viv92         = 0 
                 --        LET v_cont_1 = v_cont_1 + 1
                 --     ELSE
                 --        IF g_ar_solo_infonavit[v_cont].pes_viv97 = 0 THEN 
                 --           LET g_ar_solo_infonavit[v_cont].pes_viv97 = NULL
                 --        END IF  
                 --        
                 --        IF g_ar_solo_infonavit[v_cont].acc_viv97 = 0 THEN 
                 --           LET g_ar_solo_infonavit[v_cont].acc_viv97 = NULL
                 --        END IF
                 --     END IF 
                 --     LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_solo_infonavit[v_cont].id_derechohabiente
                 --     LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_solo_infonavit[v_cont].id_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_solo_infonavit[v_cont].f_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_solo_infonavit[v_cont].nss
                 --     LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_solo_infonavit[v_cont].acc_viv97
                 --     LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_solo_infonavit[v_cont].estado_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_solo_infonavit[v_cont].cod_rechazo
                 --     LET v_cont_ctrl = v_cont_ctrl + 1
                 --END FOR
                 --WHEN 2
                 --  LET v_cont_encabezados = v_cont_encabezados +1 
                 --  LET p_cont = p_cont + g_ar_fondo_ahorro.getLength()
                 --  FOR v_cont = 1 TO g_ar_fondo_ahorro.getLength()
                 --     IF g_ar_fondo_ahorro[v_cont].id_derechohabiente IS NOT NULL THEN 
                 --        LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                 --        LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0 
                 --        LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                 --        LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
                 --        LET v_cont_1 = v_cont_1 + 1
                 --     ELSE
                 --        IF g_ar_fondo_ahorro[v_cont].importe_viv72 = 0 THEN
                 --          LET g_ar_fondo_ahorro[v_cont].importe_viv72 = NULL
                 --        END IF
                 --     END IF
                 --     LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_fondo_ahorro[v_cont].id_derechohabiente
                 --     LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_fondo_ahorro[v_cont].id_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_fondo_ahorro[v_cont].f_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_fondo_ahorro[v_cont].nss
                 --     LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = g_ar_fondo_ahorro[v_cont].importe_viv72
                 --     LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_fondo_ahorro[v_cont].estado_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_fondo_ahorro[v_cont].cod_rechazo
                 --     LET v_cont_ctrl = v_cont_ctrl + 1
                 -- END FOR 
                 --WHEN 3
                 --  LET v_cont_encabezados = v_cont_encabezados +1 
                 --  LET p_cont = p_cont + g_ar_ley73.getLength()
                 --  FOR v_cont = 1 TO g_ar_ley73.getLength()
                 --     IF g_ar_ley73[v_cont].id_derechohabiente IS NOT NULL THEN 
                 --        LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                 --        LET v_cont_1 = v_cont_1 + 1
                 --     ELSE
                 --        IF     g_ar_ley73[v_cont].importe_viv92 = 0
                 --           AND g_ar_ley73[v_cont].importe_viv97 = 0 THEN
                 --           
                 --           LET g_ar_ley73[v_cont].importe_viv92 = NULL
                 --           LET g_ar_ley73[v_cont].importe_viv97 = NULL
                 --           LET g_ar_ley73[v_cont].importe_viv92 = NULL
                 --           LET g_ar_ley73[v_cont].importe_viv97 = NULL
                 --        END IF  
                 --     END IF
                 --     --LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont
                 --     LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_ley73[v_cont].id_derechohabiente
                 --     LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_ley73[v_cont].id_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_ley73[v_cont].f_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_ley73[v_cont].nss
                 --     LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                 --     LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                 --     LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_ley73[v_cont].aivs_viv92
                 --     LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_ley73[v_cont].aivs_viv97
                 --     LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_ley73[v_cont].estado_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_ley73[v_cont].cod_rechazo
                 --     LET v_cont_ctrl = v_cont_ctrl + 1
                 --END FOR 
                   
                 --WHEN 4
                 --  LET v_cont_encabezados = v_cont_encabezados +1 
                 --  LET p_cont = p_cont + g_ar_tipo_n.getLength()
                 --  FOR v_cont = 1 TO g_ar_tipo_n.getLength()
                 --     IF g_ar_tipo_n[v_cont].id_derechohabiente IS NOT NULL THEN 
                 --        LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                 --        LET v_cont_1 = v_cont_1 + 1
                 --     ELSE
                 --        IF g_ar_tipo_n[v_cont].aivs_viv92 = 0 THEN 
                 --           LET g_ar_tipo_n[v_cont].aivs_viv92 = NULL
                 --        END IF  
                 --     END IF
                 --     --LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont
                 --     LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_tipo_n[v_cont].id_derechohabiente
                 --     LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_tipo_n[v_cont].id_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_tipo_n[v_cont].f_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_tipo_n[v_cont].nss
                 --     LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                 --     LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                 --     LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_tipo_n[v_cont].aivs_viv92
                 --     LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
                 --     LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_tipo_n[v_cont].estado_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_tipo_n[v_cont].cod_rechazo
                 --     LET v_cont_ctrl = v_cont_ctrl + 1
                 --END FOR 
                 WHEN 5
                 LET v_cont_encabezados = v_cont_encabezados +1 
                   LET p_cont = p_cont + g_ar_disposicion.getLength()
                   FOR v_cont = 1 TO g_ar_disposicion.getLength()
                      IF g_ar_disposicion[v_cont].id_derechohabiente IS NOT NULL THEN 
                         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                         LET v_cont_1 = v_cont_1 + 1
                      ELSE
                         IF g_ar_disposicion[v_cont].importe_viv72 = 0 
                            AND g_ar_disposicion[v_cont].importe_viv92 = 0
                            AND g_ar_disposicion[v_cont].importe_viv97 = 0 THEN
                            
                            LET g_ar_disposicion[v_cont].importe_viv72 = NULL
                            LET g_ar_disposicion[v_cont].importe_viv92 = NULL
                            LET g_ar_disposicion[v_cont].importe_viv97 = NULL
                         END IF  
                      END IF
                      --LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont
                      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_disposicion[v_cont].id_derechohabiente
                      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_disposicion[v_cont].id_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_disposicion[v_cont].f_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_disposicion[v_cont].nss
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = g_ar_disposicion[v_cont].importe_viv72
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_disposicion[v_cont].importe_viv92
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_disposicion[v_cont].importe_viv97
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_disposicion[v_cont].estado_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_disposicion[v_cont].cod_rechazo
                      LET v_cont_ctrl = v_cont_ctrl + 1
                  END FOR 
                 --WHEN 6
                 --  LET v_cont_encabezados = v_cont_encabezados +1 
                 --  LET p_cont = p_cont + g_ar_transferencia.getLength()
                 --  FOR v_cont = 1 TO g_ar_transferencia.getLength()
                 --     IF g_ar_transferencia[v_cont].id_derechohabiente IS NOT NULL THEN 
                 --        LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                 --        LET v_cont_1 = v_cont_1 + 1
                 --     ELSE
                 --        IF g_ar_transferencia[v_cont].aivs_viv97 = 0 THEN 
                 --           LET g_ar_transferencia[v_cont].aivs_viv97 = NULL
                 --        END IF  
                 --     END IF
                 --     --LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont
                 --     LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_transferencia[v_cont].id_derechohabiente
                 --     LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_transferencia[v_cont].id_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_transferencia[v_cont].f_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_transferencia[v_cont].nss
                 --     IF g_ar_transferencia[v_cont].id_derechohabiente IS NULL THEN 
                 --        LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = NULL
                 --        LET  g_ar_combinado[v_cont_ctrl].pes_viv49           = NULL  
                 --        LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = NULL 
                 --     ELSE
                 --        LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                 --        LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                 --        LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                 --     END IF 
                 --     LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_transferencia[v_cont].aivs_viv97
                 --     LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_transferencia[v_cont].estado_solicitud
                 --     LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_transferencia[v_cont].cod_rechazo
                 --     LET v_cont_ctrl = v_cont_ctrl + 1
                 -- END FOR 
                  
                --WHEN 7                                                                                                   
                -- LET v_cont_encabezados = v_cont_encabezados +1                                                          
                --   LET p_cont = p_cont + g_ar_fortalecimiento.getLength()                                                  
                --   FOR v_cont = 1 TO g_ar_fortalecimiento.getLength()                                                      
                --      IF g_ar_fortalecimiento[v_cont].id_derechohabiente IS NOT NULL THEN                                  
                --         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1                                      
                --         LET v_cont_1 = v_cont_1 + 1                                                                     
                --      ELSE                                                                                               
                --         IF g_ar_fortalecimiento[v_cont].importe_viv = 0 THEN                                               
                --            LET g_ar_fortalecimiento[v_cont].importe_viv = NULL                                             
                --         END IF                                                                                          
                --      END IF                                                                                             
                --      
                --      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_fortalecimiento[v_cont].id_derechohabiente
                --      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_fortalecimiento[v_cont].id_solicitud      
                --      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_fortalecimiento[v_cont].f_solicitud       
                --      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_fortalecimiento[v_cont].nss               
                --      IF g_ar_fortalecimiento[v_cont].id_derechohabiente IS NULL THEN                                      
                --         LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = NULL
                --         LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = NULL                         
                --         LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = NULL
                --         LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = NULL                         
                --      ELSE                                                                                               
                --         LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                --         LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = g_ar_fortalecimiento[v_cont].importe_viv
                --         LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                --         LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
                --         
                --      END IF
                --      
                --      LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = g_ar_fortalecimiento[v_cont].importe_viv
                --      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_fortalecimiento[v_cont].estado_solicitud  
                --      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_fortalecimiento[v_cont].cod_rechazo       
                --      LET v_cont_ctrl = v_cont_ctrl + 1                                                                  
                --  END FOR     
                  
                 OTHERWISE
                   EXIT FOR
              END CASE
            END IF 
          END FOR

          LET v_total_pesos = 0
          LET v_total_pesos49 = 0
          LET v_total_aivs92 = 0
          LET v_total_aivs97 = 0
          FOR v_c = 1 TO p_cont
            IF g_ar_combinado[v_c].id_tipo_retiro IS NOT NULL OR  g_ar_combinado[v_c].id_tipo_retiro <> "" THEN
               LET v_total_pesos   = v_total_pesos + g_ar_combinado[v_c].pes_viv72
               LET v_total_pesos49 = v_total_pesos49 + g_ar_combinado[v_c].pes_viv49
               LET v_total_aivs92  = v_total_aivs92 + g_ar_combinado[v_c].aivs_viv92
               LET v_total_aivs97  = v_total_aivs97 + g_ar_combinado[v_c].aivs_viv97
            END IF 
          END FOR 

   OPEN WINDOW w_consulta_detalle WITH FORM "RETF026"
   DISPLAY ARRAY  g_ar_combinado TO t_detalle_gral.*
         ATTRIBUTE (ACCEPT = FALSE ,CANCEL = FALSE )
   
    BEFORE DISPLAY     
       LET v_cont_encabezados = v_cont_encabezados * 2 
       DISPLAY "Retiro Detalle General" TO ff_desc_consul
       DISPLAY "Total Registros: "||p_cont - v_cont_encabezados TO ff_cont_reg
       DISPLAY v_total_pesos   TO ff_total
       DISPLAY v_total_pesos49 TO ff_total3
       DISPLAY v_total_aivs92  TO ff_total1
       DISPLAY v_total_aivs97  TO ff_total2
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


