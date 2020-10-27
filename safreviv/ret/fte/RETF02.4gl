--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETP02                                                                 #
#OBJETIVO     => PROGRAMA DE CONSULTA GENERAL PARA EL MODULO DE RETIROS                 #
#Fecha inicio => Marzo 1, 2012                                                          #
#Modificacion => Marzo 29, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS 
DEFINE g_tabla              SMALLINT    ,
       g_estado             SMALLINT    ,
       g_modalidad          SMALLINT    ,
       g_fecha_inicio       DATE        ,
       g_fecha_fin          DATE        ,
       g_rechazo            SMALLINT    ,
       g_tpo_retiro         CHAR(1)     ,
       g_folio              DECIMAL(9,0),
       g_nss                CHAR(20)    ,
       g_id_derechohabiente CHAR(18)    ,
       g_ind_consistencia   SMALLINT     ,

       g_ar_solo_infonavit  DYNAMIC ARRAY OF RECORD    --LIKE ret_solo_infonavit 
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20)    ,
            f_solicitud        LIKE ret_solo_infonavit.f_solicitud,
            clabe              CHAR(18)      ,
            acc_viv97          DECIMAL (19,6),
            pes_viv97          DECIMAL (19,6),
            f_valuacion        DATE          ,
            estado_solicitud   CHAR(18)      ,
            cod_rechazo        CHAR(18)
       END RECORD,
    
       g_ar_combinado        DYNAMIC ARRAY OF RECORD
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
       END RECORD,
       g_ar_ley73            DYNAMIC ARRAY OF RECORD  --LIKE ret_fondo_ahorro
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
       END RECORD,
       g_ar_fondo_ahorro      DYNAMIC ARRAY OF RECORD  --LIKE ret_fondo_ahorro
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
       END RECORD,
       g_ar_fortalecimiento  DYNAMIC ARRAY OF RECORD
           id_solicitud       DECIMAL(9,0),
           id_derechohabiente DECIMAL(9,0),
           nss                CHAR(20),
           folio              LIKE ret_fortalecimiento_credito.folio,
           f_solicitud        LIKE ret_fortalecimiento_credito.f_solicitud,
           --h_solicitud        DATETIME HOUR TO SECOND,
           importe_viv        DECIMAL (19,6),
           estado_solicitud   CHAR(18),
           cod_rechazo        CHAR(18)
       END RECORD,
       g_ar_totales         DYNAMIC ARRAY OF RECORD
              id             SMALLINT ,
              tipo_retiro    VARCHAR(50),
              movimiento     SMALLINT,
              importe        DECIMAL (19,6),
              importe_49     DECIMAL (19,6),
              aivs92         DECIMAL (19,6),
              aivs97         DECIMAL (19,6),
              id_tipo_retiro SMALLINT 
       END RECORD,
       g_ar_retiro              DYNAMIC ARRAY OF RECORD
              id                  SMALLINT,      --modulo
              desc_tipo_retiro    VARCHAR(50),   --desc
              movimiento          SMALLINT,      --movimiento
              importe72           DECIMAL(14,6), --importe
              importe49           DECIMAL(14,6), --importe fortalecimiento
              aivs92              DECIMAL(14,6), --aivs92
              aivs97              DECIMAL(14,6), --aivs97
              tipo_retiro         CHAR(1),       -- tipo de retiro = 'A'
              id_matriz_derecho   SMALLINT       --identificador para la tabla matriz derecho 
       END RECORD,
       g_ar_preliquida          DYNAMIC ARRAY OF RECORD  --LIKE ret_preliquida
              id_derechohabiente LIKE ret_preliquida.id_derechohabiente,
              nss                CHAR(20),
              movimiento         LIKE ret_preliquida.movimiento,
              f_liquida          LIKE ret_preliquida.f_liquida,
              id_referencia      LIKE ret_preliquida.id_referencia,
              folio_liquida      LIKE ret_preliquida.folio_liquida,
              monto_pesos        LIKE ret_preliquida72.importe
       END RECORD,
       
       g_ar_liquida             DYNAMIC ARRAY OF RECORD  --LIKE cta_movimiento
              id_derechohabiente LIKE cta_movimiento.id_derechohabiente,
              nss                CHAR(20),
              movimiento         LIKE cta_movimiento.movimiento,
              f_liquida          LIKE cta_movimiento.f_liquida,
              id_referencia      LIKE cta_movimiento.id_referencia,
              folio_liquida      LIKE cta_movimiento.folio_liquida,
              monto_pesos        LIKE cta_movimiento.monto_pesos
       END RECORD,

       g_ar_disposicion       DYNAMIC ARRAY OF RECORD  --LIKE ret_disposicion.*
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_disposicion.f_solicitud,
            folio              LIKE ret_disposicion.folio,
            importe_viv72      LIKE ret_disposicion.importe_viv72,
            importe_viv92      LIKE ret_disposicion.aivs_viv92,
            importe_viv97      LIKE ret_disposicion.aivs_viv97,
            id_datamart        VARCHAR(50),
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18),
            id_causal          VARCHAR(50),
            id_ret_matriz_derecho SMALLINT
       END RECORD,

       g_ar_transferencia     DYNAMIC ARRAY OF RECORD  --LIKE ret_transferencia.*
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_transferencia.f_resolucion,
            folio              LIKE ret_transferencia.folio,
            aivs_viv97          LIKE ret_transferencia.aivs_viv97,
            id_datamart        VARCHAR(50),
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18),
            f_valuacion        DATE ,
            id_ret_matriz_derecho SMALLINT
       END RECORD  ,

       g_ar_tipo_n            DYNAMIC ARRAY OF RECORD  --LIKE ret_transferencia.*
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_transferencia.f_resolucion,    
            folio              LIKE ret_transferencia.folio,
            aivs_viv92         LIKE ret_transferencia.aivs_viv97,
            id_datamart        VARCHAR(50),
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18),
            f_valuacion        DATE ,
            id_ret_matriz_derecho SMALLINT 
        END RECORD,
      g_usuario_cod  LIKE seg_usuario.usuario_cod      
END GLOBALS 

MAIN 
DEFINE cb_tpo_retiro       ui.ComboBox,
       cb_estado           ui.ComboBox,
       cb_rechazo          ui.ComboBox,
       cb_modalidad        ui.ComboBox,
       v_nss_id            CHAR(18)   ,
       cb_ind_consistencia ui.ComboBox,
       p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
       p_s_titulo          STRING,   -- titulo de la ventana 
       v_cont              INTEGER,
       v_c                 INTEGER, 
       t_totalgral_pesos   DECIMAL(19,6),
       t_totalgral_pesos49 DECIMAL(19,6),
       t_totalgral_aivs92  DECIMAL(19,6),
       t_totalgral_aivs97  DECIMAL(19,6),
       r_total_pesos       DECIMAL(19,6),
       r_total_avis92      DECIMAL(19,6),
       r_total_avis97      DECIMAL(19,6),
       ar_ret_modalidad_retiro RECORD LIKE ret_modalidad_retiro.*,
       ar_ret_tipo_retiro      RECORD LIKE ret_tipo_retiro.*,
       ar_ret_afi_decreto      RECORD
        ind_consistencia SMALLINT 
       END RECORD,
       ar_ret_estado_solicitud RECORD LIKE ret_estado_solicitud.*,
       ar_ret_rechazo          RECORD LIKE ret_rechazo.*,
       w                       ui.Window,
       f                       ui.Form

   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   --Mando a global para imprimir
   LET g_usuario_cod = p_usuario_cod

   DISPLAY "usuario",g_usuario_cod
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN 
   OPEN WINDOW w_consulta WITH FORM "RETF020"

   LET cb_modalidad         = ui.ComboBox.forName("formonly.cb_modalidad") 
   LET cb_tpo_retiro        = ui.ComboBox.forName("formonly.cb_tpo_retiro")
   LET cb_estado            = ui.ComboBox.forName("formonly.cb_estado")
   LET cb_rechazo           = ui.ComboBox.forName("formonly.cb_rechazo")
   LET cb_ind_consistencia  = ui.ComboBox.forName("formonly.cb_ind_consistencia")

   CALL cb_modalidad.clear()
   CALL cb_tpo_retiro.clear()
   CALL cb_estado.clear()
   CALL cb_rechazo.clear()
   CALL cb_ind_consistencia.clear()

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   
   -- se capturan los datos para la consulta
   INPUT g_tabla              , -- solicitudes, preliquidacion, liquidacion
         g_fecha_inicio       , -- fecha de inicio de consulta
         g_fecha_fin          , -- fecha fin de consulta
         g_modalidad          ,
         g_tpo_retiro         ,
         g_estado             ,
         g_rechazo            ,
         g_nss                ,
         g_id_derechohabiente ,
         g_ind_consistencia   ,
         g_folio
   FROM  rg_estados          ,
         d_ini               ,
         d_fin               ,
         cb_modalidad        ,
         cb_tpo_retiro       ,
         cb_estado           ,
         cb_rechazo          ,
         e_nss               ,
         e_dere              ,
         cb_ind_consistencia ,
         e_folio
   ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE  )
 
      BEFORE INPUT
         -- la modalidad de retiro se inicia en TODOS
         LET g_modalidad = 0
        -- CALL  f.setElementHidden("formonly.cb_ind_consistencia",1)
        -- CALL  f.setElementHidden("lb_ind_consistencia",1)
         
         -- se llena el combo de inconsistencias
        -- CALL cb_ind_consistencia.clear()
         
         {DECLARE  c_cb_ind_consistencia CURSOR FOR  
         SELECT ind_consistencia
         FROM   afi_decreto
         WHERE  NOT ind_consistencia IS NULL 
         GROUP BY ind_consistencia
                                             
         CALL cb_ind_consistencia.addItem('0' ,"TODOS")
         
         -- se agregan los tipos de inconsistencia al combo
         FOREACH c_cb_ind_consistencia INTO ar_ret_afi_decreto.ind_consistencia
            CALL cb_ind_consistencia.addItem(ar_ret_afi_decreto.ind_consistencia ,ar_ret_afi_decreto.ind_consistencia)
         END FOREACH}

         -- se llena el combo de tipo de retiro         
         CALL cb_tpo_retiro.clear()
         DECLARE  c_cb_tpo_retiro CURSOR FOR  
         SELECT tpo_retiro, des_corta
         FROM ret_tipo_retiro
         WHERE (modalidad_retiro = g_modalidad 
         OR  0 = g_modalidad) 
         ORDER BY tpo_retiro
        
         CALL cb_tpo_retiro.addItem('0' ,"TODOS")
         
         -- se agregan los tipos de retiro al combo
         FOREACH c_cb_tpo_retiro INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
            CALL cb_tpo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
         END FOREACH
         
         INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL  
         LET g_tpo_retiro = 0
         
         -- se llena el combo de modalidad de retiro
         CALL cb_modalidad.clear()
         
         DECLARE c_cb_modalidad CURSOR FOR  
         SELECT * 
         FROM ret_modalidad_retiro
         ORDER BY modalidad_retiro
         
         CALL cb_modalidad.addItem(0 ,"TODOS")
         
         FOREACH c_cb_modalidad INTO ar_ret_modalidad_retiro.*
            CALL cb_modalidad.addItem(ar_ret_modalidad_retiro.modalidad_retiro ,ar_ret_modalidad_retiro.modalidad_retiro||" - "||ar_ret_modalidad_retiro.des_corta)
         END FOREACH
         
         INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
       
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
         
         LET g_modalidad           = 0  --tipos de retiro
         LET g_tabla               = 1  --solicitud - peliquidacion -liquidacion
         LET g_estado              = 0  --estado de la solicitud
         LET g_rechazo             = 0  --codigo de rechazo
         LET g_ind_consistencia    = 0  --indicador consistencia
         LET g_fecha_inicio        = TODAY
         LET g_fecha_fin           = TODAY
         LET t_totalgral_pesos     = 0
         LET t_totalgral_pesos49   = 0
         LET t_totalgral_aivs92    = 0
         LET t_totalgral_aivs97    = 0       
      
      -- al cambiar la modalidad 
      ON CHANGE cb_modalidad
         CALL cb_tpo_retiro.clear()

          DISPLAY g_modalidad
          
          CALL cb_tpo_retiro.addItem("0" ,"TODOS") 
          
          FOREACH c_cb_tpo_retiro INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
             CALL cb_tpo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
          END FOREACH 
          INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL

          IF g_modalidad = 4 THEN
               CALL  f.setElementHidden("formonly.cb_ind_consistencia",0)
               CALL  f.setElementHidden("lb_ind_consistencia",0)
          ELSE 
               CALL  f.setElementHidden("formonly.cb_ind_consistencia",1)
               CALL  f.setElementHidden("lb_ind_consistencia",1)
          END IF 

      ON CHANGE rg_estados
         IF ( g_tabla = 1 ) THEN
            CALL  f.setElementHidden("lb_estado",0)
            CALL  f.setElementHidden("lb_rechazo",0)
            CALL  f.setElementHidden("formonly.cb_rechazo",0)
            CALL  f.setElementHidden("formonly.cb_estado",0)            
         
            IF ( g_modalidad = 4 ) THEN
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
      
      -- =================================================================================
      -- =================================================================================
      --
      -- BLOQUE DE EJECUCION DE LA CONSULTA
      --
      -- =================================================================================
      -- =================================================================================
      ON ACTION consultar
         IF ( g_nss IS NOT NULL ) THEN
            IF ( g_id_derechohabiente IS NULL ) THEN
               IF ( g_modalidad = 1 OR g_modalidad = 3 OR 
                    g_modalidad = 5 OR g_modalidad = 6 OR 
                    g_modalidad = 7 ) THEN
                  SELECT id_derechohabiente
                  INTO   g_id_derechohabiente
                  FROM   afi_derechohabiente
                  WHERE  nss = g_nss
               END IF 
              
               IF g_modalidad = 0 THEN 
                  SELECT  id_derechohabiente
                    INTO  g_id_derechohabiente
                    FROM  afi_derechohabiente
                   WHERE  nss = g_nss
               
                  IF g_id_derechohabiente IS NULL THEN
                     SELECT  id_afi_fondo72
                      INTO  g_id_derechohabiente
                      FROM  afi_fondo72
                     WHERE  nss = g_nss 
                  END IF
               
                  IF g_id_derechohabiente IS NULL THEN
                     SELECT  id_decreto
                       INTO  g_id_derechohabiente
                       FROM  afi_decreto
                      WHERE  nss = g_nss
                  END IF
               END IF 

               IF g_modalidad = 2 THEN  
                  SELECT  id_afi_fondo72
                   INTO  g_id_derechohabiente
                   FROM  afi_fondo72
                  WHERE  nss = g_nss
               END IF

               IF g_modalidad = 4 THEN
                 SELECT  id_decreto
                   INTO  g_id_derechohabiente
                   FROM  afi_decreto
                  WHERE  nss = g_nss
               END IF              
            ELSE 
               IF g_modalidad = 1 OR g_modalidad = 3 OR g_modalidad = 5 OR 
                  g_modalidad = 6 OR g_modalidad = 7 OR g_modalidad = 8 THEN
                  SELECT  id_derechohabiente
                    INTO  v_nss_id
                    FROM  afi_derechohabiente
                   WHERE  nss = g_nss
               END IF
               
               IF g_modalidad = 0 THEN
                  SELECT  id_derechohabiente
                    INTO  v_nss_id
                    FROM  afi_derechohabiente
                   WHERE  nss = g_nss
               
                   IF v_nss_id IS NULL THEN
                       SELECT  id_afi_fondo72
                         INTO  v_nss_id
                         FROM  afi_fondo72
                        WHERE  nss = g_nss 
                   END IF
                   IF v_nss_id IS NULL THEN
                       SELECT  id_decreto
                         INTO  v_nss_id
                         FROM  afi_decreto
                        WHERE  nss = g_nss 
                   END IF 
               END IF 

               IF g_modalidad = 2 THEN 
                  SELECT  id_afi_fondo72
                  INTO  v_nss_id
                  FROM  afi_fondo72
                  WHERE  nss = g_nss
               END IF  
               
               IF g_modalidad = 4 THEN 
                  SELECT  id_decreto
                  INTO  v_nss_id
                  FROM  afi_decreto
                  WHERE  nss = g_nss
               END IF 
                    
               IF g_id_derechohabiente <> v_nss_id THEN
                  CALL fn_mensaje("Aviso","El nss debe de estar en blanco o ser igual al id derechohabiente","exclamation")
                  --ERROR "El nss debe de estar en blanco o ser igual al id derechohabiente" 
                  CONTINUE INPUT  
               END IF 
            END IF 
         END IF 
    
      CASE g_tabla 
         WHEN 1     --solicitudes 
            LET v_cont = 0 
               LET t_totalgral_pesos  = 0
               LET t_totalgral_pesos49  = 0
               LET t_totalgral_aivs92 = 0
               LET t_totalgral_aivs97 = 0
            
               --solo infonavit
            IF g_modalidad = 1 OR g_modalidad = 0  THEN 
                LET r_total_pesos  = 0 
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0
            
               CALL fn_solicitud_solo_infonavit(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin) 
               RETURNING r_total_pesos , r_total_avis97
              -- LET t_totalgral_pesos  = t_totalgral_pesos + r_total_pesos
              -- LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF
            
               --fondo-ahorro
            IF g_modalidad = 2 OR g_modalidad = 0 THEN
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0 
                LET r_total_pesos  = 0 
               CALL fn_solicitud_fondo_ahorro(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin)
                RETURNING r_total_pesos  
                
               LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               --LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF
         
            --ley 73
            IF g_modalidad = 3 OR g_modalidad = 0 THEN
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0 
                LET r_total_pesos = 0
               CALL fn_solicitud_ley73(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin)
                RETURNING r_total_pesos,r_total_avis92,r_total_avis97
            
               LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF
            
               --tipo_n
            IF g_modalidad = 4 OR g_modalidad = 0 THEN
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0 
                LET r_total_pesos = 0
               CALL fn_solicitud_tipo_n(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin)
                RETURNING r_total_pesos,r_total_avis92
            
               LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF
                --disposicion y PMG
            IF g_modalidad = 5 OR g_modalidad = 8 OR g_modalidad = 0 THEN
               LET r_total_avis92 = 0 
               LET r_total_avis97 = 0 
               LET r_total_pesos  = 0 

               CALL fn_solicitud_disposicion(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin, g_modalidad)
                    RETURNING r_total_pesos, r_total_avis92, r_total_avis97
                
            END IF
               --transferencia
            IF g_modalidad = 6 OR g_modalidad = 0 THEN
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0 
                LET r_total_pesos  = 0 
               CALL fn_solicitud_transferencia(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin)
                RETURNING r_total_pesos, r_total_avis97
               LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF
            
               --fortalecimiento
            IF g_modalidad = 7 OR g_modalidad = 0 THEN
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0 
                LET r_total_pesos = 0 
               CALL fn_solicitud_fortalecimiento(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin)
                RETURNING r_total_pesos
                --RETURNING r_total_pesos, r_total_avis97
               --LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               LET t_totalgral_pesos49 = t_totalgral_pesos49 + r_total_pesos
               --LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF
            
            -- muestra el resumen de modalidades si existen modlos distintos 
            --agregar los registros encontrados por retiro con criterios de la solicitud .
            CALL fn_consulta_modalidad(t_totalgral_pesos,t_totalgral_pesos49, t_totalgral_aivs92, t_totalgral_aivs97) 
         
         WHEN 2     --preliquidacion
            CALL fn_preliquidacion(g_fecha_inicio , g_fecha_fin)

         WHEN 3     --liquidacion
            CALL fn_liquidacion(g_fecha_inicio , g_fecha_fin)   
           
      END CASE 
    AFTER FIELD e_folio
      NEXT FIELD e_nss  

    AFTER FIELD d_ini
      IF g_fecha_inicio IS NULL THEN
        CALL fn_mensaje("Aviso","El valor del campo no debe ser nulo","exclamation")
        LET g_fecha_inicio = TODAY 
        NEXT FIELD d_ini
      END IF

    AFTER FIELD d_fin
      IF g_fecha_fin IS NULL THEN
        CALL fn_mensaje("Aviso","El valor del campo no debe ser nulo","exclamation")
        LET g_fecha_fin = TODAY 
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

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_solo_infonavit(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
   DEFINE p_con_ini      DATE
   DEFINE p_con_fin      DATE
   DEFINE p_estado       SMALLINT
   DEFINE p_rechazo      SMALLINT
   DEFINE v_cont         INTEGER
   DEFINE v_query        VARCHAR(500)
   DEFINE v_total_pesos  DECIMAL(19,6)
   DEFINE v_c            INTEGER
   DEFINE v_con_tot      INTEGER
   DEFINE v_total_aivs   DECIMAL(19,6)
   DEFINE v_desc_estado  CHAR(18)
   DEFINE v_desc_rechazo CHAR(18)
   
   CALL g_ar_solo_infonavit.clear()

   {IF p_estado < 100 THEN 
     LET p_rechazo = 0
   END if} 
 
   LET v_query =  "\n SELECT id_solicitud,id_derechohabiente,' ',f_solicitud, ",
                  "\n clabe,aivs_viv97,importe_viv97,f_valuacion,estado_solicitud,cod_rechazo",
                  "\n FROM ret_solo_infonavit",
                  "\n WHERE (estado_solicitud = ? or '0' = ?)",
                  "\n   AND (cod_rechazo = ? or '0' = ?)",
                  "\n   AND f_solicitud between ? and ?",
                  "\n   AND (id_derechohabiente = ?",
                  "\n    or ? is  null or ? = '')",
                  "\n   AND (folio = ?",
                  "\n    or ? is  null or ? = '')"
                  
          --DISPLAY p_estado," ",p_rechazo," ",p_con_ini ," ", p_con_fin ," ", 
          --g_id_derechohabiente," ",
          --g_folio

          --DISPLAY v_query
          
          LET v_cont = 1
          LET  v_total_aivs = 0
          LET g_ar_solo_infonavit[v_cont].nss = "1-Sólo Infonavit"
          LET g_ar_solo_infonavit[v_cont].pes_viv97 = 0
          LET g_ar_solo_infonavit[v_cont].acc_viv97 = 0
          LET g_ar_solo_infonavit[v_cont].clabe = "------------"
          LET v_cont = v_cont + 1

          PREPARE pr_solo_infonavit FROM v_query
          DECLARE cur_solo_infonavit CURSOR FOR pr_solo_infonavit
          FOREACH cur_solo_infonavit USING p_estado,p_estado, p_rechazo ,p_rechazo,p_con_ini , p_con_fin , 
          g_id_derechohabiente, g_id_derechohabiente,g_id_derechohabiente, 
          g_folio, g_folio, g_folio INTO g_ar_solo_infonavit[v_cont].*
             SELECT nss
               INTO g_ar_solo_infonavit[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_solo_infonavit[v_cont].id_derechohabiente

              INITIALIZE v_desc_estado TO NULL 
              SELECT  des_corta
              INTO v_desc_estado
              FROM ret_estado_solicitud
              WHERE estado_solicitud = g_ar_solo_infonavit[v_cont].estado_solicitud

              LET g_ar_solo_infonavit[v_cont].estado_solicitud = g_ar_solo_infonavit[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED

              INITIALIZE v_desc_rechazo TO NULL
              SELECT  des_corta
              INTO v_desc_rechazo
              FROM ret_rechazo
              WHERE cod_rechazo = g_ar_solo_infonavit[v_cont].cod_rechazo

              LET g_ar_solo_infonavit[v_cont].cod_rechazo = g_ar_solo_infonavit[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
              
              LET v_cont = v_cont + 1
              --DISPLAY "entro", g_ar_solo_infonavit[v_cont-1].*
          END FOREACH  

          IF g_ar_solo_infonavit[v_cont].id_derechohabiente IS NULL  THEN
            CALL g_ar_solo_infonavit.deleteElement(v_cont)
          END IF 
          
          LET v_total_pesos = 0
          LET v_total_aivs = 0
          FOR v_c = 1 TO v_cont 
           IF g_ar_solo_infonavit[v_c].id_derechohabiente IS NOT NULL THEN 
             LET v_total_pesos = v_total_pesos + g_ar_solo_infonavit[v_c].pes_viv97
             LET v_total_aivs  = v_total_aivs  + g_ar_solo_infonavit[v_c].acc_viv97             
           END IF  
          END FOR 

          IF v_total_pesos > 0 OR v_total_aivs > 0  OR v_cont - 2 > 0 THEN 
          
             LET v_con_tot  = g_ar_totales.getLength() + 1
             LET g_ar_totales[v_con_tot].id             = 0
             LET g_ar_totales[v_con_tot].tipo_retiro    = "1 Retiro Solo Infonavit"
             LET g_ar_totales[v_con_tot].movimiento     = 172
             --LET g_ar_totales[v_con_tot].importe      = v_total_pesos
             LET g_ar_totales[v_con_tot].importe        = 0
             LET g_ar_totales[v_con_tot].importe_49     = 0
             LET g_ar_totales[v_con_tot].aivs92         = 0
             LET g_ar_totales[v_con_tot].aivs97         = v_total_aivs
             LET g_ar_totales[v_con_tot].id_tipo_retiro = 1

            --CALL fn_consulta_solo_infonavit(v_cont)
            LET g_ar_solo_infonavit[v_cont].clabe = "------------"
            LET g_ar_solo_infonavit[v_cont].acc_viv97 = v_total_aivs
            LET g_ar_solo_infonavit[v_cont].nss = "Totales "
            --LET g_ar_solo_infonavit[v_cont].pes_viv97 = v_total_pesos
            LET g_ar_solo_infonavit[v_cont].pes_viv97 = v_total_pesos
            LET v_cont = v_cont + 1
         END IF 

             RETURN v_total_pesos, v_total_aivs
END FUNCTION

--realiza la carga de los registros  al array de solicitudes
FUNCTION fn_solicitud_fondo_ahorro(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini     DATE
 DEFINE p_con_fin     DATE  
 DEFINE p_estado      SMALLINT
 DEFINE p_rechazo     SMALLINT
 DEFINE v_cont        INTEGER
 DEFINE v_query       VARCHAR(500)
 DEFINE v_total_pesos DECIMAL(14,6)
 DEFINE v_c           INTEGER 
 DEFINE v_con_tot     INTEGER 
 DEFINE v_desc_estado CHAR(18)
 DEFINE v_desc_rechazo CHAR(18)

 CALL g_ar_fondo_ahorro.clear( )


    LET v_query =  "\n SELECT id_solicitud,id_derechohabiente,' ',",
                           "\n f_solicitud,cve_referencia,folio,saldo_viv72,",
                           "\n id_datamart,estado_solicitud,cod_rechazo,causal_retiro",
                           "\n FROM ret_fondo_ahorro",
                           "\n WHERE (estado_solicitud   = ? or '0' = ?)",
                           "\n   AND (cod_rechazo        = ? or '0' = ?)",
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
                  ,g_id_derechohabiente ,g_id_derechohabiente, g_id_derechohabiente, 
                   g_folio, g_folio, g_folio INTO g_ar_fondo_ahorro[v_cont].*
             SELECT nss
               INTO g_ar_fondo_ahorro[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_fondo_ahorro[v_cont].id_derechohabiente

              INITIALIZE v_desc_estado TO NULL 
              SELECT  des_corta
              INTO v_desc_estado
              FROM ret_estado_solicitud
              WHERE estado_solicitud = g_ar_fondo_ahorro[v_cont].estado_solicitud

              LET g_ar_fondo_ahorro[v_cont].estado_solicitud = g_ar_fondo_ahorro[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED

              INITIALIZE v_desc_rechazo TO NULL  
              SELECT  des_corta
                INTO v_desc_rechazo
                FROM ret_rechazo
               WHERE cod_rechazo = g_ar_fondo_ahorro[v_cont].cod_rechazo

              LET g_ar_fondo_ahorro[v_cont].cod_rechazo = g_ar_fondo_ahorro[v_cont].cod_rechazo CLIPPED ," -"|| v_desc_rechazo CLIPPED 
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
             LET g_ar_totales[v_con_tot].movimiento   = 182          
             LET g_ar_totales[v_con_tot].importe      = v_total_pesos
             LET g_ar_totales[v_con_tot].importe_49   = 0
             LET g_ar_totales[v_con_tot].aivs92       = 0
             LET g_ar_totales[v_con_tot].aivs97       = 0
             LET g_ar_totales[v_con_tot].id_tipo_retiro = 2
             
             LET g_ar_fondo_ahorro[v_cont].cve_referencia = "------------"
             LET g_ar_fondo_ahorro[v_cont].nss = "Totales "
             LET g_ar_fondo_ahorro[v_cont].importe_viv72 = v_total_pesos
             LET v_cont = v_cont + 1 

             CALL g_ar_fondo_ahorro.deleteElement(v_cont)
         END IF 

          RETURN v_total_pesos
END FUNCTION 

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_ley73(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini      DATE
 DEFINE p_con_fin      DATE  
 DEFINE p_estado       SMALLINT
 DEFINE p_rechazo      SMALLINT
 DEFINE v_cont         INTEGER
 DEFINE v_query        STRING 
 DEFINE v_total_pesos  DECIMAL(14,6)
 DEFINE v_total_aivs92 DECIMAL(14,6)
 DEFINE v_total_aivs97 DECIMAL(14,6)
 DEFINE v_total_aivs   DECIMAL(14,6)
 DEFINE v_c            INTEGER 
 DEFINE v_con_tot      INTEGER 
 DEFINE v_desc_estado  CHAR(18)
 DEFINE v_desc_rechazo CHAR(18)
 
 CALL g_ar_ley73.clear( )

    --LET p_estado = 0

    LET v_query =  "\n SELECT id_solicitud,id_derechohabiente,' ',                                    ",
                   "\n f_solicitud,' ',folio,importe_viv92,importe_viv97,aivs_viv92,aivs_viv97,       ",
                   "\n estado_solicitud,cod_retorno,cod_rechazo                                       ",
                   "\n FROM ret_ley73                                                                 ",                   
                   "\n WHERE (estado_solicitud = ? or '0' = ?)                                        ",
                   "\n   AND folio = folio                                                            ",
                   "\n   AND (cod_rechazo = ? or '0' = ?)                                             ",
                   "\n   AND f_solicitud between ? and ?                                              ", 
                   "\n   AND (id_derechohabiente = ?                                                  ",
                   "\n    or ? is null or ? = '')                                                     ",
                   "\n   AND (folio = ?                                                               ",
                   "\n    or ? is null or ? = '')                                                     "
                                      
          LET v_cont = 1 
          
          LET g_ar_ley73[v_cont].nss = "3-Ley 73"
          
          LET g_ar_ley73[v_cont].aivs_viv92 = 0
          LET g_ar_ley73[v_cont].aivs_viv97 = 0
          
          LET g_ar_ley73[v_cont].importe_viv92 = 0
          LET g_ar_ley73[v_cont].importe_viv97 = 0
          LET v_cont = v_cont + 1

          --DISPLAY v_query
         
{         DISPLAY "p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin ", 
                  "\n p_estado, p_rechazo ,p_con_ini , p_con_fin \n"
                  ,p_estado,"-", p_rechazo ,"-",p_con_ini ,"-", p_con_fin,g_id_derechohabiente,g_folio,
                  "\n ",p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin}

          PREPARE pr_Ley73 FROM v_query
          DECLARE cur_ley73 CURSOR FOR pr_Ley73
          FOREACH cur_ley73 USING p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin 
          , g_id_derechohabiente ,g_id_derechohabiente, g_id_derechohabiente, 
            g_folio, g_folio, g_folio INTO g_ar_ley73[v_cont].*
             SELECT nss
               INTO g_ar_ley73[v_cont].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_ley73[v_cont].id_derechohabiente

              INITIALIZE v_desc_estado TO NULL 
              SELECT  des_corta
                INTO v_desc_estado
                FROM ret_estado_solicitud
               WHERE estado_solicitud = g_ar_ley73[v_cont].estado_solicitud

              LET g_ar_ley73[v_cont].estado_solicitud = g_ar_ley73[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED 

              INITIALIZE v_desc_rechazo TO NULL
                 SELECT  des_corta
                   INTO v_desc_rechazo
                   FROM ret_rechazo
                  WHERE cod_rechazo = g_ar_ley73[v_cont].cod_rechazo

              LET g_ar_ley73[v_cont].cod_rechazo = g_ar_ley73[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
              LET v_cont = v_cont + 1
          END FOREACH  
          LET v_total_pesos = 0
          LET v_total_aivs  = 0
          LET v_total_aivs92  = 0
          LET v_total_aivs97  = 0

           FOR v_c = 1 TO v_cont
              IF  g_ar_ley73[v_c].id_derechohabiente IS NOT NULL THEN
                 LET v_total_pesos  = 0
                 LET v_total_aivs92 = v_total_aivs92 + g_ar_ley73[v_c].aivs_viv92
                 LET v_total_aivs97 = v_total_aivs97 + g_ar_ley73[v_c].aivs_viv97
                 LET v_total_aivs   = v_total_aivs   + g_ar_ley73[v_c].aivs_viv92 + g_ar_ley73[v_c].aivs_viv97
              END IF  
           END FOR 

          IF v_total_pesos > 0 OR v_total_aivs > 0 OR v_cont - 3 > 0 THEN 
             LET v_con_tot  = g_ar_totales.getLength() + 1
             LET g_ar_totales[v_con_tot].id  = 0
             LET g_ar_totales[v_con_tot].id_tipo_retiro = 3
             LET g_ar_totales[v_con_tot].tipo_retiro  = "3 Retiro Ley 73"
             LET g_ar_totales[v_con_tot].movimiento   =  192
             LET g_ar_totales[v_con_tot].importe      = v_total_pesos
             LET g_ar_totales[v_con_tot].importe_49   = 0
             LET g_ar_totales[v_con_tot].aivs92       = v_total_aivs92
             LET g_ar_totales[v_con_tot].aivs97       = v_total_aivs97

             LET g_ar_ley73[v_cont].nss = "Totales "
             LET g_ar_ley73[v_cont].aivs_viv92 = v_total_aivs92
             LET g_ar_ley73[v_cont].aivs_viv97 = v_total_aivs97
             --LET g_ar_ley73[v_cont].importe_viv92 = v_total_pesos
             --LET g_ar_ley73[v_cont].importe_viv97 = v_total_aivs97

             --DISPLAY  g_ar_ley73[v_cont].*
             LET v_cont = v_cont + 1
             
             CALL g_ar_ley73.deleteElement(v_cont)
         END IF
          RETURN v_total_pesos ,v_total_aivs92 ,v_total_aivs97
END FUNCTION    

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_tipo_n(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini     DATE
 DEFINE p_con_fin     DATE  
 DEFINE p_estado      SMALLINT
 DEFINE p_rechazo     SMALLINT
 DEFINE v_cont        INTEGER
 DEFINE v_query       STRING 
 DEFINE v_total_pesos  DECIMAL(19,6)
 DEFINE v_total_aivs92 DECIMAL(19,6)
 
 DEFINE v_c           INTEGER 
 DEFINE v_con_tot     INTEGER
 DEFINE v_desc_estado  CHAR(18)
 DEFINE v_desc_rechazo  CHAR(18) 
 
 CALL g_ar_tipo_n.clear( )

 LET v_query =  "\n SELECT rn.id_solicitud,rn.id_decreto,' ',",
                "\n rcn.f_carga,rn.folio,rn.aivs_viv92,",
                "\n ' ',rn.estado_solicitud,rn.cod_rechazo,today,rn.id_ret_matriz_derecho",
                "\n FROM ret_tipo_n rn,      ",
                "\n      ret_cza_tipo_n rcn, ",
                "\n      afi_decreto afi     ",
                "\n WHERE (rn.estado_solicitud = ? or '0' = ?)",
                "\n   AND (rn.cod_rechazo = ? or '0' = ?)",
                "\n   AND rcn.f_carga between ? and ?",
                "\n   AND (rn.id_decreto = ?",
                "\n    or ? is null or ? =  '')",
                "\n   AND (rn.folio = ?",
                "\n    or ? is null or ? = '')",
                 "\n   AND (afi.ind_consistencia = ?",
                "\n    or ? is null or ? = '0')",
                "\n   AND rn.folio = rcn.folio", 
                "\n    order by rn.id_ret_matriz_derecho"

 LET v_cont = 1 
 --DISPLAY v_query
 LET g_ar_tipo_n[v_cont].nss = "4-Tipo N"
 LET g_ar_tipo_n[v_cont].aivs_viv92 = 0
 LET g_ar_tipo_n[v_cont].f_solicitud = "------------"
 LET v_cont = v_cont + 1

 PREPARE pr_tipo_n FROM v_query
 DECLARE cur_tpo_n CURSOR FOR pr_tipo_n
 --se suprimio la validacion rechazo
 --FOREACH cur_tpo_n USING p_estado,p_estado, p_rechazo ,p_rechazo ,p_con_ini , p_con_fin  
 FOREACH cur_tpo_n USING p_estado,p_estado, p_rechazo ,p_rechazo,p_con_ini , p_con_fin  
      ,g_id_derechohabiente ,g_id_derechohabiente, g_id_derechohabiente
      ,g_ind_consistencia ,g_ind_consistencia, g_ind_consistencia
      ,g_folio, g_folio, g_folio INTO g_ar_tipo_n[v_cont].*

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

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_fortalecimiento(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini       DATE
 DEFINE p_con_fin       DATE  
 DEFINE p_estado        SMALLINT
 DEFINE p_rechazo       SMALLINT
 DEFINE v_cont          INTEGER
 DEFINE v_query         STRING 
 DEFINE v_total_pesos   DECIMAL(14,6)
 DEFINE v_total_aivs97  DECIMAL(14,6)
 DEFINE v_c             INTEGER 
 DEFINE v_con_tot       INTEGER
 DEFINE v_desc_estado   CHAR(18)
 DEFINE v_desc_rechazo  CHAR(18) 
 
    CALL g_ar_fortalecimiento.clear( )
   
    LET v_query =  "\n SELECT rt.id_solicitud,rt.id_derechohabiente,' ',   ",
                   "\n  rt.folio,rt.f_solicitud,rt.importe_viv,            ",
                   "\n  rt.estado_solicitud,rt.cod_rechazo                 ",
                   "\n FROM ret_fortalecimiento_credito rt                 ",
                   "\n WHERE (rt.estado_solicitud = ? or '0' = ?)          ",
                   "\n   AND (rt.cod_rechazo = ? or '0' = ?)               ",
                   "\n   AND rt.f_solicitud between ? and ?                ",
                   "\n   AND (rt.id_derechohabiente = ?                    ",
                   "\n    or ? is null or ? =  '')                         ",
                   "\n   AND (rt.folio = ?                                 ",
                   "\n    or ? is null or ? = '')                          "
       
    LET v_cont = 1 
    DISPLAY  v_query
    LET g_ar_fortalecimiento[v_cont].nss = "7-Fortalecimiento"
    LET g_ar_fortalecimiento[v_cont].importe_viv = 0    
    LET g_ar_fortalecimiento[v_cont].f_solicitud = "------------"
    LET v_cont = v_cont + 1
         
    PREPARE pr_fortalecimiento FROM v_query
    DECLARE cur_fortalecimiento CURSOR FOR pr_fortalecimiento
    FOREACH cur_fortalecimiento USING p_estado,p_estado, p_rechazo ,p_rechazo ,p_con_ini , p_con_fin  
       , g_id_derechohabiente ,g_id_derechohabiente, g_id_derechohabiente, 
         g_folio, g_folio, g_folio INTO g_ar_fortalecimiento[v_cont].*
                 
        SELECT nss
          INTO g_ar_fortalecimiento[v_cont].nss 
          FROM afi_derechohabiente
         WHERE id_derechohabiente = g_ar_fortalecimiento[v_cont].id_derechohabiente

         INITIALIZE v_desc_estado TO NULL 
        SELECT  des_corta
          INTO v_desc_estado
          FROM ret_estado_solicitud
         WHERE estado_solicitud = g_ar_fortalecimiento[v_cont].estado_solicitud
   
        LET g_ar_fortalecimiento[v_cont].estado_solicitud = g_ar_fortalecimiento[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED 
   
        INITIALIZE v_desc_rechazo TO NULL 
        SELECT  des_corta
          INTO v_desc_rechazo
          FROM ret_rechazo
         WHERE cod_rechazo = g_ar_fortalecimiento[v_cont].cod_rechazo
   
        LET g_ar_fortalecimiento[v_cont].cod_rechazo = g_ar_fortalecimiento[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
        LET v_cont = v_cont + 1
    END FOREACH  
    LET v_total_pesos  = 0
    LET v_total_aivs97 = 0
              
    FOR v_c = 1 TO v_cont 
      IF  g_ar_fortalecimiento[v_c].id_derechohabiente IS NOT NULL THEN
        LET v_total_pesos = g_ar_fortalecimiento[v_c].importe_viv + v_total_pesos        
      END IF 
    END FOR 
   
   IF v_total_pesos > 0 OR v_total_aivs97 > 0 OR v_cont - 2 > 0 THEN 
     
       LET v_con_tot  = g_ar_totales.getLength() + 1
       LET g_ar_totales[v_con_tot].id  = 0
       LET g_ar_totales[v_con_tot].tipo_retiro    = "7 Retiro Fortalecimiento"
       LET g_ar_totales[v_con_tot].movimiento     = 462 
       LET g_ar_totales[v_con_tot].importe        = 0
       LET g_ar_totales[v_con_tot].importe_49     = v_total_pesos
       LET g_ar_totales[v_con_tot].aivs92         = 0
       LET g_ar_totales[v_con_tot].aivs97         = 0
       LET g_ar_totales[v_con_tot].id_tipo_retiro = 7
   
       LET g_ar_fortalecimiento[v_cont].f_solicitud = "------------"
       LET g_ar_fortalecimiento[v_cont].nss         = "Totales "
       LET g_ar_fortalecimiento[v_cont].importe_viv = v_total_pesos
       LET v_cont = v_cont + 1
       
       CALL g_ar_fortalecimiento.deleteElement(v_cont)
    END IF
          RETURN v_total_pesos
END FUNCTION  


--realiza la carga de los registros  al array
FUNCTION fn_solicitud_disposicion(p_estado, p_rechazo ,p_con_ini , p_con_fin, p_modalidad) 
DEFINE p_con_ini      DATE         ,
       p_con_fin      DATE         ,
       p_modalidad    SMALLINT     , -- 5: disposicion, 8:pmg
       p_estado       SMALLINT     ,
       p_rechazo      SMALLINT     ,
       v_cont         INTEGER     ,
       v_query        STRING       ,
       v_total_pesos  DECIMAL(14,6),
       v_total_aivs92 DECIMAL(14,6),
       v_total_aivs97 DECIMAL(14,6),
       v_total_aivs   DECIMAL(14,6),
       v_c            INTEGER     ,
       v_con_tot      INTEGER     ,
       v_desc_estado  CHAR(18)     ,
       v_desc_rechazo CHAR(18)     ,
       v_proceso_cod  SMALLINT
 
   CALL g_ar_disposicion.clear( )

   --LET p_estado = 0

   --LET v_query =  "\n SELECT rd.id_solicitud,rd.id_derechohabiente,' ',",
                  --"\n rcd.f_carga,rd.folio,importe_viv72,rd.aivs_viv92,rd.aivs_viv97,",
                  --"\n ' ',rd.estado_solicitud,rd.cod_rechazo,' ',rd.id_ret_matriz_derecho",
                  --"\n FROM ret_disposicion  rd,",
                  --"\n      ret_cza_disposicion rcd",
                  --"\n WHERE (rd.estado_solicitud = ? or '0' = ?)",
                  --"\n   AND rd.folio = rcd.folio",
                  --"\n   AND (rd.cod_rechazo = ? or '0' = ?)",
                  --"\n   AND rcd.f_carga between ? and ?" , 
                  --"\n   AND (rd.id_derechohabiente = ?",
                  --"\n    or ? is null or ? = '')",
                  --"\n   AND (rd.folio = ?",
                  --"\n    or ? is null or ? = '')",
                  --"\n    order by rd.id_ret_matriz_derecho"

   LET v_query = "\n SELECT rd.id_solicitud,rd.id_derechohabiente,' ',",
                  "\n rcd.f_carga,rd.folio,importe_viv72,rd.aivs_viv92,rd.aivs_viv97,",
                  "\n ' ',rd.estado_solicitud,rd.cod_rechazo,' ',rd.id_ret_matriz_derecho"
   IF g_tpo_retiro = "0" THEN
       LET v_query = v_query CLIPPED, "\n FROM ret_disposicion  rd,",
                                      "\n      ret_cza_disposicion rcd"
   ELSE 
       LET v_query = v_query CLIPPED, "\n FROM ret_disposicion  rd ",
                                      "\n      inner join ret_matriz_derecho rmd ",
                                      "\n                   on rd.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho ",
                                      "\n                  and rmd.tpo_retiro = '", g_tpo_retiro, "',",
                                      "\n      ret_cza_disposicion rcd"
   END IF 
   LET v_query = v_query CLIPPED, "\n WHERE 1 = 1 ",
                                  "\n   AND rd.folio = rcd.folio"
   IF p_estado <> 0 THEN
        LET v_query = v_query CLIPPED, "\n AND rd.estado_solicitud = ", p_estado
   END IF 
   IF p_rechazo <> 0 THEN 
        LET v_query = v_query CLIPPED, "\n AND (rd.cod_rechazo = ", p_rechazo
   END IF 
   IF p_con_ini IS NOT NULL AND p_con_fin IS NOT NULL THEN
       LET v_query = v_query CLIPPED, "\n AND rcd.f_carga between '", p_con_ini, "' AND '", p_con_fin, "'"
   END IF 
   IF g_id_derechohabiente IS NOT NULL THEN 
       LET v_query = v_query CLIPPED, "\n AND rd.id_derechohabiente = ", g_id_derechohabiente
   END IF    
   IF g_folio IS NOT NULL THEN 
       LET v_query = v_query CLIPPED, "\n AND rd.folio = ", g_folio
   END IF 
   LET v_query = v_query CLIPPED, "\n    order by rd.id_ret_matriz_derecho"
               
   LET v_cont = 1 

   DISPLAY "Query para obtener acumulado por tipo de retiro: \n", v_query

   IF ( p_modalidad <> 0 ) THEN

      -- se verifica la modalidad para ver si se pone Disposicion o PMG
      IF ( p_modalidad = 5 ) THEN
         -- disposicion
         LET g_ar_disposicion[v_cont].nss = "5-Disposición"
      ELSE
         LET g_ar_disposicion[v_cont].nss = "8-Disposición PMG"
      END IF
   ELSE
      -- se verifica si se tiene folio
      IF ( g_folio IS NOT NULL ) THEN
         -- se obtiene el proceso cod asociado al folio
         SELECT proceso_cod
         INTO   v_proceso_cod
         FROM   glo_folio
         WHERE  folio = g_folio
         
         -- si es disposicion
         IF ( v_proceso_cod = 1502 ) THEN
            LET g_ar_disposicion[v_cont].nss = "5-Disposición"
            LET p_modalidad = 5
         ELSE
            LET g_ar_disposicion[v_cont].nss = "8-Disposición PMG"
            LET p_modalidad = 8
         END IF
      END IF
   END IF
   
   LET g_ar_disposicion[v_cont].importe_viv72 = 0
   LET g_ar_disposicion[v_cont].importe_viv92 = 0
   LET g_ar_disposicion[v_cont].importe_viv97 = 0
   LET v_cont = v_cont + 1

   DISPLAY v_query
   DISPLAY  p_estado,'- \n'
           ,p_estado,'- \n'
           ,p_rechazo ,'- \n'
           ,p_rechazo,'- \n'
           ,p_con_ini ,'- \n'
           ,p_con_fin,'- \n'
           ,g_id_derechohabiente ,'- \n'
           ,g_id_derechohabiente,'- \n'
           ,g_id_derechohabiente,'- \n'
           ,g_folio,'- \n'
           ,g_folio,'- \n'
           ,g_folio

   PREPARE pr_disposicion FROM v_query
   DECLARE cur_disposicion CURSOR FOR pr_disposicion
   FOREACH cur_disposicion 
                            --USING p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin 
                                 --,g_id_derechohabiente ,g_id_derechohabiente, g_id_derechohabiente, 
                                  --g_folio, g_folio, g_folio
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
      IF ( g_ar_disposicion[v_c].id_derechohabiente IS NOT NULL ) THEN
         LET v_total_pesos  = v_total_pesos  + g_ar_disposicion[v_c].importe_viv72
         LET v_total_aivs92 = v_total_aivs92 + g_ar_disposicion[v_c].importe_viv92
         LET v_total_aivs97 = v_total_aivs97 + g_ar_disposicion[v_c].importe_viv97
         LET v_total_aivs   = v_total_aivs   + g_ar_disposicion[v_c].importe_viv92 + g_ar_disposicion[v_c].importe_viv97
      END IF  
   END FOR 

   IF ( v_total_pesos > 0 OR v_total_aivs > 0 OR v_cont - 3 > 0 ) THEN 
      LET v_con_tot  = g_ar_totales.getLength() + 1
      LET g_ar_totales[v_con_tot].id  = 0           
      
      -- se asigna como tipo de retiro la modalidad
      --LET g_ar_totales[v_con_tot].id_tipo_retiro = 5
      LET g_ar_totales[v_con_tot].id_tipo_retiro = p_modalidad
      
      -- segun el tipo de modalidad, Disposicion o PMG se pone la etiqueta
      IF ( p_modalidad = 5 ) THEN
         -- DISPOSICION
         LET g_ar_totales[v_con_tot].tipo_retiro  = "5 Retiro Disposicion de Recursos"
         LET g_ar_totales[v_con_tot].movimiento   =  212 
      ELSE
         -- DISPOSICION PMG
         LET g_ar_totales[v_con_tot].tipo_retiro  = "8 Retiro Disposicion PMG"
         LET g_ar_totales[v_con_tot].movimiento   =  822
      END IF
      
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

--realiza la carga de los registros  al array
FUNCTION fn_solicitud_transferencia(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini     DATE
 DEFINE p_con_fin     DATE  
 DEFINE p_estado      SMALLINT
 DEFINE p_rechazo     SMALLINT
 DEFINE v_cont        INTEGER
 DEFINE v_query       STRING 
 DEFINE v_total_pesos DECIMAL(14,6)
 DEFINE v_total_aivs97  DECIMAL(14,6)
 DEFINE v_c           INTEGER 
 DEFINE v_con_tot     INTEGER
 DEFINE v_desc_estado  CHAR(18)
 DEFINE v_desc_rechazo  CHAR(18) 
 
 CALL g_ar_transferencia.clear( )

 --LET v_query =  "\n SELECT rt.id_solicitud,rt.id_derechohabiente,' ',",
                --"\n rct.f_carga,rt.folio,rt.aivs_viv97,",
                --"\n ' ',rt.estado_solicitud,rt.cod_rechazo,today,rt.id_ret_matriz_derecho",
                --"\n FROM ret_transferencia rt,", 
                --"\n      ret_cza_transferencia rct ",
                --"\n WHERE (rt.estado_solicitud = ? or '0' = ?)",
                --"\n   AND (rt.cod_rechazo = ? or '0' = ?)",
                --"\n   AND rct.f_carga between ? and ?",
                --"\n   AND (rt.id_derechohabiente = ?",
                --"\n    or ? is null or ? =  '')",
                --"\n   AND (rt.folio = ?",                
                --"\n    or ? is null or ? = '')",
                --"\n   AND rt.folio = rct.folio", 
                --"\n    order by rt.id_ret_matriz_derecho"                

 LET v_query =  "\n SELECT rt.id_solicitud,rt.id_derechohabiente,' ',",
                "\n rct.f_carga,rt.folio,rt.aivs_viv97,",
                "\n ' ',rt.estado_solicitud,rt.cod_rechazo,today,rt.id_ret_matriz_derecho"
 IF g_tpo_retiro = 0 THEN 
     LET v_query = v_query CLIPPED,  "\n FROM ret_transferencia rt,", 
                                     "\n      ret_cza_transferencia rct "
 ELSE 
     LET v_query = v_query CLIPPED,  "\n FROM ret_transferencia rt INNER JOIN ret_matriz_derecho rmd ", 
                                     "\n       ON rt.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho ",
                                     "\n      AND rmd.tpo_retiro = '", g_tpo_retiro, "',",
                                     "\n      ret_cza_transferencia rct "
 END IF 
 LET v_query = v_query CLIPPED, "\n WHERE 1 = 1 "
 IF p_estado <> 0 THEN 
     LET v_query = v_query CLIPPED, "\n      AND rt.estado_solicitud = ", p_estado
 END IF
 IF  p_rechazo <> 0 THEN
     LET v_query = v_query CLIPPED, "\n      AND rt.cod_rechazo = ", p_rechazo
 END IF 

 IF p_con_ini IS NOT NULL AND p_con_fin IS NOT NULL THEN
     LET v_query = v_query CLIPPED, "\n      AND rct.f_carga between '", p_con_ini, "' AND '", p_con_fin, "'"
 END IF 
 IF g_id_derechohabiente IS NOT NULL THEN 
     LET v_query = v_query CLIPPED, "\n      AND rt.id_derechohabiente = ", g_id_derechohabiente
 END IF    
 IF g_folio IS NOT NULL THEN 
     LET v_query = v_query CLIPPED, "\n      AND rt.folio = ", g_folio
 END IF 
 LET v_query = v_query CLIPPED, "\n          AND rt.folio = rct.folio ",
                                "\n        order by rt.id_ret_matriz_derecho"                

 LET v_cont = 1 
 DISPLAY  v_query
 LET g_ar_transferencia[v_cont].nss = "6-Transferencia"
 LET g_ar_transferencia[v_cont].aivs_viv97 = 0
 LET g_ar_transferencia[v_cont].f_solicitud = "------------"
 LET v_cont = v_cont + 1
      
 PREPARE pr_transferencia FROM v_query
 DECLARE cur_transferencia CURSOR FOR pr_transferencia
 FOREACH cur_transferencia INTO g_ar_transferencia[v_cont].*
              
     SELECT nss
       INTO g_ar_transferencia[v_cont].nss 
       FROM afi_derechohabiente
      WHERE id_derechohabiente = g_ar_transferencia[v_cont].id_derechohabiente

      INITIALIZE v_desc_estado TO NULL 
     SELECT  des_corta
       INTO v_desc_estado
       FROM ret_estado_solicitud
      WHERE estado_solicitud = g_ar_transferencia[v_cont].estado_solicitud

     LET g_ar_transferencia[v_cont].estado_solicitud = g_ar_transferencia[v_cont].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED 

     INITIALIZE v_desc_rechazo TO NULL 
     SELECT  des_corta
       INTO v_desc_rechazo
       FROM ret_rechazo
      WHERE cod_rechazo = g_ar_transferencia[v_cont].cod_rechazo

     LET g_ar_transferencia[v_cont].cod_rechazo = g_ar_transferencia[v_cont].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
     LET v_cont = v_cont + 1
 END FOREACH  
 LET v_total_pesos  = 0
 LET v_total_aivs97 = 0
           
 FOR v_c = 1 TO v_cont 
   IF  g_ar_transferencia[v_c].id_derechohabiente IS NOT NULL THEN
    --LET v_total_pesos = g_ar_transferencia[v_c].aivs_viv97 + v_total_pesos
     LET v_total_aivs97  = g_ar_transferencia[v_c].aivs_viv97 + v_total_aivs97
   END IF 
 END FOR 

IF v_total_pesos > 0 OR v_total_aivs97 > 0 OR v_cont - 3 > 0 THEN 
  
    LET v_con_tot  = g_ar_totales.getLength() + 1
    LET g_ar_totales[v_con_tot].id             = 0
    LET g_ar_totales[v_con_tot].tipo_retiro    = "6 Retiro Transferencia"
    LET g_ar_totales[v_con_tot].movimiento     =  222 
    LET g_ar_totales[v_con_tot].importe        = v_total_pesos
    LET g_ar_totales[v_con_tot].importe_49     = 0
    LET g_ar_totales[v_con_tot].aivs97         = v_total_aivs97
    LET g_ar_totales[v_con_tot].id_tipo_retiro = 6

    LET g_ar_transferencia[v_cont].f_solicitud = "------------"
    LET g_ar_transferencia[v_cont].nss = "Totales "
    LET g_ar_transferencia[v_cont].aivs_viv97 = v_total_aivs97
    LET v_cont = v_cont + 1
    
    CALL g_ar_transferencia.deleteElement(v_cont)
 END IF
          RETURN v_total_pesos,v_total_aivs97
END FUNCTION  

--genera datos para el resumen general preliquidacion
FUNCTION fn_consulta_preliquidacion(p_cont)
   DEFINE p_cont        INTEGER
   DEFINE v_total_pesos DECIMAL(14,6) 
   DEFINE v_c           INTEGER 

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
   ON ACTION REPORTE
  
     CALL fn_genera_reporte_preliquida()     
     
   ON ACTION regresar 
      EXIT DISPLAY 
      
   END DISPLAY 
   CLOSE WINDOW w_consulta_preliquida
END FUNCTION

--genera datos para el resumen general Liquidacion 
FUNCTION fn_consulta_liquidacion(p_cont)
   DEFINE p_cont INTEGER
   DEFINE v_total_pesos DECIMAL(19,6) 
   DEFINE v_c           INTEGER 

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
   ON ACTION REPORTE
 
     CALL fn_genera_reporte_liquida()
     
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
 DEFINE v_cont           INTEGER
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
          IF g_modalidad = 0 THEN 
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
          IF g_modalidad = 1 THEN 
             LET v_cadena = "\n   AND movimiento in (172)"
          END IF 

          --habilita la preliquidacion para fondo ahorro
          IF g_modalidad = 2 THEN 
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

             --DISPLAY v_query
          END IF

           --habilita la preliquidacion para Ley73
          IF g_modalidad = 3 THEN 
             LET v_cadena = "\n   AND movimiento in (192)"
          END IF

          --habilita la preliquidacion para tipo_n
          IF g_modalidad = 4 THEN 

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
          IF g_modalidad = 5 THEN 
             IF g_tpo_retiro <> "0" THEN 
                 LET v_query =  "\n SELECT c.id_derechohabiente,' ',",
                                "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                                "\n FROM ret_preliquida c, ret_disposicion rt, ret_matriz_derecho rmd",
                                "\n WHERE f_liquida between ? and ?",
                                "\n   AND (c.id_derechohabiente = ?",
                                "\n    or ? is null or ? =  '')",
                                "\n   AND (folio_liquida = ?",
                                "\n    or ? is null or ? = '')",
                                "\n   AND c.id_derechohabiente = rt.id_derechohabiente ",
                                "\n   AND rt.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho  ",
                                "\n   AND rmd.tpo_retiro = '", g_tpo_retiro, "'",
                                "\n   AND c.folio_liquida = rt.folio "
                 LET v_cadena = "\n   AND c.movimiento in (212)"
             ELSE 
                 LET v_cadena = "\n   AND movimiento in (212)"
             END IF 
          END IF 

          --habilita la preliquidacion para transferenicia
          IF g_modalidad = 6 THEN 
             IF g_tpo_retiro <> "0" THEN 
                 LET v_query =  "\n SELECT c.id_derechohabiente,' ',",
                                "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                                "\n FROM ret_preliquida c, ret_transferencia rt, ret_matriz_derecho rmd",
                                "\n WHERE f_liquida between ? and ?",
                                "\n   AND (c.id_derechohabiente = ?",
                                "\n    or ? is null or ? =  '')",
                                "\n   AND (folio_liquida = ?",
                                "\n    or ? is null or ? = '')",
                                "\n   AND c.id_derechohabiente = rt.id_derechohabiente ",
                                "\n   AND rt.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho  ",
                                "\n   AND rmd.tpo_retiro = '", g_tpo_retiro, "'",
                                "\n   AND c.folio_liquida = rt.folio "
                 LET v_cadena = "\n   AND c.movimiento in (222)"
             ELSE 
                 LET v_cadena = "\n   AND movimiento in (222)"
             END IF 
          END IF 

          --habilita la preliquidacion para fortalecimiento
          IF g_modalidad = 7 THEN 
             LET v_cadena = "\n   AND movimiento in (462)"
          END IF 

          LET v_query  = v_query CLIPPED || v_cadena CLIPPED             
                           
          --DISPLAY   v_query
          --DISPLAY   v_cadena
          LET v_pesos_total = 0 
          PREPARE pr_preliquida FROM v_query
          DECLARE cur_preliquida CURSOR FOR pr_preliquida

          CASE g_modalidad
          
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

--genera la consulta para la parte de liquidacion
FUNCTION fn_liquidacion(p_con_ini,p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_query          VARCHAR(2000)
 DEFINE v_cont           INTEGER
 DEFINE v_cadena         VARCHAR(200)
 DEFINE v_pesos_total    LIKE cta_fondo72.importe
 
 CALL g_ar_liquida.clear( )
      

 LET v_cont = 1

 LET v_query =  "\n SELECT id_derechohabiente,' ',",
                "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                "\n FROM ",
                "\n      (SELECT id_derechohabiente, id_referencia, movimiento, ",
                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                "\n         FROM cta_movimiento12 ",
                "\n       UNION ALL ",
                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                "\n         FROM cta_movimiento13 ",
                "\n       UNION ALL ",
                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                "\n         FROM cta_movimiento14 ",
                "\n       UNION ALL ",
                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                "\n         FROM cta_movimiento ",
                "\n       )             ",
                "\n WHERE f_liquida between ? and ?",
                "\n   AND (id_derechohabiente = ?",
                "\n    or ? is null or ? =  '')",
                "\n   AND (folio_liquida = ?",
                "\n    or ? is null or ? = '')"

          --habilita la liquidacion para todos los retiros 
          IF g_modalidad = 0 THEN 
             LET v_query =   "\n SELECT id_derechohabiente,' ', ",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                             "\n FROM ",
                             "\n      (SELECT id_derechohabiente, id_referencia, movimiento, ",
                             "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                             "\n         FROM cta_movimiento12 ",
                             "\n       UNION ALL ",
                             "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                             "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                             "\n         FROM cta_movimiento13 ",
                             "\n       UNION ALL ",
                             "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                             "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                             "\n         FROM cta_movimiento14 ",
                             "\n       UNION ALL ",
                             "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                             "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                             "\n         FROM cta_movimiento ",
                             "\n       )             ",
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
          IF g_modalidad = 1 THEN 
             LET v_cadena = "\n   AND movimiento in (172)"
          END IF 

          --habilita la liquidacion para fondo ahorro
          IF g_modalidad = 2 THEN 
             LET v_query =  "\n SELECT 0, '' ,",
                            "\n movimiento,f_liquida,id_referencia,folio_liquida,importe",
                            "\n FROM cta_fondo72",
                            "\n WHERE f_liquida between ? and ?",
                            "\n   AND (folio_liquida = ?",
                            "\n    or ? is null or ? = '')"
                            
             LET v_cadena = "\n   AND movimiento in (182)"
          END IF

            --habilita la liquidacion para Ley73
          IF g_modalidad = 3 THEN 
             LET v_cadena = "\n   AND movimiento in (192)"
          END IF

          --habilita la liquidacion para tipo_n
          IF g_modalidad = 4 THEN 
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
          IF g_modalidad = 5 THEN 
             IF g_tpo_retiro <> "0" THEN 
                 LET v_query =  "\n SELECT c.id_derechohabiente,' ',",
                                "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                                "\n FROM ",
                                "\n      (SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento12 ",
                                "\n       UNION ALL ",
                                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento13 ",
                                "\n       UNION ALL ",
                                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento14 ",
                                "\n       UNION ALL ",
                                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento ",
                                "\n       )  c,         ",
                                "\n       ret_disposicion rt, ret_matriz_derecho rmd",
                                "\n WHERE f_liquida between ? and ?",
                                "\n   AND (c.id_derechohabiente = ?",
                                "\n    or ? is null or ? =  '')",
                                "\n   AND (folio_liquida = ?",
                                "\n    or ? is null or ? = '')",
                                "\n   AND c.id_derechohabiente = rt.id_derechohabiente ",
                                "\n   AND rt.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho  ",
                                "\n   AND rmd.tpo_retiro = '", g_tpo_retiro, "'",
                                "\n   AND c.folio_liquida = rt.folio "
                 LET v_cadena = "\n   AND c.movimiento in (212)"
             ELSE 
                 LET v_cadena = "\n   AND movimiento in (212)"
             END IF 
          END IF 

          --habilita la liquidacion para transferenicia
          IF g_modalidad = 6 THEN 
             IF g_tpo_retiro <> "0" THEN 
                 LET v_query =  "\n SELECT c.id_derechohabiente,' ',",
                                "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                                "\n FROM ",
                                "\n      (SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento12 ",
                                "\n       UNION ALL ",
                                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento13 ",
                                "\n       UNION ALL ",
                                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento14 ",
                                "\n       UNION ALL ",
                                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                                "\n         FROM cta_movimiento ",
                                "\n       )  c,         ",
                                "\n       ret_transferencia rt, ret_matriz_derecho rmd",
                                "\n WHERE f_liquida between ? and ?",
                                "\n   AND (c.id_derechohabiente = ?",
                                "\n    or ? is null or ? =  '')",
                                "\n   AND (folio_liquida = ?",
                                "\n    or ? is null or ? = '')",
                                "\n   AND c.id_derechohabiente = rt.id_derechohabiente ",
                                "\n   AND rt.id_ret_matriz_derecho = rmd.id_ret_matriz_derecho  ",
                                "\n   AND rmd.tpo_retiro = '", g_tpo_retiro, "'",
                                "\n   AND c.folio_liquida = rt.folio "
                 LET v_cadena = "\n   AND c.movimiento in (222)"
             ELSE 
                 LET v_cadena = "\n   AND movimiento in (222)"
             END IF
             DISPLAY "Query Liquidacion", v_query
          END IF 

                    --habilita la preliquidacion para fortalecimiento
          IF g_modalidad = 7 THEN 
             LET v_cadena = "\n   AND movimiento in (462)"
          END IF 

          LET v_query  = v_query CLIPPED || v_cadena CLIPPED             
                           
          DISPLAY  v_query
          LET v_pesos_total = 0
           
          PREPARE pr_liquida FROM v_query
          DECLARE cur_liquida CURSOR FOR pr_liquida

          CASE g_modalidad
          
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

--pantalla del resumen general por modalidad
FUNCTION fn_consulta_modalidad(p_total_pesos,p_total_pesos49,p_total_aivs92,p_total_aivs97)
DEFINE p_total_pesos  DECIMAL(19,6)  ,
       p_total_pesos49  DECIMAL(19,6),
       p_total_aivs92 DECIMAL(19,6)  ,
       p_total_aivs97 DECIMAL(19,6)  ,
       v_c            INTEGER       ,
       v_cont_ar      INTEGER 
   
   OPEN WINDOW w_consulta_totales WITH FORM "RETF025"
   
   INPUT ARRAY g_ar_totales FROM t_totales.*
   ATTRIBUTES ( UNBUFFERED ,WITHOUT DEFAULTS ,APPEND ROW = FALSE 
                ,DELETE ROW = FALSE , INSERT ROW = FALSE ,ACCEPT = FALSE,CANCEL = FALSE )
   
      BEFORE INPUT 
         LET v_c = g_ar_totales.getLength()
         IF ( v_c <= 0 ) THEN
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
            IF ( g_ar_totales[v_c].id = 1 ) THEN
               LET v_cont_ar = v_cont_ar + 1
            END IF 
         END FOR  
         
         LET v_cont_ar = g_ar_totales.getLength()

         IF v_cont_ar = 1 THEN
            DISPLAY "entro a solo 1"
            FOR v_c = 1 TO g_ar_totales.getLength()
               IF g_ar_totales[v_c].id = 1 THEN
                  --seleccion de modulo cuando solo un modulo esta seleccionado
                  CASE g_ar_totales[v_c].id_tipo_retiro
                     WHEN 1
                        --solo infonavit  
                        CALL fn_consulta_solo_infonavit(g_ar_solo_infonavit.getLength())
                   
                     WHEN 2
                        --fondo_ahorro 
                        CALL fn_consulta_fondo_ahorro(g_ar_fondo_ahorro.getLength())
                       
                     WHEN 3
                        --Ley73
                        CALL fn_consulta_ley73(g_ar_ley73.getLength())
                       
                     WHEN 4 
                        --tipo_n
                        CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)
                     
                     WHEN 5
                        --disposicion 
                        display "consultando disposicion"
                        CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)

                     WHEN 8
                        --disposicion PMG
                        display "consultando PMG"
                        CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)
                       
                     WHEN 6
                        --transferencia
                        CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)

                     WHEN 7
                        --fortalecimiento
                        CALL fn_consulta_fortalecimiento(g_ar_fortalecimiento.getLength())

                     OTHERWISE
                        EXIT FOR

                  END CASE
               END IF 
            END FOR
         ELSE
            DISPLAY "entro a combinado"
            IF ( v_cont_ar > 0 ) THEN
               --si existe mas de un seleccionado entra a esta opcion 
               CALL fn_consulta_combinado_modulos()
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
   DEFINE p_cont             INTEGER
   DEFINE v_c                INTEGER 
   DEFINE v_cont             INTEGER 
   DEFINE v_cont_ctrl        INTEGER 
   DEFINE v_total_pesos      DECIMAL(14,6)
   DEFINE v_total_pesos49      DECIMAL(14,6)
   DEFINE v_total_aivs92     DECIMAL(14,6)
   DEFINE v_total_aivs97     DECIMAL(14,6)
   DEFINE v_cont_encabezados INTEGER
   DEFINE v_cont_1           INTEGER 
   DEFINE g_usuario_cod  LIKE seg_usuario.usuario_cod  --Para mandar usuario a reporte
   
   LET p_cont = 0
   LET v_cont_ctrl = 1
   LET v_cont_1 = 1
   
   FOR v_c = 1 TO g_ar_totales.getLength()
            IF g_ar_totales[v_c].id = 1 THEN
              CASE g_ar_totales[v_c].id_tipo_retiro
                 WHEN 1                 
                 LET v_cont_encabezados = v_cont_encabezados + 1 
                   LET p_cont = p_cont + g_ar_solo_infonavit.getLength()
                   FOR v_cont = 1 TO g_ar_solo_infonavit.getLength()
                      IF g_ar_solo_infonavit[v_cont].id_derechohabiente IS NOT NULL  THEN 
                         LET  g_ar_combinado[v_cont].id_tipo_retiro = v_cont_1
                         LET  g_ar_combinado[v_cont].pes_viv72          = 0
                         LET  g_ar_combinado[v_cont].pes_viv49          = 0
                         LET  g_ar_combinado[v_cont].aivs_viv92         = 0 
                         LET v_cont_1 = v_cont_1 + 1
                      ELSE
                         IF g_ar_solo_infonavit[v_cont].pes_viv97 = 0 THEN 
                            LET g_ar_solo_infonavit[v_cont].pes_viv97 = NULL
                         END IF  
                         
                         IF g_ar_solo_infonavit[v_cont].acc_viv97 = 0 THEN 
                            LET g_ar_solo_infonavit[v_cont].acc_viv97 = NULL
                         END IF
                      END IF 
                      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_solo_infonavit[v_cont].id_derechohabiente
                      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_solo_infonavit[v_cont].id_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_solo_infonavit[v_cont].f_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_solo_infonavit[v_cont].nss
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_solo_infonavit[v_cont].acc_viv97
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_solo_infonavit[v_cont].estado_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_solo_infonavit[v_cont].cod_rechazo
                      LET v_cont_ctrl = v_cont_ctrl + 1
                  END FOR
                 WHEN 2
                 LET v_cont_encabezados = v_cont_encabezados +1 
                   LET p_cont = p_cont + g_ar_fondo_ahorro.getLength()
                   FOR v_cont = 1 TO g_ar_fondo_ahorro.getLength()
                      IF g_ar_fondo_ahorro[v_cont].id_derechohabiente IS NOT NULL THEN 
                         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0 
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
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
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_fondo_ahorro[v_cont].estado_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_fondo_ahorro[v_cont].cod_rechazo
                      LET v_cont_ctrl = v_cont_ctrl + 1
                  END FOR 
                WHEN 3
                 LET v_cont_encabezados = v_cont_encabezados +1 
                   LET p_cont = p_cont + g_ar_ley73.getLength()
                   FOR v_cont = 1 TO g_ar_ley73.getLength()
                      IF g_ar_ley73[v_cont].id_derechohabiente IS NOT NULL THEN 
                         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                         LET v_cont_1 = v_cont_1 + 1
                      ELSE
                         IF     g_ar_ley73[v_cont].importe_viv92 = 0
                            AND g_ar_ley73[v_cont].importe_viv97 = 0 THEN
                            
                            LET g_ar_ley73[v_cont].importe_viv92 = NULL
                            LET g_ar_ley73[v_cont].importe_viv97 = NULL
                            LET g_ar_ley73[v_cont].importe_viv92 = NULL
                            LET g_ar_ley73[v_cont].importe_viv97 = NULL
                         END IF  
                      END IF
                      --LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont
                      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_ley73[v_cont].id_derechohabiente
                      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_ley73[v_cont].id_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_ley73[v_cont].f_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_ley73[v_cont].nss
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_ley73[v_cont].aivs_viv92
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_ley73[v_cont].aivs_viv97
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_ley73[v_cont].estado_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_ley73[v_cont].cod_rechazo
                      LET v_cont_ctrl = v_cont_ctrl + 1
                  END FOR 
                   
                WHEN 4
                 LET v_cont_encabezados = v_cont_encabezados +1 
                   LET p_cont = p_cont + g_ar_tipo_n.getLength()
                   FOR v_cont = 1 TO g_ar_tipo_n.getLength()
                      IF g_ar_tipo_n[v_cont].id_derechohabiente IS NOT NULL THEN 
                         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                         LET v_cont_1 = v_cont_1 + 1
                      ELSE
                         IF g_ar_tipo_n[v_cont].aivs_viv92 = 0 THEN 
                            LET g_ar_tipo_n[v_cont].aivs_viv92 = NULL
                         END IF  
                      END IF
                      --LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont
                      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_tipo_n[v_cont].id_derechohabiente
                      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_tipo_n[v_cont].id_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_tipo_n[v_cont].f_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_tipo_n[v_cont].nss
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_tipo_n[v_cont].aivs_viv92
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_tipo_n[v_cont].estado_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_tipo_n[v_cont].cod_rechazo
                      LET v_cont_ctrl = v_cont_ctrl + 1
                  END FOR 
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
                 WHEN 6
                 LET v_cont_encabezados = v_cont_encabezados +1 
                   LET p_cont = p_cont + g_ar_transferencia.getLength()
                   FOR v_cont = 1 TO g_ar_transferencia.getLength()
                      IF g_ar_transferencia[v_cont].id_derechohabiente IS NOT NULL THEN 
                         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                         LET v_cont_1 = v_cont_1 + 1
                      ELSE
                         IF g_ar_transferencia[v_cont].aivs_viv97 = 0 THEN 
                            LET g_ar_transferencia[v_cont].aivs_viv97 = NULL
                         END IF  
                      END IF
                      --LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont
                      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_transferencia[v_cont].id_derechohabiente
                      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_transferencia[v_cont].id_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_transferencia[v_cont].f_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_transferencia[v_cont].nss
                      IF g_ar_transferencia[v_cont].id_derechohabiente IS NULL THEN 
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = NULL
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = NULL  
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = NULL 
                      ELSE
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                      END IF 
                      LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = g_ar_transferencia[v_cont].aivs_viv97
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_transferencia[v_cont].estado_solicitud
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_transferencia[v_cont].cod_rechazo
                      LET v_cont_ctrl = v_cont_ctrl + 1
                  END FOR 
                  
                WHEN 7                                                                                                   
                 LET v_cont_encabezados = v_cont_encabezados +1                                                          
                   LET p_cont = p_cont + g_ar_fortalecimiento.getLength()                                                  
                   FOR v_cont = 1 TO g_ar_fortalecimiento.getLength()                                                      
                      IF g_ar_fortalecimiento[v_cont].id_derechohabiente IS NOT NULL THEN                                  
                         LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1                                      
                         LET v_cont_1 = v_cont_1 + 1                                                                     
                      ELSE                                                                                               
                         IF g_ar_fortalecimiento[v_cont].importe_viv = 0 THEN                                               
                            LET g_ar_fortalecimiento[v_cont].importe_viv = NULL                                             
                         END IF                                                                                          
                      END IF                                                                                             
                      
                      LET  g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_fortalecimiento[v_cont].id_derechohabiente
                      LET  g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_fortalecimiento[v_cont].id_solicitud      
                      LET  g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_fortalecimiento[v_cont].f_solicitud       
                      LET  g_ar_combinado[v_cont_ctrl].nss                = g_ar_fortalecimiento[v_cont].nss               
                      IF g_ar_fortalecimiento[v_cont].id_derechohabiente IS NULL THEN                                      
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = NULL
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = NULL                         
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = NULL
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = NULL                         
                      ELSE                                                                                               
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                         LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = g_ar_fortalecimiento[v_cont].importe_viv
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv92         = 0
                         LET  g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
                         
                      END IF
                      
                      LET  g_ar_combinado[v_cont_ctrl].pes_viv49          = g_ar_fortalecimiento[v_cont].importe_viv
                      LET  g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_fortalecimiento[v_cont].estado_solicitud  
                      LET  g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_fortalecimiento[v_cont].cod_rechazo       
                      LET v_cont_ctrl = v_cont_ctrl + 1                                                                  
                  END FOR     
                  
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
   --Cambio
   ON ACTION REPORTE
  
   CALL fn_genera_reporte()
       
   ON ACTION Regresar 
      EXIT DISPLAY 
      
   END DISPLAY 
   CLOSE WINDOW w_consulta_detalle
   CALL g_ar_combinado.clear()
END FUNCTION 

--funcion genera consulta de retiros por modalidad 
--para seleccion multiple en los casos de tipo n, disposicion y transferencia 
FUNCTION fn_consulta_retiro(id_tipo_retiro)
DEFINE p_total_pesos     DECIMAL(14,6),
       p_total_pesos49   DECIMAL(14,6),
       p_total_aivs92    DECIMAL(14,6),
       p_total_aivs97    DECIMAL(14,6),
       v_c               INTEGER     ,
       v_cont_ar         INTEGER     ,
       v_cu              INTEGER     ,
       v_bnd_tipo_retiro CHAR(1)      ,
       id_tipo_retiro    SMALLINT     ,
       v_count_retiro    INTEGER     ,
       v_id_retiro       SMALLINT     ,
       v_query           CHAR(200)    ,
       v_movimiento      SMALLINT -- para movimientos distintos
   
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
   
   -- retiro PMG aunque es 8, en realidad es una disposicion, por lo que se pasa como 5
   -- y se asigna el movimiento del retiro en turno
   IF ( id_tipo_retiro = 8 ) THEN
      LET id_tipo_retiro = 5
      LET v_movimiento = 822
   END IF
   
   IF ( id_tipo_retiro = 5 ) THEN
      LET v_movimiento = 212
   END IF

   LET v_query =  " SELECT COUNT(UNIQUE tpo_retiro) ",
                  "\n FROM ret_matriz_derecho ",
                  "\n WHERE (modalidad_retiro = ",id_tipo_retiro,
                  "\n OR '0' = ",id_tipo_retiro,")"
   DISPLAY v_query 
   PREPARE  c_cuenta_tipo_retiros FROM v_query

   EXECUTE c_cuenta_tipo_retiros INTO  v_count_retiro
   
   DECLARE c_ret_matriz_derecho CURSOR FOR 
                                       SELECT id_ret_matriz_derecho,tpo_retiro,modalidad_retiro
                                        FROM ret_matriz_derecho
                                       WHERE (modalidad_retiro = id_tipo_retiro
                                          OR '0' = id_tipo_retiro)
                                         AND (tpo_retiro = g_tpo_retiro 
                                          OR "0" = g_tpo_retiro)
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

      IF ( id_tipo_retiro = 4 OR id_tipo_retiro = 0 ) THEN 
         FOR v_c = 1 TO g_ar_tipo_n.getLength()
            IF g_ar_tipo_n[v_c].id_ret_matriz_derecho = g_ar_retiro[v_cont_ar].id_matriz_derecho THEN

               INSERT INTO tmp_retiro VALUES (0, --g_ar_retiro[v_cont_ar].id,
                                              g_ar_retiro[v_cont_ar].desc_tipo_retiro,
                                              202,
                                              0,                                    --importe72
                                              0,
                                              g_ar_tipo_n[v_c].aivs_viv92,          --aivs92
                                              0,                                    --aivs97
                                              g_ar_retiro[v_cont_ar].tipo_retiro,
                                              g_ar_tipo_n[v_c].id_ret_matriz_derecho)

            END IF 
         END FOR
      END IF

      -- disposicion y PMG
      IF id_tipo_retiro = 5 OR id_tipo_retiro = 0 THEN
      
         FOR v_c = 1 TO g_ar_disposicion.getLength()
            IF g_ar_disposicion[v_c].id_ret_matriz_derecho = g_ar_retiro[v_cont_ar].id_matriz_derecho THEN
               INSERT INTO tmp_retiro VALUES (0, --g_ar_retiro[v_cont_ar].id,
                                              g_ar_retiro[v_cont_ar].desc_tipo_retiro,
                                              v_movimiento,
                                              g_ar_disposicion[v_c].importe_viv72,
                                              0,
                                              g_ar_disposicion[v_c].importe_viv92, 
                                              g_ar_disposicion[v_c].importe_viv97,
                                              g_ar_retiro[v_cont_ar].tipo_retiro,
                                              g_ar_disposicion[v_c].id_ret_matriz_derecho)

            END IF 
         END FOR 
      END IF
      
      IF id_tipo_retiro = 6 OR id_tipo_retiro = 0 THEN 
         FOR v_c = 1 TO g_ar_transferencia.getLength()
            IF g_ar_transferencia[v_c].id_ret_matriz_derecho = g_ar_retiro[v_cont_ar].id_matriz_derecho THEN
               INSERT INTO tmp_retiro VALUES (0, --g_ar_retiro[v_cont_ar].id,
                                              g_ar_retiro[v_cont_ar].desc_tipo_retiro,
                                              222,
                                              0,
                                              0,
                                              0,
                                              g_ar_transferencia[v_c].aivs_viv97,
                                              g_ar_retiro[v_cont_ar].tipo_retiro,
                                              g_ar_transferencia[v_c].id_ret_matriz_derecho)
           END IF 
         END FOR
      END IF
      --IF id_tipo_retiro = 7 OR id_tipo_retiro = 0 THEN                                                      
         --FOR v_c = 1 TO g_ar_fortalecimiento.getLength()                                                      
            --IF g_ar_fortalecimiento[v_c].id_derechohabiente = g_ar_retiro[v_cont_ar]. THEN
                --INSERT INTO tmp_retiro VALUES (0, --g_ar_retiro[v_cont_ar].id,                              
                                               --g_ar_retiro[v_cont_ar].desc_tipo_retiro,                     
                                               --999,                                                         
                                               --0,                                                           
                                               --0,                                                           
                                               --g_ar_fortalecimiento[v_c].importe_viv,                          
                                               --g_ar_retiro[v_cont_ar].tipo_retiro,                          
                                               --99)      --erv TIPO DE RETIRO QE LE TOCA DENTRO DE LA MATRIZ DERECHO          
                                                                                                            --
                                                                                                            --
           --END IF --ERV checa5r como esta funcionandoesata parte                                                                                           
         --END FOR                                                                                            
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
                                    --id_matriz_derecho
                           FROM tmp_retiro
                           GROUP BY tipo_retiro,desc_tipo_retiro,movimiento
                           ORDER BY tipo_retiro,desc_tipo_retiro,movimiento
                           
   CALL g_ar_retiro.clear()
   LET v_cont_ar       = 1
   LET p_total_pesos   = 0.0
   LET p_total_pesos49 = 0.0
   LET p_total_aivs92  = 0.0
   LET p_total_aivs97  = 0.0
    
   FOREACH c_retiro 
   INTO g_ar_retiro[v_cont_ar].id,
         g_ar_retiro[v_cont_ar].desc_tipo_retiro,
         g_ar_retiro[v_cont_ar].importe72,
         g_ar_retiro[v_cont_ar].importe49,
         g_ar_retiro[v_cont_ar].aivs92,
         g_ar_retiro[v_cont_ar].aivs97,
         g_ar_retiro[v_cont_ar].tipo_retiro ,
         g_ar_retiro[v_cont_ar].movimiento
         --g_ar_retiro[v_cont_ar].id_matriz_derecho
       IF v_count_retiro = 1 THEN 
          LET g_ar_retiro[v_cont_ar].id = 1
       ELSE 
          LET g_ar_retiro[v_cont_ar].id = 0
       END IF
       LET p_total_pesos    = p_total_pesos   + g_ar_retiro[v_cont_ar].importe72
       LET p_total_pesos49  = p_total_pesos49 + g_ar_retiro[v_cont_ar].importe49
       LET p_total_aivs92   = p_total_aivs92 + g_ar_retiro[v_cont_ar].aivs92
       LET p_total_aivs97   = p_total_aivs97 + g_ar_retiro[v_cont_ar].aivs97
       --DISPLAY p_total_pesos ,g_ar_retiro[v_cont_ar].importe
       LET v_cont_ar = v_cont_ar +1
           
   END FOREACH 
   
   CALL g_ar_retiro.deleteElement(v_cont_ar)
   
   LET v_cont_ar = v_cont_ar - 1
  
   OPEN WINDOW w_consulta_retiro WITH FORM "RETF029"
   
   INPUT ARRAY  g_ar_retiro FROM t_retiro.*
   ATTRIBUTES (UNBUFFERED ,WITHOUT DEFAULTS ,APPEND ROW = FALSE 
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
         
         IF ( v_cont_ar <= 0 ) THEN
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
   DEFINE p_cont         INTEGER
   DEFINE v_c            INTEGER
   DEFINE v_cont         INTEGER
   DEFINE v_cont_ctrl    INTEGER
   DEFINE v_total_pesos  DECIMAL(14,6)
   DEFINE v_total_pesos49  DECIMAL(14,6)
   DEFINE v_total_aivs92 DECIMAL(14,6)
   DEFINE v_total_aivs97 DECIMAL(14,6)
   DEFINE id_tipo_retiro INTEGER
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
  DEFINE v_cont_1                       INTEGER
  DEFINE v_contx                        INTEGER
  DEFINE v_cont_control                 INTEGER
  DEFINE v_encabezado                   INTEGER
  DEFINE v_cont_mostrar                 INTEGER
  DEFINE v_totales                      INTEGER
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

         --  WHEN 4 tipo_n
         LET v_encabezado = TRUE    
         FOR v_cont = 1 TO g_ar_tipo_n.getLength()
            DECLARE c_tmp_retiro CURSOR FOR 
                                     SELECT *
                                       FROM tmp_retiro
                                      WHERE tipo_retiro = g_ar_retiro[v_cont_control].tipo_retiro
                                      ORDER BY 1
                                      
            --recorre todos los registros comparando los retiros que coinciden con los del corte en turno
            FOREACH c_tmp_retiro INTO re_tmp_retiro.*
               IF re_tmp_retiro.id_matriz_derecho = g_ar_tipo_n[v_cont].id_ret_matriz_derecho THEN
                  IF g_ar_tipo_n[v_cont].id_ret_matriz_derecho <> v_ya_paso THEN

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
                              LET v_total_pesos   = 0
                              LET v_total_pesos49 = 0
                              LET v_total_aivs92  = 0
                              LET v_total_aivs97  = 0
                           END IF 
                           
                           LET g_ar_combinado[v_cont_ctrl].nss = re_tmp_retiro.desc_tipo_retiro
                           LET v_desc_encabezados = re_tmp_retiro.desc_tipo_retiro
                           LET v_cont_ctrl = v_cont_ctrl + 1
                           LET v_cont_mostrar = v_cont_mostrar + 1
                           LET v_encabezado = FALSE
                           LET v_totales = TRUE
                        END IF 
                        
                     FOR v_contx = v_cont TO g_ar_tipo_n.getLength()
                        IF g_ar_tipo_n[v_contx].id_ret_matriz_derecho = g_ar_tipo_n[v_cont].id_ret_matriz_derecho THEN
                           IF g_ar_tipo_n[v_cont].id_derechohabiente IS NOT NULL  THEN 
                              LET  g_ar_combinado[v_cont_ctrl].id_tipo_retiro = v_cont_1
                              LET v_cont_1 = v_cont_1 + 1
                           ELSE
                              IF g_ar_tipo_n[v_cont].aivs_viv92 = 0 THEN 
                                  LET g_ar_tipo_n[v_cont].aivs_viv92 = NULL
                              END IF  
                           END IF  
                           --se carga el array para mostrar con los regirtros seleccionados ´para mostrar
                           LET g_ar_combinado[v_cont_ctrl].id_derechohabiente = g_ar_tipo_n[v_contx].id_derechohabiente
                           LET g_ar_combinado[v_cont_ctrl].id_solicitud       = g_ar_tipo_n[v_contx].id_solicitud
                           LET g_ar_combinado[v_cont_ctrl].f_solicitud        = g_ar_tipo_n[v_contx].f_solicitud
                           LET g_ar_combinado[v_cont_ctrl].nss                = g_ar_tipo_n[v_contx].nss
                           LET g_ar_combinado[v_cont_ctrl].pes_viv72          = 0
                           LET g_ar_combinado[v_cont_ctrl].pes_viv49          = 0
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv92         = g_ar_tipo_n[v_contx].aivs_viv92
                           LET g_ar_combinado[v_cont_ctrl].aivs_viv97         = 0
                           LET g_ar_combinado[v_cont_ctrl].estado_solicitud   = g_ar_tipo_n[v_contx].estado_solicitud
                           LET g_ar_combinado[v_cont_ctrl].cod_rechazo        = g_ar_tipo_n[v_contx].cod_rechazo
                           LET v_total_pesos  = v_total_pesos  + g_ar_combinado[v_cont_ctrl].pes_viv72
                           LET v_total_pesos49  = v_total_pesos49  + g_ar_combinado[v_cont_ctrl].pes_viv49
                           LET v_total_aivs92 = v_total_aivs92 + g_ar_combinado[v_cont_ctrl].aivs_viv92
                           LET v_total_aivs97 = v_total_aivs97 + g_ar_combinado[v_cont_ctrl].aivs_viv97                           
                           LET v_cont_ctrl = v_cont_ctrl + 1
                        END IF 
                     END FOR 
                 END IF
                 LET v_ya_paso     = re_tmp_retiro.id_matriz_derecho
               END IF
            END FOREACH
         END FOR

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
                           LET v_cont_ctrl     = v_cont_ctrl + 1   
                           LET v_cont_mostrar  = v_cont_mostrar + 1 
                           LET v_total_pesos   = 0
                           LET v_total_pesos49 = 0
                           LET v_total_aivs92  = 0
                           LET v_total_aivs97  = 0
                        END IF 

                        LET g_ar_combinado[v_cont_ctrl].nss = re_tmp_retiro.desc_tipo_retiro
                        LET v_desc_encabezados = re_tmp_retiro.desc_tipo_retiro
                        LET v_cont_ctrl        = v_cont_ctrl + 1
                        LET v_cont_mostrar     = v_cont_mostrar + 1
                        LET v_encabezado       = FALSE
                        LET v_totales          = TRUE
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
                           LET v_total_pesos    = v_total_pesos  + g_ar_combinado[v_cont_ctrl].pes_viv72
                           LET v_total_pesos49  = v_total_pesos49  + g_ar_combinado[v_cont_ctrl].pes_viv49
                           LET v_total_aivs92   = v_total_aivs92 + g_ar_combinado[v_cont_ctrl].aivs_viv92
                           LET v_total_aivs97   = v_total_aivs97 + g_ar_combinado[v_cont_ctrl].aivs_viv97
                           LET v_cont_ctrl      = v_cont_ctrl + 1
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
                           LET v_cont_ctrl     = v_cont_ctrl + 1   
                           LET v_cont_mostrar  = v_cont_mostrar + 1 
                           LET v_total_pesos   = 0
                           LET v_total_pesos49 = 0
                           LET v_total_aivs92  = 0
                           LET v_total_aivs97  = 0
                        END IF 

                        LET g_ar_combinado[v_cont_ctrl].nss = re_tmp_retiro.desc_tipo_retiro
                        LET v_desc_encabezados = re_tmp_retiro.desc_tipo_retiro
                        LET v_cont_ctrl        = v_cont_ctrl + 1
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
           LET v_cont_ctrl     = v_cont_ctrl + 1  
           LET v_cont_mostrar  = v_cont_mostrar + 1 
           LET v_totales       = FALSE
           LET v_total_pesos   = 0
           LET v_total_pesos49 = 0
           LET v_total_aivs92  = 0
           LET v_total_aivs97  = 0
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
           LET v_total_pesos = v_total_pesos + g_ar_combinado[v_c].pes_viv72
           LET v_total_pesos49 = v_total_pesos49 + g_ar_combinado[v_c].pes_viv49
           LET v_total_aivs92 = v_total_aivs92 + g_ar_combinado[v_c].aivs_viv92
           LET v_total_aivs97 = v_total_aivs97 + g_ar_combinado[v_c].aivs_viv97
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
       DISPLAY v_total_pesos TO ff_total
       DISPLAY v_total_pesos49 TO ff_total3
       DISPLAY v_total_aivs92 TO ff_total1
       DISPLAY v_total_aivs97 TO ff_total2
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

FUNCTION fn_consulta_solo_infonavit(p_cont)
   DEFINE p_cont        INTEGER
   DEFINE v_total_pesos DECIMAL(19,6)
   DEFINE v_total_aivs  DECIMAL(19,6)
   DEFINE v_c           INTEGER 

   IF p_cont <= 0 THEN 
     RETURN 
   END IF

   LET v_total_pesos = 0
   LET v_total_aivs = 0
   FOR v_c = 1 TO p_cont
      IF g_ar_solo_infonavit[v_c].id_derechohabiente IS NOT NULL THEN 
         LET v_total_pesos = v_total_pesos + g_ar_solo_infonavit[v_c].pes_viv97
         LET v_total_aivs  = v_total_aivs + g_ar_solo_infonavit[v_c].acc_viv97
      ELSE
         IF g_ar_solo_infonavit[v_c].pes_viv97 = 0 THEN
            LET g_ar_solo_infonavit[v_c].pes_viv97 = NULL
         END IF
         IF g_ar_solo_infonavit[v_c].acc_viv97 = 0 THEN 
            LET g_ar_solo_infonavit[v_c].acc_viv97 = NULL
         END IF
      END IF
   END FOR

   OPEN WINDOW w_consulta_solo_infonavit WITH FORM "RETF021"
   DISPLAY ARRAY  g_ar_solo_infonavit TO t_solo_infonavit.*
           ATTRIBUTE (ACCEPT = FALSE ,CANCEL  = FALSE )

    BEFORE DISPLAY 
     DISPLAY "Retiro Sólo Infonavit" TO ff_desc_consul
     DISPLAY "Total Registros: "||p_cont -2 TO ff_cont_reg
     DISPLAY v_total_aivs TO ff_total_aivs
     DISPLAY v_total_pesos TO ff_total
     IF p_cont <= 0 THEN
       CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
       EXIT DISPLAY  
     END IF 
     
   ON ACTION REPORTE
  
   CALL fn_genera_reporte_solo_inf()

   ON ACTION regresar 
      EXIT DISPLAY 

   END DISPLAY 
   CLOSE WINDOW w_consulta_solo_infonavit
END FUNCTION

FUNCTION fn_consulta_fondo_ahorro(p_cont)
   DEFINE p_cont INTEGER
   DEFINE v_total_pesos DECIMAL(14,6)
   DEFINE v_c    INTEGER 

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

   ON ACTION REPORTE
  
   CALL fn_genera_reporte_fondo_ahorro()
     
   ON ACTION regresar 
      EXIT DISPLAY 

   END DISPLAY 
   CLOSE WINDOW w_consulta_fondo_ahorro
END FUNCTION

FUNCTION fn_consulta_fortalecimiento(p_cont)
   DEFINE p_cont INTEGER
   DEFINE v_total_pesos DECIMAL(14,6)
   DEFINE v_c    INTEGER 

   IF p_cont <= 0 THEN 
     RETURN 
   END IF 

    LET v_total_pesos = 0
    FOR v_c = 1 TO p_cont
       IF g_ar_fortalecimiento[v_c].id_derechohabiente IS NOT NULL THEN 
          LET v_total_pesos = v_total_pesos + g_ar_fortalecimiento[v_c].importe_viv
       ELSE
         IF g_ar_fortalecimiento[v_c].importe_viv = 0 THEN 
            LET g_ar_fortalecimiento[v_c].importe_viv = NULL
         END IF  
       END IF  
    END FOR 

   OPEN WINDOW w_consulta_fortalecimiento WITH FORM "RETF0222"
   DISPLAY ARRAY  g_ar_fortalecimiento TO t_fortalecimiento.*
   ATTRIBUTE (ACCEPT = FALSE ,CANCEL  = FALSE )

    BEFORE DISPLAY 
     DISPLAY "Retiro Fortalecimiento" TO ff_desc_consul
     DISPLAY "Total Registros: "||p_cont -2 TO ff_cont_reg
     DISPLAY v_total_pesos TO ff_total

     IF p_cont <= 0 THEN
       CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
       EXIT DISPLAY
     END IF
   ON ACTION REPORTE
  
     CALL fn_genera_reporte_fortalecimiento()
     
   ON ACTION regresar
      EXIT DISPLAY

   END DISPLAY
   CLOSE WINDOW w_consulta_fortalecimiento
END FUNCTION

FUNCTION fn_consulta_ley73(p_cont)
   DEFINE p_cont        INTEGER
   DEFINE v_total_pesos DECIMAL(14,6)
   DEFINE v_c           INTEGER 

   IF p_cont <= 0 THEN 
     RETURN 
   END IF 

    LET v_total_pesos = 0
    FOR v_c = 1 TO p_cont
       IF g_ar_ley73[v_c].id_derechohabiente IS NOT NULL THEN 
          LET v_total_pesos = v_total_pesos + g_ar_ley73[v_c].importe_viv92 + g_ar_ley73[v_c].importe_viv97
       ELSE
         IF g_ar_ley73[v_c].importe_viv92 = 0 AND g_ar_ley73[v_c].importe_viv97 = 0 THEN 
            LET g_ar_ley73[v_c].importe_viv92 = NULL
            LET g_ar_ley73[v_c].importe_viv97 = NULL
         END IF  
       END IF  
    END FOR 

   OPEN WINDOW w_consulta_Ley73 WITH FORM "RETF0221"
   DISPLAY ARRAY  g_ar_ley73 TO t_ley73.*
   ATTRIBUTE (ACCEPT = FALSE ,CANCEL  = FALSE )

    BEFORE DISPLAY 
     DISPLAY "Retiro Ley73" TO ff_desc_consul
     DISPLAY "Total Registros: "||p_cont -2 TO ff_cont_reg
     DISPLAY v_total_pesos TO ff_total

     IF p_cont <= 0 THEN
       CALL fn_mensaje("Aviso","No se encontraron registros con los criterios proporcionados","exclamation")
       EXIT DISPLAY  
     END IF 

   ON ACTION REPORTE
  
     CALL fn_genera_reporte_ley73()

   ON ACTION regresar 
      EXIT DISPLAY 

   END DISPLAY 
   CLOSE WINDOW w_consulta_Ley73
END FUNCTION

FUNCTION fn_preliquida_modulo_0(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           INTEGER
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida72.importe

 LET v_cont = 1
 LET v_pesos_total = 0

        FOREACH cur_preliquida USING p_con_ini , p_con_fin
                                   , g_id_derechohabiente ,g_id_derechohabiente
                                   , g_id_derechohabiente ,g_folio,g_folio,g_folio 
                                   , p_con_ini , p_con_fin
                                   , g_nss ,g_nss
                                   , g_nss ,g_folio,g_folio,g_folio
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
 DEFINE v_cont           INTEGER
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida.monto_pesos

 LET v_cont = 1
 LET v_pesos_total = 0

             FOREACH cur_preliquida USING p_con_ini , p_con_fin
                 , g_id_derechohabiente ,g_id_derechohabiente
                 , g_id_derechohabiente ,g_folio,g_folio,g_folio 
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
 DEFINE v_cont           INTEGER
 DEFINE v_bnd_movimiento LIKE ret_preliquida72.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida72.importe

 LET v_cont = 1
 LET v_pesos_total = 0

      FOREACH cur_preliquida USING p_con_ini , p_con_fin
                                 , g_nss ,g_nss, g_nss 
                                 ,g_folio,g_folio,g_folio 
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

FUNCTION fn_liquida_modulo_0(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           INTEGER
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE cta_fondo72.importe
 LET v_cont = 1
 LET v_pesos_total = 0
        FOREACH cur_liquida USING  p_con_ini , p_con_fin
                                   , g_id_derechohabiente ,g_id_derechohabiente
                                   , g_id_derechohabiente ,g_folio,g_folio,g_folio 
                                   , p_con_ini , p_con_fin
                                   --, g_nss ,g_nss, g_nss  --cta_fondo72 no tiene nss 
                                   ,g_folio,g_folio,g_folio
                                   , p_con_ini , p_con_fin
                                   , g_nss ,g_nss
                                   , g_nss ,g_folio,g_folio,g_folio
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
 DEFINE v_cont           INTEGER
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida.monto_pesos

 LET v_cont = 1
 LET v_pesos_total = 0

             FOREACH cur_liquida USING p_con_ini , p_con_fin
                 , g_id_derechohabiente ,g_id_derechohabiente
                 , g_id_derechohabiente ,g_folio,g_folio,g_folio 
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
 DEFINE v_cont           INTEGER
 DEFINE v_bnd_movimiento LIKE cta_movimiento.movimiento
 DEFINE v_pesos_total    LIKE cta_fondo72.importe

 LET v_cont = 1
 LET v_pesos_total = 0

      FOREACH cur_liquida USING p_con_ini , p_con_fin
                               ,g_folio,g_folio,g_folio 
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


FUNCTION fn_genera_reporte()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   INTEGER

   
    LET v_reporte = "RETF026.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF026.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_combinado.getLength()
         OUTPUT TO REPORT rpt_modulo(g_ar_combinado[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo
    END IF
END FUNCTION


REPORT rpt_modulo(p_combinado,g_usuario)

    DEFINE p_combinado        RECORD
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
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_combinado.*
END REPORT

FUNCTION fn_genera_reporte_solo_inf()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   INTEGER

   
    LET v_reporte = "RETF021.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF021.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo_solo_inf TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_solo_infonavit.getLength()
         OUTPUT TO REPORT rpt_modulo_solo_inf(g_ar_solo_infonavit[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo_solo_inf
    END IF
END FUNCTION


REPORT rpt_modulo_solo_inf(p_solo_inf,g_usuario)

    DEFINE p_solo_inf        RECORD
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20)    ,
            f_solicitud        LIKE ret_solo_infonavit.f_solicitud,
            clabe              CHAR(18)      ,
            acc_viv97          DECIMAL (19,6),
            pes_viv97          DECIMAL (19,6),
            f_valuacion        DATE          ,
            estado_solicitud   CHAR(18)      ,
            cod_rechazo        CHAR(18)
       END RECORD
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_solo_inf.*
END REPORT

FUNCTION fn_genera_reporte_fondo_ahorro()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   INTEGER

   
    LET v_reporte = "RETF022.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF022.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo_fondo_ahorro TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_fondo_ahorro.getLength()
         OUTPUT TO REPORT rpt_modulo_fondo_ahorro(g_ar_fondo_ahorro[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo_fondo_ahorro
    END IF
END FUNCTION

REPORT rpt_modulo_fondo_ahorro(p_fondo_ahorro,g_usuario)

    DEFINE p_fondo_ahorro        RECORD
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
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_fondo_ahorro.*
END REPORT

FUNCTION fn_genera_reporte_ley73()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   INTEGER

   
    LET v_reporte = "RETF0221.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF0221.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo_ley73 TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_ley73.getLength()
         OUTPUT TO REPORT rpt_modulo_ley73(g_ar_ley73[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo_ley73
    END IF
END FUNCTION

REPORT rpt_modulo_ley73(p_ley73,g_usuario)
    DEFINE p_ley73        RECORD
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
       
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_ley73.*
END REPORT

FUNCTION fn_genera_reporte_fortalecimiento()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   INTEGER

   
    LET v_reporte = "RETF0222.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF0222.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo_fortalecimiento TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_fortalecimiento.getLength()
         OUTPUT TO REPORT rpt_modulo_fortalecimiento(g_ar_fortalecimiento[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo_fortalecimiento
    END IF
END FUNCTION

REPORT rpt_modulo_fortalecimiento(p_fortalecimiento,g_usuario)
    DEFINE p_fortalecimiento        RECORD
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
       
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_fortalecimiento.*
END REPORT

FUNCTION fn_genera_reporte_preliquida()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   INTEGER

   
    LET v_reporte = "RETF023.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF023.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo_preliquida TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_preliquida.getLength()
         OUTPUT TO REPORT rpt_modulo_preliquida(g_ar_preliquida[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo_preliquida
    END IF
END FUNCTION

REPORT rpt_modulo_preliquida(p_preliquida,g_usuario)
    DEFINE p_preliquida        RECORD
              id_derechohabiente LIKE ret_preliquida.id_derechohabiente,
              nss                CHAR(20),
              movimiento         LIKE ret_preliquida.movimiento,
              f_liquida          LIKE ret_preliquida.f_liquida,
              id_referencia      LIKE ret_preliquida.id_referencia,
              folio_liquida      LIKE ret_preliquida.folio_liquida,
              monto_pesos        LIKE ret_preliquida72.importe
       END RECORD
       
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_preliquida.*
END REPORT

FUNCTION fn_genera_reporte_liquida()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   INTEGER

   
    LET v_reporte = "RETF024.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF024.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo_liquida TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_liquida.getLength()
         OUTPUT TO REPORT rpt_modulo_liquida(g_ar_liquida[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo_liquida
    END IF
END FUNCTION

REPORT rpt_modulo_liquida(p_liquida,g_usuario)
    DEFINE p_liquida        RECORD
              id_derechohabiente LIKE ret_preliquida.id_derechohabiente,
              nss                CHAR(20),
              movimiento         LIKE ret_preliquida.movimiento,
              f_liquida          LIKE ret_preliquida.f_liquida,
              id_referencia      LIKE ret_preliquida.id_referencia,
              folio_liquida      LIKE ret_preliquida.folio_liquida,
              monto_pesos        LIKE ret_preliquida72.importe
       END RECORD
       
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_liquida.*
END REPORT