--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#AUTOR        => MARIA DE LOURDES PADILLA MORENO                                        #
#MODULO       => RET                                                                    #
#PROGRAMA     => RETF473                                                                #
#OBJETIVO     => PROGRAMA DE CONSULTA GENERAL PARA EL MODULO RET LEY73 CONTING.         #
#Fecha inicio => DICIEMBRE 23, 2014                                                     #
#Modificacion => NO YET                                                                 #
#########################################################################################
DATABASE safre_viv
GLOBALS 
DEFINE g_tabla              INTEGER    ,
       g_estado             INTEGER    ,
       g_modalidad          INTEGER    ,
       g_fecha_inicio       DATE        ,
       g_fecha_fin          DATE        ,
       g_rechazo            INTEGER    ,
       g_tpo_retiro         CHAR(1)     ,
       g_folio              DECIMAL(9,0),
       g_nss                CHAR(20)    ,
       g_id_derechohabiente CHAR(18)    ,
       g_ind_consistencia   INTEGER     ,

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
            id_tipo_retiro     INTEGER,
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
            tpo_proceso        INTEGER  ,
            folio              DECIMAL(9,0),
            importe_viv92      DECIMAL(14,2),
            importe_viv97      DECIMAL(14,2),
            aivs_viv92         DECIMAL(19,6),
            aivs_viv97         DECIMAL(19,6),
            importe_tesofe     DECIMAL(14,2),
            estado_solicitud   CHAR(18),
            cod_retorno        INTEGER,  
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
              --importe        DECIMAL (19,6),
              importe_47     DECIMAL (19,6),
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

   CLOSE WINDOW SCREEN  --check version
   OPEN WINDOW w_consulta WITH FORM "RETF4731"

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
         -- la modalidad de retiro se inicia en 3
         LET g_modalidad = 3
         CALL  f.setElementHidden("formonly.cb_ind_consistencia",1)
         CALL  f.setElementHidden("lb_ind_consistencia",1)
         
         -- se llena el combo de inconsistencias
         --CALL cb_ind_consistencia.clear()
         
        { DECLARE  c_cb_ind_consistencia473 CURSOR FOR  
         SELECT ind_consistencia
         FROM   afi_decreto
         WHERE  NOT ind_consistencia IS NULL 
         GROUP BY ind_consistencia
                                             
         CALL cb_ind_consistencia.addItem('0' ,"TODOS")
         
         -- se agregan los tipos de inconsistencia al combo
         FOREACH c_cb_ind_consistencia473 INTO ar_ret_afi_decreto.ind_consistencia
            CALL cb_ind_consistencia.addItem(ar_ret_afi_decreto.ind_consistencia ,ar_ret_afi_decreto.ind_consistencia)
         END FOREACH}

         -- se llena el combo de tipo de retiro         
         CALL cb_tpo_retiro.clear()
         LET g_modalidad = 3
         DECLARE  c_cb_tpo_retiro473 CURSOR FOR  
         SELECT tpo_retiro, des_corta
         FROM ret_tipo_retiro
         WHERE modalidad_retiro = 3
         --ORDER BY tpo_retiro
        
         --CALL cb_tpo_retiro.addItem('0' ,"TODOS")
         
         -- se agregan los tipos de retiro al combo
         FOREACH c_cb_tpo_retiro473 INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
            CALL cb_tpo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
         END FOREACH
         
         INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL  
         LET g_tpo_retiro = 0
         
         -- se llena el combo de modalidad de retiro
         CALL cb_modalidad.clear()
         
         DECLARE c_cb_modalidad473 CURSOR FOR  
         SELECT * 
         FROM ret_modalidad_retiro
         WHERE modalidad_retiro = 3 -- LULIS RETIRO LEY 73 #CAMBIO
         --ORDER BY modalidad_retiro
         
         --CALL cb_modalidad.addItem(0 ,"TODOS") --LULIS
         
         FOREACH c_cb_modalidad473 INTO ar_ret_modalidad_retiro.*
            CALL cb_modalidad.addItem(ar_ret_modalidad_retiro.modalidad_retiro ,ar_ret_modalidad_retiro.modalidad_retiro||" - "||ar_ret_modalidad_retiro.des_corta)
         END FOREACH
         
         INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
       
         DECLARE  c_cb_estado473 CURSOR FOR  
         SELECT * 
         FROM ret_estado_solicitud
         ORDER BY estado_solicitud 
         
         CALL cb_estado.addItem(0 ,"TODOS") 
         
         FOREACH c_cb_estado473 INTO ar_ret_estado_solicitud.*
            CALL cb_estado.addItem(ar_ret_estado_solicitud.estado_solicitud ,ar_ret_estado_solicitud.estado_solicitud||" - "||ar_ret_estado_solicitud.des_corta)
         END FOREACH
         
         INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
       
         DECLARE  c_cb_rechazo473 CURSOR FOR  
                                    SELECT * 
                                      FROM ret_rechazo
                                      ORDER BY cod_rechazo 
         
         CALL cb_rechazo.addItem(0 ,"TODOS")                            
         FOREACH c_cb_rechazo473 INTO ar_ret_rechazo.*
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
      
      -- al cambiar la modalidad --DEberá ser solo la modalidad de ret conting ley73
      ON CHANGE cb_modalidad
         CALL cb_tpo_retiro.clear()

          DISPLAY g_modalidad
          
          --CALL cb_tpo_retiro.addItem("0" ,"TODOS") 
          
          FOREACH c_cb_tpo_retiro473 INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
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
            {IF g_modalidad = 1 OR g_modalidad = 0  THEN 
                LET r_total_pesos  = 0 
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0
            
               CALL fn_solicitud_solo_infonavit(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin) 
               RETURNING r_total_pesos , r_total_avis97
              -- LET t_totalgral_pesos  = t_totalgral_pesos + r_total_pesos
              -- LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF}
            
               --fondo-ahorro
            {IF g_modalidad = 2 OR g_modalidad = 0 THEN
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0 
                LET r_total_pesos  = 0 
               CALL fn_solicitud_fondo_ahorro(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin)
                RETURNING r_total_pesos  
                
               LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               --LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               --LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF}
         
            --ley 73 LA QUE IMPORTA
            IF g_modalidad = 3 OR g_modalidad = 0 THEN
                LET r_total_avis92 = 0 
                LET r_total_avis97 = 0 
                LET r_total_pesos = 0
               CALL fn_solicitud_ley73_conting(g_estado, g_rechazo ,g_fecha_inicio , g_fecha_fin)
                RETURNING r_total_pesos,r_total_avis92,r_total_avis97
            
               LET t_totalgral_pesos = t_totalgral_pesos + r_total_pesos
               LET t_totalgral_aivs92 = t_totalgral_aivs92 + r_total_avis92
               LET t_totalgral_aivs97 = t_totalgral_aivs97 + r_total_avis97
            END IF
                        
            -- muestra el resumen de modalidades si existen modlos distintos 
            --agregar los registros encontrados por retiro con criterios de la solicitud .
            CALL fn_consulta_modalidad_conting(t_totalgral_pesos,t_totalgral_pesos49, t_totalgral_aivs92, t_totalgral_aivs97) 
         
         WHEN 2     --preliquidacion
            CALL fn_preliquidacion_conting(g_fecha_inicio , g_fecha_fin)

         WHEN 3     --liquidacion
            CALL fn_liquidacion_conting(g_fecha_inicio , g_fecha_fin)   
           
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
FUNCTION fn_solicitud_ley73_conting(p_estado, p_rechazo ,p_con_ini , p_con_fin) 
 DEFINE p_con_ini      DATE
 DEFINE p_con_fin      DATE  
 DEFINE p_estado       SMALLINT
 DEFINE p_rechazo      SMALLINT
 DEFINE v_cont473         INTEGER
 DEFINE v_query,v_query1        STRING 
 DEFINE v_total_pesos  DECIMAL(14,6)
 DEFINE v_total_aivs92 DECIMAL(14,6)
 DEFINE v_total_aivs97 DECIMAL(14,6)
 DEFINE v_total_importe_tesofe DECIMAL(14,6)
 DEFINE v_total_aivs   DECIMAL(14,6)
 DEFINE v_c            INTEGER 
 DEFINE v_con_tot      INTEGER 
 DEFINE v_desc_estado  CHAR(18)
 DEFINE v_desc_rechazo CHAR(18)
 DEFINE v_contador INTEGER
  
    DISPLAY "Dentro e la funcion fn_solicitud_ley73_conting"
    --LET p_estado = 0
     --Primero cuento si hay registros de esas caracteristicas
    --LET v_query1 =  "\n SELECT count(*) FROM ret_ley73          ",                                                                        
                   --"\n WHERE (estado_solicitud = ? or '0' = ?)                                        ",
                   --"\n   AND folio = folio                                                            ",
                   --"\n   AND (cod_rechazo = ? or '0' = ?)                                             ",
                   --"\n   AND f_solicitud between ? and ?                                              ", 
                   --"\n   AND (id_derechohabiente = ?                                                  ",
                   --"\n    or ? is null or ? = '')                                                     ",
                   --"\n   AND (folio = ?                                                               ",
                   --"\n    or ? is null or ? = '')                                                     "

    LET v_query1 =  "\n SELECT count(*) FROM ret_ley73          ",                                                                        
                   "\n WHERE 1 = 1 "
    IF g_folio IS NOT NULL THEN 
        LET v_query1 =  v_query1 CLIPPED, "\n AND folio = ",  g_folio
    END IF 
    IF p_estado IS NOT NULL AND p_estado <> 0 THEN 
        LET v_query1 = v_query1 CLIPPED, "\n AND estado_solicitud = ", g_estado
    END IF 
    IF p_rechazo IS NOT NULL AND p_rechazo <> 0 THEN 
        LET v_query1 = v_query1 CLIPPED, "\n AND cod_rechazo = ", g_rechazo
    END IF 
    IF p_con_ini IS NOT NULL AND p_con_fin IS NOT NULL THEN
        LET v_query1 = v_query1 CLIPPED, "\n AND f_solicitud between '", p_con_ini, "' AND '", p_con_fin, "'"
    END IF 
    IF g_id_derechohabiente IS NOT NULL THEN 
        LET v_query1 = v_query1 CLIPPED, "\n AND id_derechohabiente = ", g_id_derechohabiente
    END IF 

    DISPLAY "El query " || v_query1
    DISPLAY "Con los valores: "
    DISPLAY "Parametro 1>" || p_estado || "<"
    DISPLAY "Parametro 2>" || p_estado || "<"
    DISPLAY "Parametro 3>" || p_rechazo || "<"
    DISPLAY "Parametro 4>" || p_rechazo || "<"
    DISPLAY "Parametro 5>" || p_con_fin || "<"
    DISPLAY "Parametro 6>" || p_con_fin || "<"
    DISPLAY "Parametro 7>" || g_id_derechohabiente || "<"
    DISPLAY "Parametro 8>" || g_id_derechohabiente || "<"
    DISPLAY "Parametro 9>" || g_id_derechohabiente || "<"
    DISPLAY "Parametro 10>" || g_folio || "<"
    DISPLAY "Parametro 11>" || g_folio || "<"
    DISPLAY "Parametro 12>" || g_folio || "<"

    PREPARE s1 FROM v_query1
    --EXECUTE s1 USING p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin 
          --, g_id_derechohabiente ,g_id_derechohabiente, g_id_derechohabiente, 
            --g_folio, g_folio, g_folio
    EXECUTE s1 INTO v_contador

    DISPLAY "contador",v_contador
   
   -- IF v_contador >= 5000 THEN
     -- CALL fn_mensaje("Aviso","Su búsqueda arroja mas de 5000 registros tardará mucho","exclamation")
    --ELSE 
     ----
    CALL g_ar_ley73.clear()
    LET v_cont473 = 1 
    --LET v_query =  "\n SELECT id_solicitud,id_derechohabiente,' ',                                    ",
                   --"\n f_solicitud,' ',folio,importe_viv92,importe_viv97,aivs_viv92,aivs_viv97,       ",
                   --"\n estado_solicitud,cod_retorno,cod_rechazo                                       ",
                   --"\n FROM ret_ley73                                                                 ",                   
                   --"\n WHERE (estado_solicitud = ? or '0' = ?)                                        ",
                   --"\n   AND folio = folio                                                            ",
                   --"\n   AND (cod_rechazo = ? or '0' = ?)                                             ",
                   --"\n   AND f_solicitud between ? and ?                                              ", 
                   --"\n   AND (id_derechohabiente = ?                                                  ",
                   --"\n    or ? is null or ? = '')                                                     ",
                   --"\n   AND (folio = ?                                                               ",
                   --"\n    or ? is null or ? = '')                                                     "
--
    LET v_query =  "\n SELECT id_solicitud,rl.id_derechohabiente,nss,                                      "||
                   "\n f_solicitud,tpo_proceso,folio,importe_viv92,importe_viv97,aivs_viv92,aivs_viv97, "||
                   "\n importe_tesofe,estado_solicitud,cod_retorno,cod_rechazo                                         "||
                   "\n FROM ret_ley73 rl, afi_derechohabiente ad                                        "||
                   "\n WHERE rl.id_derechohabiente = ad.id_derechohabiente                              "
    IF g_folio IS NOT NULL THEN 
        LET v_query = v_query CLIPPED, "\n AND folio = ", g_folio
    END IF 
    IF p_estado IS NOT NULL AND p_estado <> 0 THEN 
        LET v_query = v_query CLIPPED, "\n AND estado_solicitud = ", p_estado
    END IF 
    IF p_rechazo IS NOT NULL  AND p_rechazo <> 0 THEN 
        LET v_query = v_query CLIPPED, "\n AND cod_rechazo = ", p_rechazo
    END IF 
    IF p_con_ini IS NOT NULL AND p_con_fin IS NOT NULL THEN
        LET v_query = v_query CLIPPED, "\n AND f_solicitud between '", p_con_ini, "' AND '", p_con_fin, "'"
    END IF 
    IF g_id_derechohabiente IS NOT NULL THEN 
        LET v_query = v_query CLIPPED, "\n AND rl.id_derechohabiente = ", g_id_derechohabiente
    END IF                    
          
          LET g_ar_ley73[v_cont473].nss = "3-Ley 73"
          
          LET g_ar_ley73[v_cont473].aivs_viv92 = 0
          LET g_ar_ley73[v_cont473].aivs_viv97 = 0
          
          LET g_ar_ley73[v_cont473].importe_viv92 = 0
          LET g_ar_ley73[v_cont473].importe_viv97 = 0
          LET v_cont473 = v_cont473 + 1 

          {DISPLAY v_query
         
         DISPLAY "p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin ", 
                  "\n p_estado, p_rechazo ,p_con_ini , p_con_fin \n"
                  ,p_estado,"-", p_rechazo ,"-",p_con_ini ,"-", p_con_fin,g_id_derechohabiente,g_folio,
                  "\n ",p_estado,p_estado, p_rechazo , p_rechazo, p_con_ini , p_con_fin,g_id_derechohabiente
         DISPLAY "derechohabiente",g_id_derechohabiente,"g_folio",g_folio}

         
         PREPARE pr_Ley73 FROM v_query
         DECLARE cur_ley473 CURSOR FOR pr_Ley73

          FOREACH cur_ley473 INTO g_ar_ley73[v_cont473].*

             SELECT nss
               INTO g_ar_ley73[v_cont473].nss 
               FROM afi_derechohabiente
              WHERE id_derechohabiente = g_ar_ley73[v_cont473].id_derechohabiente

              INITIALIZE v_desc_estado TO NULL 
              SELECT  des_corta
                INTO v_desc_estado
                FROM ret_estado_solicitud
               WHERE estado_solicitud = g_ar_ley73[v_cont473].estado_solicitud

              LET g_ar_ley73[v_cont473].estado_solicitud = g_ar_ley73[v_cont473].estado_solicitud CLIPPED ,"-"|| v_desc_estado CLIPPED 

              INITIALIZE v_desc_rechazo TO NULL
                 SELECT  des_corta
                   INTO v_desc_rechazo
                   FROM ret_rechazo
                  WHERE cod_rechazo = g_ar_ley73[v_cont473].cod_rechazo

              LET g_ar_ley73[v_cont473].cod_rechazo = g_ar_ley73[v_cont473].cod_rechazo CLIPPED ,"-"|| v_desc_rechazo CLIPPED 
              LET v_cont473 = v_cont473 + 1
              
          END FOREACH  
          LET v_total_pesos = 0
          LET v_total_aivs  = 0
          LET v_total_aivs92  = 0
          LET v_total_aivs97  = 0
          LET v_total_importe_tesofe = 0

           FOR v_c = 1 TO v_cont473
              IF  g_ar_ley73[v_c].id_derechohabiente IS NOT NULL THEN
                 LET v_total_pesos  = 0
                 LET v_total_importe_tesofe = v_total_importe_tesofe + g_ar_ley73[v_c].importe_tesofe
                 LET v_total_aivs92 = v_total_aivs92 + g_ar_ley73[v_c].aivs_viv92
                 LET v_total_aivs97 = v_total_aivs97 + g_ar_ley73[v_c].aivs_viv97
                 LET v_total_aivs   = v_total_aivs   + g_ar_ley73[v_c].aivs_viv92 + g_ar_ley73[v_c].aivs_viv97
              END IF  
           END FOR 

          IF v_total_pesos > 0 OR v_total_aivs > 0 OR v_cont473 - 3 > 0 THEN 
             LET v_con_tot  = g_ar_totales.getLength() + 1
             LET g_ar_totales[v_con_tot].id  = 0
             LET g_ar_totales[v_con_tot].id_tipo_retiro = 3
             LET g_ar_totales[v_con_tot].tipo_retiro  = "3 Retiro Ley 73"
             LET g_ar_totales[v_con_tot].movimiento   =  192
             --LET g_ar_totales[v_con_tot].importe      = v_total_pesos
             LET g_ar_totales[v_con_tot].importe_47   = v_total_importe_tesofe
             LET g_ar_totales[v_con_tot].aivs92       = v_total_aivs92
             LET g_ar_totales[v_con_tot].aivs97       = v_total_aivs97

             LET g_ar_ley73[v_cont473].nss = "Totales "
             LET g_ar_ley73[v_cont473].aivs_viv92 = v_total_aivs92
             LET g_ar_ley73[v_cont473].aivs_viv97 = v_total_aivs97
             --LET g_ar_ley73[v_cont].importe_viv92 = v_total_pesos
             --LET g_ar_ley73[v_cont].importe_viv97 = v_total_aivs97

             --DISPLAY  g_ar_ley73[v_cont].*
             LET v_cont473 = v_cont473 + 1
             
             CALL g_ar_ley73.deleteElement(v_cont473)
         END IF
     --  END IF   
          RETURN v_total_pesos ,v_total_aivs92 ,v_total_aivs97
END FUNCTION    


--genera datos para el resumen general preliquidacion
FUNCTION fn_consulta_preliquidacion_con(p_cont)
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

   OPEN WINDOW w_consulta_preliquida WITH FORM "RETF4735" --Copiar la forma lulis
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
FUNCTION fn_consulta_liquidacion_conting(p_cont)
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

   OPEN WINDOW w_consulta_liquida WITH FORM "RETF4736" 
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
--Solo va a entrar la modalidad 3 lulis
FUNCTION fn_preliquidacion_conting(p_con_ini,p_con_fin)
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
		  --Modalidad 3 nada mas lulis
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
		   --Esta Importa lulis
		   Display "Entra preliquidacion 73 contingente"
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
             LET v_cadena = "\n   AND movimiento in (212)"
          END IF 

          --habilita la preliquidacion para transferenicia
          IF g_modalidad = 6 THEN 
             LET v_cadena = "\n   AND movimiento in (222)"
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
          DECLARE cur_preliquida473 CURSOR FOR pr_preliquida

          CASE g_modalidad
          
           WHEN 0
           --DISPLAY "si entro "
            --foreach con las instrucciones
             {CALL fn_preliquida_modulo_0(p_con_ini , p_con_fin)
             RETURNING v_cont ,v_pesos_total}
             
           WHEN 2
             {CALL fn_preliquida_modulo_2(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total}
             
           OTHERWISE
             CALL fn_preliquida_modulo_1_con(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total
              
          END CASE 
          --LET v_pesos_total = 0
           
          LET g_ar_preliquida[v_cont].nss  = "TOTAL"
          LET g_ar_preliquida[v_cont].monto_pesos  = v_pesos_total
          --DISPLAY v_pesos_total
          
          CALL fn_consulta_preliquidacion_con(v_cont)
END FUNCTION

--genera la consulta para la parte de liquidacion
FUNCTION fn_liquidacion_conting(p_con_ini,p_con_fin)
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
                "\n FROM  ",
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
                "\n       UNION ALL",
                "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                "\n         FROM cta_movimiento ",
                "\n       ) ",
                "\n WHERE f_liquida between ? and ?",
                "\n   AND (id_derechohabiente = ?",
                "\n    or ? is null or ? =  '')",
                "\n   AND (folio_liquida = ?",
                "\n    or ? is null or ? = '')"

          --habilita la liquidacion para todos los retiros 
		  -- Importa la 3 lulis 
          IF g_modalidad = 0 THEN 
             LET v_query =   "\n SELECT id_derechohabiente,' ', ",
                             "\n movimiento,f_liquida,id_referencia,folio_liquida,monto_pesos",
                             "\n FROM (SELECT id_derechohabiente, id_referencia, movimiento, ",
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
                             "\n       UNION ALL",
                             "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                             "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                             "\n         FROM cta_movimiento ",
                             "\n       ) ",
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
             LET v_cadena = "\n   AND movimiento in (212)"
          END IF 

          --habilita la liquidacion para transferenicia
          IF g_modalidad = 6 THEN 
             LET v_cadena = "\n   AND movimiento in (222)"
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
             
             {CALL fn_liquida_modulo_0(p_con_ini , p_con_fin)
             RETURNING v_cont ,v_pesos_total}
             
           WHEN 2
             {CALL fn_liquida_modulo_2(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total}
             
           OTHERWISE
             CALL fn_liquida_modulo_1_conting(p_con_ini , p_con_fin)
             RETURNING v_cont, v_pesos_total
              
          END CASE 
          --LET v_pesos_total = 0
           
          LET g_ar_liquida[v_cont].nss  = "TOTAL"
          LET g_ar_liquida[v_cont].monto_pesos  = v_pesos_total
          --DISPLAY v_pesos_total
          
          CALL fn_consulta_liquidacion_conting(v_cont)
END FUNCTION

--pantalla del resumen general por modalidad
FUNCTION fn_consulta_modalidad_conting(p_total_pesos,p_total_pesos49,p_total_aivs92,p_total_aivs97)
DEFINE p_total_pesos  DECIMAL(19,6)  ,
       p_total_pesos49  DECIMAL(19,6),
       p_total_aivs92 DECIMAL(19,6)  ,
       p_total_aivs97 DECIMAL(19,6)  ,
       v_c            INTEGER      ,
       v_cont_ar      INTEGER 
   
   OPEN WINDOW w_consulta_totales WITH FORM "RETF4732"
   
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
         --DISPLAY p_total_pesos49  TO ff_total3
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
                       { CALL fn_consulta_solo_infonavit(g_ar_solo_infonavit.getLength())}
                   
                     WHEN 2
                        --fondo_ahorro 
                        {CALL fn_consulta_fondo_ahorro(g_ar_fondo_ahorro.getLength())}
                       
                     WHEN 3
                        --Ley73
                        CALL fn_consulta_ley73_conting(g_ar_ley73.getLength())
                       
                     WHEN 4 
                        --tipo_n
                       { CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)}
                     
                     WHEN 5
                        --disposicion 
                        display "consultando disposicion"
                        {CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)}

                     WHEN 8
                        --disposicion PMG
                        display "consultando PMG"
                        {CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)}
                       
                     WHEN 6
                        --transferencia
                        {CALL fn_consulta_retiro(g_ar_totales[v_c].id_tipo_retiro)}

                     WHEN 7
                        --fortalecimiento
                        {CALL fn_consulta_fortalecimiento(g_ar_fortalecimiento.getLength())}

                     OTHERWISE
                        EXIT FOR

                  END CASE
               END IF 
            END FOR
        { ELSE
            DISPLAY "entro a combinado"
            IF ( v_cont_ar > 0 ) THEN
               --si existe mas de un seleccionado entra a esta opcion 
               CALL fn_consulta_combinado_modulos()
            END IF }
         END IF

      ON ACTION Regresar
         EXIT INPUT  

   END INPUT
   
   CALL g_ar_totales.clear()
   
   CLOSE WINDOW w_consulta_totales
   
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
  DEFINE v_ya_paso                      INTEGER
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

   OPEN WINDOW w_consulta_detalle WITH FORM "RETF4733"
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



FUNCTION fn_consulta_ley73_conting(p_cont)
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

   OPEN WINDOW w_consulta_Ley73 WITH FORM "RETF4734"  
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
   --Cambio  --lulis 
   ON ACTION REPORTE  
    CALL fn_reporte_ley73_conting()
   
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

        FOREACH cur_preliquida473 USING p_con_ini , p_con_fin
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

FUNCTION fn_preliquida_modulo_1_con(p_con_ini , p_con_fin)
 DEFINE p_con_ini        DATE
 DEFINE p_con_fin        DATE  
 DEFINE v_cont           INTEGER 
 DEFINE v_bnd_movimiento LIKE ret_preliquida.movimiento
 DEFINE v_pesos_total    LIKE ret_preliquida.monto_pesos

 LET v_cont = 1
 LET v_pesos_total = 0

             FOREACH cur_preliquida473 USING p_con_ini , p_con_fin
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

      FOREACH cur_preliquida473 USING p_con_ini , p_con_fin
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

FUNCTION fn_liquida_modulo_1_conting(p_con_ini , p_con_fin)
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

FUNCTION fn_reporte_ley73_conting()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         INTEGER  
   DEFINE i                   INTEGER

   DISPLAY "Entro a fn_reporte_ley73_conting"
    LET v_reporte = "RETF473.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF473.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_contgley73_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    DISPLAY "size", g_ar_combinado.getLength()
    DISPLAY "size g_ar_ley73", g_ar_ley73.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_ley73.getLength()
         OUTPUT TO REPORT rpt_modulo(g_ar_ley73[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo
    END IF
END FUNCTION


REPORT rpt_modulo(p_contingley73,g_usuario)

    DEFINE p_contingley73        RECORD
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(11),
            f_solicitud        DATE ,
            tpo_proceso        INTEGER  ,
            folio              DECIMAL(9,0),
            importe_viv92      DECIMAL(14,2),
            importe_viv97      DECIMAL(14,2),
            aivs_viv92         DECIMAL(19,6),
            aivs_viv97         DECIMAL(19,6),
            importe_tesofe     DECIMAL(14,6),
            estado_solicitud   CHAR(18),
            cod_retorno        INTEGER,  
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
         PRINTX p_contingley73.*

END REPORT 

