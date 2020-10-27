--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RET                                                                    #
#OBJETIVO     => CONSULTA DE RETIROS PARA LAS SOLICITUDES RECHAZADAS                    #

#Fecha inicio => Mayo 31, 2012                                                          #
#Modificacion =>                                                                        #
#########################################################################################
GLOBALS
DEFINE  g_id_derechohabiente DECIMAL(9,0)
       ,g_id_solicitud       DECIMAL(9,0)
       ,g_folio              DECIMAL(9,0)
       ,g_select             SMALLINT 
       ,g_nss                CHAR(11)
       ,g_nss_validacion     CHAR(11)              
       ,g_f_ini_pen          DATE 
       ,g_f_fin_pen          DATE 
       ,g_f_ini_resol        DATE 
       ,g_f_fin_resol        DATE 
       ,g_tipo_retiro        CHAR(1)
       ,g_regimen            CHAR(2)
       ,g_tipo_pension       CHAR(2)
       ,g_tipo_prestacion    CHAR(2)
       
END GLOBALS  

DATABASE safre_viv
MAIN
 DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING   -- titulo de la ventana 
       
DEFINE cb_tipo_retiro      ui.ComboBox
DEFINE cb_regimen          ui.ComboBox
DEFINE cb_tipo_pension     ui.ComboBox
DEFINE cb_tipo_prestacion  ui.ComboBox

DEFINE w  ui.window
DEFINE f  ui.form

DEFINE v_c                     SMALLINT 
DEFINE ar_ret_tipo_retiro      RECORD LIKE ret_tipo_retiro.* 
DEFINE ar_ret_regimen          RECORD LIKE ret_regimen.*
DEFINE ar_ret_tipo_pension     RECORD LIKE ret_tipo_pension.*
DEFINE ar_ret_tipo_prestacion  RECORD LIKE ret_tipo_prestacion.*

 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
 
    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

  CLOSE WINDOW SCREEN 
  OPEN WINDOW rech_retiro  WITH FORM "RETF020"

  LET cb_tipo_retiro     = ui.ComboBox.forName("formonly.cb_tipo_retiro") 
  LET cb_regimen         = ui.ComboBox.forName("formonly.cb_regimen")
  LET cb_tipo_pension    = ui.ComboBox.forName("formonly.cb_tipo_pension")
  LET cb_tipo_prestacion = ui.ComboBox.forName("formonly.cb_tipo_prestacion")


  
  LET w = ui.Window.getCurrent()
  LET f = w.getForm()

  #se capturan los campos para realizar los fltros 
  INPUT  g_id_derechohabiente 
         ,g_id_solicitud
         ,g_nss
         ,g_folio
         --,g_select
         ,g_f_ini_pen
         ,g_f_fin_pen
         ,g_f_ini_resol
         ,g_f_fin_resol 
         ,g_tipo_retiro
         ,g_regimen
         ,g_tipo_pension
         ,g_tipo_prestacion
         WITHOUT DEFAULTS
  FROM 
          v_id_derechohabiente
         ,v_id_solicitud
         ,v_nss
         ,v_folio 
         --,ch_select   
         ,de_ini_pen
         ,de_fin_pen
         ,de_ini_res
         ,de_fin_res  
         ,cb_tipo_retiro
         ,cb_regimen
         ,cb_tipo_pension
         ,cb_tipo_prestacion      
      ATTRIBUTE (ACCEPT  = FALSE ,CANCEL  = FALSE ,UNBUFFERED = TRUE)
      
  
     BEFORE INPUT 
          INITIALIZE  g_id_derechohabiente TO NULL  
          INITIALIZE  g_id_solicitud       TO NULL
          INITIALIZE  g_folio              TO NULL
          INITIALIZE  g_f_ini_pen          TO NULL
          INITIALIZE  g_f_ini_resol        TO NULL

          CALL cb_tipo_retiro.clear()
          CALL cb_regimen.clear()
          CALL cb_tipo_pension.clear()
          CALL cb_tipo_prestacion.clear()
          
          LET g_select      = 0
          LET g_f_fin_pen = TODAY 
          LET g_f_fin_resol = TODAY 
          
          #se llenan los combos de tipo de retiro
          --DECLARE  c_cb_tipo_retiro CURSOR FOR  
                                    --SELECT tpo_retiro, des_corta
                                      --FROM ret_tipo_retiro
                                      --WHERE tpo_retiro IN ('A','B','C')
                                         --ORDER BY tpo_retiro
        --LET v_c = 1
        --CALL cb_tipo_retiro.clear()
        --FOREACH c_cb_tipo_retiro INTO ar_ret_tipo_retiro.tpo_retiro,ar_ret_tipo_retiro.des_corta
           --CALL cb_tipo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro ,ar_ret_tipo_retiro.tpo_retiro||" - "||ar_ret_tipo_retiro.des_corta)
        --END FOREACH
        
        #se llena el combo de regimen
        --DECLARE  c_cb_regimen CURSOR FOR  
                                    --SELECT regimen, des_corta
                                      --FROM ret_regimen
                                    --ORDER BY regimen                                          
        --LET v_c = 1
--
        --CALL cb_regimen.clear()
        --FOREACH c_cb_regimen INTO ar_ret_regimen.regimen,ar_ret_regimen.des_corta
           --CALL cb_regimen.addItem(ar_ret_regimen.regimen ,ar_ret_regimen.regimen||" - "||ar_ret_tipo_retiro.des_corta)
        --END FOREACH
        --
        #se llena el combo de tipo de pension
        --DECLARE  c_cb_tipo_pension CURSOR FOR  
                                    --SELECT tpo_pension, des_corta
                                      --FROM ret_tipo_pension
                                      --WHERE (tpo_pension = g_tipo_pension 
                                         --OR  0 = g_tipo_pension) 
                                         --ORDER BY tpo_pension 
                                         --
        --LET v_c = 1
        --CALL cb_tipo_pension.clear()
        --FOREACH c_cb_tipo_retiro INTO ar_ret_tipo_pension.tpo_pension,ar_ret_tipo_pension.des_corta
           --CALL cb_tipo_pension.addItem(ar_ret_tipo_pension.tpo_pension ,ar_ret_tipo_pension.tpo_pension||" - "||ar_ret_tipo_pension.des_corta)
        --END FOREACH
        #se llena el combo de prestacion 
        --DECLARE  c_cb_tipo_prestacion CURSOR FOR  
                                    --SELECT tpo_prestacion, des_corta
                                      --FROM ret_tipo_prestacion
                                      --WHERE (tpo_prestacion = g_tipo_prestacion 
                                         --OR  0 = g_tipo_prestacion ) 
                                         --ORDER BY tpo_prestacion 
        --LET v_c = 1
        --CALL cb_tipo_prestacion.clear()
        --FOREACH c_cb_tipo_prestacion INTO ar_ret_tipo_prestacion.tpo_prestacion,ar_ret_tipo_retiro.des_corta
           --CALL cb_tipo_prestacion.addItem(ar_ret_tipo_prestacion.tpo_prestacion ,ar_ret_tipo_prestacion.tpo_prestacion||" - "||ar_ret_tipo_retiro.des_corta)
        --END FOREACH

     ON CHANGE de_fin_pen
          IF g_f_fin_pen IS NULL THEN
            LET g_f_fin_pen = TODAY 
          END IF

     ON CHANGE de_fin_res
          IF g_f_fin_resol IS NULL THEN
            LET g_f_fin_resol = TODAY 
          END IF  
        
          
     ON ACTION aceptar
           CALL fn_display_consulta()
        
     ON ACTION cancelar 
     EXIT INPUT  
   
  END INPUT
CLOSE WINDOW rech_retiro  
END MAIN

FUNCTION  fn_display_consulta()

DEFINE arr_ret_transferecia  DYNAMIC ARRAY OF RECORD LIKE ret_transferencia.*
DEFINE arr_ret_transferecia_rch  DYNAMIC ARRAY OF RECORD LIKE ret_transferencia_rch.*

DEFINE arr_tmp_ret_rch_solicitud DYNAMIC ARRAY OF RECORD 
                   id_solicitud         DECIMAL(9,0) ,
                   id_derechohabiente   DECIMAL(9,0) ,
                   folio                DECIMAL(9,0) ,
                   nss                  CHAR(11)     ,
                   tipo_retiro          CHAR(1)      ,
                   regimen              CHAR(2)      ,
                   tpo_pension          CHAR(2)      ,
                   tpo_prestacion       CHAR(2)      ,
                   f_inicio_pension     DATE         ,
                   f_resolucion         DATE         ,
                   tpo_seguro           CHAR(2)      ,
                   importe_viv72        DECIMAL(14,2),
                   aivs_viv92           DECIMAL(14,6),
                   aivs_viv97           DECIMAL(14,6),
                   cod_rechazo_1        SMALLINT     ,
                   cod_rechazo_2        SMALLINT     ,
                   cod_rechazo_3        SMALLINT
END RECORD

DEFINE r_tmp_ret_rch_solicitud RECORD 
                   id_solicitud         DECIMAL(9,0) ,
                   id_derechohabiente   DECIMAL(9,0) ,
                   folio                DECIMAL(9,0) ,
                   nss                  CHAR(11)     ,
                   tipo_retiro          CHAR(1)      ,
                   regimen              CHAR(2)      ,
                   tpo_pension          CHAR(2)      ,
                   tpo_prestacion       CHAR(2)      ,
                   f_inicio_pension     DATE         ,
                   f_resolucion         DATE         ,
                   tpo_seguro           CHAR(2)      ,
                   importe_viv72        DECIMAL(14,2),
                   aivs_viv92           DECIMAL(14,6),
                   aivs_viv97           DECIMAL(14,6),
                   cod_rechazo_1        SMALLINT     ,
                   cod_rechazo_2        SMALLINT     ,
                   cod_rechazo_3        SMALLINT             
END RECORD

DEFINE v_bnd_prime     SMALLINT  
DEFINE v_count         SMALLINT
DEFINE v_script        STRING 

LET v_bnd_prime = 0
LET v_script = 'DROP TABLE IF EXISTS tmp_ret_rch_solicitud'
# se prepara script para el retiro disposicion
PREPARE s1 FROM v_script
 EXECUTE s1
 
CREATE TEMP TABLE tmp_ret_rch_solicitud(
                   id_solicitud         DECIMAL(9,0) ,
                   id_derechohabiente   DECIMAL(9,0) ,
                   folio                DECIMAL(9,0) ,
                   nss                  CHAR(11)     ,
                   tipo_retiro          CHAR(1)      ,
                   regimen              CHAR(2)      ,
                   tpo_pension          CHAR(2)      ,
                   tpo_prestacion       CHAR(2)      ,
                   f_inicio_pension     DATE         ,
                   f_resolucion         DATE         ,
                   tpo_seguro           CHAR(2)      ,
                   importe_viv72        DECIMAL(14,2),
                   aivs_viv92           DECIMAL(14,6),
                   aivs_viv97           DECIMAL(14,6),
                   cod_rechazo_1        SMALLINT     ,
                   cod_rechazo_2        SMALLINT     ,
                   cod_rechazo_3        SMALLINT              
);

     LET v_script = "SELECT '0'                    ,-- id_solicitud      ",
                    "\n     tn.id_derechohabiente  ,                     ",
                    "\n     tn.folio               ,                     ",
                    "\n     '0'                    ,--nss                ",
                    "\n     md.tpo_retiro          ,--tipo_retiro        ",
                    "\n     md.regimen             ,--regimen            ",
                    "\n     md.tpo_pension         ,--tpo_pension        ",
                    "\n     md.tpo_prestacion      ,--tpo_prestacion     ",
                    "\n     tn.f_inicio_pension    ,                     ",
                    "\n     tn.f_resolucion        ,                     ",
                    "\n     md.tpo_seguro          ,--tpo_seguro         ",
                    "\n     0                      ,--importe_viv72      ",
                    "\n     0                      ,--aivs_viv92         ",
                    "\n     tn.aivs_viv97          ,                     ",
                    '\n     cod_rechazo_1          ,                     ',
                    '\n     cod_rechazo_2          ,                     ',
                    '\n     cod_rechazo_3                                ',
                    "\n FROM ret_transferencia_rch tn  ,                  ",
                    "\n      ret_matriz_derecho   md                     ",
                    "\n  WHERE  ((f_inicio_pension BETWEEN '",  g_f_ini_pen ,"'",
                    "\n   AND '", g_f_fin_pen   ,"') OR '",g_f_ini_pen  ,"' ='          '", 
                    "\n     OR '",g_f_ini_pen  ,"' is NULL  )",
                    "\n    AND  ((f_resolucion BETWEEN '"  ,g_f_ini_resol ,"'",
                    "\n    AND '", g_f_fin_resol ,"') OR '",g_f_ini_resol,"' ='          '",
                    "\n     OR '",g_f_ini_resol ,"' is NULL  )",
                    "\n    AND tn.id_ret_matriz_derecho = md.id_ret_matriz_derecho "

                 IF g_folio IS NOT NULL THEN 
                      LET v_script = v_script CLIPPED ||"\n AND   (tn.folio = ",g_folio,")"
                 END IF 
 
               IF g_nss IS NOT NULL THEN 
                  IF g_id_derechohabiente IS NULL THEN
                    SELECT NVL(id_derechohabiente,0)
                      INTO g_id_derechohabiente 
                      FROM afi_derechohabiente
                     WHERE nss = g_nss

                     IF g_id_derechohabiente IS NULL THEN 
                       LET g_id_derechohabiente = 0
                     ELSE
                       LET v_script = v_script CLIPPED ||"\n AND  (tn.id_derechohabiente = ",g_id_derechohabiente,")"
                     END IF
                  ELSE
                      SELECT nss
                        INTO g_nss_validacion 
                        FROM afi_derechohabiente
                       WHERE id_derechohabiente = g_id_derechohabiente
                       
                       IF g_nss_validacion = g_nss THEN 
                          LET v_script = v_script CLIPPED ||"\n AND  (tn.id_derechohabiente = ",g_id_derechohabiente,")"
                       ELSE 
                          CALL fn_mensaje("Atención", "No son iguales nss y el derechohabiente proporcionados", "stop")
                       END IF 
                  END IF
               ELSE
                  IF g_id_derechohabiente IS NULL THEN 
                    LET g_id_derechohabiente = 0
                  ELSE
                    LET v_script = v_script CLIPPED ||"\n AND  (tn.id_derechohabiente = ",g_id_derechohabiente,")"
                  END IF
               END IF

               --IF g_tipo_retiro IS NOT NULL THEN 
                     --LET v_script = v_script CLIPPED || "\n   AND  (md.tpo_retiro       = '",g_tipo_retiro,"')"
               --END IF

               --IF g_regimen IS NOT NULL THEN 
                     --LET v_script = v_script CLIPPED || "\n   AND  (md.regimen       = '",g_regimen,"')"
               --END IF

               --IF g_tipo_pension IS NOT NULL THEN 
                     --LET v_script = v_script CLIPPED || "\n   AND  (md.tpo_pension       = '",g_tipo_pension,"')"
               --END IF

               --IF g_tipo_prestacion IS NOT NULL THEN 
                     --LET v_script = v_script CLIPPED || "\n   AND  (md.tpo_prestacion       = '",g_tipo_prestacion,"')"
               --END IF

    DISPLAY  v_script
    PREPARE p_tranferencia FROM v_script
    DECLARE cur_tranferencia CURSOR  FOR p_tranferencia
    FOREACH cur_tranferencia INTO r_tmp_ret_rch_solicitud.*
       INSERT INTO tmp_ret_rch_solicitud
            VALUES(r_tmp_ret_rch_solicitud.*)
    END FOREACH 
    
   LET v_script = "\n SELECT ",
                 "\n id_solicitud        ,",
                 "\n id_derechohabiente  ,",
                 "\n folio               ,",
                 "\n nss                 ,",
                 "\n tipo_retiro         ,",
                 "\n regimen             ,",
                 "\n tpo_pension         ,",
                 "\n tpo_prestacion      ,",
                 "\n f_inicio_pension    ,",
                 "\n f_resolucion        ,",
                 "\n tpo_seguro          ,",
                 "\n importe_viv72       ,",
                 "\n aivs_viv92          ,",
                 "\n aivs_viv97          ,",
                 '\n cod_rechazo_1       ,',
                 '\n cod_rechazo_2       ,',
                 '\n cod_rechazo_3        ',
                 "\n  FROM  tmp_ret_rch_solicitud"
                  
  --DISPLAY  v_script  
  PREPARE p_c_1 FROM v_script
  DECLARE c_1 CURSOR FOR p_c_1
  
  LET v_count = 1
  FOREACH c_1 INTO  arr_tmp_ret_rch_solicitud[v_count].*
  LET v_count = v_count + 1
  END FOREACH

   OPEN WINDOW ret_rch_consulta WITH FORM "RETF081"
   DISPLAY ARRAY  arr_tmp_ret_rch_solicitud TO t_retiro_rechazo_solic.*
        ATTRIBUTE (ACCEPT = FALSE ,CANCEL  = FALSE )
     
   ON ACTION regresar 
      EXIT DISPLAY 
      
   END DISPLAY 
   CLOSE WINDOW ret_rch_consulta
   DROP TABLE tmp_ret_rch_solicitud
END FUNCTION  
