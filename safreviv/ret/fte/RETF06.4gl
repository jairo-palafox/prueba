--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETF06                                        #
#Objetivo          =>Consulta del ret_datamart del spess           #
#Autor             =>Luis Erick Rodríguez Vázquez, EFP             #
#Fecha inicio      =>21 Agosto 2012                                #
####################################################################
IMPORT os   
DATABASE safre_viv

DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod
DEFINE p_tipo_ejecucion SMALLINT
DEFINE p_titulo         STRING

       
DEFINE v_ar_spess   DYNAMIC ARRAY OF RECORD LIKE ret_datamart.*
        
DEFINE v_ventana        ui.Window
DEFINE v_forma          ui.Form
DEFINE v_SqlQry         STRING
DEFINE cb               ui.ComboBox


MAIN
   DEFINE v_count   INTEGER
   DEFINE v_arr_cur INTEGER 
   
   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

 
   -- Menu principal de la captura de registros
   OPEN WINDOW w_mtto_spess WITH FORM "RETF062"
   IF p_titulo IS NOT NULL THEN
        CALL ui.Interface.setText(p_titulo)
        LET v_ventana = ui.Window.getCurrent()
        CALL v_ventana.setText(p_titulo)
    END IF
                      
    DECLARE pr_v_ar_spess CURSOR FOR SELECT  * 
                                          FROM ret_datamart 
    CALL  v_ar_spess.clear()
    
    LET v_count = 0
    FOREACH  pr_v_ar_spess INTO v_ar_spess[v_count + 1].*
       LET v_count  = v_count + 1
    END FOREACH 

    DISPLAY ARRAY v_ar_spess TO  f_e_retdatamart.*
       ATTRIBUTES (UNBUFFERED,CANCEL = FALSE ,ACCEPT  = FALSE)
       
     ON ACTION nuevo
         -- Rutina para la captura de registros nuevos
         CALL fn_captura_registro('A',0)
         
     ON ACTION modificar
         LET v_arr_cur = arr_curr() 
         -- Rutina para la captura de registros nuevos
         CALL fn_captura_registro('M',v_arr_cur)

     ON ACTION eliminar
         LET v_arr_cur = arr_curr() 
         -- Rutina para eliminar registros
         CALL fn_eliminar_registro(v_arr_cur)
        
      ON ACTION cancelar
         EXIT DISPLAY
         
   END DISPLAY  
   CLOSE WINDOW w_mtto_spess

END MAIN

FUNCTION  fn_captura_registro(p_modulo,p_arr_cur)
   DEFINE p_modulo                CHAR(1)
   DEFINE p_arr_cur               INTEGER 
   DEFINE r_datamart              RECORD LIKE ret_datamart.*
   DEFINE r_v_datamart            RECORD LIKE ret_datamart.*
   
   DEFINE  v_id_datamart          DECIMAL(9,0) ,
           v_nss                  CHAR(11)     , 
           v_sec_pension          SMALLINT     , 
           v_regimen              SMALLINT     , 
           v_tpo_seguro           CHAR(2)      , 
           v_tpo_pension          CHAR(2)      , 
           v_tpo_prestacion       CHAR(2)      , 
           v_diag_registro        CHAR(3)      , 
           v_folio                DECIMAL(9,0) , 
           v_curp                 CHAR(18)     , 
           v_nombre_datamart      CHAR(40)     , 
           v_nombre_afore         CHAR(40)     , 
           v_paterno_afore        CHAR(40)     , 
           v_materno_afore        CHAR(40)     , 
           v_tpo_movimiento       CHAR(3)      , 
           v_articulo_negativa    CHAR(3)      , 
           v_fraccion_negativa    CHAR(2)      , 
           v_num_considerando     CHAR(2)      , 
           v_f_inicio_pension     DATE         , 
           v_f_resolucion         DATE         , 
           v_porcentaje_valuacion DECIMAL(5,2) , 
           v_semanas_cotizadas    INTEGER      , 
           v_estado_sub_viv       SMALLINT     , 
           v_aivs_viv97           DECIMAL(14,6), 
           v_aivs_viv92           DECIMAL(14,6), 
           v_importe_viv72        DECIMAL(14,2), 
           v_cve_afore            SMALLINT
           
           
           IF p_modulo = 'M' THEN 
           DISPLAY p_arr_cur
              LET r_v_datamart.* = v_ar_spess[p_arr_cur].*
              --LET v_id_datamart            = v_ar_spess[p_arr_cur].id_datamart
              --LET v_nss                    = v_ar_spess[p_arr_cur].nss
              --LET v_sec_pension            = v_ar_spess[p_arr_cur].sec_pension
              --LET v_regimen                = v_ar_spess[p_arr_cur].regimen
              --LET v_tpo_seguro             = v_ar_spess[p_arr_cur].tpo_seguro
              --LET v_tpo_pension            = v_ar_spess[p_arr_cur].tpo_pension
              --LET v_tpo_prestacion         = v_ar_spess[p_arr_cur].tpo_prestacion
              --LET v_diag_registro          = v_ar_spess[p_arr_cur].diag_registro
              --LET v_folio                  = v_ar_spess[p_arr_cur].folio
              --LET v_curp                   = v_ar_spess[p_arr_cur].curp
              --LET v_nombre_datamart        = v_ar_spess[p_arr_cur].nombre_datamart
              --LET v_nombre_afore           = v_ar_spess[p_arr_cur].nombre_afore
              --LET v_paterno_afore          = v_ar_spess[p_arr_cur].paterno_afore
              --LET v_materno_afore          = v_ar_spess[p_arr_cur].materno_afore
              --LET v_tpo_movimiento         = v_ar_spess[p_arr_cur].tpo_movimiento
              --LET v_articulo_negativa      = v_ar_spess[p_arr_cur].articulo_negativa
              --LET v_fraccion_negativa      = v_ar_spess[p_arr_cur].fraccion_negativa
              --LET v_num_considerando       = v_ar_spess[p_arr_cur].num_considerando
              --LET v_f_inicio_pension       = v_ar_spess[p_arr_cur].f_inicio_pension
              --LET v_f_resolucion           = v_ar_spess[p_arr_cur].f_resolucion
              --LET v_porcentaje_valuacion   = v_ar_spess[p_arr_cur].porcentaje_valuacion
              --LET v_semanas_cotizadas      = v_ar_spess[p_arr_cur].semanas_cotizadas
              --LET v_estado_sub_viv         = v_ar_spess[p_arr_cur].estado_sub_viv
              --LET v_aivs_viv97             = v_ar_spess[p_arr_cur].aivs_viv97
              --LET v_aivs_viv92             = v_ar_spess[p_arr_cur].aivs_viv92
              --LET v_importe_viv72          = v_ar_spess[p_arr_cur].importe_viv72
              --LET v_cve_afore              = v_ar_spess[p_arr_cur].cve_afore
           END IF 
           
   OPEN WINDOW w_mtto_spess_alta_modifi WITH FORM "RETF061"
   INPUT  r_v_datamart.*
   	      --v_id_datamart          ,
          --v_nss                  ,
          --v_sec_pension          ,
          --v_regimen              ,
          --v_tpo_seguro           ,
          --v_tpo_pension          ,
          --v_tpo_prestacion       ,
          --v_diag_registro        ,
          --v_folio                ,
          --v_curp                 ,
          --v_nombre_datamart      ,
          --v_nombre_afore         ,
          --v_paterno_afore        ,
          --v_materno_afore        ,
          --v_tpo_movimiento       ,
          --v_articulo_negativa    ,
          --v_fraccion_negativa    ,
          --v_num_considerando     ,
          --v_f_inicio_pension     ,
          --v_f_resolucion         ,
          --v_porcentaje_valuacion ,
          --v_semanas_cotizadas    ,
          --v_estado_sub_viv       ,
          --v_aivs_viv97           ,
          --v_aivs_viv92           ,
          --v_importe_viv72        ,
          --v_cve_afore             
        WITHOUT DEFAULTS  
     FROM   f_e_retdatamart.*
          --v_id_datamart          ,
          --v_nss                  ,
          --v_sec_pension          ,
          --v_regimen              ,
          --v_tpo_seguro           ,
          --v_tpo_pension          ,
          --v_tpo_prestacion       ,
          --v_diag_registro        ,
          --v_folio                ,
          --v_curp                 ,
          --v_nombre_datamart      ,
          --v_nombre_afore         ,
          --v_paterno_afore        ,
          --v_materno_afore        ,
          --v_tpo_movimiento       ,
          --v_articulo_negativa    ,
          --v_fraccion_negativa    ,
          --v_num_considerando     ,
          --v_f_inicio_pension     ,
          --v_f_resolucion         ,
          --v_porcentaje_valuacion ,
          --v_semanas_cotizadas    ,
          --v_estado_sub_viv       ,
          --v_aivs_viv97           ,
          --v_aivs_viv92           ,
          --v_importe_viv72        ,
          --v_cve_afore
        ATTRIBUTES (UNBUFFERED,CANCEL = FALSE ,ACCEPT  = FALSE)  
     BEFORE INPUT
          
     ON ACTION guardar
        IF p_modulo = 'M' THEN 
            UPDATE ret_datamart 
              SET id_datamart            =  v_id_datamart
                  ,nss                    =  v_nss
                  ,sec_pension            =  v_sec_pension
                  ,regimen                =  v_regimen
                  ,tpo_seguro             =  v_tpo_seguro
                  ,tpo_pension            =  v_tpo_pension
                  ,tpo_prestacion         =  v_tpo_prestacion
                  ,diag_registro          =  v_diag_registro
                  ,folio                  =  v_folio
                  ,curp                   =  v_curp
                  ,nombre_datamart        =  v_nombre_datamart
                  ,nombre_afore           =  v_nombre_afore
                  ,paterno_afore          =  v_paterno_afore
                  ,materno_afore          =  v_materno_afore
                  ,tpo_movimiento         =  v_tpo_movimiento
                  ,articulo_negativa      =  v_articulo_negativa
                  ,fraccion_negativa      =  v_fraccion_negativa
                  ,num_considerando       =  v_num_considerando
                  ,f_inicio_pension       =  v_f_inicio_pension
                  ,f_resolucion           =  v_f_resolucion
                  ,porcentaje_valuacion   =  v_porcentaje_valuacion
                  ,semanas_cotizadas      =  v_semanas_cotizadas
                  ,estado_sub_viv         =  v_estado_sub_viv
                  ,aivs_viv97             =  v_aivs_viv97
                  ,aivs_viv92             =  v_aivs_viv92
                  ,importe_viv72          =  v_importe_viv72
                  ,cve_afore              =  v_cve_afore
              WHERE id_datamart      =  v_id_datamart
                  AND nss            =  v_nss
                  AND folio          =  v_folio
                  
             IF (SQLCA.SQLCODE <> 0) THEN
                CALL fn_mensaje("Error",
                      "Codigo de error encontrado "||SQLCA.SQLCODE,"info") 
                --DISPLAY "Error, Codigo de error encontrado " , SQLCA.SQLCODE
               EXIT INPUT
             ELSE
               CALL fn_mensaje("Atención",
                    "Actalzacion realzada correctamente","info") 
             END IF
         ELSE 
         	 INSERT INTO ret_datamart VALUES(v_id_datamart
                                          ,v_nss
                                          ,v_sec_pension
                                          ,v_regimen
                                          ,v_tpo_seguro
                                          ,v_tpo_pension
                                          ,v_tpo_prestacion
                                          ,v_diag_registro
                                          ,v_folio
                                          ,v_curp
                                          ,v_nombre_datamart
                                          ,v_nombre_afore
                                          ,v_paterno_afore
                                          ,v_materno_afore
                                          ,v_tpo_movimiento
                                          ,v_articulo_negativa
                                          ,v_fraccion_negativa
                                          ,v_num_considerando
                                          ,v_f_inicio_pension
                                          ,v_f_resolucion
                                          ,v_porcentaje_valuacion
                                          ,v_semanas_cotizadas
                                          ,v_estado_sub_viv
                                          ,v_aivs_viv97
                                          ,v_aivs_viv92
                                          ,v_importe_viv72
                                          ,v_cve_afore)
                                          
             IF (SQLCA.SQLCODE <> 0) THEN
               DISPLAY "Error, Codigo de error encontrado " , SQLCA.SQLCODE
               EXIT INPUT
               --fn_mensaje
             END IF  
        END IF 
        
        ON ACTION cancelar
           EXIT INPUT
        
   END INPUT
   CLOSE WINDOW w_mtto_spess_alta_modifi 
   
END FUNCTION

FUNCTION  fn_eliminar_registro(p_arr_cur)
   DEFINE p_modulo                CHAR(1)
   DEFINE p_arr_cur               INTEGER 
   DEFINE r_datamart              RECORD  LIKE  ret_datamart.*

    DELETE 
     FROM ret_datamart 
     WHERE id_datamart      =  v_ar_spess[p_arr_cur].id_datamart

     IF (SQLCA.SQLCODE <> 0) THEN
               DISPLAY "Error, Codigo de error encontrado " , SQLCA.SQLCODE
               --fn_mensaje
     END IF
     
END FUNCTION 