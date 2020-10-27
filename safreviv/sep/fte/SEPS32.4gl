--=============================================================================                                                           
-- VERSION: 1.0.0                                                                                                                         
-- FECHA ULTIMA MODIFICACION:                                                                                                             
--=============================================================================                                                           
################################################################################                                                          
#MODULO            =>RET                                                       #                                                          
#PROGRAMA          =>SEPS32                                                    #                                                          
#OBJETIVO          =>GENERA ARCHIVO DE RECHAZOS    SEPARADOR DETALLE 02-03     #                           #                                                          
#                                                                              #                                                          
#                                                                              #                                                          
################################################################################                                                          
DATABASE safre_viv                                                                                                                        
                                                                                                                                          
GLOBALS                                                                                                                                   
DEFINE g_pid            LIKE bat_ctr_proceso.pid,           --  ID del proceso                                                            
       g_proceso_cod    LIKE cat_proceso.proceso_cod,       -- codigo del proceso                                                         
       g_opera_cod      LIKE cat_operacion.opera_cod,       -- codigo de operacion                                                        
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo                                                         
END GLOBALS                                                                                                                               
                                                                                                                                          
--  MAIN                                                                                                                                        
FUNCTION fn_archivo_rechazo_op28(p_folio , p_usuario_cod)                                                                                 
   DEFINE v_query     STRING                                                                                                                 
                                                                                                                                          
    --CALL fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)                                                              
                                                                                                                                          
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod,  -- nombre del usuario                                                     
          p_folio                LIKE glo_folio.folio           -- numero de folio                                                        
                                                                                                                                          
                                                                                                                                          
   DEFINE                                                                                                                                 
                                                                                                                                          
          v_v_nom_archivo                  STRING,        -- nombre del archivo de salida                                                 
          v_v_ruta_nomarch                 STRING,        -- ruta y nombre del archivo de salida                                          
          v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo                               
          v_ch_arch_separacion              BASE.CHANNEL,  -- manejador de apuntador hacia archivo                                        
          v_s_registro                     STRING,        -- registro a insertar                                                          
                                                                                                                                          
          v_id_det_02_op28 DECIMAL(9)                                                                                                     
         ,v_tpo_registro CHAR(2)       --Tipo de registro                                          X   02   0   001   -   002             
         ,v_cont_serv    CHAR(10)      --Contador de servicio                                      X   10   0   003   -   012             
         ,v_nss_sepa     CHAR(11)      --NSS Separado                                              X   11   0   013   -   023             
         ,v_mrc_cta      CHAR(1)       --Marcaje de cuenta                                         X   01   0   024   -   024             
         ,v_tot_viv92    DECIMAL(16,6)  --Número total de Aplicación de Intereses de Vivienda 92    9   09   06   025   -   039           
         ,v_tot_viv97    DECIMAL(16,6)  --Número total de Aplicación de Intereses de Vivienda 97    9   09   06   040   -   054           
         ,v_res_opera    CHAR(2)       --Resultado de la operación                                 X   02   0   055   -   056             
         ,v_diag_1       CHAR(3)       --Diagnóstico 1                                             X   03   0   057   -   059             
         ,v_diag_2       CHAR(3)       --Diagnóstico 2                                             X   03   0   060   -   062             
         ,v_diag_3       CHAR(3)       --Diagnóstico 3                                             X   03   0   063   -   065             
         ,v_clf_separ    CHAR(1)       --Clasificación de la separación                            X   01   0   066   -   066             
         --,v_filler       CHAR(234)     --Filler                                                    X   234   0   067   -   300          
                                                                                                                                          
         ,v_tot_viv92_op28    DECIMAL(15,0)  --Número total de Aplicación de Intereses de Vivienda 92    9   09   06   025   -   039      
         ,v_tot_viv97_op28    DECIMAL(15,0)  --Número total de Aplicación de Intereses de Vivienda 97    9   09   06   040   -   054      
                                                                                                                                          
         ,v_03_tpo_registro CHAR(2)                                                                                                       
         ,v_03_cont_serv    CHAR(10)                                                                                                      
         ,v_03_nss_trab     CHAR(11)                                                                                                      
         ,v_03_mrc_cta      CHAR(1)                                                                                                       
         ,v_03_tot_viv92    DECIMAL(16,6)                                                                                                 
         ,v_03_tot_viv97    DECIMAL(16,6)                                                                                                 
         ,v_03_res_opera    CHAR(2)                                                                                                       
         ,v_03_diag_1       CHAR(3)                                                                                                       
         ,v_03_diag_2       CHAR(3)                                                                                                       
         ,v_03_diag_3       CHAR(3)                                                                                                       
         --,v_03_filler       CHAR(234)                                                                                                   
                                                                                                                                          
         ,v_03_tot_viv92_op28    DECIMAL(15,0)  --Número total de Aplicación de Intereses de Vivienda 92    9   09   06   025   -   039   
         ,v_03_tot_viv97_op28    DECIMAL(15,0)  --Número total de Aplicación de Intereses de Vivienda 97    9   09   06   040   -   054  

        ,v_cba_tpo_registro  CHAR(2)      --1 Tipo de registro             X 02 0 001 - 002
        ,v_cba_id_servicio   CHAR(2)      --2 Identificador de servicio    X 02 0 003 - 004
        ,v_cba_id_operacion  CHAR(2)      --3 Identificador de operación   X 02 0 005 - 006
        ,v_cba_tpo_ent_orig  CHAR(2)      --4 Tipo entidad origen          X 02 0 007 - 008
        ,v_cba_cve_ent_orig  CHAR(3)      --5 Clave entidad origen         X 03 0 009 - 011
        ,v_cba_tpo_ent_dest  CHAR(2)      --6 Tipo entidad  destino        X 02 0 012 - 013
        ,v_cba_cve_ent_dest  CHAR(3)      --7 Clave entidad destino        X 03 0 014 - 016
        ,v_cba_f_trasnf_lot  CHAR(8)      --8 Fecha transferencia lote     X 08 0 017 - 024
        ,v_cba_cons_dia      DECIMAL(3,0) --9 Consecutivo del día          9 03 0 025 - 027
        ,v_cba_res_operacion CHAR(2)      --10 Resultado de la operación   X 02 0 028 - 029
        ,v_cba_mot_rech_lot  CHAR(9)      --11 Motivo de Rechazo de lote   X 09 0 030 - 038
        --,v_cba_fller       CHAR(262)    --13 Filler                      X 262 0 039 - 300

        ,v_sum_tpo_registro  char(1)       --1 Tipo de registro     X 02 0 001 
        ,v_sum_total_det_02  decimal(10,0) --2 Total de detalles 02 9 10 0 003 
        ,v_sum_total_det_03  decimal(10,0) --3 Total de detalles 03 9 10 0 013 
        ,v_sum_total_regstro decimal(10,0) --4 Total de registros   9 10 0 023 
        --,v_sum_Filler      char(268)     --5 Filler               X 268 0 033

                                                                                                                                          
   DEFINE v_ind SMALLINT;                                                                                                           
   DEFINE v_diag CHAR(3);                                                                                                           
   DEFINE v_estado_destino SMALLINT;                                                                                                
                                                                                                                                          
                                                                                                                                          
--        LET  p_folio = 3643                                                                                                             
--        LET  p_usuario_cod = 'safreviv'   
                                                                                              
             -- se crea el archivo log                                                                                                    
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".SEPS32.log")                                                                                   
                                                                                                                                          
    -- se obtienen la ruta envio del modulo                                                                                               
    SELECT ruta_envio                                                                                                                     
      INTO v_c_ruta_env_acr                                                                                                               
      FROM seg_modulo                                                                                                                     
     WHERE modulo_cod = 'sep'                                                                                                             
                                                                                                                                          
                                                                                                                                          
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta                                                          
   --se modifica el nombre del archivo que comprende el año-mes-dia-consecutivo.soloInfonavit                                          
   --LET v_v_nom_archivo = "/" ||"solicitudes_liq_tesoreria_" || p_folio || ".ret"}                                                    
   LET v_v_nom_archivo = "/",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&",YEAR(TODAY) USING "&&&&",p_folio USING "&&&&&&" ,"_rechazo_op28.txt"
                                                                                                                                       
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo                                                                  
   -- se crea el manejador de archivo                                                                                                  
   --DISPLAY v_v_ruta_nomarch                                                                                                          
   LET v_ch_arch_separacion = base.Channel.create()                                                                                    
                                                                                                                                       
   -- se crea archivo y se indica que se escribira en el mismo                                                                         
   CALL v_ch_arch_separacion.openFile(v_v_ruta_nomarch, "w" )                                                                          
   CALL v_ch_arch_separacion.setDelimiter("")                                                                                          
   --sep_det_02_op28       
      -----------------------
        LET v_query =            "\n select tipo_registro                     ",
                                 "\n       ,id_servicio                       ",
                                 "\n       ,id_operacion                      ",
                                 "\n       ,tipo_ent_origen                   ",
                                 "\n       ,cve_ent_origen                    ",
                                 "\n       ,tipo_ent_destino                  ",
                                 "\n       ,cve_ent_destino                   ",
                                 "\n       ,f_trans_lote                      ",
                                 "\n       ,consecutivo_dia                   ",
                                 "\n       ,resultado_operacion               ",
                                 "\n       ,diagnostico                       ",
                                 "\n      FROM  sep_cza_op28                  ",
                                 "\n     WHERE folio         =  ?             "                                 
                                                                                                                                          
    --DISPLAY v_query                                                                                                                     
    PREPARE pr_cabezado FROM v_query                                                                                                        
    DECLARE cur_cabezado CURSOR FOR pr_cabezado                                                                                               
    FOREACH cur_cabezado USING p_folio                                                                                                      
                      INTO v_cba_tpo_registro  
                           ,v_cba_id_servicio   
                           ,v_cba_id_operacion  
                           ,v_cba_tpo_ent_orig  
                           ,v_cba_cve_ent_orig  
                           ,v_cba_tpo_ent_dest  
                           ,v_cba_cve_ent_dest  
                           ,v_cba_f_trasnf_lot  
                           ,v_cba_cons_dia      
                           ,v_cba_res_operacion 
                           ,v_cba_mot_rech_lot  

       LET v_s_registro =   v_cba_tpo_registro  USING "##"
                           ,v_cba_id_servicio   USING "##"
                           ,v_cba_id_operacion  USING "##"
                           ,v_cba_tpo_ent_orig  USING "##"
                           ,v_cba_cve_ent_orig  USING "###"
                           ,v_cba_tpo_ent_dest  USING "##"
                           ,v_cba_cve_ent_dest  USING "###"
                           ,v_cba_f_trasnf_lot  USING "mmddyyyy"
                           ,v_cba_cons_dia      USING "&&&"
                           ,v_cba_res_operacion USING "##"
                           ,v_cba_mot_rech_lot  USING "#########"                                                                                                                   
                           ,262 SPACES                                                                                                         
                                                                                                                                          
       -- se escribe el registro                                                                                                           
       --DISPLAY v_s_registro                                                                                                              
       CALL v_ch_arch_separacion.write([v_s_registro])                             
    END FOREACH 
      --------------------------   
                                                                                                                                       
      LET v_query =   "\n SELECT    id_det_02_op28         ",                                                                          
                      "\n          , tipo_registro          ",                                                                         
                      "\n           ,contador_servicio      ",                                                                         
                      "\n           ,invadido               ",                                                                         
                      "\n           ,ind_marca              ",                                                                         
                      "\n           ,saldo_viv_92           ",                                                                         
                      "\n           ,saldo_viv_97           ",                                                                         
                      "\n           ,resultado_operacion    ",                                                                         
                      "\n           ,diagnostico1           ",                                                                         
                      "\n           ,diagnostico2           ",                                                                         
                      "\n           ,diagnostico3           ",                                                                         
                      "\n           ,clasifica_separacion   ",                                                                         
                      "\n      FROM  sep_det_02_op28        ",                                                                         
                      "\n     WHERE folio         =  ?      ",                                                                         
                      "\n       AND estado = 30             ",                                                                         
                      "\n     ORDER BY id_det_02_op28 ,contador_servicio"                                                              
                                                                                                                                          
    --DISPLAY v_query                                                                                                                     
    PREPARE pr_separa FROM v_query                                                                                                        
    DECLARE cur_separa CURSOR FOR pr_separa                                                                                               
    FOREACH cur_separa USING p_folio                                                                                                      
                      INTO v_id_det_02_op28                                                                                               
                          ,v_tpo_registro                                                                                                 
                          ,v_cont_serv                                                                                                    
                          ,v_nss_sepa                                                                                                     
                          ,v_mrc_cta                                                                                                      
                          ,v_tot_viv92                                                                                                    
                          ,v_tot_viv97                                                                                                     
                          ,v_res_opera                                                                                                    
                          ,v_diag_1                                                                                                       
                          ,v_diag_2                                                                                                       
                          ,v_diag_3                                                                                                       
                          ,v_clf_separ                                                                                                    
                                                                                                                                          
       --Tipo de registro                                          X   02   0   001   -   002                                                   
       --Contador de servicio                                      X   10   0   003   -   012                                                   
       --NSS Separado                                              X   11   0   013   -   023                                                   
       --Marcaje de cuenta                                         X   01   0   024   -   024                                                   
       --Número total de Aplicación de Intereses de Vivienda 92    9   09   06  025   -   039                                                   
       --Número total de Aplicación de Intereses de Vivienda 97    9   09   06  040   -   054                                                   
       --Resultado de la operación                                 X   02   0   055   -   056                                                   
       --Diagnóstico 1                                             X   03   0   057   -   059                                                   
       --Diagnóstico 2                                             X   03   0   060   -   062                                                   
       --Diagnóstico 3                                             X   03   0   063   -   065                                                   
       --Clasificación de la separación                            X   01   0   066   -   066                                                   
       --Filler                                                    X   234  0   067   -   300                                                   
                                                                                                                                          
       LET v_tot_viv92_op28   =  v_tot_viv92 * 1000000                                                                                          
       LET v_tot_viv97_op28   =  v_tot_viv97 * 1000000                                                                                          
                                                                                                                                            
       LET v_s_registro =   v_tpo_registro    USING "##"                                                                                       
                           ,v_cont_serv       USING "##########"                                                                               
                           ,v_nss_sepa        USING "###########"                                                                              
                           ,v_mrc_cta         USING "#"                                                                                        
                           ,v_tot_viv92_op28  USING "&&&&&&&&&&&&&&&"                                                                          
                           ,v_tot_viv97_op28  USING "&&&&&&&&&&&&&&&"                                                                          
                           ,v_res_opera       USING "##"                                                                                       
                           ,v_diag_1                                                                                                           
                           ,v_diag_2                                                                                                           
                           ,v_diag_3                                                                                                           
                           ,v_clf_separ                                                                                                        
                           ,233 SPACES                                                                                                         
                                                                                                                                          
       -- se escribe el registro                                                                                                           
       --DISPLAY v_s_registro                                                                                                              
       CALL v_ch_arch_separacion.write([v_s_registro])                                                                                     
                                                                                                                                          
         LET v_query =   "\n SELECT                           ",                                                                          
                         "\n           tipo_registro          ",                                                                          
                         "\n           ,contador_servicio      ",                                                                         
                         "\n           ,asociado               ",                                                                         
                         "\n           ,ind_marca              ",                                                                         
                         "\n           ,saldo_viv_92           ",                                                                         
                         "\n           ,saldo_viv_97           ",                                                                         
                         "\n           ,resultado_operacion    ",                                                                         
                         "\n           ,diagnostico1           ",                                                                         
                         "\n           ,diagnostico2           ",                                                                         
                         "\n           ,diagnostico3           ",                                                                         
                         "\n      FROM  sep_det_03_op28        ",                                                                         
                         "\n     WHERE folio         =  ?      ",                                                                         
                         "\n       and id_det_02_op28 = ?      ",                                                                         
                         "\n     ORDER BY id_det_02_op28, contador_servicio "                                                             
                                                                                                                                          
       --DISPLAY v_query                                                                                                                     
       PREPARE pr_asociado FROM v_query                                                                                                      
       DECLARE cur_asociado CURSOR FOR pr_asociado                                                                                           
       FOREACH cur_asociado USING p_folio,v_id_det_02_op28                                                                                   
                         INTO  v_03_tpo_registro                                                                                             
                              ,v_03_cont_serv                                                                                                
                              ,v_03_nss_trab                                                                                                 
                              ,v_03_mrc_cta                                                                                                  
                              ,v_03_tot_viv92                                                                                                
                              ,v_03_tot_viv97                                                                                                
                              ,v_03_res_opera                                                                                                
                              ,v_03_diag_1                                                                                                   
                              ,v_03_diag_2                                                                                                   
                              ,v_03_diag_3                                                                                                   
                                                                                                                                             
                                                                                                                                             
            LET v_03_tot_viv92_op28   =  v_03_tot_viv92 * 1000000                                                                             
            LET v_03_tot_viv97_op28   =  v_03_tot_viv97 * 1000000                                                                             
                                                                                                                                             
            LET v_s_registro =   v_03_tpo_registro  USING "##"                                                                               
                                ,v_03_cont_serv     USING "##########"                                                                       
                                ,v_03_nss_trab      USING "###########"                                                                      
                                ,v_03_mrc_cta       USING "#"                                                                                
                                ,v_03_tot_viv92_op28    USING "&&&&&&&&&&&&&&&"                                                              
                                ,v_03_tot_viv97_op28    USING "&&&&&&&&&&&&&&&"                                                              
                                ,v_03_res_opera     USING "##"                                                                               
                                ,v_03_diag_1                                                                                                 
                                ,v_03_diag_2                                                                                                 
                                ,v_03_diag_3                                                                                                 
                                ,234 SPACES                                                                                                  
                                                                                                                                             
           -- se escribe el registro                                                                                                           
           --DISPLAY v_s_registro                                                                                                              
           CALL v_ch_arch_separacion.write([v_s_registro])                                                                                     
       END FOREACH
       PREPARE prp_maq_ind FROM " EXECUTE FUNCTION safre_viv:fn_maquinaria(?,?,?)"
       EXECUTE prp_maq_ind USING "maq_sep_ctr_op28",
                                 "15",
                                 "10"
                            INTO v_ind ,
                                 v_diag ,
                                 v_estado_destino
       
       IF v_ind <> 0 THEN                                                                                                                    
             DISPLAY "Error en maquinaria de encabezado código: " || v_ind , "\nDiagnóstico: " || v_diag                                                  
             EXIT FOREACH
       ELSE
          # actualiza el estado del encabezado a rechazos informados
          UPDATE sep_cza_op28
             SET estado = v_estado_destino
           WHERE folio = p_folio
       END IF
       --DISPLAY "maq_sep_det_op28"," ",v_id_det_02_op28                                                                                   
             --," ","id_det_02_op28"," ","35"," ",p_usuario_cod                                                                            
       PREPARE pr_map FROM " EXECUTE FUNCTION safre_viv:fn_maquinaria_individual(?,?,?,?,?)"
       EXECUTE  pr_map USING "maq_sep_det_op28"   --  char(40),                                                                            
                             ,v_id_det_02_op28    -- DEC(9,0),                                                                             
                             ,"id_det_02_op28"    -- CHAR(40),                                                                             
                             ,"30"                -- SMALLINT,                                                                             
                             ,p_usuario_cod       -- CHAR(20))                                                                             
                       INTO  v_ind, v_diag, v_estado_destino                                                                               
                                                                                                                                           
       IF v_ind <> 0 THEN                                                                                                                    
             DISPLAY "Error en maquinaria de detalle código: " || v_ind , "\nDiagnóstico: " || v_diag                                                  
             EXIT FOREACH                                                                                                                    
       END IF                                                                                                                                
    END FOREACH      
      -----------------------
        LET v_query =            "\n SELECT                    ",
                                 "\n      tipo_registro,       ",
                                 "\n      total_registro_det2, ",
                                 "\n      total_registro_det3, ",
                                 "\n      total_registros      ",
                                 "\n      FROM  sep_sum_op28   ",
                                 "\n     WHERE folio =  ?      " 
                                                                                                                                          
    --DISPLAY v_query                                                                                                                     
    PREPARE pr_saldo FROM v_query                                                                                                        
    DECLARE cur_saldo CURSOR FOR pr_saldo                                                                                               
    FOREACH cur_saldo USING p_folio                                                                                                      
                      INTO  v_sum_tpo_registro    
                            ,v_sum_total_det_02    
                            ,v_sum_total_det_03    
                            ,v_sum_total_regstro   
  

       LET v_s_registro =   v_sum_tpo_registro     USING "##"
                            ,v_sum_total_det_02    USING "&&&&&&&&&&&"
                            ,v_sum_total_det_03    USING "&&&&&&&&&&&"
                            ,v_sum_total_regstro   USING "&&&&&&&&&&"
                            ,268 SPACES                                                                                                         
                                                                                                                                          
       -- se escribe el registro                                                                                                           
       --DISPLAY v_s_registro                                                                                                              
       CALL v_ch_arch_separacion.write([v_s_registro])                             
    END FOREACH 
      --------------------------
    
END FUNCTION
--END MAIN                                                                                                                               