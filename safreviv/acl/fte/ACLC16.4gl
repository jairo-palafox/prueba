--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:26/07/2012
--===============================================================

################################################################################
#Modulo       => ACL                                                           #
#Programa     => ACLC16                                                        #
#Objetivo     => Inconsistencias aclaratorio sin cambio                        #
#Fecha inicio => 25 de Julio de 2012                                           #
#Autor        => Luis Erick Rodríguez Vázquez                                  #
#Modifica     => Rubén Haro Castro                                             # 
################################################################################

DATABASE safre_viv
GLOBALS 
DEFINE arr_salida_aclara_cc    DYNAMIC ARRAY OF RECORD
           cve_ent_receptora           LIKE cta_his_pagos.cve_ent_receptora
          ,num_reg_pat                 LIKE cta_his_pagos.nrp
          ,rfc_pat                     LIKE cta_pag_complemento.rfc_patron
          ,per_pago                    LIKE cta_his_pagos.periodo_pago
          ,f_pgo_pat                   LIKE cta_his_pagos.f_pago
          ,num_folio_sua               LIKE cta_his_pagos.folio_sua
          ,nss                         LIKE afi_derechohabiente.nss
          ,rfc_trab                    LIKE afi_derechohabiente.rfc
          ,curp_trab                   LIKE afi_derechohabiente.curp
          ,num_cred_info               LIKE cta_his_pagos.num_crd_ifv
          ,f_ini_dec_info              LIKE cta_pag_complemento.f_ini_desc_crd_ifv
          ,num_mov_per                 LIKE cta_pag_complemento.num_mov_periodo
          ,nom_trab                    LIKE afi_derechohabiente.nombre_imss
          ,ult_sal_dia_int_per         LIKE cta_pag_complemento.ult_sdi
          ,tpo_trab                    LIKE cta_pag_complemento.tpo_trabajador
          ,jor_sem_redu                LIKE cta_pag_complemento.jornada
          ,loc_trab                    LIKE cta_his_pagos.localiza_trabajador
          ,dest_ap_viv                 LIKE cta_pag_complemento.destino_ap_viv
          ,dias_cot_bim                LIKE cta_pag_complemento.dias_cot_bim 
          ,dias_inca_bim               LIKE cta_pag_complemento.dias_incap_bim
          ,dias_ausent_bim             LIKE cta_pag_complemento.dias_ausent_bim
          ,imp_ap_pat                  LIKE cta_his_pagos.imp_ap_pat          --imp_ap_pat      
          ,imp_am_cred                 LIKE cta_his_pagos.imp_am_cre          --imp_am_cre      
          ,imp_ren_viv_pago_ext        LIKE cta_his_pagos.imp_ren_viv_pgo_ext --imp_ren_viv_    
          ,marca_cred_sua              LIKE cta_pag_complemento.marca_sua
          ,marca_cred_bonsar           LIKE cta_pag_complemento.marca_bdnsar
          ,diag_aclara                 LIKE cta_his_pagos.tpo_aclaracion --tpo_aclaracion ct_
          ,f_proceso                   LIKE cta_his_pagos.f_proceso
          ,aiv_ap_pat                  LIKE cta_his_pagos.aiv_ap_pat  --aiv_ap_pat     
          ,valor_aiv                   LIKE cta_his_pagos.valor_aiv   --valor del AI  
          ,int_gen_pgo_ext             LIKE cta_his_pagos.int_gen_pgo_ext  --int_gen_pgo_e  
          ,aiv_gen_pgo_ext             LIKE cta_his_pagos.aiv_gen_pgo_ext  --aiv_gen_pgo_ex 
          ,estado_pago_desc            CHAR(60) --LIKE pag_cat_edo_pago.estado_pago_desc --
       END RECORD,                                                                 
       g_ruta_envio               LIKE seg_modulo.ruta_envio,
       g_nom_archivo              VARCHAR(50),
       g_origen_archivo           SMALLINT
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
       p_s_titulo       STRING                        -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_origen_archivo = 5
   
   -- consulta de informacion Reporte de Inconsistencias 
   CALL fn_opciones_reporte_inconsistencia(p_usuario_cod, p_s_titulo)

END MAIN

{ ======================================================================
Clave: ACLC16
Nombre: fn_opciones_reporte_inconsistencia
Fecha creacion: 25 de Julio de 2012  
Autor: Luis Erick Rodríguez Vázquez 
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_opciones_reporte_inconsistencia(p_usuario_cod, p_s_titulo)
DEFINE p_usuario_cod                LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_s_titulo                   STRING, -- titulo de la ventana
       v_folio                      DECIMAL(9,0), -- folio       
       v_query                      STRING,
       v_registros                  INTEGER,
       v_indice                     INTEGER,  --Indice generl de registrtos
       v_i_indice_for               SMALLINT,
       v_tot_imp_ap_pat             LIKE acl_sum_sc_nss.suma_ap_pat,
       v_tot_imp_am_cre             LIKE acl_sum_sc_nss.suma_am,
       v_tot_imp_ren_viv_pgo_ext    DECIMAL(12,2),--
       v_tot_aiv_ap_pat             LIKE acl_sum_sc_nss.suma_aivs,    
       v_tot_int_gen_pgo_ext        LIKE acl_sum_sc_nss.suma_int_viv_pgo_ext,
       v_tot_aiv_gen_pgo_ext        LIKE acl_sum_sc_nss.suma_aiv_pgo_ext

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

    CLOSE WINDOW SCREEN 
    --se abre la ventana de  forma de captura de parametros de busqueda
   OPEN WINDOW w_acl_rep_inconsistencias WITH FORM "ACLC161"

      INPUT v_folio
            WITHOUT DEFAULTS
         FROM folio
         ATTRIBUTE (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

          BEFORE INPUT  
             LET INT_FLAG = FALSE

          ON ACTION aceptar
              
             IF (v_folio IS NULL) THEN
                CALL fn_mensaje("Consulta","Debe de ingresar un campo de búsqueda","about")
             ELSE
                ACCEPT INPUT  
             END IF

          ON ACTION cancelar
             LET INT_FLAG = TRUE
             EXIT INPUT 
      END INPUT 

      IF NOT INT_FLAG THEN           
      	
      	 LET v_tot_imp_ap_pat           = 0 
         LET v_tot_imp_am_cre           = 0
         LET v_tot_imp_ren_viv_pgo_ext  = 0
         LET v_tot_aiv_ap_pat           = 0
         LET v_tot_int_gen_pgo_ext      = 0
         LET v_tot_aiv_gen_pgo_ext      = 0
         
          LET v_query =  "\n   SELECT  COUNT (*)",
                            "\n   FROM cta_his_pagos a, cta_pag_complemento b, afi_derechohabiente c ",
                            "\n  WHERE a.folio              = ", v_folio,
                            "\n    AND a.id_derechohabiente = c.id_derechohabiente ",
                            "\n    AND a.id_derechohabiente = b.id_derechohabiente ",
                            "\n    AND a.id_referencia      = b.id_referencia",
                            "\n    AND a.folio              = b.folio",
                            "\n    AND a.origen_archivo     = ",g_origen_archivo,
                            "\n    AND a.result_operacion   = '02'"
                            --display " v_query  " ,v_query
                            PREPARE prp_conteo FROM v_query
                            EXECUTE prp_conteo INTO v_registros
         --valida que se econtrarón registros
         IF v_registros > 0 THEN
            --realizala busqueda para llenar el arreglo
             LET v_query =  "\n   SELECT                   ",
                            "\n      a.cve_ent_receptora   ",
                            "\n     ,a.nrp                 ",
                            "\n     ,b.rfc_patron          ",
                            "\n     ,a.periodo_pago        ",
                            "\n     ,a.f_pago              ",
                            "\n     ,a.folio_sua           ",
                            "\n     ,c.nss                 ",
                            "\n     ,b.rfc                 ",
                            "\n     ,c.curp                ",
                            "\n     ,a.num_crd_ifv         ",
                            "\n     ,b.f_ini_desc_crd_ifv  ",
                            "\n     ,b.num_mov_periodo     ",
                            "\n     ,c.nombre_imss         ",
                            "\n     ,b.ult_sdi             ",
                            "\n     ,b.tpo_trabajador      ",
                            "\n     ,b.jornada             ", 
                            "\n     ,a.localiza_trabajador ",
                            "\n     ,b.destino_ap_viv      ",
                            "\n     ,b.dias_cot_bim        ",
                            "\n     ,b.dias_incap_bim      ",
                            "\n     ,b.dias_ausent_bim     ",
                            "\n     ,a.imp_ap_pat          ",
                            "\n     ,a.imp_am_cre          ",
                            "\n     ,a.imp_ren_viv_pgo_ext ",
                            "\n     ,b.marca_sua           ",
                            "\n     ,b.marca_bdnsar        ",
                            "\n     ,a.tpo_aclaracion      ",
                            "\n     ,a.f_proceso           ",
                            "\n     ,a.aiv_ap_pat          ",
                            "\n     ,a.valor_aiv           ",
                            "\n     ,a.int_gen_pgo_ext     ",
                            "\n     ,a.aiv_gen_pgo_ext     ",
                            "\n     ,e.estado_pago_desc    ",
                            "\n   FROM cta_his_pagos a, cta_pag_complemento b, afi_derechohabiente c, ",
                            "\n        pag_ctr_pago d,  pag_cat_edo_pago e ",
                            "\n  WHERE a.folio              = ", v_folio,
                            "\n    AND a.id_derechohabiente = c.id_derechohabiente",
                            "\n    AND a.id_derechohabiente = b.id_derechohabiente",
                            "\n    AND a.id_referencia      = b.id_referencia     ",
                            "\n    AND a.folio              = b.folio             ",
                            "\n    AND a.folio              = d.folio             ",
                            "\n    AND a.id_referencia      = d.id_referencia     ",
                            "\n    AND d.estado_pago        = e.estado_pago       ",
                            "\n    AND a.origen_archivo     = ",g_origen_archivo,
                            "\n    AND a.result_operacion   = '02'                 "
            --DISPLAY v_query
            PREPARE prp_cur_folio FROM v_query
            DECLARE cur_folio CURSOR FOR prp_cur_folio

            LET v_indice = 1
            --DISPLAY " v_indice",v_indice
            --llen ael arreglo
            FOREACH cur_folio INTO  arr_salida_aclara_cc[v_indice].cve_ent_receptora
                                   ,arr_salida_aclara_cc[v_indice].num_reg_pat              
                                   ,arr_salida_aclara_cc[v_indice].rfc_pat                 
                                   ,arr_salida_aclara_cc[v_indice].per_pago                
                                   ,arr_salida_aclara_cc[v_indice].f_pgo_pat               
                                   ,arr_salida_aclara_cc[v_indice].num_folio_sua           
                                   ,arr_salida_aclara_cc[v_indice].nss                     
                                   ,arr_salida_aclara_cc[v_indice].rfc_trab                
                                   ,arr_salida_aclara_cc[v_indice].curp_trab               
                                   ,arr_salida_aclara_cc[v_indice].num_cred_info           
                                   ,arr_salida_aclara_cc[v_indice].f_ini_dec_info          
                                   ,arr_salida_aclara_cc[v_indice].num_mov_per             
                                   ,arr_salida_aclara_cc[v_indice].nom_trab                
                                   ,arr_salida_aclara_cc[v_indice].ult_sal_dia_int_per     
                                   ,arr_salida_aclara_cc[v_indice].tpo_trab                
                                   ,arr_salida_aclara_cc[v_indice].jor_sem_redu            
                                   ,arr_salida_aclara_cc[v_indice].loc_trab                
                                   ,arr_salida_aclara_cc[v_indice].dest_ap_viv             
                                   ,arr_salida_aclara_cc[v_indice].dias_cot_bim            
                                   ,arr_salida_aclara_cc[v_indice].dias_inca_bim           
                                   ,arr_salida_aclara_cc[v_indice].dias_ausent_bim         
                                   ,arr_salida_aclara_cc[v_indice].imp_ap_pat              
                                   ,arr_salida_aclara_cc[v_indice].imp_am_cred             
                                   ,arr_salida_aclara_cc[v_indice].imp_ren_viv_pago_ext    
                                   ,arr_salida_aclara_cc[v_indice].marca_cred_sua          
                                   ,arr_salida_aclara_cc[v_indice].marca_cred_bonsar       
                                   ,arr_salida_aclara_cc[v_indice].diag_aclara             
                                   ,arr_salida_aclara_cc[v_indice].f_proceso               
                                   ,arr_salida_aclara_cc[v_indice].aiv_ap_pat              
                                   ,arr_salida_aclara_cc[v_indice].valor_aiv               
                                   ,arr_salida_aclara_cc[v_indice].int_gen_pgo_ext         
                                   ,arr_salida_aclara_cc[v_indice].aiv_gen_pgo_ext
                                   ,arr_salida_aclara_cc[v_indice].estado_pago_desc         

                     LET v_tot_imp_ap_pat          = v_tot_imp_ap_pat          +  arr_salida_aclara_cc[v_indice].imp_ap_pat
                     LET v_tot_imp_am_cre          = v_tot_imp_am_cre          +  arr_salida_aclara_cc[v_indice].imp_am_cred
                     LET v_tot_imp_ren_viv_pgo_ext = v_tot_imp_ren_viv_pgo_ext +  arr_salida_aclara_cc[v_indice].imp_ren_viv_pago_ext
                     LET v_tot_int_gen_pgo_ext     = v_tot_int_gen_pgo_ext     +  arr_salida_aclara_cc[v_indice].int_gen_pgo_ext
                     LET v_tot_aiv_ap_pat          = v_tot_aiv_ap_pat          +  arr_salida_aclara_cc[v_indice].aiv_ap_pat
                     LET v_tot_aiv_gen_pgo_ext     = v_tot_aiv_gen_pgo_ext     +  arr_salida_aclara_cc[v_indice].aiv_gen_pgo_ext

                    LET v_indice = v_indice + 1
            END FOREACH
            FREE cur_folio 

           --elinina ultimo renglon en blanco
           LET v_indice = v_indice - 1             
           CALL arr_salida_aclara_cc.deleteElement(arr_salida_aclara_cc.getLength())
           
           --Se visualizan los totales consultados y las opciones de generación de reporte y layout de incosnsistencias 
           --para Aclaratorio sin cambio 
           MENU "Inconsistencia"                    
              --se genera el reporte de inconsistencias  
                  
                  
              BEFORE MENU       
                 DISPLAY v_tot_imp_ap_pat           TO tot_ap_pat  
                 DISPLAY v_tot_imp_am_cre           TO tot_am_cred
                 DISPLAY v_tot_imp_ren_viv_pgo_ext  TO tot_rend_pag_ext 
                 DISPLAY v_tot_int_gen_pgo_ext      TO tot_interes
                 DISPLAY v_tot_aiv_ap_pat           TO tot_aiv_ap_pat
                 DISPLAY v_tot_aiv_gen_pgo_ext      TO tot_int_gen_pgo_ext 
             
             --se invoca a función que genra el reporte  de inconsistencias aclaratorio sin cambio 
             ON ACTION reporte 
                CALL fn_reporte_acl_inconsistencias_sc( v_tot_imp_ap_pat          ,
                                                        v_tot_imp_am_cre          ,
                                                        v_tot_imp_ren_viv_pgo_ext ,
                                                        v_tot_int_gen_pgo_ext     ,
                                                        v_tot_aiv_ap_pat          ,
                                                        v_tot_aiv_gen_pgo_ext     ,
                                                        v_folio
                                                       )
                 --Se genera el archivo (layout )de inconsistencias aclaratorio sin cambio 
                 ON ACTION genera_archivo
                    CALL fn_genera_archivo_txt_acl_rep_inconsistencias_sc(  v_indice                    ,
                                                                            v_tot_imp_ap_pat            ,
                                                                            v_tot_imp_am_cre            ,
                                                                            v_tot_imp_ren_viv_pgo_ext   ,
                                                                            v_tot_int_gen_pgo_ext       ,
                                                                            v_tot_aiv_ap_pat            ,
                                                                            v_tot_aiv_gen_pgo_ext    
                                                                          )

              ON ACTION CANCEL 
                 EXIT MENU
              
           END MENU          
          
         ELSE
           CALL fn_mensaje("Consulta","No existen registros con los criterios dados.","about")   
         END IF --IF v_registros > 0 THEN
      END IF  --IF NOT INT_FLAG THEN
   CLOSE WINDOW w_acl_rep_inconsistencias
END FUNCTION
{
======================================================================
Clave: ACLC16
Nombre: fn_genera_archivo_txt_acl_rep_inconsistencias_sc
Fecha creacion: Julio 4 de 2012
Autor: Luis Erick Rodríguez Vázquez
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
#OBJETIVO: Genera el reporte de aclaracion_pendiente
FUNCTION fn_genera_archivo_txt_acl_rep_inconsistencias_sc( v_indice                   ,
                                                           v_tot_imp_ap_pat           ,
                                                           v_tot_imp_am_cre           ,
                                                           v_tot_imp_ren_viv_pgo_ext  ,
                                                           v_tot_int_gen_pgo_ext      ,
                                                           v_tot_aiv_ap_pat           ,        
                                                           v_tot_aiv_gen_pgo_ext     
                                                          )
DEFINE v_indice                       INTEGER,
       v_tot_imp_ap_pat               DECIMAL(12,2),
       v_tot_imp_am_cre               DECIMAL(12,2),
       v_tot_imp_ren_viv_pgo_ext      DECIMAL(12,2), 
       v_tot_int_gen_pgo_ext          DECIMAL(12,2),
       v_tot_aiv_ap_pat               DECIMAL(18,6),  
       v_tot_aiv_gen_pgo_ext          DECIMAL(18,6),
       v_s_encabezado                 STRING,
       v_s_detalle                    STRING, 
       v_s_sumario                    STRING,
       v_canal_archivo                BASE.CHANNEL,
       v_query                        STRING,       
       v_total_imp_am_cred_info       DECIMAL(12,2),    
       tpo_registro                   CHAR(2),
       id_operación                   CHAR(3),        
       v_i_indice_for                 SMALLINT

       #layout documento de salida detalle 
      ,v_c_d_tpo_reg                     CHAR(1)-- 1             
      ,v_c_d_cve_ent_rec                 CHAR(3)-- 3             
      ,v_c_d_num_reg_pat                 CHAR(11)                
      ,v_filler                          CHAR(12) --filler 12    
      ,v_c_d_rfc_pat                     CHAR(13)                 
      ,v_c_d_per_pago                    CHAR(6)  --6             
      ,v_c_d_f_pgo_pat                   CHAR(8)  --8             
      ,v_c_d_num_folio_sua               CHAR(6)  --6             
      ,v_c_d_nss                         CHAR(11)                 
      ,v_c_d_rfc_trab                    CHAR(13)                 
      ,v_c_d_curp_trab                   CHAR(18)                 
      ,v_c_d_num_cred_info               CHAR(10)  --10           
      ,v_c_d_f_ini_dec_info              LIKE cta_pag_complemento.f_ini_desc_crd_ifv                  
      ,v_c_d_num_mov_per                 CHAR(2)--2               
      ,v_c_d_nom_trab                    CHAR(50)                 
      ,v_c_d_ult_sal_dia_int_per         CHAR(7)  --5             
      ,v_c_d_tpo_trab                    CHAR(1)--1               
      ,v_c_d_jor_sem_redu                CHAR(1)--1               
      ,v_c_d_loc_trab                    CHAR(1)--1               
      ,v_c_d_dest_ap_viv                 CHAR(1)--1               
      ,v_c_d_dias_cot_bim                CHAR(2)--2               
      ,v_c_d_dias_inca_bim               CHAR(2)--2               
      ,v_c_d_dias_ausent_bim             CHAR(2)--2               
      ,v_filler_1                        CHAR(7)  --filler  7     
      ,v_c_d_imp_ap_pat_info             CHAR(7)  --5,2            "\n     ,a.imp_ap_pat          ", 
      ,v_filler_2                        CHAR(7) --filler  7       
      ,v_c_d_imp_am_cred_info            CHAR(7)  --5,2            "\n     ,a.imp_am_cre          ", 
      ,v_c_d_imp_ren_sub_viv             CHAR(7)  --5,2            "\n     ,a.imp_ren_viv_pgo_ext ", 
      ,v_c_d_marca_cred_sua              CHAR(2)--2                
      ,v_c_d_marca_cred_bonsar           CHAR(1)--1                
      ,v_c_d_diag_aclara                 CHAR(2)--2                
      ,v_c_d_f_proceso                   CHAR(8)                   
      ,v_c_d_apl_int_viv                 CHAR(15)  --7             "\n     ,a.aiv_ap_pat          ", 
      ,v_c_d_prec_apl_int_viv            CHAR(11)  --3             "\n     ,a.valor_aiv           ", 
      ,v_c_d_int_gen_pgo_ext_viv         CHAR(7)  --5              "\n     ,a.int_gen_pgo_ext     ", 
      ,v_c_d_num_apl_int_gen_pgo_ext_viv CHAR(13)  --7             "\n     ,a.aiv_gen_pgo_ext     ",
      ,v_c_d_estado_pago_desc            CHAR(60) 
      ,v_filler_3                        CHAR(17) 
      -- variables para el sumario
      ,v_c_s_tpo_reg                     CHAR(1)  --1
      ,v_c_s_total_ap                    CHAR(9)  --9
      ,v_c_s_sum_ap_pat                  CHAR(13) --11,02
      ,v_c_s_sum_am                      CHAR(13) --11,02
      ,v_c_s_sum_apl_int_viv             CHAR(18) --12,06
      ,v_c_s_sum_int_viv_pag_ext         CHAR(13) --11,02
      ,v_c_s_sum_apl_int_viv_pag_ext     CHAR(18) --12,06
      ,v_c_s_filler                      CHAR(214) --215
       
   LET v_i_indice_for = 1 
   --Se obtiene la ruta de envio de los archivos
   LET v_query = "\n SELECT ruta_envio         "
                ,"\n FROM   seg_modulo         "
                ,"\n WHERE  modulo_cod = 'acl' "
                
   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO g_ruta_envio
   

   
   LET v_total_imp_am_cred_info = 0

   LET g_nom_archivo = TIME
   LET g_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
                      , g_nom_archivo[4,5], g_nom_archivo[7,8],".INCSCNSS"

   -- se crea el manejador de archivo
   LET v_canal_archivo = BASE.CHANNEL.CREATE()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_canal_archivo.openFile(g_ruta_envio CLIPPED||"/"||g_nom_archivo, "w" )
   CALL v_canal_archivo.setDelimiter("")

   -- se preparan los valores para los registros de detalle
   LET v_c_d_tpo_reg     = "2"  
   LET v_c_d_cve_ent_rec = "002"   
   
   -- se asignan los espacios a los filler
   LET v_filler     = 12 SPACES
   LET v_filler_1   = 7 SPACES
   LET v_filler_2   = 7 SPACES
   LET v_filler_3   = 17 SPACES
   LET v_c_s_filler = 214 SPACES
   
   FOR v_i_indice_for = 1 TO v_indice                                                                                                        
   
      LET v_s_detalle = v_c_d_tpo_reg                                                             ,                                             
                        arr_salida_aclara_cc[v_i_indice_for].cve_ent_receptora                    ,
                        arr_salida_aclara_cc[v_i_indice_for].num_reg_pat                          ,                               
                        v_filler                                                                  ,
                        arr_salida_aclara_cc[v_i_indice_for].rfc_pat                              ,                               
                        arr_salida_aclara_cc[v_i_indice_for].per_pago                             ,                               
                        arr_salida_aclara_cc[v_i_indice_for].f_pgo_pat                            USING "yyyymmdd",               
                        arr_salida_aclara_cc[v_i_indice_for].num_folio_sua                        USING "&&&&&&",                 
                        arr_salida_aclara_cc[v_i_indice_for].nss                                  ,                               
                        arr_salida_aclara_cc[v_i_indice_for].rfc_trab                             ,                               
                        arr_salida_aclara_cc[v_i_indice_for].curp_trab                            ,                               
                        arr_salida_aclara_cc[v_i_indice_for].num_cred_info                        USING "&&&&&&&&&&",             
                        arr_salida_aclara_cc[v_i_indice_for].f_ini_dec_info                       USING "yyyymmdd",               
                        arr_salida_aclara_cc[v_i_indice_for].num_mov_per                          USING "&&",                     
                        arr_salida_aclara_cc[v_i_indice_for].nom_trab                             ,                               
                        (arr_salida_aclara_cc[v_i_indice_for].ult_sal_dia_int_per * 100)          USING "&&&&&&&",                
                        arr_salida_aclara_cc[v_i_indice_for].tpo_trab                             USING "&",                      
                        arr_salida_aclara_cc[v_i_indice_for].jor_sem_redu                         USING "&",                      
                        arr_salida_aclara_cc[v_i_indice_for].loc_trab                             USING "&",                      
                        arr_salida_aclara_cc[v_i_indice_for].dest_ap_viv                          USING "&",                      
                        arr_salida_aclara_cc[v_i_indice_for].dias_cot_bim                         USING "&&",                     
                        arr_salida_aclara_cc[v_i_indice_for].dias_inca_bim                        USING "&&",                     
                        arr_salida_aclara_cc[v_i_indice_for].dias_ausent_bim                      USING "&&",                     
                        v_filler_1                                                                ,                               
                        (arr_salida_aclara_cc[v_i_indice_for].imp_ap_pat * 100)                   USING "&&&&&&&" ,               
                        v_filler_2                                                                ,               
                        (arr_salida_aclara_cc[v_i_indice_for].imp_am_cred * 100)                  USING "&&&&&&&" ,               
                        (arr_salida_aclara_cc[v_i_indice_for].imp_ren_viv_pago_ext * 100)         USING "&&&&&&&" ,               
                        arr_salida_aclara_cc[v_i_indice_for].marca_cred_sua                       USING "&&"    ,                 
                        arr_salida_aclara_cc[v_i_indice_for].marca_cred_bonsar                    USING "&"     ,                 
                        arr_salida_aclara_cc[v_i_indice_for].diag_aclara                          USING "&&"    ,                 
                        arr_salida_aclara_cc[v_i_indice_for].f_proceso                            USING "yyyymmdd"  ,             
                        (arr_salida_aclara_cc[v_i_indice_for].aiv_ap_pat  * 1000000 )             USING "&&&&&&&&&&&&&&&" ,       
                        (arr_salida_aclara_cc[v_i_indice_for].valor_aiv * 1000000)                USING "&&&&&&&&&&&"  ,          
                        (arr_salida_aclara_cc[v_i_indice_for].int_gen_pgo_ext  * 100)             USING "&&&&&&&"  ,              
                        (arr_salida_aclara_cc[v_i_indice_for].aiv_gen_pgo_ext  * 1000000)         USING "&&&&&&&&&&&&&" ,         
                        arr_salida_aclara_cc[v_i_indice_for].estado_pago_desc
--                        v_filler_3   ---- se comenta por requerimiento 241

      -- se escribe en el archivo los datos obtenidos
      CALL v_canal_archivo.WRITE([v_s_detalle])

   END FOR

   -- Se obtienen los registros del sumario
   LET v_c_s_tpo_reg                  = "9"
   LET v_c_s_total_ap                 = v_indice
   LET v_c_s_sum_ap_pat               = (v_tot_imp_ap_pat * 100)
   LET v_c_s_sum_am                   = (v_tot_imp_am_cre  * 100)
   LET v_c_s_sum_apl_int_viv          = (v_tot_aiv_ap_pat * 1000000)  
   LET v_c_s_sum_int_viv_pag_ext      = (v_tot_int_gen_pgo_ext * 100)
   LET v_c_s_sum_apl_int_viv_pag_ext  = (v_tot_aiv_gen_pgo_ext * 1000000)
   LET v_c_s_filler = NULL


   -- Se asigna la cadena de escritur de sumario en archivo
   LET v_s_sumario = v_c_s_tpo_reg     
                     ,v_c_s_total_ap                USING "&&&&&&&&&"   
                     ,v_c_s_sum_ap_pat              USING "&&&&&&&&&&&&&"
                     ,v_c_s_sum_am                  USING "&&&&&&&&&&&&&"  
                     ,v_c_s_sum_apl_int_viv         USING "&&&&&&&&&&&&&&&&&&"   
                     ,v_c_s_sum_int_viv_pag_ext     USING "&&&&&&&&&&&&&"
                     ,v_c_s_sum_apl_int_viv_pag_ext USING "&&&&&&&&&&&&&&&&&&" 
                     ,v_c_s_filler

   -- Se escribe en el archivo los datos obtenidos del sumario
   CALL v_canal_archivo.WRITE([v_s_sumario])
   
   -- Se cierra el archivo
   CALL v_canal_archivo.CLOSE()
   
   -- se envia mensaje de creación de archivo
   CALL fn_mensaje ("Consulta","Se genero el archivo de Inconsistencias en la ruta de envio.","about")

END FUNCTION

{
======================================================================
Clave: ACLC16
Nombre: fn_reporte_acl_inconsistencias_sc
Fecha creacion: Julio 27 de 2012
Autor: Luis Erick Rodríguez Vázquez
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
-- OBJETIVO: Obtener los datos necesarios para emitir el reporte Inconsistencias
FUNCTION fn_reporte_acl_inconsistencias_sc( v_tot_imp_ap_pat          ,
                                            v_tot_imp_am_cre          ,
                                            v_tot_imp_ren_viv_pgo_ext ,
                                            v_tot_int_gen_pgo_ext     ,
                                            v_tot_aiv_ap_pat          ,
                                            v_tot_aiv_gen_pgo_ext     ,
                                            v_folio                                                                  
                                           )
 DEFINE v_tot_imp_ap_pat             DECIMAL(12,2),
        v_tot_imp_am_cre             DECIMAL(12,2),
        v_tot_imp_ren_viv_pgo_ext    DECIMAL(12,2),
        v_tot_int_gen_pgo_ext        DECIMAL(12,2),
        v_tot_aiv_ap_pat             DECIMAL(18,6),
        v_tot_aiv_gen_pgo_ext        DECIMAL(12,2),
        v_folio                      DECIMAL(9,0)
 DEFINE v_d_indice               INTEGER,
        v_d_indice_for           INTEGER, 
        p_b_despliegue_pantalla  SMALLINT, -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
        v_desc_origen_pago       VARCHAR(60),  --descripción del origen de pago
        manejador_rpt               om.SaxDocumentHandler,    
        v_ruta_reporte              STRING, -- ruta del archivo del reporte
        v_ruta_listados             STRING, -- ruta de los listados
        v_ruta_ejecutable           STRING -- ruta del ejecutable

      LET v_desc_origen_pago = NULL
      LET v_d_indice_for = 1

    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
    DISPLAY v_ruta_ejecutable, v_ruta_listados
    --r_ruta_bin, r_ruta_listados
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("rpt_aclsc_inconsistenca.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","rpt_aclsc_inconsistenca"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
    --Inicia el reporte de registros con rechazo
    START REPORT rpt_aclsc_inconsistenca TO XML HANDLER manejador_rpt    
     
       OUTPUT TO REPORT rpt_aclsc_inconsistenca( v_tot_imp_ap_pat           ,
                                                 v_tot_imp_am_cre           ,
                                                 v_tot_imp_ren_viv_pgo_ext  ,
                                                 v_tot_int_gen_pgo_ext      ,
                                                 v_tot_aiv_ap_pat           ,
                                                 v_tot_aiv_gen_pgo_ext      ,
                                                 v_folio
                                                )
    FINISH REPORT rpt_aclsc_inconsistenca 
END FUNCTION
{
======================================================================
Clave: ACLC16
Nombre: rpt_aclaracion_inconsistencia
Fecha creacion: Julio 27 de 2012
Autor: Luis Erick Rodríguez Vázquez
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
#OBJETIVO: Genera el reporte de aclaracion_pendiente
REPORT rpt_aclsc_inconsistenca( v_tot_imp_ap_pat           ,
                                v_tot_imp_am_cre           ,
                                v_tot_imp_ren_viv_pgo_ext  ,
                                v_tot_int_gen_pgo_ext      ,
                                v_tot_aiv_ap_pat           ,
                                v_tot_aiv_gen_pgo_ext      ,
                                v_folio
                               )
DEFINE v_tot_imp_ap_pat             DECIMAL(12,2),
       v_tot_imp_am_cre             DECIMAL(12,2),
       v_tot_imp_ren_viv_pgo_ext    DECIMAL(12,2),  
       v_tot_int_gen_pgo_ext        DECIMAL(12,2),  
       v_tot_aiv_ap_pat             DECIMAL(18,6),      
       v_tot_aiv_gen_pgo_ext        DECIMAL(12,2),
       v_fecha_reporte              DATE ,
       v_folio                      INTEGER
                                                                                                                                                                                          
FORMAT                                                                                        

   FIRST PAGE HEADER
   
      LET v_fecha_reporte = TODAY USING "dd-mm-yyyy" 
      PRINTX v_folio
      PRINTX v_fecha_reporte
      PRINTX v_tot_imp_ap_pat
      PRINTX v_tot_imp_am_cre
      PRINTX v_tot_imp_ren_viv_pgo_ext   
      PRINTX v_tot_int_gen_pgo_ext
      PRINTX v_tot_aiv_ap_pat
      PRINTX v_tot_aiv_gen_pgo_ext
                                                                                           
END REPORT          
