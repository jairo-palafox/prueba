--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:26/07/2012
--===============================================================

################################################################################
#Modulo       => ACL                                                           #
#Programa     => ACLC18                                                        #
#Objetivo     => Inconsistencias aclaratorio con cambio de NSS                 #
#Fecha inicio => 25 de Julio de 2012                                           #
#Autor        => Ivan Vega                                                     #
#Modifica     =>                                                               #
################################################################################
DATABASE safre_viv
GLOBALS 
DEFINE arr_salida_aclara_cc    DYNAMIC ARRAY OF RECORD
       num_reg_pat                 CHAR(11)      -- nrp                  char(11)                            
      ,rfc_pat                     CHAR(13)      -- rfc_patron           char(13)        
      ,per_pago                    CHAR(6)       -- periodo_pago         char(6)         
      ,f_pgo_pat                   DATE          -- f_pago               date            
      ,num_folio_sua               DECIMAL(6,0)  -- folio_sua            decimal(6,0)    
      ,nss                         CHAR(11)      -- nss                  char(11)        
      ,rfc_trab                    CHAR(13)      -- rfc                  char(13)        
      ,curp_trab                   CHAR(18)      -- curp                 char(18)        
      ,num_cred_info               DECIMAL(10,0) -- num_crd_ifv          decimal(10,0)   
      ,f_ini_desc_cred_info       DATE          -- f_ini_desc_crd_ifv   date            
      ,num_mov_per                 SMALLINT      -- num_mov_periodo      smallint        
      ,nom_trab                    CHAR(50)      -- nombre_imss          char(50)        
      ,ult_sal_dia_int_per         INTEGER       -- ult_sdi              decimal(7,2)    
      ,tpo_trab                    CHAR(1)       -- tpo_trabajador       char(1)         
      ,jor_sem_redu                SMALLINT      -- jornada              char(1)         
      ,loc_trab                    CHAR(1)       -- localiza_trabajad+   char(1)         
      ,dest_ap_viv                 SMALLINT      -- destino_ap_viv       char(1)         
      ,dias_cot_bim                SMALLINT      -- dias_cot_bim         smallint        
      ,dias_inca_bim               SMALLINT      -- dias_incap_bim       smallint        
      ,dias_ausent_bim             SMALLINT      -- dias_ausent_bim      smallint        
      ,imp_ap_pat                  DECIMAL(12,2) -- imp_ap_pat           decimal(12,2)   
      ,imp_am_cred                 DECIMAL(12,2) -- imp_am_cre           decimal(12,2)   
      ,imp_ren_viv_pgo_ext         DECIMAL(12,2) -- imp_ren_viv_pgo_e+   decimal(12,2)   
      ,marca_cred_sua              CHAR(2)       -- marca_sua            char(2)         
      ,marca_cred_bonsar           CHAR(1)       -- marca_bdnsar         char(1)         
      ,diag_aclara                 CHAR(2)       -- tpo_aclaracion       char(2)         
      ,f_proceso                   DATE          -- f_proceso            date            
      ,id_derhab_nuevo             DECIMAL(9,0)  -- nss_disp             char(11)
      ,ape_pat_afore               CHAR(40)      -- nombre_af            char(40)        
      ,ape_mat_afore               CHAR(40)      -- ap_paterno_af        char(40)        
      ,nom_afore                   CHAR(40)      -- ap_materno_af        char(40)        
      ,aiv_ap_pat                  DECIMAL(18,6) -- aiv_ap_pat           decimal(18,6)   
      ,valor_aiv                   DECIMAL(18,6) -- valor_aiv            decimal(18,6)   
      ,int_gen_pgo_ext             DECIMAL(12,2) -- int_gen_pgo_ext      decimal(12,2)   
      ,aiv_gen_pgo_ext             DECIMAL(18,6) -- aiv_gen_pgo_ext      decimal(18,6)    
      ,id_referencia               DECIMAL(9,0)  -- id_referencia        decimal(9,0)
      ,estado_pago_desc            CHAR(60)
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
   LET g_origen_archivo = 6 -- con cambio de NSS
   
   -- consulta de informacion Reporte de Inconsistencias 
   CALL fn_opciones_reporte_inconsistencia(p_usuario_cod, p_s_titulo)

END MAIN

{ ======================================================================
Clave: ACLC18
Nombre: fn_opciones_reporte_inconsistencia
Fecha creacion: 25 de Julio de 2012  
Autor: Ivan Vega                      
Narrativa del proceso que realiza:
Presentan las inconsistencias encontradas en el proceso ACl con cambio de NSS
para generar el reporte y/o archivo de inconsistencias

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
       v_tot_imp_ap_pat             DECIMAL(12,2),
       v_tot_imp_am_cre             DECIMAL(12,2),
       v_tot_imp_ren_viv_pgo_ext    DECIMAL(12,2),
       v_tot_aiv_ap_pat             DECIMAL(18,6),      
       v_tot_int_gen_pgo_ext        DECIMAL(12,2),
       v_tot_aiv_gen_pgo_ext        DECIMAL(18,6)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --Se cierra la ventana por defecto
   CLOSE WINDOW SCREEN 
    --se abre la ventana de  forma de captura de parametros de busqueda
   OPEN WINDOW w_acl_rep_inconsistencias WITH FORM "ACLC171"

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
                            "\n     a.nrp                  ",
                            "\n     ,b.rfc_patron          ",
                            "\n     ,a.periodo_pago        ",
                            "\n     ,a.f_pago              ",
                            "\n     ,a.folio_sua           ",
                            "\n     ,c.nss                 ",
                            "\n     ,b.rfc                 ",
                            "\n     ,b.curp                ",
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
                            "\n     ,b.id_derhab_nuevo     ",
                            "\n     ,c.ap_paterno_af       ",
                            "\n     ,c.ap_materno_af       ",
                            "\n     ,c.nombre_af           ",
                            "\n     ,a.aiv_ap_pat          ",
                            "\n     ,a.valor_aiv           ",
                            "\n     ,a.int_gen_pgo_ext     ",
                            "\n     ,a.aiv_gen_pgo_ext     ", 
                            "\n     ,b.id_referencia       ",
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
            FOREACH cur_folio INTO  arr_salida_aclara_cc[v_indice].num_reg_pat            
                                   ,arr_salida_aclara_cc[v_indice].rfc_pat                
                                   ,arr_salida_aclara_cc[v_indice].per_pago               
                                   ,arr_salida_aclara_cc[v_indice].f_pgo_pat              
                                   ,arr_salida_aclara_cc[v_indice].num_folio_sua          
                                   ,arr_salida_aclara_cc[v_indice].nss                    
                                   ,arr_salida_aclara_cc[v_indice].rfc_trab               
                                   ,arr_salida_aclara_cc[v_indice].curp_trab              
                                   ,arr_salida_aclara_cc[v_indice].num_cred_info          
                                   ,arr_salida_aclara_cc[v_indice].f_ini_desc_cred_info  
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
                                   ,arr_salida_aclara_cc[v_indice].imp_ren_viv_pgo_ext        
                                   ,arr_salida_aclara_cc[v_indice].marca_cred_sua         
                                   ,arr_salida_aclara_cc[v_indice].marca_cred_bonsar      
                                   ,arr_salida_aclara_cc[v_indice].diag_aclara            
                                   ,arr_salida_aclara_cc[v_indice].f_proceso              
                                   ,arr_salida_aclara_cc[v_indice].id_derhab_nuevo
                                   ,arr_salida_aclara_cc[v_indice].ape_pat_afore          
                                   ,arr_salida_aclara_cc[v_indice].ape_mat_afore          
                                   ,arr_salida_aclara_cc[v_indice].nom_afore              
                                   ,arr_salida_aclara_cc[v_indice].aiv_ap_pat             
                                   ,arr_salida_aclara_cc[v_indice].valor_aiv              
                                   ,arr_salida_aclara_cc[v_indice].int_gen_pgo_ext        
                                   ,arr_salida_aclara_cc[v_indice].aiv_gen_pgo_ext        
                                   ,arr_salida_aclara_cc[v_indice].id_referencia
                                   ,arr_salida_aclara_cc[v_indice].estado_pago_desc          

                     --instruccion de GERrdo Vega  se asigna el mismo NSS
                     --LET arr_salida_aclara_cc[v_indice].nss_disp = arr_salida_aclara_cc[v_indice].nss
                     
                      --se genera la suma de los importes 
                     LET v_tot_imp_ap_pat          = v_tot_imp_ap_pat          +  arr_salida_aclara_cc[v_indice].imp_ap_pat
                     LET v_tot_imp_am_cre          = v_tot_imp_am_cre          +  arr_salida_aclara_cc[v_indice].imp_am_cred
                     LET v_tot_imp_ren_viv_pgo_ext = v_tot_imp_ren_viv_pgo_ext +  arr_salida_aclara_cc[v_indice].imp_ren_viv_pgo_ext
                     LET v_tot_int_gen_pgo_ext     = v_tot_int_gen_pgo_ext     +  arr_salida_aclara_cc[v_indice].int_gen_pgo_ext
                     LET v_tot_aiv_ap_pat          = v_tot_aiv_ap_pat          +  arr_salida_aclara_cc[v_indice].aiv_ap_pat
                     LET v_tot_aiv_gen_pgo_ext     = v_tot_aiv_gen_pgo_ext     +  arr_salida_aclara_cc[v_indice].aiv_gen_pgo_ext

                    LET v_indice = v_indice + 1
            END FOREACH
            FREE cur_folio 

           --elinina ultimo renglon en blanco
           LET v_indice = v_indice - 1             
           CALL arr_salida_aclara_cc.deleteElement(arr_salida_aclara_cc.getLength())
           
           MENU "Inconsistencia"                    
              --se genera el reporte de inconsistencias  
                  
                  
              BEFORE MENU       
                 DISPLAY v_tot_imp_ap_pat          TO tot_ap_pat
                 DISPLAY v_tot_imp_am_cre          TO tot_am_cred
                 DISPLAY v_tot_imp_ren_viv_pgo_ext TO tot_rend_pag_ext 
                 DISPLAY v_tot_int_gen_pgo_ext     TO tot_interes
                 DISPLAY v_tot_aiv_ap_pat          TO tot_aiv_ap_pat
                 DISPLAY v_tot_aiv_gen_pgo_ext     TO tot_int_gen_pgo_ext 
                 

             ON ACTION reporte 
                CALL fn_reporte_acl_inconsistencias( v_tot_imp_ap_pat          ,
                                                     v_tot_imp_am_cre          ,
                                                     v_tot_imp_ren_viv_pgo_ext ,
                                                     v_tot_int_gen_pgo_ext     ,
                                                     v_tot_aiv_ap_pat          ,
                                                     v_tot_aiv_gen_pgo_ext     ,
                                                     v_folio
                                                   )
                 --Se genera el archivo txt
                 ON ACTION genera_archivo
                    CALL fn_genera_archivo_txt_acl_rep_inconsistencias( v_tot_imp_ap_pat           ,
                                                                        v_tot_imp_am_cre           ,
                                                                        v_tot_imp_ren_viv_pgo_ext  ,
                                                                        v_tot_int_gen_pgo_ext      ,
                                                                        v_tot_aiv_ap_pat           ,
                                                                        v_tot_aiv_gen_pgo_ext      ,                                                                      
                                                                        v_indice
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
Clave: ACLC17
Nombre: fn_genera_archivo_txt_acl_rep_inconsistencias
Fecha creacion: Julio 4 de 2012
Autor: Luis Erick Rodríguez Vázquez
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
#OBJETIVO: Genera el reporte de aclaracion_pendiente
FUNCTION fn_genera_archivo_txt_acl_rep_inconsistencias ( v_tot_imp_ap_pat           ,
                                                         v_tot_imp_am_cre           ,
                                                         v_tot_imp_ren_viv_pgo_ext  ,
                                                         v_tot_int_gen_pgo_ext      ,
                                                         v_tot_aiv_ap_pat           ,
                                                         v_tot_aiv_gen_pgo_ext      ,
                                                         v_indice
                                                        )
--definición de variables                                                         
DEFINE v_tot_imp_ap_pat               DECIMAL(12,2),  
       v_tot_imp_am_cre               DECIMAL(12,2),  
       v_tot_imp_ren_viv_pgo_ext      DECIMAL(12,2),  
       v_tot_aiv_ap_pat               DECIMAL(18,6),  
       v_tot_int_gen_pgo_ext          DECIMAL(12,2),  
       v_tot_aiv_gen_pgo_ext          DECIMAL(18,6),
       v_indice                       INTEGER,
       v_s_encabezado                 STRING,
       v_s_detalle                    STRING, 
       v_s_sumario                    STRING,
       v_canal_archivo                BASE.CHANNEL,
       v_query                        STRING,       
       tpo_registro                   CHAR(2),
       id_operación                   CHAR(3), 
       f_creacion                     CHAR(8),
       v_i_indice_for                 SMALLINT
       --v_desc_origen_pago           VARCHAR(60)       --descripción del origen de pago

       #layout documento de salida detalle 
      ,v_c_d_tpo_reg                     CHAR(1)-- 1           --1 Tipo de registro N 01 00 01 - 01 Siempre 2                                                                                                                                                
      ,v_c_d_cve_ent_rec                 CHAR(3)-- 3           --2 Clave de entidad receptora N 03 00 02 - 04 002 INFONAVIT                                                                                                         
      ,v_c_d_num_reg_pat                 CHAR(11)              --3 Número de Registro Patronal IMSS A 11 00 05 - 15 Numero de Registro Patronal según SUA                                                                           
      ,v_filler                          CHAR(12) --filler 12  --4 Filler 12 00 16 - 27                                                                                                                                             
      ,v_c_d_rfc_pat                     CHAR(13)              --5 RFC del Patrón A 13 00 28 - 40 RFC patrón según SUA                                                                                                              
      ,v_c_d_per_pago                    CHAR(6)  --6          --6 Período de Pago N 06 00 41 - 46 Periodo de pago de la Aportación según SUA                                                                                       
      ,v_c_d_f_pgo_pat                   CHAR(8)  --8          --7 Fecha de Pago del Patrón N 08 00 47 - 54 Fecha de pago en la ER, según SUA                                                                                       
      ,v_c_d_num_folio_sua               CHAR(6)  --6          --8 Número de Folio SUA N 06 00 55 - 60 Según SUA                                                                                                                    
      ,v_c_d_nss                         CHAR(11)              --9 Número de Seguridad Social NSS A 11 00 61 - 71 NSS con el cual se recibió la aportación                                                                          
      ,v_c_d_rfc_trab                    CHAR(13)              --10 RFC del Trabajador A 13 00 72 - 84 RFC trabajador según SUA                                                                                                     
      ,v_c_d_curp_trab                   CHAR(18)              --11 CURP A 18 00 85 - 102 A 18 posiciones segúin SUA                                                                                                                
      ,v_c_d_num_cred_info               CHAR(10)  --10        --12 Número de Crédito INFONAVIT N 10 00 103 - 112 Crédito reportado por el Patrón Según SUA                                                                         
      ,v_c_d_f_ini_dec_info              CHAR(8)               --13 Fecha de inicio de Descuento de Crédito INFONAVIT A 08 00 113 - 120 Según SUA                                                                                   
      ,v_c_d_num_mov_per                 CHAR(2)--2            --14 Número de Movimientos en el Período N 02 00 121 - 122 Según SUA                                                                                                 
      ,v_c_d_nom_trab                    CHAR(50)              --15 Nombre del Trabajador A 50 00 123 - 172 Según SUA                                                                                                               
      ,v_c_d_ult_sal_dia_int_per         CHAR(7)  --5          --16 Último Salario Diario Integrado del Período N 05 02 173 - 179 Según SUA                                                                                         
      ,v_c_d_tpo_trab                    CHAR(1)--1            --17 Tipo de Trabajador N 01 00 180 - 180 Según SUA                                                                                                                  
      ,v_c_d_jor_sem_redu                CHAR(1)--1            --18 Jornada / Semana Reducida N 01 00 181 - 181 Según SUA                                                                                                           
      ,v_c_d_loc_trab                    CHAR(1)--1            --19 Localización del Trabajador N 01 00 182 - 182 "1" Con AFORE                                                                                                     
      ,v_c_d_dest_ap_viv                 CHAR(1)--1            --20 Destino Aportación Vivienda N 01 00 183 - 183 Según SUA                                                                                                         
      ,v_c_d_dias_cot_bim                CHAR(2)--2            --21 Días Cotizados en el Bimestre N 02 00 184 - 185 Según SUA                                                                                                       
      ,v_c_d_dias_inca_bim               CHAR(2)--2            --22 Días de Incapacidad en el Bimestre N 02 00 186 - 187                                                                                                            
      ,v_c_d_dias_ausent_bim             CHAR(2)--2            --23 Días de ausentismo en el Bimestre N 02 00 188 - 189 Según SUA                                                                                                   
      ,v_filler_1                        CHAR(7)  --filler  7  --24 Filler 07 190 - 196                                                                                                                                             
      ,v_c_d_imp_ap_pat                  CHAR(7)  --5,2        --25 Importe Aportación Patronal INFONAVIT N 05 02 197 - 203 Según SUA                                                                                               
      ,v_filler_2                        CHAR(7) --filler  7   --26 Filler 07 204 - 210                                                                                                                                             
      ,v_c_d_imp_am_cred                 CHAR(7)  --5,2        --27 Importe Amortización de Crédito INFONAVIT N 05 02 211 - 217 Según SUA                                                                                           
      ,v_c_d_imp_ren_sub_viv             CHAR(7)  --5,2        --28 Importe Rendimientos Subcuenta de Vivienda N 05 02 218 - 224 Según SUA                                                                                          
      ,v_c_d_marca_cred_sua              CHAR(2)--2            --29 Marca de Crédito según SUA N 02 00 225 - 226 Según SUA                                                                                                          
      ,v_c_d_marca_cred_bonsar           CHAR(1)--1            --30 Marca de Crédito según BDNSAR N 01 00 227 - 227                                                                                                                 
      ,v_c_d_diag_aclara                 CHAR(2)--2            --31 Diagnóstico de Aclaración N 02 00 228 - 229 Según BDNSAR                                                                                                        
      ,v_c_d_f_proceso                   CHAR(8)               --32 Fecha de Proceso A 08 00 230 - 237 Fecha del proceso de aclaraciones                                                                                            
      ,v_c_d_nss_disp                    CHAR(11)              --33 NSS dispersión A 11 00 238 - 248 NSS con el que se dispersó la aportación                                                                                       
      ,v_c_d_ape_pat_afore               CHAR(40)              --34 Apellido paterno según Afore A 40 00 249 - 288 Apellido paterno según Afore                                                                                     
      ,v_c_d_ape_mat_afore               CHAR(40)              --35 Apellido materno según Afore A 40 00 289 - 328 Apellido materno según Afore                                                                                     
      ,v_c_d_nom_afore                   CHAR(40)              --36 Nombre(s) según Afore A 40 00 329 - 368 Nombre(s) según Afore                                                                                                   
      ,v_c_d_apl_int_viv                 CHAR(13)  --7          --37 Aplicaciones de Intereses de Vivienda N 07 06 369 - 381 Número de aplicación de intereses de vivienda                                                           
      ,v_c_d_prec_apl_int_viv            CHAR(9)  --3          --38 Precio de la Aplicación de Intereses de Vivienda X 03 06 382 - 390                                                                                              
      ,v_c_d_int_gen_pgo_ext_viv         CHAR(7)  --5          --39 Intereses generados por Pagos Extemporáneos de vivienda N 5 2 391 - 397 Intereses por pagos extemporáneos de vivienda                                           
      ,v_c_d_num_apl_int_gen_pgo_ext_viv CHAR(13)  --7          --40 Numero de aplicaciones de Intereses generados por Pagos                                                                                                         
       #sumario                                                                                                                                                                   
      ,v_c_s_tpo_reg                     CHAR(1)  --1                                                                                                                                                                   
      ,v_c_s_total_ap                    CHAR(9)  --9
      ,v_c_s_sum_ap_pat                  CHAR(13) --11,02
      ,v_c_s_sum_am                      CHAR(13) --11,02
      ,v_c_s_sum_apl_int_viv             CHAR(18) --12,06
      ,v_c_s_sum_int_viv_pag_ext         CHAR(13) --11,02
      ,v_c_s_sum_apl_int_viv_pag_ext     CHAR(18) --12,06
      ,v_c_s_filler                      CHAR(324) --325

   LET v_i_indice_for = 1 
   --Se obtiene la ruta de envio de los archivos
   LET v_query = "\n SELECT ruta_envio         "
                ,"\n FROM   seg_modulo         "
                ,"\n WHERE  modulo_cod = 'acl' "
                
   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO g_ruta_envio

   LET g_nom_archivo = TIME
   LET g_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
                      , g_nom_archivo[4,5], g_nom_archivo[7,8],".INCCCNSS"

   -- se crea el manejador de archivo
   LET v_canal_archivo = BASE.CHANNEL.CREATE()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_canal_archivo.openFile(g_ruta_envio CLIPPED||"/"||g_nom_archivo, "w" ) 
   CALL v_canal_archivo.setDelimiter("")

   --Se obtienen los datos del detalle
   FOR v_i_indice_for = 1 TO v_indice

      --Se pasan los detalles a su equivalente en tipo char     
      LET v_c_d_tpo_reg              = "2"              
      LET v_c_d_cve_ent_rec          = "002"
      LET v_c_d_num_reg_pat          = arr_salida_aclara_cc[v_i_indice_for].num_reg_pat  
      LET v_filler                   = NULL      
      LET v_c_d_rfc_pat              = arr_salida_aclara_cc[v_i_indice_for].rfc_pat
      LET v_c_d_per_pago             = arr_salida_aclara_cc[v_i_indice_for].per_pago
      LET v_c_d_f_pgo_pat            = arr_salida_aclara_cc[v_i_indice_for].f_pgo_pat  
      LET v_c_d_num_folio_sua        = arr_salida_aclara_cc[v_i_indice_for].num_folio_sua
      LET v_c_d_nss                  = arr_salida_aclara_cc[v_i_indice_for].nss
      LET v_c_d_rfc_trab             = arr_salida_aclara_cc[v_i_indice_for].rfc_trab 
      LET v_c_d_curp_trab            = arr_salida_aclara_cc[v_i_indice_for].curp_trab
      LET v_c_d_num_cred_info        = arr_salida_aclara_cc[v_i_indice_for].num_cred_info
      LET v_c_d_f_ini_dec_info       = arr_salida_aclara_cc[v_i_indice_for].f_ini_desc_cred_info
      LET v_c_d_num_mov_per          = arr_salida_aclara_cc[v_i_indice_for].num_mov_per
      LET v_c_d_nom_trab             = arr_salida_aclara_cc[v_i_indice_for].nom_trab
      LET v_c_d_ult_sal_dia_int_per  = arr_salida_aclara_cc[v_i_indice_for].ult_sal_dia_int_per
      LET v_c_d_tpo_trab             = arr_salida_aclara_cc[v_i_indice_for].tpo_trab             
      LET v_c_d_jor_sem_redu         = arr_salida_aclara_cc[v_i_indice_for].jor_sem_redu 
      LET v_c_d_loc_trab             = arr_salida_aclara_cc[v_i_indice_for].loc_trab      
      LET v_c_d_dest_ap_viv          = arr_salida_aclara_cc[v_i_indice_for].dest_ap_viv  
      LET v_c_d_dias_cot_bim         = arr_salida_aclara_cc[v_i_indice_for].dias_cot_bim     
      LET v_c_d_dias_inca_bim        = arr_salida_aclara_cc[v_i_indice_for].dias_inca_bim 
      LET v_c_d_dias_ausent_bim      = arr_salida_aclara_cc[v_i_indice_for].dias_ausent_bim 
      LET v_filler_1                 = NULL
      LET v_c_d_imp_ap_pat           = (arr_salida_aclara_cc[v_i_indice_for].imp_ap_pat * 100)
      LET v_filler_2                 = NULL  
      LET v_c_d_imp_am_cred           = (arr_salida_aclara_cc[v_i_indice_for].imp_am_cred * 100)
      LET v_c_d_imp_ren_sub_viv             = (arr_salida_aclara_cc[v_i_indice_for].imp_ren_viv_pgo_ext * 100) 
      LET v_c_d_marca_cred_sua              = arr_salida_aclara_cc[v_i_indice_for].marca_cred_sua
      LET v_c_d_marca_cred_bonsar           = arr_salida_aclara_cc[v_i_indice_for].marca_cred_bonsar
      LET v_c_d_diag_aclara                 = arr_salida_aclara_cc[v_i_indice_for].diag_aclara
      LET v_c_d_f_proceso                   = arr_salida_aclara_cc[v_i_indice_for].f_proceso

      -- se obtiene el NSS de dispersion
      SELECT nss
      INTO v_c_d_nss_disp
      FROM afi_derechohabiente
      WHERE id_derechohabiente = arr_salida_aclara_cc[v_i_indice_for].id_derhab_nuevo

      -- si no se encontro, se ponen ceros
      IF ( v_c_d_nss_disp IS NULL ) THEN
         LET v_c_d_nss_disp = "00000000000"
      END IF
      
      LET v_c_d_ape_pat_afore               = arr_salida_aclara_cc[v_i_indice_for].ape_pat_afore
      LET v_c_d_ape_mat_afore               = arr_salida_aclara_cc[v_i_indice_for].ape_mat_afore 
      LET v_c_d_nom_afore                   = arr_salida_aclara_cc[v_i_indice_for].nom_afore
      LET v_c_d_apl_int_viv                 =( arr_salida_aclara_cc[v_i_indice_for].aiv_ap_pat  * 100 ) 
      LET v_c_d_prec_apl_int_viv            =( arr_salida_aclara_cc[v_i_indice_for].valor_aiv * 1000000)
      LET v_c_d_int_gen_pgo_ext_viv         =( arr_salida_aclara_cc[v_i_indice_for].int_gen_pgo_ext  * 100)
      LET v_c_d_num_apl_int_gen_pgo_ext_viv =( arr_salida_aclara_cc[v_i_indice_for].aiv_gen_pgo_ext  * 1000000)  

      --Se asigna todo lo obtenido a una sola cadena que sera escrita en el archivo
      LET v_s_detalle =  v_c_d_tpo_reg             , 
                         v_c_d_cve_ent_rec         , 
                         v_c_d_num_reg_pat         , 
                         v_filler                  , 
                         v_c_d_rfc_pat             , 
                         v_c_d_per_pago            , 
                         v_c_d_f_pgo_pat           , 
                         v_c_d_num_folio_sua       , 
                         v_c_d_nss                 , 
                         v_c_d_rfc_trab            , 
                         v_c_d_curp_trab           , 
                         v_c_d_num_cred_info       , 
                         v_c_d_f_ini_dec_info      , 
                         v_c_d_num_mov_per         , 
                         v_c_d_nom_trab            , 
                         v_c_d_ult_sal_dia_int_per , 
                         v_c_d_tpo_trab            , 
                         v_c_d_jor_sem_redu        , 
                         v_c_d_loc_trab            , 
                         v_c_d_dest_ap_viv         , 
                         v_c_d_dias_cot_bim        , 
                         v_c_d_dias_inca_bim       , 
                         v_c_d_dias_ausent_bim     , 
                         v_filler_1                , 
                         v_c_d_imp_ap_pat  USING "&&&&&&&" , 
                         v_filler_2                , 
                         v_c_d_imp_am_cred      USING "&&&&&&&" ,
                         v_c_d_imp_ren_sub_viv  USING "&&&&&&&" ,
                         v_c_d_marca_cred_sua      ,        
                         v_c_d_marca_cred_bonsar   ,        
                         v_c_d_diag_aclara         ,        
                         v_c_d_f_proceso           ,        
                         v_c_d_nss_disp            ,        
                         v_c_d_ape_pat_afore       ,        
                         v_c_d_ape_mat_afore       ,        
                         v_c_d_nom_afore           ,        
                         v_c_d_apl_int_viv      USING "&&&&&&&&&&&&&",        
                         v_c_d_prec_apl_int_viv  USING "&&&&&&&&&" ,       
                         v_c_d_int_gen_pgo_ext_viv USING "&&&&&&&" ,       
                         v_c_d_num_apl_int_gen_pgo_ext_viv USING "&&&&&&&&&&&&&",
                         arr_salida_aclara_cc[v_i_indice_for].estado_pago_desc   --ERV
            
             --Se escribe en el archivo los datos obtenidos
             CALL v_canal_archivo.WRITE([v_s_detalle])
   END FOR

   --Se obtienen los registros del sumario
   LET v_c_s_tpo_reg                  = "9"
   LET v_c_s_total_ap                 = v_indice   
   LET v_c_s_sum_ap_pat               = (v_tot_imp_ap_pat * 100)  
   LET v_c_s_sum_am                   = (v_tot_imp_am_cre * 100) 
   LET v_c_s_sum_apl_int_viv          = (v_tot_aiv_ap_pat * 1000000 ) 
   LET v_c_s_sum_int_viv_pag_ext      = (v_tot_int_gen_pgo_ext * 100 )  
   LET v_c_s_sum_apl_int_viv_pag_ext  = (v_tot_aiv_gen_pgo_ext * 1000000)  
   LET v_c_s_filler = NULL
                                                                                                               

   LET v_s_sumario = v_c_s_tpo_reg 
                     ,v_c_s_total_ap USING "&&&&&&&&&"
                     ,v_c_s_sum_ap_pat USING "&&&&&&&&&&&&&"
                     ,v_c_s_sum_am  USING "&&&&&&&&&&&&&&&"
                     ,v_c_s_sum_apl_int_viv   USING "&&&&&&&&&&&&&&&&&&"
                     ,v_c_s_sum_int_viv_pag_ext USING "&&&&&&&&&&&&&"
                     ,v_c_s_sum_apl_int_viv_pag_ext  USING "&&&&&&&&&&&&&&&&&&"
                     ,v_c_s_filler

 
   
   --Se escribe en el archivo los datos obtenidos del sumario
      CALL v_canal_archivo.WRITE([v_s_sumario])
   
   --Se cierra el archivo
   CALL v_canal_archivo.CLOSE()
   --se envia mensaje de creación de archivo
   CALL fn_mensaje ("Consulta","Se genero el archivo de Inconsistencias en la ruta de envio.","about")

END FUNCTION

{
======================================================================
Clave: ACLC17
Nombre: fn_reporte_acl_inconsistencias
Fecha creacion: Julio 3 de 2012
Autor: Luis Erick Rodríguez Vázquez
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
-- OBJETIVO: Obtener los datos necesarios para emitir el reporte Inconsistencias
FUNCTION fn_reporte_acl_inconsistencias( v_tot_imp_ap_pat          ,
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
        v_tot_aiv_gen_pgo_ext        DECIMAL(18,6),
        v_folio                      DECIMAL(9,0),
        v_d_indice                   INTEGER,
        v_d_indice_for               INTEGER, 
        p_b_despliegue_pantalla      SMALLINT, -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
        v_desc_origen_pago           VARCHAR(60),  --descripción del origen de pago
        manejador_rpt                om.SaxDocumentHandler,    
        v_ruta_reporte               STRING, -- ruta del archivo del reporte
        v_ruta_listados              STRING, -- ruta de los listados
        v_ruta_ejecutable            STRING -- ruta del ejecutable

    LET v_desc_origen_pago = NULL
    LET v_d_indice_for = 1

    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
    DISPLAY v_ruta_ejecutable, v_ruta_listados
    --r_ruta_bin, r_ruta_listados
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("ACLC18.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","aclaracion_pendiente"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
    --Inicia el reporte de registros con rechazo
    START REPORT rpt_aclaracion_inconsistencia TO XML HANDLER manejador_rpt    
     
       OUTPUT TO REPORT rpt_aclaracion_inconsistencia( v_tot_imp_ap_pat           ,
                                                       v_tot_imp_am_cre           ,
                                                       v_tot_imp_ren_viv_pgo_ext  ,
                                                       v_tot_int_gen_pgo_ext      ,
                                                       v_tot_aiv_ap_pat           ,
                                                       v_tot_aiv_gen_pgo_ext      ,
                                                       v_folio
                                                      )
    FINISH REPORT rpt_aclaracion_inconsistencia 
END FUNCTION
{
======================================================================
Clave: ACLC17
Nombre: rpt_aclaracion_inconsistencia
Fecha creacion: Julio 3 de 2012
Autor: Luis Erick Rodríguez Vázquez
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
#OBJETIVO: Genera el reporte de aclaracion_pendiente
REPORT rpt_aclaracion_inconsistencia( v_tot_imp_ap_pat           ,
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
       v_tot_aiv_gen_pgo_ext        DECIMAL(18,6),
       v_folio                      INTEGER,
       v_fecha_reporte              DATE
                                                                                                                                                                                          
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
