--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC05                                                                 #
#Objetivo     => Consulta de LQINFO                                                     #
#Autor        => Ivan Vega                                                              #
#Fecha inicio => Junio 20, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo     RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
        ruta_listados   CHAR(40)
       END RECORD,
       g_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_sql            STRING -- cadena con instruccion SQL
          
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion     SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo           STRING                       -- titulo de la ventana      
       ,v_folio              LIKE glo_folio.folio -- folio de consulta
       ,v_folio_temp         LIKE cta_his_pagos.folio
       
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "PAGC051"
   
   -- se inicia el folio en null
   LET v_folio = NULL
   
   INPUT v_folio WITHOUT DEFAULTS
     FROM folio
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_folio IS NOT NULL ) THEN

            LET v_sql = "SELECT FIRST 1 (a.folio)
                           FROM cta_his_pagos a
                          WHERE a.folio = ", v_folio,
                         "ORDER BY folio 
                          DESC"
            -- se prepara y ejecuta la consulta
            PREPARE consulta_existe_folio FROM v_sql
            DECLARE cur_consulta_folio CURSOR FOR consulta_existe_folio
   
            -- se transfieren los datos al arreglo de consulta
            FOREACH cur_consulta_folio INTO v_folio_temp
            END FOREACH 
   
            IF v_folio_temp IS NULL OR 0 THEN
              CALL fn_mensaje("Atención","El folio capturado no existe","stop")
            ELSE 
              CALL f_consulta_lqinfo(v_folio)
            END IF
            
         ELSE
         
            CALL fn_mensaje("Atención","Debe capturar un folio","stop")
            CONTINUE INPUT
         END IF
         
      
      ON ACTION cancel
         EXIT INPUT
   
   END INPUT   
   CLOSE WINDOW w_consulta

   
END MAIN

{ ======================================================================
Clave: PAGC05
Nombre: f_consulta_lqinfo
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Realiza una consulta sobre los datos de LQINFO y permite emitir el resultado de
la misma en un reporte

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_consulta_lqinfo(p_folio)
DEFINE    p_folio                 LIKE glo_folio.folio, -- folio de consulta
          v_r_despliegue          RECORD -- registro de consulta
             destino_ap_viv        VARCHAR(50)  ,
             ind_liquidacion       VARCHAR(50)  ,
             nrp                   CHAR(11)  ,
             tpo_aclaracion        VARCHAR(50)  ,
             num_registros         DECIMAL(9,0) ,
             imp_ap_pat            DECIMAL(22,2),
             aiv_ap_pat            DECIMAL(22,6),
             imp_am_cre            DECIMAL(22,2),
             int_gen_pgo_ext       DECIMAL(22,2),
             aiv_gen_pgo_ext       DECIMAL(22,6)
          END RECORD,             
          v_arr_despliegue        DYNAMIC ARRAY OF RECORD -- arreglo de despliegue
             destino_ap_viv        VARCHAR(50)  ,
             ind_liquidacion       VARCHAR(50)  ,
             nrp                   CHAR(11)  ,
             tpo_aclaracion        VARCHAR(50)  ,
             num_registros         DECIMAL(9,0) ,
             imp_ap_pat            DECIMAL(22,2),
             aiv_ap_pat            DECIMAL(22,6),
             imp_am_cre            DECIMAL(22,2),
             int_gen_pgo_ext       DECIMAL(22,2),
             aiv_gen_pgo_ext       DECIMAL(22,6)
          END RECORD,
          v_arr_reporte           DYNAMIC ARRAY OF RECORD -- arreglo de despliegue
             destino_ap_viv        VARCHAR(50)  ,
             ind_liquidacion       VARCHAR(50)  ,
             nrp                   CHAR(11)  ,
             tpo_aclaracion        VARCHAR(50)  ,
             num_registros         DECIMAL(9,0),
             imp_ap_pat            DECIMAL(22,2),
             aiv_ap_pat            DECIMAL(22,6),
             imp_am_cre            DECIMAL(22,2),
             int_gen_pgo_ext       DECIMAL(22,2),
             aiv_gen_pgo_ext       DECIMAL(22,6)
          END RECORD,
          -- para acumulados
          v_num_registros   DECIMAL(9,0),
          v_imp_ap_pat      DECIMAL(22,2),
          v_aiv_ap_pat      DECIMAL(22,6),
          v_imp_am_cre      DECIMAL(22,2),
          v_int_gen_pgo_ext DECIMAL(22,2),
          v_aiv_gen_pgo_ext DECIMAL(22,6),
          v_indice_reporte        INTEGER,
          v_contador              INTEGER,
          v_handler               om.SaxDocumentHandler -- handler para el reporte

DEFINE v_descripcion_acl VARCHAR (50)
DEFINE v_cont_nrp_riss   INTEGER
    
    
          
   -- ============================================================================
   --  SECCION DE DATOS DE DESTINO INFONAVIT
   -- ============================================================================
   -- GRUPO CAUSAL 13 Y 17
   LET v_sql = "\n select c.destino_ap_viv       dest,     ",
               "\n        d.ind_liquidacion      ind,      ",
               "\n        CASE                             ",
               "\n           WHEN d.nrp = 'B0799994105' OR ",
               "\n               d.nrp = (select n.nrp     ",
               "\n                        FROM   cat_riss_nrp n ",
               "\n                        where  n.nrp = d.nrp and n.id_nrp = 0) THEN 'VOL' ",
               "\n           ELSE 'APO'                    ",
               "\n        END nrp,                         ",
               "\n        d.tpo_aclaracion       acl,      ",
               "\n        count(*)               cua,      ",
               "\n        sum(d.imp_ap_pat)      viv,      ",
               "\n        sum(d.aiv_ap_pat)      aiv_ap,   ",
               "\n        sum(d.imp_am_cre)      am,       ",
               "\n        sum(d.int_gen_pgo_ext) int,      ",
               "\n        sum(d.aiv_gen_pgo_ext) aiv_int   ",
               "\n from   cta_his_pagos d,                 ",
               "\n        cta_pag_complemento c            ",
               "\n where  c.folio = d.folio                ",
               "\n and    d.id_referencia = c.id_referencia",
               "\n and    c.folio = ", p_folio, "          ", -- folio dado
               "\n and    d.ind_liquidacion in (0,1,2,3)   ",
               "\n and    d.tpo_aclaracion in (13, 17)     ", -- 13 y 17
               "\n and    c.destino_ap_viv = 1             ", -- Infonavit
               "\n group  by 1,2,3,4                       ",
               "\n order  by 1,2,3 DESC                    "

   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_lqinfoinf FROM v_sql
   DECLARE cur_consulta_lqinfoinf CURSOR FOR sid_consulta_lqinfoinf
   
   -- se inicia el contador
   LET v_contador = 1
   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_lqinfoinf INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].* = v_r_despliegue.*

      SELECT aclaracion_descripcion 
        INTO v_descripcion_acl
        FROM pag_tpo_aclaracion 
       WHERE aclaracion_cod = v_arr_despliegue[v_contador].tpo_aclaracion

      LET v_arr_despliegue[v_contador].tpo_aclaracion = v_arr_despliegue[v_contador].tpo_aclaracion CLIPPED || " - " || v_descripcion_acl CLIPPED 
           
      -- se cambian las descripciones
      --Valores destino_ap_vviv
      --1 = INFONAVIT
      --2 = AFORE
      DISPLAY "CAUSAL 13 Y 17: ", v_r_despliegue.destino_ap_viv
      DISPLAY "ind liquidacion: ", v_r_despliegue.ind_liquidacion
      CASE v_r_despliegue.destino_ap_viv
         WHEN 1
            LET v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT"
         WHEN 2
            LET v_arr_despliegue[v_contador].destino_ap_viv = "AFORE"
      END CASE
           
      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH
   
   -- GRUPO DISTINTO A CAUSAL 13 Y 17
   -- se ejecuta la consulta para resto de 13 y 17
   LET v_sql = "\n select c.destino_ap_viv       dest,     ",
               "\n        d.ind_liquidacion      ind,      ",
               "\n        CASE                             ",
               "\n           WHEN d.nrp = 'B0799994105' OR ",
               "\n               d.nrp = (select n.nrp     ",
               "\n                        FROM   cat_riss_nrp n ",
               "\n                        where  n.nrp = d.nrp and n.id_nrp = 0) THEN 'VOL' ",
               "\n           ELSE 'APO'                    ",
               "\n        END nrp,                         ",               
               "\n        'APORTACIONES'            ,      ",
               "\n        count(*)               cua,      ",
               "\n        sum(d.imp_ap_pat)      viv,      ",
               "\n        sum(d.aiv_ap_pat)      aiv_ap,   ",
               "\n        sum(d.imp_am_cre)      am,       ",
               "\n        sum(d.int_gen_pgo_ext) int,      ",
               "\n        sum(d.aiv_gen_pgo_ext) aiv_int   ",
               "\n from   cta_his_pagos d,                 ",
               "\n        cta_pag_complemento c            ",
               "\n where  c.folio = d.folio                ",
               "\n and    d.id_referencia = c.id_referencia",
               "\n and    c.folio = ", p_folio, "          ", -- folio dado
               "\n and    d.ind_liquidacion in (0,1,2,3)   ",
               "\n and    d.tpo_aclaracion NOT IN (13, 17) ", -- 13 y 17
               "\n and    c.destino_ap_viv = 1             ", -- INFONAVIT
               "\n group  by 1,2,3,4                       ",
               "\n order  by 1,2,3 DESC                    "

   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_lqinforestoinf FROM v_sql
   DECLARE cur_consulta_lqinforestoinf CURSOR FOR sid_consulta_lqinforestoinf
      
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_lqinforestoinf INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].* = v_r_despliegue.*
      
      -- se cambian las descripciones
      --Valores destino_ap_vviv
      --1 = INFONAVIT
      --2 = AFORE
      DISPLAY "no es CAUSAL 13 Y 17: ", v_r_despliegue.destino_ap_viv
      DISPLAY "ind liquidacion: ", v_r_despliegue.ind_liquidacion
      CASE v_r_despliegue.destino_ap_viv
         WHEN 1
            LET v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT"
         WHEN 2
            LET v_arr_despliegue[v_contador].destino_ap_viv = "AFORE"
      END CASE
      
      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH
   
   -- ============================================================================
   --  SECCION DE DATOS DE DESTINO AFORE
   -- ============================================================================
   -- GRUPO CAUSAL 13 Y 17
   LET v_sql = "\n select c.destino_ap_viv       dest,     ",
               "\n        d.ind_liquidacion      ind,      ",
               "\n        CASE                             ",
               "\n           WHEN d.nrp = 'B0799994105' OR ",
               "\n               d.nrp = (select n.nrp     ",
               "\n                        FROM   cat_riss_nrp n ",
               "\n                        where  n.nrp = d.nrp and n.id_nrp = 0) THEN 'VOL' ",
               "\n           ELSE 'APO'                    ",
               "\n        END nrp,                         ",
               "\n        d.tpo_aclaracion       acl,      ",
               "\n        count(*)               cua,      ",
               "\n        sum(d.imp_ap_pat)      viv,      ",
               "\n        sum(d.aiv_ap_pat)      aiv_ap,   ",
               "\n        sum(d.imp_am_cre)      am,       ",
               "\n        sum(d.int_gen_pgo_ext) int,      ",
               "\n        sum(d.aiv_gen_pgo_ext) aiv_int   ",
               "\n from   cta_his_pagos d,                 ",
               "\n        cta_pag_complemento c            ",
               "\n where  c.folio = d.folio                ",
               "\n and    d.id_referencia = c.id_referencia",
               "\n and    c.folio = ", p_folio, "          ", -- folio dado
               "\n and    d.ind_liquidacion in (0,1,2,3)   ",
               "\n and    d.tpo_aclaracion in (13, 17)     ", -- 13 y 17
               "\n and    c.destino_ap_viv = 2             ", -- AFORE
               "\n group  by 1,2,3,4                       ",
               "\n order  by 1,2,3 DESC                    "

   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_lqinfoaf FROM v_sql
   DECLARE cur_consulta_lqinfoaf CURSOR FOR sid_consulta_lqinfoaf
     
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_lqinfoaf INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].* = v_r_despliegue.*

      SELECT aclaracion_descripcion 
        INTO v_descripcion_acl
        FROM pag_tpo_aclaracion 
       WHERE aclaracion_cod = v_arr_despliegue[v_contador].tpo_aclaracion

      LET v_arr_despliegue[v_contador].tpo_aclaracion = v_arr_despliegue[v_contador].tpo_aclaracion CLIPPED || " - " || v_descripcion_acl CLIPPED 
           
      -- se cambian las descripciones
      --Valores destino_ap_vviv
      --1 = INFONAVIT
      --2 = AFORE
      DISPLAY "AFORE CAUSAL 13 Y 17: ", v_r_despliegue.destino_ap_viv
      DISPLAY "ind liquidacion: ", v_r_despliegue.ind_liquidacion
      CASE v_r_despliegue.destino_ap_viv
         WHEN 1
            LET v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT"
         WHEN 2
            LET v_arr_despliegue[v_contador].destino_ap_viv = "AFORE"
      END CASE

      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH
   
   -- GRUPO DISTINTO A CAUSAL 13 Y 17
   -- se ejecuta la consulta para resto de 13 y 17
   LET v_sql = "\n select c.destino_ap_viv       dest,     ",
               "\n        d.ind_liquidacion      ind,      ",
               "\n        CASE                             ",
               "\n           WHEN d.nrp = 'B0799994105' OR ",
               "\n               d.nrp = (select n.nrp     ",
               "\n                        FROM   cat_riss_nrp n ",
               "\n                        where  n.nrp = d.nrp and n.id_nrp = 0) THEN 'VOL' ",
               "\n           ELSE 'APO'                    ",
               "\n        END nrp,                         ",
               "\n        'APORTACIONES'            ,      ",
               "\n        count(*)               cua,      ",
               "\n        sum(d.imp_ap_pat)      viv,      ",
               "\n        sum(d.aiv_ap_pat)      aiv_ap,   ",
               "\n        sum(d.imp_am_cre)      am,       ",
               "\n        sum(d.int_gen_pgo_ext) int,      ",
               "\n        sum(d.aiv_gen_pgo_ext) aiv_int   ",
               "\n from   cta_his_pagos d,                 ",
               "\n        cta_pag_complemento c            ",
               "\n where  c.folio = d.folio                ",
               "\n and    d.id_referencia = c.id_referencia",
               "\n and    c.folio = ", p_folio, "          ", -- folio dado
               "\n and    d.ind_liquidacion in (0,1,2,3)   ",
               "\n and    d.tpo_aclaracion NOT IN (13, 17) ", -- 13 y 17
               "\n and    c.destino_ap_viv = 2             ", -- AFORE
               "\n group  by 1,2,3,4                       ",
               "\n order  by 1,2,3 DESC                    "

   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_lqinforestoaf FROM v_sql
   DECLARE cur_consulta_lqinforestoaf CURSOR FOR sid_consulta_lqinforestoaf
      
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_lqinforestoaf INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].* = v_r_despliegue.*
      
      -- se cambian las descripciones
      --Valores destino_ap_vviv
      --1 = INFONAVIT
      --2 = AFORE
      DISPLAY "Afore no es CAUSAL 13 Y 17: ", v_r_despliegue.destino_ap_viv
      DISPLAY "ind liquidacion: ", v_r_despliegue.ind_liquidacion
      CASE v_r_despliegue.destino_ap_viv
         WHEN 1
            LET v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT"
         WHEN 2
            LET v_arr_despliegue[v_contador].destino_ap_viv = "AFORE"
      END CASE
      
      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH
   -- =============================================================================
   
   
---- =====================================================================================================   
---   S E C C I Ó N   D E   D E S T I N O  A P O R T A C I Ó N   I G U A L = 3   P O R T A B I L I D A D
  
   -- GRUPO CAUSAL 13 Y 17
   LET v_sql = "\n select c.destino_ap_viv       dest,     ",
               "\n        d.ind_liquidacion      ind,      ",
               "\n        CASE                             ",
               "\n           WHEN d.nrp = 'B0799994105' OR ",
               "\n               d.nrp = (select n.nrp     ",
               "\n                        FROM   cat_riss_nrp n ",
               "\n                        where  n.nrp = d.nrp and n.id_nrp = 0) THEN 'VOL' ",
               "\n           ELSE 'APO'                    ",
               "\n        END nrp,                         ",
               "\n        d.tpo_aclaracion       acl,      ",
               "\n        count(*)               cua,      ",
               "\n        sum(d.imp_ap_pat)      viv,      ",
               "\n        sum(d.aiv_ap_pat)      aiv_ap,   ",
               "\n        sum(d.imp_am_cre)      am,       ",
               "\n        sum(d.int_gen_pgo_ext) int,      ",
               "\n        sum(d.aiv_gen_pgo_ext) aiv_int   ",
               "\n from   cta_his_pagos d,                 ",
               "\n        cta_pag_complemento c            ",
               "\n where  c.folio = d.folio                ",
               "\n and    d.id_referencia = c.id_referencia",
               "\n and    c.folio = ", p_folio, "          ", -- folio dado
               "\n and    d.ind_liquidacion in (0,1,2,3)   ",
               "\n and    d.tpo_aclaracion in (13, 17)     ", -- 13 y 17
               "\n and    c.destino_ap_viv = 3             ", -- Portabilidad
               "\n group  by 1,2,3,4                       ",
               "\n order  by 1,2,3 DESC                    "

   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_lqinfoPort FROM v_sql
   DECLARE cur_consulta_lqinfoPort CURSOR FOR sid_consulta_lqinfoPort
   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_lqinfoPort INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].* = v_r_despliegue.*

      SELECT aclaracion_descripcion 
        INTO v_descripcion_acl
        FROM pag_tpo_aclaracion 
       WHERE aclaracion_cod = v_arr_despliegue[v_contador].tpo_aclaracion

      LET v_arr_despliegue[v_contador].tpo_aclaracion = v_arr_despliegue[v_contador].tpo_aclaracion CLIPPED || " - " || v_descripcion_acl CLIPPED 
           
      -- se cambian las descripciones
      --Valores destino_ap_vviv
      --1 = INFONAVIT
      --2 = AFORE
      DISPLAY "CAUSAL 13 Y 17: ", v_r_despliegue.destino_ap_viv
      DISPLAY "ind liquidacion: ", v_r_despliegue.ind_liquidacion
      CASE v_r_despliegue.destino_ap_viv
         WHEN 1
            LET v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT"
         WHEN 2
            LET v_arr_despliegue[v_contador].destino_ap_viv = "AFORE"
         WHEN 3
            LET v_arr_despliegue[v_contador].destino_ap_viv = "PORTABILIDAD"            
      END CASE
           
      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH
   
   -- GRUPO DISTINTO A CAUSAL 13 Y 17
   -- se ejecuta la consulta para resto de 13 y 17
   LET v_sql = "\n select c.destino_ap_viv       dest,     ",
               "\n        d.ind_liquidacion      ind,      ",
               "\n        CASE                             ",
               "\n           WHEN d.nrp = 'B0799994105' OR ",
               "\n               d.nrp = (select n.nrp     ",
               "\n                        FROM   cat_riss_nrp n ",
               "\n                        where  n.nrp = d.nrp and n.id_nrp = 0) THEN 'VOL' ",
               "\n           ELSE 'APO'                    ",
               "\n        END nrp,                         ",               
               "\n        'APORTACIONES'            ,      ",
               "\n        count(*)               cua,      ",
               "\n        sum(d.imp_ap_pat)      viv,      ",
               "\n        sum(d.aiv_ap_pat)      aiv_ap,   ",
               "\n        sum(d.imp_am_cre)      am,       ",
               "\n        sum(d.int_gen_pgo_ext) int,      ",
               "\n        sum(d.aiv_gen_pgo_ext) aiv_int   ",
               "\n from   cta_his_pagos d,                 ",
               "\n        cta_pag_complemento c            ",
               "\n where  c.folio = d.folio                ",
               "\n and    d.id_referencia = c.id_referencia",
               "\n and    c.folio = ", p_folio, "          ", -- folio dado
               "\n and    d.ind_liquidacion in (0,1,2,3)   ",
               "\n and    d.tpo_aclaracion NOT IN (13, 17) ", -- 13 y 17
               "\n and    c.destino_ap_viv = 3             ", -- PORTABILIDAD
               "\n group  by 1,2,3,4                       ",
               "\n order  by 1,2,3 DESC                    "

   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_lqinforestoPort FROM v_sql
   DECLARE cur_consulta_lqinforestoPort CURSOR FOR sid_consulta_lqinforestoPort
      
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_lqinforestoPort INTO v_r_despliegue.*
      LET v_arr_despliegue[v_contador].* = v_r_despliegue.*
      
      -- se cambian las descripciones
      --Valores destino_ap_vviv
      --1 = INFONAVIT
      --2 = AFORE
      DISPLAY "no es CAUSAL 13 Y 17: ", v_r_despliegue.destino_ap_viv
      DISPLAY "ind liquidacion: ", v_r_despliegue.ind_liquidacion
      CASE v_r_despliegue.destino_ap_viv
         WHEN 1
            LET v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT"
         WHEN 2
            LET v_arr_despliegue[v_contador].destino_ap_viv = "AFORE"
         WHEN 3
            LET v_arr_despliegue[v_contador].destino_ap_viv = "PORTABILIDAD"
      END CASE
      
      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH   
   
----     F I N   D E  D E S T I N O   A P O R T A C I Ó N   P O R T A B I L I D A D
---- =================================================================================================   
   
         -- se indica que se usara la plantilla
         IF ( fgl_report_loadCurrentSettings("PAGI05.4rp") ) THEN
            LET v_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         ELSE
            CALL fn_mensaje("Error","No se encuentra la plantilla PAGC05.4rp. No se puede emitir el reporte","stop")
            RETURN
         END IF
      
         -- se inicia el indice del arreglo de reporte
         LET v_indice_reporte = 1

         -- ======================================================================
         -- INFONAVIT

         -- se transfieren los datos. APORTACION NORMAL
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 0 AND v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION NORMAL"
               END IF
               
               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR

         -- se transfieren los datos. CAUSAL 13 O 17
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 3 AND v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO CAUSAL 13 O 17 ADELANTADA RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO CAUSAL 13 O 17 ADELANTADA"                
               END IF               

               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR

         -- se transfieren los datos. INFONAVIT
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 2 AND v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*
               LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO SOLO INFONAVIT ADELANTADA"
               
               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR
         

         -- se transfieren los datos. SIN LIQUIDAR
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 1 AND v_arr_despliegue[v_contador].destino_ap_viv = "INFONAVIT" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO SIN LIQUIDAR RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACIÓN ACLARATORIO SIN LIQUIDAR"
               END IF

               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR


---- =====================================================================================================   
---   S E C C I Ó N   D E   D E S T I N O  A P O R T A C I Ó N   I G U A L = 3   P O R T A B I L I D A D
---   TRANSFERENCIA DE DATOS

         -- se transfieren los datos. APORTACION NORMAL
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 0 AND v_arr_despliegue[v_contador].destino_ap_viv = "PORTABILIDAD" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

--10-jul-2015               -- nvo port
--               LET v_arr_reporte[v_indice_reporte].aiv_ap_pat = v_arr_reporte[v_indice_reporte].imp_ap_pat
--               LET v_arr_reporte[v_indice_reporte].aiv_gen_pgo_ext = v_arr_reporte[v_indice_reporte].int_gen_pgo_ext
               
               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "PORTABILIDAD RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "PORTABILIDAD NORMAL"
               END IF
               
               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR

         -- se transfieren los datos. CAUSAL 13 O 17
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 3 AND v_arr_despliegue[v_contador].destino_ap_viv = "PORTABILIDAD" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

--10-jul-2015               -- nvo port
--               LET v_arr_reporte[v_indice_reporte].aiv_ap_pat = v_arr_reporte[v_indice_reporte].imp_ap_pat
--               LET v_arr_reporte[v_indice_reporte].aiv_gen_pgo_ext = v_arr_reporte[v_indice_reporte].int_gen_pgo_ext
               
               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "PORTABILIDAD ACLARATORIO CAUSAL 13 O 17 ADELANTADA RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "PORTABILIDAD ACLARATORIO CAUSAL 13 O 17 ADELANTADA"                
               END IF               

               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR

         -- se transfieren los datos. INFONAVIT
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 2 AND v_arr_despliegue[v_contador].destino_ap_viv = "PORTABILIDAD" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

               -- nvo port
               LET v_arr_reporte[v_indice_reporte].aiv_ap_pat = v_arr_reporte[v_indice_reporte].imp_ap_pat
               LET v_arr_reporte[v_indice_reporte].aiv_gen_pgo_ext = v_arr_reporte[v_indice_reporte].int_gen_pgo_ext
               
               LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "PORTABILIDAD ACLARATORIO SOLO INFONAVIT ADELANTADA"
               
               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR
         

         -- se transfieren los datos. SIN LIQUIDAR
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 1 AND v_arr_despliegue[v_contador].destino_ap_viv = "PORTABILIDAD" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

--10-jul-20115               -- nvo port
--               LET v_arr_reporte[v_indice_reporte].aiv_ap_pat = v_arr_reporte[v_indice_reporte].imp_ap_pat
--               LET v_arr_reporte[v_indice_reporte].aiv_gen_pgo_ext = v_arr_reporte[v_indice_reporte].int_gen_pgo_ext
               
               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "PORTABILIDAD ACLARATORIO SIN LIQUIDAR RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "PORTABILIDAD ACLARATORIO SIN LIQUIDAR"
               END IF

               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR


----     F I N   D E  D E S T I N O   A P O R T A C I Ó N   P O R T A B I L I D A D
---- =================================================================================================   



         -- ======================================================================
         -- AFORE

         -- se transfieren los datos. APORTACION NORMAL
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 0 AND v_arr_despliegue[v_contador].destino_ap_viv = "AFORE" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION NORMAL"
               END IF

               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR

         -- se transfieren los datos. CAUSAL 13 O 17
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 3 AND v_arr_despliegue[v_contador].destino_ap_viv = "AFORE" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO CAUSAL 13 O 17 ADELANTADA RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO CAUSAL 13 O 17 ADELANTADA"
               END IF
               
               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR

         -- se transfieren los datos. INFONAVIT
         -- ===============================================================
         -- VERSION AGRUPANDO TIPO DE ACLARACION
         -- ===============================================================
         -- se acumulan los montos
         LET v_num_registros   = 0 
         LET v_imp_ap_pat      = 0
         LET v_aiv_ap_pat      = 0
         LET v_imp_am_cre      = 0
         LET v_int_gen_pgo_ext = 0
         LET v_aiv_gen_pgo_ext = 0

         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 2 AND v_arr_despliegue[v_contador].destino_ap_viv = "AFORE" ) THEN               
               LET v_num_registros   = v_num_registros   + v_arr_despliegue[v_contador].num_registros  
               LET v_imp_ap_pat      = v_imp_ap_pat      + v_arr_despliegue[v_contador].imp_ap_pat     
               LET v_aiv_ap_pat      = v_aiv_ap_pat      + v_arr_despliegue[v_contador].aiv_ap_pat     
               LET v_imp_am_cre      = v_imp_am_cre      + v_arr_despliegue[v_contador].imp_am_cre     
               LET v_int_gen_pgo_ext = v_int_gen_pgo_ext + v_arr_despliegue[v_contador].int_gen_pgo_ext
               LET v_aiv_gen_pgo_ext = v_aiv_gen_pgo_ext + v_arr_despliegue[v_contador].aiv_gen_pgo_ext
               LET v_arr_reporte[v_indice_reporte].destino_ap_viv  = "AFORE"
               LET v_arr_reporte[v_indice_reporte].tpo_aclaracion  = NULL
               
               LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO SOLO INFONAVIT ADELANTADA"
               
            END IF            
            
         END FOR

         -- si se obtuvieron datos acumulados, se agregan al arreglo del reporte
         IF ( v_num_registros <> 0 ) THEN

            LET v_arr_reporte[v_indice_reporte].destino_ap_viv  = "AFORE"
            LET v_arr_reporte[v_indice_reporte].tpo_aclaracion  = NULL               
            LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO SOLO INFONAVIT ADELANTADA"
            LET v_arr_reporte[v_indice_reporte].num_registros   = v_num_registros  
            LET v_arr_reporte[v_indice_reporte].imp_ap_pat      = v_imp_ap_pat     
            LET v_arr_reporte[v_indice_reporte].aiv_ap_pat      = v_aiv_ap_pat     
            LET v_arr_reporte[v_indice_reporte].imp_am_cre      = v_imp_am_cre     
            LET v_arr_reporte[v_indice_reporte].int_gen_pgo_ext = v_int_gen_pgo_ext
            LET v_arr_reporte[v_indice_reporte].aiv_gen_pgo_ext = v_aiv_gen_pgo_ext

            -- se incrementa el indice del reporte
            LET v_indice_reporte = v_indice_reporte + 1
         END IF

         -- se transfieren los datos. SIN LIQUIDAR
         FOR v_contador = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_contador].ind_liquidacion = 1 AND v_arr_despliegue[v_contador].destino_ap_viv = "AFORE" ) THEN
               LET v_arr_reporte[v_indice_reporte].* = v_arr_despliegue[v_contador].*

               IF v_arr_despliegue[v_contador].nrp = "VOL" THEN
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACION ACLARATORIO SIN LIQUIDAR RISS VOLUNTARIO"
               ELSE
                  LET v_arr_reporte[v_indice_reporte].ind_liquidacion = "APORTACIÓN ACLARATORIO SIN LIQUIDAR"
               END IF

               -- se incrementa el indice del reporte
               LET v_indice_reporte = v_indice_reporte + 1
            END IF            
         END FOR



      
         -- se inicia la emision del reporte
         START REPORT rpt_consulta_lqinfo TO XML HANDLER v_handler
         
         FOR v_indice_reporte = 1 TO v_arr_reporte.getLength()
            OUTPUT TO REPORT rpt_consulta_lqinfo(p_folio, v_arr_reporte[v_indice_reporte].*)
         END FOR
        
         -- se finaliza el reporte
         FINISH REPORT rpt_consulta_lqinfo
   
   
END FUNCTION

{ ======================================================================
Clave: PAGC05
Nombre: rpt_consulta_lqinfo
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de lqinfo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_consulta_lqinfo(v_folio, v_r_despliegue)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
             destino_ap_viv        VARCHAR(50)  ,
             ind_liquidacion       VARCHAR(50)  ,
             nrp                   CHAR(11)  ,
             tpo_aclaracion        VARCHAR(50)  ,
             num_registros         DECIMAL(9,0) ,
             imp_ap_pat            DECIMAL(22,2),
             aiv_ap_pat            DECIMAL(22,6),
             imp_am_cre            DECIMAL(22,2),
             int_gen_pgo_ext       DECIMAL(22,2),
             aiv_gen_pgo_ext       DECIMAL(22,2)
          END RECORD,
          v_folio                 LIKE glo_folio.folio, -- folio
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular por destino
          v_destino_regs             DECIMAL(9,0)      ,
          v_destino_imp_ap_pat       DECIMAL(22,2),
          v_destino_aiv_ap_pat       DECIMAL(22,6),
          v_destino_imp_am_cre       DECIMAL(22,2),
          v_destino_int_gen_pgo_ext  DECIMAL(22,2),
          v_destino_aiv_gen_pgo_ext  DECIMAL(22,2),
          -- variables para acumular por ind liquidacion
          v_indliq_regs              DECIMAL(9,0)      ,
          v_indliq_imp_ap_pat        DECIMAL(22,2),
          v_indliq_aiv_ap_pat        DECIMAL(22,6),
          v_indliq_imp_am_cre        DECIMAL(22,2),
          v_indliq_int_gen_pgo_ext   DECIMAL(22,2),
          v_indliq_aiv_gen_pgo_ext   DECIMAL(22,2),
          -- variables para acumular por localiza trabajador
          v_localiza_regs            DECIMAL(9,0)      ,
          v_localiza_imp_ap_pat      DECIMAL(22,2),
          v_localiza_aiv_ap_pat      DECIMAL(22,6),
          v_localiza_imp_am_cre      DECIMAL(22,2),
          v_localiza_int_gen_pgo_ext DECIMAL(22,2),
          v_localiza_aiv_gen_pgo_ext DECIMAL(22,2),
          -- variables para el totalizador
          -- variables para acumular por localiza trabajador
          v_total_regs               DECIMAL(9,0)      ,
          v_total_imp_ap_pat         DECIMAL(22,2),
          v_total_aiv_ap_pat         DECIMAL(22,6),
          v_total_imp_am_cre         DECIMAL(22,2),
          v_total_int_gen_pgo_ext    DECIMAL(22,2),
          v_total_aiv_gen_pgo_ext    DECIMAL(22,2)
          
FORMAT

   FIRST PAGE HEADER
      -- variables para acumular por destino
      LET v_destino_imp_ap_pat       = 0
      LET v_destino_aiv_ap_pat       = 0
      LET v_destino_imp_am_cre       = 0
      LET v_destino_int_gen_pgo_ext  = 0
      LET v_destino_aiv_gen_pgo_ext  = 0

      -- variables para acumular por ind liquidacion
      LET v_indliq_imp_ap_pat        = 0
      LET v_indliq_aiv_ap_pat        = 0
      LET v_indliq_imp_am_cre        = 0
      LET v_indliq_int_gen_pgo_ext   = 0
      LET v_indliq_aiv_gen_pgo_ext   = 0

      -- variables para acumular por localiza trabajador
      LET v_localiza_imp_ap_pat      = 0
      LET v_localiza_aiv_ap_pat      = 0
      LET v_localiza_imp_am_cre      = 0
      LET v_localiza_int_gen_pgo_ext = 0
      LET v_localiza_aiv_gen_pgo_ext = 0
      
      -- variables para acumular gran total
      LET v_total_imp_ap_pat         = 0
      LET v_total_aiv_ap_pat         = 0
      LET v_total_imp_am_cre         = 0
      LET v_total_int_gen_pgo_ext    = 0
      LET v_total_aiv_gen_pgo_ext    = 0

      -- conteo de registros
      LET v_destino_regs             = 0
      LET v_indliq_regs              = 0
      LET v_localiza_regs            = 0
      LET v_total_regs               = 0

      
      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = g_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      PRINTX v_folio, g_usuario_cod, v_fecha, v_nombre_usuario
      
   BEFORE GROUP OF v_r_despliegue.destino_ap_viv
      PRINTX v_r_despliegue.destino_ap_viv
      DISPLAY "Grupo destino aportacion: ", v_r_despliegue.destino_ap_viv
      -- se inician los acumuladores
      LET v_destino_imp_ap_pat       = 0
      LET v_destino_aiv_ap_pat       = 0
      LET v_destino_imp_am_cre       = 0
      LET v_destino_int_gen_pgo_ext  = 0
      LET v_destino_aiv_gen_pgo_ext  = 0
      
      LET v_destino_regs             = 0


   AFTER GROUP OF v_r_despliegue.destino_ap_viv
      PRINTX v_destino_regs           ,
             v_destino_imp_ap_pat     ,
             v_destino_aiv_ap_pat     ,
             v_destino_imp_am_cre     ,
             v_destino_int_gen_pgo_ext,
             v_destino_aiv_gen_pgo_ext



   BEFORE GROUP OF v_r_despliegue.ind_liquidacion
      PRINTX v_r_despliegue.ind_liquidacion
DISPLAY "Grupo ind liquidacion: ", v_r_despliegue.ind_liquidacion
      -- se inician los acumuladores
      LET v_indliq_imp_ap_pat        = 0
      LET v_indliq_aiv_ap_pat        = 0
      LET v_indliq_imp_am_cre        = 0
      LET v_indliq_int_gen_pgo_ext   = 0
      LET v_indliq_aiv_gen_pgo_ext   = 0

      LET v_indliq_regs              = 0


   ON EVERY ROW
      PRINTX v_r_despliegue.destino_ap_viv,
             v_r_despliegue.ind_liquidacion,
             v_r_despliegue.tpo_aclaracion,
             v_r_despliegue.num_registros,
             v_r_despliegue.imp_ap_pat,
             v_r_despliegue.aiv_ap_pat,
             v_r_despliegue.imp_am_cre,
             v_r_despliegue.int_gen_pgo_ext,
             v_r_despliegue.aiv_gen_pgo_ext
      
      -- se acumulan los montos 
      LET v_localiza_imp_ap_pat      = v_localiza_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_localiza_aiv_ap_pat      = v_localiza_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_localiza_imp_am_cre      = v_localiza_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_localiza_int_gen_pgo_ext = v_localiza_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_localiza_aiv_gen_pgo_ext = v_localiza_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext

      -- se acumulan los montos de ind_liquidacion
      LET v_indliq_imp_ap_pat        = v_indliq_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_indliq_aiv_ap_pat        = v_indliq_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_indliq_imp_am_cre        = v_indliq_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_indliq_int_gen_pgo_ext   = v_indliq_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_indliq_aiv_gen_pgo_ext   = v_indliq_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext
      
      -- se acumulan los montos por destino
      LET v_destino_imp_ap_pat       = v_destino_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_destino_aiv_ap_pat       = v_destino_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_destino_imp_am_cre       = v_destino_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_destino_int_gen_pgo_ext  = v_destino_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_destino_aiv_gen_pgo_ext  = v_destino_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext

      -- se acumula el total
      LET v_total_imp_ap_pat         = v_total_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_total_aiv_ap_pat         = v_total_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_total_imp_am_cre         = v_total_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_total_int_gen_pgo_ext    = v_total_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_total_aiv_gen_pgo_ext    = v_total_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext
      
      LET v_destino_regs             = v_destino_regs   + v_r_despliegue.num_registros
      LET v_indliq_regs              = v_indliq_regs    + v_r_despliegue.num_registros
      LET v_localiza_regs            = v_localiza_regs  + v_r_despliegue.num_registros
      LET v_total_regs               = v_total_regs     + v_r_despliegue.num_registros
                                                          
   
   ON LAST ROW
      PRINTX v_total_regs           ,
             v_total_imp_ap_pat     ,
             v_total_aiv_ap_pat     ,
             v_total_imp_am_cre     ,
             v_total_int_gen_pgo_ext,
             v_total_aiv_gen_pgo_ext

             
END REPORT   
             
             