--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => ACL                                                     #
#Programa          => ACLS01                                                  #
#Objetivo          => Generar Archivo de salida de aclaratorio                #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   g_opera_cod                 LIKE cat_operacion.opera_cod, -- codigo de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod,  -- codigo de operacion
   g_ruta_envio                LIKE seg_modulo.ruta_envio,
   g_nom_archivo               STRING,
   p_num_folio           DECIMAL(9,0),
   p_origen_archivo  SMALLINT,
   p_id_referencia   DECIMAL(9,0),
   v_f_lote          DATE
DEFINE v_total_casos, v_total_casos_procesados DYNAMIC ARRAY OF RECORD
      estado  CHAR(1)
     ,casos   INTEGER
   END RECORD
DEFINE g_total_procesados INTEGER
DEFINE v_total_x_procesar INTEGER
END GLOBALS
######################################################################
#               Inicia el MAIN
######################################################################
MAIN
DEFINE 
   --p_num_folio       DECIMAL(9)
   p_nom_archivo     STRING
  ,v_f_lote          LIKE acl_ctr_lote.f_lote
  ,v_consecutivo     LIKE acl_ctr_lote.consecutivo
  ,v_estatus         SMALLINT
  ,v_indice          INTEGER
  ,v_hora            CHAR(8)
  ,v_query           STRING
  ,v_nss             LIKE afi_derechohabiente.nss
  ,v_nom_archivo_aux LIKE acl_ctr_lote.nombre_archivo
  ,p_lote_cod        SMALLINT
  
   #Si se ha recibido parámetros se continua
      #Primer parámetro
      LET p_usuario_cod = ARG_VAL(1)
      #Segundo parámetro
      LET g_pid         = ARG_VAL(2)
      #Tercer parámetro
      LET g_proceso_cod = ARG_VAL(3)
      #Cuarto parámetro
      LET g_opera_cod   = ARG_VAL(4)  --numero de proceso
      --Quinto parámetro
      LET p_num_folio   = ARG_VAL(5)
      --Octavo parámetro
      LET p_origen_archivo = ARG_VAL(6)
      --Noveno parámetro
      LET p_id_referencia  = ARG_VAL(7)
      --Decimo Parámetro(lote_cod: 1 = Seleccion y Preliquidación. 2 = Consulta
      LET p_lote_cod  = ARG_VAL(8) 
      
      LET v_f_lote = TODAY

      SELECT NVL(MAX(consecutivo),0) + 1
      INTO   v_consecutivo
      FROM   acl_ctr_lote
      WHERE  f_lote = v_f_lote
      
      LET v_hora = TIME (CURRENT HOUR TO SECOND)

      --Se obtiene la ruta de envio de los archivos
      LET v_query = "\n SELECT ruta_envio         "
                   ,"\n FROM   seg_modulo         "
                   ,"\n WHERE  modulo_cod = 'acl' "
      PREPARE prp_ruta_archivo FROM v_query
      EXECUTE prp_ruta_archivo INTO g_ruta_envio

      IF ( p_num_folio = 0 OR p_num_folio IS NULL ) THEN
         --Se obtiene el folio de la operacion anterior
        CALL fn_obten_folio(g_proceso_cod, g_opera_cod -1) RETURNING p_num_folio
        IF p_num_folio IS NULL THEN  LET p_num_folio = 0 END IF
      END IF
      
      --Se obtiene el numero de nss
      SELECT a.nss
      INTO   v_nss
      FROM   afi_Derechohabiente a, cta_his_pagos c
      WHERE  a.id_derechohabiente = c.id_derechohabiente
      AND    c.folio = p_num_folio
      --AND    c.origen_archivo = p_origen_archivo
      --AND    c.id_referencia = p_id_referencia

      --DISPLAY "    c.folio =",p_num_folio
      --DISPLAY "    c.origen_archivo =",p_origen_archivo
      --DISPLAY "    c.id_referencia =",p_id_referencia
      --DISPLAY "v_nss: ",v_nss

      -- se obtiene el id_referencia
      SELECT id_referencia
      INTO   p_id_referencia
      FROM   cta_his_pagos
      WHERE  folio = p_num_folio
      
      -- 12sep2012 El nombre del archivo de salida sera
      -- Salida_acla_infonavit_FAAMMDD_”foliosafre”.txt
      LET g_nom_archivo = "SALIDA_ACLA_INFONAVIT_", YEAR(TODAY) USING "&&&&", MONTH(TODAY) USING "&&",
                          DAY(TODAY) USING "&&", p_num_folio USING "&&&&&&&&&", ".txt"

      --DISPLAY " PID ASIGNADO :",g_pid
      CALL fn_display_proceso(0,"GENERAR ARCHIVO DE SALIDA DE ACLARATORIO")


      --Se marca el proceso como iniciado
      --CALL fn_actualiza_opera_ini (g_pid
                                  --,g_proceso_cod
                                  --,g_opera_cod
                                  --,p_num_folio
                                  --,"ACLS01"
                                  --,g_nom_archivo
                                  --,p_usuario_cod)
                                  --RETURNING v_estatus
                                  
      --Se genera el archivo
      CALL f_genera_archivo()
      
      LET v_nom_archivo_aux = g_nom_archivo
      --Se actualiza el nombre del archivo generado
     {
      INSERT INTO acl_ctr_lote
      VALUES (1, v_f_lote, v_consecutivo, p_num_folio, p_origen_archivo, p_id_referencia, v_nom_archivo_aux)
}
      UPDATE acl_ctr_lote
      SET    nombre_archivo = v_nom_archivo_aux
      WHERE  folio = p_num_folio
      --AND    id_referencia = p_id_referencia
      --AND    lote_cod = p_lote_cod

      CALL fn_actualiza_opera_fin (g_pid
                                  ,g_proceso_cod
                                  ,g_opera_cod)
                                  RETURNING v_estatus

      
      DISPLAY "TOTAL DE REGISTROS PROCESADOS:",g_total_procesados

      DISPLAY "ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",g_nom_archivo
      
      CALL fn_display_proceso(1,"GENERAR ARCHIVO DE SALIDA DE ACLARATORIO")
END MAIN

###############################################################################
#Modulo            => ACL                                                     #
#Programa          => ACLS01                                                  #
#Objetivo          => Genera el archivo                                       #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
#Modifica Rubén Haro Castro 22 de Junio de 2012                               #  
###############################################################################
FUNCTION f_genera_archivo()
   --Se definen las variables para el archivo fisico
   DEFINE v_canal_archivo             BASE.CHANNEL
         ,v_query                     STRING
         ,v_string_detalle            STRING
         ,v_string_encabezado         STRING
         ,v_string_sumario            STRING
         ,v_id_lote                   INTEGER
         ,v_lote_x_dia                SMALLINT
         ,v_total_imp_ap_pat          LIKE cta_his_pagos.imp_ap_pat
         ,v_total_imp_am_cre          LIKE cta_his_pagos.imp_am_cre
         ,v_total_imp_ren_viv_pgo_ext LIKE cta_his_pagos.imp_ren_viv_pgo_ext
         ,v_total_imp_aiv_ap_pat      LIKE cta_his_pagos.aiv_ap_pat
         ,v_total_imp_int_gen_pgo_ext LIKE cta_his_pagos.int_gen_pgo_ext
         ,v_total_imp_aiv_gen_pgo_ext LIKE cta_his_pagos.aiv_gen_pgo_ext
         ,v_id_derechohabiente_tmp    DECIMAL (9,0)

{
   --RECORD con el encabezado del archivo
   DEFINE v_enc RECORD
        tpo_registro   CHAR(2),  --Tipo Registro            X   2   0
        id_operación   CHAR(3),  --Identificador Operación  X   3   0
        f_creacion     CHAR(8),  --Fecha Creación Lote      X   8   0
        consecutivo    CHAR(3)   --Consecutivo de Lote      X   3   0
   END RECORD
}
   --RECORD con el detalle del archivo
   DEFINE v_det RECORD
     tpo_registro            CHAR(2)      , -- X   2   0   Tipo Registro
     consecutivo_reg_lote    DECIMAL(8,0) , -- N   8       Consecutivo Registro dentro del Lote
     cve_enti_receptora      CHAR(3)      , -- X   3   0   Clave de Entidad Receptora
     reg_patronal_imsss      CHAR(11)     , -- X   11  0   Registro Patronal IMSS
     rfc_patrón              CHAR(13)     , -- X   13  0   RFC del Patrón
     periodo_pago            DECIMAL(6,0) , -- N   6   0   Periodo de Pago
     folio_pag_sua           DECIMAL(6,0) , -- N   6   0   Folio de Pago SUA
     nss                     CHAR(11),     -- X   11  0   Número de Seguriedad Social (NSS)
     rfc_trabajador          CHAR(13),     -- X   13  0   RFC del Trabajador
     curp                    CHAR(18),     -- X   18  0   Clave Única de Registro de Población (CURP)
     num_crd_ifv             LIKE cta_his_pagos.num_crd_ifv,     --DECIMAL(10,0)-- N   10  0   Número de Crédito INFONAVIT
     f_ini_desc_crd_ifv      LIKE cta_pag_complemento.f_ini_desc_crd_ifv   ,--DECIMAL(8,0) -- N   8   0   Fecha de inicio del descuento de crédito INFONAVIT
     paterno_trabajador      CHAR(40),      -- X   40  0   Paterno Trabajador
     materno_rabajador       CHAR(40),     -- X   40  0   Materno Trabajador
     nombre_trabajador       CHAR(40),     -- X   40  0   Nombre Trabajador
     tipo_aclaración         CHAR(2) ,     -- X   2   0   Tipo de Aclaración
     ult_sdi                 LIKE cta_pag_complemento.ult_sdi,   --DECIMAL(7,2) -- N   5   2   Último Salario Diario
     dias_cot_bim            LIKE cta_pag_complemento.dias_cot_bim    ,  --DECIMAL(2,0) -- N   2   0   Días Cotizados en Bimestre
     dias_incap_bim          LIKE cta_pag_complemento.dias_incap_bim  ,--DECIMAL(2,0) -- N   2   0   Días Incapacidad en Bimestre
     dias_ausent_bim         LIKE cta_pag_complemento.dias_ausent_bim ,--DECIMAL(2,0) -- N   2   0   Días Ausentismo en Bimestre
     imp_ap_pat              LIKE cta_his_pagos.imp_ap_pat         ,    --DECIMAL(7,2) -- N   5   2   Importe Aportación Patronal
     imp_am_cre              LIKE cta_his_pagos.imp_am_cre         ,   --DECIMAL(7,2) -- N   5   2   Importe Amortización Crédito INFONAVIT
     imp_ren_viv_pgo_ext     LIKE cta_his_pagos.imp_ren_viv_pgo_ext,  --DECIMAL(7,2) -- N   5   2   Importe Rendimientos Subcuenta Vivienda
     aiv_ap_pat              LIKE cta_his_pagos.aiv_ap_pat         ,  --DECIMAL(15,6) -- N   9   6   Aplicaciones de Intereses Vivienda
     valor_aiv               LIKE cta_his_pagos.valor_aiv          ,  --DECIMAL(11,6) -- N   5   6   Precio Aplicación de Intereses Vivienda
     int_gen_pgo_ext         LIKE cta_his_pagos.int_gen_pgo_ext    ,  --DECIMAL(7,2)  -- N   5   2   Intereses Generado por Pagos Extemporaneos de Vivienda
     aiv_gen_pgo_ext         LIKE cta_his_pagos.aiv_gen_pgo_ext    ,  --DECIMAL(11,6) -- N   9   6   Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda
     result_operación        CHAR(2)       -- X   2   0   Resultado de la Operación                                             
   END RECORD
   
   --RECORD con el encabezado del archivo todos en char
   DEFINE v_det_string RECORD
      tpo_registro             CHAR(02),    -- X   2   0   Tipo Registro                                                         
      consecutivo_reg_lote     CHAR(08),    -- N   8       Consecutivo Registro dentro del Lote                                  
      cve_enti_receptora       CHAR(03),    -- X   3   0   Clave de Entidad Receptora                                            
      reg_patronal_imsss       CHAR(11),    -- X   11  0   Registro Patronal IMSS                                                
      rfc_patrón               CHAR(13),    -- X   13  0   RFC del Patrón                                                        
      periodo_pago             CHAR(06),    -- N   6   0   Periodo de Pago                                                       
      folio_pag_sua            CHAR(06),    -- N   6   0   Folio de Pago SUA                                                     
      nss                      CHAR(11),    -- X   11  0   Número de Seguriedad Social (NSS)                                     
      rfc_trabajador           CHAR(13),    -- X   13  0   RFC del Trabajador                                                    
      curp                     CHAR(18),    -- X   18  0   Clave Única de Registro de Población (CURP)                           
      num_crd_ifv              CHAR(10),    -- N   10  0   Número de Crédito INFONAVIT                                           
      f_ini_desc_crd_ifv       CHAR(08),    -- N   8   0   Fecha de inicio del descuento de crédito INFONAVIT                    
      paterno_trabajador       CHAR(40),    -- X   40  0   Paterno Trabajador                                                    
      materno_rabajador        CHAR(40),    -- X   40  0   Materno Trabajador                                                    
      nombre_trabajador        CHAR(40),    -- X   40  0   Nombre Trabajador                                                     
      tipo_aclaración          CHAR(02),    -- X   2   0   Tipo de Aclaración                                                    
      ult_sdi                  CHAR(07),    -- N   5   2   Último Salario Diario                                                 
      dias_cot_bim             CHAR(02),    -- N   2   0   Días Cotizados en Bimestre                                            
      dias_incap_bim           CHAR(02),    -- N   2   0   Días Incapacidad en Bimestre                                          
      dias_ausent_bim          CHAR(02),    -- N   2   0   Días Ausentismo en Bimestre                                           
      imp_ap_pat               CHAR(07),    -- N   5   2   Importe Aportación Patronal                                           
      imp_am_cre               CHAR(07),    -- N   5   2   Importe Amortización Crédito INFONAVIT                                
      imp_ren_viv_pgo_ext      CHAR(07),    -- N   5   2   Importe Rendimientos Subcuenta Vivienda                               
      aiv_ap_pat               CHAR(15),    -- N   9   6   Aplicaciones de Intereses Vivienda                                    
      valor_aiv                CHAR(11),    -- N   5   6   Precio Aplicación de Intereses Vivienda                               
      int_gen_pgo_ext          CHAR(07),    -- N   5   2   Intereses Generado por Pagos Extemporaneos de Vivienda                
      aiv_gen_pgo_ext          CHAR(15),    -- N   9   6   Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda
      result_operación         CHAR(02)     -- X   2   0   Resultado de la Operación                                             
   END RECORD
   
   --RECORD con el sumario del archivo
   DEFINE v_sum RECORD
       tipo_registro                       CHAR(02),    --X   2   0                                                                 
       id_operación                        CHAR(03),    --X   3   0                                                       
       f_creacion_lote                     CHAR(08),    --X   8   0   Fecha Creación Lote                                                         
       consecutivo_lote                    CHAR(03),    --X   3   0   Consecutivo de Lote                                                         
       num_reg_detalle                     CHAR(09),    --N   9   0   Numero Registros de Detalle                                                 
       tot_imp_ap_patronal                 CHAR(11),    --N   9   2   Total Importe Aportación Patronal                                           
       tot_imp_am_cred_ifv                 CHAR(11),    --N   9   2   Total Importe Amortización Crédito INFONAVIT                                
       tot_imp_rend_subcta_viv             CHAR(11),    --N   9   2   Total Importe Rendimientos Subcuenta Vivienda                               
       tot_apl_intereses_viv               CHAR(18),    --N   12  6   Total Aplicaciones de Intereses  Vivienda                                   
       tot_int_gene_pag_ext_viv            CHAR(11),    --N   9   2   Total Intereses Generado por Pagos Extemporaneos de  Vivienda               
       tot_apl_intereses_viv_pag_ext_viv   CHAR(18)    --N   12  6   Total Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda
   END RECORD
   
   
   -- se crea el manejador de archivo
   LET v_canal_archivo = BASE.CHANNEL.CREATE()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_canal_archivo.openFile(g_ruta_envio CLIPPED||"/"||g_nom_archivo, "w" )
   CALL v_canal_archivo.setDelimiter("")
   LET g_total_procesados = 0
   LET v_id_lote = 0
   LET v_lote_x_dia = 1

   -- se obtiene el id_referencia
   
   LET v_query =  "\n SELECT '02'                                        " --Tipo Registro                                                                     
                 ,"\n       ,''                                          " --Consecutivo Registro dentro del Lote                                              
                 ,"\n       ,''                                          " --Clave de Entidad Receptora                                                        
                 ,"\n       ,''                                          " --Registro Patronal IMSS                                                            
                 ,"\n       ,''                                          " --RFC del Patrón                                                                    
                 ,"\n       ,h.periodo_pago                              " --Periodo de Pago                                                                   
                 ,"\n       ,h.folio_sua                                 " --Folio de Pago SUA                                                                 
                 ,"\n       ,a.nss                                       " --Número de Seguriedad Social (NSS)                                                 
                 ,"\n       ,a.rfc                                       " --RFC del Trabajador                                                                
                 ,"\n       ,a.curp                                      " --Clave Única de Registro de Población (CURP)                                       
                 ,"\n       ,''                                          " --Número de Crédito INFONAVIT                                                       
                 ,"\n       ,''                                          " --Fecha de inicio del descuento de crédito INFONAVIT                                
                 ,"\n       ,a.ap_paterno_af                             " --Paterno Trabajador                                                                
                 ,"\n       ,a.ap_materno_af                             " --Materno Trabajador                                                                
                 ,"\n       ,a.nombre_af                                 " --Nombre Trabajador                                                                 
                 ,"\n       ,p.tpo_aclaracion                            " --Tipo de Aclaración                                                                
                 ,"\n       ,''                                          " --Último Salario Diario                                                             
                 ,"\n       ,''                                          " --Días Cotizados en Bimestre                                                        
                 ,"\n       ,''                                          " --Días Incapacidad en Bimestre                                                      
                 ,"\n       ,''                                          " --Días Ausentismo en Bimestre                                                       
                 ,"\n       ,h.imp_ap_pat                                " --Importe Aportación Patronal                                                       
                 ,"\n       ,h.imp_am_cre                                " --Importe Amortización Crédito INFONAVIT                                            
                 ,"\n       ,0                                           " --Importe Rendimientos Subcuenta Vivienda                                           
                 ,"\n       ,0                                           " --Aplicaciones de Intereses Vivienda                                                
                 ,"\n       ,0                                           " --Precio Aplicación de Intereses Vivienda                                           
                 ,"\n       ,0                                           " --Intereses Generado por Pagos Extemporaneos de Vivienda                            
                 ,"\n       ,0                                           " --Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda            
                 ,"\n       ,h.result_operacion                          " --Resultado de la Operación                                                         
                 ,"\n       ,h.id_derechohabiente                        " 
                 ,"\n FROM  cta_his_pagos h                              " 
                 ,"\n      ,pag_ctr_pago p                               " 
                 ,"\n      ,afi_derechohabiente a                        " 
                 ,"\n WHERE h.id_derechohabiente = p.id_derechohabiente  " 
                 ,"\n AND   h.id_derechohabiente = a.id_derechohabiente  " 
                 ,"\n AND   h.id_referencia = p.id_referencia            " 
                 ,"\n AND   h.folio = ?                       " 
                 ,"\n AND   h.origen_archivo = ?                         " 
                 ,"\n AND   h.id_referencia = ?                          " 
                  
   PREPARE prp_datos_archivo FROM v_query
   DECLARE cur_datos_archivo CURSOR FOR prp_datos_archivo
   
   --Se inicializan los contadores en CERO
   LET v_total_imp_ap_pat          = 0
   LET v_total_imp_am_cre          = 0
   LET v_total_imp_ren_viv_pgo_ext = 0
   LET v_total_imp_aiv_ap_pat      = 0
   LET v_total_imp_int_gen_pgo_ext = 0
   LET v_total_imp_aiv_gen_pgo_ext = 0
  { 
   --Se escribe el encabezado  
   LET v_enc.tpo_registro = "01"
   LET v_enc.tpo_registro = "ACL"
   LET v_enc.f_creacion = TODAY USING "yyyymmdd"
   LET v_enc.consecutivo = v_lote_x_dia USING "&&&&&&&&&&&&&&&&"
   
   --Se pasan a una sola cadena los datos del encabezado
   LET v_string_encabezado = v_enc.tpo_registro,
                             v_enc.tpo_registro,
                             v_enc.f_creacion ,
                             v_enc.consecutivo 
   
   --Se escribe en el archivo los datos obtenidos del encabezado
   CALL v_canal_archivo.WRITE([v_string_encabezado])
   }

   --Se obtienen los datos del detalle con el query
   FOREACH cur_datos_archivo USING p_num_folio, p_origen_archivo, p_id_referencia
                             INTO v_det.*, v_id_derechohabiente_tmp
      
      --Se obtienen los detalles segun el origen del archivo
      CASE p_origen_archivo
         WHEN 1 --LQINFO
            --Se obtienen solo los datos que contiene LQINFO
            LET v_query =  "\n   SELECT  ctp.nrp                       "   
                          ,"\n          ,ctc.rfc_patron                "
                          ,"\n          ,ctp.num_crd_ifv               "
                          ,"\n          ,ctc.f_ini_desc_crd_ifv        "
                          ,"\n          ,ctc.ult_sdi                   "
                          ,"\n          ,ctc.dias_cot_bim              "
                          ,"\n          ,ctc.dias_incap_bim            "
                          ,"\n          ,ctc.dias_ausent_bim           "
                          ,"\n          ,ctp.imp_ren_viv_pgo_ext       "
                          ,"\n          ,ctp.aiv_ap_pat                "
                          ,"\n          ,ctp.valor_aiv                 "
                          ,"\n          ,ctp.int_gen_pgo_ext           "
                          ,"\n          ,ctp.aiv_gen_pgo_ext           "
                          ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                          ,"\n    WHERE ctp.folio  = ctc.folio  "
                          ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                          ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                          ,"\n      AND ctp.folio = ?   "
                          ,"\n     AND  ctp.id_derechohabiente = ? "  
                          ,"\n     AND  ctp.id_referencia = ?      "  
            PREPARE prp_detalle_1 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_1 USING p_num_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO  v_det.reg_patronal_imsss  ,  
                                      v_det.rfc_patrón          ,
                                      v_det.num_crd_ifv         ,
                                      v_det.f_ini_desc_crd_ifv  ,
                                      v_det.ult_sdi             ,
                                      v_det.dias_cot_bim        ,
                                      v_det.dias_incap_bim      , 
                                      v_det.dias_ausent_bim     , 
                                      v_det.imp_ren_viv_pgo_ext , 
                                      v_det.aiv_ap_pat          , 
                                      v_det.valor_aiv           , 
                                      v_det.int_gen_pgo_ext     , 
                                      v_det.aiv_gen_pgo_ext 
            FREE prp_detalle_1
         WHEN 2 --SAR92
            --Se obtienen solo los datos que contiene SAR92
            LET v_query = "\n SELECT rfc_patron            "
                         ,"\n FROM  pag_det_sar92          "
                         ,"\n WHERE folio = ?              "
                         ,"\n AND   id_referencia = ?      "
            PREPARE prp_detalle_2 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_2 USING p_num_folio, p_id_referencia
                                INTO  v_det.rfc_patrón
            FREE prp_detalle_2
         WHEN 3 --SOLO INFONAVIT
            --Se obtienen solo los datos que contiene SOLO INFONAVIT
            LET v_query = "\n SELECT nrp                   "
                         ,"\n FROM  cta_his_pagos          "
                         ,"\n WHERE folio = ?              "
                         ,"\n AND   id_derechohabiente = ? "
                         ,"\n AND   id_referencia = ?      "
            PREPARE prp_detalle_3 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_3 USING p_num_folio, v_id_derechohabiente_tmp, p_id_referencia
                                   INTO v_det.reg_patronal_imsss
            FREE prp_detalle_3
         WHEN 4 --CARGA INICIAL ACLARATORIO
         WHEN 5 --ACLARATORIO SIN CAMBIO NSS
            --Se obtienen solo los datos que contiene ACLARATORIO SIN CAMBIO NSS
            LET v_query =  "\n   SELECT  ctp.nrp                       "   
                          ,"\n          ,ctc.rfc_patron                "
                          ,"\n          ,ctp.num_crd_ifv               "
                          ,"\n          ,ctc.f_ini_desc_crd_ifv        "
                          ,"\n          ,ctc.ult_sdi                   "
                          ,"\n          ,ctc.dias_cot_bim              "
                          ,"\n          ,ctc.dias_incap_bim            "
                          ,"\n          ,ctc.dias_ausent_bim           "
                          ,"\n          ,ctp.imp_ren_viv_pgo_ext       "
                          ,"\n          ,ctp.aiv_ap_pat                "
                          ,"\n          ,ctp.valor_aiv                 "
                          ,"\n          ,ctp.int_gen_pgo_ext           "
                          ,"\n          ,ctp.aiv_gen_pgo_ext           "
                          ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                          ,"\n    WHERE ctp.folio  = ctc.folio  "
                          ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                          ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                          ,"\n      AND ctp.folio = ?   "
                          ,"\n      AND ctp.id_derechohabiente = ? "
                          ,"\n      AND ctp.id_referencia = ?      "
            
            display "v_query  ",v_query
            display " ", p_num_folio, v_id_derechohabiente_tmp, p_id_referencia
            PREPARE prp_detalle_5 FROM v_query
            
            
            --Ejecutamos la consulta
            EXECUTE prp_detalle_5 USING p_num_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO  v_det.reg_patronal_imsss   ,  
                                      v_det.rfc_patrón          ,
                                      v_det.num_crd_ifv         ,
                                      v_det.f_ini_desc_crd_ifv  ,
                                      v_det.ult_sdi             ,
                                      v_det.dias_cot_bim        ,
                                      v_det.dias_incap_bim      , 
                                      v_det.dias_ausent_bim     , 
                                      v_det.imp_ren_viv_pgo_ext , 
                                      v_det.aiv_ap_pat          , 
                                      v_det.valor_aiv           , 
                                      v_det.int_gen_pgo_ext     , 
                                      v_det.aiv_gen_pgo_ext 

             FREE prp_detalle_5                                     
         WHEN 6 --ACLARATORIO CON CAMBIO NSS
            --Se obtienen solo los datos que contiene ACLARATORIO CON CAMBIO NSS
            LET v_query =  "\n   SELECT  ctp.nrp                       "   
                          ,"\n          ,ctc.rfc_patron                "
                          ,"\n          ,ctp.num_crd_ifv               "
                          ,"\n          ,ctc.f_ini_desc_crd_ifv        "
                          ,"\n          ,ctc.ult_sdi                   "
                          ,"\n          ,ctc.dias_cot_bim              "
                          ,"\n          ,ctc.dias_incap_bim            "
                          ,"\n          ,ctc.dias_ausent_bim           "
                          ,"\n          ,ctp.imp_ren_viv_pgo_ext       "
                          ,"\n          ,ctp.aiv_ap_pat                "
                          ,"\n          ,ctp.valor_aiv                 "
                          ,"\n          ,ctp.int_gen_pgo_ext           "
                          ,"\n          ,ctp.aiv_gen_pgo_ext           "
                          ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                          ,"\n    WHERE ctp.folio  = ctc.folio  "
                          ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                          ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                          ,"\n      AND ctp.folio = ?   "
                         ,"\n       AND ctp.id_derechohabiente = ? "  
                         ,"\n       AND ctp.id_referencia = ?      "  
            PREPARE prp_detalle_6 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_6 USING p_num_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO v_det.reg_patronal_imsss  ,    
                                     v_det.rfc_patrón          ,    
                                     v_det.num_crd_ifv         ,    
                                     v_det.f_ini_desc_crd_ifv  ,    
                                     v_det.ult_sdi             ,    
                                     v_det.dias_cot_bim        ,    
                                     v_det.dias_incap_bim      ,    
                                     v_det.dias_ausent_bim     ,    
                                     v_det.imp_ren_viv_pgo_ext ,    
                                     v_det.aiv_ap_pat          ,    
                                     v_det.valor_aiv           ,    
                                     v_det.int_gen_pgo_ext     ,    
                                     v_det.aiv_gen_pgo_ext          
            FREE prp_detalle_6                                      
         WHEN 7 --ACLARATORIO CON CAMBIO NOMBRE
         WHEN 8 --ENACLARA
         	   --Se obtienen solo los datos que contiene ENACLARA
             LET v_query = "\n SELECT    ctp.nrp                    "           	
                          ,"\n          ,ctc.rfc                    "
                          ,"\n          ,ctp.num_crd_ifv            "
                          ,"\n          ,ctc.f_ini_desc_crd_ifv     "
                          ,"\n          ,ctc.ult_sdi                "
                          ,"\n          ,ctc.dias_cot_bim           "
                          ,"\n          ,ctc.dias_incap_bim         "
                          ,"\n          ,ctc.dias_ausent_bim        "
                          ,"\n          ,ctp.imp_ren_viv_pgo_ext    "
                          ,"\n          ,ctp.aiv_ap_pat             "
                          ,"\n          ,ctp.valor_aiv              "
                          ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                          ,"\n    WHERE ctp.folio  = ctc.folio  "
                          ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                          ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                          ,"\n      AND ctp.folio = ?   "
                          ,"\n      AND ctp.id_derechohabiente = ? "  
                          ,"\n      AND ctp.id_referencia = ?      "  
            PREPARE prp_detalle_8 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_8 USING p_num_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO  v_det.reg_patronal_imsss  ,    
                                      v_det.rfc_patrón          ,    
                                      v_det.num_crd_ifv         ,    
                                      v_det.f_ini_desc_crd_ifv  ,    
                                      v_det.ult_sdi             ,    
                                      v_det.dias_cot_bim        ,    
                                      v_det.dias_incap_bim      ,    
                                      v_det.dias_ausent_bim     ,    
                                      v_det.aiv_ap_pat          ,    
                                      v_det.valor_aiv              
            FREE prp_detalle_8                                       
      END CASE
      
      
      --Se realiza la suma de los importes
      LET v_total_imp_ap_pat          = v_total_imp_ap_pat          + v_det.imp_ap_pat         
      LET v_total_imp_am_cre          = v_total_imp_am_cre          + v_det.imp_am_cre         
      LET v_total_imp_ren_viv_pgo_ext = v_total_imp_ren_viv_pgo_ext + v_det.imp_ren_viv_pgo_ext
      LET v_total_imp_aiv_ap_pat      = v_total_imp_aiv_ap_pat      + v_det.aiv_ap_pat         
      LET v_total_imp_int_gen_pgo_ext = v_total_imp_int_gen_pgo_ext + v_det.int_gen_pgo_ext         
      LET v_total_imp_aiv_gen_pgo_ext = v_total_imp_aiv_gen_pgo_ext + v_det.aiv_gen_pgo_ext         
      LET v_id_lote = v_id_lote + 1
      LET g_total_procesados          = g_total_procesados + 1
      
      
      --Se pasan los detalles a su equivalente en tipo char
      LET v_det_string.tpo_registro            = v_det.tpo_registro 
      LET v_det_string.consecutivo_reg_lote    = v_det.consecutivo_reg_lote   USING "&&&&&&&&"                                                                         
      LET v_det_string.cve_enti_receptora      = v_det.cve_enti_receptora                                                                                            
      LET v_det_string.reg_patronal_imsss      = v_det.reg_patronal_imsss     
      LET v_det_string.rfc_patrón              = v_det.rfc_patrón             
      LET v_det_string.periodo_pago            = v_det.periodo_pago           
      LET v_det_string.folio_pag_sua           = v_det.folio_pag_sua          
      LET v_det_string.nss                     = v_det.nss                    
      LET v_det_string.rfc_trabajador          = v_det.rfc_trabajador         
      LET v_det_string.curp                    = v_det.curp                   
      LET v_det_string.num_crd_ifv             = v_det.num_crd_ifv            
      LET v_det_string.f_ini_desc_crd_ifv      = YEAR(v_det.f_ini_desc_crd_ifv) USING "&&&&", MONTH(v_det.f_ini_desc_crd_ifv) USING "&&",DAY(v_det.f_ini_desc_crd_ifv) USING "&&"
      LET v_det_string.paterno_trabajador      = v_det.paterno_trabajador     
      LET v_det_string.materno_rabajador       = v_det.materno_rabajador      
      LET v_det_string.nombre_trabajador       = v_det.nombre_trabajador
      LET v_det_string.tipo_aclaración         = v_det.tipo_aclaración
      LET v_det_string.ult_sdi                 =(v_det.ult_sdi * 100) USING "&&&&&&&"
      LET v_det_string.dias_cot_bim            = v_det.dias_cot_bim
      LET v_det_string.dias_incap_bim          = v_det.dias_incap_bim
      LET v_det_string.dias_ausent_bim         = v_det.dias_ausent_bim
      LET v_det_string.imp_ap_pat              =(v_det.imp_ap_pat * 100) USING "&&&&&&&"                                                                             
      LET v_det_string.imp_am_cre              =(v_det.imp_am_cre * 100) USING "&&&&&&&"                                                                             
      LET v_det_string.imp_ren_viv_pgo_ext     =(v_det.imp_ren_viv_pgo_ext * 100) USING "&&&&&&&"                                                                    
      LET v_det_string.aiv_ap_pat              =(v_det.aiv_ap_pat * 1000000) USING "&&&&&&&&&&&&&&&"                                                                 
      LET v_det_string.valor_aiv               =(v_det.valor_aiv * 1000000) USING "&&&&&&&&&&&"                                                                      
      LET v_det_string.int_gen_pgo_ext         =(v_det.int_gen_pgo_ext * 100) USING "&&&&&&&"                                                                        
      LET v_det_string.aiv_gen_pgo_ext         =(v_det.aiv_gen_pgo_ext * 1000000) USING "&&&&&&&&&&&&&&&"                                                            
      LET v_det_string.result_operación        = v_det.result_operación                                                                                               
      
      --Se asigna todo lo obtenido a una sola cadena que sera escrita en el archivo
      LET v_string_detalle = v_det_string.tpo_registro         ,  
                             v_det_string.consecutivo_reg_lote ,
                             v_det_string.cve_enti_receptora   ,
                             v_det_string.reg_patronal_imsss   ,
                             v_det_string.rfc_patrón           ,
                             v_det_string.periodo_pago         ,
                             v_det_string.folio_pag_sua        ,
                             v_det_string.nss                  ,
                             v_det_string.rfc_trabajador       ,
                             v_det_string.curp                 ,
                             v_det_string.num_crd_ifv          ,
                             v_det_string.f_ini_desc_crd_ifv   ,
                             v_det_string.paterno_trabajador   ,
                             v_det_string.materno_rabajador    ,
                             v_det_string.nombre_trabajador    ,
                             v_det_string.tipo_aclaración      ,
                             v_det_string.ult_sdi              ,
                             v_det_string.dias_cot_bim         ,
                             v_det_string.dias_incap_bim       ,
                             v_det_string.dias_ausent_bim      ,
                             v_det_string.imp_ap_pat           ,
                             v_det_string.imp_am_cre           ,
                             v_det_string.imp_ren_viv_pgo_ext  ,
                             v_det_string.aiv_ap_pat           ,
                             v_det_string.valor_aiv            ,
                             v_det_string.int_gen_pgo_ext      ,
                             v_det_string.aiv_gen_pgo_ext      ,
                             v_det_string.result_operación      

      --Se escribe en el archivo los datos obtenidos
      CALL v_canal_archivo.WRITE([v_string_detalle])

      --Se dejan los arrays como nulos
      INITIALIZE v_det TO NULL
      INITIALIZE v_det_string TO NULL
   END FOREACH
   
   --Se obtienen los registros del sumario
   LET v_sum.tipo_registro                     = "09"
   LET v_sum.id_operación                      = "ACL"
   LET v_sum.f_creacion_lote                   = YEAR(TODAY)  USING "&&&&", MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
   LET v_sum.consecutivo_lote                  = v_lote_x_dia USING "&&&"
   LET v_sum.num_reg_detalle                   = g_total_procesados USING "&&&&&&&&&"
   LET v_sum.tot_imp_ap_patronal               = (v_total_imp_ap_pat * 100) USING "&&&&&&&&&&&"
   LET v_sum.tot_imp_am_cred_ifv               = (v_total_imp_am_cre * 100) USING "&&&&&&&&&&&"
   LET v_sum.tot_imp_rend_subcta_viv           = (v_total_imp_ren_viv_pgo_ext * 100)     USING "&&&&&&&&&&&"
   LET v_sum.tot_apl_intereses_viv             = (v_total_imp_aiv_ap_pat * 1000000)      USING "&&&&&&&&&&&&&&&&&&"
   LET v_sum.tot_int_gene_pag_ext_viv          = (v_total_imp_int_gen_pgo_ext * 100)     USING "&&&&&&&&&&&"
   LET v_sum.tot_apl_intereses_viv_pag_ext_viv = (v_total_imp_aiv_gen_pgo_ext * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   
   LET v_string_sumario = v_sum.tipo_registro                     
                         ,v_sum.id_operación                      
                         ,v_sum.f_creacion_lote                   
                         ,v_sum.consecutivo_lote                  
                         ,v_sum.num_reg_detalle                   
                         ,v_sum.tot_imp_ap_patronal               
                         ,v_sum.tot_imp_am_cred_ifv               
                         ,v_sum.tot_imp_rend_subcta_viv           
                         ,v_sum.tot_apl_intereses_viv             
                         ,v_sum.tot_int_gene_pag_ext_viv          
                         ,v_sum.tot_apl_intereses_viv_pag_ext_viv 
   
   --Se escribe en el archivo los datos obtenidos del sumario
      CALL v_canal_archivo.WRITE([v_string_sumario])
   
   --Se cierra el archivo
   CALL v_canal_archivo.CLOSE()

END FUNCTION