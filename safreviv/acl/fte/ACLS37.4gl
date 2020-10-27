--==============================================================================
-- Version                    => 1.0.0
-- Fecha ultima modificacion  => 17/05/2016
-- Proyecto          => SAFRE VIVIENDA
-- Autor             => GERARDO ALFONSO VEGA PAREDES
---------------------------------------------------------------------------------
-- Modulo            => ACLn
-- Componente        => ACLS37
-- Funcionalidad     => Lanzado de extractor de pagos pendientes en aclaratorio 
--                      y adelantados sin conciliar
-- Fecha inicio      => 17 de Mayo de 2016
--==============================================================================
-- Modificación      => Continuar con el desarrollo
-- Autor             => GERARDO ALFONSO VEGA PAREDES
-- Fecha             => 9 de Junio de 2016
--==============================================================================

DATABASE safre_viv

GLOBALS 
   DEFINE g_tipo       CHAR(1),
          v_modulo_cod LIKE seg_modulo.modulo_cod

   DEFINE p_usuario     LIKE seg_usuario.usuario_cod,
          p_pid         DECIMAL (9,0),
          p_proceso_cod LIKE cat_proceso.proceso_cod,
          p_opera_cod   LIKE cat_operacion.opera_cod

   DEFINE v_archivo_copia VARCHAR (25),
          v_comando_dos   STRING

END GLOBALS 
     
MAIN
   DEFINE r_bandera      SMALLINT, 
      v_consulta STRING,
      v_ruta_envio LIKE seg_modulo.ruta_envio,
      v_ruta_bin LIKE seg_modulo.ruta_bin,
      v_archivo_exe STRING

  DEFINE v_reg RECORD
   	  id_derecho   decimal(9,0),
      nss          char(11),
      nrp          char(11),
      periodo_pago char(06),
      bim          char(06),
      f_pago       date,
      tpo_acl      char(02),
      folio_sua    decimal(6,0),
      cve_ent_rece char(03),
      imp_ap_pat   decimal(12,2),
      aiv_ap_pat   decimal(18,6),
      imp_am_cre   decimal(12,2),
      int_pgo_ext  decimal(12,2),
      aiv_pgo_ext  decimal(18,6),
      folio        decimal(9,0),
      origen_archivo SMALLINT,
      result_operacion SMALLINT  
   END RECORD
   
   DEFINE v_existe SMALLINT
      
   --Asignación de parametros generales 
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2) 
   LET p_proceso_cod    = ARG_VAL(3) 
   LET p_opera_cod      = ARG_VAL(4)
   LET g_tipo           = ARG_VAL(5) -- Valor de argumento uno de DISL18 


   
   LET v_modulo_cod = "acl"
   
   SELECT ruta_envio,ruta_bin
   INTO v_ruta_envio,v_ruta_bin
   FROM seg_modulo
   WHERE modulo_cod = 'acl'

   LET v_archivo_exe = v_ruta_bin CLIPPED,"/ext_acl_",TODAY USING "ddmmyyyy" ,".sql "

   DISPLAY "Inicia generación de reporte TXT"
   
   IF g_tipo == "A" THEN
      --query de alcaraciones pendientes aclaratorio:
      LET v_consulta = 
          "echo \"",
         "unload to '",v_ruta_envio CLIPPED, "/ext_acl_pendiente-",TODAY USING "ddmmyyyy" ,".acl' ",
         "\n select afi.nss, ",
         "\n        det.nrp, ",
         "\n        fn_bimestre_pago(periodo_pago), ",
         "\n        TO_CHAR(det.f_pago,'%Y%m%d'), ",--CABC fecha en formato YYYYMMDD   
         "\n        det.cve_ent_receptora, ",  
         "\n        det.tpo_aclaracion, ",
         "\n        det.folio_sua, ",
         "\n        det.imp_ap_pat, ",
         "\n        det.aiv_ap_pat, ",                  
         "\n        det.imp_am_cre, ",
         "\n        det.int_gen_pgo_ext, ",
         "\n        det.aiv_gen_pgo_ext,  " ,
         "\n        det.folio, ",
         "\n        det.origen_archivo, ",
         "\n        det.result_operacion ",
         "\n from cta_his_pagos det ",--Se coloca LEFT OUTER JOIN ya que no existen algunos id_derechohabiente 
         "\n left outer join afi_derechohabiente afi ON (afi.id_derechohabiente = det.id_derechohabiente) ", 
         "\n where  det.folio in (SELECT folio FROM glo_folio ",
         "\n                      WHERE proceso_cod IN (1401, 101,102,103,105,107,110)) ",
         "\n and    det.ind_liquidacion = 1 ", 
         --"\n and    det.tpo_aclaracion <> '' ",se comenta linea CABC PRODINFXVI-90 no cuadran cifras con causales sin liquidar
         "\" > ",v_archivo_exe
   ELSE

      DATABASE safre_tmp
      WHENEVER ERROR CONTINUE
      LET v_consulta = "DROP TABLE tmp_ade_pendientes"
      PREPARE cla_drop FROM v_consulta
      EXECUTE cla_drop
      WHENEVER ERROR STOP

      LET v_consulta = " CREATE TABLE tmp_ade_pendientes ",
                       " (id_derechohabiente         DECIMAL(9,0),  ",
                       " nss         char(11), ",
                       " nrp         char(11),  ",
                       " bim         char(06),  ",
                       " f_pago      date,      ",
                       " cve_ent_receptora    char(03),      ",
                       " tpo_acl     char(02),  ",
                       " folio_sua   decimal(6,0),  ",
                       " imp_ap_pat  decimal(12,2), ",
                       " aiv_ap_pat  decimal(18,6), ",
                       " imp_am_cre  decimal(12,2), ",
                       " int_pgo_ext decimal(12,2), ",
                       " aiv_pgo_ext decimal(18,6), ",
                       " folio       decimal(9,0), ",
                       " origen_archivo smallint, ",
                       " result_operacion  smallint ",
                       ") "
      
      PREPARE cla_crea FROM v_consulta
      EXECUTE cla_crea
      DATABASE safre_viv

      DECLARE cur_ade_pendientes CURSOR FOR      
      SELECT 
             det.id_derechohabiente,
             afi.nss,                                                        
             det.nrp,                                                        
             periodo_pago,                        
             fn_bimestre_pago(periodo_pago),         
             det.f_pago,                                                     
             det.tpo_aclaracion,                                             
             det.folio_sua,
             det.cve_ent_receptora,
             det.imp_ap_pat,                                                 
             det.aiv_ap_pat,                                                 
             det.imp_am_cre,                                                 
             det.int_gen_pgo_ext,                                            
             det.aiv_gen_pgo_ext,
             det.folio,
             det.origen_archivo,
             det.result_operacion             
      from   cta_his_pagos det LEFT OUTER JOIN --Se coloca LEFT OUTER JOIN ya que no existen algunos id_derechohabiente
                afi_derechohabiente afi ON (afi.id_derechohabiente = det.id_derechohabiente)                          
      where  det.folio in (SELECT folio FROM glo_folio                       
                           WHERE proceso_cod IN (1401, 101,102,103,105,107,110))
      and    det.ind_liquidacion in (2,3)  -- registros adelantados por SI o fusión afore o traspaso
      
      FOREACH cur_ade_pendientes INTO v_reg.*

        LET v_existe=0

         SELECT FIRST 1 1               -- busca los adelantados no confirmados
         INTO v_existe
         FROM   cta_his_pagos
         WHERE  id_derechohabiente = v_reg.id_derecho
         AND    folio_sua          = v_reg.folio_sua
         AND    periodo_pago       = v_reg.periodo_pago
         AND    f_pago             = v_reg.f_pago
         AND    nrp                = v_reg.nrp
         AND    imp_ap_pat         = v_reg.imp_ap_pat
         AND    imp_am_cre         = v_reg.imp_am_cre
         AND    ind_liquidacion    = 4
         AND    folio              > v_reg.folio

       --  DISPLAY "ENTRO existe:",v_existe,"derechohabiente:",v_reg.id_derecho,"FOLIO:",v_reg.folio
      
         IF v_existe <>1 THEN
         	  INSERT INTO safre_tmp:tmp_ade_pendientes VALUES ( 
                v_reg.id_derecho,
         	    v_reg.nss,        
                v_reg.nrp,        
                v_reg.bim,        
                v_reg.f_pago,  
                v_reg.cve_ent_rece,   
                v_reg.tpo_acl,    
                v_reg.folio_sua,  
                v_reg.imp_ap_pat, 
                v_reg.aiv_ap_pat, 
                v_reg.imp_am_cre, 
                v_reg.int_pgo_ext,
                v_reg.aiv_pgo_ext,
                v_reg.folio,
                v_reg.origen_archivo,
                v_reg.result_operacion)
                             
            --PREPARE cla_inserta FROM v_consulta
            --EXECUTE cla_inserta
         END IF
      END FOREACH

      --Query de Aclaraciones pendientes adelantados.
      LET v_consulta = 
          "echo \"",
         "unload to '",v_ruta_envio CLIPPED, "/ext_acl_pend_adel-",TODAY USING "ddmmyyyy" ,".acl' ",
         "\n select nss, ",
         "\n        nrp, ",
         "\n        bim, ",
         "\n        TO_CHAR(f_pago,'%Y%m%d'), ",--CABC fecha en formato YYYYMMDD   
         "\n        cve_ent_receptora, ",
         "\n        tpo_acl, ",
         "\n        folio_sua, ",
         "\n        imp_ap_pat, ",
         "\n        aiv_ap_pat, ",                  
         "\n        imp_am_cre, ",
         "\n        int_pgo_ext, ",
         "\n        aiv_pgo_ext,  " ,
         "\n        folio, ",
         "\n        origen_archivo, ",
         "\n        result_operacion ",
         "\n from   safre_tmp:tmp_ade_pendientes ",
         "\" > ",v_archivo_exe
   END IF
   RUN v_consulta

   LET v_consulta = "$INFORMIXDIR/bin/dbaccess safre_viv ",v_archivo_exe
   RUN v_consulta
   DISPLAY "Fin de generación de reporte TXT"
   CALL fn_genera_reporte()
   
   --Finaliza la operación
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
   RETURNING r_bandera
  
   IF r_bandera = 0 THEN 
      DISPLAY "Se ha realizado la generación del archivo."
      DISPLAY "El archivo se encuentra en la ruta ",v_ruta_envio CLIPPED," con formato ext_acl*-DDMMYYYY.acl "
      EXIT PROGRAM 
   ELSE --Si ocurrió error 
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
      CALL fn_desplega_inc_operacion(r_bandera)
      EXIT PROGRAM 
   END IF
   
END MAIN

FUNCTION fn_genera_reporte()

   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE v_v_nom_reporte VARCHAR(80)
   DEFINE v_existe SMALLINT
   DEFINE v_cont SMALLINT

   DEFINE reg_pend RECORD
      reg      DECIMAL(10,0),
      reg_ctas DECIMAL(10,0),
      sum_aivs DECIMAL(12,2),
      sum_amor DECIMAL(12,2),
      titulo_pdf VARCHAR(15)
   END RECORD
   
   DEFINE arr_pend DYNAMIC ARRAY OF RECORD
      reg      DECIMAL(10,0),
      reg_ctas DECIMAL(10,0),
      sum_aivs DECIMAL(12,2),
      sum_amor DECIMAL(12,2),
      titulo_pdf VARCHAR(15)
   END RECORD

   DEFINE v_consulta STRING
   DEFINE v_manejador_rpt om.SaxDocumentHandler -- handler para el reporte

--===============================================================================
    -- NOMENCLARUTA PARA QUE SALGA REPORTE EN EL AOPERACIÓN EN MONITOR DE PROCESOS
    LET v_v_nom_reporte = p_usuario CLIPPED,"-ACLS37-",p_pid USING "&&&&&", "-",p_proceso_cod USING "&&&&&",
                          "-",p_opera_cod USING "&&&&&"

    CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

    CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/ACLS371.4rp") RETURNING v_existe

    -- se indica la salida del reporte
    CALL fgl_report_selectDevice("PDF")

    CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)

    -- sin indica que no es necesario el preview
    CALL fgl_report_selectPreview(0)

    -- se asigna la configuración en el menejo del reporte
    LET v_manejador_rpt = fgl_report_commitCurrentSettings()

--===============================================================================
   DISPLAY "Inicia generación de reporte PDF"
   
   IF g_tipo == "A" THEN

      LET v_consulta = 
         " select count(*), ",#Pagos pendientes
         "        count(unique det.id_derechohabiente), ",#trabajadores en aclaracion 
         --se cambia por linea de abajo "        sum(det.aiv_ap_pat), ",      #total aivs   
         "        SUM(det.aiv_ap_pat + det.aiv_gen_pgo_ext ), ",
         "        sum(det.imp_am_cre), ",#total amortizacion 
         "        'Trabajadores' ",#total amortizacion
         " from   cta_his_pagos det ",
         " where  det.folio in (SELECT folio FROM glo_folio ",
         "                      WHERE proceso_cod IN (1401, 101,102,103,105,107,110)) ",
         " and    det.ind_liquidacion = 1 "
        -- " and    det.tpo_aclaracion <> '' "se comenta linea CABC PRODINFXVI-90 no cuadran cifras con causales sin liquidar

      LET v_cont = 1 

      PREPARE cla_acl_pend FROM v_consulta
      DECLARE cur_acl_pend CURSOR FOR cla_acl_pend
      FOREACH cur_acl_pend INTO reg_pend.*
         LET arr_pend[v_cont].* = reg_pend.* 
         LET v_cont = v_cont + 1
      END FOREACH
      
      LET v_consulta = 
         " select 0, ",                        #Pagos pendientes
         "        count(unique det.nrp), ",    #patrones en aclaracion 
         "        0, ",                        #total aivs             
         "        0,  ",                       #toal amortizacion
         "        'Patrones' ",                #total amortizacion
         " from   cta_his_pagos det",
         " where  det.folio in (SELECT folio FROM glo_folio ",
         "                      WHERE proceso_cod IN (1401, 101,102,103,105,107,110)) ",
         " and    det.ind_liquidacion = 1 "
     --    " and    det.tpo_aclaracion <> '' "     se comenta linea CABC PRODINFXVI-90 no cuadran cifras con causales sin liquidar

      PREPARE cla_acl_pend2 FROM v_consulta
      DECLARE cur_acl_pend2 CURSOR FOR cla_acl_pend2
      FOREACH cur_acl_pend2 INTO reg_pend.*
         LET arr_pend[v_cont].* = reg_pend.* 
         LET v_cont = v_cont + 1
      END FOREACH     

      START REPORT rpt_acl_pend TO XML HANDLER v_manejador_rpt   

      FOR v_cont = 1 TO arr_pend.getLength()
         OUTPUT TO REPORT rpt_acl_pend(arr_pend[v_cont].*)
      END FOR
      FINISH REPORT rpt_acl_pend

   ELSE
   
         LET v_consulta = 
                 " select count(*), ",#Pagos pendientes
                 "        count(unique det.id_derechohabiente), ",#trabajadores en aclaracion 
                  --se cambia por linea de abajo "        sum(det.aiv_ap_pat), ",      #total aivs   
                 "        SUM(det.aiv_ap_pat + det.aiv_pgo_ext ), ",            
                 "        sum(det.imp_am_cre), ",#total amortizacion
                 "        'Trabajadores' ",#total amortizacion
                 " from safre_tmp:tmp_ade_pendientes det "

              LET v_cont = 1 

              PREPARE cla_acl_pend_temp FROM v_consulta
              DECLARE cur_acl_pend_temp CURSOR FOR cla_acl_pend_temp
              FOREACH cur_acl_pend_temp INTO reg_pend.*
                 LET arr_pend[v_cont].* = reg_pend.* 
                 LET v_cont = v_cont + 1
              END FOREACH
              
              LET v_consulta = 
                 " select 0, ",                       #Pagos pendientes
                 "        count(unique det.nrp), ",   #patrones en aclaracion 
                 "        0, ",                       #total aivs             
                 "        0,  ",                      #toal amortizacion
                 "        'Patrones' ",               #total amortizacion
                 " from  safre_tmp:tmp_ade_pendientes det "

              PREPARE cla_acl_pend2_temp FROM v_consulta
              DECLARE cur_acl_pend2_temp CURSOR FOR cla_acl_pend2_temp
              FOREACH cur_acl_pend2_temp INTO reg_pend.*
                 LET arr_pend[v_cont].* = reg_pend.* 
                 LET v_cont = v_cont + 1
              END FOREACH     

              START REPORT rpt_acl_pend TO XML HANDLER v_manejador_rpt   

              FOR v_cont = 1 TO arr_pend.getLength()
                 OUTPUT TO REPORT rpt_acl_pend(arr_pend[v_cont].*)
              END FOR
              FINISH REPORT rpt_acl_pend
              
           END IF 

   DISPLAY "Fin de generación de reporte PDF"
   
END FUNCTION

REPORT rpt_acl_pend(reg_pend)

   DEFINE reg_pend RECORD
      reg      DECIMAL(10,0),
      reg_ctas DECIMAL(10,0),
      sum_aivs DECIMAL(12,2),
      sum_amor DECIMAL(12,2),
      titulo_pdf VARCHAR(15)
   END RECORD
    
   DEFINE v_fecha STRING
   DEFINE v_nombre_usuario VARCHAR(100)

   FORMAT
   
      FIRST PAGE HEADER
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         
         SELECT usuario_desc
         INTO   v_nombre_usuario
         FROM   seg_usuario
         WHERE  usuario_cod = p_usuario

         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         PRINTX v_fecha, p_usuario, v_nombre_usuario
         
      ON EVERY ROW
         PRINTX reg_pend.*

END REPORT