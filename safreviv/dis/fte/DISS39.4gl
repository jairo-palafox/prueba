################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 07/07/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS39                                                   #
#Objetivo          => Programa que genera la interface de aportaciones         # 
#                     subsecuentes (acreditados no existentes)                 #
#Fecha inicio      => 07/07/2015                                               #
################################################################################
DATABASE
     safre_viv

GLOBALS
  DEFINE 
    v_archivo_copia          VARCHAR(25),
    v_comando_dos            STRING
    
  DEFINE g_sql_txt           STRING,     --Consultas 
         g_usuario           VARCHAR(30) --Almacena al usuario

    --Datos de detalle
  DEFINE a_fac_apo_sub DYNAMIC ARRAY OF RECORD    
    entidad_financiera   CHAR(65),
    cuenta_bancaria      CHAR(40),    
    documento_fico 		 CHAR(10),
    concepto			 CHAR(50),
    estado               CHAR(50),
    monto                DECIMAL (18,6)    
  END RECORD   

  --Totales 
  DEFINE a_totales_con DYNAMIC ARRAY OF RECORD
    concepto_tot        CHAR(50),     -- Concepto
    monto_tot           DECIMAL(18,6) -- Total monto
  END RECORD

    --Totales 
  DEFINE 
    v_tot_det           CHAR(50),     -- Total del detalle
    v_tot_con           DECIMAL(18,6) -- Total por concepto  
  
END GLOBALS
     
MAIN 
  DEFINE 
    v_cuenta                 INTEGER

    --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)

  CALL STARTLOG (g_usuario CLIPPED||".DISS39.log")


  --Valida que exista información para generar la interface
  --Valida aportaciones
  SELECT COUNT(*)
  INTO   v_cuenta 
  FROM   tmp_dis_fac_aps_tns

  DISPLAY "v_cuenta: -",v_cuenta,"-" 
  
  IF v_cuenta IS NULL THEN  
     LET v_cuenta = 0 
  END IF 

  IF v_cuenta = 0 OR v_cuenta IS NULL THEN 
     DISPLAY "No se generó la interface de Aportaciones Subsecuentes Facturar (Acreditados no existentes) por falta de información"
     EXIT PROGRAM 
  ELSE 
    CALL fn_genera_archivo()

    CALL fn_genera_cifras_control()
  END IF 


END MAIN 


FUNCTION fn_genera_archivo()

  DEFINE  v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
          v_ruta_envio_dis         CHAR(40),
          v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
          v_ch_arch_salida         BASE.CHANNEL,          
          v_comando_dos            STRING,
          v_encabezado             STRING,
          v_detalle                STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 SMALLINT

  DEFINE arr_info_apo DYNAMIC ARRAY OF RECORD
    v_nss     CHAR(11), 
    v_nombre_af  CHAR(30),
    v_ap_paterno_af CHAR(30), 
    v_ap_materno_af CHAR(30), 
    v_periodo CHAR(6), 
    v_aportacion DECIMAL(8,2), 
    v_folio_sua  DECIMAL(6,0), 
    v_f_liquida  DATE, 
    v_f_entrega  DATE, 
    v_interface  CHAR(2), 
    v_tpo_credito CHAR(20)
  END RECORD 

  DEFINE v_aportacion CHAR(10)
  DEFINE v_desc_credito CHAR(20) 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/consulta_acre_no_exist_", v_hora

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  --Imprime encabezado del archivo
  LET v_encabezado = "NSS |NOMBRE |PERIODO |APORTACIÓN |FOLIO SUA |FECHA APLIACIÓN EN SAFRE |FECHA ENTREGA |INTERFACE |TIPO CRÉDITO"
  CALL v_ch_arch_salida.write([v_encabezado])


  LET g_sql_txt = ""

  LET g_sql_txt = "\n   SELECT ad.nss, ",
                  "\n          ad.nombre_af, ",
                  "\n          ad.ap_paterno_af, ", 
                  "\n          ad.ap_materno_af, ",
                  "\n          dc.periodo_pago, ",
                  "\n          dc.imp_ap_pat, ",
                  "\n          dc.folio_sua, ",
                  "\n          dc.f_liquida, ",
                  "\n          TODAY as f_entrega, ",
                  "\n          'AS' as interface, ",
                  "\n          dc.tpo_credito ",
                  "\n     FROM afi_derechohabiente ad, ",
                  "\n          tmp_dis_fac_aps_tns dc ",                 
                  "\n    WHERE dc.id_derechohabiente = ad.id_derechohabiente " 
                
  LET g_sql_txt = g_sql_txt,"\n ORDER BY ad.nss "

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"  
      
  PREPARE pr_sl_inf_arc FROM g_sql_txt
  DECLARE cur_sl_inf_arc CURSOR FOR pr_sl_inf_arc
  
  LET v_indice           = 1
  
  FOREACH cur_sl_inf_arc INTO arr_info_apo[v_indice].v_nss,     
                          arr_info_apo[v_indice].v_nombre_af,
                          arr_info_apo[v_indice].v_ap_paterno_af,
                          arr_info_apo[v_indice].v_ap_materno_af, 
                          arr_info_apo[v_indice].v_periodo, 
                          arr_info_apo[v_indice].v_aportacion,
                          arr_info_apo[v_indice].v_folio_sua,
                          arr_info_apo[v_indice].v_f_liquida,
                          arr_info_apo[v_indice].v_f_entrega,
                          arr_info_apo[v_indice].v_interface,
                          arr_info_apo[v_indice].v_tpo_credito
                         
    SELECT UNIQUE(desc_credito)
             INTO v_desc_credito 
             FROM cat_tipo_credito
            WHERE tpo_credito = arr_info_apo[v_indice].v_tpo_credito

    LET v_aportacion = arr_info_apo[v_indice].v_aportacion * 100 USING "&&&&&&&&&&"

    LET v_detalle =  arr_info_apo[v_indice].v_nss, "|",
                     arr_info_apo[v_indice].v_nombre_af CLIPPED," ", arr_info_apo[v_indice].v_ap_paterno_af CLIPPED," ", arr_info_apo[v_indice].v_ap_materno_af CLIPPED, "|",
                     arr_info_apo[v_indice].v_periodo CLIPPED, "|",
                     --arr_info_apo[v_indice].v_aportacion  USING "###,###,###,###,##&.##", "|",
                     v_aportacion, "|",
                     arr_info_apo[v_indice].v_folio_sua, "|",
                     arr_info_apo[v_indice].v_f_liquida USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_f_entrega USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_interface, "|",
                     arr_info_apo[v_indice].v_tpo_credito CLIPPED, " - ", v_desc_credito CLIPPED

    CALL v_ch_arch_salida.write([v_detalle])
    
    LET v_indice          = v_indice + 1 
  END FOREACH

  FREE cur_sl_inf_arc
  
  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice    = v_indice - 1 

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Aportaciones Subsecuentes Facturar (Acreditados nos existentes)\n en la ruta "||v_ruta_nomarch,"information") 
  
END FUNCTION 


FUNCTION fn_genera_cifras_control()

  DEFINE manejador_rpt om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_rep_indice  INTEGER
  DEFINE v_tot_reporte SMALLINT 
  DEFINE v_indice1     SMALLINT
  DEFINE v_indice2     SMALLINT
  DEFINE v_encabezado  SMALLINT -- 1 detalle, 2 totales por tipo de crédito y concepto

  DEFINE v_entidad_financiera  CHAR(5), 
         v_concepto            CHAR(5), 
         v_estado              CHAR(5), 
         v_tipo_credito        CHAR(5)

  LET g_sql_txt = ""

  -- Consulta del detalle principal
  LET g_sql_txt = "\n   SELECT dc.cve_ent_financiera, ",
                  "\n          'Descripción de Entidad Financiera' AS entidad_financiera, ", 
                  "\n          '08-70058980-4' AS cuenta_bancaria, ", 
                  "\n          '4600000000' AS documento_fico, ", 
                  "\n          dc.concepto, ",
                  "\n          dc.estado, ", 
                  "\n          ce.desc_edo_aps, ", 
                  "\n          SUM(dc.imp_ap_pat) AS monto ",       
                  "\n     FROM tmp_dis_ctr_aps_tns dc, ",                       
                  "\n          OUTER cat_edo_aps ce ",
                  "\n    WHERE dc.estado = ce.cod_edo_aps ",
                  "\n GROUP BY dc.cve_ent_financiera, ",
                  "\n          entidad_financiera, ", 
                  "\n          cuenta_bancaria, ", 
                  "\n          documento_fico, ", 
                  "\n          dc.concepto, ", 
                  "\n          dc.estado, ", 
                  "\n          ce.desc_edo_aps "
                
  DISPLAY "g_sql_txt: -",g_sql_txt,"-"  
      
  PREPARE pr_sl_inf FROM g_sql_txt
  DECLARE cur_sl_inf CURSOR FOR pr_sl_inf
  
  LET v_indice1 = 1
  LET v_tot_det = 0.0   
  
  FOREACH cur_sl_inf INTO v_entidad_financiera,
                          a_fac_apo_sub[v_indice1].entidad_financiera,    
                          a_fac_apo_sub[v_indice1].cuenta_bancaria,
                          a_fac_apo_sub[v_indice1].documento_fico,
                          v_concepto,
                          a_fac_apo_sub[v_indice1].concepto,
                          v_estado,
                          a_fac_apo_sub[v_indice1].estado,
                          a_fac_apo_sub[v_indice1].monto

  LET a_fac_apo_sub[v_indice1].entidad_financiera = v_entidad_financiera CLIPPED,' - ',a_fac_apo_sub[v_indice1].entidad_financiera CLIPPED  
  LET a_fac_apo_sub[v_indice1].concepto = v_concepto CLIPPED,' - ',a_fac_apo_sub[v_indice1].concepto CLIPPED
  LET a_fac_apo_sub[v_indice1].estado = v_estado CLIPPED,' - ',a_fac_apo_sub[v_indice1].estado CLIPPED

  LET v_tot_det =  v_tot_det + a_fac_apo_sub[v_indice1].monto                         

  LET v_indice1 = v_indice1 + 1

  END FOREACH                           
  
  CALL a_fac_apo_sub.deleteElement(v_indice1)
  LET v_indice1    = v_indice1 - 1  

  FREE cur_sl_inf


  ----- Busqueda de Totales
  LET g_sql_txt = ""

  LET g_sql_txt = "\n   SELECT dc.concepto, ",
                  "\n          'Descripción de concepto' AS desc_concepto, ",  
                  "\n          SUM(dc.imp_ap_pat) ", 
                  "\n     FROM dis_ctr_aps_tns dc", 
                  "\n GROUP BY dc.concepto, desc_concepto ",
                  "\N ORDER BY dc.concepto "

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_tot FROM g_sql_txt
  DECLARE cur_sl_tot CURSOR FOR pr_sl_tot
  
  LET v_indice2 = 1
  LET v_concepto = "" 
  LET v_tot_con = 0.0
  
  FOREACH cur_sl_tot INTO v_concepto,
                          a_totales_con[v_indice2].concepto_tot,                          
                          a_totales_con[v_indice2].monto_tot

  LET a_totales_con[v_indice2].concepto_tot = v_concepto CLIPPED,' - ',a_totales_con[v_indice2].concepto_tot CLIPPED  

  LET v_tot_con = v_tot_con + a_totales_con[v_indice2].monto_tot
  
  LET v_indice2 = v_indice2 + 1

  END FOREACH                           
  
  CALL a_totales_con.deleteElement(v_indice2)
  LET v_indice2    = v_indice2 - 1  

  FREE cur_sl_tot

  -- Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISS391.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte
  START REPORT rep_con_disp TO XML HANDLER manejador_rpt

  LET v_tot_reporte = 1
  LET v_encabezado = 1

  FOR v_rep_indice = 1 TO  v_indice1
    OUTPUT TO REPORT rep_con_disp(a_fac_apo_sub[v_rep_indice].*, 
                                  a_totales_con[v_rep_indice].*, 
                                  v_tot_det,  
                                  v_tot_con,
                                  v_encabezado, 
                                  v_tot_reporte, 
                                  v_indice1)
  END FOR

  LET v_tot_reporte = 2
  LET v_encabezado = 2

  FOR v_rep_indice = 1 TO  v_indice2
    OUTPUT TO REPORT rep_con_disp(a_fac_apo_sub[v_rep_indice].*, 
                                  a_totales_con[v_rep_indice].*, 
                                  v_tot_det,  
                                  v_tot_con, 
                                  v_encabezado, 
                                  v_tot_reporte, 
                                  v_indice2)
  END FOR
  
  FINISH REPORT rep_con_disp

END FUNCTION


#Objetivo: Estructura reporte de Factura Aportaciones Subsecuentes
REPORT rep_con_disp(a_apo_sub_det, a_apo_sub_tot, v_t_detalle, v_t_total, 
                    v_encabezado, v_tot_reporte, v_total_registros)

    --Datos de detalle
  DEFINE a_apo_sub_det RECORD    
    entidad_financiera    CHAR(65),
    cuenta_bancaria       CHAR(40),    
    documento_fico 		  CHAR(10),
    concepto			  CHAR(50),
    estado                CHAR(50),
    monto                 DECIMAL (18,6)    
  END RECORD   

  --Totales 
  DEFINE a_apo_sub_tot RECORD    
    concepto_tot          CHAR(50),     -- Concepto
    monto_tot             DECIMAL(18,6) -- Total monto
  END RECORD

  -- Cifras totales de detalle y Totales por tipo de crédito y concepto
  DEFINE v_t_detalle      DECIMAL(18,6),
         v_t_total        DECIMAL(18,6)                  
  
  DEFINE v_fecha_consulta  DATE, -- Fecha de proceso 
         v_tot_reporte     SMALLINT, 
         v_encabezado      SMALLINT, 
         v_total_registros SMALLINT
    
  FORMAT
    FIRST PAGE HEADER      
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      PRINTX g_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

    PAGE HEADER            
      PRINTX g_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

    BEFORE GROUP OF v_tot_reporte
      PRINTX v_total_registros
      PRINTX v_tot_reporte
      PRINTX v_t_detalle USING "###,###,###,###,##&.##"      
      PRINTX v_t_total USING "###,###,###,###,##&.##" 
    
    ON EVERY ROW
      PRINTX a_apo_sub_det.entidad_financiera
      PRINTX a_apo_sub_det.cuenta_bancaria 
      PRINTX a_apo_sub_det.documento_fico 
      PRINTX a_apo_sub_det.concepto 
      PRINTX a_apo_sub_det.estado
      PRINTX a_apo_sub_det.monto USING "###,###,###,###,##&.##"
         
      PRINTX a_apo_sub_tot.concepto_tot
      PRINTX a_apo_sub_tot.monto_tot USING "###,###,###,###,##&.##"

END REPORT