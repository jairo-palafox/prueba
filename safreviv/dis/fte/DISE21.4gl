###############################################################################
#VERSION                    => 1.0.0                                           #
#FECHA ULTIMA MODIFICACION  => 29/06/2016                                      #
#------------------------------------------------------------------------------#
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#MODULO           => DIS                                                       #
#PROGRAMA         => DISE21                                                    #
#OBJETIVO         => Ejecuta la función de la Autorización del Pago            #
#                                                                              #
#FECHA DE INICIO  => 24/06/2015                                                #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_usuario                      VARCHAR(30),                  --Almacena al usuario
    g_proceso_cod                  LIKE cat_proceso.proceso_cod, --codigo de proceso
    g_opera_cod                    LIKE cat_operacion.opera_cod, --codigo de operacion
    g_arch_proceso                 VARCHAR(100),
    g_qrytxt                       STRING                        --Prepara consultas

  DEFINE 
    g_folio_regpag                 DECIMAL(9,0),
    g_folio_pago                   DECIMAL(9,0),
    g_f_actualiza                  DATE,         
    p_pid                          DECIMAL(9,0),
    g_proceso_cod_reg_pago         SMALLINT,
    g_folio_cons                   DECIMAL(9,0)

  DEFINE 
    p_b_despliegue_pantalla        SMALLINT,
    v_ruta_rep                     STRING,
    manejador_rpt                  om.SaxDocumentHandler,
    v_origen_datos                 STRING,
    v_ruta_listados                STRING,
    v_ruta_ejecutable              STRING

  DEFINE p_folio                   DECIMAL(9,0)
  DEFINE v_bandera                 SMALLINT --Para verificar resultado de iniciar la operacion
  DEFINE v_comando                 STRING
  DEFINE l_bat_ruta_listado        CHAR(40)
  DEFINE v_ruta_origen             CHAR(40)
  DEFINE v_desc_salida             VARCHAR(100)
  DEFINE v_mensaje                 STRING
  DEFINE v_folio                   LIKE glo_folio.folio
  DEFINE v_nombre_archivo          LIKE glo_ctr_archivo.nombre_archivo
  DEFINE r_bnd_proceso_cnt         SMALLINT

  DEFINE g_folio_factura           DECIMAL(9,0)  --Folio de factura
  DEFINE g_f_factura_ini           DATE          --Fecha de factura inicial
  DEFINE g_f_factura_fin           DATE          --Fecha de factura final 
  DEFINE g_tipo_credito            SMALLINT      --Tipo de crédito

  --Datos de detalle
  DEFINE a_fac_apo_sub             DYNAMIC ARRAY OF RECORD    
    entidad_financiera             CHAR(65),
    cuenta_bancaria                CHAR(40),    
    documento_fico 		           CHAR(10),
    concepto			           CHAR(50),
    estado                         CHAR(50),
    monto                          DECIMAL(18,6)    
  END RECORD   

  --Totales 
  DEFINE a_totales_con             DYNAMIC ARRAY OF RECORD     
    tpo_credito                    CHAR(50),     --Tipo de crédito    
    concepto_tot                   CHAR(50),     --Concepto
    monto_tot                      DECIMAL(18,6) --Total monto
  END RECORD

  --Cifras totales de detalle y Totales por tipo de crédito y concepto
  DEFINE 
    v_tot_detalle                  DECIMAL(18,6),
    v_tot_totales                  DECIMAL(18,6)    

  DEFINE
    v_tot_registros                DECIMAL(9,0), --Total de registros
    v_tot_aivs                     DECIMAL(18,6),--Total de AIVS
    v_tot_aportacion               DECIMAL(18,2) --Total de aportaciones

  DEFINE v_referencia              CHAR(16)
  DEFINE v_tpo_credito             SMALLINT
  DEFINE v_cve_ent_financiera      SMALLINT
  DEFINE v_clabe                   CHAR(18)
  DEFINE v_cta_contable            CHAR(10)
  DEFINE v_banco_interlocutor      SMALLINT
  DEFINE v_importe                 DECIMAL(22,2)
  
  DEFINE wsstatus                  INTEGER
  DEFINE v_ruta_ejecutable2        CHAR(40)
         
END GLOBALS

MAIN
  LET g_folio_pago            = 0
  LET g_folio_factura         = 0
  INITIALIZE g_f_factura_ini TO NULL
  INITIALIZE g_f_factura_fin TO NULL 
  LET g_tipo_credito          = 0
  INITIALIZE g_f_actualiza   TO NULL
  INITIALIZE g_usuario       TO NULL
  
  LET g_folio_pago    = ARG_VAL(1) --Folio pago 
  LET g_folio_factura = ARG_VAL(2) --Folio factura
  LET g_f_factura_ini = ARG_VAL(3) --Fecha factura inicial
  LET g_f_factura_fin = ARG_VAL(4) --Fecha factura final
  LET g_tipo_credito  = ARG_VAL(5) --Tipo Crédito
  LET g_f_actualiza   = ARG_VAL(6) --Fecha de actualización
  LET g_usuario       = ARG_VAL(7) --Usuario que ejecuta

  INITIALIZE g_f_factura_ini TO NULL
  INITIALIZE g_f_factura_fin TO NULL 

  CALL STARTLOG(g_usuario CLIPPED||".DISE21.log")

  {DISPLAY "g_folio_factura: -",g_folio_factura,"-"
  DISPLAY "g_folio_regpag: -",g_folio_regpag,"-"
  DISPLAY "g_proceso_cod_reg_pago: -",g_proceso_cod_reg_pago,"-" 
  DISPLAY "g_f_actualiza: -",g_f_actualiza,"-"
  DISPLAY "g_usuario: -",g_usuario,"-"}

  --Obtiene las rutas ejecutable
  SELECT ruta_bin
  INTO   v_ruta_origen
  FROM   seg_modulo
  WHERE  modulo_cod = 'ocg'

  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   l_bat_ruta_listado
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --Obtiene la ruta ejecutable de dispersión
  SELECT ruta_bin
  INTO   v_ruta_ejecutable2
  FROM   seg_modulo
  WHERE  modulo_cod = 'dis'

  DISPLAY "Inicia Validaciones para autorizar el pago: " --,TIME
  CALL fn_facturar_apo_sub()

END MAIN

FUNCTION fn_facturar_apo_sub()
  DEFINE 
    bnd_continuar                  SMALLINT,
    v_fecha_01mm                   CHAR(11),
    p_transaccion                  SMALLINT,
    r_bandera                      SMALLINT,          
    v_bnd_confirmacion             SMALLINT,            
    v_QryTxt                       STRING

  DEFINE r_nom_archivo             CHAR(40)
  DEFINE v_bandera                 SMALLINT --Para verificar resultado de iniciar la operacion 
  DEFINE v_nombre_archivo          LIKE glo_ctr_archivo.nombre_archivo
  DEFINE v_status                  SMALLINT
  DEFINE v_error                   CHAR(70)
  DEFINE r_ruta_reporte            STRING
  DEFINE v_bnd_existe_rech         SMALLINT
   
  LET g_proceso_cod = 3914 --Codigo del Proceso de Autorización del Pago
  LET g_opera_cod   = 1    --Codigo de la operacion de preliquidacion

  CALL fn_max_pid(g_proceso_cod,g_opera_cod) RETURNING p_pid
   
  LET p_transaccion  = 0

  --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  SELECT  ind_tipo_ejecucion 
  INTO    p_transaccion
  FROM    bat_ctr_operacion 
  WHERE   proceso_cod = g_proceso_cod   
  AND     pid         = p_pid
  AND     opera_cod   = g_opera_cod
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod,g_usuario)
     RETURNING g_folio_pago
  END IF 

  LET bnd_continuar = 0
   
  --LET p_pid = r_bandera
  LET v_fecha_01mm =  MONTH(TODAY) USING "&&"||"/"||"01"||"/"||YEAR(TODAY)

  IF g_usuario IS NULL THEN 
     LET g_usuario = "SAFREVIV"
  END IF  

  LET bnd_continuar = 1
  --DISPLAY "bnd_continuar: ",bnd_continuar
  IF bnd_continuar THEN
     ------------------------------------------Inicia Operacion 2 (LIQUIDACION)
     LET g_proceso_cod = 3914 -- Autorizar Pago
     LET g_opera_cod   = 1    -- liquidación        
     LET r_nom_archivo = ""

     WHENEVER ERROR CONTINUE  
       PREPARE prp_exe_alta FROM "EXECUTE PROCEDURE sp_dis_fac_apo_subs2(?)"
       EXECUTE prp_exe_alta USING g_folio_factura
       INTO v_status, v_error

       --DISPLAY "------------->>> v_status: ",v_status
       --DISPLAY "------------->>> v_error: ",v_error

       IF v_status = 0 THEN                                            
          LET v_bnd_confirmacion = 0
          DISPLAY "La identificación de las EF a pagar se ha realizado exitosamente."
       ELSE                      
          LET v_bnd_confirmacion = 1
          --CALL fn_mensaje("Aviso","Ocurrió un error inesperado.","stop")
          DISPLAY "Ocurrió un error inesperado en la identificación de las EF a pagar."
       END IF
   
       --DISPLAY "v_bnd_confirmacion: ", v_bnd_confirmacion
       IF v_bnd_confirmacion = 0 THEN      
          WHENEVER ERROR CONTINUE         
             --Actualiza el status del folio de dispersión a Preliquidado 
             LET v_QryTxt = " UPDATE glo_folio",
                            " SET    status      = 1",
                            " WHERE  folio       = ",g_folio_factura,
                            " AND    proceso_cod = ",g_proceso_cod,
                            " AND    opera_cod   = 1"
                     
             PREPARE prep_actualiza_folio_confirmado FROM v_QryTxt
             EXECUTE prep_actualiza_folio_confirmado
      
             LET bnd_continuar = 1
       ELSE
          DISPLAY "Error en la identificación de las EF a pagar: ",v_bnd_confirmacion
          CALL fn_error_opera(p_pid,g_proceso_cod,g_opera_cod)
          RETURNING v_bnd_confirmacion
          EXIT PROGRAM
       END IF

       {LET v_QryTxt = "\n  SELECT UNIQUE referencia ",
                      "\n  FROM   dis_ctr_factura_aps ",
                      "\n  WHERE  folio_factura = ? "}
                      
       LET v_QryTxt = "\n SELECT referencia, clabe, cta_contable, banco_interlocutor, sum(importe) ",
                      "\n FROM   dis_ctr_factura_aps ",
                      "\n WHERE  folio_factura = ? ",
                      "\n GROUP BY 1,2,3,4",
                      "\n ORDER BY 1,4,2,3"

       PREPARE prp_sl_fac FROM v_QryTxt
       DECLARE cur_sl_fac CURSOR FOR prp_sl_fac 
       FOREACH cur_sl_fac USING g_folio_factura INTO v_referencia, 
                                                     v_clabe, 
                                                     v_cta_contable, 
                                                     v_banco_interlocutor, 
                                                     v_importe 

         --DISPLAY "v_referencia: ",v_referencia
                                
         --LET v_comando = "fglrun ",v_ruta_ejecutable2 CLIPPED,"/DISWS02.42r ",v_referencia
         LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISWS02.42r ", g_tipo_credito CLIPPED, " ", 
                                                                          "'", v_referencia CLIPPED, "' ",
                                                                          "'", v_clabe, "' ",
                                                                          "'",v_cta_contable, "' ",
                                                                          v_banco_interlocutor CLIPPED, " ", 
                                                                          v_importe
         --DISPLAY v_comando
         RUN v_comando RETURNING wsstatus
         --DISPLAY "wsstatus disp05: ",wsstatus
                                   
         IF wsstatus != 0  THEN
            --DISPLAY "Error en WS de la autorización del pago ",v_bnd_confirmacion                                      
            CALL fn_error_opera(p_pid, g_proceso_cod, g_opera_cod)
            RETURNING v_bnd_confirmacion
            EXIT PROGRAM
         END IF
       END FOREACH

       CALL fn_act_edo_ef_pagada() --ANALIZAR SI APLICA PARA EL PAGO
    
       CALL fn_actualiza_opera_fin(p_pid, g_proceso_cod, g_opera_cod)
       RETURNING r_bandera  

       LET r_bandera = 0
       --DISPLAY "r_bandera: ", r_bandera
       IF r_bandera = 0 THEN 
          CALL fn_genera_reporte_fact_apo_sub(g_folio_pago,
                                              g_folio_factura,
                                              g_f_factura_ini,    
                                              g_f_factura_fin,    
                                              g_tipo_credito)
          RETURNING r_ruta_reporte

          LET v_bnd_existe_rech = 0
          CALL fn_verifica_fact_rech(g_folio_factura,
                                     g_f_factura_ini,    
                                     g_f_factura_fin,    
                                     g_tipo_credito) 
          RETURNING v_bnd_existe_rech

          IF v_bnd_existe_rech = 1 THEN
             CALL fn_genera_archivo_fact_rech(g_folio_factura,
                                              g_f_factura_ini,    
                                              g_f_factura_fin,    
                                              g_tipo_credito)
          END IF

          --DISPLAY "r_ruta_reporte: ", r_ruta_reporte

          -- Envío de correo de notificación de proceso finalizado
          CALL fn_correo_proceso(p_pid,
                                 g_proceso_cod,
                                 g_opera_cod,
                                 r_ruta_reporte CLIPPED,
                                 "Autorización Pago",
                                 "\n ID Proceso   : "||p_pid||
                                 "\n Proceso      : Autorización Pago"||
                                 "\n Operacion    : Autorización Pago"||
                                 "\n Fecha Inicio : "||TODAY||
                                 "\n Fecha Fin    : "||TODAY)
       ELSE
          CALL fn_error_opera(p_pid, g_proceso_cod, g_opera_cod)
          RETURNING v_bnd_confirmacion
       END IF
  END IF
END FUNCTION

FUNCTION fn_consultar_info_rep_fact()
  DEFINE v_entidad_financiera      CHAR(5), 
         v_concepto                CHAR(5), 
         v_estado                  CHAR(5), 
         v_tipo_credito            CHAR(5)

  DEFINE v_indice1                 INTEGER
  DEFINE v_indice2                 INTEGER
  DEFINE v_ls_query                STRING

  ----- Busqueda de Detalle
  LET v_ls_query = "\n SELECT dc.cve_ent_financiera, ",
                   "\n        ef.ent_financiera_desc, ",
                   "\n        ef.clabe, ", 
                   "\n        fac.num_poliza,             ", 
                   "\n        dc.concepto, ",
                   "\n        co.desc_concepto_ocg, ",
                   "\n        dc.estado, ", 
                   "\n        ce.desc_edo_aps, ", 
                   "\n        SUM(imp_ap_pat) AS monto ",       
                   "\n FROM   dis_ctr_aps_tns dc, ",
                   "\n        OUTER cat_edo_aps ce, ",
                   "\n        OUTER cat_cta_cnt_ocg ef, ",
                   "\n        OUTER cat_concepto_ocg co, ",
                   "\n        OUTER dis_ctr_factura_aps fac                   ",
                   "\n WHERE  dc.estado             = ce.cod_edo_aps          ",                  
                   "\n AND    dc.cve_ent_financiera = ef.cve_ent_financiera   ",
                   "\n AND    dc.tpo_credito        = ef.tpo_credito          ",
                   "\n AND    dc.concepto           = co.cod_concepto_ocg     ",
                   "\n AND    dc.folio_factura      = fac.folio_factura       ",
                   "\n AND    dc.cve_ent_financiera = fac.cve_ent_financiera  ",
                   "\n AND    dc.estado            IN (60,70)                    ",
                   "\n AND    dc.folio_factura      = ", g_folio_factura
                                                       
  LET v_ls_query = v_ls_query,"\n GROUP BY 1,2,3,4,5,6,7,8 ",
                              "\n ORDER BY 5,1,4,3 "
  --DISPLAY "v_ls_query: -",v_ls_query,"-"

  PREPARE pr_sl_inf FROM v_ls_query
  DECLARE cur_sl_inf CURSOR FOR pr_sl_inf
  
  LET v_indice1     = 1
  LET v_tot_detalle = 0.0   
  
  FOREACH cur_sl_inf INTO v_entidad_financiera,
                          a_fac_apo_sub[v_indice1].entidad_financiera,    
                          a_fac_apo_sub[v_indice1].cuenta_bancaria,
                          a_fac_apo_sub[v_indice1].documento_fico,
                          v_concepto,
                          a_fac_apo_sub[v_indice1].concepto,
                          v_estado,
                          a_fac_apo_sub[v_indice1].estado,
                          a_fac_apo_sub[v_indice1].monto

    LET a_fac_apo_sub[v_indice1].entidad_financiera = v_entidad_financiera USING "&&&",' - ',a_fac_apo_sub[v_indice1].entidad_financiera CLIPPED  
    LET a_fac_apo_sub[v_indice1].concepto           = v_concepto CLIPPED,' - ',a_fac_apo_sub[v_indice1].concepto CLIPPED
    LET a_fac_apo_sub[v_indice1].estado             = v_estado CLIPPED,' - ',a_fac_apo_sub[v_indice1].estado CLIPPED

    LET v_tot_detalle                               =  v_tot_detalle + a_fac_apo_sub[v_indice1].monto                         
    LET v_indice1                                   = v_indice1 + 1
  END FOREACH
  
  CALL a_fac_apo_sub.deleteElement(v_indice1)
  
  LET v_indice1    = v_indice1 - 1  

  FREE cur_sl_inf

  ----- Busqueda de Totales
  LET v_ls_query = ""
  LET v_ls_query = "\n SELECT dc.tpo_credito,                           ",
                   "\n        dc.concepto,                              ",
                   "\n        co.desc_concepto_ocg,                     ",
                   "\n        SUM (dc.imp_ap_pat)                       ",
                   "\n FROM   dis_ctr_aps_tns dc,                       ",
                   "\n        OUTER cat_concepto_ocg co                 ",
                   "\n WHERE  dc.concepto       = co.cod_concepto_ocg   ",
                   "\n AND    dc.estado        IN (60,70)                  ",
                   "\n AND    dc.folio_factura  = ", g_folio_factura
  LET v_ls_query = v_ls_query,"\n GROUP BY dc.tpo_credito, dc.concepto, desc_concepto_ocg ",
                              "\n ORDER BY 1,2 "

  --DISPLAY "v_ls_query: -",v_ls_query,"-"

  PREPARE pr_sl_tot FROM v_ls_query
  DECLARE cur_sl_tot CURSOR FOR pr_sl_tot
  
  LET v_indice2     = 1
  LET v_concepto    = "" 
  LET v_tot_totales = 0.0
  
  FOREACH cur_sl_tot INTO v_tipo_credito,                                                    
                          v_concepto,
                          a_totales_con[v_indice2].concepto_tot,                          
                          a_totales_con[v_indice2].monto_tot

    SELECT a.desc_credito_ocg
    INTO   a_totales_con[v_indice2].tpo_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = v_tipo_credito
    AND    a.ind_activo      = 1

    LET a_totales_con[v_indice2].tpo_credito  = v_tipo_credito CLIPPED,' - ',a_totales_con[v_indice2].tpo_credito CLIPPED  
    LET a_totales_con[v_indice2].concepto_tot = v_concepto CLIPPED,' - ',a_totales_con[v_indice2].concepto_tot CLIPPED  
    LET v_tot_totales                         = v_tot_totales + a_totales_con[v_indice2].monto_tot
    LET v_indice2                             = v_indice2 + 1

  END FOREACH                           
  
  CALL a_totales_con.deleteElement(v_indice2)
  LET v_indice2    = v_indice2 - 1  

  FREE cur_sl_tot

  RETURN v_indice1, v_indice2
END FUNCTION  

FUNCTION fn_genera_reporte_fact_apo_sub(p_folio_pago,
                                        p_folio_factura,
                                        p_f_factura_ini,    
                                        p_f_factura_fin,    
                                        p_tipo_credito)

  DEFINE p_folio_pago              DECIMAL(9,0),
         p_folio_factura           DECIMAL(9,0),
         p_f_factura_ini           DATE,    
         p_f_factura_fin           DATE,    
         p_tipo_credito            SMALLINT
                                        
  DEFINE r_folio                   DECIMAL(9,0)
  DEFINE v_tot_nss                 DECIMAL(9,0)
  DEFINE v_tot_conf_no_acr         DECIMAL(9,0)
  DEFINE v_fec_proc                DATE
  DEFINE v_origen_datos            STRING
  DEFINE v_ruta_reporte            STRING
  DEFINE v_ruta_listados           STRING
  DEFINE v_ruta_ejecutable         STRING
  DEFINE manejador_rpt             om.SaxDocumentHandler

  DEFINE arr_confirma              DYNAMIC ARRAY OF RECORD
    v_estado                       SMALLINT,
    v_desc_estado                  CHAR(50),
    v_total_edo                    DECIMAl(9,0)
  END RECORD

  DEFINE v_indice_1                INTEGER
  DEFINE v_ind_rpt                 INTEGER
  DEFINE v_cantidad                STRING

  DEFINE v_ruta_envio_ocg          LIKE seg_modulo.ruta_envio
  DEFINE v_modulo_cod              LIKE seg_modulo.modulo_cod
  DEFINE v_nom_archivo             VARCHAR(40)
  DEFINE v_ddmmaaaa                VARCHAR(08)
  DEFINE v_busca_nom_archivo       STRING

  DEFINE v_rep_indice              INTEGER
  DEFINE v_tot_reporte             INTEGER 
  DEFINE v_indice1                 INTEGER
  DEFINE v_indice2                 INTEGER
  DEFINE v_encabezado              SMALLINT -- 1 detalle, 2 totales por tipo de crédito y concepto

  DEFINE v_desc_credito            CHAR(50)
  
  LET v_fec_proc   = TODAY
  
  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = 'ocg'

  --Se obtiene la ruta envío del módulo
  SELECT a.ruta_envio
  INTO   v_ruta_envio_ocg
  FROM   seg_modulo a
  WHERE  a.modulo_cod = v_modulo_cod

  --DISPLAY "v_ruta_envio_ocg: ", v_ruta_envio_ocg
  --Se crea el nombre dl archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "autoriza_pago_"         --nombre de archivo
   
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy" --fecha del archivo sin separadores
  LET v_busca_nom_archivo = "autoriza_pago_" || v_ddmmaaaa --Concatena nombre a buscar
  
  CALL fn_obtiene_nombre_archivo(v_ruta_envio_ocg, v_busca_nom_archivo)
  RETURNING v_nom_archivo

  --DISPLAY "v_nom_archivo: ", v_nom_archivo

  --Despliega información en el log
  DISPLAY "\n ####### AUTORIZA PAGO ########"

  LET v_origen_datos = g_usuario

  --Se construte la ruta del archivo
  CALL fn_rutas("ocg")
  RETURNING v_ruta_ejecutable, v_ruta_listados

  LET v_indice1 = 0  
  LET v_indice2 = 0 
  INITIALIZE v_ruta_reporte TO NULL
  
  CALL fn_consultar_info_rep_fact()
  RETURNING v_indice1, v_indice2 

  IF v_indice1 >= 1 THEN
     LET v_ruta_reporte = v_ruta_listados.trim(), "/",
                           v_origen_datos.trim(), "-",
                           "DISE21", "-",
                           p_pid USING "&&&&&", "-",
                           g_proceso_cod USING "&&&&&", "-",
                           g_opera_cod USING "&&&&&", ".pdf"

     DISPLAY " Ruta del reporte: ", v_ruta_reporte

     --Se asigna la plantilla para generar el reporte
     IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE211.4rp") THEN
        CALL fgl_report_selectDevice ("PDF")
        CALL fgl_report_selectPreview(0)
        CALL fgl_report_setOutputFileName(v_ruta_reporte)

        LET manejador_rpt = fgl_report_commitCurrentSettings()

        ### Inicia Reporte ###
        START REPORT rep_fact_apo_sub TO XML HANDLER manejador_rpt

          --DISPLAY "v_indice1: ", v_indice1
          --DISPLAY "v_indice2: ", v_indice2

          LET v_tot_reporte = 1
          LET v_encabezado  = 1

          FOR v_rep_indice = 1 TO  v_indice1
              LET v_desc_credito = ""
               
              SELECT a.desc_credito_ocg
              INTO   v_desc_credito 
              FROM   cat_tpo_credito_ocg a
              WHERE  a.tpo_credito_ocg = p_tipo_credito
              AND    a.ind_activo      = 1
       
              LET v_desc_credito = p_tipo_credito CLIPPED, "- ",v_desc_credito
            
              OUTPUT TO REPORT rep_fact_apo_sub(p_folio_pago,
                                                p_folio_factura,
                                                p_f_factura_ini,    
                                                p_f_factura_fin,    
                                                v_desc_credito,
                                                a_fac_apo_sub[v_rep_indice].*, 
                                                a_totales_con[v_rep_indice].*, 
                                                v_tot_detalle,  
                                                v_tot_totales,
                                                v_encabezado, 
                                                v_tot_reporte, 
                                                v_indice1)
          END FOR

          LET v_tot_reporte = 2
          LET v_encabezado  = 2

          FOR v_rep_indice = 1 TO  v_indice2
              OUTPUT TO REPORT rep_fact_apo_sub(p_folio_pago,
                                                p_folio_factura,
                                                p_f_factura_ini,    
                                                p_f_factura_fin,    
                                                v_desc_credito,
                                                a_fac_apo_sub[v_rep_indice].*, 
                                                a_totales_con[v_rep_indice].*, 
                                                v_tot_detalle,  
                                                v_tot_totales, 
                                                v_encabezado, 
                                                v_tot_reporte, 
                                                v_indice1)
          END FOR
           
          FINISH REPORT rep_fact_apo_sub
      ELSE
        DISPLAY "no funciono"
        EXIT PROGRAM      
      END IF

   ELSE
      DISPLAY "No existen registros para generar el reporte de Autorización de Pago."
   END IF
    
RETURN v_ruta_reporte   
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_fact_apo_sub(p_folio_pago,
                        p_folio_factura,
                        p_f_factura_ini,    
                        p_f_factura_fin,    
                        v_desc_credito,
                        a_apo_sub_det, 
                        a_apo_sub_tot, 
                        v_t_detalle, 
                        v_t_total, 
                        v_encabezado, 
                        v_tot_reporte, 
                        v_total_registros)

  DEFINE p_folio_pago              DECIMAL(9,0),
         p_folio_factura           DECIMAL(9,0),
         p_f_factura_ini           DATE,    
         p_f_factura_fin           DATE,    
         v_desc_credito            CHAR(50)
                        
  --Datos de detalle
  DEFINE a_apo_sub_det             RECORD    
    entidad_financiera             CHAR(65),
    cuenta_bancaria                CHAR(40),    
    documento_fico 		           CHAR(10),
    concepto			           CHAR(50),
    estado                         CHAR(50),
    monto                          DECIMAL(18,6)    
  END RECORD   

  --Totales 
  DEFINE a_apo_sub_tot             RECORD     
    tpo_credito                    CHAR(50),     -- Tipo de crédito    
    concepto_tot                   CHAR(50),     -- Concepto
    monto_tot                      DECIMAL(18,6) -- Total monto
  END RECORD

  --Cifras totales de detalle y Totales por tipo de crédito y concepto
  DEFINE v_t_detalle               DECIMAL(18,6),
         v_t_total                 DECIMAL(18,6)                  
  
  DEFINE v_fecha_consulta          DATE, -- Fecha de proceso 
         v_tot_reporte             INTEGER, 
         v_encabezado              SMALLINT, 
         v_total_registros         SMALLINT
    
  FORMAT
    FIRST PAGE HEADER      
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      PRINTX g_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

      PRINTX p_folio_pago
      PRINTX p_folio_factura
      PRINTX p_f_factura_ini USING "dd-mm-yyyy"    
      PRINTX p_f_factura_fin USING "dd-mm-yyyy"    
      PRINTX v_desc_credito

    PAGE HEADER  
      LET v_fecha_consulta = TODAY    
      PRINTX g_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

      PRINTX p_folio_pago
      PRINTX p_folio_factura
      PRINTX p_f_factura_ini USING "dd-mm-yyyy"    
      PRINTX p_f_factura_fin USING "dd-mm-yyyy"    
      PRINTX v_desc_credito

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
   
      PRINTX a_apo_sub_tot.tpo_credito
      PRINTX a_apo_sub_tot.concepto_tot
      PRINTX a_apo_sub_tot.monto_tot USING "###,###,###,###,##&.##"

END REPORT

FUNCTION fn_obtiene_nombre_archivo(p_ruta_envio_ocg, p_busca_nom_archivo)
  DEFINE
    p_ruta_envio_ocg               LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo            VARCHAR(40), --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                          STRING,      --Cadena de comando a ejecutar
    v_consecutivo                  INTEGER,     --Consecutivo del archivo por día
    v_nom_archivo_sal              VARCHAR(40)  --Nombre del archivo de salida

  DEFINE fn                        CHAR(28)     --Almacena el nombre completo del nombre del archivo en el servidor con su extensión
  DEFINE ch                        base.Channel --Canal de lectura

  DEFINE
    v_reg_dia                      CHAR(03),   --Parametro consecutivo de registro por dia
    v_modulo_cod                   LIKE seg_modulo.modulo_cod,
    v_ddmmaaaa                     VARCHAR(08) --fecha del archivo de salida

  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = "ocg"

  LET v_cmd = "ls -lrt ",p_ruta_envio_ocg CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"
  LET ch    = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo     = fn[23,25] --Posición del consecutivo dentro de la cadena
    LET v_nom_archivo_sal = fn
  END WHILE

  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1 --Incrementa consecutivo del día

  IF LENGTH(v_consecutivo) = 0 THEN --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  LET v_nom_archivo_sal = "autoriza_pago_"          --nombre de archivo
  LET v_ddmmaaaa        = TODAY USING "ddmmyyyy"    --Fecha del archivo sin separadores
  LET v_reg_dia         = v_consecutivo USING "&&&" --Consecutivo del día de numerico a char
  LET v_nom_archivo_sal = v_nom_archivo_sal CLIPPED || v_ddmmaaaa CLIPPED|| v_reg_dia CLIPPED||"."|| v_modulo_cod

  RETURN v_nom_archivo_sal --Regresa el nombre del archivo del día

END FUNCTION

FUNCTION fn_verifica_fact_rech(p_folio_factura,
                               p_f_factura_ini,    
                               p_f_factura_fin,    
                               p_tipo_credito)
                                     
  DEFINE p_folio_factura           DECIMAL(9,0), --Folio de transacción
         p_f_factura_ini           DATE,    
         p_f_factura_fin           DATE,    
         p_tipo_credito            SMALLINT

  DEFINE v_bnd_existe_fact_rech    INTEGER
  DEFINE v_ls_query                STRING
  
  LET v_bnd_existe_fact_rech = 0

  LET v_ls_query = ""
  LET v_ls_query = "\n SELECT COUNT(*) ",
                   "\n FROM   dis_ctr_factura_aps fac, ",
                   "\n        dis_ctr_edo_factura_aps edo  ",
                   "\n WHERE  fac.referencia = edo.referencia ",
                   "\n AND    edo.tpo_estado = 1 ",
                   "\n AND    edo.estado    <> 1 " 
                  
  IF p_folio_factura IS NOT NULL THEN          
     LET v_ls_query = v_ls_query,"\n AND fac.folio_factura = ",p_folio_factura
  END IF

  IF p_f_factura_ini IS NOT NULL AND p_f_factura_fin IS NOT NULL THEN 
     LET v_ls_query = v_ls_query,"\n AND fac.f_factura >= '",p_f_factura_ini,"'",
                                 "\n AND fac.f_factura <= '",p_f_factura_fin,"'"
  END IF  

  IF p_tipo_credito IS NOT NULL THEN 
     LET v_ls_query = v_ls_query,"\n AND fac.tpo_credito = ",p_tipo_credito  
  END IF

  --LET v_ls_query = v_ls_query,"\n  ORDER BY ad.nss "
                  
  --DISPLAY v_ls_query
  PREPARE ps_count_fact_rech FROM v_ls_query
  EXECUTE ps_count_fact_rech INTO v_bnd_existe_fact_rech

  IF v_bnd_existe_fact_rech >= 1 THEN
      LET v_bnd_existe_fact_rech = 1
  ELSE
      LET v_bnd_existe_fact_rech = 0
  END IF

RETURN v_bnd_existe_fact_rech
END FUNCTION

FUNCTION fn_genera_archivo_fact_rech(p_folio_factura,
                                     p_f_factura_ini,    
                                     p_f_factura_fin,    
                                     p_tipo_credito)
                                     
  DEFINE p_folio_factura           DECIMAL(9,0), --Folio de factura
         p_f_factura_ini           DATE,    
         p_f_factura_fin           DATE,    
         p_tipo_credito            SMALLINT

  DEFINE
    v_nom_archivo                  VARCHAR(50),  --nombre del archivo de salida
    v_ruta_envio_dis               CHAR(40),
    v_ruta_nomarch                 VARCHAR(120), --ruta y nombre del archivo de salida
    v_ch_arch_salida               BASE.CHANNEL,
    v_comando_dos                  STRING,
    v_encabezado                   STRING,
    v_titulos                      STRING,
    v_detalle                      STRING,
    v_sumario                      STRING
    
  DEFINE
    v_fecha_archivo                DATE,
    v_hora_archivo                 DATETIME HOUR TO HOUR ,
    v_min_archivo                  DATETIME MINUTE TO MINUTE,
    v_sec_archivo                  DATETIME SECOND TO SECOND,
    v_hora                         STRING,
    v_indice                       INTEGER

  DEFINE arr_info_apo              DYNAMIC ARRAY OF RECORD
    v_folio_factura                DECIMAL(9,0),
    v_f_factura                    DATE,
    v_tpo_credito                  CHAR(20),
    v_cve_ent_financiera           SMALLINT,
    v_clabe                        CHAR(18),
    v_referencia                   CHAR(16),
    v_importe                      DECIMAL(18,2),
    v_estado                       SMALLINT,
    v_f_entrega                    DATE,
    v_interface                    CHAR(2)
  END RECORD

  DEFINE v_desc_ent_financiera     CHAR(65)
  DEFINE v_desc_estado_pago        CHAR(40)
  DEFINE v_desc_credito            CHAR(20) 

  DEFINE v_aportacion              CHAR(10)

  DEFINE v_ls_query                STRING

  LET v_fecha_archivo = TODAY
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND

  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo CLIPPED, v_min_archivo CLIPPED, v_sec_archivo CLIPPED,".ocg"
  LET v_nom_archivo   = "/auto_pag_rech_", v_hora

  --DISPLAY "v_hora: ", v_hora
  --DISPLAY "v_nom_archivo: ",v_nom_archivo
  
  --se obtienen la ruta envio del módulo
  SELECT ruta_envio
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED

  DISPLAY " Ruta Archivo: ", v_ruta_nomarch

  --se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  LET v_ls_query = ""
  LET v_ls_query = "\n SELECT pag.folio_factura, ",
                   "\n        pag.f_factura, ",
                   "\n        pag.tpo_credito, ",
                   "\n        pag.cve_ent_financiera, ",
                   "\n        pag.clabe, ",
                   "\n        pag.referencia, ",
                   "\n        pag.importe, ",
                   "\n        edo.estado, ",
                   "\n        TODAY as f_entrega, ",
                   "\n        'AS' as interface ",
                   "\n FROM   dis_ctr_factura_aps pag, ",
                   "\n        dis_ctr_edo_factura_aps edo ",
                   "\n WHERE  fac.referencia = edo.referencia ",
                   "\n AND    edo.tpo_estado = 1 ",
                   "\n AND    edo.estado    <> 1 "
                  
  IF p_folio_factura IS NOT NULL THEN          
     LET v_ls_query   = v_ls_query,"\n AND pag.folio_factura = ",p_folio_factura
     LET v_encabezado = "FOLIO FACTURA: ",p_folio_factura
     CALL v_ch_arch_salida.write([v_encabezado]) 
  END IF

  {IF p_f_factura_ini IS NOT NULL AND p_f_factura_fin IS NOT NULL THEN 
     LET v_ls_query   = v_ls_query,"\n AND pag.f_factura >= '",p_f_factura_ini,"'",
                                   "\n AND pag.f_factura <= '",p_f_factura_fin,"'"
     LET v_encabezado = "PERIODO FECHAS: ",p_f_factura_ini USING "dd-mm-yyyy", " - ", p_f_factura_fin USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF}  

  IF p_tipo_credito IS NOT NULL THEN 
     LET v_ls_query     = v_ls_query, "\n AND pag.tpo_credito = ",p_tipo_credito  
     LET v_desc_credito = ""
      
     SELECT a.desc_credito_ocg
     INTO   v_desc_credito 
     FROM   cat_tpo_credito_ocg a
     WHERE  a.tpo_credito_ocg = p_tipo_credito
     AND    a.ind_activo      = 1

     LET v_encabezado = "TIPO CREDITO: ",p_tipo_credito CLIPPED ," - ", v_desc_credito
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  LET v_ls_query = v_ls_query,"\n  ORDER BY pag.folio_factura, pag.tpo_credito, pag.cve_ent_financiera, pag.referencia, pag.clabe "
                  
  --DISPLAY v_ls_query
  PREPARE pr_sl_inf_arc FROM v_ls_query
  DECLARE cur_sl_inf_arc CURSOR FOR pr_sl_inf_arc

  LET v_indice         = 1
  LET v_tot_aivs       = 0
  LET v_tot_aportacion = 0

  --Imprime encabezado del archivo
  LET v_titulos = "FOLIO FACTURA |FECHA FACTURA |TIPO CRÉDITO |ENTIDAD FINANCIERA |CLABE |REFERENCIA |IMPORTE |ESTADO |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])

  FOREACH cur_sl_inf_arc INTO arr_info_apo[v_indice].v_folio_factura,
                              arr_info_apo[v_indice].v_f_factura,
                              arr_info_apo[v_indice].v_tpo_credito,
                              arr_info_apo[v_indice].v_cve_ent_financiera,
                              arr_info_apo[v_indice].v_clabe,
                              arr_info_apo[v_indice].v_referencia,
                              arr_info_apo[v_indice].v_importe,
                              arr_info_apo[v_indice].v_estado,
                              arr_info_apo[v_indice].v_f_entrega,
                              arr_info_apo[v_indice].v_interface

    SELECT a.desc_credito_ocg
    INTO   v_desc_credito
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = arr_info_apo[v_indice].v_tpo_credito
    AND    a.ind_activo      = 1

    IF arr_info_apo[v_indice].v_tpo_credito = 3 THEN
       LET arr_info_apo[v_indice].v_interface = 'UG'
    END IF

    SELECT a.ent_financiera_desc
    INTO   v_desc_ent_financiera
    FROM   cat_cta_cnt_ocg a
    WHERE  a.cve_ent_financiera = arr_info_apo[v_indice].v_cve_ent_financiera
    
    SELECT ed.desc_edo_fac_aps
    INTO   v_desc_estado_pago
    FROM   cat_edo_factura_aps ed
    WHERE  ed.cod_edo_fac_aps = arr_info_apo[v_indice].v_estado

    LET v_detalle =  arr_info_apo[v_indice].v_folio_factura, "|",
                     arr_info_apo[v_indice].v_f_factura USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_tpo_credito CLIPPED, "-", v_desc_credito CLIPPED, "|",
                     arr_info_apo[v_indice].v_cve_ent_financiera USING "&&&", "-", v_desc_ent_financiera CLIPPED, "|",
                     arr_info_apo[v_indice].v_clabe, "|",
                     arr_info_apo[v_indice].v_referencia, "|",
                     arr_info_apo[v_indice].v_importe USING "###,###,##&.##", "|",
                     arr_info_apo[v_indice].v_estado CLIPPED, "-", v_desc_estado_pago CLIPPED, "|",                    
                     arr_info_apo[v_indice].v_f_entrega USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_interface, "|"

    CALL v_ch_arch_salida.write([v_detalle])

    LET v_tot_aportacion = v_tot_aportacion + arr_info_apo[v_indice].v_importe
    LET v_indice         = v_indice + 1
  END FOREACH

  FREE cur_sl_inf_arc

  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice    = v_indice - 1

  LET v_tot_registros =  v_indice

  LET v_sumario = "TOTALES: | ",v_tot_registros," | | | | |",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Rechazos (Autorizar Pago) \n en la ruta "||v_ruta_nomarch,"information")

END FUNCTION

FUNCTION fn_act_edo_ef_pagada()
  DEFINE v_entidad_financiera      SMALLINT, 
         v_tipo_credito            SMALLINT,
         v_f_factura               DATE

  DEFINE v_indice2                 INTEGER
  DEFINE v_ls_query                STRING

  DEFINE v_id_ctr_transaccion      DECIMAL(9,0)
  DEFINE v_id_dis_interface_ef     DECIMAL(9,0)
  DEFINE v_folio_liquida           DECIMAL(9,0)

  ----- Busqueda de Totales
  LET v_ls_query = ""
  LET v_ls_query = "\n SELECT df.cve_ent_financiera,                    ",
                   "\n        df.tpo_credito,                           ",
                   "\n        df.f_factura                              ",
                   "\n FROM   dis_ctr_factura_aps df                    ",
                   "\n WHERE  df.folio_factura   = ", g_folio_factura,
                   "\n AND    df.num_poliza     <> 0                    "

  --DISPLAY "v_ls_query: -",v_ls_query,"-"

  PREPARE pr_act_pag FROM v_ls_query
  DECLARE cur_act_pag CURSOR FOR pr_act_pag
  
  LET v_indice2     = 1
  
  FOREACH cur_act_pag INTO v_entidad_financiera,
                           v_tipo_credito,
                           v_f_factura

    UPDATE dis_ctr_aps_tns
    SET    estado             = 60
    WHERE  folio_factura      = g_folio_factura
    AND    f_factura          = v_f_factura
    AND    cve_ent_financiera = v_entidad_financiera
    AND    tpo_credito        = v_tipo_credito

  END FOREACH                           
  
  FREE cur_act_pag

  
  ----- Actualiza ocg_ctr_transaccion
  {LET v_ls_query = ""
  LET v_ls_query = "\n SELECT df.id_ctr_transaccion,                   ",
                   "\n        df.id_dis_interface_ef,                  ",
                   "\n        df.folio_liquida                         ",
                   "\n FROM   dis_ctr_aps_tns df                       ",
                   "\n WHERE  df.folio_factura = ", g_folio_factura,
                   "\n AND    df.estado        = 60                    "

  --DISPLAY "v_ls_query: -",v_ls_query,"-"

  PREPARE pr_act_ocg_tran FROM v_ls_query
  DECLARE cur_ocg_tran CURSOR FOR pr_act_ocg_tran
  
  LET v_indice2     = 1
  
  FOREACH cur_act_pag INTO v_id_ctr_transaccion,
                           v_id_dis_interface_ef,
                           v_folio_liquida

    UPDATE ocg_ctr_transaccion
    SET    estado                 = 60
    WHERE  id_ocg_ctr_transaccion = v_id_ctr_transaccion
    AND    id_referencia_cta      = v_id_dis_interface_ef
    AND    folio_referencia       = v_folio_liquida

  END FOREACH                           
  
  FREE cur_ocg_tran}
  
END FUNCTION