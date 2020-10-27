################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 10/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC26                                                    #
#Objetivo         => Programa de consulta de Transacciones por NSS             #
#Fecha de Inicio  => 05/09/2016                                                #
################################################################################
--Base que se utilizará
DATABASE safre_viv

--Definición de variables globales
GLOBALS
DEFINE g_sql_txt             STRING,      --Consultas
       g_usuario             VARCHAR(30), --Almacena al usuario
       g_tipo_proceso        SMALLINT,    --Forma como ejecutara el programa
       g_nom_prog            VARCHAR(30)  --Almacena opción del menú
 
--Datos de salida
DEFINE g_nss	             CHAR(11)
DEFINE g_transaccion         VARCHAR(50)

DEFINE a_datos_apo_sub_tran  DYNAMIC ARRAY OF RECORD
  nss                        CHAR(11),
  nombre                     VARCHAR(100),
  periodo_pago               CHAR(06),
  f_pago                     DATE,
  folio_sua                  DECIMAL(6,0),
  nrp                        CHAR(11),
  aiv_ap_pat                 DECIMAL(18,6),
  imp_ap_pat                 DECIMAL(12,2),
  ent_financiera             VARCHAR(50),
  tpo_credito                VARCHAR(50),	
  concepto                   VARCHAR(50),
  estado                     VARCHAR(50)	            
END RECORD
  
--Totales 
DEFINE 
  v_tot_registros            DECIMAL(9,0),  --Total de registros
  v_tot_aivs                 DECIMAL(18,6), --Total de AIVS
  v_tot_aportacion           DECIMAL(12,2)  --Total de aportaciones
         
DEFINE v_indice              DECIMAL(9,0)

DEFINE
  g_sql_txt                  STRING,
  v_proc_entra               SMALLINT,
  v_proc_val                 SMALLINT,
  v_cod_conv                 SMALLINT,
  v_desc_proc_val            CHAR(40),
  v_mensaje_val              STRING,
  p_proceso_cod              SMALLINT
  
END GLOBALS

MAIN     
DEFINE v_cb_transaccion      SMALLINT    
DEFINE v_ind_llena_cb        SMALLINT
DEFINE v_nss                 CHAR(11)
DEFINE v_folio_transaccion   DECIMAL(9,0)   --Folio de transacción
DEFINE v_f_transaccion_ini   DATE           --Fecha de transacción inicial
DEFINE v_f_transaccion_fin   DATE           --Fecha de transacción final 
DEFINE v_folio_facturacion   DECIMAL(9,0)   --Folio de facturación, 
DEFINE v_f_facturacion_ini   DATE           --Fecha de facturacion inicial
DEFINE v_f_facturacion_fin   DATE           --Fecha de facturacion final

DEFINE bnd_consulta          SMALLINT, 
       f_ventana             ui.Window, --Define las propiedades de la Ventana
       f_forma               ui.Form    --Define las propiedades de la forma

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  LET p_proceso_cod  = 3904

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  LET bnd_consulta    = 0
  LET v_nss           = 0
  LET v_ind_llena_cb  = 0

  INITIALIZE g_transaccion, g_nss TO NULL

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISC36"
    DIALOG ATTRIBUTES(UNBUFFERED)    
      INPUT BY NAME v_cb_transaccion, v_nss,
                    v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, 
                    v_folio_facturacion, v_f_facturacion_ini, v_f_facturacion_fin 
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          --DISPLAY "fn_llena_combo_transaccion()"
          CALL fn_llena_combo_transaccion(NULL)
          CALL f_forma.setElementHidden("gr_detalle", 1) --Oculta detalle de la consulta
          NEXT FIELD v_cb_transaccion
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           IF v_cb_transaccion IS NULL THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe ingresar la Transacción a consultar.",
                              "about")
               NEXT FIELD v_cb_transaccion
           END IF

           IF v_f_transaccion_ini > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La fecha de Transacción Inicial no puede ser mayor a la Fecha Actual.",
                              "about")
              NEXT FIELD v_f_transaccion_ini
           END IF    

           IF v_f_transaccion_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Transacción Final no puede ser mayor a la Fecha Actual.",
                              "about")
              NEXT FIELD v_f_transaccion_fin
           END IF 
             
           IF v_f_transaccion_ini IS NOT NULL AND v_f_transaccion_fin IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Transacción Final.",
                              "about")
              NEXT FIELD v_f_transaccion_fin
           END IF 

           IF v_f_transaccion_fin IS NOT NULL AND v_f_transaccion_ini IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Transacción Inicial.",
                              "about")
              NEXT FIELD v_f_transaccion_ini
           END IF 

           IF (v_f_transaccion_ini > v_f_transaccion_fin) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Inicial de Transacción no puede ser mayor a la Fecha Final de Transacción.",
                              "about")
              NEXT FIELD v_f_transaccion_ini 
           END IF

           IF (v_f_transaccion_fin < v_f_transaccion_ini) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Final de Transacción no puede ser menor a la Fecha Inicial de Transacción.",
                              "about")
              NEXT FIELD v_f_transaccion_fin 
           END IF       

           IF v_f_facturacion_ini > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Facturación Inicial no puede ser mayor a la Fecha Actual.",
                              "about")
              NEXT FIELD v_f_facturacion_ini
           END IF    

           IF v_f_facturacion_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Facturación Final no puede ser mayor a la Fecha Actual.",
                              "about")
              NEXT FIELD v_f_facturacion_fin
           END IF   

           IF v_f_facturacion_ini IS NOT NULL AND v_f_facturacion_fin IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Facturación Final.",
                              "about")
              NEXT FIELD v_f_facturacion_fin
           END IF 

           IF v_f_facturacion_fin IS NOT NULL AND v_f_facturacion_ini IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Facturación Inicial.",
                              "about")
              NEXT FIELD v_f_facturacion_fin
           END IF 

           IF (v_f_facturacion_ini > v_f_facturacion_fin) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Inicial de Facturación no puede ser mayor a la Fecha Final de Facturación.",
                              "about")
              NEXT FIELD v_f_facturacion_ini 
           END IF

           IF (v_f_facturacion_fin < v_f_facturacion_ini) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Final de Facturación no puede ser menor a la Fecha Inicial de Facturación.",
                              "about")
              NEXT FIELD v_f_facturacion_fin 
           END IF       
                    
           --DISPLAY "v_cb_transaccion  : ", v_cb_transaccion
           --DISPLAY "v_nss             : ", v_nss
           DISPLAY "fn_consultar()"
           CALL fn_consultar(v_cb_transaccion, v_nss,
                             v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, 
                             v_folio_facturacion, v_f_facturacion_ini, v_f_facturacion_fin)  

           IF v_indice > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0) --Muestra detalle de la consulta
              --CALL DIALOG.setActionHidden("reporte", 0)
              --CALL DIALOG.setActionHidden("archivo", 0)
            
              DISPLAY ARRAY a_datos_apo_sub_tran TO rec_detalle.*
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE DISPLAY      
                  --DISPLAY v_nss            TO v_nss1
                  --DISPLAY v_nombre         TO v_nombre
                  --DISPLAY v_curp           TO v_curp
            
                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)                                  
                  CALL ui.interface.refresh()

                ON ACTION reporte
                   CALL fn_reporte()
                   
                ON ACTION archivo
                   CALL fn_genera_archivo_transacciones_nss(g_transaccion,g_nss) 

                ON ACTION cancelar 
                    EXIT PROGRAM 
              END DISPLAY 
           ELSE
              CALL f_forma.setElementHidden("gr_detalle", 1)
              --CALL DIALOG.setActionHidden("reporte", 1)
              --CALL DIALOG.setActionHidden("archivo", 1)
               
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros.",
                              "about")
              CALL ui.interface.refresh()
           END IF
      END INPUT

      ON ACTION cancelar
         EXIT DIALOG      

    END DIALOG 
  CLOSE WINDOW w1 
END MAIN 

FUNCTION fn_consultar(p_cod_transaccion, p_nss,
                      p_folio_transaccion, p_f_transaccion_ini, p_f_transaccion_fin, 
                      p_folio_facturacion, p_f_facturacion_ini, p_f_facturacion_fin)
                      
DEFINE p_cod_transaccion     SMALLINT
DEFINE p_nss                 CHAR(11)    --Folio de liquidación de la dispersión 

DEFINE p_folio_transaccion   DECIMAL(9,0), --Folio de transacción
       p_f_transaccion_ini   DATE,         --Fecha de transacción inicial
       p_f_transaccion_fin   DATE,         --Fecha de transacción final 
       p_folio_facturacion   DECIMAL(9,0), --Folio de facturación 
       p_f_facturacion_ini   DATE,         --Fecha de facturacion inicial
       p_f_facturacion_fin   DATE          --Fecha de facturacion final
         
DEFINE v_desc_concepto       VARCHAR(50)
DEFINE v_desc_ent_financiera VARCHAR(50)
DEFINE v_desc_credito        VARCHAR(50)
DEFINE v_desc_estado         VARCHAR(50)

DEFINE v_concepto            SMALLINT
DEFINE v_ent_financiera      SMALLINT
DEFINE v_tpo_credito         SMALLINT 
DEFINE v_estado              SMALLINT

DEFINE v_f_liquida           DATE,
       v_f_transaccion       DATE,
       v_f_factura           DATE

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_afi_disc36;
  WHENEVER ERROR STOP

  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT H.id_dis_interface_ef, ",
                  "\n        A.id_derechohabiente,  ",
                  "\n        A.nss,                 ",
                  "\n        A.nombre_af,           ",
                  "\n        A.ap_paterno_af,       ",
                  "\n        A.ap_materno_af        ",
                  "\n FROM   dis_ctr_aps_tns H,     ",
                  "\n        afi_derechohabiente A  ",
                  "\n WHERE  H.id_derechohabiente = A.id_derechohabiente "

  IF p_cod_transaccion IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt, "\n AND H.concepto = ", p_cod_transaccion
  END IF
  
  IF p_nss IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND A.nss = '", p_nss, "'"
  END IF

  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND H.folio_transaccion = ", p_folio_transaccion
  END IF

  IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND H.f_transaccion >= '", p_f_transaccion_ini, "'",
                                "\n AND H.f_transaccion <= '", p_f_transaccion_fin, "'"
  END IF  

  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND H.folio_factura = ", p_folio_facturacion
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND H.f_factura >= '", p_f_facturacion_ini, "'",
                                "\n AND H.f_factura <= '", p_f_facturacion_fin, "'"
  END IF

  LET g_sql_txt = g_sql_txt, "\n INTO TEMP tmp_afi_disc36 "

  PREPARE prep_tmp_afi FROM g_sql_txt
  EXECUTE prep_tmp_afi

  UPDATE STATISTICS FOR TABLE tmp_afi_disc36

  LET g_sql_txt = ""       
  LET g_sql_txt = "\n SELECT afi.nss, ",
                  "\n        TRIM(afi.nombre_af)||' '|| ",
                  "\n        TRIM(afi.ap_paterno_af)||' '|| ",
                  "\n        TRIM(afi.ap_materno_af) , ",
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.aiv_ap_pat, ",
                  "\n        dca.imp_ap_pat, ",
                  "\n        dca.cve_ent_financiera, ",
                  "\n        dca.tpo_credito, ",
                  "\n        dca.concepto,",
                  "\n        dca.estado ",
                  --"\n FROM   afi_derechohabiente afi, ",
                  --"\n        dis_ctr_aps_tns dca ",
                  --"\n WHERE afi.id_derechohabiente = dca.id_derechohabiente "
                  "\n FROM   dis_ctr_aps_tns dca, ",
                  "\n        tmp_afi_disc36 afi ",
                  "\n WHERE  dca.id_dis_interface_ef = afi.id_dis_interface_ef ",
                  "\n AND    dca.id_derechohabiente  = afi.id_derechohabiente "
                  
  INITIALIZE  v_f_liquida, v_f_transaccion, v_f_factura TO NULL

  IF p_cod_transaccion IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND dca.concepto = ",p_cod_transaccion
  END IF
  
  IF p_nss IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND afi.nss = '",p_nss,"'"
  END IF

  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dca.folio_transaccion = ",p_folio_transaccion
  END IF

  IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dca.f_transaccion >= '",p_f_transaccion_ini,"'",
                               "\n AND dca.f_transaccion <= '",p_f_transaccion_fin,"'"
  END IF  

  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dca.folio_factura = ",p_folio_facturacion
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dca.f_factura >= '",p_f_facturacion_ini,"'",
                               "\n AND dca.f_factura <= '",p_f_facturacion_fin,"'"
  END IF  

  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.tpo_credito, dca.concepto, dca.periodo_pago DESC, dca.f_pago DESC, dca.cve_ent_financiera, afi.nss, dca.estado "
                
  DISPLAY "g_sql_txt: - ",g_sql_txt,"-"

  PREPARE ps_transaccion_nss FROM g_sql_txt
  DECLARE cur_transaccion_nss CURSOR FOR ps_transaccion_nss

  LET v_indice         = 1
  LET v_tot_registros  = 0
  LET v_tot_aivs       = 0.00
  LET v_tot_aportacion = 0.00

  LET v_concepto       = 0
  LET v_ent_financiera = 0
  LET v_tpo_credito    = 0
  LET v_estado         = 0

  INITIALIZE v_desc_concepto       TO NULL
  INITIALIZE v_desc_ent_financiera TO NULL
  INITIALIZE v_desc_credito        TO NULL
  INITIALIZE v_desc_estado         TO NULL
  
  FOREACH cur_transaccion_nss INTO a_datos_apo_sub_tran[v_indice].nss,
                                   a_datos_apo_sub_tran[v_indice].nombre,
                                   a_datos_apo_sub_tran[v_indice].periodo_pago,
                                   a_datos_apo_sub_tran[v_indice].f_pago,	     
                                   a_datos_apo_sub_tran[v_indice].folio_sua,	        
                                   a_datos_apo_sub_tran[v_indice].nrp,
                                   a_datos_apo_sub_tran[v_indice].aiv_ap_pat,                
                                   a_datos_apo_sub_tran[v_indice].imp_ap_pat,
                                   v_ent_financiera,         --ent_financiera	      
                                   v_tpo_credito,            --tpo_credito	         	
                                   v_concepto,               --concepto	           
                                   v_estado                  --estado                                                      
                                     
    SELECT a.desc_credito_ocg ---Tipo de Crédito
    INTO   v_desc_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = v_tpo_credito
    AND    a.ind_activo      = 1

    SELECT UNIQUE(desc_edo_aps) ---Estado
    INTO   v_desc_estado 
    FROM   cat_edo_aps
    WHERE  cod_edo_aps = v_estado

    SELECT UNIQUE(ent_financiera_desc)
    INTO   v_desc_ent_financiera
    FROM   cat_cta_cnt_ocg
    WHERE  cve_ent_financiera = v_ent_financiera
    AND    tpo_credito        = v_tpo_credito 

    SELECT UNIQUE(co.desc_concepto_ocg)
    INTO   v_desc_concepto
    FROM   cat_concepto_ocg co
    WHERE  co.cod_concepto_ocg = v_concepto

    LET a_datos_apo_sub_tran[v_indice].concepto       = v_concepto, " - ", v_desc_concepto
    LET a_datos_apo_sub_tran[v_indice].ent_financiera = v_ent_financiera USING "&&&", " - ", v_desc_ent_financiera
    LET a_datos_apo_sub_tran[v_indice].tpo_credito    = v_tpo_credito, " - ", v_desc_credito  
    LET a_datos_apo_sub_tran[v_indice].estado         = v_estado, " - ",v_desc_estado  

    IF v_indice = 1 THEN
       IF p_nss IS NOT NULL THEN
          LET g_nss = p_nss
       END IF
        
       LET g_transaccion = a_datos_apo_sub_tran[v_indice].concepto
    END IF
      
    --LET v_tot_registros  = v_tot_registros    + a_datos_apo_sub_tran[v_indice].tot_registros         
    LET v_tot_aivs       = v_tot_aivs         + a_datos_apo_sub_tran[v_indice].aiv_ap_pat
    LET v_tot_aportacion = v_tot_aportacion   + a_datos_apo_sub_tran[v_indice].imp_ap_pat     
    LET v_indice         = v_indice           + 1 
  END FOREACH

  FREE cur_transaccion_nss
  
  CALL a_datos_apo_sub_tran.deleteElement(v_indice)
  LET v_indice        = v_indice - 1 
  LET v_tot_registros = v_indice 
END FUNCTION  

FUNCTION fn_reporte()
DEFINE manejador_rpt         om.SaxDocumentHandler --Contenedor documentos reporte
DEFINE v_rep_indice          DECIMAL(9,0)
  
  --Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISC361.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte
  START REPORT rep_transacciones_por_nss TO XML HANDLER manejador_rpt
    OUTPUT TO REPORT rep_transacciones_por_nss(g_nss,
                                               g_transaccion,
                                               v_tot_registros,
                                               v_tot_aivs,
                                               v_tot_aportacion,
                                               g_usuario)
  FINISH REPORT rep_transacciones_por_nss
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_transacciones_por_nss(v_nss, 
                                 v_transaccion,
                                 v_rep_tot_registros, 
                                 v_rep_sum_aivs, 
                                 v_rep_sum_aportacion, 
                                 v_usuario)                      

DEFINE 
  v_nss                      CHAR(11),
  v_transaccion              VARCHAR(50)

DEFINE 
  v_fecha_consulta           DATE,         --Fecha de proceso
  v_usuario                  VARCHAR(30),  --Almacena al usuario
  v_rep_tot_registros        DECIMAL(9,0), --Total de registros
  v_rep_sum_aivs             DECIMAL(18,6),
  v_rep_sum_aportacion       DECIMAL(12,2)      
    
  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_nss
      PRINTX v_transaccion

    PAGE HEADER
      LET v_fecha_consulta = TODAY
      PRINTX v_nss
      PRINTX v_transaccion
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
   
    ON EVERY ROW
       PRINTX v_rep_tot_registros  USING "###,###,##&"
       PRINTX v_rep_sum_aivs       USING "###,###,###,##&.######" 
       PRINTX v_rep_sum_aportacion USING "###,###,###,###,##&.&&" 

    {ON LAST ROW
       PRINTX v_rep_tot_registros  USING "###,###,##&"
       PRINTX v_rep_sum_aivs       USING "###,###,###,##&.######" 
       PRINTX v_rep_sum_aportacion USING "###,###,###,###,##&.&&" }
END REPORT


FUNCTION fn_genera_archivo_transacciones_nss(p_transaccion, p_nss)
DEFINE p_transaccion         VARCHAR(50),
       p_nss                 CHAR(11)

DEFINE v_nom_archivo         VARCHAR(60),  --Nombre del archivo de salida
       v_ruta_envio_dis      CHAR(60),
       v_ruta_nomarch        VARCHAR(100), --Ruta y nombre del archivo de salida
       v_ch_arch_salida      BASE.CHANNEL,
       v_comando_dos         STRING,
       v_titulos             STRING,                 
       v_encabezado          STRING,
       v_detalle             STRING,
       v_sumario             STRING

DEFINE v_fecha_archivo       DATE,  
       v_hora_archivo        DATETIME HOUR TO HOUR ,
       v_min_archivo         DATETIME MINUTE TO MINUTE,
       v_sec_archivo         DATETIME SECOND TO SECOND,
       v_hora                STRING,
       --v_indice              DECIMAL(9,0)
       v_arh_indice          INTEGER

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  LET v_nom_archivo   = "/rep_trans_nss_", v_hora

  --Se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --Se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  LET v_encabezado = "TRANSACCIÓN: ",p_transaccion
  CALL v_ch_arch_salida.write([v_encabezado])

  IF p_nss IS NOT NULL THEN
     LET v_encabezado = "NSS: ",p_nss
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF
                
  --LET v_indice         = 1
  LET v_arh_indice = 1

  LET v_encabezado = " "
  CALL v_ch_arch_salida.write([v_encabezado])
      
  LET v_titulos = "NSS |NOMBRE |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |AIVS |APORTACIÓN |ENTIDAD FINANCIERA |CONCEPTO PAGO |TRANSACCIÓN |STATUS |FECHA ARCHIVO " --|INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])
  
  {SELECT a.desc_credito_ocg   ---Tipo de Crédito
  INTO   v_desc_credito 
  FROM   cat_tpo_credito_ocg a
  WHERE  a.tpo_credito_ocg = v_tpo_credito
  AND    a.ind_activo  = 1

  IF v_tpo_credito = 2 THEN
     LET v_interface = 'AS'
  END IF

  IF v_tpo_credito = 3 THEN
     LET v_interface = 'UG'
  END IF

  IF v_tpo_credito = 5 THEN
     LET v_interface = 'AS'
  END IF}

  FOR v_arh_indice = 1 TO a_datos_apo_sub_tran.getLength()
      LET v_detalle = a_datos_apo_sub_tran[v_arh_indice].nss, "|",
                      a_datos_apo_sub_tran[v_arh_indice].nombre CLIPPED, "|",
                      a_datos_apo_sub_tran[v_arh_indice].periodo_pago CLIPPED, "|",
                      a_datos_apo_sub_tran[v_arh_indice].f_pago USING "dd-mm-yyyy", "|",
                      a_datos_apo_sub_tran[v_arh_indice].folio_sua, "|",
                      a_datos_apo_sub_tran[v_arh_indice].nrp, "|",
                      a_datos_apo_sub_tran[v_arh_indice].aiv_ap_pat USING "#,###,##&.######", "|",
                      a_datos_apo_sub_tran[v_arh_indice].imp_ap_pat USING "#,###,##&.##", "|",
                      a_datos_apo_sub_tran[v_arh_indice].ent_financiera, "|",
                      a_datos_apo_sub_tran[v_arh_indice].tpo_credito CLIPPED, "|",
                      a_datos_apo_sub_tran[v_arh_indice].concepto CLIPPED, "|",
                      a_datos_apo_sub_tran[v_arh_indice].estado, "|",
                      TODAY USING "dd-mm-yyyy"   --, "|",
                      --v_interface

      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
        
  LET v_sumario = "TOTALES: | ",v_tot_registros," | | | | | ",
                                v_tot_aivs       USING "###,###,##&.######"," | ",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Transacciones por NSS \n en la ruta "||v_ruta_nomarch,"information") 
END FUNCTION


# Funcion para llenar el combo Transacción
FUNCTION fn_llena_combo_transaccion(p_transaccion)
DEFINE p_transaccion         SMALLINT
DEFINE a_transaccion         DYNAMIC ARRAY OF RECORD
  cod_concepto_ocg           SMALLINT,
  desc_concepto_ocg          VARCHAR(40)
END RECORD

DEFINE
  v_indice                   DECIMAL(9,0), --Variable del indice
  v_ls_query                 STRING,       --Cadena para almacenar Query
  cb                         ui.ComboBox   --Variable de Combobox

  LET cb = ui.ComboBox.forName("v_cb_transaccion") --Asignación del combo a la forma

  --Validación si el combo es nulo
  IF cb IS NULL THEN
     DISPLAY "El combo Transacción no se encuentra en la forma"
     EXIT PROGRAM
  END IF

  LET v_indice = 1

  --DISPLAY "edo_interno: ", p_edo_solicitud
  IF p_transaccion IS NULL THEN
     LET v_ls_query = "\n SELECT cod_concepto_ocg, desc_concepto_ocg ",
                      "\n FROM   cat_concepto_ocg ",
                      "\n ORDER BY cod_concepto_ocg "
  ELSE
     LET v_ls_query = "\n SELECT cod_concepto_ocg, desc_concepto_ocg ",
                      "\n FROM   cat_concepto_ocg ",
                      "\n WHERE cod_concepto_ocg = ",p_transaccion,
                      "\n ORDER BY cod_concepto_ocg "

  END IF

  PREPARE ps_cb_edo_sol FROM v_ls_query
  --DISPLAY "Llena combo Transacción ",v_ls_query

  CALL cb.clear()
  CALL a_transaccion.clear()

  --Declara el cursor para la consulta
  DECLARE cur_cb_edo_sol CURSOR FOR ps_cb_edo_sol
  FOREACH cur_cb_edo_sol INTO a_transaccion[v_indice].cod_concepto_ocg,
                              a_transaccion[v_indice].desc_concepto_ocg

    --Agrega elementos al combobox
    CALL cb.addItem(a_transaccion[v_indice].cod_concepto_ocg,
                    a_transaccion[v_indice].cod_concepto_ocg||" - "||a_transaccion[v_indice].desc_concepto_ocg CLIPPED)

    --DISPLAY a_transaccion[v_indice].cod_concepto_ocg, " - ", a_transaccion[v_indice].desc_concepto_ocg
    LET v_indice = v_indice + 1
  END FOREACH

  CALL a_transaccion.deleteElement(v_indice)
  LET v_indice = v_indice - 1

  --DISPLAY "v_indice: ", v_indice
  IF v_indice = 0 THEN
      CALL cb.addItem(p_transaccion, p_transaccion||" - NO EXISTE")
  END IF

  DISPLAY p_transaccion TO v_cb_transaccion # MUESTRA DATO EN EL COMBO
END FUNCTION