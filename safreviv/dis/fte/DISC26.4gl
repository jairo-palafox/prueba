################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 10/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC26                                                    #
#Objetivo         => Programa de consulta de Facturaci�n por NSS               #
#Fecha de Inicio  => 22/06/2015                                                #
################################################################################
-- Base que se utilizar�
DATABASE safre_viv

-- Definici�n de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30)  --Almacena opci�n del men�
 
  --Datos de salida
  DEFINE 
    v_nss	                 CHAR(11),
    v_nombre	             VARCHAR(50),
    v_curp	                 CHAR(18)
            
  DEFINE a_datos_apo_sub     DYNAMIC ARRAY OF RECORD 
    f_aplicacion             DATE,
    periodo_pago	         CHAR(06),
    f_pago	                 DATE,
    folio_sua	             DECIMAL(6,0),
    nrp	                     CHAR(11),
    aiv_ap_pat		         DECIMAL(18,6),
    imp_ap_pat		         DECIMAL(12,2),
    ent_financiera	         VARCHAR(50),
    num_ctr_int_ef	         CHAR(18),
    tpo_credito	             VARCHAR(50),	
    concepto	             VARCHAR(50),
    estado	                 VARCHAR(50)	            
  END RECORD
  
  --Totales 
  DEFINE 
    v_tot_registros          DECIMAL(9,0),  --Total de registros
    v_tot_aivs               DECIMAL(18,6), --Total de AIVS
    v_tot_aportacion         DECIMAL(12,2)  --Total de aportaciones
         
  DEFINE v_indice            SMALLINT

  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING,
    p_proceso_cod            SMALLINT

END GLOBALS

MAIN
  --Datos de entrada
  DEFINE 
    v_f_liquida_ini          DATE, --Fecha de liquidaci�n inicial
    v_f_liquida_fin          DATE  --Fecha de liquidacion final
         
  DEFINE v_cb_estado         SMALLINT    
  --DEFINE cb                  ui.ComboBox
  DEFINE v_ind_llena_cb      SMALLINT
  
  DEFINE 
    bnd_consulta             SMALLINT, 
    f_ventana                ui.Window, --Define las propiedades de la Ventana
    f_forma                  ui.Form    --Define las propiedades de la forma

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  LET p_proceso_cod  = 3904

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  --Validaci�n que NO se tenga alguna operaci�n de Dispersi�n de Pagos ejecut�ndose
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
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecut�ndose,\ningrese a esta opci�n cuando finalice."
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
  LET v_f_liquida_ini = ""
  LET v_f_liquida_fin = ""
  LET v_ind_llena_cb  = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISC26"
    DIALOG ATTRIBUTES(UNBUFFERED)    
      INPUT BY NAME v_nss, v_f_liquida_ini, v_f_liquida_fin
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_datos", 1)            --Oculta datos
          CALL f_forma.setElementHidden("gr_tot_registros", 1)    --Oculta el total de registros
          CALL f_forma.setElementHidden("gr_tot_aportaciones", 1) --Oculta el total de aportaciones
          CALL f_forma.setElementHidden("gr_tot_aivs", 1)         --Oculta el total de aivs
          --CALL f_forma.setElementHidden("lbl_totales", 1)         --Oculta etiqueta Totales
          NEXT FIELD v_nss
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           IF v_nss IS NULL THEN
              CALL fn_mensaje("ATENCI�N",
                              "Debe ingresar el NSS a consultar.",
                              "about")
              NEXT FIELD v_nss
           END IF
            
           {--Valida que se inserte al menos un par�metro
           IF (v_nss IS NULL AND
               v_f_liquida_ini IS NULL AND v_f_liquida_fin IS NULL) THEN
                
               CALL fn_mensaje("ATENCI�N",
                               "No ha capturado ning�n criterio de b�squeda",
                               "about")
               NEXT FIELD v_nss   
           END IF} 

           IF v_f_liquida_ini > TODAY THEN 
              CALL fn_mensaje("ATENCI�N",
                              "La Fecha de Operaci�n Inicial no puede ser mayor al D�a de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF    

           IF v_f_liquida_fin > TODAY THEN 
              CALL fn_mensaje("ATENCI�N",
                              "La Fecha de Operaci�n Final no puede ser mayor al D�a de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 
          
           IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NULL THEN 
              CALL fn_mensaje("ATENCI�N", 
                              "Debe capturar una Fecha de Operaci�n Final.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF

           IF v_f_liquida_fin IS NOT NULL AND v_f_liquida_ini IS NULL THEN 
              CALL fn_mensaje("ATENCI�N", 
                              "Debe capturar una Fecha de Operaci�n Inicial.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF 

           IF (v_f_liquida_ini > v_f_liquida_fin) THEN
              CALL fn_mensaje("ATENCI�N", 
                              "La Fecha Inicial de Operaci�n no puede ser mayor a la Fecha Final de Operaci�n.",
                              "about")
              NEXT FIELD v_f_liquida_ini 
           END IF  

           --DISPLAY "v_folio_liquida: -",v_folio_liquida,"-"

           --LET v_f_liquida_ini = v_f_liquida_ini USING "dd-mm-yyyy"
           --LET v_f_liquida_fin = v_f_liquida_fin USING "dd-mm-yyyy"

           --DISPLAY "v_f_liquida_ini: --",v_f_liquida_ini,"-"
           --DISPLAY "v_f_liquida_fin: --",v_f_liquida_fin,"-"          

           CALL fn_consultar(v_nss, v_f_liquida_ini, v_f_liquida_fin)  

           IF v_indice > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0)          --Muestra detalle de la consulta
              CALL f_forma.setElementHidden("gr_datos", 0)            --Muestra datos de la consulta 
              --CALL f_forma.setElementHidden("gr_tot_registros", 0)    --Muestra el total de registros
              --CALL f_forma.setElementHidden("gr_tot_aportaciones", 0) --Muestra el total de aportaciones
              --CALL f_forma.setElementHidden("gr_tot_aivs", 0)         --Muestra el total de aivs
              --CALL f_forma.setElementHidden("lbl_totales", 0)         --Muestra etiqueta Totales

              DISPLAY ARRAY a_datos_apo_sub TO rec_detalle.*
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE DISPLAY      
                  DISPLAY v_nss            TO v_nss1
                  DISPLAY v_nombre         TO v_nombre
                  DISPLAY v_curp           TO v_curp
                  DISPLAY v_tot_registros  TO txt_tot_registros
                  DISPLAY v_tot_aivs       TO txt_tot_aivs
                  DISPLAY v_tot_aportacion TO txt_tot_aportaciones
            
                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)                  
                  CALL DIALOG.setActionHidden("btn_liquidar", 0)                  
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                ON ACTION reporte
                   CALL fn_reporte(v_f_liquida_ini, v_f_liquida_fin)
                
                ON ACTION archivo
                   CALL fn_genera_archivo_apo_sub_nss(v_nss, v_f_liquida_ini, v_f_liquida_fin)   
              END DISPLAY                    
           ELSE
             CALL fn_mensaje("ATENCI�N",
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

FUNCTION fn_consultar(p_nss, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_nss               CHAR(11) --Folio de liquidaci�n de la dispersi�n
  DEFINE p_f_liquida_ini     DATE     --Fecha de liquidaci�n inicial
  DEFINE p_f_liquida_fin     DATE     --Fecha de liquidacion final   

  DEFINE v_desc_concepto     VARCHAR(50)
  DEFINE v_desc_ent_financiera VARCHAR(50)
  DEFINE v_desc_credito      VARCHAR(50)
  DEFINE v_desc_estado       VARCHAR(50)
  
  DEFINE v_concepto          SMALLINT
  DEFINE v_ent_financiera    SMALLINT
  DEFINE v_tpo_credito       SMALLINT 
  DEFINE v_estado            SMALLINT

  DEFINE 
    v_f_liquida              DATE,
    v_f_transaccion          DATE,
    v_f_factura              DATE

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_afi_disc26
  WHENEVER ERROR STOP

  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT H.id_dis_interface_ef, ",
                  "\n        A.id_derechohabiente,  ",
                  "\n        A.nss,                 ",
                  "\n        A.nombre_af,           ",
                  "\n        A.ap_paterno_af,       ",
                  "\n        A.ap_materno_af,       ",
                  "\n        A.curp                 ",
                  "\n FROM   dis_ctr_aps_tns H,     ",
                  "\n        afi_derechohabiente A  ",
                  "\n WHERE  H.id_derechohabiente = A.id_derechohabiente "

  IF p_nss IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND A.nss = ", p_nss
  END IF

  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND ((H.f_liquida     BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"' ) OR  ",
                                "\n      (H.f_transaccion BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"' ) OR  ",
                                "\n      (H.f_factura     BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"' )) "
  END IF
  
  LET g_sql_txt = g_sql_txt, "\n INTO TEMP tmp_afi_disc26 "

  PREPARE prep_tmp_afi4 FROM g_sql_txt
  EXECUTE prep_tmp_afi4

  UPDATE STATISTICS FOR TABLE tmp_afi_disc26
  
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT afi.nss, ",
                  "\n        TRIM(afi.nombre_af)||' '|| ",
                  "\n        TRIM(afi.ap_paterno_af)||' '|| ",
                  "\n        TRIM(afi.ap_materno_af) , ",
                  "\n        afi.curp, ",
                  "\n        dca.f_liquida, ",
                  "\n        dca.f_transaccion, ",
                  "\n        dca.f_factura, ",
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.aiv_ap_pat, ",
                  "\n        dca.imp_ap_pat, ",
                  "\n        dca.cve_ent_financiera, ",
                  "\n        dca.num_ctr_int_ef, ",
                  "\n        dca.tpo_credito, ",
                  "\n        dca.concepto,",
                  "\n        dca.estado ",
                  --"\n FROM   afi_derechohabiente afi, ",
                  --"\n        dis_ctr_aps_tns dca ",
                  --"\n WHERE afi.id_derechohabiente = dca.id_derechohabiente "
                  "\n FROM   dis_ctr_aps_tns dca, ",
                  "\n        tmp_afi_disc26 afi ",
                  "\n WHERE  dca.id_dis_interface_ef = afi.id_dis_interface_ef ",
                  "\n AND    dca.id_derechohabiente  = afi.id_derechohabiente "
                  
  INITIALIZE  v_f_liquida, v_f_transaccion, v_f_factura TO NULL
   
  IF p_nss IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND afi.nss = ",p_nss
  END IF

  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n  AND ((dca.f_liquida  BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"' ) OR  ",
                                "\n       (dca.f_transaccion  BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"' ) OR  ",
                                "\n       (dca.f_factura  BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"' )) "
  END IF
        
  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.f_liquida DESC, dca.f_transaccion DESC, dca.f_factura DESC, dca.periodo_pago DESC, dca.f_pago DESC "
                
  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE ps_apo_sub_concepto FROM g_sql_txt
  DECLARE cur_apo_sub_concepto CURSOR FOR ps_apo_sub_concepto

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
  
  FOREACH cur_apo_sub_concepto INTO v_nss,
                                    v_nombre,
                                    v_curp,
                                    v_f_liquida,
                                    v_f_transaccion,
                                    v_f_factura,
                                    a_datos_apo_sub[v_indice].periodo_pago,
                                    a_datos_apo_sub[v_indice].f_pago,	     
                                    a_datos_apo_sub[v_indice].folio_sua,	        
                                    a_datos_apo_sub[v_indice].nrp,
                                    a_datos_apo_sub[v_indice].aiv_ap_pat,                
                                    a_datos_apo_sub[v_indice].imp_ap_pat,
                                    v_ent_financiera,         --ent_financiera	      
                                    a_datos_apo_sub[v_indice].num_ctr_int_ef,	
                                    v_tpo_credito,            --tpo_credito	         	
                                    v_concepto,               --concepto	           
                                    v_estado                  --estado                                                      

    --Asiganaci�n de FECHA DE APLICACI�N de acuerdo al estado
    ------------------------------------------------------------
    -- v_estado = 10 (REGISTRADO)          <----> Fecha de Liquidaci�n
    -- v_estado >= 20 <= 29 (CONFIRMADO)   <----> Fecha de Transacci�n
    -- v_estado >= 30 <= 39 (FACTUTRADO)   <----> Fecha de Facturaci�n
    ------------------------------------------------------------
    CASE 
      WHEN v_estado = 10 OR --REGISTRADO     --> Fecha Liquidaci�n
           v_estado = 40 OR --EF BLOQUEADA   --> Fecha Liquidaci�n 
           v_estado = 50    --PAGO DUPLICADO --> Fecha Liquidaci�n
        LET a_datos_apo_sub[v_indice].f_aplicacion = v_f_liquida
        
      WHEN v_estado >= 20 AND v_estado <= 29 --CONFIRMADO --> Fecha de Transacci�n
        LET a_datos_apo_sub[v_indice].f_aplicacion = v_f_transaccion
            
      WHEN v_estado >= 30 AND v_estado <= 39 --FACTURADO --> Fecha de Facturaci�n
        LET a_datos_apo_sub[v_indice].f_aplicacion = v_f_factura

      WHEN v_estado = 55 OR v_estado = 60 OR v_estado = 70    --PROVISIONADO, PUBLICADO Y PAGADO --> Fecha Liquidacion
        LET a_datos_apo_sub[v_indice].f_aplicacion = v_f_factura

      OTHERWISE
    END CASE
                                     
    SELECT a.desc_credito_ocg ---Tipo de Cr�dito
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

    LET a_datos_apo_sub[v_indice].concepto       = v_concepto, " - ", v_desc_concepto
    LET a_datos_apo_sub[v_indice].ent_financiera = v_ent_financiera USING "&&&", " - ", v_desc_ent_financiera
    LET a_datos_apo_sub[v_indice].tpo_credito    = v_tpo_credito, " - ", v_desc_credito  
    LET a_datos_apo_sub[v_indice].estado         = v_estado, " - ",v_desc_estado  
                 
    --LET v_tot_registros  = v_tot_registros    + a_datos_apo_sub[v_indice].tot_registros         
    LET v_tot_aivs       = v_tot_aivs         + a_datos_apo_sub[v_indice].aiv_ap_pat
    LET v_tot_aportacion = v_tot_aportacion   + a_datos_apo_sub[v_indice].imp_ap_pat     
    LET v_indice         = v_indice           + 1 
  END FOREACH

  FREE cur_apo_sub_concepto
  
  CALL a_datos_apo_sub.deleteElement(v_indice)
  LET v_indice        = v_indice - 1 
  LET v_tot_registros = v_indice 
END FUNCTION  

FUNCTION fn_reporte(p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_f_liquida_ini     DATE
  DEFINE p_f_liquida_fin     DATE
   
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_rep_indice        INTEGER
  
  --Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISC261.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte
  START REPORT rep_apo_sub_por_nss TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO  a_datos_apo_sub.getLength()
        OUTPUT TO REPORT rep_apo_sub_por_nss(a_datos_apo_sub[v_rep_indice].*,
                                             v_tot_registros,
                                             v_tot_aivs,
                                             v_tot_aportacion,
                                             g_usuario,
                                             v_nss,
                                             v_nombre,
                                             v_curp,
                                             p_f_liquida_ini, 
                                             p_f_liquida_fin)
    END FOR
  FINISH REPORT rep_apo_sub_por_nss
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Cr�dito igual a Cero
REPORT rep_apo_sub_por_nss(rec_datos_apo_sub, 
                           v_rep_tot_registros, 
                           v_rep_sum_aivs, 
                           v_rep_sum_aportacion, 
                           v_usuario,
                           v_nss,
                           v_nombre,
                           v_curp,
                           p_f_liquida_ini, 
                           p_f_liquida_fin)

  DEFINE rec_datos_apo_sub   RECORD  
    f_aplicacion             DATE,
    periodo_pago	         CHAR(06),
    f_pago	                 DATE,
    folio_sua	             CHAR(06),
    nrp	                     CHAR(11),
    aiv_ap_pat		         DECIMAL(18,6),
    imp_ap_pat		         DECIMAL(12,2),
    ent_financiera	         VARCHAR(50),
    num_ctr_int_ef	         CHAR(18),
    tpo_credito	             VARCHAR(50),	
    concepto	             VARCHAR(50),
    estado	                 VARCHAR(50)	       
  END RECORD                        

  DEFINE 
    v_nss	                 CHAR(11),
    v_nombre	             VARCHAR(50),
    v_curp	                 CHAR(18)

  DEFINE 
    p_f_liquida_ini          DATE, 
    p_f_liquida_fin          DATE
          
  DEFINE 
    v_fecha_consulta         DATE,         --Fecha de proceso
    v_usuario                VARCHAR(30),  --Almacena al usuario
    v_rep_tot_registros      DECIMAL(9,0), --Total de registros
    v_rep_sum_aivs           DECIMAL(18,6),
    v_rep_sum_aportacion     DECIMAL(12,2)      
    
  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_nss
      PRINTX v_nombre
      PRINTX v_curp
      PRINTX p_f_liquida_ini USING "dd-mm-yyyy" 
      PRINTX p_f_liquida_fin USING "dd-mm-yyyy"
      PRINTX v_rep_tot_registros USING "###,###,##&"

    PAGE HEADER
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
   
    ON EVERY ROW
       PRINTX rec_datos_apo_sub.f_aplicacion USING "dd-mm-yyyy"      
       PRINTX rec_datos_apo_sub.periodo_pago	      
       PRINTX rec_datos_apo_sub.f_pago USING "dd-mm-yyyy"	            
       PRINTX rec_datos_apo_sub.folio_sua USING "######"	         
       PRINTX rec_datos_apo_sub.nrp
       PRINTX rec_datos_apo_sub.aiv_ap_pat	USING "###,###,###,##&.######"
       PRINTX rec_datos_apo_sub.imp_ap_pat	USING "###,###,###,###,##&.&&"       
       PRINTX rec_datos_apo_sub.ent_financiera
       PRINTX rec_datos_apo_sub.num_ctr_int_ef
       PRINTX rec_datos_apo_sub.tpo_credito	         
       PRINTX rec_datos_apo_sub.concepto	            
       PRINTX rec_datos_apo_sub.estado

    ON LAST ROW
       PRINTX v_rep_tot_registros
       PRINTX v_rep_sum_aivs       USING "###,###,###,##&.######" 
       PRINTX v_rep_sum_aportacion USING "###,###,###,###,##&.&&" 
END REPORT

FUNCTION fn_genera_archivo_apo_sub_nss(v_nss, v_f_liquida_ini, v_f_liquida_fin)
  DEFINE 
    v_nss                    CHAR(11), --Folio de liquidaci�n de la dispersi�n
    v_f_liquida_ini          DATE,     --Fecha de liquidaci�n inicial
    v_f_liquida_fin          DATE      --Fecha de liquidacion final

  DEFINE 
    v_nom_archivo            VARCHAR(60),  --Nombre del archivo de salida
    v_ruta_envio_dis         CHAR(60),
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_comando_dos            STRING,
    v_titulos                STRING,                 
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 SMALLINT

  DEFINE arr_info_apo        DYNAMIC ARRAY OF RECORD
    f_aplicacion             DATE,
    periodo_pago	         CHAR(06),
    f_pago	                 DATE,
    folio_sua	             DECIMAL(6,0),
    nrp	                     CHAR(11),
    imp_ap_pat		         DECIMAL(12,2),
    aiv_ap_pat		         DECIMAL(18,6),
    ent_financiera	         VARCHAR(50),
    num_ctr_int_ef	         CHAR(18),
    tpo_credito	             VARCHAR(50),	
    concepto	             VARCHAR(50),
    estado	                 VARCHAR(50)
  END RECORD 

  DEFINE v_aportacion        CHAR(10)

  DEFINE 
    v_f_liquida              DATE, 
    v_f_transaccion          DATE, 
    v_f_factura              DATE

  DEFINE v_desc_concepto     VARCHAR(50)
  DEFINE v_desc_ent_financiera VARCHAR(50)
  DEFINE v_desc_credito      VARCHAR(50)
  DEFINE v_desc_estado       VARCHAR(50)
   
  DEFINE v_concepto          SMALLINT
  DEFINE v_ent_financiera    SMALLINT
  DEFINE v_tpo_credito       SMALLINT 
  DEFINE v_estado            SMALLINT
  DEFINE v_interface         CHAR(2) 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  LET v_nom_archivo   = "/fact_nss_", v_hora

  --Se obtienen la ruta envio del m�dulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --Se crea el manejador de archivo y se indica que se escribir� en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT UNIQUE afi.nss, ",
                  "\n        TRIM(afi.nombre_af)||' '|| ",
                  "\n        TRIM(afi.ap_paterno_af)||' '|| ",
                  "\n        TRIM(afi.ap_materno_af) , ",
                  "\n        afi.curp ",
                  --"\n FROM   afi_derechohabiente afi ",
                  "\n FROM   tmp_afi_disc26 afi ",
                  "\n WHERE  afi.nss = '",v_nss,"'"

  DISPLAY g_sql_txt
  PREPARE ps_datos_apo_sub FROM g_sql_txt
  EXECUTE ps_datos_apo_sub INTO v_nss, v_nombre, v_curp

  LET v_encabezado = "NSS: ",v_nss
  CALL v_ch_arch_salida.write([v_encabezado])
   
  LET v_encabezado = "Nombre: ",v_nombre
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = "CURP: ",v_curp
  CALL v_ch_arch_salida.write([v_encabezado])
                  
  LET g_sql_txt = "\n SELECT dca.f_liquida, ",
                  "\n        dca.f_transaccion, ",
                  "\n        dca.f_factura, ",
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.aiv_ap_pat, ",
                  "\n        dca.imp_ap_pat, ",
                  "\n        dca.cve_ent_financiera, ",
                  "\n        dca.num_ctr_int_ef, ",
                  "\n        dca.tpo_credito, ",
                  "\n        dca.concepto,",
                  "\n        dca.estado ",
                  --"\n FROM   afi_derechohabiente afi, ",
                  --"\n        dis_ctr_aps_tns dca ",
                  --"\n WHERE  afi.id_derechohabiente = dca.id_derechohabiente ",
                  --"\n AND    afi.nss                = '",v_nss,"'"
                  "\n FROM   dis_ctr_aps_tns dca, ",
                  "\n        tmp_afi_disc26 afi ",
                  "\n WHERE  dca.id_dis_interface_ef = afi.id_dis_interface_ef ",
                  "\n AND    dca.id_derechohabiente  = afi.id_derechohabiente ",
                  "\n AND    afi.nss                 = '",v_nss,"'"

  INITIALIZE  v_f_liquida, v_f_transaccion, v_f_factura TO NULL

  IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n  AND ((dca.f_liquida  BETWEEN '", v_f_liquida_ini ,"' AND '", v_f_liquida_fin ,"' ) OR  ",
                                "\n       (dca.f_transaccion  BETWEEN '", v_f_liquida_ini ,"' AND '", v_f_liquida_fin ,"' ) OR  ",
                                "\n       (dca.f_factura  BETWEEN '", v_f_liquida_ini ,"' AND '", v_f_liquida_fin ,"' )) "

     LET v_encabezado = "FECHA DE OPERACI�N INICIAL: ",v_f_liquida_ini USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
      
     LET v_encabezado = "FECHA DE OPERACI�N FINAL: ",v_f_liquida_fin USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF
        
  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.f_liquida DESC, dca.f_transaccion DESC, dca.f_factura DESC, dca.periodo_pago DESC, dca.f_pago DESC "
                
  DISPLAY "g_sql_txt: -",g_sql_txt,"-" 
   
  PREPARE ps_arch_apo_sub_nss FROM g_sql_txt
  DECLARE cur_arch_apo_sub_nss CURSOR FOR ps_arch_apo_sub_nss
  
  LET v_indice         = 1
  LET v_tot_aivs       = 0
  LET v_tot_aportacion = 0

  LET v_encabezado = " "
  CALL v_ch_arch_salida.write([v_encabezado])
      
  LET v_titulos = "FECHA OPERACI�N |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |AIVS |APORTACI�N |ENTIDAD FINANCIERA |N�MERO CONTROL INTERNO |CONCEPTO PAGO |TRANSACCI�N |STATUS |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])
  
  FOREACH cur_arch_apo_sub_nss INTO v_f_liquida,
                                    v_f_transaccion,
                                    v_f_factura,
                                    arr_info_apo[v_indice].periodo_pago,
                                    arr_info_apo[v_indice].f_pago,	     
                                    arr_info_apo[v_indice].folio_sua,	        
                                    arr_info_apo[v_indice].nrp,
                                    arr_info_apo[v_indice].aiv_ap_pat,
                                    arr_info_apo[v_indice].imp_ap_pat,
                                    v_ent_financiera,         --ent_financiera	      
                                    arr_info_apo[v_indice].num_ctr_int_ef,	
                                    v_tpo_credito,            --tpo_credito	         	
                                    v_concepto,               --concepto	           
                                    v_estado                  --estado                                                      

    --Asiganaci�n de FECHA DE APLICACI�N de acuerdo al estado
    ------------------------------------------------------------
    -- v_estado = 10 (REGISTRADO)          <----> Fecha de Liquidaci�n
    -- v_estado >= 20 <= 29 (CONFIRMADO)   <----> Fecha de Transacci�n
    -- v_estado >= 30 <= 39 (FACTUTRADO)   <----> Fecha de Facturaci�n
    ------------------------------------------------------------
    CASE 
      WHEN v_estado = 10 OR --REGISTRADO     --> Fecha Liquidaci�n
           v_estado = 40 OR --EF BLOQUEADA   --> Fecha Liquidaci�n
           v_estado = 50    --PAGO DUPLICADO --> Fecha Liquidaci�n
        LET arr_info_apo[v_indice].f_aplicacion = v_f_liquida
            
      WHEN v_estado >= 20 AND v_estado <= 29 --CONFIRMADO --> Fecha de Transacci�n
        LET arr_info_apo[v_indice].f_aplicacion = v_f_transaccion
            
      WHEN v_estado >= 30 AND v_estado <= 39 --FACTURADO --> Fecha de Facturaci�n
        LET arr_info_apo[v_indice].f_aplicacion = v_f_factura

      WHEN v_estado = 55 OR --PROVISIONADO --> Fecha Facturaci�n
           v_estado = 60 OR --PUBLICADO    --> Fecha Facturaci�n
           v_estado = 70    --PAGADO       --> Fecha Facturaci�n
        LET arr_info_apo[v_indice].f_aplicacion = v_f_factura

      OTHERWISE
    END CASE
                                     
    SELECT a.desc_credito_ocg   ---Tipo de Cr�dito
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
    END IF
    
    SELECT UNIQUE(desc_edo_aps)   ---Estado
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

    LET arr_info_apo[v_indice].concepto       = v_concepto, " - ", v_desc_concepto
    LET arr_info_apo[v_indice].ent_financiera = v_ent_financiera USING "&&&", " - ", v_desc_ent_financiera
    LET arr_info_apo[v_indice].tpo_credito    = v_tpo_credito, " - ", v_desc_credito  
    LET arr_info_apo[v_indice].estado         = v_estado, " - ",v_desc_estado  
                      
    LET v_detalle = arr_info_apo[v_indice].f_aplicacion USING "dd-mm-yyyy", "|",
                    arr_info_apo[v_indice].periodo_pago CLIPPED, "|",
                    arr_info_apo[v_indice].f_pago USING "dd-mm-yyyy", "|",
                    arr_info_apo[v_indice].folio_sua, "|",
                    arr_info_apo[v_indice].nrp, "|",
                    arr_info_apo[v_indice].aiv_ap_pat USING "#,###,##&.######", "|",
                    arr_info_apo[v_indice].imp_ap_pat USING "#,###,##&.##", "|",
                    arr_info_apo[v_indice].ent_financiera, "|",
                    arr_info_apo[v_indice].num_ctr_int_ef, "|",
                    arr_info_apo[v_indice].tpo_credito CLIPPED, "|",
                    arr_info_apo[v_indice].concepto CLIPPED, "|",
                    arr_info_apo[v_indice].estado, "|",
                    TODAY USING "dd-mm-yyyy", "|",
                    v_interface

    CALL v_ch_arch_salida.write([v_detalle])
        
    LET v_tot_aivs       = v_tot_aivs       + arr_info_apo[v_indice].aiv_ap_pat
    LET v_tot_aportacion = v_tot_aportacion + arr_info_apo[v_indice].imp_ap_pat     
    LET v_indice         = v_indice         + 1  
  END FOREACH

  FREE cur_arch_apo_sub_nss
  
  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice        = v_indice - 1 
  LET v_tot_registros = v_indice

  LET v_sumario = "TOTALES: | ",v_tot_registros," | | | | ",
                                v_tot_aivs       USING "###,###,##&.######"," | ",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Informaci�n","Se ha generado el archivo de Consulta Facturaci�n por NSS \n en la ruta "||v_ruta_nomarch,"information") 
END FUNCTION