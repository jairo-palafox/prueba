################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 05/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC40                                                    #
#Objetivo         => Programa de consulta de Cargos Duplicados (Aportaciones   #
#                    Subsecuentes).                                            #
#Fecha de Inicio  => 05/01/2018                                                #
################################################################################
-- Base que se utilizará
DATABASE safre_viv

-- Definición de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30)  --Almacena opción del menú
 
  --Datos de salida
  DEFINE 
    v_nss	                 CHAR(11),
    v_nombre	             VARCHAR (50),
    v_curp	                 CHAR(18)
            
  {DEFINE a_datos_apo_sub     DYNAMIC ARRAY OF RECORD 
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
  END RECORD}

  DEFINE a_datos_apo_sub     DYNAMIC ARRAY OF RECORD 
    f_registro               DATE,
    folio_registro           DECIMAL(9,0),
    folio_liquida            DECIMAL(9,0),
    nss                      CHAR(11),
    periodo_pago	         CHAR(06),
    f_pago	                 DATE,
    nrp	                     CHAR(11),
    folio_sua	             DECIMAL(6,0),
    aiv_ap_pat		         DECIMAL(18,6),
    imp_ap_pat		         DECIMAL(12,2),
    folio_liquida_ao         DECIMAL(9,0),
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
    v_f_liquida_ini          DATE, --Fecha de liquidación inicial
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
  LET p_proceso_cod  = 932

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
  LET v_f_liquida_ini = ""
  LET v_f_liquida_fin = ""
  LET v_ind_llena_cb  = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISC40"
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
              CALL fn_mensaje("ATENCIÓN",
                              "Debe ingresar el NSS a consultar.",
                              "about")
              NEXT FIELD v_nss
           END IF
            
           {--Valida que se inserte al menos un parámetro
           IF (v_nss IS NULL AND
               v_f_liquida_ini IS NULL AND v_f_liquida_fin IS NULL) THEN
                
               CALL fn_mensaje("ATENCIÓN",
                               "No ha capturado ningún criterio de búsqueda",
                               "about")
               NEXT FIELD v_nss   
           END IF} 

           IF v_f_liquida_ini > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Registro Inicial no puede ser mayor al Día de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF    

           IF v_f_liquida_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Registro Final no puede ser mayor al Día de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 
          
           IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Registro Final.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF

           IF v_f_liquida_fin IS NOT NULL AND v_f_liquida_ini IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Registro Inicial.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF 

           IF (v_f_liquida_ini > v_f_liquida_fin) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Inicial de Registro no puede ser mayor a la Fecha Final de Registro.",
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

FUNCTION fn_consultar(p_nss, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_nss               CHAR(11) --Folio de liquidación de la dispersión
  DEFINE p_f_liquida_ini     DATE     --Fecha de liquidación inicial
  DEFINE p_f_liquida_fin     DATE     --Fecha de liquidacion final   

  DEFINE v_desc_estado       VARCHAR(50)
  
  DEFINE v_estado            SMALLINT

  DEFINE 
    v_f_liquida              DATE,
    v_f_transaccion          DATE,
    v_f_factura              DATE

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_afi_disc40
  WHENEVER ERROR STOP

  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT A.id_derechohabiente,  ",
                  "\n        A.nss,                 ",
                  "\n        A.nombre_af,           ",
                  "\n        A.ap_paterno_af,       ",
                  "\n        A.ap_materno_af,       ",
                  "\n        A.curp                 ",
                  "\n FROM   dis_ap_cargo_dup H,    ",
                  "\n        afi_derechohabiente A  ",
                  "\n WHERE  H.id_derechohabiente = A.id_derechohabiente "

  IF p_nss IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND A.nss = ", p_nss
  END IF

  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND H.f_registro BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"'"
  END IF
  
  LET g_sql_txt = g_sql_txt, "\n INTO TEMP tmp_afi_disc40 "

  PREPARE prep_tmp_afi4 FROM g_sql_txt
  EXECUTE prep_tmp_afi4

  UPDATE STATISTICS FOR TABLE tmp_afi_disc40
  
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT UNIQUE afi.nss, ",
                  "\n        TRIM(afi.nombre_af)||' '|| ",
                  "\n        TRIM(afi.ap_paterno_af)||' '|| ",
                  "\n        TRIM(afi.ap_materno_af) , ",
                  "\n        afi.curp, ",
                  "\n        dca.f_registro, ",
                  "\n        dca.folio, ",
                  "\n        dca.folio_liquida, ",            
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.aivs_ap_pat, ",
                  "\n        dca.imp_apo_pat, ",
                  "\n        dca.folio_liq_ajuste, ",
                  "\n        dca.estado ",
                  "\n FROM   dis_ap_cargo_dup dca, ",
                  "\n        tmp_afi_disc40 afi ",
                  "\n WHERE  dca.id_derechohabiente  = afi.id_derechohabiente "
                  
  INITIALIZE  v_f_liquida, v_f_transaccion, v_f_factura TO NULL
   
  IF p_nss IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND afi.nss = ",p_nss
  END IF

  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dca.f_registro BETWEEN '", p_f_liquida_ini ,"' AND '", p_f_liquida_fin ,"'"
  END IF
        
  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.f_registro DESC, dca.periodo_pago DESC, dca.f_pago DESC, dca.folio DESC "
                
  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE ps_apo_sub_concepto FROM g_sql_txt
  DECLARE cur_apo_sub_concepto CURSOR FOR ps_apo_sub_concepto

  LET v_indice         = 1
  LET v_tot_registros  = 0
  LET v_tot_aivs       = 0.00
  LET v_tot_aportacion = 0.00
  LET v_estado         = 0

  INITIALIZE v_desc_estado TO NULL
  
  FOREACH cur_apo_sub_concepto INTO v_nss,
                                    v_nombre,
                                    v_curp,
                                    v_f_liquida,
                                    a_datos_apo_sub[v_indice].folio_registro,
                                    a_datos_apo_sub[v_indice].folio_liquida,
                                    a_datos_apo_sub[v_indice].periodo_pago,
                                    a_datos_apo_sub[v_indice].f_pago,
                                    a_datos_apo_sub[v_indice].folio_sua,
                                    a_datos_apo_sub[v_indice].nrp,
                                    a_datos_apo_sub[v_indice].aiv_ap_pat,
                                    a_datos_apo_sub[v_indice].imp_ap_pat,
                                    a_datos_apo_sub[v_indice].folio_liquida_ao,
                                    v_estado                  --estado                                                      

    LET a_datos_apo_sub[v_indice].nss        = v_nss
    LET a_datos_apo_sub[v_indice].f_registro = v_f_liquida
    
    SELECT UNIQUE(co.desc_edo_apo_sub)
    INTO   v_desc_estado
    FROM   cat_edo_ap_subsecuente co
    WHERE  co.cod_edo_apo_sub = v_estado

    LET a_datos_apo_sub[v_indice].estado = v_estado, " - ",v_desc_estado  
                 
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
  IF fgl_report_loadCurrentSettings("DISC401.4rp") THEN 
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

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
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
    f_registro               DATE,
    folio_registro           DECIMAL(9,0),
    folio_liquida            DECIMAL(9,0),
    nss                      CHAR(11),
    periodo_pago	         CHAR(06),
    f_pago	                 DATE,
    nrp	                     CHAR(11),
    folio_sua	             DECIMAL(6,0),
    aiv_ap_pat		         DECIMAL(18,6),
    imp_ap_pat		         DECIMAL(12,2),
    folio_liquida_ao         DECIMAL(9,0),
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
       PRINTX rec_datos_apo_sub.f_registro USING "dd-mm-yyyy"
       PRINTX rec_datos_apo_sub.folio_registro
       PRINTX rec_datos_apo_sub.folio_liquida
       PRINTX rec_datos_apo_sub.periodo_pago	      
       PRINTX rec_datos_apo_sub.f_pago USING "dd-mm-yyyy"	            
       PRINTX rec_datos_apo_sub.folio_sua USING "######"	         
       PRINTX rec_datos_apo_sub.nrp
       PRINTX rec_datos_apo_sub.aiv_ap_pat	USING "###,###,###,##&.######"
       PRINTX rec_datos_apo_sub.imp_ap_pat	USING "###,###,###,###,##&.&&"
       PRINTX rec_datos_apo_sub.folio_liquida_ao       
       PRINTX rec_datos_apo_sub.estado

    ON LAST ROW
       PRINTX v_rep_tot_registros
       PRINTX v_rep_sum_aivs       USING "###,###,###,##&.######" 
       PRINTX v_rep_sum_aportacion USING "###,###,###,###,##&.&&" 

END REPORT

FUNCTION fn_genera_archivo_apo_sub_nss(v_nss, v_f_liquida_ini, v_f_liquida_fin)
  DEFINE 
    v_nss                    CHAR(11), --Folio de liquidación de la dispersión
    v_f_liquida_ini          DATE,     --Fecha de liquidación inicial
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
    f_registro               DATE,
    folio_registro           DECIMAL(9,0),
    folio_liquida            DECIMAL(9,0),
    nss                      CHAR(11),
    periodo_pago	         CHAR(06),
    f_pago	                 DATE,
    nrp	                     CHAR(11),
    folio_sua	             DECIMAL(6,0),
    aiv_ap_pat		         DECIMAL(18,6),
    imp_ap_pat		         DECIMAL(12,2),
    folio_liquida_ao         DECIMAL(9,0),
    estado	                 VARCHAR(50)	            
  END RECORD 

  DEFINE 
    v_f_liquida              DATE
    
  DEFINE v_desc_credito      VARCHAR(50)
  DEFINE v_desc_estado       VARCHAR(50)
   
  DEFINE v_tpo_credito       SMALLINT 
  DEFINE v_estado            SMALLINT
  DEFINE v_interface         CHAR(6) 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/apsd_nss_", v_hora

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --Se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT UNIQUE afi.nss, ",
                  "\n        TRIM(afi.nombre_af)||' '|| ",
                  "\n        TRIM(afi.ap_paterno_af)||' '|| ",
                  "\n        TRIM(afi.ap_materno_af) , ",
                  "\n        afi.curp ",
                  "\n FROM   tmp_afi_disc40 afi ",
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
                  
  LET g_sql_txt = "\n SELECT UNIQUE dca.f_registro, ",
                  "\n        dca.folio, ",
                  "\n        dca.folio_liquida, ",                  
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.aivs_ap_pat, ",
                  "\n        dca.imp_apo_pat, ",
                  "\n        dca.folio_liq_ajuste, ",
                  "\n        dca.estado ",
                  "\n FROM   dis_ap_cargo_dup dca, ",
                  "\n        tmp_afi_disc40 afi ",
                  "\n WHERE  dca.id_derechohabiente  = afi.id_derechohabiente ",
                  "\n AND    afi.nss                 = '",v_nss,"'"

  IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dca.f_registro  BETWEEN '", v_f_liquida_ini ,"' AND '", v_f_liquida_fin ,"'"

     LET v_encabezado = "FECHA DE REGISTRO INICIAL: ",v_f_liquida_ini USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
      
     LET v_encabezado = "FECHA DE REGISTRO FINAL: ",v_f_liquida_fin USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF
        
  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.f_registro DESC, dca.periodo_pago DESC, dca.f_pago DESC, dca.folio DESC "
                
  DISPLAY "g_sql_txt: -",g_sql_txt,"-" 
   
  PREPARE ps_arch_apo_sub_nss FROM g_sql_txt
  DECLARE cur_arch_apo_sub_nss CURSOR FOR ps_arch_apo_sub_nss
  
  LET v_indice         = 1
  LET v_tot_aivs       = 0
  LET v_tot_aportacion = 0

  LET v_encabezado = " "
  CALL v_ch_arch_salida.write([v_encabezado])
      
  LET v_titulos = "FECHA REGISTRO |FOLIO REGISTRO |FOLIO LIQUIDA |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |AIVS |APORTACIÓN |FOLIO LIQUIDA AJUSTE OPERATIVO |STATUS |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])
  
  FOREACH cur_arch_apo_sub_nss INTO v_f_liquida,
                                    arr_info_apo[v_indice].folio_registro,
                                    arr_info_apo[v_indice].folio_liquida,
                                    arr_info_apo[v_indice].periodo_pago,
                                    arr_info_apo[v_indice].f_pago,	     
                                    arr_info_apo[v_indice].folio_sua,	        
                                    arr_info_apo[v_indice].nrp,
                                    arr_info_apo[v_indice].aiv_ap_pat,
                                    arr_info_apo[v_indice].imp_ap_pat,
                                    arr_info_apo[v_indice].folio_liquida_ao,
                                    v_estado                  --estado                                                      

    LET arr_info_apo[v_indice].f_registro = v_f_liquida
                                    
    SELECT UNIQUE(co.desc_edo_apo_sub)
    INTO   v_desc_estado
    FROM   cat_edo_ap_subsecuente co
    WHERE  co.cod_edo_apo_sub = v_estado

    LET arr_info_apo[v_indice].estado = v_estado, " - ",v_desc_estado  

    LET v_interface = 'AS-DUP'
                          
    LET v_detalle = arr_info_apo[v_indice].f_registro USING "dd-mm-yyyy", "|",
                    arr_info_apo[v_indice].folio_registro, "|",
                    arr_info_apo[v_indice].folio_liquida, "|",
                    arr_info_apo[v_indice].periodo_pago CLIPPED, "|",
                    arr_info_apo[v_indice].f_pago USING "dd-mm-yyyy", "|",
                    arr_info_apo[v_indice].folio_sua, "|",
                    arr_info_apo[v_indice].nrp, "|",
                    arr_info_apo[v_indice].aiv_ap_pat USING "#,###,##&.######", "|",
                    arr_info_apo[v_indice].imp_ap_pat USING "#,###,##&.##", "|",
                    arr_info_apo[v_indice].folio_liquida_ao, "|",
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
  LET v_tot_registros =  v_indice

  LET v_sumario = "TOTALES: | ",v_tot_registros," | | | | | |",
                                v_tot_aivs       USING "###,###,##&.######"," | ",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Aportaciones Subsecuentes Duplicados por NSS \n en la ruta "||v_ruta_nomarch,"information") 
  
END FUNCTION