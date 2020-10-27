################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 18/02/2020                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC41                                                    #
#Objetivo         => Programa de consulta Dispersión Cartera por Tipo de       #
#                    Crédito.                                                  #
#Fecha de Inicio  => 11/02/2020                                                #
################################################################################
-- Base que se utilizará
DATABASE safre_viv
-- Definición de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_pid                    DECIMAL(9,0)
 
  --Datos de detalle por tipo de crédito
  DEFINE a_tpo_credito       DYNAMIC ARRAY OF RECORD    
    tpo_credito              CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Datos de detalle reporte
  DEFINE a_tpo_credito_rep   DYNAMIC ARRAY OF RECORD    
    tpo_credito              CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Datos de detalle por tipo de originación 
  DEFINE a_tpo_originacion   DYNAMIC ARRAY OF RECORD
    tpo_originacion          CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Datos de detalle por tipo de originación reporte
  DEFINE a_tpo_originacion_rep DYNAMIC ARRAY OF RECORD
    tpo_originacion          CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Datos de detalle reporte devoluciones
  DEFINE a_tpo_credito_rep_dev DYNAMIC ARRAY OF RECORD    
    tpo_credito              CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Cifras totales de detalle y Totales por tipo de crédito y originación
  DEFINE
    v_sum_aivs_cred          DECIMAL(22,6),
    v_sum_apos_cred          DECIMAL(18,2),
    v_sum_amos_cred          DECIMAL(18,2),
    v_sum_regs_cred          DECIMAL(12,0),
    v_sum_aivs_orig          DECIMAL(22,6),
    v_sum_apos_orig          DECIMAL(18,2),
    v_sum_amos_orig          DECIMAL(18,2),
    v_sum_regs_orig          DECIMAL(12,0)

    {v_tot_detalle            DECIMAL(22,6),
    v_tot_detalle_rep        DECIMAL(22,6),
    v_tot_detalle_rep_dev    DECIMAL(22,6),
    v_tot_totales            DECIMAL(22,6)}    

  DEFINE v_respuesta         SMALLINT
  DEFINE v_proceso_cod       SMALLINT --Código del proceso

  DEFINE arr_fac_apo_sub     DYNAMIC ARRAY OF RECORD 
    id_dis_interface_ef      DECIMAL(9,0),
    id_derechohabiente       DECIMAL(9,0),
    folio_sua                DECIMAL(6,0),
    periodo_pago             CHAR(6),
    f_pago                   DATE,
    nrp                      CHAR(11),
    ind_liquidacion          SMALLINT,
    folio_liquida            DECIMAL(9,0),
    f_liquida                DATE,
    num_crd_ifv              DECIMAL(10,0),
    imp_ap_pat               DECIMAL(12,2),
    aiv_ap_pat               DECIMAL(22,6),
    tpo_credito              SMALLINT,
    cve_ent_financiera       SMALLINT,
    num_ctr_int_ef           CHAR(18),
    concepto                 SMALLINT,
    id_ctr_transaccion       DECIMAL(9,0),
    folio_transaccion        DECIMAL(9,0),
    f_transaccion            DATE,
    folio_factura			 DECIMAL(9,0),
    f_factura			     DATE,
    estado                   SMALLINT
  END RECORD

  DEFINE 
    v_folio_disp             DECIMAL(9,0),
    p_programa               CHAR(10),  
    r_bandera                SMALLINT,
    r_nom_archivo            CHAR(40)  

  DEFINE 
    l_comando                STRING,
    v_ruta_ejecutable        CHAR(40),
    v_ruta_listados          CHAR(40),
    v_qwery_ibx              STRING, 
    v_mensaje                STRING    

  DEFINE 
    v_tot_registros          DECIMAL(9,0),  --Total de registros
    v_tot_aivs               DECIMAL(22,6), --Total de AIVS
    v_tot_aportacion         DECIMAL(12,2), --Total de aportaciones
    v_tot_amortizacion       DECIMAL(12,2), --Total de amortizaciones
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING
END GLOBALS

MAIN
  --Datos de entrada
  DEFINE 
    v_folio_liquida          DECIMAL(9,0)   --Folio de liquidación
 
  DEFINE 
    bnd_consulta             SMALLINT, 
    f_ventana                ui.Window, -- Define las propìedades de la Ventana
    f_forma                  ui.Form    -- Define las propiedades de la forma    

  DEFINE 
    p_proceso_cod            SMALLINT,
    p_opera_cod              SMALLINT

  DEFINE 
    v_indice1                INTEGER, 
    v_indice2                INTEGER  
  
  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  LET bnd_consulta  = 0
  LET p_proceso_cod = 901 --Código aún no definido ???       
  LET p_opera_cod   = 1   --Código aún no definido ???

  CALL STARTLOG (g_usuario CLIPPED||".DISC41.log")

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

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISC411"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio_liquida
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_totales", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_tot_registros", 1)    --Oculta el total de registros
          CALL f_forma.setElementHidden("gr_tot_aportaciones", 1) --Oculta el total de aportaciones
                    
          NEXT FIELD v_folio_liquida
          CALL ui.interface.refresh()

        ON ACTION ACCEPT
           --Válidaciones de criterios de búsqueda
           IF v_folio_liquida IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar la fecha de liquidación.",
                              "about")
              NEXT FIELD v_folio_liquida
           END IF 
          
           CALL fn_consultar(v_folio_liquida) RETURNING v_indice1, v_indice2         
          
           IF v_indice1 >0 AND v_indice2 > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0)  --Muestra detalle de la consulta
              CALL f_forma.setElementHidden("gr_totales", 0)  --Muestra detalle de la consulta                    

              DISPLAY ARRAY a_tpo_credito TO rec_detalle.*
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE DISPLAY
                  --DISPLAY v_tot_detalle TO tot_detalle
                  --DISPLAY v_tot_totales TO tot_totales
                  DISPLAY ARRAY a_tpo_originacion TO rec_totales.*
                  ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                    BEFORE DISPLAY
                      EXIT DISPLAY
                    END DISPLAY                 
                
                    ON ACTION reporte
                       CALL fn_reporte(v_indice1, v_indice2, v_folio_liquida)

                    ON ACTION archivo
                       CALL fn_genera_archivo(v_folio_liquida)

                    ON ACTION cancelar 
                       EXIT PROGRAM
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

FUNCTION fn_consultar(p_folio_liquida)
  DEFINE p_folio_liquida     DECIMAL(9,0)   --Folio de liquidación
  DEFINE v_tpo_credito       CHAR(05)       --Clave tipo de crédito
  DEFINE v_tpo_originacion   CHAR(05)       --Clave tipo de originación

  DEFINE 
    v_entidad_financiera     CHAR(5),
    v_concepto               CHAR(5),
    v_estado                 CHAR(5), 
    v_tipo_credito           CHAR(5)

  DEFINE v_indice1           INTEGER
  DEFINE v_indice2           INTEGER

  LET v_sum_aivs_cred = 0
  LET v_sum_apos_cred = 0
  LET v_sum_amos_cred = 0
  LET v_sum_regs_cred = 0
  LET v_sum_aivs_orig = 0
  LET v_sum_apos_orig = 0
  LET v_sum_amos_orig = 0
  LET v_sum_regs_orig = 0

  -----Busqueda de Detalle agrupado por tipo de crédito
  LET g_sql_txt = "\n SELECT a.tpo_credito,         ",
                  "\n        c.desc_credito,        ",
                  "\n        SUM(b.aiv_ap_pat),     ",
                  "\n        SUM(b.imp_ap_pat),     ",
                  "\n        SUM(b.imp_am_cre),     ",
                  "\n        COUNT(*)               ",
                  "\n FROM   dis_his_transaccion a, ",
                  "\n        dis_interface_hs b,    ",
                  "\n        cat_tipo_credito c     ",
                  "\n WHERE  a.folio_liquida      = ", p_folio_liquida,
                  "\n AND    a.folio_liquida      = b.folio_liquida      ",
                  "\n AND    a.id_derechohabiente = b.id_derechohabiente ",
                  "\n AND    a.folio_sua          = b.folio_sua          ",
                  "\n AND    a.periodo_pago       = b.periodo_pago       ",
                  "\n AND    a.f_pago             = b.f_pago             ",
                  "\n AND    a.nrp                = b.nrp                ",
                  "\n AND    a.num_credito        = b.num_crd_ifv        ",
                  "\n AND    b.tipo_hs            = 0                    ",
                  "\n AND    a.tpo_credito        = c.tpo_credito        ",
                  "\n AND    a.tpo_originacion    = c.tpo_originacion    ",
                  "\n GROUP BY 1,2 ",
                  "\n ORDER BY 1   "
  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_inf FROM g_sql_txt
  DECLARE cur_sl_inf CURSOR FOR pr_sl_inf
  
  LET v_indice1     = 1
  --LET v_tot_detalle = 0.0   
  
  FOREACH cur_sl_inf INTO v_tpo_credito,
                          a_tpo_credito[v_indice1].tpo_credito,
                          a_tpo_credito[v_indice1].aivs,
                          a_tpo_credito[v_indice1].aportacion,
                          a_tpo_credito[v_indice1].amortizacion,
                          a_tpo_credito[v_indice1].total_registros
    LET a_tpo_credito[v_indice1].tpo_credito = v_tpo_credito USING "&&", '-', a_tpo_credito[v_indice1].tpo_credito CLIPPED

    LET v_sum_aivs_cred = v_sum_aivs_cred + a_tpo_credito[v_indice1].aivs
    LET v_sum_apos_cred = v_sum_apos_cred + a_tpo_credito[v_indice1].aportacion
    LET v_sum_amos_cred = v_sum_amos_cred + a_tpo_credito[v_indice1].amortizacion
    LET v_sum_regs_cred = v_sum_regs_cred + a_tpo_credito[v_indice1].total_registros
    LET v_indice1       = v_indice1 + 1
  END FOREACH
  
  CALL a_tpo_credito.deleteElement(v_indice1)
  LET v_indice1    = v_indice1 - 1  

  FREE cur_sl_inf

  -----Busqueda de Detalle agrupado por tipo de originación
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT a.tpo_originacion,     ",
                  "\n        c.originacion_desc,    ",
                  "\n        SUM(b.aiv_ap_pat),     ",
                  "\n        SUM(b.imp_ap_pat),     ",
                  "\n        SUM(b.imp_am_cre),     ",
                  "\n        COUNT(*)               ",
                  "\n FROM   dis_his_transaccion a, ",
                  "\n        dis_interface_hs b,    ",
                  "\n OUTER  cat_cre_originacion c  ",
                  "\n WHERE  a.folio_liquida      = ", p_folio_liquida,
                  "\n AND    a.folio_liquida      = b.folio_liquida      ",
                  "\n AND    a.id_derechohabiente = b.id_derechohabiente ",
                  "\n AND    a.folio_sua          = b.folio_sua          ",
                  "\n AND    a.periodo_pago       = b.periodo_pago       ",
                  "\n AND    a.f_pago             = b.f_pago             ",
                  "\n AND    a.nrp                = b.nrp                ",
                  "\n AND    a.num_credito        = b.num_crd_ifv        ",
                  "\n AND    b.tipo_hs            = 0                    ",
                  "\n AND    a.tpo_originacion    = c.tpo_originacion    ",
                  "\n GROUP BY 1,2 ",
                  "\n ORDER BY 1   "

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_tot FROM g_sql_txt
  DECLARE cur_sl_tot CURSOR FOR pr_sl_tot
  
  LET v_indice2     = 1
  LET v_concepto    = "" 
  --LET v_tot_totales = 0.0
  
  FOREACH cur_sl_tot INTO v_tpo_originacion,
                          a_tpo_originacion[v_indice2].tpo_originacion,
                          a_tpo_originacion[v_indice2].aivs,
                          a_tpo_originacion[v_indice2].aportacion,
                          a_tpo_originacion[v_indice2].amortizacion,
                          a_tpo_originacion[v_indice2].total_registros
    IF v_tpo_originacion = 0 THEN
       LET a_tpo_originacion[v_indice2].tpo_originacion = v_tpo_originacion USING "&&", '-SIN CRÉDITO'
    ELSE
       LET a_tpo_originacion[v_indice2].tpo_originacion = v_tpo_originacion USING "&&", '-', a_tpo_originacion[v_indice2].tpo_originacion CLIPPED
    END IF

    LET v_sum_aivs_orig = v_sum_aivs_orig + a_tpo_originacion[v_indice2].aivs
    LET v_sum_apos_orig = v_sum_apos_orig + a_tpo_originacion[v_indice2].aportacion
    LET v_sum_amos_orig = v_sum_amos_orig + a_tpo_originacion[v_indice2].amortizacion
    LET v_sum_regs_orig = v_sum_regs_orig + a_tpo_originacion[v_indice2].total_registros
    LET v_indice2       = v_indice2 + 1
  END FOREACH
  
  CALL a_tpo_originacion.deleteElement(v_indice2)
  LET v_indice2 = v_indice2 - 1  

  FREE cur_sl_tot

  RETURN v_indice1, v_indice2
END FUNCTION

FUNCTION fn_genera_archivo(p_folio_liquida) 
  DEFINE p_folio_liquida     DECIMAL(9,0)  --Folio de liquidación
  DEFINE v_f_liquida         DATE          --Fecha de liquidación

  DEFINE 
    v_nom_archivo            VARCHAR(40),   --Nombre del archivo de salida
    v_ruta_envio_dis         CHAR(40),
    v_ruta_nomarch           VARCHAR(100),  --Ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,          
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 INTEGER

  DEFINE arr_info_car        DYNAMIC ARRAY OF RECORD
    v_nss                    CHAR(11),
    v_id_derechohabiente     DECIMAL(9,0),
    v_tpo_credito            SMALLINT,
    v_tpo_originacion        SMALLINT,
    v_aiv_ap_pat             DECIMAL(18,6),
    v_imp_ap_pat             DECIMAL(12,2),
    v_imp_am_cre             DECIMAL(12,2),
    v_folio_sua              DECIMAL(6,0),
    v_periodo_pago           CHAR(6),
    v_f_pago                 DATE,
    v_nrp                    CHAR(11),
    v_num_crd_ifv            DECIMAL(10,0)
  END RECORD 
  
  DEFINE v_desc_credito      CHAR(30)
  DEFINE v_desc_originacion  CHAR(40) 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/car_tpocre_", v_hora

  --Se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  SELECT f_actualiza
  INTO   v_f_liquida
  FROM   glo_folio
  WHERE  folio = p_folio_liquida 

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  --Imprime encabezado del archivo
  LET v_encabezado = "DISPERSIÓN CARTERA POR TIPO DE CRÉDITO "
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = "FOLIO LIQUIDACIÓN |", p_folio_liquida
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = "FECHA LIQUIDACIÓN |", v_f_liquida
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = "NSS |TIPO CRÉDITO |TIPO ORIGINACIÓN |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |NÚMERO DE CRÉDITO|AIVS |APORTACIÓN |AMORTIZACIÓN "
  CALL v_ch_arch_salida.write([v_encabezado])

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_dis_car_cred;
  WHENEVER ERROR STOP

  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT a.nss,                 ",
                  "\n        a.id_derechohabiente,  ",
                  "\n        b.aiv_ap_pat,          ",
                  "\n        b.imp_ap_pat,          ",
                  "\n        b.imp_am_cre,          ",
                  "\n        b.folio_sua,           ",
                  "\n        b.periodo_pago,        ",
                  "\n        b.f_pago,              ",
                  "\n        b.nrp,                 ",
                  "\n        b.num_crd_ifv          ",
                  "\n FROM   dis_interface_hs b,    ",
                  "\n OUTER  afi_derechohabiente a  ",
                  "\n WHERE  b.folio_liquida      = ", p_folio_liquida,
                  "\n AND    a.id_derechohabiente = b.id_derechohabiente ",
                  "\n AND    b.tipo_hs            = 0                    ",
                  "\n INTO TEMP tmp_dis_car_cred "

  PREPARE prep_tmp_carc FROM g_sql_txt
  EXECUTE prep_tmp_carc

  UPDATE STATISTICS FOR TABLE tmp_dis_car_cred
  
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT b.nss,                 ",
                  "\n        b.id_derechohabiente,  ",
                  "\n        a.tpo_credito,         ",
                  "\n        a.tpo_originacion,     ",
                  "\n        b.aiv_ap_pat,          ",
                  "\n        b.imp_ap_pat,          ",
                  "\n        b.imp_am_cre,          ",
                  "\n        b.folio_sua,           ",
                  "\n        b.periodo_pago,        ",
                  "\n        b.f_pago,              ",
                  "\n        b.nrp,                 ",
                  "\n        b.num_crd_ifv          ",
                  "\n FROM   tmp_dis_car_cred b,    ",
                  "\n OUTER  dis_his_transaccion a  ",
                  "\n WHERE  a.folio_liquida      = ",p_folio_liquida,
                  "\n AND    a.id_derechohabiente = b.id_derechohabiente ",
                  "\n AND    a.folio_sua          = b.folio_sua          ",
                  "\n AND    a.periodo_pago       = b.periodo_pago       ",
                  "\n AND    a.f_pago             = b.f_pago             ",
                  "\n AND    a.nrp                = b.nrp                ",
                  "\n AND    a.num_credito        = b.num_crd_ifv        ",
                  "\n ORDER BY 3,4,1                                     "

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"  
      
  PREPARE pr_sl_inf_arc FROM g_sql_txt
  DECLARE cur_sl_inf_arc CURSOR FOR pr_sl_inf_arc
  
  LET v_indice           = 1
  LET v_tot_aivs         = 0
  LET v_tot_aportacion   = 0
  LET v_tot_amortizacion = 0
  
  FOREACH cur_sl_inf_arc INTO arr_info_car[v_indice].*

    INITIALIZE v_desc_credito     TO NULL
    INITIALIZE v_desc_originacion TO NULL
    
    SELECT a.desc_credito
    INTO   v_desc_credito 
    FROM   cat_tipo_credito a
    WHERE  a.tpo_credito     = arr_info_car[v_indice].v_tpo_credito
    AND    a.tpo_originacion = arr_info_car[v_indice].v_tpo_originacion

    SELECT a.originacion_desc
    INTO   v_desc_originacion
    FROM   cat_cre_originacion a
    WHERE  a.tpo_originacion = arr_info_car[v_indice].v_tpo_originacion

    LET v_detalle = arr_info_car[v_indice].v_nss, "|",
                    arr_info_car[v_indice].v_tpo_credito CLIPPED, ' - ',v_desc_credito CLIPPED, "|",
                    arr_info_car[v_indice].v_tpo_originacion CLIPPED, ' - ',v_desc_originacion CLIPPED, "|",
                    arr_info_car[v_indice].v_periodo_pago, "|",
                    arr_info_car[v_indice].v_f_pago USING "dd-mm-yyyy", "|",
                    arr_info_car[v_indice].v_folio_sua, "|",
                    arr_info_car[v_indice].v_nrp, "|",
                    arr_info_car[v_indice].v_num_crd_ifv USING "&&&&&&&&&&", "|",
                    arr_info_car[v_indice].v_aiv_ap_pat USING "#,###,##&.######", "|",
                    arr_info_car[v_indice].v_imp_ap_pat USING "#,###,##&.##", "|",
                    arr_info_car[v_indice].v_imp_am_cre USING "#,###,##&.##", "|"

    CALL v_ch_arch_salida.write([v_detalle])

    LET v_tot_aivs         = v_tot_aivs         + arr_info_car[v_indice].v_aiv_ap_pat
    LET v_tot_aportacion   = v_tot_aportacion   + arr_info_car[v_indice].v_imp_ap_pat
    LET v_tot_amortizacion = v_tot_amortizacion + arr_info_car[v_indice].v_imp_am_cre
    LET v_indice           = v_indice           + 1 
  END FOREACH

  FREE cur_sl_inf_arc
  
  CALL arr_info_car.deleteElement(v_indice)
  LET v_indice  = v_indice - 1 

  LET v_sumario = "TOTALES: | | | | | | | |",
                                v_tot_aivs USING "###,###,##&.######",     "|",
                                v_tot_aportacion USING "###,###,##&.##",   "|",
                                v_tot_amortizacion USING "###,###,##&.##", "|"
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Dispersión a Cartera agrupado por Tipo de Crédito \n en la ruta "||v_ruta_nomarch,"information") 
END FUNCTION

FUNCTION fn_reporte(v_indice1, v_indice2, v_folio_liquida)
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_rep_indice        INTEGER
  DEFINE v_tot_reporte       INTEGER 
  DEFINE v_indice1           INTEGER
  DEFINE v_indice2           INTEGER
  DEFINE v_folio_liquida     DECIMAL(9,0)
  DEFINE v_encabezado        SMALLINT -- 1 detalle, 2 totales por tipo de crédito y concepto

  DEFINE v_indice_rep_cre    INTEGER
  DEFINE v_indice_rep_ori    INTEGER

  --Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISC411.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  CALL a_tpo_credito_rep.clear()
  CALL a_tpo_credito_rep_dev.clear()

  LET v_indice_rep_cre      = 1
  LET v_indice_rep_ori      = 1

  FOR v_rep_indice = 1 TO a_tpo_credito.getLength()
      LET a_tpo_credito_rep[v_indice_rep_cre].* = a_tpo_credito[v_indice_rep_cre].*
      LET v_indice_rep_cre                      = v_indice_rep_cre + 1
  END FOR

  IF a_tpo_credito_rep.getLength() > 0 THEN
     IF a_tpo_credito_rep[a_tpo_credito_rep.getLength()].tpo_credito IS NULL THEN
        CALL a_tpo_credito_rep.deleteElement(v_indice_rep_cre)
     END IF
     LET v_indice_rep_cre = v_indice_rep_cre - 1
  END IF

  FOR v_rep_indice = 1 TO a_tpo_credito.getLength()
      LET a_tpo_originacion_rep[v_indice_rep_ori].* = a_tpo_originacion[v_indice_rep_ori].*
      LET v_indice_rep_ori                          = v_indice_rep_ori + 1
  END FOR

  IF a_tpo_originacion_rep.getLength() > 0 THEN
     IF a_tpo_originacion_rep[a_tpo_originacion_rep.getLength()].tpo_originacion IS NULL THEN
        CALL a_tpo_originacion_rep.deleteElement(v_indice_rep_ori)
     END IF
     LET v_indice_rep_ori = v_indice_rep_ori - 1
  END IF

  START REPORT rep_con_disp TO XML HANDLER manejador_rpt
    LET v_tot_reporte = 1
    LET v_encabezado  = 1

    FOR v_rep_indice = 1 TO a_tpo_credito_rep.getLength()
        OUTPUT TO REPORT rep_con_disp(a_tpo_credito_rep[v_rep_indice].*,
                                      a_tpo_credito_rep_dev[v_rep_indice].*,
                                      a_tpo_originacion_rep[v_rep_indice].*,
                                      v_sum_aivs_cred,
                                      v_sum_apos_cred,
                                      v_sum_amos_cred,
                                      v_sum_regs_cred,
                                      v_sum_aivs_orig,
                                      v_sum_apos_orig,
                                      v_sum_amos_orig,
                                      v_sum_regs_orig,
                                      v_encabezado, 
                                      v_tot_reporte, 
                                      v_indice1,
                                      v_folio_liquida)
    END FOR

    LET v_tot_reporte = 2
    LET v_encabezado  = 2

    DISPLAY "a_tpo_credito_rep_dev.getLength() : ",a_tpo_credito_rep_dev.getLength()
    IF a_tpo_credito_rep_dev.getLength() >= 1 THEN
       FOR v_rep_indice = 1 TO a_tpo_credito_rep_dev.getLength()
           OUTPUT TO REPORT rep_con_disp(a_tpo_credito_rep[v_rep_indice].*, 
                                         a_tpo_credito_rep_dev[v_rep_indice].*, 
                                         a_tpo_originacion_rep[v_rep_indice].*, 
                                         v_sum_aivs_cred,
                                         v_sum_apos_cred,
                                         v_sum_amos_cred,
                                         v_sum_regs_cred,
                                         v_sum_aivs_orig,
                                         v_sum_apos_orig,
                                         v_sum_amos_orig,
                                         v_sum_regs_orig,
                                         v_encabezado, 
                                         v_tot_reporte, 
                                         v_indice1,
                                         v_folio_liquida)
       END FOR
    END IF

    LET v_tot_reporte = 3
    LET v_encabezado  = 3

    FOR v_rep_indice = 1 TO a_tpo_originacion_rep.getLength()
        OUTPUT TO REPORT rep_con_disp(a_tpo_credito_rep[v_rep_indice].*, 
                                      a_tpo_credito_rep_dev[v_rep_indice].*, 
                                      a_tpo_originacion_rep[v_rep_indice].*, 
                                      v_sum_aivs_cred,
                                      v_sum_apos_cred,
                                      v_sum_amos_cred,
                                      v_sum_regs_cred,
                                      v_sum_aivs_orig,
                                      v_sum_apos_orig,
                                      v_sum_amos_orig,
                                      v_sum_regs_orig,
                                      v_encabezado, 
                                      v_tot_reporte, 
                                      v_indice1,
                                      v_folio_liquida)
    END FOR
  FINISH REPORT rep_con_disp
END FUNCTION

#Objetivo: Estructura reporte de Dispersión Cartera por Tipo de Crédito y Orig
REPORT rep_con_disp(a_tpo_cred_rep, 
                    a_tpo_cred_rep_dev,
                    a_tpo_orig_rep, 
                    r_sum_aivs_cred,
                    r_sum_apos_cred,
                    r_sum_amos_cred,
                    r_sum_regs_cred,
                    r_sum_aivs_orig,
                    r_sum_apos_orig,
                    r_sum_amos_orig,
                    r_sum_regs_orig,
                    v_encabezado, 
                    v_tot_reporte, 
                    v_total_registros,
                    v_folio_liquida)

  --Datos de detalle por tipo de crédito
  DEFINE a_tpo_cred_rep      RECORD    
    tpo_credito              CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Datos de detalle reporte devoluciones
  DEFINE a_tpo_cred_rep_dev  RECORD    
    tpo_credito              CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Datos de detalle por tipo de originación 
  DEFINE a_tpo_orig_rep      RECORD
    tpo_originacion          CHAR(30),
    aivs                     DECIMAL(22,6),
    aportacion               DECIMAL(18,2),
    amortizacion             DECIMAL(18,2),
    total_registros          DECIMAL(12,0)
  END RECORD

  --Cifras totales de detalle y Totales por tipo de crédito y originación
  DEFINE 
    r_sum_aivs_cred          DECIMAL(22,6),
    r_sum_apos_cred          DECIMAL(18,2),
    r_sum_amos_cred          DECIMAL(18,2),
    r_sum_regs_cred          DECIMAL(12,0),
    r_sum_aivs_orig          DECIMAL(22,6),
    r_sum_apos_orig          DECIMAL(18,2),
    r_sum_amos_orig          DECIMAL(18,2),
    r_sum_regs_orig          DECIMAL(12,0)
         
  DEFINE 
    v_fecha_consulta         DATE, --Fecha de proceso 
    v_tot_reporte            INTEGER, 
    v_encabezado             INTEGER, 
    v_total_registros        INTEGER,
    v_folio_liquida          DECIMAL(9,0)
    
  FORMAT
    FIRST PAGE HEADER      
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      PRINTX g_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_folio_liquida

    PAGE HEADER            
      PRINTX g_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_folio_liquida

    BEFORE GROUP OF v_tot_reporte
      PRINTX v_total_registros
      PRINTX v_tot_reporte
      
      PRINTX r_sum_aivs_cred 
      PRINTX r_sum_apos_cred 
      PRINTX r_sum_amos_cred 
      PRINTX r_sum_regs_cred 
      PRINTX r_sum_aivs_orig 
      PRINTX r_sum_apos_orig 
      PRINTX r_sum_amos_orig 
      PRINTX r_sum_regs_orig 

    ON EVERY ROW
      PRINTX a_tpo_cred_rep.tpo_credito
      PRINTX a_tpo_cred_rep.aivs 
      PRINTX a_tpo_cred_rep.aportacion 
      PRINTX a_tpo_cred_rep.amortizacion
      PRINTX a_tpo_cred_rep.total_registros

      PRINTX a_tpo_cred_rep_dev.tpo_credito
      PRINTX a_tpo_cred_rep_dev.aivs 
      PRINTX a_tpo_cred_rep_dev.aportacion 
      PRINTX a_tpo_cred_rep_dev.amortizacion 
      PRINTX a_tpo_cred_rep_dev.total_registros 

      PRINTX a_tpo_orig_rep.tpo_originacion
      PRINTX a_tpo_orig_rep.aivs 
      PRINTX a_tpo_orig_rep.aportacion 
      PRINTX a_tpo_orig_rep.amortizacion 
      PRINTX a_tpo_orig_rep.total_registros

END REPORT