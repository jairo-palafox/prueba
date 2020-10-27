################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 10/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC25                                                    #
#Objetivo         => Programa de consulta de Facturación por Folio             #
#Fecha de Inicio  => 29/06/2015                                                #
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
 
  --Datos de detalle
  DEFINE a_fac_apo_sub       DYNAMIC ARRAY OF RECORD    
    entidad_financiera       CHAR(65),
    cuenta_bancaria          CHAR(40),    
    documento_fico 		     CHAR(10),
    concepto			     CHAR(50),
    estado                   CHAR(50),
    monto                    DECIMAL(22,6)    
  END RECORD  

  --Datos de detalle reporte
  DEFINE a_fac_apo_sub_rep   DYNAMIC ARRAY OF RECORD    
    entidad_financiera       CHAR(65),
    cuenta_bancaria          CHAR(40),    
    documento_fico 		     CHAR(10),
    concepto			     CHAR(50),
    estado                   CHAR(50),
    monto                    DECIMAL(22,6)    
  END RECORD

  --Datos de detalle reporte devoluciones
  DEFINE a_fac_apo_sub_rep_dev  DYNAMIC ARRAY OF RECORD    
    entidad_financiera       CHAR(65),
    cuenta_bancaria          CHAR(40),    
    documento_fico 		     CHAR(10),
    concepto			     CHAR(50),
    estado                   CHAR(50),
    monto                    DECIMAL(22,6)    
  END RECORD

  --Totales 
  DEFINE a_totales_con       DYNAMIC ARRAY OF RECORD     
    tpo_credito              CHAR(50),     --Tipo de crédito    
    concepto_tot             CHAR(50),     --Concepto
    monto_tot                DECIMAL(22,6) --Total monto
  END RECORD

  --Cifras totales de detalle y Totales por tipo de crédito y concepto
  DEFINE 
    v_tot_detalle            DECIMAL(22,6),
    v_tot_detalle_rep        DECIMAL(22,6),
    v_tot_detalle_rep_dev    DECIMAL(22,6),
    v_tot_totales            DECIMAL(22,6)    

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

  DEFINE v_folio_disp        DECIMAL(9,0),
         p_programa          CHAR(10),  
         r_bandera           SMALLINT,
         r_nom_archivo       CHAR(40)  

  DEFINE l_comando           STRING,
         v_ruta_ejecutable   CHAR(40),
         v_ruta_listados     CHAR(40),
         v_qwery_ibx         STRING, 
         v_mensaje           STRING    

   DEFINE 
    v_tot_registros          DECIMAL(9,0),  --Total de registros
    v_tot_aivs               DECIMAL(22,6), --Total de AIVS
    v_tot_aportacion         DECIMAL(12,2), --Total de aportaciones
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  --Datos de entrada
  DEFINE v_folio_transaccion DECIMAL(9,0),  --Folio de transacción
         v_f_transaccion_ini DATE,          --Fecha de transacción inicial
         v_f_transaccion_fin DATE,          --Fecha de transacción final 
         v_folio_facturacion DECIMAL(9,0),  --Folio de facturación, 
         v_f_facturacion_ini DATE,          --Fecha de facturacion inicial
         v_f_facturacion_fin DATE,          --Fecha de facturacion final
         v_tipo_credito      SMALLINT,      --Tipo de crédito
         v_estado            SMALLINT       --Estado 
 
  DEFINE bnd_consulta        SMALLINT, 
         f_ventana           ui.Window, -- Define las propìedades de la Ventana
         f_forma             ui.Form    -- Define las propiedades de la forma    

  DEFINE p_proceso_cod       SMALLINT,
         p_opera_cod         SMALLINT

  DEFINE v_indice1           INTEGER, 
         v_indice2           INTEGER  
  
  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  LET bnd_consulta  = 0
  LET p_proceso_cod = 3905 --Código aún no definido ???       
  LET p_opera_cod   = 1    --Código aún no definido ???

  CALL STARTLOG (g_usuario CLIPPED||".DISC25.log")

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

  OPEN WINDOW w1 WITH FORM "DISC251"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, 
                    v_folio_facturacion, v_f_facturacion_ini, v_f_facturacion_fin, 
                    v_tipo_credito, v_estado 
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_totales", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_tot_registros", 1)    --Oculta el total de registros
          CALL f_forma.setElementHidden("gr_tot_aportaciones", 1) --Oculta el total de aportaciones
          

          CALL f_llena_tipo_credito()
          CALL f_llena_estado()
          
          NEXT FIELD v_folio_transaccion
          CALL ui.interface.refresh()

        ON ACTION ACCEPT
           --Válidaciones de criterios de búsqueda
           IF v_folio_transaccion IS NULL AND v_f_transaccion_ini IS NULL AND v_f_transaccion_fin IS NULL AND  
              v_folio_facturacion IS NULL AND v_f_facturacion_ini IS NULL AND v_f_facturacion_fin IS NULL AND  
              v_tipo_credito IS NULL AND v_estado IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "Debe seleccionar algún criterio de búsqueda.",
                              "about")
              NEXT FIELD v_tipo_credito  
           END IF 
          
           IF v_f_transaccion_ini > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La fecha de Transacción Inicial no puede ser mayor a la Fecha Actual.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF    

           IF v_f_transaccion_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Transacción Final no puede ser mayor a la Fecha Actual.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 
          
           IF v_f_transaccion_ini IS NOT NULL AND v_f_transaccion_fin IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Transacción Final.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 

           IF v_f_transaccion_fin IS NOT NULL AND v_f_transaccion_ini IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Transacción Inicial.",
                              "about")
              NEXT FIELD v_f_liquida_fin
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
              NEXT FIELD v_f_liquida_ini
           END IF    

           IF v_f_facturacion_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Facturación Final no puede ser mayor a la Fecha Actual.",
                              "about")
              NEXT FIELD v_f_liquida_fin
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
              NEXT FIELD v_f_transaccion_fin 
           END IF       

           CALL fn_consultar(v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, 
                             v_folio_facturacion, v_f_facturacion_ini, v_f_facturacion_fin, 
                             v_tipo_credito, v_estado) RETURNING v_indice1, v_indice2         
          
           IF v_indice1 >0 AND v_indice2 > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0)  --Muestra detalle de la consulta
              CALL f_forma.setElementHidden("gr_totales", 0)  --Muestra detalle de la consulta                    

              DISPLAY ARRAY a_fac_apo_sub TO rec_detalle.*
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE DISPLAY
                  --DISPLAY v_tot_detalle TO tot_detalle
                  --DISPLAY v_tot_totales TO tot_totales
                  DISPLAY ARRAY a_totales_con TO rec_totales.*
                  ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                    BEFORE DISPLAY
                      EXIT DISPLAY
                    END DISPLAY                 
                
                    ON ACTION reporte
                       CALL fn_reporte(v_indice1, v_indice2)

                    ON ACTION archivo
                       CALL fn_genera_archivo(v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, 
                                              v_folio_facturacion, v_f_facturacion_ini, v_f_facturacion_fin, 
                                              v_tipo_credito, v_estado)

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

FUNCTION f_llena_tipo_credito()
  DEFINE v_cbx_tipo_credito  ui.ComboBox -- Combo de tipo de crédito
  DEFINE v_tipo_credito      SMALLINT,
         v_desc_credito      CHAR(30)

  LET v_cbx_tipo_credito = ui.ComboBox.forName("formonly.v_tipo_credito")

  CALL v_cbx_tipo_credito.clear()

  LET g_sql_txt = "\n SELECT a.tpo_credito_ocg, a.desc_credito_ocg ",
                  "\n FROM   cat_tpo_credito_ocg a ",
                  "\n WHERE  a.tpo_credito_ocg IN (2,3,5) ",
                  "\n AND    a.ind_activo       = 1 ",
                  "\n ORDER BY a.tpo_credito_ocg "

  PREPARE pr_sl_tc FROM g_sql_txt
  DECLARE cur_sl_tc CURSOR FOR pr_sl_tc

  FOREACH cur_sl_tc INTO v_tipo_credito, 
                         v_desc_credito 
    CALL v_cbx_tipo_credito.addItem(v_tipo_credito, v_tipo_credito||' - '||v_desc_credito)                         
  END FOREACH  
  
  FREE cur_sl_tc
END FUNCTION 

FUNCTION f_llena_estado()
  DEFINE v_cbx_estado        ui.ComboBox -- Combo de tipo de crédito
  DEFINE v_cod_edo_aps       SMALLINT,
         v_desc_edo_aps      CHAR(30)

  LET v_cbx_estado = ui.ComboBox.forName("formonly.v_estado")

  CALL v_cbx_estado.clear()

  LET g_sql_txt = "\n SELECT cod_edo_aps, desc_edo_aps ",
                  "\n FROM   cat_edo_aps ",    
                  "\n WHERE  cod_edo_aps in (20, 30, 55, 60, 70) ",  
                  "\n ORDER BY cod_edo_aps "

  PREPARE pr_sl_edo FROM g_sql_txt
  DECLARE cur_sl_edo CURSOR FOR pr_sl_edo

  FOREACH cur_sl_edo INTO v_cod_edo_aps, 
                         v_desc_edo_aps
    CALL v_cbx_estado.addItem(v_cod_edo_aps, v_cod_edo_aps||' - '||v_desc_edo_aps)                         
  END FOREACH  
  
  FREE cur_sl_edo
END FUNCTION

FUNCTION fn_consultar(p_folio_transaccion, p_f_transaccion_ini, p_f_transaccion_fin, 
                      p_folio_facturacion, p_f_facturacion_ini, p_f_facturacion_fin, 
                      p_tipo_credito, p_estado)
  DEFINE p_folio_transaccion DECIMAL(9,0),  --Folio de transacción
         p_f_transaccion_ini DATE,          --Fecha de transacción inicial
         p_f_transaccion_fin DATE,          --Fecha de transacción final 
         p_folio_facturacion DECIMAL(9,0),  --Folio de facturación 
         p_f_facturacion_ini DATE,          --Fecha de facturacion inicial
         p_f_facturacion_fin DATE,          --Fecha de facturacion final
         p_tipo_credito      SMALLINT,      --Tipo de crédito
         p_estado            SMALLINT       --Estado 

  DEFINE v_entidad_financiera CHAR(5),
         v_concepto          CHAR(5),
         v_estado            CHAR(5), 
         v_tipo_credito      CHAR(5)

  DEFINE v_indice1           INTEGER
  DEFINE v_indice2           INTEGER

  -----Busqueda de Detalle
  LET g_sql_txt = "\n SELECT dc.cve_ent_financiera,      ",
                  "\n        cta.ent_financiera_desc,    ",
                  "\n        cta.clabe,                  ", 
                  "\n        fac.num_poliza,             ", 
                  "\n        dc.concepto,                ",
                  "\n        co.desc_concepto_ocg,       ",
                  "\n        dc.estado,                  ", 
                  "\n        ce.desc_edo_aps,            ", 
                  "\n        SUM(imp_ap_pat) AS monto    ",       
                  "\n FROM   dis_ctr_aps_tns dc,         ",
                  "\n        OUTER cat_edo_aps ce,       ",
                  "\n        OUTER cat_cta_cnt_ocg cta,  ",
                  "\n        OUTER cat_concepto_ocg co,  ",
                  "\n        OUTER dis_ctr_factura_aps fac                    ",
                  "\n WHERE  dc.estado             = ce.cod_edo_aps           ",                  
                  "\n AND    dc.cve_ent_financiera = cta.cve_ent_financiera   ",
                  "\n AND    dc.tpo_credito	       = cta.tpo_credito          ",
                  "\n AND    dc.concepto           = co.cod_concepto_ocg      ",
                  "\n AND    dc.folio_factura      = fac.folio_factura        ",
                  "\n AND    dc.cve_ent_financiera = fac.cve_ent_financiera   ",
                  "\n AND    dc.tpo_credito	       = fac.tpo_credito          "
                                  
  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_transaccion = ",p_folio_transaccion
  END IF

  IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                               "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'"
  END IF

  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_factura = ",p_folio_facturacion
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.f_factura >= '",p_f_facturacion_ini,"'",
                               "\n AND dc.f_factura <= '",p_f_facturacion_fin,"'"
  END IF  

  IF p_tipo_credito IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = ",p_tipo_credito                    
  END IF

  IF p_estado IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND dc.estado = ",p_estado
  ELSE
     LET g_sql_txt = g_sql_txt,"\n AND dc.estado in (20,30,55,60,70) "
  END IF
                  
  LET g_sql_txt = g_sql_txt,"\n GROUP BY 1,2,3,4,5,6,7,8 ",
                            "\n ORDER BY dc.cve_ent_financiera, dc.concepto, dc.estado, fac.num_poliza"
  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_inf FROM g_sql_txt
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
    LET a_fac_apo_sub[v_indice1].estado             = v_estado   CLIPPED,' - ',a_fac_apo_sub[v_indice1].estado   CLIPPED

    LET v_tot_detalle = v_tot_detalle + a_fac_apo_sub[v_indice1].monto                         
    LET v_indice1     = v_indice1     + 1

  END FOREACH
  
  CALL a_fac_apo_sub.deleteElement(v_indice1)
  LET v_indice1    = v_indice1 - 1  

  FREE cur_sl_inf

  ----- Busqueda de Totales
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT dc.tpo_credito, ",
                  "\n        dc.concepto, ",
                  "\n        co.desc_concepto_ocg, ",
                  "\n        SUM (dc.imp_ap_pat) ",
                  "\n FROM   dis_ctr_aps_tns dc, ",
                  "\n        OUTER cat_concepto_ocg co ",
                  "\n WHERE  dc.concepto = co.cod_concepto_ocg "

  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_transaccion = ",p_folio_transaccion
  END IF

  IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                               "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'"
  END IF  

  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_factura = ",p_folio_facturacion
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.f_factura >= '",p_f_facturacion_ini,"'",
                               "\n AND dc.f_factura <= '",p_f_facturacion_fin,"'"
  END IF  

  IF p_tipo_credito IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.tpo_credito = ",p_tipo_credito                    
  END IF

  IF p_estado IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND dc.estado = ",p_estado
  ELSE
     LET g_sql_txt = g_sql_txt,"\n AND dc.estado IN (20,30,55,60,70) "
  END IF
                  
  LET g_sql_txt = g_sql_txt,"\n GROUP BY dc.tpo_credito, dc.concepto, desc_concepto_ocg ",
                            "\n ORDER BY dc.tpo_credito, dc.concepto"

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_tot FROM g_sql_txt
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

    LET a_totales_con[v_indice2].tpo_credito  = v_tipo_credito CLIPPED,' - ',a_totales_con[v_indice2].tpo_credito  CLIPPED  
    LET a_totales_con[v_indice2].concepto_tot = v_concepto     CLIPPED,' - ',a_totales_con[v_indice2].concepto_tot CLIPPED  

    LET v_tot_totales = v_tot_totales + a_totales_con[v_indice2].monto_tot  
    LET v_indice2     = v_indice2     + 1
  END FOREACH
  
  CALL a_totales_con.deleteElement(v_indice2)
  LET v_indice2 = v_indice2 - 1  

  FREE cur_sl_tot

  RETURN v_indice1, v_indice2
END FUNCTION

FUNCTION fn_genera_archivo(p_folio_transaccion, p_f_transaccion_ini, p_f_transaccion_fin, 
                           p_folio_facturacion, p_f_facturacion_ini, p_f_facturacion_fin, 
                           p_tipo_credito, p_estado) 
  DEFINE p_folio_transaccion DECIMAL(9,0),  --Folio de transacción
         p_f_transaccion_ini DATE,          --Fecha de transacción inicial
         p_f_transaccion_fin DATE,          --Fecha de transacción final 
         p_folio_facturacion DECIMAL(9,0),  --Folio de facturación 
         p_f_facturacion_ini DATE,          --Fecha de facturacion inicial
         p_f_facturacion_fin DATE,          --Fecha de facturacion final
         p_tipo_credito      SMALLINT,      --Tipo de crédito
         p_estado            SMALLINT       --Estado

  DEFINE v_nom_archivo       VARCHAR(40),   --Nombre del archivo de salida
         v_ruta_envio_dis    CHAR(40),
         v_ruta_nomarch      VARCHAR(100),  --Ruta y nombre del archivo de salida
         v_ch_arch_salida    BASE.CHANNEL,          
         v_comando_dos       STRING,
         v_encabezado        STRING,
         v_detalle           STRING,
         v_sumario           STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 INTEGER

  DEFINE arr_info_apo        DYNAMIC ARRAY OF RECORD
    v_nss                    CHAR(11),
    v_periodo_pago           CHAR(6),
    v_f_pago                 DATE, 
    v_folio_sua              DECIMAL(6,0),
    v_nrp                    CHAR(11),
    v_imp_ap_pat             DECIMAL(12,2),
    v_aiv_ap_pat             DECIMAL(22,6), 
    v_cve_ent_financiera     SMALLINT,
    v_des_ent_fin            CHAR(60),
    v_tpo_credito            SMALLINT,
    v_des_tpo_credito        CHAR(30),  
    v_concepto               SMALLINT,
    v_des_concepto           CHAR(50),
    v_folio_transaccion      DECIMAL(9,0),    
    v_f_transaccion          DATE,
    v_folio_factura          DECIMAL(9,0),
    v_f_factura              DATE,
    v_estado                 SMALLINT,
    v_desc_edo_aps           CHAR(40),
    v_f_archivo              DATE, 
    v_interface              CHAR(2)
  END RECORD 
  
  DEFINE v_desc_credito      CHAR(20) 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  LET v_nom_archivo   = "/fact_folio_", v_hora

  --Se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  --Imprime encabezado del archivo
  LET v_encabezado = "NSS |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |AIVS |APORTACIÓN |ENTIDAD FINANCIERA |CONCEPTO PAGO |TRANSACCIÓN |FOLIO TRANSACCIÓN |FECHA TRANSACCIÓN |FOLIO FACTURA |FECHA FACTURA |STATUS |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_encabezado])

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_afi_disc25;
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

  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND H.folio_transaccion = ",p_folio_transaccion
  END IF

  IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND H.f_transaccion >= '",p_f_transaccion_ini,"'",
                               "\n AND H.f_transaccion <= '",p_f_transaccion_fin,"'"
  END IF  

  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND H.folio_factura = ",p_folio_facturacion
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND H.f_factura >= '",p_f_facturacion_ini,"'",
                               "\n AND H.f_factura <= '",p_f_facturacion_fin,"'"
  END IF  

  IF p_tipo_credito IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND H.tpo_credito = ",p_tipo_credito                    
  END IF

  IF p_estado IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND H.estado = ",p_estado
  ELSE
     LET g_sql_txt = g_sql_txt,"\n AND H.estado IN (20,30,55,60,70) "
  END IF

  LET g_sql_txt = g_sql_txt,"\n INTO TEMP tmp_afi_disc25 "

  PREPARE prep_tmp_afi2 FROM g_sql_txt
  EXECUTE prep_tmp_afi2

  UPDATE STATISTICS FOR TABLE tmp_afi_disc25
  
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT ad.nss, ", 
                  "\n        dc.periodo_pago, ",
                  "\n        dc.f_pago, ",
                  "\n        dc.folio_sua, ",
                  "\n        dc.nrp, ",
                  "\n        dc.imp_ap_pat, ",
                  "\n        dc.aiv_ap_pat, ",
                  "\n        dc.cve_ent_financiera, ",
                  "\n        ef.ent_financiera_desc, ",
                  "\n        dc.tpo_credito, ",
                  "\n        dc.concepto, ",
                  "\n        co.desc_concepto_ocg, ",
                  "\n        dc.folio_transaccion, ",
                  "\n        dc.f_transaccion, ", 
                  "\n        dc.folio_factura, ", 
                  "\n        dc.f_factura, ",
                  "\n        dc.estado, ", 
                  "\n        ce.desc_edo_aps, ",
                  "\n        TODAY AS f_archivo, ",
                  "\n        'AS' AS interface ",
                  --"\n FROM   afi_derechohabiente ad, ",
                  --"\n        dis_ctr_aps_tns dc, ",
                  "\n FROM   dis_ctr_aps_tns dc, ",
                  "\n        tmp_afi_disc25 ad, ",
                  "\n OUTER  cat_edo_aps ce, ",
                  "\n OUTER  cat_cta_cnt_ocg ef, ",
                  "\n OUTER  cat_concepto_ocg co ",
                  "\n WHERE  ad.id_dis_interface_ef = dc.id_dis_interface_ef ",
                  "\n AND    ad.id_derechohabiente  = dc.id_derechohabiente ",
                  "\n AND    dc.estado              = ce.cod_edo_aps ",
                  "\n AND    dc.cve_ent_financiera  = ef.cve_ent_financiera ",
                  "\n AND    dc.tpo_credito	        = ef.tpo_credito ",
                  "\n AND    dc.concepto            = co.cod_concepto_ocg "
  
  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_transaccion = ",p_folio_transaccion
  END IF

  IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                               "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'"
  END IF  

  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_factura = ",p_folio_facturacion
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.f_factura >= '",p_f_facturacion_ini,"'",
                               "\n AND dc.f_factura <= '",p_f_facturacion_fin,"'"
  END IF  

  IF p_tipo_credito IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = ",p_tipo_credito                    
  END IF

  IF p_estado IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND dc.estado = ",p_estado
  ELSE
     LET g_sql_txt = g_sql_txt,"\n AND dc.estado in (20,30,55,60,70) "
  END IF

  LET g_sql_txt = g_sql_txt,"\n ORDER BY dc.folio_transaccion, dc.f_transaccion DESC, dc.folio_factura, dc.f_factura DESC, ad.nss, dc.periodo_pago DESC, dc.f_pago DESC "

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"  
      
  PREPARE pr_sl_inf_arc FROM g_sql_txt
  DECLARE cur_sl_inf_arc CURSOR FOR pr_sl_inf_arc
  
  LET v_indice         = 1
  LET v_tot_aivs       = 0
  LET v_tot_aportacion = 0
  
  FOREACH cur_sl_inf_arc INTO arr_info_apo[v_indice].v_nss,
                              arr_info_apo[v_indice].v_periodo_pago,
                              arr_info_apo[v_indice].v_f_pago,
                              arr_info_apo[v_indice].v_folio_sua,
                              arr_info_apo[v_indice].v_nrp,
                              arr_info_apo[v_indice].v_imp_ap_pat,
                              arr_info_apo[v_indice].v_aiv_ap_pat,
                              arr_info_apo[v_indice].v_cve_ent_financiera,
                              arr_info_apo[v_indice].v_des_ent_fin,
                              arr_info_apo[v_indice].v_tpo_credito,
                              arr_info_apo[v_indice].v_concepto,
                              arr_info_apo[v_indice].v_des_concepto,
                              arr_info_apo[v_indice].v_folio_transaccion,
                              arr_info_apo[v_indice].v_f_transaccion,
                              arr_info_apo[v_indice].v_folio_factura,
                              arr_info_apo[v_indice].v_f_factura,
                              arr_info_apo[v_indice].v_estado,
                              arr_info_apo[v_indice].v_desc_edo_aps,
                              arr_info_apo[v_indice].v_f_archivo,
                              arr_info_apo[v_indice].v_interface

    INITIALIZE v_desc_credito TO NULL
    
    SELECT a.desc_credito_ocg
    INTO   v_desc_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = arr_info_apo[v_indice].v_tpo_credito
    AND    a.ind_activo      = 1

    IF arr_info_apo[v_indice].v_tpo_credito = 3 THEN
       LET arr_info_apo[v_indice].v_interface = 'UG'
    END IF

    LET v_detalle =  arr_info_apo[v_indice].v_nss, "|",
                     arr_info_apo[v_indice].v_periodo_pago, "|",
                     arr_info_apo[v_indice].v_f_pago USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_folio_sua, "|",
                     arr_info_apo[v_indice].v_nrp, "|",
                     arr_info_apo[v_indice].v_aiv_ap_pat USING "#,###,##&.######", "|",
                     arr_info_apo[v_indice].v_imp_ap_pat USING "#,###,##&.##", "|",
                     arr_info_apo[v_indice].v_cve_ent_financiera USING "&&&", ' - ',arr_info_apo[v_indice].v_des_ent_fin CLIPPED, "|",
                     arr_info_apo[v_indice].v_tpo_credito CLIPPED, ' - ',v_desc_credito CLIPPED, "|",
                     arr_info_apo[v_indice].v_concepto CLIPPED, ' - ',arr_info_apo[v_indice].v_des_concepto CLIPPED, "|",
                     arr_info_apo[v_indice].v_folio_transaccion USING "&&&&&&&&&&", "|",
                     arr_info_apo[v_indice].v_f_transaccion USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_folio_factura USING "&&&&&&&&&&", "|",
                     arr_info_apo[v_indice].v_f_factura USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_estado CLIPPED, ' - ',arr_info_apo[v_indice].v_desc_edo_aps, "|",
                     arr_info_apo[v_indice].v_f_archivo USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_interface

    CALL v_ch_arch_salida.write([v_detalle])

    LET v_tot_aivs       = v_tot_aivs       + arr_info_apo[v_indice].v_aiv_ap_pat
    LET v_tot_aportacion = v_tot_aportacion + arr_info_apo[v_indice].v_imp_ap_pat
    LET v_indice         = v_indice         + 1 
  END FOREACH

  FREE cur_sl_inf_arc
  
  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice  = v_indice - 1 

  LET v_sumario = "TOTALES: | | | | |",
                                v_tot_aivs USING "###,###,##&.######"," | ",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | | | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Facturación por Folio \n en la ruta "||v_ruta_nomarch,"information") 
END FUNCTION

FUNCTION fn_reporte(v_indice1, v_indice2)
DEFINE manejador_rpt         om.SaxDocumentHandler --Contenedor documentos reporte
DEFINE v_rep_indice          INTEGER
DEFINE v_tot_reporte         INTEGER 
DEFINE v_indice1             INTEGER
DEFINE v_indice2             INTEGER
DEFINE v_encabezado          SMALLINT -- 1 detalle, 2 totales por tipo de crédito y concepto

DEFINE v_indice_rep          INTEGER
DEFINE v_indice_rep_dev      INTEGER

  --Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISC251.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  CALL a_fac_apo_sub_rep.clear()
  CALL a_fac_apo_sub_rep_dev.clear()

  LET v_indice_rep          = 1
  LET v_indice_rep_dev      = 1
  LET v_tot_detalle_rep     = 0
  LET v_tot_detalle_rep_dev = 0

  FOR v_rep_indice = 1 TO  a_fac_apo_sub.getLength()
      --DISPLAY "a_fac_apo_sub[v_rep_indice].concepto[1,3]: ", a_fac_apo_sub[v_rep_indice].concepto[1,7]
      IF a_fac_apo_sub[v_rep_indice].concepto[1,3] MATCHES "*8" THEN        
         LET a_fac_apo_sub_rep_dev[v_indice_rep_dev].* = a_fac_apo_sub[v_rep_indice].*
         LET v_tot_detalle_rep_dev = v_tot_detalle_rep_dev + a_fac_apo_sub_rep_dev[v_indice_rep_dev].monto 
         LET v_indice_rep_dev      = v_indice_rep_dev + 1
      ELSE
         LET a_fac_apo_sub_rep[v_indice_rep].* = a_fac_apo_sub[v_rep_indice].*
         LET v_tot_detalle_rep     = v_tot_detalle_rep + a_fac_apo_sub_rep[v_indice_rep].monto
         LET v_indice_rep          = v_indice_rep + 1
      END IF
  END FOR

  IF a_fac_apo_sub_rep.getLength() > 0 THEN
     IF a_fac_apo_sub_rep[a_fac_apo_sub_rep.getLength()].concepto IS NULL THEN
        CALL a_fac_apo_sub_rep.deleteElement(v_indice_rep)
     END IF
     LET v_indice_rep     = v_indice_rep - 1
  END IF

  IF a_fac_apo_sub_rep_dev.getLength() > 0 THEN
     IF a_fac_apo_sub_rep_dev[a_fac_apo_sub_rep_dev.getLength()].concepto IS NULL THEN
        CALL a_fac_apo_sub_rep_dev.deleteElement(v_indice_rep_dev)
     END IF
     LET v_indice_rep_dev = v_indice_rep_dev - 1
  END IF

  --Inicia el reporte
  {START REPORT rep_con_disp TO XML HANDLER manejador_rpt

  LET v_tot_reporte = 1
  LET v_encabezado = 1

  FOR v_rep_indice = 1 TO  v_indice1
      OUTPUT TO REPORT rep_con_disp(a_fac_apo_sub[v_rep_indice].*, 
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
      OUTPUT TO REPORT rep_con_disp(a_fac_apo_sub[v_rep_indice].*, 
                                    a_totales_con[v_rep_indice].*, 
                                    v_tot_detalle,  
                                    v_tot_totales, 
                                    v_encabezado, 
                                    v_tot_reporte, 
                                    v_indice1)
  END FOR
  
  FINISH REPORT rep_con_disp}

  START REPORT rep_con_disp TO XML HANDLER manejador_rpt
    LET v_tot_reporte = 1
    LET v_encabezado  = 1

    FOR v_rep_indice = 1 TO  a_fac_apo_sub_rep.getLength()
        OUTPUT TO REPORT rep_con_disp(a_fac_apo_sub_rep[v_rep_indice].*,
                                      a_fac_apo_sub_rep_dev[v_rep_indice].*,
                                      a_totales_con[v_rep_indice].*, 
                                      v_tot_detalle_rep, 
                                      v_tot_detalle_rep_dev, 
                                      v_tot_totales,
                                      v_encabezado, 
                                      v_tot_reporte, 
                                      v_indice1)
    END FOR

    LET v_tot_reporte = 2
    LET v_encabezado  = 2

    DISPLAY "a_fac_apo_sub_rep_dev.getLength() : ",a_fac_apo_sub_rep_dev.getLength()
    IF a_fac_apo_sub_rep_dev.getLength() >= 1 THEN
       FOR v_rep_indice = 1 TO a_fac_apo_sub_rep_dev.getLength()
           OUTPUT TO REPORT rep_con_disp(a_fac_apo_sub_rep[v_rep_indice].*, 
                                         a_fac_apo_sub_rep_dev[v_rep_indice].*, 
                                         a_totales_con[v_rep_indice].*, 
                                         v_tot_detalle_rep, 
                                         v_tot_detalle_rep_dev,  
                                         v_tot_totales, 
                                         v_encabezado, 
                                         v_tot_reporte, 
                                         v_indice1)
       END FOR
    END IF

    LET v_tot_reporte = 3
    LET v_encabezado  = 3

    FOR v_rep_indice = 1 TO a_totales_con.getLength()
        OUTPUT TO REPORT rep_con_disp(a_fac_apo_sub_rep[v_rep_indice].*, 
                                    a_fac_apo_sub_rep_dev[v_rep_indice].*, 
                                    a_totales_con[v_rep_indice].*, 
                                    v_tot_detalle_rep, 
                                    v_tot_detalle_rep_dev,  
                                    v_tot_totales, 
                                    v_encabezado, 
                                    v_tot_reporte, 
                                    v_indice1)
    END FOR
  FINISH REPORT rep_con_disp
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_con_disp(a_apo_sub_det, 
                    a_apo_sub_det_dev,
                    a_apo_sub_tot, 
                    v_t_detalle,
                    v_t_detalle_dev, 
                    v_t_total, 
                    v_encabezado, 
                    v_tot_reporte, 
                    v_total_registros)
  --Datos de detalle
  DEFINE a_apo_sub_det       RECORD    
    entidad_financiera       CHAR(65),
    cuenta_bancaria          CHAR(40),    
    documento_fico 		     CHAR(10),
    concepto			     CHAR(50),
    estado                   CHAR(50),
    monto                    DECIMAL(18,6)    
  END RECORD  

  --Datos de detalle devolcuiones
  DEFINE a_apo_sub_det_dev   RECORD    
    entidad_financiera       CHAR(65),
    cuenta_bancaria          CHAR(40),    
    documento_fico 		     CHAR(10),
    concepto			     CHAR(50),
    estado                   CHAR(50),
    monto                    DECIMAL(18,6)    
  END RECORD 

  --Totales 
  DEFINE a_apo_sub_tot       RECORD     
    tpo_credito              CHAR(50),     --Tipo de crédito    
    concepto_tot             CHAR(50),     --Concepto
    monto_tot                DECIMAL(18,6) --Total monto
  END RECORD

  --Cifras totales de detalle y Totales por tipo de crédito y concepto
  DEFINE v_t_detalle         DECIMAL(22,6),
         v_t_detalle_dev     DECIMAL(22,6),
         v_t_total           DECIMAL(22,6)                  
   
  DEFINE v_fecha_consulta    DATE, --Fecha de proceso 
         v_tot_reporte       INTEGER, 
         v_encabezado        INTEGER, 
         v_total_registros   INTEGER
    
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
      PRINTX v_t_detalle_dev USING "###,###,###,###,##&.##" 
      PRINTX v_t_total USING "###,###,###,###,##&.##" 
    
    ON EVERY ROW
      PRINTX a_apo_sub_det.entidad_financiera
      PRINTX a_apo_sub_det.cuenta_bancaria 
      PRINTX a_apo_sub_det.documento_fico 
      PRINTX a_apo_sub_det.concepto 
      PRINTX a_apo_sub_det.estado
      PRINTX a_apo_sub_det.monto USING "###,###,###,###,##&.##"

      PRINTX a_apo_sub_det_dev.entidad_financiera
      PRINTX a_apo_sub_det_dev.cuenta_bancaria 
      PRINTX a_apo_sub_det_dev.documento_fico 
      PRINTX a_apo_sub_det_dev.concepto 
      PRINTX a_apo_sub_det_dev.estado
      PRINTX a_apo_sub_det_dev.monto USING "###,###,###,###,##&.##"
   
      PRINTX a_apo_sub_tot.tpo_credito
      PRINTX a_apo_sub_tot.concepto_tot
      PRINTX a_apo_sub_tot.monto_tot USING "###,###,###,###,##&.##"

END REPORT