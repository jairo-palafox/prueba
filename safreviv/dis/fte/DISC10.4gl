################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC10                                                    #
#Objetivo         => Realiza la consulta por totales por periodo y             #
#                    cuentas afectadas en el proceso de preliquidación         #
#Fecha de Inicio  => 17/09/2012                                                #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose
DATABASE safre_viv

GLOBALS 
  --Arreglo de cortes por periodo
  DEFINE arr_corte_periodo   DYNAMIC ARRAY OF RECORD
    arr_periodo_pago         CHAR(6),
    arr_total_apo_aivs       DECIMAL(22,6), --Verificar
    arr_total_aportacion     DECIMAL(22,2),
    arr_total_amortizacion   DECIMAL(22,2),
    arr_total_apo_amo        DECIMAL(22,2),
    arr_total_cuentas        DECIMAL(10,0)
  END RECORD 

  --Sección de variables del programa
  DEFINE 
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT
      
  --Sección de variables UI
  DEFINE 
    f_ventana                ui.Window, --provee la interfaz para la ventana
    f_forma                  ui.Form --provee la interfaz para la forma
      
  DEFINE 
    f_folio                  DECIMAL(9,0),
    fecha_liquida            DATE

  DEFINE arr_folios_fechas   DYNAMIC ARRAY OF RECORD 
    f_folio_ins              DECIMAL(9,0),
    fecha_liquida_ins        DATE
  END RECORD  

  DEFINE 
    criterios_hs             STRING,
    criterios                STRING  

  DEFINE 
    f_tot_amortizacion       DECIMAL(22,2),
    f_tot_aportacion         DECIMAL(22,2),
    f_tot_cuentas            DECIMAL(10,0)

  DEFINE 
    f_tot_apo_aivs           DECIMAL(22,6), --Verificar
    f_tot_apo_amo            DECIMAL(22,2)

  DEFINE
    v_folio_reg_pag          DECIMAL(9,0),
    v_etiqueta               STRING,
    v_etiqueta_precio        STRING

  CONSTANT                   Por_Folio = 0
  CONSTANT                   Por_Fecha = 1
  CONSTANT                   Sin       = 0
   
  DEFINE
    v_tbl_mov                VARCHAR(50), 
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING,
    p_proceso_cod            SMALLINT
      
END GLOBALS

MAIN
  LET p_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa

  LET p_proceso_cod  = 901

  --20032015 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
  PREPARE fn_tbl_mov FROM "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

  CLOSE WINDOW SCREEN

  OPEN WINDOW v_consulta WITH FORM "DISC101" 
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio, fecha_liquida
        BEFORE INPUT 
          LET f_ventana    = ui.Window.getCurrent()
          LET f_forma      = f_ventana.getForm()
          LET criterios    = ""
          LET criterios_hs = ""
               
          --Invocamos la función para asignar el título a la ventana
          CALL ui.Interface.setText(p_nom_prog)

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


          --Oculta las secciones de detalle
          CALL f_forma.setElementHidden("gr_resumen",TRUE) --Oculta la sección de resumen
          CALL f_forma.setElementHidden("gr_info_reg_pag",TRUE) --Oculta la sección de información de registro de pagos
          CALL f_forma.setElementHidden("gr_info_precio",TRUE) --Oculta la sección de información del precio del fondo al día
          
          ON ACTION cancelar
             EXIT PROGRAM 
               
          ON ACTION aceptar
             IF f_folio IS NULL AND fecha_liquida IS NULL THEN
                CALL fn_mensaje("Error", "Capture al menos un criterio para realizar la búsqueda", "information")
                NEXT FIELD f_folio
             ELSE 
                CALL fn_consulta_corte_periodo()

                --si existe información, muestra en pantalla
                IF arr_corte_periodo.getLength() >= 1 THEN
                   CALL f_forma.setElementHidden("gr_info_reg_pag",FALSE) --False la sección de información de registro de pagos
                   CALL f_forma.setElementHidden("gr_resumen",FALSE) --Muestra la sección de resumen
                   CALL f_forma.setElementHidden("gr_info_precio",FALSE) --Muestra la sección de información del precio del fondo al día

                   DISPLAY ARRAY arr_corte_periodo TO src_resumen.* ATTRIBUTES (ACCEPT = FALSE , CANCEL = FALSE )
                     BEFORE DISPLAY 
                       --Muestra folio de registro de pagos
                       IF v_folio_reg_pag IS NULL THEN 
                          LET v_etiqueta = "No existe Folio de Registro de Pagos asociado."
                       ELSE 
                          LET v_etiqueta = "Folio de Registro de Pagos:",v_folio_reg_pag
                          CALL fn_obtiene_precio_valor_fondo()
                       END IF

                       CALL f_forma.setElementText("lbl_reg_pag",v_etiqueta)
                       CALL f_forma.setElementText("lbl_precio",v_etiqueta_precio)
                        
                       DISPLAY BY NAME f_tot_cuentas, f_tot_aportacion,f_tot_amortizacion, f_tot_apo_aivs, f_tot_apo_amo
                     
                     ON ACTION cancelar 
                        EXIT PROGRAM 

                     ON ACTION reporte
                        CALL fn_genera_reporte_periodos()

                   END DISPLAY
                ELSE 
                   CALL fn_mensaje ("information","No existe información a mostrar","info")
                   CALL f_forma.setElementHidden("gr_resumen",TRUE ) --Oculta la sección de resumen
                   CLEAR FORM 
                END IF  
             END IF 
      END INPUT 
    END DIALOG

  CLOSE WINDOW v_consulta
   
END MAIN

--Función que realiza la búsqueda de la información de la preliquidación
FUNCTION fn_consulta_corte_periodo()
  DEFINE 
    v_query                  STRING,
    v_ind_periodo            INTEGER,
    v_folio_existe           SMALLINT  

  --Inicializa variables
  LET f_tot_cuentas      = 0
  LET f_tot_aportacion   = 0.00
  LET f_tot_amortizacion = 0.00
  LET f_tot_apo_aivs     = 0.00
  LET f_tot_apo_amo      = 0.00
   
  DISPLAY "Folio de entrada: ",f_folio

  --Valida que existan datos en dis_his_hs
  IF f_folio IS NOT NULL AND fecha_liquida IS NOT NULL THEN 
     --Valida que exista info con los criterios
     SELECT COUNT(*)
     INTO   v_folio_existe
     FROM   dis_his_hs 
     WHERE  folio     = f_folio
     AND    f_liquida = fecha_liquida;

     LET criterios_hs =  " \n AND HS.folio_liquida = ",f_folio,
                         " \n AND DP.f_liquida = '",fecha_liquida,"'"

     LET criterios    =  " \n AND folio = ",f_folio,
                         " \n AND f_liquida = '",fecha_liquida,"'"
  END IF 

  IF f_folio IS NOT NULL AND fecha_liquida IS NULL THEN
     --Valida que exista info con los criterios
     SELECT COUNT(*)
     INTO   v_folio_existe
     FROM   dis_his_hs --Cambiar dis_det por la tabla nueva
     WHERE  folio = f_folio --Validar que el campo folio exista en la nueva tabla

     LET criterios_hs =  " \n AND HS.folio_liquida = ",f_folio
     LET criterios    =  " \n AND folio = ",f_folio
  END IF  

  IF f_folio IS NULL AND fecha_liquida IS NOT NULL THEN
     --Valida que exista info con los criterios
     SELECT COUNT(*)
     INTO   v_folio_existe
     FROM   dis_his_hs 
     WHERE  f_liquida = fecha_liquida;

     LET criterios_hs = " \n AND DP.f_liquida = '",fecha_liquida,"'"
     LET criterios    = " \n AND f_liquida = '",fecha_liquida,"'"
  END IF  

  DISPLAY "Existe? -- ",v_folio_existe
   
  --Si no hay datos en la tabla, hacer la consulta e ingresar la información
  IF v_folio_existe = 0 OR v_folio_existe IS NULL THEN 
     {LET v_ind_periodo = 1
         
     LET v_query =  "\n SELECT  HS.periodo_pago,SUM  (HS.imp_ap_pat) total_aportacion,SUM  (HS.imp_am_cre) total_amortizacion,COUNT (*) cuentas ,HS.folio_liquida, DP.f_liquida",
                    "\n FROM  dis_interface_hs HS, dis_preliquida DP",
                    "\n WHERE HS.folio_liquida = DP.folio_liquida",
                    "\n AND HS.id_derechohabiente = DP.id_derechohabiente"

     LET v_query = v_query, criterios_hs, "\nGROUP  BY  HS.periodo_pago,HS.folio_liquida, DP.f_liquida ",
                                          "\ORDER  BY  HS.periodo_pago ASC"

     DISPLAY "v_query -- ",v_query
         
     PREPARE prp_periodos FROM v_query
     DECLARE cu_periodos CURSOR FOR prp_periodos
     FOREACH cu_periodos INTO arr_corte_periodo[v_ind_periodo].arr_periodo_pago,
                              arr_corte_periodo[v_ind_periodo].arr_total_aportacion,
                              arr_corte_periodo[v_ind_periodo].arr_total_amortizacion,
                              arr_corte_periodo[v_ind_periodo].arr_total_cuentas,
                              arr_folios_fechas[v_ind_periodo].f_folio_ins,
                              arr_folios_fechas[v_ind_periodo].fecha_liquida_ins

       --Valida que no haya nulos
       IF arr_corte_periodo[v_ind_periodo].arr_total_aportacion IS NULL THEN 
          LET arr_corte_periodo[v_ind_periodo].arr_total_aportacion = 0.00
       END IF 
            
       IF arr_corte_periodo[v_ind_periodo].arr_total_amortizacion IS NULL THEN 
          LET arr_corte_periodo[v_ind_periodo].arr_total_amortizacion = 0.00
       END IF 

       IF arr_corte_periodo[v_ind_periodo].arr_total_cuentas IS NULL THEN 
          LET arr_corte_periodo[v_ind_periodo].arr_total_cuentas = 0
       END IF 

       --Hace sumatoria de aportaciones y amortizaciones
       LET f_tot_cuentas      = f_tot_cuentas + arr_corte_periodo[v_ind_periodo].arr_total_cuentas
       LET f_tot_aportacion   = f_tot_aportacion + arr_corte_periodo[v_ind_periodo].arr_total_aportacion
       LET f_tot_amortizacion = f_tot_amortizacion + arr_corte_periodo[v_ind_periodo].arr_total_amortizacion

       LET v_ind_periodo = v_ind_periodo + 1
     END FOREACH 

     CALL arr_corte_periodo.deleteElement(v_ind_periodo)

     --Valida que exista información 
     IF arr_corte_periodo.getLength() >= 1 THEN 
        --Si existe, inserta información en la nueva tabla
        FOR v_ind_periodo = 1 TO arr_corte_periodo.getLength()
            INSERT INTO dis_his_hs
            (folio,f_liquida,periodo_pago,monto_aportacion,monto_amortizacion,cuentas)
            VALUES 
            (arr_folios_fechas[v_ind_periodo].f_folio_ins,
             arr_folios_fechas[v_ind_periodo].fecha_liquida_ins,
             arr_corte_periodo[v_ind_periodo].arr_periodo_pago,
             arr_corte_periodo[v_ind_periodo].arr_total_aportacion,
             arr_corte_periodo[v_ind_periodo].arr_total_amortizacion,
             arr_corte_periodo[v_ind_periodo].arr_total_cuentas)
        END FOR 
     END IF}
   
  ELSE --Si el folio existe, trae la información directamente de esa tabla
     LET v_ind_periodo = 1

     -- aiv_ap_pat (18,6)
     LET v_query =  "\n SELECT periodo_pago, SUM(monto_apo_aivs) total_apo_aivs, SUM(monto_aportacion) total_aportacion, SUM(monto_amortizacion) total_amortizacion, SUM(monto_aportacion + monto_amortizacion) total_apo_amo,cuentas ",

     --LET v_query =  "\n SELECT periodo_pago,SUM  (monto_aportacion) total_aportacion,SUM  (monto_amortizacion) total_amortizacion, cuentas ",
                    "\n FROM   dis_his_hs ",
                    "\n WHERE  1 = 1 "

     LET v_query = v_query, criterios ,  "\n GROUP  BY  periodo_pago, cuentas ",
                                         "\n ORDER  BY  periodo_pago ASC"

     DISPLAY "v_query -- ",v_query
         
     PREPARE prp_periodos_corte FROM v_query
     DECLARE cu_periodos_corte CURSOR FOR prp_periodos_corte
     FOREACH cu_periodos_corte INTO arr_corte_periodo[v_ind_periodo].arr_periodo_pago,
                                    arr_corte_periodo[v_ind_periodo].arr_total_apo_aivs,
                                    arr_corte_periodo[v_ind_periodo].arr_total_aportacion,
                                    arr_corte_periodo[v_ind_periodo].arr_total_amortizacion,
                                    arr_corte_periodo[v_ind_periodo].arr_total_apo_amo,
                                    arr_corte_periodo[v_ind_periodo].arr_total_cuentas

       --Hace sumatoria de aportaciones y amortizaciones
       LET f_tot_cuentas      = f_tot_cuentas + arr_corte_periodo[v_ind_periodo].arr_total_cuentas
       LET f_tot_aportacion   = f_tot_aportacion + arr_corte_periodo[v_ind_periodo].arr_total_aportacion
       LET f_tot_amortizacion = f_tot_amortizacion + arr_corte_periodo[v_ind_periodo].arr_total_amortizacion
       LET f_tot_apo_aivs     = f_tot_apo_aivs + arr_corte_periodo[v_ind_periodo].arr_total_apo_aivs
       LET f_tot_apo_amo      = f_tot_apo_amo + arr_corte_periodo[v_ind_periodo].arr_total_apo_amo
       LET v_ind_periodo      = v_ind_periodo + 1
     END FOREACH 
         
     CALL arr_corte_periodo.deleteElement(v_ind_periodo)

     --obtiene fecha de liquidación del proceso
     {SELECT  DISTINCT  f_liquida
     INTO  fecha_liquida
     FROM  dis_his_hs 
     WHERE  folio = f_folio;

     DISPLAY "Fecha: ",fecha_liquida}

     IF f_folio IS NULL THEN 
        --Obtiene folio de registro de pagos
        SELECT DISTINCT(folio) 
        INTO   f_folio
        FROM   glo_folio
        WHERE  f_actualiza = fecha_liquida
        AND    proceso_cod = 901
        AND    opera_cod   = 1
     END IF 
         
     --Obtiene folio de registro de pagos
     SELECT folio_referencia
     INTO   v_folio_reg_pag 
     FROM   glo_folio
     WHERE  folio = f_folio

     DISPLAY "v_folio_reg_pag :",v_folio_reg_pag
  END IF 
   
END FUNCTION 

--Obtiene el precio del valor del fondo el día que se liquidó
FUNCTION fn_obtiene_precio_valor_fondo()
  DEFINE 
    v_precio_fondo           LIKE glo_valor_fondo.precio_fondo,
    v_fecha_precio           DATE,
    v_query_inc              STRING

  --20032015 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
  EXECUTE fn_tbl_mov USING Por_Folio, f_folio, Sin INTO v_tbl_mov --por folio

  --Obtiene fecha de liquidación
  LET v_query_inc =  "\n SELECT DISTINCT f_liquida ",
                     "\n FROM   ",v_tbl_mov,
                     "\n WHERE  folio_liquida = ?"
  PREPARE prp_mov_a FROM v_query_inc
  EXECUTE prp_mov_a USING f_folio INTO v_fecha_precio

  IF v_fecha_precio IS NULL OR v_fecha_precio = '12-31-1899' THEN 
     SELECT DISTINCT f_liquida 
     INTO   v_fecha_precio
     FROM   dis_preliquida 
     WHERE  folio_liquida = f_folio
  END IF 
      
  SELECT precio_fondo
  INTO   v_precio_fondo
  FROM   glo_valor_fondo
  WHERE  f_valuacion = v_fecha_precio
  AND    fondo       = 11
  IF v_fecha_precio IS NOT NULL THEN 
     LET v_etiqueta_precio = "El precio de fondo es ",v_precio_fondo USING  "######.&&&&&&", " al día ", v_fecha_precio USING "dd-mm-yyyy" 
  END IF 

END FUNCTION 

--Función para generar el reporte de cortes por periodo
FUNCTION fn_genera_reporte_periodos()
  DEFINE
    v_manejador_rpt          om.SaxDocumentHandler,
    v_indice_periodo         INTEGER 

  LET v_indice_periodo = 1
      
  IF fgl_report_loadCurrentSettings("DISC101.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET v_manejador_rpt = fgl_report_commitCurrentSettings()
  END IF 

  START REPORT rpt_cortes_periodo TO XML HANDLER v_manejador_rpt
    FOR v_indice_periodo = 1 TO arr_corte_periodo.getLength()
        OUTPUT TO REPORT rpt_cortes_periodo(arr_corte_periodo[v_indice_periodo].*)
    END FOR 
  FINISH REPORT rpt_cortes_periodo

END FUNCTION 

--Reporte de cortes por periodo y totales de aportación, amortización y cuentas por periodo
REPORT rpt_cortes_periodo(arr_periodos)
  --Arreglo de cortes por periodo
  DEFINE arr_periodos        RECORD
     arr_periodo_pago        CHAR(6),
     arr_total_apo_aivs      DECIMAL(22,6), --Verificar
     arr_total_aportacion    DECIMAL(22,2),
     arr_total_amortizacion  DECIMAL(22,2),
     arr_total_apo_amo       DECIMAL(22,2),
     arr_total_cuentas       DECIMAL(10,0)
  END RECORD 

  DEFINE f_reporte           DATE  
      
  FORMAT
    FIRST PAGE HEADER
      LET f_reporte = TODAY 

      PRINTX f_reporte USING "yyyy-mm-dd"
      PRINTX p_usuario
      PRINTX f_folio
      PRINTX fecha_liquida USING "yyyy-mm-dd"
      PRINTX f_tot_aportacion
      PRINTX f_tot_amortizacion
      PRINTX f_tot_cuentas
      PRINTX v_folio_reg_pag
      PRINTX v_etiqueta_precio
      PRINTX f_tot_apo_aivs
      PRINTX f_tot_apo_amo
         
    ON EVERY ROW 
       PRINTX arr_periodos.arr_periodo_pago
       PRINTX arr_periodos.arr_total_amortizacion
       PRINTX arr_periodos.arr_total_aportacion
       PRINTX arr_periodos.arr_total_apo_aivs
       PRINTX arr_periodos.arr_total_cuentas
       PRINTX arr_periodos.arr_total_apo_amo

END REPORT 

{#Función para crear la tabla temporal para consulta
FUNCTION fn_crea_tabla_consulta_tmp()
  DEFINE v_existe            SMALLINT 

  WHENEVER ERROR CONTINUE 
    --Pasa el control a safre_tmp
    DATABASE safre_tmp

    --Verifica si la tabla existe; si existe, le hace un drop
    SELECT COUNT(*)
    INTO   v_existe  -- 0 No existe; 1 Sí existe 
    FROM   sysmaster:systabnames S,
           sysmaster:systabinfo I
    WHERE  I.ti_partnum = S.partnum
    AND    S.tabname    = 'tmp_dis_periodos_preliquidacion' ;

    DISPLAY "existe -- ",v_existe
   
    IF v_existe = 1 THEN 
       DROP TABLE tmp_dis_periodos_preliquidacion;
    END IF 

    CREATE TABLE tmp_dis_periodos_preliquidacion (periodo_pago CHAR (6),
                                                  importe_apo  DECIMAL (12,2),
                                                  importe_amo  DECIMAL (12,2),
                                                  cuentas      INTEGER);
   
    CREATE INDEX xpktmp_cnt_folio_liquida ON tmp_cnt_folio_liquida(folio_liquida,cod_proceso_cnt )
   
    --regresa el control a safre_viv 
    DATABASE safre_viv 

END FUNCTION}
