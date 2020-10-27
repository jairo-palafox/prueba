################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 04/04/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC29                                                   #
#Objetivo          => Programa para consultar la conciliación de aportaciones  #
#                     subsecuentes                                             #
#                                                                              #
#Fecha inicio      => 23/11/2015                                               #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            SMALLINT,    --Código del proceso
    v_proceso_cod            SMALLINT,    --Código del proceso
    p_opera_cod              SMALLINT,    --Código de operación
    p_pid                    DECIMAL(9,0)

  DEFINE v_arr_conc_tot      DYNAMIC ARRAY OF RECORD
    v_no_conciliados         DECIMAL(12,2), --Total de registros no conciliados
    v_monto_aivs_no          DECIMAL(18,6), --Monto total de AVIS de no conciliados
    v_monto_apo_no           DECIMAL(12,2), --Monto total de pesos de no conciliados
    v_conciliados            DECIMAL(12,2), --Total de registros conciliados
    v_monto_aivs             DECIMAL(18,6), --Monto total de AVIS de no conciliados
    v_monto_apo              DECIMAL(12,2), --Monto total de pesos de no conciliados
    v_porcentaje             DECIMAL(5,2),  --Porcentaje de avance de conciliados vs. no conciliados
    v_tot_registros          DECIMAL(12,2)  --Total de registros  
  END RECORD  

  DEFINE 
    r_tot_registros          DECIMAL(9,0),  --Total de registros    
    r_conciliados            DECIMAL(12,2), --Total de registros conciliados 
    r_no_conciliados         DECIMAL(12,2)  --Total de registros no conciliados

  DEFINE v_arr_conc          DYNAMIC ARRAY OF RECORD
    v_nss                    CHAR(11),
    v_folio_sua              DECIMAL(10,0),
    v_periodo_pago           CHAR(06),
    v_f_pago                 DATE,
    v_nrp                    VARCHAR(11),
    v_imp_apo_pat            DECIMAL(12,2),
    v_aivs                   DECIMAL(18,6),
    v_folio_liquida          DECIMAL(9,0),
    v_conciliado             SMALLINT, 
    v_id_derechohabiente     DECIMAL(9,0)
  END RECORD

  DEFINE 
    v_folio                  LIKE dis_ap_subsecuente.folio,  --Folio
    v_nombre_archivo         CHAR(100),
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form ,  --Define las propiedades de la forma
    v_ruta_listados          CHAR(40),
    v_id_derechohabiente     DECIMAL(9,0),
    v_cuenta_folio           INTEGER,
    bnd_consulta             SMALLINT,
    r_bnd_periodo            SMALLINT,
    v_qwery_ibx              STRING 

  DEFINE l_comando           STRING
  DEFINE v_ruta_ejecutable   CHAR(40)
  DEFINE v_max_pid           LIKE bat_ctr_proceso.pid   
  DEFINE l_v_arch_proceso    VARCHAR(100)
  DEFINE v_cont_num_cred     INTEGER
   
  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 932
  LET p_opera_cod    = 1 
  LET r_bnd_periodo  = 0

  INITIALIZE l_v_arch_proceso TO NULL

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
   
  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
   
  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --Obtiene las rutas ejecutable
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

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
  
  LET bnd_consulta = 0

  CLOSE WINDOW SCREEN
   
  OPEN WINDOW w1 WITH FORM "DISC291" 
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          --CALL DIALOG.setActionHidden("reporte", 1 ) --Oculta el botón reporte
          --CALL DIALOG.setActionHidden("archivo", 1 ) --Oculta el botón archivo
          CALL f_forma.setElementHidden("gr_resultado", 1 ) --Oculta el drupo del resultado de la búsqueda
          CALL ui.interface.refresh()

          {ON ACTION reporte
             CALL fn_genera_reporte()
           
          ON ACTION archivo
             CALL fn_genera_archivo()}   
       
          ON ACTION ACCEPT 
             --Valida que se inserte el folio como parámetro
             IF (v_folio IS NULL) THEN
                CALL fn_mensaje("ATENCIÓN",
                                "Debe capturar el folio para la búsqueda",
                                "about")
                NEXT FIELD v_folio   
             END IF             

             IF v_folio IS NOT NULL THEN
                --Valida que exista el folio
                SELECT COUNT(*) 
                INTO   v_cuenta_folio
                FROM   dis_ap_subsecuente
                WHERE  folio  = v_folio
                AND    estado = 10 
                IF v_cuenta_folio  = 0    OR 
                   v_cuenta_folio IS NULL THEN 
                   CALL fn_mensaje("Atención","Folio no registrado en el proceso de aportaciones subsecuentes",
                                   "about")
                   NEXT FIELD v_folio
                END IF 
             END IF        
           
             CALL fn_consultar(v_folio)

             IF r_tot_registros > 0 THEN
                --CALL DIALOG.setActionHidden("reporte", 0 ) --Muestra el botón reporte
                --CALL DIALOG.setActionHidden("archivo", 0 ) --Muestra el botón archivo
                CALL f_forma.setElementHidden("gr_resultado", 0 ) --Muestra el grupo del resultado de la búsqueda

                DISPLAY ARRAY v_arr_conc_tot TO r_totales.*               
                ATTRIBUTES(CANCEL = FALSE, ACCEPT = FALSE) 
                  ON ACTION cancelar               
                     EXIT PROGRAM 
                  
                  ON ACTION reporte
                     CALL fn_genera_reporte()
           
                  ON ACTION archivo
                     CALL fn_genera_archivo()               
                END DISPLAY
             ELSE
                CALL fn_mensaje("ATENCIÓN",
                                "No existen registros conciliados",
                                "about")
                CALL ui.interface.refresh()
             END IF
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      

    END DIALOG 
  CLOSE WINDOW w1
END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consultar(p_folio)
  DEFINE p_folio             LIKE dis_ap_subsecuente.folio,
         v_tot_registros     DECIMAL(12,0),
         v_conciliados       DECIMAL(12,0),
         v_no_conciliados    DECIMAL(12,0),
         v_indice            SMALLINT

  DROP TABLE IF EXISTS tmp_dis_subs_con

  CREATE TABLE tmp_dis_subs_con (nss            CHAR(11),
                                 folio_sua      DECIMAL(10,0),
                                 periodo_pago   CHAR(06),
                                 f_pago         DATE,
                                 nrp            VARCHAR(11),
                                 imp_apo_pat    DECIMAL(12,2),
                                 aivs           DECIMAL(18,6),
                                 folio_liquida  DECIMAL(9,0),
                                 conciliado     SMALLINT, 
                                 id_derechohabiente DECIMAL(9,0))
   
  DISPLAY "folio: ", p_folio

  LET g_sql_txt = "" 
  LET g_sql_txt = "\n INSERT INTO tmp_dis_subs_con ",
                  "\n SELECT ds.nss, ",
                  "\n        ds.folio_sua, ",
                  "\n        ds.periodo_pago, ",
                  "\n        ds.f_pago, ",
                  "\n        ds.reg_pat_imss, ",
                  "\n        ds.imp_apo_pat, ",
                  "\n        ds.apl_apo_pat, ",
                  "\n        ds.folio, ",                   
                  "\n        0 as conciliado, ",
                  "\n        ds.id_derechohabiente ",
                  "\n FROM   dis_ap_subsecuente ds ",
                  "\n WHERE  ds.estado = 10 ",
                  "\n AND    ds.folio  = ? "

  --DISPLAY "--- g_sql_txt -- " , g_sql_txt

  PREPARE ps_sl_tmp_apo FROM g_sql_txt
  EXECUTE ps_sl_tmp_apo USING p_folio

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT di.id_derechohabiente, ",
                  "\n        ds.folio_sua, ",
                  "\n        ds.periodo_pago, ", 
                  "\n        ds.f_pago, ",
                  "\n        ds.reg_pat_imss, ",
                  "\n        di.folio_liquida ",                   
                  "\n FROM   dis_ap_subsecuente ds, ",
                  "\n        dis_interface_ef_ad di ",                   
                  "\n WHERE  ds.id_derechohabiente = di.id_derechohabiente ",
                  "\n AND    ds.folio_sua          = di.folio_sua ",
                  "\n AND    ds.periodo_pago       = di.periodo_pago ", 
                  "\n AND    ds.f_pago             = di.f_pago ", 
                  "\n AND    ds.reg_pat_imss       = di.nrp ",
                  --"\n AND    ds.folio = di.folio_liquida ",
                  "\n AND    ds.estado             = 10 ",  
                  "\n AND    ds.folio              = ? "

  --DISPLAY "--- g_sql_txt -- " , g_sql_txt

  PREPARE ps_sl_int FROM g_sql_txt
  --Iteración de registros con base en la consulta temporal
  DECLARE cur_apo_subs CURSOR FOR ps_sl_int
  LET g_sql_txt = ""
  LET g_sql_txt = "\n UPDATE tmp_dis_subs_con ",
                  "\n SET    conciliado         = 1 ",    
                  "\n WHERE  id_derechohabiente = ? ",
                  "\n AND    folio_sua          = ? ", 
                  "\n AND    periodo_pago       = ? ", 
                  "\n AND    f_pago             = ? ",
                  "\n AND    nrp                = ? "
                  --"\n AND    folio_liquida = ? "

  --DISPLAY "--- g_sql_txt -- " , g_sql_txt

  PREPARE ps_upd_tmp FROM g_sql_txt
  LET v_indice           = 1
  FOREACH cur_apo_subs USING p_folio 
                       INTO v_arr_conc[v_indice].v_id_derechohabiente,
                            v_arr_conc[v_indice].v_folio_sua,
                            v_arr_conc[v_indice].v_periodo_pago,
                            v_arr_conc[v_indice].v_f_pago,
                            v_arr_conc[v_indice].v_nrp,
                            v_arr_conc[v_indice].v_folio_liquida

     EXECUTE ps_upd_tmp USING v_arr_conc[v_indice].v_id_derechohabiente,
                              v_arr_conc[v_indice].v_folio_sua,
                              v_arr_conc[v_indice].v_periodo_pago,
                              v_arr_conc[v_indice].v_f_pago,
                              v_arr_conc[v_indice].v_nrp
                              --v_arr_conc[v_indice].v_folio_liquida    

     LET v_indice = v_indice + 1
  END FOREACH

  CALL v_arr_conc.deleteElement(v_indice)
  LET v_indice = v_indice - 1

  INITIALIZE v_arr_conc_tot TO NULL  

  LET v_indice = 1 

  --Busca el total de registros
  SELECT COUNT(*) 
  INTO   v_arr_conc_tot[v_indice].v_tot_registros
  FROM   tmp_dis_subs_con

  LET r_tot_registros = v_arr_conc_tot[v_indice].v_tot_registros
  
  --Busca los totales de los conciliados
  SELECT COUNT(*), SUM(imp_apo_pat), SUM(aivs)
  INTO   v_arr_conc_tot[v_indice].v_conciliados, 
         v_arr_conc_tot[v_indice].v_monto_apo,
         v_arr_conc_tot[v_indice].v_monto_aivs
  FROM   tmp_dis_subs_con
  WHERE  conciliado = 1

  LET r_conciliados = v_arr_conc_tot[v_indice].v_conciliados  

  IF v_arr_conc_tot[v_indice].v_monto_apo IS NULL THEN 
     LET v_arr_conc_tot[v_indice].v_monto_apo = 0.00
  END IF

  IF v_arr_conc_tot[v_indice].v_monto_aivs IS NULL THEN 
     LET v_arr_conc_tot[v_indice].v_monto_aivs = 0.00
  END IF

  --Busca los totales de los no conciliados
  SELECT COUNT(*), SUM(imp_apo_pat), SUM(aivs)
  INTO   v_arr_conc_tot[v_indice].v_no_conciliados, 
         v_arr_conc_tot[v_indice].v_monto_apo_no,
         v_arr_conc_tot[v_indice].v_monto_aivs_no
  FROM   tmp_dis_subs_con
  WHERE  conciliado = 0

  LET r_no_conciliados = v_arr_conc_tot[v_indice].v_no_conciliados 

  DISPLAY "v_arr_conc_tot[v_indice].v_monto_apo: -",v_arr_conc_tot[v_indice].v_monto_apo
  IF v_arr_conc_tot[v_indice].v_monto_apo_no IS NULL THEN 
     LET v_arr_conc_tot[v_indice].v_monto_apo_no = 0.00
  END IF

  IF v_arr_conc_tot[v_indice].v_monto_aivs_no IS NULL THEN 
     LET v_arr_conc_tot[v_indice].v_monto_aivs_no = 0.00
  END IF

  LET v_arr_conc_tot[v_indice].v_porcentaje = v_arr_conc_tot[v_indice].v_conciliados * 100 / v_arr_conc_tot[v_indice].v_tot_registros 

END FUNCTION

#Objetivo: Genera reporte de inconsistencias de numero de crédito igual a cero
FUNCTION fn_genera_reporte()            
  DEFINE  manejador_rpt      om.SaxDocumentHandler, --Contenedor documentos reporte
          v_rep_indice       INTEGER,
          v_fecha_actual     DATE
  
  LET v_rep_indice = 1

  CALL fn_obtiene_nombre_archivo()
  
  --Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC291.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  LET v_fecha_actual = TODAY

  --Inicia el reporte de registros con rechazo
  START REPORT rep_con_disp TO XML HANDLER manejador_rpt    
    FOR v_rep_indice = 1 TO  v_arr_conc_tot.getLength()
        OUTPUT TO REPORT rep_con_disp(g_usuario,
                                      v_fecha_actual,
                                      v_folio, 
                                      v_nombre_archivo,
                                      v_arr_conc_tot[v_rep_indice].*)
    END FOR 
  FINISH REPORT rep_con_disp
END FUNCTION

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_archivo ()
  DEFINE v_alt_folio LIKE dis_ap_subsecuente.folio  --Folio alterno 

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT nombre_archivo",                  
                  "\n FROM   glo_ctr_archivo",
                  "\n WHERE  proceso_cod = 932 ",
                  "\n AND    folio       = ? "
                  
  --DISPLAY "g_sql_txt -",g_sql_txt,"-" 
  PREPARE ps_sl_arc FROM g_sql_txt
  EXECUTE ps_sl_arc USING v_folio INTO v_nombre_archivo

  DISPLAY "Nombre: ",v_nombre_archivo
END FUNCTION 

#Objetivo: genera el archivo con la consulta obtenida
FUNCTION fn_genera_archivo() 
  DEFINE v_nom_archivo       VARCHAR(40),  --nombre del archivo de salida
         v_ruta_envio_dis    CHAR(40),
         v_ruta_nomarch      VARCHAR(100), --ruta y nombre del archivo de salida
         v_ch_arch_salida    BASE.CHANNEL,
         v_recorre_arreglo   INTEGER,
         v_comando_dos       STRING,
         v_encabezado        STRING,
         v_detalle           STRING,
         v_sumario           STRING,
         --v_folio           DECIMAL(9,0), --Folio liquidación dispersión
         v_indice            DECIMAL(12,2),
         v_desc_conciliado   CHAR(20)
       
  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/dis_ap_sub_concilia_", v_hora

  --Se obtienen la ruta envio del módulo
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
  LET v_encabezado = "FECHA DEL DÍA: ",TODAY USING "dd-mm-yyyy"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = "FOLIO: ",v_folio
  CALL v_ch_arch_salida.write([v_encabezado])
  
  LET v_encabezado = "NOMBRE ARCHIVO: ",v_nombre_archivo 
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = " NSS |FOLIO SUA |PERIODO PAGO |FECHA PAGO |NRP |APORTACIÓN |AIVS |FOLIO LIQUIDACIÓN |CONCILIADO"
  CALL v_ch_arch_salida.write([v_encabezado])
  
  LET g_sql_txt = "SELECT * ",
                  " FROM  tmp_dis_subs_con ",
                  "ORDER BY f_pago DESC, periodo_pago DESC"

  PREPARE ps_slc_ap FROM g_sql_txt
  DECLARE cur_sl_ap CURSOR FOR ps_slc_ap 
  LET v_indice = 1 
  FOREACH cur_sl_ap INTO v_arr_conc[v_indice].v_nss,
                         v_arr_conc[v_indice].v_folio_sua,
                         v_arr_conc[v_indice].v_periodo_pago,
                         v_arr_conc[v_indice].v_f_pago,
                         v_arr_conc[v_indice].v_nrp,
                         v_arr_conc[v_indice].v_imp_apo_pat, 
                         v_arr_conc[v_indice].v_aivs,
                         v_arr_conc[v_indice].v_folio_liquida,
                         v_arr_conc[v_indice].v_conciliado

    IF v_arr_conc[v_indice].v_conciliado = 0 THEN
      LET v_desc_conciliado = ' - NO CONCILIADO' 
    ELSE
      IF v_arr_conc[v_indice].v_conciliado = 1 THEN
        LET v_desc_conciliado = ' - CONCILIADO' 
      END IF
    END IF

    LET v_detalle = v_arr_conc[v_indice].v_nss, "|",
                    v_arr_conc[v_indice].v_folio_sua, "|",
                    v_arr_conc[v_indice].v_periodo_pago, "|",
                    v_arr_conc[v_indice].v_f_pago USING "dd-mm-yyyy", "|",
                    v_arr_conc[v_indice].v_nrp, "|",
                    v_arr_conc[v_indice].v_imp_apo_pat, "|",
                    v_arr_conc[v_indice].v_aivs, "|",
                    v_arr_conc[v_indice].v_folio_liquida, "|",
                    v_arr_conc[v_indice].v_conciliado,v_desc_conciliado CLIPPED, "|"

    CALL v_ch_arch_salida.write([v_detalle])
      
    LET v_indice = v_indice + 1
  END FOREACH

  CALL v_arr_conc.deleteElement(v_indice)
  LET v_indice = v_indice - 1
   
  --Escribe el sumario
  LET v_sumario = "NO CONCILIADOS:|",r_no_conciliados USING "###,###,###,##&","|"
  CALL v_ch_arch_salida.write([v_sumario])

  LET v_sumario = "CONCILIADOS:|",r_conciliados USING "###,###,###,##&","|"                  
  CALL v_ch_arch_salida.write([v_sumario])
  
  LET v_sumario = "TOTAL DE REGISTROS:|",r_tot_registros,"|"                  
  CALL v_ch_arch_salida.write([v_sumario])  

  --Cierra el archivo
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Conciliación Aportaciones Subsecuentes\n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_con_disp(g_usuario,
                    v_fecha_actual,
                    v_folio, 
                    v_nombre_archivo,
                    v_arr_tot)

  DEFINE v_arr_tot RECORD
    V_no_conciliados         DECIMAL(12,2), --Total de registros no conciliados
    v_monto_aivs_no          DECIMAL(18,6), --Monto total de AVIS de no conciliados
    v_monto_apo_no           DECIMAL(12,2), --Monto total de pesos de no conciliados
    v_conciliados            DECIMAL(12,2), --Total de registros conciliados
    v_monto_aivs             DECIMAL(18,6), --Monto total de AVIS de no conciliados
    v_monto_apo              DECIMAL(12,2), --Monto total de pesos de no conciliados
    v_porcentaje             DECIMAL(5,2),  --Porcentaje de avance de conciliados vs. no conciliados
    v_tot_registros          DECIMAL(12,2)  --Total de registros  
  END RECORD

  DEFINE g_usuario           VARCHAR(30),  --Almacena al usuario
         v_fecha_actual      DATE,
         v_folio             DECIMAL(9,0), --Folio liquidación dispersión
         v_nombre_archivo    CHAR(100)

  FORMAT
    FIRST PAGE HEADER
      PRINTX g_usuario
      PRINTX v_fecha_actual USING "dd-mm-yyyy"
      PRINTX v_folio
      PRINTX v_nombre_archivo      

    ON EVERY ROW
       PRINTX v_arr_tot.v_conciliados
       PRINTX v_arr_tot.v_monto_aivs
       PRINTX v_arr_tot.v_monto_aivs_no
       PRINTX v_arr_tot.v_monto_apo
       PRINTX v_arr_tot.v_monto_apo_no
       PRINTX v_arr_tot.v_no_conciliados
       PRINTX v_arr_tot.v_porcentaje
       PRINTX v_arr_tot.v_tot_registros

END REPORT