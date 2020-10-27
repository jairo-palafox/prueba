################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 05/04/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC31                                                   #
#Objetivo          => Programa para consultar las cifras control por folio de  #
#                     aportaciones subsecuentes (Sin Adelanto).                #
#                                                                              #
#Fecha inicio      => 03/12/2015                                               #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            SMALLINT,    --codigo del proceso
    v_proceso_cod            SMALLINT,    --codigo del proceso
    v_proceso_desc           CHAR(40),    --descripción del proceso
    p_opera_cod              SMALLINT,    --codigo de operacion
    p_pid                    DECIMAL(9,0)

  DEFINE v_arr_conc_tot      DYNAMIC ARRAY OF RECORD
    v_folio                  DECIMAL(10,0), --Folio Registro de Pagos
    v_proceso                CHAR(40),      --Proceso
    v_monto_aivs             DECIMAL(18,6), --Monto total de AVIS
    v_monto_apo              DECIMAL(12,2), --Monto total de pesos
    v_tot_registros          DECIMAL(12,2)  --Total de registros 
  END RECORD  

  DEFINE 
    r_tot_registros          DECIMAL(9,0),  --Total de registros    
    r_conciliados            DECIMAL(12,2), --Total de registros conciliados 
    r_no_conciliados         DECIMAL(12,2)  --Total de registros no conciliados

  DEFINE v_arr_conc          DYNAMIC ARRAY OF RECORD
    v_nss                    CHAR(11),
    --v_folio_sua              DECIMAL(10,0),
    v_periodo_pago           CHAR(06),
    --v_f_pago                 DATE,
    --v_nrp                    VARCHAR(11),
    v_imp_apo_pat            DECIMAL(12,2),
    v_aivs                   DECIMAL(18,6),
    v_folio_liquida          DECIMAL(9,0),
    v_conciliado             CHAR(40) 
    --v_id_derechohabiente     DECIMAL(9,0)
  END RECORD

  DEFINE 
    v_folio                  LIKE dis_ap_subsecuente.folio, --Folio
    v_aivs                   DECIMAL(18,6),
    v_aportacion             DECIMAL(12,2),
    v_total_registros        DECIMAL(12),
    v_nombre_archivo         CHAR(100),
    v_edo_folio              SMALLINT,
    v_f_actualiza            DATE,
    v_precio_fondo           DECIMAL(19,14),
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING    

END GLOBALS

MAIN 
  DEFINE 
    f_ventana                ui.Window, --Define las propiedades de la Ventana
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
   
  -- Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 932
  LET p_opera_cod    = 1 
  LET r_bnd_periodo  = 0

  INITIALIZE l_v_arch_proceso TO NULL

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
   
  -- Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
   
  -- Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  -- Obtiene las rutas ejecutable
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  -- Se asigna el titulo del programa
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
   
  OPEN WINDOW w1 WITH FORM "DISC311" 
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("gr_resultado", 1 ) -- Oculta el drupo del resultado de la búsqueda
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           -- Valida que se inserte el folio como parámetro
           IF (v_folio IS NULL) THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar el folio para la búsqueda",
                              "about")
              NEXT FIELD v_folio   
           END IF             

           IF v_folio IS NOT NULL THEN
              -- Valida que exista el folio
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
              CALL fn_obtiene_nombre_archivo()

              SELECT SUM(apl_apo_pat), SUM(imp_apo_pat), COUNT(id_derechohabiente)
              INTO   v_aivs, v_aportacion, v_total_registros
              FROM   dis_ap_subsecuente
              WHERE  folio  = v_folio
              AND    estado = 10

              DISPLAY BY NAME v_nombre_archivo
              DISPLAY BY NAME v_aivs
              DISPLAY BY NAME v_aportacion
              DISPLAY BY NAME v_total_registros
              
              CALL f_forma.setElementHidden("gr_resultado", 0 ) -- Muestra el grupo del resultado de la búsqueda

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
                             "No existen registros a desplegar",
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
  DEFINE 
    p_folio                  LIKE dis_ap_subsecuente.folio,
    v_tot_registros          DECIMAL(12,0),
    v_conciliados            DECIMAL(12,0),
    v_no_conciliados         DECIMAL(12,0),
    v_indice                 SMALLINT

  SELECT  gl.status, gl.f_actualiza
  INTO    v_edo_folio, v_f_actualiza
  FROM    cat_proceso cp,
          glo_folio gl
  WHERE   cp.proceso_cod = gl.proceso_cod
  AND     gl.folio       = p_folio

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_dis_ap_subsecuente_sn;

  SELECT afi.nss,
         aps.id_derechohabiente,
         aps.folio_sua,
         aps.periodo_pago,
         aps.f_pago,
         aps.reg_pat_imss,
         aps.apl_apo_pat,
         aps.imp_apo_pat
  FROM   afi_derechohabiente afi,
         dis_ap_subsecuente aps
  WHERE  afi.id_derechohabiente = aps.id_derechohabiente
  AND    aps.folio              = p_folio
  AND    aps.estado             = 10
  INTO TEMP tmp_dis_ap_subsecuente_sn

  UPDATE STATISTICS FOR TABLE tmp_dis_ap_subsecuente_sn
    
  DROP TABLE IF EXISTS tmp_dis_subs_cf

  CREATE TEMP TABLE tmp_dis_subs_cf (nss                 CHAR(11),
                                     id_derechohabiente  DECIMAL(9,0),
                                     folio_sua           DECIMAL(10,0),
                                     periodo_pago        CHAR(06),
                                     f_pago              DATE,
                                     nrp                 CHAR(11),
                                     aivs                DECIMAL(18,6),
                                     imp_apo_pat         DECIMAL(12,2),
                                     folio               DECIMAL(9,0),
                                     localiza_trabajador CHAR(1))
   
  DISPLAY "folio: ", p_folio

  LET g_sql_txt = "" 
  LET g_sql_txt = "\n INSERT INTO tmp_dis_subs_cf  ",
                  "\n SELECT ap.nss, ",
                  "\n        ap.id_derechohabiente, ",
                  "\n        ap.folio_sua, ",
                  "\n        ap.periodo_pago, ",
                  "\n        ap.f_pago, ",
                  "\n        ap.reg_pat_imss, ",
                  "\n        ap.apl_apo_pat, ",
                  "\n        ap.imp_apo_pat, ",
                  "\n        rp.folio, ",                   
                  "\n        rp.localiza_trabajador ",
                  "\n FROM   tmp_dis_ap_subsecuente_sn ap, ",
                  "\n        OUTER cta_his_pagos rp ",
                  "\n WHERE  ap.id_derechohabiente = rp.id_derechohabiente ",
                  "\n AND    ap.folio_sua          = rp.folio_sua ",
                  "\n AND    ap.periodo_pago       = fn_bimestre_pago(rp.periodo_pago) ",
                  "\n AND    ap.f_pago             = rp.f_pago ",
                  "\n AND    ap.reg_pat_imss       = rp.nrp "

  --DISPLAY "--- g_sql_txt -- " , g_sql_txt

  PREPARE ps_sl_tmp_apo FROM g_sql_txt
  EXECUTE ps_sl_tmp_apo

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT folio, ",
                  "\n        localiza_trabajador, ",
                  "\n        SUM(aivs), ", 
                  "\n        SUM(imp_apo_pat), ",
                  "\n        COUNT(id_derechohabiente) ",
                  "\n FROM   tmp_dis_subs_cf ",
                  "\n GROUP BY 1,2 ",
                  "\n ORDER BY 1,2 "

  --DISPLAY "--- g_sql_txt -- " , g_sql_txt

  PREPARE ps_sl_int FROM g_sql_txt
  --Iteración de registros con base en la consulta temporal
  DECLARE cur_apo_subs CURSOR FOR ps_sl_int
  --DISPLAY "--- g_sql_txt -- " , g_sql_txt
  LET v_indice = 1
  FOREACH cur_apo_subs INTO v_arr_conc_tot[v_indice].v_folio,
                            v_arr_conc_tot[v_indice].v_proceso,
                            v_arr_conc_tot[v_indice].v_monto_aivs,
                            v_arr_conc_tot[v_indice].v_monto_apo,
                            v_arr_conc_tot[v_indice].v_tot_registros

    SELECT cp.proceso_cod, cp.proceso_desc
    INTO   v_proceso_cod, v_proceso_desc
    FROM   cat_proceso cp,
           glo_folio gl
    WHERE  cp.proceso_cod = gl.proceso_cod
    AND    gl.folio       = v_arr_conc_tot[v_indice].v_folio
--display "v_edo_folio ", v_edo_folio

    IF (v_proceso_desc IS NULL OR 
        v_proceso_desc = ' ')  THEN
        LET v_arr_conc_tot[v_indice].v_proceso = 'SIN PAGO LQINFO'
    ELSE
       IF v_proceso_cod <> 1401 THEN
          LET v_arr_conc_tot[v_indice].v_proceso = 'PAGO EN ACLARATORIO'
       ELSE
          IF v_arr_conc_tot[v_indice].v_proceso = 3 THEN
             LET v_arr_conc_tot[v_indice].v_proceso = 'PAGO EN ACLARATORIO'
          ELSE   
             LET v_arr_conc_tot[v_indice].v_proceso = v_proceso_desc
          END IF
       END IF
    END IF

    IF (v_arr_conc_tot[v_indice].v_monto_apo  = 0   AND
        v_arr_conc_tot[v_indice].v_monto_aivs = 0 ) THEN
        LET v_arr_conc_tot[v_indice].v_proceso = 'APORTACIÓN SUBSECUENTE EN CEROS'
    END IF

    IF v_edo_folio = 2 THEN
       SELECT vf.precio_fondo
       INTO   v_precio_fondo
       FROM   glo_valor_fondo vf
       WHERE  vf.fondo       = 11
       AND    vf.f_valuacion = v_f_actualiza

       LET v_arr_conc_tot[v_indice].v_monto_apo = (v_arr_conc_tot[v_indice].v_monto_aivs * v_precio_fondo)
    END IF
    
    LET v_indice = v_indice + 1
  END FOREACH

  CALL v_arr_conc_tot.deleteElement(v_indice)
  LET v_indice = v_indice - 1

  --Busca el total de registros
  SELECT COUNT(*) 
  INTO   r_tot_registros
  FROM   tmp_dis_subs_cf

  --INITIALIZE v_arr_conc_tot TO NULL  
    
END FUNCTION

#Objetivo: Genera reporte de inconsistencias de numero de crédito igual a cero
FUNCTION fn_genera_reporte()   
  DEFINE  manejador_rpt            om.SaxDocumentHandler, --Contenedor documentos reporte
          v_rep_indice             INTEGER,
          v_fecha_actual           DATE
  
  LET v_rep_indice = 1
  
  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC311.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  LET v_fecha_actual = TODAY

  -- Inicia el reporte de registros con rechazo
  START REPORT rep_con_disp TO XML HANDLER manejador_rpt    
     FOR v_rep_indice = 1 TO v_arr_conc_tot.getLength()
         OUTPUT TO REPORT rep_con_disp(g_usuario,
                                       v_fecha_actual,
                                       v_folio, 
                                       v_nombre_archivo,
                                       v_arr_conc_tot[v_rep_indice].*)
     END FOR 
  FINISH REPORT rep_con_disp
END FUNCTION

#Objetivo: Obtiene nombre del archivo de aportaciones subsecuentes
FUNCTION fn_obtiene_nombre_archivo()
  DEFINE v_alt_folio               LIKE dis_ap_subsecuente.folio  --Folio alterno 

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT nombre_archivo",                  
                  "\n FROM   glo_ctr_archivo",
                  "\n WHERE  proceso_cod IN (904, 932) ",
                  "\n AND    folio        = ? "
                  
  --DISPLAY "g_sql_txt -",g_sql_txt,"-" 

  PREPARE ps_sl_arc FROM g_sql_txt
  EXECUTE ps_sl_arc USING v_folio INTO v_nombre_archivo

  DISPLAY "Nombre: ",v_nombre_archivo
END FUNCTION 

#Objetivo: genera el archivo con la consulta obtenida
FUNCTION fn_genera_archivo() 
  DEFINE 
    v_nom_archivo            VARCHAR(40),  --nombre del archivo de salida
    v_ruta_envio_dis         CHAR(40),
    v_ruta_nomarch           VARCHAR(100), --ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_recorre_arreglo        INTEGER,
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    --v_folio                DECIMAL(9,0), --Folio liquidación dispersión
    v_indice                 DECIMAL(12,2),
    v_desc_conciliado        CHAR(20)
    
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
  LET v_nom_archivo   = "/dis_ap_sub_cif_ctr_", v_hora

  -- Se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- Se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  -- Imprime encabezado del archivo
  LET v_encabezado = "FECHA DEL DÍA: ",TODAY USING "dd-mm-yyyy"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = "FOLIO: ",v_folio
  CALL v_ch_arch_salida.write([v_encabezado])
  
  LET v_encabezado = "NOMBRE ARCHIVO: ",v_nombre_archivo 
  CALL v_ch_arch_salida.write([v_encabezado])

  --LET v_encabezado = " NSS |FOLIO SUA |PERIODO PAGO |FECHA PAGO |NRP |APORTACIÓN |AIVS |FOLIO LIQUIDACIÓN |CONCILIADO"
  LET v_encabezado = " NSS |PERIODO PAGO |APORTACIÓN |AIVS |FOLIO LIQUIDACIÓN |TIPO"
  CALL v_ch_arch_salida.write([v_encabezado])
  
  {LET g_sql_txt = " SELECT a.nss, ",
                  "        a.periodo_pago, ",
                  "        a.imp_apo_pat, ",
                  "        a.aivs, ",
                  "        a.folio, ",
                  "        ' ' ",
                  " FROM   tmp_dis_subs_cf a, ",
                  "        glo_folio b ",
                  " WHERE  a.folio        = b.folio ",
                  " AND    b.proceso_cod <> 1401 ",
                  " UNION ALL ",
                  " SELECT a.nss, ",
                  "        a.periodo_pago, ",
                  "        a.imp_apo_pat, ",
                  "        a.aivs, ",
                  "        a    .folio, ",
                  "        ' ' ",
                  " FROM   tmp_dis_subs_cf a ",
                  " WHERE  a.folio IS NULL ",
                  "ORDER BY a.folio DESC, a.nss, a.periodo_pago DESC"}

  LET g_sql_txt = " SELECT a.nss, ",
                  "        a.periodo_pago, ",
                  "        a.imp_apo_pat, ",
                  "        a.aivs, ",
                  "        a.folio, ",
                  "        a.localiza_trabajador ",
                  " FROM   tmp_dis_subs_cf a, ",
                  "        glo_folio b ",
                  " WHERE  a.folio        = b.folio ",
                  " UNION ALL ",
                  " SELECT a.nss, ",
                  "        a.periodo_pago, ",
                  "        a.imp_apo_pat, ",
                  "        a.aivs, ",
                  "        a    .folio, ",
                  "        ' ' ",
                  " FROM   tmp_dis_subs_cf a ",
                  " WHERE  a.folio IS NULL ",
                  "ORDER BY a.folio DESC, a.nss, a.periodo_pago DESC"
                  
  --DISPLAY g_sql_txt
  PREPARE ps_slc_ap FROM g_sql_txt
  DECLARE cur_sl_ap CURSOR FOR ps_slc_ap 
  LET v_indice = 1 
  FOREACH cur_sl_ap INTO v_arr_conc[v_indice].v_nss,
                         --v_arr_conc[v_indice].v_folio_sua,
                         v_arr_conc[v_indice].v_periodo_pago,
                         --v_arr_conc[v_indice].v_f_pago,
                         --v_arr_conc[v_indice].v_nrp,
                         v_arr_conc[v_indice].v_imp_apo_pat, 
                         v_arr_conc[v_indice].v_aivs,
                         v_arr_conc[v_indice].v_folio_liquida,
                         v_arr_conc[v_indice].v_conciliado

    SELECT cp.proceso_cod, cp.proceso_desc
    INTO   v_proceso_cod, v_proceso_desc
    FROM   cat_proceso cp,
           glo_folio gl
    WHERE  cp.proceso_cod = gl.proceso_cod
    AND    gl.folio       = v_arr_conc[v_indice].v_folio_liquida 

    IF (v_proceso_cod                     = 1401 AND
        v_arr_conc[v_indice].v_conciliado = 1)   THEN
        LET v_proceso_cod  = NULL
        LET v_proceso_desc = NULL
        --LET v_edo_folio    = NULL
        --LET v_f_actualiza  = NULL
        CONTINUE FOREACH
    END IF
        
    IF (v_proceso_desc IS NULL OR 
        v_proceso_desc = ' ')  THEN
        LET v_arr_conc[v_indice].v_conciliado = 'SIN PAGO LQINFO'
    ELSE
       IF v_proceso_cod <> 1401 THEN
          LET v_arr_conc[v_indice].v_conciliado = 'PAGO EN ACLARATORIO'
       ELSE 
          IF v_arr_conc[v_indice].v_conciliado = 3 THEN
             LET v_arr_conc[v_indice].v_conciliado = 'PAGO EN ACLARATORIO'
          ELSE
             LET v_arr_conc[v_indice].v_conciliado = v_proceso_desc
          END IF
       END IF
    END IF

    IF (v_arr_conc[v_indice].v_imp_apo_pat = 0   AND
        v_arr_conc[v_indice].v_aivs        = 0 ) THEN
        LET v_arr_conc[v_indice].v_conciliado = 'APORTACIÓN SUBSECUENTE EN CEROS'
    END IF

    IF v_edo_folio = 2 THEN
       SELECT vf.precio_fondo
       INTO   v_precio_fondo
       FROM   glo_valor_fondo vf
       WHERE  vf.fondo       = 11
       AND    vf.f_valuacion = v_f_actualiza

       LET v_arr_conc[v_indice].v_imp_apo_pat = (v_arr_conc[v_indice].v_aivs * v_precio_fondo)
    END IF
    
    LET v_detalle = v_arr_conc[v_indice].v_nss, "|",
                    --v_arr_conc[v_indice].v_folio_sua, "|",
                    v_arr_conc[v_indice].v_periodo_pago, "|",
                    --v_arr_conc[v_indice].v_f_pago USING "dd-mm-yyyy", "|",
                    --v_arr_conc[v_indice].v_nrp, "|",
                    v_arr_conc[v_indice].v_imp_apo_pat, "|",
                    v_arr_conc[v_indice].v_aivs, "|",
                    v_arr_conc[v_indice].v_folio_liquida, "|",
                    v_arr_conc[v_indice].v_conciliado CLIPPED, "|"

    CALL v_ch_arch_salida.write([v_detalle])
      
    LET v_indice = v_indice + 1

    LET v_proceso_cod  = NULL
    LET v_proceso_desc = NULL
    --LET v_edo_folio    = NULL
    --LET v_f_actualiza  = NULL

  END FOREACH

  CALL v_arr_conc.deleteElement(v_indice)
  LET v_indice = v_indice - 1
   
  -- Escribe el sumario
  {LET v_sumario = "NO CONCILIADOS:|",r_no_conciliados USING "###,###,###,##&","|"
  CALL v_ch_arch_salida.write([v_sumario])

  LET v_sumario = "CONCILIADOS:|",r_conciliados USING "###,###,###,##&","|"                  
  CALL v_ch_arch_salida.write([v_sumario])
  
  LET v_sumario = "TOTAL DE REGISTROS:|",r_tot_registros,"|"                  
  CALL v_ch_arch_salida.write([v_sumario])}  

  -- Cierra el archivo
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Cifras Control Aportaciones Subsecuentes\n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_con_disp(g_usuario,
                    v_fecha_actual,
                    v_folio, 
                    v_nombre_archivo,
                    v_arr_tot)

  DEFINE v_arr_tot           RECORD
    v_folio                  DECIMAL(10,0), --Folio Registro de Pagos
    v_proceso                CHAR(40),      --Proceso
    v_monto_aivs             DECIMAL(18,6), --Monto total de AVIS
    v_monto_apo              DECIMAL(12,2), --Monto total de pesos
    v_tot_registros          DECIMAL(12,2)  --Total de registros 
  END RECORD

  DEFINE 
    g_usuario                VARCHAR(30),  --Almacena al usuario
    v_fecha_actual           DATE,
    v_folio                  DECIMAL(9,0), --Folio liquidación dispersión
    v_nombre_archivo         CHAR(100)

  FORMAT
   FIRST PAGE HEADER      
     PRINTX g_usuario
     PRINTX v_fecha_actual USING "dd-mm-yyyy"
     PRINTX v_folio
     PRINTX v_nombre_archivo      

   ON EVERY ROW
      PRINTX v_arr_tot.v_folio
      PRINTX v_arr_tot.v_proceso
      PRINTX v_arr_tot.v_monto_aivs
      PRINTX v_arr_tot.v_monto_apo
      PRINTX v_arr_tot.v_tot_registros

END REPORT