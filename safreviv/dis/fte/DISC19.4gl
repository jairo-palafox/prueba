################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 21/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC19                                                   #
#Objetivo          => Programa para consultar los Créditos Ceros Liquidados.   #
#                                                                              #
#Fecha inicio      => 04/03/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE g_sql_txt           STRING,      --Consultas
         g_usuario           VARCHAR(30), --Almacena al usuario
         g_tipo_proceso      SMALLINT,    --Forma como ejecutara el programa
         g_nom_prog          VARCHAR(30), --Almacena opción del menú
         p_proceso_cod       SMALLINT,    --codigo del proceso
         v_proceso_cod       SMALLINT,    --codigo del proceso
         p_opera_cod         SMALLINT,    --codigo de operacion
         p_pid               DECIMAL(9,0)

  DEFINE v_tot_registros     DECIMAL(9,0), -- Total de registros
         v_sum_aivs          DECIMAL(18,6),
         v_sum_aportacion    DECIMAL(12,2), 
         v_sum_amortizacion  DECIMAL(12,2)

  DEFINE v_arr_cred_cero_liq DYNAMIC ARRAY OF RECORD
         folio               DECIMAL(9,0),
         nss                 CHAR(11),
         num_credito		 DECIMAL(10),
         periodo_pago	     CHAR(6),
         f_pago	             DATE,
         nrp	             CHAR(11),
         aportacion	      	 DECIMAL(12,2),
         amortizacion	   	 DECIMAL(12,2),
         folio_sua	     	 DECIMAL(6,0),
         --inconsistencia      CHAR(30),
         num_credito_act     DECIMAL(10,0),
         aportacion_liq      DECIMAL(12,2),
         amortizacion_liq    DECIMAL(12,2),
         aivs_liq            DECIMAL(22,2),
         estado              CHAR(30),
         id_derechohabiente  DECIMAL(9,0)
  END RECORD

  DEFINE v_arr_cifras_reporte DYNAMIC ARRAY OF RECORD
         v_estado            SMALLINT,
         v_desc_estado       VARCHAR(50),
         v_tot_estado        INTEGER
  END RECORD

  DEFINE v_edo_credito_desc  VARCHAR(40),
         v_desc_tipo_trabajador  VARCHAR(40),
         v_desc_estado       VARCHAR(50)

  DEFINE v_nombre_completo   VARCHAR(50), --Nombre del derechohabiente
         v_periodo_cvt1      VARCHAR(6),
         v_periodo_cvt2      VARCHAR(6)

  DEFINE v_mensaje           STRING

  DEFINE v_precio_fondo      DECIMAL(19,14)
   
END GLOBALS

MAIN
  DEFINE f_folio             DECIMAL(9,0),  --Folio de la liquidación de la dispersión
         r_registros         DECIMAL(9,0),  --Bandera de consulta avance de pago
         r_sum_aivs          DECIMAL(18,6),
         r_sum_aportacion    DECIMAL(12,2),
         r_sum_amortizacion  DECIMAL(12,2) 
  
  DEFINE f_ventana           ui.Window, --Define las propìedades de la Ventana
         f_forma             ui.Form ,  --Define las propiedades de la forma
         v_ruta_listados     CHAR(40),
         --v_cuenta_derechohabiente SMALLINT,
         v_cuenta_folio      INTEGER,
         bnd_consulta        SMALLINT,
         r_bnd_periodo       SMALLINT,
         v_qwery_ibx         STRING 

  DEFINE l_comando           STRING
  DEFINE v_ruta_ejecutable   CHAR(40)
  DEFINE l_v_arch_proceso    VARCHAR(100)
   
  DEFINE v_folio_disp        DECIMAL(9,0)
  DEFINE p_programa          CHAR(10),  
         r_bandera           SMALLINT,
         r_nom_archivo       CHAR(40)

  DEFINE v_proc_entra        SMALLINT,
         v_proc_val          SMALLINT,
         v_cod_conv          SMALLINT,
         v_desc_proc_val     CHAR(40),
         v_mensaje_val       STRING

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
   
  LET p_proceso_cod  = 920
  LET p_opera_cod    = 1 
  LET r_bnd_periodo  = 0

  CALL STARTLOG (g_usuario CLIPPED||".DISC19.log")

  INITIALIZE l_v_arch_proceso TO NULL

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
   
  OPEN WINDOW w_cred_cero_liq WITH FORM "DISC191"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", TRUE ) --Oculta detalle de la consulta

          NEXT FIELD f_folio
          CALL ui.interface.refresh()
          
          ON ACTION ACCEPT 
             LET v_cuenta_folio = 0
             LET r_registros = 0
               
             IF f_folio IS NOT NULL THEN
                --Valida que exista el folio
                SELECT COUNT(*) 
                INTO   v_cuenta_folio
                FROM   dis_liq_inconsistente
                WHERE  folio_liquida = f_folio
             ELSE
                SELECT COUNT(*) 
                INTO   v_cuenta_folio
                FROM   dis_liq_inconsistente
             END IF

             DISPLAY "f_folio: ",f_folio
             DISPLAY "v_cuenta_folio: ", v_cuenta_folio
               
             IF v_cuenta_folio = 0  THEN 
                CALL fn_mensaje("Atención","No existen registros para el folio capturado",
                                "about")
                NEXT FIELD f_folio
             ELSE
                CALL fn_consulta_inconsistencias(f_folio)
                RETURNING r_registros,
                          r_sum_aivs,
                          r_sum_aportacion,
                          r_sum_amortizacion

                DISPLAY "r_registros -- ",r_registros
             END IF
            
             IF r_registros > 0 THEN
                CALL f_forma.setElementHidden("gr_detalle", FALSE ) --muestra detalle de la consulta
              
                DISPLAY ARRAY v_arr_cred_cero_liq TO rcd_detalle.* 
                --DISPLAY ARRAY v_arr_incon_pantalla TO rcd_detalle.* 
                ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)

                  BEFORE ROW
                  --CALL fn_obtiene_nombre_derechohabiente(v_arr_cred_cero_liq[ARR_CURR()].id_derechohabiente) 
             
                  BEFORE DISPLAY 
                    DISPLAY r_registros        TO f_total_registros
                    DISPLAY r_sum_aivs         TO f_suma_aivs
                    DISPLAY r_sum_aportacion   TO f_suma_aportaciones
                    DISPLAY r_sum_amortizacion TO f_suma_amortizaciones

                  AFTER DISPLAY 
                    CALL ui.interface.refresh()
                    CALL DIALOG.setActionHidden("reporte",0)
                    CALL DIALOG.setActionHidden("btn_liquidar", 0)
                    CALL ui.interface.refresh()

                  ON ACTION cancelar 
                     EXIT PROGRAM 
                    
                  ON ACTION reporte
                     CALL fn_reporte_cred_cero_liq(f_folio)
                   
                  --Genera archivo
                   ON ACTION archivo
                      CALL fn_genera_archivo_inconsistencias(f_folio)   
                END DISPLAY     
             ELSE
                CALL fn_mensaje("ATENCIÓN",
                                "No se encontraron registros",
                                "about")
                NEXT FIELD f_folio
                CALL ui.interface.refresh()
             END IF
      END INPUT
      
      ON ACTION cancelar
            EXIT DIALOG
 
    END DIALOG 
  CLOSE WINDOW w_cred_cero_liq 
END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consulta_inconsistencias(f_folio)
  DEFINE f_folio             DECIMAL(9,0), --Folio de liquidación dispersión
         v_indice            INTEGER

  DEFINE j                   INTEGER

  LET g_sql_txt = "\n SELECT dli.folio_liquida, ",
                  "\n        cero.nss, ",
                  "\n        cero.num_credito, ",
                  "\n        cero.periodo_pago, ",
                  "\n        cero.f_pago, ",
                  "\n        cero.nrp, ",
                  "\n        NVL(cero.aportacion,0), ",
                  "\n        NVL(cero.amortizacion,0), ",
                  "\n        cero.folio_sua, ",
                  "\n        dli.num_credito,",
                  "\n        NVL(dli.aportacion,0),",
                  "\n        NVL(dli.amortizacion,0),",
                  "\n        NVL(dli.aivs,0),",
                  --"\n        cero.inconsistencia, ",
                  "\n        dli.edo_liquida, ",
                  "\n        cero.id_derechohabiente ",
                  "\n FROM   dis_arh_num_cred_0 cero, ",
                  "\n        dis_liq_inconsistente dli ",
                  "\n WHERE  cero.id_dis_arh_num_cred = dli.id_dis_arh_num_cred ",
                  "\n AND    cero.folio               = dli.folio_arh_num_cred ",
                  "\n AND    dli.edo_liquida         IN(2,3,4) "

  IF f_folio IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt||"\n AND  dli.folio_liquida = ", f_folio,
                                "\n ORDER BY dli.folio_liquida DESC, cero.periodo_pago"
  ELSE
     LET g_sql_txt = g_sql_txt||"\n ORDER BY dli.folio_liquida DESC, cero.periodo_pago"
  END IF

  DISPLAY "g_sql_txt -- ",g_sql_txt
  PREPARE ps_cred_cero_liq FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_sum_aivs         = 0.00
  LET v_sum_aportacion   = 0.00
  LET v_sum_amortizacion = 0.00

  LET j = 1
  CALL v_arr_cifras_reporte.clear()
  LET v_arr_cifras_reporte[j].v_tot_estado = 0

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_inconsistencia CURSOR FOR ps_cred_cero_liq
  FOREACH cur_inconsistencia INTO v_arr_cred_cero_liq[v_indice].folio,
                                  v_arr_cred_cero_liq[v_indice].nss,
                                  v_arr_cred_cero_liq[v_indice].num_credito,
                                  v_arr_cred_cero_liq[v_indice].periodo_pago,
                                  v_arr_cred_cero_liq[v_indice].f_pago,
                                  v_arr_cred_cero_liq[v_indice].nrp,
                                  v_arr_cred_cero_liq[v_indice].aportacion,
                                  v_arr_cred_cero_liq[v_indice].amortizacion,
                                  v_arr_cred_cero_liq[v_indice].folio_sua,
                                  v_arr_cred_cero_liq[v_indice].num_credito_act,
                                  v_arr_cred_cero_liq[v_indice].aportacion_liq,
                                  v_arr_cred_cero_liq[v_indice].amortizacion_liq,
                                  v_arr_cred_cero_liq[v_indice].aivs_liq,
                                  --v_arr_cred_cero_liq[v_indice].inconsistencia,
                                  v_arr_cred_cero_liq[v_indice].estado,
                                  v_arr_cred_cero_liq[v_indice].id_derechohabiente
                                   
    {CASE v_arr_cred_cero_liq[v_indice].estado
      WHEN 0
        LET v_arr_cifras_reporte[1].v_estado      = 0
        LET v_arr_cifras_reporte[1].v_desc_estado = "PENDIENTE POR LIQUIDAR"
        LET v_arr_cifras_reporte[1].v_tot_estado  = v_arr_cifras_reporte[1].v_tot_estado + 1
      WHEN 1
        LET v_arr_cifras_reporte[2].v_estado      = 1
        LET v_arr_cifras_reporte[2].v_desc_estado = "RECHAZADO"
        LET v_arr_cifras_reporte[2].v_tot_estado  = v_arr_cifras_reporte[2].v_tot_estado + 1
      WHEN 2
        LET v_arr_cifras_reporte[3].v_estado      = 2
        LET v_arr_cifras_reporte[3].v_desc_estado = "LIQUIDADO"
        LET v_arr_cifras_reporte[3].v_tot_estado  = v_arr_cifras_reporte[3].v_tot_estado + 1
      WHEN 3
        LET v_arr_cifras_reporte[4].v_estado      = 3
        LET v_arr_cifras_reporte[4].v_desc_estado = "LIQUIDA AMORTIZACIÓN"
        LET v_arr_cifras_reporte[4].v_tot_estado  = v_arr_cifras_reporte[4].v_tot_estado + 1
      WHEN 4
        LET v_arr_cifras_reporte[5].v_estado      = 4
        LET v_arr_cifras_reporte[5].v_desc_estado = "LIQUIDA APORTACIÓN"
        LET v_arr_cifras_reporte[5].v_tot_estado  = v_arr_cifras_reporte[5].v_tot_estado + 1
      OTHERWISE
    END CASE}

    IF v_indice = 1 THEN
       LET v_arr_cifras_reporte[j].v_estado     = v_arr_cred_cero_liq[v_indice].estado
       LET v_arr_cifras_reporte[j].v_tot_estado = v_arr_cifras_reporte[j].v_tot_estado + 1

       {IF v_arr_cifras_reporte[j].v_estado = 2 THEN
          LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
       ELSE
          IF v_arr_cifras_reporte[j].v_estado = 1 THEN
             LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
          ELSE
             IF v_arr_cifras_reporte[j].v_estado = 0 THEN
                LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
             END IF
          END IF
       END IF}

       CASE v_arr_cifras_reporte[j].v_estado
         WHEN 0
           LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
         WHEN 1
           LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
         WHEN 2
           LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
         WHEN 3
           LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA AMORTIZACIÓN"
         WHEN 4
           LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA APORTACIÓN"
         OTHERWISE
       END CASE
    ELSE
       IF v_arr_cred_cero_liq[v_indice - 1].estado <> v_arr_cred_cero_liq[v_indice].estado THEN
          LET j = j + 1
          LET v_arr_cifras_reporte[j].v_tot_estado = 0
          LET v_arr_cifras_reporte[j].v_estado     =  v_arr_cred_cero_liq[v_indice].estado
          LET v_arr_cifras_reporte[j].v_tot_estado = v_arr_cifras_reporte[j].v_tot_estado + 1
            
          {IF v_arr_cifras_reporte[j].v_estado = 2 THEN
            LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
          ELSE
            IF v_arr_cifras_reporte[j].v_estado = 1 THEN
               LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
            ELSE
               IF v_arr_cifras_reporte[j].v_estado = 0 THEN
                  LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
               END IF
            END IF
          END IF}

          CASE v_arr_cifras_reporte[j].v_estado
            WHEN 0
              LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
            WHEN 1
              LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
            WHEN 2
              LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
            WHEN 3
              LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA AMORTIZACIÓN"
            WHEN 4
              LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA APORTACIÓN"
            OTHERWISE
          END CASE
       ELSE
          LET v_arr_cifras_reporte[j].v_tot_estado = v_arr_cifras_reporte[j].v_tot_estado + 1
       END IF
    END IF
                                   
    LET v_tot_registros    = v_tot_registros    + 1
    LET v_sum_aivs         = v_sum_aivs         + v_arr_cred_cero_liq[v_indice].aivs_liq
    LET v_sum_aportacion   = v_sum_aportacion   + v_arr_cred_cero_liq[v_indice].aportacion_liq
    LET v_sum_amortizacion = v_sum_amortizacion + v_arr_cred_cero_liq[v_indice].amortizacion_liq
    LET v_indice           = v_indice           + 1
  END FOREACH

  IF v_arr_cifras_reporte[v_arr_cifras_reporte.getLength()].v_estado IS NULL THEN
     CALL v_arr_cifras_reporte.deleteElement(v_arr_cifras_reporte.getLength())
  END IF

  CALL v_arr_cred_cero_liq.deleteElement(v_arr_cred_cero_liq.getLength())
  LET v_tot_registros    = v_tot_registros - 1

  FOR j = 1 TO v_arr_cred_cero_liq.getLength()
      CASE v_arr_cred_cero_liq[j].estado
        WHEN 4
          LET v_arr_cred_cero_liq[j].estado = v_arr_cred_cero_liq[j].estado CLIPPED||" - LIQUIDA APORTACIÓN"
        WHEN 3
          LET v_arr_cred_cero_liq[j].estado = v_arr_cred_cero_liq[j].estado CLIPPED||" - LIQUIDA AMORTIZACIÓN"
        WHEN 2
          LET v_arr_cred_cero_liq[j].estado = v_arr_cred_cero_liq[j].estado CLIPPED||" - LIQUIDADO"
        WHEN 1
          LET v_arr_cred_cero_liq[j].estado = v_arr_cred_cero_liq[j].estado CLIPPED||" - RECHAZADO"
        WHEN 0
          LET v_arr_cred_cero_liq[j].estado = v_arr_cred_cero_liq[j].estado CLIPPED||" - PENDIENTE POR LIQUIDAR"
        OTHERWISE
          --ESTADO INCORRECTO
      END CASE
  END FOR

  RETURN v_tot_registros,
         v_sum_aivs,
         v_sum_aportacion,
         v_sum_amortizacion

END FUNCTION

#Objetivo: Genera reporte de inconsistencias de numero de crédito igual a cero
FUNCTION fn_reporte_cred_cero_liq(v_rfolio)
  DEFINE v_rfolio            DECIMAL(9,0), --Folio de liquidación de dispersión
         manejador_rpt       om.SaxDocumentHandler, --Contenedor documentos reporte
         v_rep_indice        INTEGER
  
  LET v_rep_indice = 1
  
  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC191.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte de registros con rechazo
  START REPORT rep_cred_cero_liq TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO  v_arr_cifras_reporte.getLength()
        OUTPUT TO REPORT rep_cred_cero_liq( v_arr_cifras_reporte[v_rep_indice].*,
                                            v_tot_registros,
                                           v_sum_aivs,
                                           v_sum_aportacion,
                                           v_sum_amortizacion,
                                           g_usuario,
                                           v_rfolio)
    END FOR
  FINISH REPORT rep_cred_cero_liq
END FUNCTION

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE v_id_consulta       DECIMAL(9,0)

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#Objetivo: genera el archivo con la consulta obtenida
FUNCTION fn_genera_archivo_inconsistencias(v_folio)
  DEFINE v_nom_archivo       VARCHAR(50), -- nombre del archivo de salida
         v_ruta_envio_dis    CHAR(40),
         v_ruta_nomarch      VARCHAR(100), -- ruta y nombre del archivo de salida
         v_ch_arch_salida    BASE.CHANNEL,
         v_recorre_arreglo   INTEGER,
         v_comando_dos       STRING,
         v_encabezado        STRING,
         v_detalle           STRING,
         v_sumario           STRING,
         v_folio             DECIMAL(9,0), --Folio liquidación dispersión
         v_nss               CHAR(11) --NSS trabajador

  DEFINE v_fecha_archivo     DATE,  
         v_hora_archivo      DATETIME HOUR TO HOUR ,
         v_min_archivo       DATETIME MINUTE TO MINUTE,
         v_sec_archivo       DATETIME SECOND TO SECOND,
         v_hora              STRING

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/consulta_info_cred_cero_liq_", v_hora

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
   
  --Obtiene el valor de fondo (AIVS)
  SELECT f.precio_fondo
  INTO   v_precio_fondo
  FROM   glo_valor_fondo f
  WHERE  f.fondo       = 11
  AND    f.f_valuacion = TODAY
   
  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA ",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])

  --Si se solicitó el folio de dispersión se incluye en el encabezado
  IF v_folio IS NOT NULL THEN
     LET v_encabezado = " FOLIO ",v_folio
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  LET v_encabezado = " VALOR DE FONDO ",v_precio_fondo
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = " FOLIO |NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |MONTO AMORTIZACIÓN |FOLIO SUA |NÚMERO DE CRÉDITO ACTIVO |MONTO APORTACIÓN LIQUIDADO | MONTO AMORTIZACION LIQUIDADO | AIVS| ESTADO "
  CALL v_ch_arch_salida.write([v_encabezado])
   
  FOR v_recorre_arreglo = 1 TO v_arr_cred_cero_liq.getLength()
      LET v_detalle = v_arr_cred_cero_liq[v_recorre_arreglo].folio USING "#########&" ,"|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].nss USING "##########&" ,"|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].num_credito USING "&&&&&&&&&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].periodo_pago USING "&&&&&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].f_pago USING "dd-mm-yyyy","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].nrp,"|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].aportacion USING "-##,###,###,##&.&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].amortizacion  USING "-##,###,###,##&.&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].folio_sua USING "&&&&&&","|",
                      --v_arr_cred_cero_liq[v_recorre_arreglo].inconsistencia  USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].num_credito_act USING "&&&&&&&&&&", "|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].aportacion_liq  USING "-##,###,###,##&.&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].amortizacion_liq USING "-##,###,###,##&.&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].aivs_liq USING "-##,###,###,##&.&&","|",
                      v_arr_cred_cero_liq[v_recorre_arreglo].estado --,
                      --v_arr_cred_cero_liq[v_recorre_arreglo].id_derechohabiente USING "&&&&&&&&&","|"
                                  
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
   
  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros USING "-##,###,###,##&","| | | | | | | | | ",
                  v_sum_aportacion USING "-##,###,###,##&.&&", "|",
                  v_sum_amortizacion USING "-##,###,###,##&.&&", "|",
                  v_sum_aivs USING "-##,###,###,##&.&&"
                  
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo Creditos Cero Liquidados \n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_cred_cero_liq(v_rep_inconsist, 
                         v_rep_tot_registros, 
                         v_rep_sum_aivs, 
                         v_rep_sum_aportacion,
                         v_rep_sum_amortizacion, 
                         v_usuario,
                         v_rep_folio)

  DEFINE v_rep_inconsist     RECORD
         v_estado            SMALLINT,
         v_desc_estado       VARCHAR(50),
         v_tot_estado        INTEGER
  END RECORD

  DEFINE v_fecha_consulta         DATE, -- Fecha de proceso
         v_usuario                VARCHAR(30), -- Almacena al usuario
         v_rep_tot_registros      DECIMAL(9,0), -- Total de registros
         v_rep_sum_aivs           DECIMAL(18,6),
         v_rep_sum_aportacion     DECIMAL(12,2),
         v_rep_sum_amortizacion   DECIMAL(12,2),
         v_rep_folio              DECIMAL(9,0)

  FORMAT
    PAGE HEADER
      LET v_fecha_consulta = TODAY
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_rep_folio
      
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_rep_folio

    ON EVERY ROW
       PRINTX v_rep_inconsist.v_estado
       PRINTX v_rep_inconsist.v_desc_estado
       PRINTX v_rep_inconsist.v_tot_estado

    ON LAST ROW
       PRINTX v_rep_tot_registros
       PRINTX v_rep_sum_aivs
       PRINTX v_rep_sum_aportacion
       PRINTX v_rep_sum_amortizacion

END REPORT