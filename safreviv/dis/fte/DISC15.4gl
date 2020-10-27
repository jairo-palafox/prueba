################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 18/02/2019                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC15                                                   #
#Objetivo          => Programa para consultar o identificar los registros con  #
#                     avances abiertos con pagos por cubrir.                   #
#Fecha inicio      => 16/04/2014                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            SMALLINT,    --codigo del proceso
    p_opera_cod              SMALLINT,    --codigo de operacion
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tot_registros          DECIMAL(9,0), -- Total de registros
    v_sum_aportacion         DECIMAL(12,2), 
    v_sum_amortizacion       DECIMAL(12,2),
    v_sum_aivs               DECIMAL(18,6),
    v_sum_apo_pag            DECIMAL(12,2), 
    v_sum_amo_pag            DECIMAL(12,2)
    
  DEFINE v_arr_ava_por_cub   DYNAMIC ARRAY OF RECORD
    v_arr_nss                CHAR(11),
    v_arr_nombre_completo    VARCHAR(50),
    v_arr_folio_dis          DECIMAL(9,0),
    v_arr_periodo_pago       CHAR(6),
    v_arr_nrp                CHAR(11),     
    v_arr_num_credito        DECIMAL(10,0),
    v_arr_monto_aportacion   DECIMAL(12,2),
    v_arr_monto_amortizacion DECIMAL(12,2), 
    v_arr_folio_pag          DECIMAL(9,0),
    v_arr_aivs               DECIMAL(18,6),
    v_arr_monto_apo_pag      DECIMAL(12,2),
    v_arr_monto_amo_pag      DECIMAL(12,2),     
    v_arr_id_derecho         DECIMAL(9,0)
  END RECORD
  
  DEFINE 
    v_nombre_completo        VARCHAR(50), --Nombre del derechohabiente
    v_pp                     CHAR(6),     --Periodo de Pago
    v_existe                 DECIMAL(9,0)

END GLOBALS

MAIN
  DEFINE 
    f_folio                  DECIMAL(9,0),  --Folio de la liquidación de la dispersión
    f_nss                    CHAR(11),      --NSS trabajador
    r_registros              DECIMAL(9,0),  --Bandera de consulta avance de pago
    r_sum_aportacion         DECIMAL(12,2),
    r_sum_amortizacion       DECIMAL(12,2), 
    r_sum_aivs               DECIMAL(18,6),
    r_sum_apo_pag            DECIMAL(12,2),
    r_sum_amo_pag            DECIMAL(12,2) 
  
  DEFINE 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form ,  --Define las propiedades de la forma
    v_ruta_listados          CHAR(40),
    v_cuenta_derechohabiente SMALLINT,
    v_cuenta_folio           INTEGER,
    bnd_consulta             SMALLINT,
    r_bnd_periodo            SMALLINT,
    v_qwery_ibx              STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 903
  LET p_opera_cod    = 1 
  LET r_bnd_periodo  = 0

  --Validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  {IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF}

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
   
  OPEN WINDOW vtn_avance_con_cub WITH FORM "DISC151"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio, f_nss
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_nombre", TRUE )  --Oculta el nombre del trabajador
          CALL f_forma.setElementHidden("gr_detalle", TRUE ) --Oculta detalle de la consulta
          CALL ui.interface.refresh()
       
        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro
           IF (f_folio IS NULL) AND 
              (f_nss   IS NULL) THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar un criterio para la búsqueda",
                              "about")
              NEXT FIELD f_folio   
           END IF  

           IF f_folio IS NOT NULL THEN
              --Valida que exista el folio
              SELECT COUNT(*) 
              INTO   v_cuenta_folio
              FROM   dis_det_avance_pago
              WHERE  folio  = f_folio
              AND    estado = 30
              IF v_cuenta_folio  = 0    OR 
                 v_cuenta_folio IS NULL THEN 
                 CALL fn_mensaje("Atención","No existen registros para el folio capturado",
                                 "about")
                 NEXT FIELD f_folio
              END IF 
           END IF
               
           IF f_nss IS NOT NULL THEN 
              IF LENGTH(f_nss) = 11 THEN 
                 --Valida que exista el nss
                 SELECT COUNT(id_derechohabiente) 
                 INTO   v_cuenta_derechohabiente
                 FROM   afi_derechohabiente
                 WHERE  nss = f_nss
                 IF v_cuenta_derechohabiente = 0     OR 
                    v_cuenta_derechohabiente IS NULL THEN 
                    CALL fn_mensaje("Atención","El NSS no existe",
                                    "about")
                    NEXT FIELD f_nss
                 END IF 
              ELSE 
                 CALL fn_mensaje("Atención","El NSS debe ser de 11 caracteres",
                                 "about")
                 NEXT FIELD f_nss
              END IF 
           END IF  

           DISPLAY "f_nss -- ",f_nss
           DISPLAY "f_folio -- ",f_folio
            
           CALL fn_consulta_ava_por_cub(f_folio,
                                        f_nss)
           RETURNING r_registros,
                     r_sum_aportacion,
                     r_sum_amortizacion,           
                     r_sum_aivs,
                     r_sum_apo_pag,
                     r_sum_amo_pag

           DISPLAY "r_registros -- ",r_registros

           IF r_registros > 0 THEN
              CALL f_forma.setElementHidden("gr_detalle", FALSE ) --muestra detalle de la consulta
               
              DISPLAY ARRAY v_arr_ava_por_cub TO rcd_detalle.* 
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE ROW
                  CALL fn_obtiene_nombre_derechohabiente(v_arr_ava_por_cub[ARR_CURR()].v_arr_id_derecho) 
               
                BEFORE DISPLAY 
                  DISPLAY r_registros        TO f_total_registros
                  DISPLAY r_sum_aportacion   TO f_suma_aportaciones
                  DISPLAY r_sum_amortizacion TO f_suma_amortizaciones
                  DISPLAY r_sum_aivs         TO f_suma_aivs
                  DISPLAY r_sum_apo_pag      TO f_suma_apo_pag
                  DISPLAY r_sum_amo_pag      TO f_suma_amo_pag

                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                --Genera archivo
                ON ACTION archivo
                   CALL fn_genera_archivo_inconsistencias(f_folio,
                                                          f_nss)                             
              END DISPLAY                    
           ELSE
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros",
                              "about")
              CALL ui.interface.refresh()
           END IF
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG
          
    END DIALOG 
  CLOSE WINDOW vtn_avance_con_cub

END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consulta_ava_por_cub(f_folio, f_nss)
  DEFINE 
    f_folio                  DECIMAL(9,0), --Folio de liquidación dispersión
    f_nss                    CHAR(11),     --NSS trabajador
    v_derechohabiente        DECIMAL(9,0), --Id
    v_indice                 INTEGER,
    v_val_fol                DECIMAL(9,0)

  CALL fn_cons_ava_ab(f_folio, f_nss)
  RETURNING v_derechohabiente
  
  LET g_sql_txt = "\n SELECT ap.nss, ",
                  "\n        ap.nombre_completo, ",
                  "\n        ap.folio, ",
                  "\n        ap.periodo_pago, ",
                  "\n        ap.nrp, ",
                  "\n        ap.num_credito, ",
                  "\n        ap.monto_aportacion, ",
                  "\n        ap.monto_amortizacion, ",
                  "\n        pg.folio, ",
                  "\n        pg.aiv_ap_pat, ",
                  "\n        pg.imp_ap_pat, ",
                  "\n        pg.imp_am_cre, ",
                  "\n        pg.id_derechohabiente ",
                  "\n FROM   tmp_dis_ava_con_pago ap, ",
                  "\n        cta_his_pagos pg ",
                  "\n WHERE  pg.id_derechohabiente             = ap.id_derechohabiente ",
                  "\n AND    fn_bimestre_pago(pg.periodo_pago) = ap.periodo_pago ",
                  "\n AND    pg.nrp                            = ap.nrp ",
                  "\n AND    pg.ind_liquidacion           NOT IN (1,6) ",
                  "\n AND    pg.destino_ap_viv                 = 1",
                  "\n ORDER BY 1,3"
                 
  DISPLAY "g_sql_txt -- ",g_sql_txt
  PREPARE prp_sql_inconsistencia FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_sum_aportacion   = 0.00
  LET v_sum_amortizacion = 0.00
  LET v_sum_apo_pag      = 0.00
  LET v_sum_amo_pag      = 0.00
  LET v_sum_aivs         = 0.00

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_inconsistencia CURSOR FOR prp_sql_inconsistencia
  FOREACH cur_inconsistencia INTO v_arr_ava_por_cub[v_indice].v_arr_nss,
                                  v_arr_ava_por_cub[v_indice].v_arr_nombre_completo,
                                  v_arr_ava_por_cub[v_indice].v_arr_folio_dis,
                                  v_arr_ava_por_cub[v_indice].v_arr_periodo_pago,
                                  v_arr_ava_por_cub[v_indice].v_arr_nrp,
                                  v_arr_ava_por_cub[v_indice].v_arr_num_credito,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_aportacion,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_amortizacion,
                                  v_arr_ava_por_cub[v_indice].v_arr_folio_pag,
                                  v_arr_ava_por_cub[v_indice].v_arr_aivs,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_apo_pag,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag,
                                  v_arr_ava_por_cub[v_indice].v_arr_id_derecho

    IF v_arr_ava_por_cub[v_indice].v_arr_aivs IS NULL THEN
       LET v_arr_ava_por_cub[v_indice].v_arr_aivs = 0
    END IF

    IF v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag IS NULL THEN
       LET v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag = 0
    END IF

    --Si las aivs y amortizaciones de los pagos son menores o
    --iguales a cero no se despliega el registro
    IF (v_arr_ava_por_cub[v_indice].v_arr_aivs          <= 0.00  AND 
        v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag <= 0.00) THEN
       CONTINUE FOREACH
    END IF

    SELECT gf.folio
    INTO   v_val_fol
    FROM   glo_folio gf
    WHERE  gf.folio        = v_arr_ava_por_cub[v_indice].v_arr_folio_pag
    AND    gf.folio       IN ( SELECT fg.folio_referencia
                               FROM   glo_folio fg
                               WHERE  fg.proceso_cod           = 901
                               AND    fg.status                = 2
                               AND    fg.folio_referencia IS NOT NULL)
    AND    gf.status       = 2
    AND    gf.proceso_cod IN (1401,1403)
    IF v_val_fol IS NULL THEN
       CONTINUE FOREACH
    ELSE
       LET v_val_fol = NULL
    END IF

    LET v_tot_registros    = v_tot_registros    + 1
    LET v_sum_aivs         = v_sum_aivs         + v_arr_ava_por_cub[v_indice].v_arr_aivs
    LET v_sum_aportacion   = v_sum_aportacion   + v_arr_ava_por_cub[v_indice].v_arr_monto_aportacion
    LET v_sum_amortizacion = v_sum_amortizacion + v_arr_ava_por_cub[v_indice].v_arr_monto_amortizacion
    LET v_sum_apo_pag      = v_sum_apo_pag      + v_arr_ava_por_cub[v_indice].v_arr_monto_apo_pag
    LET v_sum_amo_pag      = v_sum_amo_pag      + v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag
    LET v_indice           = v_indice           + 1

  END FOREACH

  CALL v_arr_ava_por_cub.deleteElement(v_indice)
  LET v_tot_registros    = v_tot_registros - 1

  RETURN v_tot_registros,
         v_sum_aportacion,
         v_sum_amortizacion,
         v_sum_aivs,
         v_sum_apo_pag,
         v_sum_amo_pag

END FUNCTION

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE   
    v_id_consulta            DECIMAL(9,0)

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#Objetivo: Genera el archivo con la consulta obtenida
FUNCTION fn_genera_archivo_inconsistencias(v_folio, v_nss)
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
    v_folio                  DECIMAL(9,0), --Folio liquidación dispersión
    v_nss                    CHAR(11)      --NSS trabajador

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
  LET v_nom_archivo   = "/consulta_ava_por_cub", v_hora

  --Se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --Se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE ",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])

  --Si se solicitó el folio de dispersión se incluye en el encabezado
  IF  v_folio IS NOT NULL THEN
      LET v_encabezado = " FOLIO DE CONSULTA ",v_folio
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF
    
  --Si se solicitó número de seguridad social se incluye en el encabezado
  IF  v_nss IS NOT NULL THEN
      LET v_encabezado = " NSS DE CONSULTA ",v_nss
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  LET v_encabezado = " NSS |NOMBRE COMPLETO |FOLIO AVANCE |PERIODO PAGO AVANCE|NRP AVANCE |NÚMERO DE CRÉDITO AVANCE |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |FOLIO PAGOS |AIVS |MONTO APORTACIÓN PAGO |MONTO AMORTIZACIÓN PAGO|"
  CALL v_ch_arch_salida.write([v_encabezado])
   
  FOR v_recorre_arreglo = 1 TO v_arr_ava_por_cub.getLength()
      LET v_detalle = v_arr_ava_por_cub[v_recorre_arreglo].v_arr_nss, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_nombre_completo, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_folio_dis, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_periodo_pago, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_nrp, "|",                     
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_num_credito USING "&&&&&&&&&&", "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_monto_aportacion, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_monto_amortizacion, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_folio_pag, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_aivs, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_monto_apo_pag, "|",
                      v_arr_ava_por_cub[v_recorre_arreglo].v_arr_monto_amo_pag, "|"
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
   
  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | | ",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion, "| |",
                  v_sum_aivs, "|",
                  v_sum_apo_pag, "|",
                  v_sum_amo_pag
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Avances Abiertos con Pago por Cubrir \n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: genera archivo masivo, sin parámetros de consulta
FUNCTION fn_genera_archivo_masivo()
  DEFINE         
    v_nom_archivo            VARCHAR(40),  --nombre del archivo de salida
    v_ruta_envio_dis         VARCHAR(40),
    v_ruta_nomarch           VARCHAR(100), --ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_indice                 INTEGER,
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING
  
  --Fecha y hora del archivo
  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
      
  --Creación del encabezado del archivo 
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/cons_mas_ava_por_cub", v_hora

  --Se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --Se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE ",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])
  LET v_encabezado = " NSS |NOMBRE COMPLETO |FOLIO AVANCE |PERIODO PAGO AVANCE|NRP AVANCE |NÚMERO DE CRÉDITO AVANCE |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |FOLIO PAGOS |AIVS |MONTO APORTACIÓN PAGO |MONTO AMORTIZACIÓN PAGO|"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice = 1
 
  FOR v_indice = 1 TO v_arr_ava_por_cub.getLength()
      --Concatenación del detalle
      LET v_detalle = v_arr_ava_por_cub[v_indice].v_arr_nss, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_nombre_completo, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_folio_dis, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_periodo_pago, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_nrp, "|",                     
                      v_arr_ava_por_cub[v_indice].v_arr_num_credito USING "&&&&&&&&&&", "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_aportacion, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_amortizacion, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_folio_pag, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_aivs, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_apo_pag, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag, "|"      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR

  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | | ",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion, "| |",
                  v_sum_aivs, "|",
                  v_sum_apo_pag, "|",
                  v_sum_amo_pag                  
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Avances Abiertos con Pago por Cubrir \n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

FUNCTION fn_cons_ava_ab(c_folio, c_nss)
  DEFINE 
    c_folio                  DECIMAL(9,0), --Folio de liquidación dispersión
    c_nss                    CHAR(11),     --NSS trabajador
    c_derechohabiente        DECIMAL(9,0)  --Id

  --Si se captura el nss, obtener id_derechohabiente de afi_derechoabiente
  IF LENGTH(c_nss) > 0 THEN
     SELECT id_derechohabiente 
     INTO   c_derechohabiente
     FROM   afi_derechohabiente
     WHERE  nss = c_nss
  END IF

  IF (c_folio           IS NOT NULL) AND
     (c_derechohabiente IS NULL)     THEN
     SELECT af.nss nss,
            rtrim(af.nombre_af) ||' '||
            rtrim(af.ap_paterno_af) ||' '||
            rtrim(af.ap_materno_af) nombre_completo,
            av.folio,
            av.periodo_pago,
            av.nrp,
            av.num_credito,
            av.monto_aportacion,
            av.monto_amortizacion,
            af.id_derechohabiente
     FROM   dis_det_avance_pago av,
            afi_derechohabiente af
     WHERE  av.folio              = c_folio
     AND    av.id_derechohabiente = af.id_derechohabiente
     AND    av.estado             = 30
     INTO TEMP tmp_dis_ava_con_pago
  END IF

  IF (c_folio           IS NULL)      AND
     (c_derechohabiente IS NOT NULL)  THEN
     SELECT af.nss nss,
            rtrim(af.nombre_af) ||' '||
            rtrim(af.ap_paterno_af) ||' '||
            rtrim(af.ap_materno_af) nombre_completo,
            av.folio,
            av.periodo_pago,
            av.nrp,
            av.num_credito,
            av.monto_aportacion,
            av.monto_amortizacion,
            af.id_derechohabiente
     FROM   dis_det_avance_pago av,
            afi_derechohabiente af
     WHERE  av.id_derechohabiente = c_derechohabiente
     AND    av.id_derechohabiente = af.id_derechohabiente
     AND    av.estado             = 30
     INTO TEMP tmp_dis_ava_con_pago  
  END IF

  IF (c_folio           IS NOT NULL) AND
     (c_derechohabiente IS NOT NULL) THEN
     SELECT af.nss nss,
            rtrim(af.nombre_af) ||' '||
            rtrim(af.ap_paterno_af) ||' '||
            rtrim(af.ap_materno_af) nombre_completo,
            av.folio,
            av.periodo_pago,
            av.nrp,
            av.num_credito,
            av.monto_aportacion,
            av.monto_amortizacion,
            af.id_derechohabiente
     FROM   dis_det_avance_pago av,
            afi_derechohabiente af
     WHERE  av.folio              = c_folio
     AND    av.id_derechohabiente = c_derechohabiente
     AND    av.id_derechohabiente = af.id_derechohabiente
     AND    av.estado             = 30
     INTO TEMP tmp_dis_ava_con_pago  
  END IF

  EXECUTE IMMEDIATE "CREATE INDEX xdis_ava_con_pag ON tmp_dis_ava_con_pago(id_derechohabiente, periodo_pago, nrp) in dis_ix_dbs;"
 
  UPDATE STATISTICS FOR TABLE tmp_dis_ava_con_pago

  RETURN c_derechohabiente

END FUNCTION