################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 18/02/2019                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC17                                                   #
#Objetivo          => Programa para consultar o identificar los avances que no #
#                     fueron integrados por haberse identificado con un        #
#                     registro de pago previo.                                 #
#Fecha inicio      => 11/07/2014                                               #
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
    v_tot_registros          DECIMAL(9,0),--Total de registros
    v_sum_apo_ava            DECIMAL(12,2),
    v_sum_amo_ava            DECIMAL(12,2),
    v_sum_aivs               DECIMAL(18,6),
    v_sum_aportacion         DECIMAL(12,2), 
    v_sum_amortizacion       DECIMAL(12,2)

  DEFINE v_arr_ava_sin_int   DYNAMIC ARRAY OF RECORD
    v_arr_nss                CHAR(11),
    v_arr_nombre_completo    VARCHAR(50),
    v_arr_folio_ava          DECIMAL(9,0),
    v_arr_periodo_pago       CHAR(6),
    v_arr_num_credito        DECIMAL(10,0),
    v_arr_f_pago             DATE,
    v_arr_nrp                CHAR(11), 
    v_arr_monto_apo_ava      DECIMAL(12,2),
    v_arr_monto_amo_ava      DECIMAL(12,2),
    v_arr_folio_pag          DECIMAL(9,0),
    v_arr_folio_sua          DECIMAL(6,0),
    v_arr_aivs               DECIMAL(18,6),
    v_arr_monto_aportacion   DECIMAL(12,2),
    v_arr_monto_amortizacion DECIMAL(12,2), 
    v_arr_id_derecho         DECIMAL(9,0)
  END RECORD

  DEFINE 
    v_nombre_completo        VARCHAR(50), --Nombre del derechohabiente
    v_periodo_cvt1           VARCHAR(6),
    v_periodo_cvt2           VARCHAR(6),
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE 
    f_folio                  DECIMAL(9,0),--Folio del avance de pagos
    f_nss                    CHAR(11),    --NSS trabajador
    r_registros              DECIMAL(9,0),--Bandera de consulta avance de pago
    r_sum_aivs               DECIMAL(18,6),
    r_sum_aportacion         DECIMAL(12,2),
    r_sum_amortizacion       DECIMAL(12,2), 
    r_sum_apo_ava            DECIMAL(12,2),
    r_sum_amo_ava            DECIMAL(12,2) 

  DEFINE 
    f_ventana                ui.Window,   --Define las propìedades de la Ventana
    f_forma                  ui.Form ,    --Define las propiedades de la forma
    v_ruta_listados          CHAR(40),
    v_cuenta_derechohabiente SMALLINT,
    v_cuenta_folio           INTEGER,
    bnd_consulta             SMALLINT,
    r_bnd_periodo            SMALLINT,
    v_qwery_ibx              STRING 

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 902
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
   
  OPEN WINDOW vtn_ava_sin_int WITH FORM "DISC171"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      --INPUT BY NAME f_folio, f_nss
      INPUT f_folio, f_nss
      FROM  f_e_folio, f_e_nss
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
              NEXT FIELD f_e_folio   
           END IF  

           IF f_folio IS NOT NULL THEN
              --Valida que exista el folio
                 SELECT COUNT(*) 
                 INTO   v_cuenta_folio
                 FROM   dis_ava_pago_previo a
                 WHERE  a.folio = f_folio
                 IF v_cuenta_folio  = 0    OR 
                    v_cuenta_folio IS NULL THEN 
                    CALL fn_mensaje("Atención","No existen registros para el folio capturado",
                                    "about")
                    NEXT FIELD f_e_folio
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
                    NEXT FIELD f_e_nss
                 END IF 
              ELSE 
                 CALL fn_mensaje("Atención","El NSS debe ser de 11 caracteres",
                                 "about")
                 NEXT FIELD f_e_nss
              END IF 
           END IF  

           DISPLAY "f_nss -- ",f_nss
           DISPLAY "f_folio -- ",f_folio
            
           CALL fn_consulta_ava_sin_int(f_folio,
                                        f_nss)
           RETURNING r_registros,
                     r_sum_apo_ava,
                     r_sum_amo_ava,
                     r_sum_aivs,
                     r_sum_aportacion,
                     r_sum_amortizacion

           DISPLAY "r_registros -- ",r_registros

           IF r_registros > 0 THEN
              CALL f_forma.setElementHidden("gr_detalle", FALSE ) --muestra detalle de la consulta
               
              DISPLAY ARRAY v_arr_ava_sin_int TO rcd_detalle.* 
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE ROW
                  CALL fn_obtiene_nombre_derechohabiente(v_arr_ava_sin_int[ARR_CURR()].v_arr_id_derecho) 
               
                BEFORE DISPLAY 
                  DISPLAY r_registros        TO f_total_registros
                  DISPLAY r_sum_apo_ava      TO f_sum_apo_ava
                  DISPLAY r_sum_amo_ava      TO f_sum_amo_ava
                  DISPLAY r_sum_aivs         TO f_suma_aivs
                  DISPLAY r_sum_aportacion   TO f_suma_aportaciones
                  DISPLAY r_sum_amortizacion TO f_suma_amortizaciones

                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                ON ACTION reporte
                   CALL fn_reporte_ava_sin_int(f_folio,
                                               f_nss)
            
                --Genera archivo
                ON ACTION archivo
                   CALL fn_genera_archivo_ava_sin_int(f_folio,
                                                      f_nss)                             
              END DISPLAY                    
           ELSE
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros",
                              "about")
              CALL ui.interface.refresh()
           END IF

        --Se invoca la generación de archivo masivo
        ON ACTION btn_arcmas
           CALL fn_consulta_ava_sin_int(f_folio,
                                        f_nss)
           RETURNING r_registros,
                     r_sum_apo_ava,
                     r_sum_amo_ava,
                     r_sum_aivs,
                     r_sum_aportacion,
                     r_sum_amortizacion
                          
           IF r_registros >0 THEN          
              CALL fn_genera_archivo_masivo()
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
  CLOSE WINDOW vtn_ava_sin_int 

END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consulta_ava_sin_int(f_folio, f_nss)
  DEFINE 
    f_folio                  DECIMAL(9,0),--Folio de avance de pagos
    f_nss                    CHAR(11),    --NSS trabajador
    v_derechohabiente        DECIMAL(9,0),--Id
    v_indice                 INTEGER 

  SELECT af.nss nss,
         rtrim(af.nombre_af) ||' '||
         rtrim(af.ap_paterno_af) ||' '||
         rtrim(af.ap_materno_af) nombre_completo,
         di.id_dis_det_avance_pago,
         di.folio,
         di.periodo_pago,
         di.num_credito,
         di.f_pago,
         di.nrp,
         di.monto_aportacion,
         di.monto_amortizacion,
         gf.folio_pago,
         gf.id_referencia_pago,
         af.id_derechohabiente
  FROM   dis_ava_pago_previo di,
         afi_derechohabiente af,
         dis_ref_pago_previo gf
  WHERE  di.id_derechohabiente     = af.id_derechohabiente
  AND    di.id_dis_det_avance_pago = gf.id_dis_det_avance_pago
  INTO TEMP tmp_dis_ava_sin_int

  UPDATE STATISTICS FOR TABLE tmp_dis_ava_sin_int

  --Si se captura el nss, obtener id_derechohabiente de afi_derechoabiente
  IF LENGTH(f_nss) > 0 THEN
     SELECT id_derechohabiente 
     INTO   v_derechohabiente
     FROM   afi_derechohabiente
     WHERE  nss = f_nss
  END IF

  LET g_sql_txt = "\n SELECT it.nss, ",
                  "\n        it.nombre_completo, ",
                  "\n        it.folio, ",
                  "\n        it.periodo_pago, ",
                  "\n        it.num_credito, ",
                  "\n        it.f_pago, ",
                  "\n        it.nrp, ",
                  "\n        it.monto_aportacion, ",
                  "\n        it.monto_amortizacion, ",
                  "\n        it.folio_pago, ",
                  "\n        pg.folio_sua, ",
                  "\n        pg.aiv_ap_pat, ",
                  "\n        pg.imp_ap_pat, ",
                  "\n        pg.imp_am_cre, ",
                  "\n        pg.id_derechohabiente ",
                  "\n FROM   tmp_dis_ava_sin_int it, ",
                  "\n        cta_his_pagos pg ",
                  "\n WHERE  it.folio_pago         = pg.folio ",
                  "\n AND    it.id_referencia_pago = pg.id_referencia "

  --Valida si se capturo el folio avance de pagos
  IF f_folio IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt||"\n AND it.folio = ", f_folio
  END IF

  --Valida si se capturo el nss (id_derechohabiente)
  IF v_derechohabiente IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt||"\n AND it.id_derechohabiente = ",v_derechohabiente
  END IF

  LET g_sql_txt = g_sql_txt||"\n ORDER BY 1,3"

  DISPLAY "g_sql_txt -- ",g_sql_txt
  PREPARE prp_sql_ava_sin_int FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_sum_apo_ava      = 0.00
  LET v_sum_amo_ava      = 0.00
  LET v_sum_aivs         = 0.00
  LET v_sum_aportacion   = 0.00
  LET v_sum_amortizacion = 0.00

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_ava_sin_int CURSOR FOR prp_sql_ava_sin_int
  FOREACH cur_ava_sin_int INTO v_arr_ava_sin_int[v_indice].v_arr_nss,
                               v_arr_ava_sin_int[v_indice].v_arr_nombre_completo,
                               v_arr_ava_sin_int[v_indice].v_arr_folio_ava,
                               v_arr_ava_sin_int[v_indice].v_arr_periodo_pago,
                               v_arr_ava_sin_int[v_indice].v_arr_num_credito,
                               v_arr_ava_sin_int[v_indice].v_arr_f_pago,
                               v_arr_ava_sin_int[v_indice].v_arr_nrp,
                               v_arr_ava_sin_int[v_indice].v_arr_monto_apo_ava,
                               v_arr_ava_sin_int[v_indice].v_arr_monto_amo_ava,
                               v_arr_ava_sin_int[v_indice].v_arr_folio_pag,
                               v_arr_ava_sin_int[v_indice].v_arr_folio_sua,
                               v_arr_ava_sin_int[v_indice].v_arr_aivs,
                               v_arr_ava_sin_int[v_indice].v_arr_monto_aportacion,
                               v_arr_ava_sin_int[v_indice].v_arr_monto_amortizacion,
                               v_arr_ava_sin_int[v_indice].v_arr_id_derecho
                                   
    LET v_tot_registros    = v_tot_registros    + 1
    LET v_sum_apo_ava      = v_sum_apo_ava      + v_arr_ava_sin_int[v_indice].v_arr_monto_apo_ava
    LET v_sum_amo_ava      = v_sum_amo_ava      + v_arr_ava_sin_int[v_indice].v_arr_monto_amo_ava
    LET v_sum_aivs         = v_sum_aivs         + v_arr_ava_sin_int[v_indice].v_arr_aivs
    LET v_sum_aportacion   = v_sum_aportacion   + v_arr_ava_sin_int[v_indice].v_arr_monto_aportacion
    LET v_sum_amortizacion = v_sum_amortizacion + v_arr_ava_sin_int[v_indice].v_arr_monto_amortizacion
    LET v_indice           = v_indice           + 1
  END FOREACH

  CALL v_arr_ava_sin_int.deleteElement(v_indice)
  LET v_tot_registros    = v_tot_registros - 1

  RETURN v_tot_registros,
         v_sum_apo_ava,
         v_sum_amo_ava,
         v_sum_aivs,
         v_sum_aportacion,
         v_sum_amortizacion

END FUNCTION

#Objetivo: Genera reporte de avances sin integrar
FUNCTION fn_reporte_ava_sin_int(v_rfolio, v_rnss)
  DEFINE 
    v_rfolio                 DECIMAL(9,0), --Folio de avance de pagos
    v_rnss                   CHAR(11),     --NSS trabajador
    manejador_rpt            om.SaxDocumentHandler, --Contenedor documentos reporte
    v_rep_indice             INTEGER
  
  LET v_rep_indice = 1
  
  --Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC171.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte de avances sin integrar
  START REPORT rp_inco_avance TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO v_arr_ava_sin_int.getLength()
        OUTPUT TO REPORT rp_inco_avance(v_arr_ava_sin_int[v_rep_indice].*,
                                        v_tot_registros,
                                        v_sum_apo_ava,
                                        v_sum_amo_ava,
                                        v_sum_aivs,
                                        v_sum_aportacion,
                                        v_sum_amortizacion,
                                        g_usuario,
                                        v_rfolio)
    END FOR 
  FINISH REPORT rp_inco_avance
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
FUNCTION fn_genera_archivo_ava_sin_int(v_folio, v_nss)
  DEFINE 
    v_nom_archivo            VARCHAR(40), --Nombre del archivo de salida
    v_ruta_envio_dis         CHAR(40),
    v_ruta_nomarch           VARCHAR(100),--Ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_recorre_arreglo        INTEGER,
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_folio                  DECIMAL(9,0),--Folio avance de pagos
    v_nss                    CHAR(11)     --NSS trabajador

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
  LET v_nom_archivo   = "/avances_sin_integrar", v_hora

  --Se obtiene la ruta envio del módulo
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

  --Si se solicitó el folio de avance de pagos se incluye en el encabezado
  IF  v_folio IS NOT NULL THEN
      LET v_encabezado = " FOLIO DE CONSULTA ",v_folio
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF
    
  --Si se solicitó número de seguridad social se incluye en el encabezado
  IF  v_nss IS NOT NULL THEN
      LET v_encabezado = " NSS DE CONSULTA ",v_nss
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  LET v_encabezado = " NSS |NOMBRE COMPLETO |FOLIO AVANCE DE PAGO |PERIODO PAGO |NÚMERO DE CRÉDITO |FECHA PAGO |NRP |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |FOLIO PAGO |FOLIO SUA |AIVS |MONTO APORTACIÓN |MONTO AMORTIZACIÓN|"
  CALL v_ch_arch_salida.write([v_encabezado])
   
  FOR v_recorre_arreglo = 1 TO v_arr_ava_sin_int.getLength()
      LET v_detalle = v_arr_ava_sin_int[v_recorre_arreglo].v_arr_nss, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_nombre_completo, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_folio_ava, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_periodo_pago, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_num_credito USING "&&&&&&&&&&", "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_f_pago USING "dd-mm-yyyy", "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_nrp, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_monto_apo_ava, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_monto_amo_ava, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_folio_pag, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_folio_sua, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_aivs, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_monto_aportacion, "|",
                      v_arr_ava_sin_int[v_recorre_arreglo].v_arr_monto_amortizacion, "|"
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
   
  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | | | ",
                  v_sum_apo_ava, "|",
                  v_sum_amo_ava, "| | |",
                  v_sum_aivs, "|",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Avances de Pagos sin Integrar \n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Genera archivo masivo, sin parámetros de consulta
FUNCTION fn_genera_archivo_masivo()
  DEFINE         
    v_nom_archivo            VARCHAR(40), --Nombre del archivo de salida
    v_ruta_envio_dis         VARCHAR(40),
    v_ruta_nomarch           VARCHAR(100),--Ruta y nombre del archivo de salida
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
  LET v_nom_archivo   = "/cons_mas_ava_sin_int", v_hora

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
  LET v_encabezado = " NSS |NOMBRE COMPLETO |FOLIO AVANCE DE PAGO |PERIODO PAGO |NÚMERO DE CRÉDITO |FECHA PAGO |NRP |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |FOLIO PAGO |FOLIO SUA |AIVS |MONTO APORTACIÓN |MONTO AMORTIZACIÓN|"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice = 1
 
  FOR v_indice = 1 TO v_arr_ava_sin_int.getLength()
      --Concatenación del detalle
      LET v_detalle = v_arr_ava_sin_int[v_indice].v_arr_nss, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_nombre_completo, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_folio_ava, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_periodo_pago, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_num_credito USING "&&&&&&&&&&", "|",
                      v_arr_ava_sin_int[v_indice].v_arr_f_pago USING "dd-mm-yyyy", "|",
                      v_arr_ava_sin_int[v_indice].v_arr_nrp, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_monto_apo_ava, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_monto_amo_ava, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_folio_pag, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_folio_sua, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_aivs, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_monto_aportacion, "|",
                      v_arr_ava_sin_int[v_indice].v_arr_monto_amortizacion, "|"
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR

  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | | | ",
                  v_sum_apo_ava, "|",
                  v_sum_amo_ava, "| | |",
                  v_sum_aivs, "|",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Avances de Pago sin Integrar\n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Estructura reporte de Avances sin Integrar
REPORT rp_inco_avance(v_rep_ava_sin_int, 
                      v_rep_tot_registros, 
                      v_rep_sum_apo_ava,
                      v_rep_sum_amo_ava,
                      v_rep_sum_aivs, 
                      v_rep_sum_aportacion,
                      v_rep_sum_amortizacion, 
                      v_usuario,
                      v_rep_folio)

  DEFINE v_rep_ava_sin_int   RECORD
    v_rep_nss                CHAR(11),
    v_rep_nombre_completo    VARCHAR(30),
    v_rep_folio_ava          DECIMAL(9,0),
    v_rep_periodo_pago       CHAR(6),
    v_rep_num_credito        DECIMAL(10,0),
    v_rep_f_pago             DATE,
    v_rep_nrp                CHAR(11),
    v_rep_monto_apo_ava      DECIMAL(12,2),
    v_rep_monto_amo_ava      DECIMAL(12,2),
    v_rep_folio_pag          DECIMAL(9,0),
    v_rep_folio_sua          DECIMAL(6,0),
    v_rep_aivs               DECIMAL(18,6),
    v_rep_monto_aportaciones DECIMAL(12,2),
    v_rep_monto_amortizaciones DECIMAL(12,2),
    v_rep_id                 DECIMAL(9,0)
  END RECORD

  DEFINE 
    v_fecha_consulta         DATE,        --Fecha de proceso
    v_usuario                VARCHAR(30), --Almacena al usuario
    v_rep_tot_registros      DECIMAL(9,0),--Total de registros
    v_rep_sum_apo_ava        DECIMAL(12,2),
    v_rep_sum_amo_ava        DECIMAL(12,2),
    v_rep_sum_aivs           DECIMAL(18,6),
    v_rep_sum_aportacion     DECIMAL(12,2),
    v_rep_sum_amortizacion   DECIMAL(12,2),
    v_rep_folio              DECIMAL(9,0)

  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

    ON EVERY ROW
      PRINTX v_rep_ava_sin_int.v_rep_nss
      PRINTX v_rep_ava_sin_int.v_rep_nombre_completo
      PRINTX v_rep_ava_sin_int.v_rep_folio_ava
      PRINTX v_rep_ava_sin_int.v_rep_periodo_pago
      PRINTX v_rep_ava_sin_int.v_rep_num_credito
      PRINTX v_rep_ava_sin_int.v_rep_f_pago USING "dd-mm-yyyy"
      PRINTX v_rep_ava_sin_int.v_rep_nrp
      PRINTX v_rep_ava_sin_int.v_rep_monto_apo_ava
      PRINTX v_rep_ava_sin_int.v_rep_monto_amo_ava
      PRINTX v_rep_ava_sin_int.v_rep_folio_pag
      PRINTX v_rep_ava_sin_int.v_rep_folio_sua
      PRINTX v_rep_ava_sin_int.v_rep_aivs
      PRINTX v_rep_ava_sin_int.v_rep_monto_aportaciones
      PRINTX v_rep_ava_sin_int.v_rep_monto_amortizaciones

    ON LAST ROW
      PRINTX v_rep_tot_registros
      PRINTX v_rep_sum_apo_ava
      PRINTX v_rep_sum_amo_ava
      PRINTX v_rep_sum_aivs
      PRINTX v_rep_sum_aportacion
      PRINTX v_rep_sum_amortizacion

END REPORT