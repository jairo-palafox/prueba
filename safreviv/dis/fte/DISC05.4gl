################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 27/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC05                                                   #
#Objetivo          => Programa para consultar o identificar los avances        #
#                     abiertos para su análisis.                               #
#Fecha inicio      => 18/01/2012                                               #
################################################################################
#Registro de modificaciones:                                                   #
#Autor           Fecha         Descrip. cambio                                 #
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la        #
#                              Preliquidación de Dispersión de Pagos           #
#                              ejecutándose                                    #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo del proceso
    p_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tot_registros          DECIMAL(9,0), -- Total de registros
    v_sum_aportacion         LIKE dis_det_avance_pago.monto_aportacion,
    v_sum_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion

  DEFINE 
    v_arr_det_avance_pago    DYNAMIC ARRAY OF RECORD
    v_arr_nss                LIKE afi_derechohabiente.nss,
    v_arr_nombre_completo    VARCHAR(50),
    v_arr_num_credito        LIKE dis_det_avance_pago.num_credito,
    v_arr_nrp                LIKE dis_det_avance_pago.nrp,
    v_arr_f_pago             DATE,
    v_arr_perido_pago        LIKE dis_det_avance_pago.periodo_pago,
    v_arr_monto_aportaciones LIKE dis_det_avance_pago.monto_aportacion,
    v_arr_monto_amortizaciones LIKE dis_det_avance_pago.monto_amortizacion,
    v_arr_id_derecho         LIKE dis_det_avance_pago.id_derechohabiente
  END RECORD

  DEFINE 
    v_nombre_completo        VARCHAR(50), --Nombre del derechohabiente
    v_periodo_cvt1           VARCHAR(6),
    v_periodo_cvt2           VARCHAR(6),
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE 
    f_periodo1               INTEGER, --Periodo pago inicio 
    f_periodo2               INTEGER, --Periodo pago final 
    f_monto1                 LIKE dis_det_avance_pago.monto_aportacion, --Rango1
    f_monto2                 LIKE dis_det_avance_pago.monto_aportacion, --Rango2
    f_nss                    LIKE afi_derechohabiente.nss, --NSS trabajador
    r_registros              DECIMAL(9,0),  --Bandera de consulta avance de pago
    r_sum_aportacion         LIKE dis_det_avance_pago.monto_aportacion,
    r_sum_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion
  
  DEFINE 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form ,  --Define las propiedades de la forma
    v_ruta_listados          LIKE seg_modulo.ruta_listados,
    v_cuenta_derechohabiente SMALLINT,
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

  LET f_periodo1   = ""
  LET f_periodo2   = ""
  LET bnd_consulta = 0

  CLOSE WINDOW SCREEN
   
  OPEN WINDOW vtn_quebranto WITH FORM "DISC051"
    DIALOG   ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_periodo1,f_periodo2,f_monto1,f_monto2,f_nss
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_nombre", TRUE ) --Oculta el nombre del trabajador
          CALL f_forma.setElementHidden("gr_detalle", TRUE ) --Oculta detalle de la consulta
          CALL ui.interface.refresh()
       
        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro
           IF (f_periodo1 IS NULL) AND 
              (f_periodo2 IS NULL) AND 
              (f_monto1 IS NULL)   AND 
              (f_monto2 IS NULL)   AND 
              (f_nss IS NULL)      THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar un criterio para la búsqueda",
                              "about")
              NEXT FIELD f_periodo1
           END IF  
               
           IF (f_periodo1 IS NULL AND f_periodo2 IS NOT NULL ) OR 
              (f_periodo1 IS NOT NULL AND f_periodo2 IS NULL ) THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar ambos periodos para la búsqueda",
                              "about")
              NEXT FIELD f_periodo1
           END IF 
            
           IF f_periodo1 IS NOT NULL AND f_periodo2 IS NOT NULL  THEN
              ###########3 valida primer periodo ###########
              IF (f_periodo1 IS NOT NULL) THEN
                 LET v_periodo_cvt1 = f_periodo1

                 IF LENGTH(v_periodo_cvt1) <> 6 THEN  
                    CALL fn_mensaje ("ATENCION", 
                                     "El periodo de pago inicial debe ser de 6 caracteres", 
                                     "stop")
                    NEXT FIELD f_periodo1
                 END IF 
                     
                 --DISPLAY "f_periodo antes -- ",v_periodo_cvt1
                 PREPARE prp_verifica_periodo FROM "EXECUTE FUNCTION fn_valida_formato_periodo_pago(?)"
                 EXECUTE prp_verifica_periodo USING f_periodo1 INTO f_periodo1, r_bnd_periodo

                 LET v_periodo_cvt1 = f_periodo1
                 DISPLAY "f_periodo después -- ",v_periodo_cvt1

                 --La función "fn_valida_formato_periodo_pago" regresa 2 parámetros, uno el periodo y dos el estatus
                 --Si el estatus es 0, no hay error y el periodo es válido
                 --Si el estatus es 1, entonces el año del periodo es inválido, es decir, es mayor al año actual
                 --Si el estatus es 2, entonces el mes es incorrecto
                 IF r_bnd_periodo = 0 THEN 
                    DISPLAY "Correcto periodo 1"
                 ELSE 
                    IF r_bnd_periodo = 1 THEN 
                       CALL fn_mensaje ("ATENCION", 
                                        "Verifique el periodo de pago inicial. El año es incorrecto", 
                                        "stop")
                    END IF
                    IF r_bnd_periodo = 2 THEN 
                       CALL fn_mensaje ("ATENCION", 
                                        "Verifique el periodo de pago inicial. El mes es incorrecto", 
                                        "stop")
                    END IF  
                    NEXT FIELD f_periodo1
                 END IF 
              END IF 

              ###########3 valida segundo periodo ###########
              IF (f_periodo2 IS NOT NULL) THEN
                 LET v_periodo_cvt2 = f_periodo2

                 IF LENGTH(v_periodo_cvt2) <> 6 THEN  
                    CALL fn_mensaje ("ATENCION", 
                                     "El periodo de pago final debe ser de 6 caracteres", 
                                     "stop")
                    NEXT FIELD f_periodo2
                 END IF 
                     
                 PREPARE prp_verifica_periodo_final FROM "EXECUTE FUNCTION fn_valida_formato_periodo_pago(?)"
                 EXECUTE prp_verifica_periodo_final USING f_periodo2 INTO f_periodo2, r_bnd_periodo

                 LET v_periodo_cvt2 = f_periodo2
                 DISPLAY "f_periodo después -- ",v_periodo_cvt2

                 --La función "fn_valida_formato_periodo_pago" regresa 2 parámetros, uno el periodo y dos el estatus
                 --Si el estatus es 0, no hay error y el periodo es válido
                 --Si el estatus es 1, entonces el año del periodo es inválido, es decir, es mayor al año actual
                 --Si el estatus es 2, entonces el mes es incorrecto
                 IF r_bnd_periodo = 0 THEN 
                    DISPLAY "Correcto periodo 2"
                 ELSE 
                    IF r_bnd_periodo = 1 THEN 
                       CALL fn_mensaje ("ATENCION", 
                                        "Verifique el periodo de pago final. El año es incorrecto", 
                                        "stop")
                    END IF
                    IF r_bnd_periodo = 2 THEN 
                       CALL fn_mensaje ("ATENCION", 
                                        "Verifique el periodo de pago final. El mes es incorrecto", 
                                        "stop")
                    END IF  
                    NEXT FIELD f_periodo2
                 END IF 
              END IF 

              IF v_periodo_cvt1 > v_periodo_cvt2 THEN 
                 CALL fn_mensaje ("ATENCION", 
                                  "El periodo inicial no puede ser mayor al periodo final", 
                                  "stop")
                 NEXT FIELD f_periodo2
              END IF 
           END IF 
            
           IF (f_monto1 IS NULL AND f_monto2 IS NOT NULL ) OR 
              (f_monto1 IS NOT NULL AND f_monto2 IS NULL ) THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar ambos montos para la búsqueda",
                              "about")
              NEXT FIELD f_monto1
           END IF 

           IF (f_monto1 IS NOT NULL) AND 
              (f_monto2 IS NOT NULL) THEN
              IF f_monto1 > f_monto2 THEN 
                 CALL fn_mensaje("ATENCIÓN",
                                 "El monto inicial no debe ser mayor al monto final.",
                                 "about")
                 NEXT FIELD f_monto1
              END IF 
           END IF   
            
           IF f_nss IS NOT NULL THEN 
              IF LENGTH(f_nss) = 11 THEN 
                 --Valida que exista el nss
                 SELECT COUNT (id_derechohabiente) 
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

           DISPLAY "f_monto1 -- ",f_monto1
           DISPLAY "f_monto2 -- ",f_monto2
           DISPLAY "f_periodo1 -- ",f_periodo1
           DISPLAY "f_periodo2 -- ",f_periodo2
           DISPLAY "f_nss -- ",f_nss
            
           CALL fn_consulta_avance_pago(f_periodo1,
                                        f_periodo2,
                                        f_monto1,
                                        f_monto2,
                                        f_nss)
           RETURNING r_registros,
                     r_sum_aportacion,
                     r_sum_amortizacion

           DISPLAY "r_registros -- ",r_registros

           IF r_registros > 0 THEN
              CALL f_forma.setElementHidden("gr_detalle", FALSE ) --muestra detalle de la consulta
               
              DISPLAY ARRAY v_arr_det_avance_pago TO rcd_detalle.* 
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE ROW
                  CALL fn_obtiene_nombre_derechohabiente(v_arr_det_avance_pago[ARR_CURR()].v_arr_id_derecho) 
               
                BEFORE DISPLAY 
                  DISPLAY r_registros        TO f_total_registros
                  DISPLAY r_sum_aportacion   TO f_suma_aportaciones
                  DISPLAY r_sum_amortizacion TO f_suma_amortizaciones

                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                ON ACTION reporte
                   CALL fn_reporte_quebranto(f_periodo1,
                                             f_periodo2,
                                             f_monto1,
                                             f_monto2,
                                             f_nss)
                --Genera archivo
                ON ACTION archivo
                   CALL fn_genera_archivo_con_avances(f_periodo1,
                                                      f_periodo2,
                                                      f_monto1,
                                                      f_monto2,
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
           CALL fn_consulta_avance_pago(f_periodo1,
                                        f_periodo2,
                                        f_monto1,
                                        f_monto2,
                                        f_nss)
           RETURNING r_registros,
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
  CLOSE WINDOW vtn_quebranto 

END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consulta_avance_pago(f_periodo1,f_periodo2,f_monto1,f_monto2,f_nss)
  DEFINE 
    f_periodo1               CHAR(6), --Periodo pago inicio 
    f_periodo2               CHAR(6), --Periodo pago final
    f_monto1                 DECIMAL(22,2),
    f_monto2                 DECIMAL(22,2),
    f_nss                    LIKE afi_derechohabiente.nss, -- NSS trabajador
    v_derechohabiente        LIKE afi_derechohabiente.id_derechohabiente, -- Id
    v_indice                 INTEGER 

  -- Si se captura el nns, obtener id_derechohabiente de afi_derechoabiente
  IF LENGTH(f_nss) > 0 THEN
     SELECT id_derechohabiente 
     INTO   v_derechohabiente
     FROM   afi_derechohabiente
     WHERE  nss = f_nss
  END IF
      
  -- Consulta si existen registros con los parametros capturados
  LET g_sql_txt = "\n SELECT af.nss, rtrim(af.nombre_af) ||' '|| rtrim(af.ap_paterno_af) ||' '|| rtrim(af.ap_materno_af),",
                  "\n        da.num_credito, da.nrp, da.f_pago,",
                  "\n        da.periodo_pago, da.monto_aportacion,",
                  "\n        da.monto_amortizacion, af.id_derechohabiente",
                  "\n FROM   dis_det_avance_pago da, afi_derechohabiente af",
                  "\n WHERE  da.id_derechohabiente = af.id_derechohabiente",
                  "\n AND    da.estado             = 30"

  -- Valida si se capturo periodo de fechas
  IF LENGTH(f_periodo1) > 0 AND 
     LENGTH(f_periodo2) > 0 THEN
     LET g_sql_txt = g_sql_txt||"\n AND periodo_pago between '",f_periodo1,"'
                                    AND '",f_periodo2,"'"
  END IF
       
  -- Valida si se capturo rango de montos
  IF LENGTH(f_monto1) > 0 AND 
     LENGTH(f_monto2) > 0 THEN
     LET g_sql_txt = g_sql_txt||"\n AND (da.monto_aportacion between ",f_monto1,"\n AND ",f_monto2,
                                "\n OR  da.monto_amortizacion between ",f_monto1,"\n AND ",f_monto2, "\n )"  
  END IF

  -- Valida si se capturo el nss (id_derechohabiente)
  IF v_derechohabiente IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt||"\n AND da.id_derechohabiente = ",v_derechohabiente
  END IF

  DISPLAY "g_sql_txt -- ",g_sql_txt
  PREPARE prp_sql_avance_pago_existe FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_sum_aportacion   = 0.00
  LET v_sum_amortizacion = 0.00

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_det_avance_pago CURSOR FOR prp_sql_avance_pago_existe
  FOREACH cur_det_avance_pago INTO v_arr_det_avance_pago[v_indice].v_arr_nss,
                                   v_arr_det_avance_pago[v_indice].v_arr_nombre_completo,
                                   v_arr_det_avance_pago[v_indice].v_arr_num_credito,
                                   v_arr_det_avance_pago[v_indice].v_arr_nrp,
                                   v_arr_det_avance_pago[v_indice].v_arr_f_pago,
                                   v_arr_det_avance_pago[v_indice].v_arr_perido_pago,
                                   v_arr_det_avance_pago[v_indice].v_arr_monto_aportaciones,
                                   v_arr_det_avance_pago[v_indice].v_arr_monto_amortizaciones,
                                   v_arr_det_avance_pago[v_indice].v_arr_id_derecho
                                   
    LET v_tot_registros    = v_tot_registros    + 1
    LET v_sum_aportacion   = v_sum_aportacion   + 
        v_arr_det_avance_pago[v_indice].v_arr_monto_aportaciones
    LET v_sum_amortizacion = v_sum_amortizacion + 
        v_arr_det_avance_pago[v_indice].v_arr_monto_amortizaciones
    LET v_indice           = v_indice           + 1
  END FOREACH

  CALL v_arr_det_avance_pago.deleteElement(v_indice)
  LET v_tot_registros    = v_tot_registros - 1

  RETURN v_tot_registros,
         v_sum_aportacion,
         v_sum_amortizacion

END FUNCTION

#Objetivo: Genera reporte de quebrantos de avance de pago
FUNCTION fn_reporte_quebranto(v_rperiodo1,v_rperiodo2,v_rmonto1,v_rmonto2,v_rnss)
  DEFINE 
    v_rperiodo1              CHAR(6), --Periodo de Pago Inicial
    v_rperiodo2              CHAR(6), --Periodo de Pago Final
    v_rmonto1                LIKE dis_det_avance_pago.monto_aportacion, --Rango 1
    v_rmonto2                LIKE dis_det_avance_pago.monto_aportacion, --Rango 2
    v_rnss                   LIKE afi_derechohabiente.nss, --NSS trabajador
    manejador_rpt            om.SaxDocumentHandler, --Contenedor documentos reporte
    v_rep_indice             INTEGER
  
  LET v_rep_indice = 1
  
  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC052.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte de registros con rechazo
  START REPORT rp_qbto_avance TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO v_arr_det_avance_pago.getLength()
        OUTPUT TO REPORT rp_qbto_avance(v_arr_det_avance_pago[v_rep_indice].*,
                                        v_tot_registros,
                                        v_sum_aportacion,
                                        v_sum_amortizacion,
                                        g_usuario,
                                        v_rperiodo1,
                                        v_rperiodo2,
                                        v_rmonto1,
                                        v_rmonto2)
    END FOR 
  FINISH REPORT rp_qbto_avance
END FUNCTION

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE   
    v_id_consulta            LIKE afi_derechohabiente.id_derechohabiente 

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#Objetivo: genera el archivo con la consulta obtenida
FUNCTION fn_genera_archivo_con_avances(v_periodo_inicio,v_periodo_fin,v_monto_inicio,
                                       v_monto_fin,v_nss)
  DEFINE 
    v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_recorre_arreglo        INTEGER,
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_periodo_inicio         CHAR(6),
    v_periodo_fin            CHAR(6),
    v_monto_inicio           LIKE dis_det_avance_pago.monto_aportacion, --Rango 1
    v_monto_fin              LIKE dis_det_avance_pago.monto_aportacion, --Rango 2
    v_nss                    LIKE afi_derechohabiente.nss --NSS trabajador

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
  LET v_nom_archivo   = "/consulta_avan_abier", v_hora

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

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE ",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])
    
  --Si se solicitó periodo se incluye en el encabezado
  IF LENGTH(v_periodo_inicio) > 0 AND 
     LENGTH(v_periodo_fin)    > 0 THEN
     LET v_encabezado = " PERIODO DE CONSULTA ",v_periodo_inicio,"-",v_periodo_fin
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF
    
  --Si se solicitó montos se incluye en el encabezado
  IF LENGTH(v_monto_inicio) > 0 AND 
     LENGTH(v_monto_fin)    > 0 THEN
     LET v_encabezado = " MONTO DE CONSULTA ",v_monto_inicio,"-",v_monto_fin
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF           

  --Si se solicitó número de seguridad social se incluye en el encabezado
  IF  v_nss IS NOT NULL THEN
      LET v_encabezado = " NSS DE CONSULTA ",v_nss
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  --LET v_encabezado = " |NSS| |NOMBRE COMPLETO| |NÚMERO DE CRÉDITO| |NRP| |FECHA PAGO| |PERIODO PAGO| |MONTO APORTACION| |MONTO AMORTIZACIÓN|"
  LET v_encabezado = " NSS |NOMBRE COMPLETO |NÚMERO DE CRÉDITO |NRP |FECHA PAGO |PERIODO PAGO |MONTO APORTACION |MONTO AMORTIZACIÓN|"
  CALL v_ch_arch_salida.write([v_encabezado])
   
  FOR v_recorre_arreglo = 1 TO v_arr_det_avance_pago.getLength()
      LET v_detalle = v_arr_det_avance_pago[v_recorre_arreglo].v_arr_nss, "|",
                      v_arr_det_avance_pago[v_recorre_arreglo].v_arr_nombre_completo, "|",
                      v_arr_det_avance_pago[v_recorre_arreglo].v_arr_num_credito, "|",
                      v_arr_det_avance_pago[v_recorre_arreglo].v_arr_nrp, "|",
                      v_arr_det_avance_pago[v_recorre_arreglo].v_arr_f_pago, "|",
                      v_arr_det_avance_pago[v_recorre_arreglo].v_arr_perido_pago, "|",
                      v_arr_det_avance_pago[v_recorre_arreglo].v_arr_monto_aportaciones, "|",
                      v_arr_det_avance_pago[v_recorre_arreglo].v_arr_monto_amortizaciones, "|"
                      --v_arr_det_avance_pago[v_recorre_arreglo].v_arr_id_derecho, "|"
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
   
  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | |",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Avances Abiertos\n en la ruta"||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: genera archivo masivo, sin parámetros de consulta
FUNCTION fn_genera_archivo_masivo()
  DEFINE         
    v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
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
  LET v_nom_archivo   = "/cons_mas_avan_abier", v_hora

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

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE ",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])
  LET v_encabezado = " NSS| NOMBRE COMPLETO| NÚMERO DE CRÉDITO| NRP| FECHA PAGO| PERIODO PAGO| MONTO APORTACION| MONTO AMORTIZACIÓN|"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice = 1
 
  FOR v_indice = 1 TO v_arr_det_avance_pago.getLength()
      --Concatenación del detalle
      LET v_detalle = v_arr_det_avance_pago[v_indice].v_arr_nss, "|",
                      v_arr_det_avance_pago[v_indice].v_arr_nombre_completo, "|",
                      v_arr_det_avance_pago[v_indice].v_arr_num_credito, "|",
                      v_arr_det_avance_pago[v_indice].v_arr_nrp, "|",
                      v_arr_det_avance_pago[v_indice].v_arr_f_pago, "|",
                      v_arr_det_avance_pago[v_indice].v_arr_perido_pago, "|",
                      v_arr_det_avance_pago[v_indice].v_arr_monto_aportaciones, "|",
                      v_arr_det_avance_pago[v_indice].v_arr_monto_amortizaciones, "|"
                      --v_arr_det_avance_pago[v_indice].v_arr_id_derecho
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR

  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | |",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion

  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Avances Abiertos\n en la ruta"||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Estructura reporte de quebranto de avance de pago
REPORT rp_qbto_avance(v_rep_quebranto, v_rep_tot_registros, v_rep_sum_aportacion,
                      v_rep_sum_amortizacion, v_usuario, v_rperiodo1, v_rperiodo2,
                      v_rmonto1, v_rmonto2)
  DEFINE 
    v_rep_quebranto          RECORD
    v_rep_nss                LIKE afi_derechohabiente.nss,
    v_arr_nombre_completo    VARCHAR(50),
    v_rep_num_credito        LIKE dis_det_avance_pago.num_credito,
    v_rep_nrp                LIKE dis_det_avance_pago.nrp,
    v_rep_f_pago             LIKE dis_det_avance_pago.f_pago,
    v_rep_perido_pago        LIKE dis_det_avance_pago.periodo_pago,
    v_rep_monto_aportaciones LIKE dis_det_avance_pago.monto_aportacion,
    v_rep_monto_amortizaciones LIKE dis_det_avance_pago.monto_amortizacion,
    v_rep_id                 LIKE dis_det_avance_pago.id_derechohabiente
  END RECORD

  DEFINE 
    v_rperiodo1              LIKE dis_det_avance_pago.f_pago, -- Fecha inicio de pago1
    v_rperiodo2              LIKE dis_det_avance_pago.f_pago, -- Fecha fin de pago2
    v_rmonto1                LIKE dis_det_avance_pago.monto_aportacion, -- Rango 1
    v_rmonto2                LIKE dis_det_avance_pago.monto_aportacion, -- Rango 2
    v_fecha_consulta         LIKE dis_det_avance_pago.f_pago, -- Fecha de proceso
    v_usuario                VARCHAR(30), -- Almacena al usuario
    v_rep_tot_registros      DECIMAL(9,0), -- Total de registros
    v_rep_sum_aportacion     LIKE dis_det_avance_pago.monto_aportacion,
    v_rep_sum_amortizacion   LIKE dis_det_avance_pago.monto_amortizacion

  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta=TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_rperiodo1 USING "dd-mm-yyyy"
      PRINTX v_rperiodo2 USING "dd-mm-yyyy"
      PRINTX v_rmonto1
      PRINTX v_rmonto2

    ON EVERY ROW
      PRINTX v_rep_quebranto.v_rep_nss
      PRINTX v_rep_quebranto.v_rep_num_credito
      PRINTX v_rep_quebranto.v_rep_nrp
      PRINTX v_rep_quebranto.v_rep_f_pago USING "dd-mm-yyyy"
      PRINTX v_rep_quebranto.v_rep_perido_pago
      PRINTX v_rep_quebranto.v_rep_monto_aportaciones
      PRINTX v_rep_quebranto.v_rep_monto_amortizaciones
      PRINTX v_rep_quebranto.v_arr_nombre_completo

    ON LAST ROW
      PRINTX v_rep_tot_registros
      PRINTX v_rep_sum_aportacion
      PRINTX v_rep_sum_amortizacion

END REPORT