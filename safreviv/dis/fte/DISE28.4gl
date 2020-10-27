################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 29/05/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE28                                                   #
#Objetivo          => Programa lanzado para integrar el archivo de Cancelación #
#                     de Avance de Pagos por Aclaraciones Patronales           #
#Fecha inicio      => 29/05/2018                                               #
################################################################################
DATABASE safre_viv 
GLOBALS
  DEFINE 
    v_usuario                VARCHAR(30),                 --Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod,--Código de proceso
    l_pid                    LIKE glo_pid.pid,
    g_opera_cod              LIKE cat_operacion.opera_cod,--Código de operación
    g_folio                  LIKE dis_det_avance_pago.folio, --Folio generado
    v_ruta_ejecutable        STRING,                      --Ruta del ejecutable
    v_ruta_listados          STRING,                      --Ruta de los listados
    l_arch_proceso           VARCHAR(100)

  DEFINE p_cve_proceso_cnt   SMALLINT
  DEFINE p_transaccion       SMALLINT
  DEFINE r_bnd_proceso_cnt   SMALLINT
  DEFINE v_fecha_reg         DATE 

  DEFINE arr_cancelacion     DYNAMIC ARRAY OF RECORD 
         arr_nrp             LIKE dis_det_avance_pago.nrp,
         arr_periodo_pago    LIKE dis_det_avance_pago.periodo_pago,
         arr_cuenta_reg      DECIMAL (10,0)
  END RECORD
END GLOBALS

MAIN
  DEFINE 
    r_b_valida               SMALLINT,
    r_bnd_edo_act_archivo    SMALLINT,
    r_bnd_oera_error         SMALLINT, --Bandera actualiza operacion en error
    r_bnd_error_op           SMALLINT,
    l_comando                STRING,
    p_transaccion_cnt        SMALLINT,
    p_transaccion            SMALLINT, --Bandera que indica si la ejecución es manual o automática
    v_cuenta_contable        DECIMAL(10,0)

  LET v_usuario      = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)

  LET p_transaccion  = 0
  LET v_fecha_reg    = TODAY
  LET r_bnd_proceso_cnt = 0
    
  --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  SELECT ind_tipo_ejecucion 
  INTO   p_transaccion
  FROM   bat_ctr_operacion 
  WHERE  proceso_cod = g_proceso_cod   
  AND    pid         = l_pid
  AND    opera_cod   = g_opera_cod
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usuario)
     RETURNING g_folio
  END IF 

  --Ejecuta StoreProcedure para llevar a cabo la cancelación
  WHENEVER ERROR CONTINUE
    PREPARE prp_carga_cancelacion
    FROM "EXECUTE PROCEDURE safre_viv:sp_dis_avances_pago_can(?,?)"
    EXECUTE prp_carga_cancelacion USING g_folio,v_usuario
  WHENEVER ERROR STOP 
      
  IF SQLCA.sqlcode < 0 THEN
     DISPLAY "Código de ERROR SQL ",SQLCA.sqlcode
     --Función para finalizar la operación en error
     CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
     RETURNING r_bnd_oera_error

     CALL fn_desplega_inc_operacion(r_bnd_oera_error)
     EXIT PROGRAM
  END IF

  LET p_cve_proceso_cnt = 111  --Cancelación Ava Pag Acl Pat
  LET p_transaccion_cnt = 140  --SE DEBERÁ REGISTRAR EL IMPORTE DEL AVANCE

  --DISPLAY "g_folio -- ",g_folio
  --DISPLAY "v_fecha_reg -- ",v_fecha_reg
  --DISPLAY "p_cve_proceso_cnt -- ",p_cve_proceso_cnt
  --DISPLAY "g_proceso_cod -- ",g_proceso_cod
  --DISPLAY "p_transaccion_cnt -- ",p_transaccion_cnt
         
  WHENEVER ERROR CONTINUE 
    --Se agrega función para realizar el registro contable ##
    PREPARE prp_reg_contable
    FROM "EXECUTE PROCEDURE fn_avance_cnt18(?,?,?,?,?)"
    EXECUTE prp_reg_contable USING g_folio,
                                   v_fecha_reg,
                                   p_cve_proceso_cnt,
                                   g_proceso_cod,
                                   p_transaccion_cnt
    INTO r_bnd_proceso_cnt
  WHENEVER ERROR STOP 

  IF SQLCA.sqlcode < 0 THEN
     DISPLAY "Código de ERROR SQL de registro contable: ",SQLCA.sqlcode
     --Función para finalizar la operación en error
     CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
     RETURNING r_bnd_oera_error
     EXIT PROGRAM
  END IF

  IF r_bnd_proceso_cnt = 1 THEN
     SELECT COUNT (*)
     INTO v_cuenta_contable
     FROM cnt_transaccion
     WHERE folio_liquida = g_folio
     IF v_cuenta_contable > 0 THEN  
        DISPLAY "El registro contable de la cancelación de avances de pago por aclaraciones patronales se realizó exitosamente."
     ELSE 
        DISPLAY "Error: El registro contable no se realizó debidamente."
     END IF 
  ELSE 
     DISPLAY "Ocurrió un error al realizar el registro contable."
  END IF 

  --Actualiza el estado del archivo procesado
  CALL fn_act_edo_archivo(l_arch_proceso,g_folio,2,v_usuario)
  RETURNING r_bnd_edo_act_archivo
         
  --Función para finalizar la operación
  CALL fn_actualiza_opera_fin(l_pid,g_proceso_cod,g_opera_cod)
  RETURNING r_b_valida
         
  IF r_b_valida <> 0 THEN
     CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
     RETURNING r_bnd_error_op

     CALL fn_desplega_inc_operacion(r_bnd_oera_error)
  ELSE 
     CALL fn_genera_reporte_can()

     CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados

     DISPLAY "\n\n\n ############### GENERACIÓN INTERFACE CARGO HS ###############"
            
     --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
     LET l_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/DISS04.42r ",g_folio, " ",g_proceso_cod
     RUN l_comando

     {--Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
     LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS04.42r ",
                     g_folio,
                     " 1>",v_ruta_listados CLIPPED,"/nohup:",
                     l_pid USING "&&&&&",":",
                     g_proceso_cod USING "&&&&&",":",
                     g_opera_cod USING "&&&&&" ," 2>&1 &"
     RUN l_comando}

     DISPLAY "Finaliza operación"
  END IF --Operación

  -- Envío de correo de notificación de proceso finalizado
  CALL fn_correo_proceso(l_pid,
                         g_proceso_cod,
                         g_opera_cod,
                         'adjunto?',
                         'Integración de Cancelación de Avance de Pagos por Aclaraciones Patronales',
                         'ID Proceso   : '||l_pid||
                         'Proceso      : '||g_proceso_cod||
                         'Operacion    : '||g_opera_cod||
                         'Fecha Inicio : '||TODAY||
                         'Fecha Fin    : '||TODAY)
END MAIN

--Genera el reporte de los datos que fueron procesados en la Cancelación de Avances de Pago
FUNCTION fn_genera_reporte_can()
  DEFINE 
    v_ruta_reporte           STRING,             --Ruta del archivo del reporte
    v_origen_datos           STRING,
    v_query                  STRING,
    v_indice                 INTEGER,
    manejador_rpt            om.SaxDocumentHandler --Contenedor documentos reporte
         
  DEFINE
    v_total_registros        DECIMAL(10,0),
    v_total_rechazados       DECIMAL(10,0),
    v_total_aceptados        DECIMAL(10,0),
    v_total_rechazados_duplicados DECIMAL(10,0)

  DEFINE 
    v_total_aportacion_can   DECIMAL(22,2),
    v_total_amortizacion_can DECIMAL(22,2)
         
  CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados

  LET v_origen_datos = v_usuario
   
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(),"-",
                       "DISE28","-",
                       l_pid USING "&&&&&","-",
                       g_proceso_cod USING "&&&&&","-",
                       g_opera_cod USING "&&&&&",".pdf"

  DISPLAY "Ruta del reporte --- ",v_ruta_reporte

  SELECT COUNT(*)
  INTO   v_total_registros
  FROM   safre_tmp:tmp_dis_can_ava_pag0

  --DISPLAY "Total de registros: ",v_total_registros

  SELECT COUNT(*)
  INTO   v_total_rechazados
  FROM   dis_rch_avance_pago
  WHERE  folio = g_folio

  --Se agrega total de registros rechazados por duplicidad
  SELECT COUNT(*)
  INTO   v_total_rechazados_duplicados
  FROM   dis_rch_avance_pago
  WHERE  folio  = g_folio
  AND    estado = 103;
   
  --DISPLAY "Total Rechazados: ",v_total_rechazados

  SELECT COUNT(*)
  INTO   v_total_aceptados
  FROM   dis_det_avance_pago
  WHERE  folio = g_folio

  --DISPLAY "Total Aceptados: ",v_total_aceptados

  --Obtiene totales de aportación y amortización cancelados
  SELECT SUM(monto_aportacion), SUM(monto_amortizacion)
  INTO   v_total_aportacion_can, v_total_amortizacion_can
  FROM   dis_det_avance_pago   
  WHERE  folio = g_folio
   
  DISPLAY "\n ############### INTEGRACIÓN CANCELACIÓN AVANCES DE PAGO POR ACLARACIONES PATRONALES ###############"
  DISPLAY "Total de registros: ",v_total_registros
  DISPLAY "Total de registros aceptados: ",v_total_aceptados
  DISPLAY "Total de registros rechazados: ",v_total_rechazados
  DISPLAY "Total de aportación cancelada: ",v_total_aportacion_can
  DISPLAY "Total de amortización cancelada: ",v_total_amortizacion_can
  DISPLAY "Nombre del Archivo: ",l_arch_proceso

  LET v_query = "\n SELECT D.nrp, D.periodo_pago, count(*) ",
                "\n FROM   dis_det_avance_pago D ",
                "\n WHERE  D.folio = ",g_folio,
                "\n GROUP BY 1,2 ",
                "\n UNION ALL  ",
                "\n SELECT D.nrp, D.periodo_pago, count(*) ",
                "\n FROM   dis_rch_avance_pago D ",
                "\n WHERE  D.folio = ",g_folio,
                "\n GROUP BY 1,2 "

  PREPARE prp_datos_rpt FROM v_query
  DECLARE cur_datos_rpt CURSOR FOR prp_datos_rpt

  LET v_indice = 1

  FOREACH cur_datos_rpt INTO arr_cancelacion[v_indice].arr_nrp,
                             arr_cancelacion[v_indice].arr_periodo_pago,
                             arr_cancelacion[v_indice].arr_cuenta_reg
    LET v_indice = v_indice + 1
  END FOREACH 

  CALL arr_cancelacion.deleteElement(v_indice)
   
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE281.4rp") THEN
     CALL fgl_report_selectDevice ("PDF")
     --Sin preview
     CALL fgl_report_selectPreview(0)
     --Se indica que se escriba en archivo
     CALL fgl_report_setOutputFileName(v_ruta_reporte)
       
     LET manejador_rpt = fgl_report_commitCurrentSettings()

     START REPORT rpt_cancelacion TO XML HANDLER manejador_rpt
       FOR v_indice = 1 TO arr_cancelacion.getLength()
           OUTPUT TO REPORT rpt_cancelacion(arr_cancelacion[v_indice].*,
                                            v_total_registros,
                                            v_total_rechazados,
                                            v_total_aceptados,
                                            v_total_rechazados_duplicados,
                                            v_total_aportacion_can,
                                            v_total_amortizacion_can)
       END FOR 
     FINISH REPORT rpt_cancelacion
  ELSE
     DISPLAY "No se pudo generar el reporte."
     --EXIT PROGRAM
  END IF
END FUNCTION

--Reporte de cancelación de avance de pagos por aclaraciones patronales
REPORT rpt_cancelacion(rpt_arr_cancelacion,p_total_registros,p_total_rechazados,
                       p_total_aceptados,p_total_rechazados_duplicados,
                       p_total_aportacion_can,p_total_amortizacion_can)

  DEFINE rpt_arr_cancelacion RECORD 
         arr_nrp             LIKE dis_det_avance_pago.nrp,
         arr_periodo_pago    LIKE dis_det_avance_pago.periodo_pago,
         arr_cuenta_reg      INTEGER 
  END RECORD 

  DEFINE
     p_total_registros       DECIMAL(10,0),
     p_total_rechazados      DECIMAL(10,0),
     p_total_aceptados       DECIMAL(10,0),
     p_total_rechazados_duplicados DECIMAL(10,0)

  --Define variables de la función 
  DEFINE 
    v_fecha_presentacion     DATE,
    v_fecha_reporte          DATE

  DEFINE 
    p_total_aportacion_can   DECIMAL(22,2),
    p_total_amortizacion_can DECIMAL(22,2)
         
  FORMAT 
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX v_fecha_presentacion USING "dd-mm-yyyy" 
      PRINTX l_arch_proceso
      PRINTX g_folio
      PRINTX v_usuario
      PRINTX p_total_registros
      PRINTX p_total_rechazados
      PRINTX p_total_aceptados
      PRINTX p_total_rechazados_duplicados
      PRINTX p_total_aportacion_can
      PRINTX p_total_amortizacion_can

  ON EVERY ROW 
     PRINTX rpt_arr_cancelacion.*

END REPORT