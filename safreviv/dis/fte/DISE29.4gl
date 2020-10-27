################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 11/10/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE29                                                   #
#Objetivo          => Programa lanzado para integrar el arhivo de Cancelación  #
#                     Parcial de Avances de Pago.                              #
#Fecha inicio      => 26/09/2018                                               #
################################################################################
DATABASE safre_viv 
GLOBALS
  DEFINE 
    v_usuario                VARCHAR(30),                 --Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod,--Código de proceso
    l_pid                    LIKE glo_pid.pid,
    g_opera_cod              LIKE cat_operacion.opera_cod,--Código de operación
    g_folio                  LIKE dis_det_avance_pago.folio,--Folio generado
    v_ruta_ejecutable        STRING,                      --Ruta del ejecutable
    v_ruta_listados          STRING,                      --Ruta de los listados
    l_arch_proceso           VARCHAR(100)

  DEFINE p_cve_proceso_cnt   SMALLINT
  DEFINE p_transaccion       SMALLINT
  DEFINE r_bnd_proceso_cnt   SMALLINT
  DEFINE v_fecha_reg         DATE 

  DEFINE arr_cancelacion     DYNAMIC ARRAY OF RECORD 
    arr_nrp                  LIKE dis_det_avance_pago.nrp,
    arr_periodo_pago         LIKE dis_det_avance_pago.periodo_pago,
    arr_cuenta_reg           DECIMAL(10,0) 
  END RECORD

  --DEFINE v_val_rechazos      DECIMAL(10,0)
  
END GLOBALS

MAIN
  DEFINE 
    r_b_valida               SMALLINT,
    r_bnd_edo_act_archivo    SMALLINT,
    r_bnd_oera_error         SMALLINT, --Bandera actualiza operacion en error
    r_bnd_error_op           SMALLINT,
    p_transaccion            SMALLINT --Bandera que indica si la ejecución es manual o automática

  LET v_usuario      = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)

  LET p_transaccion     = 0
  LET v_fecha_reg       = TODAY
  LET r_bnd_proceso_cnt = 0
  --LET v_val_rechazos    = 0
    
  --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  SELECT ind_tipo_ejecucion 
  INTO   p_transaccion
  FROM   bat_ctr_operacion 
  WHERE  proceso_cod = g_proceso_cod   
  AND    pid         = l_pid
  AND    opera_cod   = g_opera_cod

  DISPLAY "Inicia operación.", TIME
  DISPLAY ""
  
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usuario)
     RETURNING g_folio
  END IF 

  --Borra y crea tablas dis_canc_par_ava_pag y dis_rch_avance_pago
  CALL fn_borra_crea_tablas()

  DISPLAY "Inicia integración del archivo de cancelación parcial avance de pagos. ", TIME
  DISPLAY ""
 
  --Ejecuta StoreProcedure para llevar a cabo la cancelación
  WHENEVER ERROR CONTINUE
    PREPARE prp_carga_cancelacion
    FROM "EXECUTE PROCEDURE safre_viv:sp_dis_can_par_ava_pag(?,?)"
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

  DISPLAY ""
  DISPLAY "Finalizó integración del archivo de cancelación parcial avance de pagos. ", TIME
  DISPLAY ""

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
     DISPLAY "Inicia la generación del reporte de cancelación parcial avance de pagos. ", TIME
     DISPLAY ""
     CALL fn_genera_reporte_can_par()
     DISPLAY ""
     DISPLAY "Finalizó la generación del reporte de cancelación parcial avance de pagos. ", TIME
     DISPLAY ""

     --Valida rechazos
     {IF v_val_rechazos = 0 THEN
        CALL fn_error_opera(l_pid,g_proceso_cod,3)
        RETURNING r_bnd_error_op

        CALL fn_desplega_inc_operacion(r_bnd_oera_error)

        CALL fn_error_opera(l_pid,g_proceso_cod,4)
        RETURNING r_bnd_error_op

        CALL fn_desplega_inc_operacion(r_bnd_oera_error)
     END IF}
     
     CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados

     DISPLAY "\n ############### GENERACIÓN ARCHIVO ###############"
     DISPLAY "Inicia la generación del archivo de cancelación parcial avance de pagos. ", TIME
     DISPLAY ""
     CALL fn_genera_arh_cpap()
     DISPLAY ""
     DISPLAY "Finalizó la generación del archivo de cancelación parcial avance de pagos. ", TIME
     DISPLAY ""
     DISPLAY "Finaliza operación.", TIME
     DISPLAY ""
  END IF --Operación

  --Envío de correo de notificación de proceso finalizado
  CALL fn_correo_proceso(l_pid,
                         g_proceso_cod,
                         g_opera_cod,
                         'adjunto?',
                         'Integración de Cancelación Parcial Avances de Pago',
                         'ID Proceso   : '||l_pid||
                         'Proceso      : '||g_proceso_cod||
                         'Operacion    : '||g_opera_cod||
                         'Fecha Inicio : '||TODAY||
                         'Fecha Fin    : '||TODAY)

END MAIN

--Genera el reporte de los datos que fueron procesados en la Cancelación Parcial Avances de Pago
FUNCTION fn_genera_reporte_can_par()
  DEFINE 
    v_ruta_reporte           STRING, --Ruta del archivo del reporte
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
    v_total_amortizacion_can DECIMAL(22,2),
    v_total_apo_can_rch      DECIMAL(22,2),
    v_total_amo_can_rch      DECIMAL(22,2),
    v_tot_apo                DECIMAL(22,2),
    v_tot_amo                DECIMAL(22,2),
    v_total_montos           DECIMAL(22,2),
    v_tot_monto_acep         DECIMAL(22,2),
    v_tot_monto_rch          DECIMAL(22,2)
         
  CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados

  LET v_origen_datos = v_usuario
   
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(),"-",
                       "DISE29","-",
                       l_pid USING "&&&&&","-",
                       g_proceso_cod USING "&&&&&","-",
                       g_opera_cod USING "&&&&&",".pdf"

  DISPLAY "Ruta del reporte --- ",v_ruta_reporte
  
  LET v_total_registros             = 0;
  LET v_total_rechazados            = 0;
  LET v_total_rechazados_duplicados = 0;
  LET v_total_aceptados             = 0;
  LET v_total_aportacion_can        = 0;
  LET v_total_amortizacion_can      = 0;
  LET v_total_apo_can_rch           = 0;
  LET v_total_amo_can_rch           = 0;
  LET v_tot_apo                     = 0;
  LET v_tot_amo                     = 0;
  LET v_total_montos                = 0;
  LET v_tot_monto_acep              = 0;
  LET v_tot_monto_rch               = 0;

  SELECT COUNT(*)
  INTO   v_total_registros
  FROM   dis_canc_par_ava_pag
  WHERE  folio = g_folio;
  IF v_total_registros IS NULL THEN
     LET v_total_registros = 0;
  END IF

  --DISPLAY "Total de registros: ",v_total_registros

  SELECT COUNT(*)
  INTO   v_total_rechazados
  FROM   dis_canc_par_ava_pag a
  WHERE  a.folio  = g_folio
  AND    a.estado = 0;
  IF v_total_rechazados IS NULL THEN
     LET v_total_rechazados = 0;
  END IF

  --LET v_val_rechazos = v_total_registros - v_total_rechazados;  
  
  --Se agrega total de registros rechazados por duplicidad
  SELECT COUNT(*)
  INTO   v_total_rechazados_duplicados
  FROM   dis_rch_avance_pago a
  WHERE  a.folio  = g_folio
  AND    a.estado = 103;

  --DISPLAY "Total Rechazados: ",v_total_rechazados

  SELECT COUNT(*)
  INTO   v_total_aceptados
  FROM   dis_canc_par_ava_pag a
  WHERE  a.folio  = g_folio
  AND    a.estado = 1;
  IF v_total_aceptados IS NULL THEN
     LET v_total_aceptados = 0;
  END IF

  --DISPLAY "Total Aceptados: ",v_total_aceptados

  --Obtiene totales de aportación y amortización cancelados aceptados
  SELECT SUM(a.monto_aportacion), SUM(a.monto_amortizacion)
  INTO   v_total_aportacion_can, v_total_amortizacion_can
  FROM   dis_canc_par_ava_pag a
  WHERE  a.folio  = g_folio
  AND    a.estado = 1;
  IF v_total_aportacion_can IS NULL THEN
     LET v_total_aportacion_can = 0;
  END IF
  IF v_total_amortizacion_can IS NULL THEN
     LET v_total_amortizacion_can = 0;
  END IF

  --Obtiene totales de aportación y amortización cancelados rechazados
  SELECT SUM(a.monto_aportacion), SUM(a.monto_amortizacion)
  INTO   v_total_apo_can_rch, v_total_amo_can_rch
  FROM   dis_canc_par_ava_pag a
  WHERE  a.folio  = g_folio
  AND    a.estado = 0;
  IF v_total_apo_can_rch IS NULL THEN
     LET v_total_apo_can_rch = 0;
  END IF
  IF v_total_amo_can_rch IS NULL THEN
     LET v_total_amo_can_rch = 0;
  END IF

  LET v_tot_monto_acep = v_total_aportacion_can   + v_total_amortizacion_can;
  LET v_tot_monto_rch  =  v_total_apo_can_rch     + v_total_amo_can_rch;
  LET v_tot_apo        = v_total_aportacion_can   + v_total_apo_can_rch;
  LET v_tot_amo        = v_total_amortizacion_can + v_total_amo_can_rch;
  LET v_total_montos   = v_tot_apo                + v_tot_amo;

  ---------1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901
  DISPLAY "\n ############### INTEGRACIÓN CANCELACIÓN POR DEPURACIÓN (AVANCES DE PAGO) ###############"
  DISPLAY "CONCEPTO           REGISTROS              APORTACIÓN            AMORTIZACIÓN                   TOTAL"
  DISPLAY "Aceptados        ", v_total_aceptados  USING "###,###,##&", "          ", v_total_aportacion_can USING "###,###,##&.&&", "          ", v_total_amortizacion_can USING "###,###,##&.&&", "          ", v_tot_monto_acep USING "###,###,##&.&&"  
  DISPLAY "Rechazados       ", v_total_rechazados USING "###,###,##&", "          ", v_total_apo_can_rch    USING "###,###,##&.&&", "          ", v_total_amo_can_rch      USING "###,###,##&.&&", "          ", v_tot_monto_rch  USING "###,###,##&.&&"
  DISPLAY "____________________________________________________________________________________________________________________________________"
  DISPLAY "TOTAL            ", v_total_registros  USING "###,###,##&", "          ", v_tot_apo              USING "###,###,##&.&&", "          ", v_tot_amo                USING "###,###,##&.&&", "          ", v_total_montos   USING "###,###,##&.&&" 

  {DISPLAY "Total de registros             : ",v_total_registros
  DISPLAY "Total de registros aceptados   : ",v_total_aceptados
  DISPLAY "Total de registros rechazados  : ",v_total_rechazados
  DISPLAY "Total de aportación cancelada  : ",v_total_aportacion_can
  DISPLAY "Total de amortización cancelada: ",v_total_amortizacion_can}
  DISPLAY ""
  DISPLAY "Nombre del Archivo: ",l_arch_proceso
  DISPLAY ""
  
  LET v_query = "\n SELECT D.nrp, D.periodo_pago, count(*) ",
                "\n FROM   dis_canc_par_ava_pag D ",
                "\n WHERE  D.folio = ",g_folio,
                --"\n WHERE  D.folio = 0",
                {"\n GROUP BY 1,2 ",
                "\n UNION ALL  ",
                "\n SELECT D.nrp, D.periodo_pago, count(*) ",
                "\n FROM   dis_rch_avance_pago D ",
                "\n WHERE  D.folio = ",g_folio,}
                "\n GROUP BY 1,2 "

  PREPARE prp_datos_rpt FROM v_query
  DECLARE cur_datos_rpt CURSOR FOR prp_datos_rpt

  LET v_indice = 1

  FOREACH cur_datos_rpt INTO arr_cancelacion[v_indice].arr_nrp,
                             arr_cancelacion[v_indice].arr_periodo_pago,
                             arr_cancelacion[v_indice].arr_cuenta_reg
    LET v_indice = v_indice + 1
    EXIT FOREACH
  END FOREACH 

  CALL arr_cancelacion.deleteElement(v_indice)
  
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE291.4rp") THEN
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
                                            v_total_rechazados_duplicados,
                                            v_total_aceptados,
                                            v_total_aportacion_can,
                                            v_total_amortizacion_can,
                                            v_total_apo_can_rch,
                                            v_total_amo_can_rch,
                                            v_tot_apo,
                                            v_tot_amo,
                                            v_total_montos,
                                            v_tot_monto_acep,
                                            v_tot_monto_rch
                                           )
       END FOR 
     FINISH REPORT rpt_cancelacion
  ELSE
     DISPLAY "No se pudo generar el reporte"
  END IF
END FUNCTION

--Reporte de cancelación de avance de pagos
REPORT rpt_cancelacion(rpt_arr_cancelacion,
                       p_total_registros,
                       p_total_rechazados,
                       p_total_rechazados_duplicados,
                       p_total_aceptados,
                       p_total_aportacion_can,
                       p_total_amortizacion_can,
                       p_total_apo_can_rch,
                       p_total_amo_can_rch,
                       p_tot_apo,
                       p_tot_amo,
                       p_total_montos,
                       p_tot_monto_acep,
                       p_tot_monto_rch)

  DEFINE rpt_arr_cancelacion RECORD 
    arr_nrp                  LIKE dis_det_avance_pago.nrp,
    arr_periodo_pago         LIKE dis_det_avance_pago.periodo_pago,
    arr_cuenta_reg           INTEGER 
  END RECORD 

  DEFINE
    p_total_registros        DECIMAL(10,0),
    p_total_rechazados       DECIMAL(10,0),
    p_total_aceptados        DECIMAL(10,0),
    p_total_rechazados_duplicados DECIMAL(10,0)
    
  --Define variables de la función 
  DEFINE 
    v_fecha_presentacion     DATE,
    v_fecha_reporte          DATE

  DEFINE 
    p_total_aportacion_can   DECIMAL(22,2),
    p_total_amortizacion_can DECIMAL(22,2),
    p_total_apo_can_rch      DECIMAL(22,2),
    p_total_amo_can_rch      DECIMAL(22,2),
    p_tot_apo                DECIMAL(22,2),
    p_tot_amo                DECIMAL(22,2),
    p_total_montos           DECIMAL(22,2),
    p_tot_monto_acep         DECIMAL(22,2),
    p_tot_monto_rch          DECIMAL(22,2)
         
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
      PRINTX p_total_apo_can_rch
      PRINTX p_total_amo_can_rch
      PRINTX p_tot_apo
      PRINTX p_tot_amo
      PRINTX p_total_montos
      PRINTX p_tot_monto_acep
      PRINTX p_tot_monto_rch

  ON EVERY ROW 
     PRINTX rpt_arr_cancelacion.*

END REPORT

#Objetivo: Borrar y crear sobre la base de datos las estructuras de las tablas
#          dis_rch_avance_pago y dis_canc_par_ava_pag
FUNCTION fn_borra_crea_tablas()

   WHENEVER ERROR CONTINUE;
     EXECUTE IMMEDIATE "SET INDEXES FOR dis_rch_avance_pago DISABLED;"
     EXECUTE IMMEDIATE "SET INDEXES FOR dis_canc_par_ava_pag DISABLED;"     
   WHENEVER ERROR STOP;

END FUNCTION

# Genera un archivo txt
FUNCTION fn_genera_arh_cpap()
  DEFINE
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo_orig       VARCHAR(55),  --Nombre del archivo de salida orig
    v_nom_archivo            VARCHAR(55),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100)  --Ruta y nombre del archivo de salida

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/cancelap_"    --nombre de archivo
  LET v_nom_archivo_orig  = v_nom_archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy" --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "cancelap_" || v_ddmmaaaa --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia         = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  --LET v_nom_archivo     = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_nom_archivo     = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||".txt"
  LET v_ruta_nomarch    = v_ruta_envio_dis CLIPPED || v_nom_archivo

  UNLOAD TO v_ruta_nomarch
  SELECT a.nss,
         a.nrp,
         a.periodo_pago,
         a.monto_aportacion,
         a.monto_amortizacion,
         CASE WHEN a.estado = 0 THEN 'RECHAZADO'
              WHEN a.estado = 1 THEN 'ACEPTADO'
         END CASE
  FROM   dis_canc_par_ava_pag a
  WHERE  a.folio = g_folio
  ORDER BY 3,1

  DISPLAY "Se ha generado el archivo de Cancelación Parcial Avances de Pago\n     en la ruta ",v_ruta_nomarch

END FUNCTION

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,       --Cadena de comando a ejecutar
    v_consecutivo            INTEGER       --Consecutivo del archivo por día

  DEFINE
    fn                       CHAR(24)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[18,20] --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION