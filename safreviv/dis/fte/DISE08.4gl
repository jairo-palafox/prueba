################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 11/04/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE08                                                   #
#Objetivo          => Programa para integrar el archivo de rechazos            #
#Fecha inicio      => 19/06/2012                                               #
################################################################################
DATABASE safre_viv 
GLOBALS
 DEFINE v_usuario       VARCHAR(30), -- Almacena al usuario
        g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo de proceso
        g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
        l_pid          LIKE glo_pid.pid,
        g_folio        LIKE dis_det_avance_pago.folio, -- Folio generado
        l_arch_proceso VARCHAR(100)
END GLOBALS
--Objetivo: Funcion que realiza la carga de tablas hitoricas de avance de pago
MAIN
DEFINE 
       l_s_qryTxt             STRING, -- guarda una sentencia SQL a ejecutar
       r_b_valida             SMALLINT,
       r_bnd_proceso_cnt      SMALLINT,
       v_fecha_reg            DATE,
       r_bnd_edo_act_archivo  SMALLINT,
       r_bnd_oera_error       SMALLINT,  -- Bandera actualiza operacion en error
       r_bnd_error_op         SMALLINT,
       p_cve_proceso_cnt      SMALLINT,
       p_transaccion_cnt      SMALLINT,
       p_transaccion          SMALLINT, --Bandera que indica si la ejecución es manual o automática
       v_cuenta_contable      DECIMAL (10,0) --Contador de registro contable
       
   LET v_usuario      = ARG_VAL(1)
   LET l_pid          = ARG_VAL(2)
   LET g_proceso_cod  = ARG_VAL(3)
   LET g_opera_cod    = ARG_VAL(4)
   LET g_folio        = ARG_VAL(5)
   LET l_arch_proceso = ARG_VAL(6)



      LET p_transaccion  = 0
      LET r_bnd_proceso_cnt = 0
      LET v_fecha_reg = TODAY

   --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso

   SELECT  ind_tipo_ejecucion 
   INTO p_transaccion
   FROM  bat_ctr_operacion 
   WHERE proceso_cod = g_proceso_cod   
   AND  pid = l_pid
   AND opera_cod = g_opera_cod

   IF p_transaccion = 1 THEN 

      CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usuario)
      RETURNING g_folio

   END IF 

   
   -- Ejecuta StoreProcedure para cargar tablas históricas de avance pago
   WHENEVER ERROR CONTINUE
      PREPARE prp_carga_disp
         FROM "EXECUTE PROCEDURE safre_viv:sp_dis_avances_pago_rch(?,?)"
      EXECUTE prp_carga_disp USING g_folio,v_usuario
   WHENEVER ERROR STOP 

      IF SQLCA.sqlcode < 0 THEN
         DISPLAY "Código de ERROR SQL ",SQLCA.sqlcode
         -- Función para finalizar la operación en error
         CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
                   RETURNING r_bnd_oera_error
            EXIT PROGRAM
      END IF

      
      LET p_cve_proceso_cnt = 45  --Avance de pagos
      LET p_transaccion_cnt = 22  --SE DEBERÁ REGISTRAR EL IMPORTE DEL AVANCE

      WHENEVER ERROR CONTINUE
      --Se agrega función para realizar el registro contable ##
        PREPARE prp_reg_contable
           FROM "EXECUTE PROCEDURE safre_viv:fn_avance_cnt18(?,?,?,?,?)"
        EXECUTE prp_reg_contable USING g_folio,
                                       v_fecha_reg,
                                       p_cve_proceso_cnt,
                                       g_proceso_cod,
                                       p_transaccion_cnt
                                  INTO r_bnd_proceso_cnt

      WHENEVER ERROR STOP 

      IF SQLCA.sqlcode < 0 THEN
         DISPLAY "Código de ERROR SQL de registro contable: ",SQLCA.sqlcode
         -- Función para finalizar la operación en error
         CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
                   RETURNING r_bnd_oera_error
         EXIT PROGRAM
      END IF

                                  
         IF r_bnd_proceso_cnt = 1 THEN -- 1 es correcto

            SELECT COUNT (*)
            INTO v_cuenta_contable
            FROM cnt_transaccion
            WHERE folio_liquida = g_folio

            IF v_cuenta_contable > 0 THEN  
               DISPLAY "El registro contable del rechazo de avance de pagos se realizó exitosamente."

            ELSE 
               DISPLAY "Error: El registro contable no se realizó debidamente."
            END IF 
         ELSE 
            DISPLAY "Ocurrió un error al realizar el registro contable."
         
         END IF 



                                  
         DISPLAY "Nombre Archivo  ",l_arch_proceso
         

         
         -- Actualiza el estado del archivo procesado
         CALL fn_act_edo_archivo(l_arch_proceso,g_folio,2,v_usuario)
                       RETURNING r_bnd_edo_act_archivo
         
         -- Función para finalizar la operación
         CALL fn_actualiza_opera_fin(l_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_b_valida
         --Valida Operación Final
         IF r_b_valida <> 0 THEN
            CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_error_op
         ELSE 
            --Generar reporte
            CALL fn_genera_reporte_rch(g_folio,v_usuario)
         END IF --Operación
          -- Envío de correo de notificación de proceso finalizado
         CALL fn_correo_proceso(l_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                'adjunto?',
                                'Integración de Registro Rechazo de Avance de Pagos',
                                'ID Proceso   : '||l_pid||
                                'Proceso      : '||g_proceso_cod||
                                'Operacion    : '||g_opera_cod||
                                'Fecha Inicio : '||TODAY||
                                'Fecha Fin    : '||TODAY)
         
        
        

END MAIN

#Función para generar el reporte de rechazos de aportaciones subsecuentes
FUNCTION fn_genera_reporte_rch(g_folio,v_usuario)
   DEFINE 
         v_ruta_reporte    STRING, -- ruta del archivo del reporte
         v_ruta_listados   STRING, -- ruta de los listados
         v_ruta_ejecutable STRING, -- ruta del ejecutable
         v_origen_datos    STRING,
         v_QryTxt          STRING,
         v_indice          INTEGER,
         v_usuario         VARCHAR(30), -- Almacena al usuario
         manejador_rpt     om.SaxDocumentHandler,  -- Contenedor documentos reporte
         g_folio           LIKE dis_det_avance_pago.folio
         

   DEFINE --Define variables para encabezado
         v_total0          DECIMAL (10,0),--SMALLINT, 
         v_total1          DECIMAL (10,0),--SMALLINT,
         v_total2          DECIMAL (10,0),--INTEGER,
         v_total3          DECIMAL (10,0),--SMALLINT,
         v_total4          DECIMAL (10,0)--SMALLINT

   DEFINE --Define variabels para detalle
         v_total_reg_apo      DECIMAL (10,0),--INTEGER,
         v_total_reg_amo      DECIMAL (10,0),--INTEGER,
         v_total_reg_apo_amo  DECIMAL (10,0),--DECIMAL (10,0),--INTEGER,
         v_total_reg          INTEGER,
         v_total_suma_apo     DECIMAL (22,2),
         v_total_suma_amo     DECIMAL (22,2),
         v_total_reg_num_cred DECIMAL (10,0),--INTEGER,
         v_total_reg_rch      DECIMAL (10,0),--INTEGER,
         v_total_reg_rch_om   DECIMAL (10,0),--INTEGER,
         arr_total_reg_periodo  DYNAMIC ARRAY OF RECORD 
            v_perido          LIKE dis_rch_avance_pago.periodo_pago,
            v_total_reg_p     DECIMAL (10,0)--INTEGER
         END RECORD 

   --Total Tipos de Registro 0 - Encabezado.
   SELECT count(*)   INTO  v_total0
   FROM  safre_tmp:tmp_dis_ravances_pago0

   --Total Tipos de Registro 1 – Encabezado Detalle.
   SELECT count(*)   INTO  v_total1
   FROM  safre_tmp:tmp_dis_ravances_pago1

   --Total Tipos de Registro 2 – Detalle.
   SELECT count(*)   INTO  v_total2
   FROM  safre_tmp:tmp_dis_ravances_pago2

   --Total Tipos de Registro 3 – Sumario Detalle.
   SELECT count(*)   INTO  v_total3
   FROM  safre_tmp:tmp_dis_ravances_pago3

   --Total Tipos de Registro 4 – Sumario Archivo. 
   SELECT count(*)   INTO  v_total4 
   FROM  safre_tmp:tmp_dis_ravances_pago4

   -- Total de Registros con Aportaciones.
   SELECT count(*) INTO v_total_reg_apo
   FROM  safre_tmp:tmp_dis_ravances_pago2
   WHERE monto_aportacion <> 0.00
   AND (monto_amortizacion = 0.0 OR monto_amortizacion IS NULL)  

   --Total de Registros con Amortizaciones.
   SELECT count(*) INTO v_total_reg_amo
   FROM  safre_tmp:tmp_dis_ravances_pago2
   WHERE monto_amortizacion <> 0.00
   AND (monto_aportacion = 0.0 OR monto_aportacion IS NULL)  

   --Total de Registros con Aportaciones y Amortizaciones.
   SELECT count(*) INTO v_total_reg_apo_amo
   FROM  safre_tmp:tmp_dis_ravances_pago2
   WHERE monto_amortizacion <> 0.00
   AND monto_aportacion <> 0.00

   --Total de Registros.
   SELECT tot_registros INTO v_total_reg
   FROM dis_sum_avance_pago
   WHERE folio = g_folio

   -- Importe Total de Aportaciones.
   SELECT tot_aportacion INTO v_total_suma_apo
   FROM dis_sum_avance_pago
   WHERE folio = g_folio

   --Importe Total de Amortizaciones. 
   SELECT tot_amortizacion INTO v_total_suma_amo
   FROM dis_sum_avance_pago
   WHERE folio = g_folio

   --Total de Registros con Número de Crédito en ceros.
   SELECT count(*) INTO v_total_reg_num_cred
   FROM dis_rch_avance_pago
   WHERE folio = g_folio
   AND estado = 27

   --Total de Registros Rechazados (Por no encontrarse en el maestro de derechohabientes).
   SELECT count(*) INTO v_total_reg_rch
   FROM dis_rch_avance_pago
   WHERE folio = g_folio
   AND estado = 20

   --Total de Registros Rechazados (Otros motivos).
   SELECT count(*) INTO v_total_reg_rch_om
   FROM dis_rch_avance_pago
   WHERE folio = g_folio
   AND (estado <> 27 AND estado <> 20)


   --Despliega información en el log
   DISPLAY "\n ############### INTEGRACIÓN REGISTRO AVANCES DE PAGO ###############"
   DISPLAY "Total de registros con aportaciones: ",v_total_reg_apo CLIPPED 
   DISPLAY "Total de registros con amortizaciones: ",v_total_reg_amo CLIPPED
   DISPLAY "Total de registros con aportaciones y amortizaciones: ",v_total_reg_apo_amo CLIPPED
   DISPLAY "Total de aportaciones: ",v_total_suma_apo CLIPPED
   DISPLAY "Total de amortizaciones: ",v_total_suma_amo CLIPPED
   DISPLAY "Total de registros: ",v_total2
   DISPLAY "Nombre del Archivo: ",l_arch_proceso
   
   
   --DISPLAY "Ruta del reporte: ",v_ruta_reporte



   
   --Total de Registros por Período.
   LET v_QryTxt = "\n SELECT periodo_pago, count(*) AS  TOTAL",
                  "\n FROM dis_rch_avance_pago ",
                  "\n WHERE folio = ",g_folio, --folio de rechazo
                  "\n AND periodo_pago IN",
                  "\n (",
                  "\n SELECT DISTINCT periodo_pago FROM dis_rch_avance_pago",
                  "\n WHERE folio = ",g_folio,
                  "\n UNION ",
                  "\n SELECT DISTINCT periodo_pago FROM dis_det_avance_pago",
                  "\n WHERE folio = ",g_folio,
                  "\n )",
                  "\n GROUP BY 1",
                  
                  "\n UNION ",
                  
                  "\n SELECT periodo_pago, count(*) AS  TOTAL",
                  "\n FROM dis_det_avance_pago ",
                  "\n WHERE folio = ",g_folio, --folio de rechazo
                  "\n AND periodo_pago IN",
                  "\n (",
                  "\n SELECT DISTINCT periodo_pago FROM dis_rch_avance_pago",
                  "\n WHERE folio = ",g_folio,
                  "\n UNION ",
                  "\n SELECT DISTINCT periodo_pago FROM dis_det_avance_pago",
                  "\n WHERE folio = ",g_folio,
                  "\n )",
                  "\n GROUP BY 1"

   PREPARE prp_datos_rpt FROM v_QryTxt
   DECLARE cur_datos_rpt CURSOR FOR prp_datos_rpt

   LET v_indice = 1

   FOREACH cur_datos_rpt INTO arr_total_reg_periodo[v_indice].v_perido,
                              arr_total_reg_periodo[v_indice].v_total_reg_p
   
      LET v_indice = v_indice + 1

   END FOREACH 

   CALL arr_total_reg_periodo.deleteElement(v_indice)
   LET v_origen_datos = v_usuario


   CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
    LET v_ruta_reporte = v_ruta_listados.trim(),
                         "/",
                         v_origen_datos.trim(),"-",
                         "DISE08","-",
                         l_pid USING "&&&&&","-",
                         g_proceso_cod USING "&&&&&","-",
                         g_opera_cod USING "&&&&&",".pdf"

   


   
   --Se asigna la plantilla para generar el reporte
   IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE081.4rp") THEN
       
          CALL fgl_report_selectDevice ("PDF")
          -- sin preview
          CALL fgl_report_selectPreview(0)
          -- se indica que se escriba en archivo
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
       
          LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE
       DISPLAY "No se pudo generar el reporte"
       EXIT PROGRAM
    END IF

   --DISPLAY "Longitud -- ",arr_total_reg_periodo.getLength()
    
   START REPORT rpt_dis_avance_pago_rch TO XML HANDLER manejador_rpt
         
         FOR v_indice = 1 TO arr_total_reg_periodo.getLength()
            OUTPUT TO REPORT rpt_dis_avance_pago_rch(    g_folio,
                                                         v_usuario,
                                                         v_total0,
                                                         v_total1,
                                                         v_total2,
                                                         v_total3,
                                                         v_total4,
                                                         v_total_reg_apo,
                                                         v_total_reg_amo,
                                                         v_total_reg_apo_amo,
                                                         v_total_reg,
                                                         v_total_suma_apo,
                                                         v_total_suma_amo,
                                                         v_total_reg_num_cred,
                                                         v_total_reg_rch,
                                                         v_total_reg_rch_om,
                                                         arr_total_reg_periodo[v_indice].*
                                                      )
         END FOR

   FINISH REPORT rpt_dis_avance_pago_rch
   
END FUNCTION 

REPORT rpt_dis_avance_pago_rch(g_folio,v_usuario, v_total0,v_total1,v_total2,v_total3,
                               v_total4,v_total_reg_apo,v_total_reg_amo,
                               v_total_reg_apo_amo,v_total_reg,v_total_suma_apo,
                               v_total_suma_amo,v_total_reg_num_cred,v_total_reg_rch,
                               v_total_reg_rch_om,arr_total_reg_periodo)

   DEFINE --Define variables para encabezado
         v_total0          DECIMAL (10,0),--SMALLINT, 
         v_total1          DECIMAL (10,0),--SMALLINT,
         v_total2          DECIMAL (10,0),--INTEGER,
         v_total3          DECIMAL (10,0),--SMALLINT,
         v_total4          DECIMAL (10,0)--SMALLINT

   DEFINE --Define variabels para detalle
         v_total_reg_apo      DECIMAL (10,0),--INTEGER,
         v_total_reg_amo      DECIMAL (10,0),--INTEGER,
         v_total_reg_apo_amo  DECIMAL (10,0),--INTEGER,
         v_total_reg          DECIMAL (10,0),--INTEGER,
         v_total_suma_apo     DECIMAL (22,2),
         v_total_suma_amo     DECIMAL (22,2),
         v_total_reg_num_cred DECIMAL (10,0),--INTEGER,
         v_total_reg_rch      DECIMAL (10,0),--INTEGER,
         v_total_reg_rch_om   DECIMAL (10,0),--INTEGER,
         arr_total_reg_periodo  RECORD 
            v_perido          LIKE dis_rch_avance_pago.periodo_pago,
            v_total_reg_p     DECIMAL (10,0)--INTEGER
         END RECORD 

   --Define variables de la función 
   DEFINE 
         v_fecha_presentacion    DATE,
         v_fecha_reporte         DATE,
         g_folio                 LIKE dis_det_avance_pago.folio,
         v_usuario               VARCHAR (30)
         
   FORMAT 
      FIRST PAGE HEADER
         LET v_fecha_reporte = TODAY
         PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
         PRINTX v_fecha_presentacion USING "dd-mm-yyyy" 
         PRINTX g_folio
         PRINTX v_usuario
         PRINTX v_total0 
         PRINTX v_total1 
         PRINTX v_total2 
         PRINTX v_total3 
         PRINTX v_total4 
         PRINTX v_total_reg_apo 
         PRINTX v_total_reg_amo 
         PRINTX v_total_reg_apo_amo 
         PRINTX v_total_reg 
         PRINTX v_total_suma_apo 
         PRINTX v_total_suma_amo 
         PRINTX v_total_reg_num_cred 
         PRINTX v_total_reg_rch 
         PRINTX v_total_reg_rch_om 
         PRINTX l_arch_proceso
      
      ON EVERY ROW
         PRINTX arr_total_reg_periodo.*



         
END REPORT 