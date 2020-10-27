################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 15/01/2013                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE11                                                   #
#Objetivo          => Programa lanzado para la integración de los ajustes      #
#                     de Avance de Pagos                                       #
#Fecha inicio      => 15/01/2013                                               #
################################################################################

DATABASE safre_viv 
GLOBALS
 DEFINE v_usuario       VARCHAR(30), -- Almacena al usuario
        g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo de proceso
        l_pid          LIKE glo_pid.pid,
        g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
        g_folio        LIKE dis_det_avance_pago.folio, -- Folio generdo
        l_arch_proceso VARCHAR(100)

 DEFINE p_cve_proceso_cnt SMALLINT
 DEFINE p_transaccion     SMALLINT
 DEFINE r_bnd_proceso_cnt SMALLINT
 DEFINE v_fecha_reg       DATE 

END GLOBALS

MAIN
DEFINE 
    r_b_valida     SMALLINT,
    r_bnd_edo_act_archivo SMALLINT,
    r_bnd_oera_error SMALLINT,-- Bandera actualiza operacion en error
    r_bnd_error_op   SMALLINT,
    p_transaccion    SMALLINT, --Bandera que indica si la ejecución es manual o automática
    v_error          INTEGER,
    v_estatus        INTEGER,
    v_info           VARCHAR (70)


   DEFINE
      p_transaccion_cnt      SMALLINT,
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

   {IF p_transaccion = 1 THEN 

      CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usuario)
      RETURNING g_folio

   END IF }


   -- Ejecuta StoreProcedure para cargar tablas
   WHENEVER ERROR CONTINUE
      PREPARE prp_carga_disp_ajuste
         FROM "EXECUTE PROCEDURE safre_viv:sp_dis_avances_pago_ajuste(?,?)"
      EXECUTE prp_carga_disp_ajuste USING g_folio,v_usuario
      INTO v_error, v_estatus, v_info

      
         IF SQLCA.sqlcode < 0 THEN
         DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
         -- Función para finalizar la operación en error
         CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
                   RETURNING r_bnd_oera_error
            EXIT PROGRAM
         END IF

   
      --Se agrega generación del registro contable
      LET p_cve_proceso_cnt = 51  --Verificar
      LET p_transaccion_cnt = 62  --Verificar

      --DISPLAY "g_folio: ", g_folio
      --DISPLAY "v_fecha_reg: ", v_fecha_reg
      --DISPLAY "p_cve_proceso_cnt: ", p_cve_proceso_cnt
      --DISPLAY "g_proceso_cod: ", g_proceso_cod
      --DISPLAY "p_transaccion_cnt: ", p_transaccion_cnt
      
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
               DISPLAY "El registro contable del ajuste de avances abiertos se realizó exitosamente."

            ELSE 
               DISPLAY "Error: El registro contable no se realizó debidamente."
            END IF 
         ELSE 
            DISPLAY "Ocurrió un error al realizar el registro contable."
         
         END IF 

         
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
            CALL fn_genera_reporte_ajuste()

            
         END IF --Operación
         -- Envío de correo de notificación de proceso finalizado
         CALL fn_correo_proceso(l_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                'adjunto?',
                                'Integración de Rechazo de diferencias de Avance de Pagos',
                                'ID Proceso   : '||l_pid||
                                '\nProceso      : '||g_proceso_cod||
                                '\nOperacion    : '||g_opera_cod||
                                '\nFecha Inicio : '||TODAY||
                                '\nFecha Fin    : '||TODAY)

END MAIN


--Genera el reporte de los datos que fueron procesados en el rechazo de diferencias de avance de pagos
FUNCTION fn_genera_reporte_ajuste()

   DEFINE 
         v_ruta_reporte    STRING, -- ruta del archivo del reporte
         v_ruta_listados   STRING, -- ruta de los listados
         v_origen_datos    STRING,
         v_query           STRING,
         v_indice          INTEGER,
         manejador_rpt     om.SaxDocumentHandler,  -- Contenedor documentos reporte
         v_ruta_ejecutable STRING -- ruta del ejecutable

    DEFINE
         v_total_registros          DECIMAL (10,0),--
         v_total_rechazados         DECIMAL (10,0),--
         v_total_aceptados          DECIMAL (10,0)--
    
         
    CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados

    LET v_origen_datos = v_usuario
    
    LET v_ruta_reporte = v_ruta_listados.trim(),
                         "/",
                         v_origen_datos.trim(),"-",
                         "DISL14","-",
                         l_pid USING "&&&&&","-",
                         g_proceso_cod USING "&&&&&","-",
                         g_opera_cod USING "&&&&&",".pdf"

   DISPLAY "Ruta del reporte: ",v_ruta_reporte

   --DISPLAY "Ruta del reporte --- ",v_ruta_reporte

   --Obtenemos total de registros
   SELECT COUNT (*)
   INTO v_total_registros
   FROM safre_tmp:tmp_dis_ajuste_ava_pag0

   --DISPLAY "Total de registros: ",v_total_registros

   --Obtenemos total de registros rechazados
   SELECT COUNT (*)
   INTO v_total_rechazados
   FROM dis_rch_avance_pago
   WHERE folio = g_folio

   --Obtenemos total de registros aceptados
   SELECT COUNT (*)
   INTO v_total_aceptados
   FROM dis_DET_avance_pago
   WHERE folio = g_folio


   DISPLAY "\n ############### INTEGRACIÓN DEL AJUSTE DE AVANCES ABIERTOS ###############"
   DISPLAY "Total de registros: ",v_total_registros
   DISPLAY "Total de registros aceptados: ",v_total_aceptados
   DISPLAY "Total de registros rechazados: ",v_total_rechazados
   DISPLAY "Nombre del Archivo: ",l_arch_proceso

   
   --Se asigna la plantilla para generar el reporte
   IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE111.4rp") THEN
       
          CALL fgl_report_selectDevice ("PDF")
          -- sin preview
          CALL fgl_report_selectPreview(0)
          -- se indica que se escriba en archivo
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
       
          LET manejador_rpt = fgl_report_commitCurrentSettings()


            START REPORT rpt_ajuste TO XML HANDLER manejador_rpt


                  OUTPUT TO REPORT rpt_ajuste(v_total_registros,
                                                   v_total_rechazados,
                                                   v_total_aceptados)

            
            FINISH REPORT rpt_ajuste
          
    ELSE
       DISPLAY "No se pudo generar el reporte"
       --EXIT PROGRAM
    END IF
   

END FUNCTION 


--Reporte de rechazo de diferencias de avance de pago
REPORT rpt_ajuste(p_total_registros,p_total_rechazados,p_total_aceptados)



   DEFINE
         p_total_registros    DECIMAL (10,0),
         p_total_rechazados   DECIMAL (10,0),
         p_total_aceptados    DECIMAL (10,0)

   --Define variables de la función 
   DEFINE 
         v_fecha_presentacion    DATE,
         v_fecha_reporte         DATE
         
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

         
END REPORT 
