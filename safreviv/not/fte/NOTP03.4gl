#####################################################################################################
#Modulo       => Notificaciones   'not'                                                             #
#Programa     => NOTP03                                                                             #
#Objetivo     => Programa que manda a ejecutar el lanzador NOTL02,                                  #
#                llamar al procedure cbd_det_omisos_trm                                             #                                 
#Fecha_inicio =>                                                                                    # 
#####################################################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario            CHAR(20)  -- parametro de entrada 
   DEFINE v_resul              SMALLINT  -- devuelve el resultado del procedure y almacena el resultado

   DEFINE v_pid                LIKE bat_ctr_proceso.pid  -- ID del proceso
   DEFINE v_proceso_cod        SMALLINT 
   DEFINE v_operacion_cod      SMALLINT
   DEFINE v_folio              DECIMAL(9,0)
   DEFINE v_archivo            VARCHAR(200)
END GLOBALS 

PRIVATE DEFINE v_ruta_listado  STRING

MAIN
   DEFINE v_error_opera        SMALLINT 
   DEFINE v_opera_fin          SMALLINT 
   
   -- se recuperan los parametros del lanzador
   LET p_usuario = ARG_VAL(1)
   LET v_pid = ARG_VAL(2)
   LET v_proceso_cod = ARG_VAL(3)
   LET v_operacion_cod = ARG_VAL(4)
   LET v_folio = ARG_VAL(5)
   LET v_archivo = ARG_VAL(6)
   

   DISPLAY  "**********************************************************************************"
   DISPLAY  " PROCESO:     Mostrar not_det_omisos_trm los registros encontrados de la consulta " 
   DISPLAY  " OPERACION:   Registros en la not_det_omisos_trm         "
   DISPLAY  " FECHA: ", MDY(MONTH(TODAY),DAY(TODAY),YEAR(TODAY))
   DISPLAY  " HORA:   ", CURRENT HOUR TO SECOND 
   DISPLAY  "**********************************************************************************"
   
   DISPLAY  "Lanzando el sp_not_det_omisos_trm"
   DISPLAY  "Termina la ejecución del proceso del sp_not_det_omisos_trm"

   DISPLAY  "**********************************************************************************"
   DISPLAY  " Término de la generación del lanzado sp_not_det_omisos_trm"
   DISPLAY  " PROCESO:   Termino del proceso sp_not_det_omisos_trm "
   DISPLAY  " OPERACION:   Notificaciones         "
   DISPLAY  " FECHA:   ", MDY(MONTH(TODAY),DAY(TODAY),YEAR(TODAY))
   DISPLAY  " HORA:   ", CURRENT HOUR TO SECOND                                      
   DISPLAY  "**********************************************************************************"
   
   -- funcion que genera el folio
   CALL fn_genera_folio(v_proceso_cod, v_operacion_cod, p_usuario) RETURNING v_folio   -- se llama a la funcion que genera el folio

   UPDATE bat_ctr_operacion 
      SET folio = v_folio
    WHERE pid = v_pid

   UPDATE bat_ctr_proceso
      SET folio = v_folio
    WHERE pid = v_pid

   UPDATE glo_ctr_archivo
      SET folio = v_folio
    WHERE nombre_archivo = v_archivo 
    
   CALL sp_not_det_omisos_trm(v_folio) RETURNING v_resul
   
      IF (v_resul = 0) THEN

         UPDATE glo_ctr_archivo
            SET estado = 2
          WHERE folio = v_folio
          
         -- Se genera el reporte con las cifras control
         CALL fn_genera_reporte()
         -- Finaliza la operacion de generacion del archivo
         CALL fn_actualiza_opera_fin(v_pid, v_proceso_cod, v_operacion_cod) RETURNING v_opera_fin 

         CALL fn_notifica_proceso(v_folio, v_proceso_cod, p_usuario)
      ELSE
         CALL fn_error_opera(v_pid, v_proceso_cod, v_operacion_cod) RETURNING v_error_opera
      END IF
      
END MAIN

FUNCTION sp_not_det_omisos_trm(p_folio)
   DEFINE p_folio    DECIMAL(9,0)
   DEFINE v_det_trm  STRING 
   DEFINE v_resul    INTEGER 

   LET v_det_trm = "EXECUTE PROCEDURE sp_not_det_omisos_trm(?)"
   PREPARE prp_det FROM v_det_trm
   EXECUTE prp_det USING p_folio INTO v_resul
   DISPLAY "Resultado del procedure: ", v_resul
   RETURN v_resul
END FUNCTION 

FUNCTION fn_genera_reporte()
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

	DEFINE v_ruta_exe          LIKE seg_modulo.ruta_bin         -- Ruta del ejecutable
   DEFINE v_ruta_listado      LIKE seg_modulo.ruta_listados    -- Ruta de listados
	DEFINE v_nombre	         STRING
   DEFINE v_folio             DECIMAL(9,0)

   SELECT ruta_bin, ruta_listados
     INTO v_ruta_exe,
          v_ruta_listado
     FROM seg_modulo 
    WHERE modulo_cod = 'not'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

   LET v_nombre = v_ruta_exe CLIPPED, "/NOTP03.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )

   START REPORT rp_cifras_contr_pagos_omisos TO XML HANDLER vhandler
      OUTPUT TO REPORT rp_cifras_contr_pagos_omisos(v_folio)
   FINISH REPORT rp_cifras_contr_pagos_omisos
   
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generación del archivo de cifras control de pagos omisos", v_ruta_listado
   DISPLAY ""
   DISPLAY "*******************************************************************"
   
END FUNCTION 

FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados    -- Ruta de listados

   DEFINE v_reporte          STRING
   DEFINE v_formato          STRING
   DEFINE v_preview          INTEGER
    
   -- /ds/safreviv_lst/bat
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'not'
   
   LET v_ruta_listado = v_ruta_listados CLIPPED , "/" ,
                        p_usuario CLIPPED , "-", -- usuario
                        "NOTP03" CLIPPED, "-", -- programa
                        v_pid USING "&&&&&","-", -- PID
                        v_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        v_operacion_cod   USING "&&&&&",".pdf" -- codigo de la operación
   
   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_ruta_listado)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
       EXIT PROGRAM
   END IF

   RETURN fgl_report_commitCurrentSettings()
   
END FUNCTION 

REPORT rp_cifras_contr_pagos_omisos(p_folio)
   DEFINE p_folio           DECIMAL(9,0)
   DEFINE v_fecha           DATE

   DEFINE v_nombre_archivo  CHAR(40) 
   DEFINE v_f_actualiza     DATE

   DEFINE v_count_det          INTEGER 
   DEFINE v_count_cta          INTEGER 

   DEFINE v_tot_reg_tpo1       INTEGER 
   DEFINE v_sum_tot_reg_tpo2   INTEGER 
   DEFINE v_tot_registros      INTEGER 

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY

      # Cifras Control de Pagos Omisos
         SELECT nombre_archivo,
                f_actualiza
           INTO v_nombre_archivo,
                v_f_actualiza
           FROM glo_ctr_archivo
          WHERE folio = v_folio
          
      # Cifras de Archivo de Pagos Omisos
         SELECT COUNT(*)
           INTO v_count_cta
           FROM safre_tmp:temp_cta_omisos_trm
   
         SELECT COUNT(*)
           INTO v_count_det
           FROM safre_tmp:temp_det_omisos_trm
      
      # Cifras Sumario de Pagos Omisos
         SELECT tot_reg_tpo1, (tot_reg_tpo2_scred + tot_reg_tpo2_ccred), tot_registros
           INTO v_tot_reg_tpo1,
                v_sum_tot_reg_tpo2,
                v_tot_registros
           FROM safre_tmp:temp_sum_omisos_trm
   -----------------------------------------------------------------------------

   PRINTX v_nombre_archivo
   PRINTX v_f_actualiza

   PRINTX v_count_det
   PRINTX v_count_cta

   PRINTX v_tot_reg_tpo1
   PRINTX v_sum_tot_reg_tpo2
   PRINTX v_tot_registros

   PRINTX v_folio
   PRINTX p_usuario
   PRINTX v_fecha USING "dd-mm-yyyy"

END REPORT 