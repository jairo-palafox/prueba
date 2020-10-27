--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 31/Oct/2016
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEP05                                                        #
#Objetivo     => Programa lanzado de la integracion de la respuesta PROCESAR   #
#                para Devolucion de Pagos Indebidos o en Exceso                #
#Fecha inicio => Octubre 31, 2016                                              #
################################################################################

GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN

DEFINE p_pid                   LIKE bat_ctr_operacion.pid -- PID del proceso
       ,p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod            LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
       ,p_usuario_cod          LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,v_s_sql                STRING -- cadena con una instruccion SQL
       ,v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper         SMALLINT
       ,v_si_correcto_integra  SMALLINT
       ,p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       ,v_folio                LIKE deo_preliquida.folio_liquida
       --
       ,v_si_status_det_trab   SMALLINT
       ,v_si_status_sum_patron SMALLINT
       ,v_si_status_sum_exceso SMALLINT
       ,p_titulo               STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje              STRING -- cuerpo del mensaje enviado
       ,v_layout               LIKE cat_operacion.layout_cod
       ,v_ruta_rescate         STRING
       ,v_usuario              LIKE seg_modulo.usuario
       ,v_proceso_desc         LIKE cat_proceso.proceso_desc
       ,v_extension            LIKE cat_operacion.extension
       ,v_opera_desc           LIKE cat_operacion.opera_desc
       ,v_ruta_listados        LIKE seg_modulo.ruta_listados
       ,v_nss                  CHAR(11)  
       ,v_dte_fecha_hoy        DATE
       ,v_fondo                SMALLINT
       ,v_error_sql            INTEGER
       ,v_error_isam           INTEGER
       ,v_mensaje_error        VARCHAR(250)
       ,v_ruta_bin             LIKE seg_modulo.ruta_bin
       ,v_s_comando            STRING
       ,v_msj_sql              CHAR(250)
       ,v_si_estado            SMALLINT
       ,v_rp_nss               CHAR(11)
       ,v_folio_liquidacion    DECIMAL(9,0)
       ,v_tot_regs_integrados  INTEGER

       
   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DPEP05.log")

   LET v_dte_fecha_hoy = TODAY

   LET v_fondo = 11
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   LET v_i_resultado = 0
   LET v_si_correcto_integra = 0
   
   -- Ejecuta SP que integra la respuesta de PROCESAR
   LET v_s_sql = "EXECUTE FUNCTION fn_dpe_integra_resp_procesar(?,?,?,?,?)"

   -- se prepara y ejecuta el SP de integracion
   PREPARE prp_cal_acciones FROM v_s_sql
   EXECUTE prp_cal_acciones USING p_usuario_cod, 
                                  v_folio, 
                                  p_nombre_archivo, 
                                  g_pid, 
                                  g_proceso_cod
                             INTO v_error_sql, 
                                  v_error_isam, 
                                  v_mensaje_error, 
                                  v_nss  

--DISPLAY  v_error_sql,  " - ", v_error_isam, " - ",v_mensaje_error,  " - ",v_nss                                    
   -- si no ocurrio error al integrar
   IF ( v_error_sql = 0 ) THEN
      DISPLAY " "
      DISPLAY "   La integración se terminó completamente."
      DISPLAY " "
      DISPLAY "   Integración realizada con exito"
      DISPLAY " "
      DISPLAY "   Estatus Resultado :",v_error_sql        
      
      -- Genera cifras control por registro de patron.
      CALL fn_obtiene_cifras_control(v_folio)                  
      
      LET p_mensaje = "   La integración se terminó completamente.","\n",
                      "   Integración realizada con exito","\n",
                      "   Folio lote o de integración : ",v_folio,"\n",
                      "   Estatus Resultado :",v_error_sql,"\n"

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bnd_fin_oper
   ELSE
      DISPLAY "Ocurrió un error al realizar el proceso de integración."
      DISPLAY "Error (SQL) : ", v_error_sql
      DISPLAY "Error (ISAM): ", v_error_isam
      DISPLAY "Mensaje     : ", v_mensaje_error
      DISPLAY "NSS         : ", v_nss

      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper

      IF ( v_i_resultado <> 100 ) THEN
         DISPLAY "   Error. No se integró ninguna solicitud"
      ELSE
         DISPLAY "   ",fn_status_secciones_integradas(v_si_status_det_trab 
                 ,v_si_status_sum_patron,v_si_status_sum_exceso)
      END IF
      
      LET p_mensaje = " --- ERROR ---\n",
                   " El proceso de Integración no terminó correctamente.\n",
                   " Código de error : ", v_error_sql,"\n ",
                   " FECHA           : ",TODAY,"\n",
                   " HORA            : ",CURRENT HOUR TO SECOND,"\n"           
      DISPLAY "\n"

   END IF

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
   
END MAIN


#OBJETIVO: Generar cifras por patron para monitor de procesos mediante display.
FUNCTION fn_obtiene_cifras_control(p_folio)
   DEFINE 
    p_folio  LIKE deo_preliquida.folio_liquida
   --
   ,v_reg_patranal           CHAR(11)
   ,v_total_solicitudes      INTEGER
   ,v_suma_importes_patronal DECIMAL(18,6)
   --
   ,v_s_qry                  STRING
 
    LET v_s_qry = "\n SELECT reg_patronal_imss, SUM(imp_viv_dev), COUNT(*)",
                  "\n   FROM safre_viv:dpe_resp_procesar",
                  "\n  WHERE folio = ?",
                  "\n  GROUP BY 1",
                  "\n  ORDER BY 1"
   PREPARE Prpr_ObtDatosCbzaCtrl FROM v_s_qry CLIPPED
   DECLARE Curr_ObtDatosCbzaCtrl CURSOR FOR Prpr_ObtDatosCbzaCtrl 
   
   DISPLAY "-------- CIFRAS GLOBALES POR PATRON -------- "
   DISPLAY " "
   DISPLAY " "
   DISPLAY "   FOLIO :",p_folio
   DISPLAY " "
   
   
   FOREACH Curr_ObtDatosCbzaCtrl USING p_folio
                                 INTO  v_reg_patranal, 
                                       v_suma_importes_patronal,
                                       v_total_solicitudes
                                       
      DISPLAY "   REG. PATRONAL: ",v_reg_patranal
      DISPLAY "   TOTAL PESOS  : ",v_suma_importes_patronal
      DISPLAY "   SOLICITUDES  :", v_total_solicitudes
   END FOREACH

   FREE Curr_ObtDatosCbzaCtrl 

END FUNCTION -- fn_obtiene_cifras_control

#OBJETIVO: Asignar una descripción acorde al error que se muestra.
FUNCTION fn_status_secciones_integradas(v_si_detalle,
                                        v_si_sumario_patron, 
                                        v_si_sumario_exceso)
   DEFINE v_si_detalle        SMALLINT,
          v_si_sumario_patron SMALLINT,
          v_si_sumario_exceso SMALLINT,
          v_c_mensaje CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]" 
   END IF

   IF(v_si_sumario_patron = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Sumario Patron ]" 
   END IF

   IF(v_si_sumario_exceso = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Sumario Exceso ]"  
   END IF

    RETURN v_c_mensaje

END FUNCTION