--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11/06/2012
--===============================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPE33                                                     #
#Objetivo        => Programa de preliquidación de op 28                        #
#Fecha Inicio    => Junio 11, 2012                                             #
################################################################################
DATABASE safre_viv

DEFINE
   v_tot_liq                   INTEGER,
   v_txt                       VARCHAR(500),   
   v_opera_cod_next            SMALLINT,   
   v_programa                  VARCHAR(30),   
   r_resp                      SMALLINT,
   v_hay_desmarcas             SMALLINT,   
   g_pid                       LIKE bat_ctr_proceso.pid,     # ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, # Código del proceso
   g_opera_cod_carga           LIKE cat_operacion.opera_cod, # Código de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, # Clave de usuario
   g_opera_cod     LIKE cat_operacion.opera_cod, # Código de operacion
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod, # Código de operacion
   r_parejas_procesadas        INTEGER,
   r_parejas_expediente        INTEGER,
   r_parejas_diferencias       INTEGER,
   r_total_cargo_sar92         DECIMAL(22,2),
   r_total_abono_sar92         DECIMAL(22,2),
   r_total_cargo_viv92         DECIMAL(22,2),
   r_total_abono_viv92         DECIMAL(22,2),
   v_comando                   STRING

MAIN
DEFINE p_folio       LIKE glo_folio.folio,
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,
       r_error          SMALLINT,
       r_reultado_opera SMALLINT,
       p_titulo         STRING, # titulo del mensaje enviado en el correo
       p_mensaje        STRING,  # cuerpo del mensaje enviado
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_lst        LIKE seg_modulo.ruta_listados,
       r_ruta            LIKE seg_modulo.ruta_listados

   #Si se ha recibido parámetros se continua    
   IF(NUM_ARGS() > 0)THEN
      
      LET p_usuario_cod = ARG_VAL(1)
      LET g_pid         = ARG_VAL(2)
      LET g_proceso_cod = ARG_VAL(3)
      LET g_opera_cod   = ARG_VAL(4)
      LET p_folio       = ARG_VAL(5)
      LET p_nom_archivo = ARG_VAL(6)
      
            
      # Operacion de Preliquidacion
      LET g_opera_cod_preliquidacion = 3
      
      #Llamada a ejecución de procedimiento almacenado
      CALL fn_ejecuta_preliquidacion(p_folio,p_usuario_cod) RETURNING r_error

      IF(r_error = 0)THEN
         

         # actualiza folio a la operacion
         UPDATE bat_ctr_operacion
            SET folio = p_folio
          WHERE pid = g_pid
            AND proceso_cod = g_proceso_cod
            AND opera_cod = g_opera_cod_preliquidacion
            
         LET p_mensaje = "Preliquidación realizada con éxito"
         DISPLAY "\nFOLIO DE OPERACIÓN 28 PROCESADO: ",p_folio
         DISPLAY "TOTAL DE PAREJAS PROCESADAS: ",r_parejas_procesadas
         DISPLAY "TOTAL DE PAREJAS CON EXPEDIENTE: ",r_parejas_expediente
         DISPLAY "TOTAL DE PAREJAS CON DIFERENCIAS: ",r_parejas_diferencias
         DISPLAY "MONTO TOTAL POR MOVIMIENTOS DE CARGO SAR92: ",r_total_cargo_sar92
         DISPLAY "MONTO TOTAL POR MOVIMIENTOS DE ABONO SAR92: ",r_total_abono_sar92
         DISPLAY "MONTO TOTAL POR MOVIMIENTOS DE CARGO VIV97: ",r_total_cargo_viv92
         DISPLAY "MONTO TOTAL POR MOVIMIENTOS DE ABONO VIV97: ",r_total_abono_viv92

         CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta
         CALL fn_rutas("bat") RETURNING r_ruta, r_ruta_lst
         LET v_comando = "nohup fglrun ",r_ruta_ejecutable CLIPPED,"/SEPI33.42r "
                                        ,p_usuario_cod, " ",g_pid, " ",g_proceso_cod," "
                                        ,g_opera_cod," ",p_folio," '",p_nom_archivo,
                         "' 1>>", r_ruta_lst CLIPPED,
                         "/nohup:",g_pid USING "&&&&&",":",
                                   g_proceso_cod USING "&&&&&",":",
                                   g_opera_cod USING "&&&&&",
                         " 2>&1 &"
         RUN v_comando
         IF(STATUS)THEN
            DISPLAY "Ocurrió un error al ejecutar el reporte de la preliquidación"
         ELSE
            --DISPLAY "Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos"
         END IF
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod_preliquidacion)
                RETURNING r_reultado_opera
         IF(r_reultado_opera <> 0)THEN
            CALL fn_desplega_inc_operacion(r_reultado_opera)
         END IF

         -- verificar si no hay movimientos a liquidar
         -- si solo hay desmarcas ejecutarlas de una vez
         LET v_tot_liq = 0
         
         SELECT COUNT(*)
         INTO v_tot_liq 
         FROM sep_preliquida_op28 
         WHERE folio_liquida = p_folio
         
         IF v_tot_liq = 0 THEN 
             
            LET v_hay_desmarcas = 0 
                 
            SELECT COUNT(*) 
            INTO v_hay_desmarcas 
            FROM  sep_det_02_op28 
            WHERE folio = p_folio 
            AND   estado = 15 
            AND   ind_conciliar = 9 

            IF v_hay_desmarcas > 0 THEN 

                LET v_opera_cod_next = g_opera_cod + 1 
                LET v_programa = ""         
         
                LET v_txt = "EXECUTE FUNCTION fn_actualiza_opera_ini(?,?,?,?,?,?,?)"
                PREPARE qry_opera_ini FROM v_txt
                EXECUTE qry_opera_ini USING g_pid            ,
                                            g_proceso_cod    ,
                                            v_opera_cod_next ,
                                            p_folio          ,  
                                            v_programa       ,
                                            p_nom_archivo    ,
                                            p_usuario_cod
                                      INTO r_resp 
       
                LET v_txt = "EXECUTE FUNCTION fn_sep_liq_op28(?,?,?,?,?)"
                PREPARE qry_liq FROM v_txt 
                EXECUTE qry_liq USING p_usuario_cod ,
                                      g_pid ,
                                      g_proceso_cod ,
                                      v_opera_cod_next ,
                                      p_folio 
                                INTO  r_resp 

                LET v_txt = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
                PREPARE qry_opera_fin FROM v_txt 
                EXECUTE qry_opera_fin USING g_pid ,
                                            g_proceso_cod ,
                                            v_opera_cod_next
                                     INTO r_resp                
                                
             END IF
            END IF 
      ELSE
         # Si ocurrio un error se actualiza el estatus como erroneo
         CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_preliquidacion)  
                 RETURNING r_reultado_opera
         IF(r_reultado_opera <> 0)THEN
            CALL fn_desplega_inc_operacion(r_reultado_opera)
         END IF  
         LET p_mensaje = "El proceso de Preliquidación ha finalizado con errores."
      END IF
      # Envío de correo con estado de finalizacionde operacion
      LET p_titulo = "Finalización de operación - Op. 28 - Preliquidación"
      CALL fn_correo_proceso(g_pid, 
                             g_proceso_cod, 
                             g_opera_cod_preliquidacion, 
                             NULL, 
                             p_titulo,
                             p_mensaje)
   END IF
   
END MAIN

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPE33                                                     #
#Objetivo        => Ejecucion de SP de preliquidación de op 28                 #
#Fecha Inicio    => Junio 11, 2012                                             #
################################################################################
FUNCTION fn_ejecuta_preliquidacion(p_folio,p_usuario)
DEFINE p_folio     LIKE pag_cza_pag_patronal.folio_sua,
       p_usuario   LIKE seg_usuario.usuario_cod,
       v_consulta  STRING,
       v_sql_error SMALLINT,
       v_msn_error VARCHAR(40),
       v_error     BOOLEAN

   WHENEVER ERROR CONTINUE
   # inicializacion de flujo correcto
   LET v_error = 0 
   
   LET v_consulta = "EXECUTE FUNCTION fn_sep_preliquidar_op28(?)"
   PREPARE prp_ejecuta_preliquidacion FROM v_consulta
   EXECUTE prp_ejecuta_preliquidacion USING p_folio
                                       INTO v_sql_error,
                                            v_msn_error,
                                            r_parejas_procesadas,
                                            r_parejas_expediente,
                                            r_parejas_diferencias,
                                            r_total_cargo_sar92,
                                            r_total_abono_sar92,
                                            r_total_cargo_viv92,
                                            r_total_abono_viv92
   
   IF(v_sql_error <> 0)THEN
      LET v_error = 1  # Ocurrió error
      DISPLAY "\nError de ejecución en la preliquidación Op. 28 (Código): ",v_sql_error
      DISPLAY "Error de ejecución en la preliquidación Op. 28 (Mensaje):",v_msn_error,"\n"
   END IF
   RETURN v_error
END FUNCTION