--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/02/2015
--==============================================================================

################################################################################
#M¿½dulo          => PRT                                                        #
#Programa        => PRTL28                                                     #
#Objetivo        => Lanzador de solicitudes de traspaso                        #
#Fecha Inicio    => 07 Mayo 2015                                               #
################################################################################
SCHEMA "safre_viv"

GLOBALS "PRTWS02.inc"
GLOBALS "PRTG01.4gl"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ventana         ui.Window

MAIN
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)

   CONNECT TO "safre_tmp"
   CONNECT TO "safre_viv"
   SET CONNECTION "safre_viv"
   CALL fn_inicializa_consultas()
   CALL fn_consulta_solicitudes()
   DISCONNECT "safre_viv"
   DISCONNECT "safre_tmp"

END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   

END FUNCTION

# Descripción: 
FUNCTION fn_consulta_solicitudes()
DEFINE r_sol_pendientes DYNAMIC ARRAY OF RECORD
         v_id_prt_sol_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
         v_seleccion          SMALLINT,
         v_nss                LIKE prt_solicitud_cedente.nss,
         v_curp               LIKE prt_solicitud_cedente.curp,
         v_n_caso             LIKE prt_solicitud_cedente.n_caso,
         v_nombre             LIKE prt_solicitud_cedente.nombre,
         v_paterno            LIKE prt_solicitud_cedente.paterno,
         v_materno            LIKE prt_solicitud_cedente.materno,
         v_cve_diag           LIKE prt_traspaso_cedente.diag_procesar,
         v_desc_diag          LIKE prt_his_solicitud_cedente.valor_actual
       END RECORD,
       v_pid             LIKE glo_pid.pid,
       r_resultado_opera SMALLINT, # Bandera para ejecutar operación
       r_confirma        SMALLINT,
       r_error           BOOLEAN,
       v_bandera         BOOLEAN,
       v_indice          SMALLINT

   OPEN WINDOW vtna_consulta_sol WITH FORM v_ruta_ejecutable CLIPPED||"/PRTL281"

      INPUT ARRAY r_sol_pendientes FROM sr_sol_pendientes.* ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED, INSERT ROW=FALSE,DELETE ROW=FALSE,APPEND ROW=FALSE)

         BEFORE INPUT

            #Se asigna el titulo de la ventana
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo_ventana)
            END IF

            CALL r_sol_pendientes.clear()
            CALL fn_consulta_sol_pendientes() RETURNING r_sol_pendientes

            IF( r_sol_pendientes.getLength() = 0 )THEN
               CALL fn_mensaje("AVISO","No se encontraron solicitudes para reenvio","information")
               EXIT INPUT
            END IF
            DISPLAY r_sol_pendientes.getLength() TO total_solicitudes

            LET v_pid = 0

            #Se verifica si se puede iniciar la operacion      
            CALL fn_valida_operacion(v_pid,
                                     C_PROCESO_COD_REENVIO_CRM,
                                     C_OPERA_COD_REENVIA_CRM) RETURNING r_resultado_opera

            IF( r_resultado_opera <> 0 )THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               # Indica que ocurrió error
               EXIT INPUT
            END IF

         ON ACTION reenviar

            # valida que al menos un regsitro esté seleccionado
            LET v_bandera = FALSE
            FOR v_indice = 1 TO r_sol_pendientes.getLength()
               IF( r_sol_pendientes[v_indice].v_seleccion = TRUE )THEN
                  LET v_bandera = TRUE
                  EXIT FOR
               END IF
            END FOR
            IF( v_bandera = FALSE )THEN
               CALL fn_mensaje(p_titulo_ventana,"Al menos debe seleccionar un registro","information")
               CONTINUE INPUT
            END IF
            
            CALL fn_ventana_confirma("AVISO","¿Reenviar solicitudes?","about")RETURNING r_confirma
            IF( r_confirma )THEN
               CALL fn_reenvia_notif_pendientes(r_sol_pendientes) RETURNING r_error 
               IF( r_error )THEN
                  CONTINUE INPUT
               ELSE
                  CALL fn_mensaje(p_titulo_ventana,"Se ha iniciado la operación.\nProdrá revisar el detalle en el monitor de procesos","information")
                  ACCEPT INPUT
               END IF
            END IF

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_consulta_sol

END FUNCTION

# Descripción:
FUNCTION fn_consulta_sol_pendientes()
DEFINE v_sol_pendientes DYNAMIC ARRAY OF RECORD
         v_id_prt_sol_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
         v_seleccion          SMALLINT,
         v_nss                LIKE prt_solicitud_cedente.nss,
         v_curp               LIKE prt_solicitud_cedente.curp,
         v_n_caso             LIKE prt_solicitud_cedente.n_caso,
         v_nombre             LIKE prt_solicitud_cedente.nombre,
         v_paterno            LIKE prt_solicitud_cedente.paterno,
         v_materno            LIKE prt_solicitud_cedente.materno,
         v_cve_diag           LIKE prt_traspaso_cedente.diag_procesar,
         v_desc_diag          LIKE prt_his_solicitud_cedente.valor_actual
       END RECORD,
       v_indice INTEGER,
       v_consulta STRING

   LET v_indice = 1

   LET v_consulta = "SELECT ren.id_solicitud,",
                    "       1,",
                    "       sol.nss,",
                    "       sol.curp,",
                    "       sol.n_caso,",
                    "       sol.nombre,",
                    "       sol.paterno,",
                    "       sol.materno,",
                    "       ren.diagnostico_sol,",
                    "       dia.descripcion_general",
                    "  FROM prt_reenvios_solicitud ren JOIN prt_solicitud_cedente sol",
                    "    ON ren.id_solicitud = sol.id_prt_solicitud_cedente",
                    "       LEFT OUTER JOIN prt_diagnostico dia",
                    "    ON dia.diagnostico_externo = ren.diagnostico_sol",
                    "   AND dia.destino_diagnostico = ?",
                    " WHERE ren.estado_envio = ?",
                    "   AND ren.tipo_solicitud = ?"

   PREPARE prp_recupera_registros FROM v_consulta
   DECLARE cur_recupera_registros CURSOR FOR prp_recupera_registros
   FOREACH cur_recupera_registros USING C_DESTINO_DIAG_P_I,
                                        C_REENVIO_NO_ENVIADO,
                                        C_FLUJO_CEDENTE
                                   INTO v_sol_pendientes[v_indice].*

      LET v_indice = v_indice + 1

   END FOREACH

   FREE cur_recupera_registros
   
   IF( v_sol_pendientes.getLength() > 0 )THEN
      IF( v_sol_pendientes[v_indice].v_nss IS NULL )THEN
         CALL v_sol_pendientes.deleteElement(v_sol_pendientes.getLength())
      END IF
   END IF
   
   RETURN v_sol_pendientes
END FUNCTION

# Descripción: función para lanzar batch de reenvio de solicitudes
FUNCTION fn_reenvia_notif_pendientes(p_sol_pendientes)
DEFINE p_sol_pendientes DYNAMIC ARRAY OF RECORD
         v_id_prt_sol_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
         v_seleccion          SMALLINT,
         v_nss                LIKE prt_solicitud_cedente.nss,
         v_curp               LIKE prt_solicitud_cedente.curp,
         v_n_caso             LIKE prt_solicitud_cedente.n_caso,
         v_nombre             LIKE prt_solicitud_cedente.nombre,
         v_paterno            LIKE prt_solicitud_cedente.paterno,
         v_materno            LIKE prt_solicitud_cedente.materno,
         v_cve_diag           LIKE prt_traspaso_cedente.diag_procesar,
         v_desc_diag          LIKE prt_his_solicitud_cedente.valor_actual
       END RECORD,
       v_pid             LIKE glo_pid.pid,
       r_resultado_opera SMALLINT, # Bandera para ejecutar operación
       v_bnd_error       BOOLEAN,
       v_comando         STRING,
       v_proceso_txt     STRING,
       v_opera_txt       STRING,
       v_pid_txt         STRING

   LET v_pid = 0
   LET v_bnd_error = FALSE
   CALL fn_valida_operacion(v_pid,
                            C_PROCESO_COD_REENVIO_CRM,
                            C_OPERA_COD_REENVIA_CRM) RETURNING r_resultado_opera
     
   IF( r_resultado_opera <> 0 )THEN
      LET v_bnd_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # Indica que ocurrió error
      RETURN v_bnd_error
   END IF

   CALL fn_genera_pid(C_PROCESO_COD_REENVIO_CRM,
                      C_OPERA_COD_REENVIA_CRM,
                      p_usuario_cod) RETURNING v_pid

   CALL fn_inicializa_proceso(v_pid,
                              C_PROCESO_COD_REENVIO_CRM,
                              C_OPERA_COD_REENVIA_CRM,
                              0,
                              "PRTL28",
                              "NA",
                              p_usuario_cod) RETURNING r_resultado_opera
   IF( r_resultado_opera = 0 )THEN
      CALL fn_actualiza_opera_ini(v_pid,
                                  C_PROCESO_COD_REENVIO_CRM,
                                  C_OPERA_COD_REENVIA_CRM,
                                  0,
                                  "PRTL28",
                                  "NA",
                                  p_usuario_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera = 0 )THEN
         
         CALL fn_registra_no_reenvio(p_sol_pendientes)
         
         
         LET v_proceso_txt = C_PROCESO_COD_REENVIO_CRM 
         LET v_opera_txt   = C_OPERA_COD_REENVIA_CRM
         LET v_pid_txt     = v_pid
         LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PRTP28.42r '",p_usuario_cod CLIPPED, "' ",
                                                                                   v_pid CLIPPED," ",
                                                                                   C_PROCESO_COD_REENVIO_CRM," ",
                                                                                   C_OPERA_COD_REENVIA_CRM," ",
                                                                                   "0 ",
                                                                                   "'NA' ",
                                                                                   "1 ",
                         " 1>", v_ruta_listados CLIPPED,"/nohup:",v_pid_txt USING "&&&&&",":",
                                                                  v_proceso_txt USING "&&&&&",":",
                                                                  v_opera_txt USING "&&&&&"," 2>&1 &" 

         --DISPLAY v_comando 
         RUN v_comando
   
         IF(STATUS)THEN
            LET v_bnd_error = TRUE
            CALL fn_mensaje(p_titulo_ventana,"Ocurrio un error al ejecutar la integración","information")
         END IF         
      ELSE
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         LET v_bnd_error = TRUE
      END IF
   ELSE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      LET v_bnd_error = TRUE
   END IF

   RETURN v_bnd_error
END FUNCTION

# Descripción: Función para registrar las solicitudes que no se reenviarón
FUNCTION fn_registra_no_reenvio(p_sol_pendientes)
DEFINE p_sol_pendientes DYNAMIC ARRAY OF RECORD
         v_id_prt_sol_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
         v_seleccion          SMALLINT,
         v_nss                LIKE prt_solicitud_cedente.nss,
         v_curp               LIKE prt_solicitud_cedente.curp,
         v_n_caso             LIKE prt_solicitud_cedente.n_caso,
         v_nombre             LIKE prt_solicitud_cedente.nombre,
         v_paterno            LIKE prt_solicitud_cedente.paterno,
         v_materno            LIKE prt_solicitud_cedente.materno,
         v_cve_diag           LIKE prt_traspaso_cedente.diag_procesar,
         v_desc_diag          LIKE prt_his_solicitud_cedente.valor_actual
       END RECORD,
       v_indice   SMALLINT,
       v_consulta STRING

   
   TRY
      SET CONNECTION "safre_tmp"
      LET v_consulta = " DROP TABLE IF EXISTS prt_tmp_reg_no_reenvia"
      PREPARE prp_elimina_tmp_no_reenvio FROM v_consulta

      # Tabla para registrar las solicitudes que no se reenviarón
      LET v_consulta = "CREATE TABLE prt_tmp_reg_no_reenvia(",
                       " id_solicitud DECIMAL(9,0))"
      PREPARE prp_crea_tmp_no_reenvio FROM v_consulta

      EXECUTE prp_elimina_tmp_no_reenvio

      EXECUTE prp_crea_tmp_no_reenvio
      
      LET v_consulta = " INSERT INTO prt_tmp_reg_no_reenvia(id_solicitud) VALUES(?)"
      PREPARE prp_ins_tmp_no_reenvio FROM v_consulta
      
      FOR v_indice = 1 TO p_sol_pendientes.getLength()
         # si no esta seleccionado se ingresa en la tabla para descartar los que no se van a reenviar
         IF NOT( p_sol_pendientes[v_indice].v_seleccion )THEN
            EXECUTE prp_ins_tmp_no_reenvio USING p_sol_pendientes[v_indice].v_id_prt_sol_cedente
         END IF
      END FOR
      SET CONNECTION "safre_viv"
      
   CATCH
      DISPLAY "Ocurrión un error"
      DISPLAY "Código error:"||SQLCA.sqlcode
      DISPLAY "Descripción error:"||SQLCA.sqlerrm
      SET CONNECTION "safre_viv"
   END TRY
   
   
END FUNCTION