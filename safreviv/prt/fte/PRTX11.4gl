--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/02/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC11                                                     #
#Objetivo        => Programa para la administración de la marca de portabilidad#
#                   cedente. Considera el reenvio de marca, traspaso de saldos #
#                   y desmarca de la cuenta de manera automatizada via         #
#                   control de procesos batch                                  #
#Fecha Inicio    => 27 Junio 2017                                              #
################################################################################

GLOBALS "PRTWS02.inc"     

DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod        ,
       p_pid             LIKE bat_ctr_proceso.pid            ,
       p_proceso_cod     LIKE bat_ctr_proceso.proceso_cod    ,
       p_opera_cod       LIKE bat_ctr_operacion.opera_cod    ,
       p_folio           LIKE glo_ctr_archivo.folio          ,
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo ,
       v_ruta_rescate    LIKE seg_modulo.ruta_rescate        ,
       v_ruta_envio      LIKE seg_modulo.ruta_envio          ,
       r_resultado_opera SMALLINT,
              
       g_ruta_ejecutable LIKE seg_modulo.ruta_bin

DEFINE g_resultado_maq RECORD
          v_ind            SMALLINT,
          v_diag           CHAR(3),
          v_error_sql      INTEGER,
          v_error_isam     INTEGER,
          v_msg_sql        VARCHAR(254),
          v_estado_destino SMALLINT
       END RECORD
       
MAIN
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nom_archivo    = ARG_VAL(6)

   
   CALL fn_inicializa_ambiente()
   
   CALL fn_busqueda_cuenta()

   CALL fn_actualiza_opera_fin(p_pid,
                               p_proceso_cod,
                               p_opera_cod) RETURNING r_resultado_opera
   # si ocurri? un error con la actualizacion de la operacion operacion
   # muestra el mensaje
   IF( r_resultado_opera )THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
   ELSE
      # Env?o de correo de notificaci?n de proceso finalizado
      CALL fn_correo_proceso(p_pid,
                             p_proceso_cod,
                             p_opera_cod,
                             '',
                             'GENERACIÓN INFORME REENVIOS PORTABILIDAD',
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||DATE||
                             'Fecha Fin    : '||DATE
                             )
   END IF

END MAIN

# Descripción:
PRIVATE FUNCTION fn_inicializa_ambiente()
DEFINE v_consulta STRING

   SELECT ruta_bin
     INTO g_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_consulta = " SELECT MAX(id_prt_solicitud_cedente) id_prt_solicitud_cedente, ",
                    "        nss,",
                    "        curp,",
                    "        nombre,",
                    "        paterno,",
                    "        materno",
                    "   FROM prt_solicitud_cedente ",
                    "  WHERE estado = ? ",
                    "  GROUP BY 2,3,4,5,6 ",
                    "  ORDER BY id_prt_solicitud_cedente "

   PREPARE prp_verifica_registro FROM v_consulta
   DECLARE cur_verifica_registro CURSOR FOR prp_verifica_registro

   LET v_consulta = " SELECT diagnostico_externo,",
                    "        descripcion_general",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_interno = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_consulta_diag_externo FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 seq_prt_solicitud_cedente.NEXTVAL",
                    "   FROM systables"
   PREPARE prp_rec_seq_sol FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 estado",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_rec_edo_sol FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET diagnostico_interno = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_solicitud FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET tipo_portabilidad = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_act_tpo_sol_env_marca FROM v_consulta

   LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET folio_desmarca = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_act_fol_desmarca FROM v_consulta
   
   {LET v_consulta = " UPDATE prt_solicitud_cedente",
                    "    SET id_prt_solicitud_cedente = ?",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_id_solicitud FROM v_consulta}

   LET v_consulta = " SELECT a.id_prt_traspaso_cedente " ,
                    " FROM   prt_traspaso_cedente a ",
                    " WHERE  a.id_prt_solicitud_cedente = ? "
   PREPARE prp_obtiene_id_traspaso_ced FROM v_consulta

   LET v_consulta = " DELETE ",
                    "   FROM prt_traspaso_cedente",
                    "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_elimina_traspaso FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_prt_solicita_marca_cedente(?,?,?,?)"
   PREPARE prp_solicita_marca_cedente FROM v_consulta

   LET v_consulta = "EXECUTE PROCEDURE sp_prt_solicita_traspaso_cedente_reenv(?,?,?,?,?)"
   PREPARE prp_solicita_traspaso FROM v_consulta

   LET v_consulta = "EXECUTE PROCEDURE sp_prt_error_bus(?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_error_bus FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_glo_maq_individual(?,?,?,?)"
   PREPARE prp_avanza_maquinaria FROM v_consulta
   
END FUNCTION

# Descripción:
PRIVATE FUNCTION fn_busqueda_cuenta()
DEFINE v_nss_busqueda LIKE prt_solicitud_cedente.nss
       
               CALL fn_administra_solicitudes(v_nss_busqueda) 
               
END FUNCTION

# Descripción:
PRIVATE FUNCTION fn_administra_solicitudes(p_nss)

DEFINE p_nss LIKE prt_solicitud_cedente.nss,
       v_tot_reenv_marca    INTEGER,
       v_tot_reenv_saldo    INTEGER,
       v_tot_reenv_desmarca INTEGER

         # Valida estado para envío de marca
         DISPLAY "================================================================="         
         DISPLAY "REENVIO SOLICITUDES DE MARCA DE PORTABILIDAD"
         CALL fn_recupera_registro(C_ESTADO_MARCA_SOL_PRO)
         RETURNING v_tot_reenv_marca
         IF v_tot_reenv_marca = 0 THEN
             DISPLAY "=> No se encontraron registros para reenvío de solicitud de marca"
             DISPLAY "================================================================="         
         ELSE 
             DISPLAY "Se reenviaron ",v_tot_reenv_marca," solicitudes de marca"
             DISPLAY "================================================================="         
         END IF 

         DISPLAY "REENVIO SOLICITUDES DE TRANSFERENCIA DE SALDO PORTABILIDAD"
         # Valida estado para solicitud de saldos
         --CALL fn_recupera_registro(C_ESTADO_SDO_SOLICITADO_PRO)
         CALL fn_recupera_registro(44) -- transferencia solicitada sin folio_procesar 
         RETURNING v_tot_reenv_saldo
         IF v_tot_reenv_saldo = 0 THEN
             DISPLAY "No se encontraron solicitudes para reenvio de solicitud de saldo"
             DISPLAY "================================================================="         
         ELSE 
             DISPLAY "Se reenviaron ",v_tot_reenv_saldo," solicitudes de saldo"
             DISPLAY "================================================================="         
         END IF 

         DISPLAY "REENVIO SOLICITUDES DE DESMARCA PORTABILIDAD"
         # Valida estado para solicitud de desmarca (reenvio desmarca)
         CALL fn_recupera_registro(C_ESTADO_DESMARCA_SOL_PRO)
         RETURNING v_tot_reenv_desmarca
         IF v_tot_reenv_desmarca = 0 THEN
         
             DISPLAY "No se encontraron solicitudes para reenvio de solicitud de desmarca"
             DISPLAY "================================================================="         
         ELSE 
             DISPLAY "Se reenviaron ",v_tot_reenv_desmarca," solicitudes de desmarca"
             DISPLAY "================================================================="         
         END IF 
         
END FUNCTION

# Descripción:
PRIVATE FUNCTION fn_recupera_registro(p_estado)

DEFINE v_nss     LIKE prt_solicitud_cedente.nss,
       p_estado  LIKE prt_solicitud_cedente.estado,
       v_bnd_existe_reg INTEGER,
       v_id_prt_solicitud_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_curp    LIKE prt_solicitud_cedente.curp,
       v_nombre  LIKE prt_solicitud_cedente.nombre,
       v_paterno LIKE prt_solicitud_cedente.paterno,
       v_materno LIKE prt_solicitud_cedente.materno,
       v_bnd_reenv_marca    BOOLEAN,
       v_bnd_reenv_saldo    BOOLEAN,
       v_bnd_reenv_desmarca BOOLEAN,
       r_bnd_envio          BOOLEAN

   INITIALIZE v_nss ,
              v_id_prt_solicitud_cedente,
              v_curp,
              v_nombre,
              v_paterno,
              v_materno TO NULL
              
   LET v_bnd_existe_reg     = 0
   LET v_bnd_reenv_marca    = 0
   LET v_bnd_reenv_saldo    = 0
   LET v_bnd_reenv_desmarca = 0
   
   CASE p_estado
        WHEN C_ESTADO_MARCA_SOL_PRO
             LET v_bnd_reenv_marca = 1        
        EXIT CASE
        --WHEN C_ESTADO_SDO_SOLICITADO_PRO        
        WHEN 44 --traspaso solicitado sin folio procesar
             LET v_bnd_reenv_saldo = 1        
        EXIT CASE
        WHEN C_ESTADO_DESMARCA_SOL_PRO
             LET v_bnd_reenv_desmarca = 1
        EXIT CASE
   END CASE

   FOREACH cur_verifica_registro USING p_estado
                                  INTO v_id_prt_solicitud_cedente,
                                       v_nss,
                                       v_curp,
                                       v_nombre,
                                       v_paterno,
                                       v_materno
                                       
           LET v_bnd_existe_reg = v_bnd_existe_reg + 1

           CALL fn_ejecuta_solicitud(v_bnd_reenv_marca,
                                     v_bnd_reenv_saldo,
                                     v_bnd_reenv_desmarca,
                                     v_id_prt_solicitud_cedente) RETURNING r_bnd_envio
                                     
         IF( r_bnd_envio )THEN
             DISPLAY "     Solicitud reenviada : ",v_nss," ",v_curp," ",v_paterno," ",v_materno," ",v_nombre
         ELSE
             DISPLAY "     Excepción de reenvío: ",v_nss," ",v_curp," ",v_paterno," ",v_materno," ",v_nombre
         END IF
   END FOREACH
   
RETURN v_bnd_existe_reg   

END FUNCTION

# Descripción: 
FUNCTION fn_ejecuta_solicitud(p_bnd_reenv_marca,
                              p_bnd_reenv_saldo,
                              p_bnd_reenv_desmarca,
                              p_id_prt_solicitud_cedente)
                              
DEFINE p_bnd_reenv_marca BOOLEAN,
       p_bnd_reenv_saldo BOOLEAN,
       p_bnd_reenv_desmarca BOOLEAN,
       p_id_prt_solicitud_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       p_id_prt_traspaso_cedente LIKE prt_traspaso_cedente.id_prt_traspaso_cedente,
       v_nvo_id_prt_solicitud_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_bnd_envio  BOOLEAN,
       v_f_actual   DATETIME YEAR TO SECOND,
       v_estado SMALLINT,
       v_errores RECORD
          v_sql_error  INTEGER,
          v_isam_error INTEGER,
          v_msg_error  CHAR(254),
          v_ind        CHAR(5),
          v_diag       CHAR(254)
       END RECORD,
       r_ind        INTEGER,
       r_diag       CHAR(3),
       r_error_sql  INTEGER,
       r_error_isam INTEGER,
       r_msg_sql    CHAR(254),
       r_estado_destino SMALLINT

CONSTANT c_tipo_solicitud_desmarca SMALLINT = 3
   # Inicializa bandera de envío
   LET v_bnd_envio = FALSE
   
   CASE
      WHEN p_bnd_reenv_marca
         # Genera nuevo identificador y actualiza en portabilidad, para que PROCESAR no rechace por "Folio PROCESAR" repetido 
         EXECUTE prp_rec_seq_sol INTO v_nvo_id_prt_solicitud_cedente
         {EXECUTE prp_actualiza_id_solicitud USING v_nvo_id_prt_solicitud_cedente, 
                                                  p_id_prt_solicitud_cedente}
         
         EXECUTE prp_solicita_marca_cedente USING p_id_prt_solicitud_cedente,
                                                  C_BUS_PROCESO_COD_SOL_MARCA,
                                                  C_BUS_OPERACION_COD_SOL_MARCA,
                                                  v_nvo_id_prt_solicitud_cedente
                                             INTO r_error_sql,
                                                  r_error_isam,
                                                  r_msg_sql
         IF(r_error_sql <> 0)THEN
            DISPLAY "Error al solicitar marca:"
            DISPLAY "ID prt solicitud: ",p_id_prt_solicitud_cedente
            DISPLAY "Código: ",r_error_sql
            DISPLAY "Mensaje: ",r_msg_sql
            
            LET v_bnd_envio = FALSE
         ELSE
            LET v_bnd_envio = TRUE
         END IF

      WHEN p_bnd_reenv_saldo
         # Obtiene el id del traspaso actual      
         EXECUTE prp_obtiene_id_traspaso_ced USING p_id_prt_solicitud_cedente
                                             INTO  p_id_prt_traspaso_cedente
         # Elimina la solicitud de saldo anterior para ingresar la nueva solicitud
         EXECUTE prp_elimina_traspaso USING p_id_prt_solicitud_cedente
         EXECUTE prp_solicita_traspaso USING p_id_prt_solicitud_cedente,
                                             C_BUS_PROCESO_COD_SOL_TRASP,
                                             C_BUS_OPERACION_COD_SOL_TRASP,
                                             p_usuario_cod,
                                             p_id_prt_traspaso_cedente
                                        INTO r_ind,
                                             r_diag,
                                             r_error_sql,
                                             r_error_isam,
                                             r_msg_sql,
                                             r_estado_destino
   
         IF( r_error_sql <> 0 AND r_error_sql <> 100 )THEN
            DISPLAY "Error al solicitar traspaso:"
            DISPLAY "ID prt solicitud: ",p_id_prt_solicitud_cedente
            DISPLAY "Código: ",r_error_sql
            DISPLAY "Mensaje: ",r_msg_sql

            EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_1,
                                                  p_id_prt_solicitud_cedente
            LET v_errores.v_sql_error  = r_error_sql
            LET v_errores.v_isam_error = r_error_isam
            LET v_errores.v_msg_error  = r_msg_sql
            LET v_errores.v_ind        = r_ind
            LET v_errores.v_diag       = "Error al solicitar traspaso cedente "||p_id_prt_solicitud_cedente

            {LET v_f_actual = CURRENT YEAR TO SECOND
            EXECUTE prp_error_bus USING p_id_bus_solicitud_tramite,
                                        C_BUS_PROCESO_COD_SOL_TRASP,
                                        C_BUS_OPERACION_COD_SOL_TRASP,
                                        p_usuario_cod,
                                        v_f_actual,
                                        v_errores.v_sql_error,
                                        v_errores.v_isam_error,
                                        v_errores.v_msg_error,
                                        "PRTC11",
                                        v_errores.v_ind,
                                        v_errores.v_diag}
            LET v_bnd_envio = FALSE
         ELSE
            LET v_bnd_envio = TRUE   
         END IF

      WHEN p_bnd_reenv_desmarca
      
         # Actualiza el folio de desmarca
         
         # Genera nuevo identificador y actualiza en portabilidad, para que PROCESAR no rechace por "Folio PROCESAR" repetido 
         EXECUTE prp_rec_seq_sol INTO v_nvo_id_prt_solicitud_cedente
         EXECUTE prp_act_tpo_sol_env_marca USING c_tipo_solicitud_desmarca,
                                                 p_id_prt_solicitud_cedente
         
         EXECUTE prp_solicita_marca_cedente USING p_id_prt_solicitud_cedente,
                                                  C_BUS_PROCESO_COD_SOL_MARCA,
                                                  C_BUS_OPERACION_COD_SOL_MARCA,
                                                  v_nvo_id_prt_solicitud_cedente
                                             INTO r_error_sql,
                                                  r_error_isam,
                                                  r_msg_sql
         IF(r_error_sql <> 0)THEN
            DISPLAY "Error al solicitar marca:"
            DISPLAY "ID prt solicitud: ",p_id_prt_solicitud_cedente
            DISPLAY "Código: ",r_error_sql
            DISPLAY "Mensaje: ",r_msg_sql
            
            LET v_bnd_envio = FALSE
         ELSE
            EXECUTE prp_rec_edo_sol USING p_id_prt_solicitud_cedente
                                     INTO v_estado

            IF( v_estado = C_ESTADO_SDO_LIQUIDADO_CED )THEN
               EXECUTE prp_avanza_maquinaria USING C_ID_MAQUINARIA_SOL_CED,
                                                   p_id_prt_solicitud_cedente,
                                                   C_SENAL_SOLICITA_DESMARCA,
                                                   p_usuario_cod
                                              INTO g_resultado_maq.*
            
            END IF
            LET v_bnd_envio = TRUE
         END IF
   END CASE
   RETURN v_bnd_envio
END FUNCTION