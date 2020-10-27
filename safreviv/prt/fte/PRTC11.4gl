--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/02/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC11                                                     #
#Objetivo        => Programa para la administración de la marca de portabilidad#
#                   cedente. Considera el reenvio de marca, traspaso de saldos #
#                   y desmarca de la cuenta                                    #
#Fecha Inicio    => 27 Junio 2017                                              #
################################################################################

GLOBALS "PRTWS02.inc"

DATABASE safre_viv

DEFINE g_usuario_cod     LIKE seg_usuario.usuario_cod,
       g_tpo_ejecucion   SMALLINT,
       g_titulo_ventana  STRING,
       g_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_ventana         ui.Window,
       g_forma           ui.Form

DEFINE g_resultado_maq RECORD
          v_ind            SMALLINT,
          v_diag           CHAR(3),
          v_error_sql      INTEGER,
          v_error_isam     INTEGER,
          v_msg_sql        VARCHAR(254),
          v_estado_destino SMALLINT
       END RECORD
       
MAIN
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_tpo_ejecucion  = ARG_VAL(2)
   LET g_titulo_ventana = ARG_VAL(3)

   CALL fn_inicializa_ambiente()
   CALL fn_busqueda_cuenta()

END MAIN

# Descripción:
PRIVATE FUNCTION fn_inicializa_ambiente()
DEFINE v_consulta STRING

   SELECT ruta_bin
     INTO g_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_consulta = " SELECT FIRST 1 id_prt_solicitud_cedente,",
                    "        curp,",
                    "        nombre,",
                    "        paterno,",
                    "        materno",
                    "   FROM prt_solicitud_cedente",
                    "  WHERE nss = ?",
                    "    AND estado = ?",
                    "  ORDER BY id_prt_solicitud_cedente DESC"
   PREPARE prp_verifica_registro FROM v_consulta

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
DEFINE v_nss_busqueda LIKE prt_solicitud_cedente.nss,
       v_bnd_continua BOOLEAN

   OPEN WINDOW vtna_busqueda WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC111"

      INPUT v_nss_busqueda FROM filtro_nss ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE, CANCEL=FALSE)

         BEFORE INPUT
            LET g_ventana = ui.Window.getCurrent()
            LET g_forma = g_ventana.getForm()
            IF( g_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(g_titulo_ventana)
               CALL g_ventana.setText(g_titulo_ventana)
            END IF
            CALL g_forma.setElementHidden("gpo_gestion_marcas",TRUE) 
            INITIALIZE v_nss_busqueda TO NULL

         ON ACTION aceptar
            IF(LENGTH(v_nss_busqueda CLIPPED) > 0)THEN
               CALL g_forma.setElementHidden("gpo_gestion_marcas",FALSE) 
               CALL fn_administra_solicitudes(v_nss_busqueda) RETURNING v_bnd_continua
               IF NOT( v_bnd_continua )THEN
                  ACCEPT INPUT
               END IF
            ELSE
               CALL fn_mensaje(g_titulo_ventana,"Capture NSS","information")
               CONTINUE INPUT
            END IF
            CALL g_forma.setElementHidden("gpo_gestion_marcas",TRUE) 
            CONTINUE INPUT

         ON ACTION CLOSE
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_busqueda

END FUNCTION

# Descripción:
PRIVATE FUNCTION fn_administra_solicitudes(p_nss)
DEFINE p_nss LIKE prt_solicitud_cedente.nss,
       v_bnd_reenv_marca BOOLEAN,
       v_bnd_reenv_saldo BOOLEAN,
       v_bnd_reenv_desmarca BOOLEAN,
       v_bnd_continua BOOLEAN,
       v_folio_fovissste VARCHAR(255),
       r_confirma BOOLEAN,
       r_id_prt_solicitud_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       r_curp    LIKE prt_solicitud_cedente.curp,
       r_nombre  LIKE prt_solicitud_cedente.nombre,
       r_paterno LIKE prt_solicitud_cedente.paterno,
       r_materno LIKE prt_solicitud_cedente.materno,
       r_bnd_envio  BOOLEAN

   INPUT v_bnd_reenv_marca,
         v_bnd_reenv_saldo,
         v_bnd_reenv_desmarca,
         v_folio_fovissste
    FROM marca,
         traspaso,
         desmarca,
         oficio_fovissste ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE, CANCEL=FALSE)

      BEFORE INPUT
         CALL DIALOG.setFieldActive("traspaso",TRUE)
         CALL DIALOG.setFieldActive("marca",TRUE)
         CALL DIALOG.setFieldActive("desmarca",TRUE)
         CALL g_forma.setFieldHidden("oficio_fovissste",TRUE)
         CALL g_forma.setElementHidden("lbl_oficio",TRUE) 
         LET v_bnd_continua    = FALSE
         LET v_bnd_reenv_marca = FALSE
         LET v_bnd_reenv_saldo = FALSE
         LET v_bnd_reenv_desmarca = FALSE
         # Valida estado para envío de marca
         CALL fn_recupera_registro(p_nss,C_ESTADO_MARCA_SOL_PRO)
         RETURNING v_bnd_reenv_marca,
                   r_id_prt_solicitud_cedente,
                   r_curp,
                   r_nombre,
                   r_paterno,
                   r_materno
         
         IF( v_bnd_reenv_marca )THEN
            CALL DIALOG.setFieldActive("traspaso",FALSE)
            CALL DIALOG.setFieldActive("desmarca",FALSE)
         ELSE
            # Valida estado para solicitud de saldos
            CALL fn_recupera_registro(p_nss,C_ESTADO_SDO_SOLICITADO_PRO)
            RETURNING v_bnd_reenv_saldo,
                      r_id_prt_solicitud_cedente,
                      r_curp,
                      r_nombre,
                      r_paterno,
                      r_materno
            IF( v_bnd_reenv_saldo )THEN
               CALL DIALOG.setFieldActive("marca",FALSE)
               CALL DIALOG.setFieldActive("desmarca",FALSE)
            ELSE
               # Valida estado para solicitud de desmarca (liquidado)
               CALL fn_recupera_registro(p_nss,C_ESTADO_SDO_LIQUIDADO_CED)
               RETURNING v_bnd_reenv_desmarca,
                         r_id_prt_solicitud_cedente,
                         r_curp,
                         r_nombre,
                         r_paterno,
                         r_materno
               IF( v_bnd_reenv_desmarca )THEN
                  CALL DIALOG.setFieldActive("marca",FALSE)
                  CALL DIALOG.setFieldActive("traspaso",FALSE)
                  CALL g_forma.setFieldHidden("oficio_fovissste",FALSE)
                  CALL g_forma.setElementHidden("lbl_oficio",FALSE) 
               ELSE
                  # Valida estado para solicitud de desmarca (reenvio desmarca)
                  CALL fn_recupera_registro(p_nss,C_ESTADO_DESMARCA_SOL_PRO)
                  RETURNING v_bnd_reenv_desmarca,
                            r_id_prt_solicitud_cedente,
                            r_curp,
                            r_nombre,
                            r_paterno,
                            r_materno
                  IF( v_bnd_reenv_desmarca )THEN
                     CALL DIALOG.setFieldActive("marca",FALSE)
                     CALL DIALOG.setFieldActive("traspaso",FALSE)
                     CALL g_forma.setFieldHidden("oficio_fovissste",FALSE)
                     CALL g_forma.setElementHidden("lbl_oficio",FALSE) 
                  ELSE
                     CALL fn_mensaje(g_titulo_ventana,"No se encontraron registros con criterio dado","information")
                     LET v_bnd_continua = TRUE
                     ACCEPT INPUT
                  END IF
               END IF                         
            END IF
         END IF
         DISPLAY r_curp TO curp
         DISPLAY r_nombre TO nombre
         DISPLAY r_paterno TO paterno
         DISPLAY r_materno TO materno

      ON ACTION aceptar
         # Validaciones
         IF NOT (v_bnd_reenv_marca OR v_bnd_reenv_saldo OR v_bnd_reenv_desmarca)THEN
            CALL fn_mensaje(g_titulo_ventana,"Seleccione operación","information")
            CONTINUE INPUT
         END IF
         IF( v_bnd_reenv_desmarca AND v_folio_fovissste CLIPPED IS NULL)THEN
            CALL fn_mensaje(g_titulo_ventana,"Capture oficio FOVISSSTE","information")
            NEXT FIELD oficio_fovissste
         END IF
         
         CALL fn_ventana_confirma("Confimar","Se ejecutará la operación, ¿Desea continuar?","information") RETURNING r_confirma
         IF( r_confirma )THEN
            CALL fn_ejecuta_solicitud(v_bnd_reenv_marca,
                                      v_bnd_reenv_saldo,
                                      v_bnd_reenv_desmarca,
                                      v_folio_fovissste,
                                      r_id_prt_solicitud_cedente) RETURNING r_bnd_envio
            IF( r_bnd_envio )THEN
               CALL fn_mensaje(g_titulo_ventana,"Se ha envido la solicitud correctamente","information")
               DISPLAY "" TO curp
               DISPLAY "" TO nombre
               DISPLAY "" TO paterno
               DISPLAY "" TO materno
               LET v_bnd_continua = TRUE
               ACCEPT INPUT
            ELSE
               CALL fn_mensaje(g_titulo_ventana,"Ocurrió un error al enviar la solicitud","information")
               DISPLAY "" TO curp
               DISPLAY "" TO nombre
               DISPLAY "" TO paterno
               DISPLAY "" TO materno
            END IF
            
         END IF
         CONTINUE INPUT

      ON ACTION regresar
         DISPLAY "" TO curp
         DISPLAY "" TO nombre
         DISPLAY "" TO paterno
         DISPLAY "" TO materno
         LET v_bnd_continua = TRUE
         EXIT INPUT

   END INPUT

   RETURN v_bnd_continua
END FUNCTION

# Descripción:
PRIVATE FUNCTION fn_recupera_registro(p_nss, p_estado)
DEFINE p_nss     LIKE prt_solicitud_cedente.nss,
       p_estado  LIKE prt_solicitud_cedente.estado,
       v_bnd_existe_reg BOOLEAN,
       v_id_prt_solicitud_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       v_curp    LIKE prt_solicitud_cedente.curp,
       v_nombre  LIKE prt_solicitud_cedente.nombre,
       v_paterno LIKE prt_solicitud_cedente.paterno,
       v_materno LIKE prt_solicitud_cedente.materno

   INITIALIZE v_id_prt_solicitud_cedente,
              v_curp,
              v_nombre,
              v_paterno,
              v_materno TO NULL
                                       
   EXECUTE prp_verifica_registro USING p_nss,
                                       p_estado
                                  INTO v_id_prt_solicitud_cedente,
                                       v_curp,
                                       v_nombre,
                                       v_paterno,
                                       v_materno

   IF( v_id_prt_solicitud_cedente IS NOT NULL )THEN
      LET v_bnd_existe_reg = TRUE
   ELSE
      LET v_bnd_existe_reg = FALSE
   END IF

   RETURN v_bnd_existe_reg,
          v_id_prt_solicitud_cedente,
          v_curp,
          v_nombre,
          v_paterno,
          v_materno
END FUNCTION

# Descripción: 
FUNCTION fn_ejecuta_solicitud(p_bnd_reenv_marca,
                              p_bnd_reenv_saldo,
                              p_bnd_reenv_desmarca,
                              p_folio_fovissste,
                              p_id_prt_solicitud_cedente)
DEFINE p_bnd_reenv_marca BOOLEAN,
       p_bnd_reenv_saldo BOOLEAN,
       p_bnd_reenv_desmarca BOOLEAN,
       p_folio_fovissste VARCHAR(255),
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
         # Elimina la solicitud de saldo anterior para ingresar la nueva solicitud
         EXECUTE prp_obtiene_id_traspaso_ced USING p_id_prt_solicitud_cedente 
                                             INTO  p_id_prt_traspaso_cedente        
         EXECUTE prp_elimina_traspaso USING p_id_prt_solicitud_cedente
         EXECUTE prp_solicita_traspaso USING p_id_prt_solicitud_cedente,
                                             C_BUS_PROCESO_COD_SOL_TRASP,
                                             C_BUS_OPERACION_COD_SOL_TRASP,
                                             g_usuario_cod ,
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
                                        g_usuario_cod,
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
         EXECUTE prp_act_fol_desmarca USING p_folio_fovissste,
                                            p_id_prt_solicitud_cedente
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
                                                   g_usuario_cod
                                              INTO g_resultado_maq.*
            
            END IF
            LET v_bnd_envio = TRUE
         END IF
         

   END CASE

   RETURN v_bnd_envio
END FUNCTION