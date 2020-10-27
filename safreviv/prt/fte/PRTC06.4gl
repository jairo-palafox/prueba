--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/07/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC05                                                     #
#Objetivo        => Consulta cifras control y traspasos                        #
#Fecha Inicio    => 10 Junio 2015                                              #
################################################################################
SCHEMA "safre_viv"

GLOBALS "PRTG01.4gl"
GLOBALS "PRTWS02.inc"
GLOBALS "PRTWS07.inc"
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       g_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_ruta_listados   LIKE seg_modulo.ruta_listados,
       g_ruta_envio      LIKE seg_modulo.ruta_envio,
       g_ventana         ui.Window,
       g_forma           ui.Form,
       g_pid             LIKE glo_pid.pid,
       g_id_entidad_etiqueta DECIMAL(9,0)
 
MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)

   CONNECT TO "safre_tmp"
   CONNECT TO "safre_viv"
   CALL fn_inicializa_consultas()
   CALL fn_filtra_consulta()
   DISCONNECT "safre_viv"
   DISCONNECT "safre_tmp"

END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING,
       v_existe   BOOLEAN

   --DISCONNECT "safre_viv"
   --CONNECT TO "safre_tmp"
   SET CONNECTION "safre_tmp"
   LET v_consulta = "CREATE TABLE prt_tmp_con_trp_prt_ced(
                         id_solicitud DECIMAL(9,0),
                         nss     CHAR(11),
                         curp    CHAR(18),
                         credito CHAR(11),
                         sdo_insoluto  DECIMAL(22,2),
                         estado_marca  VARCHAR(20),
                         f_originacion DATE,
                         f_ini_tra     DATE,
                         saldo_viv97   DECIMAL(22,2),
                         aivs_solicitado  DECIMAL(22,2),
                         pesos_solicitado DECIMAL(22,2),
                         f_marca        DATE,
                         folio_tramite  VARCHAR(50),                         
                         aivs_procesar  DECIMAL(22,2),
                         pesos_procesar DECIMAL(22,2),
                         diag_procesar  CHAR(3),
                         desc_diag_procesar VARCHAR(100),
                         aivs_cedido        DECIMAL(22,2),
                         pesos_cedido       DECIMAL(22,2),
                         fecha_proceso DATE,
                         diag_notif    VARCHAR(40),
                         diagnostico   VARCHAR(100))"
   PREPARE prp_crea_tmp_ced_ini FROM v_consulta

   LET v_consulta = " CREATE TABLE prt_tmp_con_trp_prt_rec(
                         id_solicitud DECIMAL(9,0),
                         nss     CHAR(11),
                         curp    CHAR(18),
                         credito CHAR(11),
                         f_marca DATE,
                         folio_tramite CHAR(50),
                         sdo_insoluto  DECIMAL(22,2),
                         aivs_recibido DECIMAL(22,2),
                         pesos_recibido DECIMAL(22,2),
                         fecha_proceso DATE,
                         pesos_dispersion DECIMAL(22,2),
                         fecha_dispersion DATE)"
   PREPARE prp_crea_tmp_rec_ini FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 1",
                    "   FROM systables",
                    "  WHERE tabname = ?"
   PREPARE prp_cons_tabla_ini FROM v_consulta
   
   EXECUTE prp_cons_tabla_ini USING "prt_tmp_con_trp_prt_ced"
                               INTO v_existe
   IF NOT( v_existe )THEN
      EXECUTE prp_crea_tmp_ced_ini
   END IF
   LET v_existe = FALSE;
   EXECUTE prp_cons_tabla_ini USING "prt_tmp_con_trp_prt_rec"
                               INTO v_existe
   IF NOT( v_existe )THEN
      EXECUTE prp_crea_tmp_rec_ini
   END IF
   
   --DISCONNECT "safre_tmp"
   --CONNECT TO "safre_viv"
   SET CONNECTION "safre_viv"
   
   SELECT ruta_bin,
          ruta_envio
     INTO g_ruta_ejecutable,
          g_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   SELECT ruta_listados
     INTO g_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"

   LET v_consulta = " SELECT FIRST 1 etq.id_entidad_etiqueta",
                    "   FROM glo_entidad_etiqueta etq JOIN glo_entidad_historico_consulta his",
                    "     ON etq.id_entidad_historico = his.id_entidad_historico",
                    "  WHERE his.entidad_cod = 'prt_solicitud_cedente'",
                    "    AND etq.cve_natural = 'diagnostico_interno'"
   PREPARE prp_rec_id_entidad_etiqueta FROM v_consulta
   EXECUTE prp_rec_id_entidad_etiqueta INTO g_id_entidad_etiqueta

   # Consulta el rechazo de la marca
   LET v_consulta = "EXECUTE FUNCTION fn_glo_recupera_descripciones(?,?)"
   PREPARE prp_rec_diagnostico FROM v_consulta 

   LET v_consulta = " SELECT diagnostico_externo",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_interno = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_consulta_diag_externo FROM v_consulta

   LET v_consulta = " SELECT descripcion_general",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_externo = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_consulta_desc_diagostico FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 1",
                    "   FROM systables",
                    "  WHERE tabname = ?"
   PREPARE prp_cons_tabla FROM v_consulta

END FUNCTION 

# Descripción: Filtra consulta de cifras control de procesar
FUNCTION fn_filtra_consulta()
DEFINE v_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE,
          v_marcada BOOLEAN
       END RECORD

   OPEN WINDOW vtna_con_prt WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC061"

      INPUT v_filtros.* FROM filtro_flujo,
                             filtro_nss,
                             filtro_curp,
                             filtro_credito,
                             filtro_f_marca_ini,
                             filtro_f_marca_fin,
                             filtro_traspaso ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE, CANCEL=FALSE)
         BEFORE INPUT
            LET g_ventana = ui.Window.getCurrent()
            LET g_forma = g_ventana.getForm()               
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)         
               CALL g_ventana.setText(p_titulo_ventana)
            END IF

         ON CHANGE filtro_f_marca_ini
            IF( v_filtros.v_f_marca_ini > TODAY )THEN
               LET v_filtros.v_f_marca_ini = TODAY
            END IF

         ON CHANGE filtro_f_marca_fin
            IF( v_filtros.v_f_marca_fin > TODAY )THEN
               LET v_filtros.v_f_marca_fin = TODAY
            END IF

         ON CHANGE filtro_flujo
            IF( v_filtros.v_flujo = 1 )THEN
              # Cedente
              CALL g_forma.setFieldHidden("filtro_traspaso",FALSE)
              CALL g_forma.setElementHidden("lbl_estado_marca",FALSE)
            ELSE
              # Receptora
              CALL g_forma.setFieldHidden("filtro_traspaso",TRUE)
              CALL g_forma.setElementHidden("lbl_estado_marca",TRUE)
            END IF

         ON ACTION consultar
            IF( v_filtros.v_f_marca_ini > v_filtros.v_f_marca_fin )THEN
                CALL fn_mensaje(p_titulo_ventana,"Fecha inicio no puede ser mayor a fecha fin","information")
                CONTINUE INPUT
            END IF
            # 1 = cedente, 2 = receptor
            IF( v_filtros.v_flujo = 1 )THEN
               # Flujo cedente
               # 1 = marca aceptada, 0 = marca rechazada
               IF(v_filtros.v_marcada = 1)THEN
                  CALL g_forma.setElementHidden("gpo_solicitud_cedente_aceptados",FALSE)
                  CALL fn_consulta_solicitud_cedente_ace(v_filtros.*)
                  CALL g_forma.setElementHidden("gpo_solicitud_cedente_aceptados",TRUE)
               ELSE
                  CALL g_forma.setElementHidden("gpo_solicitud_cedente_rechazados",FALSE)
                  CALL fn_consulta_solicitud_cedente_rch(v_filtros.*)
                  CALL g_forma.setElementHidden("gpo_solicitud_cedente_rechazados",TRUE)
               END IF
            ELSE
               CALL g_forma.setElementHidden("gpo_solicitud_receptora",FALSE)
               # Flujo receptora               
               CALL fn_consulta_solicitud_receptora(v_filtros.*)
               CALL g_forma.setElementHidden("gpo_solicitud_receptora",TRUE)
            END IF
            CONTINUE INPUT

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_con_prt

END FUNCTION

# Descripción: Consulta estado de solicitud y saldos de traspaso
FUNCTION fn_consulta_solicitud_cedente_ace(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE,
          v_marcada BOOLEAN
       END RECORD,
       r_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida
       END RECORD,
       v_registros_rpt DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_indice INTEGER,
       r_confirma BOOLEAN,
       r_resultado_opera INTEGER,
       r_error BOOLEAN

   DISPLAY ARRAY r_registros TO sr_registros_ced_ace.* ATTRIBUTES( UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

      BEFORE DISPLAY 
         CALL fn_recupera_registros_cedente_ace(p_filtros.*) RETURNING r_registros
         IF( r_registros.getLength() = 0 )THEN
            CALL fn_mensaje(g_ventana,"No se encontraron registros con criterio dado","information")
            EXIT DISPLAY
         END IF

      ON ACTION generar_reporte
         CALL fn_ventana_confirma("AVISO","¿Generar reporte de portabilidad?","question")RETURNING r_confirma
         IF( r_confirma )THEN
            LET g_pid = 0
            CALL fn_valida_operacion(g_pid,
                                     C_PROCESO_COD_REPORTES_PRT,
                                     C_OPERA_COD_CARGA) RETURNING r_resultado_opera
            IF( r_resultado_opera = 0 )THEN
               CALL v_registros_rpt.clear()               
               FOR v_indice = 1 TO r_registros.getLength()
                  LET v_registros_rpt[v_indice].v_id_solicitud     = r_registros[v_indice].v_id_solicitud
                  LET v_registros_rpt[v_indice].v_nss              = r_registros[v_indice].v_nss 
                  LET v_registros_rpt[v_indice].v_curp             = r_registros[v_indice].v_curp
                  LET v_registros_rpt[v_indice].v_credito          = r_registros[v_indice].v_credito 
                  LET v_registros_rpt[v_indice].v_sdo_insoluto     = r_registros[v_indice].v_sdo_insoluto 
                  LET v_registros_rpt[v_indice].v_estado_marca     = r_registros[v_indice].v_estado_marca 
                  LET v_registros_rpt[v_indice].v_f_originacion    = r_registros[v_indice].v_f_originacion 
                  LET v_registros_rpt[v_indice].v_f_ini_tramite    = r_registros[v_indice].v_f_ini_tramite 
                  LET v_registros_rpt[v_indice].v_saldo_viv97_inf  = r_registros[v_indice].v_saldo_viv97_inf
                  LET v_registros_rpt[v_indice].v_aivs_solicitado  = r_registros[v_indice].v_aivs_solicitado
                  LET v_registros_rpt[v_indice].v_pesos_solicitado = r_registros[v_indice].v_pesos_solicitado
                  LET v_registros_rpt[v_indice].v_f_marca          = r_registros[v_indice].v_f_marca
                  LET v_registros_rpt[v_indice].v_folio_tram_proce = r_registros[v_indice].v_folio_tram_proce   
                  LET v_registros_rpt[v_indice].v_aivs_procesar    = r_registros[v_indice].v_aivs_procesar   
                  LET v_registros_rpt[v_indice].v_pesos_procesar   = r_registros[v_indice].v_pesos_procesar
                  LET v_registros_rpt[v_indice].v_diag_procesar    = r_registros[v_indice].v_diag_procesar
                  LET v_registros_rpt[v_indice].v_desc_diag_procesar = r_registros[v_indice].v_desc_diag_procesar
                  LET v_registros_rpt[v_indice].v_aivs_cedido      = r_registros[v_indice].v_aivs_cedido
                  LET v_registros_rpt[v_indice].v_pesos_cedido     = r_registros[v_indice].v_pesos_cedido
                  LET v_registros_rpt[v_indice].v_fecha_proceso    = r_registros[v_indice].v_fecha_proceso                  
               END FOR
               # Función para generar el reporte
               CALL fn_genera_reportes_cedente(v_registros_rpt) RETURNING r_error
               IF( r_error )THEN
                  CONTINUE DISPLAY
               ELSE
                  CALL r_registros.clear()
                  EXIT DISPLAY
               END IF
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
            END IF
         END IF
         CONTINUE DISPLAY
         
      ON ACTION cancelar
         CALL r_registros.clear()
         EXIT DISPLAY

   END DISPLAY
   
END FUNCTION

# Descripción: Consulta estado de solicitud y saldos de traspaso
FUNCTION fn_consulta_solicitud_cedente_rch(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE,
          v_marcada BOOLEAN
       END RECORD,
       r_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_registros_rpt DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_indice INTEGER,
       r_confirma BOOLEAN,
       r_resultado_opera INTEGER,
       r_error BOOLEAN

   DISPLAY ARRAY r_registros TO sr_registros_ced_rch.* ATTRIBUTES( UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

      BEFORE DISPLAY 
         CALL fn_recupera_registros_cedente_rch(p_filtros.*) RETURNING r_registros
         IF( r_registros.getLength() = 0 )THEN
            CALL fn_mensaje(g_ventana,"No se encontraron registros con criterio dado","information")
            EXIT DISPLAY
         END IF

      ON ACTION generar_reporte
         CALL fn_ventana_confirma("AVISO","¿Generar reporte de portabilidad?","question")RETURNING r_confirma
         IF( r_confirma )THEN
            LET g_pid = 0
            CALL fn_valida_operacion(g_pid,
                                     C_PROCESO_COD_REPORTES_PRT,
                                     C_OPERA_COD_CARGA) RETURNING r_resultado_opera
            IF( r_resultado_opera = 0 )THEN
               CALL v_registros_rpt.clear()
               FOR v_indice = 1 TO r_registros.getLength()
                  LET v_registros_rpt[v_indice].v_id_solicitud = r_registros[v_indice].v_id_solicitud
                  LET v_registros_rpt[v_indice].v_nss          = r_registros[v_indice].v_nss 
                  LET v_registros_rpt[v_indice].v_curp         = r_registros[v_indice].v_curp
                  LET v_registros_rpt[v_indice].v_credito      = r_registros[v_indice].v_credito
                  LET v_registros_rpt[v_indice].v_f_marca      = r_registros[v_indice].v_f_marca 
                  LET v_registros_rpt[v_indice].v_sdo_insoluto = r_registros[v_indice].v_sdo_insoluto 
                  LET v_registros_rpt[v_indice].v_estado_marca = r_registros[v_indice].v_estado_marca                 
                  LET v_registros_rpt[v_indice].v_diag_notif   = r_registros[v_indice].v_diag_notif
                  LET v_registros_rpt[v_indice].v_diag_desc    = r_registros[v_indice].v_diag_desc                  
               END FOR
               # Función para generar el reporte
               CALL fn_genera_reportes_cedente(v_registros_rpt) RETURNING r_error
               IF( r_error )THEN
                  CONTINUE DISPLAY
               ELSE
                  CALL r_registros.clear()
                  EXIT DISPLAY
               END IF
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
            END IF
         END IF
         CONTINUE DISPLAY
         
      ON ACTION cancelar
         CALL r_registros.clear()
         EXIT DISPLAY

   END DISPLAY
   
END FUNCTION

# Descripción: Recupera los registros cedente según el filtro
FUNCTION fn_recupera_registros_cedente_ace(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE,
          v_marcada BOOLEAN
       END RECORD,
       v_registro RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto     LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca     VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,          
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida,
          v_id_derechohabiente LIKE cta_movimiento.id_derechohabiente,
          v_folio_liquida      LIKE cta_movimiento.folio_liquida,
          v_id_referencia      LIKE cta_movimiento.id_referencia
       END RECORD,
       v_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto     LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca     VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,          
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida
       END RECORD,
       r_anio_consulta STRING,
       r_ciclos   SMALLINT,
       v_indice   INTEGER,
       v_consulta STRING,
       v_filtro   STRING,
       v_filtro_sin_marca STRING

   CALL fn_recupera_ciclos_fechas(p_filtros.v_f_marca_ini,
                                  p_filtros.v_f_marca_fin) RETURNING r_anio_consulta, r_ciclos

   LET v_filtro = "1=1 AND"
   LET v_filtro_sin_marca = "1=1 AND"
   LET v_consulta = " SELECT sol.id_prt_solicitud_cedente,",
                    "        sol.nss,",
                    "        sol.curp,",
                    "        sol.id_credito_fovissste,",
                    "        sol.saldo_insoluto_credito_fovissste,",
                    "        'ACEPTADA',",
                    "        sol.f_originacion_fovissste,",
                    "        sol.f_ini_tramite,",
                    "        sol.pesos_saldo_viv97_infonavit,",
                    "        sol.aivs_saldo_viv97_infonavit,",--"        trp.mto_aivs_infonavit97,",
                    "        sol.pesos_saldo_viv97_infonavit,",--"        trp.mto_pesos_infonavit97,",
                    "        sfr.f_inicio,",
                    "        bust.folio_procesar,",
                    "        trp.mto_aivs_infonavit97_afo,",
                    "        trp.mto_pesos_infonavit97_afo,",
                    "        trp.diag_procesar,",
                    "        '',",
                    "        ''::decimal(16,6),",--cta.monto_acciones,",
                    "        ''::decimal(12,2),",--cta.monto_pesos,",
                    "        ''::date,",--cta.f_liquida,",
                    "        afi.id_derechohabiente,",
                    "        trp.folio_liquida,",
                    "        trp.id_prt_traspaso_cedente",
                    "   FROM prt_solicitud_cedente sol",
                    "        JOIN afi_derechohabiente afi",
                    "     ON afi.nss = sol.nss",                    

                    "        LEFT OUTER JOIN (prt_traspaso_cedente trp",
                    
                    "        JOIN prt_cza_cedente cza",
                    "     ON trp.folio_liquida = cza.folio_liquida",
                    "    AND cza.tipo_traspaso = ?)",
                       
                    "     ON trp.id_prt_solicitud_cedente = sol.id_prt_solicitud_cedente",
                                        
                    {"        LEFT OUTER JOIN tmp_cta_movimiento_con cta", # Tabla temporal
                    "     ON cta.id_derechohabiente = afi.id_derechohabiente",
                    "    AND cta.folio_liquida = trp.folio_liquida",
                    --"    AND cta.movimiento = ?",
                    "    AND cta.id_referencia = trp.id_prt_traspaso_cedente",}

                    "        JOIN sfr_marca_historica sfr",
                    "     ON sfr.marca = ?",
                    "    AND sfr.id_derechohabiente = afi.id_derechohabiente",                    
                    "    AND sfr.n_referencia = sol.id_prt_solicitud_cedente",
                    "    AND sfr.estado_marca = 0",

                    "        LEFT OUTER JOIN bus_solicitud_tramite buss",
                    "     ON buss.id_bus_solicitud_tramite = sfr.folio",
                    "        LEFT OUTER JOIN bus_tramite bust",
                    "     ON bust.id_bus_tramite = buss.id_bus_tramite",
                    "  WHERE "

   IF( p_filtros.v_nss IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.nss MATCHES '"||p_filtros.v_nss||"' AND"
      LET v_filtro_sin_marca = v_filtro_sin_marca || " a.nss MATCHES '"||p_filtros.v_nss||"' AND"
   END IF
   
   IF( p_filtros.v_curp IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.curp MATCHES '"||p_filtros.v_curp||"' AND"
      LET v_filtro_sin_marca = v_filtro_sin_marca || " a.curp MATCHES '"||p_filtros.v_curp||"' AND"
   END IF

   IF( p_filtros.v_credito IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.id_credito_fovissste = "||p_filtros.v_credito||" AND"
      LET v_filtro_sin_marca = v_filtro_sin_marca || " a.id_credito_fovissste = "||p_filtros.v_credito||" AND"
   END IF

   IF( p_filtros.v_f_marca_ini IS NOT NULL )THEN
      IF( p_filtros.v_f_marca_fin IS NOT NULL )THEN
         LET v_filtro = v_filtro || " sfr.f_inicio between '"||p_filtros.v_f_marca_ini||"' AND '"||p_filtros.v_f_marca_fin||"' AND"
      ELSE
         LET v_filtro = v_filtro || " sfr.f_inicio = '"||p_filtros.v_f_marca_ini||"' AND"
      END IF      
   ELSE
      IF( p_filtros.v_f_marca_fin CLIPPED IS NOT NULL )THEN
         LET v_filtro = v_filtro || " sfr.f_inicio = '"||p_filtros.v_f_marca_fin||"' AND"
      END IF
   END IF

   LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3)
   LET v_filtro_sin_marca = v_filtro_sin_marca.subString(1,v_filtro_sin_marca.getLength()-3)
   
   LET v_consulta = v_consulta||v_filtro|| 
                    " UNION "||  # Recupera las solicitudes de las cuales no se tiene respuesta de marca
                    " SELECT a.id_prt_solicitud_Cedente         ,"||
                    "        a.nss                              ,"||
                    "        a.curp                             ,"||
                    "        a.id_credito_fovissste             ,"||
                    "        a.saldo_insoluto_credito_fovissste ,"||
                    "        'ACEPTADA',"||
                    "        a.f_originacion_fovissste,"||
                    "        a.f_ini_tramite,"||
                    "        a.pesos_saldo_viv97_infonavit,"||
                    "        ''::decimal(22,2),"||
                    "        ''::decimal(22,2),"||
                    "        ''::date,"||
                    "        ''::char(50),"||
                    "        ''::decimal(22,2),"||
                    "        ''::decimal(22,2),"||
                    "        ''::char(3),"||
                    "        'SIN RESPUESTA NOTIFICACION RESULTADO DE MARCA PROCESAR' ,"||
                    "        ''::decimal(16,6),"||
                    "        ''::decimal(12,2),"||
                    "        ''::date,"||
                    "        ''::decimal(9,0),"||
                    "        ''::decimal(9,0),"||
                    "        ''::decimal(9,0)"||
                    " FROM prt_solicitud_cedente a,"||
                    "      cta_movimiento d,"||
                    "      afi_derechohabiente e"||
                    " WHERE a.estado in (30)"||
                    " AND (a.diagnostico_interno = 0 or a.diagnostico_interno is null)"||
                    " AND a.id_prt_solicitud_cedente not in "||
                    "     (SELECT b.id_prt_solicitud_Cedente FROM prt_his_solicitud_cedente b WHERE b.id_cat_dato_actualizado = 109)"||
                    " AND a.nss = e.nss"||
                    " AND e.id_derechohabiente = d.id_derechohabiente"||
                    " AND d.subcuenta = 4"||
                    " AND ",v_filtro_sin_marca||
                    " group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20"||
                    " ORDER BY sol.id_prt_solicitud_cedente "

   LET v_indice = 1
   --DISPLAY v_consulta
   PREPARE prp_consulta_registros_ace FROM v_consulta
   DECLARE cur_consulta_registros_ace CURSOR FOR prp_consulta_registros_ace
   FOREACH cur_consulta_registros_ace USING C_TIPO_TRASPASO_CORRIENTE,                                        
                                            --C_MOV_CED_ABONO_TRASPASO,
                                            C_MARCA_PRT_CEDENTE
                                       INTO v_registro.*--s[v_indice].*

      LET v_registros[v_indice].v_id_solicitud  = v_registro.v_id_solicitud
      LET v_registros[v_indice].v_nss           = v_registro.v_nss
      LET v_registros[v_indice].v_curp          = v_registro.v_curp
      LET v_registros[v_indice].v_credito       = v_registro.v_credito
      LET v_registros[v_indice].v_sdo_insoluto  = v_registro.v_sdo_insoluto
      LET v_registros[v_indice].v_estado_marca  = v_registro.v_estado_marca
      LET v_registros[v_indice].v_f_originacion = v_registro.v_f_originacion
      LET v_registros[v_indice].v_f_ini_tramite = v_registro.v_f_ini_tramite
      LET v_registros[v_indice].v_saldo_viv97_inf = v_registro.v_saldo_viv97_inf
      LET v_registros[v_indice].v_aivs_solicitado = v_registro.v_aivs_solicitado
      LET v_registros[v_indice].v_pesos_solicitado = v_registro.v_pesos_solicitado
      LET v_registros[v_indice].v_f_marca          = v_registro.v_f_marca
      LET v_registros[v_indice].v_folio_tram_proce = v_registro.v_folio_tram_proce
      LET v_registros[v_indice].v_aivs_procesar    = v_registro.v_aivs_procesar
      LET v_registros[v_indice].v_pesos_procesar   = v_registro.v_pesos_procesar
      LET v_registros[v_indice].v_diag_procesar    = v_registro.v_diag_procesar
      LET v_registros[v_indice].v_desc_diag_procesar = v_registro.v_desc_diag_procesar

      CALL fn_genera_tabla_tmp_movimiento(v_registro.v_id_derechohabiente,
                                          v_registro.v_folio_liquida,
                                          C_MOV_CED_ABONO_TRASPASO,
                                          v_registro.v_id_referencia,
                                          r_anio_consulta, r_ciclos) 
         RETURNING v_registros[v_indice].v_fecha_proceso,
                   v_registros[v_indice].v_aivs_cedido,
                   v_registros[v_indice].v_pesos_cedido
                                             
      IF( v_registro.v_diag_procesar IS NOT NULL )THEN
         EXECUTE prp_consulta_desc_diagostico USING v_registros[v_indice].v_diag_procesar,
                                                    C_DESTINO_DIAG_T_I
                                               INTO v_registros[v_indice].v_desc_diag_procesar
      END IF
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_consulta_registros_ace
   {IF( v_registros[v_registros.getLength()].v_nss IS NULL)THEN
      CALL v_registros.deleteElement(v_registros.getLength())
   END IF}

   RETURN v_registros
END FUNCTION

# Descripción: Recupera los registros cedente según el filtro
FUNCTION fn_recupera_registros_cedente_rch(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE,
          v_marcada BOOLEAN
       END RECORD,
       v_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_diag_notif   VARCHAR(20),
       v_desc_diagnostico RECORD
          v_ind          SMALLINT,
          v_diag         CHAR(3),
          v_sql_error    INTEGER,
          v_isam_error   INTEGER,
          v_msg_error    VARCHAR(100),
          v_valor_actual VARCHAR(200)
       END RECORD,
       v_indice   INTEGER,
       v_consulta STRING,
       v_filtro   STRING

   LET v_filtro = "1=1 AND"
   LET v_consulta = " SELECT sol.id_prt_solicitud_cedente,",
                    "        sol.nss,",
                    "        sol.curp,",
                    "        sol.id_credito_fovissste,",
                    "        sol.saldo_insoluto_credito_fovissste,",
                    "        'RECHAZADA',",
                    "        sfr.f_inicio,",
                    "        his.valor_actual",
                    "   FROM prt_solicitud_cedente sol",
                    "        JOIN afi_derechohabiente afi",
                    "     ON afi.nss = sol.nss",
                    
                    "        LEFT OUTER JOIN (prt_traspaso_cedente trp",
                    
                    "        JOIN prt_cza_cedente cza",
                    "     ON trp.folio_liquida = cza.folio_liquida",
                    "    AND cza.tipo_traspaso = ?)",

                    "     ON trp.id_prt_solicitud_cedente = sol.id_prt_solicitud_cedente",
                    
                    {"        LEFT OUTER JOIN tmp_cta_movimiento_con cta", # Tabla temporal
                    "     ON cta.id_derechohabiente = afi.id_derechohabiente",
                    "    AND cta.folio_liquida = trp.folio_liquida",
                    "    AND cta.id_referencia = trp.id_prt_traspaso_cedente",}
                    --"    AND cta.movimiento = ?",
                    
                    "        JOIN sfr_marca_historica sfr",
                    "     ON sfr.marca = ?",
                    "    AND sfr.id_derechohabiente = afi.id_derechohabiente",
                    "    AND sfr.n_referencia = sol.id_prt_solicitud_cedente",
                    "    AND sfr.estado_marca <> 0",
                    "    AND sfr.id_derechohabiente NOT IN (",
                    "        SELECT id_derechohabiente",
                    "          FROM sfr_marca_historica sfr2",
                    "         WHERE sfr2.marca = ?",
                    "           AND sfr2.estado_marca = 0)", # Si ya existe la marca aceptada no recupera registro de marca rechazada
                    "        LEFT OUTER JOIN prt_his_solicitud_cedente his",
                    "     ON his.id_prt_solicitud_cedente = sol.id_prt_solicitud_cedente",
                    "    AND his.id_cat_dato_actualizado = ?",
                    "  WHERE "

   IF( p_filtros.v_nss IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.nss MATCHES '"||p_filtros.v_nss||"' AND"
   END IF
   
   IF( p_filtros.v_curp IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.curp MATCHES '"||p_filtros.v_curp||"' AND"
   END IF

   IF( p_filtros.v_credito IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.id_credito_fovissste = "||p_filtros.v_credito||" AND"
   END IF

   IF( p_filtros.v_f_marca_ini IS NOT NULL )THEN
      IF( p_filtros.v_f_marca_fin IS NOT NULL )THEN
         LET v_filtro = v_filtro || " sfr.f_inicio between '"||p_filtros.v_f_marca_ini||"' AND '"||p_filtros.v_f_marca_fin||"' AND"
      ELSE
         LET v_filtro = v_filtro || " sfr.f_inicio = '"||p_filtros.v_f_marca_ini||"' AND"
      END IF      
   ELSE
      IF( p_filtros.v_f_marca_fin CLIPPED IS NOT NULL )THEN
         LET v_filtro = v_filtro || " sfr.f_inicio = '"||p_filtros.v_f_marca_fin||"' AND"      
      END IF
   END IF

   LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3)
   LET v_consulta = v_consulta||v_filtro|| " ORDER BY sol.id_prt_solicitud_cedente"
   LET v_indice = 1

   --DISPLAY v_consulta
   PREPARE prp_consulta_registros_rch FROM v_consulta
   DECLARE cur_consulta_registros_rch CURSOR FOR prp_consulta_registros_rch
   FOREACH cur_consulta_registros_rch USING C_TIPO_TRASPASO_CORRIENTE,
                                            --C_MOV_CED_ABONO_TRASPASO,
                                            C_MARCA_PRT_CEDENTE,
                                            C_MARCA_PRT_CEDENTE,
                                            g_id_entidad_etiqueta
                                       INTO v_registros[v_indice].v_id_solicitud,
                                            v_registros[v_indice].v_nss,
                                            v_registros[v_indice].v_curp,
                                            v_registros[v_indice].v_credito,
                                            v_registros[v_indice].v_sdo_insoluto,
                                            v_registros[v_indice].v_estado_marca,
                                            v_registros[v_indice].v_f_marca,
                                            v_diag_notif

      EXECUTE prp_rec_diagnostico USING g_id_entidad_etiqueta,
                                        v_diag_notif
                                   INTO v_desc_diagnostico.*
      EXECUTE prp_consulta_diag_externo USING v_diag_notif,
                                              C_DESTINO_DIAG_P_I
                                         INTO v_registros[v_indice].v_diag_notif # Recupera el diagnostico de procesar
      LET v_registros[v_indice].v_diag_desc = v_desc_diagnostico.v_valor_actual
      LET v_indice = v_indice + 1
      
   END FOREACH
   FREE cur_consulta_registros_rch
   IF( v_registros[v_registros.getLength()].v_nss IS NULL)THEN
      CALL v_registros.deleteElement(v_registros.getLength())
   END IF

   RETURN v_registros
END FUNCTION

# Descripción: Especifica parametros y construcción de reporte
FUNCTION fn_genera_reportes_cedente(p_registros)
DEFINE p_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_tipo_cedente    SMALLINT,
       v_estado_marca    VARCHAR(20),
       r_resultado_opera INTEGER,
       v_bnd_error       BOOLEAN,
       v_indice          INTEGER,
       v_comando         STRING,
       v_consulta        STRING,
       v_proceso_txt     STRING,
       v_opera_txt       STRING,
       v_pid_txt         STRING

   LET v_bnd_error = TRUE
   
   CALL fn_genera_pid(C_PROCESO_COD_REPORTES_PRT,
                      C_OPERA_COD_CARGA,
                      p_usuario_cod) RETURNING g_pid
                      
   CALL fn_inicializa_proceso(g_pid,
                               C_PROCESO_COD_REPORTES_PRT,
                               C_OPERA_COD_CARGA,
                               0,
                               "PRTC06",
                               "NA",
                               p_usuario_cod) RETURNING r_resultado_opera
                               
   IF( r_resultado_opera = 0 )THEN
         CALL fn_actualiza_opera_ini(g_pid,
                                     C_PROCESO_COD_REPORTES_PRT,
                                     C_OPERA_COD_CARGA,
                                     0,
                                     "PRTC06",
                                     "NA",
                                     p_usuario_cod) RETURNING r_resultado_opera 
      IF( r_resultado_opera = 0 )THEN
         --DISCONNECT "safre_viv"
         --CONNECT TO "safre_tmp"
         SET CONNECTION "safre_tmp"
         LET v_consulta = " TRUNCATE TABLE prt_tmp_con_trp_prt_ced"{; 
                            CREATE TABLE prt_tmp_con_trp_prt_ced(
                              id_solicitud DECIMAL(9,0),
                              nss     CHAR(11),
                              curp    CHAR(18),
                              credito CHAR(11),
                              f_marca DATE,
                              estado_marca VARCHAR(20),
                              diagnostico  VARCHAR(40),
                              sdo_insoluto    DECIMAL(22,2),
                              aivs_solicitado DECIMAL(22,2),
                              aivs_procesar   DECIMAL(22,2),
                              aivs_cedido     DECIMAL(22,2),
                              fecha_proceso   DATE)"}
         PREPARE prp_crea_tmp_ced FROM v_consulta
         EXECUTE prp_crea_tmp_ced
            
         LET v_consulta = "INSERT INTO prt_tmp_con_trp_prt_ced VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
         PREPARE prp_ins_tmp_ced FROM v_consulta
            
         FOR v_indice = 1 TO p_registros.getLength()
            LET v_estado_marca = p_registros[v_indice].v_estado_marca
            EXECUTE prp_ins_tmp_ced USING p_registros[v_indice].*
         END FOR
         IF(v_estado_marca = "ACEPTADA")THEN
            LET v_tipo_cedente = 1 # Aceptada
         ELSE
            LET v_tipo_cedente = 2 #Rechazadas
         END IF
            
         --DISCONNECT "safre_tmp"
         --CONNECT TO "safre_viv"
         SET CONNECTION "safre_viv"

         LET v_proceso_txt = C_PROCESO_COD_REPORTES_PRT 
         LET v_opera_txt   = C_OPERA_COD_CARGA
         LET v_pid_txt     = g_pid
         LET v_comando = "nohup fglrun ",g_ruta_ejecutable CLIPPED,"/PRTI06.42r '",p_usuario_cod CLIPPED, "' ",
                                                                                      g_pid CLIPPED," ",
                                                                                      C_PROCESO_COD_REPORTES_PRT," ",
                                                                                      C_OPERA_COD_CARGA," ",
                                                                                      "0 ",
                                                                                      "'NA' ",
                                                                                      "1 ", # Cedente
                                                                                      v_tipo_cedente," ",
                     " 1>", g_ruta_listados CLIPPED,"/nohup:",v_pid_txt USING "&&&&&",":",
                                                              v_proceso_txt USING "&&&&&",":",
                                                              v_opera_txt USING "&&&&&"," 2>&1 &" 

         
         TRY
            --DISPLAY ":",v_comando 
            RUN v_comando
            IF NOT( STATUS )THEN
               CALL fn_mensaje(p_titulo_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","information")
               LET v_bnd_error = FALSE
            END IF
         CATCH
            LET v_bnd_error = TRUE
            CALL fn_mensaje(p_titulo_ventana,"Ocurrio un error al ejecutar la operación","information")
         END TRY         
         
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

# Descripción: Consulta estado de solicitud y saldos de traspaso
FUNCTION fn_consulta_solicitud_receptora(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE,
          v_marcada BOOLEAN
       END RECORD,
       r_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_folio_tramite  LIKE bus_tramite.folio_procesar,
          v_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
          v_aivs_recibido  LIKE prt_traspaso_receptora.mto_aivs_fov2008,
          v_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
          v_fecha_proceso  LIKE prt_preliquida.f_liquida,
          v_pesos_dispersion LIKE cta_movimiento.monto_pesos,
          v_f_liquida_disp LIKE cta_movimiento.f_liquida
       END RECORD,
       r_confirma BOOLEAN,
       r_resultado_opera INTEGER,
       r_error BOOLEAN

   DISPLAY ARRAY r_registros TO sr_registros_rec.* ATTRIBUTES( UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

      BEFORE DISPLAY 
         CALL fn_recupera_registros_receptora(p_filtros.*) RETURNING r_registros
         IF( r_registros.getLength() = 0 )THEN
            CALL fn_mensaje(g_ventana,"No se encontraron registros con criterio dado","information")
            EXIT DISPLAY
         END IF

      ON ACTION generar_reporte
         CALL fn_ventana_confirma("AVISO","¿Generar reporte de portabilidad?","question")RETURNING r_confirma
         IF( r_confirma )THEN
            LET g_pid = 0
            CALL fn_valida_operacion(g_pid,
                                     C_PROCESO_COD_REPORTES_PRT,
                                     C_OPERA_COD_CARGA) RETURNING r_resultado_opera
            IF( r_resultado_opera = 0 )THEN
               # Función para generar el reporte
               CALL fn_genera_reportes_receptora(r_registros) RETURNING r_error
               IF( r_error )THEN
                  CONTINUE DISPLAY
               ELSE
                  CALL r_registros.clear()
                  EXIT DISPLAY
               END IF
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
            END IF
         END IF
         CONTINUE DISPLAY
         
      ON ACTION cancelar
         CALL r_registros.clear()
         EXIT DISPLAY

   END DISPLAY
END FUNCTION

# Descripción: Recupera los registros receptora según el filtro
FUNCTION fn_recupera_registros_receptora(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE,
          v_marcada BOOLEAN
       END RECORD,
       v_registro RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_folio_tramite  LIKE bus_tramite.folio_procesar,
          v_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
          {v_aivs_recibido  LIKE prt_traspaso_receptora.mto_aivs_fov2008,
          v_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
          v_fecha_proceso  LIKE prt_preliquida.f_liquida,
          v_pesos_dispersion LIKE cta_movimiento.monto_pesos,
          v_f_liquida_disp LIKE cta_movimiento.f_liquida,}
          v_id_derechohabiente LIKE cta_movimiento.id_derechohabiente,
          v_id_referencia LIKE cta_movimiento.id_referencia,
          v_folio_liquida LIKE cta_movimiento.folio_liquida
       END RECORD,
       v_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_folio_tramite  LIKE bus_tramite.folio_procesar,
          v_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
          v_aivs_recibido  LIKE prt_traspaso_receptora.mto_aivs_fov2008,
          v_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
          v_fecha_proceso  LIKE prt_preliquida.f_liquida,
          v_pesos_dispersion LIKE cta_movimiento.monto_pesos,
          v_f_liquida_disp LIKE cta_movimiento.f_liquida
       END RECORD,
       r_anio_consulta STRING,
       v_aivs LIKE cta_movimiento.monto_acciones,
       r_ciclos   SMALLINT,
       v_indice   INTEGER,
       v_consulta STRING,
       v_filtro   STRING

   CALL fn_recupera_ciclos_fechas(p_filtros.v_f_marca_ini,
                                  p_filtros.v_f_marca_fin) RETURNING r_anio_consulta, r_ciclos
   
   LET v_filtro = "1=1 AND"
   LET v_consulta = " SELECT sol.id_prt_solicitud_receptora,",
                    "        sol.nss,",
                    "        sol.curp,",
                    "        sol.id_credito_infonavit,",
                    "        sfr.f_inicio,",
                    "        bust.folio_procesar,",
                    "     case trp.tpo_operacion ",
                    "       when '01' THEN TO_CHAR(sol.saldo_insoluto_credito_infonavit) ",
                    "       when '02' THEN ' ' ", 
                    "     end saldo_insoluto_credito_infonavit, ",
                    {"        cta.monto_acciones,",
                    "        cta.monto_pesos,",
                    "        cta.f_liquida,",
                    "        cta2.monto_pesos,",
                    "        cta2.f_liquida,",}
                    "        afi.id_derechohabiente,",
                    "        trp.id_prt_traspaso_receptora,",
                    "        trp.folio_liquida",
                    "   FROM prt_solicitud_receptora sol",
                    "        JOIN afi_derechohabiente afi",
                    "     ON afi.nss = sol.nss",  
                   
                    "        LEFT OUTER JOIN (prt_traspaso_receptora trp",
                  
                    "        JOIN prt_cza_receptora cza",
                    "     ON trp.folio_liquida = cza.folio_liquida",
                    "     AND trp.estado = 30 ",                    
                    "    AND cza.tipo_traspaso = ?)",
                    "     ON trp.id_prt_solicitud_receptora = sol.id_prt_solicitud_receptora",

                    {"        LEFT OUTER JOIN tmp_cta_movimiento_con cta", # tabla temporal
                    "     ON cta.id_derechohabiente = afi.id_derechohabiente",
                    "    AND cta.id_referencia = trp.id_prt_traspaso_receptora",
                    "    AND cta.folio_liquida = trp.folio_liquida",
                    "    AND cta.movimiento = ?",

                    "        LEFT OUTER JOIN tmp_cta_movimiento_con cta2", # tabla temporal
                    "     ON cta2.id_derechohabiente = afi.id_derechohabiente",
                    "    AND cta2.id_referencia = trp.id_prt_traspaso_receptora",
                    "    AND cta2.movimiento = ?",}
                    
                    "        LEFT OUTER JOIN sfr_marca_historica sfr",
                    "     ON sfr.marca = ?",
                    "    AND sfr.id_derechohabiente = afi.id_derechohabiente",                    
                    "    AND sfr.n_referencia = sol.id_prt_solicitud_receptora",
                    "    AND sfr.estado_marca = 0 " ,                   
                    "        LEFT OUTER JOIN bus_solicitud_tramite buss",
                    "     ON buss.id_bus_solicitud_tramite = sfr.folio",
                    "        LEFT OUTER JOIN bus_tramite bust",
                    "     ON bust.id_bus_tramite = buss.id_bus_tramite",
                    "  WHERE "
                    
   IF( p_filtros.v_nss IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.nss MATCHES '"||p_filtros.v_nss||"' AND"
   END IF
   
   IF( p_filtros.v_curp IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.curp MATCHES '"||p_filtros.v_curp||"' AND"
   END IF

   IF( p_filtros.v_credito IS NOT NULL )THEN
      LET v_filtro = v_filtro || " sol.id_credito_infonavit = "||p_filtros.v_credito||" AND"
   END IF

   IF( p_filtros.v_f_marca_ini IS NOT NULL )THEN
      IF( p_filtros.v_f_marca_fin IS NOT NULL )THEN
         LET v_filtro = v_filtro || " sfr.f_inicio BETWEEN '"||p_filtros.v_f_marca_ini||"' AND '"||p_filtros.v_f_marca_fin||"' AND"
      ELSE
         LET v_filtro = v_filtro || " sfr.f_inicio = '"||p_filtros.v_f_marca_ini||"' AND"
      END IF      
   ELSE
      IF( p_filtros.v_f_marca_fin IS NOT NULL )THEN
         LET v_filtro = v_filtro || " sfr.f_inicio = '"||p_filtros.v_f_marca_fin||"' AND"      
      END IF
   END IF
   # Recupera los registros que ya tienen marca
   --LET v_filtro = v_filtro || " sfr.estado_marca = 0 AND"
   LET v_filtro = v_filtro || " 1 = 1 AND"

   LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3)
   
   LET v_consulta = v_consulta||v_filtro|| " ORDER BY sol.id_prt_solicitud_receptora"
   LET v_indice = 1
   --DISPLAY v_consulta 
   PREPARE prp_consulta_registros_rec FROM v_consulta
   DECLARE cur_consulta_registros_rec CURSOR FOR prp_consulta_registros_rec
   FOREACH cur_consulta_registros_rec USING C_TIPO_TRASPASO_CORRIENTE,
                                            --C_MOV_REC_ABONO_TRASPASO,
                                            --C_MOV_REC_CARGO_DISP,
                                            C_MARCA_PRT_RECEPTORA
                                       INTO v_registro.*--s[v_indice].*
                                       
      LET v_registros[v_indice].v_id_solicitud  = v_registro.v_id_solicitud
      LET v_registros[v_indice].v_nss           = v_registro.v_nss
      LET v_registros[v_indice].v_curp          = v_registro.v_curp
      LET v_registros[v_indice].v_credito       = v_registro.v_credito
      LET v_registros[v_indice].v_f_marca       = v_registro.v_f_marca
      LET v_registros[v_indice].v_folio_tramite = v_registro.v_folio_tramite
      LET v_registros[v_indice].v_sdo_insoluto  = v_registro.v_sdo_insoluto

      CALL fn_genera_tabla_tmp_movimiento(v_registro.v_id_derechohabiente,
                                          v_registro.v_folio_liquida,
                                          C_MOV_REC_ABONO_TRASPASO,
                                          v_registro.v_id_referencia,
                                          r_anio_consulta, r_ciclos) 
         RETURNING v_registros[v_indice].v_fecha_proceso,
                   v_registros[v_indice].v_aivs_recibido,
                   v_registros[v_indice].v_pesos_recibido
      CALL fn_genera_tabla_tmp_movimiento(v_registro.v_id_derechohabiente,
                                          NULL,
                                          C_MOV_REC_CARGO_DISP,
                                          v_registro.v_id_referencia,
                                          r_anio_consulta, r_ciclos) 
         RETURNING v_registros[v_indice].v_f_liquida_disp,
                   v_aivs,
                   v_registros[v_indice].v_pesos_dispersion
      
      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_consulta_registros_rec
   {IF( v_registros[v_registros.getLength()].v_nss IS NULL)THEN
      CALL v_registros.deleteElement(v_registros.getLength())
   END IF}
   RETURN v_registros
END FUNCTION

# Descripción: Especifica parametros y construcción de reporte
FUNCTION fn_genera_reportes_receptora(p_registros)
DEFINE p_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_folio_tramite  LIKE bus_tramite.folio_procesar,
          v_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
          v_aivs_recibido  LIKE prt_traspaso_receptora.mto_aivs_fov2008,
          v_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
          v_fecha_proceso  LIKE prt_preliquida.f_liquida,
          v_pesos_dispersion LIKE cta_movimiento.monto_pesos,
          v_f_liquida_disp LIKE cta_movimiento.f_liquida
       END RECORD,       
       r_resultado_opera INTEGER,
       v_bnd_error       BOOLEAN,
       v_indice          INTEGER,
       v_comando         STRING,
       v_consulta        STRING,
       v_proceso_txt     STRING,
       v_opera_txt       STRING,
       v_pid_txt         STRING

   LET v_bnd_error = TRUE
   
   CALL fn_genera_pid(C_PROCESO_COD_REPORTES_PRT,
                      C_OPERA_COD_CARGA,
                      p_usuario_cod) RETURNING g_pid
                      
   CALL fn_inicializa_proceso(g_pid,
                               C_PROCESO_COD_REPORTES_PRT,
                               C_OPERA_COD_CARGA,
                               0,
                               "PRTC06",
                               "NA",
                               p_usuario_cod) RETURNING r_resultado_opera
                               
   IF( r_resultado_opera = 0 )THEN
         CALL fn_actualiza_opera_ini(g_pid,
                                     C_PROCESO_COD_REPORTES_PRT,
                                     C_OPERA_COD_CARGA,
                                     0,
                                     "PRTC06",
                                     "NA",
                                     p_usuario_cod) RETURNING r_resultado_opera 
      IF( r_resultado_opera = 0 )THEN
         --DISCONNECT "safre_viv"
         --CONNECT TO "safre_tmp"
         SET CONNECTION "safre_tmp"
         LET v_consulta = " TRUNCATE TABLE prt_tmp_con_trp_prt_rec"{; 
                            CREATE TABLE prt_tmp_con_trp_prt_rec(
                              id_solicitud DECIMAL(9,0),
                              nss     CHAR(11),
                              curp    CHAR(18),
                              credito CHAR(11),
                              f_marca DATE,
                              sdo_insoluto  DECIMAL(22,2),
                              aivs_recibido DECIMAL(22,2),
                              fecha_proceso DATE)"}
         PREPARE prp_crea_tmp_rec FROM v_consulta
         EXECUTE prp_crea_tmp_rec
            
         LET v_consulta = "INSERT INTO prt_tmp_con_trp_prt_rec VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"
         PREPARE prp_ins_tmp_rec FROM v_consulta
            
         FOR v_indice = 1 TO p_registros.getLength()
            EXECUTE prp_ins_tmp_rec USING p_registros[v_indice].*
         END FOR
            
         --DISCONNECT "safre_tmp"
         --CONNECT TO "safre_viv"
         SET CONNECTION "safre_viv"

         LET v_proceso_txt = C_PROCESO_COD_REPORTES_PRT 
         LET v_opera_txt   = C_OPERA_COD_CARGA
         LET v_pid_txt     = g_pid
         LET v_comando = "nohup fglrun ",g_ruta_ejecutable CLIPPED,"/PRTI06.42r '",p_usuario_cod CLIPPED, "' ",
                                                                                      g_pid CLIPPED," ",
                                                                                      C_PROCESO_COD_REPORTES_PRT," ",
                                                                                      C_OPERA_COD_CARGA," ",
                                                                                      "0 ",
                                                                                      "'NA' ",
                                                                                      "2 ", # Receptora
                                                                                      "0 ", 
                     " 1>", g_ruta_listados CLIPPED,"/nohup:",v_pid_txt USING "&&&&&",":",
                                                              v_proceso_txt USING "&&&&&",":",
                                                              v_opera_txt USING "&&&&&"," 2>&1 &" 

         
         TRY
            RUN v_comando
            IF NOT( STATUS )THEN
               CALL fn_mensaje(p_titulo_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","information")
               LET v_bnd_error = FALSE
            END IF
         CATCH
            LET v_bnd_error = TRUE
            CALL fn_mensaje(p_titulo_ventana,"Ocurrio un error al ejecutar la operación","information")
         END TRY         
         
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

# Descripción: Función que calcula los ciclos (en años) entre dos fechas
#              Devuelve el año de inicio y los ciclos
PRIVATE FUNCTION fn_recupera_ciclos_fechas(p_fecha_inicio, p_fecha_fin)
DEFINE p_fecha_inicio DATE,
       p_fecha_fin DATE,
       v_anio_inicio STRING,
       v_anio_fin STRING,
       v_anio_consulta STRING,
       v_ciclos SMALLINT

   IF( p_fecha_inicio IS NOT NULL)THEN
      LET v_anio_inicio = YEAR(p_fecha_inicio)
      IF( p_fecha_fin IS NOT NULL)THEN
         LET v_anio_fin = YEAR(p_fecha_fin)
         LET v_anio_consulta = v_anio_inicio
         LET v_ciclos = v_anio_fin - v_anio_inicio
         LET v_ciclos = v_ciclos + 1
      ELSE
         LET v_anio_consulta = YEAR(p_fecha_inicio)
         LET v_ciclos = 1
      END IF
   ELSE
      IF( p_fecha_fin IS NOT NULL)THEN
         LET v_anio_consulta = YEAR(p_fecha_fin)
         LET v_ciclos = 1
      ELSE
         # si las fechas son nulas, el año a consultar, será desde que empezó a operar portabilidad
         LET v_anio_consulta = "2015"
         LET v_anio_fin = YEAR(TODAY)
         LET v_ciclos = v_anio_fin - v_anio_consulta
         LET v_ciclos = v_ciclos + 1
      END IF
   END IF

   RETURN v_anio_consulta,
          v_ciclos
END FUNCTION

# Descripción: Especifica parametros y construcción de reporte
PRIVATE FUNCTION fn_recupera_nombre_tabla_movimientos_anio(v_cad_anio)
DEFINE v_cad_anio STRING,
       v_nombre_tabla STRING

   IF( v_cad_anio == YEAR(TODAY) )THEN
      LET v_nombre_tabla = "cta_movimiento"
   ELSE
      LET v_nombre_tabla = "cta_movimiento",v_cad_anio.subString(3,4)
   END IF

   RETURN v_nombre_tabla
END FUNCTION

#
FUNCTION fn_genera_tabla_tmp_movimiento(p_id_derechohabiente,
                                        p_folio_liquida,
                                        p_movimiento,
                                        p_id_referencia,
                                        p_anio_consulta, 
                                        p_ciclos)
DEFINE p_id_derechohabiente LIKE cta_movimiento.folio_liquida,
       p_folio_liquida LIKE cta_movimiento.folio_liquida,
       p_movimiento    LIKE cta_movimiento.movimiento,
       p_id_referencia LIKE cta_movimiento.id_referencia,
       p_filtro        STRING,
       p_anio_consulta STRING,
       p_ciclos        SMALLINT,
       v_consulta      STRING,
       v_variable_consulta STRING,
       v_filtros       STRING,
       v_contador      SMALLINT,
       r_nom_tabla_cta_movimiento VARCHAR(80),
       r_f_liquida      LIKE cta_movimiento.f_liquida,
       r_monto_acciones LIKE cta_movimiento.monto_acciones,
       r_monto_pesos    LIKE cta_movimiento.monto_pesos,
       v_existe   BOOLEAN


   {PREPARE prp_crea_tmp_movimientos FROM 
   " DROP TABLE IF EXISTS tmp_cta_movimiento_con;
     CREATE TEMP TABLE tmp_cta_movimiento_con(
      f_liquida date NOT NULL ,
      id_derechohabiente DECIMAL(9,0) NOT NULL ,
      subcuenta SMALLINT NOT NULL ,
      fondo_inversion SMALLINT NOT NULL ,
      movimiento SMALLINT NOT NULL ,
      folio_liquida DECIMAL(9,0) NOT NULL ,
      id_referencia DECIMAL(9,0) NOT NULL ,
      monto_acciones DECIMAL(16,6),
      monto_pesos DECIMAL(12,2),
      f_valor DATE,
      f_registro DATE,
      h_registro DATETIME HOUR TO SECOND,
      origen CHAR(20));"
   EXECUTE prp_crea_tmp_movimientos

   

   LET v_consulta = " SET PDQPRIORITY HIGH; INSERT INTO tmp_cta_movimiento_con SELECT * ",
                    "   FROM TABLE(MULTISET("
   LET v_variable_consulta = " "
   FOR v_contador = 1 TO p_ciclos
      CALL fn_recupera_nombre_tabla_movimientos_anio( p_anio_consulta ) RETURNING r_nom_tabla_cta_movimiento
      LET p_anio_consulta = p_anio_consulta + 1
      EXECUTE prp_cons_tabla USING r_nom_tabla_cta_movimiento
                              INTO v_existe
      IF( v_existe )THEN

         LET v_variable_consulta = v_variable_consulta,
                                   " SELECT f_liquida,",
                                   "        id_derechohabiente,",
                                   "        subcuenta,",
                                   "        fondo_inversion,",
                                   "        movimiento,",
                                   "        folio_liquida,",
                                   "        id_referencia,",
                                   "        monto_acciones,",
                                   "        monto_pesos,",
                                   "        f_valor,",
                                   "        f_registro,",
                                   "        h_registro,",
                                   "        origen",
                                   "   FROM ",r_nom_tabla_cta_movimiento,
                                   "  WHERE ",p_filtro,
                                   " UNION"
      END IF
   END FOR

   LET v_variable_consulta = v_variable_consulta.subString(1,v_variable_consulta.getLength()-5)
   LET v_consulta = v_consulta,v_variable_consulta, "))"
   PREPARE prp_rec_mvimientos FROM v_consulta
   EXECUTE prp_rec_mvimientos}

   IF( p_movimiento <> C_MOV_REC_CARGO_DISP )THEN
      IF( p_id_derechohabiente IS NOT NULL)THEN
         LET v_filtros = "id_derechohabiente = ",p_id_derechohabiente
      ELSE
         LET v_filtros = "id_derechohabiente IS NULL"
      END IF
      
      LET v_filtros = v_filtros,"    AND movimiento = ",p_movimiento
      
      IF( p_folio_liquida IS NOT NULL )THEN
         LET v_filtros = v_filtros,"    AND folio_liquida = ",p_folio_liquida
      ELSE
         LET v_filtros = v_filtros,"    AND folio_liquida IS NULL"
      END IF
      IF( p_id_referencia IS NOT NULL )THEN
         LET v_filtros = v_filtros,"    AND id_referencia = ",p_id_referencia
      ELSE
         LET v_filtros = v_filtros,"    AND id_referencia IS NULL"
      END IF
      
   ELSE
      IF( p_id_referencia IS NOT NULL )THEN
         LET v_filtros = "id_derechohabiente = ",p_id_derechohabiente
      ELSE
         LET v_filtros = "id_derechohabiente IS NULL"
      END IF
      
      LET v_filtros = v_filtros,"    AND movimiento = ",p_movimiento

      IF( p_id_referencia IS NOT NULL )THEN
         LET v_filtros = v_filtros,"    AND id_referencia = ",p_id_referencia
      ELSE
         LET v_filtros = v_filtros,"    AND id_referencia IS NULL"
      END IF
   END IF
   LET v_variable_consulta = " "
   FOR v_contador = 1 TO p_ciclos
      CALL fn_recupera_nombre_tabla_movimientos_anio( p_anio_consulta ) RETURNING r_nom_tabla_cta_movimiento
      LET p_anio_consulta = p_anio_consulta + 1
      INITIALIZE v_existe TO NULL
      EXECUTE prp_cons_tabla USING r_nom_tabla_cta_movimiento
                              INTO v_existe
      IF( v_existe )THEN

         LET v_variable_consulta = v_variable_consulta,
                                   " SELECT f_liquida,",
                                   "        monto_acciones,",
                                   "        monto_pesos",
                                   "   FROM ",r_nom_tabla_cta_movimiento,
                                   "  WHERE ",v_filtros,
                                   " UNION"
      END IF
   END FOR
   LET v_variable_consulta = v_variable_consulta.subString(1,v_variable_consulta.getLength()-5)
   
--DISPLAY "v_variable_consulta:",v_variable_consulta
   
   IF( v_variable_consulta IS NOT NULL )THEN
      PREPARE prp_rec_mvimientos FROM v_variable_consulta
      EXECUTE prp_rec_mvimientos INTO r_f_liquida,
                                      r_monto_acciones,
                                      r_monto_pesos
   END IF

   RETURN r_f_liquida,
          r_monto_acciones,
          r_monto_pesos
END FUNCTION