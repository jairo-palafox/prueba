--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16/07/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC05                                                     #
#Objetivo        => Consulta cifras control y traspasos                        #
#Fecha Inicio    => 17 Junio 2015                                              #
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

   CONNECT TO "safre_viv"
   CALL fn_inicializa_consultas()
   CALL fn_filtra_consulta()
   DISCONNECT "safre_viv"

END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING,
       v_existe   BOOLEAN

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

   DISCONNECT "safre_viv"
   CONNECT TO "safre_tmp"
   
   # Los prepare se reinician al cambiar de conexión de bd
   
   LET v_consulta = " CREATE TABLE prt_tmp_con_trp_prt_subsec_ced(
                         id_solicitud DECIMAL(9,0),
                         nss     CHAR(11),
                         curp    CHAR(18),
                         credito CHAR(11),
                         movimiento    CHAR(40),
                         aivs           DECIMAL(22,2),
                         pesos          DECIMAL(22,2),
                         fecha_proceso DATE)"
   PREPARE prp_crea_tmp_ced_ini FROM v_consulta

   LET v_consulta = " CREATE TABLE prt_tmp_con_trp_prt_subsec_rec(
                         id_solicitud DECIMAL(9,0),
                         nss     CHAR(11),
                         curp    CHAR(18),
                         credito CHAR(11),
                         movimiento    CHAR(40),
                         aivs           DECIMAL(22,2),
                         pesos          DECIMAL(22,2),
                         fecha_proceso DATE)"
   PREPARE prp_crea_tmp_rec_ini FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 1",
                    "   FROM systables",
                    " WHERE tabname = ?"
   PREPARE prp_cons_tabla_ini FROM v_consulta
   
   EXECUTE prp_cons_tabla_ini USING "prt_tmp_con_trp_prt_subsec_ced"
                               INTO v_existe
   IF NOT( v_existe )THEN
      EXECUTE prp_crea_tmp_ced_ini
   END IF
   LET v_existe = FALSE;
   EXECUTE prp_cons_tabla_ini USING "prt_tmp_con_trp_prt_subsec_rec"
                               INTO v_existe
   IF NOT( v_existe )THEN
      EXECUTE prp_crea_tmp_rec_ini
   END IF
   
   DISCONNECT "safre_tmp"
   CONNECT TO "safre_viv"
   
END FUNCTION 

# Descripción: Filtra consulta de cifras control de procesar
FUNCTION fn_filtra_consulta()
DEFINE v_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE
       END RECORD

   OPEN WINDOW vtna_con_prt WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC071"

      INPUT v_filtros.* FROM filtro_flujo,
                             filtro_nss,
                             filtro_curp,
                             filtro_credito,
                             filtro_f_ini,
                             filtro_f_fin ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE, CANCEL=FALSE)
         BEFORE INPUT
            LET g_ventana = ui.Window.getCurrent()
            LET g_forma = g_ventana.getForm()               
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)         
               CALL g_ventana.setText(p_titulo_ventana)
            END IF

         ON ACTION consultar
            IF( v_filtros.v_f_marca_ini > v_filtros.v_f_marca_fin )THEN
                CALL fn_mensaje(p_titulo_ventana,"Fecha inicio no puede ser mayor a fecha fin","information")
                CONTINUE INPUT
            END IF
            IF( v_filtros.v_flujo = 1 )THEN
               CALL g_forma.setElementHidden("gpo_mov_subsecuentes",FALSE)
               CALL g_forma.setElementText("formonly.credito","No. Crédito\nFovissste")               
               CALL fn_consulta_solicitud(v_filtros.*)
               CALL g_forma.setElementText("formonly.credito","No. Crédito")
               CALL g_forma.setElementHidden("gpo_mov_subsecuentes",TRUE)
            ELSE
               CALL g_forma.setElementHidden("gpo_mov_subsecuentes",FALSE)
               CALL g_forma.setElementText("formonly.credito","No. Crédito\nInfonavit")
               CALL fn_consulta_solicitud(v_filtros.*)
               CALL g_forma.setElementText("formonly.credito","No. Crédito")
               CALL g_forma.setElementHidden("gpo_mov_subsecuentes",TRUE)
            END IF

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_con_prt

END FUNCTION

# Descripción: Consulta estado de solicitud y saldos de traspaso
FUNCTION fn_consulta_solicitud(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE
       END RECORD,
       r_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
       END RECORD,
       r_confirma BOOLEAN,
       r_resultado_opera INTEGER,
       r_error BOOLEAN

   DISPLAY ARRAY r_registros TO sr_registros.* ATTRIBUTES( UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

      BEFORE DISPLAY
         IF( p_filtros.v_flujo = 1 )THEN
            CALL fn_recupera_registros_cedente(p_filtros.*) RETURNING r_registros
            CALL g_forma.setElementText("formonly.pesos","PESOS transferidos a Fovissste")
         ELSE
            CALL fn_recupera_registros_receptora(p_filtros.*) RETURNING r_registros
            CALL g_forma.setElementText("formonly.pesos","PESOS recibidos")
         END IF 
         
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
               IF( p_filtros.v_flujo = 1 )THEN
                  CALL fn_genera_reportes_cedente(r_registros) RETURNING r_error
               ELSE
                  CALL fn_genera_reportes_receptora(r_registros) RETURNING r_error
               END IF
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
FUNCTION fn_recupera_registros_cedente(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE
       END RECORD,
       v_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
       END RECORD,
       r_nom_tabla_cta_movimiento STRING,
       r_anio_consulta STRING,
       r_ciclos   SMALLINT,
       v_contador SMALLINT,
       v_indice   INTEGER,
       v_consulta STRING,
       v_filtro   STRING

   CALL fn_recupera_ciclos_fechas(p_filtros.v_f_marca_ini,
                                  p_filtros.v_f_marca_fin) RETURNING r_anio_consulta, r_ciclos

   FOR v_contador = 1 TO r_ciclos
      CALL fn_recupera_nombre_tabla_movimientos_anio( r_anio_consulta ) RETURNING r_nom_tabla_cta_movimiento
      LET r_anio_consulta = r_anio_consulta + 1
      
      LET v_filtro = "1=1 AND"
      LET v_consulta = " SELECT sol.id_prt_solicitud_cedente,",
                       "        sol.nss,",
                       "        sol.curp,",
                       "        sol.id_credito_fovissste,",
                       "        cat.movimiento_desc,",
                       "        cta.monto_acciones,",
                       "        cta.monto_pesos,",
                       "        cta.f_liquida",
                       "   FROM prt_solicitud_cedente sol",
                       "        JOIN afi_derechohabiente afi",
                       "     ON afi.nss = sol.nss",                    
                       "        JOIN prt_traspaso_cedente trp",
                       --"     ON trp.tpo_operacion IN (?,?)",
                       "     ON trp.id_prt_solicitud_cedente = sol.id_prt_solicitud_cedente",

                       "        JOIN prt_cza_cedente cza",
                       "     ON trp.folio_liquida = cza.folio_liquida",
                       "    AND cza.tipo_traspaso IN (?,?)",
                    
                       --"        JOIN cta_movimiento cta",
                       "        JOIN ",r_nom_tabla_cta_movimiento," cta",
                       "     ON cta.id_derechohabiente = afi.id_derechohabiente",
                       "    AND cta.id_referencia = trp.id_prt_traspaso_cedente",
                       "    AND cta.folio_liquida = trp.folio_liquida",
                       "    AND cta.movimiento = ?",
                       "        LEFT OUTER JOIN cat_movimiento cat",
                       "     ON cat.movimiento = cta.movimiento",                    
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
            LET v_filtro = v_filtro || " cta.f_liquida between '"||p_filtros.v_f_marca_ini||"' AND '"||p_filtros.v_f_marca_fin||"' AND"
         ELSE
            LET v_filtro = v_filtro || " cta.f_liquida = '"||p_filtros.v_f_marca_ini||"' AND"
         END IF      
      ELSE
         IF( p_filtros.v_f_marca_fin IS NOT NULL )THEN
            LET v_filtro = v_filtro || " cta.f_liquida = '"||p_filtros.v_f_marca_fin||"' AND"      
         END IF
      END IF

      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3)

      LET v_consulta = v_consulta||v_filtro|| " ORDER BY sol.id_prt_solicitud_cedente"

      --DISPLAY v_consulta 
      LET v_indice = v_registros.getLength() + 1 
      PREPARE prp_consulta_registros FROM v_consulta
      DECLARE cur_consulta_registros CURSOR FOR prp_consulta_registros
      FOREACH cur_consulta_registros USING C_TIPO_TRASPASO_SUBSECUENTE,
                                           C_TIPO_TRASPASO_DEVOLUCION,
                                           C_MOV_CED_CARGO_SUB
                                      INTO v_registros[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH
      FREE cur_consulta_registros
      IF( v_registros[v_registros.getLength()].v_nss IS NULL)THEN
         CALL v_registros.deleteElement(v_registros.getLength())
      END IF
   END FOR

   RETURN v_registros
END FUNCTION

# Descripción: Especifica parametros y construcción de reporte
FUNCTION fn_genera_reportes_cedente(p_registros)
DEFINE p_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
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
                               "PRTC07",
                               "NA",
                               p_usuario_cod) RETURNING r_resultado_opera
                               
   IF( r_resultado_opera = 0 )THEN
         CALL fn_actualiza_opera_ini(g_pid,
                                     C_PROCESO_COD_REPORTES_PRT,
                                     C_OPERA_COD_CARGA,
                                     0,
                                     "PRTC07",
                                     "NA",
                                     p_usuario_cod) RETURNING r_resultado_opera 
      IF( r_resultado_opera = 0 )THEN
         DISCONNECT "safre_viv"
         CONNECT TO "safre_tmp"
         LET v_consulta = " TRUNCATE TABLE prt_tmp_con_trp_prt_subsec_ced"
         PREPARE prp_crea_tmp_ced FROM v_consulta
         EXECUTE prp_crea_tmp_ced
         LET v_consulta = "INSERT INTO prt_tmp_con_trp_prt_subsec_ced VALUES(?,?,?,?,?,?,?,?)"
         PREPARE prp_ins_tmp_ced FROM v_consulta
         
         FOR v_indice = 1 TO p_registros.getLength()
            EXECUTE prp_ins_tmp_ced USING p_registros[v_indice].*
         END FOR
            
         DISCONNECT "safre_tmp"
         CONNECT TO "safre_viv"

         LET v_proceso_txt = C_PROCESO_COD_REPORTES_PRT 
         LET v_opera_txt   = C_OPERA_COD_CARGA
         LET v_pid_txt     = g_pid
         LET v_comando = "nohup fglrun ",g_ruta_ejecutable CLIPPED,"/PRTI07.42r '",p_usuario_cod CLIPPED, "' ",
                                                                                      g_pid CLIPPED," ",
                                                                                      C_PROCESO_COD_REPORTES_PRT," ",
                                                                                      C_OPERA_COD_CARGA," ",
                                                                                      "0 ",
                                                                                      "'NA' ",
                                                                                      "1 ",
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

# Descripción: Consulta estado de solicitud y saldos de traspaso
FUNCTION fn_consulta_solicitud_receptora(p_filtros)
DEFINE p_filtros RECORD
          v_flujo   SMALLINT,
          v_nss     VARCHAR(11),
          v_curp    VARCHAR(18),
          v_credito VARCHAR(11),
          v_f_marca_ini DATE,
          v_f_marca_fin DATE
       END RECORD,
       r_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento    LIKE cat_movimiento.movimiento_desc,
          v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
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
          v_f_marca_fin DATE
       END RECORD,
       v_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
       END RECORD,
       r_nom_tabla_cta_movimiento STRING,
       r_anio_consulta STRING,
       r_ciclos   SMALLINT,
       v_contador SMALLINT,
       v_indice   INTEGER,
       v_consulta STRING,
       v_filtro   STRING

   CALL fn_recupera_ciclos_fechas(p_filtros.v_f_marca_ini,
                                  p_filtros.v_f_marca_fin) RETURNING r_anio_consulta, r_ciclos

   FOR v_contador = 1 TO r_ciclos
      CALL fn_recupera_nombre_tabla_movimientos_anio( r_anio_consulta ) RETURNING r_nom_tabla_cta_movimiento
      LET r_anio_consulta = r_anio_consulta + 1
      
      LET v_filtro = "1=1 AND"
      LET v_consulta = " SELECT sol.id_prt_solicitud_receptora,",
                       "        sol.nss,",
                       "        sol.curp,",
                       "        sol.id_credito_infonavit,",
                       "        cat.movimiento_desc,",
                       "        cta.monto_acciones,",
                       "        cta.monto_pesos,",
                       "        cta.f_liquida",
                       "   FROM prt_solicitud_receptora sol",
                       "        JOIN afi_derechohabiente afi",
                       "     ON afi.nss = sol.nss",                    
                       "        JOIN prt_traspaso_receptora trp",
                       --"     ON trp.tpo_operacion IN (?,?)",
                       "      ON trp.id_prt_solicitud_receptora = sol.id_prt_solicitud_receptora",

                       "        JOIN prt_cza_receptora cza",
                       "     ON trp.folio_liquida = cza.folio_liquida",
                       "    AND cza.tipo_traspaso IN (?,?)",
                    
                       "        JOIN ",r_nom_tabla_cta_movimiento," cta",
                       "     ON cta.id_derechohabiente = afi.id_derechohabiente",
                       "    AND cta.id_referencia = trp.id_prt_traspaso_receptora",                    
                       "    AND cta.folio_liquida = trp.folio_liquida",
                       "    AND cta.movimiento = ?",
                       "        LEFT OUTER JOIN cat_movimiento cat",
                       "     ON cat.movimiento = cta.movimiento",
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
            LET v_filtro = v_filtro || " cta.f_liquida between '"||p_filtros.v_f_marca_ini||"' AND '"||p_filtros.v_f_marca_fin||"' AND"
         ELSE
            LET v_filtro = v_filtro || " cta.f_liquida = '"||p_filtros.v_f_marca_ini||"' AND"
         END IF      
      ELSE
         IF( p_filtros.v_f_marca_fin IS NOT NULL )THEN
            LET v_filtro = v_filtro || " cta.f_liquida = '"||p_filtros.v_f_marca_fin||"' AND"      
         END IF
      END IF
   
      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3)
   
      LET v_consulta = v_consulta||v_filtro|| " ORDER BY sol.id_prt_solicitud_receptora"

      --DISPLAY v_consulta
      LET v_indice = v_registros.getLength() + 1
      PREPARE prp_consulta_registros_rec FROM v_consulta
      DECLARE cur_consulta_registros_rec CURSOR FOR prp_consulta_registros_rec
      FOREACH cur_consulta_registros_rec USING C_TIPO_TRASPASO_SUBSECUENTE,
                                               C_TIPO_TRASPASO_DEVOLUCION,
                                               C_MOV_REC_ABONO_SUBSECUENTE
                                          INTO v_registros[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH
      FREE cur_consulta_registros_rec
      IF( v_registros[v_registros.getLength()].v_nss IS NULL)THEN
         CALL v_registros.deleteElement(v_registros.getLength())
      END IF
   END FOR
   RETURN v_registros
END FUNCTION

# Descripción: Especifica parametros y construcción de reporte
FUNCTION fn_genera_reportes_receptora(p_registros)
DEFINE p_registros DYNAMIC ARRAY OF RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
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
                               "PRTC07",
                               "NA",
                               p_usuario_cod) RETURNING r_resultado_opera
                               
   IF( r_resultado_opera = 0 )THEN
         CALL fn_actualiza_opera_ini(g_pid,
                                     C_PROCESO_COD_REPORTES_PRT,
                                     C_OPERA_COD_CARGA,
                                     0,
                                     "PRTC07",
                                     "NA",
                                     p_usuario_cod) RETURNING r_resultado_opera 
      IF( r_resultado_opera = 0 )THEN
         DISCONNECT "safre_viv"
         CONNECT TO "safre_tmp"
         LET v_consulta = " TRUNCATE TABLE prt_tmp_con_trp_prt_subsec_rec"
         PREPARE prp_crea_tmp_rec FROM v_consulta
         EXECUTE prp_crea_tmp_rec

         LET v_consulta = "INSERT INTO prt_tmp_con_trp_prt_subsec_rec VALUES(?,?,?,?,?,?,?,?)"
         PREPARE prp_ins_tmp_rec FROM v_consulta
   
         FOR v_indice = 1 TO p_registros.getLength()
            EXECUTE prp_ins_tmp_rec USING p_registros[v_indice].*
         END FOR
            
         DISCONNECT "safre_tmp"
         CONNECT TO "safre_viv"

         LET v_proceso_txt = C_PROCESO_COD_REPORTES_PRT 
         LET v_opera_txt   = C_OPERA_COD_CARGA
         LET v_pid_txt     = g_pid
         LET v_comando = "nohup fglrun ",g_ruta_ejecutable CLIPPED,"/PRTI07.42r '",p_usuario_cod CLIPPED, "' ",
                                                                                      g_pid CLIPPED," ",
                                                                                      C_PROCESO_COD_REPORTES_PRT," ",
                                                                                      C_OPERA_COD_CARGA," ",
                                                                                      "0 ",
                                                                                      "'NA' ",
                                                                                      "2 ", # Receptora
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
         # si las fechas son nulas, el año a consultar, seraá desde que empezó a operar portabilidad
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