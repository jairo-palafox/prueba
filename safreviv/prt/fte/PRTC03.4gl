--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/12/2013
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC02                                                     #
#Objetivo        => Consulta de traspasos                                      #
#Fecha Inicio    => 27 Febrero 2015                                            #
################################################################################
SCHEMA safre_viv

GLOBALS "PRTG01.4gl" 

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       p_nss_consulta    CHAR(11),
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_forma           ui.Form

MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)
   LET p_nss_consulta   = ARG_VAL(4)

   CALL fn_inicializa_consultas()
   CALL fn_filtra_consulta_traspasos(p_nss_consulta)
   
END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   CONNECT TO "safre_viv"
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_consulta = " DROP TABLE IF EXISTS tmp_prt_acreditados;",
                    " CREATE TEMP TABLE tmp_prt_acreditados",
                    " (nss         CHAR(11),",
                    "  nombre      VARCHAR(50),",
                    "  flujo       VARCHAR(20),",
                    "  folio       DECIMAL(9,0)",
                    "  )"
   PREPARE prp_crea_tbl_temp_consulta FROM v_consulta
   EXECUTE prp_crea_tbl_temp_consulta

   LET v_consulta = " SELECT cta.f_liquida,",
                    "        cta.folio_liquida,",
                    "        sub.subcuenta_desc,",
                    "        mov.movimiento_desc,",
                    "        cta.monto_acciones,",
                    "        cta.monto_pesos,",
                    "        cta.id_referencia",
                    "   FROM prt_preliquida pre JOIN cta_movimiento cta",
                    "     ON cta.id_derechohabiente = pre.id_derechohabiente",
                    "    AND cta.folio_liquida = pre.folio_liquida",
                    "        LEFT OUTER JOIN cat_subcuenta sub",
                    "     ON sub.subcuenta = cta.subcuenta",
                    "        LEFT OUTER JOIN cat_movimiento mov",
                    "     ON mov.movimiento = cta.movimiento",
                    "  WHERE cta.id_derechohabiente = ?",
                    "    AND cta.folio_liquida = ?"
   PREPARE prp_recupera_movimientos FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_glo_recupera_etiquetas(?,?)"
   PREPARE prp_rec_detalle_traspaso FROM v_consulta

END FUNCTION

# Descripción: Consulta traspasos de portabilidad (Cedente y receptora)
FUNCTION fn_filtra_consulta_traspasos(p_nss)
DEFINE p_nss LIKE prt_traspaso_cedente.nss,
       v_filtros RECORD
          v_nss     LIKE prt_traspaso_cedente.nss,
          v_nombre  LIKE prt_traspaso_cedente.nombre,
          v_paterno LIKE prt_traspaso_cedente.ap_paterno,
          v_materno LIKE prt_traspaso_cedente.ap_materno,
          v_flujo   SMALLINT,
          v_folio   LIKE prt_traspaso_cedente.folio_liquida
       END RECORD

   OPEN WINDOW vtna_con_traspasos WITH FORM v_ruta_ejecutable CLIPPED||"/PRTC031"

      INPUT v_filtros.* FROM nss_filtro,
                             nombre_filtro,
                             paterno_filtro,
                             materno_filtro,
                             flujo_filtro,
                             folio_filtro ATTRIBUTES( ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

            BEFORE INPUT
               --LET v_filtros.v_flujo = 1
               LET v_ventana = ui.Window.getCurrent()
               LET v_forma = v_ventana.getForm()               
               IF(p_titulo_ventana IS NOT NULL)THEN
                  CALL ui.Interface.setText(p_titulo_ventana)         
                  CALL v_ventana.setText(p_titulo_ventana)
               END IF
               IF( p_nss IS NOT NULL)THEN
                  LET v_filtros.v_nss = p_nss
                  CALL fn_consulta_traspasos(v_filtros.*) 
                  EXIT INPUT
               END IF
               
            ON ACTION consultar
               IF( v_filtros.v_nss IS NULL AND
                   v_filtros.v_nombre IS NULL AND
                   v_filtros.v_paterno IS NULL AND 
                   v_filtros.v_materno IS NULL AND
                   v_filtros.v_folio IS NULL )THEN

                  CALL fn_mensaje(p_titulo_ventana,"Capture algún filtro","information")
                  CONTINUE INPUT
               END IF
            
               CALL fn_consulta_traspasos(v_filtros.*) 

            ON ACTION cancelar
               DISCONNECT ALL
               EXIT INPUT

      END INPUT


   CLOSE WINDOW vtna_con_traspasos

END FUNCTION

# Descrición: muestra resultados de acreditados y traspasos
FUNCTION fn_consulta_traspasos(p_filtros)
DEFINE p_filtros RECORD
          v_nss     LIKE prt_traspaso_cedente.nss,
          v_nombre  LIKE prt_traspaso_cedente.nombre,
          v_paterno LIKE prt_traspaso_cedente.ap_paterno,
          v_materno LIKE prt_traspaso_cedente.ap_materno,
          v_flujo   SMALLINT,
          v_folio   LIKE prt_traspaso_cedente.folio_liquida
       END RECORD,
       v_acreditado DYNAMIC ARRAY OF RECORD
          v_id_derechohabiente LIKE prt_traspaso_cedente.id_derechohabiente,
          v_folio_liquida LIKE prt_traspaso_cedente.folio_liquida,
          v_nss    LIKE prt_traspaso_cedente.nss,
          v_nombre VARCHAR(50),
          v_flujo  VARCHAR(20),
          v_tabla  VARCHAR(50)
       END RECORD,
       v_movimientos DYNAMIC ARRAY OF RECORD
          v_f_liquida       DATE,
          v_folio           LIKE prt_traspaso_cedente.folio_liquida,
          v_subcuenta_desc  LIKE cat_subcuenta.subcuenta_desc,
          v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
          v_monto_aivs      LIKE cta_movimiento.monto_acciones,
          v_monto_pesos     LIKE cta_movimiento.monto_pesos,
          v_id_referencia   LIKE cta_movimiento.id_referencia
       END RECORD,
       v_detalle_traspaso DYNAMIC ARRAY OF RECORD
          v_campo VARCHAR(40),
          v_valor VARCHAR(40)
       END RECORD,
       v_indice_acreditado SMALLINT

   DIALOG ATTRIBUTES(UNBUFFERED)
         

      DISPLAY ARRAY v_acreditado TO sr_acreditado.*
         BEFORE ROW
            LET v_indice_acreditado = ARR_CURR()
            CALL fn_recupera_movimientos_trasp(v_acreditado[v_indice_acreditado].v_id_derechohabiente,
                                               v_acreditado[v_indice_acreditado].v_folio_liquida ) RETURNING v_movimientos

      END DISPLAY

      DISPLAY ARRAY v_movimientos TO sr_movimientos.*
         BEFORE ROW
            CALL fn_recupera_detalle_traspaso(v_acreditado[v_indice_acreditado].v_tabla,
                                              v_movimientos[ARR_CURR()].v_id_referencia) RETURNING v_detalle_traspaso

      END DISPLAY

      DISPLAY ARRAY v_detalle_traspaso TO sr_detalle_traspaso.*

      END DISPLAY

      BEFORE DIALOG
         CALL fn_recupera_acreditados(p_filtros.*) RETURNING v_acreditado
         IF( v_acreditado.getLength() = 0 )THEN
            CALL fn_mensaje(p_titulo_ventana,"No se encontraron registros con criterio dado","information")
            EXIT DIALOG
         END IF

      ON ACTION cancelar
         INITIALIZE v_acreditado,
                    v_movimientos,
                    v_detalle_traspaso TO NULL
         EXIT DIALOG
         
   END DIALOG
      
END FUNCTION

# Descripción: Filtra acreditados
FUNCTION fn_recupera_acreditados(p_filtros)
DEFINE p_filtros RECORD
          v_nss     LIKE prt_traspaso_cedente.nss,
          v_nombre  LIKE prt_traspaso_cedente.nombre,
          v_paterno LIKE prt_traspaso_cedente.ap_paterno,
          v_materno LIKE prt_traspaso_cedente.ap_materno,
          v_flujo   SMALLINT,
          v_folio   LIKE prt_traspaso_cedente.folio_liquida
       END RECORD,
       v_acreditados DYNAMIC ARRAY OF RECORD
          v_id_derechohabiente LIKE prt_traspaso_cedente.id_derechohabiente,
          v_folio_liquida LIKE prt_traspaso_cedente.folio_liquida,
          v_nss    LIKE prt_traspaso_cedente.nss,
          v_nombre VARCHAR(50),
          v_flujo  VARCHAR(20),
          v_tabla VARCHAR(50)
       END RECORD,
       v_consulta  STRING,
       v_cedente   STRING,
       v_receptora STRING,
       v_filtro    STRING,
       v_indice    INTEGER

   LET v_cedente = " SELECT id_derechohabiente,",
                   "        folio_liquida,",
                   "        nss,",
                   "        TRIM(nombre) ||' '||TRIM(ap_paterno)||' '||TRIM(ap_materno),",
                   "        'CEDENTE',",
                   "        'prt_traspaso_cedente'",
                   "   FROM prt_traspaso_cedente",
                   "  WHERE "

   LET v_receptora = " SELECT id_derechohabiente,",
                     "        folio_liquida,",
                     "        nss,",
                     "        TRIM(nombre)||' '||TRIM(ap_paterno)||' '||TRIM(ap_materno),",
                     "        'RECEPTORA',",
                     "        'prt_traspaso_receptora'",
                     "   FROM prt_traspaso_receptora",
                     "  WHERE "
   LET v_filtro = ""

   IF( p_filtros.v_nss IS NOT NULL AND p_filtros.v_nss <> " " )THEN
      LET v_filtro = "nss MATCHES '",p_filtros.v_nss CLIPPED,"' AND"
   END IF

   IF( p_filtros.v_nombre IS NOT NULL AND p_filtros.v_nombre <> " " )THEN
      LET v_filtro = "nombre MATCHES '",p_filtros.v_nombre CLIPPED,"' AND"
   END IF

   IF( p_filtros.v_paterno IS NOT NULL AND p_filtros.v_paterno <> " " )THEN
      LET v_filtro = v_filtro," ap_paterno MATCHES '",p_filtros.v_paterno CLIPPED,"' AND"
   END IF

   IF( p_filtros.v_materno IS NOT NULL AND p_filtros.v_materno <> " " )THEN
      LET v_filtro = v_filtro," ap_materno MATCHES '",p_filtros.v_materno CLIPPED,"' AND"
   END IF

   IF( p_filtros.v_folio IS NOT NULL AND p_filtros.v_folio <> 0 )THEN
      LET v_filtro = v_filtro," folio_liquida = ",p_filtros.v_folio CLIPPED," AND"
   END IF

   
   LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3) # elimina la última instrucción AND  
   --LET v_filtro = v_filtro," AND estado <> ? "

   CASE p_filtros.v_flujo

      WHEN 1 # Ambos
         LET v_consulta = --" INSERT INTO tmp_prt_acreditados",
                          "  SELECT FIRST 100 DISTINCT *",
                          "    FROM TABLE(MULTISET(",
                          v_receptora,
                          v_filtro,
                          " UNION",
                          v_cedente,
                          v_filtro,
                          "))ORDER BY nss"

      WHEN 2 # Cedente
         LET v_consulta = --" INSERT INTO tmp_prt_solicitudes",
                          v_cedente,
                          v_filtro,
                          "ORDER BY nss"

      WHEN 3 # Receptora
         LET v_consulta = --" INSERT INTO tmp_prt_solicitudes",
                          v_receptora,
                          v_filtro,
                          "ORDER BY nss"

   END CASE

   LET v_indice = 1
   --DISPLAY v_consulta
   PREPARE prp_recupera_registros FROM v_consulta
   DECLARE cur_recupera_registros CURSOR FOR prp_recupera_registros 
   --FOREACH cur_recupera_registros USING C_ESTADO_TRASPASO_RECHAZADO
   FOREACH cur_recupera_registros 
                                   INTO v_acreditados[v_indice].*
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_registros
   IF( v_acreditados[v_acreditados.getLength()].v_nss IS NULL )THEN
      CALL v_acreditados.deleteElement(v_acreditados.getLength())
   END IF

   RETURN v_acreditados
END FUNCTION

# Descripción: Recupera los movimientos de traspasos
FUNCTION fn_recupera_movimientos_trasp(p_id_derechohabiente,p_folio_liquida)
DEFINE p_id_derechohabiente LIKE cta_movimiento.id_derechohabiente,
       p_folio_liquida LIKE prt_traspaso_cedente.folio_liquida,
       v_movimientos DYNAMIC ARRAY OF RECORD
          v_f_liquida       DATE,
          v_folio  LIKE prt_traspaso_cedente.folio_liquida,
          v_subcuenta_desc  LIKE cat_subcuenta.subcuenta_desc,
          v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
          v_monto_aivs      LIKE cta_movimiento.monto_acciones,
          v_monto_pesos     LIKE cta_movimiento.monto_pesos,
          v_id_referencia   LIKE cta_movimiento.id_referencia
       END RECORD,
       v_indice SMALLINT

   LET v_indice = 1
   DECLARE cur_recupera_movimientos CURSOR FOR prp_recupera_movimientos
   FOREACH cur_recupera_movimientos USING p_id_derechohabiente,
                                          p_folio_liquida
                                     INTO v_movimientos[v_indice].*
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_movimientos
   IF( v_movimientos[v_movimientos.getLength()].v_f_liquida IS NULL )THEN
      CALL v_movimientos.deleteElement(v_movimientos.getLength())
   END IF

   RETURN v_movimientos

END FUNCTION

# Descripción: 
FUNCTION fn_recupera_detalle_traspaso(p_tabla,p_id_registro)
DEFINE p_tabla       VARCHAR(50),
       p_id_registro DECIMAL(9,0),
       v_tmp_detalle_solicitud RECORD
          v_ind        SMALLINT, 
          v_diag       CHAR(3),
          v_sql_error  INTEGER,
          v_isam_error INTEGER,
          v_msg_error  VARCHAR(100),
          v_etiqueta   VARCHAR(50),
          v_valor      VARCHAR(80)
       END RECORD,
       v_detalle_traspaso DYNAMIC ARRAY OF RECORD
          v_campo VARCHAR(40),
          v_valor VARCHAR(40)
       END RECORD,
       v_indice INTEGER

   LET v_indice = 1
   DECLARE cur_rec_detalle_traspaso CURSOR FOR prp_rec_detalle_traspaso
   FOREACH cur_rec_detalle_traspaso USING p_tabla,
                                          p_id_registro
                                     INTO v_tmp_detalle_solicitud.*
      LET v_detalle_traspaso[v_indice].v_campo = v_tmp_detalle_solicitud.v_etiqueta
      LET v_detalle_traspaso[v_indice].v_valor = v_tmp_detalle_solicitud.v_valor
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_detalle_traspaso
   
   RETURN v_detalle_traspaso
END FUNCTION