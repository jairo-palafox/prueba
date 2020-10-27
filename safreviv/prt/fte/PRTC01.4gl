--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificación: 04/01/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTC01                                                        #
#Objetivo     => Consulta registros de procedencia de solicitudes              #
#Fecha inicio => 19 Febrero 2015                                               #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion   SMALLINT,
       p_titulo_vtna      STRING,
       v_ruta_ejecutables LIKE seg_modulo.ruta_bin,
       v_ventana          ui.Window,
       v_forma            ui.Form

MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   SELECT ruta_bin
     INTO v_ruta_ejecutables
     FROM seg_modulo
    WHERE modulo_cod = "prt"
    
   CALL fn_inicializa_consultas()
   CALL fn_consulta_solicitudes_prt()

END MAIN

FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " DROP TABLE IF EXISTS tmp_prt_solicitudes;",
                    " CREATE TEMP TABLE tmp_prt_solicitudes",
                    " (tabla       VARCHAR(50),",
                    "  tabla_his   VARCHAR(50),",
                    "  id_registro DECIMAL(9,0),",
                    "  nss         CHAR(11),",
                    "  flujo       CHAR(10),",
                    "  resultado_operacion CHAR(2),",
                    "  diagnostico CHAR(3)",
                    "  )"
   PREPARE prp_crea_tbl_temp_consulta FROM v_consulta
   EXECUTE prp_crea_tbl_temp_consulta

   LET v_consulta = " DELETE FROM tmp_prt_solicitudes WHERE 1 = 1"
   PREPARE prp_genera_tbl_temp FROM v_consulta
   
   LET v_consulta = " SELECT tabla,",
                    "        tabla_his,",
                    "        nss,",
                    "        flujo,",
                    "        COUNT(*)",
                    "   FROM tmp_prt_solicitudes",
                    "  WHERE 1 = 1",
                    "  GROUP BY tabla,tabla_his,nss,flujo"
   PREPARE prp_rec_acreditados FROM v_consulta

   LET v_consulta = " SELECT DISTINCT tmp.id_registro,",
                    "        tmp.nss,",
                    "        tmp.resultado_operacion,",
                    "        NVL(res.descripcion,'NO ESPECIFICADO'),",
                    "        dig.descripcion_general",
                    "   FROM tmp_prt_solicitudes tmp JOIN prt_entidad_diagnostico ent",
                    "     ON ent.entidad = tmp.tabla ",
                    "        JOIN prt_diagnostico dig",
                    "     ON dig.destino_diagnostico = ent.destino_diagnostico",
                    "    AND dig.diagnostico_interno = tmp.diagnostico",
                    "        JOIN prt_cat_resultado_operacion res",
                    "     ON res.resultado_operacion = tmp.resultado_operacion",
                    "  WHERE tmp.nss = ?",
                    "    AND tmp.tabla = ?",
                    "  ORDER BY tmp.id_registro DESC"
   PREPARE prp_rec_registros_acreditados FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_glo_recupera_etiquetas(?,?)"
   PREPARE prp_rec_detalle_solicitud FROM v_consulta

   LET v_consulta = " EXECUTE FUNCTION fn_glo_recupera_historia(?,?)"
   PREPARE prp_rec_historia_solicitud FROM v_consulta

END FUNCTION

# Decripción: Función para el filtrado y consulta de solicitudes de portabilidad
FUNCTION fn_consulta_solicitudes_prt()
DEFINE v_filtros RECORD
          v_nss      LIKE prt_solicitud_cedente.nss,
          v_curp     LIKE prt_solicitud_cedente.curp,
          v_nombre   LIKE prt_solicitud_cedente.nombre,
          v_paterno  LIKE prt_solicitud_cedente.paterno,
          v_materno  LIKE prt_solicitud_cedente.materno,
          v_f_inicio DATE,
          v_f_fin    DATE,
          v_flujo    SMALLINT
       END RECORD

   OPEN WINDOW vtna_consulta_solicitud WITH FORM v_ruta_ejecutables CLIPPED||"/PRTC011"
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm()
      IF(p_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_vtna)         
         CALL v_ventana.setText(p_titulo_vtna)
      END IF
      
      INPUT v_filtros.* FROM captura_nss,
                             captura_curp,
                             captura_nombre,
                             captura_paterno,
                             captura_materno,
                             captura_f_inicio,
                             captura_f_fin,
                             rdog_flujo ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE INPUT
            # Oculta grupos de consulta 
            CALL v_forma.setElementHidden("gpo_acreditado",TRUE)
            CALL v_forma.setElementHidden("gpo_registros",TRUE)
            CALL v_forma.setElementHidden("gpo_detalle_solicitud",TRUE)
            CALL v_forma.setElementHidden("gpo_historico",TRUE)
         
         ON ACTION aceptar
            IF(v_filtros.v_f_inicio > v_filtros.v_f_fin)THEN
               CALL fn_mensaje(p_titulo_vtna,"Fecha inicial no puede ser mayor a fecha fin","information")
               NEXT FIELD captura_f_inicio
            END IF
            
            # Elimina registros temporales de la consulta
            EXECUTE prp_genera_tbl_temp
            # Construye filtro para recuperar datos según lo ingresado
            CALL fn_genera_filtro(v_filtros.*)
            CALL fn_consulta_acreditado()
            CALL v_forma.setElementHidden("gpo_acreditado",TRUE)
            CALL v_forma.setElementHidden("gpo_registros",TRUE)
            CALL v_forma.setElementHidden("gpo_detalle_solicitud",TRUE)
            CALL v_forma.setElementHidden("gpo_historico",TRUE)
            

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_consulta_solicitud

END FUNCTION

# Descripción: Genera el filtro según lo capturado
FUNCTION fn_genera_filtro(p_filtros)
DEFINE p_filtros RECORD
          v_nss      LIKE prt_solicitud_cedente.nss,
          v_curp     LIKE prt_solicitud_cedente.curp,
          v_nombre   LIKE prt_solicitud_cedente.nombre,
          v_paterno  LIKE prt_solicitud_cedente.paterno,
          v_materno  LIKE prt_solicitud_cedente.materno,
          v_f_inicio DATE,
          v_f_fin    DATE,
          v_flujo    SMALLINT
       END RECORD,
       v_consulta  STRING,
       v_receptora STRING,
       v_cedente   STRING,
       v_filtro    STRING       
   
   LET v_receptora = " SELECT 'prt_solicitud_receptora',",
                     "        'prt_his_solicitud_receptora',",
                     "        id_prt_solicitud_receptora,",
                     "        nss,",
                     "        'RECEPTORA',",
                     "        resultado_operacion,",
                     "        diagnostico_interno",
                     "   FROM prt_solicitud_receptora",
                     "  WHERE "

   LET v_cedente = " SELECT 'prt_solicitud_cedente',",
                   "        'prt_his_solicitud_cedente',",
                   "        id_prt_solicitud_cedente,",
                   "        nss,",
                   "        'CEDENTE',",
                   "        resultado_operacion,",
                   "        diagnostico_interno",
                   "   FROM prt_solicitud_cedente",
                   "  WHERE "
   LET v_filtro = " "

   IF(p_filtros.v_nss IS NOT NULL AND p_filtros.v_nss <> " ")THEN
      LET v_filtro = "nss MATCHES '",p_filtros.v_nss CLIPPED,"' AND"
   END IF

   IF(p_filtros.v_curp IS NOT NULL AND p_filtros.v_curp <> " ")THEN
      LET v_filtro = v_filtro," curp MATCHES '",p_filtros.v_curp CLIPPED,"' AND"
   END IF

   IF(p_filtros.v_nombre IS NOT NULL AND p_filtros.v_nombre <> " ")THEN
      LET v_filtro = v_filtro," nombre MATCHES '",p_filtros.v_nombre CLIPPED,"' AND"
   END IF

   IF(p_filtros.v_paterno IS NOT NULL AND p_filtros.v_paterno <> " ")THEN
      LET v_filtro = v_filtro," paterno MATCHES '",p_filtros.v_paterno CLIPPED,"' AND"
   END IF
   
   IF(p_filtros.v_materno IS NOT NULL AND p_filtros.v_materno <> " ")THEN
      LET v_filtro = v_filtro," materno MATCHES '",p_filtros.v_materno CLIPPED,"' AND"
   END IF

   IF(p_filtros.v_f_inicio IS NOT NULL)THEN
      IF(p_filtros.v_f_fin IS NOT NULL)THEN
         LET v_filtro = v_filtro," f_consulta_credito BETWEEN '",p_filtros.v_f_inicio,"' AND '",p_filtros.v_f_fin,"' AND"
      ELSE
         LET v_filtro = v_filtro," f_consulta_credito = '",p_filtros.v_f_inicio,"' AND"
      END IF
   ELSE
      IF(p_filtros.v_f_fin IS NOT NULL)THEN
         LET v_filtro = v_filtro," f_consulta_credito = '",p_filtros.v_f_fin,"' AND"
      END IF
   END IF

   IF(v_filtro = " ")THEN
      LET v_filtro = " 1 = 1 "
   ELSE
      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3) # elimina la última instrucción AND      
   END IF
   
   CASE p_filtros.v_flujo

      WHEN 1 # Consulta tanto receptora como cedente
         LET v_consulta = " INSERT INTO tmp_prt_solicitudes",
                          "  SELECT FIRST 100 *",
                          "    FROM TABLE(MULTISET(",
                          v_receptora,
                          v_filtro,
                          " UNION",
                          v_cedente,
                          v_filtro,
                          "))ORDER BY nss"

      WHEN 2 # Consulta sólo en Receptora
         LET v_consulta = " INSERT INTO tmp_prt_solicitudes",
                          v_receptora,
                          v_filtro,
                          "ORDER BY nss"

      WHEN 3 # Consulta sólo en Cedente
         LET v_consulta = " INSERT INTO tmp_prt_solicitudes",
                          v_cedente,
                          v_filtro,
                          "ORDER BY nss"

   END CASE
   --DISPLAY v_consulta
   PREPARE prp_recupera_registros FROM v_consulta
   EXECUTE prp_recupera_registros

END FUNCTION

# Descripción: Recupera y consulta los derechohabientes
FUNCTION fn_consulta_acreditado()
DEFINE v_acreditados DYNAMIC ARRAY OF RECORD
          v_tabla         VARCHAR(50),
          v_tabla_his     VARCHAR(50),
          v_nss           LIKE prt_solicitud_cedente.nss,
          v_flujo         CHAR(10),
          v_coinsidencias INTEGER,
          v_detalle       CHAR
       END RECORD,
       v_detalle_acreditado DYNAMIC ARRAY OF RECORD
          v_id_registro         DECIMAL(9,0),
          v_nss                 CHAR(11),
          v_resultado_operacion CHAR(2),
          v_resultado_operacion_desc VARCHAR(40),
          v_diagnostico         LIKE prt_diagnostico.descripcion_general
       END RECORD,
       v_detalle_solicitud DYNAMIC ARRAY OF RECORD
          v_etiqueta VARCHAR(50),
          v_valor    VARCHAR(80)
       END RECORD,
       v_historia_solicitud DYNAMIC ARRAY OF RECORD
          v_etiqueta         VARCHAR(50),
          v_f_modificacion   DATE,
          v_valor_modificado VARCHAR(50),
          v_valor_actual     VARCHAR(50)
       END RECORD,
       v_tmp_tabla     VARCHAR(50),
       v_tmp_tabla_his VARCHAR(50),
       v_comando       STRING

       
   DIALOG ATTRIBUTES(UNBUFFERED)
      INPUT ARRAY v_acreditados FROM sr_acreditado.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)

         BEFORE INPUT
            CALL v_forma.setElementHidden("gpo_registros",FALSE)

         BEFORE ROW
            CALL v_detalle_acreditado.clear()
            CALL v_detalle_solicitud.clear()
            CALL v_historia_solicitud.clear()
            --CALL v_forma.setElementHidden("gpo_detalle_solicitud",TRUE)
            --CALL v_forma.setElementHidden("gpo_historico",TRUE)
            LET v_tmp_tabla = v_acreditados[ARR_CURR()].v_tabla
            LET v_tmp_tabla_his = v_acreditados[ARR_CURR()].v_tabla_his
            CALL fn_recupera_registros_acreditado(v_tmp_tabla,
                                                  v_acreditados[ARR_CURR()].v_nss) RETURNING v_detalle_acreditado

         ON ACTION consulta_tramite
            INITIALIZE v_comando TO NULL
            LET v_comando = "fglrun ",v_ruta_ejecutables CLIPPED,"/PRTC02.42r ",p_usuario_cod," ",
                                                                                p_tipo_ejecucion," ",
                                                                                p_titulo_vtna," ",
                                                                                v_acreditados[ARR_CURR()].v_nss
            RUN v_comando
                             
      END INPUT

      DISPLAY ARRAY v_detalle_acreditado TO sr_registros.*
         BEFORE DISPLAY
            --CALL v_forma.setElementHidden("gpo_detalle_solicitud",FALSE)
            --CALL v_forma.setElementHidden("gpo_historico",FALSE)

         BEFORE ROW
            CALL v_detalle_solicitud.clear()
            CALL fn_recupera_detalle_solicitud(v_tmp_tabla,
                                               v_detalle_acreditado[ARR_CURR()].v_id_registro) RETURNING v_detalle_solicitud
            CALL v_historia_solicitud.clear()
            CALL fn_recupera_historia_solicitud(v_tmp_tabla_his,
                                                v_detalle_acreditado[ARR_CURR()].v_id_registro) RETURNING v_historia_solicitud

      END DISPLAY

      DISPLAY ARRAY v_detalle_solicitud TO sr_detalle_solicitud.*
         
      END DISPLAY

      DISPLAY ARRAY v_historia_solicitud TO sr_historico.* 
         
      END DISPLAY

      ON ACTION cancelar
         CALL v_acreditados.clear()
         CALL v_detalle_acreditado.clear()
         CALL v_detalle_solicitud.clear()
         CALL v_historia_solicitud.clear()
         
         EXIT DIALOG

      BEFORE DIALOG
         CALL v_forma.setElementHidden("gpo_acreditado",FALSE)
         CALL v_forma.setElementHidden("gpo_registros",FALSE)
         CALL v_forma.setElementHidden("gpo_detalle_solicitud",FALSE)
         CALL v_forma.setElementHidden("gpo_historico",FALSE)
         
         CALL fn_recupera_acreditado() RETURNING v_acreditados
            IF(v_acreditados.getLength() = 0)THEN
               CALL fn_mensaje(p_titulo_vtna,"No se encontraron registros con criterio dado","information")
               EXIT DIALOG
            END IF
        
   END DIALOG

END FUNCTION

# Descripción: recupera los registros de acreditados
FUNCTION fn_recupera_acreditado()
DEFINE v_acreditados DYNAMIC ARRAY OF RECORD
          v_tabla         VARCHAR(50),
          v_tabla_his     VARCHAR(50),
          v_nss           LIKE prt_solicitud_cedente.nss,
          v_flujo         CHAR(10),
          v_coinsidencias INTEGER,
          v_detalle       CHAR
       END RECORD,
       v_acreditado RECORD
          v_tabla         VARCHAR(50),
          v_tabla_his     VARCHAR(50),
          v_nss           LIKE prt_solicitud_cedente.nss,
          v_flujo         CHAR(10),
          v_coinsidencias INTEGER
       END RECORD,
       v_indice INTEGER

   LET v_indice = 1
   
   DECLARE cur_rec_acreditados CURSOR FOR prp_rec_acreditados 
   FOREACH cur_rec_acreditados INTO v_acreditado.*
      LET v_acreditados[v_indice].v_tabla         = v_acreditado.v_tabla
      LET v_acreditados[v_indice].v_tabla_his     = v_acreditado.v_tabla_his
      LET v_acreditados[v_indice].v_nss           = v_acreditado.v_nss
      LET v_acreditados[v_indice].v_flujo         = v_acreditado.v_flujo
      LET v_acreditados[v_indice].v_coinsidencias = v_acreditado.v_coinsidencias
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_acreditados

   RETURN v_acreditados
END FUNCTION

# Descripción: Recupera los registros del NSS según el flujo
FUNCTION fn_recupera_registros_acreditado(p_tabla,p_nss)
DEFINE p_tabla    VARCHAR(50),
       p_nss      LIKE prt_solicitud_cedente.nss,
       v_detalle_acreditado DYNAMIC ARRAY OF RECORD
          v_id_registro              DECIMAL(9,0),
          v_nss                      CHAR(11),
          v_resultado_operacion      CHAR(2),
          v_resultado_operacion_desc VARCHAR(40),
          v_diagnostico              LIKE prt_diagnostico.descripcion_general
       END RECORD,
       v_indice INTEGER

   LET v_indice = 1

   DECLARE cur_rec_registros_acreditado CURSOR FOR prp_rec_registros_acreditados
   FOREACH cur_rec_registros_acreditado USING p_nss,
                                              p_tabla
                                         INTO v_detalle_acreditado[v_indice].*
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_registros_acreditado

   IF(v_detalle_acreditado[v_detalle_acreditado.getLength()].v_id_registro IS NULL)THEN
      CALL v_detalle_acreditado.deleteElement(v_detalle_acreditado.getLength())
   END IF

   RETURN v_detalle_acreditado
END FUNCTION

# Descripción: Recupera detalle de la solicitud por funciones globales
FUNCTION fn_recupera_detalle_solicitud(p_tabla,p_id_registro)
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
       v_detalle_solicitud DYNAMIC ARRAY OF RECORD
          v_etiqueta VARCHAR(50),
          v_valor    VARCHAR(80)
       END RECORD,
       v_indice INTEGER

   LET v_indice = 1
 
   DECLARE cur_rec_detalle_solicitud CURSOR FOR prp_rec_detalle_solicitud
   FOREACH cur_rec_detalle_solicitud USING p_tabla,
                                           p_id_registro
                                      INTO v_tmp_detalle_solicitud.*
      LET v_detalle_solicitud[v_indice].v_etiqueta = v_tmp_detalle_solicitud.v_etiqueta
      LET v_detalle_solicitud[v_indice].v_valor    = v_tmp_detalle_solicitud.v_valor
      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_rec_detalle_solicitud

   RETURN v_detalle_solicitud
END FUNCTION

# Descripción: Recupera historia de la solicitud por funciones globales
FUNCTION fn_recupera_historia_solicitud(p_tabla,p_id_registro)
DEFINE p_tabla       VARCHAR(50),
       p_id_registro DECIMAL(9,0),
       v_tmp_historia_solicitud RECORD
          v_ind              SMALLINT, 
          v_diag             CHAR(3),
          v_sql_error        INTEGER,
          v_isam_error       INTEGER,
          v_msg_error        VARCHAR(100),
          v_etiqueta         VARCHAR(50),
          v_f_modificacion   DATE,
          v_valor_modificado VARCHAR(50),
          v_valor_actual     VARCHAR(50),
          v_usuario          VARCHAR(80)
       END RECORD,
       v_historia_solicitud DYNAMIC ARRAY OF RECORD
          v_etiqueta         VARCHAR(50),
          v_f_modificacion   DATE,
          v_valor_modificado VARCHAR(50),
          v_valor_actual     VARCHAR(50)
       END RECORD,
       v_indice INTEGER

   LET v_indice = 1
   DECLARE cur_rec_historia_solicitud CURSOR FOR prp_rec_historia_solicitud
   FOREACH cur_rec_historia_solicitud USING p_tabla,
                                            p_id_registro
                                      INTO v_tmp_historia_solicitud.*
      LET v_historia_solicitud[v_indice].v_etiqueta         = v_tmp_historia_solicitud.v_etiqueta
      LET v_historia_solicitud[v_indice].v_f_modificacion   = v_tmp_historia_solicitud.v_f_modificacion
      LET v_historia_solicitud[v_indice].v_valor_modificado = v_tmp_historia_solicitud.v_valor_modificado
      LET v_historia_solicitud[v_indice].v_valor_actual     = v_tmp_historia_solicitud.v_valor_actual
      LET v_indice = v_indice + 1

   END FOREACH
   {IF(v_historia_solicitud[v_historia_solicitud.getLength()].v_etiqueta IS NULL)THEN
      CALL v_historia_solicitud.deleteElement(v_historia_solicitud.getLength())
   END IF}
   FREE cur_rec_historia_solicitud

   RETURN v_historia_solicitud
END FUNCTION