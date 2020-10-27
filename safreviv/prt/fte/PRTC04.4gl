--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07/05/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC04                                                     #
#Objetivo        => Consulta de excepciones BUS y otros                        #
#Fecha Inicio    => 07 Mayo 2015                                               #
################################################################################
SCHEMA safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       p_nss_consulta    CHAR(11),
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_forma           ui.Form

GLOBALS "PRTG01.4gl"

MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)
   LET p_nss_consulta   = ARG_VAL(4)

   CONNECT TO "safre_viv"
   CALL fn_inicializa_consultas()
   CALL fn_filtra_excepciones()
   DISCONNECT "safre_viv"

END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_consulta = " SELECT id_cat_bus_proceso,",
                    "        cod_proceso_bus,",
                    "        desc_proceso_bus",
                    "   FROM cat_bus_proceso",
                    "  WHERE modulo_cod = ?"
   PREPARE prp_rec_proceso FROM v_consulta

   LET v_consulta = " SELECT id_cat_bus_operacion,",
                    "        cod_opera_bus,",
                    "        desc_opera_bus",
                    "   FROM cat_bus_operacion",
                    "  WHERE id_cat_bus_proceso = ?"
   PREPARE prp_rec_operaciones FROM v_consulta

   LET v_consulta = " SELECT pro.desc_proceso_bus",
                    "        ope.desc_opera_bus,",
                    "        bus.usuario_cod,",
                    "        bus.error_sql,",
                    "        bus.error_descripcion,",
                    "        bus.error_origen,",
                    "        bus.error_v_ind,",
                    "        bus.error_diag,",
                    "        bus.f_error",
                    "   FROM prt_error_bus bus JOIN cat_bus_proceso pro",
                    "     ON bus.proceso_cod = pro.cod_proceso_bus",
                    "        JOIN cat_bus_operacion ope",
                    "     ON bus.operacion_cod = ope.cod_opera_bus",
                    "  WHERE bus.p_id_prt_sol_ced = ?",
                    "    AND bus.f_error = ?",
                    "    AND pro.id_cat_bus_proceso = ?",
                    "    AND ope.id_cat_bus_operacion = ?"
   PREPARE prp_rec_det_excepcion FROM v_consulta

END FUNCTION

# Descripción: Filtros de consulta de excepciones
FUNCTION fn_filtra_excepciones()
DEFINE v_filtros RECORD
          v_proceso_cod LIKE cat_bus_proceso.cod_proceso_bus,
          v_opera_cod   LIKE cat_bus_operacion.cod_opera_bus,
          v_origen      LIKE prt_error_bus.error_origen,
          v_fecha_ini   DATE,
          v_fecha_fin   DATE
       END RECORD,
       v_cb_proceso     ui.ComboBox,
       v_cb_operacion   ui.ComboBox

   OPEN WINDOW vtna_excepciones WITH FORM v_ruta_ejecutable CLIPPED||"/PRTC041"
      INPUT v_filtros.v_proceso_cod,
            v_filtros.v_opera_cod,
            v_filtros.v_origen,
            v_filtros.v_fecha_ini,
            v_filtros.v_fecha_fin
       FROM filtro_proceso,
            filtro_operacion,
            filtro_origen,
            filtro_fecha_ini,
            filtro_fecha_fin ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

         BEFORE INPUT
            LET v_ventana = ui.Window.getCurrent()
            LET v_forma = v_ventana.getForm()
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)
               CALL v_ventana.setText(p_titulo_ventana)
            END IF
            LET v_cb_proceso = ui.ComboBox.forName("filtro_proceso")
            LET v_cb_operacion = ui.ComboBox.forName("filtro_operacion")
            CALL fn_llena_cb_proceso(v_cb_proceso)

         ON CHANGE filtro_proceso
            CALL fn_llena_cb_operacion(v_cb_operacion,
                                       v_filtros.v_proceso_cod)

         ON ACTION aceptar
            IF( v_filtros.v_proceso_cod IS NULL AND
                v_filtros.v_opera_cod IS NULL AND
                v_filtros.v_origen IS NULL AND
                v_filtros.v_fecha_ini IS NULL AND
                v_filtros.v_fecha_fin IS NULL )THEN
               CALL fn_mensaje(p_titulo_ventana,"Debe capturar al menos algún filtro","information")
               CONTINUE INPUT
            END IF
            CALL fn_consulta_excepciones(v_filtros.*)
            CONTINUE INPUT

         ON ACTION cancelar
            EXIT INPUT

      END INPUT
   CLOSE WINDOW vtna_excepciones

END FUNCTION

# Decripción: Recupera procesos de portabilidad BUS
FUNCTION fn_llena_cb_proceso(p_cb_proceso)
DEFINE p_cb_proceso ui.ComboBox,
       v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_proceso_cod  LIKE cat_bus_proceso.cod_proceso_bus,
       v_proceso_desc LIKE cat_bus_proceso.desc_proceso_bus

   CALL p_cb_proceso.clear()
   DECLARE cur_rec_proceso CURSOR FOR prp_rec_proceso
   FOREACH cur_rec_proceso USING C_MODULO_COD_PRT
                            INTO v_id_cat_bus_proceso,
                                 v_proceso_cod,
                                 v_proceso_desc
      CALL p_cb_proceso.addItem(v_id_cat_bus_proceso,v_proceso_cod||"-"||v_proceso_desc CLIPPED)

   END FOREACH
   FREE cur_rec_proceso

END FUNCTION

# Descripción: Recupera las operaciones de portabilidad bus
FUNCTION fn_llena_cb_operacion(p_cb_operacion,
                               p_id_cat_bus_proceso)
DEFINE p_cb_operacion       ui.ComboBox,
       p_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_operacion_cod        LIKE cat_bus_operacion.cod_opera_bus,
       v_operacion_desc       LIKE cat_bus_operacion.desc_opera_bus

   CALL p_cb_operacion.clear()
   DECLARE cur_rec_operaciones CURSOR FOR prp_rec_operaciones
   FOREACH cur_rec_operaciones USING p_id_cat_bus_proceso
                                INTO v_id_cat_bus_operacion,
                                     v_operacion_cod,
                                     v_operacion_desc
      CALL p_cb_operacion.addItem(v_id_cat_bus_operacion, v_operacion_cod||"-"||v_operacion_desc)

   END FOREACH
   FREE cur_rec_operaciones

END FUNCTION

# Descripción: Consulta excepciones del bus
FUNCTION fn_consulta_excepciones(p_filtros)
DEFINE p_filtros RECORD
          v_proceso_cod LIKE cat_bus_proceso.cod_proceso_bus,
          v_opera_cod   LIKE cat_bus_operacion.cod_opera_bus,
          v_origen      LIKE prt_error_bus.error_origen,
          v_fecha_ini   DATE,
          v_fecha_fin   DATE
       END RECORD,
       v_excepciones DYNAMIC ARRAY OF RECORD
          v_id_prt_sol   LIKE prt_error_bus.p_id_prt_sol_ced,
          v_proceso_cod  LIKE prt_error_bus.proceso_cod,
          v_proceso_desc LIKE cat_bus_proceso.desc_proceso_bus,
          v_opera_cod    LIKE prt_error_bus.operacion_cod,
          v_opera_desc   LIKE cat_bus_operacion.desc_opera_bus,
          v_origen       LIKE prt_error_bus.error_origen,
          v_fecha        LIKE prt_error_bus.f_error
       END RECORD

   DISPLAY ARRAY v_excepciones TO sr_excepciones.* ATTRIBUTES( ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED )

      BEFORE DISPLAY
         CALL fn_recupera_excepciones(p_filtros.*) RETURNING v_excepciones

      BEFORE ROW
         CALL fn_recupera_det_excepcion(v_excepciones[ARR_CURR()].v_id_prt_sol) 

      ON ACTION aceptar

      ON ACTION cancelar
         EXIT DISPLAY

   END DISPLAY

END FUNCTION

# Descipción: Recupera las excepciones de prt_error_bus según los filtros
FUNCTION fn_recupera_excepciones(p_filtros)
DEFINE p_filtros RECORD
          v_proceso_cod LIKE cat_bus_proceso.id_cat_bus_proceso,
          v_opera_cod   LIKE cat_bus_operacion.id_cat_bus_operacion,
          v_origen      LIKE prt_error_bus.error_origen,
          v_fecha_ini   DATE,
          v_fecha_fin   DATE
       END RECORD,
       v_excepciones DYNAMIC ARRAY OF RECORD
          v_id_prt_sol   LIKE prt_error_bus.p_id_prt_sol_ced,
          v_proceso_cod  LIKE prt_error_bus.proceso_cod,
          v_proceso_desc LIKE cat_bus_proceso.desc_proceso_bus,
          v_opera_cod    LIKE prt_error_bus.operacion_cod,
          v_opera_desc   LIKE cat_bus_operacion.desc_opera_bus,
          v_origen       LIKE prt_error_bus.error_origen,
          v_fecha        LIKE prt_error_bus.f_error
       END RECORD,
       v_consulta STRING,
       v_filtro   STRING,
       v_indice   INTEGER

   LET v_consulta = " SELECT bus.p_id_prt_sol_ced,",
                    "        bus.proceso_cod,",
                    "        pro.desc_proceso_bus,",
                    "        bus.operacion_cod,",
                    "        ope.desc_opera_bus,",
                    "        bus.error_origen,",
                    "        bus.f_error",
                    "   FROM prt_error_bus bus JOIN cat_bus_proceso pro",
                    "     ON bus.proceso_cod = pro.cod_proceso_bus",
                    "        JOIN cat_bus_operacion ope",
                    "     ON bus.operacion_cod = ope.cod_opera_bus",
                    "  WHERE "
   LET v_filtro = "1 = 1"
   IF( p_filtros.v_proceso_cod IS NOT NULL )THEN
      LET v_filtro = "pro.id_cat_bus_proceso = "||p_filtros.v_proceso_cod CLIPPED||" AND"
   END IF

   IF( p_filtros.v_opera_cod IS NOT NULL )THEN
      LET v_filtro = "ope.id_cat_bus_operacion = '"||p_filtros.v_opera_cod ||"' AND"
   END IF

   IF( p_filtros.v_origen IS NOT NULL )THEN
      LET v_filtro = "bus.error_origen MATCHES '"||p_filtros.v_origen CLIPPED||"' AND"
   END IF

   IF( p_filtros.v_fecha_ini IS NOT NULL )THEN
      IF( p_filtros.v_fecha_fin IS NOT NULL )THEN
         LET v_filtro = "bus.f_error BETWEEN '"||p_filtros.v_fecha_ini||"' AND '"||p_filtros.v_fecha_fin||"' AND"
      ELSE
         LET v_filtro = "bus.f_error = '"||p_filtros.v_fecha_ini||"' AND"
      END IF
   ELSE
      IF( p_filtros.v_fecha_fin IS NOT NULL )THEN
         LET v_filtro = "bus.f_error = '"||p_filtros.v_fecha_fin||"' AND"
      END IF
   END IF

   LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-3)

   LET v_consulta = v_consulta||v_filtro

   LET v_indice = 1
   PREPARE prp_rec_excepciones FROM v_consulta
   DECLARE cur_rec_excepciones CURSOR FOR prp_rec_excepciones
   FOREACH cur_rec_excepciones INTO v_excepciones[v_indice].*
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_excepciones
   IF(v_excepciones[v_excepciones.getLength()].v_id_prt_sol IS NULL)THEN
      CALL v_excepciones.deleteElement(v_excepciones.getLength())
   END IF

   RETURN v_excepciones
END FUNCTION

# Descripción: Recuepra detalle de a excpeción
FUNCTION fn_recupera_det_excepcion(p_id_exepcion)
DEFINE p_id_exepcion LIKE prt_error_bus.p_id_prt_sol_ced

   DECLARE cur_rec_det_excepcion CURSOR FOR prp_rec_det_excepcion

END FUNCTION