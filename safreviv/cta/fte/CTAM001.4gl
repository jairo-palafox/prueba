###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAM001                                                 #
#Objetivo          => Actualizar el catalogo de movimientos                   #
#Fecha Inicio      => 01-AGOSTO-2018                                          #
###############################################################################

DATABASE safre_viv

GLOBALS "CTAM001.inc"

PRIVATE DEFINE p_usuario            VARCHAR(20)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        VARCHAR(50)
PRIVATE DEFINE p_movimiento         INTEGER

PRIVATE DEFINE v_ciclo              SMALLINT

PRIVATE DEFINE ventana              ui.Window
PRIVATE DEFINE forma                ui.Form
PRIVATE DEFINE ventana_list         ui.Window
PRIVATE DEFINE forma_list           ui.Form

MAIN
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2)
   LET p_nombre_menu        = ARG_VAL(3)
   LET p_movimiento         = ARG_VAL(4)

   CLOSE WINDOW SCREEN

   LET p_nombre_menu = "Administracion de movimientos"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW ctam0011 WITH FORM "CTAM0011"
      LET ventana = ui.Window.forName("ctam0011")
      LET forma = ventana.getForm()

      LET v_ciclo = TRUE
      
      WHILE v_ciclo
         IF p_movimiento IS NULL THEN
            LET p_movimiento = 0
            WHILE p_movimiento = 0
               CALL fn_busca_movimiento() RETURNING p_movimiento
            END WHILE
         ELSE
            LET v_ciclo = FALSE
         END IF

         IF p_movimiento > 0 THEN
            CALL fn_actualiza_movimiento()
         END IF
         
         LET p_movimiento = NULL
      END WHILE
     LET v_ciclo = TRUE
   CLOSE WINDOW ctam0011
   
END MAIN

PRIVATE FUNCTION fn_busca_movimiento()
   DEFINE v_movimiento           INTEGER
   DEFINE v_tpo_movimiento       INTEGER
   DEFINE v_modulo               CHAR(3)
   DEFINE cb_tpo_movimiento      ui.ComboBox
   DEFINE cb_modulo              ui.ComboBox
   DEFINE v_query                STRING
   DEFINE v_condicion            STRING
   DEFINE v_clave                CHAR(3)
   DEFINE v_desc                 VARCHAR(50)
   DEFINE i                      INTEGER

   DEFINE v_lista_movimientos DYNAMIC ARRAY OF movimiento

   INITIALIZE v_lista_movimientos TO NULL
   #Ocultamos las secciones que no tienen datos
   CALL forma.setElementHidden("group2",1)
   
	LET cb_tpo_movimiento = ui.ComboBox.forName("formonly.v_tpo_movimiento")
   LET cb_modulo = ui.ComboBox.forName("formonly.v_modulo")

   CONSTRUCT v_condicion ON mov.movimiento, mov.tipo, mov.modulo_cod
                  FROM v_movimiento, v_tpo_movimiento, v_modulo
      BEFORE CONSTRUCT
         CLEAR FORM

         CALL cb_tpo_movimiento.clear()
         CALL cb_modulo.clear()

         #Se llena el catalogo de tipo de movimiento
         CALL cb_tpo_movimiento.addItem(1,'Abono')
         CALL cb_tpo_movimiento.addItem(-1,'Cargo')

         #Se llena el combo de modulos
         LET v_query =  "SELECT DISTINCT ", 
                           "mov.modulo_cod, ",
                           "mov.modulo_cod || ' - ' || TRIM(seg.modulo_desc) ",
                        "FROM cat_movimiento mov ",
                        "INNER JOIN seg_modulo seg ON seg.modulo_cod = mov.modulo_cod ",
                        "ORDER BY mov.modulo_cod ASC  "
                        
         PREPARE exe_consulta_modulos FROM v_query
         DECLARE cur_consulta_modulos CURSOR FOR exe_consulta_modulos
         FOREACH cur_consulta_modulos INTO v_clave, v_desc
				IF v_clave IS NOT NULL THEN
               LET v_desc = v_desc CLIPPED
					CALL cb_modulo.addItem(v_clave, v_desc)
				END IF
			END FOREACH

      ON ACTION ACCEPT
         LET v_movimiento = GET_FLDBUF(v_movimiento)
			LET v_tpo_movimiento = GET_FLDBUF(v_tpo_movimiento)
			LET v_modulo = GET_FLDBUF(v_modulo)

         IF v_movimiento IS NULL AND
            v_tpo_movimiento IS NULL AND
            v_modulo IS NULL THEN
            CALL fn_mensaje("Actualiza movimiento", "Debe de ingresar algún campo de búsqueda.", "about")
            NEXT FIELD v_movimiento
         END IF

         LET v_ciclo  = FALSE
         LET INT_FLAG = FALSE
         ACCEPT CONSTRUCT

      ON ACTION CANCEL
         LET v_ciclo  = FALSE
         LET INT_FLAG = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT INT_FLAG THEN
      LET v_query =  "SELECT ",
                     "movimiento, ",
                     "movimiento_desc, ",
                     "desc_ciudadana, ",
                     "mov.modulo_cod || ' - ' || NVL(mod.modulo_desc, ' ') ",
                     "FROM cat_movimiento mov ",
                     "LEFT JOIN seg_modulo mod ON mod.modulo_cod = mov.modulo_cod ",
                     "WHERE ", v_condicion CLIPPED, " ",
                     "ORDER BY movimiento ASC"

      PREPARE exe_busca_movimientos FROM v_query
      DECLARE cur_busca_movimientos CURSOR FOR exe_busca_movimientos

      LET i = 1
      FOREACH cur_busca_movimientos INTO  v_lista_movimientos[i].*
         LET i = i + 1
         #IF i > MAX_REGISTROS THEN
         #   CALL fn_mensaje("Actualiza movimiento",
         #                   "Acotar más el criterio de búsqueda. \n"||
         #                   "Se muestran sólo los primeros 200 registros",
         #                   "about")
         #   EXIT FOREACH
         #END IF
      END FOREACH

      IF v_lista_movimientos[v_lista_movimientos.getLength()].movimiento IS NULL 
      OR v_lista_movimientos[v_lista_movimientos.getLength()].movimiento <= 0 THEN
         CALL v_lista_movimientos.deleteElement(v_lista_movimientos.getLength())
      END IF

      IF i > 1 THEN
         IF i = 2 THEN
            LET v_movimiento = v_lista_movimientos[1].movimiento
         ELSE
            OPEN WINDOW ctam0012 WITH FORM "CTAM0012" ATTRIBUTES (STYLE="dialog")
               LET ventana_list = ui.Window.forName("ctam0012")
               LET forma_list = ventana.getForm()

               CALL ventana_list.setText(p_nombre_menu);
               
               DISPLAY ARRAY v_lista_movimientos TO lista_movimientos.*
                  ON ACTION ACCEPT 
                     LET INT_FLAG = FALSE
                     LET v_movimiento = v_lista_movimientos[ARR_CURR()].movimiento
                     EXIT DISPLAY

                  ON ACTION CANCEL
                     LET v_movimiento = 0
                     EXIT DISPLAY
               END DISPLAY
            CLOSE WINDOW ctam0012
         END IF
      ELSE
         CALL fn_mensaje("Actualiza movimiento",
                         "No existe registros con el criterio de búsqueda. \n",
                         "about")
         LET v_movimiento = 0
      END IF
   ELSE
      LET v_movimiento = -1
   END IF
   #CALL fn_mensaje("Actualiza movimiento",v_movimiento,"about")
   RETURN v_movimiento
END FUNCTION

PRIVATE FUNCTION fn_actualiza_movimiento()
   DEFINE v_query             STRING
   DEFINE v_movimiento        INTEGER
   DEFINE v_movimiento_desc   VARCHAR(40)
   DEFINE v_tipo              INTEGER
   DEFINE v_tipo_movimiento   VARCHAR(20)
   DEFINE v_modulo            VARCHAR(60)
   DEFINE v_ind_visible       INTEGER
   DEFINE v_tmp_visible       INTEGER 
   DEFINE v_desc_ciudadana    VARCHAR(60)

   DEFINE v_desc_original     VARCHAR(60)
   DEFINE v_visible_original  INTEGER
   DEFINE v_cambio_desc       BOOLEAN
   DEFINE v_cambio_vis        BOOLEAN
   DEFINE v_respuesta         INTEGER

   DEFINE v_cb_visible        ui.ComboBox

  

   LET v_query =  "SELECT ", 
                     "mov.movimiento, ",
                     "mov.movimiento_desc, ",
                     "mov.tipo, ",
                     "mov.modulo_cod || ' - ' || NVL(mod.modulo_desc, ' '), ",
                     "mu.movimiento, ",
                     "mov.desc_ciudadana ",
                  "FROM cat_movimiento mov ",
                  "LEFT JOIN seg_modulo mod ON mod.modulo_cod = mov.modulo_cod ",
                  "LEFT JOIN cat_muestra_mov mu ON mu.movimiento = mov.movimiento ",
                  "WHERE mov.movimiento = ? "
   PREPARE exe_consulta_movimiento FROM v_query
   EXECUTE exe_consulta_movimiento USING  p_movimiento
                                   INTO   v_movimiento,
                                          v_movimiento_desc,
                                          v_tipo,
                                          v_modulo,
                                          v_tmp_visible,
                                          v_desc_ciudadana

   IF v_tipo = 1 THEN
      LET v_tipo_movimiento = 'Abono'
   ELSE
      LET v_tipo_movimiento = 'Cargo'
   END IF

   IF v_tmp_visible IS NOT NULL AND v_tmp_visible > 0 THEN
      LET v_ind_visible = 1
   ELSE
      LET v_ind_visible = 2
   END IF

   LET v_desc_original = v_desc_ciudadana
   LET v_visible_original = v_ind_visible

   CALL forma.setElementHidden("group2",0)
   CALL forma.setElementHidden("group1",1)

   INPUT v_ind_visible, v_desc_ciudadana FROM ind_visible, desc_ciudadana ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
      BEFORE INPUT
         #Ocultamos las secciones que no tienen datos
         CALL DIALOG.setActionHidden("accept", TRUE)
         
         LET v_cb_visible = ui.ComboBox.forName("formonly.ind_visible")
         CALL v_cb_visible.clear()
         CALL v_cb_visible.addItem(1,"SI")
         CALL v_cb_visible.addItem(2,"NO")

         DISPLAY v_movimiento TO movimiento
         DISPLAY v_movimiento_desc TO desc_movimiento
         DISPLAY v_tipo_movimiento TO tpo_movimiento
         DISPLAY v_modulo TO modulo
         DISPLAY v_ind_visible TO ind_visible
         DISPLAY v_desc_ciudadana TO desc_ciudadana

      ON ACTION actualizar
         IF v_ind_visible <> v_visible_original THEN
            LET v_cambio_vis = TRUE
         ELSE
            LET v_cambio_vis = FALSE
         END IF

         IF v_desc_ciudadana <> v_desc_original 
         OR (v_desc_ciudadana IS NOT NULL AND v_desc_original IS NULL) 
         OR (v_desc_ciudadana IS NULL AND v_desc_original IS NOT NULL) THEN
            LET v_cambio_desc = TRUE
         ELSE
            LET v_cambio_desc = FALSE
         END IF

         IF v_cambio_desc OR v_cambio_vis THEN
            #Se aplican las actualizaciones
            CALL fn_ventana_confirma("Atención",
                    "¿Desea aplicar la actualizacion al movimiento?",
                     "quest") RETURNING v_respuesta
            
            IF v_respuesta = 1 THEN
               CALL fn_aplica_actualizacion(v_cambio_vis, v_ind_visible, v_cambio_desc, v_desc_ciudadana, v_movimiento)
               CALL fn_mensaje("Actualiza movimiento","La actualizacion se aplico correctamente en el sistema","about")
               EXIT INPUT
            END IF
         ELSE
            CALL fn_mensaje("Actualiza movimiento","Para continuar es necesario aplicar alguna actualizacion...","about")
         END IF
         
      ON ACTION CANCEL
         EXIT INPUT

   END INPUT
END FUNCTION

PRIVATE FUNCTION fn_aplica_actualizacion(p_cambio_vis, p_ind_visible, p_cambio_desc, p_desc_ciudadana, p_mov)
   DEFINE p_cambio_vis        BOOLEAN
   DEFINE p_ind_visible       INTEGER
   DEFINE p_cambio_desc       BOOLEAN
   DEFINE p_desc_ciudadana    VARCHAR(60)
   DEFINE p_mov               INTEGER
   DEFINE v_tipo_modificacion INTEGER
   DEFINE v_query             STRING
   DEFINE v_f_proceso         DATE

   DEFINE v_bitacora          bitacora_mov

   LET v_tipo_modificacion = 0
   LET v_f_proceso = TODAY

   IF p_cambio_desc THEN
      LET v_tipo_modificacion = v_tipo_modificacion + 1
   END IF
   
   IF p_cambio_vis THEN
      LET v_tipo_modificacion = v_tipo_modificacion + 2
   END IF

   #Se llena la bitacora
   LET v_query = "SELECT ",
                     "mov.movimiento, ",
                     "mov.movimiento_desc, ",
                     "mov.tipo, ",
                     "mov.categoria, ",
                     "mov.modulo_cod, ",
                     "mov.desc_ciudadana ",
                  "FROM cat_movimiento mov ",
                  "WHERE mov.movimiento = ? "
   PREPARE exe_consulta_mov FROM v_query
   EXECUTE exe_consulta_mov USING   p_mov
                           INTO     v_bitacora.movimiento,
                                    v_bitacora.movimiento_desc,
                                    v_bitacora.tipo,
                                    v_bitacora.categoria,
                                    v_bitacora.modulo_cod,
                                    v_bitacora.desc_ciudadana
   IF v_bitacora.movimiento IS NOT NULL AND v_bitacora.movimiento > 0 THEN
      LET v_bitacora.f_modifica = TODAY
      LET v_bitacora.usuario = p_usuario
      LET v_bitacora.tpo_modifica = v_tipo_modificacion

      LET v_query = "INSERT INTO cat_his_movimiento(
                                 id_cat_his_movimiento,
                                 movimiento,
                                 movimiento_desc,
                                 tipo,
                                 categoria,
                                 modulo_cod,
                                 desc_ciudadana,
                                 f_modifica,
                                 usuario,
                                 tpo_modifica)
                                 values (seq_cat_his_mov.nextval,?,?,?,?,?,?,?,?,?) "
      PREPARE exe_inserta_bitacora FROM v_query
      EXECUTE exe_inserta_bitacora USING v_bitacora.*
   END IF

   IF p_cambio_vis THEN
      IF p_ind_visible = 1 THEN
         #Se actualiza para mostrar el movimiento
         INSERT INTO cat_muestra_mov (movimiento,f_actualiza,usuario) VALUES (p_mov,TODAY,p_usuario);
      ELSE
         #Se actualiza para ocultar el movimiento
         DELETE FROM cat_muestra_mov WHERE movimiento = p_mov
      END IF
   END IF
   
   IF p_cambio_desc THEN
      UPDATE cat_movimiento SET desc_ciudadana = p_desc_ciudadana WHERE movimiento = p_mov
   END IF
END FUNCTION