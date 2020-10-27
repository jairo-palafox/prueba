###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC001                                                 #
#Objetivo          => Consulta de movimientos nuevos                          #
#Fecha Inicio      => 01-AGOSTO-2018                                          #
###############################################################################

DATABASE safre_viv

GLOBALS "CTAC002.inc"

PRIVATE DEFINE p_usuario            VARCHAR(20)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        VARCHAR(50)
PRIVATE DEFINE p_movimiento         INTEGER

PRIVATE DEFINE v_ciclo              SMALLINT

PRIVATE DEFINE ventana              ui.Window
PRIVATE DEFINE forma                ui.Form

PRIVATE DEFINE v_datos              SMALLINT
PRIVATE DEFINE v_lista_movimientos  DYNAMIC ARRAY OF movimiento

MAIN
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2)
   LET p_nombre_menu        = ARG_VAL(3)
   LET p_movimiento         = ARG_VAL(4)

   CLOSE WINDOW SCREEN

   LET p_nombre_menu = "Movimientos nuevos"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW ctac0021 WITH FORM "CTAC0021"
      LET ventana = ui.Window.forName("ctac0021")
      LET forma = ventana.getForm()

      LET v_ciclo = 1
      LET v_datos = 0
      
      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
   CLOSE WINDOW ctac0021
   
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
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
   DEFINE v_tipo                 INTEGER
   DEFINE v_tmp_visible          INTEGER

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
                        "WHERE mov.f_actualiza >= MDY(1,1,YEAR(TODAY)) ",
                        "ORDER BY mov.modulo_cod ASC "
                        
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
   
   #Si en la seccion de parametros de busqueda se selecciono aceptar pinta las siguientes secciones
   IF NOT INT_FLAG THEN
      LET v_query =  "SELECT ",
                     "mov.movimiento, ",
                     "mov.movimiento_desc, ",
                     "mov.tipo, ",
                     "mov.modulo_cod || ' - ' || NVL(mod.modulo_desc, ' '), ",
                     "mu.movimiento, ",
                     "mov.desc_ciudadana, ",
                     "mov.f_actualiza ",
                     "FROM cat_movimiento mov ",
                     "LEFT JOIN seg_modulo mod ON mod.modulo_cod = mov.modulo_cod ",
                     "LEFT JOIN cat_muestra_mov mu ON mu.movimiento = mov.movimiento ",
                     "WHERE ", v_condicion CLIPPED, " ",
                     "AND mov.f_actualiza >= MDY(1,1,YEAR(TODAY)) ",
                     "ORDER BY mov.movimiento ASC"

      PREPARE exe_busca_movimientos FROM v_query
      DECLARE cur_busca_movimientos CURSOR FOR exe_busca_movimientos

      LET i = 1
      FOREACH cur_busca_movimientos INTO  v_lista_movimientos[i].movimiento,
                                          v_lista_movimientos[i].movimiento_desc,
                                          v_tipo,
                                          v_lista_movimientos[i].modulo,
                                          v_tmp_visible,
                                          v_lista_movimientos[i].desc_ciudadana,
                                          v_lista_movimientos[i].f_alta

         IF v_tipo = 1 THEN
            LET v_lista_movimientos[i].tipo_mov = 'Abono'
         ELSE
            LET v_lista_movimientos[i].tipo_mov = 'Cargo'
         END IF

         IF v_tmp_visible IS NOT NULL AND v_tmp_visible > 0 THEN
            LET v_lista_movimientos[i].ind_visible = 'SI'
         ELSE
            LET v_lista_movimientos[i].ind_visible = 'NO'
         END IF
         
         LET i = i + 1
         #IF i > MAX_REGISTROS THEN
         #   CALL fn_mensaje("Movimientos nuevos",
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
         LET v_datos = 1
      ELSE
         CALL fn_mensaje("Actualiza movimiento",
                         "No existe registros con el criterio de búsqueda. \n",
                         "about")
         LET v_datos = 0
      END IF
      RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()

   #CALL fn_mensaje("","muestra","about")
   CALL forma.setElementHidden("group2",0)
   
	#Se muestran los datos en pantalla
   DIALOG   ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_movimientos TO lista_movimientos.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)

      ON ACTION ACCEPT
			#LET v_folio_reverso.folio = v_lista[ARR_CURR()].folio
			#LET v_folio_reverso.f_proceso = v_lista[ARR_CURR()].f_proceso
			#LET v_folio_reverso.folio_ajustado = v_lista[ARR_CURR()].folio_ajustado
			#LET v_folio_reverso.estado = v_lista[ARR_CURR()].estado
			#LET v_folio_reverso.usuario_desc = v_lista[ARR_CURR()].usuario
			
			#CALL fn_genera_reporte()
         LET v_datos = 0
         RETURN 1
         EXIT DIALOG
        
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG
   END DIALOG
   RETURN 0
END FUNCTION