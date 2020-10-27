-----------------------------------------------------------------------------------------
-- Modulo        => CTA
-- Componente    => CTAC011
-- Funcionalidad => Consulta catálogo de aclaraciones (causales).
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 23 de enero de 2020.
-- Requerimiento =>  
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "CTAC011.inc"

PRIVATE DEFINE p_usuario     VARCHAR(20)
PRIVATE DEFINE p_tipo_proc   CHAR(1)
PRIVATE DEFINE p_nombre_menu VARCHAR(50)
PRIVATE DEFINE p_movimiento  INTEGER

PRIVATE DEFINE v_ciclo SMALLINT

PRIVATE DEFINE ventana ui.Window
PRIVATE DEFINE forma   ui.Form

PRIVATE DEFINE v_datos SMALLINT
PRIVATE DEFINE v_lista_movimientos DYNAMIC ARRAY OF movimiento

MAIN
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2)
   LET p_nombre_menu        = ARG_VAL(3)
   LET p_movimiento         = ARG_VAL(4)

   CLOSE WINDOW SCREEN

   LET p_nombre_menu = "Consulta de tipo de aclaraciones"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW ctac0111 WITH FORM "CTAC0111"
      LET ventana = ui.Window.forName("ctac0111")
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
   CLOSE WINDOW ctac0111
   
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   DEFINE v_movimiento     INTEGER
   DEFINE v_tpo_movimiento  INTEGER
   DEFINE v_modulo          CHAR(3)
   DEFINE cb_tpo_movimiento ui.ComboBox
   DEFINE cb_modulo         ui.ComboBox
   DEFINE v_query           STRING
   DEFINE v_condicion       STRING
   DEFINE v_clave           CHAR(3)
   DEFINE v_desc            VARCHAR(50)
   DEFINE i                 INTEGER
   DEFINE v_tipo            INTEGER
   DEFINE v_tmp_visible     INTEGER
   DEFINE v_id_bitacora     DECIMAL(10,0)

   INITIALIZE v_lista_movimientos TO NULL
   #Ocultamos las secciones que no tienen datos
   CALL forma.setElementHidden("group2",1)
   
   LET cb_tpo_movimiento = ui.ComboBox.forName("formonly.v_tpo_movimiento")
   LET cb_modulo = ui.ComboBox.forName("formonly.v_modulo")

   CONSTRUCT v_condicion ON mov.aclaracion_cod
                  FROM v_movimiento
      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_movimiento = GET_FLDBUF(v_movimiento)
         LET v_tpo_movimiento = GET_FLDBUF(v_tpo_movimiento)

--         IF v_movimiento IS NULL THEN
--            CALL fn_mensaje("Actualiza clave aclaración", "Debe de ingresar algún campo de búsqueda.", "about")
--            NEXT FIELD v_movimiento
--         END IF

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
      LET v_query =  " SELECT ",
                     " mov.aclaracion_cod, ",
                     " mov.aclaracion_descripcion, ",
                     " mu.aclaracion_cod, ",
                     " mov.desc_ciudadana ",
                     " FROM pag_tpo_aclaracion mov ",
                     " LEFT JOIN cat_muestra_acl mu ON mu.aclaracion_cod = mov.aclaracion_cod ",
                     " WHERE ", v_condicion CLIPPED, " ",
                     " ORDER BY mov.aclaracion_cod ASC"

      #CALL fn_mensaje("Actualiza movimiento", "Consulta: " || v_query, "about")
      PREPARE exe_busca_movimientos FROM v_query
      DECLARE cur_busca_movimientos CURSOR FOR exe_busca_movimientos

      LET i = 1
      FOREACH cur_busca_movimientos INTO  v_lista_movimientos[i].movimiento,
                                          v_lista_movimientos[i].movimiento_desc,
                                          v_tmp_visible,
                                          v_lista_movimientos[i].desc_ciudadana
         IF v_lista_movimientos[i].movimiento IS NOT NULL AND v_lista_movimientos[i].movimiento > 0 THEN

            IF v_tmp_visible IS NOT NULL AND v_tmp_visible > 0 THEN
               LET v_lista_movimientos[i].ind_visible = 'SI'
            ELSE
               LET v_lista_movimientos[i].ind_visible = 'NO'
            END IF

            #Se busca si existen cambios en bitacora
            SELECT MAX(id_cat_his_aclaracion) 
            INTO v_id_bitacora
            FROM cat_his_aclaracion
            WHERE aclaracion_cod = v_lista_movimientos[i].movimiento

            IF v_id_bitacora IS NOT NULL AND v_id_bitacora > 0 THEN
               SELECT 
                  cat.f_modifica,
                  cat.usuario,
                  tpo.descripcion
               INTO 
                  v_lista_movimientos[i].f_modifica,
                  v_lista_movimientos[i].usuario,
                  v_lista_movimientos[i].tipo_modifica
               FROM cat_his_aclaracion cat
               INNER JOIN cat_tpo_modifica tpo ON tpo.tpo_modifica = cat.tpo_modifica
               WHERE cat.id_cat_his_aclaracion = v_id_bitacora
            ELSE
               SELECT 
                  mov.f_actualiza,
                  mov.usuario
               INTO
                  v_lista_movimientos[i].f_modifica,
                  v_lista_movimientos[i].usuario
               FROM pag_tpo_aclaracion mov
               WHERE mov.aclaracion_cod = v_lista_movimientos[i].movimiento

               LET v_lista_movimientos[i].tipo_modifica = 'Carga inicial'
            END IF
            
            LET i = i + 1
         END IF
         
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
         LET v_datos = 1
      ELSE
         CALL fn_mensaje("Actualiza tipo aclaracion",
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

FUNCTION fn_mensaje(titulo,mensaje,imagen)
    DEFINE titulo, mensaje, imagen STRING

    LET titulo = "Información"
    LET imagen = "information"
    MENU titulo
         ATTRIBUTES ( STYLE="dialog", COMMENT=mensaje, IMAGE=imagen )
         ON ACTION accept
         RETURN
    END MENU
END FUNCTION
