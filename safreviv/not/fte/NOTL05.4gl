################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 30/07/2015                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => NOT                                                      #
#Programa          => NOTL05                                                   #
#Objetivo          => Pantalla para ejecutar el reporoceso de notificaciones   #
#Fecha inicio      => 30/07/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS "NOTL05.inc"

PRIVATE DEFINE v_tipo_proceso                SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog                    VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario                     VARCHAR(30) -- Almacena al usuario

#Parametros de busqueda
PRIVATE DEFINE v_condicion                   STRING
PRIVATE DEFINE v_folio                       DECIMAL(9,0)
PRIVATE DEFINE v_datos                       SMALLINT

PRIVATE DEFINE v_detalle_folio					info_folio

#Lista de notificaciones configuradas
PRIVATE DEFINE v_lista_notificaciones DYNAMIC ARRAY OF notificaciones

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form

MAIN

   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1
   
   -- se asignan los parametros que vienen del fglrun
   LET v_usuario       = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   
   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   OPEN WINDOW vtn_notl051 WITH FORM "NOTL051"

      LET ventana = ui.Window.forName("vtn_notl051")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_notl051

END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   #Se inicializan las valiables del filtro
   INITIALIZE v_folio               TO NULL
   INITIALIZE v_condicion           TO NULL
	INITIALIZE v_detalle_folio       TO NULL
   LET v_datos = 0

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)
	CALL forma.setElementHidden("group3",1)
	CALL forma.setElementHidden("group4",1)

   #Seccion que pintara los parametros de busqueda
   CONSTRUCT v_condicion ON glo.folio
                  FROM folio
      BEFORE CONSTRUCT
         CLEAR FORM
         CLEAR SCREEN

      ON ACTION ACCEPT
         LET v_folio             = GET_FLDBUF(folio)
         LET INT_FLAG = 0
         IF v_folio IS NULL THEN
            CALL fn_mensaje("Notificaciones",
                         "Debe de ingresar un folio para la búsqueda.",
                         "about")
            RETURN 1
         END IF
         ACCEPT CONSTRUCT

      ON ACTION CANCEL
         LET INT_FLAG = 1
         EXIT CONSTRUCT
   END CONSTRUCT
   
   #Si en la seccion de parametros de busqueda se selecciono aceptar pinta las siguientes secciones
   IF NOT INT_FLAG THEN
      CALL fn_buscar_folio()
      RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_buscar_folio()
   
   #Variables para el manejo de las consultas
   DEFINE v_query                      STRING
	DEFINE i 									SMALLINT
   
   LET v_query = "SELECT ",
										"glo.folio, ",
										"proc.modulo_cod || ' - ' || mod.modulo_desc, ",
										"glo.proceso_cod, ",
										"proc.proceso_desc, ",
										"usr.usuario_desc, ",
                              "glo.f_actualiza ",
									"FROM glo_folio glo ",
									"INNER JOIN cat_proceso proc ON proc.proceso_cod = glo.proceso_cod ",
									"INNER JOIN seg_modulo mod ON mod.modulo_cod = proc.modulo_cod ",
									"LEFT JOIN seg_usuario usr ON usr.usuario_cod = glo.usuario ",
									"WHERE ", v_condicion CLIPPED
   PREPARE exe_consulta_folio FROM v_query 
   EXECUTE exe_consulta_folio INTO 	v_detalle_folio.folio,
												v_detalle_folio.modulo,
												v_detalle_folio.proceso_cod,
												v_detalle_folio.proceso_desc,
												v_detalle_folio.usuario_desc,
                                    v_detalle_folio.f_proceso

   IF v_detalle_folio.folio IS NULL THEN
      CALL fn_mensaje("Notificaciones",
                         "No se encontraron resultados en la búsqueda.",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

	#Se valida que existan procesos de notificacion configurados para el folio
	LET v_query = "SELECT ",
                  "cat_not.proceso_cod_notifica, ",
                  "prc_not.proceso_desc ",
                  "FROM cat_notificacion cat_not ",
                  "INNER JOIN cat_proceso prc_ori ON prc_ori.proceso_cod = cat_not.proceso_cod_origen ",
                  "INNER JOIN cat_proceso prc_not ON prc_not.proceso_cod = cat_not.proceso_cod_notifica ",
                  "WHERE cat_not.proceso_cod_origen = ? "
	PREPARE exe_consulta_notificaciones FROM v_query
	DECLARE cur_consulta_notificaciones CURSOR FOR exe_consulta_notificaciones

	LET i = 1
   FOREACH cur_consulta_notificaciones USING v_detalle_folio.proceso_cod INTO v_lista_notificaciones[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_notificaciones.deleteElement(v_lista_notificaciones.getLength())
   CLOSE cur_consulta_notificaciones
   FREE cur_consulta_notificaciones

   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
   DEFINE v_valida_notificacion  SMALLINT

	#Se valida si el proceso tiene dada de alta la funcion de reverso para el negocio 
	IF v_lista_notificaciones.getLength() > 0 THEN
      LET v_valida_notificacion = 1
   ELSE
      LET v_valida_notificacion = 0
   END IF

	#Se muestran los datos en pantalla
   DIALOG   ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_notificaciones       TO lista_notificaciones.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)
         CALL forma.setElementHidden("group3",0)
			IF v_valida_notificacion = 0 THEN
				CALL forma.setElementHidden("group4",0)
         END IF
				
			DISPLAY v_detalle_folio.f_proceso     	TO f_proceso
			DISPLAY v_detalle_folio.folio        	TO folio
			DISPLAY v_detalle_folio.modulo			TO modulo
			DISPLAY v_detalle_folio.proceso_cod    TO proceso_cod
			DISPLAY v_detalle_folio.proceso_desc   TO proceso_desc
			DISPLAY v_detalle_folio.usuario_desc   TO usuario_desc

      ON ACTION notificar
			IF v_valida_notificacion <> 1 THEN
				CALL fn_mensaje("Atención", "No se puede ejecutar el proceso porque no existen notificaciones configuradas","info")
				LET v_datos = 0
				RETURN 0
				EXIT DIALOG
			END IF
         CALL fn_notifica_proceso(v_folio, v_detalle_folio.proceso_cod, v_usuario)
         CALL fn_mensaje("Atención", "Se activó correctamente el reproceso de notificaciones para el folio " || v_folio,"info")
         INSERT INTO not_ctr_reproceso VALUES(v_folio,v_detalle_folio.proceso_cod,TODAY,v_usuario)
         LET v_datos = 0
         RETURN 0
         EXIT DIALOG
        
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG
   END DIALOG
   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_notificacion()
      
END FUNCTION
