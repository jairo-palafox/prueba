################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => AOP                                                      #
#Programa          => AOPCL3                                                   #
#Objetivo          => Pantalla Lanzar la ejecucion de la funcion de reverso    #
#							 de negocio                                               #
#Fecha inicio      => 21/05/2013                                               #
################################################################################
DATABASE safre_viv

GLOBALS "AOPL03.inc"

PRIVATE DEFINE v_tipo_proceso                SMALLINT
PRIVATE DEFINE v_nom_prog                    VARCHAR(30)
PRIVATE DEFINE v_usuario                     VARCHAR(30)

#Parametros de busqueda
PRIVATE DEFINE v_condicion                   STRING
PRIVATE DEFINE v_folio								DECIMAL(9,0)
PRIVATE DEFINE v_proceso_cod                 SMALLINT
PRIVATE DEFINE v_opera_cod         				SMALLINT

#Variables para el manejo del folio ajustado
PRIVATE DEFINE v_folio_ajustado					DECIMAL(9,0)
PRIVATE DEFINE v_proceso_cod_ajuste				SMALLINT
PRIVATE DEFINE v_nombre_funcion					VARCHAR(40)
PRIVATE DEFINE v_datos                       SMALLINT

DEFINE v_ruta_ejecutable                     VARCHAR(40) -- Ruta del ejecutable
DEFINE v_ruta_listados                       VARCHAR(40) -- Rute del log

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form

MAIN

   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1
   
   -- se asignan los parametros que vienen del fglrun
   LET v_usuario      = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   
   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   OPEN WINDOW vtn_aopl031 WITH FORM "AOPL031"

      LET ventana = ui.Window.forName("vtn_aopl031")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_aopl031

END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
	DEFINE cb_combo            ui.ComboBox
	DEFINE v_catalogo				STRING
	DEFINE v_clave					DECIMAL(9,0)
	
   #Se inicializan las valiables del filtro
   INITIALIZE v_proceso_cod         TO NULL
	INITIALIZE v_condicion           TO NULL
	
   LET v_datos = 0

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)
	CALL forma.setElementHidden("group3",1)
	LET cb_combo = ui.ComboBox.forName("formonly.folio")

	#Seccion que pintara los parametros de busqueda
   CONSTRUCT v_condicion ON aop.folio
                  FROM folio
      BEFORE CONSTRUCT
         CLEAR FORM
         CLEAR SCREEN
			
			CALL cb_combo.clear()

			LET v_catalogo =	"SELECT ",
									"folio  ",
									"FROM aop_ctr_ajuste ",
									"WHERE cve_estado = 2  ",
									"ORDER BY f_proceso"

			PREPARE exe_catalogo FROM v_catalogo
			DECLARE cur_catalogo CURSOR FOR exe_catalogo                                                 
			FOREACH cur_catalogo INTO v_clave
				IF v_clave IS NOT NULL THEN
					CALL cb_combo.addItem(v_clave, v_clave)
				END IF
			END FOREACH
			
      ON ACTION ACCEPT
         LET v_folio          		= GET_FLDBUF(folio)
         LET INT_FLAG = 0
         IF v_folio IS NULL THEN
            CALL fn_mensaje("Ajustes Operativos",
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
	DEFINE v_consulta_reverso           STRING

	LET v_consulta_reverso = 	"SELECT ",
										"aop.folio_ajustado, ",
										"aop.proceso_cod, ",
										"rev.nombre_funcion ",
										"FROM aop_ctr_ajuste aop  ",
										"INNER JOIN cat_reverso_negocio rev ON rev.proceso_cod = aop.proceso_cod ",
										"WHERE ", v_condicion CLIPPED
   PREPARE exe_consulta_reverso FROM v_consulta_reverso
	EXECUTE exe_consulta_reverso INTO v_folio_ajustado,
												 v_proceso_cod_ajuste,
												 v_nombre_funcion

   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
	#Se muestran los datos en pantalla
	DISPLAY v_folio_ajustado     				TO folio_ajustado
	DISPLAY v_proceso_cod_ajuste        	TO proceso_cod_ajuste
	DISPLAY v_nombre_funcion					TO nombre_funcion
	DISPLAY v_usuario								TO usuario
	CALL forma.setElementHidden("group2",0)
	CALL forma.setElementHidden("group3",0)

	MENU "."
      COMMAND "Ejecutar"
         CALL fn_ejecuta_funcion()
			LET v_datos = 0
			RETURN 0
			EXIT MENU

		COMMAND "Cancelar"
			LET v_datos = 0
			RETURN 0
			EXIT MENU
       
   END MENU				 
   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_funcion()
	DEFINE v_nom_archivo       CHAR(40)
	DEFINE v_comando           STRING
	DEFINE r_resultado_opera   INTEGER
	DEFINE v_pid               DECIMAL(9,0)

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
   INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = MODULO

   --Obtiene ruta listados
   SELECT ruta_listados
   INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   LET v_proceso_cod = PROCESO
   LET v_opera_cod = OPERACION
   LET v_nom_archivo = 'funcion_negocio'

	# se busca el pid para el proceso
	SELECT pid INTO v_pid FROM bat_ctr_proceso WHERE folio = v_folio
		
   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
		# Inicia operación
		CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"AOPP02",
									 v_nom_archivo,v_usuario) RETURNING r_resultado_opera
		# En el caso de que exista una inconsistencia al iniciar el proceso, se
		# Muestra un mensaje con la descripcion
		IF(r_resultado_opera)THEN
			CALL fn_muestra_inc_operacion(r_resultado_opera)
		ELSE
			LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/AOPP02.42r ",
														v_usuario," ",
														v_pid," ",
														v_proceso_cod," ",
														v_opera_cod," ",
														v_folio," '",
														v_nom_archivo,
														"' 1>", v_ruta_listados CLIPPED ,
														"/nohup:",v_pid USING "&&&&&",":",
																	v_proceso_cod USING "&&&&&",":",
																	v_opera_cod USING "&&&&&" ," 2>&1 &"

			DISPLAY v_comando                        
			RUN v_comando
			IF(STATUS)THEN
				DISPLAY "Reverso Operativo", 
									 "Ocurrió un error al iniciar el proceso batch"
			ELSE
				# Se indica que se realizo el proceso de carga
				CALL fn_mensaje("Reverso Operativo",
                         "Se ha iniciado el proceso batch... \nPodrá revisar el detalle en el monitoreo de procesos para el pid " ||v_pid,
                         "about")
			END IF
		END IF
   END IF
END FUNCTION

