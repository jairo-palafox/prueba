################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 26/04/2013                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => AOP                                                      #
#Programa          => AOPL01                                                   #
#Objetivo          => Pantalla que ejecuta la preliquidacion de ajustes        #
#							 operativos                                               #
#Fecha inicio      => 26/04/2013                                               #
################################################################################
DATABASE safre_viv

GLOBALS "AOPL01.inc"

PRIVATE DEFINE v_tipo_proceso                SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog                    VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario                     VARCHAR(30) -- Almacena al usuario

#Parametros de busqueda
PRIVATE DEFINE v_condicion                   STRING
PRIVATE DEFINE v_folio                       DECIMAL(9,0)
PRIVATE DEFINE v_datos                       SMALLINT

PRIVATE DEFINE v_detalle_folio					info_folio

#Lista para los movimientos
PRIVATE DEFINE v_lista_cifras DYNAMIC ARRAY OF cifras

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

   OPEN WINDOW vtn_aopl01 WITH FORM "AOPL011"

      LET ventana = ui.Window.forName("vtn_aopl01")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_aopl01

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
	CALL forma.setElementHidden("group5",1)

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
   DEFINE v_consulta_folio           	STRING
   DEFINE v_consulta_f_liquida         STRING
	DEFINE v_consulta_cifras				STRING
	DEFINE v_consulta_decreto				STRING
	DEFINE v_consulta_fondo					STRING

	DEFINE i 									SMALLINT
   
   LET v_consulta_folio = "SELECT ",
										"glo.folio, ",
										"proc.modulo_cod || ' - ' || mod.modulo_desc, ",
										"glo.proceso_cod, ",
										"proc.proceso_desc, ",
										"usr.usuario_desc ",
									"FROM glo_folio glo ",
									"INNER JOIN cat_proceso proc ON proc.proceso_cod = glo.proceso_cod ",
									"INNER JOIN seg_modulo mod ON mod.modulo_cod = proc.modulo_cod ",
									"LEFT JOIN seg_usuario usr ON usr.usuario_cod = glo.usuario ",
									"WHERE ", v_condicion CLIPPED
   PREPARE exe_consulta_folio FROM v_consulta_folio 
   EXECUTE exe_consulta_folio INTO 	v_detalle_folio.folio,
												v_detalle_folio.modulo,
												v_detalle_folio.proceso_cod,
												v_detalle_folio.proceso_desc,
												v_detalle_folio.usuario_desc

   IF v_detalle_folio.folio IS NULL THEN
      CALL fn_mensaje("Ajustes Operativos",
                         "No se encontraron resultados en la búsqueda.",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

   #Se valida que ya exista la informacion de saldos por periodo necesaria para la conciliacion
   LET v_consulta_f_liquida =  "SELECT FIRST 1 ",
										"f_liquida ",
										"FROM cta_movimiento ",
										"WHERE folio_liquida = ?"
   PREPARE exe_consulta_f_liquida FROM v_consulta_f_liquida
   EXECUTE exe_consulta_f_liquida USING v_detalle_folio.folio INTO v_detalle_folio.f_liquida

   IF v_detalle_folio.f_liquida IS NULL THEN

      -- Se busca el folio en cta_decreto
      LET v_consulta_f_liquida =  "SELECT FIRST 1 ",
										"f_liquida ",
										"FROM cta_decreto ",
										"WHERE folio_liquida = ?"
      PREPARE exe_consulta_f_liquida_decreto FROM v_consulta_f_liquida
      EXECUTE exe_consulta_f_liquida_decreto USING v_detalle_folio.folio INTO v_detalle_folio.f_liquida

      IF ( v_detalle_folio.f_liquida IS NULL ) THEN

         -- se busca en fondo 72
         LET v_consulta_f_liquida =  "SELECT FIRST 1 ",
	    							    "f_liquida ",
										"FROM cta_fondo72 ",
										"WHERE folio_liquida = ?"
         PREPARE exe_consulta_f_liquida_fondo72 FROM v_consulta_f_liquida
         EXECUTE exe_consulta_f_liquida_fondo72 USING v_detalle_folio.folio INTO v_detalle_folio.f_liquida

         IF ( v_detalle_folio.f_liquida IS NULL ) THEN
   
            CALL fn_mensaje("Ajustes Operativos",
                               "El folio " || v_detalle_folio.folio || " no tiene movimientos liquidados",
                               "about")
            LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
            RETURN
         END IF
      END IF
   END IF
	#Una vez que se valida que existen movimientos liquidados para el folio se consulta el resumen de las cifras
	LET v_consulta_cifras = "SELECT ",
										"mov.subcuenta || ' - ' || sub_cta.subcuenta_desc, ",
										"mov.fondo_inversion || ' - ' || cat_fondo.razon_social, ",
										"SUM(mov.monto_acciones), ",
										"SUM(mov.monto_pesos) ",
									"FROM cta_movimiento mov ",
									"INNER JOIN cat_subcuenta sub_cta ON sub_cta.subcuenta = mov.subcuenta ",
									"INNER JOIN cat_fondo_local cat_fondo ON cat_fondo.fondo = mov.fondo_inversion ",
									"WHERE mov.folio_liquida = ? ",
									"GROUP BY 1,2"
	PREPARE exe_consulta_cifras FROM v_consulta_cifras
	DECLARE cur_consulta_cifras CURSOR FOR exe_consulta_cifras

	LET i = 1
   FOREACH cur_consulta_cifras USING v_detalle_folio.folio INTO v_lista_cifras[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_cifras.deleteElement(v_lista_cifras.getLength())
   CLOSE cur_consulta_cifras
   FREE cur_consulta_cifras

	#Se buscan los posibles movimentos para decreto
	LET v_consulta_decreto = "SELECT ",
										"mov.subcuenta || ' - ' || sub_cta.subcuenta_desc, ",
										"mov.fondo_inversion || ' - ' || cat_fondo.razon_social, ",
										"SUM(mov.monto_acciones), ",
										"SUM(mov.monto_pesos) ",
									"FROM cta_decreto mov ",
									"INNER JOIN cat_subcuenta sub_cta ON sub_cta.subcuenta = mov.subcuenta ",
									"INNER JOIN cat_fondo_local cat_fondo ON cat_fondo.fondo = mov.fondo_inversion ",
									"WHERE mov.folio_liquida = ? ",
									"GROUP BY 1,2"
	PREPARE exe_consulta_decreto FROM v_consulta_decreto
	DECLARE cur_consulta_decreto CURSOR FOR exe_consulta_decreto

	LET i = v_lista_cifras.getLength() + 1
	FOREACH cur_consulta_decreto USING v_detalle_folio.folio INTO v_lista_cifras[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_cifras.deleteElement(v_lista_cifras.getLength())
   CLOSE cur_consulta_decreto
   FREE cur_consulta_decreto

	#Se buscan los posibles movimentos para Fondo 72
	LET v_consulta_fondo = "SELECT ",
										"mov.subcuenta || ' - ' || sub_cta.subcuenta_desc, ",
										"cat_fondo.fondo || ' - ' || cat_fondo.razon_social, ",
										"0.000000, ",
										"SUM(mov.importe) ",
									"FROM cta_fondo72 mov ",
									"INNER JOIN cat_subcuenta sub_cta ON sub_cta.subcuenta = mov.subcuenta ",
									"INNER JOIN cat_fondo_local cat_fondo ON cat_fondo.fondo = 10 ",
									"WHERE mov.folio_liquida = ? ",
									"GROUP BY 1,2"
	PREPARE exe_consulta_fondo FROM v_consulta_fondo
	DECLARE cur_consulta_fondo CURSOR FOR exe_consulta_fondo

	LET i = v_lista_cifras.getLength() + 1
	FOREACH cur_consulta_fondo USING v_detalle_folio.folio INTO v_lista_cifras[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_cifras.deleteElement(v_lista_cifras.getLength())
   CLOSE cur_consulta_fondo
   FREE cur_consulta_fondo

   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
   DEFINE v_valida_reverso       SMALLINT
	DEFINE v_valida_funcion       SMALLINT
	DEFINE v_nombre_funcion			CHAR(50)
	DEFINE v_funcion_negocio		STRING

	#Se valida si el proceso tiene dada de alta la funcion de reverso para el negocio 
	SELECT
		nombre_funcion
	INTO
		v_nombre_funcion
	FROM cat_reverso_negocio
	WHERE proceso_cod = v_detalle_folio.proceso_cod

	IF v_nombre_funcion IS NULL THEN
      #No se configuro funcion para el reverso de negocio
		LET v_valida_funcion = 1
	ELSE
		WHENEVER ERROR CONTINUE
			LET v_funcion_negocio = "EXECUTE FUNCTION ", v_nombre_funcion CLIPPED, "(?,?,?)" 
			PREPARE exe_funcion_negocio FROM v_funcion_negocio
			IF SQLCA.SQLCODE <> 0 THEN
            #La funcion no se encuentra dada de alta en la BD
				LET v_valida_funcion = 2
			ELSE
            #Funcion correcta
				LET v_valida_funcion = 0
			END IF
		WHENEVER ERROR STOP
	END IF

	#Se muestran los datos en pantalla
   DIALOG   ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_cifras       TO lista_cifras.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)
			CALL forma.setElementHidden("group3",0)
			IF v_valida_funcion = 1 THEN
				CALL forma.setElementHidden("group4",0)
			END IF
			IF v_valida_funcion = 2 THEN
				CALL forma.setElementHidden("group5",0)
			END IF
				
			DISPLAY v_detalle_folio.f_liquida     	TO f_liquida
			DISPLAY v_detalle_folio.folio        	TO folio
			DISPLAY v_detalle_folio.modulo			TO modulo
			DISPLAY v_detalle_folio.proceso_cod    TO proceso_cod
			DISPLAY v_detalle_folio.proceso_desc   TO proceso_desc
			DISPLAY v_detalle_folio.usuario_desc   TO usuario_desc

      ON ACTION reversar
			IF v_valida_funcion <> 0 THEN
				CALL fn_mensaje("Atención", "No se puede ejecutar el Reverso Operativo porque se detectó un problema con la función de reverso de negocio","info")
				LET v_datos = 0
				RETURN 1
				EXIT DIALOG
			END IF
         CALL fn_valida_reverso() RETURNING v_valida_reverso
			IF v_valida_reverso = 1 THEN
				CALL fn_ejecuta_reverso() 
			END IF
			LET v_datos = 0
         RETURN 1
         EXIT DIALOG
        
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG
   END DIALOG
   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_valida_reverso()
   DEFINE v_mensaje              STRING
	DEFINE v_fn_validacion_cnt		STRING
	DEFINE v_fn_valida_previo		STRING
	DEFINE v_resultado				SMALLINT
	DEFINE v_respuesta				VARCHAR(60)
	DEFINE v_ind_execute				SMALLINT
	DEFINE v_folio_anterior			DECIMAL(9,0)

	LET v_ind_execute = 0

	#Se valida que el folio no alla pasado por un reverso operativo previo
	
	LET v_fn_valida_previo = 	"SELECT FIRST 1 folio ",
										"FROM aop_ctr_ajuste ",
										"WHERE folio_ajustado = ? ",
										"AND cve_estado NOT IN (5,9) "
	PREPARE exe_fn_valida_previo 	FROM v_fn_valida_previo
	EXECUTE exe_fn_valida_previo 	USING v_folio
											INTO v_folio_anterior
	IF v_folio_anterior IS NOT NULL THEN
		LET v_mensaje = "El Reverso Operativo no se puede ejecutar porque el folio ", v_folio, 
							 "\npresenta un reverso operativo previo con folio: ", v_folio_anterior
		CALL fn_mensaje("Atención", v_mensaje,"info")
		RETURN 0
	END IF
	WHENEVER ERROR CONTINUE
		LET v_fn_validacion_cnt = "EXECUTE PROCEDURE fn_reverso_aop_cnt(?,?,?)"
		PREPARE exe_fn_validacion_cnt FROM v_fn_validacion_cnt
		EXECUTE exe_fn_validacion_cnt USING v_folio, v_detalle_folio.proceso_cod, v_ind_execute
												INTO v_resultado, v_respuesta
		IF SQLCA.SQLCODE <> 0 THEN
			LET v_mensaje = "Ocurrio un ERROR al intentar ejecutar la validacion de contabilidad: ", SQLERRMESSAGE
			CALL fn_mensaje("Atención", v_mensaje,"info")
			RETURN 0
		ELSE
			IF v_resultado <> 0 THEN
				LET v_mensaje = "El Reverso Operativo no se puede ejecutar por el siguiente error de validacion: ", v_respuesta
				CALL fn_mensaje("Atención", v_mensaje,"info")
				RETURN 0
			END IF
		END IF
	WHENEVER ERROR STOP
   
   RETURN 1
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_reverso()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   	LIKE seg_modulo.ruta_listados -- Rute del log

   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)
   DEFINE v_comando           STRING

   LET v_proceso_cod = PROCESO
   LET v_opera_cod = OPERACION

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
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

      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,v_usuario)
             RETURNING v_pid

      #Se asigna el nombre del archivo
      LET v_nom_archivo = "NA"

		CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                             "AOPP01",v_nom_archivo,v_usuario)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
			# Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,null,"CTAP01",
                               v_nom_archivo,v_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
				LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/AOPP01.42r ",
															v_usuario," ",
															v_pid," ",
															v_proceso_cod," ",
															v_opera_cod," ",
															v_detalle_folio.folio," '",
															v_nom_archivo,
															"' 1>", v_ruta_listados CLIPPED ,
															"/nohup:",v_pid USING "&&&&&",":",
																		v_proceso_cod USING "&&&&&",":",
																		v_opera_cod USING "&&&&&" ," 2>&1 &"                      
				RUN v_comando
				IF(STATUS)THEN
					CALL fn_mensaje("Ajustes Operativos", 
										 "Ocurrió un error al iniciar el proceso batch",
										 "bn_about")
				ELSE
					# Se indica que se realizo el proceso de carga
					CALL fn_mensaje("Ajustes Operativos", 
										 "Se ha iniciado el proceso de preliquidacion. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " || v_pid,
										 "bn_about")
				END IF
			END IF
		END IF
   END IF
   
END FUNCTION
