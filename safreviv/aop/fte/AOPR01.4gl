################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => AOP                                                      #
#Programa          => AOPR01                                                   #
#Objetivo          => Pantalla que ejecuta el reverso de la preliquidacion     # 
#							 de ajustes operativos                                    #
#Fecha inicio      => 26/04/2013                                               #
################################################################################
DATABASE safre_viv

GLOBALS "AOPR01.inc"

PRIVATE DEFINE v_tipo_proceso                SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog                    VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario                     VARCHAR(30) -- Almacena al usuario

#Parametros de busqueda
PRIVATE DEFINE v_condicion                   STRING
PRIVATE DEFINE v_folio                       DECIMAL(9,0)
PRIVATE DEFINE v_datos                       SMALLINT

PRIVATE DEFINE v_detalle_reverso					info_reverso
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

   OPEN WINDOW vtn_aopr01 WITH FORM "AOPR011"

      LET ventana = ui.Window.forName("vtn_aopr01")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_aopr01

END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   #Se inicializan las valiables del filtro
   INITIALIZE v_folio               TO NULL
   INITIALIZE v_condicion           TO NULL
	INITIALIZE v_detalle_folio       TO NULL
	INITIALIZE v_detalle_reverso 		TO NULL
   LET v_datos = 0

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)
	CALL forma.setElementHidden("group3",1)

   #Seccion que pintara los parametros de busqueda
   CONSTRUCT v_condicion ON aop.folio
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
	DEFINE v_consulta_reverso           STRING
   DEFINE v_consulta_folio           	STRING
	DEFINE v_consulta_cifras				STRING

	DEFINE i 									SMALLINT

	LET v_consulta_reverso = 	"SELECT ",
										"aop.folio, ",
										"aop.f_proceso, ",
										"aop.folio_ajustado, ",
										"aop.cve_estado, ",
										"cat.desc_estado, ",
										"usr.usuario_desc ",
										"FROM aop_ctr_ajuste aop ",
										"INNER JOIN cat_estado_ajuste cat ON cat.cve_estado = aop.cve_estado ",
										"LEFT JOIN seg_usuario usr ON usr.usuario_cod = aop.usuario ",
										"WHERE ", v_condicion CLIPPED
   PREPARE exe_consulta_reverso FROM v_consulta_reverso
   EXECUTE exe_consulta_reverso INTO 	v_detalle_reverso.*

	IF v_detalle_reverso.folio IS NULL THEN
      CALL fn_mensaje("Ajustes Operativos",
                         "No se encontraron resultados en la búsqueda.",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

	IF v_detalle_reverso.cve_estado = 5 THEN
      CALL fn_mensaje("Ajustes Operativos",
                         "El folio " || v_detalle_reverso.folio || " ya se había reversado con anterioridad",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

	IF v_detalle_reverso.cve_estado = 9 THEN
      CALL fn_mensaje("Ajustes Operativos",
                         "El folio " || v_detalle_reverso.folio || " fue rechazado con anterioridad",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

	IF v_detalle_reverso.cve_estado > 2 THEN
      CALL fn_mensaje("Ajustes Operativos",
                         "El folio " || v_detalle_reverso.folio || " ya ejecuto la funcion de reverso de negocio y no se puede reversar",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF
   
   LET v_consulta_folio = "SELECT ",
										"glo.folio, ",
										"proc.modulo_cod || ' - ' || mod.modulo_desc, ",
										"glo.proceso_cod, ",
										"proc.proceso_desc, ",
										"usr.usuario_desc ",
									"FROM glo_folio glo ",
									"INNER JOIN cat_proceso proc ON proc.proceso_cod = glo.proceso_cod ",
									"INNER JOIN seg_modulo mod ON mod.modulo_cod = proc.modulo_cod ",
									"INNER JOIN seg_usuario usr ON usr.usuario_cod = glo.usuario ",
									"WHERE glo.folio = ? "
   PREPARE exe_consulta_folio FROM v_consulta_folio 
   EXECUTE exe_consulta_folio USING v_detalle_reverso.folio_ajustado
										INTO 	v_detalle_folio.folio,
												v_detalle_folio.modulo,
												v_detalle_folio.proceso_cod,
												v_detalle_folio.proceso_desc,
												v_detalle_folio.usuario_desc

	#Se consulta el resumen de las cifras
	LET v_consulta_cifras = "SELECT ",
										"aop.subcuenta || ' - ' || sub_cta.subcuenta_desc, ",
										"aop.fondo_inversion || ' - ' || cat_fondo.razon_social, ",
										"aop.total_acciones, ",
										"aop.total_pesos ",
									"FROM aop_cifras_ajuste aop ",
									"INNER JOIN cat_subcuenta sub_cta ON sub_cta.subcuenta = aop.subcuenta ",
									"INNER JOIN cat_fondo_local cat_fondo ON cat_fondo.fondo = aop.fondo_inversion ",
									"WHERE aop.folio = ? ", 
									"ORDER BY 1,2"
	PREPARE exe_consulta_cifras FROM v_consulta_cifras
	DECLARE cur_consulta_cifras CURSOR FOR exe_consulta_cifras

	LET i = 1
   FOREACH cur_consulta_cifras USING v_detalle_reverso.folio INTO v_lista_cifras[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_cifras.deleteElement(v_lista_cifras.getLength())
   CLOSE cur_consulta_cifras
   FREE cur_consulta_cifras

   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()

	#Se muestran los datos en pantalla
   DIALOG   ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_cifras       TO lista_cifras.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)
			CALL forma.setElementHidden("group3",0)

			DISPLAY v_detalle_reverso.folio        TO folio
			DISPLAY v_detalle_folio.folio        	TO folio_ajuste
			DISPLAY v_detalle_folio.modulo			TO modulo
			DISPLAY v_detalle_folio.proceso_cod    TO proceso_cod
			DISPLAY v_detalle_folio.proceso_desc   TO proceso_desc
			DISPLAY v_detalle_folio.usuario_desc   TO usuario_desc

      ON ACTION reversar
			CALL fn_ejecuta_reverso() 
			LET v_datos = 0
         RETURN 1
         EXIT DIALOG
        
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG
   END DIALOG
   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_reverso()
	DEFINE v_fn_aop_reverso					STRING
	DEFINE v_resultado                  SMALLINT
   DEFINE v_mensaje                    VARCHAR(100)

   LET v_fn_aop_reverso = "EXECUTE FUNCTION fn_aop_reverso(?,?)"
   PREPARE exe_fn_aop_reverso FROM v_fn_aop_reverso
   EXECUTE exe_fn_aop_reverso USING v_detalle_reverso.folio, v_usuario 
										INTO v_resultado, v_mensaje

	CALL fn_mensaje("Ajustes Operativos",
                         v_mensaje,
                         "about")
END FUNCTION