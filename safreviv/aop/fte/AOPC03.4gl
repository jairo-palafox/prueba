################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => AOP                                                      #
#Programa          => AOPC03                                                   #
#Objetivo          => Pantalla para la consulta de historicos del proceso      # 
#							 de ajustes operativos                                    #
#Fecha inicio      => 16/05/2013                                               #
################################################################################
DATABASE safre_viv

GLOBALS "AOPC03.inc"

PRIVATE DEFINE v_tipo_proceso                SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog                    VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario                     VARCHAR(30) -- Almacena al usuario

#Parametros de busqueda
PRIVATE DEFINE v_condicion                   STRING
PRIVATE DEFINE v_proceso_cod                 DECIMAL(9,0)
PRIVATE DEFINE v_cve_estado						SMALLINT
PRIVATE DEFINE v_fproceso							DATE
PRIVATE DEFINE v_datos                       SMALLINT

#Lista de reversos
PRIVATE DEFINE v_lista DYNAMIC ARRAY OF lista

#Variables para los reportes
PRIVATE DEFINE v_folio_ajustado					info_folio
PRIVATE DEFINE v_folio_reverso					info_reverso
PRIVATE DEFINE v_cifras 							cifras

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

   OPEN WINDOW vtn_aopc031 WITH FORM "AOPC031"

      LET ventana = ui.Window.forName("vtn_aopc031")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_aopc031

END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
	DEFINE cb_combo            ui.ComboBox
	DEFINE v_catalogo				STRING
	DEFINE v_clave					SMALLINT
	DEFINE v_desc					VARCHAR(50)
	
   #Se inicializan las valiables del filtro
   INITIALIZE v_proceso_cod         TO NULL
	INITIALIZE v_cve_estado          TO NULL
	INITIALIZE v_fproceso            TO NULL
	INITIALIZE v_condicion           TO NULL
	
   LET v_datos = 0

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)
	LET cb_combo = ui.ComboBox.forName("formonly.cve_estado")

	#Seccion que pintara los parametros de busqueda
   CONSTRUCT v_condicion ON aop.proceso_cod, aop.cve_estado, aop.f_proceso
                  FROM proceso_cod, cve_estado, fproceso
      BEFORE CONSTRUCT
         CLEAR FORM
         CLEAR SCREEN
			
			CALL cb_combo.clear()

			LET v_catalogo =	"SELECT ",
									"cve_estado, ",
									"cve_estado || ' - ' || desc_estado ",
									"FROM cat_estado_ajuste ",
									"ORDER BY cve_estado" 

			PREPARE exe_catalogo FROM v_catalogo
			DECLARE cur_catalogo CURSOR FOR exe_catalogo                                                 
			FOREACH cur_catalogo INTO v_clave, v_desc
				IF v_clave IS NOT NULL THEN
					CALL cb_combo.addItem(v_clave, v_desc)
				END IF
			END FOREACH
			
      ON ACTION ACCEPT
         LET v_proceso_cod          = GET_FLDBUF(proceso_cod)
			LET v_cve_estado           = GET_FLDBUF(cve_estado)
			LET v_fproceso             = GET_FLDBUF(fproceso)
         LET INT_FLAG = 0
         IF v_proceso_cod IS NULL AND v_cve_estado IS NULL AND v_fproceso IS NULL THEN
            CALL fn_mensaje("Ajustes Operativos",
                         "Debe de ingresar algun para la búsqueda.",
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

	DEFINE i 									SMALLINT

	LET v_consulta_reverso = 	"SELECT ",
										"aop.f_proceso, ",
										"aop.folio, ",
										"aop.folio_ajustado, ",
										"aop.proceso_cod || ' - ' || proc.proceso_desc, ",
										"aop.cve_estado || ' - ' || cat.desc_estado, ",
										"usr.usuario_desc ",
										"FROM aop_ctr_ajuste aop ",
										"INNER JOIN cat_proceso proc ON proc.proceso_cod = aop.proceso_cod ",
										"INNER JOIN cat_estado_ajuste cat ON cat.cve_estado = aop.cve_estado ",
										"LEFT JOIN seg_usuario usr ON usr.usuario_cod = aop.usuario ",
										"WHERE ", v_condicion CLIPPED
   PREPARE exe_consulta_reverso FROM v_consulta_reverso
	DECLARE cur_consulta_reverso CURSOR FOR exe_consulta_reverso

	LET i = 1
   FOREACH cur_consulta_reverso INTO v_lista[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista.deleteElement(v_lista.getLength())
   CLOSE cur_consulta_reverso
   FREE cur_consulta_reverso

   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()

	#Se muestran los datos en pantalla
   DIALOG   ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista       TO lista.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)

      ON ACTION ACCEPT
			LET v_folio_reverso.folio = v_lista[ARR_CURR()].folio
			LET v_folio_reverso.f_proceso = v_lista[ARR_CURR()].f_proceso
			LET v_folio_reverso.folio_ajustado = v_lista[ARR_CURR()].folio_ajustado
			LET v_folio_reverso.estado = v_lista[ARR_CURR()].estado
			LET v_folio_reverso.usuario_desc = v_lista[ARR_CURR()].usuario
			
			CALL fn_genera_reporte() 
         RETURN 1
         EXIT DIALOG
        
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG
   END DIALOG
   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte()
	
	DEFINE reporte          		om.SaxDocumentHandler
	DEFINE v_consulta_folio			STRING
	DEFINE v_consulta_cifras		STRING

	LET v_folio_ajustado.folio = v_folio_reverso.folio_ajustado
	LET v_consulta_folio = 	"SELECT ",
										"proc.modulo_cod || ' - ' || mod.modulo_desc, ",
										"glo.proceso_cod || ' - ' || proc.proceso_desc, ",
										"usr.usuario_desc ",
									"FROM glo_folio glo ",
									"INNER JOIN cat_proceso proc ON proc.proceso_cod = glo.proceso_cod ",
									"INNER JOIN seg_modulo mod ON mod.modulo_cod = proc.modulo_cod ",
									"INNER JOIN seg_usuario usr ON usr.usuario_cod = glo.usuario ",
									"WHERE glo.folio = ?"
	PREPARE exe_consulta_folio FROM v_consulta_folio 
   EXECUTE exe_consulta_folio USING v_folio_ajustado.folio 
										INTO 	v_folio_ajustado.modulo,
												v_folio_ajustado.proceso_desc,
												v_folio_ajustado.usuario_desc

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
								 
   IF fgl_report_loadCurrentSettings("AOPC03.4rp") THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(TRUE)
      LET reporte = fgl_report_commitCurrentSettings()
      
      IF reporte IS NOT NULL THEN
         START REPORT cifras TO XML HANDLER reporte
				FOREACH cur_consulta_cifras USING v_folio_reverso.folio INTO v_cifras.*
					OUTPUT TO REPORT cifras(v_cifras.*)
				END FOREACH
         FINISH REPORT cifras
      END IF
   END IF
   CLOSE cur_consulta_cifras
   FREE cur_consulta_cifras
END FUNCTION

REPORT cifras(p_cifras)
   DEFINE p_cifras                	cifras
	DEFINE v_today							DATE

   FORMAT

   FIRST PAGE HEADER
		LET v_today = TODAY

		PRINTX v_today USING 'dd-mm-yyyy'

      #Seccion para los datos del folio de reversos operativos
      PRINTX   v_folio_reverso.folio,
					v_folio_reverso.f_proceso USING 'dd-mm-yyyy',
					v_folio_reverso.estado

      #Seccion para los datos del folio reversado
      PRINTX   v_folio_ajustado.*

	ON EVERY ROW
      PRINTX   p_cifras.*
END REPORT