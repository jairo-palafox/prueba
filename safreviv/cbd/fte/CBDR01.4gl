################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 11/04/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDR01                                                   #
#Objetivo          =>  #
#Fecha inicio      => 09/04/2012                                               #
################################################################################
DATABASE safre_viv

PRIVATE DEFINE v_tipo_proceso SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog     VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usurio       VARCHAR(30) -- Almacena al usuario

#Variables para el filtro de busqueda
PRIVATE DEFINE v_condicion         STRING
PRIVATE DEFINE v_folio             DECIMAL(9,0)
PRIVATE DEFINE v_f_conciliacion    DATE
PRIVATE DEFINE v_datos             SMALLINT

#Variables para la seccion de sumario
PRIVATE DEFINE v_sum_pesos          DECIMAL(18,2)
PRIVATE DEFINE v_sum_acciones       DECIMAL(18,2)
PRIVATE DEFINE v_sum_registros      DECIMAL(15,0)

PRIVATE DEFINE v_lista_bdnsviv DYNAMIC ARRAY OF RECORD
   subcuenta        SMALLINT,
   subcuenta_desc   VARCHAR(50),
   pesos            DECIMAL(18,2),
   acciones         DECIMAL(18,2),
   registros        DECIMAL(15,0)
END RECORD

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form

MAIN
   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1
   
   -- se asignan los parametros que vienen del fglrun
   LET v_usurio       = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)

   -- se crear el archivo log
   CALL STARTLOG(v_usurio CLIPPED|| ".CBDR013.log")

   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   OPEN WINDOW vtn_cbdr01 WITH FORM "CBDR011"

      LET ventana = ui.Window.forName("vtn_cbdr01")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_cbdr01
   
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   #Se inicializan las valiables del filtro
   INITIALIZE v_folio               TO NULL
   INITIALIZE v_f_conciliacion      TO NULL
   INITIALIZE v_condicion           TO NULL
   LET v_datos = 0

   #Se inicializan las valiables del sumario
   INITIALIZE v_sum_pesos              TO NULL
   INITIALIZE v_sum_acciones           TO NULL
   INITIALIZE v_sum_registros          TO NULL

   #Se inicializan las listas
   INITIALIZE v_lista_bdnsviv       TO NULL

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)

   #Seccion que pintara los parametros de busqueda
   CONSTRUCT v_condicion ON folio, f_operacion
                  FROM folio, f_conciliacion
      BEFORE CONSTRUCT
         CLEAR FORM
         CLEAR SCREEN

      ON ACTION ACCEPT
         LET v_folio             = GET_FLDBUF(folio)
         LET v_f_conciliacion    = GET_FLDBUF(f_conciliacion)
         LET INT_FLAG = 0
         IF v_folio IS NULL AND v_f_conciliacion IS NULL THEN
            CALL fn_mensaje("Conciliacion",
                         "Debe de ingresar algún campo de búsqueda.",
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
      CALL fn_busca_proceso()
      RETURN 1
   ELSE
      RETURN 0
   END IF
   
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
   DEFINE v_respuesta         INTEGER
   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_bdnsviv   TO lista_bdnsviv.*
       END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)

         DISPLAY v_sum_pesos           TO sum_pesos
         DISPLAY v_sum_acciones        TO sum_acciones
         DISPLAY v_sum_registros       TO sum_registros
         DISPLAY v_folio               TO folio
         DISPLAY v_f_conciliacion      TO f_conciliacion

      ON ACTION Reversar
         CALL fn_ventana_confirma("Atención",
                    "¿Desea ejecutar el reverso de la Conciliación?",
                     "quest") RETURNING v_respuesta
         IF v_respuesta = 1 THEN
            CALL fn_reversar_conciliacion()
            LET v_datos = 0
         END IF
         RETURN 1
         EXIT DIALOG
      
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG
   END DIALOG
END FUNCTION

PRIVATE FUNCTION fn_busca_proceso()
   DEFINE v_consulta_proceso              STRING
   DEFINE v_consulta_cifras               STRING
   DEFINE v_consulta_interface            STRING
   DEFINE i                               SMALLINT

   DEFINE v_folio_interface               DECIMAL(9,0)
   DEFINE v_nom_archivo                   VARCHAR(40)

   LET v_consulta_proceso = "SELECT FIRST 1 ",
                              "folio, ",
                              "f_operacion ",
                              "FROM cbd_cza_bdnsviv ",
                              "WHERE ", v_condicion CLIPPED
                              
   PREPARE exe_consulta_proceso FROM v_consulta_proceso 
   EXECUTE exe_consulta_proceso INTO v_folio,v_f_conciliacion

   IF v_folio IS NULL OR v_f_conciliacion IS NULL THEN
      CALL fn_mensaje("Conciliacion",
                         "No se encontraron resultados en la búsqueda.",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

   #Se valida que no se alla generado el archivo de salida
   LET v_consulta_interface = "SELECT ",
                              "folio, ",
                              "nombre_archivo ",
                              "FROM cbd_ctr_interface ",
                              "WHERE f_corte = ?"
   PREPARE exe_consulta_interface FROM v_consulta_interface
   EXECUTE exe_consulta_interface USING v_f_conciliacion INTO v_folio_interface,
                                                              v_nom_archivo
   IF v_folio_interface IS NOT NULL THEN
      CALL fn_mensaje("Conciliacion",
                         "El proceso de conciliación con fecha de corte " ||
                         v_f_conciliacion || " ya genero \nel "||
                         "archivo de salida " || v_nom_archivo CLIPPED || " por lo que no "||
                         "puede ser reversado.",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

   LET v_consulta_cifras =   "SELECT ",
                                 "cbd.subcuenta, ",
                                 "(cbd.subcuenta || ' - ' || cat.subcuenta_desc), ",
                                 "cbd.monto_pesos_bd, ",
                                 "cbd.monto_acciones_bd, ",
                                 "cbd.total_cuentas_bd ",
                              "FROM cbd_cifras_concilia_global cbd ",
                              "INNER JOIN cat_subcuenta cat ON cat.subcuenta = cbd.subcuenta ",
                              "WHERE cbd.subcuenta IN (4,8) ",
                              "AND cbd.folio = ? ",
                              "ORDER BY cbd.subcuenta"
   PREPARE exe_consulta_cifras FROM v_consulta_cifras
   DECLARE cur_consulta_cifras CURSOR FOR exe_consulta_cifras

   LET v_sum_pesos = 0
   LET v_sum_acciones = 0
   LET v_sum_registros = 0

   LET i = 1
   FOREACH cur_consulta_cifras USING v_folio INTO v_lista_bdnsviv[i].*
      IF v_lista_bdnsviv[i].subcuenta IS NOT NULL THEN
         LET v_sum_pesos = v_sum_pesos + v_lista_bdnsviv[i].pesos
         LET v_sum_acciones = v_sum_acciones + v_lista_bdnsviv[i].acciones
         LET v_sum_registros = v_sum_registros +  v_lista_bdnsviv[i].registros
      END IF
      LET i = i + 1
   END FOREACH
   CALL v_lista_bdnsviv.deleteElement(v_lista_bdnsviv.getLength())
   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION 

PRIVATE FUNCTION fn_reversar_conciliacion()
   DEFINE v_fn_cbd_reverso             STRING
   DEFINE v_consulta_pid               STRING
   DEFINE v_resultado                  SMALLINT
   DEFINE v_mensaje                    VARCHAR(50)
   DEFINE v_pid                        DECIMAL(9,0)

   LET v_fn_cbd_reverso = "EXECUTE FUNCTION safre_viv:fn_cbd_reverso(?,?)"
   PREPARE exe_fn_cbd_reverso FROM v_fn_cbd_reverso
   EXECUTE exe_fn_cbd_reverso USING v_folio, v_usurio INTO v_mensaje

   #Se busca el pid del proceso
   LET v_consulta_pid = "SELECT FIRST 1 pid FROM bat_ctr_operacion WHERE proceso_cod = 2101 AND folio = ?"
   PREPARE exe_consulta_pid FROM v_consulta_pid
   EXECUTE exe_consulta_pid USING v_folio INTO v_pid

   #Se reversan las operaciones
   CALL fn_reversa_operacion(v_pid,2101,2) RETURNING v_resultado  #Reversando la integracion
   CALL fn_reversa_operacion(v_pid,2101,1) RETURNING v_resultado  #Reversando la validacion
   
END FUNCTION