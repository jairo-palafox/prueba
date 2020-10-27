################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 11/04/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDC02                                                   #
#Objetivo          =>                                                          #
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

PRIVATE DEFINE v_lista_archivo DYNAMIC ARRAY OF RECORD
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

   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   OPEN WINDOW vtn_cbdc02 WITH FORM "CBDC021"

      LET ventana = ui.Window.forName("vtn_cbdc02")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
      
   CLOSE WINDOW vtn_cbdc02

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
   INITIALIZE v_lista_archivo       TO NULL

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)

   #Seccion que pintara los parametros de busqueda
   CONSTRUCT v_condicion ON folio, f_corte
                  FROM folio, f_conciliacion
      BEFORE CONSTRUCT
         CLEAR FORM
         CLEAR SCREEN

      ON ACTION ACCEPT
         LET v_folio             = GET_FLDBUF(folio)
         LET v_f_conciliacion    = GET_FLDBUF(f_conciliacion)
         LET INT_FLAG = 0
         IF v_folio IS NULL AND v_f_conciliacion IS NULL THEN
            CALL fn_mensaje("Saldos",
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
   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_archivo   TO lista_archivo.*
       END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)

         DISPLAY v_sum_pesos           TO sum_pesos
         DISPLAY v_sum_acciones        TO sum_acciones
         DISPLAY v_sum_registros       TO sum_registros
         DISPLAY v_folio               TO folio
         DISPLAY v_f_conciliacion      TO f_conciliacion
         
      ON ACTION reporte
         CALL fn_genera_reporte()
         RETURN 1
      
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG
   END DIALOG
END FUNCTION

PRIVATE FUNCTION fn_busca_proceso()
   DEFINE v_consulta_proceso              STRING
   DEFINE v_consulta_cifras               STRING
   DEFINE i                               SMALLINT

   LET v_consulta_proceso = "SELECT FIRST 1 ",
                              "folio, ",
                              "f_corte ",
                              "FROM cbd_ctr_interface ",
                              "WHERE ", v_condicion CLIPPED
                              
   PREPARE exe_consulta_proceso FROM v_consulta_proceso 
   EXECUTE exe_consulta_proceso INTO v_folio,v_f_conciliacion

   IF v_folio IS NULL OR v_f_conciliacion IS NULL THEN
      CALL fn_mensaje("Saldos",
                         "No se encontraron resultados en la búsqueda.",
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

   LET v_consulta_cifras =   "SELECT ",
                                 "sdo.subcuenta, ",
                                 "(sdo.subcuenta || ' - ' || cat.subcuenta_desc), ",
                                 "sdo.monto_pesos, ",
                                 "sdo.monto_acciones, ",
                                 "sdo.total_cuentas ",
                              "FROM cbd_cifras_interface sdo ",
                              "INNER JOIN cat_subcuenta cat ON cat.subcuenta = sdo.subcuenta ",
                              "WHERE sdo.subcuenta IN (4,8,44,42) ",
                              "AND sdo.folio = ? ",
                              "ORDER BY sdo.subcuenta"
   PREPARE exe_consulta_cifras FROM v_consulta_cifras
   DECLARE cur_consulta_cifras CURSOR FOR exe_consulta_cifras

   LET v_sum_pesos = 0
   LET v_sum_acciones = 0
   LET v_sum_registros = 0

   LET i = 1
   FOREACH cur_consulta_cifras USING v_folio INTO v_lista_archivo[i].*
      IF v_lista_archivo[i].subcuenta IS NOT NULL THEN
         LET v_sum_pesos = v_sum_pesos + v_lista_archivo[i].pesos
         LET v_sum_acciones = v_sum_acciones + v_lista_archivo[i].acciones
         LET v_sum_registros = v_sum_registros +  v_lista_archivo[i].registros
      END IF
      LET i = i + 1
   END FOREACH
   CALL v_lista_archivo.deleteElement(v_lista_archivo.getLength())
   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION 

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE reporte          om.SaxDocumentHandler
   DEFINE i                SMALLINT
   
   IF fgl_report_loadCurrentSettings("CBDC02.4rp") THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(TRUE)
      LET reporte = fgl_report_commitCurrentSettings()
      
      IF reporte IS NOT NULL THEN
         START REPORT cifras TO XML HANDLER reporte
            FOR i = 1 TO v_lista_archivo.getLength()
               OUTPUT TO REPORT cifras(v_lista_archivo[i].*)
            END FOR 
         FINISH REPORT cifras
      END IF
   END IF
END FUNCTION

REPORT cifras(p_lista)
   DEFINE p_lista RECORD
      subcuenta        SMALLINT,
      subcuenta_desc   VARCHAR(50),
      pesos            DECIMAL(18,2),
      acciones         DECIMAL(18,2),
      registros        DECIMAL(15,0)
   END RECORD

   DEFINE v_today               DATE
   
   ORDER BY p_lista.subcuenta

   FORMAT

   FIRST PAGE HEADER

      LET v_today = TODAY
      
      PRINTX   v_folio,
               v_f_conciliacion USING "DD-MM-YYYY",
               v_today USING "DD-MM-YYYY"

   ON EVERY ROW
      PRINTX   p_lista.*

   ON LAST ROW
      PRINTX   v_sum_pesos,
               v_sum_acciones,
               v_sum_registros
      
END REPORT