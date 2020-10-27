################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 11/04/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDC01                                                   #
#Objetivo          =>                                                          #
#                                                                              #
#Fecha inicio      => 09/04/2012                                               #
################################################################################
DATABASE safre_viv

#Variables para el manejo de la pantalla se historicos
PRIVATE DEFINE v_folio           DECIMAL(9,0)
PRIVATE DEFINE v_f_conciliacion  DATE
PRIVATE DEFINE v_condicion       STRING
PRIVATE DEFINE v_datos           SMALLINT

PRIVATE DEFINE v_tipo_proceso    SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog        VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usurio          VARCHAR(30) -- Almacena al usuario

#Variables para almacenar los totales por seccion
PRIVATE DEFINE v_sum_pesos_bdnsviv              DECIMAL(18,2)
PRIVATE DEFINE v_sum_acciones_bdnsviv           DECIMAL(18,2)
PRIVATE DEFINE v_sum_registros_bdnsviv          DECIMAL(9,0)
PRIVATE DEFINE v_sum_acciones_infonavit         DECIMAL(18,2)
PRIVATE DEFINE v_sum_registros_infonavit        DECIMAL(9,0)
PRIVATE DEFINE v_sum_acciones_diferencias       DECIMAL(18,2)
PRIVATE DEFINE v_sum_registros_diferencias      DECIMAL(9,0)

#Arreglo dinamico para la seccion se saldos de la bdnsviv
PRIVATE DEFINE v_lista_bdnsviv DYNAMIC ARRAY OF RECORD
   subcuenta               STRING,
   pesos                   DECIMAL(18,2),
   acciones                DECIMAL(18,2),
   tot_registros           DECIMAL(9,0)
END RECORD

#Arreglo dinamico para la seccion se saldos del infonavit
PRIVATE DEFINE v_lista_infonavit  DYNAMIC ARRAY OF RECORD
   subcuenta_in            STRING,
   acciones_in             DECIMAL(18,2),
   tot_registros_in        DECIMAL(9,0)
END RECORD

#Arreglo dinamico para la seccion se diferencias
PRIVATE DEFINE v_lista_diferencias  DYNAMIC ARRAY OF RECORD
   subcuenta_dif           STRING,
   acciones_dif            DECIMAL(18,2),
   tot_registros_dif       DECIMAL(9,0)
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

   OPEN WINDOW vtn_cbdc01 WITH FORM "CBDC011"

      LET ventana = ui.Window.forName("vtn_cbdc01")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE

   CLOSE WINDOW vtn_cbdc01

END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   #Se inicializan las valiables del filtro
   INITIALIZE v_folio               TO NULL
   INITIALIZE v_f_conciliacion      TO NULL
   INITIALIZE v_condicion           TO NULL
   LET v_datos = 0

   #Se inicializan las valiables del sumario
   INITIALIZE v_sum_pesos_bdnsviv              TO NULL
   INITIALIZE v_sum_acciones_bdnsviv           TO NULL
   INITIALIZE v_sum_registros_bdnsviv          TO NULL
   INITIALIZE v_sum_acciones_infonavit         TO NULL
   INITIALIZE v_sum_registros_infonavit        TO NULL
   INITIALIZE v_sum_acciones_diferencias       TO NULL
   INITIALIZE v_sum_registros_diferencias      TO NULL

   #Se inicializan las listas
   INITIALIZE v_lista_bdnsviv       TO NULL
   INITIALIZE v_lista_infonavit     TO NULL
   INITIALIZE v_lista_diferencias   TO NULL
   
   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)
   CALL forma.setElementHidden("group3",1)
   CALL forma.setElementHidden("group4",1)

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
      CALL fn_busca_historico()
      RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
   DIALOG   ATTRIBUTES(UNBUFFERED) 
      DISPLAY ARRAY v_lista_bdnsviv       TO lista_bdnsviv.* END DISPLAY
      DISPLAY ARRAY v_lista_infonavit     TO lista_infonavit.* END DISPLAY
      DISPLAY ARRAY v_lista_diferencias   TO lista_diferencias.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)
         CALL forma.setElementHidden("group3",0)
         CALL forma.setElementHidden("group4",0)

         DISPLAY v_sum_pesos_bdnsviv         TO sum_pesos_bdnsviv
         DISPLAY v_sum_acciones_bdnsviv      TO sum_acciones_bdnsviv
         DISPLAY v_sum_registros_bdnsviv     TO sum_registros_bdnsviv
         DISPLAY v_sum_acciones_infonavit    TO sum_acciones_infonavit
         DISPLAY v_sum_registros_infonavit   TO sum_registros_infonavit
         DISPLAY v_sum_acciones_diferencias  TO sum_acciones_diferencias
         DISPLAY v_sum_registros_diferencias TO sum_registros_diferencias
         DISPLAY v_folio                     TO folio
         DISPLAY v_f_conciliacion            TO f_conciliacion

      ON ACTION ACCEPT
         LET v_datos = 0
         RETURN 1
         EXIT DIALOG
        
      ON ACTION cancelar
         RETURN 0
         EXIT DIALOG

      ON ACTION reporte
         CALL fn_genera_reporte()
         RETURN 1
          
   END DIALOG 
END FUNCTION

PRIVATE FUNCTION fn_busca_historico()
   DEFINE i                         SMALLINT
   DEFINE v_subcuenta               LIKE cbd_cifras_concilia_global.subcuenta
   DEFINE v_subcuenta_des           VARCHAR(50)
   DEFINE v_acciones_inf            LIKE cbd_cifras_concilia_global.monto_acciones
   DEFINE v_pesos_inf               LIKE cbd_cifras_concilia_global.monto_pesos
   DEFINE v_registros_inf           LIKE cbd_cifras_concilia_global.total_cuentas
   DEFINE v_acciones_bd             LIKE cbd_cifras_concilia_global.monto_acciones
   DEFINE v_pesos_bd                LIKE cbd_cifras_concilia_global.monto_pesos
   DEFINE v_registros_bd            LIKE cbd_cifras_concilia_global.total_cuentas

   #Variables para el manejo de las consultas
   DEFINE v_consulta_proceso           STRING
   DEFINE v_consulta_cifras           STRING

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

   #Se buscan las cifras globales del archivi cargade
   LET v_consulta_cifras = "SELECT ",
                              "cbd.subcuenta, ",
                              "cbd.subcuenta || ' - ' || cat.subcuenta_desc, ",
                              "cbd.monto_acciones,  ",
                              "cbd.monto_pesos,  ",
                              "cbd.total_cuentas,  ",
                              "cbd.monto_acciones_bd,  ",
                              "cbd.monto_pesos_bd,  ",
                              "cbd.total_cuentas_bd ",
                           "FROM cbd_cifras_concilia_global cbd ",
                           "INNER JOIN cat_subcuenta cat ON cat.subcuenta = cbd.subcuenta ",
                           "WHERE cbd.subcuenta IN (4,8) ",
                           "AND cbd.folio = ? ",
                           "ORDER BY cbd.subcuenta"

   PREPARE exe_consulta_cifras FROM v_consulta_cifras
   DECLARE cur_consulta_cifras CURSOR FOR exe_consulta_cifras
   
   LET v_sum_pesos_bdnsviv = 0
   LET v_sum_acciones_bdnsviv = 0
   LET v_sum_registros_bdnsviv = 0

   LET v_sum_acciones_infonavit = 0
   LET v_sum_registros_infonavit = 0

   LET v_sum_acciones_diferencias = 0
   LET v_sum_registros_diferencias = 0

   LET i = 1
   FOREACH cur_consulta_cifras USING v_folio INTO  v_subcuenta,
                                                   v_subcuenta_des,
                                                   v_acciones_inf,
                                                   v_pesos_inf,
                                                   v_registros_inf,
                                                   v_acciones_bd,
                                                   v_pesos_bd,
                                                   v_registros_bd
      IF v_subcuenta IS NOT NULL THEN
         #Se llena la seccion de bdnsviv
         LET v_lista_bdnsviv[i].subcuenta = v_subcuenta_des
         LET v_lista_bdnsviv[i].pesos = v_pesos_bd
         LET v_lista_bdnsviv[i].acciones = v_acciones_bd
         LET v_lista_bdnsviv[i].tot_registros = v_registros_bd

         LET v_sum_pesos_bdnsviv = v_sum_pesos_bdnsviv + v_pesos_bd
         LET v_sum_acciones_bdnsviv = v_sum_acciones_bdnsviv + v_acciones_bd
         LET v_sum_registros_bdnsviv = v_sum_registros_bdnsviv + v_registros_bd

         #Se llena la seccion del infonavit
         LET v_lista_infonavit[i].subcuenta_in = v_subcuenta_des
         LET v_lista_infonavit[i].acciones_in = v_acciones_inf
         LET v_lista_infonavit[i].tot_registros_in = v_registros_inf

         LET v_sum_acciones_infonavit = v_sum_acciones_infonavit + v_acciones_inf
         LET v_sum_registros_infonavit = v_sum_registros_infonavit + v_registros_inf

         #Se llena la seccion de diferencias (bdnsviv - infonavit)
         LET v_lista_diferencias[i].subcuenta_dif =  v_subcuenta_des
         LET v_lista_diferencias[i].acciones_dif = v_acciones_bd - v_acciones_inf
         LET v_lista_diferencias[i].tot_registros_dif = v_registros_bd - v_registros_inf

         LET v_sum_acciones_diferencias = v_sum_acciones_diferencias + v_lista_diferencias[i].acciones_dif
         LET v_sum_registros_diferencias = v_sum_registros_diferencias + v_lista_diferencias[i].tot_registros_dif
      END IF 
      LET i = i + 1
   END FOREACH

   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE reporte          om.SaxDocumentHandler
   
   IF fgl_report_loadCurrentSettings("CBDC01.4rp") THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(TRUE)
      LET reporte = fgl_report_commitCurrentSettings()
      
      IF reporte IS NOT NULL THEN
         START REPORT cifras TO XML HANDLER reporte
         OUTPUT TO REPORT cifras(1)
         FINISH REPORT cifras
      END IF
   END IF
END FUNCTION

REPORT cifras(p_item)
   DEFINE p_item                SMALLINT

   DEFINE v_today               DATE

   #Variables para mostrar la tabla bdnsviv
   DEFINE subcuenta97               STRING
   DEFINE pesos97                   DECIMAL(18,2)
   DEFINE acciones97                DECIMAL(18,2)
   DEFINE tot_registros97           DECIMAL(9,0)

   DEFINE subcuenta92               STRING
   DEFINE pesos92                   DECIMAL(18,2)
   DEFINE acciones92                DECIMAL(18,2)
   DEFINE tot_registros92           DECIMAL(9,0)

   #Variables para mostrar la tabla infonavit
   DEFINE acciones_in97             DECIMAL(18,2)
   DEFINE tot_registros_in97        DECIMAL(9,0)

   DEFINE acciones_in92             DECIMAL(18,2)
   DEFINE tot_registros_in92        DECIMAL(9,0)

   #Variables para mostrar la tabla de diferencias
   DEFINE acciones_dif97             DECIMAL(18,2)
   DEFINE tot_registros_dif97        DECIMAL(9,0)

   DEFINE acciones_dif92             DECIMAL(18,2)
   DEFINE tot_registros_dif92        DECIMAL(9,0)

   FORMAT

   FIRST PAGE HEADER

      LET v_today = TODAY

      #Se llenan los datos que se mandaran al teporte
      LET subcuenta97 = v_lista_bdnsviv[1].subcuenta
      LET pesos97 = v_lista_bdnsviv[1].pesos
      LET acciones97 = v_lista_bdnsviv[1].acciones
      LET tot_registros97 = v_lista_bdnsviv[1].tot_registros

      LET subcuenta92 = v_lista_bdnsviv[2].subcuenta
      LET pesos92 = v_lista_bdnsviv[2].pesos
      LET acciones92 = v_lista_bdnsviv[2].acciones
      LET tot_registros92 = v_lista_bdnsviv[2].tot_registros

      LET acciones_in97 = v_lista_infonavit[1].acciones_in
      LET tot_registros_in97 = v_lista_infonavit[1].tot_registros_in

      LET acciones_in92 = v_lista_infonavit[2].acciones_in
      LET tot_registros_in92 = v_lista_infonavit[2].tot_registros_in

      LET acciones_dif97 = v_lista_diferencias[1].acciones_dif
      LET tot_registros_dif97 = v_lista_diferencias[1].tot_registros_dif

      LET acciones_dif92 = v_lista_diferencias[2].acciones_dif
      LET tot_registros_dif92 = v_lista_diferencias[2].tot_registros_dif

      #Seccion del encabezado
      PRINTX   v_folio,
               v_f_conciliacion USING "DD-MM-YYYY",
               v_today USING "DD-MM-YYYY"

      #Seccion de la tabla de bdnsviv
      PRINTX   subcuenta97,
               pesos97,
               acciones97,
               tot_registros97,
               subcuenta92,
               pesos92,
               acciones92,
               tot_registros92

      #Seccion de la tabla infonavit
      PRINTX   acciones_in97,
               tot_registros_in97,
               acciones_in92,
               tot_registros_in92

      #Seccion de la tabla de diferencias
      PRINTX   acciones_dif97,
               tot_registros_dif97,
               acciones_dif92,
               tot_registros_dif92

      #Seccion de sumarios
      PRINTX   v_sum_pesos_bdnsviv,
               v_sum_acciones_bdnsviv,
               v_sum_registros_bdnsviv,
               v_sum_acciones_infonavit,
               v_sum_registros_infonavit,
               v_sum_acciones_diferencias,
               v_sum_registros_diferencias
END REPORT