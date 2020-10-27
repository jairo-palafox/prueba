################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 11/04/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDC03                                                   #
#Objetivo          =>                                                          #
#                                                                              #
#Fecha inicio      => 09/04/2012                                               #
################################################################################
DATABASE safre_viv

PRIVATE DEFINE v_tipo_proceso                SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog                    VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usurio                      VARCHAR(30) -- Almacena al usuario

#Variables para las fechas del periodo
PRIVATE DEFINE v_f_inicio_infonavit            DATE
PRIVATE DEFINE v_f_fin_infonavit               DATE
PRIVATE DEFINE v_corte_anterior                DATE

#Variables para el filtro de busqueda
PRIVATE DEFINE v_condicion                   STRING
PRIVATE DEFINE v_folio                       DECIMAL(9,0)
PRIVATE DEFINE v_f_conciliacion              DATE
PRIVATE DEFINE v_datos                       SMALLINT

#Variables para la seccion de sumario
PRIVATE DEFINE v_sum_inicio_infonavit          DECIMAL(18,2)
PRIVATE DEFINE v_sum_fin_infonavit             DECIMAL(18,2)
PRIVATE DEFINE v_sum_saldo_concilia            DECIMAL(18,2)
PRIVATE DEFINE v_sum_saldo_bdnsviv             DECIMAL(18,2)
PRIVATE DEFINE v_sum_diferencias               DECIMAL(18,2)

PRIVATE DEFINE v_lista_conciliacion DYNAMIC ARRAY OF RECORD
   subcuenta                                 STRING,
   inicio_infonavit                          DECIMAL(18,2),
   fin_infonavit                             DECIMAL(18,2),
   saldo_a_conciliar                         DECIMAL(18,2),
   saldo_bdnsviv                             DECIMAL(18,2),
   diferencias                               DECIMAL(18,2)
END RECORD

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form

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

   OPEN WINDOW vtn_cbdc03 WITH FORM "CBDC031"
      LET ventana = ui.Window.forName("vtn_cbdc03")
      LET forma = ventana.getForm()
      
      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
   CLOSE WINDOW vtn_cbdc03
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
    #Se inicializan las valiables del filtro
   INITIALIZE v_folio               TO NULL
   INITIALIZE v_f_conciliacion      TO NULL
   INITIALIZE v_condicion           TO NULL
   LET v_datos = 0

   #Se inicializan las valiables para las fechas del periodo
   INITIALIZE v_f_inicio_infonavit            TO NULL
   INITIALIZE v_f_fin_infonavit               TO NULL
   INITIALIZE v_corte_anterior                TO NULL

   #Se inicializan las valiables para la seccion de sumario
   INITIALIZE v_sum_inicio_infonavit          TO NULL
   INITIALIZE v_sum_fin_infonavit             TO NULL
   INITIALIZE v_sum_saldo_concilia            TO NULL
   INITIALIZE v_sum_saldo_bdnsviv             TO NULL
   INITIALIZE v_sum_diferencias               TO NULL

   #Se inicializan las listas
   INITIALIZE v_lista_conciliacion       TO NULL

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
   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_conciliacion   TO lista_conciliacion.*
       END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group2",0)

         DISPLAY v_sum_inicio_infonavit      TO sum_inicio_infonavit
         DISPLAY v_sum_fin_infonavit         TO sum_fin_infonavit
         DISPLAY v_sum_saldo_concilia        TO sum_saldo_concilia
         DISPLAY v_sum_saldo_bdnsviv         TO sum_saldo_bdnsviv
         DISPLAY v_sum_diferencias           TO sum_diferencias
         DISPLAY v_folio                     TO folio
         DISPLAY v_f_conciliacion            TO f_conciliacion

      ON ACTION Reporte
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
   DEFINE v_consulta_infonavit            STRING
   DEFINE v_consulta_conciliacion         STRING

   DEFINE v_folio_valida                  DECIMAL(9,0)
   DEFINE v_mensaje                       STRING

   DEFINE v_subcuenta                     SMALLINT
   DEFINE v_subcuenta_desc                VARCHAR(50)
   DEFINE v_monto                         DECIMAL(18,2)
   DEFINE v_monto_bdnsviv                 DECIMAL(18,2)
   DEFINE i                               SMALLINT

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

   #Se inicializan los sumarios
   LET v_sum_inicio_infonavit = 0
   LET v_sum_fin_infonavit = 0
   LET v_sum_saldo_concilia = 0
   LET v_sum_saldo_bdnsviv = 0
   LET v_sum_diferencias = 0

   #Se genera la informacion para la seccion de saldos iniciales de infonavit
   LET v_f_inicio_infonavit = MDY(MONTH(v_f_conciliacion), 1, YEAR(v_f_conciliacion));
   LET v_corte_anterior = v_f_inicio_infonavit - 1
   LET v_f_fin_infonavit = v_f_conciliacion

   #Se valida que ya se tenga la informacion de la conciliacion del periodo
   LET v_consulta_conciliacion = "SELECT first 1 ",
                                 "folio ",
                                 "FROM cbd_cifras_concilia ",
                                 "where folio = ?"
   PREPARE exe_consulta_conciliacion FROM v_consulta_conciliacion
   EXECUTE exe_consulta_conciliacion USING v_folio INTO v_folio_valida
   IF v_folio_valida IS NULL OR v_folio_valida <> v_folio THEN
      LET v_mensaje = "Aun no se corre el proceso de conciliación para el periodo \n",
                      " del ", v_f_inicio_infonavit USING 'dd-mm-yyyy', " al ",
                      v_f_fin_infonavit USING 'dd-mm-yyyy'
      CALL fn_mensaje("Conciliacion", v_mensaje, "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

   LET v_consulta_infonavit = "SELECT ",
                              "sdo.subcuenta, ",
                              "(sdo.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
                              "sdo.monto_acciones ",
                              "FROM safre_sdo@vivws_tcp:cta_saldo_mensual_global sdo ",
                              "INNER JOIN cat_subcuenta cat on cat.subcuenta = sdo.subcuenta ",
                              "WHERE sdo.subcuenta IN (4,8) ",
                              "AND sdo.fondo_inversion <> 0 ",
                              "AND sdo.f_saldo = ? ",
                              "ORDER BY sdo.subcuenta"
   PREPARE exe_consulta_infonavit FROM v_consulta_infonavit
   DECLARE cur_consulta_infonavit CURSOR FOR exe_consulta_infonavit

   #Se genera la informacion para la seccion de saldos iniciales de infonavit
   FOREACH cur_consulta_infonavit USING v_corte_anterior INTO v_subcuenta, v_subcuenta_desc, v_monto
      IF v_subcuenta IS NOT NULL THEN
         IF v_subcuenta = 4 THEN
            LET i = 1
         END IF
         IF v_subcuenta = 8 THEN
            LET i = 2
         END IF
         LET v_lista_conciliacion[i].subcuenta = v_subcuenta_desc
         LET v_lista_conciliacion[i].inicio_infonavit = v_monto

         #Se incrementa el sumario
         LET v_sum_inicio_infonavit = v_sum_inicio_infonavit + v_monto
      END IF
   END FOREACH

   #Se genera la informacion para la seccion de saldos finales de infonavit
   FOREACH cur_consulta_infonavit USING v_f_fin_infonavit INTO v_subcuenta, v_subcuenta_desc, v_monto
      IF v_subcuenta IS NOT NULL THEN
         IF v_subcuenta = 4 THEN
            LET i = 1
         END IF
         IF v_subcuenta = 8 THEN
            LET i = 2
         END IF
         LET v_lista_conciliacion[i].subcuenta = v_subcuenta_desc
         LET v_lista_conciliacion[i].fin_infonavit = v_monto

         #Se incrementa el sumario
         LET v_sum_fin_infonavit = v_sum_fin_infonavit + v_monto
      END IF
   END FOREACH

   #Se carga la informacion de conciliacion
   LET v_consulta_cifras = "SELECT ",
                              "cbd.subcuenta, ",
                              "SUM(cbd.monto_acciones), ",
                              "SUM(cbd.monto_acciones_bd) ",
                           "FROM cbd_cifras_concilia cbd ",
                           "WHERE cbd.folio = ? ",
                           "GROUP BY cbd.subcuenta"
   PREPARE exe_consulta_cifras FROM v_consulta_cifras
   DECLARE cur_consulta_cifras CURSOR FOR exe_consulta_cifras

   FOREACH cur_consulta_cifras USING v_folio INTO v_subcuenta, v_monto, v_monto_bdnsviv
      IF v_subcuenta IS NOT NULL THEN
         IF v_subcuenta = 4 THEN
            LET i = 1
         END IF
         IF v_subcuenta = 8 THEN
            LET i = 2
         END IF
         LET v_lista_conciliacion[i].saldo_a_conciliar = v_monto
         LET v_lista_conciliacion[i].saldo_bdnsviv = v_monto_bdnsviv
         LET v_lista_conciliacion[i].diferencias = v_monto - v_monto_bdnsviv

         #Se incrementan  los sumarios
         LET v_sum_saldo_concilia = v_sum_saldo_concilia + v_monto
         LET v_sum_saldo_bdnsviv = v_sum_saldo_bdnsviv + v_monto_bdnsviv
         LET v_sum_diferencias = v_sum_diferencias + v_lista_conciliacion[i].diferencias
      END IF
   END FOREACH
   
   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION 

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE reporte          om.SaxDocumentHandler
   DEFINE i                SMALLINT
   
   IF fgl_report_loadCurrentSettings("CBDC03.4rp") THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(TRUE)
      LET reporte = fgl_report_commitCurrentSettings()
      
      IF reporte IS NOT NULL THEN
         START REPORT cifras TO XML HANDLER reporte
            FOR i = 1 TO v_lista_conciliacion.getLength()
               OUTPUT TO REPORT cifras(v_lista_conciliacion[i].*)
            END FOR 
         FINISH REPORT cifras
      END IF
   END IF
END FUNCTION

REPORT cifras(p_lista)
   DEFINE p_lista RECORD
      subcuenta                                 VARCHAR(50),
      inicio_infonavit                          DECIMAL(18,2),
      fin_infonavit                             DECIMAL(18,2),
      saldo_a_conciliar                         DECIMAL(18,2),
      saldo_bdnsviv                             DECIMAL(18,2),
      diferencias                               DECIMAL(18,2)
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
      PRINTX   v_sum_inicio_infonavit,
               v_sum_fin_infonavit,
               v_sum_saldo_concilia,
               v_sum_saldo_bdnsviv,
               v_sum_diferencias
      
END REPORT