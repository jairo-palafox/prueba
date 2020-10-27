################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDC11                                                   #
#Objetivo          => Pantalla de consulta para el resumen del saldo  en SAFRE #
#Fecha inicio      => 22/07/2014                                               #
################################################################################
DATABASE safre_viv

GLOBALS "CBDC11.inc"

PRIVATE DEFINE v_tipo_proceso                SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog                    VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario                     VARCHAR(30) -- Almacena al usuario

#Parametros de busqueda
PRIVATE DEFINE v_condicion                   STRING
PRIVATE DEFINE v_folio                       DECIMAL(9,0)
PRIVATE DEFINE v_f_conciliacion              DATE
PRIVATE DEFINE v_precio_fondo                LIKE glo_valor_fondo.precio_fondo
PRIVATE DEFINE v_datos                       SMALLINT

#Variables para la seccion de saldos DBNSVIV
PRIVATE DEFINE v_acciones97_bdnsviv          DECIMAL(18,2)
PRIVATE DEFINE v_pesos97_bdnsviv             DECIMAL(18,2)
PRIVATE DEFINE v_acciones92_bdnsviv          DECIMAL(18,2)
PRIVATE DEFINE v_pesos92_bdnsviv             DECIMAL(18,2)
PRIVATE DEFINE v_registros_bdnsviv           DECIMAL(18,2)

#Variables para las fechas del periodo
PRIVATE DEFINE v_f_inicio_infonavit            DATE
PRIVATE DEFINE v_f_fin_infonavit               DATE
PRIVATE DEFINE v_corte_anterior                DATE

PRIVATE DEFINE v_viv97_desc                    STRING
PRIVATE DEFINE v_viv92_desc                    STRING

#Lista de datos para la seccion del saldo inicial infonavit
PRIVATE DEFINE v_lista_saldo_inico              DYNAMIC ARRAY OF resumen_saldo
PRIVATE DEFINE v_lista_conciliar                DYNAMIC ARRAY OF detalle_saldo
PRIVATE DEFINE v_lista_conciliar_sum            DYNAMIC ARRAY OF sumario
PRIVATE DEFINE v_lista_saldo_fin                DYNAMIC ARRAY OF resumen_saldo

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

   OPEN WINDOW vtn_cbdc111 WITH FORM "CBDC111"

      LET ventana = ui.Window.forName("vtn_cbdc111")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_cbdc111

END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   #Se inicializan las valiables del filtro
   INITIALIZE v_folio               TO NULL
   INITIALIZE v_f_conciliacion      TO NULL
   INITIALIZE v_condicion           TO NULL
   INITIALIZE v_precio_fondo        TO NULL
   LET v_datos = 0

   #Se inicializan las variables para la seccion de saldos DBNSVIV
   INITIALIZE v_acciones97_bdnsviv           TO NULL
   INITIALIZE v_pesos97_bdnsviv              TO NULL
   INITIALIZE v_acciones92_bdnsviv           TO NULL
   INITIALIZE v_pesos92_bdnsviv              TO NULL
   INITIALIZE v_registros_bdnsviv            TO NULL

   #Se inicializan las variables para las fechas del periodo
   INITIALIZE v_f_inicio_infonavit            TO NULL
   INITIALIZE v_f_fin_infonavit               TO NULL
   INITIALIZE v_corte_anterior                TO NULL

   INITIALIZE v_viv97_desc                    TO NULL
   INITIALIZE v_viv92_desc                    TO NULL

   #Se inicializan las listas
   INITIALIZE v_lista_saldo_inico      TO NULL
   INITIALIZE v_lista_conciliar        TO NULL
   INITIALIZE v_lista_conciliar_sum    TO NULL
   INITIALIZE v_lista_saldo_fin        TO NULL

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group3",1)
   CALL forma.setElementHidden("group4",1)
   CALL forma.setElementHidden("group5",1)
   CALL forma.setElementHidden("group6",1)

   LET v_f_conciliacion = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1
   #Seccion que pintara los parametros de busqueda
   CONSTRUCT v_condicion ON f_operacion
                  FROM f_conciliacion
      BEFORE CONSTRUCT
         CLEAR FORM
         CLEAR SCREEN
         --DISPLAY v_f_conciliacion         TO f_conciliacion

      ON ACTION ACCEPT
         LET v_f_conciliacion    = GET_FLDBUF(f_conciliacion)
         LET INT_FLAG = 0
         IF v_f_conciliacion IS NULL THEN
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
      CALL fn_buscar_saldos()
      RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()

   DIALOG ATTRIBUTES(UNBUFFERED)
   DISPLAY ARRAY v_lista_saldo_inico   TO lista_saldo_inico.*
   END DISPLAY
   DISPLAY ARRAY v_lista_conciliar     TO lista_conciliar.*
   END DISPLAY
   DISPLAY ARRAY v_lista_conciliar_sum TO lista_conciliar_sum.*
   END DISPLAY
   DISPLAY ARRAY v_lista_saldo_fin     TO lista_saldo_fin.*
   END DISPLAY

   BEFORE DIALOG
      CALL forma.setElementHidden("group2",0)
      CALL forma.setElementHidden("group3",0)
      CALL forma.setElementHidden("group4",0)
      CALL forma.setElementHidden("group5",0)
      CALL forma.setElementHidden("group6",0)
      CALL forma.setElementHidden("group7",0)
      #DISPLAY v_acciones97_bdnsviv     TO acciones97_bdnsviv
      #DISPLAY v_pesos97_bdnsviv        TO pesos97_bdnsviv
      #DISPLAY v_acciones92_bdnsviv     TO acciones92_bdnsviv
      #DISPLAY v_pesos92_bdnsviv        TO pesos92_bdnsviv
      #DISPLAY v_registros_bdnsviv      TO registros_bdnsviv
      DISPLAY v_f_inicio_infonavit     TO f_inicio_infonavit 
      DISPLAY v_f_fin_infonavit        TO f_fin_infonavit
      #DISPLAY v_folio                  TO folio
      DISPLAY v_f_conciliacion         TO f_conciliacion

   ON ACTION reporte
      CALL fn_genera_reporte()
      RETURN 1

   ON ACTION salir
      RETURN 0
      EXIT DIALOG
      
   END DIALOG
END FUNCTION

PRIVATE FUNCTION fn_buscar_saldos()
   DEFINE i                      SMALLINT
   
   #Variables para el manejo de las consultas
   DEFINE v_consulta_infonavit         STRING
   DEFINE v_consulta_precio            STRING
   DEFINE v_consulta_adelanto          STRING

   DEFINE v_fadelanto                  DATE

   #Se valida que ya exista la informacion de saldos por periodo necesaria para la conciliacion
   LET v_consulta_adelanto =  "SELECT FIRST 1 ",
                              "f_saldo  ",
                              "FROM cbd_saldo_global_safre ",
                              "WHERE f_saldo = ?"
   PREPARE exe_valida_adelanto FROM v_consulta_adelanto
   EXECUTE exe_valida_adelanto USING v_f_conciliacion INTO v_fadelanto
   IF v_fadelanto <>  v_f_conciliacion THEN
      CALL fn_mensaje("Conciliacion",
                         "Aun no se ejecuta el proceso de preparacion de saldos para conciliacion con fecha de corte " || v_f_conciliacion,
                         "about")
      LET v_datos = 0   --v_datos = 0 significa que no encontro datos con los parametros de busqueda
      RETURN
   END IF

   LET v_consulta_precio = "SELECT precio_fondo FROM glo_valor_fondo WHERE fondo = 11 AND f_valuacion = ?"
   PREPARE exe_consulta_precio FROM v_consulta_precio
   EXECUTE exe_consulta_precio USING v_f_conciliacion INTO v_precio_fondo

   #Se genera la informacion para la seccion de saldos iniciales de infonavit
   LET v_f_inicio_infonavit = MDY(MONTH(v_f_conciliacion), 1, YEAR(v_f_conciliacion));
   LET v_corte_anterior = v_f_inicio_infonavit - 1
   LET v_f_fin_infonavit = v_f_conciliacion

   LET v_consulta_infonavit = "SELECT ",
                              "(sdo.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
                              "sdo.monto_acciones ",
                              "FROM cbd_saldo_global_safre sdo ",
                              "INNER JOIN cat_subcuenta cat on cat.subcuenta = sdo.subcuenta ",
                              "WHERE sdo.subcuenta IN (4,8) ",
                              "AND sdo.fondo_inversion = 11 ",
                              "AND sdo.f_saldo = ? ",
                              "ORDER BY subcuenta"
   PREPARE exe_consulta_infonavit FROM v_consulta_infonavit
   DECLARE cur_consulta_infonavit CURSOR FOR exe_consulta_infonavit

   LET i = 1
   FOREACH cur_consulta_infonavit USING v_corte_anterior INTO v_lista_saldo_inico[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_saldo_inico.deleteElement(v_lista_saldo_inico.getLength())

   IF v_lista_saldo_inico[1].subcuenta IS NULL OR v_lista_saldo_inico[2].subcuenta IS NULL THEN
      SELECT (cat.subcuenta || ' - ' || cat.subcuenta_desc)
      INTO v_lista_saldo_inico[1].subcuenta
      FROM cat_subcuenta cat
      WHERE subcuenta = 4

      SELECT (cat.subcuenta || ' - ' || cat.subcuenta_desc)
      INTO v_lista_saldo_inico[2].subcuenta
      FROM cat_subcuenta cat
      WHERE subcuenta = 8
   END IF

   #Se mete en variables las descripciones de las subcuentas
   LET v_viv97_desc = v_lista_saldo_inico[1].subcuenta
   LET v_viv92_desc = v_lista_saldo_inico[2].subcuenta

   #Se buscan los montos para la seccion de conciliacion
   CALL fn_carga_conciliacion()

   #Se genera la informacion para la seccion de saldos finales de infonavit
   LET i = 1
   FOREACH cur_consulta_infonavit USING v_f_fin_infonavit INTO v_lista_saldo_fin[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_saldo_fin.deleteElement(v_lista_saldo_fin.getLength())

   LET v_datos = 1      --v_datos = 1 significa que si encontro datos con los parametros de busqueda
END FUNCTION

PRIVATE FUNCTION fn_carga_conciliacion()
   DEFINE i                      SMALLINT
   
   #Variables para el manejo de modulos
   DEFINE v_modulo_cod           CHAR(3)
   DEFINE v_modulo_desc          VARCHAR(50)
   DEFINE v_ind_adelanto         SMALLINT
   DEFINE v_tipo_movimiento      INTEGER

   #Variables para el manejo de los montos
   DEFINE v_subcuenta            LIKE cat_subcuenta.subcuenta
   DEFINE v_monto                DECIMAL(18,2)
   
   #Variables para el manejo de las consultas
   DEFINE v_consulta_cat_modulo        STRING
   DEFINE v_consulta_periodo           STRING
   DEFINE v_consulta_adelanto          STRING
   DEFINE v_consulta_adelanto_his      STRING

   #Se inicializa la lista que muestra la seccion del sumario
   LET v_lista_conciliar_sum[1].subcuenta_sum = v_viv97_desc
   LET v_lista_conciliar_sum[1].cargo_sum = 0
   LET v_lista_conciliar_sum[1].abono_sum = 0
   LET v_lista_conciliar_sum[1].neto_sum = 0
   LET v_lista_conciliar_sum[1].no_confirmado_sum = 0
   LET v_lista_conciliar_sum[1].no_confirmado_his_sum = 0
   
   LET v_lista_conciliar_sum[2].subcuenta_sum = v_viv92_desc
   LET v_lista_conciliar_sum[2].cargo_sum = 0
   LET v_lista_conciliar_sum[2].abono_sum = 0
   LET v_lista_conciliar_sum[2].neto_sum = 0
   LET v_lista_conciliar_sum[2].no_confirmado_sum = 0
   LET v_lista_conciliar_sum[2].no_confirmado_his_sum = 0
   
   #Se genera la informacion para la seccion de conciliacion
   LET v_consulta_cat_modulo =   "SELECT ",
                                    "modulo_cod, ",
                                    "modulo_desc_bd, ",
                                    "ind_adelanto ",
                                 "FROM cbd_proceso_concilia ",
                                 "ORDER BY modulo_cod_cbd"
   PREPARE exe_consulta_cat_modulo FROM v_consulta_cat_modulo
   DECLARE cur_consulta_cat_modulo CURSOR FOR exe_consulta_cat_modulo

   #Se preparan las consultas de saldo
   LET v_consulta_periodo =   "SELECT ",
                              "mov.subcuenta, ",
                              "mov.monto_acciones ",
                              "FROM cbd_modulo_periodo mov ",
                              "WHERE mov.f_saldo = ? ",
                              "AND mov.tpo_saldo = ? ",
                              "AND mov.modulo = ?"
   PREPARE exe_consulta_periodo FROM v_consulta_periodo
   DECLARE cur_consulta_periodo CURSOR FOR exe_consulta_periodo

   LET v_consulta_adelanto =  "SELECT ",
                              "mov.subcuenta,  ",
                              "mov.monto_acciones ",
                              "FROM cbd_modulo_adelanto mov ",
                              "WHERE mov.subcuenta IN (4,8) ",
                              "AND mov.fondo_inversion <> 0 ",
                              "AND ind_periodo = 1 ",
                              "AND mov.f_saldo = ? ",
                              "AND mov.modulo = ? "
   PREPARE exe_consulta_adelanto FROM v_consulta_adelanto
   DECLARE cur_consulta_adelanto CURSOR FOR exe_consulta_adelanto

   LET v_consulta_adelanto_his =  "SELECT ",
                              "mov.subcuenta,  ",
                              "mov.monto_acciones ",
                              "FROM cbd_modulo_adelanto mov ",
                              "WHERE mov.subcuenta IN (4,8) ",
                              "AND mov.fondo_inversion <> 0 ",
                              "AND ind_periodo = 0 ",
                              "AND mov.f_saldo = ? ",
                              "AND mov.modulo = ? "
   PREPARE exe_consulta_adelanto_his FROM v_consulta_adelanto_his
   DECLARE cur_consulta_adelanto_his CURSOR FOR exe_consulta_adelanto_his

   LET i = 1
   FOREACH cur_consulta_cat_modulo INTO v_modulo_cod, v_modulo_desc, v_ind_adelanto
      IF v_modulo_cod IS NOT NULL THEN
         LET v_lista_conciliar[i].modulo = v_modulo_cod
         LET v_lista_conciliar[i + 1].modulo = v_modulo_cod
         LET v_lista_conciliar[i].modulo_desc = v_modulo_desc
         LET v_lista_conciliar[i].subcuenta = 4
         LET v_lista_conciliar[i + 1].subcuenta = 8
         LET v_lista_conciliar[i].subcuenta_desc = v_viv97_desc
         LET v_lista_conciliar[i + 1].subcuenta_desc = v_viv92_desc

         #Se buscan los cargos del periodo
         LET v_tipo_movimiento = -1
         FOREACH cur_consulta_periodo USING v_f_conciliacion, v_tipo_movimiento, v_modulo_cod
                                      INTO  v_subcuenta, v_monto
            IF v_subcuenta = 4 THEN
               LET v_lista_conciliar[i].cargo = v_monto
            END IF
            IF v_subcuenta = 8 THEN
               LET v_lista_conciliar[i + 1].cargo = v_monto
            END IF
         END FOREACH

         #Si no se encuentran cargos en el periodo se asigna cero
         IF v_lista_conciliar[i].cargo IS NULL THEN
            LET v_lista_conciliar[i].cargo = 0
         END IF
         IF v_lista_conciliar[i + 1].cargo IS NULL THEN
            LET v_lista_conciliar[i + 1].cargo = 0
         END IF

         #Incrementamos el sumario de cargos
         LET v_lista_conciliar_sum[1].cargo_sum = v_lista_conciliar_sum[1].cargo_sum + 
                                                  v_lista_conciliar[i].cargo
         LET v_lista_conciliar_sum[2].cargo_sum = v_lista_conciliar_sum[2].cargo_sum +
                                                  v_lista_conciliar[i + 1].cargo

         #Se buscan los abonos del periodo
         LET v_tipo_movimiento = 1
         FOREACH cur_consulta_periodo USING v_f_conciliacion, v_tipo_movimiento, v_modulo_cod
                                      INTO  v_subcuenta, v_monto
            IF v_subcuenta = 4 THEN
               LET v_lista_conciliar[i].abono = v_monto
            END IF
            IF v_subcuenta = 8 THEN
               LET v_lista_conciliar[i + 1].abono = v_monto
            END IF
         END FOREACH

         #Si no se encuentran abonos en el periodo se asigna cero
         IF v_lista_conciliar[i].abono IS NULL THEN
            LET v_lista_conciliar[i].abono = 0
         END IF
         IF v_lista_conciliar[i + 1].abono IS NULL THEN
            LET v_lista_conciliar[i + 1].abono = 0
         END IF

         #Incrementamos el sumario de cargos
         LET v_lista_conciliar_sum[1].abono_sum = v_lista_conciliar_sum[1].abono_sum + 
                                                  v_lista_conciliar[i].abono
         LET v_lista_conciliar_sum[2].abono_sum = v_lista_conciliar_sum[2].abono_sum + 
                                                  v_lista_conciliar[i + 1].abono

         #Se llenan los montos netos
         LET v_lista_conciliar[i].neto = v_lista_conciliar[i].abono +
                                                  v_lista_conciliar[i].cargo
         LET v_lista_conciliar[i + 1].neto = v_lista_conciliar[i + 1].abono +
                                                      v_lista_conciliar[i + 1].cargo

         #Incrementamos el sumario de montos netos
         LET v_lista_conciliar_sum[1].neto_sum = v_lista_conciliar_sum[1].neto_sum +
                                                 v_lista_conciliar[i].neto
         LET v_lista_conciliar_sum[2].neto_sum = v_lista_conciliar_sum[2].neto_sum +
                                                 v_lista_conciliar[i + 1].neto

         #Se llena la columna de movimientos adelantados
         IF v_ind_adelanto = 1 THEN
            FOREACH cur_consulta_adelanto USING v_f_conciliacion, v_modulo_cod
                                          INTO  v_subcuenta, v_monto
               IF v_subcuenta = 4 THEN
                  LET v_lista_conciliar[i].no_confirmados = v_monto
               END IF
               IF v_subcuenta = 8 THEN
                  LET v_lista_conciliar[i + 1].no_confirmados = v_monto
               END IF
            END FOREACH

            #Historicos
            FOREACH cur_consulta_adelanto_his USING v_f_conciliacion, v_modulo_cod
                                          INTO  v_subcuenta, v_monto
               IF v_subcuenta = 4 THEN
                  LET v_lista_conciliar[i].no_confirmados_his = v_monto
               END IF
               IF v_subcuenta = 8 THEN
                  LET v_lista_conciliar[i + 1].no_confirmados_his = v_monto
               END IF
            END FOREACH
            
         END IF

         #Si no se encuentran adelantos en el periodo se asigna cero
         IF v_lista_conciliar[i].no_confirmados IS NULL THEN
            LET v_lista_conciliar[i].no_confirmados = 0
         END IF
         IF v_lista_conciliar[i + 1].no_confirmados IS NULL THEN
            LET v_lista_conciliar[i + 1].no_confirmados = 0
         END IF

         #Si no se encuentran adelantos historicos se asigna cero
         IF v_lista_conciliar[i].no_confirmados_his IS NULL THEN
            LET v_lista_conciliar[i].no_confirmados_his = 0
         END IF
         IF v_lista_conciliar[i + 1].no_confirmados_his IS NULL THEN
            LET v_lista_conciliar[i + 1].no_confirmados_his = 0
         END IF

         #Incrementamos el sumario de montos adelantados del periodo
         LET v_lista_conciliar_sum[1].no_confirmado_sum = v_lista_conciliar_sum[1].no_confirmado_sum +
                                                          v_lista_conciliar[i].no_confirmados
         LET v_lista_conciliar_sum[2].no_confirmado_sum = v_lista_conciliar_sum[2].no_confirmado_sum +
                                                          v_lista_conciliar[i + 1].no_confirmados

         #Incrementamos el sumario de montos adelantados historicos
         LET v_lista_conciliar_sum[1].no_confirmado_his_sum = v_lista_conciliar_sum[1].no_confirmado_his_sum +
                                                              v_lista_conciliar[i].no_confirmados_his
         LET v_lista_conciliar_sum[2].no_confirmado_his_sum = v_lista_conciliar_sum[2].no_confirmado_his_sum +
                                                              v_lista_conciliar[i + 1].no_confirmados_his
      END IF
      LET i = i + 2
   END FOREACH
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE reporte          om.SaxDocumentHandler
   DEFINE i                SMALLINT
   
   IF fgl_report_loadCurrentSettings("CBDC11.4rp") THEN
      CALL fgl_report_selectDevice("XLS")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(TRUE)
      LET reporte = fgl_report_commitCurrentSettings()
      
      IF reporte IS NOT NULL THEN
         START REPORT cifras TO XML HANDLER reporte
            FOR i = 1 TO v_lista_conciliar.getLength()
               OUTPUT TO REPORT cifras(v_lista_conciliar[i].*)
            END FOR 
         FINISH REPORT cifras
      END IF
   END IF
END FUNCTION

REPORT cifras(p_lista)
   DEFINE p_lista detalle_saldo 

   DEFINE v_today                            DATE

   #Variables para guardar la subcuenta
   DEFINE subcuenta97               STRING
   DEFINE subcuenta92               STRING
   
   #Variables para mostrar la tabla inicio infonavit
   DEFINE acciones_in97             DECIMAL(18,2)
   DEFINE acciones_in92             DECIMAL(18,2)

   #Variables para mostrar la tabla fin infonavit
   DEFINE acciones_fn97             DECIMAL(18,2)
   DEFINE acciones_fn92             DECIMAL(18,2)

   #Variables para manejar la seccion de los sumarios
   DEFINE cargo_sum97                              DECIMAL(18,2)
   DEFINE abono_sum97                              DECIMAL(18,2)
   DEFINE neto_sum97                               DECIMAL(18,2)
   DEFINE no_confirmado_sum97                      DECIMAL(18,2)
   DEFINE no_confirmado__his_sum97                 DECIMAL(18,2)

   DEFINE cargo_sum92                              DECIMAL(18,2)
   DEFINE abono_sum92                              DECIMAL(18,2)
   DEFINE neto_sum92                               DECIMAL(18,2)
   DEFINE no_confirmado_sum92                      DECIMAL(18,2)
   DEFINE no_confirmado__his_sum92                 DECIMAL(18,2)

   FORMAT

   FIRST PAGE HEADER

      LET v_today = TODAY

      #Se llenan las variables para imprimir las secciones de infonavit
      LET subcuenta97 = v_lista_saldo_inico[1].subcuenta
      LET acciones_in97 = v_lista_saldo_inico[1].acciones
      
      LET subcuenta92 = v_lista_saldo_inico[2].subcuenta
      LET acciones_in92 = v_lista_saldo_inico[2].acciones

      LET acciones_fn97 = v_lista_saldo_fin[1]. acciones
      LET acciones_fn92 = v_lista_saldo_fin[2].acciones

      #Se llenan las variables para imprimir la seccion de sumario
      LET cargo_sum97 = v_lista_conciliar_sum[1].cargo_sum
      LET abono_sum97 = v_lista_conciliar_sum[1].abono_sum
      LET neto_sum97 = v_lista_conciliar_sum[1].neto_sum
      LET no_confirmado_sum97 = v_lista_conciliar_sum[1].no_confirmado_sum
      LET no_confirmado__his_sum97 = v_lista_conciliar_sum[1].no_confirmado_his_sum
      

      LET cargo_sum92 = v_lista_conciliar_sum[2].cargo_sum
      LET abono_sum92 = v_lista_conciliar_sum[2].abono_sum
      LET neto_sum92 = v_lista_conciliar_sum[2].neto_sum
      LET no_confirmado_sum92 = v_lista_conciliar_sum[2].no_confirmado_sum
      LET no_confirmado__his_sum92 = v_lista_conciliar_sum[2].no_confirmado_his_sum
      
      
      #Seccion de encabezado
      PRINTX   v_f_conciliacion USING "DD-MM-YYYY",
               v_today USING "DD-MM-YYYY"

      #Seccion de fechas
      PRINTX   v_f_inicio_infonavit USING "DD-MM-YYYY",
               v_f_fin_infonavit USING "DD-MM-YYYY"
               
      #Seccion de saldos iniciales
      PRINTX   subcuenta97,
               subcuenta92,
               acciones_in97,
               acciones_in92

      #Seccion de saldos finales
      PRINTX   acciones_fn97,
               acciones_fn92
               

   ON EVERY ROW
      PRINTX   p_lista.*

   ON LAST ROW
      #Saldos finales infonavit
      PRINTX   acciones_fn97,
               acciones_fn92
      
      #Sumario de viv97
      PRINTX   cargo_sum97,
               abono_sum97,
               neto_sum97,
               no_confirmado_sum97,
               no_confirmado__his_sum97

      #Sumario de viv97
      PRINTX   cargo_sum92,
               abono_sum92,
               neto_sum92,
               no_confirmado_sum92,
               no_confirmado__his_sum92
END REPORT