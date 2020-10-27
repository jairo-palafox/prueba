################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 14/04/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDF01                                                   #
#Objetivo          =>  #
#Fecha inicio      => 09/04/2012                                               #
################################################################################
DATABASE safre_viv

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
PRIVATE DEFINE v_lista_saldo_inico DYNAMIC ARRAY OF RECORD
   subcuenta_inicio_infonavit                VARCHAR(50),
   acciones_inicio_infonavit                 DECIMAL(18,2)
END RECORD

PRIVATE DEFINE v_lista_conciliar DYNAMIC ARRAY OF RECORD
   modulo                                CHAR(3),
   subcuenta                             SMALLINT,
   modulo_concilia                       VARCHAR(50),
   subcuenta_concilia                    VARCHAR(50),
   cargo_concilia                        DECIMAL(18,2),
   abono_concilia                        DECIMAL(18,2),
   neto_concilia                         DECIMAL(18,2),
   no_confirmados_concilia               DECIMAL(18,2),
   no_confirmados_his                    DECIMAL(18,2),
   saldo_a_conciliar                     DECIMAL(18,2),
   saldo_bdnsviv                         DECIMAL(18,2),
   diferencia_concilia                   DECIMAL(18,2)
END RECORD

PRIVATE DEFINE v_lista_conciliar_sum DYNAMIC ARRAY OF RECORD
   subcuenta_sum                          VARCHAR(50),
   cargo_sum                              DECIMAL(18,2),
   abono_sum                              DECIMAL(18,2),
   neto_sum                               DECIMAL(18,2),
   no_confirmado_sum                      DECIMAL(18,2),
   no_confirmado_his_sum                 DECIMAL(18,2),
   saldo_conciliar_sum                    DECIMAL(18,2),
   saldo_bdnsviv_sum                      DECIMAL(18,2),
   diferencias_sum                        DECIMAL(18,2)
END RECORD

PRIVATE DEFINE v_lista_saldo_fin DYNAMIC ARRAY OF RECORD
   subcuenta_fin_infonavit                VARCHAR(50),
   acciones_fin_infonavit                 DECIMAL(18,2)
END RECORD

PRIVATE DEFINE v_lista_saldo_otros DYNAMIC ARRAY OF RECORD
   subcuenta_otros                        VARCHAR(50),
   pesos_otros                            DECIMAL(18,2),
   acciones_otros                         DECIMAL(18,2),
   total_otros                            DECIMAL(15,0)
END RECORD

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form
PRIVATE DEFINE item        INTEGER

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

   OPEN WINDOW vtn_cbdf01 WITH FORM "CBDF011"

      LET ventana = ui.Window.forName("vtn_cbdf01")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos = 0 THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos = 1 THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE
 
   CLOSE WINDOW vtn_cbdf01

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
   INITIALIZE v_lista_saldo_otros      TO NULL

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group2",1)
   CALL forma.setElementHidden("group3",1)
   CALL forma.setElementHidden("group4",1)
   CALL forma.setElementHidden("group5",1)
   CALL forma.setElementHidden("group6",1)
   CALL forma.setElementHidden("group7",1)

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
      CALL fn_buscar_saldos()
      RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
   DEFINE v_valida_generacion       SMALLINT

   DIALOG ATTRIBUTES(UNBUFFERED)
   DISPLAY ARRAY v_lista_saldo_inico   TO lista_saldo_inico.*
   END DISPLAY
   INPUT ARRAY v_lista_conciliar       FROM lista_conciliar.*
      BEFORE INPUT
         CALL DIALOG.setActionHidden("insert",1)
         CALL DIALOG.setActionHidden("append",1)
         CALL DIALOG.setActionHidden("delete",1)

      ON ROW CHANGE
         LET item = DIALOG.getCurrentRow("lista_conciliar")
         CALL fn_actualiza_monto(item)
   END INPUT
   DISPLAY ARRAY v_lista_conciliar_sum TO lista_conciliar_sum.*
   END DISPLAY
   DISPLAY ARRAY v_lista_saldo_fin     TO lista_saldo_fin.*
   END DISPLAY
   DISPLAY ARRAY v_lista_saldo_otros   TO lista_saldo_otros.* 
   END DISPLAY

   BEFORE DIALOG
      CALL forma.setElementHidden("group2",0)
      CALL forma.setElementHidden("group3",0)
      CALL forma.setElementHidden("group4",0)
      CALL forma.setElementHidden("group5",0)
      CALL forma.setElementHidden("group6",0)
      CALL forma.setElementHidden("group7",0)
      DISPLAY v_acciones97_bdnsviv     TO acciones97_bdnsviv
      DISPLAY v_pesos97_bdnsviv        TO pesos97_bdnsviv
      DISPLAY v_acciones92_bdnsviv     TO acciones92_bdnsviv
      DISPLAY v_pesos92_bdnsviv        TO pesos92_bdnsviv
      DISPLAY v_registros_bdnsviv      TO registros_bdnsviv
      DISPLAY v_f_inicio_infonavit     TO f_inicio_infonavit 
      DISPLAY v_f_fin_infonavit        TO f_fin_infonavit
      DISPLAY v_folio                  TO folio
      DISPLAY v_f_conciliacion         TO f_conciliacion
      
   ON ACTION conciliar
      CALL fn_genera_conciliacion()
      CONTINUE DIALOG

   ON ACTION reporte
      CALL fn_genera_reporte()
      RETURN 1

   ON ACTION adelantos
            CALL fn_valida_generacion() RETURNING v_valida_generacion
            IF v_valida_generacion = 1 THEN
               CALL fn_genera_adelantos() 
               #EXIT DIALOG
            #ELSE
            #   CONTINUE DIALOG
            END IF
            RETURN 1

   ON ACTION salir
      RETURN 0
      EXIT DIALOG
      
   #ON ACTION cancelar
    #EXIT DIALOG
END DIALOG
END FUNCTION

PRIVATE FUNCTION fn_buscar_saldos()
   DEFINE i                      SMALLINT
   
   #Variables para el manejo de las consultas
   DEFINE v_consulta_proceso           STRING
   DEFINE v_consulta_bdnsviv           STRING
   DEFINE v_consulta_infonavit         STRING
   DEFINE v_consulta_solo_infonavit    STRING
   DEFINE v_consulta_precio            STRING
   DEFINE v_consulta_adelanto          STRING

   DEFINE v_fadelanto                  DATE
   
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

   #Se valida que ya exista la informacion de saldos por periodo necesaria para la conciliacion
   LET v_consulta_adelanto =  "SELECT FIRST 1 ",
                              "f_saldo  ",
                              "FROM cbd_modulo_periodo ",
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
   
   #Se buscan los montos globales del archivo bdnsviv
   LET v_consulta_bdnsviv =   "SELECT ",
                              "total_saldo_viv97, ",
                              "total_saldo_viv92, ",
                              "total_aivs_97, ",
                              "total_aivs_92, ",
                              "total_registros ",
                              "FROM cbd_sum_bdnsviv ",
                              "WHERE folio = ?"
   PREPARE exe_consulta_bdnsviv FROM v_consulta_bdnsviv
   EXECUTE exe_consulta_bdnsviv  USING v_folio
                                 INTO  v_pesos97_bdnsviv,
                                       v_pesos92_bdnsviv,
                                       v_acciones97_bdnsviv,
                                       v_acciones92_bdnsviv,
                                       v_registros_bdnsviv

   #Se genera la informacion para la seccion de saldos iniciales de infonavit
   LET v_f_inicio_infonavit = MDY(MONTH(v_f_conciliacion), 1, YEAR(v_f_conciliacion));
   LET v_corte_anterior = v_f_inicio_infonavit - 1
   LET v_f_fin_infonavit = v_f_conciliacion

   LET v_consulta_infonavit = "SELECT ",
                              "(sdo.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
                              "sdo.monto_acciones ",
                              "FROM safre_sdo@vivws_tcp:cta_saldo_mensual_global sdo ",
                              "INNER JOIN cat_subcuenta cat on cat.subcuenta = sdo.subcuenta ",
                              "WHERE sdo.subcuenta IN (4,8) ",
                              "AND sdo.fondo_inversion <> 0 ",
                              "AND sdo.f_saldo = ? ",
                              "ORDER BY subcuenta"
   PREPARE exe_consulta_infonavit FROM v_consulta_infonavit
   DECLARE cur_consulta_infonavit CURSOR FOR exe_consulta_infonavit

   LET i = 1
   FOREACH cur_consulta_infonavit USING v_corte_anterior INTO v_lista_saldo_inico[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_saldo_inico.deleteElement(v_lista_saldo_inico.getLength())

   IF v_lista_saldo_inico[1].subcuenta_inicio_infonavit IS NULL OR v_lista_saldo_inico[2].subcuenta_inicio_infonavit IS NULL THEN
      SELECT
         (cat.subcuenta || ' - ' || cat.subcuenta_desc)
      INTO
         v_lista_saldo_inico[1].subcuenta_inicio_infonavit
      FROM cat_subcuenta cat
      WHERE subcuenta = 4

      SELECT
         (cat.subcuenta || ' - ' || cat.subcuenta_desc)
      INTO
         v_lista_saldo_inico[2].subcuenta_inicio_infonavit
      FROM cat_subcuenta cat
      WHERE subcuenta = 8
   END IF

   #Se mete en variables las descripciones de las subcuentas
   LET v_viv97_desc = v_lista_saldo_inico[1].subcuenta_inicio_infonavit
   LET v_viv92_desc = v_lista_saldo_inico[2].subcuenta_inicio_infonavit

   

   #Se buscan los montos para la seccion de conciliacion
   CALL fn_carga_conciliacion()

   #Se genera la informacion para la seccion de saldos finales de infonavit
   LET i = 1
   FOREACH cur_consulta_infonavit USING v_f_fin_infonavit INTO v_lista_saldo_fin[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_saldo_fin.deleteElement(v_lista_saldo_fin.getLength())

   #Se genera la informacion para la seccion de saldos para solo infonavit
   LET v_consulta_solo_infonavit =  "SELECT ",
                                    "(sdo.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
                                    "sdo.monto_pesos, ",
                                    "sdo.monto_acciones ",
                                    "FROM safre_sdo@vivws_tcp:cta_saldo_mensual_global sdo ",
                                    "INNER JOIN cat_subcuenta cat on cat.subcuenta = sdo.subcuenta ",
                                    "WHERE sdo.subcuenta IN (40,42,43,44,48) ",
                                    "AND sdo.fondo_inversion <> 0 ",
                                    "AND sdo.f_saldo = ? ",
                                    "ORDER BY subcuenta"
   PREPARE exe_consulta_solo_infonavit FROM v_consulta_solo_infonavit
   DECLARE cur_consulta_solo_infonavit CURSOR FOR exe_consulta_solo_infonavit

   LET i = 1
   FOREACH cur_consulta_solo_infonavit USING v_f_fin_infonavit INTO v_lista_saldo_otros[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_saldo_otros.deleteElement(v_lista_saldo_otros.getLength())

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
   DEFINE v_folio_temp           DECIMAL(9,0)
   
   #Variables para el manejo de las consultas
   DEFINE v_consulta_cat_modulo        STRING
   DEFINE v_consulta_periodo           STRING
   DEFINE v_consulta_adelanto          STRING
   DEFINE v_consulta_adelanto_his      STRING
   DEFINE v_consulta_concilia          STRING
   DEFINE v_consulta_historico         STRING

   #Se inicializa la lista que muestra la seccion del sumario
   LET v_lista_conciliar_sum[1].subcuenta_sum = v_viv97_desc
   LET v_lista_conciliar_sum[1].cargo_sum = 0
   LET v_lista_conciliar_sum[1].abono_sum = 0
   LET v_lista_conciliar_sum[1].neto_sum = 0
   LET v_lista_conciliar_sum[1].no_confirmado_sum = 0
   LET v_lista_conciliar_sum[1].no_confirmado_his_sum = 0
   LET v_lista_conciliar_sum[1].saldo_conciliar_sum = 0
   
   LET v_lista_conciliar_sum[2].subcuenta_sum = v_viv92_desc
   LET v_lista_conciliar_sum[2].cargo_sum = 0
   LET v_lista_conciliar_sum[2].abono_sum = 0
   LET v_lista_conciliar_sum[2].neto_sum = 0
   LET v_lista_conciliar_sum[2].no_confirmado_sum = 0
   LET v_lista_conciliar_sum[2].no_confirmado_his_sum = 0
   LET v_lista_conciliar_sum[2].saldo_conciliar_sum = 0
   
   #Se genera la informacion para la seccion de conciliacion
   LET v_consulta_cat_modulo =   "SELECT ",
                                    "modulo_cod, ",
                                    "modulo_desc_bd, ",
                                    "ind_adelanto ",
                                 "FROM cbd_proceso_concilia ",
                                 "ORDER BY modulo_cod_cbd"
   PREPARE exe_consulta_cat_modulo FROM v_consulta_cat_modulo
   DECLARE cur_consulta_cat_modulo CURSOR FOR exe_consulta_cat_modulo

   #Se valida si existe una conciliacion gusrdada para el folio
   LET v_consulta_concilia = "SELECT FIRST 1 folio FROM cbd_cifras_concilia WHERE folio = ?"
   PREPARE exe_consulta_concilia FROM v_consulta_concilia
   EXECUTE exe_consulta_concilia USING v_folio INTO v_folio_temp

   #Se preparan las consultas de saldo
   LET v_consulta_periodo =   "SELECT ",
                              "mov.subcuenta, ",
                              "mov.monto_acciones ",
                              "FROM cbd_modulo_periodo mov ",
                              "WHERE mov.subcuenta IN (4,8)  ",
                              "AND mov.f_saldo = ? ",
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

   LET v_consulta_historico = "SELECT ",
                              "his.subcuenta, ",
                              "his.monto_acciones_bd  ",
                              "FROM cbd_cifras_concilia his ",
                              "WHERE his.folio = ? ",
                              "AND his.modulo = ?"
   PREPARE execonsulta_historico FROM v_consulta_historico
   DECLARE cur_consulta_historico CURSOR FOR execonsulta_historico
   
   LET i = 1
   FOREACH cur_consulta_cat_modulo INTO v_modulo_cod, v_modulo_desc, v_ind_adelanto
      IF v_modulo_cod IS NOT NULL THEN
         LET v_lista_conciliar[i].modulo = v_modulo_cod
         LET v_lista_conciliar[i + 1].modulo = v_modulo_cod
         LET v_lista_conciliar[i].modulo_concilia = v_modulo_desc
         LET v_lista_conciliar[i].subcuenta = 4
         LET v_lista_conciliar[i + 1].subcuenta = 8
         LET v_lista_conciliar[i].subcuenta_concilia = v_viv97_desc
         LET v_lista_conciliar[i + 1].subcuenta_concilia = v_viv92_desc

         #Se buscan los cargos del periodo
         LET v_tipo_movimiento = -1
         FOREACH cur_consulta_periodo USING v_f_conciliacion, v_tipo_movimiento, v_modulo_cod
                                      INTO  v_subcuenta, v_monto
            IF v_subcuenta = 4 THEN
               LET v_lista_conciliar[i].cargo_concilia = v_monto
            END IF
            IF v_subcuenta = 8 THEN
               LET v_lista_conciliar[i + 1].cargo_concilia = v_monto
            END IF
         END FOREACH

         #Si no se encuentran cargos en el periodo se asigna cero
         IF v_lista_conciliar[i].cargo_concilia IS NULL THEN
            LET v_lista_conciliar[i].cargo_concilia = 0
         END IF
         IF v_lista_conciliar[i + 1].cargo_concilia IS NULL THEN
            LET v_lista_conciliar[i + 1].cargo_concilia = 0
         END IF

         #Incrementamos el sumario de cargos
         LET v_lista_conciliar_sum[1].cargo_sum = v_lista_conciliar_sum[1].cargo_sum + 
                                                  v_lista_conciliar[i].cargo_concilia
         LET v_lista_conciliar_sum[2].cargo_sum = v_lista_conciliar_sum[2].cargo_sum +
                                                  v_lista_conciliar[i + 1].cargo_concilia

         #Se buscan los abonos del periodo
         LET v_tipo_movimiento = 1
         FOREACH cur_consulta_periodo USING v_f_conciliacion, v_tipo_movimiento, v_modulo_cod
                                      INTO  v_subcuenta, v_monto
            IF v_subcuenta = 4 THEN
               LET v_lista_conciliar[i].abono_concilia = v_monto
            END IF
            IF v_subcuenta = 8 THEN
               LET v_lista_conciliar[i + 1].abono_concilia = v_monto
            END IF
         END FOREACH

         #Si no se encuentran abonos en el periodo se asigna cero
         IF v_lista_conciliar[i].abono_concilia IS NULL THEN
            LET v_lista_conciliar[i].abono_concilia = 0
         END IF
         IF v_lista_conciliar[i + 1].abono_concilia IS NULL THEN
            LET v_lista_conciliar[i + 1].abono_concilia = 0
         END IF

         #Incrementamos el sumario de cargos
         LET v_lista_conciliar_sum[1].abono_sum = v_lista_conciliar_sum[1].abono_sum + 
                                                  v_lista_conciliar[i].abono_concilia
         LET v_lista_conciliar_sum[2].abono_sum = v_lista_conciliar_sum[2].abono_sum + 
                                                  v_lista_conciliar[i + 1].abono_concilia

         #Se llenan los montos netos
         LET v_lista_conciliar[i].neto_concilia = v_lista_conciliar[i].abono_concilia +
                                                  v_lista_conciliar[i].cargo_concilia
         LET v_lista_conciliar[i + 1].neto_concilia = v_lista_conciliar[i + 1].abono_concilia +
                                                      v_lista_conciliar[i + 1].cargo_concilia

         #Incrementamos el sumario de montos netos
         LET v_lista_conciliar_sum[1].neto_sum = v_lista_conciliar_sum[1].neto_sum +
                                                 v_lista_conciliar[i].neto_concilia
         LET v_lista_conciliar_sum[2].neto_sum = v_lista_conciliar_sum[2].neto_sum +
                                                 v_lista_conciliar[i + 1].neto_concilia

         #Se llena la columna de movimientos adelantados
         IF v_ind_adelanto = 1 THEN
            FOREACH cur_consulta_adelanto USING v_f_conciliacion, v_modulo_cod
                                          INTO  v_subcuenta, v_monto
               IF v_subcuenta = 4 THEN
                  LET v_lista_conciliar[i].no_confirmados_concilia = v_monto
               END IF
               IF v_subcuenta = 8 THEN
                  LET v_lista_conciliar[i + 1].no_confirmados_concilia = v_monto
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
         IF v_lista_conciliar[i].no_confirmados_concilia IS NULL THEN
            LET v_lista_conciliar[i].no_confirmados_concilia = 0
         END IF
         IF v_lista_conciliar[i + 1].no_confirmados_concilia IS NULL THEN
            LET v_lista_conciliar[i + 1].no_confirmados_concilia = 0
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
                                                          v_lista_conciliar[i].no_confirmados_concilia
         LET v_lista_conciliar_sum[2].no_confirmado_sum = v_lista_conciliar_sum[2].no_confirmado_sum +
                                                          v_lista_conciliar[i + 1].no_confirmados_concilia

         #Incrementamos el sumario de montos adelantados historicos
         LET v_lista_conciliar_sum[1].no_confirmado_his_sum = v_lista_conciliar_sum[1].no_confirmado_his_sum +
                                                              v_lista_conciliar[i].no_confirmados_his
         LET v_lista_conciliar_sum[2].no_confirmado_his_sum = v_lista_conciliar_sum[2].no_confirmado_his_sum +
                                                              v_lista_conciliar[i + 1].no_confirmados_his

                                                          
         #Se llena la columna se saldos a conciliar
         LET v_lista_conciliar[i].saldo_a_conciliar = v_lista_conciliar[i].neto_concilia - 
                                                      v_lista_conciliar[i].no_confirmados_concilia
         LET v_lista_conciliar[i + 1].saldo_a_conciliar = v_lista_conciliar[i + 1].neto_concilia - 
                                                      v_lista_conciliar[i + 1].no_confirmados_concilia

         #Incrementamos el sumario de saldos a conciliar
         LET v_lista_conciliar_sum[1].saldo_conciliar_sum = v_lista_conciliar_sum[1].saldo_conciliar_sum +
                                                            v_lista_conciliar[i].saldo_a_conciliar
         LET v_lista_conciliar_sum[2].saldo_conciliar_sum = v_lista_conciliar_sum[2].saldo_conciliar_sum +
                                                            v_lista_conciliar[i + 1].saldo_a_conciliar

         #Si existe historico se carga la informacion
         IF v_folio_temp IS NOT NULL THEN
            FOREACH cur_consulta_historico USING v_folio, v_modulo_cod
                                           INTO  v_subcuenta, v_monto
               IF v_subcuenta = 4 THEN
                  LET v_lista_conciliar[i].saldo_bdnsviv = v_monto
                  CALL fn_actualiza_monto(i)
               END IF
               IF v_subcuenta = 8 THEN
                  LET v_lista_conciliar[i + 1].saldo_bdnsviv = v_monto
                  CALL fn_actualiza_monto(i + 1)
               END IF
            END FOREACH
         END IF                                                   
      END IF
      LET i = i + 2
   END FOREACH
END FUNCTION

PRIVATE FUNCTION fn_actualiza_monto(item)
   DEFINE item                SMALLINT
   DEFINE suma                SMALLINT
   DEFINE i                   SMALLINT
   #Se actualizan las diferencias
   LET v_lista_conciliar[item].diferencia_concilia = v_lista_conciliar[item].saldo_a_conciliar -
                                                     v_lista_conciliar[item].saldo_bdnsviv
   IF v_lista_conciliar[item].subcuenta = 4 THEN
      LET suma = 1
   ELSE
      LET suma = 2
   END IF

   #Se inicializa le sumario para recalcular el total
   LET v_lista_conciliar_sum[suma].saldo_bdnsviv_sum = 0
   LET v_lista_conciliar_sum[suma].diferencias_sum = 0

   FOR i = 1 TO v_lista_conciliar.getLength()
      IF v_lista_conciliar[i].subcuenta = v_lista_conciliar[item].subcuenta THEN
         IF v_lista_conciliar[i].saldo_bdnsviv IS NOT NULL THEN
            LET v_lista_conciliar_sum[suma].saldo_bdnsviv_sum = v_lista_conciliar_sum[suma].saldo_bdnsviv_sum +
                                                                v_lista_conciliar[i].saldo_bdnsviv
            LET v_lista_conciliar_sum[suma].diferencias_sum = v_lista_conciliar_sum[suma].diferencias_sum +
                                                                v_lista_conciliar[i].diferencia_concilia
         END IF
      END IF
   END FOR
   
END FUNCTION

PRIVATE FUNCTION fn_genera_conciliacion()
   DEFINE v_respuesta               INTEGER
   DEFINE i                         SMALLINT
   DEFINE v_fondo                     SMALLINT

   #Variables para manejar los montos en pesos
   DEFINE v_pesos                   LIKE cbd_cifras_concilia.monto_pesos
   DEFINE v_pesos_db                LIKE cbd_cifras_concilia.monto_pesos_bd

   #Variables para manejar las instrucciones SQL que se ejecutaran
   DEFINE v_elimina                 STRING
   DEFINE v_inserta                 STRING
   
   CALL fn_ventana_confirma("Atención",
                    "Los montos de la BDNSVIV no capturados se almacenaran con $0.00, ¿desea ejecutar la Conciliación?",
                     "quest") RETURNING v_respuesta
   IF v_respuesta = 1 THEN
      LET v_fondo = 11
      
      #Primero se elimina la conciliacion del folio
      LET v_elimina = "DELETE FROM  cbd_cifras_concilia WHERE folio = ?"
      PREPARE exe_elimina FROM v_elimina
      EXECUTE exe_elimina USING v_folio

      #Se prepara la instruccion que insertara la conciliacion
      LET v_inserta = "INSERT INTO cbd_cifras_concilia VALUES (?,?,?,?,?,?,?,?)"
      PREPARE exe_inserta FROM v_inserta

      FOR i = 1 TO v_lista_conciliar.getLength()
         #Si el monto bdnsviv no fue capturado se guarda con cero
         IF v_lista_conciliar[i].saldo_bdnsviv IS NULL THEN
            LET v_lista_conciliar[i].saldo_bdnsviv = 0
            CALL fn_actualiza_monto(i)
         END IF

         #Se calculan los montos en pesos
         LET v_pesos = v_lista_conciliar[i].saldo_a_conciliar * v_precio_fondo
         LET v_pesos_db = v_lista_conciliar[i].saldo_bdnsviv * v_precio_fondo
         
         #Se insertan los nuevos montos de conciliacion
         EXECUTE exe_inserta USING  v_folio,
                                    v_lista_conciliar[i].modulo,
                                    v_lista_conciliar[i].subcuenta,
                                    v_fondo,
                                    v_lista_conciliar[i].saldo_a_conciliar,
                                    v_pesos,
                                    v_lista_conciliar[i].saldo_bdnsviv,
                                    v_pesos_db
      END FOR
   END IF
   
END FUNCTION

PRIVATE FUNCTION fn_valida_generacion()
   DEFINE v_f_genera             DATE
   DEFINE v_mensaje              STRING

   LET v_f_genera = MDY(MONTH(TODAY), 1, YEAR(TODAY)) - 1;
   LET v_corte_anterior = v_f_inicio_infonavit - 1
   LET v_f_fin_infonavit = v_f_conciliacion

   IF v_f_genera <> v_f_conciliacion THEN
      LET v_mensaje = "La generacion de los archivos de adelantos unicamente esta habilitada para la fecha de corte ", v_f_genera USING 'dd-mm-yyyy',
                      "\npor lo que no se pueden generar los archivos de salida "
      CALL fn_mensaje("Atención", v_mensaje,"info")
      RETURN 0
   END IF
   RETURN 1
END FUNCTION

PRIVATE FUNCTION fn_genera_adelantos()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log

   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)
   DEFINE v_comando           STRING

   LET v_proceso_cod = 2103
   LET v_opera_cod = 1

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      --Obtiene las rutas ejecutable
      SELECT ruta_bin
        INTO v_ruta_ejecutable
      FROM seg_modulo 
      WHERE modulo_cod = 'cbd'

      --Obtiene ruta listados
      SELECT ruta_listados
        INTO v_ruta_listados
      FROM seg_modulo 
      WHERE modulo_cod = 'bat'

      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,v_usuario)
             RETURNING v_pid

      #Se asigna el nombre del archivo
      LET v_nom_archivo = "Adelantos_",v_f_conciliacion USING "yymmdd"

      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDS04.42r ",
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
         CALL fn_mensaje("Generacion de Archivo", 
                         "Ocurrió un error al iniciar el proceso batch",
                         "bn_about")
      ELSE
         # Se indica que se realizo el proceso de carga
         CALL fn_mensaje("Generacion de Archivo", 
                         "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                         "bn_about")
      END IF
   END IF
   
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE reporte          om.SaxDocumentHandler
   DEFINE i                SMALLINT
   
   IF fgl_report_loadCurrentSettings("CBDF01.4rp") THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
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
   DEFINE p_lista RECORD
      modulo                                CHAR(3),
      subcuenta                             SMALLINT,
      modulo_concilia                       VARCHAR(50),
      subcuenta_concilia                    VARCHAR(50),
      cargo_concilia                        DECIMAL(18,2),
      abono_concilia                        DECIMAL(18,2),
      neto_concilia                         DECIMAL(18,2),
      no_confirmados_concilia               DECIMAL(18,2),
      no_confirmados_his                    DECIMAL(18,2),
      saldo_a_conciliar                     DECIMAL(18,2),
      saldo_bdnsviv                         DECIMAL(18,2),
      diferencia_concilia                   DECIMAL(18,2)
   END RECORD

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
   DEFINE saldo_conciliar_sum97                    DECIMAL(18,2)
   DEFINE saldo_bdnsviv_sum97                      DECIMAL(18,2)
   DEFINE diferencias_sum97                        DECIMAL(18,2)

   DEFINE cargo_sum92                              DECIMAL(18,2)
   DEFINE abono_sum92                              DECIMAL(18,2)
   DEFINE neto_sum92                               DECIMAL(18,2)
   DEFINE no_confirmado_sum92                      DECIMAL(18,2)
   DEFINE no_confirmado__his_sum92                 DECIMAL(18,2)
   DEFINE saldo_conciliar_sum92                    DECIMAL(18,2)
   DEFINE saldo_bdnsviv_sum92                      DECIMAL(18,2)
   DEFINE diferencias_sum92                        DECIMAL(18,2)

   FORMAT

   FIRST PAGE HEADER

      LET v_today = TODAY

      #Se llenan las variables para imprimir las secciones de infonavit
      LET subcuenta97 = v_lista_saldo_inico[1].subcuenta_inicio_infonavit
      LET acciones_in97 = v_lista_saldo_inico[1].acciones_inicio_infonavit
      
      LET subcuenta92 = v_lista_saldo_inico[2].subcuenta_inicio_infonavit
      LET acciones_in92 = v_lista_saldo_inico[2].acciones_inicio_infonavit

      LET acciones_fn97 = v_lista_saldo_fin[1].acciones_fin_infonavit
      LET acciones_fn92 = v_lista_saldo_fin[2].acciones_fin_infonavit

      #Se llenan las variables para imprimir la seccion de sumario
      LET cargo_sum97 = v_lista_conciliar_sum[1].cargo_sum
      LET abono_sum97 = v_lista_conciliar_sum[1].abono_sum
      LET neto_sum97 = v_lista_conciliar_sum[1].neto_sum
      LET no_confirmado_sum97 = v_lista_conciliar_sum[1].no_confirmado_sum
      LET no_confirmado__his_sum97 = v_lista_conciliar_sum[1].no_confirmado_his_sum
      LET saldo_conciliar_sum97 = v_lista_conciliar_sum[1].saldo_conciliar_sum
      LET saldo_bdnsviv_sum97 = v_lista_conciliar_sum[1].saldo_bdnsviv_sum
      LET diferencias_sum97 = v_lista_conciliar_sum[1].diferencias_sum

      LET cargo_sum92 = v_lista_conciliar_sum[2].cargo_sum
      LET abono_sum92 = v_lista_conciliar_sum[2].abono_sum
      LET neto_sum92 = v_lista_conciliar_sum[2].neto_sum
      LET no_confirmado_sum92 = v_lista_conciliar_sum[2].no_confirmado_sum
      LET no_confirmado__his_sum92 = v_lista_conciliar_sum[2].no_confirmado_his_sum
      LET saldo_conciliar_sum92 = v_lista_conciliar_sum[2].saldo_conciliar_sum
      LET saldo_bdnsviv_sum92 = v_lista_conciliar_sum[2].saldo_bdnsviv_sum
      LET diferencias_sum92 = v_lista_conciliar_sum[2].diferencias_sum
      
      #Seccion de encabezado
      PRINTX   v_folio,
               v_f_conciliacion USING "DD-MM-YYYY",
               v_today USING "DD-MM-YYYY"

      #Seccion de fechas
      PRINTX   v_f_inicio_infonavit,
               v_f_fin_infonavit
               
      #seccion de saldos DBNSVIV
      PRINTX   v_acciones97_bdnsviv,
               v_pesos97_bdnsviv,
               v_acciones92_bdnsviv,
               v_pesos92_bdnsviv,
               v_registros_bdnsviv

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
               no_confirmado__his_sum97,
               saldo_conciliar_sum97,
               saldo_bdnsviv_sum97,
               diferencias_sum97

      #Sumario de viv97
      PRINTX   cargo_sum92,
               abono_sum92,
               neto_sum92,
               no_confirmado_sum92,
               no_confirmado__his_sum92,
               saldo_conciliar_sum92,
               saldo_bdnsviv_sum92,
               diferencias_sum92
END REPORT