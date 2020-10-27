################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 18/08/2014                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDL17                                                   #
#Objetivo          => Lanzador para el proceso de conciliacion por cuenta      #
#Fecha inicio      => 18/08/2014                                               #
################################################################################

DATABASE safre_viv

GLOBALS "CBDL17.inc"

PRIVATE DEFINE v_tipo_proceso       SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog           VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario            VARCHAR(30) -- Almacena al usuario
PRIVATE DEFINE v_f_corte            DATE
PRIVATE DEFINE v_ind_safre			   SMALLINT
PRIVATE DEFINE v_ind_bdnsviv			SMALLINT

#Informacion del archivo BDNSVIV
PRIVATE DEFINE v_f_operacion        DATE
PRIVATE DEFINE v_nombre_archivo     VARCHAR(50)
PRIVATE DEFINE v_estado_archivo     SMALLINT

#Arreglo dinamico para el manejo de los saldos
PRIVATE DEFINE v_saldo_safre      DYNAMIC ARRAY OF saldos
PRIVATE DEFINE v_saldo_bdnsviv    DYNAMIC ARRAY OF saldos

MAIN
   -- se asignan los parametros que vienen del fglrun
   LET v_usuario       = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   
   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   #Se establece la fecha de corte en el ultimo dia natural del mes anterior
   LET v_f_corte = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1

   CALL fn_buscar_saldos()

   IF v_ind_safre = 1 AND v_ind_bdnsviv = 2 THEN
      OPEN WINDOW vtn_cbdl171 WITH FORM "CBDL171"
         DIALOG   ATTRIBUTES(UNBUFFERED)
            DISPLAY ARRAY v_saldo_safre TO saldo_safre.* 
            END DISPLAY

            DISPLAY ARRAY v_saldo_bdnsviv TO saldo_bdnsviv.* 
            END DISPLAY

            BEFORE DIALOG
               DISPLAY  v_f_corte TO f_corte
               DISPLAY  v_f_operacion TO f_operacion
               DISPLAY  v_nombre_archivo TO nombre_archivo

            ON ACTION conciliar
               CALL fn_conciliar()
               EXIT DIALOG
            
            ON ACTION cancelar
               EXIT DIALOG
         END DIALOG
      CLOSE WINDOW vtn_cbdl171
   END IF

END MAIN

PRIVATE FUNCTION fn_buscar_saldos()
   DEFINE v_consulta_saldo_safre     		STRING
   DEFINE v_valida_saldo_safre            STRING
   DEFINE v_ind_saldo                     DECIMAL(9,0)

   DEFINE v_subcuenta_desc                VARCHAR(50)
   DEFINE v_acciones                      DECIMAL(18,6)
   DEFINE i                               SMALLINT
   
   LET v_consulta_saldo_safre =  "SELECT ",
                                 "sdo.subcuenta || ' - ' || cat.subcuenta_desc, ",
                                 "sdo.monto_acciones ",
                                 "FROM cbd_saldo_conciliacion sdo ",
                                 "INNER JOIN cat_subcuenta cat ON cat.subcuenta = sdo.subcuenta ",
                                 "WHERE sdo.f_saldo = ? ",
                                 "AND sdo.origen_saldo = ? ",
                                 "ORDER BY 1"

   PREPARE exe_consulta_saldo_safre FROM v_consulta_saldo_safre

   LET v_valida_saldo_safre = "SELECT FIRST 1 id_derechohabiente FROM cbd_saldo_sin_adelanto where f_saldo = ?"
   PREPARE exe_valida_saldo_safre FROM v_valida_saldo_safre

   #Se valida que exista el saldo de safre con corte al ultimo dia del mes anterior en el sistema
   EXECUTE exe_valida_saldo_safre USING v_f_corte INTO v_ind_saldo
   IF v_ind_saldo IS NULL OR v_ind_saldo = 0 THEN
      LET v_ind_safre = 0
      CALL fn_mensaje("Conciliación por cuenta", 
                      "No es posible conciliar porque falta el cálculo del saldo SAFRE con corte al " || v_f_corte ,
                      "bn_about")
      RETURN
   ELSE
      LET v_ind_safre = 1
   END IF

   #Se valida que exista la BDNSVIV con corte al ultimo dia del mes anterior en el sistema
   SELECT
      cza.f_operacion,
      arc.nombre_archivo,
      cza.estado
   INTO 
      v_f_operacion,
      v_nombre_archivo,
      v_estado_archivo
   FROM cbd_cza_bdnsviv cza
   INNER JOIN glo_ctr_archivo arc ON arc.folio = cza.folio
   WHERE cza.f_corte = v_f_corte

   IF v_f_operacion IS NULL OR v_f_operacion < v_f_corte THEN
      LET v_ind_bdnsviv = 0
   ELSE
      LET v_ind_bdnsviv = v_estado_archivo
   END IF

   IF v_ind_bdnsviv < 2 THEN
      CALL fn_mensaje("Conciliación por cuenta", 
                      "No es posible conciliar porque falta cargar la BDNSVIV con corte al " || v_f_corte,
                      "bn_about")
      RETURN
   END IF

   IF v_ind_bdnsviv > 2 THEN
      CALL fn_mensaje("Conciliación por cuenta", 
                      "El proceso de conciliación para la fecha de corte " || v_f_corte || " ya fue ejecutado ",
                      "bn_about")
      RETURN
   END IF

   
   #Se genera la informacion para la seccion de saldos safre
   DECLARE cur_consulta_saldo_safre CURSOR FOR exe_consulta_saldo_safre
   LET i = 1
   FOREACH cur_consulta_saldo_safre USING v_f_corte, SALDO_SAFRE INTO v_subcuenta_desc, v_acciones
      IF v_subcuenta_desc IS NOT NULL THEN
         LET v_saldo_safre[i].subcuenta = v_subcuenta_desc
         LET v_saldo_safre[i].acciones = v_acciones

         LET i = i + 1
      END IF
   END FOREACH

   #Se genera la informacion para la seccion de saldos bdnsviv
   DECLARE cur_consulta_saldo_bdnsviv CURSOR FOR exe_consulta_saldo_safre
   LET i = 1
   FOREACH cur_consulta_saldo_bdnsviv USING v_f_corte, SALDO_BDNSVIV INTO v_subcuenta_desc, v_acciones
      IF v_subcuenta_desc IS NOT NULL THEN
         LET v_saldo_bdnsviv[i].subcuenta = v_subcuenta_desc
         LET v_saldo_bdnsviv[i].acciones = v_acciones

         LET i = i + 1
      END IF
   END FOREACH

END FUNCTION

PRIVATE FUNCTION fn_conciliar()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log
   
   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)

   DEFINE v_comando           STRING

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

   LET v_proceso_cod = 2109
   LET v_opera_cod = 1
   LET v_usuario = 'safreviv'
   LET v_folio = 1
   LET v_nom_archivo = 'prueba'

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      #DISPLAY "No paso la operacion"
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,v_usuario)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                             "CBDP17",v_nom_archivo,v_usuario)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"CBDP17",
                               v_nom_archivo,v_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDP17.42r ",
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

            #DISPLAY v_comando                        
            RUN v_comando
            IF(STATUS)THEN
               CALL fn_mensaje("Conciliación por cuenta", 
                               "Ocurrió un error al iniciar el proceso batch",
                               "bn_about")
            ELSE
               # Se indica que se realizo el proceso de carga
               CALL fn_mensaje("Conciliación por cuenta", 
                               "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                               "bn_about")
            END IF
         END IF
      END IF
   END IF
END FUNCTION