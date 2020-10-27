################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 11/04/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDS01                                                   #
#Objetivo          =>  #
#Fecha inicio      => 09/04/2012                                               #
################################################################################
DATABASE safre_viv

PRIVATE DEFINE v_tipo_proceso       SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog           VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario            VARCHAR(30) -- Almacena al usuario
PRIVATE DEFINE v_f_corte            DATE

#Variables para el manejo de los sumarios
PRIVATE DEFINE v_sum_pesos          DECIMAL(18,2)
PRIVATE DEFINE v_sum_acciones       DECIMAL(18,2)
PRIVATE DEFINE v_sum_registros      DECIMAL(9,0)

#Arreglo dinamico para el manejo de los saldos
PRIVATE DEFINE v_lista_saldos DYNAMIC ARRAY OF RECORD
   subcuenta                        STRING,
   pesos                            DECIMAL(18,2),
   acciones                         DECIMAL(18,2),
   tot_registros                    DECIMAL(9,0)
END RECORD

MAIN
   DEFINE v_valida_generacion       SMALLINT
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
   --LET v_f_corte = MDY(10,31,2015)

   CALL fn_buscar_saldos()

   OPEN WINDOW vtn_cbds01 WITH FORM "CBDS011"
      DIALOG   ATTRIBUTES(UNBUFFERED)
         DISPLAY ARRAY v_lista_saldos TO lista_saldos.* 
         END DISPLAY

         BEFORE DIALOG
            DISPLAY  v_f_corte TO f_corte
            DISPLAY  v_sum_pesos TO sum_pesos
            DISPLAY  v_sum_acciones TO sum_acciones
            DISPLAY  v_sum_registros TO sum_registros

         ON ACTION generar
            CALL fn_valida_generacion() RETURNING v_valida_generacion
            IF v_valida_generacion = 1 THEN
               CALL fn_genera_archivo() 
               EXIT DIALOG
            ELSE
               CONTINUE DIALOG
            END IF
            
         ON ACTION cancelar
            CALL fn_mensaje ("BDNSVIV-PLUS", "Se canceló la generación del archivo BDNSVIV-PLUS" , "info")
            EXIT DIALOG
      END DIALOG
   CLOSE WINDOW vtn_cbds01
END MAIN

PRIVATE FUNCTION fn_buscar_saldos()
   DEFINE v_consulta_saldo       STRING

   DEFINE v_subcuenta                     SMALLINT
   DEFINE v_subcuenta_desc                VARCHAR(50)
   DEFINE v_pesos                         DECIMAL(18,2)
   DEFINE v_acciones                      DECIMAL(18,2)
   DEFINE i                               SMALLINT
   DEFINE num_viv97                       SMALLINT
   
   LET v_sum_pesos = 0
   LET v_sum_acciones = 0
   LET v_sum_registros = 0

   LET v_consulta_saldo =  "SELECT ",
                              "sdo.subcuenta, ",
                              "(sdo.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
                              "sdo.monto_acciones, ",
                              "sdo.monto_pesos ",
                           "FROM safre_sdo@vivws_tcp:cta_saldo_mensual_global sdo ",
                           "INNER JOIN cat_subcuenta cat on cat.subcuenta = sdo.subcuenta ",
                           "WHERE sdo.subcuenta IN (4,8,42,44,55) ",
                           "AND fondo_inversion = 11 ",
                           "AND sdo.f_saldo = ? ",
                           "ORDER BY sdo.subcuenta"
   PREPARE exe_consulta_saldo FROM v_consulta_saldo
   DECLARE cur_consulta_saldo CURSOR FOR exe_consulta_saldo
   
   #Se genera la informacion para la seccion de saldos
   LET i = 1
   FOREACH cur_consulta_saldo USING v_f_corte INTO v_subcuenta, v_subcuenta_desc, v_acciones, v_pesos
      IF v_subcuenta IS NOT NULL THEN
         IF v_subcuenta = 4 THEN
            LET num_viv97 = i
         END IF
         IF v_subcuenta = 55 THEN
            LET v_lista_saldos[num_viv97].acciones = v_lista_saldos[num_viv97].acciones + v_acciones
            LET v_lista_saldos[num_viv97].pesos = v_lista_saldos[num_viv97].pesos + v_pesos
         ELSE 
            LET v_lista_saldos[i].subcuenta = v_subcuenta_desc
            LET v_lista_saldos[i].acciones = v_acciones
            LET v_lista_saldos[i].pesos = v_pesos
         END IF

         #Se incrementa el sumario
         LET v_sum_pesos = v_sum_pesos + v_pesos
         LET v_sum_acciones = v_sum_acciones + v_acciones

         LET i = i + 1
      END IF
   END FOREACH
END FUNCTION

PRIVATE FUNCTION fn_valida_generacion()
   DEFINE v_consulta_archivo              STRING
   DEFINE v_consulta_bdnsviv              STRING
   DEFINE v_folio_bdnsviv                 DECIMAL(9,0)
   DEFINE v_folio_archivo                 DECIMAL(9,0)
   DEFINE v_nom_archivo                   VARCHAR(50)
   DEFINE v_mensaje                       STRING

   #Se valida que ya este cargado el archivo de la bdnsviv
{   LET v_consulta_bdnsviv =   "SELECT ",
                              "folio ",
                              "FROM cbd_cza_bdnsviv ",
                              "WHERE f_operacion = ? "
   PREPARE exe_consulta_bdnsviv FROM v_consulta_bdnsviv
   EXECUTE exe_consulta_bdnsviv USING v_f_corte INTO v_folio_bdnsviv
   IF v_folio_bdnsviv IS NULL THEN
      LET v_mensaje = "Aun no se carga el archivo de la bdnsviv con fecha de operación ", v_f_corte USING 'dd-mm-yyyy',
                      "\npor lo que no se puede generar el archivo de salida "
      CALL fn_mensaje("Atención", v_mensaje,"info")
      RETURN 0
   END IF}
   
   #Se busca en la tabla de control si ya se genero el archivo del periodo
   LET v_consulta_archivo =   "SELECT ",
                                 "folio, ",
                                 "nombre_archivo ",
                              "FROM cbd_ctr_interface ",
                              "WHERE  f_corte = ?"
   PREPARE exe_consulta_archivo FROM v_consulta_archivo
   EXECUTE exe_consulta_archivo USING v_f_corte INTO v_folio_archivo, v_nom_archivo

   IF v_folio_archivo IS NOT NULL THEN
      LET v_mensaje = "El archivo de saldos para la fecha de corte ", v_f_corte USING 'dd-mm-yyyy'," ya fue generado",
                      "\nEl folio asignado fue ", v_folio_archivo, 
                      "\nEl nombre del archivo generado fue ", v_nom_archivo
      CALL fn_mensaje("Atención", v_mensaje,"info")
      RETURN 0
   END IF
   RETURN 1
END FUNCTION

PRIVATE FUNCTION fn_genera_archivo()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log

   DEFINE r_resultado_opera   INTEGER
   DEFINE v_nom_archivo       CHAR(40)
   DEFINE v_comando           STRING
   
   LET v_proceso_cod = 2102
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

      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDS02.42r ",
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
         CALL fn_mensaje("Generacion De Archivo", 
                         "Ocurrió un error al iniciar el proceso batch",
                         "bn_about")
      ELSE
         # Se indica que se realizo el proceso de carga
         CALL fn_mensaje("Generacion De Archivo", 
                         "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                         "bn_about")
      END IF
      
   END IF
END FUNCTION