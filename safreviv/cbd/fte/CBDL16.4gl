################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 16/07/2014                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDL16                                                   #
#Objetivo          => Programa de consulta de movimientos adelantados y        #
#     					 generacion de archivos de detalle de adelantos           #
#Fecha inicio      => 16/07/2014                                               #
################################################################################
DATABASE safre_viv

GLOBALS "CBDL16.inc"

PRIVATE DEFINE v_tipo_proceso       SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog           VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario            VARCHAR(30) -- Almacena al usuario
PRIVATE DEFINE v_f_corte            DATE
PRIVATE DEFINE v_ind_adelanto			SMALLINT

#Arreglo dinamico para el manejo de los saldos
PRIVATE DEFINE v_saldo_periodo      DYNAMIC ARRAY OF saldos
PRIVATE DEFINE v_saldo_historico    DYNAMIC ARRAY OF saldos

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

	#se establece el v_ind_adelanto a cero para indicar que NO se ha ejecutado el proceso mensual
	LET v_ind_adelanto = 0

   CALL fn_buscar_saldos()

	IF v_ind_adelanto <> 1 THEN
		CALL fn_mensaje("Generar Archivo Adelantos",
                         "Aun no se ejecuta el proceso de preparacion de saldos para conciliacion con fecha de corte " || v_f_corte,
                         "about")
   #   RETURN
	END IF

   OPEN WINDOW vtn_cbdl161 WITH FORM "CBDL161"
      DIALOG   ATTRIBUTES(UNBUFFERED)
         DISPLAY ARRAY v_saldo_periodo TO saldo_periodo.* 
         END DISPLAY

         DISPLAY ARRAY v_saldo_historico TO saldo_historico.* 
         END DISPLAY

         BEFORE DIALOG
            DISPLAY  v_f_corte TO f_corte
            IF v_ind_adelanto <> 1 THEN
               #No se genero el saldo
               CALL DIALOG.setActionHidden("calcular_saldo", FALSE)
               CALL DIALOG.setActionHidden("generar_archivos", TRUE)
            ELSE
               #Saldo disponible
               CALL DIALOG.setActionHidden("calcular_saldo", TRUE)
               CALL DIALOG.setActionHidden("generar_archivos", FALSE)
            END IF

         ON ACTION calcular_saldo
            CALL fn_calcula_saldo_safre()
            EXIT DIALOG
         
         ON ACTION generar_archivos
            CALL fn_valida_generacion() RETURNING v_valida_generacion
            IF v_valida_generacion = 1 THEN
               CALL fn_genera_archivo() 
               EXIT DIALOG
            ELSE
               CONTINUE DIALOG
            END IF
            
         ON ACTION cancelar
            #CALL fn_mensaje ("Generar Archivo Adelantos", "Se canceló la generación del archivo con el detalle de adelantos" , "info")
            EXIT DIALOG
      END DIALOG
   CLOSE WINDOW vtn_cbdl161
END MAIN

PRIVATE FUNCTION fn_buscar_saldos()
   DEFINE v_consulta_adelanto       		STRING

   DEFINE v_modulo                     	CHAR(3)
   DEFINE v_subcuenta_desc                VARCHAR(50)
   DEFINE v_pesos                         DECIMAL(18,2)
   DEFINE v_acciones                      DECIMAL(18,6)
   DEFINE v_ind_periodo                   SMALLINT
   DEFINE i                               SMALLINT

   LET v_consulta_adelanto =  "SELECT ",
											"sdo.modulo, ",
											"(sdo.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
											"sdo.monto_acciones, ",
											"sdo.monto_pesos ",
										"FROM cbd_modulo_adelanto sdo ",
										"INNER JOIN cat_subcuenta cat on cat.subcuenta = sdo.subcuenta ",
										"WHERE sdo.subcuenta IN (4,8,55) ",
										"AND sdo.f_saldo = ? ",
                              "AND sdo.ind_periodo = ? ",
										"ORDER BY sdo.modulo, subcuenta"
   PREPARE exe_consulta_adelanto FROM v_consulta_adelanto
   DECLARE cur_consulta_adelanto CURSOR FOR exe_consulta_adelanto
   
   #Se genera la informacion para la seccion de saldos del periodo
   LET v_ind_periodo = 1
   LET i = 1
   FOREACH cur_consulta_adelanto USING v_f_corte, v_ind_periodo INTO v_modulo, v_subcuenta_desc, v_acciones, v_pesos
      IF v_modulo IS NOT NULL THEN
			LET v_saldo_periodo[i].modulo = v_modulo
         LET v_saldo_periodo[i].subcuenta = v_subcuenta_desc
         LET v_saldo_periodo[i].acciones = v_acciones
         LET v_saldo_periodo[i].pesos = v_pesos

         LET i = i + 1
			LET v_ind_adelanto = 1
      END IF
   END FOREACH

   #Se genera la informacion para la seccion de saldos historicos
   LET v_ind_periodo = 0
   LET i = 1
   FOREACH cur_consulta_adelanto USING v_f_corte, v_ind_periodo INTO v_modulo, v_subcuenta_desc, v_acciones, v_pesos
      IF v_modulo IS NOT NULL THEN
			LET v_saldo_historico[i].modulo = v_modulo
         LET v_saldo_historico[i].subcuenta = v_subcuenta_desc
         LET v_saldo_historico[i].acciones = v_acciones
         LET v_saldo_historico[i].pesos = v_pesos

         LET i = i + 1
			LET v_ind_adelanto = 1
      END IF
   END FOREACH
END FUNCTION

PRIVATE FUNCTION fn_valida_generacion()
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

   LET v_proceso_cod = 2108
   LET v_opera_cod = 1

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

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,v_usuario)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                             "CBDP16",v_nom_archivo,v_usuario)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
          # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"CBDP16",
                               v_nom_archivo,v_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            #Se asigna el nombre del archivo
            LET v_nom_archivo = "Adelantos_",v_f_corte USING "ddmmyyyy"

            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDP16.42r ",
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
      END IF
   END IF
   
END FUNCTION

PRIVATE FUNCTION fn_calcula_saldo_safre()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_folio             LIKE glo_ctr_archivo.folio
   DEFINE v_usuario             VARCHAR(30) -- Almacena al usuario

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

   LET v_proceso_cod = 2107
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
                                             "CBDP13",v_nom_archivo,v_usuario)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"CBDP13",
                               v_nom_archivo,v_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDP13.42r ",
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
               CALL fn_mensaje("Saldo Mensual", 
                               "Ocurrió un error al iniciar el proceso batch",
                               "bn_about")
            ELSE
               # Se indica que se realizo el proceso de carga
               CALL fn_mensaje("Saldo Mensual", 
                               "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                               "bn_about")
            END IF
         END IF
      END IF
   END IF
END FUNCTION