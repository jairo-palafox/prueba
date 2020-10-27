################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 11/04/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDC04                                                   #
#Objetivo          => Programa de consulta de movimientos adelantados y        #
#     					 generacion de archivos de detalle de adelantos           #
#Fecha inicio      => 09/04/2012                                               #
################################################################################
DATABASE safre_viv

PRIVATE DEFINE v_tipo_proceso       SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog           VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario            VARCHAR(30) -- Almacena al usuario
PRIVATE DEFINE v_f_corte            DATE
PRIVATE DEFINE v_ind_adelanto			SMALLINT

#Arreglo dinamico para el manejo de los saldos
PRIVATE DEFINE v_lista_saldos DYNAMIC ARRAY OF RECORD
	modulo									CHAR(3),
   subcuenta                        STRING,
   pesos                            DECIMAL(18,2),
   acciones                         DECIMAL(18,2)
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
   #LET v_f_corte = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1
   LET v_f_corte = "09/30/2013"

	#se establece el v_ind_adelanto a cero para indicar que NO se ha ejecutado el proceso mensual
	LET v_ind_adelanto = 0

   CALL fn_buscar_saldos()

	IF v_ind_adelanto <> 1 THEN
		CALL fn_mensaje("Generar Archivo Adelantos",
                         "Aun no se ejecuta el proceso de preparacion de saldos para conciliacion con fecha de corte " || v_f_corte,
                         "about")
      RETURN
	END IF

   OPEN WINDOW vtn_cbdc041 WITH FORM "CBDC041"
      DIALOG   ATTRIBUTES(UNBUFFERED)
         DISPLAY ARRAY v_lista_saldos TO lista_saldos.* 
         END DISPLAY

         BEFORE DIALOG
            DISPLAY  v_f_corte TO f_corte

         ON ACTION generar
            CALL fn_valida_generacion() RETURNING v_valida_generacion
            IF v_valida_generacion = 1 THEN
               CALL fn_genera_archivo() 
               EXIT DIALOG
            ELSE
               CONTINUE DIALOG
            END IF
            
         ON ACTION cancelar
            CALL fn_mensaje ("Generar Archivo Adelantos", "Se canceló la generación del archivo con el detalle de adelantos" , "info")
            EXIT DIALOG
      END DIALOG
   CLOSE WINDOW vtn_cbdc041
END MAIN

PRIVATE FUNCTION fn_buscar_saldos()
   DEFINE v_consulta_adelanto       		STRING

   DEFINE v_modulo                     	CHAR(3)
   DEFINE v_subcuenta_desc                VARCHAR(50)
   DEFINE v_pesos                         DECIMAL(18,2)
   DEFINE v_acciones                      DECIMAL(18,2)
   DEFINE i                               SMALLINT

   LET v_consulta_adelanto =  "SELECT ",
											"sdo.modulo, ",
											"(sdo.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
											"SUM(sdo.monto_acciones), ",
											"SUM(sdo.monto_pesos) ",
										"FROM cbd_modulo_adelanto sdo ",
										"INNER JOIN cat_subcuenta cat on cat.subcuenta = sdo.subcuenta ",
										"WHERE sdo.subcuenta IN (4,8) ",
										"AND fondo_inversion = 11 ",
										"AND sdo.f_saldo = ? ",
										"GROUP BY sdo.modulo,subcuenta ",
										"ORDER BY sdo.modulo, subcuenta"
   PREPARE exe_consulta_adelanto FROM v_consulta_adelanto
   DECLARE cur_consulta_adelanto CURSOR FOR exe_consulta_adelanto
   
   #Se genera la informacion para la seccion de saldos
   LET i = 1
   FOREACH cur_consulta_adelanto USING v_f_corte INTO v_modulo, v_subcuenta_desc, v_acciones, v_pesos
      IF v_modulo IS NOT NULL THEN
			LET v_lista_saldos[i].modulo = v_modulo
         LET v_lista_saldos[i].subcuenta = v_subcuenta_desc
         LET v_lista_saldos[i].acciones = v_acciones
         LET v_lista_saldos[i].pesos = v_pesos

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
      LET v_nom_archivo = "Adelantos_",v_f_corte USING "yymmdd"

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
