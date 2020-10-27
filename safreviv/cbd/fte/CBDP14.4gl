#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CBD                                                     #
#Programa          => CBDP14                                                  #
#Objetivo          => PROGRAMA PARA CALCULAR LOS MOVIMIENTOS ADELANTADOS      #
#Fecha Inicio      => 07-JULIO-2014                                           #
###############################################################################
DATABASE safre_viv

GLOBALS "CBDP14.inc"

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

PRIVATE DEFINE v_lista_tabla              cat_tab_movimiento
PRIVATE DEFINE v_resultado						SMALLINT
PRIVATE DEFINE v_finicio                  DATE
PRIVATE DEFINE v_ffin                     DATE

MAIN
   DEFINE v_fn_adelanto                   STRING
   DEFINE v_consulta_tablas               STRING
   DEFINE v_tabla                         VARCHAR(40)
   DEFINE v_anio                          SMALLINT
   DEFINE i                               SMALLINT
   DEFINE v_ind_error                     SMALLINT
   
	-- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 
                         
   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   #Se establece la fecha de corte como el ultimo dia natural del mes inmediato anterior
   LET v_ffin = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1
   LET v_finicio = MDY(MONTH(v_ffin),1,YEAR(v_ffin))

   DISPLAY ""
   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY " Inicia el proceso para calcular los movimientos adelantados"
   DISPLAY " "
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY ""

   #Agregamos la tabla de movimientos actual
   INITIALIZE v_lista_tabla TO NULL
   CALL v_lista_tabla.appendElement()
   LET v_lista_tabla[1].tabla = 'cta_movimiento'
   LET v_lista_tabla[1].anio = YEAR(TODAY)

   WHENEVER ERROR CONTINUE
      LET v_consulta_tablas = "SELECT tabla, anio FROM cat_tab_movimiento"
      PREPARE exe_consulta_tablas FROM v_consulta_tablas
      IF SQLCA.SQLCODE = 0 THEN
         #Solo se continua si existe la tabla con el catalogo
         LET i = 2
         DECLARE cur_consulta_tablas CURSOR FOR exe_consulta_tablas
         FOREACH cur_consulta_tablas INTO v_lista_tabla[i].*
            LET i = i + 1
         END FOREACH
      END IF
   WHENEVER ERROR STOP

   #Inicializamos la tabla de adelantos
   WHENEVER ERROR CONTINUE
      LET v_fn_adelanto = "EXECUTE FUNCTION fn_cbd_inicializa_adelantos()"
      PREPARE exe_inicializa_adelantos FROM v_fn_adelanto
      EXECUTE exe_inicializa_adelantos INTO v_resultado
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR al intentar inicializar el calculo de adelantos: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING v_resultado
      END IF
      IF v_resultado = 0 THEN
         LET v_ind_error = 0
         FOR i = 1 TO v_lista_tabla.getLength()
            IF v_lista_tabla[i].tabla IS NOT NULL THEN
               LET v_tabla = v_lista_tabla[i].tabla CLIPPED
               LET v_anio = v_lista_tabla[i].anio
               DISPLAY "*******************************************************************"
               DISPLAY " Calculando los adelantos en la tabla ", v_tabla
               DISPLAY " "
               DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
               DISPLAY " HORA               : ",TIME(CURRENT)
               DISPLAY "*******************************************************************"
               CALL fn_calcula_adelanto(v_tabla) RETURNING v_resultado
               IF v_resultado <> 0 THEN
                  #Significa que ocurrio algun error en la ejecucion del calculo
                  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                  RETURNING v_resultado
                  LET v_ind_error = 1
                  EXIT FOR
               END IF
            ELSE 
            EXIT FOR
            END IF
         END FOR
         IF v_ind_error = 0 THEN
               DISPLAY "*******************************************************************"
               DISPLAY " Calculando los adelantos globales"
               DISPLAY " "
               DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
               DISPLAY " HORA               : ",TIME(CURRENT)
               DISPLAY "*******************************************************************"

            LET v_fn_adelanto = "EXECUTE FUNCTION fn_cbd_adelantos_globales(?)"
            PREPARE exe_saldar_adelantos FROM v_fn_adelanto
            EXECUTE exe_saldar_adelantos USING v_ffin INTO v_resultado
            IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "Ocurrio un ERROR al calcular los adelantos globales: "
               DISPLAY SQLCA.SQLCODE
               DISPLAY SQLERRMESSAGE
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING v_resultado
            END IF
            IF v_resultado = 0 THEN
               CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
               RETURNING v_resultado
               DISPLAY "*******************************************************************"
               DISPLAY ""
               DISPLAY "Termino el calculo de movimientos adelantados: "
               DISPLAY ""
               DISPLAY " PROCESO            : ",v_proceso_desc
               DISPLAY " OPERACIÓN          : ",v_opera_desc
               DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
               DISPLAY " HORA               : ",TIME(CURRENT)
               DISPLAY ""
               DISPLAY "*******************************************************************"
            END IF
         END IF
      END IF
   WHENEVER ERROR STOP
END MAIN

PRIVATE FUNCTION fn_calcula_adelanto(p_tabla)
   DEFINE p_tabla                         VARCHAR(40)
   DEFINE v_comando                       STRING
   DEFINE v_ruta_archivo                  VARCHAR(50)
   DEFINE v_nombre_archivo                STRING
   DEFINE v_archivo                       base.Channel
   DEFINE v_fecha                         STRING

   DEFINE v_modulo                        CHAR(3)
   DEFINE v_ind_executa                   SMALLINT
   DEFINE v_consulta_modulo               STRING
   DEFINE v_valida_adelanto               STRING
   DEFINE v_valida_tabla                  STRING

   SELECT ruta_envio
   INTO v_ruta_archivo
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   DELETE FROM cbd_control_adelanto

   DISPLAY "Validando la existencia de la tabla ", p_tabla

   WHENEVER ERROR CONTINUE
      LET v_valida_tabla = "SELECT * FROM ", p_tabla
      PREPARE exe_valida_tabla FROM v_valida_tabla
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "La tabla ", p_tabla, " no existe en el sistema, se continua el calculo con la siguiente tabla"
         RETURN 0
      END IF
   WHENEVER ERROR STOP

   LET v_consulta_modulo = "SELECT modulo_cod FROM cbd_proceso_concilia where ind_adelanto = 1"
   PREPARE exe_consulta_modulo FROM v_consulta_modulo
   DECLARE cur_consulta_modulo CURSOR FOR exe_consulta_modulo
   FOREACH cur_consulta_modulo INTO v_modulo
      IF v_modulo IS NOT NULL THEN
         #Primero se valida que exista la funcion que calcula los adelantos
         WHENEVER ERROR CONTINUE
			LET v_valida_adelanto = "EXECUTE FUNCTION fn_cbd_adelantos_", v_modulo CLIPPED, "(?,?,?)" 
			PREPARE exe_valida_adelanto FROM v_valida_adelanto
			IF SQLCA.SQLCODE = 0 THEN
				#Si existe la funcion se arma la instruccion para ejecutar
            LET v_nombre_archivo = v_ruta_archivo CLIPPED,"/calcula_adelanto_", v_modulo CLIPPED, ".sql"
            LET v_archivo = base.Channel.create()
            CALL v_archivo.openFile(v_nombre_archivo,"w")
            CALL v_archivo.writeLine('SET OPTIMIZATION LOW;');
            CALL v_archivo.writeLine('SET LOCK MODE TO WAIT 3;');
            CALL v_archivo.writeLine("EXECUTE PROCEDURE fn_cbd_adelantos_" || v_modulo || "(")
            LET v_fecha = v_finicio USING 'mmddyy'
            CALL v_archivo.writeLine("'" || v_fecha || "',")
            LET v_fecha = v_ffin USING 'mmddyy'
            CALL v_archivo.writeLine("'" || v_fecha || "',")
            CALL v_archivo.writeLine("'" || p_tabla || "');")

            LET v_comando = "nohup dbaccess safre_viv ", v_nombre_archivo CLIPPED, " &"
            DISPLAY ""
            DISPLAY "Inicia el calculo de adelantos para : ", v_modulo CLIPPED
            INSERT INTO cbd_control_adelanto (modulo,estado) VALUES (v_modulo,1)
            RUN v_comando
            SLEEP (2)    #El proceso espera 2 segundos para lanzal la siguiente etapa
			END IF
		WHENEVER ERROR STOP
      END IF
   END FOREACH

   #Inicia el monitoreo de los procesos en ejecucion
   LET v_valida_adelanto = "SELECT FIRST 1 modulo FROM cbd_control_adelanto where estado = 1"
   PREPARE exe_valida_ejecucion FROM v_valida_adelanto

   LET v_ind_executa = 1
   WHILE v_ind_executa = 1
      SLEEP (60)    #El proceso se duerme 3 minutos
      INITIALIZE v_modulo TO NULL
      EXECUTE exe_valida_ejecucion INTO v_modulo
      DISPLAY ""
      DISPLAY "Modulo en ejecucion: ", v_modulo
      IF v_modulo IS NULL THEN
         #Si el id es nulo significa que ya se termino la ejecucion de todos los procesos
         LET v_ind_executa = 0
      END IF 
   END WHILE

   DISPLAY "Termina el calculo de adelantos en la tabla ", p_tabla

   LET v_comando = "cd ",v_ruta_archivo CLIPPED,";rm calcula_adelanto_*.sql"
   RUN v_comando

   INITIALIZE v_modulo TO NULL
   LET v_valida_adelanto = "SELECT FIRST 1 modulo FROM cbd_control_adelanto where estado = 9"
   PREPARE exe_valida_estatus FROM v_valida_adelanto
   EXECUTE exe_valida_estatus INTO v_modulo
   IF v_modulo IS NULL THEN
      #Si el id es nulo significa que no ocurrieron errores
      DISPLAY ""
      DISPLAY "El calculo de adelantos termino cerroectamente para todos los modulos"
      DISPLAY ""
      RETURN 0
   ELSE
      DISPLAY ""
      DISPLAY "El calculo de adelantos para el modulo ", v_modulo, " no termino correctamente"
      DISPLAY ""
      RETURN 1
   END IF 
END FUNCTION