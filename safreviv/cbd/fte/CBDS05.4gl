#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CBD                                                     #
#Programa          => CBDS05                                                 #
#Objetivo          => PROGRAMA PARA GENERAR EL ARCHIVO DE SALDOS NEGATIVOS    #
#Fecha Inicio      => 05-SEPTIEMBRE-2013                                      #
###############################################################################

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE p_fecha                    DATE

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

PRIVATE DEFINE v_ruta_listado             STRING

PRIVATE DEFINE v_formato_fecha            STRING
PRIVATE DEFINE v_comando                  STRING

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_fn_cbd_prepara_negativos      STRING
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio

   DEFINE v_resultado                     SMALLINT

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_fecha          = ARG_VAL(7)


   WHENEVER ERROR CONTINUE

	CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso  

   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'
   
   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   
   -- se solicita el numero de folio asociado a la operacion
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid
   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
   
   #Se genera la tabla de saldos para precalificacion
   DISPLAY ""
   DISPLAY "Inicia el calculo de saldos negativos con fecha de corte ", p_fecha USING 'dd-mm-yyyy'
   WHENEVER ERROR CONTINUE
      LET v_fn_cbd_prepara_negativos = "EXECUTE FUNCTION fn_cbd_prepara_negativos(?)"
      PREPARE exe_fn_cbd_prepara_negativos FROM v_fn_cbd_prepara_negativos
      EXECUTE exe_fn_cbd_prepara_negativos USING p_fecha
                                           INTO v_resultado
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR en la funcion que calcula los saldos: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         RETURN
      END IF
   WHENEVER ERROR STOP

   #Se genera el archivo de salida
   CALL fn_genera_salida()
   
   #DISPLAY ""
   #DISPLAY "Finaliza la funcion de integracion de cuentas clave"
   #DISPLAY "Mensaje de respuesta: ", v_mensaje_respuesta
   IF v_resultado = 0 THEN
      # Finaliza la operacion de generacion del archivo
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN         
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
      CALL fn_genera_reporte()
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Termino el proceso que obtiene los saldos negativos"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "La salida se encuentra en el archivo:"
      DISPLAY v_ruta_envio CLIPPED, "/SALDOS_NEGATIVOS_", v_formato_fecha ,".sdo"
      DISPLAY ""
      DISPLAY "*******************************************************************"
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Ocurrio un ERROR al obtener los saldos negativos"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   END IF
END MAIN

PRIVATE FUNCTION fn_genera_salida()
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel

   DEFINE v_sdo_92				DECIMAL(22,6)
	DEFINE v_sdo_97				DECIMAL(22,6)
	DEFINE v_sdo_92_inf			DECIMAL(22,6)
	DEFINE v_sdo_97_inf			DECIMAL(22,6)
   DEFINE v_total_registros   INTEGER

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/EXPORTA_NEGATIVOS.sql"
   LET v_formato_fecha = p_fecha USING 'ddmmyyyy'

   DISPLAY v_nombre_archivo
   
   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/SALDOS_NEGATIVOS_' || v_formato_fecha || '.sdo')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('nss,')
   CALL v_archivo.writeLine('acciones_92,')
   CALL v_archivo.writeLine('acciones_97,')
   CALL v_archivo.writeLine('acciones_92_inf,')
   CALL v_archivo.writeLine('acciones_97_inf')
   CALL v_archivo.writeLine('FROM cbd_saldos_negativos ')

   CALL v_archivo.close()

   LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
   RUN v_comando

   LET v_comando = "rm ", v_nombre_archivo
   RUN v_comando

   #Se genera el sumario del archivo
   SELECT 
		SUM(acciones_92),
		SUM(acciones_97),
      SUM(acciones_92_inf),
		SUM(acciones_97_inf),
		COUNT(*)
	INTO
      v_sdo_92,
      v_sdo_97,
      v_sdo_92_inf,
      v_sdo_97_inf,
		v_total_registros
	FROM cbd_saldos_negativos

   LET v_comando = "echo 'SUMARIO|",
										v_sdo_92 CLIPPED, '|',
										v_sdo_97 CLIPPED, '|',
                              v_sdo_92_inf CLIPPED, '|',
                              v_sdo_97_inf CLIPPED, '|',
										v_total_registros CLIPPED, '|',
						"' >> ", v_ruta_envio CLIPPED, "/SALDOS_NEGATIVOS_", v_formato_fecha, ".sdo"
	RUN v_comando

END FUNCTION

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

	DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
	DEFINE v_nombre	         STRING

	SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cbd'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

	LET v_nombre = v_ruta_ejecutable CLIPPED, "/CBDS051.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )
   
   START REPORT rep_cifras TO XML HANDLER vhandler
      OUTPUT TO REPORT rep_cifras (v_folio)
   FINISH REPORT rep_cifras
   
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion del archivo de cifras control, ", v_ruta_listado
   DISPLAY ""
   DISPLAY "*******************************************************************"
END FUNCTION

FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
---------------------------------------------------------------------------

  DEFINE 
    v_reporte                STRING,
    v_formato                STRING,
    v_preview                INTEGER

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'cbd'

   LET v_ruta_listado = v_ruta_listados CLIPPED , "/" ,
                        p_usuario_cod CLIPPED , "-", -- usuario
                        "CBDS05" CLIPPED, "-", -- programa
                        p_pid USING "&&&&&","-", -- PID
                        p_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        p_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_ruta_listado)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
       EXIT PROGRAM
   END IF
  
   RETURN fgl_report_commitCurrentSettings()

END FUNCTION


REPORT rep_cifras (p_folio) 
   DEFINE p_folio                DECIMAL(9,0)
   DEFINE v_fecha                DATE

   DEFINE v_sdo_92				DECIMAL(22,6)
	DEFINE v_sdo_97				DECIMAL(22,6)
	DEFINE v_sdo_92_inf			DECIMAL(22,6)
	DEFINE v_sdo_97_inf			DECIMAL(22,6)

   DEFINE v_num_92            INTEGER
   DEFINE v_num_97            INTEGER
   DEFINE v_num_92_inf        INTEGER
   DEFINE v_num_97_inf        INTEGER
   DEFINE v_num_total         INTEGER
   
   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY

      SELECT
         SUM(acciones_92),
         SUM(acciones_97),
         SUM(acciones_92_inf),
         SUM(acciones_97_inf)
      INTO
         v_sdo_92,
         v_sdo_97,
         v_sdo_92_inf,
         v_sdo_97_inf
      FROM cbd_saldos_negativos

      SELECT COUNT(*)
      INTO v_num_92
      FROM cbd_saldos_negativos
      WHERE acciones_92 < 0

      SELECT COUNT(*)
      INTO v_num_97
      FROM cbd_saldos_negativos
      WHERE acciones_97 < 0

      SELECT COUNT(*)
      INTO v_num_92_inf
      FROM cbd_saldos_negativos
      WHERE acciones_92_inf < 0

      SELECT COUNT(*)
      INTO v_num_97_inf
      FROM cbd_saldos_negativos
      WHERE acciones_97_inf < 0

      SELECT COUNT(*)
      INTO v_num_total
      FROM cbd_saldos_negativos

      PRINTX p_folio
      PRINTX p_usuario_cod
      PRINTX v_fecha USING "dd-mm-yyyy"
      PRINTX p_fecha USING "dd-mm-yyyy"
      PRINTX v_sdo_92
      PRINTX v_sdo_97
      PRINTX v_sdo_92_inf
      PRINTX v_sdo_97_inf

      PRINTX v_num_92
      PRINTX v_num_97
      PRINTX v_num_92_inf
      PRINTX v_num_97_inf
      PRINTX v_num_total
  
END REPORT
