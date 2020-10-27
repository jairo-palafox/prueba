#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CBD                                                     #
#Programa          => CBDP18                                                  #
#Objetivo          => PROGRAMA PARA GENERAR LOS ARCHIVOS CON EL DETALLE DE    #
#                     DIFERENCIAS                                             #
#Fecha Inicio      => 23-JULIO-2014                                           #
###############################################################################

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_fcorte                   DATE

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
   DEFINE v_ruta_envio                    LIKE seg_modulo.ruta_envio

   DEFINE v_resultado                     SMALLINT

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)


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

   LET v_fcorte = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1
   
   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   
   #Se genera el archivo de salida
   CALL fn_genera_salida() RETURNING v_resultado
   
   IF v_resultado = 0 THEN
      # Finaliza la operacion de generacion del archivo
      #CALL fn_genera_reporte()
      
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN         
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Termino el proceso que genera los archivos de cuentas con diferencia de saldo"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "Los archivos de salida se encuentra en:"
      DISPLAY v_ruta_envio CLIPPED, "/CONCILIACION_POR_CUENTA_ARCHIVO_#_", v_formato_fecha ,".diferencia"
      DISPLAY ""
      DISPLAY "*******************************************************************"
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Ocurrio un ERROR al generar los archivos de salida"
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

   DEFINE v_consulta_intervalo   STRING
   DEFINE v_num_archivo          INTEGER
   DEFINE v_inicio               INTEGER
   DEFINE v_fin                  INTEGER

   DEFINE v_s_num_archivo        STRING
   DEFINE v_s_inicio             STRING
   DEFINE v_s_fin                STRING
   DEFINE v_ind_valido           SMALLINT  

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   LET v_consulta_intervalo = "SELECT num_archivo, inicio, fin FROM cbd_archivos_diferencia ORDER BY num_archivo"
   PREPARE exe_consulta_intervalo FROM v_consulta_intervalo
   DECLARE cur_consulta_intervalo CURSOR FOR exe_consulta_intervalo 

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/DETALLE_CONCILIACION.sql"
   LET v_formato_fecha = v_fcorte USING 'ddmmyyyy'
   
   FOREACH cur_consulta_intervalo INTO v_num_archivo, v_inicio, v_fin
      IF v_num_archivo IS NOT NULL AND v_num_archivo > 0 THEN
         LET v_s_num_archivo = v_num_archivo
         LET v_s_inicio = v_inicio
         LET v_s_fin = v_fin
         LET v_ind_valido = 0

         LET v_archivo = base.Channel.create()
         CALL v_archivo.openFile(v_nombre_archivo,"w")
         CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
         CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/CONCILIACION_POR_CUENTA_ARCHIVO_' || v_s_num_archivo CLIPPED || 
                                  '_'|| v_formato_fecha || '.diferencia')
         CALL v_archivo.writeLine('SELECT')
         CALL v_archivo.writeLine('det.nss,')
         CALL v_archivo.writeLine('det.cve_afore,')
         CALL v_archivo.writeLine('dif.subcuenta,')
         CALL v_archivo.writeLine('to_char(dif.acciones_safre,\'-##############&.&&&&&&\'),')
         CALL v_archivo.writeLine('to_char(dif.acciones_bdnsviv,\'-##############&.&&&&&&\'),')
         CALL v_archivo.writeLine('to_char(dif.diferencia,\'-##############&.&&&&&&\')')
         CALL v_archivo.writeLine('FROM cbd_detalle_bdnsviv det')
         CALL v_archivo.writeLine('INNER JOIN cbd_diferencia_saldo dif ON dif.id_derechohabiente = det.id_derechohabiente')

         #Se arman las condiciones para establecer los rangos
         IF v_inicio IS NULL OR v_inicio < 0 THEN
            IF v_fin IS NULL OR v_fin <= 0 THEN
               #Significa que los dos limites no son validos por lo que se envia el mensaje de error
               DISPLAY ""
               DISPLAY "*******************************************************************"
               DISPLAY " El rango definido para el archivo ", v_s_num_archivo CLIPPED , " no es valido"
               DISPLAY ""
               DISPLAY " Se omitirá la generación de ese archivo"
               DISPLAY "*******************************************************************"
            ELSE
               #Solo se indico el final del rango
               DISPLAY ""
               DISPLAY "*******************************************************************"
               DISPLAY ""
               DISPLAY " El archivo ", v_s_num_archivo CLIPPED , " contemplara los registros con DIFERENCIA MENOR a ", v_s_fin CLIPPED USING '###########&.&&&&&&'
               DISPLAY ""
               DISPLAY "*******************************************************************"
               CALL v_archivo.writeLine('WHERE ABS(dif.diferencia) <= ' || v_s_fin CLIPPED)
               CALL v_archivo.writeLine('AND dif.diferencia <> 0 ')
               LET v_ind_valido = 1
            END IF
         ELSE
            IF v_fin IS NULL OR v_fin <= 0 THEN
               #Significa que solo se indico el inicio del rango
               DISPLAY ""
               DISPLAY "*******************************************************************"
               DISPLAY ""
               DISPLAY " El archivo ", v_s_num_archivo CLIPPED , " contemplara los registros con DIFERENCIA MAYOR a ", v_s_inicio CLIPPED USING '###########&.&&&&&&'
               DISPLAY ""
               DISPLAY "*******************************************************************"
               CALL v_archivo.writeLine('WHERE ABS(dif.diferencia) > ' || v_s_inicio CLIPPED)
               CALL v_archivo.writeLine('AND dif.diferencia <> 0 ')
               LET v_ind_valido = 1
            ELSE
               #Se indico el inicio y fin del rango
               DISPLAY ""
               DISPLAY "*******************************************************************"
               DISPLAY ""
               DISPLAY " El archivo ", v_s_num_archivo CLIPPED , " contemplara el siguiente rango: "
               DISPLAY " Diferencia mayor a ", v_s_inicio CLIPPED USING '###########&.&&&&&&', " y menor a ", v_s_fin CLIPPED USING '###########&.&&&&&&'
               DISPLAY ""
               DISPLAY "*******************************************************************"
               CALL v_archivo.writeLine('WHERE ABS(dif.diferencia) <= ' || v_s_fin CLIPPED)
               CALL v_archivo.writeLine('AND ABS(dif.diferencia) > ' || v_s_inicio CLIPPED)
               CALL v_archivo.writeLine('AND dif.diferencia <> 0 ')
               LET v_ind_valido = 1
            END IF
         END IF

         CALL v_archivo.close()
         IF v_ind_valido = 1 THEN
            LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
            RUN v_comando
         END IF

         LET v_comando = "rm ", v_nombre_archivo
         #LET v_comando = "mv ", v_nombre_archivo CLIPPED, " ", v_nombre_archivo CLIPPED, "_",  v_num_archivo USING '&&&&&'
         RUN v_comando
         
      END IF
   END FOREACH

   RETURN 0
END FUNCTION

{PRIVATE FUNCTION fn_genera_reporte()
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
}
{
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
}
{
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
}