#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CBD                                                     #
#Programa          => CBDP20                                                  #
#Objetivo          => PROGRAMA PARA GENERAR EL ARCHIVO DE SALDOS DE FONDO     #
#                     ANTERIOR CON CORTE AL 31 DE DICIEMBRE DEL 2014          #
#Fecha Inicio      => 22/09/2014                                              #
###############################################################################

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE p_fcorte                  DATE
PRIVATE DEFINE v_fecha                    DATE

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

PRIVATE DEFINE v_ruta_listado             STRING
PRIVATE DEFINE v_salida                   STRING

PRIVATE DEFINE v_formato_fecha            STRING
PRIVATE DEFINE v_comando                  STRING

MAIN
   DEFINE r_resultado_opera               INTEGER
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
   LET p_fcorte         = ARG_VAL(7)


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

   #Generando el folio del proceso
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   DISPLAY ""
   DISPLAY "Inicia la generación del archivo con los saldos de Fondo 72"
   DISPLAY "Fecha de corte: ", p_fcorte USING 'dd-mm-yyyy'

   #Se genera el archivo de salida
   CALL fn_genera_salida() RETURNING v_resultado
   
   IF v_resultado = 0 THEN
      #Se genera el reporte con las cifras control
      CALL fn_genera_reporte()
      # Finaliza la operacion de generacion del archivo
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN         
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
      DISPLAY "*******************************************************************"
      DISPLAY "Termino el proceso que exporta los saldos de Fondo Anterior"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "La salida se encuentra en el archivo:"
      DISPLAY v_ruta_envio CLIPPED, "/" ,v_salida CLIPPED
      DISPLAY "*******************************************************************"
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY "Ocurrio un ERROR al generar el archivo"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY "*******************************************************************"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   END IF
END MAIN

PRIVATE FUNCTION fn_genera_salida()
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/EXPORTA_SALDO_FONDO_ANTERIOR.sql"
   LET v_formato_fecha = p_fcorte USING 'ddmmyyyy'
   LET v_salida = "SALDO_FONDO_ANTERIOR_" || v_formato_fecha || ".fondo"
   
   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/' || v_salida CLIPPED)
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('afi.nss,')
   CALL v_archivo.writeLine('afi.rfc,')
   CALL v_archivo.writeLine('afi.nombre,')
   CALL v_archivo.writeLine('SUM(mov.importe)')
   CALL v_archivo.writeLine('FROM afi_fondo72 afi')
   CALL v_archivo.writeLine('INNER JOIN cta_fondo72 mov ON mov.id_afi_fondo72 = afi.id_afi_fondo72')
   CALL v_archivo.writeLine('WHERE mov.f_liquida <= "' || p_fcorte || '"')
   CALL v_archivo.writeLine('AND mov.movimiento NOT IN (422,601)')  --Se omiten los movimientos de cargo y abono para el tanto adicional
   CALL v_archivo.writeLine('GROUP BY afi.nss, afi.rfc, afi.nombre')

   CALL v_archivo.close()

   LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
   RUN v_comando

   LET v_comando = "rm ", v_nombre_archivo
   RUN v_comando
   
   RETURN 0
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

	LET v_nombre = v_ruta_ejecutable CLIPPED, "/CBDS07.4rp"
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
                        "CBDS07" CLIPPED, "-", -- programa
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
   DEFINE v_monto_total          DECIMAL(22,2)
   
   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY

      SELECT SUM(mov.importe)
      INTO v_monto_total
      FROM cta_fondo72 mov
      INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = mov.id_afi_fondo72
      WHERE mov.f_liquida <= p_fcorte
      AND mov.movimiento NOT IN (422,601)  --Se omiten los movimientos de cargo y abono para el tanto adicional

      PRINTX v_fecha USING "dd-mm-yyyy"
      PRINTX p_fcorte USING "dd-mm-yyyy"
      PRINTX v_monto_total
      PRINTX v_salida
      PRINTX p_folio
      PRINTX p_usuario_cod
  
END REPORT
