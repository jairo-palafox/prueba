#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => RET                                                     #
#Programa          => RETM03                                                  #
#Objetivo          => PROGRAMA PARA REEVALUAR LOS CANDIDATOS                  #
#Fecha Inicio      => 1-AGOSTO-2013                                           #
###############################################################################
IMPORT os

DATABASE safre_viv

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
PRIVATE DEFINE v_ruta_rescate             CHAR(40)
PRIVATE DEFINE v_ruta_listados            CHAR(40)

PRIVATE DEFINE v_ruta_reporte             STRING

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_fn_reevalua_masivo            STRING
   #DEFINE v_ruta_envio                    LIKE seg_modulo.ruta_envio

   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)

   DEFINE v_reenvio                       INTEGER

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

   IF v_resultado = 0 THEN
      WHENEVER ERROR CONTINUE
         #Despues se integra el archivo en el sistema
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY " Inicia el proceso que reevalua los candidatos al programa de pago masivo"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         
         LET v_fn_reevalua_masivo = "EXECUTE FUNCTION fn_reevalua_masivo_fondo72()"
         PREPARE exe_fn_reevalua_masivo FROM v_fn_reevalua_masivo
         EXECUTE exe_fn_reevalua_masivo INTO v_resultado, v_mensaje_respuesta
         IF SQLCA.SQLCODE <> 0 THEN
            DISPLAY "Ocurrio un ERROR en la funcion: "
            DISPLAY SQLCA.SQLCODE
            DISPLAY SQLERRMESSAGE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            RETURN
         END IF
      WHENEVER ERROR STOP
      
      DISPLAY ""
      DISPLAY "Finaliza la funcion"
      DISPLAY "Mensaje de respuesta: ", v_mensaje_respuesta
      IF v_resultado = 0 THEN
         # Finaliza la operacion de generacion del archivo
         CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING r_resultado_opera
         IF(r_resultado_opera <> 0)THEN         
            # Actualiza a estado erróneo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
         END IF

         #Se valida si el proceso encontro nuevos candidatos
         SELECT 
            count(*)
         INTO
            v_reenvio
         FROM ret_fondo_ahorro_masivo
         WHERE estado_solicitud = 66
            
         IF v_reenvio > 0 THEN
            CALL fn_genera_salida()
            CALL fn_genera_reporte()
            DISPLAY ""
            DISPLAY "EL PROCESO ENCONTRO NUEVOS CANDIDATOS PARA EL PAGO MASIVO"
            DISPLAY "ANTES DE GENERAR EL PAGO SIAFF ES NECESARIO QUE SE ENVIE EL ARCHIVO DE NUEVOS ACEPTADOS AL SISTEMA DE ADS"
         ELSE
            DISPLAY ""
            DISPLAY "EL PROCESO NO ENCONTRO NUEVOS CANDIDATOS"
            DISPLAY "YA ESTA ACTIVO EL NUEVO PAGO SIAFF PARA SU EJECUCION"
         END IF

         
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Termino el proceso"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
      ELSE
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Ocurrio un ERROR"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
   END IF
END MAIN

PRIVATE FUNCTION fn_genera_salida()
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel

   DEFINE v_fecha             STRING
   DEFINE v_comando           STRING

   DISPLAY "*******************************************************************"
   DISPLAY " Inicia la generacion de archivos con el detalle de cuentas"
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'ret'

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/GENERA_SALIDA_REENVIO_SIAFF.sql"
   LET v_fecha = TODAY USING 'yyyymmdd'

   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/REGISTROS_ACEPTADOS_REENVIO_SIAFF_' || v_fecha || '.siaff')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('ret.nss,')
   CALL v_archivo.writeLine('afi.rfc,')
   CALL v_archivo.writeLine('afi.nombre,')
   CALL v_archivo.writeLine('ret.importe_viv72,')
   CALL v_archivo.writeLine('"0"')
   CALL v_archivo.writeLine('FROM ret_fondo_ahorro_masivo ret')
   CALL v_archivo.writeLine('INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72')
   CALL v_archivo.writeLine('WHERE ret.estado_solicitud = 66;')
   CALL v_archivo.writeLine('')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/REGISTROS_RECHAZADOS_REENVIO_SIAFF_' || v_fecha || '.siaff')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('ret.nss,')
   CALL v_archivo.writeLine('afi.rfc,')
   CALL v_archivo.writeLine('afi.nombre,')
   CALL v_archivo.writeLine('ret.importe_viv72,')
   CALL v_archivo.writeLine('ret.cod_rechazo ')
   CALL v_archivo.writeLine('FROM ret_fondo_ahorro_masivo ret')
   CALL v_archivo.writeLine('INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72')
   CALL v_archivo.writeLine('WHERE ret.estado_solicitud = 100 ')
   CALL v_archivo.writeLine('ORDER BY ret.cod_rechazo DESC;')

   CALL v_archivo.close()

   LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
   RUN v_comando

   DISPLAY "*******************************************************************"
   DISPLAY " Termina la generacion de archivos con el detalle de cuentas"
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY " El archivo con los nuevos registros pendientes de pago previo a la generacion"
   DISPLAY " del pago SIAFF es:"
   DISPLAY " ",  v_ruta_envio CLIPPED,  "/REGISTROS_ACEPTADOS_REENVIO_SIAFF_", v_fecha, ".siaff"
   DISPLAY ""
   DISPLAY " El archivo con los registros rechazados previo a la generacion"
   DISPLAY " del pago SIAFF es:"
   DISPLAY " ",  v_ruta_envio CLIPPED,  "/REGISTROS_RECHAZADOS_REENVIO_SIAFF_", v_fecha, ".siaff"
   DISPLAY "*******************************************************************"

END FUNCTION

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

   DEFINE v_consulta          STRING
	DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
	DEFINE v_nombre	         STRING
	

   DEFINE v_rec_cifras  RECORD
      estado_solicitud     VARCHAR(50),
      cod_rechazo          VARCHAR(50),
      reg_total        DECIMAL(12,0)
   END RECORD

	SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'ret'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

	LET v_nombre = v_ruta_ejecutable CLIPPED, "/RETP031.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )

   LET v_consulta =  "SELECT ",
                        "ret.estado_solicitud || ' - ' || edo.des_larga, ",
                        "ret.cod_rechazo || ' - ' || rch.des_larga, ",
                        "count(*) ",
                     "FROM ret_fondo_ahorro_masivo ret ",
                     "INNER JOIN ret_estado_solicitud edo ON edo.estado_solicitud = ret.estado_solicitud ",
                     "LEFT JOIN ret_rechazo rch ON rch.cod_rechazo = ret.cod_rechazo ",
                     "WHERE ret.estado_solicitud NOT IN (65,71) ",
                     "GROUP BY 1,2 ",
                     "ORDER BY 1 DESC"
   PREPARE exe_consulta FROM v_consulta
   DECLARE cur_cifras CURSOR FOR exe_consulta
   
   START REPORT rep_cifras TO XML HANDLER vhandler
      FOREACH cur_cifras INTO v_rec_cifras.*
         OUTPUT TO REPORT rep_cifras (v_rec_cifras.*)
      END FOREACH
   FINISH REPORT rep_cifras
   
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion del archivo de cifras control, ", v_ruta_reporte
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
    WHERE modulo_cod = 'ret'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        p_usuario_cod CLIPPED , "-", -- usuario
                        "RETP03" CLIPPED, "-", -- programa
                        p_pid USING "&&&&&","-", -- PID
                        p_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        p_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
       EXIT PROGRAM
   END IF
  
   RETURN fgl_report_commitCurrentSettings()

END FUNCTION


REPORT rep_cifras (p_rec_cifras) 
   DEFINE p_rec_cifras  RECORD
      estado_solicitud     VARCHAR(50),
      cod_rechazo          VARCHAR(50),
      reg_total            DECIMAL(12,0)
   END RECORD

   DEFINE v_fecha               DATE
   
   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY
      PRINTX v_fecha USING "dd-mm-yyyy"
  
  ON EVERY ROW

   PRINTX p_rec_cifras.estado_solicitud,
          p_rec_cifras.cod_rechazo,
          p_rec_cifras.reg_total
END REPORT