#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => RET                                                     #
#Programa          => RETMP02                                                 #
#Objetivo          => PROGRAMA PARA INTEGRAR EL ARCHIVO DE CUENTAS CLAVE      #
#Fecha Inicio      => 15-JULIO-2013                                           #
###############################################################################

DATABASE safre_viv

GLOBALS "RETMP02.inc"

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

PRIVATE DEFINE v_ruta_listado             STRING

PRIVATE DEFINE v_fecha                    STRING

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_fn_integra_cuentas_clabe      STRING
   DEFINE v_fn_actualiza_archivo          STRING
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio

   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)
   DEFINE v_script                        VARCHAR(60)
   DEFINE v_comando                       STRING

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
   WHERE modulo_cod = 'ret'
   
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
   DISPLAY "Inicia la integracion de cuentas clabe"
   WHENEVER ERROR CONTINUE

      LET v_fn_actualiza_archivo = "EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(?,?,?,?)"
      PREPARE exe_fn_actualiza_archivo FROM v_fn_actualiza_archivo
      LET p_nombre_archivo = p_nombre_archivo CLIPPED
      CALL fn_prepara_salida() RETURNING v_script
   
      LET v_fn_integra_cuentas_clabe = "EXECUTE FUNCTION fn_integra_cuentas_clabe(?,?,?,?,?)"
      PREPARE exe_fn_integra_cuentas_clabe FROM v_fn_integra_cuentas_clabe
      EXECUTE exe_fn_integra_cuentas_clabe USING v_folio, p_nombre_archivo, p_pid, p_usuario_cod, v_script
                                           INTO v_resultado, v_mensaje_respuesta
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR en la funcion de integracion: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         CALL fn_actualiza_error()
         RETURN
      END IF
   WHENEVER ERROR STOP

   --Se elimina el archivo con el script que genera el archivo de salida
   LET v_comando = "rm ", v_script CLIPPED
   RUN v_comando
   
   DISPLAY ""
   DISPLAY "Finaliza la funcion de integracion de cuentas clave"
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

      EXECUTE exe_fn_actualiza_archivo USING p_nombre_archivo,
                                             v_folio,
                                             EDO_ARCHIVO_INTEGRADO,
                                             p_usuario_cod
                                       INTO  r_resultado_opera
      CALL fn_genera_reporte()
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Termino el proceso que integra las cuentas clabe"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "El detalle de registros aceptados con pago pendiente se encuentra en el archivo:"
      DISPLAY v_ruta_envio CLIPPED, "/PAGO_MASIVO_FONDO72_", v_fecha ,".PENDIENTE"
      DISPLAY ""
      DISPLAY "El detalle de registros rechazados se encuentra en el archivo:"
      DISPLAY v_ruta_envio CLIPPED, "/PAGO_MASIVO_FONDO72_", v_fecha ,".RECHAZO"
      DISPLAY ""
      DISPLAY "*******************************************************************"
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Ocurrio un ERROR al integrar las cuentas clabe"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      CALL fn_actualiza_error()
   END IF
END MAIN

PRIVATE FUNCTION fn_actualiza_error()
   DEFINE v_resultado_fn            INTEGER
   
   EXECUTE exe_fn_actualiza_archivo USING p_nombre_archivo,
                                          v_folio,
                                          EDO_ARCHIVO_ERROR, --Estado reversado (Esto para que permita cargar un archivo con el mismo nombre)
                                          p_usuario_cod
                                    INTO  v_resultado_fn
END FUNCTION

PRIVATE FUNCTION fn_prepara_salida()
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'ret'

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/GENERA_MASIVO_FONDO72.sql"
   LET v_fecha = TODAY USING 'yyyymmdd'

   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/PAGO_MASIVO_FONDO72_' || v_fecha || '.PENDIENTE')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('ret.nss,')
   CALL v_archivo.writeLine('afi.rfc,')
   CALL v_archivo.writeLine('afi.nombre,')
   CALL v_archivo.writeLine('ret.importe_viv72,')
   CALL v_archivo.writeLine('"0"')
   CALL v_archivo.writeLine('FROM ret_fondo_ahorro_masivo ret')
   CALL v_archivo.writeLine('INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72')
   CALL v_archivo.writeLine('WHERE ret.estado_solicitud IN (65,66);')
   CALL v_archivo.writeLine('')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/PAGO_MASIVO_FONDO72_' || v_fecha || '.RECHAZO')
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

   RETURN v_nombre_archivo
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte()
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

   DEFINE v_consulta          STRING
   DEFINE v_duplicados        STRING
   DEFINE v_no_afiliados      STRING
   DEFINE v_reg_dup           STRING
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

	LET v_nombre = v_ruta_ejecutable CLIPPED, "/RETMP021.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )

   LET v_consulta =  "SELECT ",
                        "ret.estado_solicitud || ' - ' || edo.des_larga, ",
                        "ret.cod_rechazo || ' - ' || rch.des_larga, ",
                        "count(*) ",
                     "FROM ret_fondo_ahorro_masivo ret ",
                     "INNER JOIN ret_estado_solicitud edo ON edo.estado_solicitud = ret.estado_solicitud ",
                     "LEFT JOIN ret_rechazo rch ON rch.cod_rechazo = ret.cod_rechazo ",
                     "GROUP BY 1,2 ",
                     "ORDER BY 1 DESC"
   {
   select d.objname,count(*) --v.nss,v.clabe,d.objname --count(*) --distinct nss)
     from ret_cta_clabe_vio v,
          ret_cta_clabe_dia d
    where v.informix_tupleid = d.informix_tupleid
    group by 1

   select count(*)
     from ret_cta_clabe
    where nss not in ( select nss from afi_fondo72)
   }
   PREPARE exe_consulta FROM v_consulta
   DECLARE cur_cifras CURSOR FOR exe_consulta

   LET v_duplicados =   "SELECT ",
                        "'100 - SOLICITUD RECHAZADA', ",
                        "ind.objname, ",
                        "count(*) ",
                        "FROM ret_cta_clabe_vio dup ",
                        "INNER JOIN ret_cta_clabe_dia ind ON ind.informix_tupleid = dup.informix_tupleid ",
                        "AND ind.informix_tupleid IN (SELECT informix_tupleid ",
                        "						            FROM ret_cta_clabe_dia ",
                        "						            GROUP BY 1 ",
                        "						            HAVING COUNT(*) = 1 ",
                        "						            ) ",
                        "group by 1,2"
   PREPARE exe_duplicados FROM v_duplicados
   DECLARE cur_duplicados CURSOR FOR exe_duplicados

   LET v_reg_dup =   "SELECT FIRST 1 ",
                     "'100 - SOLICITUD RECHAZADA', ",
                     "ind.objname, ",
                     "count(*) ",
                     "FROM ret_cta_clabe_vio dup ",
                     "INNER JOIN ret_cta_clabe_dia ind ON ind.informix_tupleid = dup.informix_tupleid ",
                     "AND ind.informix_tupleid IN (SELECT informix_tupleid ",
                     "						            FROM ret_cta_clabe_dia ",
                     "						            GROUP BY 1 ",
                     "						            HAVING COUNT(*) = 2 ",
                     "						            ) ",
                     "group by 1,2"
   PREPARE exe_reg_dup FROM v_reg_dup
   DECLARE cur_reg_dup CURSOR FOR exe_reg_dup

   LET v_no_afiliados = "SELECT ",
                           "'100 - SOLICITUD RECHAZADA', ",
                           "'CUENTA CLABE SIN NSS ASOCIADOS AL FONDO DE AHORRO', ",
                           "COUNT(*) ",
                        "FROM ret_cta_clabe ",
                        "WHERE nss NOT IN (SELECT nss FROM ret_fondo_ahorro_masivo) "
                       # "AND nss NOT IN (SELECT nss FROM ret_cta_clabe_vio)"
   PREPARE exe_no_afiliados FROM v_no_afiliados
   DECLARE cur_no_afiliados CURSOR FOR exe_no_afiliados
   
   START REPORT rep_cifras TO XML HANDLER vhandler
      FOREACH cur_cifras INTO v_rec_cifras.*
         OUTPUT TO REPORT rep_cifras (v_rec_cifras.*)
      END FOREACH

      #Se agregan los casos de nss o cuenta clabe repetida
      FOREACH cur_duplicados INTO v_rec_cifras.*
         IF v_rec_cifras.cod_rechazo = 'xpkret_cta_clabe' THEN
            LET v_rec_cifras.cod_rechazo = 'NSS DUPLICADO EN ARCHIVO DE CLABES'
         ELSE
            LET v_rec_cifras.cod_rechazo = 'CUENTA CLABE DUPLICADA EN EL ARCHIVO'
         END IF
         OUTPUT TO REPORT rep_cifras (v_rec_cifras.*)
      END FOREACH

      #Se agregan los casos de nss y cuenta clabe repetida
      FOREACH cur_reg_dup INTO v_rec_cifras.*
         LET v_rec_cifras.cod_rechazo = 'REGISTROS CON NSS Y CLABE DUPLICADOS EN ARCHIVO DE CLABES'
         OUTPUT TO REPORT rep_cifras (v_rec_cifras.*)
      END FOREACH

      #Se agregan los registros que no se encuentran en fondo de ahorro
      FOREACH cur_no_afiliados INTO v_rec_cifras.*
         OUTPUT TO REPORT rep_cifras (v_rec_cifras.*)
      END FOREACH
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
    WHERE modulo_cod = 'ret'

   LET v_ruta_listado = v_ruta_listados CLIPPED , "/" ,
                        p_usuario_cod CLIPPED , "-", -- usuario
                        "RETMP02" CLIPPED, "-", -- programa
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
