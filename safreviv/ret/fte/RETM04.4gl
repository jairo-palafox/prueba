#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => RET                                                     #
#Programa          => RETM03                                                  #
#Objetivo          => PROGRAMA PARA INTEGRAR LOS RECHAZOS DEL BANCO           #
#Fecha Inicio      => 29-JULIO-2013                                           #
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

#Variables para el manejo del registro contable
PRIVATE DEFINE v_fpago                    DATE
PRIVATE DEFINE v_proceso_cnt              INTEGER
PRIVATE DEFINE v_transaccion_cnt          INTEGER
PRIVATE DEFINE v_result_cnt               INTEGER
PRIVATE DEFINE v_cuenta_contable          INTEGER

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_fn_integra_rechazos_banco     STRING
   DEFINE v_contabilidad                       STRING
   #DEFINE v_ruta_envio                    LIKE seg_modulo.ruta_envio

   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)

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
   
   #Primero se carga el archivo a una tabla temporal
   CALL fn_carga_archivo() RETURNING v_resultado

   IF v_resultado = 0 THEN
      WHENEVER ERROR CONTINUE
         #Despues se integra el archivo en el sistema
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY " Inicia el proceso que integra los rechazos del banco"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         
         LET v_fn_integra_rechazos_banco = "EXECUTE FUNCTION fn_integra_rechazos_banco(?)"
         PREPARE exe_fn_integra_rechazos_banco FROM v_fn_integra_rechazos_banco
         EXECUTE exe_fn_integra_rechazos_banco USING v_folio INTO v_resultado, v_mensaje_respuesta
         IF SQLCA.SQLCODE <> 0 THEN
            DISPLAY "Ocurrio un ERROR en la funcion de integracion: "
            DISPLAY SQLCA.SQLCODE
            DISPLAY SQLERRMESSAGE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            RETURN
         END IF
      WHENEVER ERROR STOP
      
      DISPLAY ""
      DISPLAY "Finaliza la funcion de integracion de rechazos del banco"
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

         CALL fn_genera_salida()

         #Se ejecuta el registro contable
         LET v_contabilidad = "EXECUTE PROCEDURE fn_ret_fnd_ah_cnt57(?,?,?,?,?)"
         PREPARE exe_contabilidad FROM v_contabilidad

         LET v_fpago = TODAY
         LET v_proceso_cnt = 71
         LET v_transaccion_cnt = 0
         WHENEVER ERROR CONTINUE
            EXECUTE exe_contabilidad USING   v_folio,
                                             v_fpago,
                                             v_proceso_cnt,
                                             p_proceso_cod,
                                             v_transaccion_cnt
                                     INTO    v_result_cnt
         WHENEVER ERROR STOP
         IF SQLCA.sqlcode < 0 THEN
            DISPLAY "Código de ERROR SQL de registro contable: ",SQLCA.sqlcode
            -- Función para finalizar la operación en error
            # Actualiza a estado erróneo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
         END IF
         
         IF v_result_cnt = 1 THEN -- 1 es correcto
            SELECT COUNT (*)
            INTO v_cuenta_contable
            FROM cnt_transaccion
            WHERE folio_liquida = v_folio

            IF v_cuenta_contable > 0 THEN
               DISPLAY "El registro contable de retiros fondo de ahorro masivo se realizó exitosamente."
            ELSE
               DISPLAY "Error: El registro contable no se realizó debidamente."
               # Actualiza a estado erróneo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            END IF
         ELSE
            DISPLAY "Ocurrió un error al realizar el registro contable."
            # Actualiza a estado erróneo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
         END IF

   
         
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Termino el proceso que integra los rechazos del banco"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
      ELSE
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Ocurrio un ERROR al integrar los rechazos del banco"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
   END IF
END MAIN

PRIVATE FUNCTION fn_carga_archivo()
   DEFINE v_elimina_tabla           STRING
   DEFINE v_crea_tabla              STRING
   DEFINE v_estadisticas            STRING

   DEFINE r_resultado_opera         INTEGER
   DEFINE v_comando                 STRING
   DEFINE v_nombre_archivo          STRING
   DEFINE v_nombre_archivo_carga    STRING
   DEFINE v_archivo                 base.Channel

   -- se obtienen la ruta envio del modulo
   DATABASE safre_viv

   DISPLAY "*******************************************************************"
   DISPLAY " Inicia la carga del archivo con los rechazos del banco"
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   
   SELECT ruta_rescate 
   INTO v_ruta_rescate
   FROM seg_modulo
   WHERE modulo_cod = 'ret'

   DATABASE safre_tmp
   
   #Primero se inicializa la tabla temporal
   LET v_elimina_tabla = "DROP TABLE IF EXISTS tmp_fondo72_rechazo_banco"
   PREPARE prp_drop FROM v_elimina_tabla
   EXECUTE prp_drop
   IF(SQLCA.sqlcode < 0)THEN
      DISPLAY "Ocurrió error al borrar tabla tmp_fondo72_rechazo_banco"
      DISPLAY SQLCA.SQLCODE
      DISPLAY SQLERRMESSAGE
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      RETURN -1
   END IF

   #Se crea la tabla temporal
   LET v_crea_tabla = "create table tmp_fondo72_rechazo_banco ",
                       "( ",
                         "cve_rastreo char(30), ",
                         "estatus_detalle char(1), ",
                         "clabe char(20), ",
                         "cve_rechazo smallint, ",
                         "desc   char(40) ",
                       ") fragment by round robin in tmp_1_dbs, tmp_2_dbs, tmp_3_dbs, tmp_4_dbs;"
  
   PREPARE prp_crea_tabla FROM v_crea_tabla
   EXECUTE prp_crea_tabla
   IF(SQLCA.sqlcode < 0)THEN
      DISPLAY "Ocurrió error al crear tabla tmp_fondo72_rechazo_banco"
      DISPLAY SQLCA.SQLCODE
      DISPLAY SQLERRMESSAGE
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      RETURN -1
   END IF

   #Se crea el archivo para utilizar DBLOAD

   LET v_nombre_archivo_carga = v_ruta_rescate CLIPPED,"/", p_nombre_archivo CLIPPED,"_OK"
   
   LET v_comando = "sed -f ",v_ruta_rescate CLIPPED,"/elimina ", v_ruta_rescate CLIPPED, "/",
                    p_nombre_archivo CLIPPED," > ", v_nombre_archivo_carga CLIPPED
   RUN v_comando
   
   LET v_nombre_archivo = v_ruta_rescate CLIPPED,"/CARGA_RECHAZOS_BANCO.dbload"
   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   
   CALL v_archivo.writeLine('FILE "' || v_nombre_archivo_carga CLIPPED||'" DELIMITER "|" 27;')
   CALL v_archivo.writeLine('INSERT INTO tmp_fondo72_rechazo_banco (cve_rastreo, estatus_detalle, clabe, cve_rechazo, desc)')
   CALL v_archivo.writeLine('VALUES(f12,f13,f23,f26, f27);')
   CALL v_archivo.close()

   LET v_comando = "chmod 777 ", v_nombre_archivo CLIPPED
   RUN v_comando
   
   LET v_comando = "dbload -d safre_tmp -c ", v_nombre_archivo CLIPPED, " -e 1 -n 1000 -l error_dbload.log"
   RUN v_comando

   --Se corren estadisticas a la tabla temporal
   LET v_estadisticas = "UPDATE statistics FOR TABLE tmp_fondo72_rechazo_banco;"
   PREPARE exe_estadisticas FROM v_estadisticas
   EXECUTE exe_estadisticas
   IF(SQLCA.sqlcode < 0)THEN
      DISPLAY "Ocurrió error al correr las estadisticas de la tabla tmp_fondo72_rechazo_banco"
      DISPLAY SQLCA.SQLCODE
      DISPLAY SQLERRMESSAGE
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      RETURN -1
   END IF

   --Se elimina el archivo con el script para dbload
   LET v_comando = "rm ", v_nombre_archivo CLIPPED," ",v_nombre_archivo_carga CLIPPED
   RUN v_comando

   DATABASE safre_viv

   DISPLAY "*******************************************************************"
   DISPLAY " Termina la carga del archivo con los rechazos de ADS"
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   RETURN 0
END FUNCTION

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

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/GENERA_SALIDA_RECHAZO_BANCO.sql"
   LET v_fecha = TODAY USING 'yyyymmdd'

   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/REGISTROS_RECHAZADOS_BANCO_' || v_fecha || '.rchsiaff')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('afi.nss,')
   CALL v_archivo.writeLine('afi.rfc,')
   CALL v_archivo.writeLine('afi.nombre,')
   CALL v_archivo.writeLine('ret.saldo,')
   CALL v_archivo.writeLine('ret.cod_rechazo ')
   CALL v_archivo.writeLine('FROM ret_detalle_spei ret')
   CALL v_archivo.writeLine('INNER JOIN safre_tmp:tmp_fondo72_rechazo_banco rch ON rch.cve_rastreo = ret.cve_rastreo')
   CALL v_archivo.writeLine('INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72')
   CALL v_archivo.writeLine('WHERE rch.estatus_detalle = 3')
   CALL v_archivo.writeLine('AND ret.cod_rechazo BETWEEN 300 AND 350')
   CALL v_archivo.writeLine('ORDER BY ret.cod_rechazo DESC;')

   CALL v_archivo.close()

   LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
   RUN v_comando

   LET v_comando = "rm ", v_nombre_archivo
   RUN v_comando

   DISPLAY "*******************************************************************"
   DISPLAY " Termina la generacion de archivos con el detalle de cuentas"
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY " El archivo con los registros rechazados por el banco es:"
   DISPLAY " ",  v_ruta_envio CLIPPED,  "/REGISTROS_RECHAZADOS_BANCO_", v_fecha, ".rchsiaff"
   DISPLAY "*******************************************************************"

END FUNCTION