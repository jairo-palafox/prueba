#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CTA                                                     #
#Programa          => CTAP03                                                  #
#Objetivo          => PROGRAMA PARA GENERAR EL ARCHIVO DE SALDOS PARA         #
#                     EL AREA DE PRECALIFICACION                              #
#Fecha Inicio      => 26-FEBRERO-2013                                         #
###############################################################################
DATABASE safre_viv

GLOBALS "CTAP03.inc"

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_fcorte                           DATE

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados
PRIVATE DEFINE v_ruta_envio               LIKE seg_modulo.ruta_envio

#Variables para la generacion del archivo
PRIVATE DEFINE v_detalle                  detalle

#Variable para el manejo del archivo
PRIVATE DEFINE v_nom_archivo              STRING
PRIVATE DEFINE v_ruta_archivo             STRING
PRIVATE DEFINE v_archivo                  BASE.CHANNEL

DEFINE v_ruta_listado                     STRING

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_resultado_gen                 INTEGER
   DEFINE v_estado_ant                    SMALLINT
   

   DEFINE v_fn_genera_saldo_preca         STRING

   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)

   DEFINE v_consulta_anterior             STRING

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
      
   #Primero se valida si se puede ejecutar la generacion de saldos
   --Se establece la fecha de corte como el dia natural inmediato anterior
   LET v_fcorte = TODAY - 1;
   
      LET v_consulta_anterior = "SELECT estado_genera FROM safre_sdo@vivws_tcp:glo_ctr_saldo ",
      "WHERE tpo_saldo = 3 AND f_saldo = ?"
      PREPARE exe_consulta_anterior FROM v_consulta_anterior
      EXECUTE exe_consulta_anterior USING v_fcorte INTO v_estado_ant
   IF v_estado_ant = 1 THEN
      DISPLAY "El proceso no se puede ejecutar porque existe una generación de saldo en ejecución"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   ELSE
      IF v_estado_ant = 2 THEN
         DISPLAY "El proceso no se puede ejecutar porque el saldo para la fecha de corte ", v_fcorte USING 'dd,mm,yyyy', " ya fue generado"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      ELSE

         -- se obtienen la ruta envio del modulo
         SELECT ruta_envio 
         INTO v_ruta_envio
         FROM seg_modulo
         WHERE modulo_cod = 'cta'
   
         -- se solicita el numero de folio asociado a la operacion
         -- parametros: proceso, operacion, usuario
         CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
         RETURNING v_folio

         #Se actualiza el folio del proceso               
         UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

         UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
         
         #Se genera la tabla de saldos para precalificacion
         DISPLAY "Creando la tabla de saldos..."
         WHENEVER ERROR CONTINUE
            LET v_fn_genera_saldo_preca = "EXECUTE FUNCTION fn_genera_saldo_preca()"
            PREPARE exe_fn_genera_saldo_preca FROM v_fn_genera_saldo_preca
            EXECUTE exe_fn_genera_saldo_preca INTO v_resultado, v_mensaje_respuesta
            IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "Ocurrio un ERROR al intentar obtener el saldo para precalificacion: "
               DISPLAY SQLCA.SQLCODE
               DISPLAY SQLERRMESSAGE
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
               CALL fn_actualiza_error()
               RETURN
            END IF
         WHENEVER ERROR STOP
         
         DISPLAY " termina la ejecucion de la funcion que genera la tabla de saldos"
         DISPLAY "Mensaje de respuesta: ", v_mensaje_respuesta
         IF v_resultado = 1 THEN
            DISPLAY "Inicia la generacion del archivo de Saldos para Precalificacion"
            #Se manda a generar el archivo de movimientos adelantados
            CALL fn_genera_archivo() RETURNING v_resultado_gen
            IF v_resultado_gen = 0 THEN
               # Finaliza la operacion de generacion del archivo
               CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
               RETURNING r_resultado_opera
               IF(r_resultado_opera <> 0)THEN         
                  # Actualiza a estado erróneo
                  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                  RETURNING r_resultado_opera
               ELSE
                  CALL fn_cifras_control()
               END IF 
            ELSE
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                  RETURNING r_resultado_opera
            END IF
         ELSE
            DISPLAY "Ocurrio un ERROR al intentar crear la tabla de saldos"
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            
            CALL fn_actualiza_error()
         END IF
      END IF
   END IF
END MAIN

#Funcion principal que genera el archivo
PRIVATE FUNCTION fn_genera_archivo()

   DEFINE v_consulta_detalle                    STRING
   DEFINE v_modulo_actual                       CHAR(3)
   DEFINE ind_infonavit                         CHAR(1)

   DEFINE v_registro                            STRING

   LET v_consulta_detalle =  "SELECT ",
                                 "nss, ",
                                 "f_proceso, ",
                                 "rfc, ",
                                 "curp, ",
                                 "ap_paterno, ",
                                 "ap_materno, ",
                                 "nombre, ",
                                 "pesos_92, ",
                                 "pesos_97, ",
                                 "f_valor, ",
                                 "afore ",
                              "FROM cta_saldo_preca ",
                              "WHERE ind_infonavit = ?"
   PREPARE exe_consulta_detalle FROM v_consulta_detalle
   DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle

   INITIALIZE v_modulo_actual    TO NULL
   INITIALIZE v_nom_archivo      TO NULL
   INITIALIZE v_ruta_archivo     TO NULL
   INITIALIZE v_archivo          TO NULL

   #Se crea el archivo
   CALL crea_archivo()

   #Inicia el barrido de registros IMSS
   LET ind_infonavit = '0'
   FOREACH cur_consulta_detalle  USING ind_infonavit INTO v_detalle.*
      INITIALIZE v_registro TO NULL
      IF v_detalle.nss IS NOT NULL THEN
         LET v_registro =  v_detalle.nss,
                           v_detalle.f_proceso USING 'yyyymmdd',
                           v_detalle.rfc,
                           v_detalle.curp,
                           v_detalle.ap_paterno,
                           v_detalle.ap_materno,
                           v_detalle.nombre,
                           v_detalle.pesos_92,
                           v_detalle.pesos_97,
                           v_detalle.f_valor  USING 'yyyymmdd',
                           v_detalle.afore
                        
         #se escribe el registro en el archivo                   
         CALL v_archivo.write([v_registro])
      END IF
   END FOREACH

   #Inicia el barrido de registros SOLO INFONAVIT
   LET ind_infonavit = '1'
   FOREACH cur_consulta_detalle  USING ind_infonavit INTO v_detalle.*
      INITIALIZE v_registro TO NULL
      IF v_detalle.nss IS NOT NULL THEN
         LET v_registro =  v_detalle.nss,
                           v_detalle.f_proceso USING 'yyyymmdd',
                           v_detalle.rfc,
                           v_detalle.curp,
                           v_detalle.ap_paterno,
                           v_detalle.ap_materno,
                           v_detalle.nombre,
                           v_detalle.pesos_92,
                           v_detalle.pesos_97,
                           v_detalle.f_valor  USING 'yyyymmdd',
                           v_detalle.afore
                        
         #se escribe el registro en el archivo                   
         CALL v_archivo.write([v_registro])
      END IF
   END FOREACH

   #Se valida que no exista un archivo abierto
   IF v_archivo IS NOT NULL THEN
      CALL finaliza_archivo()
   END IF

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion de los archivos de adelantos: "
   DISPLAY ""
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY ""
   DISPLAY "*******************************************************************"
   
   RETURN 0
END FUNCTION

#Funcion que genera el archivo para el modulo 
PRIVATE FUNCTION crea_archivo()

   #Se asigna el nombre del archivo
   LET v_nom_archivo = "SALDO_SAFRE_", v_fcorte USING 'dd-mm-yyyy' ,".preca" 

   #Se crea el nombre del archivo con la ruta fisica en el servidor
   LET v_ruta_archivo = v_ruta_envio CLIPPED || "/" || v_nom_archivo CLIPPED
   
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Se crea el archivo: "
   DISPLAY v_ruta_archivo
   DISPLAY ""
   DISPLAY "*******************************************************************"

   -- se crea el manejador de archivo
   LET v_archivo = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo.openFile(v_ruta_archivo, "w" )
   CALL v_archivo.setDelimiter("")

END FUNCTION

PRIVATE FUNCTION finaliza_archivo()
   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Finalizando la generacion del archivo: "
   DISPLAY ""
   DISPLAY " ARCHIVO            : ",v_ruta_archivo
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY ""
   DISPLAY "*******************************************************************"

   #se cierran los archivos
   CALL v_archivo.close()

   #Se inicializan las variables para el manejo del archivo
   INITIALIZE v_nom_archivo      TO NULL
   INITIALIZE v_ruta_archivo     TO NULL
   INITIALIZE v_archivo          TO NULL
END FUNCTION

PRIVATE FUNCTION fn_actualiza_error()
   UPDATE safre_sdo@vivws_tcp:glo_ctr_saldo SET estado_genera = 3 
   WHERE tpo_saldo = 3
   AND f_saldo = v_fcorte;
END FUNCTION


FUNCTION fn_cifras_control()

   DEFINE preview   SMALLINT
   DEFINE i         INTEGER
   DEFINE vhandler   om.SaxDocumentHandler

   DEFINE v_rec_cifras  RECORD
      ind_infonavit    CHAR(1),
      f_proceso        DATE,
      f_valor          DATE,
      pesos_92         DECIMAL(22,2),
      aivs_92          DECIMAL(26,6),
      pesos_97         DECIMAL(22,2),
      aivs_97          DECIMAL(26,6),
      reg_total        DECIMAL(12,0)
   END RECORD

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

   LET vhandler = fn_configuracion( "CTAP031.4rp", "PDF", preview )

   DECLARE cur_cifras CURSOR FOR SELECT ind_infonavit,
                                        f_proceso,
                                        f_valor,
                                        SUM(pesos_92),
                                        SUM(aivs_92),
                                        SUM(pesos_97),
                                        SUM(aivs_97),
                                        COUNT(*)
                                   FROM cta_saldo_preca
                                  GROUP BY 1,2,3
                                  ORDER BY 1 
   
   START REPORT rep_cifras TO XML HANDLER vhandler
      FOREACH cur_cifras INTO v_rec_cifras.*
         OUTPUT TO REPORT rep_cifras (v_rec_cifras.*)
      END FOREACH
   FINISH REPORT rep_cifras
   
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion del archivo de cifras control, ", v_ruta_listado
   DISPLAY ""
   DISPLAY "*******************************************************************"
 
END FUNCTION
  
---------------------------------------------------------------------------
FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
---------------------------------------------------------------------------

  DEFINE 
    v_reporte                STRING,
    v_formato                STRING,
    v_preview                INTEGER

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'cta'

   LET v_ruta_listado = v_ruta_listados CLIPPED , "/" ,
                        p_usuario_cod CLIPPED , "-", -- usuario
                        "CTAP03" CLIPPED, "-", -- programa
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
      ind_infonavit    CHAR(1),
      f_proceso        DATE,
      f_valor          DATE,
      pesos_92         DECIMAL(22,2),
      aivs_92          DECIMAL(26,6),
      pesos_97         DECIMAL(22,2),
      aivs_97          DECIMAL(26,6),
      registros        DECIMAL(10,0)
   END RECORD

   DEFINE v_fecha               DATE
   DEFINE v_tipo_trabajador     STRING
   
   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY
      PRINTX p_rec_cifras.f_proceso USING "dd-mm-yyyy"
      PRINTX p_rec_cifras.f_valor USING "dd-mm-yyyy"
      PRINTX v_fecha USING "dd-mm-yyyy"
  
  ON EVERY ROW

   CASE p_rec_cifras.ind_infonavit
       WHEN "0" LET v_tipo_trabajador = "IMSS" 
       WHEN "1" LET v_tipo_trabajador = "SOLO INFONAVIT"
   END CASE
   PRINTX v_tipo_trabajador,
          p_rec_cifras.pesos_92,
          p_rec_cifras.aivs_92,
          p_rec_cifras.pesos_97,
          p_rec_cifras.aivs_97,
          p_rec_cifras.registros
END REPORT
