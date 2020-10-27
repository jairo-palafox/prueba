################################################################################
#Modulo       => CBD                                                           #
#Programa     => CBDS08                                                        #
#Objetivo     => Programa que genera los archivos especiales de                # 
#                movimientos adelantados separados por modulo operativo        #
#Fecha inicio => 17/04/2015                                                    #
################################################################################
DATABASE safre_viv

GLOBALS "CBDS08.inc"

##Parametros generales del proceso
PRIVATE DEFINE p_pid                      LIKE bat_ctr_operacion.pid                  -- PID del proceso
PRIVATE DEFINE p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod          -- codigo del proceso
PRIVATE DEFINE p_opera_cod                LIKE bat_ctr_operacion.opera_cod            -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod                -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo         -- nombre dle archivo
PRIVATE DEFINE p_fcorte                   DATE

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_detalle_monitoreo        STRING
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados
PRIVATE DEFINE v_ruta_envio               LIKE seg_modulo.ruta_envio

#Variables para la generacion del archivo
PRIVATE DEFINE v_folio                    LIKE glo_ctr_archivo.folio
PRIVATE DEFINE v_detalle                  detalle

#Variable para el manejo del archivo
PRIVATE DEFINE v_nom_archivo              STRING
PRIVATE DEFINE v_ruta_archivo             STRING
PRIVATE DEFINE v_archivo                  BASE.CHANNEL

MAIN
   DEFINE r_resultado_opera            INTEGER
   DEFINE v_resultado_gen              INTEGER

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_nombre_archivo = ARG_VAL(5)
   LET p_fcorte         = ARG_VAL(6)

   WHENEVER ERROR CONTINUE

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACIÓN          : ",v_opera_desc,"\n",
                             " FECHA              : ",TODAY USING 'dd-mm-yyyy',"\n",
                             " HORA               : ",TIME(CURRENT)," "
   DISPLAY v_detalle_monitoreo;
   DISPLAY "*******************************************************************"
   -- se solicita el numero de folio asociado a la operacion
   -- parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
   
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
      END IF 
   ELSE
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
   END IF
   
   WHENEVER ERROR STOP
   
END MAIN

#Funcion principal que genera el archivo
PRIVATE FUNCTION fn_genera_archivo()

   DEFINE v_consulta_adelanto                   STRING
   DEFINE v_modulo_actual                       CHAR(3)

   DEFINE v_registro                            STRING

   DISPLAY "Se inicia la generacion del archivo"

   LET v_consulta_adelanto =  "SELECT ",
                                 "nss, ",
                                 "f_liquida, ",
                                 "modulo, ",
                                 "subcuenta, ",
                                 "monto_acciones ",
                              "FROM cbd_movimiento_adelanto ",
                              "WHERE subcuenta IN (4,8) ",
                              "AND f_liquida <= ?",
                              "ORDER BY modulo,f_liquida"
   PREPARE exe_consulta_adelanto FROM v_consulta_adelanto
   DECLARE cur_consulta_adelanto CURSOR FOR exe_consulta_adelanto

   INITIALIZE v_modulo_actual    TO NULL
   INITIALIZE v_nom_archivo      TO NULL
   INITIALIZE v_ruta_archivo     TO NULL
   INITIALIZE v_archivo          TO NULL

   #Inicia el barrido de registros para la generacion de los archivos
   FOREACH cur_consulta_adelanto USING p_fcorte INTO v_detalle.*
   
      INITIALIZE v_registro TO NULL
      #Se valida si el modulo de la consulta es distinto a v_modulo_actual para
      #ver si se requiere crear un nuevo archivo
      IF v_modulo_actual IS NULL OR v_detalle.modulo <> v_modulo_actual THEN
         DISPLAY "Nuevo Archivo ", v_detalle.modulo
         CALL crea_archivo(v_detalle.modulo)
      END IF
      LET v_modulo_actual = v_detalle.modulo

      LET v_registro = v_detalle.nss,
                        v_detalle.f_liquidacion USING 'yyyymmdd',
                        v_detalle.modulo,
                        v_detalle.subcuenta,
                        v_detalle.aivs
                        
      #se escribe el registro en el archivo                   
      CALL v_archivo.write([v_registro])
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
   DISPLAY " Los archivos de salida se encuentran en la siguiente ruta: "
   DISPLAY ""
   DISPLAY v_ruta_envio CLIPPED, "/", p_nombre_archivo CLIPPED, ".*"
   DISPLAY ""
   DISPLAY "*******************************************************************"
   
   RETURN 0
END FUNCTION

#Funcion que genera el archivo para el modulo 
PRIVATE FUNCTION crea_archivo(p_modulo)
   DEFINE p_modulo            CHAR(3)

   #Se valida que no exista un archivo abierto
   IF v_archivo IS NOT NULL THEN
      DISPLAY "cerrando archivo..."
      CALL finaliza_archivo()
   END IF

   #Se asigna el nombre del archivo
   LET v_nom_archivo = p_nombre_archivo CLIPPED ,".",p_modulo CLIPPED 

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