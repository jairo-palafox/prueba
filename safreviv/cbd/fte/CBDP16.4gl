################################################################################
#Modulo       => CBD                                                           #
#Programa     => CBDS04                                                        #
#Objetivo     => Programa que genera los archivos con el historico de          # 
#                movimientos adelantados separados por modulo operativo        #
#Fecha inicio => 15/07/2014                                                    #
################################################################################
DATABASE safre_viv

GLOBALS "CBDP16.inc"

##Parametros generales del proceso
PRIVATE DEFINE p_pid                      LIKE bat_ctr_operacion.pid                  -- PID del proceso
PRIVATE DEFINE p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod          -- codigo del proceso
PRIVATE DEFINE p_opera_cod                LIKE bat_ctr_operacion.opera_cod            -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod                -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo         -- nombre dle archivo

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
PRIVATE DEFINE v_detalle_dis              detalle_dis
PRIVATE DEFINE v_detalle_pag              detalle_pag

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
   LET p_nombre_archivo    = ARG_VAL(5)

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
                              "WHERE subcuenta IN (4,8,55) ",
                              "AND modulo NOT IN ('dis','pag') ",       #Se omite el modulo de dispersion porque tiene estructura diferente
                              "ORDER BY modulo,f_liquida"
   PREPARE exe_consulta_adelanto FROM v_consulta_adelanto
   DECLARE cur_consulta_adelanto CURSOR FOR exe_consulta_adelanto

   INITIALIZE v_modulo_actual    TO NULL
   INITIALIZE v_nom_archivo      TO NULL
   INITIALIZE v_ruta_archivo     TO NULL
   INITIALIZE v_archivo          TO NULL

   #Inicia el barrido de registros para la generacion de los archivos
   FOREACH cur_consulta_adelanto INTO v_detalle.*
   
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

   #Se genera el archivo de dispersion
   CALL fn_genera_archivo_dispersion()

   #Se genera el archivo de registro de pagos
   CALL fn_genera_archivo_pagos()

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

PRIVATE FUNCTION fn_genera_archivo_dispersion()
   DEFINE v_query          STRING
   DEFINE v_registro       STRING
   DEFINE v_origen         CHAR(2)

   LET v_query =  "SELECT ",
                     "mov.nss, ",
                     "mov.f_liquida, ",
                     "mov.modulo, ",
                     "mov.subcuenta, ",
                     "mov.monto_acciones, ",
                     "dis.folio_sua, ",
                     "dis.periodo_pago, ",
                     "dis.proceso_pag ",
                  "FROM cbd_movimiento_adelanto mov ",
                  "INNER JOIN cbd_adelanto_dis dis ON (dis.id_derechohabiente = mov.id_derechohabiente ",
                                        "AND dis.subcuenta = mov.subcuenta ",
                                        "AND dis.movimiento = mov.movimiento ",
                                        "AND dis.id_referencia = mov.id_referencia) ",
                  "WHERE mov.subcuenta IN (4,8,55) ",
                  "AND mov.modulo = 'dis' ",
                  "ORDER BY mov.f_liquida "
   PREPARE exe_consulta_dis FROM v_query
   DECLARE cur_consulta_dis CURSOR FOR exe_consulta_dis
   
   DISPLAY "Nuevo Archivo dis"
   CALL crea_archivo('dis')

   #Inicia el barrido de registros para la generacion de los archivos
   FOREACH cur_consulta_dis INTO v_detalle_dis.*
   
      INITIALIZE v_registro TO NULL
      #Se valida el tipo de proceso que origino el adelanto
      IF v_detalle_dis.proceso_pag = 101 
      OR v_detalle_dis.proceso_pag = 102
      OR v_detalle_dis.proceso_pag = 103
      OR v_detalle_dis.proceso_pag = 107
      OR v_detalle_dis.proceso_pag = 110 THEN
         LET v_origen = 'ac'
      ELSE
         LET v_origen = 'as'
      END IF
      

      LET v_registro = v_detalle_dis.nss,
                        v_detalle_dis.f_liquidacion USING 'yyyymmdd',
                        v_detalle_dis.modulo,
                        v_detalle_dis.subcuenta,
                        v_detalle_dis.aivs,
                        '0000',
                        v_detalle_dis.folio_sua,
                        v_detalle_dis.periodo_pago,
                        v_origen
                        
      #se escribe el registro en el archivo                   
      CALL v_archivo.write([v_registro])
   END FOREACH
   
END FUNCTION

PRIVATE FUNCTION fn_genera_archivo_pagos()
   DEFINE v_query          STRING
   DEFINE v_registro       STRING
   DEFINE v_tipo          CHAR(1)

   LET v_query =  "SELECT ",
                  "mov.nss, ",
                  "mov.f_liquida, ",
                  "mov.modulo, ",
                  "mov.subcuenta, ",
                  "mov.monto_acciones, ",
                  "pag.nrp, ",
                  "pag.f_pago, ",
                  "pag.periodo_pago, ",
                  "pag.folio_sua, ",
                  "pag.imp_ap_pat, ",
                  "pag.aiv_ap_pat, ",
                  "pag.imp_am_cre, ",
                  "pag.folio, ",
                  "pag.tpo_aclaracion, ",
                  "pag.int_gen_pgo_ext ",
                  "FROM cbd_movimiento_adelanto mov ",
                  "INNER JOIN cta_his_pagos pag ON (pag.folio = mov.folio_liquida ",
                                       "AND pag.id_referencia = mov.id_referencia ",
                                       "AND pag.origen_archivo = 1 ",
                                       "AND pag.ind_liquidacion = 3) ",
                  "WHERE mov.modulo = 'pag' ",
                  "ORDER BY mov.f_liquida "
   PREPARE exe_consulta_pag FROM v_query
   DECLARE cur_consulta_pag CURSOR FOR exe_consulta_pag
   
   DISPLAY "Nuevo Archivo pag"
   CALL crea_archivo('pag')

   #Inicia el barrido de registros para la generacion de los archivos
   FOREACH cur_consulta_pag INTO v_detalle_pag.*
   
      INITIALIZE v_registro TO NULL
      
      IF v_detalle_pag.int_gen_pgo_ext IS NOT NULL AND v_detalle_pag.int_gen_pgo_ext > 0 THEN
         LET v_tipo = 'I'
      ELSE
         LET v_tipo = 'A'
      END IF

      LET v_registro = v_detalle_pag.nss,
                        v_detalle_pag.f_liquidacion USING 'yyyymmdd',
                        v_detalle_pag.modulo,
                        v_detalle_pag.subcuenta,
                        v_detalle_pag.aivs,
                        '0000',
                        v_detalle_pag.nrp,
                        v_detalle_pag.f_pago USING 'yyyymmdd',
                        v_detalle_pag.periodo_pago,
                        v_detalle_pag.folio_sua USING '######',
                        v_detalle_pag.imp_ap_pat,
                        v_detalle_pag.aiv_ap_pat,
                        v_detalle_pag.imp_am_cre,
                        v_detalle_pag.folio USING '#####&',
                        v_detalle_pag.tpo_aclaracion USING '###',
                        v_tipo
                        
      #se escribe el registro en el archivo                   
      CALL v_archivo.write([v_registro])
   END FOREACH
   
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