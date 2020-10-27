#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDS02                                                                 #
#Objetivo     => Programa que genera el archivo de SIAFF                                #
#Fecha inicio => 19/06/2012                                                             #
#Modificacion:                                                                          #
# 31Oct2013 Ivan Vega. INFONAVIT solicito que se hicieran solo 2 pagos. El primero      #
#                      Contemplara los NSS que finalizan en 0,1,2,3,4                   #
#                      El segundo pago contemplara los NSS que terminan en 5,6,7,8,9    #
#                                                                                       #
#18Jun2014 Jaime Galeno. INFONAVIT solicito que se generara un tercer pago con todos los#
#                        registros que se encuentres pendientes despues de finalizar los#
#                        dos pagos originales
#########################################################################################
DATABASE safre_viv

GLOBALS "RETP02.inc"

##Parametros generales del proceso
PRIVATE DEFINE p_pid                      LIKE bat_ctr_operacion.pid                  -- PID del proceso
PRIVATE DEFINE p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod          -- codigo del proceso
PRIVATE DEFINE p_opera_cod                LIKE bat_ctr_operacion.opera_cod            -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod                -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo         -- nombre dle archivo
PRIVATE DEFINE v_folio                    LIKE glo_ctr_archivo.folio

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_detalle_monitoreo        STRING
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados

#Variables para la generacion del archivo
PRIVATE DEFINE v_encabezado               encabezado
PRIVATE DEFINE v_detalle                  detalle
PRIVATE DEFINE v_sumario                  sumario

PRIVATE DEFINE v_fpago                    DATE
PRIVATE DEFINE v_fPresentacion            DATE
PRIVATE DEFINE v_numPago                  SMALLINT
PRIVATE DEFINE v_num_secuencia            INTEGER
PRIVATE DEFINE v_referecia_numerica       INTEGER

#Variables para el manejo de los Nombres de archivos
PRIVATE DEFINE v_nom_archivo              STRING

#Variables para las rutas fisicas
PRIVATE DEFINE v_ruta                     STRING

#Variables donde se guardaran los apuntadores a los archivos
PRIVATE DEFINE v_archivo                  BASE.CHANNEL

#Variables para el manejo del registro contable
PRIVATE DEFINE v_proceso_cnt              INTEGER
PRIVATE DEFINE v_transaccion_cnt          INTEGER
PRIVATE DEFINE v_result_cnt               INTEGER
PRIVATE DEFINE v_cuenta_contable          INTEGER

MAIN
   DEFINE r_resultado_opera            INTEGER
   DEFINE v_resultado_gen              INTEGER

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

   #Se manda a generar el archivo
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

PRIVATE FUNCTION fn_genera_archivo()
   
   #Variables para el manejo de las consultas
   DEFINE v_consulta_pago                      STRING
   DEFINE v_consulta_detalle                   STRING
   DEFINE v_consulta_reenvio                   STRING
   DEFINE v_inserta                            STRING
   DEFINE v_liquida                            STRING
   DEFINE v_contabilidad                       STRING

   DEFINE v_id_fondo72                          DECIMAL(9,0)
   DEFINE v_id_solicitud                        DECIMAL(9,0)
   DEFINE v_monto                               DECIMAL(13,2)
   DEFINE v_cargo                               DECIMAL(13,2)
   DEFINE v_num_cuenta                          CHAR(18)
   DEFINE v_estado_pago                         CHAR(1)
   DEFINE v_fProceso                            DATE
   DEFINE v_hProceso                            DATETIME HOUR TO SECOND
   DEFINE v_origen                              CHAR(20)

   DEFINE v_monto_total                         DECIMAL(16,2)
   DEFINE v_rechazo                             SMALLINT
   
   DISPLAY "Se inicia la generacion del archivo"

   #Se preparan las consultas

   #Consulta las fechas del siguiente pago
   LET v_consulta_pago =   "SELECT FIRST 1 ",
                           "num_pago, ",
                           "f_pago, ",
                           "f_presentacion ",
                           "FROM ret_ctr_pago_masivo ",
                           "WHERE estado_pago = 1 ",
                           "ORDER BY num_pago "
   PREPARE exe_consulta_pago FROM v_consulta_pago

   LET v_inserta = "INSERT INTO ret_detalle_spei VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_inserta FROM v_inserta

   LET v_liquida = "INSERT INTO cta_fondo72 VALUES(?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_liquida FROM v_liquida

   LET v_contabilidad = "EXECUTE PROCEDURE fn_ret_fnd_ah_cnt57(?,?,?,?,?)"
   PREPARE exe_contabilidad FROM v_contabilidad

-- 31 OCTUBRE 2013.
-- ESTE CODIGO SE COMENTA Y SE COPIA A DONDE SE VA RECORRIENDO LOS PAGOS PUES
-- SE PARAMETRIZARA DIFERENTE
{   
   LET v_consulta_detalle =   "SELECT ",
                              "ret.id_solicitud, ",
                              "ret.id_afi_fondo72, ",
                              "cve.banco, ",
                              "ret.importe_viv72, ",
                              "ret.clabe, ",
                              "cve.nom_titular, ",
                              "ret.nss ",
                              "FROM ret_fondo_ahorro_masivo ret ",
                              "INNER JOIN ret_cta_clabe cve ON (cve.nss = ret.nss AND cve.clabe = ret.clabe) ",
                              "WHERE ret.estado_solicitud = 65 ",
                              "AND ret.nss[11] = ? ",
                              "ORDER BY ret.importe_viv72 DESC"
   PREPARE exe_consulta_detalle FROM v_consulta_detalle
   DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle
}
   LET v_consulta_reenvio =   "SELECT ",
                              "ret.id_solicitud, ",
                              "ret.id_afi_fondo72, ",
                              "cve.banco, ",
                              "ret.importe_viv72, ",
                              "ret.clabe, ",
                              "cve.nom_titular, ",
                              "afi.nss ",
                              "FROM ret_fondo_ahorro_masivo ret ",
                              "INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72 ",
                              "INNER JOIN ret_cta_clabe cve ON (cve.nss = ret.nss AND cve.clabe = ret.clabe) ",
                              "WHERE ret.estado_solicitud = 66 ",
                              "ORDER BY ret.importe_viv72 DESC"
   PREPARE exe_consulta_reenvio FROM v_consulta_reenvio
   DECLARE cur_consulta_reenvio CURSOR FOR exe_consulta_reenvio
                              
   #Se inicializan las variables del detalle
   CALL fn_inicializa_registros()

   DISPLAY ""
   DISPLAY "Inicia la generacion de los detalles del archivo"
   DISPLAY ""

   #Se consulta el pago a realizar
   EXECUTE exe_consulta_pago INTO   v_numPago,
                                    v_fpago,
                                    v_fPresentacion

   -- VALIDA QUE NO SE HAGAN MAS PAGOS QUE LOS PROGRAMADOS, EN ESTE CASO 3
   IF v_numPago IS NULL OR v_numPago <= 0 OR v_numPago > 3 THEN
      DISPLAY "ERROR: Numero de pago invalido"
      RETURN 1
   END IF
{-- ya no se requiere. 31Oct2013
   IF v_numPago = 10 THEN
      LET v_numPago = 0
   END IF
}
   #Se crean los archivos
   CALL fn_crea_archivos()

   #Generacion del encabezado
   #Requerimiento PRODINF-263 cambiar el numero de bloque de &&81001 a &&21301
   LET v_encabezado.num_bloque = DAY(v_fPresentacion) USING '&&', '21301'
   LET v_encabezado.f_presentacion = v_fPresentacion USING 'yyyymmdd'
   CALL fn_inserta_encabezado()

   #Generacion de los detalles

   LET v_monto_total = 0
   LET v_referecia_numerica = 0
   LET v_rechazo = NULL
   LET v_estado_pago = NULL
   LET v_fProceso = TODAY
   LET v_origen = 'RETIRO MASIVO'
   
   -- se prepara la consulta parametrizando segun el numero de pago
   -- MODIFICADO. ISVH. 31Oct2013
   LET v_consulta_detalle = "\n SELECT ",
                            "\n ret.id_solicitud, ",
                            "\n ret.id_afi_fondo72, ",
                            "\n cve.banco, ",
                            "\n ret.importe_viv72, ",
                            "\n ret.clabe, ",
                            "\n cve.nom_titular, ",
                            "\n ret.nss ",
                            "\n FROM ret_fondo_ahorro_masivo ret ",
                            "\n INNER JOIN ret_cta_clabe cve ON (cve.nss = ret.nss AND cve.clabe = ret.clabe) ",
                            "\n WHERE ret.estado_solicitud = 65 "
                            -- 31Oct2013. Ahora se haran 2 grupos,
                            -- el primero contiene los NSS que terminan en 0,1,2,3,4
                            -- el segundo contiene los NSS que terminan en 5,6,7,8,9
                            -- ORIGINAL
                            --"AND ret.nss[11] = ? ",
   -- si es el grupo 1
   IF ( v_numPago = 1 ) THEN
      LET v_consulta_detalle = v_consulta_detalle, "\n AND ret.nss[11] between 0 and 4 " -- grupo 1 de pagos
   ELSE
      -- grupo 2
      LET v_consulta_detalle = v_consulta_detalle, "\n AND ret.nss[11] between 5 and 9 " -- grupo 2 de pagos
   END IF
   
   -- se concatena la agrupacion
   -- MODIFICADO
   LET v_consulta_detalle = v_consulta_detalle, "\n ORDER BY ret.importe_viv72 DESC"
   
   -- se prepara y ejecuta la consulta de detalle
   PREPARE exe_consulta_detalle FROM v_consulta_detalle
   DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle

   
   #Se ejecuta la consulta de trabajadores
   -- ORIGINAL 31Oct2013
   {
   FOREACH cur_consulta_detalle USING v_numPago
                                 INTO v_id_solicitud,
                                       v_id_fondo72,
                                       v_detalle.banco_receptor,
                                       v_monto,
                                       v_num_cuenta,
                                       v_detalle.nombre_receptor,
                                       v_detalle.rfc_receptor
   }
   -- MODIFICADO ISVH 31Oct2013. YA NO SE NECESITA EL NUMERO DE PAGO

   #Se modifica porque para el tercer pago solo se consideran los registros de reenvio
   IF v_numPago < 3 THEN
      FOREACH cur_consulta_detalle INTO v_id_solicitud,
                                        v_id_fondo72,
                                        v_detalle.banco_receptor,
                                        v_monto,
                                        v_num_cuenta,
                                        v_detalle.nombre_receptor,
                                        v_detalle.rfc_receptor

      
         LET v_referecia_numerica = v_referecia_numerica + 1
         LET v_num_secuencia = v_num_secuencia + 1

         #Se agrega la mascara para el NSS
         LET v_detalle.rfc_receptor = v_detalle.rfc_receptor USING '&&&&&&&&&&&&&&&&&&'
            
         #Se asigna el numero de secuencia
         LET v_detalle.num_secuencua = v_num_secuencia USING '&&&&&&&'
         
         #Se asigna la fecha de transferencia
         LET v_detalle.f_transferencia = v_fpago USING 'yyyymmdd'

         #Se asigna el importe a pagar, se miltiplica por 200 para eliminar el punto decimal y duplicar el monto por lo del Tanto Adicional
         LET v_detalle.importe_operacion = (v_monto * 200) USING '&&&&&&&&&&&&&&&'
         LET v_monto_total = v_monto_total + (v_monto * 2)

         #Se asigna la fecha de aplicacion
         LET v_detalle.f_aplicacion = v_fpago USING 'yyyymmdd'

         #Numero de cuenta
         LET v_detalle.num_cuenta_receptor = '00', v_num_cuenta

         #Referencia numerica
         LET v_detalle.ref_numerica_ordenante =  v_referecia_numerica USING '&&&&&&&'

         #Fecha de presentacion
         LET v_detalle.f_presentacion_ini = v_fPresentacion USING 'yyyymmdd'

         #Clave de rastreo
         CALL fn_genera_clave() RETURNING v_detalle.clave_rastreo
         
         #Se inserta el registro en el archivo correspondiente
         CALL fn_inserta_detalle()

         #Se inserta el cargo en la tabla de movimientos 
         LET v_cargo = v_monto * -1
         LET v_hProceso = CURRENT HOUR TO SECOND;
         EXECUTE exe_liquida USING v_id_fondo72,
                                    v_fpago,
                                    SUBCTA_FONDO72,
                                    MOVTO_CARGO_FONDO72,
                                    v_folio,
                                    v_id_solicitud,
                                    v_cargo,
                                    v_estado_pago,
                                    v_fProceso,
                                    v_hProceso,
                                    v_origen

         #Se inserta el tanto adicional
         EXECUTE exe_liquida USING v_id_fondo72,
                                    v_fpago,
                                    SUBCTA_FONDO72,
                                    MOVTO_TANTO_ADICIONAL,
                                    v_folio,
                                    v_id_solicitud,
                                    v_cargo,
                                    v_estado_pago,
                                    v_fProceso,
                                    v_hProceso,
                                    v_origen
                                    

         #Actualiza el estado de la solicitud
         UPDATE ret_fondo_ahorro_masivo SET estado_solicitud = RET_ESTADO_PAGADA
         WHERE id_solicitud = v_id_solicitud

         #Se inserta en la tabla de control para los detalles
         EXECUTE exe_inserta USING  v_folio,                   #folio
                                    v_id_solicitud,            #id_solicitud
                                    v_id_fondo72,              #id_afi_fondo72
                                    v_numPago,                 #num_pago
                                    v_num_secuencia,           #num_secuencial
                                    v_fpago,                   #f_transferencia
                                    v_monto,                   #saldo
                                    v_fpago,                   #f_aplicacion
                                    v_detalle.num_cuenta_receptor,              #clabe
                                    v_detalle.nombre_receptor, #nombre_beneficiario
                                    v_detalle.rfc_receptor,    #rfc_curp
                                    v_referecia_numerica,      #referencia
                                    v_detalle.clave_rastreo,   #cve_rastreo
                                    RET_ESTADO_PAGADA,         #cod_estado_pago
                                    v_rechazo

         #Se inicializa el detalle
         CALL fn_inicializa_detalle()
      END FOREACH #FIN cur_consulta_detalle
   END IF

   #Se ejecuta la consulta reenvios
   CALL fn_inicializa_detalle()
   FOREACH cur_consulta_reenvio INTO v_id_solicitud,
                                     v_id_fondo72,
                                     v_detalle.banco_receptor,
                                     v_monto,
                                     v_num_cuenta,
                                     v_detalle.nombre_receptor,
                                     v_detalle.rfc_receptor
   
      LET v_referecia_numerica = v_referecia_numerica + 1
      LET v_num_secuencia = v_num_secuencia + 1

      #Se agrega la mascara para el NSS
      LET v_detalle.rfc_receptor = v_detalle.rfc_receptor USING '&&&&&&&&&&&&&&&&&&'
         
      #Se asigna el numero de secuencia
      LET v_detalle.num_secuencua = v_num_secuencia USING '&&&&&&&'
      
      #Se asigna la fecha de transferencia
      LET v_detalle.f_transferencia = v_fpago USING 'yyyymmdd'

      #Se asigna el importe a pagar, se miltiplica por 200 para eliminar el punto decimal y duplicar el monto por lo del Tanto Adicional
      LET v_detalle.importe_operacion = (v_monto * 200) USING '&&&&&&&&&&&&&&&'
      LET v_monto_total = v_monto_total + (v_monto * 2)

      #Se asigna la fecha de aplicacion
      LET v_detalle.f_aplicacion = v_fpago USING 'yyyymmdd'

      #Numero de cuenta
      LET v_detalle.num_cuenta_receptor = '00', v_num_cuenta

      #Referencia numerica
      LET v_detalle.ref_numerica_ordenante =  v_referecia_numerica USING '&&&&&&&'

      #Fecha de presentacion
      LET v_detalle.f_presentacion_ini = v_fPresentacion USING 'yyyymmdd'

      #Clave de rastreo
      CALL fn_genera_clave() RETURNING v_detalle.clave_rastreo
      
      #Se inserta el registro en el archivo correspondiente
      CALL fn_inserta_detalle()

      #Se inserta el cargo en la tabla de movimientos 
      LET v_cargo = v_monto * -1
      LET v_hProceso = CURRENT HOUR TO SECOND;
      EXECUTE exe_liquida USING v_id_fondo72,
                                 v_fpago,
                                 SUBCTA_FONDO72,
                                 MOVTO_CARGO_FONDO72,
                                 v_folio,
                                 v_id_solicitud,
                                 v_cargo,
                                 v_estado_pago,
                                 v_fProceso,
                                 v_hProceso,
                                 v_origen

      #Se inserta el tanto adicional
      EXECUTE exe_liquida USING v_id_fondo72,
                                 v_fpago,
                                 SUBCTA_FONDO72,
                                 MOVTO_TANTO_ADICIONAL,
                                 v_folio,
                                 v_id_solicitud,
                                 v_cargo,
                                 v_estado_pago,
                                 v_fProceso,
                                 v_hProceso,
                                 v_origen
                                 

      #Actualiza el estado de la solicitud
      UPDATE ret_fondo_ahorro_masivo SET estado_solicitud = RET_ESTADO_PAGADA
      WHERE id_solicitud = v_id_solicitud

      #Se inserta en la tabla de control para los detalles
      EXECUTE exe_inserta USING  v_folio,                   #folio
                                 v_id_solicitud,            #id_solicitud
                                 v_id_fondo72,              #id_afi_fondo72
                                 v_numPago,                 #num_pago
                                 v_num_secuencia,           #num_secuencial
                                 v_fpago,                   #f_transferencia
                                 v_monto,                   #saldo
                                 v_fpago,                   #f_aplicacion
                                 v_detalle.num_cuenta_receptor,              #clabe
                                 v_detalle.nombre_receptor, #nombre_beneficiario
                                 v_detalle.rfc_receptor,    #rfc_curp
                                 v_referecia_numerica,      #referencia
                                 v_detalle.clave_rastreo,   #cve_rastreo
                                 RET_ESTADO_PAGADA,         #cod_estado_pago
                                 v_rechazo

      #Se inicializa el detalle
      CALL fn_inicializa_detalle()
   END FOREACH #FIN cur_consulta_reenvio

   #Genera el sumario
   LET v_num_secuencia           = v_num_secuencia + 1
   LET v_sumario.importe_total   = (v_monto_total * 100) USING '&&&&&&&&&&&&&&&&&&'
   LET v_sumario.num_operaciones = v_referecia_numerica USING '&&&&&&&'
   LET v_sumario.num_bloque      = v_encabezado.num_bloque
   LET v_sumario.num_secuencua   = v_num_secuencia USING '&&&&&&&'
   
   CALL fn_inserta_sumario()

   #Cerramos el archivo
   CALL fn_finaliza_archivos()

   #Actualizamos el estado del pago
   update ret_ctr_pago_masivo SET estado_pago = 2 where num_pago = v_numPago
   
   #Indicamos que el folio fue liquidado
   UPDATE glo_folio SET status = 2 WHERE folio = v_folio;

   #Se ejecuta el registro contable
   LET v_proceso_cnt = 70
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
      RETURN 1
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
         RETURN 1
      END IF
   ELSE
      DISPLAY "Ocurrió un error al realizar el registro contable."
      RETURN 1
   END IF

   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_inserta_encabezado()
   DEFINE v_registro                   STRING

   LET v_registro =  v_encabezado.tpo_registro,
                     v_encabezado.num_secuencua,
                     v_encabezado.cod_operacion,
                     v_encabezado.banco,
                     v_encabezado.sentido,
                     v_encabezado.servicio,
                     v_encabezado.num_bloque,
                     v_encabezado.f_presentacion,
                     v_encabezado.cod_divisa,
                     v_encabezado.causa_rechazo,
                     v_encabezado.modalidad,
                     v_encabezado.uso_futuro_cce,
                     v_encabezado.uso_futuro_banco

   #se escribe el registro en el archivo                   
   CALL v_archivo.write([v_registro])
END FUNCTION

PRIVATE FUNCTION fn_inserta_detalle()
   DEFINE v_registro                   STRING

   LET v_registro =  v_detalle.tpo_registro,
                     v_detalle.num_secuencua,
                     v_detalle.cod_operacion,
                     v_detalle.divisa,
                     v_detalle.f_transferencia,
                     v_detalle.banco_presentador,
                     v_detalle.banco_receptor,
                     v_detalle.importe_operacion,
                     v_detalle.uso_futuro_cce,
                     v_detalle.tpo_operacion,
                     v_detalle.f_aplicacion,
                     v_detalle.tpo_cuenta_ordenante,
                     v_detalle.num_cuenta_ordenante,
                     v_detalle.nombre_ordenante,
                     v_detalle.rfc_ordenante,
                     v_detalle.tpo_cuenta_receptor,
                     v_detalle.num_cuenta_receptor,
                     v_detalle.nombre_receptor,
                     v_detalle.rfc_receptor,
                     v_detalle.referencia_servicio,
                     v_detalle.nombre_titular_servicio,
                     v_detalle.importe_iva,
                     v_detalle.ref_numerica_ordenante,
                     v_detalle.ref_leyenda_ordenante,
                     v_detalle.clave_rastreo,
                     v_detalle.movto_devolucion,
                     v_detalle.f_presentacion_ini,
                     v_detalle.solicitud_confirmacion,
                     v_detalle.uso_futuro_banco

   #se escribe el registro en el archivo                   
   CALL v_archivo.write([v_registro])
END FUNCTION

PRIVATE FUNCTION fn_inserta_sumario()
   DEFINE v_registro                   STRING

   LET v_registro =  v_sumario.tpo_registro,
                     v_sumario.num_secuencua,
                     v_sumario.cod_operacion,
                     v_sumario.num_bloque,
                     v_sumario.num_operaciones,
                     v_sumario.importe_total,
                     v_sumario.uso_futuro_cce,
                     v_sumario.uso_futuro_banco

   #se escribe el registro en el archivo                   
   CALL v_archivo.write([v_registro])
END FUNCTION

PRIVATE FUNCTION fn_crea_archivos()
   DEFINE v_ruta_envio                       LIKE seg_modulo.ruta_envio

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'ret'

   #El Archivo Electrónico de Instrucciones de Pago deberá nombrarse de la siguiente manera:
   #  RR_UUU_CLC_AAAAMMDD.dat 
   #Donde:
   #RR	=	Ramo generador de la información.                                       RR. SIEMPRE ES 06
   #UUU	=	Unidad Responsable generadora de la información.                        RR. SIEMPRE ES 800
   #CLC	=	Folio de la Cuenta por Liquidar que fue asignado por SIAFF (FOLIO_CLC)  RR. ES UN CONSECUTIVO POR ARCHIVO POR DIA SIEMPRE EMPIEZA EN 1
   #AAAA	=	Año de generación de la información.
   #MM	=	Mes de generación de la información.
   #DD	=	Día de generación de la información.

   #Se asigna el nombre del archivo
   -- CAMBIO 20131101. Se agrega 09 al inicio del nombre de archivo y se modifica el valor 800 por 810
   --LET v_nom_archivo = "06_800_001_", v_fpago USING 'yyyymmdd', ".dat"
   -- se cambia nombre  archivo ya que se agrega 213 como oficina generadora, Jira 311, JGPS
   --LET v_nom_archivo = "09_06810001", v_fpago USING 'yyyymmdd', ".dat" -- respaldo anterior
   
   LET v_nom_archivo = "09_06213001", v_fpago USING 'yyyymmdd', ".dat"

   #Se crea el nombre del archivo con la ruta fisica en el servidor
   LET v_ruta = v_ruta_envio CLIPPED || "/" ||v_nom_archivo

   -- se crea el manejador de archivo
   LET v_archivo = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo.openFile(v_ruta, "w" )
   CALL v_archivo.setDelimiter("")
      
END FUNCTION

PRIVATE FUNCTION fn_finaliza_archivos()

   #se cierran los archivos
   CALL v_archivo.close()
   IF v_numPago = 0 THEN
      LET v_numPago = 10
   END IF
   --UPDATE ret_ctr_pago_masivo SET estado_pago = 2 WHERE num_pago = v_numPago

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion del archivo de saldos: "
   DISPLAY ""
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY " El archivo generado tiene nombre = ", v_ruta
   DISPLAY "*******************************************************************"

END FUNCTION

PRIVATE FUNCTION fn_inicializa_registros()
   INITIALIZE v_encabezado.*     TO NULL
   INITIALIZE v_sumario.*        TO NULL

   LET v_num_secuencia = 1

   #Valores fijos del encabezado
   LET v_encabezado.tpo_registro          = '01'
   LET v_encabezado.num_secuencua         = v_num_secuencia USING '&&&&&&&'
   LET v_encabezado.cod_operacion         = '60'    --Se regresa por pueba del sentavo
   --LET v_encabezado.cod_operacion         = '80'   -- Jira 311, JGPS
   LET v_encabezado.banco                 = '167'
   LET v_encabezado.sentido               = 'E'
   LET v_encabezado.servicio              = '2'
   LET v_encabezado.cod_divisa            = '01'
   LET v_encabezado.causa_rechazo         = '00'
   LET v_encabezado.modalidad             = '2'
   LET v_encabezado.uso_futuro_cce        = ' '
   LET v_encabezado.uso_futuro_cce        = ' '

   CALL fn_inicializa_detalle()

   #Valores fijos del sumario
   LET v_sumario.tpo_registro             = '09'
   LET v_sumario.cod_operacion            = '60'   --Se regresa por prueba del centavo
   --LET v_sumario.cod_operacion            = '80' -- Jira 311, JGPS cambio de 60 a 80
   LET v_sumario.uso_futuro_cce           = ' '
   LET v_sumario.uso_futuro_banco         = ' '
   
END FUNCTION

PRIVATE FUNCTION fn_inicializa_detalle()
   INITIALIZE v_detalle.*        TO NULL

   #Valores fijos del detalle
   LET v_detalle.tpo_registro             = '02'
   LET v_detalle.cod_operacion            = '60'   --Se regresa por prueba del centavo
   --LET v_detalle.cod_operacion            = '80' -- Jira 311, JGPS cambio de 60 a 80
   
   LET v_detalle.divisa                   = '01'
   LET v_detalle.banco_presentador        = '167'
   LET v_detalle.uso_futuro_cce           = ' '
   #LET v_detalle.tpo_operacion            = '03'       #Requerimiento PRODINF-263 Cambiar tipo operacion a '03', antes estaba con '02'
   LET v_detalle.tpo_operacion            = '02'       #Se regresa el valor a '02' por pruebas con el archivo del centavo
   LET v_detalle.tpo_cuenta_ordenante     = '40'
   LET v_detalle.num_cuenta_ordenante     = '00167180228001016719'
   LET v_detalle.nombre_ordenante         = 'TESORERIA DE LA FEDERACION'
   LET v_detalle.rfc_ordenante            = 'SHC850101U37'
   LET v_detalle.tpo_cuenta_receptor      = '40'
   LET v_detalle.referencia_servicio      = ' '
   LET v_detalle.nombre_titular_servicio  = ' '
   LET v_detalle.importe_iva              = '000000000000000'
   -- CAMBIO 20131101
   --LET v_detalle.ref_leyenda_ordenante    = 'INFONAVIT DEV FDO AHORRO 72 92'
   LET v_detalle.ref_leyenda_ordenante    = 'ENTREGA DEL FONDO DE AHORRO'
   LET v_detalle.movto_devolucion         = '00'
   LET v_detalle.solicitud_confirmacion   = '1'
   LET v_detalle.uso_futuro_banco         = ' '
END FUNCTION

#Esta clave deberá constar de 26 caracteres, más cuatro espacios en blanco a la derecha para completar las 30 posiciones.
#Formato de los 26 caracteres de la clave:
#                                            PPYYYYRROOOJJJQQNNN999999D

#Nomenclatura:
#  PP	      =	Código de Pago; para pagos de INFONAVIT será 60 (constante)
#  YYYY	   =	Año Calendario ( en nuestro caso es siempre 2013)
#  RR	      =	Ramo Generador de Pago, para INFONAVIT  (06)
#  OOO	   =	Oficina según Catálogo de Oficinas Generadoras de Pago. para INFONAVIT (183)
#  JJJ	   =	Día consecutivo del año correspondiente a la fecha de presentación (001=1 de enero; 032=1 de febrero; 365=31 de diciembre de año no bisiesto. 
#  QQ	      =	Número de quincena que se paga, en nuestro caso siempre colocamos (01)
#  NNN	   =	Tipo de pago, para pagos de INFONAVIT será (001) 
#  999999	=	Número consecutivo por día, cambia si generas más de un archivo al día
#  D	      =	Dígito Verificador (calculado) 
PRIVATE FUNCTION fn_genera_clave()

   DEFINE v_clave             CHAR(25)
   DEFINE v_respuesta         CHAR(30)
   DEFINE v_dia               SMALLINT

   DEFINE v_funcion_rastreo   STRING

   LET v_funcion_rastreo = "EXECUTE FUNCTION fn_genera_clave_rastreo(?)"
   PREPARE exe_funcion_rastreo FROM v_funcion_rastreo

   LET v_dia = (v_fPresentacion - MDY(1,1,YEAR(TODAY))) + 1
   #LET v_clave =  '60',                  Se modifica el valor por peticion del instituto
   LET v_clave =  '80',                
                  TODAY USING 'yyyy',
                  '06',
                  -- '183',
                  '213', -- cambio Jira 311, JGPS Oficina Generadora
                  v_dia USING '&&&',
                  '01',
                  '001',
                  v_referecia_numerica USING '&&&&&&'
   EXECUTE exe_funcion_rastreo USING v_clave
                               INTO v_respuesta
   RETURN v_respuesta
END FUNCTION
