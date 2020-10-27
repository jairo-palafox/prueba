--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-07-2013
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSP05                                                   #
#Objetivo          => Programa batch de preliquidacion de servicios            #
#Autor             => Jesus David Yañez Moreno                                 #
#Fecha inicio      => 16 Marzo 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "HPSG02.4gl" -- libreria de varibales globales

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN
DEFINE r_folio           LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       r_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--LIKE mdt_cat_mandato.desc_mandato,
          v_padre           STRING,
          v_identificador   SMALLINT,--LIKE mdt_cat_mandato.id_cat_mandato,          
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--LIKE cat_municipio.municipio_desc,
          v_recurrencia     SMALLINT,
          v_complemento_recurrencia SMALLINT,
          v_ejecuta_cod      SMALLINT,
          v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa  VARCHAR(40)--LIKE cat_entidad_federativa.entidad_desc_larga
       END RECORD,
       r_continuar          BOOLEAN,
       v_consulta           STRING,
       v_fecha_actual       DATE,
       v_ind_proceso        SMALLINT,
       v_id_ctr_aplica_pago_mandato DECIMAL(9,0),--LIKE mdt_ctr_aplica_pago_mandato.id_ctr_aplica_pago_mandato,
       v_indice             SMALLINT,
       v_entidad_federativa LIKE cat_entidad_federativa.entidad_federativa,
       v_municipio          LIKE cat_municipio.municipio,
       r_sql_error          INTEGER,
       r_isam_error         SMALLINT,
       r_msg_error          CHAR(80),
       r_desc_mandato       LIKE mdt_cat_mandato.desc_mandato,
       r_sum_monto_pesos    DECIMAL(22,2),
       v_total_monto_pesos  DECIMAL(22,2),
       r_actualiza          SMALLINT,
       r_resultado_opera    SMALLINT,
       v_mensaje            STRING,
       r_archivo_salida     STRING,
       v_estado_pago_mdt    SMALLINT,
       v_tot_reg            INTEGER       

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

   LET v_total_monto_pesos = 0

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'hps'

   
   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod) RETURNING r_folio

   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   DISPLAY "\n"
   DISPLAY "Proceso: ",v_proceso_desc
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "folio  : ",r_folio
   DISPLAY "INICIANDO PROCESO DE PRELIQUIDACIÓN"
   DISPLAY "\n"

   # Actualiza folio de proceso batch
   UPDATE bat_ctr_proceso
      SET folio = r_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod

   UPDATE bat_ctr_operacion
      SET folio = r_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      --AND opera_cod = p_opera_cod

   # la libreria recuperara informacion ya que el lanzador validó que existan mandatos a preliquidar
   CALL fn_valida_ejecucion_mandato(g_estado_abonado_pago_mdt) RETURNING r_continuar, r_mandatos

   LET v_fecha_actual = TODAY
   LET v_ind_proceso  = 1 # preliquidado 
   LET v_consulta = "\n INSERT INTO hps_ctr_aplica_pago_servicio",
                    "\n (id_ctr_aplica_pago_servicio,",
                    "\n  folio_pago_servicio,",
                    "\n  f_pago_servicio,",
                    "\n  ind_proceso,",
                    "\n  usuario)",
                    "\n VALUES(seq_hps_ctr_aplica_pago_servicio.NEXTVAL,?,?,?,?)"
   PREPARE prp_almacena_ctr_pago FROM v_consulta
   EXECUTE prp_almacena_ctr_pago USING r_folio,
                                       v_fecha_actual,
                                       v_ind_proceso,
                                       p_usuario_cod

   LET v_consulta = "\n SELECT FIRST 1 seq_hps_ctr_aplica_pago_servicio.CURRVAL",
                    "\n   FROM hps_ctr_aplica_pago_servicio"
   PREPARE prp_rec_identificador_ctr FROM v_consulta
   EXECUTE prp_rec_identificador_ctr INTO v_id_ctr_aplica_pago_mandato

   LET v_consulta = "\n SELECT entidad_federativa",
                    "\n   FROM cat_entidad_federativa",
                    "\n  WHERE entidad_desc_larga MATCHES ?"
   PREPARE prp_rec_cve_entidad FROM v_consulta

   LET v_consulta = "\n SELECT municipio",
                    "\n   FROM cat_municipio",
                    "\n  WHERE entidad_federativa = ?",
                    "\n    AND municipio_desc MATCHES ?"
   PREPARE prp_rec_cve_municipio FROM v_consulta  
   LET v_consulta = " EXECUTE FUNCTION sp_hps_preliquida_pago_instruccion(?,?,?,?,?,?,?,?)"
   PREPARE prp_ejecuta_preliquidacion_mandato FROM v_consulta
   DISPLAY "\n "
   FOR v_indice = 1 TO r_mandatos.getLength()
      IF( r_mandatos[v_indice].v_padre <> 0 )THEN # solo los mandatos tienen padre <> 0
         
         DISPLAY TODAY," ",
                 EXTEND(CURRENT,HOUR TO SECOND)," ",
                 r_mandatos[v_indice].v_descripcion_mdt," "

         EXECUTE prp_rec_cve_entidad USING r_mandatos[v_indice].v_ent_federeativa
                                      INTO v_entidad_federativa

         EXECUTE prp_rec_cve_municipio USING v_entidad_federativa,
                                             r_mandatos[v_indice].v_municipio
                                        INTO v_municipio
         LET r_sql_error = 0
                 
         EXECUTE prp_ejecuta_preliquidacion_mandato USING  r_mandatos[v_indice].v_identificador,
                                                           g_estado_abonado_pago_mdt,
                                                           r_mandatos[v_indice].v_descripcion_mdt,
                                                           v_entidad_federativa,
                                                           v_municipio,
                                                           r_folio,
                                                           v_id_ctr_aplica_pago_mandato,
                                                           p_usuario_cod
                                                      INTO r_sql_error,
                                                           r_isam_error,
                                                           r_msg_error,
                                                           r_desc_mandato,
                                                           r_sum_monto_pesos
         DISPLAY "Monto: ",r_sum_monto_pesos
         DISPLAY "\n"
         LET v_total_monto_pesos = v_total_monto_pesos + r_sum_monto_pesos
         IF(r_sql_error )THEN
            DISPLAY "\n"
            DISPLAY "Código:",r_sql_error
            DISPLAY "ISAM:",r_isam_error
            DISPLAY "Mensaje:",r_msg_error
            DISPLAY "Mandato:",r_desc_mandato
            DISPLAY "Suma Monto:",r_sum_monto_pesos
            DISPLAY "\n"
            DISPLAY "Program Stopped"

            # Actualiza a estado erróneo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
            IF( r_resultado_opera )THEN
               # Muestra el mensaje de inconsistencia en archivo y consola
               CALL fn_desplega_inc_operacion(r_resultado_opera)
            END IF
            EXIT PROGRAM 
         END IF         

      END IF
   END FOR
   IF( v_total_monto_pesos = 0 )THEN
      DISPLAY "PROCESO TERMINADO SIN PROCESAR NINGÚN REGISTRO"
   END IF
   DISPLAY "\n "
   DISPLAY "\n"
   DISPLAY "FINALIZANDO PRELIQUIDACIÓN"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   # Si no procesó ningún registro, termina la operación en erronea
   IF( v_total_monto_pesos = 0 )THEN
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      RETURN
   END IF
   DISPLAY "GENERANDO ARCHIVO DE SALIDA"
   DISPLAY "\n"

   # Genera archivo de salida con pago de mandatos
   CALL fn_genera_archivo_salida(r_folio) RETURNING r_archivo_salida

   DISPLAY "\n"
   DISPLAY "ARCHIVO GENERADO:",r_archivo_salida
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "GENERANDO REPORTE"
   DISPLAY "\n"
   LET v_tot_reg = 0
   CALL fn_mdt_rpt_aplicacion_mdt(r_folio,
                                  g_estado_preliquidado_pago_mdt, # estado 105 preliquidado
                                  p_usuario_cod,
                                  p_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  TODAY,
                                  "HPSP05") # nombre de archivo batch que ejecuta reporte
   RETURNING v_tot_reg
   
   DISPLAY "\n"
   DISPLAY "REPORTE CONCLUIDO"
   DISPLAY "FIN PROCESO"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "\n"

   CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)RETURNING r_resultado_opera
   IF(r_resultado_opera = 0)THEN
      LET v_mensaje = "El proceso ha finalizado correctamente"
      # Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Preliquidación',
                             v_mensaje)
   ELSE              
      # Actualiza a estado erróneo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = "Ocurrió un error al actualizar el estado de la operación"
      # Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Preliquidación',
                             v_mensaje)
   END IF               
END MAIN

{===============================================================================
Nombre: fn_genera_archivo_salida
Fecha creacion: 11 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para generar archivo de salida pagos de mandatos
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_genera_archivo_salida(p_folio)
DEFINE p_folio LIKE glo_folio.folio,
       v_registros RECORD
         v_caso           LIKE mdt_det_aplica_pago_mandato.caso_proceso,
         v_id_credito     LIKE mdt_det_aplica_pago_mandato.id_credito,
         v_situacion      CHAR(10),
         v_ent_federativa LIKE mdt_det_aplica_pago_mandato.ent_federativa,
         v_oficina_cesi   CHAR(10),
         v_cve_convenio   LIKE mdt_det_aplica_pago_mandato.clave_convenio,
         v_monto          LIKE mdt_det_aplica_pago_mandato.valor_descuento,
         v_f_inicio       LIKE mdt_det_aplica_pago_mandato.f_inicio_mandato,
         v_trabajador     LIKE mdt_det_aplica_pago_mandato.nombre_trabajador,
         v_regimen_pago   CHAR(10),
         v_calle          CHAR(20),
         v_colonia        CHAR(20),
         v_cp             CHAR(5),
         v_municipio      LIKE mdt_det_aplica_pago_mandato.municipio,
         v_espacio        CHAR(11),
         v_plazo          CHAR(2),
         v_pago           LIKE mdt_det_aplica_pago_mandato.monto_pesos,
         v_f_liquida      LIKE mdt_det_aplica_pago_mandato.f_liquida
       END RECORD,
       v_registros_tmp RECORD
         v_caso           CHAR(1),
         v_id_credito     CHAR(10),
         v_situacion      CHAR(10),
         v_ent_federativa CHAR(5),
         v_oficina_cesi   CHAR(10),
         v_cve_convenio   CHAR(16),
         v_monto          CHAR(8),
         v_f_inicio       DATE,
         v_trabajador     CHAR(50),
         v_regimen_pago   CHAR(10),
         v_calle          CHAR(20),
         v_colonia        CHAR(20),
         v_cp             CHAR(5),
         v_municipio      CHAR(5),
         v_espacio        CHAR(11),
         v_plazo          CHAR(2),
         v_pago           CHAR(8),
         v_f_liquida      DATE
       END RECORD,
       v_f_inicio   CHAR(8),
       v_f_liquida  CHAR(8),
       v_consulta   STRING,
       v_indice     SMALLINT,
       v_ruta_envio LIKE seg_modulo.ruta_envio,
       v_canal      base.Channel,
       v_archivo_salida STRING,
       v_fecha_actual   CHAR(10),
       v_hora           CHAR(8)

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   LET v_fecha_actual = TODAY
   LET v_hora = TIME
   LET v_archivo_salida = v_fecha_actual[7,10], # Año
                          v_fecha_actual[1,2],  # Mes
                          v_fecha_actual[4,5],  # Día                    
                          v_hora[1,2], # Hora
                          v_hora[4,5], # Minutos
                          v_hora[7,8], # Segundos
                          "_DETPAGOS.HPS"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||v_archivo_salida, "w" )

   LET v_indice = 1
   LET v_consulta = "\n SELECT det.caso_proceso,",
                    "\n        det.id_credito,",
                    "\n        '          ',",
                    "\n        det.ent_federativa,",
                    "\n        '          ',",
                    "\n        det.clave_convenio,",
                    "\n        det.valor_descuento * 100,", # Multiplica * 100 para no considerar el punto decimal en archivo
                    "\n        det.f_inicio_mandato,",
                    "\n        det.nombre_trabajador,",
                    "\n        '          ',",
                    "\n        '                    ',",
                    "\n        '                    ',",
                    "\n        '     ',",
                    "\n        det.municipio,",
                    "\n        '           ',",
                    "\n        '  ',",
                    "\n        det.monto_pesos * 100,", # Multiplica * 100 para no considerar el punto decimal en archivo
                    "\n        det.f_liquida",
                    "\n   FROM hps_ctr_aplica_pago_servicio ctr JOIN hps_det_aplica_pago_servicio det",
                    "\n     ON ctr.id_ctr_aplica_pago_servicio = det.id_ctr_aplica_pago_servicio ",
                    "\n  WHERE ctr.folio_pago_servicio = ?"
   PREPARE prp_rec_registros_pago_mdt FROM v_consulta
   DECLARE cur_rec_registros_pago_mdt CURSOR FOR prp_rec_registros_pago_mdt
   FOREACH cur_rec_registros_pago_mdt USING p_folio
                                       INTO v_registros.*
      LET v_registros_tmp.v_caso           = v_registros.v_caso
      LET v_registros_tmp.v_id_credito     = v_registros.v_id_credito USING "&&&&&&&&&&"
      LET v_registros_tmp.v_situacion      = v_registros.v_situacion
      LET v_registros_tmp.v_ent_federativa = v_registros.v_ent_federativa USING "&&&&&"
      LET v_registros_tmp.v_oficina_cesi   = v_registros.v_oficina_cesi
      LET v_registros_tmp.v_cve_convenio   = v_registros.v_cve_convenio[3,18]
      LET v_registros_tmp.v_monto          = v_registros.v_monto USING "&&&&&&&&"
      LET v_f_inicio                       = v_registros.v_f_inicio USING "yyyymmdd"
      LET v_registros_tmp.v_trabajador     = v_registros.v_trabajador
      LET v_registros_tmp.v_regimen_pago   = v_registros.v_regimen_pago
      LET v_registros_tmp.v_calle          = v_registros.v_calle
      LET v_registros_tmp.v_colonia        = v_registros.v_colonia
      LET v_registros_tmp.v_cp             = v_registros.v_cp
      LET v_registros_tmp.v_municipio      = v_registros.v_municipio USING "&&&&&"
      LET v_registros_tmp.v_espacio        = v_registros.v_espacio
      LET v_registros_tmp.v_plazo          = v_registros.v_plazo
      LET v_registros_tmp.v_pago           = v_registros.v_pago USING "&&&&&&&&"
      LET v_f_liquida                      = v_registros.v_f_liquida USING "yyyymmdd"
      
      CALL v_canal.writeLine(v_registros_tmp.v_caso||
                             v_registros_tmp.v_id_credito ||
                             v_registros_tmp.v_situacion||
                             v_registros_tmp.v_ent_federativa ||
                             v_registros_tmp.v_oficina_cesi||
                             v_registros_tmp.v_cve_convenio||
                             v_registros_tmp.v_monto ||
                             v_f_inicio ||
                             v_registros_tmp.v_trabajador||
                             v_registros_tmp.v_regimen_pago||
                             v_registros_tmp.v_calle||
                             v_registros_tmp.v_colonia||
                             v_registros_tmp.v_cp||
                             v_registros_tmp.v_municipio ||
                             v_registros_tmp.v_espacio||
                             v_registros_tmp.v_plazo||
                             v_registros_tmp.v_pago ||
                             v_f_liquida )

   END FOREACH
   FREE cur_rec_registros_pago_mdt
   CALL v_canal.close()
   RETURN v_archivo_salida
END FUNCTION
