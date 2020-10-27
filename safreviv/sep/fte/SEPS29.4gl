--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 26-06-2012
--===============================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPS29                                                   #
#Objetivo          => Programa batch de generación de archivo salida de        # 
#                     contacto                                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 26, 2012                                           #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_datos_contacto RECORD
         v_ind             SMALLINT,
         v_diag            CHAR(3),
         v_sql_error       INTEGER,
         v_id_expediente   LIKE sep_expediente.id_expediente,
         v_caso_adai       CHAR(11),--LIKE sep_expediente.caso_adai,
         v_estado          LIKE sep_estado_expediente.descripcion,
         v_nss             CHAR(13),--LIKE sep_nss_expediente.nss,
         v_tipo_trabajador CHAR(10),
         v_nombre          CHAR(120),--LIKE sep_nss_expediente.nombre,
         v_tel1            CHAR(10),--LIKE sep_nss_expediente.tel_contacto1,
         v_tel2            CHAR(10),--LIKE sep_nss_expediente.tel_contacto1,
         v_cel             CHAR(12),--LIKE sep_nss_expediente.tel_celular,
         v_correo_e        CHAR(40),--LIKE sep_nss_expediente.correo_e,
         v_contactado      CHAR(40)
       END RECORD

MAIN
DEFINE v_consulta       STRING,
       v_archivo_salida STRING,
       v_fecha_actual   STRING,
       v_var_cambio     LIKE sep_expediente.id_expediente,
       v_contador       INTEGER,
       v_canal          base.Channel,
       v_ruta_envio        LIKE seg_modulo.ruta_envio,
       r_ruta_ejecutable   LIKE seg_modulo.ruta_bin,
       r_ruta_lst          LIKE seg_modulo.ruta_listados,
       v_error             BOOLEAN,
       r_resultado_opera   SMALLINT,
       v_msg_correo        STRING,
       v_renglon           STRING

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)


   WHENEVER ERROR CONTINUE
   LET v_var_cambio = 0
   LET v_contador = 0
   LET v_error = TRUE

   # se construye fecha mmddyyyy
   LET v_fecha_actual = YEAR(TODAY) CLIPPED
   LET v_fecha_actual = v_fecha_actual.trim(),
                        MONTH(TODAY) USING "&&" CLIPPED,
                        DAY(TODAY) USING "&&" CLIPPED

   # se recupera ruta en la que se depositará el archivo de salida
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   # nombre archivo salida mmddyyyy_BATCH_DATOSCONTACTO.txt
   LET v_archivo_salida = v_fecha_actual,"_BATCH_DATOSCONTACTO.txt"

   LET v_canal = base.Channel.create()
   
   CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||v_archivo_salida, "w" )

   LET v_consulta = "EXECUTE FUNCTION sp_sep_consulta_datos_contacto(0,0,0)" # estado = 0(Todos los expedinentes sin importar el estado), ind_contacto = 0, contactado = 0 (Todos los expedientes sin importar si ha sido contactado o no el trabajador)
   PREPARE prp_rec_contactos FROM v_consulta
   DECLARE cur_rec_contactos CURSOR FOR prp_rec_contactos
   FOREACH cur_rec_contactos INTO v_datos_contacto.*
      # condicion para insertar dos registros en uno solo del archivo salida
      IF(v_var_cambio <> v_datos_contacto.v_id_expediente)THEN
         LET v_renglon = v_datos_contacto.v_caso_adai USING "###########",
                         v_datos_contacto.v_nss       USING "#############",
                         v_datos_contacto.v_nombre,
                         v_datos_contacto.v_tipo_trabajador,
                         v_datos_contacto.v_tel1      USING "##########",
                         v_datos_contacto.v_tel2      USING "##########",
                         v_datos_contacto.v_cel       USING "############",
                         v_datos_contacto.v_correo_e
                         
         LET v_var_cambio = v_datos_contacto.v_id_expediente
      ELSE
         LET v_error = FALSE
         LET v_renglon = v_renglon,
                         v_datos_contacto.v_caso_adai USING "###########",
                         v_datos_contacto.v_nss       USING "#############",
                         v_datos_contacto.v_nombre,
                         v_datos_contacto.v_tipo_trabajador,
                         v_datos_contacto.v_tel1      USING "##########",
                         v_datos_contacto.v_tel2      USING "##########",
                         v_datos_contacto.v_cel       USING "############",
                         v_datos_contacto.v_correo_e
         CALL v_canal.write([v_renglon])
         IF(STATUS)THEN
            DISPLAY "\n"
            DISPLAY "OCURRIÓ UN ERROR AL ESCRIBIR EN ARCHIVO:",v_archivo_salida
            DISPLAY "\n"
            LET v_error = TRUE
            CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 
                       RETURNING r_resultado_opera
         
            IF(r_resultado_opera <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_resultado_opera)
            END IF
            EXIT FOREACH    
         ELSE
            
         END IF
         LET v_contador = v_contador + 1
      END IF
   END FOREACH

   CALL v_canal.close()
   IF(v_contador > 0 AND v_error = FALSE AND SQLCA.SQLCODE = 0)THEN
      CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
             RETURNING r_resultado_opera

      # Funcion para generar reporte
      CALL fn_genera_reporte_contactos(0,0,0,p_usuario_cod,p_pid, p_proceso_cod, p_opera_cod)

      UPDATE sep_expediente 
         SET ind_contacto = 3
       WHERE ind_contacto = 0
      IF(r_resultado_opera <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_resultado_opera)
         CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 
                RETURNING r_resultado_opera         
         IF(r_resultado_opera <> 0)THEN
            # En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_resultado_opera)
         END IF
         LET v_msg_correo = "OCURRIÓ UN ERROR AL FINALIZAR OPERACIÓN"
      ELSE
         DISPLAY "\n"
         DISPLAY "TOTAL DE REGISTROS INCLUIDOS EN EL ARCHIVO: ",v_contador
         DISPLAY "RUTA ARCHIVO: ",v_ruta_envio CLIPPED
         DISPLAY "ARCHIVO: ",v_archivo_salida
         DISPLAY "\n"               
         LET v_msg_correo = "Operación realizada exitosamente"
      END IF
   ELSE
      LET v_msg_correo = "Ocurrió un error al generar archivo"
      DISPLAY "\n"
      DISPLAY "Ocurrió un error, no se encontraron registros"
      DISPLAY "\n"
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 
         RETURNING r_resultado_opera
         
      IF(r_resultado_opera <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   END IF 
   # Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod, 
                          '',
                          'Generar archivo Avisos de Suspensión',
                          v_msg_correo||
                          ''||
                          'ID Proceso   : '||p_pid||
                          'Proceso      : '||p_proceso_cod||
                          'Operacion    : '||p_opera_cod||
                          'Fecha Inicio : '||v_fecha_actual||
                          'Fecha Fin    : '||DATE
                          )


END MAIN
