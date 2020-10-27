--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => SEP                                                                    #
#Programa     => SEPP27                                                                 #
#Objetivo     => Genera el archivo de salida de ajustes al credito de contabilidad      #
#Fecha inicio => Junio 23, 2012                                                       #
#########################################################################################
DATABASE safre_viv
--GLOBALS "SEPG01.4gl"
DEFINE v_fecha_rpt         STRING,
       v_fecha_ctr         CHAR(8),
       p_folio                LIKE deo_preliquida.folio_liquida,
       v_conteo_registros  INTEGER,
       v_total_expedientes    INTEGER, -- numero total de expedientes      
       v_archivo_salida    STRING

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado

       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       r_bnd_fin_oper         SMALLINT,
       v_rest_valida          SMALLINT,
       v_mensaje              STRING,
       v_sql                  STRING, -- cadena con una instruccion SQL
       v_ruta_listados        VARCHAR(40), -- ruta de los listados
       v_archivo              STRING, -- nombre base del archivo
       v_nombre_archivo       STRING, -- nombre del archivo
       v_registro             STRING, -- cadena con un registro
       
       rec_sep_expediente     RECORD -- registro de expediente para archivo de salida
          id_expediente         CHAR(18)     ,
          folio                 DECIMAL(9,0) ,
          trabajador            CHAR(11)     ,
          nombre_trabajador     CHAR(120)    ,
          num_caso              CHAR(40)     ,
          acreditado            CHAR(11)     ,
          nombre_acreditado     CHAR(40)     ,
          num_credito           DECIMAL(10,0),
          f_envio_dm            CHAR(10)     ,
          f_confirmacion        CHAR(10)
       END RECORD,
       r_ruta_ejecutable   LIKE seg_modulo.ruta_bin,
       r_ruta_lst          LIKE seg_modulo.ruta_listados,
       r_ruta              LIKE seg_modulo.ruta_listados,
       v_nom_reporte          STRING ,
       v_contador             INTEGER,
        v_error             BOOLEAN,
       v_msg_correo        STRING,
      
       r_resultado_opera   SMALLINT,
       v_canal             base.Channel,
        v_manejador_rpt     OM.SaxDocumentHandler,
        v_fecha_actual      STRING,    
       v_ruta_envio        LIKE seg_modulo.ruta_envio,
  

       ch_salida              base.Channel -- channel para generar archivo de salida


   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

    SELECT COUNT(*)
     INTO v_total_expedientes
     FROM sep_expediente
    WHERE estado IN (40,45,50) # 40 = Dictamen Registrado, 45 = Restitucion Solicitada, 50 = Restitucion Liquidada 
      AND ind_baja_notificacion = 2
      
     DISPLAY "TOTAL DE REGISTROS A PROCESAR: ",v_total_expedientes

   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod)
        RETURNING p_folio

   -- se crea el apuntador para el archivo de salida
   LET ch_salida = base.Channel.create()
   
   -- se configura el channel
   CALL ch_salida.setDelimiter(",")
       
   -- se inicia el contador de expedientes
   
   -- se obtiene la ruta de listados de SEP
   SELECT ruta_rescate
   INTO v_ruta_listados
   FROM seg_modulo
   WHERE modulo_cod = "sep"

   -- se crea el nombre base del archivo
   -- 4.1.	AAAAMMDD_DESVINCULACION.csv
   LET v_nombre_archivo = TODAY USING "yyyymmdd"
   LET v_nombre_archivo = v_nombre_archivo, "_DESVINCULACION.csv"
   
   -- se crea la ruta completa del archivo
   LET v_archivo = v_ruta_listados CLIPPED || "/" || v_nombre_archivo
   
   
   -- se abre el archivo 
   CALL ch_salida.openFile( v_archivo, "w" )

   -- se leen los expedientes 
   {
1.	Considerar todos los expedientes que se encuentren en estado 40 “Dictamen Registrado” 
y 45 “Restitucion Solicitada”, 50 “Restitucion Liquidada” y que el campo ind_baja_notificacion 
(sep_expediente) sea = 2.
   } 
      LET v_fecha_actual = YEAR(TODAY) CLIPPED
   LET v_fecha_actual = v_fecha_actual.trim(),
                        MONTH(TODAY) USING "&&" CLIPPED,
                        DAY(TODAY) USING "&&" CLIPPED
    LET v_fecha_rpt = YEAR(TODAY) CLIPPED

   LET v_fecha_rpt = MONTH(TODAY) USING "&&" CLIPPED,"-",
                     DAY(TODAY) USING "&&" CLIPPED,"-",
                     v_fecha_rpt.trim()

   LET v_fecha_ctr = MONTH(TODAY) USING "&&" CLIPPED,
                     DAY(TODAY) USING "&&" CLIPPED,
                     v_fecha_rpt.trim()   
    SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "sep"
  
   LET v_archivo_salida = v_fecha_actual,"_DESVINCULACION.csv"

   LET v_canal = base.Channel.create()
   CALL v_canal.setDelimiter(",")
   CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||v_archivo_salida, "w" )
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   --CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta
   --CALL fn_rutas("bat") RETURNING r_ruta, r_ruta_lst
   # CREACIÓN DE REPORTE
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPP271.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario_cod CLIPPED, "-SEPP27-", 
                          p_pid USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_cod USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      LET v_contador = 1
      LET v_conteo_registros=0
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT rpt_expedientes_baja_notificacion TO XML HANDLER v_manejador_rpt
   
   LET v_sql = "SELECT\n",
               "a.id_expediente,\n",
               "b.folio,\n",
               "b.trabajador,\n",
               "b.nombre_trabajador,\n",
               "b.num_caso,\n",
               "b.acreditado,\n",
               "b.nombre_acreditado,\n",
               "b.num_credito,\n",
               "b.f_envio_dm,\n",
               "b.f_confirmacion\n",
               "FROM\n",
               "sep_expediente a,\n",
               "sep_aviso_desvinculacion b\n",
               "WHERE\n",
               "a.estado IN (40,45,50)\n",
               "AND\n",
               "a.ind_baja_notificacion = 2",
               "AND\n",
               "a.id_expediente = b.id_expediente"
               
   PREPARE sid_expedientes_baja_notif FROM v_sql
   DECLARE cur_expedientes_baja_notif CURSOR FOR sid_expedientes_baja_notif
   
   -- se escribe encabezado de archivo
   --Trabajador, nombre, caso arc, acreditado, nombre, credito, fecha envio a dm, fecha de confirmación
   --LET v_registro = "Trabajador, nombre, caso arc, acreditado, nombre, credito, fecha envio a dm, fecha de confirmación"
   
   --CALL ch_salida.writeLine(v_registro)
   
   -- encabezados

        CALL ch_salida.write(["nss trabajador"     ,
                               "nombre trabajador" ,
                               "número de caso"    ,
                               "nss acreditado"    ,
                               "nombre acreditado" ,
                               "número de crédito" ,
                               "fecha envio dm"    ,
                               "fecha confirmación"
                              ])
         
          CALL v_canal.write(["nss trabajador"     ,
                               "nombre trabajador" ,
                               "número de caso"    ,
                               "nss acreditado"    ,
                               "nombre acreditado" ,
                               "número de crédito" ,
                               "fecha envio dm"    ,
                               "fecha confirmación"
                              ])
   
   FOREACH cur_expedientes_baja_notif INTO rec_sep_expediente.*
      --DISPLAY rec_sep_expediente.*
      
      -- si se tienen datos, se escribe el registro
      IF ( rec_sep_expediente.trabajador IS NOT NULL ) THEN
      
         -- se escribe el registro en el archivo
         CALL ch_salida.write([rec_sep_expediente.trabajador        ,
                               rec_sep_expediente.nombre_trabajador ,
                               rec_sep_expediente.num_caso          ,
                               rec_sep_expediente.acreditado        ,
                               rec_sep_expediente.nombre_acreditado ,
                               rec_sep_expediente.num_credito       ,
                               rec_sep_expediente.f_envio_dm        ,
                               rec_sep_expediente.f_confirmacion    
                              ])
         
          
        --reporte     
            LET v_error = FALSE
            CALL v_canal.write([rec_sep_expediente.trabajador        ,
                               rec_sep_expediente.nombre_trabajador ,
                               rec_sep_expediente.num_caso          ,
                               rec_sep_expediente.acreditado        ,
                               rec_sep_expediente.nombre_acreditado ,
                               rec_sep_expediente.num_credito       ,
                               rec_sep_expediente.f_envio_dm        ,
                               rec_sep_expediente.f_confirmacion    
                              ])
            --CALL v_canal.write("123")
            IF(STATUS)THEN
               DISPLAY "OCURRIÓ UN ERROR AL ESCRIBIR EN ARCHIVO:",v_archivo_salida
               LET v_error = TRUE
               CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 
                          RETURNING r_resultado_opera
         
               IF(r_resultado_opera <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_resultado_opera)
               END IF
               EXIT FOREACH
               
            END IF
            OUTPUT TO REPORT rpt_expedientes_baja_notificacion(v_contador,rec_sep_expediente.*)
            LET v_contador= v_contador + 1
          -- se incrementa el contador de expedientes
         LET v_conteo_registros = v_conteo_registros + 1
         -- CALL rpt_expedientes_baja_notificacion(v_total_expedientes,rec_sep_expediente.*)
      END IF
      
   END FOREACH
   
   -- se cierra el archivo
   CALL ch_salida.close()

   
       
         
         CALL v_canal.close()
         IF(v_error = TRUE OR SQLCA.SQLCODE <> 0)THEN
            LET v_msg_correo = "Ocurrió un error al generar archiv de avisos de Desvinculación"
            DISPLAY "Ocurrió un error, no se encontraron registros para aviso Desvinculación"
            CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 
                RETURNING r_resultado_opera
         
            IF(r_resultado_opera <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_resultado_opera)
            END IF
         ELSE
            --se actualiza el expediente a ind_baja_notificacion = 3
         UPDATE sep_expediente
         SET    ind_baja_notificacion = 3
         WHERE  ind_baja_notificacion = 2
         AND    estado IN (40,45,50)
               
            CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
               RETURNING r_resultado_opera         
            IF(r_resultado_opera <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
              CALL fn_desplega_inc_operacion(r_resultado_opera)
            ELSE
                -- se inserta en el registro de control
            INSERT INTO sep_ctr_desvinculacion (folio, f_proceso)
            VALUES ( p_folio,TODAY)
   
            -- se actualiza el folio en la tabla bat_ctr_operacion
            UPDATE bat_ctr_operacion
             SET    folio = p_folio
              WHERE  proceso_cod = p_proceso_cod
                AND    opera_cod   = p_opera_cod
                 AND    pid         = p_pid
                 
               DISPLAY "\n"
               DISPLAY "FOLIO GENERADO: ",p_folio
               DISPLAY "TOTAL DE AVISOS DE DESVINCULACIÓN: ",v_conteo_registros
               DISPLAY "RUTA ARCHIVO: ",v_ruta_envio CLIPPED
               DISPLAY "ARCHIVO: ",v_archivo_salida
               DISPLAY "\n"               
               LET v_msg_correo = "Operación realizada exitosamente"
           END IF
         END IF
         FREE cur_expedientes_baja_notif
         
      FINISH REPORT rpt_expedientes_baja_notificacion
      
   
  
 
      
   ELSE
      DISPLAY "No fue posible generar el reporte"
      LET v_msg_correo = "Ocurrió un error en la generación de archivo de salida"
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
                          'Generar archivo Avisos de Desvinculación',
                          v_msg_correo||
                          ''||
                          'ID Proceso   : '||p_pid||
                          'Proceso      : '||p_proceso_cod||
                          'Operacion    : '||p_opera_cod||
                          'Fecha Inicio : '||v_fecha_actual||
                          'Fecha Fin    : '||DATE
                          )
#####################################################################                          
   -- se obtiene el folio
  
END MAIN

-- reporte de avisos generados
REPORT rpt_expedientes_baja_notificacion(v_conteo,rec_sep_expediente)
DEFINE v_conteo               SMALLINT,
       rec_sep_expediente     RECORD -- registro de expediente para archivo de salida
          id_expediente         CHAR(18)     ,
          folio                 DECIMAL(9,0) ,
          trabajador            CHAR(11)     ,
          nombre_trabajador     CHAR(120)    ,
          num_caso              CHAR(40)     ,
          acreditado            CHAR(11)     ,
          nombre_acreditado     CHAR(40)     ,
          num_credito           DECIMAL(10,0),
          f_envio_dm            CHAR(10)     ,
          f_confirmacion        CHAR(10)
       END RECORD
      
   FORMAT
    FIRST PAGE HEADER
     PRINTX v_fecha_rpt
     PRINTX v_archivo_salida
     PRINTX p_folio USING "#########"
     PRINTX v_total_expedientes
     
   ON EVERY ROW
     
     PRINTX v_conteo                             ,
            rec_sep_expediente.trabajador        ,
            rec_sep_expediente.nombre_trabajador ,
            rec_sep_expediente.num_caso          ,
            rec_sep_expediente.acreditado        ,
            rec_sep_expediente.nombre_acreditado ,
            rec_sep_expediente.num_credito       ,
            rec_sep_expediente.f_envio_dm        ,
            rec_sep_expediente.f_confirmacion    

END REPORT