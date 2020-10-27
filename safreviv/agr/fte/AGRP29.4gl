--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRP29                                        #
#Objetivo          =>Programa lanzado que realiza la carga del     #
#                    archivo de Uso de Garantía de Estados y       #
#                    Municipios                                    #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>05 Julio 2013                                 #
####################################################################

IMPORT os

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE m_c_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       m_v_nom_arch_carga  VARCHAR(100) -- nombre del archivo a integrar

MAIN
   DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
          p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
          p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
          p_folio                DECIMAL(9,0),
          v_s_qryTxt             STRING, -- guarda una sentencia SQL a ejecutar
          v_d_id_cre_ctr_archivo DECIMAL(9,0), -- identificador de cre ctr archivo
          v_v_nom_arch_dbload_d  VARCHAR(100), -- nombre del archivo que usara el dbload
          v_r_glo_ctr_archivo    RECORD LIKE glo_ctr_archivo.*, -- registro de la tabla de control global
          r_c_ruta_rescate       LIKE seg_modulo.ruta_rescate, -- ruta rescate
          r_c_ruta_listados      LIKE seg_modulo.ruta_listados, -- ruta listados
          r_b_existe_error       LIKE bat_ctr_proceso.estado_cod, -- estado del proceso
          r_resultado_opera      SMALLINT

   -- se reciben los parametros
   LET m_c_usuario_cod    = ARG_VAL(1)
   LET p_pid              = ARG_VAL(2)
   LET p_proceso_cod      = ARG_VAL(3)
   LET p_opera_cod        = ARG_VAL(4)
   LET p_folio            = ARG_VAL(5)
   LET m_v_nom_arch_carga = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG("AGRP29.log")

   DISPLAY "=INICIA AGRP29="
   DISPLAY " USUARIO       : ",m_c_usuario_cod
   DISPLAY " PID           : ",p_pid
   DISPLAY " FOLIO         : ",p_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",m_v_nom_arch_carga

   -- se inicia la operacion
   LET r_resultado_opera = fn_actualiza_opera_ini(p_pid,
                                                  p_proceso_cod,
                                                  p_opera_cod,
                                                  p_folio,
                                                  "AGRP29",
                                                  m_v_nom_arch_carga,
                                                  m_c_usuario_cod)

   IF r_resultado_opera <> 0 THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)

      -- se marca como erroneo la operacion
      LET r_resultado_opera = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 

      IF(r_resultado_opera <> 0)THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen las rutas de control del modulo
   LET v_s_qryTxt = " SELECT ruta_rescate, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_rutaBin FROM v_s_qryTxt
   EXECUTE prp_slc_rutaBin INTO r_c_ruta_rescate, r_c_ruta_listados

   DISPLAY " >>>Se crea el archivo del DBLOAD"
   -- se asigna la ruta del archivo concatenado con el nombre del archivo
   LET v_v_nom_arch_dbload_d = "carga_uso_gtia_edo_mcpio.data"

   -- se invoca la función que crea el archivo "instruccion del dbload" para los registros detalle
   CALL fn_crea_arch_instruccion_det(r_c_ruta_rescate, v_v_nom_arch_dbload_d) RETURNING r_b_existe_error

   -- verifica si existió algun error durante el proceso de carga
   IF r_b_existe_error THEN
      -- se marca como erroneo la operacion
      LET r_resultado_opera = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 

      IF(r_resultado_opera <> 0)THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " >>>Se crea la tabla temporal de carga (detalle)"
   -- se invoca la función que crea la tabla temporal para la carga
   CALL fn_crea_tabla_tem_carga() RETURNING r_b_existe_error

   -- verifica si existió algun error durante el proceso de carga
   IF r_b_existe_error THEN
      -- se marca como erroneo la operacion
      LET r_resultado_opera = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 

      IF(r_resultado_opera <> 0)THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " >>>Inicia carga de información"
   -- se invoca la función que realiza la carga inicial del archivo
   CALL fn_carga_archivo(r_c_ruta_rescate, v_v_nom_arch_dbload_d) RETURNING r_b_existe_error

   -- verifica si existió algun error durante el proceso de carga
   IF r_b_existe_error THEN
      -- se marca como erroneo la operacion
      LET r_resultado_opera = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 

      IF(r_resultado_opera <> 0)THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " >>>Inserta registro en tabla de control (global)"
   -- se asignan los valores del registro a insertar
   LET v_r_glo_ctr_archivo.proceso_cod    = p_proceso_cod
   LET v_r_glo_ctr_archivo.opera_cod      = p_opera_cod
   LET v_r_glo_ctr_archivo.nombre_archivo = m_v_nom_arch_carga
   LET v_r_glo_ctr_archivo.folio          = p_folio
   LET v_r_glo_ctr_archivo.estado         = 1
   LET v_r_glo_ctr_archivo.f_actualiza    = TODAY
   LET v_r_glo_ctr_archivo.usuario        = m_c_usuario_cod

   -- se agrega el registro en la glo_ctr_archivo
   INSERT INTO glo_ctr_archivo (proceso_cod,
                                opera_cod,
                                nombre_archivo,
                                folio,
                                estado,
                                f_actualiza,
                                usuario)
                        VALUES (v_r_glo_ctr_archivo.proceso_cod,
                                v_r_glo_ctr_archivo.opera_cod,
                                v_r_glo_ctr_archivo.nombre_archivo,
                                v_r_glo_ctr_archivo.folio,
                                v_r_glo_ctr_archivo.estado,
                                v_r_glo_ctr_archivo.f_actualiza,
                                v_r_glo_ctr_archivo.usuario)

   DISPLAY " >>>Inserta registro en tabla de control (local)"
   -- se invoca la función que valida la información cargada
   CALL fn_inserta_cifras_control() RETURNING r_b_existe_error, v_d_id_cre_ctr_archivo

   -- verifica si existió algun error durante el proceso de carga
   IF r_b_existe_error THEN
      -- se marca como erroneo la operacion
      LET r_resultado_opera = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 

      IF(r_resultado_opera <> 0)THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF

      EXIT PROGRAM
   END IF

   -- se actualiza la operacion y el proceso como Finalizado
   LET r_resultado_opera = fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) 

   IF r_resultado_opera <> 0 THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)

      --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que crea el archivo que sirve como instruccion de la carga
#          inicial de información por medio de dbload
FUNCTION fn_crea_arch_instruccion_det(p_c_ruta_rescate, p_v_nom_arch_dbload)
   DEFINE p_c_ruta_rescate     LIKE seg_modulo.ruta_rescate, -- ruta de rescate
          p_v_nom_arch_dbload  VARCHAR(100), -- nombre del archivo que usara el dbload
          v_s_rutaArchivo      STRING, -- ruta del archivo a cargar
          v_v_primea_linea     VARCHAR(50), -- primera linea del archivo (instruccion del dbload)
          v_ch_archivo_ins     BASE.CHANNEL, -- manejador del archivo
          v_b_existe_error     BOOLEAN, -- booleana que indica si ocurrió o no un error en el proceso
          v_b_existe_arch      BOOLEAN -- booleana que indica si existe o no el archivo de carga

   -- se inicializan variables
   LET v_b_existe_error = FALSE -- se asume que no existirá error en el proceso

   -- se concatena la ruta y el nombre del archivo
   LET v_s_rutaArchivo = p_c_ruta_rescate CLIPPED || "/" || p_v_nom_arch_dbload

   -- se crea el manejador de archivo para insertar
   LET v_ch_archivo_ins = base.channel.create()

   -- se indica que el delimitador de renglon es el retorno de carro
   CALL v_ch_archivo_ins.setdelimiter("\r\n")

   -- se abre el archivo, indicando que se escribirá en él
   CALL v_ch_archivo_ins.openfile(v_s_rutaArchivo,"w")

   -- se crea la primera linea del archivo
   LET v_v_primea_linea = 'FILE "',m_v_nom_arch_carga CLIPPED,'" ('

   -- se escribe el registro en el archivo
   CALL v_ch_archivo_ins.write([v_v_primea_linea CLIPPED])
   CALL v_ch_archivo_ins.write(["nss           1-11,"])
   CALL v_ch_archivo_ins.write(["f_solic      12-19,"])
   CALL v_ch_archivo_ins.write(["monto_solic  20-34,"])
   CALL v_ch_archivo_ins.write(["fondo        35-39);"])
   CALL v_ch_archivo_ins.write(["INSERT INTO tmp_usogtia_edo_mcpio_det;"])

   -- se cierra el manejador de lectura
   CALL v_ch_archivo_ins.close()

   -- se revisa si existe el archivo en la ruta de listados
   CALL os.Path.exists(p_c_ruta_rescate CLIPPED || "/" || p_v_nom_arch_dbload) RETURNING v_b_existe_arch

   -- verifica si existió el archivo
   IF NOT(v_b_existe_arch)THEN
      DISPLAY " ERROR: El archivo: '",p_v_nom_arch_dbload CLIPPED,"' no fue encontrado en la ruta correspondiente"

      -- se indica que existió error
      LET v_b_existe_error = TRUE
   END IF

   RETURN v_b_existe_error
END FUNCTION

# Objetivo: Función que crea la tabla temporal donde se guardará la información
#           de carga inicial
FUNCTION fn_crea_tabla_tem_carga()
   DEFINE v_s_qryTxt        STRING, -- guarda una sentencia SQL a ejecutar
          v_b_existe_error  BOOLEAN -- booleana que indica si se pudo cargar el archivo

   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- se inicializan variables
   LET v_b_existe_error = FALSE -- se asume que no existirá error

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS tmp_usogtia_edo_mcpio_det;"

   PREPARE prp_drop_tmp_carga_ini_det FROM v_s_qryTxt
   EXECUTE prp_drop_tmp_carga_ini_det

   -- verifica si exisitió error en la ejecución
   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "    ERROR: Ocurrió un error al intentar eliminar la tabla detalle. Código: ",SQLCA.sqlcode

      LET v_b_existe_error = TRUE

      RETURN v_b_existe_error
   END IF

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE tmp_usogtia_edo_mcpio_det\n",
                    " (nss             CHAR(11),\n",
                    "  f_solic         DATE,\n",
                    "  monto_solic     DECIMAL(15,0),\n",
                    "  fondo           CHAR(5)\n",
                    " );"

   PREPARE prp_create_tmp_carga_ini_det FROM v_s_qryTxt
   EXECUTE prp_create_tmp_carga_ini_det

   -- verifica si exisitió error en la ejecución
   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "    ERROR: Ocurrió un error al intentar crear la tabla detalle. Código: ",SQLCA.sqlcode

      LET v_b_existe_error = TRUE

      RETURN v_b_existe_error
   END IF

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

   RETURN v_b_existe_error
END FUNCTION

#Objetivo: Función que carga la información del archivo en la tabla temporal
FUNCTION fn_carga_archivo(p_c_ruta_rescate, p_v_nom_arch_dbload_d)
   DEFINE p_c_ruta_rescate      LIKE seg_modulo.ruta_rescate, -- ruta de rescate
          p_v_nom_arch_dbload_d VARCHAR(100), -- nombre del archivo que usara el dbload (detalle)
          v_v_nom_arch_err      VARCHAR(100), -- nombre del archivo de error
          v_s_comando           STRING, -- contiene el comando a ejecutar
          v_b_existe_error      BOOLEAN -- booleana que indica si se pudo cargar el archivo

   -- se inicializan variables
   LET v_b_existe_error = FALSE -- se asume que no existirá error

   -- se asigna el nombre del archivo donde se guardarán los errores en caso de ocurrir
   LET v_v_nom_arch_err = p_v_nom_arch_dbload_d CLIPPED || ".err"

   -- El comando especifica el formato de fecha a usar, se ubica en la ruta del archivo con las especificaciones para realizar el dbload
   -- Ejecuta dbload indicado base de datos (-d), ruta de archivo de comandos (-c), log de errores (-l), limit de errores (-e), cada cuantos registros se realiza commit (-n) y bloqueo de tablas (-k)
   -- Remueve el formato de fecha especificado
   LET v_s_comando = " export DBDATE='Y4MD';",
                     " cd ",p_c_ruta_rescate CLIPPED, "/;",
                     " dbload -d safre_tmp -c ",p_v_nom_arch_dbload_d CLIPPED,
                     " -l ", v_v_nom_arch_err CLIPPED,
                     " -e 100 -n 1000 -k ; unset DBDATE"

   -- se ejecuta el comando creado
   --DISPLAY "  v_s_comando: ",v_s_comando
   RUN v_s_comando

   -- valida si fue posible ejecutar el comando
   IF STATUS THEN
      DISPLAY "    ERROR: No fue posible ejecutar el comando de manera satisfactoria:\n",
              "    ",v_s_comando
      DISPLAY "    Archivo de errores: ",v_v_nom_arch_err CLIPPED
      LET v_b_existe_error = TRUE
   ELSE
      DISPLAY "    Terminó el proceso de carga"
   END IF

   RETURN v_b_existe_error
END FUNCTION

#Objetivo: Función que valida la información cargada en la tabla temporal
FUNCTION fn_inserta_cifras_control()
   DEFINE v_ax_tot_registros    INTEGER, -- total de registros insertados
          v_r_cre_ctr_archivo   RECORD -- registro de la tabla de control
             id_cre_ctr_archivo DECIMAL(9,0),
             folio_archivo      DECIMAL(9,0),
             lote               SMALLINT,
             f_lote             DATE,
             id_proceso         SMALLINT,
             operacion          SMALLINT,
             nom_archivo        CHAR(40),
             tot_registros      DECIMAL(10,0),
             tot_aceptados      DECIMAL(10,0),
             tot_rechazados     DECIMAL(10,0),
             tot_sin_origen     DECIMAL(10,0),
             estado             SMALLINT,
             f_proceso          DATE,
             usuario            CHAR(20)
          END RECORD,
          v_s_qryTxt            STRING, -- guarda una sentencia SQL a ejecutar
          v_si_lote             SMALLINT, -- lote
          v_si_estado           SMALLINT, -- estado del archivo
          v_dt_f_lote           DATE, -- fecha de lote
          v_ax_operacion        SMALLINT, -- operacion del proceso
          v_b_existe_error      BOOLEAN -- booleana que indica si se pudo cargar el archivo

   -- se inicializan variables
   LET v_ax_operacion = 25 -- Carga Uso Garantía Estados y Municipios
   LET v_b_existe_error = FALSE
   LET v_dt_f_lote = TODAY
   LET v_si_lote = 1
   LET v_si_estado = 10

   -- se cuenta el numero de registros insertados en la temporal
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    " FROM safre_tmp:tmp_usogtia_edo_mcpio_det"

   PREPARE prp_cuenta_regs FROM v_s_qryTxt
   EXECUTE prp_cuenta_regs INTO v_ax_tot_registros

   -- se valida los registros en detalle
   IF v_ax_tot_registros = 0 THEN
      -- se asigna estatus erroneo ya que no se encontraron registros detalle
      DISPLAY " ERROR: No se encontraron registros a procesar"

      LET v_b_existe_error = TRUE
      RETURN v_b_existe_error, v_r_cre_ctr_archivo.id_cre_ctr_archivo
   END IF

   -- se asignan los valores del registro a insetar
   LET v_r_cre_ctr_archivo.folio_archivo      = 0
   LET v_r_cre_ctr_archivo.lote               = v_si_lote
   LET v_r_cre_ctr_archivo.f_lote             = v_dt_f_lote
   LET v_r_cre_ctr_archivo.id_proceso         = g_id_proceso_agr
   LET v_r_cre_ctr_archivo.operacion          = v_ax_operacion
   LET v_r_cre_ctr_archivo.nom_archivo        = m_v_nom_arch_carga
   LET v_r_cre_ctr_archivo.tot_registros      = v_ax_tot_registros
   LET v_r_cre_ctr_archivo.tot_aceptados      = 0
   LET v_r_cre_ctr_archivo.tot_rechazados     = 0
   LET v_r_cre_ctr_archivo.tot_sin_origen     = 0
   LET v_r_cre_ctr_archivo.estado             = v_si_estado
   LET v_r_cre_ctr_archivo.f_proceso          = TODAY
   LET v_r_cre_ctr_archivo.usuario            = m_c_usuario_cod
   
   -- se inserta el registro en la tabla de control
   LET v_s_qryTxt = " INSERT INTO safre_viv:cre_ctr_archivo\n",
                    "      VALUES (safre_viv:seq_cre_archivo.NEXTVAL,\n",
                    "              ",v_r_cre_ctr_archivo.folio_archivo,",\n",
                    "              ",v_r_cre_ctr_archivo.lote,",\n",
                    "              '",v_r_cre_ctr_archivo.f_lote,"',\n",
                    "              ",v_r_cre_ctr_archivo.id_proceso,",\n",
                    "              ",v_r_cre_ctr_archivo.operacion,",\n",
                    "              '",v_r_cre_ctr_archivo.nom_archivo,"',\n",
                    "              ",v_r_cre_ctr_archivo.tot_registros,",\n",
                    "              ",v_r_cre_ctr_archivo.tot_aceptados,",\n",
                    "              ",v_r_cre_ctr_archivo.tot_rechazados,",\n",
                    "              ",v_r_cre_ctr_archivo.tot_sin_origen,",\n",
                    "              ",v_r_cre_ctr_archivo.estado,",\n",
                    "              '",v_r_cre_ctr_archivo.f_proceso,"',\n",
                    "              '",v_r_cre_ctr_archivo.usuario,"')"

   PREPARE prp_insert_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_insert_ctr_arch

   -- se consulta el id cre ctr archivo insertado
   LET v_s_qryTxt = " SELECT safre_viv:seq_cre_archivo.CURRVAL\n",
                    "   FROM safre_viv:cre_ctr_archivo\n",
                    "  WHERE folio_archivo = 0\n",
                    "    AND id_proceso = ",g_id_proceso_agr,"\n",
                    "    AND operacion = ",v_ax_operacion,"\n",
                    "    AND nom_archivo = '",m_v_nom_arch_carga CLIPPED,"'\n",
                    "    AND estado = ",v_si_estado

   PREPARE prp_slct_seq_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_slct_seq_ctr_arch INTO v_r_cre_ctr_archivo.id_cre_ctr_archivo

   -- verifica si se obtuvo id del archivo
   IF v_r_cre_ctr_archivo.id_cre_ctr_archivo IS NULL THEN
      DISPLAY " No fue posible obtener el identificador del archivo"

      LET v_b_existe_error = TRUE
   END IF

   DISPLAY " Identificador del archivo: ",v_r_cre_ctr_archivo.id_cre_ctr_archivo
   RETURN v_b_existe_error, v_r_cre_ctr_archivo.id_cre_ctr_archivo
END FUNCTION
