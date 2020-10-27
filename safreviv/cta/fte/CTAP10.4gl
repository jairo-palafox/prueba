#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CTA                                                     #
#Programa          => CTAP10                                                  #
#Objetivo          => PROGRAMA QUE GENERA EL ESTADO DE CUENTA MASIVO          #
#Fecha Inicio      => 17-ENERO-2014                                           #
###############################################################################
DATABASE safre_viv

GLOBALS "CTAP10.inc"

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_ffin                     DATE
PRIVATE DEFINE v_finicio                  DATE
PRIVATE DEFINE v_fsaldo_inicio            DATE

PRIVATE DEFINE v_id_ctr_edo_cuenta        DECIMAL(9,0)
PRIVATE DEFINE v_id_maximo                INTEGER

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados

#Variables para la generacion del archivo
PRIVATE DEFINE v_archivo_edo_cta          STRING
PRIVATE DEFINE v_ruta_envio               LIKE seg_modulo.ruta_envio


MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_fn_prepara_tablas_edo_cuenta  STRING
   DEFINE v_fn_genera_saldo_edo_cuenta    STRING

   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)
   
   DEFINE v_fproceso                      DATE
   DEFINE v_intervalo                     INTEGER

   DEFINE v_fcorte_anterior               DATE
   DEFINE v_tabla_saldo                   VARCHAR(40)

   DEFINE v_num_errores                   SMALLINT
   DEFINE v_num_error                     SMALLINT
   DEFINE v_inicio_error                  INTEGER
   DEFINE v_fin_error                     INTEGER
   DEFINE v_consulta_error                STRING

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
   LET v_fproceso = TODAY
   --LET v_fproceso = MDY(2,5,2014)
   --Se establece la fecha de corte como el ultimo dia natural del mes non inmediato anterior
   IF MONTH(v_fproceso) MOD 2 = 0 THEN
      #En este caso nos encontramos en mes par, por lo que obtenemos el ultimo dia del mes anterior
      LET v_ffin = MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1
   ELSE
      #En este caso nos encontramos en un mes non por lo que aun no termina el periodo en curso, 
      #se calcula la fecha fin como el ultimo dia del mes non anterior
      LET v_ffin = MDY(MONTH(MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1),1,YEAR(MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1)) - 1
   END IF
   #Se establece la fecha de inicio como el primer dia del mes anterior a la fecha fin
   LET v_finicio = MDY(MONTH(MDY(MONTH(v_ffin),1,YEAR(v_ffin)) -1),1,YEAR(MDY(MONTH(v_ffin),1,YEAR(v_ffin)) -1))
   LET v_fsaldo_inicio = v_finicio - 1

   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY "El último bimestre concluido contempla el siguiente intervalo:"
   DISPLAY "Fecha inicio: ", v_finicio USING 'dd-mm-yyyy'
   DISPLAY "Fecha fin: ", v_ffin USING 'dd-mm-yyyy'
   DISPLAY "*******************************************************************"
   
	-- se solicita el numero de folio asociado a la operacion
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
   
   #Se genera la tabla de saldos para precalificacion
   DISPLAY ""
   DISPLAY "Inicializando el proceso..."
   DISPLAY ""
   WHENEVER ERROR CONTINUE
      LET v_fn_prepara_tablas_edo_cuenta = "EXECUTE FUNCTION fn_prepara_tablas_edo_cuenta(?,?,?)"
      PREPARE exe_fn_prepara_tablas_edo_cuenta FROM v_fn_prepara_tablas_edo_cuenta
      EXECUTE exe_fn_prepara_tablas_edo_cuenta USING v_folio, v_finicio, v_ffin 
                                                INTO v_resultado, v_mensaje_respuesta, v_id_ctr_edo_cuenta
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR al intentar inicializar el proceso para el estado de cuenta masivo: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING r_resultado_opera
         CALL fn_actualiza_error()
         RETURN
      END IF
   WHENEVER ERROR STOP
   
   IF v_resultado = 0 THEN
      DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY " Las tablas del estado de cuenta masivo se generaron correctamente"
      DISPLAY " FECHA              : ",v_fproceso USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY "*******************************************************************"

      SELECT MAX(id_derechohabiente)
      INTO v_id_maximo
      FROM afi_derechohabiente
      LET v_intervalo = v_id_maximo / INTERVALOS

      DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY " El identificador maximo de los afiliados es: ", v_id_maximo
      DISPLAY ""
      DISPLAY " Se utilizaran intervalos de: ", v_intervalo
      DISPLAY "*******************************************************************"

      #Se valida si se tiene disponible el saldo inicial del periodo
      SELECT f_saldo, tabla_saldo
      INTO v_fcorte_anterior, v_tabla_saldo
      FROM cta_ctr_sdo_edo_cuenta
      WHERE ind_saldo = 1

      IF v_fcorte_anterior IS NULL OR v_fcorte_anterior <> v_fsaldo_inicio THEN
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY "El saldo de corte del periodo anterior no corresponde al inicio del periodo actual"
         DISPLAY ""
         DISPLAY "Inicia la generación del saldo inicial del periodo, fecha de corte ", v_fsaldo_inicio USING 'dd-mm-yyyy'
         WHENEVER ERROR CONTINUE
            CLOSE DATABASE
            DATABASE safre_viv
            LET v_fn_genera_saldo_edo_cuenta = "EXECUTE FUNCTION fn_genera_saldo_edo_cuenta(?,?)"
            PREPARE exe_fn_genera_saldo_edo_cuenta FROM v_fn_genera_saldo_edo_cuenta
            EXECUTE exe_fn_genera_saldo_edo_cuenta USING v_fsaldo_inicio, v_tabla_saldo
                                                   INTO v_resultado, v_mensaje_respuesta
            IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "Ocurrió un ERROR al generar el saldo inicial con fecha de corte: ", v_fsaldo_inicio USING 'dd-mm-yyyy'
               DISPLAY SQLCA.SQLCODE
               DISPLAY SQLERRMESSAGE
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
               DISPLAY "*******************************************************************"
               CALL fn_actualiza_error()
               RETURN
            END IF
            DISPLAY ""
            DISPLAY "La generación del saldo inicial se ejecutó correctamente"
            DISPLAY "*******************************************************************"
         WHENEVER ERROR STOP
      END IF

      #Se busca la tabla con el saldo de inicio del corte anterior para calcular el nuevo saldo final
      SELECT tabla_saldo
      INTO v_tabla_saldo
      FROM cta_ctr_sdo_edo_cuenta
      WHERE ind_saldo = 0

      DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY "Inicia la generación del saldo final del periodo, fecha de corte ", v_ffin USING 'dd-mm-yyyy'
      WHENEVER ERROR CONTINUE
         CLOSE DATABASE
         DATABASE safre_viv
         LET v_fn_genera_saldo_edo_cuenta = "EXECUTE FUNCTION fn_genera_saldo_edo_cuenta(?,?)"
         PREPARE exe_fn_genera_saldo_edo_cuenta2 FROM v_fn_genera_saldo_edo_cuenta
         EXECUTE exe_fn_genera_saldo_edo_cuenta2 USING v_ffin, v_tabla_saldo
                                                INTO v_resultado, v_mensaje_respuesta
         IF SQLCA.SQLCODE <> 0 THEN
            DISPLAY "Ocurrio un ERROR al generar el saldo final con fecha de corte: ", v_ffin USING 'dd-mm-yyyy'
            DISPLAY SQLCA.SQLCODE
            DISPLAY SQLERRMESSAGE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            DISPLAY "*******************************************************************"
            CALL fn_actualiza_error()
            RETURN
         END IF
         DISPLAY ""
         DISPLAY "La generación del saldo final se ejecutó correctamente"
         DISPLAY "*******************************************************************"
      WHENEVER ERROR STOP

      #Se ejecutal todas las etapas de la generacion del estado de cuenta en paralelo
      CALL fn_genera_edo_cuenta()

      #Se monitorea el avance de las etapas 
      CALL fn_monitor_edo_cuenta()

      DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY "Termina la etapa de generacion de los archivos para el estado de cuenta masivo"

      SELECT count(*)
      INTO v_num_errores
      FROM cta_detalle_edo_cuenta 
      WHERE id_cta_ctr_edo_cuenta = v_id_ctr_edo_cuenta
      AND estado = 9

      IF v_num_errores > 0 THEN
         DISPLAY ""
         DISPLAY "El proceso detecto que se presentaron problemas en ", v_num_errores, " archivos"
         DISPLAY "Por lo cual los siguientes archivos no se generaron: "
         DISPLAY ""
         LET v_consulta_error =  "SELECT ",
                                 "num_archivo, ",
                                 "inicio_intervalo, ",
                                 "fin_intervalo ",
                                 "FROM cta_detalle_edo_cuenta  ",
                                 "WHERE id_cta_ctr_edo_cuenta = ? ",
                                 "AND estado = 9"
         PREPARE exe_consulta_error FROM v_consulta_error
         DECLARE cur_consulta_error CURSOR FOR exe_consulta_error
         FOREACH cur_consulta_error USING v_id_ctr_edo_cuenta
                                    INTO  v_num_error,
                                          v_inicio_error,
                                          v_fin_error
            DISPLAY "Archivo ", v_num_error, ", intervalo: ", v_inicio_error, " - ", v_fin_error
         END FOREACH
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
         
         CALL fn_actualiza_error()
      ELSE
         # Finaliza la operacion de generacion del archivo
         UPDATE cta_ctr_edo_cuenta SET estado = 4 WHERE id_cta_ctr_edo_cuenta = v_id_ctr_edo_cuenta
         
         CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING r_resultado_opera
         IF(r_resultado_opera <> 0)THEN         
            # Actualiza a estado erróneo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
         END IF
      END IF
      DISPLAY "*******************************************************************"
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY "Ocurrio un ERROR al intentar inicializar el proceso para el estado de cuenta masivo: "
      DISPLAY " FECHA              : ",v_fproceso USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "Mensaje: ", v_mensaje_respuesta
      DISPLAY "*******************************************************************"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      CALL fn_actualiza_error()
   END IF
END MAIN

PRIVATE FUNCTION fn_actualiza_error()
   UPDATE cta_ctr_edo_cuenta SET estado = 9 WHERE id_cta_ctr_edo_cuenta = v_id_ctr_edo_cuenta
END FUNCTION

PRIVATE FUNCTION fn_genera_edo_cuenta()
   DEFINE v_comando                       STRING
   DEFINE v_ruta_archivo                  VARCHAR(50)
   DEFINE i                               SMALLINT
   DEFINE num_archivo                     CHAR(2)
   DEFINE v_nombre_archivo                STRING
   DEFINE v_archivo                       base.Channel
   DEFINE v_fecha                         STRING

   SELECT ruta_envio
   INTO v_ruta_archivo
   FROM seg_modulo
   WHERE modulo_cod = 'cta'

   DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY " Inicia el proceso de preparacion de informacion..."
      DISPLAY " El proceso generara ", INTERVALOS, " archivos"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY "*******************************************************************"

   FOR i = 1 TO INTERVALOS
      LET num_archivo = i
      
      LET v_nombre_archivo = v_ruta_archivo CLIPPED,"/edo_cuenta_", num_archivo CLIPPED, ".sql"
      LET v_archivo = base.Channel.create()
      CALL v_archivo.openFile(v_nombre_archivo,"w")
      --CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
      CALL v_archivo.writeLine('SET OPTIMIZATION LOW;');
      CALL v_archivo.writeLine('SET LOCK MODE TO WAIT 3;');
      CALL v_archivo.writeLine("EXECUTE PROCEDURE sp_genera_edo_cuenta_masivo(")
      LET v_fecha = v_finicio USING 'mmddyy'
      CALL v_archivo.writeLine("'" || v_fecha || "',")
      LET v_fecha = v_ffin USING 'mmddyy'
      CALL v_archivo.writeLine("'" || v_fecha || "',")
      CALL v_archivo.writeLine(i || ",")
      CALL v_archivo.writeLine(INTERVALOS || ",")
      CALL v_archivo.writeLine(v_id_maximo || ",")
      CALL v_archivo.writeLine(v_id_ctr_edo_cuenta || ");")

      LET v_comando = "nohup dbaccess safre_viv ", v_nombre_archivo CLIPPED, " &"
      DISPLAY "Inicia la preparacion del archivo: ", num_archivo CLIPPED
      RUN v_comando
      SLEEP (2)    #El proceso espera 2 segundos para lanzal la siguiente etapa
   END FOR   
END FUNCTION

PRIVATE FUNCTION fn_monitor_edo_cuenta()
   DEFINE v_ind_executa               SMALLINT
   DEFINE v_tabla_finalizada          STRING
   DEFINE v_tabla_pendiente           STRING

   DEFINE v_id_cta_detalle_edo_cuenta  DECIMAL(9,0)
   DEFINE v_num_archivo                SMALLINT

   CLOSE DATABASE
   DATABASE safre_viv
         
   LET v_tabla_finalizada =   "SELECT ", 
                                 "id_cta_detalle_edo_cuenta,  ",
                                 "num_archivo  ",
                              "FROM cta_detalle_edo_cuenta  ",
                              "WHERE id_cta_ctr_edo_cuenta = ? ",
                              "AND estado = 2"
   PREPARE exe_tabla_finalizada FROM v_tabla_finalizada
   DECLARE cur_tabla_finalizada CURSOR FOR exe_tabla_finalizada

   LET v_tabla_pendiente = "SELECT FIRST 1 id_cta_detalle_edo_cuenta ",
                           "FROM cta_detalle_edo_cuenta ",
                           "WHERE id_cta_ctr_edo_cuenta = ? ",
                           "AND estado = 1"
   PREPARE exe_tabla_pendiente FROM v_tabla_pendiente

   LET v_ind_executa = 1
   WHILE v_ind_executa = 1
      SLEEP (600)    #El proceso se duerme 10 minutos

      #Se buscan las tablas que ya fueron preparadas
      FOREACH cur_tabla_finalizada USING v_id_ctr_edo_cuenta 
                                    INTO v_id_cta_detalle_edo_cuenta, 
                                         v_num_archivo
         IF v_id_cta_detalle_edo_cuenta IS NOT NULL THEN
            #Se actualiza el estado del archivo
            UPDATE cta_detalle_edo_cuenta SET estado = 3 WHERE id_cta_detalle_edo_cuenta = v_id_cta_detalle_edo_cuenta
            CALL fn_genera_archivo(v_id_cta_detalle_edo_cuenta, v_num_archivo)
            UPDATE cta_detalle_edo_cuenta SET estado = 4 WHERE id_cta_detalle_edo_cuenta = v_id_cta_detalle_edo_cuenta
         END IF
      END FOREACH

      #Se valida si existe alguna tabla que este en construccion
      LET v_id_cta_detalle_edo_cuenta = NULL
      EXECUTE exe_tabla_pendiente USING v_id_ctr_edo_cuenta INTO v_id_cta_detalle_edo_cuenta
      IF v_id_cta_detalle_edo_cuenta IS NULL THEN
         #Si el id es nulo significa que ya se termino la preparacion de todas las tablas
         LET v_ind_executa = 0
      END IF 
   END WHILE
END FUNCTION

PRIVATE FUNCTION fn_genera_archivo(p_id_cta_detalle_edo_cuenta, p_num_archivo)

   DEFINE p_id_cta_detalle_edo_cuenta  DECIMAL(9,0)
   DEFINE p_num_archivo                SMALLINT
   
   DISPLAY "Se inicia la creación del archivo ", p_num_archivo

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cta'

   LET v_archivo_edo_cta      = "edo_cta_masivo_tmp"

   #Genera device HPL para archivo de estado de cuenta masivo

   CALL fn_crea_project("edo_cta")

   CALL fn_genera_device(v_ruta_envio,
                         v_archivo_edo_cta,
                         p_usuario_cod)

   CALL fn_genera_query(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod,
                      p_num_archivo)

   CALL fn_genera_format(v_ruta_envio,
                         v_archivo_edo_cta,
                         p_usuario_cod)
   
   CALL fn_genera_map(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod)


   DISPLAY ""
   DISPLAY "Inicia la generacion de los detalles del archivo"
   DISPLAY ""

   PREPARE exe_pdq FROM "SET PDQPRIORITY HIGH"
   EXECUTE exe_pdq

   CALL fn_genera_job(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod,
                      'edo_cta')

   CALL fn_ejecuta_hpl(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod,
                      'edo_cta',
                      p_num_archivo)
END FUNCTION

PRIVATE FUNCTION fn_crea_project(p_proyecto)
   DEFINE p_proyecto   STRING
   DEFINE v_comando    STRING
   DEFINE r_ejecucion  SMALLINT

   LET v_comando = "onpladm describe project ", p_proyecto, " 1>/dev/null 2>/dev/null"
   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create project ",p_proyecto, " 1>/dev/null 2>/dev/null"
      DISPLAY "CREA PROYECTO HPL ",p_proyecto
      RUN v_comando
   END IF
END FUNCTION

PRIVATE FUNCTION fn_genera_device(p_ruta_envio,p_archivo_salida,p_usuario)
    DEFINE p_ruta_envio       STRING,
           p_archivo_salida   LIKE cat_layout.archivo,
           p_usuario          LIKE seg_modulo.usuario,
           v_str_archivo      STRING,
           v_canal            base.Channel,
           v_cadena           STRING

   LET v_str_archivo = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".device"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   LET v_cadena = p_ruta_envio CLIPPED, '/ESTADO_DE_CUENTA_MASIVO_', v_ffin USING 'ddmmyyyy'
   CALL v_canal.writeLine('BEGIN OBJECT DEVICEARRAY '||p_archivo_salida CLIPPED||".device \n")
   CALL v_canal.writeLine('BEGIN SEQUENCE ')
   CALL v_canal.writeLine('TYPE           FILE ')
   CALL v_canal.writeLine('FILE           '||v_cadena)
   CALL v_canal.writeLine('TAPEBLOCKSIZE  0 ')
   CALL v_canal.writeLine('TAPEDEVICESIZE 0 ')
   CALL v_canal.writeLine('PIPECOMMAND      ')
   CALL v_canal.writeLine('END  SEQUENCE    \n')
   CALL v_canal.writeLine('END  OBJECT      ')

   CALL v_canal.close()

   CALL fn_crea_objeto(v_str_archivo, p_archivo_salida CLIPPED||".device", "device", "")
END FUNCTION

PRIVATE FUNCTION fn_genera_format(p_ruta_envio, p_archivo_salida, p_usuario)

   DEFINE v_ch_format      base.Channel
   DEFINE p_ruta_envio     STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_format STRING

   LET v_archivo_format = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".format"

   LET v_ch_format = base.Channel.create()
   CALL v_ch_format.openFile(v_archivo_format,"w")
   CALL v_ch_format.writeLine('BEGIN OBJECT COBOLFORMAT '||p_archivo_salida CLIPPED||".format \n")
	
   CALL v_ch_format.writeLine('PROJECT       edo_cta')
   CALL v_ch_format.writeLine('CHARACTERSET  ASCII ')
   CALL v_ch_format.writeLine('MACHINE       Intel ')
	CALL v_ch_format.writeLine('DRIVER       	COBOL \n')
   
   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	tipo_registro')
	CALL v_ch_format.writeLine('PICTURE      	A(2)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	registro')
   CALL v_ch_format.writeLine('PICTURE      	A(405)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   dummycr')
   CALL v_ch_format.writeLine('PICTURE      	X')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')
   
   CALL v_ch_format.writeLine('END OBJECT')

   CALL v_ch_format.close()

   CALL fn_crea_objeto(v_archivo_format, p_archivo_salida CLIPPED||".format", "format", "edo_cta")

END FUNCTION

PRIVATE FUNCTION fn_genera_map(p_ruta_envio, p_archivo_salida, p_usuario)
   DEFINE p_ruta_envio     STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_map    STRING
   DEFINE v_comando        STRING
   DEFINE v_ch_map         base.Channel

   LET v_archivo_map = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".map"

   LET v_ch_map = base.Channel.create()
   CALL v_ch_map.openFile(v_archivo_map,"w")

   CALL v_ch_map.writeLine('BEGIN OBJECT UNLOADMAP '||p_archivo_salida CLIPPED||".map")

   CALL v_ch_map.writeLine('PROJECT      edo_cta')
   CALL v_ch_map.writeLine('FORMAT       '||p_archivo_salida CLIPPED||'.format')
   CALL v_ch_map.writeLine('DATABASE     safre_viv')
   CALL v_ch_map.writeLine('QUERY        '||p_archivo_salida CLIPPED||'.query')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      tipo_registro')
   CALL v_ch_map.writeLine('FIELDNAME       tipo_registro')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      registro')
   CALL v_ch_map.writeLine('FIELDNAME       registro')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      dummycr')
   CALL v_ch_map.writeLine('FIELDNAME       dummycr')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING     BINARY')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('END OBJECT')

   LET v_comando = "onpladm delete map ",p_archivo_salida CLIPPED||".map -fu -p edo_cta ",
                   " 1>/dev/null 2>/dev/null"
   RUN v_comando

   CALL fn_crea_objeto(v_archivo_map, p_archivo_salida CLIPPED||".map", "map", "edo_cta" )
END FUNCTION

PRIVATE FUNCTION fn_genera_query(p_ruta_envio, p_archivo_salida, p_usuario, p_num_archivo)
   DEFINE p_ruta_envio   STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE p_num_archivo    SMALLINT
   DEFINE v_archivo_query  STRING
   DEFINE v_query          STRING

   DEFINE v_txt            CHAR(2)
   DEFINE v_ch_query       base.Channel

   LET v_txt = p_num_archivo

   LET v_archivo_query = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".query"

   LET v_ch_query = base.Channel.create()
   CALL v_ch_query.openFile(v_archivo_query,"w")

   CALL v_ch_query.writeLine('BEGIN OBJECT QUERY   '||p_archivo_salida CLIPPED||".query")

   CALL v_ch_query.writeLine('PROJECT edo_cta')
   CALL v_ch_query.writeLine('DATABASE safre_viv')

   LET v_query = '"SELECT ',
                     "tipo_registro, ",
                     "registro, ",
                     "fn_salto_linea() dummycr ",
                  "FROM edo_cuenta_", v_txt CLIPPED, " ",
                  'ORDER BY id_edo_cuenta"'

   CALL v_ch_query.writeLine('SELECTSTATEMENT '||v_query)
   CALL v_ch_query.writeLine('END OBJECT')

   CALL v_ch_query.close()

   CALL fn_crea_objeto(v_archivo_query, p_archivo_salida CLIPPED||".query", "query", "edo_cta")
END FUNCTION

PRIVATE FUNCTION fn_genera_job(p_ruta_envio,p_archivo_salida,p_usuario,p_proyecto)

    DEFINE p_ruta_envio     STRING,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_modulo.usuario,
           p_proyecto       STRING,
           v_str_archivo    STRING,
           v_canal          base.Channel

   LET v_str_archivo = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".job"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   CALL v_canal.writeLine('BEGIN OBJECT UNLOADJOB '||p_archivo_salida CLIPPED||".job")
   CALL v_canal.writeLine('PROJECT           '||p_proyecto CLIPPED)
   CALL v_canal.writeLine('DEVICE            '||p_archivo_salida CLIPPED||'.device ')
   CALL v_canal.writeLine('MAP               '||p_archivo_salida CLIPPED||'.map ')
   CALL v_canal.writeLine('FILTER            ')
   CALL v_canal.writeLine('SERVER            vivop_tcp') 
   CALL v_canal.writeLine('DATABASE          safre_viv ')
   CALL v_canal.writeLine('REJECTFILE        '||
                           p_ruta_envio CLIPPED||'/'||p_usuario CLIPPED||'.'||
                           p_archivo_salida CLIPPED||'.rjt ')
   CALL v_canal.writeLine('LOGFILE           '||
                           p_ruta_envio CLIPPED||'/'||p_usuario CLIPPED||'.'||
                           p_archivo_salida CLIPPED||'.log ')
   CALL v_canal.writeLine('ISOLATIONLEVEL    DR ')
   CALL v_canal.writeLine('MAXERRORS         1 ')
   CALL v_canal.writeLine('END  OBJECT      \n')

   CALL v_canal.close()


   CALL fn_crea_objeto(v_str_archivo, p_archivo_salida CLIPPED||".job", "job", p_proyecto )
END FUNCTION

PRIVATE FUNCTION fn_crea_objeto(p_str_archivo, p_objeto, p_tipo, p_proyecto)
   DEFINE p_tipo            STRING
   DEFINE p_objeto          STRING
   DEFINE p_str_archivo     STRING
   DEFINE p_proyecto        STRING
   DEFINE v_comando         STRING
   DEFINE r_ejecucion       SMALLINT

   #Se ejecuta primero el create y despues el modify, para no verificar si ya
   #existe el objeto

   CASE p_tipo
      WHEN "device"
         LET v_comando = "onpladm describe device ", p_objeto, " 1>/dev/null 2>/dev/null"
         
      WHEN "job" 
         LET v_comando = "onpladm describe job ", p_objeto,
                         " -fu -p ", p_proyecto , " 1>/dev/null 2>/dev/null"
      WHEN "format"
         LET v_comando = "onpladm describe format ", p_objeto,
                         " -p ",p_proyecto , " 1>/dev/null 2>/dev/null"
     WHEN "map"
         LET v_comando = "onpladm describe map ", p_objeto,
                         " -fu -p ",p_proyecto , " 1>/dev/null 2>/dev/null"
     WHEN "query"
         LET v_comando = "onpladm describe query ", p_objeto,
                         " -p ",p_proyecto , " 1>/dev/null 2>/dev/null"

   END CASE

	#DISPLAY v_comando
   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create object -F ",
                      p_str_archivo , " 1>/dev/null 2>/dev/null"

		DISPLAY ""
		DISPLAY "CREA OBJETO HPL ", p_str_archivo
   ELSE
      LET v_comando = "onpladm modify object -F ",
                      p_str_archivo , " 1>/dev/null 2>/dev/null"
		DISPLAY ""
		DISPLAY "MODIFICA OBJETO HPL ", p_str_archivo
   END IF

	#DISPLAY v_comando
   RUN v_comando
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_hpl(p_ruta_envio, p_archivo_salida,p_usuario,p_proyecto, p_num_archivo)

    DEFINE p_ruta_envio   STRING,
           p_proyecto       STRING,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_usuario.usuario_cod,
           p_num_archivo    SMALLINT,
           v_comando        STRING,
           v_fecha          DATE

   DEFINE v_txt            CHAR(2)

   LET v_txt = p_num_archivo
   LET v_comando = "onpladm run job ", p_archivo_salida CLIPPED, ".job -fu -p ",
                    p_proyecto , " 1>/dev/null 2>/dev/null"
   
   DISPLAY " INICIO ETAPA       : EJECUTA JOB DESCARGA HPL"
   LET v_fecha = TODAY
   DISPLAY " FECHA              : ",v_fecha USING "DD-MM-YYYY"
   DISPLAY " HORA               : ",TIME(CURRENT),"\n"
   DISPLAY ""
   #registra etapa en archivo de monitoreo

   RUN v_comando
   --DISPLAY v_comando
   DISPLAY " FIN ETAPA          : EJECUTA JOB DESCARGA HPL"
   LET v_fecha = TODAY
   DISPLAY " FECHA              : ",v_fecha USING "DD-MM-YYYY"
   DISPLAY " HORA               : ",TIME(CURRENT),"\n"
   DISPLAY ""

	LET v_comando = "mv ", 	p_ruta_envio CLIPPED, "/ESTADO_DE_CUENTA_MASIVO_", v_ffin USING 'ddmmyyyy', " ", 
									p_ruta_envio CLIPPED, "/ESTADO_DE_CUENTA_MASIVO_", v_ffin USING 'ddmmyyyy', "_",v_txt CLIPPED, ".edo_cta"
   RUN v_comando

	DISPLAY ""
	DISPLAY "Eliminando los archivos de control de HPL..."
	DISPLAY ""
	LET v_comando = "rm ", p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,"*"
	RUN v_comando

	LET v_comando = "rm ", p_ruta_envio CLIPPED,"/edo_cuenta_",v_txt CLIPPED,".sql"
	RUN v_comando
   

END FUNCTION