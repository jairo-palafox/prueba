--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI
#Programa     => AFIP15
#Objetivo     => Programa que carga el archivo de los nuevos NSS de TRM en la tabla temporal mediante un dbload
#Fecha inicio => Enero 14, 2014
#########################################################################################
DATABASE safre_viv
MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       p_titulo               STRING, -- titulo del mensaje enviado en el correo
       p_mensaje              STRING, -- cuerpo del mensaje enviado
       v_resultado            SMALLINT,
       v_consulta             STRING, -- cadena con enunciado sql 
       v_mensaje              STRING,
       v_conteo_registros     DECIMAL(9,0),
	   v_r_registro           RECORD
         nss         CHAR(11),
         curp        CHAR(18),
         rfc         CHAR(13),
         nombre_imss CHAR(50),
         sexo        CHAR(1) 
       END RECORD,
       v_archivo           base.Channel, -- archivo de salida
       v_ruta_rescate      VARCHAR(40), -- directorio de rescate de archivos
       v_ruta_archivo      STRING -- ruta completa del archivo
       
   -- se recuperan los parametros
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "CARGA")

   -- parametros recibidos
   DISPLAY "___ Parametros de ejecucion recibidos ___ "
   DISPLAY "Usuario  : ", p_usuario_cod    
   DISPLAY "PID      : ", p_pid            
   DISPLAY "Proceso  : ", p_proceso_cod    
   DISPLAY "Operacion: ", p_opera_cod      
   DISPLAY "Folio    : ", p_folio          
   DISPLAY "Archivo  : ", p_nombre_archivo 
   
   -- se obtiene la ruta de rescate
   SELECT ruta_rescate
   INTO   v_ruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "afi"
   
   -- se conecta a la instancia tempora
   DATABASE safre_tmp
   
   -- se indica alta prioridad para la consulta
   EXECUTE IMMEDIATE "SET PDQPRIORITY HIGH"
   
   DISPLAY "Generando tabla temporal para carga de archivo"
   
   -- se elimina la tabla temporal en caso de que exista
   LET v_consulta = "\n DROP TABLE IF EXISTS tmp_afi_nuevos_nss_trm"
   
   EXECUTE IMMEDIATE v_consulta
   
   -- se crea la tabla temporal
   LET v_consulta = "\n CREATE TABLE tmp_afi_nuevos_nss_trm (",
                    "\n  nss         CHAR(11),",
					"\n  curp        CHAR(18),",
					"\n  rfc         CHAR(13),",
					"\n  nombre_imss CHAR(50),",
					"\n  sexo        CHAR(1)  ",
					"\n )"
					
   --NSS, CURP, RFC, Nombre IMSS, Sexo

   PREPARE sid_creatabla FROM v_consulta
   EXECUTE sid_creatabla
   
   -- se crea el objeto
   LET v_archivo = base.Channel.create()
   
   -- se usa el pipe como delimitador
   CALL v_archivo.setDelimiter("|")

   -- archivo que se cargara
   LET v_ruta_archivo = v_ruta_rescate CLIPPED, "/", p_nombre_archivo CLIPPED
   
   -- se abre el archivo para su escritura
   CALL v_archivo.openFile( v_ruta_archivo, "r" )
   
   -- se inicia el contador
   LET v_conteo_registros = 0
   
   WHILE ( v_archivo.read([v_r_registro.*]) )
      -- si todo esta en blanco se asume que es un renglon vacio 
	  IF ( v_r_registro.nss         IS NULL AND
           v_r_registro.curp        IS NULL AND
           v_r_registro.rfc         IS NULL AND
           v_r_registro.nombre_imss IS NULL AND
           v_r_registro.sexo        IS NULL ) THEN
         CONTINUE WHILE
      END IF
      
	  -- se inserta en la tabla temporal
	  INSERT INTO tmp_afi_nuevos_nss_trm VALUES ( v_r_registro.* )

      -- se cuenta un registro escrito
      LET v_conteo_registros = v_conteo_registros + 1
	  
	  INITIALIZE v_r_registro.* TO NULL
   END WHILE

   -- se devuelve la priodidad como estaba
   EXECUTE IMMEDIATE "SET PDQPRIORITY LOW"
   
   -- se conecta a la instancia productiva
   DATABASE safre_viv
   
   -- se cierra el archivo
   CALL v_archivo.close()
  
   LET p_mensaje = "ID Proceso   : ", p_pid, "\n", 
                   "Proceso      : AFILIACION - CARGA DE NUEVOS NSS DE TRM",
                   "Operación    : CONSULTA\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n"
                   
   -- se complementa el mensaje
   LET p_mensaje = p_mensaje || v_mensaje

   LET v_mensaje = "\nSe ha concluido la validación del archivo.\n",
                   "Archivo: ", v_ruta_archivo, "\n",
                   "Num. de registros cargados: ", v_conteo_registros USING "########&",
                   "\n\n"

   DISPLAY v_mensaje

   -- se inserta el archivo cargado a la tabla de control de archivos
   INSERT INTO glo_ctr_archivo (
      proceso_cod   ,
      opera_cod     ,
      nombre_archivo,
      folio         ,
      estado        ,
      f_actualiza   ,
      usuario       
   )
   VALUES (
      p_proceso_cod   ,
	  p_opera_cod     ,
	  p_nombre_archivo,
	  p_folio         ,
	  "1"             , -- cargado
	  TODAY           ,
	  p_usuario_cod    
   )
   
   -- se finaliza la operacion   
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING v_resultado

   LET p_titulo = "Finalización de operación - AFILIACION - CARGA DE NUEVOS NSS DE TRM"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(p_pid, p_proceso_cod, p_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "CARGA")

END MAIN