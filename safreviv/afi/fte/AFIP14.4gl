####################################################################################
#Proyecto     => SAFRE VIVIENDA                                                    #
#Propietario  => E.F.P.                                                            #
#----------------------------------------------------------------------------------#
#Modulo       => AFI                                                               #
#Programa     => AFIP14                                                            #
#Objetivo     => Programa que genera el archivo de salida                          #
#                de los registros de derechohabientes - domicilios                 #
#Fecha inicio => 21 de octubre de 2013                                             #
####################################################################################


DATABASE safre_viv

GLOBALS

   DEFINE p_pid                  LIKE bat_ctr_operacion.pid          -- PID del proceso
   DEFINE p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod  -- codigo del proceso
   DEFINE p_opera_cod            LIKE bat_ctr_operacion.opera_cod    -- codigo de la operacion
   DEFINE p_usuario              LIKE seg_usuario.usuario_cod        -- clave del usuario firmado
   DEFINE p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo

   DEFINE p_b_tipo_carga         SMALLINT -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE v_valida               SMALLINT
   DEFINE v_resultado            SMALLINT

   DEFINE p_folio                DECIMAL(9,0)
   DEFINE v_conteo_registros     DECIMAL(9,0)

   DEFINE p_v_nom_prog           VARCHAR(30)
   DEFINE v_ruta_salida          VARCHAR(40)   -- directorio de archivos de salida

   DEFINE v_ruta_archivo         STRING -- ruta completa del archivo
   DEFINE p_titulo               STRING -- titulo del mensaje enviado en el correo
   DEFINE p_mensaje              STRING -- cuerpo del mensaje enviado
   DEFINE v_s_qryTxt             STRING
   DEFINE v_consulta_sd          STRING -- cadena con enunciado sql con domicilios
   DEFINE v_consulta_nd          STRING -- cadena con enunciado sql sin domicilios
   DEFINE v_mensaje              STRING
   DEFINE v_s_registro           STRING

   DEFINE v_afi_dh_dom RECORD
      rfc       CHAR(13),
      curp      CHAR(18),
      nombre    CHAR(120),
      nss       CHAR(11),
      domicilio CHAR(90),
      localidad CHAR(50),
      estado    CHAR(50),
      municipio CHAR(50),
      cp        CHAR(5)
   END RECORD

   DEFINE v_afi_dh RECORD
      rfc       CHAR(13),
      curp      CHAR(18),
      nombre    CHAR(120),
      nss       CHAR(11),
      domicilio CHAR(245)
   END RECORD

   DEFINE v_paterno           CHAR(40)
   DEFINE v_materno           CHAR(40)
   DEFINE v_nombre            CHAR(40)

   DEFINE v_archivo           base.Channel  -- archivo de salida

   DEFINE r_si_cod_error      SMALLINT      -- en caso de error contiene el código
   DEFINE r_i_isam_error      INTEGER       -- isam error
   DEFINE r_v_msj_error       VARCHAR(250)  -- mensaje de error

END GLOBALS

MAIN

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".AFIP14.log")

   DISPLAY "=INICIA AFIP14="
   DISPLAY " USUARIO       : ",p_usuario
   DISPLAY " PID           : ",p_pid
   DISPLAY " PROCESO       : AFILIACION - GENERACIÓN ARCHIVO DERECHOHABIENTES - DOMICILIOS"
   DISPLAY " ARCHIVO       : ",p_nombre_archivo USING "#########&"
   DISPLAY " FECHA INICIIO : ", TODAY, "\n"

   LET p_mensaje = "Usuario      : ",p_usuario,"\n",
                   "ID Proceso   : ",p_pid, "\n", 
                   "Proceso      : AFILIACION - GENERACIÓN ARCHIVO DERECHOHABIENTES - DOMICILIOS","\n",
                   "Archivo      : ",p_nombre_archivo USING "#########&","\n",
                   "Fecha Inicio : ",TODAY, "\n"

   -- no se necesita folio
   LET p_folio = 0

   DISPLAY " SE CREA TABLA TEMPORAL"
   CALL fn_crea_tabla_temporal()

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "GENERACIÓN ARCHIVO")

   DISPLAY " SE EJECUTA LA EXTRACCIÓN"

   -- se crea la sentencia que ejecuta función de extracción de información
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_afi_extraccion_dh_dom()"

   PREPARE prp_extracc_acred FROM v_s_qryTxt
   EXECUTE prp_extracc_acred INTO r_si_cod_error,
                                  r_i_isam_error,
                                  r_v_msj_error

   IF r_si_cod_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY " ERROR EN EL PROCESO DE EXTRACCIÓN DE DERECHOHABIENTES - DOMICILIOS"
      DISPLAY "  COD ERR    : ",r_si_cod_error
      DISPLAY "  ISAM ERR   : ",r_i_isam_error
      DISPLAY "  MENSAJE ERR: ",r_v_msj_error

      -- se invoca la función que deja la operación en estado ERRONEA
      LET v_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

      -- se verifica si fue posible marcar la operacion como Erronea
      IF v_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(v_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " SE CREA ARCHIVO DE EXTRACCIÓN"

   EXECUTE IMMEDIATE "SET PDQPRIORITY HIGH"
   
   LET v_consulta_sd = "SELECT d.* \n",
                       "FROM safre_tmp:tmp_dh_dom d"

   LET v_consulta_nd = "SELECT af.rfc, \n",
                       "       af.curp, \n",
                       "       TRIM(af.ap_paterno_af), \n",
                       "       TRIM(af.ap_materno_af), \n",
                       "       TRIM(af.nombre_af), \n",
                       "       af.nss \n",
                       "  FROM afi_derechohabiente af \n",
                       " WHERE af.id_derechohabiente NOT IN( \n",
                       " SELECT dm.id_derechohabiente \n",
                       "   FROM afi_domicilio dm )" 

   -- se crea el objeto
   LET v_archivo = base.Channel.create()

   -- se obtiene la ruta de envio
   SELECT ruta_envio
     INTO v_ruta_salida
     FROM seg_modulo
    WHERE modulo_cod = "afi"
   
   -- se conforma la ruta del archivo de salida
   LET v_mensaje = TODAY USING "yyyymmdd"
   LET v_ruta_archivo = v_ruta_salida CLIPPED, "/", p_nombre_archivo CLIPPED
   
   -- se abre el archivo para su escritura
   CALL v_archivo.openFile( v_ruta_archivo, "w" )
   CALL v_archivo.setDelimiter("")

   PREPARE sid_dh_dom FROM v_consulta_sd
   DECLARE cur_dh_dom CURSOR FOR sid_dh_dom

   -- se inicia el contador
   LET v_conteo_registros = 0
   
   FOREACH cur_dh_dom INTO v_afi_dh_dom.*

      LET v_s_registro = v_afi_dh_dom.rfc       ,
                         v_afi_dh_dom.curp      ,
                         v_afi_dh_dom.nombre    ,
                         v_afi_dh_dom.nss       ,
                         v_afi_dh_dom.domicilio ,
                         v_afi_dh_dom.localidad ,
                         v_afi_dh_dom.estado    ,
                         v_afi_dh_dom.municipio ,
                         v_afi_dh_dom.cp

      -- se escribe el registro en el archivo
      CALL v_archivo.write([v_s_registro])

      -- se cuenta un registro escrito
      LET v_conteo_registros = v_conteo_registros + 1
   END FOREACH

   PREPARE sid_dh FROM v_consulta_nd
   DECLARE cur_dh CURSOR FOR sid_dh

   FOREACH cur_dh INTO v_afi_dh.rfc,
                       v_afi_dh.curp,
                       v_paterno,
                       v_materno,
                       v_nombre,
                       v_afi_dh.nss

      LET v_afi_dh.nombre = v_paterno," ",v_materno," ",v_nombre

      LET v_s_registro = v_afi_dh.rfc       ,
                         v_afi_dh.curp      ,
                         v_afi_dh.nombre    ,
                         v_afi_dh.nss       ,
                         v_afi_dh.domicilio

      -- se escribe el registro en el archivo
      CALL v_archivo.write([v_s_registro])

      -- se cuenta un registro escrito
      LET v_conteo_registros = v_conteo_registros + 1
   END FOREACH

   -- se devuelve la priodidad como estaba
   EXECUTE IMMEDIATE "SET PDQPRIORITY LOW"
 
   -- se cierra el archivo
   CALL v_archivo.close()

   DISPLAY " FECHA FIN : ", TODAY, "\n"

   LET p_mensaje = p_mensaje,"\n","Fecha Fin    : ", TODAY, "\n"

   -- se complementa el mensaje
   LET p_mensaje = p_mensaje || v_mensaje

   LET v_mensaje = "\nSe ha concluído la generación de archivo de derechohabientes - domicilios.\n",
                   "Archivo: ", v_ruta_archivo, "\n",
                   "Num. de registros: ", v_conteo_registros USING "########&","\n"

   DISPLAY v_mensaje

   -- se finaliza la operacion   
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING v_resultado

   LET p_titulo = "Finalización de operación - ARCHIVO DE DERECHOHABIENTES - DOMICILIOS"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(p_pid, p_proceso_cod, p_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   -- se envía la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "GENERACIÓN ARCHIVO")

END MAIN

FUNCTION fn_crea_tabla_temporal()
#fctt----------------------------

   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   -- se elimina la tabla en dado caso de que exista
   LET v_s_qryTxt = " DROP TABLE IF EXISTS tmp_dh_dom;"

   PREPARE prp_drop_extrac_acred FROM v_s_qryTxt
   EXECUTE prp_drop_extrac_acred

   -- se crea la tabla para la generacion del reporte y del archivo de salida
   LET v_s_qryTxt = " CREATE TABLE tmp_dh_dom (\n",
                    "               rfc       CHAR(13), \n",
                    "               curp      CHAR(18), \n",
                    "               nombre    CHAR(120),\n",
                    "               nss       CHAR(11), \n",
                    "               domicilio CHAR(90), \n",
                    "               localidad CHAR(50), \n",
                    "               estado    CHAR(50), \n",
                    "               municipio CHAR(50), \n",
                    "               cp        CHAR(5)   ) IN tmp_4_dbs lock mode row;"

   PREPARE prp_create_extrac_acred FROM v_s_qryTxt
   EXECUTE prp_create_extrac_acred

   -- regresa a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION
