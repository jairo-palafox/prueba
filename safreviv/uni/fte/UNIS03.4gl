--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08/01/2013
--===============================================================

################################################################################
#Modulo            => UNI                                                      #
#Programa          => UNIS03                                                   #
#Objetivo          => Programa que ejecuta el proceso de generación de archivo #
#                     de registros rechazados en la integración                #
#Fecha inicio      => 08/01/2013                                               #
################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
   DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
          g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
          v_layout       LIKE cat_operacion.layout_cod,
          v_ruta_rescate STRING,
          v_usuario      LIKE seg_modulo.usuario,
          v_proceso_desc LIKE cat_proceso.proceso_desc,
          v_extension    LIKE cat_operacion.extension,
          v_opera_desc   LIKE cat_operacion.opera_desc,
          v_ruta_listados     LIKE seg_modulo.ruta_listados
END GLOBALS

MAIN
   DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
          p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
          p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
          p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
          p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
          p_mensaje_int    STRING, --Mensaje resultado de integración
          v_s_sql          STRING, -- cadena con una instruccion SQL
          v_i_resultado    INTEGER, -- resultado del proceso
          r_bnd_fin_oper   SMALLINT

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_mensaje_int    = ARG_VAL(7)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod)
      RETURNING v_proceso_desc,
                v_extension,
                v_opera_desc,
                v_layout,
                v_ruta_rescate,
                v_ruta_listados,
                v_usuario

   -- se asigna proceso y operacion
   LET g_pid         = p_pid
   LET g_proceso_cod = p_proceso_cod -- Unificación de cuenta
   LET g_opera_cod   = p_opera_cod   -- genera archivo OCI

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

   ---- Inicio operacion.
   CALL fn_genera_archivo_rechazos(p_usuario_cod, p_folio, p_mensaje_int)

   -- se invoca la finalizacion de la operacion
   --CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               --g_proceso_cod, --- Clave del proceso
                               --g_opera_cod)   --- Clave de la operación
      --RETURNING r_bnd_fin_oper

END MAIN
{
======================================================================
Clave:
Nombre: fn_genera_archivo_rechazos
Fecha creacion: 06/06/2012
Narrativa del proceso que realiza:
Genera el archivo de salida para uni solo INFONAVIT
======================================================================
}
FUNCTION fn_genera_archivo_rechazos(p_usuario_cod, p_folio, p_mensaje_int)
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_folio             LIKE glo_folio.folio,         -- folio para preliquidar
          r_bnd_fin_oper      SMALLINT,
          p_titulo            STRING,                       -- titulo del mensaje enviado en el correo
          p_mensaje           STRING,                       -- cuerpo del mensaje enviado
          v_v_nom_archivo     CHAR(40),                     -- nombre del archivo de salida
          v_v_ruta_nomarch    VARCHAR(100),                 -- ruta y nombre del archivo de salida
          v_c_ruta_env_uni    LIKE seg_modulo.ruta_envio,   -- ruta donde se colocara el archivo
          v_ch_arch_solTransf BASE.CHANNEL,                 -- manejador de apuntador hacia archivo
          v_s_sql             VARCHAR(1000),                -- Cadena que contiene las consultas a ejecutar
          v_s_registro        STRING, -- registro a insertar
          cont_registros      SMALLINT,
          v_d_hoy             CHAR(8),
          v_busca_archivo     STRING,
          v_cont_dia          SMALLINT, -- consecutivo por dia de archivo generado
          v_reg_dia           SMALLINT,
          p_mensaje_int       STRING --resultado de la integración
          
DEFINE v_rec_detalles RECORD
          tpo_movimiento   CHAR(2),      
          espacios         CHAR(2),
          nrp              CHAR(11),
          fecha_movimiento CHAR(8),
          curp_rfc         CHAR(18),
          tpo_trabajador   SMALLINT,
          nss              CHAR(11),
          nombre           CHAR(50),
          presentacion_ext DECIMAL(1,0),
          jornada_o_semana DECIMAL(1,0),
          sdi              DECIMAL(6,0),
          sexo             DECIMAL(1,0),
          nss_correcto     CHAR(11),
          nombre_correcto  CHAR(50)
END RECORD

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".UNIS03.log")

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
     INTO v_c_ruta_env_uni
     FROM seg_modulo
    WHERE modulo_cod = 'uni'
    --/safreviv_int/uni/envio/


   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "RCH"||v_d_hoy

   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_uni,
                               v_busca_archivo)
      RETURNING v_cont_dia

   LET v_reg_dia = v_cont_dia USING "&&"
   LET v_v_nom_archivo = v_busca_archivo||v_reg_dia||".rchinf"
   LET v_v_ruta_nomarch = v_c_ruta_env_uni CLIPPED || v_v_nom_archivo

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_registros = 1
   -- Se llenan los detalles del archivo
   LET v_s_sql = "\n SELECT a.*",
                 "\n FROM   safre_tmp:tmp_mov_afi_trab_op21 a",
                 "\n WHERE  a.diagnostico = 2",
                 "\n GROUP BY 5,7,1,2,3,4,6,8,9,10,11,12,13,14,15",
                 "\n ORDER BY 5,7,1,2,3,4,6,8,9,10,11,12,13,14,15"

   --DISPLAY "v_s_sql_genera: ", v_s_sql CLIPPED
   PREPARE prp_reg_unificacion FROM v_s_sql CLIPPED
   DECLARE cur_reg_unificacion CURSOR FOR prp_reg_unificacion

   -- Se llena el detalle del archivo
   FOREACH Cur_reg_unificacion INTO v_rec_detalles.*

      -- Se llena la cadena para escribir en el archivo
      LET v_s_registro = v_rec_detalles.tpo_movimiento,
                         v_rec_detalles.espacios,
                         v_rec_detalles.nrp,
                         v_rec_detalles.fecha_movimiento,--23
                         v_rec_detalles.curp_rfc,
                         v_rec_detalles.tpo_trabajador USING "&",
                         v_rec_detalles.nss,  
                         v_rec_detalles.nombre,
                         v_rec_detalles.presentacion_ext USING "&",
                         v_rec_detalles.jornada_o_semana USING "&",
                         v_rec_detalles.sdi              USING "&&&&&&",
                         v_rec_detalles.sexo             USING "&",
                         v_rec_detalles.nss_correcto,
                         v_rec_detalles.nombre_correcto

      LET cont_registros = cont_registros + 1

      CALL v_ch_arch_solTransf.write([v_s_registro])
   END FOREACH

   IF cont_registros = 1 THEN
      DISPLAY "No existe información para generar el archivo"
      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)
         RETURNING r_bnd_fin_oper
         
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de generación de archivo no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   ELSE

      LET cont_registros = cont_registros - 1

      DISPLAY p_mensaje_int
      
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "#   \n",
                      "#   Se creó un archivo de rechazos satisfactoriamente \n",
                      "#  \n",
                      "#   Ruta y nombre del archivo: ",v_v_ruta_nomarch,"\n",
                      "#   \n",
                      "#  Total de registros rechazados : "||cont_registros,"\n",
                      "#  \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   END IF

   CALL v_ch_arch_solTransf.close()

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - GENERA ARCHIVO"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END FUNCTION --fn_archivo_salida_OCI

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,p_busca_nom_archivo)
   DEFINE p_ruta_envio     LIKE seg_modulo.ruta_envio,
          p_busca_nom_archivo  VARCHAR(40),
          v_cmd                STRING,
          v_consecutivo        INTEGER
          
   DEFINE fn CHAR(19)
   DEFINE ch base.Channel

   LET v_cmd = "ls -lrt ",p_ruta_envio CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

   LET ch = base.Channel.create()
   CALL ch.setDelimiter(".")
   CALL ch.openPipe(v_cmd,"r")
   WHILE ch.read([fn])
      LET v_consecutivo = fn[12,13]
   END WHILE
   CALL ch.close()
   LET v_consecutivo = v_consecutivo + 1

   IF length(v_consecutivo) = 0 THEN
      LET v_consecutivo = 1
   END IF

   RETURN v_consecutivo

END FUNCTION


