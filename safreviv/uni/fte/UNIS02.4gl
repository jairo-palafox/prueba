--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2012
--===============================================================

####################################################################
#Modulo            =>UNI                                           #
#Programa          =>UNIS01                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo solo INFONAVIT                          #
#Fecha inicio      => 06/06/2012                                   #
####################################################################
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

   CALL fn_archivo_salida_OCI(p_usuario_cod, p_folio)

   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               g_opera_cod)   --- Clave de la operación
      RETURNING r_bnd_fin_oper

END MAIN
{
======================================================================
Clave:
Nombre: fn_archivo_salida_OCI
Fecha creacion: 06/06/2012
Narrativa del proceso que realiza:
Genera el archivo de salida para uni solo INFONAVIT
======================================================================
}
FUNCTION fn_archivo_salida_OCI(p_usuario_cod, p_folio_unificacion)
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_folio_unificacion LIKE glo_folio.folio, 
          r_bnd_fin_oper      SMALLINT,
          p_titulo            STRING,                       -- titulo del mensaje enviado en el correo
          p_mensaje           STRING,                       -- cuerpo del mensaje enviado
          v_v_nom_archivo     CHAR(40),                     -- nombre del archivo de salida
          v_v_ruta_nomarch    VARCHAR(100),                 -- ruta y nombre del archivo de salida
          v_c_ruta_env_uni    LIKE seg_modulo.ruta_envio,   -- ruta donde se colocara el archivo
          v_ch_arch_solTransf BASE.CHANNEL,                 -- manejador de apuntador hacia archivo
          v_s_sql             VARCHAR(1000),                -- Cadena que contiene las consultas a ejecutar

          --Variables para el encabezado
          v_tpo_registro_0    CHAR(1),
          v_fec_del_dia       DATE,

          -- Variable para el detalle
          v_tpo_registro_1    CHAR(1),
          v_id_credito        SMALLINT,
          v_tpo_credito       SMALLINT,
          v_tpo_dscto         SMALLINT,
          v_estatus_credito   SMALLINT,
          v_valor_dscto       DECIMAL(8,4),
          v_tpo_unificacion   CHAR(2),
          v_num_credito       DECIMAL(10,0),
          v_r_inf_unificador  RECORD LIKE uni_inf_unificador.*,
          v_nss_unificado     CHAR(11),
          v_nrp               CHAR(11),
          --
          v_tpo_registro_3    CHAR(1),
          v_s_registro        STRING, -- registro a insertar
          cont_registros      SMALLINT,
          v_cont_con_credito  SMALLINT,
          v_cont_sin_credito  SMALLINT,
          v_d_hoy             CHAR(8),
          v_busca_archivo     STRING,
          v_cont_dia          SMALLINT, -- consecutivo por dia de archivo generado
          v_reg_dia           SMALLINT


   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".UNIS02.log")

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
     INTO v_c_ruta_env_uni
     FROM seg_modulo
    WHERE modulo_cod = 'uni'

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "RES"||v_d_hoy

   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_uni,
                               v_busca_archivo)
      RETURNING v_cont_dia

   LET v_reg_dia = v_cont_dia USING "&&"
   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia||".uninf"
   LET v_v_ruta_nomarch = v_c_ruta_env_uni CLIPPED || v_v_nom_archivo

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_registros = 1
   LET v_cont_con_credito = 0
   LET v_cont_sin_credito = 0

   -- Se llena el encabezado de la solicitud
   LET v_s_sql = "\n SELECT a.*,",
                 "\n        c.nss",
                 "\n FROM   uni_inf_unificador a,",
                 "\n        glo_folio b,",
                 "\n        uni_inf_unificado c",
                 "\n WHERE  a.folio_unificacion = b.folio",
                 "\n AND    a.folio_unificacion = c.folio_unificacion",
                 "\n AND    a.id_inf_unificador = c.id_unificador",
                 "\n AND    a.estado_familia = 1",
                 "\n AND    a.estado_unificacion = 1",
                 "\n AND    c.estado_unificacion = 1",
                 "\n AND    a.folio_unificacion = ",p_folio_unificacion,
                 "\n AND    b.proceso_cod = ",g_proceso_cod

   --DISPLAY "v_s_sql_genera: ", v_s_sql CLIPPED
   PREPARE Prpr_reg_unificacion FROM v_s_sql CLIPPED
   DECLARE Cur_reg_unificacion CURSOR FOR Prpr_reg_unificacion

   -- Se llena el primer registro encabezado
   LET v_tpo_registro_0 = "0"
   LET v_fec_del_dia = TODAY

   -- Se llena la cadena para escribir en el archivo
   LET v_s_registro = v_tpo_registro_0,
                      v_fec_del_dia USING "YYYYMMDD"

   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- Se llena el detalle del archivo
   FOREACH Cur_reg_unificacion INTO v_r_inf_unificador.*,
                                    v_nss_unificado
      SELECT id_credito
      INTO   v_id_credito
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = v_r_inf_unificador.id_derechohabiente

      IF v_id_credito <> 0 THEN
         LET v_tpo_unificacion  = "99"
         LET v_tpo_dscto        = 3      -- Valor unico
         LET v_cont_con_credito = v_cont_con_credito + 1

--display "ID_UNIFICADOR: ", v_r_inf_unificador.id_inf_unificador

         SELECT a.tpo_credito,
                a.num_credito,
                a.valor_dscto
         INTO   v_tpo_credito,
                v_num_credito,
                v_valor_dscto
         FROM   uni_cre_unificador a,
                cta_credito b
         WHERE  a.id_unificador = v_r_inf_unificador.id_inf_unificador
         AND    a.num_credito <> 0
         AND    a.tpo_credito = b.tpo_credito
         AND    a.num_credito = b.num_credito
         AND    a.folio_lote = p_folio_unificacion
         GROUP BY 1,2,3
      ELSE
         LET v_tpo_unificacion = "88"
         LET v_tpo_credito = 0
         LET v_num_credito = 0
         LET v_tpo_dscto = 0 -- Valor unico
         LET v_cont_sin_credito = v_cont_sin_credito + 1
      END IF

      -- Asigna valores en codigo duro según especificación
      LET v_tpo_registro_1 = "1"

      -- Se llena la cadena para escribir en el archivo
      LET v_s_registro = v_tpo_registro_1,
                         v_tpo_unificacion,
                         v_r_inf_unificador.nss,
                         v_num_credito USING "&&&&&&&&&&",
                         v_fec_del_dia USING "YYYYMMDD",
                        -- v_r_inf_unificador.numero_cuentasasoc,
                         v_fec_del_dia USING "YYYYMMDD",
                         v_tpo_credito USING "&&&",
                         v_estatus_credito USING "&&&",
                         v_tpo_dscto USING "&",
                         v_valor_dscto * 1000 USING "&&&&&&&&",
                         v_nrp USING "&&&&&&&&&&&",
                         v_nss_unificado

      LET cont_registros = cont_registros + 1

      CALL v_ch_arch_solTransf.write([v_s_registro])
   END FOREACH

   LET v_tpo_registro_3 = "3"
   LET v_s_registro = v_tpo_registro_3,
                      v_cont_sin_credito USING "&&&&&&&&&",
                      v_cont_sin_credito USING "&&&&&&&&&",
                      v_fec_del_dia USING "YYYYMMDD",
                      cont_registros USING "&&&&&&&&&",
                      cont_registros USING "&&&&&&&&&"

   CALL v_ch_arch_solTransf.write([v_s_registro])

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
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    "
      DISPLAY "El archivo se creo satisfactoriamente"
      DISPLAY "#    "
      DISPLAY "Ruta y nombre del archivo: ",v_v_ruta_nomarch
      DISPLAY "#    "
      DISPLAY "#  Total con credito : ",v_cont_con_credito
      DISPLAY "#  Total sin credito : ",v_cont_sin_credito
      DISPLAY "#  Total de registros: ",cont_registros
      --DISPLAY "#    "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"

      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "#   \n ",
                      "El archivo se creo satisfactoriamente \n",
                      "#  \n",
                      "Ruta y nombre del archivo: ",v_v_ruta_nomarch,"\n",
                      "#   \n",
                      "#  Total con credito : "||v_cont_con_credito,"\n",
                      "#  Total sin credito : "||v_cont_sin_credito,"\n",
                      "#  Total de registros  : "||cont_registros,"\n",
                      "#  \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"

      UPDATE uni_inf_unificador
         SET diagnostico = 6 -- liquidados
       WHERE diagnostico = 5 -- Aceptadas y liquidadas
         AND folio_unificacion = p_folio_unificacion

      UPDATE uni_inf_unificado
         SET diagnostico = 6 -- liquidados
       WHERE diagnostico = 5 -- Aceptadas y liquidadas
         AND folio_unificacion = p_folio_unificacion
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