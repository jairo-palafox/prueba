--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: Abril 3, 2018
--==============================================================================
################################################################################
#Modulo       => AGR                                                           #
#Programa     => AGRS14                                                        #
#Objetivo     => Generar el archivo de salida con los registros rechazados en  #
#                la integraci�n                                                # 
#Fecha inicio => 23/09/2015                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
   DEFINE g_pid               LIKE bat_ctr_proceso.pid,     --  ID del proceso
          g_proceso_cod       LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod         LIKE cat_operacion.opera_cod, -- codigo de operacion
          v_layout            LIKE cat_operacion.layout_cod,
          v_ruta_rescate      STRING,
          v_usuario           LIKE seg_modulo.usuario,
          v_proceso_desc      LIKE cat_proceso.proceso_desc,
          v_extension         LIKE cat_operacion.extension,
          v_opera_desc        LIKE cat_operacion.opera_desc,
          v_ruta_listados     LIKE seg_modulo.ruta_listados
   END GLOBALS
###############################################################################
MAIN
   DEFINE p_pid                  LIKE bat_ctr_operacion.pid,         -- PID del proceso
          p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
          p_opera_cod            LIKE bat_ctr_operacion.opera_cod,   -- codigo de la operacion
          p_usuario_cod          LIKE seg_usuario.usuario_cod,       -- clave del usuario firmado
          p_folio                DECIMAL(9,0),  -- folio de la operacion
          p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   
   LET p_usuario_cod          = ARG_VAL(1)
   LET g_pid                  = ARG_VAL(2)
   LET g_proceso_cod          = ARG_VAL(3)
   LET g_opera_cod            = ARG_VAL(4)
   LET p_folio                = ARG_VAL(5)
   LET p_nombre_archivo       = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod,p_opera_cod)
        RETURNING v_proceso_desc,
                  v_extension,
                  v_opera_desc,
                  v_layout,
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   ---- Inicio operacion.
   CALL fn_genera_archivo_rechazos(p_usuario_cod,p_folio)

END MAIN

#Generar archivo de rechazos en la integraci�n
FUNCTION fn_genera_archivo_rechazos(p_usuario_cod,p_folio)

   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_folio             DECIMAL(9,0),         -- folio para preliquidar, 
          r_bnd_fin_oper      SMALLINT,
          p_titulo            STRING,                       -- titulo del mensaje enviado en el correo
          p_mensaje           STRING,                       -- cuerpo del mensaje enviado
          v_v_nom_archivo     CHAR(40),                     -- nombre del archivo de salida
          v_v_ruta_nomarch    VARCHAR(100),                 -- ruta y nombre del archivo de salida
          v_c_ruta_env_uni    LIKE seg_modulo.ruta_envio,   -- ruta donde se colocara el archivo
          v_ch_arch_solTransf BASE.CHANNEL,                 -- manejador de apuntador hacia archivo
          v_s_sql             VARCHAR(1000),                -- Cadena que contiene las consultas a ejecutar
          --
          v_s_registro        STRING, -- registro a insertar
          cont_registros      INTEGER,
          v_d_hoy             CHAR(8),
          v_busca_archivo     STRING,
          v_cont_dia          SMALLINT, -- consecutivo por dia de archivo generado
          v_reg_dia           SMALLINT,
          p_tot_unificadores  INTEGER,
          p_tot_unificados    INTEGER,
          v_total_registro    INTEGER
                    
   DEFINE v_rec_detalles RECORD
          v_id_derechohabiente DECIMAL(9,0),
          v_nss                CHAR(11),
          v_aivs92             DECIMAL(12,2),
          v_aivs97             DECIMAL(12,2),
          v_periodo_pago       CHAR(6),
          v_tpo_solicitud      CHAR(2),
          v_marca_procesar     SMALLINT,
          v_cod_resultado      SMALLINT,
          v_diagnostico        SMALLINT,
          v_desc_estado        CHAR(40),
          v_separador          CHAR(1)
   END RECORD

   DEFINE v_s_aivs92           STRING,
          v_s_aivs97           STRING,
          v_s_periodo_pago     STRING,
          v_s_cod_resultado    STRING,
          v_s_desc_estado      STRING

   LET  v_total_registro = 0
   
   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".AGRS14.log")

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
   INTO   v_c_ruta_env_uni
   FROM   seg_modulo
   WHERE  modulo_cod = 'agr'

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "RCH_SEPS"||v_d_hoy

   --Obtine consecutivo para archivo por d�a
   CALL fn_crea_nombre_archivo(v_c_ruta_env_uni,
                               v_busca_archivo)
        RETURNING v_cont_dia

   LET v_reg_dia = v_cont_dia USING "&&"
   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia||".rchseps"
   LET v_v_ruta_nomarch = v_c_ruta_env_uni CLIPPED || v_v_nom_archivo

   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   LET cont_registros = 1

   LET v_s_sql = "\n SELECT a.id_derechohabiente, ",
                 "\n        b.nss, ",
                 "\n        a.aivs92, ",
                 "\n        a.aivs97, ",
                 "\n        a.periodo_pago, ",
                 "\n        a.tpo_solicitud, ",
                 "\n        a.marca_procesar, ",
                 "\n        a.cod_resultado, ",
                 "\n        a.diagnostico ",
                 "\n FROM   cre_solic_sdo a ",
                 "\n LEFT OUTER JOIN afi_derechohabiente b ",
                 "\n ON     a.id_derechohabiente = b.id_derechohabiente ",
                 "\n WHERE  a.folio = ", p_folio ,
                 "\n AND    a.cod_resultado = 2 ",
                 "\n GROUP BY 1,2,3,4,5,6,7,8,9",
                 "\n ORDER BY 2 "
--DISPLAY v_s_sql
   PREPARE prp_reg_unificacion FROM v_s_sql CLIPPED
   DECLARE cur_reg_unificacion CURSOR FOR prp_reg_unificacion

   LET v_rec_detalles.v_separador = "|"
   
   FOREACH Cur_reg_unificacion INTO v_rec_detalles.v_id_derechohabiente,
                                    v_rec_detalles.v_nss,
                                    v_rec_detalles.v_aivs92,
                                    v_rec_detalles.v_aivs97,
                                    v_rec_detalles.v_periodo_pago,
                                    v_rec_detalles.v_tpo_solicitud,
                                    v_rec_detalles.v_marca_procesar,
                                    v_rec_detalles.v_cod_resultado,
                                    v_rec_detalles.v_diagnostico

      SELECT desc_estado
      INTO   v_rec_detalles.v_desc_estado
      FROM   cat_rch_acreditado
      WHERE  estado = v_rec_detalles.v_diagnostico

      LET v_s_aivs92        = v_rec_detalles.v_aivs92
      LET v_s_aivs97        = v_rec_detalles.v_aivs97
      LET v_s_periodo_pago  = v_rec_detalles.v_periodo_pago
      LET v_s_cod_resultado = v_rec_detalles.v_cod_resultado
      LET v_s_desc_estado   = v_rec_detalles.v_desc_estado

      LET v_s_aivs92        = v_s_aivs92.trim()
      LET v_s_aivs97        = v_s_aivs97.trim()
      LET v_s_periodo_pago  = v_s_periodo_pago.trim() 
      --LET v_s_cod_resultado = v_s_cod_resultado.trim()
      LET v_s_desc_estado   = v_s_desc_estado.trim()

      LET v_s_registro = v_rec_detalles.v_nss,
                         v_rec_detalles.v_separador,
                         v_s_aivs92, --v_rec_detalles.v_aivs92,
                         v_rec_detalles.v_separador,
                         v_s_aivs97, --v_rec_detalles.v_aivs97,
                         v_rec_detalles.v_separador,
                         v_rec_detalles.v_periodo_pago,
                         v_rec_detalles.v_separador,
                         v_rec_detalles.v_tpo_solicitud,
                         v_rec_detalles.v_separador,
                         v_rec_detalles.v_marca_procesar USING "&&",
                         v_rec_detalles.v_separador,
                         v_rec_detalles.v_cod_resultado USING "&&",
                         v_rec_detalles.v_separador,
                         v_s_desc_estado, --v_rec_detalles.v_desc_estado,
                         v_rec_detalles.v_separador

      LET cont_registros = cont_registros + 1

      CALL v_ch_arch_solTransf.write([v_s_registro])
   END FOREACH

   IF cont_registros = 1 THEN
      DISPLAY "No existe informaci�n para generar el archivo"
      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)
           RETURNING r_bnd_fin_oper

      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de generaci�n de archivo no termin� correctamente.\n",
                      " C�digo de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   ELSE
   
      LET cont_registros = cont_registros - 1

      LET v_total_registro = p_tot_unificadores + p_tot_unificados;
      
      LET p_mensaje = "  La integraci�n se termin� completamente.","\n",
                      "  ","\n",
                      "  Integraci�n realizada con exito","\n",
                      "  Folio   : ",p_folio,"\n",
                      "  ","\n",
                      "  Total de registros          : ",v_total_registro,"\n",
                      "-------------------------------------------------------------------------------",
                      "   \n",
                      "   El archivo de rechazos se creo satisfactoriamente \n",
                      "  \n",
                      "   Ruta y nombre del archivo: ",v_v_ruta_nomarch,"\n",
                      "   \n",
                      "  Total de registros rechazados : "||cont_registros,"\n",
                      "  \n",
                      "  \n"

       DISPLAY p_mensaje

       CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
       RETURNING r_bnd_fin_oper

   END IF

   CALL v_ch_arch_solTransf.close()

   LET p_titulo = "Finalizaci�n de proceso - " || v_proceso_desc CLIPPED || " - GENERA ARCHIVO RECHAZOS"

   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "",               -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END FUNCTION --fn_archivo_salida
################################################################################
#Objetivo: genera el n�mero consecutivo por d�a para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,
                                p_busca_nom_archivo)
                                
   DEFINE p_ruta_envio         LIKE seg_modulo.ruta_envio,
          p_busca_nom_archivo  VARCHAR(40),
          v_cmd                STRING,
          v_consecutivo        INTEGER
          
   DEFINE fn CHAR(19),
          ch base.Channel

   LET v_cmd = "ls -lrt ",p_ruta_envio CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

   LET ch = base.Channel.create()
   
   CALL ch.setDelimiter(".")
   CALL ch.openPipe(v_cmd,"r")
   
   WHILE ch.read([fn])
      LET v_consecutivo = fn[17,18]
   END WHILE
   
   CALL ch.close()
   
   LET v_consecutivo = v_consecutivo + 1

   IF length(v_consecutivo) = 0 THEN
      LET v_consecutivo = 1
   END IF

   RETURN v_consecutivo

END FUNCTION