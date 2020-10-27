--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2012
--===============================================================

####################################################################
#Modulo            =>UNI                                           #
#Programa          =>UNIS01                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo solo IMSS                          #
#Fecha inicio      => 06/06/2012                                   #
####################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid           LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod   LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de operacion
       v_layout        LIKE cat_operacion.layout_cod,
       v_ruta_rescate  STRING,
       v_usuario       LIKE seg_modulo.usuario,
       v_proceso_desc  LIKE cat_proceso.proceso_desc,
       v_extension     LIKE cat_operacion.extension,
       v_opera_desc    LIKE cat_operacion.opera_desc,
       v_ruta_listados LIKE seg_modulo.ruta_listados,
       v_fecha_ini     DATE,
       v_fecha_fin     DATE
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
       r_bnd_fin_oper   SMALLINT,
       p_fecha_ini      DATE,
       p_fecha_fin      DATE

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_fecha_ini      = ARG_VAL(7)
   LET p_fecha_fin      = ARG_VAL(8)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario

   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod
   LET g_opera_cod   = p_opera_cod

   LET v_fecha_ini = p_fecha_ini
   LET v_fecha_fin = p_fecha_fin

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   DISPLAY "##############################################################"
   DISPLAY "# Parámetros de Consulta para Generar Archivo de Rechazados "
   DISPLAY "# " 
   DISPLAY "# FECHA INICIAL : ", p_fecha_ini USING "dd-mm-yyyy"
   DISPLAY "# " 
   DISPLAY "# FECHA FINAL   : ", p_fecha_fin USING "dd-mm-yyyy"
   DISPLAY "# " 
   DISPLAY "# FOLIO LIQUIDA : ", p_folio
   DISPLAY "##############################################################"
    
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida_trm(p_usuario_cod, p_folio, p_fecha_ini, p_fecha_fin)

   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bnd_fin_oper     
END MAIN

#OBJETIVO: Armar el archivo de salida de acuerdo a la estructura del layout
FUNCTION fn_archivo_salida_trm(p_usuario_cod, p_folio, p_fecha_ini, p_fecha_fin)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio, v_folio    LIKE glo_folio.folio, -- folio para preliquidar
       p_fecha_ini         DATE, 
       p_fecha_fin         DATE,
       r_bnd_fin_oper      SMALLINT,
       p_titulo            STRING, -- titulo del mensaje enviado en el correo
       p_mensaje           STRING, -- cuerpo del mensaje enviado
       v_v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_uni    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_sql             STRING,-- Cadena que contiene las consultas a ejecutar
       --
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
       v_nss_unificado     CHAR(11),
       v_nrp               CHAR(11),
       --Variables para el sumario
       v_tpo_registro_3    CHAR(1),
       --
       v_s_registro        STRING, -- registro a insertar
       cont_registros      INTEGER,
       v_cont_con_credito  INTEGER,
       v_cont_sin_credito  INTEGER,
       v_d_hoy             CHAR(8),
       v_busca_archivo     STRING,
       v_cont_dia          SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia           SMALLINT,
       v_resultado         INTEGER, 
       v_isam              INTEGER,
       v_nss               CHAR(11)

DEFINE v_r_det_unificador  RECORD
          v_rec_id_unificador      DECIMAL(9,0),
          v_rec_folio_unificacion  DECIMAL(9,0),
          v_rec_id_derechohabiente DECIMAL(9,0),
          v_rec_tipo_registro      CHAR(2),
          v_rec_contador_servicio  DECIMAL(9,0),
          v_rec_tipo_entidad_soli  CHAR(2),
          v_rec_cve_entidad_solic  CHAR(3),
          v_rec_tipo_entidad_unif  CHAR(2),
          v_rec_clave_entidad_uni  CHAR(3),
          v_rec_curp_unificador    CHAR(18),
          v_rec_nss_unificador     CHAR(11),
          v_rec_rfc_unificador     CHAR(13),
          v_rec_paterno_unificador CHAR(40),
          v_rec_materno_unificador CHAR(40),
          v_rec_nombre_unificador  CHAR(40),
          v_rec_nombre_imssunific  CHAR(50),
          v_rec_sexo_unificador    CHAR(1),
          v_rec_entidad_nacunific  CHAR(2),
          v_rec_f_nac_unificador   DATE,
          v_rec_tpo_docto_probato  CHAR(1),
          v_rec_clave_afore_recep  CHAR(3),
          v_rec_numero_cuentasasoc DECIMAL(2),
          v_rec_estatus_convocato  CHAR(1),
          v_rec_resultado_operaci  CHAR(2),
          v_rec_ident_movimiento   CHAR(2),
          v_rec_estatus_traspaso   CHAR(2),
          v_rec_estatus_retiro     CHAR(2),
          v_rec_cve_afore_aclarac  CHAR(3),
          v_rec_ind_credito43bis   CHAR(1),
          v_rec_estado_familia     SMALLINT,
          v_rec_estado_unificacion SMALLINT,
          v_rec_diagnostico        SMALLINT,
          v_rec_f_liquidacion      DATE,
          v_rec_f_notificacion     DATE,
          v_rec_f_aplicacion       DATE,
          v_rec_folio_liquidacion  DECIMAL(9,0),
          v_rec_nss_unificado      CHAR(11)
END RECORD

DEFINE v_fecha_unificacion DATE,
       v_QryTxt            STRING
       
   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".UNIS12.log")

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_uni
   FROM   seg_modulo
   WHERE  modulo_cod = 'uni'

    -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "YYYYMMDD"
   LET v_busca_archivo = "UNI_RTRM"||v_d_hoy 
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_uni,v_busca_archivo)
        RETURNING v_cont_dia
   LET v_reg_dia = v_cont_dia USING "&&"
   LET v_v_nom_archivo = v_busca_archivo||v_reg_dia||".uni"
   LET v_v_ruta_nomarch = v_c_ruta_env_uni CLIPPED || "/" ||v_v_nom_archivo
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
   LET v_s_sql = "\n SELECT a.*, c.nsscta1",
                 "\n FROM   uni_det_unificador a,",
                 "\n        uni_det_unificado c",
                 "\n WHERE  a.id_unificador = c.id_unificador",             
                 "\n AND    a.folio_unificacion = c.folio_unificacion",
                 "\n AND    a.estado_familia = 2",
                 "\n AND    a.folio_unificacion IN (SELECT folio 
                                                 \n FROM   glo_folio 
                                                 \n WHERE  f_actualiza BETWEEN ", "'", v_fecha_ini,"'",
                                               " \n AND ", "'", v_fecha_fin,"'", " ) " 
--DISPLAY v_s_sql                 

   IF p_folio <> 0 THEN 
      LET v_s_sql = v_s_sql || "\n AND    a.folio_liquidacion = ",v_folio   
   END IF

--   DISPLAY "v_s_sql_genera: ", v_s_sql CLIPPED
   PREPARE prp_registros_unificacion FROM v_s_sql CLIPPED
   DECLARE cur_registros_unificacion CURSOR FOR prp_registros_unificacion
   
   -- Se llena el primer registro encabezado
   LET v_tpo_registro_0 = "0"
   LET v_fec_del_dia = TODAY
   -- Se llena la cadena para escribir en el archivo
   LET v_s_registro = v_tpo_registro_0,
                      v_fec_del_dia USING "YYYYMMDD"
   
   CALL v_ch_arch_solTransf.write([v_s_registro])
   
   LET v_id_credito = 0
   LET v_cont_con_credito = 0
   LET v_cont_sin_credito = 0
   -- Se llena el detalle del archivo
   FOREACH cur_registros_unificacion INTO v_r_det_unificador.*

      LET v_QryTxt = "EXECUTE FUNCTION fn_uni_consulta_creditos(?,?,?,?)"
      PREPARE prp_consulta_creditos FROM v_QryTxt 
      EXECUTE prp_consulta_creditos USING v_r_det_unificador.v_rec_id_derechohabiente, 
                                          v_r_det_unificador.v_rec_id_unificador,
                                          v_num_credito,
                                          v_r_det_unificador.v_rec_folio_unificacion
                                    INTO  v_tpo_unificacion ,
                                          v_tpo_credito     ,
                                          v_num_credito     ,
                                          v_tpo_dscto       ,
                                          v_valor_dscto     , 
                                          v_nrp             ,
                                          v_nss             ,
                                          v_resultado       , 
                                          v_isam            ;

      IF v_resultado <> 0 THEN
         DISPLAY "Ha ocurrido un error al consultar los créditos " 
         DISPLAY "Error : ", v_resultado
         DISPLAY "ISAM  : ", v_isam
         DISPLAY "NSS   : ", v_nss
      END IF
                                          
      --Si el folio no es de carga inicial se consulta la fecha de unificación
      IF v_r_det_unificador.v_rec_folio_unificacion = 3339 THEN 
         LET v_fecha_unificacion = v_r_det_unificador.v_rec_f_liquidacion
      ELSE
         SELECT f_actualiza
         INTO   v_fecha_unificacion  
         FROM   glo_folio
         WHERE  folio = v_r_det_unificador.v_rec_folio_unificacion;
      END IF
--DISPLAY v_fecha_unificacion
      --Si el núm. de cuentas asociadas es nulo le asigna valor cero
      IF v_r_det_unificador.v_rec_numero_cuentasasoc IS NULL THEN 
         LET v_r_det_unificador.v_rec_numero_cuentasasoc = 0
      END IF 

      --Si el NRP es nulo le asigna el 0 para que tome la máscara
      IF v_nrp IS NULL THEN 
         LET v_nrp = 0
      END IF

      IF v_tpo_unificacion = "88" THEN 
         LET v_cont_sin_credito = v_cont_sin_credito + 1 
      ELSE
         LET v_cont_con_credito = v_cont_con_credito + 1 
      END IF
      -- Asigna valores en codigo duro según especificación
      LET v_tpo_registro_1 = "1"
      -- Se llena la cadena para escribir en el archivo
      LET v_s_registro = v_tpo_registro_1,
                         v_tpo_unificacion,
                         v_r_det_unificador.v_rec_nss_unificador,
                         v_num_credito USING "&&&&&&&&&&",
                         v_fecha_unificacion USING "YYYYMMDD",
                         v_r_det_unificador.v_rec_numero_cuentasasoc USING "&&",
                         v_fecha_unificacion USING "YYYYMMDD",
                         v_tpo_credito USING "&&&",
                         v_estatus_credito USING "&&&",
                         v_tpo_dscto USING "&",
                         v_valor_dscto * 1000 USING "&&&&&&&&",
                         v_nrp USING "&&&&&&&&&&&",
                         --v_nss_unificado
                         v_r_det_unificador.v_rec_nss_unificado

      LET cont_registros = cont_registros + 1

      CALL v_ch_arch_solTransf.write([v_s_registro])

   END FOREACH

   --Tipo de registro 3    
   LET v_tpo_registro_3 = "3"
   LET v_s_registro = v_tpo_registro_3,
                      v_cont_sin_credito USING "&&&&&&&&&",
                      v_cont_con_credito USING "&&&&&&&&&",
                      v_fec_del_dia USING "YYYYMMDD",
                      cont_registros USING "&&&&&&&&&",
                      cont_registros USING "&&&&&&&&&"
   
   CALL v_ch_arch_solTransf.write([v_s_registro])                   
   
   IF cont_registros = 1 THEN
      DISPLAY "No existe información para generar el archivo"
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                  RETURNING r_bnd_fin_oper
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de generación de archivo no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   ELSE
      --Actualiza nombre de archivo en monitor de procesos.
      UPDATE bat_ctr_operacion 
      SET    nom_archivo = v_v_nom_archivo
      WHERE  pid = g_pid
      AND    opera_cod = 1;
      
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "#   \n ",
                      "#  El archivo se creo satisfactoriamente \n",
                      "#  \n",
                      "#  Ruta y nombre del archivo: ",v_v_ruta_nomarch,"\n",
                      "#   \n",
                      "#  Total con credito : "||v_cont_con_credito,"\n",
                      "#  Total sin credito : "||v_cont_sin_credito,"\n",
                      "#  Total de registros  : "||cont_registros,"\n",
                      "#  \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   END IF

   CALL v_ch_arch_solTransf.close()
   
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - GENERA ARCHIVO TRM"
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   
END FUNCTION --fn_archivo_salida

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,p_busca_nom_archivo)
DEFINE p_ruta_envio     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(22)
DEFINE ch base.Channel

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
