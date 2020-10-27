--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23/09/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIS13                                                        #
#Objetivo     => Lanzado generar archivo salida                                #
#Fecha inicio => 23/09/2015                                                    #
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
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       r_bnd_fin_oper   SMALLINT

   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(g_proceso_cod, g_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

      CALL fn_archivo_salida(p_usuario_cod, p_folio)
      
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
                                  g_proceso_cod, --- Clave del proceso
                                  g_opera_cod) --- Clave de la operación
           RETURNING r_bnd_fin_oper

END MAIN

#OBJETIVO: Genera el archivo de salida para uni solo IMSS
FUNCTION fn_archivo_salida(p_usuario_cod, p_folio)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio             LIKE glo_folio.folio, 
       v_folio_uni_resp    LIKE glo_folio.folio, 
       r_bnd_fin_oper      SMALLINT,
       p_titulo            STRING, -- titulo del mensaje enviado en el correo
       p_mensaje           STRING, -- cuerpo del mensaje enviado
       v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_uni    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_sql             VARCHAR(1000),-- Cadena que contiene las consultas a ejecutar
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
       v_valor_dscto       DECIMAL(12,4),
       v_tpo_unificacion   CHAR(2),
       v_num_credito       DECIMAL(10,0),
       v_r_det_unificador  RECORD LIKE uni_det_unificador.*,
       v_nss_unificado     CHAR(11),
       v_id_unificado      DECIMAL(9,0),
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
       v_reg_dia           SMALLINT
DEFINE r_resultado         SMALLINT,
       r_tpo_originacion   SMALLINT,
       r_tpo_credito       SMALLINT,
       r_num_credito       DECIMAL(10,0),
       r_f_otorga          DATE,
       r_f_liquida         DATE,
       v_nombre_destino    STRING,
       v_nom_arch          STRING,
       v_copia_archivo     STRING,
       v_ejecuta_sh        STRING

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| "UNIS13.log")

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_c_ruta_env_uni
   FROM seg_modulo
   WHERE modulo_cod = 'uni'
     
   LET v_s_sql = "   SELECT folio_referencia",
                 "\n FROM   glo_folio",
                 "\n WHERE  folio = ",p_folio
   --DISPLAY v_s_sql 
   PREPARE Prpr_Obt_folio FROM v_s_sql CLIPPED
   EXECUTE Prpr_Obt_folio INTO v_folio_uni_resp    

    -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "YYYYMMDD"
   LET v_busca_archivo = "UNI_CNMU"||v_d_hoy 
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_uni,v_busca_archivo)
        RETURNING v_cont_dia
   LET v_reg_dia = v_cont_dia USING "&&"
   LET v_nom_archivo = v_busca_archivo||v_reg_dia||".uni"
   LET v_v_ruta_nomarch = v_c_ruta_env_uni CLIPPED || "/" || v_nom_archivo
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
   LET v_s_sql = "\n SELECT a.*, c.nsscta1, c.id_unificado",
                 "\n FROM   uni_det_unificador a,",
                 "\n        uni_det_unificado c",
                 "\n WHERE  a.folio_unificacion = ",v_folio_uni_resp ,
                 "\n AND    a.id_unificador = c.id_unificador",
                 "\n AND    a.folio_unificacion = c.folio_unificacion",
                 "\n AND    a.estado_familia = 1",
                 --"\n AND    a.diagnostico = 5 ",
                 "\n UNION ",
                 "\n SELECT a.*, c.nsscta1, c.id_unificado",
                 "\n FROM   uni_det_unificador a,",
                 "\n        uni_det_unificado c",
                 "\n WHERE  a.folio_unificacion IN (SELECT folio_unificacion ", 
                 "\n                                FROM   uni_det_procedencia ", 
                 "\n                                WHERE  folio_resp_confronta = " ,v_folio_uni_resp    ,
                 "\n                                AND    ind_procedencia      = 1",
                 "\n                                GROUP BY 1)",
                 "\n AND    a.id_unificador = c.id_unificador",
                 "\n AND    a.folio_unificacion = c.folio_unificacion",
                 "\n AND    a.estado_familia = 1"
                 --"\n AND    a.diagnostico = 5 "

  -- DISPLAY "v_s_sql_genera: ", v_s_sql CLIPPED
   PREPARE Prpr_reg_unificacion FROM v_s_sql CLIPPED
   DECLARE Cur_reg_unificacion CURSOR FOR Prpr_reg_unificacion

   -- Se llena el primer registro encabezado
   LET v_tpo_registro_0 = "0"
   LET v_fec_del_dia = TODAY
   -- Se llena la cadena para escribir en el archivo
   LET v_s_registro = v_tpo_registro_0,
                      v_fec_del_dia USING "YYYYMMDD"

   CALL v_ch_arch_solTransf.write([v_s_registro])

   LET v_id_credito = 0

   -- Se llena el detalle del archivo
   FOREACH Cur_reg_unificacion INTO v_r_det_unificador.*, v_nss_unificado, v_id_unificado
      CALL fn_credito_vivienda(v_r_det_unificador.id_derechohabiente, 0) 
      RETURNING  r_resultado,
                 r_tpo_originacion,
                 r_tpo_credito,
                 r_num_credito,
                 r_f_otorga,
                 r_f_liquida

      IF r_resultado IS NULL THEN
         LET r_resultado = 0
      END IF

      -- 19/06/2014
      -- 1 No tiene crédito
      -- 0 Si tiene crédito
      IF r_resultado = 0 THEN
         LET v_tpo_unificacion = "99"
         LET v_tpo_dscto = 3 -- Valor unico
         LET v_cont_con_credito = v_cont_con_credito + 1

         DECLARE cur_tpo_cre CURSOR FOR SELECT c.tpo_credito,
                                               c.num_credito,
                                               c.valor_dscto
                                        FROM   cre_acreditado c, cat_maq_credito a
                                        WHERE  c.id_derechohabiente = v_r_det_unificador.id_derechohabiente 
                                        AND    c.estado             = a.estado
                                        AND    a.entidad            = 1
                                        ORDER BY c.edo_credito ASC
                                        
         FOREACH cur_tpo_cre INTO v_tpo_credito,
                                  v_num_credito,
                                  v_valor_dscto

            IF v_tpo_credito IS NULL THEN
               LET v_tpo_credito = 0
            END IF  
                  
            IF v_tpo_credito IS NULL THEN
               LET v_num_credito = 0
            END IF

            IF v_tpo_dscto IS NULL THEN
               LET v_tpo_dscto = 0
            END IF

            IF v_valor_dscto IS NULL THEN
               LET v_valor_dscto = 0              
            END IF
         END FOREACH  
      ELSE
         LET v_tpo_unificacion = "88"
         LET v_tpo_credito = 0
         LET v_num_credito = 0
         LET v_tpo_dscto = 0 -- Valor unico
         LET v_valor_dscto = 0
         LET v_cont_sin_credito = v_cont_sin_credito + 1
      END IF

      IF v_r_det_unificador.numero_cuentasasoc IS NULL THEN 
         LET v_r_det_unificador.numero_cuentasasoc = 0
      END IF

      -- Asigna valores en codigo duro según especificación
      LET v_tpo_registro_1 = "1"
      -- Se llena la cadena para escribir en el archivo
      LET v_s_registro = v_tpo_registro_1,
                         v_tpo_unificacion,
                         v_r_det_unificador.nss_unificador,
                         v_num_credito USING "&&&&&&&&&&",
                         v_fec_del_dia USING "YYYYMMDD",
                         v_r_det_unificador.numero_cuentasasoc USING "&&",
                         v_fec_del_dia USING "YYYYMMDD",
                         v_tpo_credito USING "&&&",
                         v_estatus_credito USING "&&&",
                         v_tpo_dscto USING "&",
                         v_valor_dscto * 1000 USING "&&&&&&&&",
                         v_nrp USING "&&&&&&&&&&&",
                         v_nss_unificado

      LET cont_registros = cont_registros + 1

      CALL v_ch_arch_solTransf.write([v_s_registro])

      UPDATE uni_det_unificador
      SET    diagnostico = 6 -- indicadores
      WHERE  diagnostico = 5 -- liquidados
      AND    id_unificador = v_r_det_unificador.id_unificador
      ;   
      UPDATE uni_det_unificado
      SET    diagnostico = 6 -- Indicadores
      WHERE  diagnostico = 5 -- Liquidados
      AND    id_unificador = v_r_det_unificador.id_unificador
      AND    id_unificado = v_id_unificado
   END FOREACH

   LET cont_registros = cont_registros - 1 

   LET v_tpo_registro_3 = "3"
   LET v_s_registro = v_tpo_registro_3,
                      v_cont_sin_credito USING "&&&&&&&&&",
                      v_cont_sin_credito USING "&&&&&&&&&",
                      v_fec_del_dia USING "YYYYMMDD",
                      cont_registros USING "&&&&&&&&&",
                      cont_registros USING "&&&&&&&&&"
   
   CALL v_ch_arch_solTransf.write([v_s_registro])                   
   
   IF cont_registros < 1 THEN
      DISPLAY "No existe información para generar el archivo"
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                                  g_proceso_cod, --- Clave del proceso
                                  g_opera_cod)   --- Clave de la operación
           RETURNING r_bnd_fin_oper
      LET p_mensaje = " --- ATENCIÓN ---\n",
                      " No existe información para generar el archivo.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   ELSE
      DISPLAY " "
      DISPLAY "  El archivo se creo satisfactoriamente"
      DISPLAY "    "
      DISPLAY "  Ruta y nombre del archivo: ",v_v_ruta_nomarch
      DISPLAY "    "
      DISPLAY "  Total con credito : ",v_cont_con_credito
      DISPLAY "  Total sin credito : ",v_cont_sin_credito
      DISPLAY "  Total de registros: ",cont_registros
      DISPLAY " "

      LET p_mensaje = "  \n ",
                      "  El archivo se creo satisfactoriamente \n",
                      "  \n",
                      "  Ruta y nombre del archivo: ",v_v_ruta_nomarch,"\n",
                      "  \n",
                      "  Total con credito  : "||v_cont_con_credito,"\n",
                      "  Total sin credito  : "||v_cont_sin_credito,"\n",
                      "  Total de registros : "||cont_registros,"\n"

-----------------------------------------------------
      LET v_d_hoy  = TODAY USING "YYYYMMDD"
      LET v_busca_archivo = "UNI_CNMU"||v_d_hoy

      --Obtine consecutivo para archivo por día
      CALL fn_crea_nombre_archivo(v_c_ruta_env_uni,v_busca_archivo)
           RETURNING v_cont_dia

      LET v_reg_dia = v_cont_dia USING "##"

      LET v_nom_arch = "UNI_IMSS"||v_d_hoy
      LET v_nombre_destino = v_nom_arch CLIPPED|| v_reg_dia||".txt"

      --LET v_nom_archivo = v_busca_archivo||v_reg_dia||".uni"
      --LET v_v_ruta_nomarch = v_c_ruta_env_uni CLIPPED || "/" || v_nom_archivo

      --LET v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".sdae"
--UNI_IMSSAAAAMMDDCC.txt
      -- Genera copia a archivo fijo
      LET v_copia_archivo = "\n cp "||v_c_ruta_env_uni CLIPPED||"/"||v_nom_archivo CLIPPED|| " " ||v_c_ruta_env_uni CLIPPED||"/"||v_nombre_destino CLIPPED 
      RUN v_copia_archivo 

      DISPLAY "\n Copia Archivo para Cartera: \n   ", v_copia_archivo

      LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/Transf_uni.sh"
      --RUN v_ejecuta_sh
      DISPLAY v_ejecuta_sh

      DISPLAY "\n Se ejecutó la transaferencia a cartera"
-----------------------------------------------------
   END IF

   CALL v_ch_arch_solTransf.close()

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - GENERA ARCHIVO"

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