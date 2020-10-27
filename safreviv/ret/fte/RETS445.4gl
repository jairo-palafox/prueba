--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            =>RET                                                                     #
#Programa          =>RETS445                                                                 #
#Objetivo          =>Genera los archivos de aceptados y rechazados de proceso de             #
#                    excepciones de la devolución del SSV                                    #
#Modificado                                                                                  #
##############################################################################################
IMPORT os

DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- nombre del usuario        
       p_folio             LIKE glo_folio.folio, -- numero de folio
       p_incluye_rechazos  SMALLINT, -- booleana que indica que se incluyan los rechazos
       p_es_previo         SMALLINT -- booleana. 1-es archivo previo. 0 es final

   -- se reciben los parametros del programa
   LET p_folio            = ARG_VAL(1)
   LET p_usuario_cod      = ARG_VAL(2)
   LET p_incluye_rechazos = ARG_VAL(3)
   LET p_es_previo        = ARG_VAL(4)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS445.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida( p_folio, p_usuario_cod, p_incluye_rechazos, p_es_previo)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida( p_folio, p_usuario_cod, p_incluye_rechazos, p_es_previo)
DEFINE p_folio                      LIKE glo_folio.folio,
       p_usuario_cod                LIKE seg_usuario.usuario_cod,      
       v_v_nom_archivo              STRING, -- nombre del archivo de salida
       v_v_nom_archivo_copia        STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch             STRING, -- ruta y nombre del archivo de salida
       v_v_ruta_nomarch_copia       STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr             LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_disposicion        base.channel, -- manejador de apuntador hacia archivo
       v_ch_arch_disposicion_copia  base.channel, -- manejador de apuntador hacia archivo
       p_incluye_rechazos           SMALLINT,
       p_es_previo                  SMALLINT,
       ls_sql                       STRING, -- cadena con enunciado SQL
       r_tmp_ret_det_disposicion    RECORD 
          num_delega                 SMALLINT,
          nss                        CHAR(11),                 
          beneficiario               CHAR(60),                 
          importe                    DECIMAL(14,2),                 
          entidad                    CHAR(02),
          juicio                     CHAR(10),                 
          num_acuerdo                CHAR(10),                 
          desc_juez                  CHAR(40),                  
          facultado                  CHAR(50),                  
          puesto                     CHAR(40),                  
          fch_ejecucion              DATE,                  
          procede_juicio             CHAR(40),                  
          tipo_sol                   CHAR(02),
          tipo_prod                  CHAR(02),
          correo_elec                CHAR(40),                  
          cod_rechazo                SMALLINT,                  
          desc_rechazo               CHAR(40)
       END RECORD


DEFINE v_s_encabezado         STRING,
       v_d_total_registro     INTEGER,
       v_dtotal_importe       LIKE ret_cza_disposicion.total_importe,
       v_c_usuario            LIKE ret_cza_disposicion.usuario,
       v_s_detalle            STRING,
       v_id_derechohabiente   LIKE ret_disposicion.id_derechohabiente,
       v_i_contador_registros INTEGER,
       v_s_sumario            STRING,
       v_archivo_original     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo original
       v_f_inicio_pension       CHAR(8),
       v_f_resolucion           CHAR(8),
       v_f_nacimiento           CHAR(8),
       v_f_solicitud_trab       CHAR(8),
       v_f_operacion_cza        CHAR(8),
       v_f_valor_transferencia  CHAR(8),
       v_id_ret_matriz_derecho  SMALLINT,
       v_fecha_formateada       CHAR(6), -- fecha en formato AAMMDD 
       v_dif_viv92              DECIMAL(8,6), -- para calcular diferencias entre lo solicitado y lo pagado
       v_dif_viv97              DECIMAL(8,6),
       v_archivo_transfer       STRING, -- cadena auxiliar
       v_ejecuta_sh             STRING,
       v_resultado_copia        INTEGER -- resultado de ejecutar la copia de archivo

   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch2       STRING,
      v_mensaje_archivo       STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_cuenta                INTEGER,
      v_solicitud             DECIMAL(9,0),
      v_tipo                  CHAR(1),
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_contador              INTEGER,
      v_query                 STRING,
      v_nss_paso              CHAR(11),
      v_suma_aivs_97          DECIMAL(18,6),
      v_i                     INTEGER

      
   --se inicializan las variables 
   INITIALIZE v_s_encabezado TO NULL
   INITIALIZE v_s_sumario TO NULL
   INITIALIZE v_s_detalle TO NULL
   LET v_i_contador_registros = 0 

   --DISPLAY "folio =  ",p_folio
   -- se obtienen la ruta envio del modulo
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

--   DISPLAY "el folio a procesar es >", p_folio, "<"
   
   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".exssv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Aceptados
   LET v_nom_archivo = "Detalle_Excepciones_Devol_SSV_Aceptados_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle 
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   LET v_mensaje_archivo = "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   
--   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados

--   LET v_s_detalle = "NSS|RFC|DOC FICO|EJERCICIO|FECHA CONTABLE|FECHA LIQUIDACION|TIPO SOLICITUD|FOLIO|PESOS TESOFE|AIVS VIV 97|AIVS VIV 92"

--   CALL v_ch_arch_ret_generico.write(v_s_detalle)
   LET v_query = " SELECT num_delega, nss, beneficiario, importe, ", 
                 "        entidad, juicio, num_acuerdo, desc_juez, ",
                 "        facultado, puesto, fch_ejecuta, procede_juicio, ",
                 "        tipo_sol, tipo_prod, correo_elec, cod_rechazo, ",
                 "        desc_rechazo ",
                 " FROM   ret_excep_devol_ssv ",
                 " WHERE  folio = ", p_folio,
                 " AND    estado_solicitud = 10"
                 
   DISPLAY "La consulta de aceptados es >", v_query, "<"
   -- se llena el arreglo 
   PREPARE s_acept_query FROM v_query
   DECLARE cur_acept_query CURSOR FOR s_acept_query

   FOREACH cur_acept_query INTO r_tmp_ret_det_disposicion.*
      LET v_s_detalle = r_tmp_ret_det_disposicion.num_delega USING "&&", "|",
                        r_tmp_ret_det_disposicion.nss USING "&&&&&&&&&&&", "|",
                        r_tmp_ret_det_disposicion.beneficiario, "|",
                        r_tmp_ret_det_disposicion.importe * 100 USING "&&&&&&&&&&&", "|",
                        r_tmp_ret_det_disposicion.entidad, "|",
                        r_tmp_ret_det_disposicion.juicio, "|",
                        r_tmp_ret_det_disposicion.num_acuerdo, "|",
                        r_tmp_ret_det_disposicion.desc_juez, "|",
                        r_tmp_ret_det_disposicion.facultado, "|",
                        r_tmp_ret_det_disposicion.puesto, "|",
                        r_tmp_ret_det_disposicion.fch_ejecucion USING "yyyymmdd", "|",
                        r_tmp_ret_det_disposicion.procede_juicio, "|",
                        r_tmp_ret_det_disposicion.tipo_sol USING "&&", "|",
                        r_tmp_ret_det_disposicion.tipo_prod USING "&&", "|",
                        r_tmp_ret_det_disposicion.correo_elec, "|",
                        r_tmp_ret_det_disposicion.cod_rechazo USING "&&&", "|",
                        r_tmp_ret_det_disposicion.desc_rechazo
      CALL v_ch_arch_ret_generico.write(v_s_detalle)
   END FOREACH
                        
   -- Se cierra el archivo de Aceptados
   CALL v_ch_arch_ret_generico.close()

   -- Se genera el nombre del archivo de Rechazados
   LET v_nom_archivo = "Detalle_Excepciones_Devol_SSV_Rechazados_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle 
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   LET v_mensaje_archivo = "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   
--   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados

--   LET v_s_detalle = "NSS|RFC|DOC FICO|EJERCICIO|FECHA CONTABLE|FECHA LIQUIDACION|TIPO SOLICITUD|FOLIO|PESOS TESOFE|AIVS VIV 97|AIVS VIV 92"

--   CALL v_ch_arch_ret_generico.write(v_s_detalle)
   LET v_query = " SELECT num_delega, nss, beneficiario, importe, ", 
                 "        entidad, juicio, num_acuerdo, desc_juez, ",
                 "        facultado, puesto, fch_ejecuta, procede_juicio, ",
                 "        tipo_sol, tipo_prod, correo_elec, cod_rechazo, ",
                 "        desc_rechazo ",
                 " FROM   ret_excep_devol_ssv ",
                 " WHERE  folio = ", p_folio,
                 " AND    estado_solicitud = 100"
                 
   DISPLAY "La consulta de rechazados es >", v_query, "<"
   -- se llena el arreglo 
   PREPARE s_rch_query FROM v_query
   DECLARE cur_rch_query CURSOR FOR s_rch_query

   FOREACH cur_rch_query INTO r_tmp_ret_det_disposicion.*
      SELECT des_larga
      INTO   r_tmp_ret_det_disposicion.desc_rechazo
      FROM   ret_rechazo_generico
      WHERE  cod_rechazo = r_tmp_ret_det_disposicion.cod_rechazo;
      LET v_s_detalle = r_tmp_ret_det_disposicion.num_delega USING "&&", "|",
                        r_tmp_ret_det_disposicion.nss USING "&&&&&&&&&&&", "|",
                        r_tmp_ret_det_disposicion.beneficiario, "|",
                        r_tmp_ret_det_disposicion.importe * 100 USING "&&&&&&&&&&&", "|",
                        r_tmp_ret_det_disposicion.entidad, "|",
                        r_tmp_ret_det_disposicion.juicio, "|",
                        r_tmp_ret_det_disposicion.num_acuerdo, "|",
                        r_tmp_ret_det_disposicion.desc_juez, "|",
                        r_tmp_ret_det_disposicion.facultado, "|",
                        r_tmp_ret_det_disposicion.puesto, "|",
                        r_tmp_ret_det_disposicion.fch_ejecucion USING "yyyymmdd", "|",
                        r_tmp_ret_det_disposicion.procede_juicio, "|",
                        r_tmp_ret_det_disposicion.tipo_sol USING "&&", "|",
                        r_tmp_ret_det_disposicion.tipo_prod USING "&&", "|",
                        r_tmp_ret_det_disposicion.correo_elec, "|",
                        r_tmp_ret_det_disposicion.cod_rechazo USING "&&&", "|",
                        r_tmp_ret_det_disposicion.desc_rechazo
      CALL v_ch_arch_ret_generico.write(v_s_detalle)
   END FOREACH

   LET v_mensaje_archivo = "El archivo fue generado exitosamente:\n\n\t", v_v_ruta_nomarch

   DISPLAY "Los archivos se generaron correctamente"

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()

   
END FUNCTION