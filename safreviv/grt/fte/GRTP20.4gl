--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación: 26/04/2012
--===============================================================

##########################################################################
#Modulo            => GRT                                                #
#Programa          => GRTP20                                             #
#Objetivo          => Programa que ejecuta los procesos que generan los  #
#                     archivos de salida durante la liquidación GRT y el #
#                     el reporte                                         #
#Autor             => Benito Téllez,  EFP                                #
#Fecha inicio      => 26 Abril 2012                                      #
#Modificó          => Mauro Muñiz Caballero                              #
#Fecha modif       => 28 de marzo de 2016                                #
#                     Inserción de registro en tabla de dispersión para  #
#                     facturación y pago (sistema SAC43BIS) n            #
##########################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl"

GLOBALS

   DEFINE g_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE g_pid                     LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE g_proceso_cod             LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod               LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE g_folio_liquida           LIKE cta_movimiento.folio_liquida -- folio de liquidación

END GLOBALS

MAIN

   DEFINE v_opcion_fun              SMALLINT -- opcion para la funcion general
   DEFINE v_v_nom_archivo           LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo en proceso
   DEFINE v_i_opera_cod_ant         LIKE cat_operacion.opera_cod -- codigo de operacion anterior
   DEFINE v_r_cre_uso_garantia      RECORD LIKE cre_uso_garantia.*
   DEFINE v_i_estado                SMALLINT --estado al que se va a actualizar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_proceso_cod_folio     LIKE cat_proceso.proceso_cod -- codigo del proceso para el folio
   DEFINE v_i_opera_cod_folio       LIKE cat_operacion.opera_cod -- codigo de operacion para el folio
   DEFINE v_marca_entra             SMALLINT --marca de acreditado liquidado
   DEFINE v_estado_marca            SMALLINT --estado de la marca
   DEFINE v_marca_causa             SMALLINT --marca causa
   DEFINE v_edo_desmarca            SMALLINT --guarda el valor retornado por la funcion de desmarca
   DEFINE v_c_fec_hoy               CHAR(8) -- fecha con formato "yyyymmdd"
   DEFINE v_desc_desmarca           LIKE cat_rch_marca.rch_desc --descripcion del retorno de la desmarca    
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE p_v_arch_proceso          LIKE bat_ctr_operacion.nom_archivo -- archivo del proceso
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_c_nss                   LIKE afi_derechohabiente.nss -- NSS del trabajador
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_v_ruta_archivo          VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_ch_arch_reporte2        BASE.CHANNEL -- manejador de apuntador hacia archivo

   DEFINE v_r_det_rpt2 RECORD
      nss                           CHAR(11)
      fec_liquida                   CHAR(8)
   END RECORD

   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE r_ruta_listados           LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE v_f_liquida               LIKE cta_movimiento.f_liquida -- fecha de liquidacion
   DEFINE v_criterio                SMALLINT
   DEFINE v_tabla                   CHAR(20)

   -- se recuperan los parametros que envia el programa lanzador
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET g_folio_liquida  = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG (g_usuario_cod CLIPPED|| ".GRTP20.log")

   DISPLAY "=INICIA GRTP20="
   DISPLAY " USUARIO       : ",g_usuario_cod
   DISPLAY " PID           : ",g_pid
   DISPLAY " FOLIO         : ",g_folio_liquida USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan las variables
   LET v_opcion_fun          = 2 -- ejecutar liquidacion
   LET v_i_opera_cod_ant     = 1 -- código de operación anterior (Preliquidación)
   LET v_i_estado            = 140 -- liquidado
   LET v_i_proceso_cod_folio = g_proc_cod_grt_uso_liquida -- liquidación de uso en garantía
   LET v_i_opera_cod_folio   = 1 -- preliquida de uso en garantía
   LET v_marca_entra         = 223 --marca para uso en garantía  
   LET v_estado_marca        = 0 
   LET v_marca_causa         = 0
   LET v_c_fec_hoy           = TODAY USING "yyyymmdd"
   LET v_criterio            = 0
   LET v_f_liquida           = "12/31/1899"

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion crea la tabla temporal liquida_deudor
   CALL fn_crea_tbl_temp_liquida_deudor()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'grt'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio

   -- se crea el manejador de archivo
   LET v_ch_arch_reporte2 = base.Channel.create()

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_nom_archivo = "Grt"  || g_folio_liquida USING "&&&&&" || "." || v_c_extension CLIPPED
   DISPLAY " REPORTE SALIDA (FECHA LIQUIDACIÓN): ", v_v_nom_archivo

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_nom_archivo CLIPPED

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte2.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte2.setDelimiter("")

   --se prepara la ejecucion de la funcion que realiza la desmarca al liquidar
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   PREPARE prp_desmarca_cta FROM v_s_qryTxt

   -- se procesan los registros de cre uso garantia
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado = 130\n",
                    "    AND edo_procesar IN (5,10)\n",
                    "    AND tpo_transferencia IN ('18','48')"

   PREPARE prp_cre_acred FROM v_s_qryTxt
   DECLARE cur_his_trans CURSOR FOR prp_cre_acred

   FOREACH cur_his_trans INTO v_r_cre_uso_garantia.*
      -- se actualiza estado en cre uso garantia
      UPDATE cre_uso_garantia
         SET estado = 140
       WHERE id_cre_uso_garantia = v_r_cre_uso_garantia.id_cre_uso_garantia

----incluir función de inserción en dis_ctr_aps_tns



      -- ejecuta funcion que realiza la desmarca al liquidar
      EXECUTE prp_desmarca_cta USING v_r_cre_uso_garantia.id_derechohabiente,
                                     v_marca_entra,
                                     v_r_cre_uso_garantia.id_cre_uso_garantia,
                                     v_estado_marca,
                                     v_marca_causa,
                                     g_usuario_cod,
                                     g_proceso_cod
                               INTO  v_edo_desmarca

      IF v_edo_desmarca <> 0 THEN 
         -- se consulta la descripción del código de error que regresa la función de desmarca
         LET v_s_qryTxt = " SELECT rch_desc\n",
                          "   FROM cat_rch_marca\n",
                          "  WHERE rch_cod = ",v_edo_desmarca

         PREPARE prp_busca_rch_desc FROM v_s_qryTxt
         EXECUTE prp_busca_rch_desc INTO v_desc_desmarca

         DISPLAY "DESMARCA: " ,v_desc_desmarca, " Derechohabiente: ",v_r_cre_uso_garantia.id_derechohabiente
      END IF

      -- se obtiene el nss para el derechohabiente en proceso
      CALL fn_obt_nss(v_r_cre_uso_garantia.id_derechohabiente) RETURNING v_c_nss

      -- se asignan los valores en el registro detalle
      LET v_r_det_rpt2.nss         = v_c_nss
      LET v_r_det_rpt2.fec_liquida = TODAY USING "yyyymmdd"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_det_rpt2.nss, v_r_det_rpt2.fec_liquida

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_reporte2.write([v_s_registro])
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte2.close()

   --se manda el reporte de liquidacion
   DISPLAY "SE GENERA REPORTE DE LIQUIDACIÓN"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

   PREPARE prp_obt_mov FROM v_s_qryTxt
   EXECUTE prp_obt_mov USING v_criterio,
                             g_folio_liquida,
                             v_f_liquida
                        INTO v_tabla

   -- ejecuta el programa que genera el reporte de preliquidacion
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/GRTP19 ",
                               g_usuario_cod, " ",
                               g_pid, " ",
                               g_proceso_cod, " ",
                               g_opera_cod, " ",
                               g_folio_liquida, " ",
                               v_tabla, " ",
                               v_c_programa_cod --"GRTL28"

   --DISPLAY " v_s_comando ", v_s_comando
   RUN v_s_comando
   
   DISPLAY "PASA REGISTROS DE LIQUIDA DEUDOR A SALDO DEUDOR"
   WHENEVER ERROR CONTINUE
   -- pasar lo que hay en temporal de cre_saldo_deudor a la temporal de liquida_deudor
   INSERT INTO safre_tmp:tmp_liquida_deudor_grt SELECT * FROM safre_tmp:tmp_cre_saldo_deudor_grt

   IF SQLCA.SQLCODE < 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL INTENTAR PASAR LOS DATOS DE tmp_liquida a tmp cre_saldo"

      EXIT PROGRAM
   END IF
   WHENEVER ERROR STOP

   DISPLAY "SE LANZA GENERACIÓN DE ARCHIVO LIQUIDACIÓN"
   -- se inicializan las variables para el proceso de generación de archivo de liquidación
   LET g_opera_cod = 3 -- genera archivo liquidación

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se valida la extensión del archivo
   IF v_c_extension IS NULL THEN
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 

      IF r_b_valida <> 0 THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se crea el nombre del archivo
   LET v_v_nom_archivo = "A" || v_c_fec_hoy || "." || v_c_extension CLIPPED

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_folio_liquida,
                                           v_c_programa_cod,
                                           v_v_nom_archivo,
                                           g_usuario_cod)

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidación
   LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/GRTS02 ",
                                g_usuario_cod, " ",
                                g_pid, " ",
                                g_proceso_cod, " ",
                                g_opera_cod, " ",
                                g_folio_liquida, " ",
                                v_v_nom_archivo, " 1> ",
                                v_c_ruta_list_bat CLIPPED,
                                "/nohup:",g_pid USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod USING "&&&&&",
                                " 2>&1"

   RUN v_s_comando
END MAIN

# Objetivo: Función que crea la tabla temporal de deudor
FUNCTION fn_crea_tbl_temp_liquida_deudor()
   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_liquida_deudor_grt
   CREATE TABLE tmp_liquida_deudor_grt(id_cre_acreditado DECIMAL(9,0),
                                       folio_liquida DECIMAL(9,0),
                                       f_liquida DATE,
                                       movimiento SMALLINT,
                                       id_referencia DECIMAL(9,0),
                                       monto_aivs DECIMAL(22,2),
                                       monto_pesos DECIMAL(22,2),
                                       f_movimiento DATE)

   WHENEVER ERROR STOP

   -- regresa a la base de datoa safre viv
   DATABASE safre_viv
END FUNCTION

#Objetivo: Función que obtiene el nss correspondiente al id derechohabiente que entra como
#          parámetro
FUNCTION fn_obt_nss(p_d_id_derechohab)
   DEFINE p_d_id_derechohab LIKE afi_derechohabiente.id_derechohabiente, -- identificador del derechohabiente
          v_c_nss           LIKE afi_derechohabiente.nss, -- NSS del trabajador
          v_s_qryTxt        STRING -- guarda una sentencia SQL a ejecutar

   -- se obtiene los importes para el nss en proceso
   LET v_s_qryTxt = " SELECT nss\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_d_id_derechohab

   PREPARE prp_slct_nss_afi FROM v_s_qryTxt
   EXECUTE prp_slct_nss_afi INTO v_c_nss

   RETURN v_c_nss
END FUNCTION
