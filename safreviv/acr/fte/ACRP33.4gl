--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

##########################################################################
#Modulo            => ACR                                                #
#Programa          => ACRP33                                             #
#Objetivo          => Programa que realiza la preliquidación de          #
#                     Transferencia de Acreditados                       #
#Autor             => Daniel Buendia, EFP                                #
#Fecha inicio      => 6 de marzo de 2012                                 #
#Actualización     => Mauro Muñiz Caballero                              #
#Fecha actualiz    => 20 de noviembre de 2014                            #
#                     se debe liquidar con cargo a capital los deudores  #
#                     con marca rechazada por Trámite Judicial           #
#                     REQUERIMIENTO CANCELADO                            #
#Modifica:         => Mauro Muñiz Caballero                              #
#Fecha modif:      => 9 de noviembre de 2015                             #
#Adecuación        => Eliminación de adelantos                           #
#Modifica:         => Mauro Muñiz Caballero                              #
#Fecha modif:      => 14 de agosto de 2017                               #
#Adecuación        => Liquidación regitros crédito liquidado             #
##########################################################################

DATABASE safre_viv

GLOBALS "ACRG10.4gl"

MAIN

   DEFINE p_v_usuario_cod           LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid de la operación
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operación de la etapa
   DEFINE p_d_folio                 LIKE glo_folio.folio -- número de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a reversar
   DEFINE v_si_subcuenta            LIKE cta_movimiento.subcuenta -- subcuenta
   DEFINE v_c_subcuenta_desc        LIKE cat_subcuenta.subcuenta_desc -- descripción de subcuenta
   DEFINE v_si_movimiento           LIKE cre_saldo_deudor.movimiento -- movimiento
   DEFINE v_c_movto_desc            LIKE cat_movimiento.movimiento_desc -- descripción del movimiento
   DEFINE v_si_tpo_originacion      LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_si_tpo_transf           LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE v_r_dse_ctr_archivo       RECORD LIKE dse_ctr_archivo.* -- registro de dse ctr archivo
   DEFINE v_d_monto_aivs            DECIMAL(25,2) -- suma del monto en aivs
   DEFINE v_d_monto_pesos           DECIMAL(25,2) -- suma del monto en pesos
   DEFINE v_i_estado                SMALLINT -- estado al que se va a actualizar
   DEFINE v_si_cod_error            SMALLINT -- contiene el código de error en caso de excepción
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_i_tot_registros         INTEGER -- total de registros
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombre del programa origen
   DEFINE v_ind_tipo_ejecucion      LIKE bat_ctr_operacion.ind_tipo_ejecucion -- tipo de ejecución (0-manual, 1-batch)
   DEFINE r_si_lote                 LIKE dse_ctr_archivo.lote -- lote de la tabla de control de archivos DSE
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_f_liquida               DATE

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario_cod  = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario_cod CLIPPED|| ".ACRP33.log")

   DISPLAY "=INICIA ACRP33="
   DISPLAY "USUARIO    : ",p_v_usuario_cod
   DISPLAY "PID        : ",p_d_pid

   -- se inicializan variables
   LET v_si_tpo_originacion = 1 -- transferencia de acreditados
   LET v_si_tpo_transf      = "15" -- 15--DSE Transferencia de Acreditados
   LET v_f_liquida          = TODAY

   -- se obtiene el tipo de proceso de la tabla de control de procesos
   LET v_s_qryTxt = " SELECT ind_tipo_ejecucion\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

   PREPARE prp_tipo_ejecucion FROM v_s_qryTxt
   EXECUTE prp_tipo_ejecucion INTO v_ind_tipo_ejecucion

   -- se valida si la operacion se ejecutó manualmente o a través del batch
   IF v_ind_tipo_ejecucion = 1 THEN
      -- se genera el folio
      LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario_cod)
   END IF

   DISPLAY "FOLIO      : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal de saldo deudor
   CALL fn_crea_tbl_cre_saldo_deudor()

   -- se invoca la funcion que crea la tabla de preliquidación
   CALL fn_crea_tbl_preliquidacion()

   -- se invoca la funcion que crea la tabla de preliquidación fondo 72
   CALL fn_crea_tbl_preliquid_fondo72()

   -- se invoca la función que crea la tabla global de diagnóstico
   CALL fn_crea_tbl_glo_diagnostico()

   -- se ejecuta el la función que procesa los créditos y los preliquida
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_preliquidacion(?,?,?,?,?)"

   PREPARE prp_preliquida FROM v_s_qryTxt
   EXECUTE prp_preliquida USING p_d_folio,
                                v_si_tpo_originacion,
                                p_v_usuario_cod,
                                p_i_proceso_cod,
                                v_f_liquida
                           INTO v_si_cod_error, v_i_estado

   DISPLAY "STATUS: ",v_si_cod_error
   DISPLAY "ESTADO: ",v_i_estado

   IF v_si_cod_error <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR EN LA PRELIQUIDACIÓN: ",v_si_cod_error

      -- se invoca la función que deja la operación en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se consulta el total de registros insertados
   LET v_i_tot_registros = f_obt_tot_regs_dse(p_d_folio)

   -- verifica si ya existe el registro en la tabla de control de archivo DSE
   CALL fn_obtiene_lote_dse(v_si_tpo_transf) RETURNING r_si_lote

   DISPLAY " INSERTA REGISTRO EN LA TABLA DE CONTROL DSE"
   LET v_r_dse_ctr_archivo.tpo_transferencia = v_si_tpo_transf
   LET v_r_dse_ctr_archivo.lote              = r_si_lote
   LET v_r_dse_ctr_archivo.f_lote            = TODAY
   LET v_r_dse_ctr_archivo.tot_registros     = v_i_tot_registros
   LET v_r_dse_ctr_archivo.tot_aceptados     = v_i_tot_registros
   LET v_r_dse_ctr_archivo.tot_rechazados    = 0
   LET v_r_dse_ctr_archivo.estado            = 20
   LET v_r_dse_ctr_archivo.f_proceso         = TODAY
   LET v_r_dse_ctr_archivo.usuario           = p_v_usuario_cod
   LET v_r_dse_ctr_archivo.folio             = p_d_folio
   LET v_r_dse_ctr_archivo.nom_archivo       = fn_obt_nombre_recurrente()

   --se inserta registro
   INSERT INTO dse_ctr_archivo VALUES (v_r_dse_ctr_archivo.*)

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar la función
   -- general de liquidación
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_d_folio

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operación
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continúa
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se informa de la finalización de la preliquidación
   DISPLAY "SE REALIZÓ LA PRELIQUIDACIÓN SATISFACTORIAMENTE CON PID: ", p_d_pid
   DISPLAY " = SALDO DEUDOR ="

   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT sdo.movimiento, cat.movimiento_desc, SUM(sdo.monto_aivs), SUM(sdo.monto_pesos)\n",
                    "   FROM cre_saldo_deudor sdo, cat_movimiento cat\n",
                    "  WHERE id_cre_acreditado IN (\n",
                    "        SELECT id_cre_acreditado\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE estado IN (130,135,137)\n",
                    "           AND tpo_originacion = ",v_si_tpo_originacion,")\n",
                    "    AND sdo.movimiento = cat.movimiento\n",
                    "  GROUP BY 1, 2\n",
                    "  ORDER BY 1"

   PREPARE prp_cre_sdo_deudor FROM v_s_qryTxt
   DECLARE cur_cre_sdo_deudor CURSOR FOR prp_cre_sdo_deudor

   FOREACH cur_cre_sdo_deudor INTO v_si_movimiento, v_c_movto_desc, v_d_monto_aivs, v_d_monto_pesos
      DISPLAY "MOVIMIENTO   ",v_si_movimiento, "-",v_c_movto_desc
      DISPLAY "MONTO AIVS   ",v_d_monto_aivs
      DISPLAY "MONTO_PESOS  ",v_d_monto_pesos
   END FOREACH

   DISPLAY " = PRELIQUIDACIÓN ="
   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT pre.subcuenta, sub.subcuenta_desc, pre.movimiento, mto.movimiento_desc,\n",
                    "        SUM(pre.monto_acciones), SUM(pre.monto_pesos)\n",
                    "   FROM cre_ta_preliquida pre, cat_subcuenta sub, cat_movimiento mto\n",
                    "  WHERE pre.subcuenta = sub.subcuenta\n",
                    "    AND pre.movimiento = mto.movimiento\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,3"

   PREPARE prp_preliquida_acr FROM v_s_qryTxt
   DECLARE cur_preliquida_acr CURSOR FOR prp_preliquida_acr

   FOREACH cur_preliquida_acr INTO v_si_subcuenta, v_c_subcuenta_desc, v_si_movimiento,
                                   v_c_movto_desc, v_d_monto_aivs, v_d_monto_pesos
      DISPLAY "SUBCUENTA    ", v_si_subcuenta, "-", v_c_subcuenta_desc
      DISPLAY "MOVIMIENTO   ", v_si_movimiento, "-", v_c_movto_desc
      DISPLAY "MONTO AIVS   ", v_d_monto_aivs
      DISPLAY "MONTO PESOS  ", v_d_monto_pesos
   END FOREACH

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   DISPLAY "SE GENERA REPORTE DE PRELIQUIDACIÓN"
   --ejecuta el programa que genera el reporte de preliquidacion
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/ACRP34 ",
                               p_v_usuario_cod, " ",
                               p_d_pid, " ",
                               p_i_proceso_cod, " ",
                               p_i_opera_cod, " ",
                               p_d_folio, " ",
                               "cre_ta_preliquida", " ",
                               v_c_programa_cod --"ACRL19"

   --DISPLAY " v_s_comando ", v_s_comando

   RUN v_s_comando

END MAIN

# Objetivo: Función que crea la tabla tempora de deudor
FUNCTION fn_crea_tbl_cre_saldo_deudor()

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_cre_saldo_deudor_acr

   WHENEVER ERROR STOP

   CREATE TABLE tmp_cre_saldo_deudor_acr(id_cre_acreditado      DECIMAL(9,0),
                                         folio_liquida          DECIMAL(9,0),
                                         f_liquida              DATE,
                                         movimiento             SMALLINT,
                                         id_referencia          DECIMAL(9,0),
                                         monto_aivs             DECIMAL(22,2),
                                         monto_pesos            DECIMAL(22,2),
                                         f_movimiento           DATE)

   -- regresa a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

# Objetivo: Función que crea la tabla de preliquidación
FUNCTION fn_crea_tbl_preliquidacion()

   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS cre_ta_preliquida;"

   PREPARE prp_drop_preliquida FROM v_s_qryTxt
   EXECUTE prp_drop_preliquida

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE cre_ta_preliquida\n",
                    " (f_liquida          DATE NOT NULL,\n",
                    "  id_derechohabiente DECIMAL(9,0) NOT NULL,\n",
                    "  subcuenta          SMALLINT NOT NULL,\n",
                    "  fondo_inversion    SMALLINT NOT NULL,\n",
                    "  movimiento         SMALLINT NOT NULL,\n",
                    "  folio_liquida      DECIMAL(9,0) NOT NULL,\n",
                    "  id_referencia      DECIMAL(9,0) NOT NULL,\n",
                    "  monto_acciones     DECIMAL(22,2),\n",
                    "  monto_pesos        DECIMAL(22,2),\n",
                    "  f_valor            DATE,\n",
                    "  f_registro         DATE,\n",
                    "  h_registro         DATETIME HOUR TO SECOND,\n",
                    "  origen             CHAR(20)\n",
                    " ) IN cre_dbs;"

   PREPARE prp_create_preliquida FROM v_s_qryTxt
   EXECUTE prp_create_preliquida 

END FUNCTION

# Objetivo: Función que crea la tabla de preliquidación de fondo 72
FUNCTION fn_crea_tbl_preliquid_fondo72()
   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS tmp_acr_fondo72;"

   PREPARE prp_drop_preliquida_fondo72 FROM v_s_qryTxt
   EXECUTE prp_drop_preliquida_fondo72

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE tmp_acr_fondo72\n",
                    " (id_afi_fondo72 DECIMAL(9,0) NOT NULL,\n",
                    "  f_liquida      DATE,\n",
                    "  subcuenta      SMALLINT,\n",
                    "  movimiento     SMALLINT,\n",
                    "  folio_liquida  DECIMAL(9,0),\n",
                    "  id_referencia  DECIMAL(9,0),\n",
                    "  importe        DECIMAL(22,2),\n",
                    "  estado_pago    CHAR(1),\n",
                    "  f_registro     DATE,\n",
                    "  h_registro     DATETIME HOUR TO SECOND,\n",
                    "  origen         CHAR(20)\n",
                    " );"

   PREPARE prp_create_preliquida_fondo72 FROM v_s_qryTxt
   EXECUTE prp_create_preliquida_fondo72

   -- regresa a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

# Objetivo: Función que crea la tabla de global de diagnóstico
FUNCTION fn_crea_tbl_glo_diagnostico()

   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS tmp_cta_mov_acr;"

   PREPARE prp_drop_cta_mov_acr FROM v_s_qryTxt
   EXECUTE prp_drop_cta_mov_acr

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE tmp_cta_mov_acr\n",
                    " (f_liquida          DATE NOT NULL,\n",
                    "  id_derechohabiente DECIMAL(9,0) NOT NULL,\n",
                    "  subcuenta          SMALLINT NOT NULL,\n",
                    "  fondo_inversion    SMALLINT NOT NULL,\n",
                    "  movimiento         SMALLINT NOT NULL,\n",
                    "  folio_liquida      DECIMAL(9,0) NOT NULL,\n",
                    "  id_referencia      DECIMAL(9,0) NOT NULL,\n",
                    "  monto_acciones     DECIMAL(22,2),\n",
                    "  monto_pesos        DECIMAL(22,2),\n",
                    "  f_valor            DATE,\n",
                    "  f_registro         DATE,\n",
                    "  h_registro         DATETIME HOUR TO SECOND,\n",
                    "  origen             CHAR(20)\n",
                    " ) ;"

   PREPARE prp_create_cta_mov_acr FROM v_s_qryTxt
   EXECUTE prp_create_cta_mov_acr

   -- se crea la sentencia sql que crea los indices de la tabla de preliquidación
   LET v_s_qryTxt = " CREATE INDEX xpktmp_cta_mov_acr ON\n",
                    "   tmp_cta_mov_acr (id_derechohabiente,\n",
                    "                   subcuenta) USING btree ;"

   PREPARE prp_create_index_cta_mov_acr FROM v_s_qryTxt
   EXECUTE prp_create_index_cta_mov_acr

   -- regresa a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

#Objetivo: Función que realiza la consulta de registros insertados en la tabla de
#          agrupación para el folio que entra como parámetro
FUNCTION f_obt_tot_regs_dse(p_d_folio)

   DEFINE p_d_folio                 LIKE glo_folio.folio -- folio
   DEFINE v_i_tot_regs              INTEGER -- total de registros consultados
   DEFINE v_s_qryTxt                STRING -- contiene una sentencia sql a ejecutar

   -- se inicializan variables
   LET v_i_tot_regs = 0

   -- se consulta el numero de registros insertados en la tabla de agrupación
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_agrupa_folio FROM v_s_qryTxt
   EXECUTE prp_count_agrupa_folio INTO v_i_tot_regs

   RETURN v_i_tot_regs

END FUNCTION

#Objetivo: Obtiene el nombre del archivo recurrente
FUNCTION fn_obt_nombre_recurrente()

   DEFINE v_nom_archivo             LIKE dse_ctr_archivo.nom_archivo
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso
   DEFINE v_s_qryTxt                STRING -- se asigna consulta sql a ejecutar

   -- se inicializan variables
   LET v_si_id_proceso = g_id_proceso_acr -- Transferencia de Acreditados

   -- se crea la sentencia que consulta el nombre del archivo
   LET v_s_qryTxt = " SELECT FIRST 1 nom_archivo ",
                    "   FROM cre_ctr_archivo ",
                    "  WHERE id_proceso = ",v_si_id_proceso,
                    "    AND operacion = 21 ",
                    "  ORDER BY id_cre_ctr_archivo DESC "

   PREPARE prp_obt_nom FROM v_s_qryTxt
   EXECUTE prp_obt_nom INTO v_nom_archivo

   RETURN v_nom_archivo

END FUNCTION

#Objetivo: Obtiene el lote DSE correspondiente para el tipo de transferencia que
#          entra como parámentro
FUNCTION fn_obtiene_lote_dse(p_c_tpo_transferencia)

   DEFINE p_c_tpo_transferencia     LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE v_si_lote                 LIKE dse_ctr_archivo.lote -- lote DSE
   DEFINE v_s_qry_txt               STRING

   -- se obtiene el máximo lote para el tpo de transferencia y la fecha de lote (TODAY)
   LET v_s_qry_txt = " SELECT MAX(lote)\n",
                     "   FROM dse_ctr_archivo\n",
                     "  WHERE tpo_transferencia = '",p_c_tpo_transferencia,"'\n",
                     "    AND f_lote = TODAY"

   PREPARE prp_max_lote_dse FROM v_s_qry_txt
   EXECUTE prp_max_lote_dse INTO v_si_lote

   -- se valida el lote obtenido
   IF v_si_lote IS NULL THEN
      LET v_si_lote = 1
   ELSE
      LET v_si_lote = v_si_lote + 1
   END IF

   RETURN v_si_lote

END FUNCTION
