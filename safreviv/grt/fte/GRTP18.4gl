--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>GRT                                                #
#Programa          =>GRTP18                                             #
#Objetivo          =>Programa que realiza la preliquidacion del módulo  #
#                    Uso de Garantía 43 bis                             #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>26 Abril 2012                                      #
#Autor Modifica    =>Emilio Abarca, EFP.                                #
#Fecha modifica    =>03 Julio 2019                                      #
#Objetivo          =>Se incluye ejecución de fn_uso_cifras_pagos_ef     #
#########################################################################

DATABASE safre_viv

MAIN

   DEFINE p_v_usuario_cod           LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid de la operación
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_f_liq                   DATE
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a reversar
   DEFINE v_c_tpo_transferencia     LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
   DEFINE v_si_subcuenta            LIKE cta_movimiento.subcuenta -- subcuenta
   DEFINE v_c_subcuenta_desc        LIKE cat_subcuenta.subcuenta_desc -- descripción de subcuenta
   DEFINE v_si_movimiento           LIKE cre_saldo_deudor.movimiento -- movimiento
   DEFINE v_c_movto_desc            LIKE cat_movimiento.movimiento_desc -- descripción del movimiento
   DEFINE v_ind_tipo_ejecucion      LIKE bat_ctr_operacion.ind_tipo_ejecucion -- tipo de ejecucion (0-manual, 1-batch)
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- programa de la operación
   DEFINE v_d_monto_aivs            DECIMAL(25,2) -- suma del monto en aivs
   DEFINE v_d_monto_pesos           DECIMAL(25,2) -- suma del monto en pesos
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_si_cod_error            SMALLINT -- contiene el código de error en caso de excepción
   DEFINE r_i_estado                SMALLINT -- estado al que se va a actualizar
   DEFINE r_isam_err                INTEGER
   DEFINE r_c_msj                   VARCHAR(250)
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_msj_conciliacion        CHAR(300)

   DEFINE v_f_liq                   DATE
   DEFINE v_proceso_cod             SMALLINT
   DEFINE v_opera_cod               SMALLINT
   DEFINE v_folio_dis               DECIMAL(9,0)
   DEFINE v_bnd_proceso             SMALLINT
   DEFINE v_param2                  SMALLINT
   DEFINE v_msj                     CHAR(70)

   -- se recuperan los parámetros que envia el programa lanzador
   LET p_v_usuario_cod  = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_f_liq          = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario_cod CLIPPED|| ".GRTP18.log")

   DISPLAY "=INICIA GRTP18="
   DISPLAY "USUARIO    : ",p_v_usuario_cod
   DISPLAY "PID        : ",p_d_pid

   -- se inicializan variables
   LET v_c_tpo_transferencia = 18 -- Uso de Garantia 43 bis
   LET v_f_liq               = p_f_liq;

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

   DISPLAY " Se crea la tabla temporal de saldos"

   -- se invoca la funcion que crea la tabla temporal de saldo deudor
   CALL fn_crea_tbl_cre_saldo_deudor()

   DISPLAY " Se crea la tabla de preliquidación"
   -- se invoca la funcion que crea la tabla de preliquidación
   CALL fn_crea_tbl_preliquidacion() RETURNING r_si_cod_error

   -- se valida el código de error
   IF r_si_cod_error <> 0 THEN
      -- se invoca la función que deja la operación en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se valida el retorno de la función
      IF r_b_valida <> 0 THEN
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se invoca la funcion que crea la tabla global de conciliación
   CALL fn_crea_tbl_glo_conciliación() RETURNING r_si_cod_error

   -- se valida el código de error
   IF r_si_cod_error <> 0 THEN
      -- se invoca la función que deja la operación en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se valida el retorno de la función
      IF r_b_valida <> 0 THEN
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   DISPLAY " Se ejecuta la preliqudación"
   -- se ejecuta el la función que procesa los creditos y los preliquida
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_uso_preliquidacion(?,?,?)"

   PREPARE prp_preliquida FROM v_s_qryTxt
   EXECUTE prp_preliquida USING p_d_folio, v_c_tpo_transferencia, v_f_liq
                           INTO r_si_cod_error,
                                r_i_estado,
                                r_isam_err,
                                r_c_msj

   -- se valida el código de error
   IF r_si_cod_error <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR EN LA PRELIQUIDACIÓN USO: ",r_si_cod_error
      DISPLAY "CÓD. ERROR : ",r_si_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj

      -- se invoca la función que deja la operación en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      RETURN
   END IF

   DISPLAY " Actualiza estatus del folio"
   -- se actualiza el status de glo folio, para que el folio lo pueda tomar la función
   -- general de liquidación
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_d_folio

   DISPLAY "Ejecuta proceso para facturación"

   LET v_proceso_cod = 3904
   LET v_opera_cod   = 1

   CALL fn_genera_folio_dis(v_proceso_cod, v_opera_cod, p_d_folio, p_v_usuario_cod)
   RETURNING v_folio_dis

   LET v_s_qryTxt = "EXECUTE FUNCTION sp_dis_ocg_conf_ug(?,?)"

   PREPARE prp_factura FROM v_s_qryTxt
   EXECUTE prp_factura USING v_folio_dis, p_d_folio
                        INTO v_bnd_proceso,
                             v_param2,
                             v_msj

   DISPLAY ""
   DISPLAY "Folio transacción para facturación: ",v_folio_dis
   DISPLAY ""

   DISPLAY " Finaliza la operación"
   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se informa de la finalización de la preliquidación
   DISPLAY "SE REALIZÓ LA PRELIQUIDACIÓN SATISFACTORIAMENTE CON PID: ", p_d_pid
   DISPLAY " = SALDO DEUDOR ="

   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT sdo.movimiento, cat.movimiento_desc, SUM(sdo.monto_aivs), SUM(sdo.monto_pesos)\n",
                    "   FROM cre_saldo_deudor sdo, cat_movimiento cat\n",
                    "  WHERE sdo.id_cre_acreditado IN (\n",
                    "        SELECT id_cre_uso_garantia\n",
                    "          FROM cre_uso_garantia\n",
                    "         WHERE estado = 130\n",
                    "           AND edo_procesar IN (5, 10)\n",
                    "           AND tpo_transferencia IN ('18','48'))\n",
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
                    "   FROM cre_ug_preliquida pre, cat_subcuenta sub, cat_movimiento mto\n",
                    "  WHERE pre.subcuenta = sub.subcuenta\n",
                    "    AND pre.movimiento = mto.movimiento\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,3"

   PREPARE prp_cg_preliquida FROM v_s_qryTxt
   DECLARE cur_cg_preliquida CURSOR FOR prp_cg_preliquida

   FOREACH cur_cg_preliquida INTO v_si_subcuenta, v_c_subcuenta_desc, v_si_movimiento,
                                   v_c_movto_desc, v_d_monto_aivs, v_d_monto_pesos
      DISPLAY "SUBCUENTA    ", v_si_subcuenta, "-", v_c_subcuenta_desc
      DISPLAY "MOVIMIENTO   ", v_si_movimiento, "-", v_c_movto_desc
      DISPLAY "MONTO AIVS   ", v_d_monto_aivs
      DISPLAY "MONTO PESOS  ", v_d_monto_pesos
   END FOREACH

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_uso_cifras_pago_ef()"

   PREPARE prp_cifras_preliquida FROM v_s_qryTxt
   EXECUTE prp_cifras_preliquida INTO r_si_cod_error,
                                      r_isam_err,
                                      r_c_msj,
                                      r_msj_conciliacion

   DISPLAY ""
   DISPLAY "Cifras Pago EF"
   DISPLAY "Código retorno: ",r_si_cod_error
   DISPLAY "Mensaje cifras: ",r_c_msj
   DISPLAY "Mensaje conciliación: ",r_msj_conciliacion
   DISPLAY ""

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   DISPLAY "SE GENERA REPORTE DE PRELIQUIDACIÓN"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   --ejecuta el programa que genera el reporte de preliquidacion
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/GRTP19 ",
                               p_v_usuario_cod, " ",
                               p_d_pid, " ",
                               p_i_proceso_cod, " ",
                               p_i_opera_cod, " ",
                               p_d_folio, " ",
                               "cre_ug_preliquida", " ",
                               v_c_programa_cod --"GRTL27"

   --DISPLAY " v_s_comando ", v_s_comando
   RUN v_s_comando
END MAIN

# Objetivo: Función que crea la tabla tempora de deudor
FUNCTION fn_crea_tbl_cre_saldo_deudor()
   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_cre_saldo_deudor_grt

   WHENEVER ERROR STOP

   CREATE TABLE tmp_cre_saldo_deudor_grt(id_cre_acreditado      DECIMAL(9,0),
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

   DEFINE v_si_cod_error            SMALLINT -- codigo de error en caso de existir
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar

   -- se asume que no existirá error en la función
   LET v_si_cod_error = 0

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS cre_ug_preliquida;"

   PREPARE prp_drop_preliquida FROM v_s_qryTxt
   EXECUTE prp_drop_preliquida
   --DISPLAY "v_s_qryTxt: ",v_s_qryTxt

   -- verifica si existió error
   IF SQLCA.sqlcode <> 0 THEN
      DISPLAY " ERROR AL BORRAR TABLA: ",SQLCA.sqlcode

      -- se asigna el código de error en la variable de retorno
      LET v_si_cod_error = SQLCA.sqlcode

      RETURN v_si_cod_error
   END IF

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE cre_ug_preliquida\n",
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
   --DISPLAY "v_s_qryTxt: ",v_s_qryTxt

   -- verifica si existió error
   IF SQLCA.sqlcode <> 0 THEN
      DISPLAY " ERROR AL CREAR TABLA: ",SQLCA.sqlcode

      -- se asigna el código de error en la variable de retorno
      LET v_si_cod_error = SQLCA.sqlcode

      RETURN v_si_cod_error
   END IF

   RETURN v_si_cod_error

END FUNCTION

# Objetivo: Función que crea la tabla de global de conciliación
FUNCTION fn_crea_tbl_glo_conciliación()
   DEFINE v_si_cod_error SMALLINT, -- codigo de error en caso de existir
          v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   -- se asume que no existirá error en la función
   LET v_si_cod_error = 0

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS tmp_cta_mov_grt;"

   PREPARE prp_drop_cta_mov_grt FROM v_s_qryTxt
   EXECUTE prp_drop_cta_mov_grt

   -- verifica si existió error
   IF SQLCA.sqlcode <> 0 THEN
      DISPLAY " ERROR AL BORRAR TABLA: ",SQLCA.sqlcode

      -- se asigna el código de error en la variable de retorno
      LET v_si_cod_error = SQLCA.sqlcode

      RETURN v_si_cod_error
   END IF

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE tmp_cta_mov_grt\n",
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

   PREPARE prp_create_cta_mov_grt FROM v_s_qryTxt
   EXECUTE prp_create_cta_mov_grt

   -- verifica si existió error
   IF SQLCA.sqlcode <> 0 THEN
      DISPLAY " ERROR AL CREAR TABLA: ",SQLCA.sqlcode

      -- se asigna el código de error en la variable de retorno
      LET v_si_cod_error = SQLCA.sqlcode

      RETURN v_si_cod_error
   END IF

   -- se crea la sentencia sql que crea los indices de la tabla de preliquidación
   LET v_s_qryTxt = " CREATE UNIQUE INDEX xpktmp_cta_mov_grt ON\n",
                    "   tmp_cta_mov_grt (id_derechohabiente,\n",
                    "                    subcuenta) USING btree;"

   PREPARE prp_create_index_cta_mov_grt FROM v_s_qryTxt
   EXECUTE prp_create_index_cta_mov_grt

   -- regresa a la base de datos safre viv
   DATABASE safre_viv

   -- verifica si existió error
   IF SQLCA.sqlcode <> 0 THEN
      DISPLAY " ERROR AL CREAR INDICES EN TABLA: ",SQLCA.sqlcode

      -- se asigna el código de error en la variable de retorno
      LET v_si_cod_error = SQLCA.sqlcode

      RETURN v_si_cod_error
   END IF

   RETURN v_si_cod_error

END FUNCTION

