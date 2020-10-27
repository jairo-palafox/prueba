--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--Jos� Eduardo Ventura  09/07/2015   se agreg� estado 135 en cre_acreditado  
--===============================================================

#########################################################################
#Modulo            =>AGR                                                #
#Programa          =>AGRP30                                             #
#Objetivo          =>Programa que realiza la preliquidacion para el     #
#                    proceso de Uso Gt�a Estado y Municipio             #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>02 Agosto 2013                                     #
#########################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN
   DEFINE p_v_usuario_cod       LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid               LIKE bat_ctr_proceso.pid, -- pid de la operaci�n
          p_i_proceso_cod       LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod         LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio             LIKE glo_folio.folio, -- numero de folio
          p_v_arch_proceso      VARCHAR(100), -- nombre del archivo a reversar
          v_si_subcuenta        LIKE cta_movimiento.subcuenta, -- subcuenta
          v_c_subcuenta_desc    LIKE cat_subcuenta.subcuenta_desc, -- descripci�n de subcuenta
          v_si_movimiento       LIKE cre_saldo_deudor.movimiento, -- movimiento
          v_c_movto_desc        LIKE cat_movimiento.movimiento_desc, -- descripci�n del movimiento
          v_si_tpo_originacion  LIKE cre_acreditado.tpo_originacion, -- tipo de originaci�n
          v_si_tpo_transferenc  LIKE cre_uso_garantia.tpo_transferencia, -- tipo de transferencia
          v_ind_tipo_ejecucion  LIKE bat_ctr_operacion.ind_tipo_ejecucion, -- tipo de ejecucion (0-manual, 1-batch)
          v_c_programa_cod      LIKE cat_operacion.programa_cod, -- programa de la operaci�n
          v_d_monto_aivs        DECIMAL(25,2), -- suma del monto en aivs
          v_d_monto_pesos       DECIMAL(25,2), -- suma del monto en pesos
          v_s_qryTxt            STRING, -- guarda una sentencia SQL a ejecutar
          v_s_comando           STRING, -- contiene al comando a correr
          r_c_ruta_bin          LIKE seg_modulo.ruta_bin, -- ruta bin del m�dulo
          r_c_ruta_listados     LIKE seg_modulo.ruta_listados, -- ruta listados del m�dulo
          r_cod_error           SMALLINT, -- contiene el codigo de error en caso de excepci�n
          r_i_estado            SMALLINT, -- estado al que se va a actualizar
          r_isam_err            INTEGER, -- isam error
          r_c_msj               VARCHAR(250), -- mensaje del error
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no    

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario_cod  = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario_cod CLIPPED|| ".AGRP30.log")
   
   DISPLAY "=INICIA AGRP30="
   DISPLAY "USUARIO    : ",p_v_usuario_cod
   DISPLAY "PID        : ",p_d_pid

   -- se inicializan variables
   LET v_si_tpo_originacion = 4 -- Anualidades Garantizadas
   LET v_si_tpo_transferenc = "43" -- Anualidades Garantizadas

   -- se obtiene el tipo de proceso de la tabla de control de procesos
   LET v_s_qryTxt = " SELECT ind_tipo_ejecucion\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

   PREPARE prp_tipo_ejecucion FROM v_s_qryTxt
   EXECUTE prp_tipo_ejecucion INTO v_ind_tipo_ejecucion

   -- se valida si la operacion se ejecut� manualmente o a trav�s del batch
   IF v_ind_tipo_ejecucion = 1 THEN
      -- se genera el folio
      LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario_cod)
   END IF
   DISPLAY "FOLIO      : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla de preliquidaci�n
   CALL fn_crea_tbl_preliquidacion()

   -- se ejecuta el la funci�n que procesa los creditos y los preliquida
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_agr_preliquida_edo_mcpio(?,?)"

   PREPARE prp_preliquida FROM v_s_qryTxt
   EXECUTE prp_preliquida USING p_d_folio,
                                p_v_usuario_cod
                               INTO r_cod_error,
                                    r_isam_err,
                                    r_c_msj

   IF r_cod_error <> 0 THEN
      DISPLAY "OCURRI� UN ERROR EN EL PROCESO DE PRELIQUIDACI�N EDO MCPIO"
      DISPLAY "C�D. ERROR : ",r_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj

      -- se invoca la funci�n que deja la operaci�n en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar la funci�n
   -- general de liquidaci�n
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_d_folio

   -- se invoca la funci�n que deja la operaci�n en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se informa de la finalizaci�n de la preliquidaci�n
   DISPLAY "SE REALIZ� LA PRELIQUIDACI�N SATISFACTORIAMENTE CON PID: ", p_d_pid
   DISPLAY " = SALDO DEUDOR ="

   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT sdo.movimiento, cat.movimiento_desc, SUM(sdo.monto_aivs), SUM(sdo.monto_pesos)\n",
                    "   FROM cre_saldo_deudor sdo, cat_movimiento cat\n",
                    "  WHERE sdo.id_cre_acreditado IN (\n",
                    "        SELECT id_cre_acreditado\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE estado in (130,135)\n",
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

   DISPLAY " = PRELIQUIDACI�N ="
   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT pre.subcuenta, sub.subcuenta_desc, pre.movimiento, mto.movimiento_desc,\n",
                    "        SUM(pre.monto_acciones), SUM(pre.monto_pesos)\n",
                    "   FROM cre_ag_preliq_ed_mc pre, cat_subcuenta sub, cat_movimiento mto\n",
                    "  WHERE pre.subcuenta = sub.subcuenta\n",
                    "    AND pre.movimiento = mto.movimiento\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,3"

   PREPARE prp_preliquida_agr FROM v_s_qryTxt
   DECLARE cur_preliquida_agr CURSOR FOR prp_preliquida_agr

   FOREACH cur_preliquida_agr INTO v_si_subcuenta, v_c_subcuenta_desc, v_si_movimiento,
                                   v_c_movto_desc, v_d_monto_aivs, v_d_monto_pesos
      DISPLAY "SUBCUENTA    ", v_si_subcuenta, "-", v_c_subcuenta_desc
      DISPLAY "MOVIMIENTO   ", v_si_movimiento, "-", v_c_movto_desc
      DISPLAY "MONTO AIVS   ", v_d_monto_aivs
      DISPLAY "MONTO PESOS  ", v_d_monto_pesos
   END FOREACH

   -- se obtiene la ruta bin y de listados del m�dulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   DISPLAY "SE GENERA REPORTE DE PRELIQUIDACI�N"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   --ejecuta el programa que genera el reporte de preliquidacion
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/AGRP16 ",
                               p_v_usuario_cod, " ",
                               p_d_pid, " ",
                               p_i_proceso_cod, " ",
                               p_i_opera_cod, " ",
                               p_d_folio, " ",
                               "cre_ag_preliq_ed_mc", " ",
                               v_c_programa_cod

   --DISPLAY " v_s_comando ", v_s_comando
   RUN v_s_comando
END MAIN
# Objetivo: Funci�n que crea la tabla de preliquidaci�n
FUNCTION fn_crea_tbl_preliquidacion()
   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se crea la sentencia sql que elimina la tabla de preliquidaci�n
   LET v_s_qryTxt = " DROP TABLE IF EXISTS cre_ag_preliq_ed_mc;"

   PREPARE prp_drop_preliquida FROM v_s_qryTxt
   EXECUTE prp_drop_preliquida

   -- se crea la sentencia sql que crea la tabla de preliquidaci�n
   LET v_s_qryTxt = " CREATE TABLE cre_ag_preliq_ed_mc\n",
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
