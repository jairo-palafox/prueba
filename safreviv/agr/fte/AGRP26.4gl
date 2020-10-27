--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>AGR                                                #
#Programa          =>AGRP26                                             #
#Objetivo          =>Programa que realiza la liquidacion de fondo de    #
#                    ahorro 72 para el módulo de Anulidades Garant.     #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>10 Julio 2012                                      #
#########################################################################

DATABASE safre_viv

MAIN 
   DEFINE p_v_usuario_cod       LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid               LIKE bat_ctr_proceso.pid, -- pid de la operación
          p_i_proceso_cod       LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod         LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio             LIKE glo_folio.folio, -- numero de folio
          p_v_arch_proceso      VARCHAR(100), -- nombre del archivo a reversar
          v_si_subcuenta        LIKE cta_movimiento.subcuenta, -- subcuenta
          v_c_subcuenta_desc    LIKE cat_subcuenta.subcuenta_desc, -- descripción de subcuenta
          v_si_movimiento       LIKE cre_saldo_deudor.movimiento, -- movimiento
          v_c_movto_desc        LIKE cat_movimiento.movimiento_desc, -- descripción del movimiento
          v_d_monto_pesos       DECIMAL(25,2), -- suma del monto en pesos
          v_c_programa_cod      LIKE cat_operacion.programa_cod, -- nombrel del programa
          v_i_cuenta_regs       INTEGER, -- contiene el numero de registros obtenido de la consulta
          r_si_cod_error        SMALLINT, -- contiene el codigo de error en caso de excepción
          v_s_qryTxt            STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario_cod  = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario_cod CLIPPED|| ".AGRP26.log")
   
   DISPLAY "=INICIA AGRP26="
   DISPLAY " USUARIO    : ",p_v_usuario_cod
   DISPLAY " PID        : ",p_d_pid
   DISPLAY " FOLIO      : ",p_d_folio USING "#########&"

   -- se inicializan variables
   LET v_c_programa_cod = "AGRP26"

{
   -- se verifica que exista información a liquidar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
	                " FROM safre_tmp:tmp_agr_fondo72"

   PREPARE prp_cuenta_fondo72 FROM v_s_qryTxt
   EXECUTE prp_cuenta_fondo72 INTO v_i_cuenta_regs
}

   LET v_i_cuenta_regs = 0

   -- se verifica si existieron registro a liquidar
   IF v_i_cuenta_regs = 0 THEN
      DISPLAY " MENSAJE: No hay registros para liquidar en fondo ahorro 72"

      DISPLAY " SUBCUENTA    ", "40 - FONDO AHORRO 72"
      DISPLAY " MOVIMIENTO   " 
      DISPLAY " MONTO PESOS  ", "0.00"

      -- se invoca la función que deja la operación en estado Finalizado
      LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

{
   -- se ejecuta el la función que procesa los creditos y los preliquida
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_agr_liquida_fondo72(?)"

   PREPARE prp_preliquida FROM v_s_qryTxt
   EXECUTE prp_preliquida USING p_d_folio
                           INTO r_si_cod_error
}

   LET r_si_cod_error = 0

   IF r_si_cod_error <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR EN LA LIQUIDACIÓN FONDO 72: ",r_si_cod_error

      -- se invoca la función que deja la operación en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   ---CALL fn_registro_contable_34(p_d_folio, p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se informa de la finalización de la preliquidación
   DISPLAY "SE REALIZÓ LA LIQUIDACIÓN FONDO 72 SATISFACTORIAMENTE CON PID: ", p_d_pid

   DISPLAY " = LIQUIDACIÓN ="
{
   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT sdo.subcuenta, sub.subcuenta_desc, sdo.movimiento, mov.movimiento_desc, SUM(sdo.importe)\n",
                    "   FROM cta_fondo72 sdo, cat_movimiento mov, cat_subcuenta sub\n",
                    "  WHERE sdo.folio_liquida = ",p_d_folio,"\n",
                    "    AND sdo.subcuenta = sub.subcuenta\n",
                    "    AND sdo.movimiento = mov.movimiento\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,3"

   PREPARE prp_cta_fondo72 FROM v_s_qryTxt
   DECLARE cur_cta_fondo72 CURSOR FOR prp_cta_fondo72

   FOREACH cur_cta_fondo72 INTO v_si_subcuenta,
                                v_c_subcuenta_desc,
                                v_si_movimiento,
                                v_c_movto_desc,
                                v_d_monto_pesos
      DISPLAY "SUBCUENTA    ", v_si_subcuenta, "-", v_c_subcuenta_desc
      DISPLAY "MOVIMIENTO   ", v_si_movimiento, "-", v_c_movto_desc
      DISPLAY "MONTO PESOS  ", v_d_monto_pesos
   END FOREACH
}

      DISPLAY "SUBCUENTA    ", "40 - FONDO AHORRO 72"
      DISPLAY "MOVIMIENTO   " 
      DISPLAY "MONTO PESOS  ", "0.00"

   DISPLAY " = FIN ="
END MAIN

FUNCTION fn_registro_contable_34(v_folio_registro, p_pid, p_proceso_cod, p_opera_cod)

  DEFINE p_cve_proceso_cnt SMALLINT
  DEFINE p_transaccion     SMALLINT
  DEFINE r_bnd_proceso_cnt SMALLINT
  DEFINE v_fecha_reg       DATE
  DEFINE r_bnd_opera_error SMALLINT
 
  DEFINE
    r_bnd_edo_act_archivo  SMALLINT,
    v_cuenta_contable      DECIMAL (10,0)

  DEFINE
    v_folio_registro       DECIMAL(9,0),
    p_pid                  DECIMAL(9,0),
    p_proceso_cod          SMALLINT,
    p_opera_cod            SMALLINT

  LET p_cve_proceso_cnt = 34  --Anualidad Garantizada
  LET p_transaccion     = 0   --Sin transaccion
  LET r_bnd_proceso_cnt = 0   --Bandera del stored del registro de anualidad garantizada
  LET v_fecha_reg       = TODAY

  --DISPLAY  "v_folio_registro ",v_folio_registro
  --DISPLAY  "v_fecha_reg ",v_fecha_reg
  --DISPLAY  "p_cve_proceso_cnt ",p_cve_proceso_cnt
  --DISPLAY  "p_proceso_cod ",p_proceso_cod
  --DISPLAY  "p_transaccion ",p_transaccion

  --WHENEVER ERROR CONTINUE
  --Se agrega función para realizar el registro contable ##
  PREPARE prp_reg_contable
    FROM "EXECUTE PROCEDURE fn_anu_gar_cnt34(?,?,?,?,?)"
  EXECUTE prp_reg_contable USING v_folio_registro,
                                 v_fecha_reg,
                                 p_cve_proceso_cnt,
                                 p_proceso_cod,
                                 p_transaccion
                            INTO r_bnd_proceso_cnt
  --WHENEVER ERROR STOP

  IF SQLCA.sqlcode < 0 THEN
     DISPLAY "Código de ERROR SQL de registro contable: ",SQLCA.sqlcode
     -- Función para finalizar la operación en error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING r_bnd_opera_error
     --EXIT PROGRAM
  END IF

  IF r_bnd_proceso_cnt = 1 THEN

     SELECT COUNT (*)
       INTO v_cuenta_contable
       FROM cnt_transaccion
      WHERE folio_liquida = v_folio_registro

     IF v_cuenta_contable > 0 THEN
        DISPLAY "El registro contable de anualidad garantizada se realizó exitosamente."
     ELSE
        DISPLAY "Error: El registro contable no se realizó debidamente."
     END IF
  ELSE
     DISPLAY "Ocurrió un error al realizar el registro contable."
  END IF
 
END FUNCTION
