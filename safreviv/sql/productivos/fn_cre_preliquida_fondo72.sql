






CREATE FUNCTION "safreviv".fn_cre_preliquida_fondo72(p_folio_liq DECIMAL(9,0),
                                          p_id_cre_acreditado DECIMAL(9,0),
                                          p_nss CHAR(11),
                                          p_tpo_origina SMALLINT)
RETURNING SMALLINT;

   ----variables para asignación de valores
   ----a movimiento, subcuentas
   DEFINE v_movimiento      SMALLINT;
   DEFINE v_subcuenta       SMALLINT;
   DEFINE v_f_liquida       DATE;
   DEFINE v_f_registro      DATE;
   DEFINE v_h_registro      DATETIME HOUR TO SECOND;
   DEFINE v_origen          CHAR(20);
   DEFINE v_id_afi_fondo72  DECIMAL(9,0);
   DEFINE v_rfc             CHAR(13);
   DEFINE v_nombre          CHAR(40);
   DEFINE v_importe         DECIMAL(12,2);
   DEFINE v_status72        SMALLINT;

   DEFINE v_qry_ins         CHAR(1000);
   DEFINE v_tabla           CHAR(15);

   --SET DEBUG FILE TO 'preliquidaFondoAhorro.trace';
   --TRACE ON;

   LET v_importe    = 0;
   LET v_f_liquida  = TODAY;
   LET v_origen     = "TRANSF ACREDITADO";
   LET v_subcuenta  = 40;
   LET v_movimiento = 452;
   LET v_status72   = 0;

   IF p_tpo_origina = 1 THEN
      LET v_tabla = "tmp_acr_fondo72";
   ELSE
      LET v_tabla = "tmp_agr_fondo72";
   END IF
      ---verificación saldo en Fondo de Ahorro
   FOREACH
      SELECT cta.id_afi_fondo72,
             ROUND(cta.importe * (-1),2)
        INTO v_id_afi_fondo72,
             v_importe
        FROM safre_viv:afi_fondo72 afi,
             safre_viv:cta_fondo72 cta
       WHERE afi.nss = p_nss
         AND afi.id_afi_fondo72 = cta.id_afi_fondo72
         AND cta.importe > 0

      LET v_f_registro     = TODAY;
      LET v_h_registro     = CURRENT;

      LET v_qry_ins = " INSERT INTO safre_tmp:"||v_tabla||
                               "(id_afi_fondo72,"||
                               " f_liquida,"||
                               " subcuenta,"||
                               " movimiento,"||
                               " folio_liquida,"||
                               " id_referencia,"||
                               " importe,"||
                               " estado_pago,"||
                               " f_registro,"||
                               " h_registro,"||
                               " origen)"||
                      " VALUES( "||v_id_afi_fondo72||", "||
                             "'"||v_f_liquida||"', "||
                                  v_subcuenta||", "||
                                  v_movimiento||", "||
                                  p_folio_liq||", "||
                                  p_id_cre_acreditado||", "||
                                  v_importe||", "||
                                  ""||"'', "||
                             "'"||v_f_registro||"', "||
                             "'"||v_h_registro||"', "||
                             "'"||v_origen||"') ";

      EXECUTE IMMEDIATE v_qry_ins;

   END FOREACH;

   RETURN v_status72;

END FUNCTION;


