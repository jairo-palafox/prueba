






CREATE FUNCTION "safreviv".fn_cre_preliq_volriss(p_folio_liq          DECIMAL(9,0),
                                       p_id_cre_acreditado  DECIMAL(9,0),
                                       p_id_derechohabiente DECIMAL(9,0),
                                       p_valor_fondo        DECIMAL(19,14),
                                       p_tpo_trabajador     CHAR(1),
                                       p_proceso            SMALLINT)

RETURNING SMALLINT,SMALLINT

   DEFINE v_nom_tabla       CHAR(20);
   DEFINE v_exec             CHAR(300);
   DEFINE v_origen          CHAR(20);
   DEFINE v_tot_acciones    DECIMAL(16,6);
   DEFINE v_tot_pesos       DECIMAL(12,2);
   DEFINE v_cargo_acciones  DECIMAL(16,6);
   DEFINE v_cargo_pesos     DECIMAL(12,2);
   DEFINE v_f_liquida       DATE;
   DEFINE v_f_registro      DATE;
   DEFINE v_h_registro      DATETIME HOUR TO SECOND;
   DEFINE v_mov_cargo       SMALLINT;
   DEFINE v_mov_abono       SMALLINT;
   DEFINE v_status          SMALLINT;
   DEFINE v_error           SMALLINT;
   DEFINE v_subcuenta       SMALLINT;
   DEFINE v_fondo_inv       SMALLINT;

   ON EXCEPTION SET v_error
      LET v_status = 1;
      RETURN v_error,v_status;
   END EXCEPTION;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_cre_preqliq_volriss.trace";
   --SET DEBUG FILE TO "/safreviv/agr/sql/fn_cre_preqliq_volriss.trace";
   --TRACE ON;

   LET v_error = 0;
   LET v_f_liquida  = TODAY;

   IF p_proceso = 220 THEN
      LET v_origen    = "TRANSF ACREDITADO";
      LET v_mov_cargo = 1572;
      LET v_mov_abono = 881;
   ELIF p_proceso = 312 THEN
      LET v_origen    = "TRANSF ANUALIDAD";
      LET v_mov_cargo = 1582;
      LET v_mov_abono = 891;
   ELIF p_proceso = 1227 THEN
      LET v_origen    = "SDO GARANTÍA 43 BIS";
      LET v_mov_cargo = 1592;
      LET v_mov_abono = 901;
   ELIF p_proceso = 212 THEN -- Confirmar el proceso
      LET v_origen = "REMANENTE TRANS ACRED";
   END IF;

   SELECT DISTINCT(nombre_tabla)
     INTO v_nom_tabla
     FROM cat_preliquida
    WHERE proceso_cod = p_proceso;

   SELECT SUM(monto_acciones)
     INTO v_tot_acciones
     FROM cta_movimiento
    WHERE id_derechohabiente = p_id_derechohabiente
      AND subcuenta          = 55;

   --conversión de acciones a pesos
   LET v_tot_pesos = v_tot_acciones * p_valor_fondo;

   IF v_tot_acciones > 0 THEN

      LET v_cargo_acciones = v_tot_acciones * -1;
      LET v_cargo_pesos    = v_tot_pesos * -1;
      LET v_f_registro = TODAY;
      LET v_h_registro = CURRENT;

      LET v_subcuenta = 55;
      LET v_fondo_inv = 11;

      -- Se realiza el cargo a la subucuenta 55
      LET v_exec = ' INSERT INTO '||v_nom_tabla||
                   ' VALUES ("'|| v_f_liquida          ||'",'
                               || p_id_derechohabiente ||','
                               || v_subcuenta          ||','  -- subcuenta
                               || v_fondo_inv          ||','  --fondo inversión
                               || v_mov_cargo          ||','  --confirmar movimiento
                               || p_folio_liq          ||','
                               || p_id_cre_acreditado  ||','
                               || v_cargo_acciones     ||','
                               || v_cargo_pesos        ||','
                          ||'"'|| v_f_liquida          ||'"'||','
                          ||'"'|| v_f_registro         ||'"'||','
                          ||'"'|| v_h_registro         ||'"'||','
                          ||'"'|| v_origen             ||'"'||')';

      EXECUTE IMMEDIATE v_exec;

      LET v_exec       = NULL;
      LET v_subcuenta  = 4;
      LET v_fondo_inv  = 11;

      -- Se realiza el abono a la subcuenta 4
      LET v_exec = ' INSERT INTO '||v_nom_tabla||
                   ' VALUES ("'|| v_f_liquida          ||'",'
                               || p_id_derechohabiente ||','
                               || v_subcuenta          ||','  -- subcuenta
                               || v_fondo_inv          ||','  --fondo inversión
                               || v_mov_abono          ||','  --confirmar movimiento
                               || p_folio_liq          ||','
                               || p_id_cre_acreditado  ||','
                               || v_tot_acciones       ||','
                               || v_tot_pesos          ||','
                          ||'"'|| v_f_liquida          ||'"'||','
                          ||'"'|| v_f_registro         ||'"'||','
                          ||'"'|| v_h_registro         ||'"'||','
                          ||'"'|| v_origen             ||'"'||')';

      EXECUTE IMMEDIATE v_exec;

      LET v_status = 0;

   ELSE
       --regresa sin generar movimiento
       LET v_status = 1;
   END IF;

   RETURN v_error,v_status;

END FUNCTION;


