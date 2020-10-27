






CREATE FUNCTION "safreviv".fn_finaliza_ret_amort_excedente_restitucion(p_usuario_cod CHAR(20),
                                                            p_folio DECIMAL(9,0)  ,
                                                            p_proceso_cod SMALLINT,
                                                            p_cod_rechazo SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250)

 -- para desmarcar cuentas
 DEFINE v_id_solicitud       DECIMAL(9,0);
 DEFINE v_id_derechohabiente DECIMAL(9,0);
 DEFINE v_folio              DECIMAL(9,0);
 DEFINE v_nom_tabla          CHAR(25);
 DEFINE v_monto_acciones     DECIMAL(20,2);
 DEFINE v_monto_pesos        DECIMAL(20,2);
 DEFINE v_f_saldo            DATE;
 DEFINE v_precio_fondo       DECIMAL(19,14);
 DEFINE v_sql                CHAR(1000);

 DEFINE v_si_resultado                     SMALLINT;

 -- para marcar las cuentas
 DEFINE v_i_estado_marca                          INTEGER;
 DEFINE v_marca_amort_excedente              INTEGER; -- 805 de acuerdo a catalogo


 -- Control de Excepciones
 DEFINE sql_err INTEGER;
 DEFINE isam_err INTEGER;
 DEFINE err_txt  CHAR(200);
 DEFINE v_c_msj  VARCHAR(250);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_finaliza_amort_excedente.txt";

   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = '1';

   --TRACE("Desmarcando cuentas liquidadas");

   LET v_marca_amort_excedente = 810; -- marca para amort_excedente de recursos
   LET v_i_estado_marca    = 0;

   -- se actualizan las solicitudes a estado 210 RESTITUTIDO
   UPDATE ret_solicitud_generico
   SET    estado_solicitud  = 212,
          folio_restitucion = p_folio
   WHERE  estado_solicitud  = 209
   AND    cod_rechazo       = p_cod_rechazo
   AND    modalidad_retiro  = 9 ;

   -- se desmarcan las solicitudes restituidas
   FOREACH
   SELECT id_derechohabiente,
          id_solicitud      ,
          folio
   INTO   v_id_derechohabiente,
          v_id_solicitud      ,
          v_folio
   FROM   ret_solicitud_generico
   WHERE  folio_restitucion = p_folio

      -- selecciona la tabla actual
      SELECT tabla_saldo
      INTO   v_nom_tabla
      FROM   safre_sdo@vivws_tcp:glo_saldo
      WHERE  ind_saldo = 1; -- tabla activa

      --Obtención del monto de acciones
      SELECT SUM(monto_acciones)
      INTO   v_monto_acciones
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = 11
      AND    subcuenta          = 46;

      -- obtención del precio del fondo
      SELECT precio_fondo
      INTO   v_precio_fondo
      FROM   glo_valor_fondo
      WHERE  fondo       = 11
      AND    f_valuacion = TODAY;

      --genera valor de los pesos
      LET v_monto_pesos = v_monto_acciones * v_precio_fondo;

      -- Actualiza tabla de saldos
      LET v_sql=" UPDATE safre_sdo@vivws_tcp:"||v_nom_tabla||
                " SET monto_acciones = " ||v_monto_acciones||
                "    , monto_pesos = "||v_monto_pesos||
                "    , f_saldo = TODAY" ||
                " WHERE id_derechohabiente =" ||v_id_derechohabiente||" "||
                " AND fondo_inversion=11 "||
                " AND subcuenta = 46  ";

     EXECUTE IMMEDIATE v_sql;


   END FOREACH

   UPDATE ret_amort_excedente
   SET    estado_solicitud = 212
   WHERE  estado_solicitud = 209
   AND    cod_rechazo      = p_cod_rechazo;

   --TRACE("termina proceso");
   LET v_c_msj = 'Proceso terminado exitosamente';

   -- se devuelve el resultado de la operacion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


