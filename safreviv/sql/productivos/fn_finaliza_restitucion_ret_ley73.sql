






CREATE FUNCTION "safreviv".fn_finaliza_restitucion_ret_ley73(p_usuario_cod CHAR(20),
                                                  p_folio DECIMAL(9,0)  ,
                                                  p_proceso_cod SMALLINT,
                                                  p_cod_rechazo SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250)

 -- para desmarcar cuentas
 DEFINE v_id_solicitud        DECIMAL(9,0);
 DEFINE v_id_derechohabiente  DECIMAL(9,0);
 DEFINE v_folio               DECIMAL(9,0);
 DEFINE v_consec_beneficiario SMALLINT;
 DEFINE v_nom_tabla           CHAR(25);
 DEFINE v_monto_acciones      DECIMAL(20,2);
 DEFINE v_monto_pesos         DECIMAL(20,2);
 DEFINE v_f_saldo             DATE;
 DEFINE v_precio_fondo        DECIMAL(19,14);
 DEFINE v_sql                 CHAR(1000);

 DEFINE v_si_resultado                     SMALLINT;

 -- Control de Excepciones
 DEFINE sql_err INTEGER;
 DEFINE isam_err INTEGER;
 DEFINE err_txt  CHAR(200);
 DEFINE v_c_msj  VARCHAR(250);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_finaliza_restitucion_ret_ley73.trace";

   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = '1';

   --trace "Cod rechazo: " || p_cod_rechazo;
   
   -- se restaura el saldo en las solicitudes restituidas de TITULARES
   FOREACH
   SELECT a.id_derechohabiente,
          a.id_solicitud      ,
          a.folio
   INTO   v_id_derechohabiente,
          v_id_solicitud      ,
          v_folio
   FROM   ret_solicitud_generico a,
          ret_beneficiario_generico b
   WHERE  a.estado_solicitud  = 209
   AND    a.cod_rechazo       = p_cod_rechazo
   AND    a.modalidad_retiro  = 3
   AND    a.id_solicitud = b.id_solicitud
   AND    b.tpo_beneficiario = 1
      --trace "VPD a solicitud: " || v_id_solicitud;
	 
      -- se actualizan la solicitud a estado 210 RESTITUTIDO
      UPDATE ret_solicitud_generico
      SET    estado_solicitud  = 210, 
             folio_restitucion = p_folio
      WHERE  id_solicitud      = v_id_solicitud;
	  
	  -- se actualiza la solicitud a restituida
      UPDATE ret_ley73_generico
      SET    estado_solicitud = 210 
      WHERE  estado_solicitud = 209
      AND    id_solicitud     = v_id_solicitud;
	 
      -- selecciona la tabla actual 
      SELECT tabla_saldo
      INTO   v_nom_tabla
      FROM   safre_sdo@vivws_tcp:glo_saldo
      WHERE  ind_saldo = 1; -- tabla activa

      -- obtencion del precio del fondo
      SELECT precio_fondo
      INTO   v_precio_fondo
      FROM   glo_valor_fondo
      WHERE  fondo       = 11
      AND    f_valuacion = TODAY;
      
      --Obtencion del monto de acciones vivienda 92
      SELECT SUM(monto_acciones)
      INTO   v_monto_acciones
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = 11
      AND    subcuenta          = 8;
      
	  -- si se tiene monto
	  IF ( v_monto_acciones IS NOT NULL ) THEN
	  
         --genera valor de los pesos
         LET v_monto_pesos = v_monto_acciones * v_precio_fondo;
               
         -- Actualiza tabla de saldos
         LET v_sql=" UPDATE safre_sdo@vivws_tcp:"||v_nom_tabla||
                   " SET monto_acciones = " ||v_monto_acciones||
                   "    , monto_pesos = "||v_monto_pesos||
                   "    , f_saldo = TODAY" ||
                   " WHERE id_derechohabiente =" ||v_id_derechohabiente||" "||
                   " AND fondo_inversion= 11 "||
                   " AND subcuenta = 8";
                
         EXECUTE IMMEDIATE v_sql;
      END IF
	  
	  -- saldo de vivienda 97
      SELECT SUM(monto_acciones)
      INTO   v_monto_acciones
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = 11
      AND    subcuenta          = 4;
      
	  -- si se tiene monto
	  IF ( v_monto_acciones IS NOT NULL ) THEN
	  
         --genera valor de los pesos
         LET v_monto_pesos = v_monto_acciones * v_precio_fondo;
               
         -- Actualiza tabla de saldos
         LET v_sql=" UPDATE safre_sdo@vivws_tcp:"||v_nom_tabla||
                   " SET monto_acciones = " ||v_monto_acciones||
                   "    , monto_pesos = "||v_monto_pesos||
                   "    , f_saldo = TODAY" ||
                   " WHERE id_derechohabiente =" ||v_id_derechohabiente||" "||
                   " AND fondo_inversion= 11 "||
                   " AND subcuenta = 4";
                
         EXECUTE IMMEDIATE v_sql;
      END IF
   
	  -- saldo de Anexo 1 viv 97
      SELECT SUM(monto_acciones)
      INTO   v_monto_acciones
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = 10
      AND    subcuenta          = 47; -- tesofe
      
	  -- si se tiene monto
	  IF ( v_monto_acciones IS NOT NULL ) THEN
	  
         --genera valor de los pesos
         LET v_monto_pesos = v_monto_acciones;
               
         -- Actualiza tabla de saldos
         LET v_sql=" UPDATE safre_sdo@vivws_tcp:"||v_nom_tabla||
                   " SET monto_acciones = " ||v_monto_acciones||
                   "    , monto_pesos = "||v_monto_pesos||
                   "    , f_saldo = TODAY" ||
                   " WHERE id_derechohabiente =" ||v_id_derechohabiente||" "||
                   " AND fondo_inversion= 10 "||
                   " AND subcuenta = 47";
                
         EXECUTE IMMEDIATE v_sql;
      END IF
   
   END FOREACH;

   -- se restaura el saldo en las solicitudes restituidas de BENEFICIARIOS
   FOREACH
   SELECT a.id_derechohabiente,
          a.id_solicitud      ,
          a.folio             ,
          b.consec_beneficiario
   INTO   v_id_derechohabiente,
          v_id_solicitud      ,
          v_folio             ,
          v_consec_beneficiario
   FROM   ret_solicitud_generico a,
          ret_beneficiario_generico b,
          ret_beneficiario_juridico c
   WHERE  c.estado_solicitud  = 209
   AND    c.cod_rechazo       = p_cod_rechazo
   AND    a.modalidad_retiro  = 3
   AND    a.id_solicitud = b.id_solicitud
   AND    b.tpo_beneficiario = 2
   AND    a.id_solicitud = c.id_solicitud
   AND    b.consec_beneficiario = c.consec_beneficiario
      --trace "VPD a solicitud: " || v_id_solicitud;
	 
      -- se actualizan la solicitud a estado 210 RESTITUTIDO
      UPDATE ret_beneficiario_juridico
      SET    estado_solicitud  = 210
      WHERE  id_solicitud      = v_id_solicitud
      AND    consec_beneficiario = v_consec_beneficiario;
	  
      -- selecciona la tabla actual 
      SELECT tabla_saldo
      INTO   v_nom_tabla
      FROM   safre_sdo@vivws_tcp:glo_saldo
      WHERE  ind_saldo = 1; -- tabla activa

      -- obtencion del precio del fondo
      SELECT precio_fondo
      INTO   v_precio_fondo
      FROM   glo_valor_fondo
      WHERE  fondo       = 11
      AND    f_valuacion = TODAY;
      
      --Obtencion del monto de acciones vivienda 92
      SELECT SUM(monto_acciones)
      INTO   v_monto_acciones
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = 11
      AND    subcuenta          = 8;
      
	  -- si se tiene monto
	  IF ( v_monto_acciones IS NOT NULL ) THEN
	  
         --genera valor de los pesos
         LET v_monto_pesos = v_monto_acciones * v_precio_fondo;
               
         -- Actualiza tabla de saldos
         LET v_sql=" UPDATE safre_sdo@vivws_tcp:"||v_nom_tabla||
                   " SET monto_acciones = " ||v_monto_acciones||
                   "    , monto_pesos = "||v_monto_pesos||
                   "    , f_saldo = TODAY" ||
                   " WHERE id_derechohabiente =" ||v_id_derechohabiente||" "||
                   " AND fondo_inversion= 11 "||
                   " AND subcuenta = 8";
                
         EXECUTE IMMEDIATE v_sql;
      END IF
	  
	  -- saldo de vivienda 97
      SELECT SUM(monto_acciones)
      INTO   v_monto_acciones
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = 11
      AND    subcuenta          = 4;
      
	  -- si se tiene monto
	  IF ( v_monto_acciones IS NOT NULL ) THEN
	  
         --genera valor de los pesos
         LET v_monto_pesos = v_monto_acciones * v_precio_fondo;
               
         -- Actualiza tabla de saldos
         LET v_sql=" UPDATE safre_sdo@vivws_tcp:"||v_nom_tabla||
                   " SET monto_acciones = " ||v_monto_acciones||
                   "    , monto_pesos = "||v_monto_pesos||
                   "    , f_saldo = TODAY" ||
                   " WHERE id_derechohabiente =" ||v_id_derechohabiente||" "||
                   " AND fondo_inversion= 11 "||
                   " AND subcuenta = 4";
                
         EXECUTE IMMEDIATE v_sql;
      END IF
   
	  -- saldo de Anexo 1 viv 97
      SELECT SUM(monto_acciones)
      INTO   v_monto_acciones
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = 10
      AND    subcuenta          = 47; -- tesofe
      
	  -- si se tiene monto
	  IF ( v_monto_acciones IS NOT NULL ) THEN
	  
         --genera valor de los pesos
         LET v_monto_pesos = v_monto_acciones;
               
         -- Actualiza tabla de saldos
         LET v_sql=" UPDATE safre_sdo@vivws_tcp:"||v_nom_tabla||
                   " SET monto_acciones = " ||v_monto_acciones||
                   "    , monto_pesos = "||v_monto_pesos||
                   "    , f_saldo = TODAY" ||
                   " WHERE id_derechohabiente =" ||v_id_derechohabiente||" "||
                   " AND fondo_inversion= 10 "||
                   " AND subcuenta = 47";
                
         EXECUTE IMMEDIATE v_sql;
      END IF
   
   END FOREACH;
   
   --TRACE("termina proceso");
   LET v_c_msj = 'Proceso terminado exitosamente';

   -- se devuelve el resultado de la operacion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


