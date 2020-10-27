






CREATE FUNCTION "safreviv".fn_finaliza_ret_fa_generico_restitucion(p_usuario_cod CHAR(20),
                                                        p_folio       DECIMAL(9,0),
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

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  VARCHAR(250);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_finaliza_ret_fa_generico_restitucion.trace";

   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = 'Proceso finalizado correctamente';

   --TRACE("seleccionando solicitudes en estado de restitucion");
   FOREACH
   SELECT id_derechohabiente,
          id_solicitud      ,
          folio
   INTO   v_id_derechohabiente,
          v_id_solicitud      ,
          v_folio
   FROM   ret_solicitud_generico
   WHERE  estado_solicitud = 209
   AND    cod_rechazo      = p_cod_rechazo
   AND    modalidad_retiro = 2 -- fondo de ahorro
   
      -- si es rechazo de BANCO, se cambian a 212, CxP cancelada
      IF ( p_cod_rechazo = 65 ) THEN
         UPDATE ret_solicitud_generico
         SET    estado_solicitud  = 212, 
                folio_restitucion = p_folio
         WHERE  id_solicitud      = v_id_solicitud;
		 
         UPDATE ret_fondo_ahorro_generico
         SET    estado_solicitud  = 212
         WHERE  id_solicitud      = v_id_solicitud;
      ELSE
         -- se actualizan las solicitudes a estado 210 RESTITUTIDO
         UPDATE ret_solicitud_generico
         SET    estado_solicitud  = 210, 
                folio_restitucion = p_folio
         WHERE  id_solicitud      = v_id_solicitud;
		 
         UPDATE ret_fondo_ahorro_generico
         SET    estado_solicitud  = 210
         WHERE  id_solicitud      = v_id_solicitud;

      END IF
   
   END FOREACH

   --TRACE("termina proceso");
   LET v_c_msj = 'Proceso finalizado correctamente';

   -- se devuelve el resultado de la operacion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


