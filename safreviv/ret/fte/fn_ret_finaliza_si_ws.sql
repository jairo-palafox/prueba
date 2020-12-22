DROP FUNCTION IF EXISTS fn_ret_finaliza_si_ws;
CREATE FUNCTION fn_ret_finaliza_si_ws(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),p_proceso_cod SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250)

-- para desmarcar cuentas
DEFINE v_id_solicitud           DECIMAL(9,0) ;
DEFINE v_id_derechohabiente     DECIMAL(9,0) ;
DEFINE v_si_resultado           SMALLINT;

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  VARCHAR(250);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_finaliza_si_ws.txt";
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = '1';
   
   --TRACE("Desmarcando cuentas liquidadas");

   -- se actualizan las solicitudes a estado 60 liquidada para el folio dado
   UPDATE ret_solo_infonavit
   SET    estado_solicitud = 60 -- liquidada
   WHERE  estado_solicitud = 50 -- preliquidada
   AND    folio            = p_folio;

   -- se actualizan las solicitudes a estado 60 liquidada para el folio dado
   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 60 -- liquidada
   WHERE  estado_solicitud = 50 -- preliquidada
   AND    folio            = p_folio;

   --TRACE("termina proceso");
   LET v_c_msj = 'Finalizó el proceso de retiro SI WS...';
  
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;      