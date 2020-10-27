






CREATE PROCEDURE "safreviv".sp_ret_contingente_sinf_rev_liquidacion( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT,
                 p_usuario_cod         VARCHAR(20)                 
                 )
  RETURNING INTEGER, INTEGER, VARCHAR(255)
                   -- Control de Excepciones
  DEFINE sql_err                INTEGER;
  DEFINE isam_err               INTEGER;
  DEFINE err_txt                CHAR(200);
  DEFINE v_c_msj                CHAR(200);
  DEFINE v_si_resultado         SMALLINT;
  DEFINE v_id_derechohabiente   DECIMAL(9,0); 
  DEFINE v_id_solicitud         DECIMAL(9,0);
  DEFINE v_marca                SMALLINT;

  --manejo de excepciones
  ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
  END EXCEPTION

   -- se asume que el programa termina sin error
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El reverso finalizó correctamente";
   LET v_marca        = 801; -- marca retiro solo infonavit
   
   -- se borran los datos de cta_movimiento
   DELETE
   FROM   cta_movimiento
   WHERE  folio_liquida = p_folio;

   -- se actualiza el estado del folio a preliquidado
   UPDATE glo_folio
   SET status = 1
   WHERE proceso_cod = p_proceso_cod
     AND status = 2
     AND folio = p_folio;
  
   -- se realiza el reverso de la marca
   FOREACH
   SELECT
      id_derechohabiente,
      id_solicitud
   INTO 
      v_id_derechohabiente,
      v_id_solicitud
   FROM
      ret_solo_infonavit
   WHERE
      folio = p_folio
   AND 
      estado_solicitud = 60
      
      -- se reversa la desmarca
      EXECUTE PROCEDURE sp_reversa_desmarca(
                          v_id_derechohabiente
                         ,v_marca    -- marca de disposicion
                         ,v_id_solicitud
                         ,p_folio);
                         
      -- se cambian las solicitudes a estado preliquidado
      UPDATE ret_solo_infonavit
      SET    estado_solicitud = 50
      WHERE  estado_solicitud = 60
      AND    id_solicitud = v_id_solicitud
      AND    folio = p_folio;
      
   END FOREACH;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, err_txt;
END PROCEDURE;


