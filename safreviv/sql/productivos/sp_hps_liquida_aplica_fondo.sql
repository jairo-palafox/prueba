






CREATE FUNCTION "safreviv".sp_hps_liquida_aplica_fondo(p_folio   DECIMAL(9,0),
                                            p_usuario CHAR(20))
RETURNING INTEGER,
          SMALLINT,
          CHAR(80);
--=======================================================================
-- PROYECTO: SAFRE (SACI) INFONAVIT
-- CREACION: 16/03/2015
-- AUTOR: Jesus David Yañez Moreno
-- Descripcion: 
-- procedimiento fn_mdt_liquida_pago_instruccion
-- realiza la actualización de estados posteriores a la liquidacion 
-- del traspaso de fondos de vivienda a las subcuentsa de servicios
-- (predial y cuota de conservacion)
--=======================================================================

DEFINE v_estado_destino SMALLINT;   -- estado destino correspondiente a la señal y estado origen
DEFINE v_ind            SMALLINT;   -- idicador de error
DEFINE v_diag           CHAR(3);    -- diagnostico de error

DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  CHAR(80);
                                                
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      RETURN v_sql_error, 
             v_isam_error, 
             v_msg_error;
   END EXCEPTION WITH RESUME;

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_mdt_liquida_pago_instruccion.trace';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = '';
   
 
   UPDATE hps_solicitud_pago_servicio 
   SET    ind_actividad = 103 
   WHERE  ind_actividad = 102; 


   UPDATE hps_ctr_aplica_servicio 
   SET    estado = 103
   WHERE  folio_aplica_servicio = p_folio;

   UPDATE hps_det_aplica_servicio 
   SET    estado = 103
   WHERE  id_ctr_aplica_servicio 
   IN (select b.id_ctr_aplica_servicio FROM hps_ctr_aplica_servicio b WHERE b.folio_aplica_servicio = p_folio);

   RETURN v_sql_error, 
          v_isam_error, 
          v_msg_error;

END FUNCTION;


