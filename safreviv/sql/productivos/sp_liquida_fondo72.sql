






CREATE PROCEDURE "safreviv".sp_liquida_fondo72(p_folio DECIMAL(9,0), --Folio de referencia
                                    p_usuario     CHAR(20)    ,
                                    p_pid         DECIMAL(9,0),
                                    p_proceso_cod SMALLINT    ,
                                    p_opera_cod   SMALLINT    ,
                                    p_tabla       CHAR(30)    )

RETURNING SMALLINT, INTEGER, VARCHAR(250)

 DEFINE v_nss                   CHAR(11); -- No. de seguro social
 DEFINE v_importe               DECIMAL(12,2); -- Importe
 DEFINE v_restitucion_fondo72	  DECIMAL(9,0); -- Id de referencia
 DEFINE v_num_cuenta            DECIMAL(10,2); -- Id de referencia
 DEFINE v_regreso               SMALLINT; --bandera de regreso del stored
 DEFINE v_movimiento            SMALLINT;
 DEFINE v_subcuenta             SMALLINT;
 DEFINE v_origen                VARCHAR(30);
 DEFINE v_id_afi_fondo72        DECIMAL(9,0); -- Id de fondo72

 -- control de errores
 DEFINE sql_err  INTEGER;
 DEFINE isam_err INTEGER;
 DEFINE err_txt  CHAR(200);
 DEFINE v_c_msj  VARCHAR(250);

   -- se establece el comportamiento al aparecer alguna excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_regreso = sql_err;
      -- se devuelve el error SQL, el error isam y el mensaje de error
      RETURN v_regreso, isam_err, err_txt;
   END EXCEPTION

   -- se asume que el proceso finaliza correctamente
   LET v_regreso  = 0;
   LET isam_err   = 0;
   LET v_c_msj    = 'El proceso finalizó correctamente';

   LET v_nss = "";
   LET v_importe             = 0.00;
   LET v_restitucion_fondo72 = 0;
   LET v_num_cuenta          = 0.00;
   LET v_regreso             = 0; --0 es correcto y 1 es error
   LET v_movimiento          = 391;
   LET v_subcuenta           = 40;
   LET v_origen              = "RESTITUCION SSV";
   LET v_id_afi_fondo72      = 0.00;
	
	
	--SET DEBUG FILE TO '/ds/safreviv_int/BD/acr_liquida_fondo72.trace';
	--TRACE 'Folio '||p_folio;
	
   -- se verifica si hay datos para liquidar
   IF NOT EXISTS (
   SELECT folio
     FROM dse_restitucion_fondo72 
    WHERE estado = 10
      AND folio = p_folio) THEN
      -- No existe el registro en la table. Se actualizan estatus y no continua
      LET v_regreso  = 1;
      LET isam_err   = 0;
      LET v_c_msj    = 'No existen registros de restitución para ser liquidados con el folio ' || p_folio;
      
      RETURN v_regreso, isam_err, v_c_msj;
   ELSE 
      -- Subir a la tabla cta_fondo72 un registro por cada registro del archivo recibido con estado 10
      FOREACH
      SELECT id_restitucion_fondo72, nss, importe
      INTO v_restitucion_fondo72, v_nss, v_importe
      FROM dse_restitucion_fondo72 
      WHERE estado = 10 AND folio = p_folio
         
         SELECT MAX(id_afi_fondo72)
         INTO	 v_id_afi_fondo72
         FROM 	 afi_fondo72
         WHERE  nss = v_nss;
         
         --TRACE 'Inserta en cta_fondo72 con nss ='||v_nss;
         INSERT INTO cta_fondo72 
         (id_afi_fondo72, f_liquida, subcuenta, movimiento, folio_liquida,
          id_referencia, importe, estado_pago, f_registro, h_registro, origen)
         VALUES 					
         (v_id_afi_fondo72 ,TODAY, v_subcuenta, v_movimiento,  p_folio,
          v_restitucion_fondo72, v_importe, NULL, TODAY, CURRENT HOUR TO SECOND, v_origen);
         
         --TRACE 'Actualiza estado 140 con nss';
         --Actualizar el estado del registro correspondiente al NSS liquidado a 140 en la tabla dse_devolucion_fondo72.
         UPDATE dse_restitucion_fondo72 
         SET estado = 140 
         WHERE nss = v_nss 
         AND folio = p_folio;

      END FOREACH;
	 END IF 

   RETURN v_regreso, isam_err, v_c_msj;

END PROCEDURE;


