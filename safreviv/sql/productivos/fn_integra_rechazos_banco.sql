






CREATE FUNCTION "safreviv".fn_integra_rechazos_banco(p_folio DECIMAL(9,0))
RETURNING SMALLINT,VARCHAR(200);

   --Variables de salida
   DEFINE v_respuesta         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);
 
   DEFINE v_id_solicitud      DECIMAL(9,0);
   DEFINE v_id_afi_fondo72    DECIMAL(9,0);
   DEFINE v_saldo             DECIMAL(15,2);
   DEFINE v_estado_banco      SMALLINT;
   DEFINE v_rechazo_banco     SMALLINT;
   
   DEFINE v_hProceso          DATETIME HOUR TO SECOND;
   DEFINE v_origen                              CHAR(20);
   
   SET PDQPRIORITY HIGH; 

   --Se inicializan las variables de respuesta
   LET v_respuesta = 0;
   LET v_mensaje = "El archivo se integro correctamente";

   LET v_estado_banco = 100;
   LET v_origen = 'RECHAZO SIAFF';
   FOREACH
      SELECT
         det.id_solicitud,
         det.id_afi_fondo72,
         det.saldo,
         300 + rch.cve_rechazo
      INTO
         v_id_solicitud,
         v_id_afi_fondo72,
         v_saldo,
         v_rechazo_banco
      FROM ret_detalle_spei det
      INNER JOIN safre_tmp:tmp_fondo72_rechazo_banco rch ON (rch.cve_rastreo = det.cve_rastreo)
      WHERE rch.estatus_detalle = 3       --Estatus igual a 3 significa que el registro fue rechazado por el banco
        AND det.cod_estado_pago = 71


      --Se actualiza la tabla de solicitudes
      UPDATE ret_fondo_ahorro_masivo 
      SET estado_solicitud = v_estado_banco, cod_rechazo = v_rechazo_banco
      WHERE id_solicitud =  v_id_solicitud
      AND id_afi_fondo72 =  v_id_afi_fondo72;

      --Se actualiza el detalle enviado en el SIAFF
      UPDATE ret_detalle_spei
      SET cod_estado_pago = v_estado_banco, cod_rechazo = v_rechazo_banco, folio = p_folio
      WHERE id_solicitud =  v_id_solicitud
      AND id_afi_fondo72 =  v_id_afi_fondo72
      AND cod_estado_pago = 71;

      LET v_hProceso = CURRENT HOUR TO SECOND;
      --Se insertan los abonos para anular el retiro
      INSERT INTO cta_fondo72 VALUES(  v_id_afi_fondo72,
                                       TODAY,
                                       40,
                                       141,  --"ABONO, RETIRO FONDO DE AHORRO NO PAGADO"
                                       p_folio,
                                       v_id_solicitud,
                                       v_saldo,
                                       NULL,
                                       TODAY,
                                       v_hProceso,
                                       v_origen);

      --Se inserta el abono del tanto adicional
      INSERT INTO cta_fondo72 VALUES(  v_id_afi_fondo72,
                                       TODAY,
                                       40,
                                       601,  --"ABONO RET F72 NO PAGADO TANTO ADICIONAL"
                                       p_folio,
                                       v_id_solicitud,
                                       v_saldo,
                                       NULL,
                                       TODAY,
                                       v_hProceso,
                                       v_origen);
   END FOREACH;

    --Indicamos que el folio fue liquidado
   UPDATE glo_folio SET status = 2 WHERE folio = p_folio;
   
   SET PDQPRIORITY DEFAULT;
    
   RETURN v_respuesta, v_mensaje;
END FUNCTION;


