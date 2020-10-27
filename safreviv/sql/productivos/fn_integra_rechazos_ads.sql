






CREATE FUNCTION "safreviv".fn_integra_rechazos_ads()
RETURNING SMALLINT,VARCHAR(200);

   --Variables de salida
   DEFINE v_respuesta         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);
 
   DEFINE v_nss_ads           CHAR(11);
   DEFINE v_estado_ads        SMALLINT;
   DEFINE v_rechazo_ads       SMALLINT;
   
   SET PDQPRIORITY HIGH; 

   --Se inicializan las variables de respuesta
   LET v_respuesta = 0;
   LET v_mensaje = "El archivo se integro correctamente";

   LET v_estado_ads = 100;
   FOREACH
      SELECT
         nss,
         cod_rechazo
      INTO
         v_nss_ads,
         v_rechazo_ads
      FROM safre_tmp:tmp_fondo72_rechazo_ads
      --Se actualiza solo si el estado actual es 65 (pendiente de pago)
      UPDATE ret_fondo_ahorro_masivo 
      SET estado_solicitud = v_estado_ads, cod_rechazo = v_rechazo_ads
      WHERE nss = v_nss_ads AND estado_solicitud IN (65,66);               
   END FOREACH;
   SET PDQPRIORITY DEFAULT;
    
   RETURN v_respuesta, v_mensaje;
END FUNCTION;


