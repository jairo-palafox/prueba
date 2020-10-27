






CREATE FUNCTION "safreviv".fn_cbd_integra_ajuste_operativo(p_folio DECIMAL(9,0), p_usuario CHAR(20))
RETURNING SMALLINT,VARCHAR(200);

   DEFINE v_folio_ant         DECIMAL(9,0);

   --Variables de salida
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);

   --Variables para validaciones
   DEFINE v_reg_sin_cuenta    INTEGER;

   --Variables para las cifras globales
   DEFINE v_fondo             SMALLINT;
   DEFINE v_subcuenta         SMALLINT;
   DEFINE v_sum_acciones      DECIMAL(26,6);
   DEFINE v_num_registros     INTEGER;

   --Se inicializan las variables de respuesta
   LET v_resultado = 0;
   LET v_mensaje = "El archivo se integro correctamente";

   SET PDQPRIORITY HIGH;

   --Se actualizan estadisticas de la tabla de detalles
   UPDATE statistics FOR TABLE safre_tmp:tmp_cbd_ajuste;

   --Se inderta el registro de control con estado 1 - Validado
   INSERT INTO cbd_ctr_ajuste_operativo (folio,f_proceso,estado,cod_usuario) VALUES (p_folio,TODAY,1,p_usuario);

   --Se insertan los registros de detalle con estado 2 - Integrado
   INSERT INTO cbd_detalle_ajuste_operativo
   SELECT
      seq_cbd_detalle_ajuste.NEXTVAL,
      p_folio,
      tmp.nss,
      afi.id_derechohabiente,
      tmp.subcuenta,
      tmp.fondo_inversion,
      tmp.movimiento,
      (tmp.monto_acciones / 1000000),
      tmp.f_valor,
      2
   FROM safre_tmp:tmp_cbd_ajuste tmp
   LEFT JOIN afi_derechohabiente afi ON afi.nss = tmp.nss;

   --Se actualizan estadisticas de la tabla de detalles
   UPDATE statistics FOR TABLE cbd_detalle_ajuste_operativo;

   --Se buscan los registros sin cuenta en SAFRE
   SELECT COUNT(*)
   INTO v_reg_sin_cuenta
   FROM cbd_detalle_ajuste_operativo
   WHERE folio = p_folio
   AND id_derechohabiente IS NULL;

   IF (v_reg_sin_cuenta IS NOT NULL AND v_reg_sin_cuenta > 0) THEN
      --RECHAZO: Existen registros que no se encuentran en SAFRE, estado = 11 - Cuenta inexistente
      UPDATE cbd_detalle_ajuste_operativo SET estado = 11 WHERE folio = p_folio AND id_derechohabiente IS NULL;
      UPDATE cbd_ctr_ajuste_operativo SET estado = 11 WHERE folio = p_folio;
      
      LET v_resultado = 1;
      LET v_mensaje = "Archivo rechazado por registros sin cuenta en SAFRE";
      RETURN v_resultado, v_mensaje;
   END IF

   --Se actualiza el proceso al estado 2 - Integrado
   UPDATE cbd_ctr_ajuste_operativo SET estado = 2 WHERE folio = p_folio;
   
   --Se calculan las cifras globales
   FOREACH
      SELECT
         fondo_inversion,
         subcuenta,
         SUM(monto_acciones),
         COUNT(*)
      INTO
         v_fondo,
         v_subcuenta,
         v_sum_acciones,
         v_num_registros
      FROM cbd_detalle_ajuste_operativo
      WHERE folio = p_folio
      GROUP BY 1,2

      INSERT INTO cbd_cifras_ajuste_operaivo (  id_cbd_cifras_ajuste_operativo,
                                                folio,
                                                fondo_inversion,
                                                subcuenta,
                                                total_acciones,
                                                total_registros) 
                                       VALUES ( seq_cbd_cifras_ajuste.NEXTVAL,
                                                p_folio,
                                                v_fondo,
                                                v_subcuenta,
                                                v_sum_acciones,
                                                v_num_registros);
   END FOREACH;

   
   
   SET PDQPRIORITY DEFAULT;
   
   RETURN v_resultado, v_mensaje;
END FUNCTION;


