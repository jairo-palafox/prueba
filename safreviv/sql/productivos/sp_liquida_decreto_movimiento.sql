






CREATE PROCEDURE "safreviv".sp_liquida_decreto_movimiento (p_folio       DECIMAL(9,0),
                                    p_usuario     CHAR(20)    ,
                                    p_pid         DECIMAL(9,0),
                                    p_proceso_cod SMALLINT    ,
                                    p_opera_cod   SMALLINT    ,
                                    p_tabla       CHAR(30)    )

   RETURNING SMALLINT, INTEGER, VARCHAR(250)

   DEFINE v_cadena                            CHAR(300);
   DEFINE v_status                            SMALLINT;
   DEFINE sql_err                             INTEGER ;
   DEFINE isam_err                            INTEGER ;
   DEFINE error_info                          CHAR(70);
   DEFINE v_tia_preliquida_f_liquida          DATE;
   DEFINE v_tia_preliquida_id_derechohabiente DECIMAL(9,0);
   DEFINE v_tia_preliquida_subcuenta          SMALLINT;
   DEFINE v_tia_preliquida_fondo_inversion    SMALLINT;
   DEFINE v_tia_preliquida_movimiento         SMALLINT;
   DEFINE v_tia_preliquida_folio_liquida      DECIMAL(10,0);
   DEFINE v_tia_preliquida_id_referencia      DECIMAL(9,0) ;
   DEFINE v_tia_preliquida_monto_acciones     DECIMAL(20,2);
   DEFINE v_tia_preliquida_monto_pesos        DECIMAL(20,2);
   DEFINE v_tia_preliquida_f_valor            DATE;
   DEFINE v_tia_preliquida_f_registro         DATE;
   DEFINE v_tia_preliquida_h_registro         DATETIME HOUR TO SECOND;
   DEFINE v_tia_preliquida_origen             CHAR(30);
   DEFINE v_tia_preliquida_id_decreto         DECIMAL(9,0);
   DEFINE v_nss_tia_det_traspaso              VARCHAR(11);
   DEFINE v_d_id_derechohabiente              DECIMAL(9,0);
   DEFINE v_v_tabla                           CHAR(30);
   DEFINE v_i_valida_id_derechohabiente       SMALLINT;
   DEFINE v_tia_det_trabajador_curp           CHAR(18);
   DEFINE v_tia_det_trabajador_rfc            CHAR(13);
   DEFINE v_tia_det_trabajador_nom_trabajador CHAR(50);
   DEFINE v_tia_detpaterno_afo_recep          VARCHAR(40);
   DEFINE v_tia_detmaterno_afo_recep          VARCHAR(40);
   DEFINE v_tia_detnombres_afo_recep          VARCHAR(40);
   DEFINE v_existe_derechohabiente            SMALLINT;
   DEFINE v_acc_mov1011                       DECIMAL(16,2);   --saci2017-37
   DEFINE v_acc_mov231                        DECIMAL(16,2);   --saci2017-37
   DEFINE v_diferencia                        DECIMAL(12,2);   --saci2017-37
   DEFINE v_pesos                             DECIMAL(12,2);   --saci2017-37    
   DEFINE v_precio_fondo                      DECIMAL(19,14);  --saci2017-37
   DEFINE v_nss                               CHAR(11);        --SACI2017-37
   DEFINE v_mov1993                           SMALLINT;        --SACI2017-37
   DEFINE v_cont_curp                         SMALLINT;        --tia01

   DEFINE v_error_isam INTEGER;
   DEFINE v_mensaje    VARCHAR(250);

   -- en caso de excepcion
   ON EXCEPTION SET v_status, v_error_isam, v_mensaje

      RETURN v_status, v_error_isam, v_mensaje;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv/tia/tra/saci2017-37/liquida_decreto.txt';
   --TRACE ON;

   -- se asume que no hay error
   LET v_status     = 0;
   LET v_error_isam = 0;
   LET v_mensaje    = "Proceso de liquidación finalizado correctamente";

   LET v_v_tabla = TRIM(p_tabla) ;
   LET v_i_valida_id_derechohabiente = 0;

   SET PDQPRIORITY HIGH;

   LET v_cadena = " INSERT INTO cta_decreto "||
                  " SELECT pre.* "||
                  " FROM tia_preliquida pre,"||
                  "      tia_det_traspaso det "||
                  " WHERE pre.folio_liquida = "||p_folio||
                  " AND   pre.folio_liquida = det.folio "||
                  " AND   pre.id_referencia = det.id_referencia "||
                  " AND   pre.monto_pesos < 0 "||
                  " AND   det.result_operacion = '01' ";


   EXECUTE IMMEDIATE v_cadena;

   FOREACH
      SELECT f_liquida,
             pre.id_decreto,
             subcuenta,
             fondo_inversion,
             movimiento,
             folio_liquida,
             pre.id_referencia,
             monto_acciones,
             monto_pesos,
             f_valor,
             f_registro,
             h_registro,
             origen,
             TRIM(nss_afo_recep)
      INTO   v_tia_preliquida_f_liquida,
             v_tia_preliquida_id_decreto,
             v_tia_preliquida_subcuenta,
             v_tia_preliquida_fondo_inversion,
             v_tia_preliquida_movimiento,
             v_tia_preliquida_folio_liquida,
             v_tia_preliquida_id_referencia,
             v_tia_preliquida_monto_acciones,
             v_tia_preliquida_monto_pesos,
             v_tia_preliquida_f_valor,
             v_tia_preliquida_f_registro,
             v_tia_preliquida_h_registro,
             v_tia_preliquida_origen,
             v_nss
      FROM   tia_preliquida   pre,
             tia_det_traspaso det
      WHERE folio_liquida        = p_folio
      AND   monto_pesos          > 0
      AND   pre.folio_liquida    = folio
      AND   pre.id_referencia    = det.id_referencia
      AND   det.result_operacion = "01"
      ORDER  BY nss_afo_recep,movimiento

      SELECT TRIM(nss_afo_recep),
             curp,
             rfc_afo_recep,
             TRIM(paterno_afo_recep),
             TRIM(materno_afo_recep),
             TRIM(nombres_afo_recep)
      INTO   v_nss_tia_det_traspaso,
             v_tia_det_trabajador_curp,
             v_tia_det_trabajador_rfc,
             v_tia_detpaterno_afo_recep,
             v_tia_detmaterno_afo_recep,
             v_tia_detnombres_afo_recep
      FROM  tia_det_traspaso
      WHERE folio            = p_folio
      AND   id_referencia    = v_tia_preliquida_id_referencia
      AND   result_operacion = "01";

      UPDATE tia_det_traspaso
      SET    result_operacion = '99'     --- AIV CONFIRMADA EN cta_his_decreto
      WHERE  folio            = p_folio
      AND    id_referencia    = v_tia_preliquida_id_referencia
      AND    result_operacion = "01";

      --Se verifica que exista el derechohabiente con ese NSS
      SELECT COUNT(*)
      INTO   v_existe_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = v_nss_tia_det_traspaso;

      IF v_existe_derechohabiente<>0 THEN
          --SE VERIFICA QUE NO SE EL NSS NULO
          SELECT COUNT(*)
          INTO   v_existe_derechohabiente
          FROM   afi_derechohabiente
          WHERE  nss = v_nss_tia_det_traspaso
          AND id_derechohabiente=52112212;
          IF v_existe_derechohabiente<>0 THEN
             LET v_d_id_derechohabiente=0;
          ELSE
             SELECT id_derechohabiente
             INTO   v_d_id_derechohabiente
             FROM   afi_derechohabiente
             WHERE  nss = v_nss_tia_det_traspaso;
          END IF
      ELSE
          LET v_d_id_derechohabiente=0;
      END IF

      --Si no se encontro el ID_DERECHOHABIENTE por NSS se busca por CURP
      IF v_d_id_derechohabiente=0 THEN

         SELECT COUNT(*)                                   --tia01
         INTO   v_cont_curp                                --tia01
         FROM   afi_derechohabiente                        --tia01
         WHERE  curp  = v_tia_det_trabajador_curp;                            --tia01

         IF (v_cont_curp = 0) OR (v_cont_curp > 1) THEN    --tia01
            UPDATE tia_det_traspaso
            SET    result_operacion = "08"  --CURP no existe en afi_derechohabiente
            WHERE  folio            = p_folio
            AND    id_referencia    = v_tia_preliquida_id_referencia
            AND    result_operacion in ("99");
            CONTINUE FOREACH;
         END IF                                            --tia01
         
         SELECT id_derechohabiente                         --tia01
         INTO   v_d_id_derechohabiente
         FROM   afi_derechohabiente
         WHERE  curp  = v_tia_det_trabajador_curp; 

--  Se comenta siguiente query y se habilita el de arriba (tia01)
--  por correo de Claudia Navarrete del INF del 7-ene-2020 a las 15:20
--  con asunto: SD40545 INCIDENTE SACI FOLIO TIA 137323         
--         SELECT id_derechohabiente
--         INTO   v_d_id_derechohabiente
--         FROM   afi_derechohabiente
--         WHERE  curp = v_tia_det_trabajador_curp;         
      END IF

      INSERT INTO cta_movimiento
         (f_liquida,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          movimiento,
          folio_liquida,
          id_referencia,
          monto_acciones,
          monto_pesos,
          f_valor,
          f_registro,
          h_registro,
          origen)
      VALUES
         (v_tia_preliquida_f_liquida,
          v_d_id_derechohabiente,
          v_tia_preliquida_subcuenta,
          v_tia_preliquida_fondo_inversion,
          v_tia_preliquida_movimiento,
          v_tia_preliquida_folio_liquida,
          v_tia_preliquida_id_referencia,
          v_tia_preliquida_monto_acciones,
          v_tia_preliquida_monto_pesos,
          v_tia_preliquida_f_valor,
          v_tia_preliquida_f_registro,
          v_tia_preliquida_h_registro,
          v_tia_preliquida_origen
          );

--============ SACI2017-37 ===============

   LET v_acc_mov1011 = 0;
   SELECT monto_acciones                                           --saci2017-37
   INTO   v_acc_mov1011                                            --saci2017-37
   FROM   cta_movimiento                                           --saci2017-37
   WHERE  id_derechohabiente = v_d_id_derechohabiente              --saci2017-37
   AND    subcuenta          = 8                                   --saci2017-37
   AND    movimiento         = 1011;                               --saci2017-37 
--   AND    monto_acciones     = v_tia_preliquida_monto_acciones;  --saci2017-37

   IF v_acc_mov1011 > 0 THEN                --saci2017-37

      LET v_diferencia = v_acc_mov1011 - v_tia_preliquida_monto_acciones;

      --trace "id_derecho   "||v_d_id_derechohabiente;
      --trace "nss          "||v_nss;
      --trace "v_nss_tia_det"||v_nss_tia_det_traspaso;
      --trace "mov1011      "||v_acc_mov1011;
      --trace "mto_acc      "||v_tia_preliquida_monto_acciones;
      --trace "v_diferencia "||v_diferencia;
      --trace "folio        "||v_tia_preliquida_folio_liquida;
      
      LET v_acc_mov231 = 0;
      SELECT aivs_viv92
      INTO   v_acc_mov231
      FROM   tia_det_traspaso
      WHERE  folio               = v_tia_preliquida_folio_liquida
      AND    TRIM(nss_afo_recep) = v_nss_tia_det_traspaso
      AND    ROUND(aivs_viv92,2) = v_diferencia
      AND    result_operacion    = "99"; 

      --trace "v_acc_mov231   "||v_acc_mov231;

      LET v_mov1993 = 0;
      SELECT movimiento
      INTO   v_mov1993
      FROM   cta_movimiento
      WHERE  folio_liquida      = v_tia_preliquida_folio_liquida
      AND    id_derechohabiente = v_d_id_derechohabiente
      AND    f_liquida          = v_tia_preliquida_f_liquida
      AND    movimiento         = 1993;
      IF v_mov1993 IS NULL THEN
         LET v_mov1993 = 0;
      END IF

      --trace "v_mov1993      "||v_mov1993;
      
      IF v_acc_mov231 = v_diferencia AND v_mov1993 = 0 THEN
      
   	     LET v_acc_mov1011 = v_acc_mov1011 * -1;  --saci2017-37
   	     
   	     SELECT precio_fondo               --saci2017-37
   	     INTO   v_precio_fondo             --saci2017-37
   	     FROM   glo_valor_fondo            --saci2017-37
   	     WHERE  f_valuacion = TODAY        --saci2017-37 
   	     AND    fondo = 11;                --saci2017-37
   	     
   	     LET v_pesos = v_acc_mov1011 * v_precio_fondo;   --saci2017-37
         
         INSERT INTO cta_movimiento
            (f_liquida,
             id_derechohabiente,
             subcuenta,
             fondo_inversion,
             movimiento,
             folio_liquida,
             id_referencia,
             monto_acciones,
             monto_pesos,
             f_valor,
             f_registro,
             h_registro,
             origen)
         VALUES
            (v_tia_preliquida_f_liquida,
             v_d_id_derechohabiente,
             v_tia_preliquida_subcuenta,
             v_tia_preliquida_fondo_inversion,
             1993,                           -- mov cargo x variación menor retiros --saci2017-37
             v_tia_preliquida_folio_liquida,
             v_tia_preliquida_id_referencia,
             v_acc_mov1011,
             v_pesos,
             v_tia_preliquida_f_valor,
             v_tia_preliquida_f_registro,
             v_tia_preliquida_h_registro,
             v_tia_preliquida_origen
             );
      END IF
   END IF                                     --saci2017-37

--============ SACI2017-37 ===============

   END FOREACH;

--   DELETE
--   FROM   tia_preliquida
--   WHERE  folio_liquida = p_folio;

   UPDATE glo_folio
   SET    status = 2
   WHERE  folio = p_folio;

   UPDATE bat_ctr_proceso
   SET   folio       = p_folio
   WHERE pid         = p_pid
   AND   proceso_cod = p_proceso_cod;
   
   RETURN v_status, v_error_isam, v_mensaje;
END PROCEDURE
;


