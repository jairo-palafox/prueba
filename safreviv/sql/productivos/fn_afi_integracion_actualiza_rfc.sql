






CREATE FUNCTION "safreviv".fn_afi_integracion_actualiza_rfc(p_usuario_cod CHAR(20), p_folio DECIMAL(10), 
                                           p_nombre_archivo CHAR(40), p_pid DECIMAL(9,0),
                                           p_proceso_cod SMALLINT)

RETURNING INTEGER, INTEGER, VARCHAR(255), DECIMAL(9,0), DECIMAL(9,0), DECIMAL(9,0)

   -- variables de la tabla temporal
   DEFINE v_act_nss11               CHAR(11)   ;
   DEFINE v_act_nss10               CHAR(10)   ;
   DEFINE v_act_rfc                 VARCHAR(13);

   -- variables para validacion por tipo de registro
   DEFINE v_reg_leidos              DECIMAL(9,0);
   DEFINE v_reg_actualizados        DECIMAL(9,0);
   DEFINE v_reg_rechazados          DECIMAL(9,0);

   -- variables para procesar los cambios
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_rfc_anterior            CHAR(13);

   -- Control de Excepciones
   DEFINE v_i_resultado             SMALLINT;
   DEFINE sql_err                   INTEGER;
   DEFINE isam_err                  INTEGER;
   DEFINE err_txt                   VARCHAR(255);

   -- se define el comportamiento en la ocurrencia de una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN sql_err, isam_err, err_txt, v_reg_leidos, v_reg_actualizados, v_reg_rechazados;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_afi_actualiza_rfc.trace";
   --TRACE ON;

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado = 0;
   LET sql_err       = 0;
   LET isam_err      = 0;
   LET err_txt       = "Integracion finalizada.";

   LET v_reg_leidos       = 0;
   LET v_reg_actualizados = 0;
   LET v_reg_rechazados   = 0;

   -- se asigna el folio a la operacion
   UPDATE bat_ctr_operacion
      SET folio = p_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod = 2;

   UPDATE bat_ctr_proceso
      SET folio = p_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod;

   -- se cambia el estatus del archivo a integrado
   UPDATE glo_ctr_archivo
      SET estado = 2, -- integrado
          folio = p_folio
    WHERE proceso_cod = p_proceso_cod
      AND nombre_archivo = p_nombre_archivo;

   -- se crea la tabla temporal para los rechazos
   DROP TABLE IF EXISTS tmp_actualiza_rfc_rch;

   CREATE TEMP TABLE tmp_actualiza_rfc_rch (
      nss11       CHAR(11)   ,
      nss10       CHAR(10)   ,
      rfc         VARCHAR(13),
      cod_rechazo SMALLINT);

   -- se obtienen los totales de la tabla de sumario
   FOREACH
      SELECT nss,
             nss10,
             TRIM(rfc)
        INTO v_act_nss11,
             v_act_nss10,
             v_act_rfc
        FROM safre_tmp:tmp_actualiza_rfc

      -- se cuenta un registro leido
      LET v_reg_leidos = v_reg_leidos + 1;

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente, rfc
      INTO   v_id_derechohabiente, v_rfc_anterior
      FROM   afi_derechohabiente
      WHERE  nss = v_act_nss11;

      -- si el derechohabiente no existe, se cuenta como rechazado
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se cuenta un registro rechazado
         LET v_reg_rechazados = v_reg_rechazados + 1;

         -- se inserta en la tabla de rechazos
         INSERT INTO tmp_actualiza_rfc_rch (nss11       ,
                                            nss10       ,
                                            rfc         ,
                                            cod_rechazo )
                                    VALUES (v_act_nss11,
                                            v_act_nss10,
                                            v_act_rfc  ,
                                            1); -- NSS no existe 

         CONTINUE FOREACH;
      ELSE
         -- si el RFC no tiene la longitud suficiente, se salta
         IF ( LENGTH(TRIM(v_act_rfc)) < 13 ) THEN
            IF ( LENGTH(TRIM(v_act_rfc)) <> 10 ) THEN
               -- se rechaza el cambio
               LET v_reg_rechazados = v_reg_rechazados + 1;

               -- se inserta en la tabla de rechazos
               INSERT INTO tmp_actualiza_rfc_rch (nss11       ,
                                                  nss10       ,
                                                  rfc         ,
                                                  cod_rechazo )
                                          VALUES (v_act_nss11,
                                                  v_act_nss10,
                                                  v_act_rfc  ,
                                                  2); -- RFC longitud invalida

               CONTINUE FOREACH;
            END IF
         END IF

         -- se guarda el RFC en el historico
         INSERT INTO afi_his_derechohabiente (id_derechohabiente ,
                                              f_modifica         ,
                                              folio_lote_modifica,
                                              ind_modifica       ,
                                              rfc)
                                      VALUES (v_id_derechohabiente,
                                              TODAY               ,
                                              p_folio             ,
                                              2                   , -- cambio de rfc
                                              v_rfc_anterior);

         -- se actualiza el RFC
         UPDATE afi_derechohabiente
            SET rfc = v_act_rfc
          WHERE id_derechohabiente = v_id_derechohabiente;

         -- se cuenta un registro actualizado
         LET v_reg_actualizados = v_reg_actualizados + 1;
      END IF
   END FOREACH;

   RETURN sql_err, isam_err, err_txt, v_reg_leidos, v_reg_actualizados, v_reg_rechazados;

END FUNCTION
;


