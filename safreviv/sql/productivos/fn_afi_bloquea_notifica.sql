






CREATE PROCEDURE "safreviv".fn_afi_bloquea_notifica( p_nss                CHAR(11),
                                          p_tpo_notificacion   SMALLINT,
                                          p_ind_notificacion   SMALLINT,
                                          p_folio              DECIMAL(9,0),
                                          p_fuente             SMALLINT )
   RETURNING SMALLINT;

   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_fecha                   DATE;
   DEFINE v_f_inicio                DATE;
   DEFINE v_fecha_fin               DATE;
   DEFINE v_fuente_fin              SMALLINT;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_bloq_notifica           SMALLINT;
   DEFINE v_hora                    DATETIME HOUR TO SECOND;

   DEFINE r_excepcion               SMALLINT;

   LET v_fecha = TODAY;
   LET v_hora  = CURRENT;
   LET r_excepcion = 0;

   IF p_tpo_notificacion = 3 THEN
      LET v_bloq_notifica = 2;
   ELIF p_tpo_notificacion = 4 THEN
      LET v_bloq_notifica = 1;
   END IF

   SELECT id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = p_nss;
   
   IF v_id_derechohabiente IS NULL OR
      v_id_derechohabiente <= 0 THEN
      LET r_excepcion = 1; ---DERECHOHABIENTE NO EXISTE
   ELSE
      IF NOT EXISTS (SELECT tpo_notificacion
                       FROM cat_afi_tpo_notifica
                      WHERE tpo_notificacion = p_tpo_notificacion) THEN
         LET r_excepcion = 2; --EL TIPO DE NOTIFICACIÓN NO EXISTE
      ELSE
         IF p_ind_notificacion NOT BETWEEN 0 AND 1 THEN
            LET r_excepcion = 3; --EL IDENTIFICADOR O MARCA ES INCORRECTO
         ELSE
            IF p_ind_notificacion = 1 THEN
               SELECT f_inicio
                 INTO v_f_inicio
                 FROM afi_ind_notifica
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND tpo_notificacion   = v_bloq_notifica;
                  
               IF v_f_inicio IS NULL OR
                  v_f_inicio = '12/31/1899' THEN
                  LET v_estado = 99; --NO EXISTIA LA MARCA DE INDICADORES
                  LET r_excepcion = 3; --EL IDENTIFICADOR O MARCA ES INCORRECTO O NO EXISTE
               ELSE

                  DELETE
                    FROM afi_ind_notifica
                   WHERE id_derechohabiente = v_id_derechohabiente
                     AND tpo_notificacion = v_bloq_notifica;

                  LET v_estado = 0;

                  INSERT INTO afi_ind_notifica VALUES(v_id_derechohabiente,
                                                      p_tpo_notificacion,
                                                      v_fecha,
                                                      v_hora,
                                                      p_folio,
                                                      p_fuente);
               END IF

               INSERT INTO afi_his_ind_notifica VALUES (v_id_derechohabiente,
                                                        p_tpo_notificacion,
                                                        p_ind_notificacion,
                                                        v_fecha,
                                                        v_hora,
                                                        p_fuente,
                                                        p_folio,
                                                        v_estado);
            END IF
         END IF
      END IF
   END IF

   RETURN r_excepcion;
END PROCEDURE;


