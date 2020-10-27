






CREATE PROCEDURE "safreviv".sp_integra_ind_notificacion(p_folio DECIMAL(9,0))
RETURNING INTEGER, INTEGER, VARCHAR(255);

   DEFINE v_nss            CHAR(11);
   DEFINE v_tpo_notifica   SMALLINT;
   DEFINE v_ind_correo     SMALLINT;
   DEFINE v_ind_sms        SMALLINT;
   DEFINE v_ind_bloq_correo SMALLINT;
   DEFINE v_ind_bloq_sms   SMALLINT;
   DEFINE v_fuente         SMALLINT;
   
   DEFINE v_query          CHAR(150);
   
   DEFINE v_resultado      SMALLINT;

   DEFINE v_desmarca_sms    INTEGER;
   DEFINE v_marca_sms       INTEGER;
   DEFINE v_desmarca_correo INTEGER;
   DEFINE v_marca_correo    INTEGER;
   DEFINE v_bloqueo_sms     INTEGER;
   DEFINE v_bloqueo_correo  INTEGER;
   DEFINE v_tot_det         INTEGER;
   DEFINE v_tot_sum         INTEGER;

   DEFINE v_error               INTEGER       ;
   DEFINE isam_err              INTEGER       ;
   DEFINE r_txt                 VARCHAR(255)  ;

   ON EXCEPTION
      SET v_error, isam_err, r_txt
      RETURN v_error, isam_err, r_txt;
   END EXCEPTION

   LET v_error = 0;
   LET isam_err = 0;
   LET r_txt = "";

   SET DEBUG FILE TO "/tmp/sp_integra_ind_notificacion.log";
   TRACE ON;

   DROP TABLE IF EXISTS afi_rch_ind_not;
   CREATE TABLE afi_rch_ind_not (
                   nss                CHAR(11),
                   tpo_notificacion   SMALLINT,
                   indicador          SMALLINT,
                   fuente             SMALLINT,
                   folio_lote         DECIMAL(9,0),
                   cod_excepcion      SMALLINT) IN tmp_4_dbs;

   LET v_error = 0;

   FOREACH SELECT nss,
                  ind_sms,
                  ind_correo,
                  bloqueo_int_sms,
                  bloqueo_int_correo,
                  fuente
             INTO v_nss,
                  v_ind_sms,
                  v_ind_correo,
                  v_ind_bloq_sms,
                  v_ind_bloq_correo,
                  v_fuente
             FROM safre_tmp:tmp_det_ind_not

      IF v_ind_correo IS NOT NULL AND
         (v_ind_correo = 0 OR
          v_ind_correo = 1) THEN
   
         LET v_tpo_notifica = 2;
         CALL fn_afi_activa_notifica(v_nss,
                                    v_tpo_notifica,
                                    v_ind_correo,
                                    p_folio,
                                    v_fuente) RETURNING v_resultado ;
         IF v_resultado <> 0 THEN
            INSERT INTO afi_rch_ind_not VALUES(v_nss,
                                               v_tpo_notifica,
                                               v_ind_correo,
                                               v_fuente,
                                               p_folio,
                                               v_resultado);
            --LET v_error = 1;
         END IF
      END IF
   
      -----NOTIFICACION SMS-------
      IF v_ind_sms IS NOT NULL AND
         (v_ind_sms  = 0 OR
          v_ind_sms  = 1) THEN
   
         LET v_tpo_notifica = 1;
         CALL fn_afi_activa_notifica(v_nss,
                                     v_tpo_notifica,
                                     v_ind_sms ,
                                     p_folio,
                                     v_fuente)
                                RETURNING v_resultado;
         IF v_resultado <> 0 THEN
            INSERT INTO afi_rch_ind_not VALUES(v_nss,
                                               v_tpo_notifica,
                                               v_ind_sms ,
                                               v_fuente,
                                               p_folio,
                                               v_resultado);
            --LET v_error = 1;
         END IF
      END IF
   
      ---BLOQUEO INTERNO CORREO ELECTRONICO----- 
      IF v_ind_bloq_correo IS NOT NULL AND
          v_ind_bloq_correo = 1 THEN
   
         LET v_tpo_notifica = 3;
         CALL fn_afi_bloquea_notifica(v_nss,
                                      v_tpo_notifica,
                                      v_ind_bloq_correo,
                                      p_folio,
                                      v_fuente)
                                RETURNING v_resultado;
         IF v_resultado <> 0 THEN
            INSERT INTO afi_rch_ind_not VALUES(v_nss,
                                               v_tpo_notifica,
                                               v_ind_bloq_correo,
                                               v_fuente,
                                               p_folio,
                                               v_resultado);
            --LET v_error = 1;
         END IF
      END IF
   
      -----BLOQUEO INTERNO SMS-------
      IF v_ind_bloq_sms IS NOT NULL AND
          v_ind_bloq_sms  = 1 THEN
   
         LET v_tpo_notifica = 4;
         CALL fn_afi_bloquea_notifica(v_nss,
                                      v_tpo_notifica,
                                      v_ind_bloq_sms ,
                                      p_folio,
                                      v_fuente)
                                RETURNING v_resultado;
         IF v_resultado <> 0 THEN
            INSERT INTO afi_rch_ind_not VALUES(v_nss,
                                               v_tpo_notifica,
                                               v_ind_bloq_sms,
                                               v_fuente,
                                               p_folio,
                                               v_resultado);
            --LET v_error = 1;
         END IF
      END IF
      
   END FOREACH
   
   SELECT COUNT(*)
     INTO v_desmarca_sms
     FROM safre_tmp:tmp_det_ind_not
    WHERE ind_sms = 0;

   SELECT COUNT(*)
   INTO v_marca_sms
   FROM safre_tmp:tmp_det_ind_not
   WHERE ind_sms = 1;

   SELECT COUNT(*)
   INTO v_desmarca_correo
   FROM safre_tmp:tmp_det_ind_not
   WHERE ind_correo = 0;

   SELECT COUNT(*)
   INTO v_marca_correo
   FROM safre_tmp:tmp_det_ind_not
   WHERE ind_correo = 1;
   
   SELECT COUNT(*)
   INTO v_bloqueo_sms
   FROM safre_tmp:tmp_det_ind_not
   WHERE bloqueo_int_sms = 1;
   
   SELECT COUNT(*)
   INTO v_bloqueo_correo
   FROM safre_tmp:tmp_det_ind_not
   WHERE bloqueo_int_correo = 1;
   
   SELECT COUNT(*)
   INTO v_tot_det
   FROM safre_tmp:tmp_det_ind_not;
   
   SELECT tot_registros
   INTO v_tot_sum
   FROM safre_tmp:tmp_sum_ind_not;

   INSERT INTO afi_ind_cifras VALUES(p_folio,
                                     v_desmarca_sms   ,
                                     v_marca_sms      ,
                                     v_desmarca_correo,
                                     v_marca_correo   ,
                                     v_bloqueo_sms    ,
                                     v_bloqueo_correo ,
                                     v_tot_det        ,
                                     v_tot_sum        );

   RETURN v_error, isam_err, r_txt;
END PROCEDURE
;


