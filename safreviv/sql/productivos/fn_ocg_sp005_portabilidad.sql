






CREATE FUNCTION "safreviv".fn_ocg_sp005_portabilidad (p_nss         CHAR(11),
                                           p_entidad     smallint,
                                           p_tpo_credito CHAR(1),
                                           p_f_liquida   date)
   RETURNING SMALLINT

   DEFINE v_id_ocg_detalle      decimal(9,0);
   DEFINE v_id_ocg_liquidacion  decimal(9,0);
   DEFINE v_error               smallint;
   DEFINE v_nss                 char(11);
   DEFINE v_entidad             smallint;
   DEFINE v_num_ctr_int         char(18);
   DEFINE v_imp_ap              decimal(12,2);
   DEFINE v_imp_gtia            decimal(12,2);
   DEFINE v_rfc                 char(13);
   DEFINE v_id_dh               decimal(9,0);
   DEFINE v_id_ocg_formalizacion decimal(9,0);
   DEFINE v_id_ocg_tramite      decimal(9,0);
   DEFINE v_periodo             char(6);
   DEFINE v_f_deposito          char(8);
   DEFINE v_f_libera_gtia       DATE;
   DEFINE v_tpo_credito         CHAR(1);

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_sp005_portabilidad.trace';
   TRACE ON;

   LET v_error              = 0;
   LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
   LET v_id_ocg_liquidacion = seq_ocg_liquidacion.nextval;
   LET v_nss                = p_nss;
   LET v_entidad            = p_entidad;
   LET v_tpo_credito        = p_tpo_credito;
   LET v_f_libera_gtia      = p_f_liquida;
   LET v_imp_ap             = '';
   LET v_imp_gtia           = '';
   LET v_rfc                = '';
   LET v_f_deposito         = '';
   LET v_periodo            = '';
   LET v_id_ocg_formalizacion = '';
   LET v_id_ocg_tramite     = '';

   -- Se búsca id_derechohabiente de afi_derechohabiente

   SELECT id_derechohabiente
     INTO v_id_dh
     FROM afi_derechohabiente
    WHERE nss = v_nss;

   -- Se inserta en la tabla de detalle
   INSERT INTO ocg_detalle
        VALUES( v_id_ocg_detalle,
                0,
                v_id_dh,
                5,
                today,
                v_entidad,
                v_nss );

   -- Se inserta en ocg_fecha_mig
   INSERT INTO ocg_fecha_mig
        VALUES(v_id_ocg_liquidacion,
               v_id_ocg_detalle,
               v_id_dh,
               today,
               today,
               '',
               '',
               5,
               today);

   -- Se inserta en id_ocg_liquidacion
   INSERT INTO ocg_liquidacion
        VALUES(v_id_ocg_liquidacion,
               v_id_ocg_detalle,
               v_id_ocg_formalizacion,
               v_id_ocg_tramite,
               v_id_dh,
               v_entidad,
               "0",
               v_periodo,
               v_imp_gtia,
               v_f_libera_gtia,
               v_imp_ap,
               3,
               v_f_deposito,
               v_tpo_credito,
               1,
               70,
               145
               );

   RETURN v_error;
END FUNCTION;


