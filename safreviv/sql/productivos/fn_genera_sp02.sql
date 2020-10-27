






CREATE FUNCTION "safreviv".fn_genera_sp02()
   RETURNING SMALLINT


DEFINE v_id_liquidacion   decimal(9,0);
DEFINE v_id_tramite       decimal(9,0);
DEFINE v_id_dh            decimal(9,0);
DEFINE v_ef               smallint;
DEFINE v_nss              char(11);
DEFINE v_error            smallint;
DEFINE v_seq_detalle      decimal(9,0);
DEFINE v_seq_formalizacion decimal(9,0);


ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_genera_sp02.trace';
   TRACE ON;

   LET v_error      = 0;

   FOREACH
      select id_ocg_liquidacion,
             id_ocg_tramite,
             id_derechohabiente,
             cve_ent_financiera
        into v_id_liquidacion,
             v_id_tramite,
             v_id_dh,
             v_ef
        from ocg_liquidacion
       where situacion = 155

      select nss
        into v_nss
        from afi_derechohabiente
       where id_derechohabiente = v_id_dh;

      LET v_seq_detalle       = seq_ocg_detalle.NEXTVAL;
      LET v_seq_formalizacion = seq_ocg_formalizacion.NEXTVAL;

      insert into ocg_detalle
           values (v_seq_detalle,
                   0,
                   v_id_dh,
                   4,
                   today,
                   v_ef,
                   v_nss );

     insert into ocg_formalizacion
          values(v_seq_formalizacion,
                 v_seq_detalle,
                 v_id_tramite,
                 v_id_dh,
                 v_ef,
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 '',
                 155,
                 '',
                 '',
                 '',
                 '');
      update ocg_liquidacion
         set id_ocg_formalizacion   = v_seq_formalizacion
       where id_ocg_liquidacion     = v_id_liquidacion;


      LET v_id_liquidacion = NULL;
      LET v_id_tramite     = NULL;
      LET v_id_dh          = NULL;
      LET v_ef             = NULL;
      LET v_nss            = NULL;

   END FOREACH

RETURN v_error;
END FUNCTION;


