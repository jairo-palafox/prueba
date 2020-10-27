






CREATE FUNCTION "safreviv".fn_verifica_edo_trab(p_nss CHAR(11))

RETURNING SMALLINT, DECIMAL(9,0), CHAR(4), VARCHAR(140)

   DEFINE vid_derechohabiente       DECIMAL(9,0);
   DEFINE vrchCod                   SMALLINT;
   DEFINE vcodResp                  CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE vbnd                      SMALLINT;
   DEFINE vcodRch                   SMALLINT;

   DEFINE v_valida                  SMALLINT;
   DEFINE v_resultado               SMALLINT;
   DEFINE v_tpo_originacion         SMALLINT;
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_f_otorga                DATE;
   DEFINE v_f_liquida               DATE;
   DEFINE verror                    SMALLINT; -- código de error en caso de excepción
   DEFINE v_tpo_dscto               SMALLINT;

   ON EXCEPTION SET verror
      -- Devolverá el código de error cuando ocurra una excepción
      LET vcodResp  = "2999";
      LET vdescResp = "ERROR EN PROCESO, excepcion: "||verror;

      RETURN verror, vid_derechohabiente, vcodResp, vdescResp;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/verificaEdoTrab.trace';
   ---TRACE ON;

   LET verror    = 0;
   LET vbnd      = 0;
   LET vrchCod   = 0;
   LET vcodResp  = "2000";
   LET vdescResp = "CUENTA LIBRE";
   LET v_valida  = 1;
   LET v_tpo_originacion = NULL;
   LET v_tpo_credito     = NULL;
   LET v_num_credito     = NULL;
   LET v_f_otorga        = "";
   LET v_f_liquida       = "";

   SELECT afi.id_derechohabiente
     INTO vid_derechohabiente
     FROM afi_derechohabiente as afi
    WHERE afi.nss = p_nss;

   IF vid_derechohabiente IS NULL THEN
      LET vcodResp  = "211";
      LET vdescResp = "TRABAJADOR NO EXISTE";
      LET vbnd      = 1;
   END IF

   IF vbnd = 0 THEN
     FOREACH
         SELECT c.rch_cod, d.rch_desc
           INTO vrchCod, vdescResp
           FROM sfr_convivencia AS c,
                sfr_marca_activa AS a,
                cat_rch_marca AS d
          WHERE a.id_derechohabiente = vid_derechohabiente
            AND a.marca              = c.marca_activa
            AND c.marca_entra IN(
                     SELECT DISTINCT ctm.marca_inf
                       FROM cat_tipo_credito AS ctm
                      WHERE ctm.marca_inf IS NOT NULL)
            AND c.rch_cod > 0
            AND c.rch_cod = d.rch_cod
            AND c.marca_activa NOT IN(
                     SELECT DISTINCT ctm.marca_inf
                       FROM cat_tipo_credito AS ctm
                      WHERE ctm.marca_inf IS NOT NULL)
          ORDER  BY a.f_inicio DESC

         IF vrchCod IS NOT NULL THEN
            LET vcodResp = "2"||vrchCod;

            IF vrchCod = 213 OR vrchCod = 234 THEN
               IF vrchCod = 234  THEN
                  LET vdescResp = "MARCA PROCEDENTE";
                  LET vcodResp  = "3000";
               END IF

               LET vbnd = 0;
            ELSE
               LET vbnd = 1;
            END IF

            EXIT FOREACH;
         END IF
      END FOREACH;

      IF vbnd = 0 THEN
         CALL fn_edo_cred_viv (vid_derechohabiente, v_valida)
         RETURNING v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;

         IF v_resultado = 0 THEN
            SELECT desc_credito
              INTO vdescResp
              FROM cat_tipo_credito
             WHERE tpo_originacion = v_tpo_originacion
               AND tpo_credito     = v_tpo_credito;

            LET vcodResp  = "212";
            LET vdescResp = "CUENTA CON CRÉDITO VIGENTE "||vdescResp;
            LET vbnd      = 1;
         END IF
      END IF
   END IF

   RETURN verror, vid_derechohabiente, vcodResp, vdescResp;

END FUNCTION
;


