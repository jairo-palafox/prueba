






CREATE FUNCTION "safreviv".fn_verifica_edo_trab_nva(p_nss CHAR(11))

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
   DEFINE verror                    SMALLINT; -- c�digo de error en caso de excepci�n
   DEFINE v_tpo_dscto               SMALLINT;

   ON EXCEPTION SET verror
      -- Devolver� el c�digo de error cuando ocurra una excepci�n
      LET vcodResp  = "2999";
      LET vdescResp = "ERROR EN PROCESO, excepcion: "||verror;

      RETURN verror, vid_derechohabiente, vcodResp, vdescResp;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv_req/homologa/pruebas/verificaEdoTrabNva.trace';
   --TRACE ON;

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
     FROM afi_derechohabiente afi
    WHERE afi.nss = p_nss;

   IF vid_derechohabiente IS NULL THEN
      LET vcodResp  = "211";
      LET vdescResp = "TRABAJADOR NO EXISTE";
      LET vbnd      = 1;
   ELSE
     FOREACH
        SELECT c.rch_cod, d.rch_desc
          INTO vrchCod, vdescResp
          FROM sfr_marca_activa a,
               sfr_convivencia c,
               cat_rch_marca d
         WHERE a.id_derechohabiente = vid_derechohabiente
           AND a.marca              = c.marca_activa
           AND c.marca_entra IN(
                    SELECT ctm.marca_inf
                      FROM cat_tipo_credito ctm
                     WHERE ctm.marca_inf IS NOT NULL)
           AND c.marca_activa NOT IN(
                    SELECT ctm.marca_inf
                      FROM cat_tipo_credito ctm
                     WHERE ctm.marca_inf IS NOT NULL)
           AND c.rch_cod > 0
           AND c.rch_cod = d.rch_cod
         ORDER  BY a.f_inicio DESC

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
      END FOREACH;
   END IF

   RETURN verror, vid_derechohabiente, vcodResp, vdescResp;

END FUNCTION
;


