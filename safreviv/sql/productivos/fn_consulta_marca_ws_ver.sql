






CREATE FUNCTION "safreviv".fn_consulta_marca_ws_ver(p_nss CHAR(11))

RETURNING SMALLINT, CHAR(11), CHAR(4), VARCHAR(140), DECIMAL(12,2), DECIMAL(12,2)

   DEFINE vid_derechohabiente       DECIMAL(9,0);
   DEFINE vrchCod                   SMALLINT;
   DEFINE vcodResp                  CHAR(4);
   DEFINE vcodResp1                 CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE vdescResp1                VARCHAR(140);
   DEFINE vpesos92                  DECIMAL(12,2);
   DEFINE vaivs92                   DECIMAL(12,2);
   DEFINE vpesos97                  DECIMAL(12,2);
   DEFINE vaivs97                   DECIMAL(12,2);
   DEFINE vprecio                   DECIMAL(12,6);
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

   ON EXCEPTION SET verror
      -- Devolverá el código de error cuando ocurra una excepción
      LET vcodResp  = "2999";
      LET vdescResp = "ERROR EN PROCESO, excepcion: "||verror;

      RETURN verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/consultaMarcaws.trace';
   SET DEBUG FILE TO '/safreviv_int/archivos/consultaMarcaws.trace';
   TRACE ON;

   LET verror    = 0;
   LET vbnd      = 0;
   LET vrchCod   = 0;
   LET vcodResp  = "2000";
   LET vcodResp1 = "2000";
   LET vdescResp = "CUENTA LIBRE";
   LET vdescResp1= "CUENTA LIBRE";
   LET vpesos92  = 0;
   LET vaivs92   = 0;
   LET vpesos97  = 0;
   LET vaivs97   = 0;
   LET vprecio   = 0;
   LET v_valida  = 1;
   LET v_tpo_originacion = NULL;
   LET v_tpo_credito     = NULL;
   LET v_num_credito     = NULL;
   LET v_f_otorga        = "";
   LET v_f_liquida       = "";

   SELECT gv.precio_fondo
     INTO vprecio
     FROM glo_valor_fondo AS gv
    WHERE gv.fondo = 11
      AND gv.f_valuacion = today;

   IF vprecio = 0 OR vprecio IS NULL THEN ---no existe precio de aivs
      LET vcodResp  = "207";
      LET vdescResp = "NO EXISTE VALOR DE AVIS";
      LET vbnd      = 1;
   END IF

   IF vbnd = 0 THEN
      CALL fn_verifica_edo_trab (p_nss)
      RETURNING verror, vid_derechohabiente, vcodResp, vdescResp;

      IF vcodResp <> "2000" AND
         vcodResp <> "2213" THEN
         LET vbnd = 1;

         RETURN verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;
      ELSE
         IF vcodResp = "2213" THEN
            LET vcodResp1  = "2213";
            LET vdescResp1 = vdescResp;
            LET vcodResp   = "2000";
         END IF
      END IF
   END IF

   IF vbnd = 0 THEN
      CALL fn_consulta_saldo_cero (p_nss)
      RETURNING verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;

      IF vcodResp = 208 THEN
         LET vbnd      = 1;

         RETURN verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;
      ELSE
         IF vcodResp = "2213" THEN
            LET vcodResp1  = "2213";
            LET vdescResp1 = vdescResp;
         END IF
      END IF
   END IF

   IF vbnd = 0 THEN
      IF EXISTS (  ---Se verifica si está ya inciado el trámite
         SELECT id_derechohabiente
           FROM cre_tramite
          WHERE id_derechohabiente = vid_derechohabiente
            AND estado             = 18) THEN

         FOREACH
            SELECT h.diagnostico
              INTO vcodResp
              FROM cre_his_tramite h, cre_tramite c
             WHERE h.id_cre_tramite = c.id_cre_tramite
               AND c.id_derechohabiente = vid_derechohabiente
               AND c.estado            = 18
            ORDER BY h.f_proceso DESC
         END FOREACH;

            IF vcodResp[1] = "3" THEN
               IF vcodResp = "3000" THEN
                  LET vdescResp = "MARCA PROCEDENTE";
               ELSE
                  SELECT d.desc_rechazo
                    INTO vdescResp
                    FROM cat_rechazo d
                   WHERE d.cod_rechazo = vcodResp[2,4]
                     AND d.tpo_rechazo = "RCH";

                  IF vdescResp = "" THEN
                     LET vdescResp = "MARCA NO PROCEDENTE";
                  END IF
               END IF
            ELSE
               LET vcodRch = vcodResp[2,4];

               SELECT a.desc_estado
                 INTO vdescResp
                 FROM cat_rch_acreditado a
                WHERE a.estado = vcodRch;
            END IF
      END IF
   END IF

   IF vcodResp = "2000" AND vcodResp1 = "2213" THEN
      LET vcodResp  = vcodResp1;
      LET vdescResp = vdescResp1;
   END IF

   IF vcodResp = "2213" AND vdescResp IS NULL THEN
      LET vdescResp = "INFONAVIT: MARCA CRÉDITO EN TRÁMITE";
   END IF

   RETURN verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;

END FUNCTION;


