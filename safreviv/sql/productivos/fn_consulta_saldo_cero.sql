






CREATE FUNCTION "safreviv".fn_consulta_saldo_cero(p_nss CHAR(11))

RETURNING SMALLINT, CHAR(11), CHAR(4), VARCHAR(140), DECIMAL(12,2), DECIMAL(12,2)

   DEFINE vid_derechohabiente       DECIMAL(9,0);
   DEFINE vrchCod                   SMALLINT;
   DEFINE vcodResp                  CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE vpesos92                  DECIMAL(12,2);
   DEFINE vaivs92                   DECIMAL(12,2);
   DEFINE vpesos97                  DECIMAL(12,2);
   DEFINE vaivs97                   DECIMAL(12,2);
   DEFINE vprecio                   DECIMAL(12,6);
   DEFINE vbnd                      SMALLINT;
   DEFINE vcodRch                   SMALLINT;
   DEFINE vtpoTrab                  CHAR(1);

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
   --TRACE ON;

   LET verror    = 0;
   LET vbnd      = 0;
   LET vrchCod   = 0;
   LET vcodResp  = "2000";
   LET vdescResp = "CUENTA LIBRE";
   LET vpesos92  = 0;
   LET vaivs92   = 0;
   LET vpesos97  = 0;
   LET vaivs97   = 0;
   LET vprecio   = 0;
   LET v_valida  = 1;
   LET vid_derechohabiente = 0;
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

   CALL fn_verifica_derechohabiente(p_nss, vid_derechohabiente)
   RETURNING p_nss, vid_derechohabiente, vtpoTrab, vcodResp;

   IF vcodResp = 1 THEN
      LET vcodResp  = "211";
      LET vdescResp = "TRABAJADOR NO EXISTE";

      LET vbnd = 1;
   END IF

   IF vbnd = 0 THEN
      SELECT sum(mov.monto_acciones)
        INTO vaivs97
        FROM cta_movimiento mov
       WHERE mov.id_derechohabiente = vid_derechohabiente
         AND mov.subcuenta          IN( 4, 44)
         AND mov.fondo_inversion    = 11;

      IF vaivs97 IS NULL OR vaivs97 < 0 THEN
         LET vaivs97  = 0;
         LET vpesos97 = 0;
      ELSE
         LET vpesos97 = vaivs97 * vprecio;
      END IF

      SELECT sum(mov.monto_acciones)
        INTO vaivs92
        FROM cta_movimiento mov
       WHERE mov.id_derechohabiente = vid_derechohabiente
         AND mov.subcuenta          IN( 8,42)
         AND mov.fondo_inversion    = 11;

      IF vaivs92 IS NULL OR vaivs92 < 0 THEN
         LET vaivs92  = 0;
         LET vpesos92 = 0;
      ELSE
         LET vpesos92 = vaivs92 * vprecio;
      END IF

      IF vpesos97 = 0 AND vpesos92 = 0 THEN
         LET vcodResp  = "208";
         LET vdescResp = "SALDO CERO";
         LET vbnd      = 1;
      END IF
   END IF

   IF vcodResp = "0" THEN
      LET vcodResp = "2000";
   END IF

   RETURN verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;

END FUNCTION
;


