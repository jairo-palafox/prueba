






CREATE FUNCTION "safreviv".fn_marca_tmt(p_nss CHAR(11), p_num_credito DECIMAL(10,0), p_f_vigencia DATE)

RETURNING SMALLINT, CHAR(11), CHAR(4), VARCHAR(140), SMALLINT

   DEFINE vid_cre_tramite           DECIMAL(9,0);
   DEFINE vid_derechohabiente       DECIMAL(9,0);
   DEFINE vid_cre_acreditado        DECIMAL(9,0);
   DEFINE vfolio                    DECIMAL(9,0);
   DEFINE vrchCod                   SMALLINT;
   DEFINE vcodResp                  CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE vbnd                      SMALLINT;
   DEFINE vdiasReintento            SMALLINT;
   DEFINE vestado                   SMALLINT;
   DEFINE vmarca                    SMALLINT;
   DEFINE vedoMarca                 SMALLINT;
   DEFINE vrchMarca                 SMALLINT;
   DEFINE vmarcaCausa               SMALLINT;
   DEFINE vfCausa                   DATE;
   DEFINE vusuario                  CHAR(20);
   DEFINE vprocesoCod               SMALLINT;
   DEFINE v_sts_marcaje             SMALLINT;
   DEFINE vfProceso                 DATE;
   DEFINE vhProceso                 DATETIME HOUR TO SECOND;
   DEFINE vmodulocod                CHAR(2);
   DEFINE vtpoCred                  SMALLINT;
   DEFINE vmarcaPrc                 SMALLINT;
   DEFINE vcodPrc                   CHAR(2);
   DEFINE vintento                  SMALLINT;
   DEFINE vcodResultOp              SMALLINT;
   DEFINE vdiagnostico              CHAR(3);
   DEFINE vsituacion                SMALLINT;
   DEFINE verror                    SMALLINT; -- código de error en caso de excepción
   DEFINE vpesos92                  DECIMAL(12,2);
   DEFINE vpesos97                  DECIMAL(12,2);
   DEFINE vHist                     SMALLINT;
   DEFINE vtpoTrab                  CHAR(1);
   DEFINE vbnd_ws                   SMALLINT;
   DEFINE vcoderr                   SMALLINT;
   DEFINE vr_nss                    CHAR(11);

   ON EXCEPTION SET verror
      -- Devolverá el código de error cuando ocurra una excepción
      LET vcodResp  = "2999";
      LET vdescResp = "ERROR EN PROCESO, excepcion: "||verror;

      RETURN verror, p_nss, vcodResp, vdescResp, vdiasReintento;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/generaMarcaTramite.trace';
   SET DEBUG FILE TO '/safreviv_int/archivos/generaMarcaTramite.trace';
   TRACE ON;

   ---inicialización de variables
   LET vcodResp            = "2000";
   LET vdescResp           = "CUENTA MARCADA, TRÁMITE PROCEDENTE";
   LET verror              = 0;
   LET vestado             = 18;
   LET vfolio              = 1;
   LET vmarca              = 213;
   LET vedoMarca           = 0;
   LET vrchMarca           = 0;
   LET vmarcaCausa         = "";
   LET vfCausa             = "";
   LET vusuario            = "infonavit";
   LET vprocesoCod         = 301;
   LET vdiasReintento      = 0;
   LET vfProceso           = TODAY;
   LET vhProceso           = CURRENT;
   LET vid_cre_tramite     = 0;
   LET vmarcaPrc           = 234;
   LET vcodPrc             = "04";
   LET vmodulocod          = "04";
   LET vtpoCred            = 10;
   LET vintento            = 1;
   LET vsituacion          = 2;
   LET vcodResultOp        = NULL;
   LET vdiagnostico        = NULL;
   LET vbnd                = 0;
   LET vid_derechohabiente = 0;
   LET vHist               = 1;
   LET vbnd_ws             = 0;
   LET vid_cre_acreditado  = 0;

   IF p_num_credito IS NULL THEN
      LET vcodResp  = "225";
      LET vdescResp = "NUMERO DE CREDITO NO EXISTE";

      RETURN verror, p_nss, vcodResp, vdescResp, vdiasReintento;
   END IF

   CALL fn_verifica_derechohabiente(p_nss, vid_derechohabiente)
   RETURNING p_nss, vid_derechohabiente, vtpoTrab, vcodResp;

   IF vcodResp = 1 THEN
      LET vcodResp  = "211";
      LET vdescResp = "TRABAJADOR NO EXISTE";
   ELSE
      IF NOT EXISTS (  ---Se verifica que no esté ya inciado el trámite
         SELECT id_derechohabiente
           FROM cre_tramite
          WHERE id_derechohabiente = vid_derechohabiente
            AND estado             = 18) THEN

         CALL fn_verifica_edo_trab (p_nss)
         RETURNING verror, vid_derechohabiente, vcodResp, vdescResp;

         LET vid_cre_tramite = seq_cre_tramite.NEXTVAL;

         IF vcodResp = "2000" THEN
            LET vbnd = 1;
         ELSE
            LET vestado = 240;
         END IF
      ELSE
         CALL fn_consulta_saldo_cero (p_nss)
         RETURNING verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;

         IF vcodResp = 208 THEN
            RETURN verror, p_nss, vcodResp, vdescResp, vdiasReintento;
         END IF

         SELECT id_cre_tramite
           INTO vid_cre_tramite
           FROM cre_tramite
          WHERE id_derechohabiente = vid_derechohabiente
            AND estado             = 18;

         FOREACH
            SELECT id_cre_acreditado
              INTO vid_cre_acreditado
              FROM cre_acreditado
             WHERE id_derechohabiente = vid_derechohabiente
               AND estado             = 18
             ORDER BY f_otorga
         END FOREACH;

         IF vid_cre_acreditado IS NULL THEN
            LET vid_cre_acreditado = 0;
         END IF

         IF NOT EXISTS (
               SELECT id_derechohabiente
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = vid_derechohabiente
                  AND marca              = vmarca) THEN

            LET vbnd = 31;  -- no tiene marca interna
         ELSE
            IF NOT EXISTS (
               SELECT id_derechohabiente
                 FROM cta_his_marca_ws
                WHERE id_derechohabiente = vid_derechohabiente
                  AND situacion          = 2) THEN

               IF NOT EXISTS (
                  SELECT id_derechohabiente
                    FROM cta_marca_ws
                   WHERE id_derechohabiente = vid_derechohabiente
                     AND situacion          = 2) THEN

                  LET vbnd = 32;  -- no tiene marca Procesar
               ELSE
                  LET vcodResp = "2000";
                  LET vbnd = 3;  -- no tiene marca Procesar
               END IF
            ELSE
               LET vcodResp = "2000";
               LET vbnd     = 3;
            END IF
         END IF
      END IF

      IF vbnd = 1 OR vbnd = 31 THEN --marca interna
            DELETE
              FROM sfr_marca_historica
             WHERE id_derechohabiente = vid_derechohabiente
               AND marca = vmarca
               AND n_referencia = vid_cre_tramite;

            ---generar marca "Crédito en Trámite"
            CALL fn_marca_cuenta(vid_derechohabiente,
                                 vmarca,
                                 vid_cre_tramite,
                                 Vfolio,
                                 vedoMarca,
                                 vrchMarca,
                                 vmarcaCausa,
                                 vfCausa,
                                 vusuario,
                                 vprocesoCod)
                       RETURNING v_sts_marcaje;

            IF v_sts_marcaje = 0 THEN
               LET vcodResp = "2213";

               IF vbnd = 1 THEN
                  LET vbnd = 2;   --marca Procesar
               ELSE
                  LET vbnd = 32;   --marca Procesar
               END IF
            ELSE
               LET vcodResp = "2"||v_sts_marcaje;

               SELECT d.rch_desc
                 INTO vdescResp
                 FROM cat_rch_marca AS d
                WHERE d.rch_cod = v_sts_marcaje;

               LET vestado = 240;

               IF vbnd = 1 THEN
                  LET vbnd = 0;
               ELSE
                  LET vbnd = 3;
               END IF
            END IF
      END IF

      IF vtpoTrab = "I" THEN
         IF vbnd = 2 OR vbnd = 32 THEN
            LET vbnd_ws = 1;
         END IF
      END IF

      IF vbnd = 2 THEN
         LET vbnd = 0;
      ELSE
         LET vbnd = 3;
      END IF

      IF vbnd = 0 THEN
         IF NOT EXISTS (
            SELECT id_derechohabiente
              FROM cre_tramite
             WHERE id_cre_tramite = vid_cre_tramite) THEN

            INSERT INTO cre_tramite (
                        id_cre_tramite    ,
                        id_derechohabiente,
                        num_credito       ,
                        f_vigencia        ,
                        estado)
                VALUES (vid_cre_tramite    ,
                        vid_derechohabiente,
                        p_num_credito      ,
                        p_f_vigencia       ,
                        vestado);

            CALL fn_agr_integra_mf(p_nss, vid_derechohabiente, p_num_credito)
            RETURNING vcoderr, vr_nss, vid_cre_acreditado;
         END IF
      ELSE
         UPDATE cre_tramite
           SET num_credito    = p_num_credito,
               f_vigencia     = p_f_vigencia
         WHERE id_cre_tramite = vid_cre_tramite;
      END IF

      IF vbnd_ws = 1 AND vid_cre_acreditado > 0 THEN
         IF NOT EXISTS(
            SELECT id_derechohabiente
              FROM sfr_marca_activa
             WHERE id_derechohabiente = vid_derechohabiente
               AND marca = 234) THEN

               INSERT INTO cta_marca_ws (
                           id_derechohabiente,
                           id_origen         ,
                           modulo_cod        ,
                           tpo_credito       ,
                           marca             ,
                           f_solicita        ,
                           intento           ,
                           cod_result_op     ,
                           diagnostico       ,
                           situacion         ,
                           num_credito       ,
                           f_infonavit       ,
                           marca_procesar    ,
                           folio_archivo     ,
                           usuario)
                   VALUES (vid_derechohabiente,
                           vid_cre_acreditado ,
                           vmodulocod         ,
                           vtpoCred           ,
                           vmarcaPrc          ,
                           vfProceso          ,
                           vintento           ,
                           vcodResultOp       ,
                           vdiagnostico       ,
                           vsituacion         ,
                           p_num_credito      ,
                           vfProceso          ,
                           vcodPrc            ,
                           vfolio             ,
                           vusuario);
         END IF
      END IF

      IF vtpoTrab = "S" AND vestado = 18 THEN
         LET vcodResp = "3000";
         LET vdescResp = "MARCA PROCEDENTE";
      END IF

      CALL sp_agr_ins_his_tramite(vid_cre_tramite,
                                  p_num_credito,
                                  p_f_vigencia,
                                  vestado,
                                  vcodResp,
                                  vhProceso,
                                  vHist);
   END IF

   IF vcodResp = "2213" THEN
      LET vcodResp = "2000";
   END IF

   IF vcodResp <> "2000" AND vcodResp <> "3000" THEN
      LET p_f_vigencia = p_f_vigencia + vdiasReintento UNITS DAY;
      LET vdescResp = vdescResp||", Reintente el  "||DAY(p_f_vigencia)||"/"||MONTH(p_f_vigencia)||"/"||YEAR(p_f_vigencia);
   END IF

   RETURN verror, p_nss, vcodResp, vdescResp, vdiasReintento;

END FUNCTION
;


