






CREATE FUNCTION "safreviv".fn_credito_ejercido_recurr(p_id_derechohabiente DECIMAL(9,0),
                                           p_nss                CHAR(11),
                                           p_tpo_credito        CHAR(3),
                                           p_num_credito        DECIMAL(10,0),
                                           p_f_otorga           DATE,
                                           p_tpo_dscto          SMALLINT,
                                           p_val_dscto          DECIMAL(8,0),
                                           p_monto_liquida      DECIMAL(15,2),
                                           p_f_proceso          DATE,
                                           p_nrp                CHAR(11),
                                           p_tpo_operacion      CHAR(3),
                                           p_d_id_cre_ctr_arch  DECIMAL(9,0),
                                           p_folio              DECIMAL(9,0),
                                           p_tpo_trab           CHAR(1))
 
RETURNING SMALLINT, CHAR(11), CHAR(4), VARCHAR(140), DECIMAL(10,0), SMALLINT

   DEFINE vid_cre_tramite           DECIMAL(9,0);
   DEFINE vrchCod                   SMALLINT;
   DEFINE vcodResp                  CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE vdescOrig                 VARCHAR(140);
   DEFINE vbnd                      SMALLINT;
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
   DEFINE vcodResultOp              SMALLINT;
   DEFINE vdiagnostico              CHAR(3);
   DEFINE verror                    SMALLINT; -- código de error en caso de excepción
   DEFINE vdiasReintento            SMALLINT;
   DEFINE vf_vigencia               DATE;
   DEFINE vtpoOriginacion           SMALLINT;
   DEFINE vmarca_tmt                SMALLINT;
   DEFINE vdescMarca                CHAR(40);
   DEFINE vpesos92                  DECIMAL(12,2);
   DEFINE vpesos97                  DECIMAL(12,2);
   DEFINE vejCred                   SMALLINT;
   DEFINE vnvoEdo                   SMALLINT;
   DEFINE vHist                     SMALLINT;

   ---Variables para registro originado
   DEFINE vOrig_tpo_reg              CHAR(2);
   DEFINE vOrig_nss                  CHAR(11);
   DEFINE vOrig_nci                  DECIMAL(10,0);
   DEFINE vOrig_sdo_deudor           DECIMAL(8,0);
   DEFINE vOrig_fOtorga              DATE;
   DEFINE vOrig_fCulmina             DATE;
   DEFINE vOrig_tpo_credito          CHAR(3);
   DEFINE vOrig_sts_credito          SMALLINT;
   DEFINE vOrig_tpo_dscto            SMALLINT;
   DEFINE vOrig_val_dscto            DECIMAL(8,0);
   DEFINE vOrig_nrp                  CHAR(11);

   ON EXCEPTION SET verror
      -- Devolverá el código de error cuando ocurra una excepción
      LET vcodResp  = "2999";
      LET vdescResp = "ERROR EN PROCESO, excepcion: "||verror;

      RETURN verror, p_nss, vcodResp, vdescResp, p_num_credito, p_tpo_credito;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/creditoEjercido.trace';
   --SET DEBUG FILE TO '/safreviv_req/PRODINFXV-106/creditoEjercido.trace';
   --TRACE ON;

   ---inicialización de variables
   LET verror              = 0;
   LET vestado             = 15;
   LET vmarca_tmt          = 213;
   LET vedoMarca           = 0;
   LET vrchMarca           = 0;
   LET vmarcaCausa         = "";
   LET vfCausa             = "";
   LET vusuario            = "infonavit";
   LET vprocesoCod         = 301;
   LET vfProceso           = TODAY;
   LET vhProceso           = CURRENT;
   LET vcodResultOp        = NULL;
   LET vdiagnostico        = NULL;
   LET vbnd                = 0;
   LET vdiasReintento      = 0;
   LET vf_vigencia         = TODAY;
   LET vdescMarca          = "";
   LET vejCred             = 0;

   IF p_tpo_operacion   = "EJ" THEN
      LET vbnd = 0;
   ELSE
      LET vbnd = 1;
   END IF

   LET vcodResp = "0";

   IF p_tpo_operacion   = "EJ" THEN
      LET vdescResp     = "CRÉDITO EJERCIDO";
      LET vOrig_tpo_reg = "20";  ---crédito originado
      LET vnvoEdo       = 19;    ---trámite finalizado
   ELSE
      LET vdescResp     = "CRÉDITO EN TRÁMITE, MISTRACIÓN EJERCIDA";
      LET vOrig_tpo_reg = "18";  ---crédito en trámite
      LET vnvoEdo       = 17;    ---ministración por liquidar
   END IF

   IF EXISTS (  ---Se verifica si está ya ejercida la ministración
      SELECT id_cre_tramite
        FROM cre_ministracion
       WHERE id_cre_tramite IN(
             SELECT c1.id_cre_tramite
               FROM cre_tramite c1
               INNER JOIN cre_ministracion c2
               ON c1.id_cre_tramite      = c2.id_cre_tramite
               AND c1.estado             = 18
               AND c1.id_derechohabiente = p_id_derechohabiente
               AND c2.ministracion       = p_tpo_operacion
               AND c1.num_credito        = p_num_credito)) THEN

      LET vcodResp  = "3000";
      LET vdescResp = "CRÉDITO EN TRÁMITE, MISTRACIÓN EJERCIDA";

      RETURN verror, p_nss, vcodResp, vdescResp, p_num_credito, p_tpo_credito;
   END IF

   LET vOrig_nss         = p_nss;
   LET vOrig_nci         = p_num_credito;
   LET vOrig_sdo_deudor  = p_monto_liquida;
   LET vOrig_fOtorga     = p_f_otorga;
   LET vOrig_fCulmina    = NULL;
   LET vOrig_tpo_credito = p_tpo_credito;
   LET vOrig_sts_credito = 1;
   LET vOrig_tpo_dscto   = p_tpo_dscto;
   LET vOrig_val_dscto   = p_val_dscto;
   LET vOrig_nrp         = p_nrp;

   CALL fn_agr_integra_orig_mf_recurr(vOrig_tpo_reg,
                                      vOrig_nss,
                                      vOrig_nci,
                                      vOrig_sdo_deudor,
                                      vOrig_fOtorga,
                                      vOrig_fCulmina,
                                      vOrig_tpo_credito,
                                      vOrig_sts_credito,
                                      vOrig_tpo_dscto,
                                      vOrig_val_dscto,
                                      vOrig_nrp,
                                      p_d_id_cre_ctr_arch,
                                      p_folio,
                                      p_id_derechohabiente,
                                      p_tpo_trab)
   RETURNING verror, vcodResp, vdescOrig;

   IF vcodResp = "01" THEN
      LET vcodResp = "3000";

      SELECT id_cre_tramite
        INTO vid_cre_tramite
        FROM cre_tramite
       WHERE id_derechohabiente = p_id_derechohabiente
         AND estado             = 18;

      IF vbnd = 0 THEN
         -- se consulta el tipo de crédito y el tipo originación para el tipo credito
         FOREACH
          SELECT FIRST 1 tpo_originacion
            INTO vtpoOriginacion
            FROM cat_tipo_credito
           WHERE tpo_credito = p_tpo_credito
             AND f_actualiza <= p_f_otorga
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- se obtiene las marcas y el tipo de transferencia para el tipo de credito en proceso
         SELECT FIRST 1 marca_inf
           INTO vmarca
           FROM cat_tipo_credito
          WHERE tpo_originacion = vtpoOriginacion
            AND tpo_credito     = p_tpo_credito;

         CALL fn_desmarca_cuenta(p_id_derechohabiente,vmarca_tmt, vid_cre_tramite, 0, 0, "infonavit", 301)
         RETURNING vedoMarca;
      END IF

      IF p_tpo_operacion = "EJ" THEN
         UPDATE cre_tramite
            SET num_credito    = p_num_credito,
                f_vigencia     = vf_vigencia,
                estado         = vnvoEdo
          WHERE id_cre_tramite = vid_cre_tramite;
      ELSE
         LET vhProceso = CURRENT;

         INSERT INTO cre_ministracion
         VALUES(vid_cre_tramite,
                p_tpo_operacion,
                vhProceso);
      END IF

      LET vHist = 1;

      CALL sp_agr_ins_his_tramite(vid_cre_tramite,
                                  p_num_credito,
                                  vf_vigencia,
                                  vnvoEdo,
                                  vcodResp,
                                  vhProceso,
                                  vHist);

      IF p_tpo_operacion = "EJ" THEN
         LET vnvoEdo   = 20;
         LET vhProceso = CURRENT;

         UPDATE cre_tramite
            SET num_credito    = p_num_credito,
                f_vigencia     = vf_vigencia,
                estado         = vnvoEdo
          WHERE id_cre_tramite = vid_cre_tramite;

         LET vHist = 0;

         CALL sp_agr_ins_his_tramite(vid_cre_tramite,
                                     p_num_credito,
                                     vf_vigencia,
                                     vnvoEdo,
                                     vcodResp,
                                     vhProceso,
                                     vHist);
      END IF
   ELSE
      IF p_tpo_operacion = "EJ" THEN
         LET vdescResp = "CRÉDITO NO EJERCIDO, "||vdescOrig;
      ELSE
         LET vdescResp = "MINISTRACIÓN NO EJERCIDA, "||vdescOrig;
      END IF
   END IF

   LET vdescResp = TRIM(vdescResp);

   -- actualiza estadisticas a la tabla historica trámite
   UPDATE STATISTICS FOR TABLE cre_his_tramite;

   -- actualiza estadisticas a la tabla trámite
   UPDATE STATISTICS FOR TABLE cre_tramite;

   RETURN verror, p_nss, vcodResp, vdescResp, p_num_credito, p_tpo_credito;

END FUNCTION
;


