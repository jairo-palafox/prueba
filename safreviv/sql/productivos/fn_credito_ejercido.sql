






CREATE FUNCTION "safreviv".fn_credito_ejercido(p_nss           CHAR(11),
                                    p_tpo_credito   CHAR(3),
                                    p_num_credito   DECIMAL(10,0),
                                    p_f_otorga      DATE,
                                    p_tpo_dscto     SMALLINT,
                                    p_val_dscto     DECIMAL(8,0),
                                    p_monto_liquida DECIMAL(15,2),
                                    p_f_proceso     DATE,
                                    p_nrp           CHAR(11),
                                    p_tpo_operacion CHAR(3))

RETURNING SMALLINT, CHAR(11), CHAR(4), VARCHAR(140), DECIMAL(10,0), SMALLINT

   DEFINE vid_cre_tramite           DECIMAL(9,0);
   DEFINE vid_derechohabiente       DECIMAL(9,0);
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
   DEFINE verror                    SMALLINT; -- c�digo de error en caso de excepci�n
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
   DEFINE vtpoTrab                  CHAR(1);
   DEFINE vid_cre_ctr_archivo       DECIMAL(9,0);
   DEFINE vfolio                    DECIMAL(9,0);
   DEFINE v_valida                  SMALLINT;
   DEFINE v_resultado               SMALLINT;
   DEFINE v_tpo_originacion         SMALLINT;
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_f_otorga                DATE;
   DEFINE v_f_liquida               DATE;
   DEFINE v_tpo_dscto               SMALLINT;

   ---Variables para registro originado
   DEFINE vOrig_tpo_reg              CHAR(2);
   DEFINE vOrig_nss                  CHAR(11);
   DEFINE vOrig_nci                  DECIMAL(10,0);
   DEFINE vOrig_sdo_deudor           DECIMAL(15,2);
   DEFINE vOrig_fOtorga              DATE;
   DEFINE vOrig_fCulmina             DATE;
   DEFINE vOrig_tpo_credito          CHAR(3);
   DEFINE vOrig_sts_credito          SMALLINT;
   DEFINE vOrig_tpo_dscto            SMALLINT;
   DEFINE vOrig_val_dscto            DECIMAL(8,0);
   DEFINE vOrig_nrp                  CHAR(11);

   ON EXCEPTION SET verror
      -- Devolver� el c�digo de error cuando ocurra una excepci�n
      LET vcodResp  = "2999";
      LET vdescResp = "ERROR EN PROCESO, excepcion: "||verror;

      RETURN verror, p_nss, vcodResp, vdescResp, p_num_credito, p_tpo_credito;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/creditoEjercido.trace';
   --SET DEBUG FILE TO '/safreviv_int/archivos/creditoEjercido.trace';
   --TRACE ON;

   ---inicializaci�n de variables
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
   LET vid_derechohabiente = 0;
   LET vdiasReintento      = 0;
   LET vf_vigencia         = TODAY;
   LET vdescMarca          = "";
   LET vejCred             = 0;
   LET vOrig_tpo_reg       = "20";
   LET vtpoOriginacion     = 4;
   LET v_valida            = 0;

   LET v_resultado         = "";
   LET v_tpo_originacion   = "";
   LET v_tpo_credito       = "";
   LET v_num_credito       = "";
   LET v_f_otorga          = "";
   LET v_f_liquida         = "";
   LET v_tpo_dscto         = "";

   CALL fn_verifica_id_archivo()
   RETURNING vid_cre_ctr_archivo, vfolio;

   IF p_tpo_operacion   = "EJ" THEN
      LET vbnd = 0;
   ELSE
      LET vbnd = 1;
   END IF

   CALL fn_verifica_derechohabiente(p_nss, vid_derechohabiente)
   RETURNING p_nss, vid_derechohabiente, vtpoTrab, vcodResp;

   IF vcodResp = 1 THEN
      LET vcodResp  = "211";
      LET vdescResp = "TRABAJADOR NO EXISTE";
   ELSE
      LET vcodResp = "0";

      CALL fn_edo_cred_viv (vid_derechohabiente, v_valida)
      RETURNING v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;

      IF v_resultado = 0 THEN
         IF (vtpoOriginacion = v_tpo_originacion) AND
            (v_tpo_credito = p_tpo_credito) AND
            (v_num_credito = p_num_credito) THEN

            LET vdescResp     = "CR�DITO EJERCIDO";
            LET vOrig_tpo_reg = "20";  ---cr�dito originado
            LET vnvoEdo       = 19;    ---tr�mite finalizado
            LET vcodResp      = "3000";
         END IF  
      END IF

      IF vcodResp = "0" THEN 
         ---LET vcodResp = "0";

         CALL fn_consulta_marca_ws(p_nss)
         RETURNING verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;

         IF vcodResp[1] = "2" THEN
            IF vcodResp = "2000" THEN  --NSS sin inscripci�n
               LET vcodResp  = "229";
               LET vdescResp = "NO SE PUEDE EJERCER EL CR�DITO, REGISTRO SIN TR�MITE DE INSCRIPCI�N";
               LET vejCred   = 1;
            ELSE
               IF vcodResp = "2213" THEN
                  IF vtpoTrab = "I" THEN
                     LET vcodResp  = "230";
                     LET vdescResp = "NO SE PUEDE EJERCER EL CR�DITO, REGISTRO NO EST� MARCADO";
                     LET vejCred   = 1;
                  ELSE
                     LET vcodResp  = "3000";
                  END IF
               ELSE
                  LET vdescResp = "NO SE PUEDE EJERCER EL CR�DITO, "||vdescResp;
                  LET vejCred   = 1;
               END IF
            END IF
         END IF

         IF vcodResp[1] = "3" THEN
            IF vcodResp = "3000" THEN
               IF p_tpo_operacion   = "EJ" THEN
                  LET vdescResp     = "CR�DITO EJERCIDO";
                  LET vOrig_tpo_reg = "20";  ---cr�dito originado
                  LET vnvoEdo       = 19;    ---tr�mite finalizado
               ELSE
                  LET vdescResp     = "CR�DITO EN TR�MITE, MISTRACI�N EJERCIDA";
                  LET vOrig_tpo_reg = "18";  ---cr�dito en tr�mite
                  LET vnvoEdo       = 17;    ---ministraci�n por liquidar
               END IF
            END IF
         END IF

         IF vcodResp[1] <> "2" AND
            vcodResp[1] <> "3" THEN
            LET vcodResp  = "229";
            LET vdescResp = "NO SE PUEDE EJERCER EL CR�DITO, REGISTRO SIN TR�MITE DE INSCRIPCI�N";
            LET vejCred     = 1;
         END IF

         IF vejCred = 0 THEN  --Si es procedente el ejercio o la ministraci�n se crea el registro en cre_acreditado
            IF EXISTS (  ---Se verifica si est� ya ejercida la ministraci�n
               SELECT id_cre_tramite
                 FROM cre_ministracion
                WHERE id_cre_tramite IN(
                      SELECT c1.id_cre_tramite
                        FROM cre_tramite c1
                        INNER JOIN cre_ministracion c2
                        ON c1.id_cre_tramite      = c2.id_cre_tramite
                        AND c1.estado             = 18
                        AND c1.id_derechohabiente = vid_derechohabiente
                        AND c2.ministracion       = p_tpo_operacion
                        AND c1.num_credito        = p_num_credito)) THEN

               LET vcodResp  = "3000";
               LET vdescResp = "CR�DITO EN TR�MITE, MISTRACI�N EJERCIDA";

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

            CALL fn_agr_integra_orig_mf(vOrig_tpo_reg,
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
                                        vid_cre_ctr_archivo,
                                        vfolio,
                                        vid_derechohabiente,
                                        vtpoTrab)
            RETURNING verror, vcodResp, vdescOrig;

            IF vcodResp = "01" THEN
               LET vcodResp = "3000";

               SELECT id_cre_tramite
                 INTO vid_cre_tramite
                 FROM cre_tramite
                WHERE id_derechohabiente = vid_derechohabiente
                  AND estado             = 18;

               IF vbnd = 0 THEN
                  -- se consulta el tipo de cr�dito y el tipo originaci�n para el tipo credito
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

                  CALL fn_desmarca_cuenta(vid_derechohabiente,vmarca_tmt, vid_cre_tramite, 0, 0, "infonavit", 301)
                  RETURNING vedoMarca;

                  CALL fn_marca_cuenta(vid_derechohabiente, vmarca, vid_cre_tramite, 1, 0, 0, "", "", "infonavit", 301)
                  RETURNING vedoMarca;

                  SELECT sfm.descripcion_marca
                    INTO vdescMarca
                    FROM sfr_marca sfm
                   WHERE sfm.marca = vmarca;

                  IF vdescMarca IS NULL THEN
                     LET vdescMarca = "INFONAVIT: MARCA CR�DITO VIGENTE";
                  END IF

                  LET vdescResp = vdescResp||", "||vdescMarca;
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
                  LET vdescResp = "CR�DITO NO EJERCIDO, "||vdescOrig;
               ELSE
                  LET vdescResp = "MINISTRACI�N NO EJERCIDA, "||vdescOrig;
               END IF
            END IF
         END IF
      END IF
   END IF

   LET vdescResp = TRIM(vdescResp);

   RETURN verror, p_nss, vcodResp, vdescResp, p_num_credito, p_tpo_credito;

END FUNCTION
;


