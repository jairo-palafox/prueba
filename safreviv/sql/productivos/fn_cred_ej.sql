






CREATE FUNCTION "safreviv".fn_cred_ej(p_nss           CHAR(11),
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
   DEFINE vtpoTrab                  CHAR(1);
   DEFINE vid_cre_ctr_archivo       DECIMAL(9,0);
   DEFINE vfolio                    DECIMAL(9,0);

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
   SET DEBUG FILE TO '/safreviv_int/archivos/credEj.trace';
   TRACE ON;

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
   LET vid_derechohabiente = 0;
   LET vdiasReintento      = 0;
   LET vf_vigencia         = TODAY;
   LET vdescMarca          = "";
   LET vejCred             = 0;
   LET vOrig_tpo_reg       = "20";
   LET vtpoOriginacion     = 4;

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

      CALL fn_consulta_marca_ws(p_nss)
      RETURNING verror, p_nss, vcodResp, vdescResp, vpesos92, vpesos97;

      IF vcodResp[1] = "2" THEN
         IF vcodResp = "2000" THEN  --NSS sin inscripción
            LET vcodResp  = "229";
            LET vdescResp = "NO SE PUEDE EJERCER EL CRÉDITO, REGISTRO SIN TRÁMITE DE INSCRIPCIÓN";
            LET vejCred   = 1;
         ELSE
            IF vcodResp = "2213" THEN
               IF vtpoTrab = "I" THEN
                  LET vcodResp  = "230";
                  LET vdescResp = "NO SE PUEDE EJERCER EL CRÉDITO, REGISTRO NO ESTÁ MARCADO";
                  LET vejCred   = 1;
               ELSE
                  LET vcodResp  = "3000";
               END IF
            ELSE
               LET vdescResp = "NO SE PUEDE EJERCER EL CRÉDITO, "||vdescResp;
               LET vejCred   = 1;
            END IF
         END IF
      END IF

      IF vcodResp[1] = "3" THEN
         IF vcodResp = "3000" THEN
            IF p_tpo_operacion   = "EJ" THEN
               LET vdescResp     = "CRÉDITO EJERCIDO";
               LET vOrig_tpo_reg = "20";  ---crédito originado
               LET vnvoEdo       = 19;    ---trámite finalizado
            ELSE
               LET vdescResp     = "CRÉDITO EN TRÁMITE, MISTRACIÓN EJERCIDA";
               LET vOrig_tpo_reg = "18";  ---crédito en trámite
               LET vnvoEdo       = 17;    ---ministración por liquidar
            END IF
         END IF
      END IF

      IF vcodResp[1] <> "2" AND
         vcodResp[1] <> "3" THEN
         LET vcodResp  = "229";
         LET vdescResp = "NO SE PUEDE EJERCER EL CRÉDITO, REGISTRO SIN TRÁMITE DE INSCRIPCIÓN";
         LET vejCred     = 1;
      END IF

      IF vejCred = 0 THEN  --Si es procedente el ejercio o la ministración se crea el registro en cre_acreditado
         IF EXISTS (  ---Se verifica si está ya ejercida la ministración
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

      END IF
   END IF

   LET vdescResp = TRIM(vdescResp);

   RETURN verror, p_nss, vcodResp, vdescResp, p_num_credito, p_tpo_credito;

END FUNCTION

;


