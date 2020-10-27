






CREATE PROCEDURE "safreviv".sp_valida_estado_infonavit()

   DEFINE v_inconsistencia       CHAR(2);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_ax_nss               CHAR(11);
   DEFINE vcodResp               CHAR(4);
   DEFINE v_id_dh                DECIMAL(9,0);
   DEFINE v_id_ocg_detalle       DECIMAL(9,0);
   DEFINE v_id_ocg_tramite       DECIMAL(9,0);
   DEFINE vpesos92               DECIMAL(12,2);
   DEFINE vpesos97               DECIMAL(12,2);
   DEFINE verror                 SMALLINT;        -- código de error en caso de excepción  
   DEFINE v_subproceso           SMALLINT;
   DEFINE vdescResp              VARCHAR(140);

   LET v_subproceso = 1;

   -- crear función para validar el estado INFONAVIT
   FOREACH 
      SELECT id_derechohabiente,
             id_ocg_detalle,
             id_ocg_tramite
        INTO v_id_dh,
             v_id_ocg_detalle,
             v_id_ocg_tramite
        FROM ocg_tramite
       WHERE estado = 0

      SELECT nss
        INTO v_nss
        FROM afi_derechohabiente
       WHERE id_derechohabiente = v_id_dh;

      EXECUTE FUNCTION fn_consulta_marca_ws(v_nss)      -- 2000 Cuenta libre, soliitud procedente
                  INTO verror, v_ax_nss, vcodResp, vdescResp, vpesos92, vpesos97;

      IF vcodResp = "2000" THEN 
         CONTINUE FOREACH;
      ELSE
         LET v_inconsistencia = "28";  -- Solcitud rechazada     P E N D I E N T E 

         -- Se inserta la inconsistencia
         INSERT INTO ocg_inconsistencia
              --VALUES( v_id_ocg_detalle,
              VALUES( v_id_ocg_tramite,
                      v_subproceso,
                      v_inconsistencia,
                      TODAY );

         -- Se actualiza  el estado de ocg_tramite a rechazado(1)
         UPDATE ocg_tramite
            SET estado = 1
          WHERE id_ocg_tramite = v_id_ocg_tramite;

      END IF;
   END FOREACH;

END PROCEDURE;


