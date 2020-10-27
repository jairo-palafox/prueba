






CREATE FUNCTION "safreviv".fn_verifica_derechohabiente(p_nss CHAR(11), p_id_derechohabiente DECIMAL(9,0))
RETURNING CHAR(11), DECIMAL(9,0), CHAR(1), SMALLINT

   DEFINE vcodResp                  SMALLINT;
   DEFINE vnss                      CHAR(11);
   DEFINE vid_derechohabiente       DECIMAL(9,0);
   DEFINE vtpoTrab                  CHAR(1);

   LET vcodResp = "0";

   IF p_nss > 0 THEN
      SELECT afi.id_derechohabiente, afi.tipo_trabajador
        INTO vid_derechohabiente, vtpoTrab
        FROM afi_derechohabiente afi
       WHERE afi.nss = p_nss;

      IF vid_derechohabiente IS NULL THEN
         LET vid_derechohabiente = 0;
         LET vcodResp            = "1";
      END IF

      RETURN p_nss, vid_derechohabiente, vtpoTrab, vcodResp;
   ELSE
      SELECT afi.nss, afi.tipo_trabajador
        INTO vnss, vtpoTrab
        FROM afi_derechohabiente afi
       WHERE afi.id_derechohabiente = p_id_derechohabiente;

      IF vnss IS NULL THEN
         LET vnss     = "0";
         LET vcodResp = "1";
      END IF

      RETURN vnss, p_id_derechohabiente, vtpoTrab, vcodResp;
   END IF

END FUNCTION;


