






CREATE FUNCTION "safreviv".fn_verifica_marca(p_nss CHAR(11), p_marca SMALLINT)
   RETURNING CHAR(11), CHAR(1), DATE

   DEFINE vid_derechohabiente       DECIMAL(9,0);
   DEFINE vcodigo_op                CHAR(1);
   DEFINE vfecha_tramite            DATE;
   DEFINE vmarca_existe             SMALLINT;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/existeMarca.trace';
   --TRACE ON;

   LET vid_derechohabiente = 0;
   LET vmarca_existe       = 0;
   LET vcodigo_op          = "2";
   LET vfecha_tramite      = TODAY;


   SELECT afi.id_derechohabiente
     INTO vid_derechohabiente
     FROM afi_derechohabiente afi
    WHERE afi.nss = p_nss;

   IF vid_derechohabiente = 0 OR vid_derechohabiente IS NULL THEN
      LET vcodigo_op     = "3";
      LET vfecha_tramite = TODAY;
   ELSE
      -- se valida que el id_derechohabiente obtenido no exista en la tabla maestro (vigente)
      FOREACH
         SELECT marca, f_inicio
           INTO vmarca_existe, vfecha_tramite
           FROM sfr_marca_activa
          WHERE id_derechohabiente = vid_derechohabiente
            AND marca = p_marca

         IF vmarca_existe = p_marca THEN
            -- se prende la bandera
            LET vcodigo_op = "1";

            EXIT FOREACH;
         END IF
      END FOREACH;
   END IF

   RETURN p_nss, vcodigo_op, vfecha_tramite;

END FUNCTION
;


