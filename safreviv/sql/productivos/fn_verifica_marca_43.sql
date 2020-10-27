






CREATE FUNCTION "safreviv".fn_verifica_marca_43( p_nss   CHAR(11),
                                      p_marca SMALLINT,
                                      p_id_dh DECIMAL(9,0) )
   RETURNING CHAR(11), CHAR(1), DATE, SMALLINT, DECIMAL(9,0)

   DEFINE vid_derechohabiente       DECIMAL(9,0);
   DEFINE vcodigo_op                CHAR(1);
   DEFINE vfecha_tramite            DATE;
   DEFINE vmarca_existe             SMALLINT;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/existeMarca.trace';
   --TRACE ON;

   LET vid_derechohabiente = 0;
   LET vcodigo_op          = "1";
   LET vmarca_existe       = 0;

   IF p_id_dh IS NULL OR p_id_dh < 0 THEN 
      LET vid_derechohabiente = p_id_dh;
   ELSE 
      SELECT afi.id_derechohabiente
        INTO vid_derechohabiente
        FROM afi_derechohabiente afi
       WHERE afi.nss = p_nss;
   END IF
 
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
            ORDER BY f_inicio DESC 

             EXIT FOREACH;
      END FOREACH;

      IF vmarca_existe = p_marca THEN
         -- se prende la bandera
         LET vcodigo_op = "1";
      ELSE
         -- se mantiene la bandera apagada
         LET vcodigo_op     = "2";
         LET vfecha_tramite = TODAY;
      END IF
   END IF

   RETURN p_nss, vcodigo_op, vfecha_tramite, p_marca, p_id_dh;

END FUNCTION
;


