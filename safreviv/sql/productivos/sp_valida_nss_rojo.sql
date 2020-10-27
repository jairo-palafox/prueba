






CREATE PROCEDURE "safreviv".sp_valida_nss_rojo (p_nss CHAR(11), p_tpo_consulta SMALLINT, p_usuario CHAR(20))

   RETURNING SMALLINT;

   DEFINE r_indicador   SMALLINT;
   DEFINE v_f_consulta  DATE;
   DEFINE v_h_consulta  DATETIME HOUR TO SECOND;
   
   DEFINE v_ind_usuario SMALLINT;
   DEFINE v_ind_nss     SMALLINT;

   IF EXISTS (SELECT nss
                FROM afi_nss_rojo
               WHERE nss = p_nss
                 AND estado_rojo = 1) THEN

      LET v_ind_nss  = 1;
      LET v_f_consulta = TODAY;
      LET v_h_consulta = CURRENT;

   ELSE
      LET v_ind_nss = 0;
   END IF
   
   IF v_ind_nss = 1 THEN
      IF EXISTS (select s.perfil_cod
                   from cat_perfil_rojo c,
                        seg_perfil s,
                        seg_usuario_perfil u
                  where u.usuario_cod = p_usuario
                    and c.perfil_corta = s.perfil_corta
                    and s.perfil_cod = u.perfil_cod 
                    and c.estado_rojo= 1) THEN
         LET v_ind_usuario = 1;
      ELSE
         LET v_ind_usuario = 0;
      END IF
   END IF
      
      IF (v_ind_usuario = 0) AND (v_ind_nss = 1) THEN
         LET r_indicador = 1;
         
         INSERT INTO afi_log_consulta
         VALUES(p_usuario,
                p_nss,
                p_tpo_consulta,
                v_f_consulta,
                v_h_consulta);
      ELSE
         LET r_indicador = 0;
      END IF
      
      
RETURN r_indicador;

END PROCEDURE;


