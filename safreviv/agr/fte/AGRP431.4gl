-----------------------------------------------------------------------------------------
-- Modulo     => AGR
-- Programa   => AGRP431
-- Objetivo   => Programa para desplegar contadores
-- Fecha      => 22-JUN-2017
-- Autor      => GERARDO ALFONSO VEGA PAREDES
-----------------------------------------------------------------------------------------

DATABASE safre_viv

MAIN

   CALL principal()

END MAIN


FUNCTION principal()
   DEFINE v_id_derecho     DECIMAL(9,0),
          v_id_cre_tramite DECIMAL(9,0)
   
   DEFINE reg RECORD
     tpo_registro char(2), 
     nss          char(11),
     marca        char(1), 
     resultado    char(1), 
     diagnostico  char(3) 
   END RECORD
   
   DEFINE v_cont_acep INTEGER,
          v_cont_rech INTEGER

   LET v_cont_acep = 0
   LET v_cont_rech = 0

   DECLARE cur_agr CURSOR FOR
   SELECT * 
   FROM   safre_tmp:tmp_diag_procesar   

   FOREACH cur_agr INTO reg.*

      SELECT id_derechohabiente
      INTO   v_id_derecho
      FROM   afi_derechohabiente
      WHERE  nss = reg.nss
      
      IF v_id_derecho IS NULL THEN
      	 LET v_cont_rech = v_cont_rech + 1
      ELSE
         SELECT id_cre_tramite
         INTO   v_id_cre_tramite
         FROM   cre_tramite
         WHERE  id_derechohabiente = v_id_derecho
         AND    estado = 18

         IF v_id_cre_tramite IS NULL THEN
      	    LET v_cont_rech = v_cont_rech + 1
         ELSE
            LET v_cont_acep = v_cont_acep + 1

            UPDATE cre_his_tramite
            SET    diagnostico = "3"||reg.diagnostico
            WHERE  id_cre_tramite = v_id_cre_tramite
         END IF
         LET v_id_cre_tramite = NULL
      END IF

--         UPDATE cre_his_tramite
--         SET    diagnostico = reg.diagnostico
--         WHERE  id_cre_tramite in (SELECT id_cre_tramite 
--                                   FROM   cre_tramite
--                                   WHERE id_derechohabiente = v_id_derecho)      
--         IF SQLCA.sqlcode = 0 THEN
--            LET v_cont_acep = v_cont_acep + 1
--         ELSE
--     	      LET v_cont_rech = v_cont_rech + 1
--         END IF

   END FOREACH

   DISPLAY "REGISTROS ACEPTADOS:  ",v_cont_acep
   DISPLAY ""
   DISPLAY "REGISTROS RECHAZADOS: ",v_cont_rech

END FUNCTION