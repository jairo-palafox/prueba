DATABASE safre_viv

MAIN

   CALL fn_valida_archivo()

END MAIN

FUNCTION fn_valida_archivo ()
   DEFINE v_total_sum      INTEGER
   DEFINE v_total_detalle  INTEGER

   DEFINE v_proceso_cod    SMALLINT
   DEFINE v_opera_cod      SMALLINT
   DEFINE v_pid            DECIMAL(9,0)

   LET v_proceso_cod = 1815
   LET v_opera_cod = 1

   SELECT MAX(pid)
     INTO v_pid 
     FROM bat_ctr_proceo
    WHERE proceso_cod = v_proceso_cod
      AND estado_cod = 2

   SELECT tot_registros
     INTO v_total_sum
     FROM tmp_sum_ind_not

   SELECT COUNT(*)
     INTO v_total_detalle
     FROM tmp_det_ind_not

   IF v_total_sum <> v_total_detalle THEN
      
   ELSE
      
   END IF
   
END FUNCTION
