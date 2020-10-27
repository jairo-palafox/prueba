






CREATE PROCEDURE "safreviv".sp_act_edo_procesar(p_id_cre_acreditado DECIMAL(9,0),
                                     p_edo_procesar SMALLINT)
   --actualiza edo_procesar en acreditado
   UPDATE safre_viv:cre_acreditado
      SET edo_procesar =  p_edo_procesar
    WHERE id_cre_acreditado = p_id_cre_acreditado;

END PROCEDURE;


