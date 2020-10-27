






CREATE PROCEDURE "safreviv".sp_act_edo_acre_his_id_acre(p_d_id_cre_acreditado DECIMAL(9,0),
                                             p_i_estado SMALLINT)
   --actualiza estado en acreditado
   UPDATE safre_viv:cre_acreditado
      SET estado = p_i_estado
    WHERE id_cre_acreditado = p_d_id_cre_acreditado;

   --actualiza estado en his acreditado
   UPDATE safre_viv:cre_his_acreditado
      SET estado = p_i_estado
    WHERE id_cre_acreditado = p_d_id_cre_acreditado;
END PROCEDURE;


