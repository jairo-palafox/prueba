






CREATE PROCEDURE "safreviv".fn_act_edo_acre_his_id_ctr(p_d_id_cre_ctr_arch DECIMAL(9,0),
                                            p_i_estado SMALLINT)
   -- actualiza estado en la tabla maestro
   UPDATE safre_viv:cre_acreditado
      SET estado =  p_i_estado
    WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch;

   -- actualiza estado en his transferencia
   UPDATE safre_viv:cre_his_acreditado
      SET estado =  p_i_estado
    WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
END PROCEDURE        ;


