






CREATE PROCEDURE "safreviv".sp_act_cre_transf(p_id_cre_acreditado DECIMAL(9,0),
                                   p_i_edo_procesar SMALLINT)
   --  se actualiza el estado procesar a p_i_edo_procesar para el registro de
   --  transferencia con id_cre_acreditado igual a p_id_cre_acreditado
   UPDATE safre_viv:cre_acreditado
      SET edo_procesar = p_i_edo_procesar
    WHERE id_cre_acreditado = p_id_cre_acreditado;
END PROCEDURE;


