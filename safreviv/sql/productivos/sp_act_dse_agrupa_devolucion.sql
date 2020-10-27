






CREATE PROCEDURE "safreviv".sp_act_dse_agrupa_devolucion(p_id_dse_grp_devolucion DECIMAL(9,0),
                                              p_i_edo_procesar SMALLINT)
   --  se actualiza el estado procesar a p_i_edo_procesar
   UPDATE safre_viv:dse_agrupa_devolucion
      SET edo_procesar = p_i_edo_procesar
    WHERE id_dse_grp_devolucion = p_id_dse_grp_devolucion;
END PROCEDURE;


