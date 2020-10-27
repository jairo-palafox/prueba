






CREATE PROCEDURE "safreviv".fn_act_dse_devolucion(p_id_dse_devolucion DECIMAL(9,0),
                                       p_i_edo_procesar SMALLINT)
   --  se actualiza el estado procesar a p_i_edo_procesar para el registro de devolución
   -- con id dse devolucion igual a p id dse devolucion
   UPDATE safre_viv:dse_devolucion
      SET edo_procesar = p_i_edo_procesar
    WHERE id_dse_devolucion = p_id_dse_devolucion;
END PROCEDURE;


