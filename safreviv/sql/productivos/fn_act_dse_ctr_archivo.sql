






CREATE PROCEDURE "safreviv".fn_act_dse_ctr_archivo(p_d_folio DECIMAL(9,0),
                                        p_d_pid DECIMAL(9,0),
                                        v_i_tpo_transferencia SMALLINT,
                                        v_i_tot_aceptados INTEGER,
                                        v_i_tot_rechazados INTEGER)
   --  se actualiza el registro de control de archivos a estus "integrado" (marcada) 
   UPDATE safre_viv:dse_ctr_archivo
      SET folio = p_d_folio,
          estado = 20,
          tot_aceptados = v_i_tot_aceptados,
          tot_rechazados = v_i_tot_rechazados
    WHERE tpo_transferencia = v_i_tpo_transferencia
      AND estado = 10;

END PROCEDURE;


