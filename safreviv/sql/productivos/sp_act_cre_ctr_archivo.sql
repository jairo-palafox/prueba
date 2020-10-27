






CREATE PROCEDURE "safreviv".sp_act_cre_ctr_archivo(p_d_folio DECIMAL(9,0),
                                        p_tot_aceptados INTEGER,
                                        p_tot_rechazados INTEGER,
                                        p_tot_sin_origen INTEGER,
                                        p_ax_id_cre_ctr_arch DECIMAL(9,0))
   --  se actualiza el registro de control de archivos a estus "integrado" (marcada) 
   UPDATE safre_viv:cre_ctr_archivo
      SET estado = 20,
          folio_archivo = p_d_folio,
          tot_aceptados = p_tot_aceptados,
          tot_rechazados = p_tot_rechazados,
          tot_sin_origen = p_tot_sin_origen
    WHERE id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
END PROCEDURE
;


