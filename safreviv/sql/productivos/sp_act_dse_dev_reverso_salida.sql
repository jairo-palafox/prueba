






CREATE PROCEDURE "safreviv".sp_act_dse_dev_reverso_salida(p_c_tpo_transferencia CHAR(2),
                                               p_folio  DECIMAL(9,0))
   -- se actualiza registros de devolucion
   UPDATE safre_viv:dse_agrupa_devolucion
      SET edo_procesar = 20
    WHERE edo_procesar = 80
      AND estado = 140
      AND tpo_transferencia = p_c_tpo_transferencia;

   -- se actualiza registros de devolucion
   UPDATE safre_viv:dse_agrupa_devolucion
      SET edo_procesar = 70
    WHERE edo_procesar = 85
      AND estado = 140
      AND tpo_transferencia = p_c_tpo_transferencia;

   -- se actualiza registros de his devolucion
   UPDATE safre_viv:dse_his_devolucion
      SET edo_procesar = 20
    WHERE edo_procesar = 80
      AND estado = 140
      AND tpo_transferencia = p_c_tpo_transferencia;

   -- se actualiza registros de his devolucion
   UPDATE safre_viv:dse_his_devolucion
      SET edo_procesar = 70
    WHERE edo_procesar = 85
      AND estado = 140
      AND tpo_transferencia = p_c_tpo_transferencia;

   UPDATE safre_viv:dse_ctr_archivo
      SET estado = 30
    WHERE folio = p_folio;
{
   DELETE
     FROM safre_viv:dse_ctr_archivo
    WHERE folio = p_folio;
}
END PROCEDURE
;


