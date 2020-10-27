






CREATE PROCEDURE "safreviv".sp_dis_avance_pago3(p_folio   DECIMAL(9,0),
                                     p_usuario CHAR(20))
                                     
DEFINE v_sp_f_actualiza DATE;
--DEFINE v_sp_estado      SMALLINT;

LET v_sp_f_actualiza  = TODAY;
--LET v_sp_estado = 0;


   --Actualiza los valores de registro del archivo de avance de pagos
   -- en la tabla de sumario
   UPDATE safre_viv:dis_sum_avance_pago 
      SET safre_viv:dis_sum_avance_pago.estado      = 30--v_sp_estado,
          --safre_viv:dis_sum_avance_pago.f_actualiza = v_sp_f_actualiza,
          --safre_viv:dis_sum_avance_pago.usuario     = p_usuario
    WHERE safre_viv:dis_sum_avance_pago.folio       = p_folio;

   --Actualiza los valores de registro del archivo de avance de pagos
   -- en la tabla de detalle
   UPDATE safre_viv:dis_det_avance_pago 
      SET safre_viv:dis_det_avance_pago.estado      = 30--v_sp_estado,     
          --safre_viv:dis_det_avance_pago.f_actualiza = v_sp_f_actualiza,
          --safre_viv:dis_det_avance_pago.usuario     = p_usuario    
    WHERE safre_viv:dis_det_avance_pago.folio       = p_folio;         

END PROCEDURE;


