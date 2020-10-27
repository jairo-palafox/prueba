






create procedure "safreviv".calcula_saldos_dia_act()

define v_id_det_02_op28 decimal(9,0);
define v_id_invadido char(11);
define v_id_asociado char(11);
define v_sar92_invadido_actual decimal(16,2);
define v_viv97_invadido_actual decimal(16,2);
define v_sar92_asociado_actual decimal(16,2);
define v_viv97_asociado_actual decimal(16,2);

foreach select id_det_02_op28   ,
               id_invadido      ,
               id_asociado
          into v_id_det_02_op28 ,
               v_id_invadido    ,
               v_id_asociado
   from verifica_saldos_op28
  where ind = 6
    and (sar92_separacion <> 0 or viv97_separacion <> 0 )

let v_sar92_invadido_actual = "";
let v_viv97_invadido_actual = "";
let v_sar92_asociado_actual = "";
let v_viv97_asociado_actual = "";

    if v_id_invadido is not null then
     select sum(monto_acciones)
     into   v_sar92_invadido_actual
     from   cta_movimiento
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= "04/01/2016"
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_invadido_actual
     from   cta_movimiento
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= "04/01/2016"
     and    subcuenta = 4;
 end if;
 if v_id_asociado is not null then 
     select sum(monto_acciones)
     into   v_sar92_asociado_actual
     from   cta_movimiento
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= "04/01/2016"
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_asociado_actual
     from   cta_movimiento
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= "04/01/2016"
     and    subcuenta = 4;
 end if;
      update verifica_aplica_op28_def
      set s_92_act_inv = v_sar92_invadido_actual,
          s_97_act_inv = v_viv97_invadido_actual,
          s_92_act_asc = v_sar92_asociado_actual,
          s_97_act_asc = v_viv97_asociado_actual
      where id_det_02_op28 = v_id_det_02_op28;

end foreach;
end procedure;


