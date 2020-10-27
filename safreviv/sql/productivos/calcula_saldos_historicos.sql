






create procedure "safreviv".calcula_saldos_historicos()

define v_id_det_02_op28 decimal(9,0);
define v_id_invadido char(11);
define v_id_asociado char(11);
define v_f_proceso date;

define v_sar92_invadido_his decimal(16,2);
define v_viv97_invadido_his decimal(16,2);
define v_sar92_asociado_his decimal(16,2);
define v_viv97_asociado_his decimal(16,2);

define v_nat  char(10);
define v_habil date;
define v_ano  char(4);

foreach select f_proceso,id_det_02_op28,id_invadido,id_asociado
        into v_f_proceso,v_id_det_02_op28, v_id_invadido,v_id_asociado
        from verifica_saldos_op28


        IF v_id_invadido is null  or 
           v_id_asociado is null then 
           CONTINUE FOREACH; 
        end if;

   let v_sar92_invadido_his = "";
   let v_viv97_invadido_his = "";

   let v_sar92_asociado_his = "";
   let v_viv97_asociado_his = "";

   let v_ano = year(v_f_proceso);

   let v_nat = mdy(month(v_f_proceso),"01",year(v_f_proceso));

   EXECUTE FUNCTION fn_habil_siguiente(v_nat,1) INTO v_habil; 

    IF v_ano = 2012 THEN

     select sum(monto_acciones)
     into   v_sar92_invadido_his
     from   cta_movimiento12
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_invadido_his
     from   cta_movimiento12
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 4;

     select sum(monto_acciones)
     into   v_sar92_asociado_his
     from   cta_movimiento12
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_asociado_his
     from   cta_movimiento12
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 4;


   ELIF v_ano = 2013 THEN

     select sum(monto_acciones)
     into   v_sar92_invadido_his
     from   cta_movimiento13
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_invadido_his
     from   cta_movimiento13
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 4;

     select sum(monto_acciones)
     into   v_sar92_asociado_his
     from   cta_movimiento13
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_asociado_his
     from   cta_movimiento13
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 4;

   ELIF v_ano = 2014 THEN

     select sum(monto_acciones)
     into   v_sar92_invadido_his
     from   cta_movimiento14
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_invadido_his
     from   cta_movimiento14
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 4;

     select sum(monto_acciones)
     into   v_sar92_asociado_his
     from   cta_movimiento14
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_asociado_his
     from   cta_movimiento14
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 4;


   ELIF v_ano = 2015 THEN

     select sum(monto_acciones)
     into   v_sar92_invadido_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_invadido_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 4;

     select sum(monto_acciones)
     into   v_sar92_asociado_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_asociado_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 4;

   ELIF v_ano = 2016 THEN

     select sum(monto_acciones)
     into   v_sar92_invadido_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_invadido_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_invadido
     and    f_liquida <= v_habil
     and    subcuenta = 4;

     select sum(monto_acciones)
     into   v_sar92_asociado_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 8;

     select sum(monto_acciones)
     into   v_viv97_asociado_his
     from   cta_movimiento
     where  id_derechohabiente = v_id_asociado
     and    f_liquida <= v_habil
     and    subcuenta = 4;

   END IF;

      update verifica_saldos_op28
      set sar92_invadido_historico = v_sar92_invadido_his,
          viv97_invadido_historico = v_viv97_invadido_his,
          sar92_asociado_historico = v_sar92_asociado_his,
          viv97_asociado_historico = v_viv97_asociado_his
      where id_det_02_op28 = v_id_det_02_op28;

end foreach;
end procedure;


