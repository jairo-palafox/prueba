






create procedure "safreviv".act_nss()

define v_contador integer;
define v_id_asociado decimal(9,0);
define v_asociado  char(011);

let v_contador  = 0;
let v_id_asociado = "";

foreach select contador,
               asociado
        into v_contador,
             v_asociado
        from verifica_saldos_op28
        where ind  = 1

        select id_derechohabiente
        into   v_id_asociado
         from afi_derechohabiente 
        where  nss = v_asociado;

        if v_id_asociado is not null then
           update verifica_saldos_op28
           set    id_asociado = v_id_asociado
           where  contador = v_contador;
           let v_id_asociado = "";
        end if;
end foreach;

end procedure;


