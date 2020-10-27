






create procedure "safreviv".sp_aperturaf72()

define v_id_derechohabiente decimal(9,0);
define v_folio decimal(9,0);
define v_id_referencia decimal(9,0);
define v_id_fondo72 decimal(9,0);
define v_rfc char(13);
define v_nss char(11);
define v_f_solicitud date;
define v_ind_cuenta smallint;

let v_ind_cuenta = 0;

set pdqpriority high;

drop table if exists tmp_todos;

select mov.folio_liquida folio,
       mov.id_referencia,
       ret.id_derechohabiente
from cta_fondo72 mov
inner join ret_fondo_ahorro ret
on ( ret.folio = mov.folio_liquida and
     ret.id_solicitud = mov.id_referencia)
where mov.id_afi_fondo72 is null
into temp tmp_todos
;
update statistics for table tmp_todos;

foreach
  select distinct tmp.folio, tmp.id_referencia,
         afid.id_derechohabiente,
         afid.nss, afid.rfc, ret.f_solicitud
    into v_folio, v_id_referencia, v_id_derechohabiente,
         v_nss, v_rfc, v_f_solicitud
    from tmp_todos tmp
    inner join afi_fondo72_d afid
    on ( afid.id_derechohabiente = tmp.id_derechohabiente)
    inner join ret_fondo_ahorro ret
    on ( ret.id_solicitud = tmp.id_referencia)

-- apertura

   insert into afi_fondo72 values (seq_afi_fondo72.NEXTVAL,
                            v_rfc                  ,
                            v_nss                  ,
                            v_id_derechohabiente   ,
                            NULL                   ,
                            v_f_solicitud          ,
                            v_ind_cuenta           ,
                            NULL);

   select seq_afi_fondo72.CURRVAL
     INTO v_id_fondo72
     FROM systables
    WHERE tabid = 99;

  update cta_fondo72
     set id_afi_fondo72 = v_id_fondo72
    where folio_liquida = v_folio
      and id_referencia = v_id_referencia
      and id_afi_fondo72 is null;

end foreach;

end procedure


;


