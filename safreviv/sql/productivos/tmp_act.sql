






create procedure "safreviv".tmp_act()

define v_id_bus dec(10,0);
define v_id_bus_tramite dec(10,0);
define v_id_prt_solicitud_cedente dec(10,0);
define v_nss char(011);
define v_id_servicio smallint;
define v_folio_procesar_bus char(050);
define v_folio_procesar_prt char(050);

foreach
select min(a.id_bus_solicitud_tramite) id_bus_solicitud_tramite ,
       min(c.id_bus_tramite) id_bus_tramite,
       b.id_prt_solicitud_cedente,
       b.nss ,
       a.id_servicio ,
       max(c.folio_procesar) folio_procesar_bus,
       d.folio_procesar
into v_id_bus ,
     v_id_bus_tramite ,
     v_id_prt_solicitud_cedente ,
     v_nss ,
     v_id_servicio ,
     v_folio_procesar_bus ,
     v_folio_procesar_prt
from bus_solicitud_tramite a ,
     prt_solicitud_cedente b,
     bus_tramite c ,
     prt_traspaso_cedente d
where a.nss = b.nss
and   b.estado = 50
and   a.id_servicio = 594
and   a.id_bus_tramite = c.id_bus_tramite
and   b.id_prt_solicitud_cedente = d.id_prt_solicitud_cedente
group by 3,4,5,7


      UPDATE prt_traspaso_cedente
       set   folio_procesar = v_folio_procesar_bus
      where id_prt_solicitud_cedente = v_id_prt_solicitud_cedente
      and    estado = 10
      and   folio_procesar = v_folio_procesar_prt;


end foreach;
end procedure
;


