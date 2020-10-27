






create procedure "safreviv".prueba_vista();

define ej char(200);
set debug file to 'salida';
trace on;
let ej='cd /safreviv/sep/bin; dbaccess safre_viv .crea_vista.sql';
system ej;

end procedure

;


