






create function "safreviv".fn_cbd_integra_archivo_2116(p_folio decimal(9,0),
                                         p_usuario char(20))
returning integer , integer ,char(255);

define v_nss                    char(11);
define v_id_derechohabiente     decimal (9,0);
define v_subcuenta              smallint;
define v_fondo_de_inversion     smallint;
define v_tipo_movimiento        smallint ;
define v_monto_acciones         decimal(16,0);
define v_monto_acciones_div     decimal(16,6);
define v_monto_pesos            decimal(12,0);
define v_monto_pesos_div        decimal(12,2);

define v_estado                 smallint;
define v_existe_subcuenta       smallint;
define v_total_acciones         decimal (26,6);
define v_total_pesos            decimal (22,2);
define v_total_registros        decimal(9,0);

define v_error                  integer;
define v_error_isam             integer;
define v_cadena                 char (255);
define v_bandera                smallint; 


define v_total                  integer;

on exception set v_error, v_error_isam, v_cadena
   return v_error, v_error_isam, v_cadena;
end exception ;

let v_error=0;
let v_error_isam=0;
let v_cadena = "";
let v_total = 0;

--Se inserta la información del proceso en curso, 
--el estado se deja en cero para ser modificado posteriormente 
--conforme a ciertas evaluaciones
insert into cbd_ctr_ajuste_saldo
values (p_folio,TODAY,0,p_usuario);

let v_bandera=0;                --si algún registro fue correcto se activara y se modificara el
                --estado del proceso en l en la tabla cbd_ctr_ajuste_saldo 
foreach
 --Se obtienen todos los registros de la tabla temporal     
        select tmp.nss,
               tmp.subcuenta,
               tmp.fondo_de_inversion,
               tmp.codigo_del_movimiento,
               tmp.monto_en_acciones,
               tmp.monto_en_pesos
            into v_nss, 
                 v_subcuenta, 
                 v_fondo_de_inversion, 
                 v_tipo_movimiento,
                 v_monto_acciones,
                 v_monto_pesos
            from safre_tmp:tmp_cbd_detalle_ajuste_saldo tmp 
                 
       
   --Se obtiene el id_derechohabiente     
        select id_derechohabiente 
            into v_id_derechohabiente
        from afi_derechohabiente
            where nss=v_nss;
                 
    --Se iniciliza la variable que sirve para editar el estado de la tabla de detalles
    --v_estado=10-->el registro obtenido de la tabla temporal es correcto
            let v_estado=10;
    --Se realizan las pruebas para determinar si el registro obtenido de la tabla temporal
    --fue correcto
            if ((v_id_derechohabiente is null) or (v_subcuenta is null) or
                (v_tipo_movimiento is null) or (v_monto_acciones is null) or
                 (v_monto_pesos is null) or (v_monto_pesos=0) ) then 
                    let v_estado=11;                                    --cambia a 11, es decir incorrecto
            end if
            
--Se realiza una última validación de la existencia del conjunto subcuenta y fondo de inversión
--en la tabla donde se encuentran los conjuntos validos
            select subcuenta 
                into v_existe_subcuenta
                from cbd_valida_ajuste_saldo
                where subcuenta= v_subcuenta and
                      fondo_inversion=v_fondo_de_inversion;
                      
--En caso de que no exista este conjunto se modifica la bandera de estado a incorrecto
-- 
            if v_existe_subcuenta is null then 
                let v_estado=11;
            end if

--Con que un registro sea correcto modificamos el estado de la tabla cbd_ctr_ajuste_saldo
--a 1 (integrado), además de que se activi una bandera para no volver a realizar este update
--y para dar a conocer que se a obtenido almenos un registro correcto
            if v_estado=10 and v_bandera=0 then 
                update cbd_ctr_ajuste_saldo
                    set estado=1
                    where folio=p_folio;
                    let v_bandera=1;
            end if
            
            if v_tipo_movimiento=1 then --Se evalua si se trata de un abono o un cargo
                let v_monto_acciones_div=v_monto_acciones/1000000; 
                let v_monto_pesos_div=v_monto_pesos/100;
            end if 
            
            if v_tipo_movimiento=2 then -- se evalua si se trata de un abono o un cargo 
                let v_monto_acciones_div=v_monto_acciones/1000000; 
                let v_monto_pesos_div=v_monto_pesos/100;
                let v_monto_acciones_div=v_monto_acciones_div*-1;
                let v_monto_pesos_div=v_monto_pesos_div*-1;
            end if 
            

    --Se inserta la información procesada extraida de las tablas temporales 
            insert into cbd_detalle_ajuste_saldo
            values (seq_cbd_detalle_ajuste_saldo.nextval,p_folio,v_nss,v_id_derechohabiente,v_subcuenta,v_fondo_de_inversion,
                    v_tipo_movimiento,v_monto_acciones_div,v_monto_pesos_div,today,v_estado);

            
     let v_total = v_total+1;
end foreach;
--Revisamos si se obtuvo almenos un registro correcto para no cambiar el estado de la tabla
 if v_bandera=0 then 
        update cbd_ctr_ajuste_saldo
            set estado=5
            where folio=p_folio;
    end if



    

drop table if exists tmp_cbd_cifras_ajuste_saldo_2116;
create table tmp_cbd_cifras_ajuste_saldo_2116(
subcuenta smallint,
fondo_inversion smallint,
total_acciones decimal (26,6),
total_pesos decimal (22,2),
total_registros decimal(9,0)
);

--Cargamos una tabla temporal con las cifras generales de los datos obtenidos enteriormente
insert into tmp_cbd_cifras_ajuste_saldo_2116
    select   subcuenta, fondo_inversion ,sum(monto_acciones), sum(monto_pesos),count(*) 
    from cbd_detalle_ajuste_saldo
    where folio=p_folio
    group by subcuenta, fondo_inversion ;
    
foreach
 --Se inserta los registros de la tabla temporal con cifras generales en la tabla correspondiente
    select *
    into v_subcuenta, v_fondo_de_inversion, v_total_acciones, v_total_pesos, v_total_registros 
    from tmp_cbd_cifras_ajuste_saldo_2116

    insert into cbd_cifras_ajuste_saldo
    values (seq_cbd_cifras_ajuste_saldo.nextval,p_folio,v_subcuenta,v_fondo_de_inversion,v_total_acciones,v_total_pesos,v_total_registros);

end foreach ;

--Se elimina tabla temporal de cifras generales
drop table if exists tmp_cbd_cifras_ajuste_saldo_2116;


let v_cadena= "Registros ingresados "|| v_total;
return v_error, v_error_isam, v_cadena;
end function ;


