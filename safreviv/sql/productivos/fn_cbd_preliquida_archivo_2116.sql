






create function "safreviv".fn_cbd_preliquida_archivo_2116( p_folio decimal(9,0),
                                                  p_usuario char(20))
    returning integer , integer ,char(255),char(11),smallint,smallint,smallint,
               decimal(16,6),decimal(12,2),date;
    define v_nss                            char(11);
    define v_f_liquida                      date;
    define v_id_derechohabiente             decimal(9,0);
    define v_subcuenta                      smallint;
    define v_fondo_inversion                smallint;
    define v_movimiento                     smallint;
    define v_folio_liquida                  decimal(9,0);
    define v_id_referencia                  decimal (9,0);
    define v_monto_acciones                 decimal (16,6);
    define v_monto_pesos                    decimal(12,2);
    define v_f_valor                        date;
    define v_f_registro                     date;
    define v_h_registro                     DATETIME hour to second ;
    define v_origen                         char(20);
    define v_id_cbd_detalle_ajuste_saldo    decimal(9,0);
    define v_marca                          smallint;
    define v_error                          integer;
    define v_error_isam                     integer;
    define v_cadena                         char (255);

    on exception set v_error, v_error_isam, v_cadena
        return v_error, v_error_isam, v_cadena,null,null,null,null,null,null,null;
    end exception ;

   
    let v_error=0;
    let v_error_isam=0;
    let v_cadena="";

    
 --Se crea la tabla que contendra los detalles de la integración   
    drop table if exists cbd_preliquida_ajuste_saldo; 
    create table cbd_preliquida_ajuste_saldo (
        f_liquida                   date,
        id_derechohabiente          decimal(9,0),
        subcuenta                   smallint,
        fondo_inversion             smallint,
        movimiento                  smallint,
        folio_liquida               decimal(9,0),
        id_referencia               decimal(9,0),
        monto_acciones              decimal(16,6),
        monto_pesos                 decimal(12,2),
        f_valor                     date,
        f_registro                  date,
        h_registro                  DATETIME hour to second,
        origen                      char(20)
                                              );
--Se realiza el barrido e insersión de los datos con validaciones previas
    foreach 
        select id_derechohabiente,
                subcuenta,
                fondo_inversion,
                tipo_movimiento,
                id_cbd_detalle_ajuste_saldo,
                monto_acciones,
                monto_pesos,
                f_valor
            into    v_id_derechohabiente,
                    v_subcuenta,
                    v_fondo_inversion,
                    v_movimiento,
                    v_id_referencia,
                    v_monto_acciones,
                    v_monto_pesos,
                    v_f_valor
            from cbd_detalle_ajuste_saldo
            where folio=p_folio and
                   estado=10     --se evalua el estado 10 (registros aceptados)

         
                select marca --Consulta que obtiene la marca del derechohabiente para posteriormente verificar que no tenga la marca 160
                    into v_marca
                    from sfr_marca_activa
                    where id_derechohabiente=v_id_derechohabiente and 
                          marca=160;

        
        if v_marca=160 then--Se verifica que no se tenga la marca 160

                update cbd_detalle_ajuste_saldo --Se modifica el estado del registro a 11 (rechazado) pues contiene la marca 160
                       set estado=11 
                       where nss in (select nss 
                                        from afi_derechohabiente
                                        where id_derechohabiente=v_id_derechohabiente);

                
        else 
                if v_fondo_inversion = 11 then --se realizan las validaciones para 
                                               --elegir el tipo de movimiento con
                                               --con respecto al fondo de inversion y el tipo de movimiento 
                        if v_movimiento=1 then --abono
                           let v_movimiento=1681;
                        end if

                        if v_movimiento=2 then --Cargo
                           let v_movimiento=1712;                          
                        end if
                end if 


                
                if v_fondo_inversion=0 then --se realizan las validaciones para 
                                            --elegir el tipo de movimiento con
                                            --con respecto al fondo de inversion y el tipo de movimiento
                    if v_movimiento=1 then  --abono
                           let v_movimiento=1681;                           let v_monto_acciones=0;
                           
                    end if

                    if v_movimiento=2 then --cargo
                           let v_movimiento=1712;
                           let v_monto_acciones=0;
                          
                    end if
                    
                end if 



                if v_fondo_inversion=10 then --se realizan las validaciones para 
                                             --elegir el tipo de movimiento con
                                             --con respecto al fondo de inversion y el tipo de movimiento
                    if v_movimiento=1 then 
                           let v_movimiento=1681;                           let v_monto_acciones=v_monto_pesos;
                          
                    end if

                    if v_movimiento=2 then 
                           let v_movimiento=1712;
                           let v_monto_acciones=v_monto_pesos;                          
                    end if
                    
                end if 

                let v_h_registro=current hour to second ;
                insert into cbd_preliquida_ajuste_saldo
                           values(today,
                                  v_id_derechohabiente,
                                  v_subcuenta,
                                  v_fondo_inversion,
                                  v_movimiento,
                                  p_folio,
                                  v_id_referencia,
                                  v_monto_acciones,
                                  v_monto_pesos,
                                  v_f_valor,
                                  today,
                                  v_h_registro,
                                  "AJUSTE DE SALDO");
        end if
        
    end foreach

   foreach --Se cargan los datos de los registros que tienen el estado 11 (rechazados) para retornarlos
        select nss,
               subcuenta,
               fondo_inversion,
               tipo_movimiento,
               abs(monto_acciones),
               abs(monto_pesos),
               f_valor
         into v_nss,
              v_subcuenta,
              v_fondo_inversion,
              v_movimiento,
              v_monto_acciones,
              v_monto_pesos,
              v_f_valor
         from cbd_detalle_ajuste_saldo
         where estado=11 and folio=p_folio

         return v_error, v_error_isam, v_cadena,v_nss,v_subcuenta,
                v_fondo_inversion,v_movimiento,v_monto_acciones,
                v_monto_pesos, v_f_valor with resume;
  end foreach 
    

end function;


