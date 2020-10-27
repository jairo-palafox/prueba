






CREATE FUNCTION "safreviv".fn_prt_rec_subsec_pendientes()
RETURNING DECIMAL(9,0),
          DECIMAL(22,2),
          DECIMAL(22,2);
          
DEFINE v_registros DECIMAL(9,0);
DEFINE v_monto_aivs_subsec DECIMAL(22,2);
DEFINE v_monto_pesos_subsec DECIMAL(22,2);

   -- Acreditados que estan activos en portabilidad
   DROP TABLE IF EXISTS prt_trp_pendientes;
   CREATE TEMP TABLE prt_trp_pendientes(id_derechohabiente DECIMAL(9,0));
   DROP TABLE IF EXISTS prt_saldos_sub;
   CREATE TEMP TABLE prt_saldos_sub(id_derechohabiente decimal(9,0) , 
                                    monto_acciones decimal(22,2) ,
                                    monto_pesos    decimal(22,2));
   
   -- acréditados con aportaciónes subsecuentes
   INSERT INTO prt_trp_pendientes
   SELECT unique afi.id_derechohabiente
     FROM prt_solicitud_cedente sol JOIN
          afi_derechohabiente afi
       ON sol.nss = afi.nss
    WHERE sol.estado = 40
      AND sol.tipo_portabilidad = 2;
   
   -- acréditados con crédito nuevo
   INSERT INTO prt_trp_pendientes
   SELECT unique afi.id_derechohabiente
     FROM prt_solicitud_cedente sol JOIN
          afi_derechohabiente afi
       ON sol.nss = afi.nss
    WHERE sol.estado = 80
      AND sol.tipo_portabilidad = 1;
   
   INSERT INTO prt_saldos_sub 
   SELECT id_derechohabiente,
          NVL(SUM(monto_acciones),0),
          NVL(SUM(monto_pesos),0)
     FROM TABLE(MULTISET(

   SELECT cta.id_derechohabiente,
          cta.monto_acciones,
          cta.monto_pesos
     FROM cta_movimiento cta JOIN 
          prt_trp_pendientes trp
       ON cta.id_derechohabiente = trp.id_derechohabiente
    WHERE cta.subcuenta = 60
      AND cta.movimiento NOT BETWEEN 1600 AND 1699
      -- Todos los movimientos de subsecuentes que no sean por parte de portabilidad
      -- Provienen de Liquidación de pagos, pero pueden tener diferentes movimientos      

   UNION ALL

   SELECT cta.id_derechohabiente,
          cta.monto_acciones,
          cta.monto_pesos  
     FROM cta_movimiento cta JOIN 
          prt_trp_pendientes trp
       ON cta.id_derechohabiente = trp.id_derechohabiente
    WHERE cta.subcuenta = 60
      AND cta.movimiento = 1604 
))
group by 1
having sum(monto_pesos) > 0;

SELECT count(id_derechohabiente) ,
       sum(monto_acciones ) ,
       sum(monto_pesos) 
INTO   v_registros ,
       v_monto_aivs_subsec,
       v_monto_pesos_subsec
FROM   prt_saldos_sub ;

         -- Los movimientos de subsecuentes portabilidad cedente (1604), matan los saldos que ya se hayan procesado

      
   RETURN v_registros,
          v_monto_aivs_subsec,
          v_monto_pesos_subsec;
END FUNCTION;


