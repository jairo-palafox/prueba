DATABASE safre_viv
GLOBALS
   
    DEFINE reg_3 RECORD
       ruta_rescate           CHAR(40) ,
       ruta_envio             CHAR(40)
    END RECORD
    
    DEFINE
        v_archivo_entrada     CHAR(100) ,
        v_archivo_salida      CHAR(100) ,
        v_sql                 CHAR(200)
        
END GLOBALS

MAIN
    SELECT ruta_rescate ,
           ruta_envio
    INTO   reg_3.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
     
    DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_nss_retiro_e;
        DROP TABLE tmp_nss_laudos_pagados;
        DROP TABLE tmp_nss_laudos_en_tramite;
    WHENEVER ERROR STOP 

    CREATE TABLE tmp_nss_retiro_e (
                                   nss char(11)
                                  );
    CREATE TABLE tmp_nss_laudos_pagados (
                                         nss char(11)
                                        );

    CREATE TABLE tmp_nss_laudos_en_tramite (
                                            nss char(11)
                                           );
                                    

    LET v_archivo_entrada = reg_3.ruta_rescate CLIPPED,"/","NSS_RETIRO_E.dat"
    --prompt "v_archivo_entrada",v_archivo_entrada for char enter 
    LOAD FROM v_archivo_entrada INSERT INTO tmp_nss_retiro_e;

    LET v_archivo_entrada = reg_3.ruta_rescate CLIPPED,"/","NSS_LAUDOS_PAGADOS.dat"
    --prompt "v_archivo_entrada",v_archivo_entrada for char enter 
    LOAD FROM v_archivo_entrada INSERT INTO tmp_nss_laudos_pagados;

    LET v_archivo_entrada = reg_3.ruta_rescate CLIPPED,"/","NSS_LAUDOS_EN_TRAMITE.dat"
    --prompt "v_archivo_entrada",v_archivo_entrada for char enter 
    LOAD FROM v_archivo_entrada INSERT INTO tmp_nss_laudos_en_tramite;

    DATABASE safre_viv
    
    --RESPALDAMOS LA INFORMACIÓN--------------------    
    LET v_archivo_salida = reg_3.ruta_envio CLIPPED,"/","casos_retiro_e.unl"
    --prompt "v_archivo_salida",v_archivo_salida for char enter 
      
    UNLOAD TO v_archivo_salida
    SELECT tnrs.nss || "," || nvl(sma.marca,"SIN MARCA") || "," || nvl(sm.descripcion_marca,"SIN MARCA") || "," || 
           nvl(viv92.acc92,0) || "," || nvl(viv92.pesos92,0) || "," || 
           nvl(viv97.acc97,0) || "," || nvl(viv97.pesos97,0)
      FROM safre_tmp:tmp_nss_retiro_e  tnrs 
           LEFT OUTER JOIN (SELECT tnr.nss, SUM(monto_acciones) AS acc92, SUM(monto_pesos) AS pesos92,
                                   ad.id_derechohabiente, cm.subcuenta
                              FROM safre_tmp:tmp_nss_retiro_e tnr 
                                   LEFT OUTER JOIN afi_derechohabiente ad 
                                                ON tnr.nss = ad.nss
                                   LEFT OUTER JOIN cta_movimiento cm 
                                                ON ad.id_derechohabiente = cm.id_derechohabiente
                                               AND cm.subcuenta = 4  
                                               AND cm.monto_acciones > 0
                             GROUP BY tnr.nss, ad.id_derechohabiente, cm.subcuenta) AS viv92 
                        ON tnrs.nss = viv92.nss
           LEFT OUTER JOIN (SELECT tnr.nss, SUM(monto_acciones) AS acc97, SUM(monto_pesos) AS pesos97, 
                                   ad.id_derechohabiente, cm.subcuenta
                              FROM safre_tmp:tmp_nss_retiro_e tnr 
                                   LEFT OUTER JOIN afi_derechohabiente ad 
                                                ON tnr.nss = ad.nss
                                   LEFT OUTER JOIN cta_movimiento cm 
                                                ON ad.id_derechohabiente = cm.id_derechohabiente
                                               AND cm.subcuenta = 8  
                                               AND cm.monto_acciones > 0
                             GROUP BY tnr.nss, ad.id_derechohabiente, cm.subcuenta) AS viv97 
                        ON tnrs.nss = viv97.nss
           LEFT OUTER JOIN afi_derechohabiente AS addm 
                        ON tnrs.nss = addm.nss
           LEFT OUTER JOIN sfr_marca_activa AS sma 
                        ON addm.id_derechohabiente = sma.id_derechohabiente
           LEFT OUTER JOIN sfr_marca AS sm 
                        ON sma.marca = sm.marca;

    
    LET v_archivo_salida = reg_3.ruta_envio CLIPPED,"/","casos_laudos_pagados.unl"
    --prompt "v_archivo_salida",v_archivo_salida for char enter 
      
    UNLOAD TO v_archivo_salida  

    SELECT tnrs.nss || "," || nvl(sma.marca,"SIN MARCA") || "," || nvl(sm.descripcion_marca,"SIN MARCA") || "," || 
           nvl(viv92.acc92,0) || "," || nvl(viv92.pesos92,0) || "," || 
           nvl(viv97.acc97,0) || "," || nvl(viv97.pesos97,0)
      FROM safre_tmp:tmp_nss_laudos_pagados  tnrs 
           LEFT OUTER JOIN (SELECT tnr.nss, SUM(monto_acciones) AS acc92, SUM(monto_pesos) AS pesos92, 
                                   ad.id_derechohabiente, cm.subcuenta
                              FROM safre_tmp:tmp_nss_laudos_pagados tnr 
                                 LEFT OUTER JOIN afi_derechohabiente ad 
                                              ON tnr.nss = ad.nss
                                 LEFT OUTER JOIN cta_movimiento cm 
                                              ON ad.id_derechohabiente = cm.id_derechohabiente
                                             AND cm.subcuenta = 4  
                                             AND cm.monto_acciones > 0
                            GROUP by tnr.nss, ad.id_derechohabiente, cm.subcuenta) AS viv92 
                        ON tnrs.nss = viv92.nss
           LEFT OUTER JOIN (SELECT tnr.nss, SUM(monto_acciones) AS acc97, SUM(monto_pesos) AS pesos97, 
                                   ad.id_derechohabiente, cm.subcuenta
                              FROM safre_tmp:tmp_nss_laudos_pagados tnr 
                                   LEFT OUTER JOIN afi_derechohabiente ad 
                                                ON tnr.nss = ad.nss
                                   LEFT OUTER JOIN cta_movimiento cm 
                                                ON ad.id_derechohabiente = cm.id_derechohabiente
                                               AND  cm.subcuenta = 8  
                                               AND cm.monto_acciones > 0
                              GROUP BY tnr.nss, ad.id_derechohabiente, cm.subcuenta) AS viv97 
                        ON tnrs.nss = viv97.nss
           LEFT OUTER JOIN afi_derechohabiente as addm 
                        ON tnrs.nss = addm.nss
           LEFT OUTER JOIN sfr_marca_activa sma 
                        ON addm.id_derechohabiente = sma.id_derechohabiente
           LEFT OUTER JOIN sfr_marca sm 
                        ON sma.marca = sm.marca;
    
    LET v_archivo_salida = reg_3.ruta_envio CLIPPED,"/","casos_laudos_en_tramite.unl"
    --prompt "v_archivo_salida",v_archivo_salida for char enter 
      
    UNLOAD TO v_archivo_salida   

      SELECT tnrs.nss || "," || nvl(sma.marca,"SIN MARCA") || "," || nvl(sm.descripcion_marca,"SIN MARCA") || "," || 
             nvl(viv92.acc92,0) || "," || nvl(viv92.pesos92,0) || "," || 
             nvl(viv97.acc97,0) || "," || nvl(viv97.pesos97,0)
        FROM safre_tmp:tmp_nss_laudos_en_tramite  tnrs 
             LEFT OUTER JOIN (SELECT tnr.nss, SUM(monto_acciones) AS acc92, SUM(monto_pesos) AS pesos92, 
                                     ad.id_derechohabiente, cm.subcuenta
                                FROM safre_tmp:tmp_nss_laudos_en_tramite tnr 
                                     LEFT OUTER JOIN afi_derechohabiente ad 
                                                  ON tnr.nss = ad.nss
                                     LEFT OUTER JOIN cta_movimiento cm 
                                                  ON ad.id_derechohabiente = cm.id_derechohabiente
                                                 AND cm.subcuenta = 4  
                                                 AND cm.monto_acciones > 0
                                GROUP BY tnr.nss, ad.id_derechohabiente, cm.subcuenta) AS viv92 
                          ON tnrs.nss = viv92.nss
             LEFT OUTER JOIN (SELECT tnr.nss, SUM(monto_acciones) AS acc97, SUM(monto_pesos) AS pesos97, 
                                     ad.id_derechohabiente, cm.subcuenta
                                FROM safre_tmp:tmp_nss_laudos_en_tramite tnr 
                                     LEFT OUTER JOIN afi_derechohabiente ad 
                                                  ON tnr.nss = ad.nss
                                     LEFT OUTER JOIN cta_movimiento cm 
                                                  ON ad.id_derechohabiente = cm.id_derechohabiente
                                                 AND cm.subcuenta = 8  
                                                 AND cm.monto_acciones > 0
                                GROUP BY tnr.nss, ad.id_derechohabiente, cm.subcuenta) AS viv97 
                          ON tnrs.nss = viv97.nss
             LEFT OUTER JOIN afi_derechohabiente as addm 
                          ON tnrs.nss = addm.nss
             LEFT OUTER JOIN sfr_marca_activa sma 
                          ON addm.id_derechohabiente = sma.id_derechohabiente
             LEFT OUTER JOIN sfr_marca sm 
                          ON sma.marca = sm.marca;

END MAIN
