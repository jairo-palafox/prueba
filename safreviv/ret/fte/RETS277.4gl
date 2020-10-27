################################################################################
#Nombre del Programa => RETS277 (RET423)  PRODINF-423                          #
#Programa            => PROGRAMA LANZADO POR EL RETL277                        #
#                       GENERA UN EXTRACTOR DE LOS TRABAJADORES PENDIENTES DE  #
#                       PAGO                                                   #
#Fecha creacion      => 25 DE SEPTIEMBRE DEL 2014                              #
#Desarrado por       => FRANCO ESTEBAN ULLOA VIDELA                            #
################################################################################
--IMPORT FGL WSHelper
--IMPORT com
DATABASE safre_viv

GLOBALS "RETG01.4gl"
GLOBALS   
    DEFINE
       f_solicitud_inicial   DATE ,
       f_solicitud_final     DATE
  
    DEFINE reg_2 RECORD
       nss                   CHAR(11)     ,
       id_derechohabiente    DECIMAL(9,0) ,
       monto_acciones        DECIMAL(16,6)
    END RECORD  
    
    DEFINE reg_3 RECORD
       ruta_rescate           CHAR(40) ,
       ruta_envio             CHAR(40)
    END RECORD
    
    DEFINE
        v_archivo_salida      CHAR(100)
        
    DEFINE #glo #char
        enter                 CHAR(1)
END GLOBALS

MAIN
    DEFINE
        g_pid                 LIKE bat_ctr_proceso.pid     ,--Id del proceso
        g_proceso_cod         LIKE cat_proceso.proceso_cod ,--codigo del proceso
        g_opera_cod           LIKE cat_operacion.opera_cod ,--codigo de operacion 
        p_usuario_cod         LIKE seg_usuario.usuario_cod ,--clave del usuario firmado
        v_rest_valida         SMALLINT

    LET p_usuario_cod        = ARG_VAL(1)
    LET g_pid                = ARG_VAL(2)
    LET g_proceso_cod        = ARG_VAL(3)
    LET g_opera_cod          = ARG_VAL(4)
    LET f_solicitud_inicial  = ARG_VAL(5)
    LET f_solicitud_final    = ARG_VAL(6)
    
    DISPLAY "Obteniendo cuentas pendientes de pago para Amortizaciones Excedentes ..."
    --DISPLAY "Con las Fechas en el llamado a la funcion inicio >", f_solicitud_inicial, "< Final >", f_solicitud_final
    CALL primer_paso()  #pp Carga tabla de trabajo
    DISPLAY "Obteniendo detalle de solicitudes de Amortizaciones Excedentes ..."
    CALL segundo_paso() #sp Inserta todos los retiros de amortizaciones realizados
    DISPLAY "Finaliza Operacion"
    CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
    RETURNING v_rest_valida
END MAIN

FUNCTION primer_paso()
#pp-------------------  

   WHENEVER ERROR CONTINUE
    DROP TABLE tmp_saldo_amort
   WHENEVER ERROR STOP

   PREPARE eje_pdq_high FROM "SET PDQPRIORITY HIGH"
   EXECUTE eje_pdq_high

   --Se midifica esta seccion para reportar la informacion directamente de la tabla de Solicitudes
   
--    SELECT id_derechohabiente, SUM(monto_acciones) saldo
--      FROM cta_movimiento
--     WHERE subcuenta = 46
--     GROUP BY 1
--     HAVING SUM(monto_acciones) > 0
--     INTO TEMP tmp_saldo_amort

--     CREATE INDEX tmp_saldo_amort1 ON tmp_saldo_amort (id_derechohabiente)

--     UPDATE STATISTICS FOR TABLE tmp_saldo_amort
    --DISPLAY "Con las Fechas dentro de la funcion inicio >", f_solicitud_inicial, "< Final >", f_solicitud_final
    IF f_solicitud_final IS NULL THEN 
        LET f_solicitud_final = TODAY 
    END IF     
    INSERT INTO safre_tmp:ret_prodinf423
--    SELECT dae.id_dae_referencia  ,
--           afi.nss                ,
--           tmp.id_derechohabiente ,
--           dae.num_credito        ,
--           dae.fecha_pago         ,
--           dae.periodo_pago       ,
--           dae.registro_pago      ,
--           dae.origen             ,
--           dae.delegacion         ,
--           dae.importe_amort      ,
--           dae.total_importe      ,
--           dae.tipo_pago          ,
--           dae.entidad_receptora  ,
--           dae.folio_liquida      ,
--           dae.fecha_liquida      ,--Fecha ingreso cuenta
--           tmp.saldo                     --monto_acciones
--    FROM   tmp_saldo_amort AS tmp
--    INNER JOIN afi_derechohabiente AS afi
--          ON (afi.id_derechohabiente = tmp.id_derechohabiente)
--    INNER JOIN dae_det_solicitud AS dae
--          ON (dae.id_derechohabiente = afi.id_derechohabiente) 
--    WHERE fecha_liquida BETWEEN f_solicitud_inicial AND f_solicitud_final
--      AND dae.resul_opera = "01" 
   SELECT dae.id_dae_referencia  ,
           afi.nss                ,
           dae.id_derechohabiente ,
           dae.num_credito        ,
           dae.fecha_pago         ,
           dae.periodo_pago       ,
           dae.registro_pago      ,
           dae.origen             ,
           dae.delegacion         ,
           dae.importe_amort      ,
           dae.total_importe      ,
           dae.tipo_pago          ,
           dae.entidad_receptora  ,
           dae.folio_liquida      ,
           dae.fecha_liquida      ,  --Fecha ingreso cuenta
           0 AS Saldo
    FROM   dae_det_solicitud AS dae
    INNER JOIN afi_derechohabiente AS afi
          ON (afi.id_derechohabiente = dae.id_derechohabiente)
    WHERE fecha_liquida BETWEEN f_solicitud_inicial AND f_solicitud_final
      AND dae.resul_opera = "01" 
      AND dae.status_retiro in (1,3)

    DISPLAY "Obteniendo movimientos de retiro y restituciones..."
    
--    INSERT INTO safre_tmp:ret_prodinf423
--        (nss,id_derechohabiente,folio_liquida,fecha_liquida,monto_acciones,importe_amort)
--    SELECT 
--           B.nss                ,
--           A.id_derechohabiente ,
--           A.folio_liquida      ,
--           A.f_liquida          ,
--           A.monto_acciones     ,
--           A.monto_pesos
--    FROM   safre_tmp:ret_prodinf423 AS ret
--    INNER  JOIN cta_movimiento AS A 
--           ON ( A.id_derechohabiente = ret.id_derechohabiente )
--    INNER  JOIN afi_derechohabiente AS  B 
--           ON ( B.id_derechohabiente = A.id_derechohabiente )
--    WHERE  A.f_liquida BETWEEN f_solicitud_inicial AND f_solicitud_final
--    AND    A.subcuenta          = 46
--    AND    A.movimiento         IN (1402,521)
    

   PREPARE eje_pdq_low FROM "SET PDQPRIORITY LOW"
   EXECUTE eje_pdq_low
   
    DATABASE safre_tmp
     UPDATE STATISTICS FOR TABLE ret_prodinf423
    DATABASE safre_viv

END FUNCTION

FUNCTION segundo_paso()
#cp------------------
    SELECT ruta_rescate ,
           ruta_envio
    INTO   reg_3.*
    FROM   seg_modulo
    WHERE  modulo_cod = "dae"
    
    LET v_archivo_salida = reg_3.ruta_envio CLIPPED,"/","sdos_pend_amo_excedentes.sdae"
    
    UNLOAD TO v_archivo_salida
    SELECT num_credito        ,
           fecha_pago         ,
           periodo_pago       ,
           registro_pago      ,
           origen             ,
           delegacion         ,
           importe_amort      ,
           total_importe      ,
           tipo_pago          ,
           nss                ,
           entidad_receptora  ,
           folio_liquida      ,
           fecha_liquida      ,--Fecha ingreso cuenta
           monto_acciones
    FROM   safre_tmp:ret_prodinf423
    ORDER BY nss,fecha_liquida, fecha_pago
    
    DISPLAY "El archivo se generó de manera exitosa en la ruta:"
    DISPLAY reg_3.ruta_envio
    DISPLAY "\n"
    DISPLAY "Con el nombre:","sdos_pend_amo_excedentes.sdae"
    DISPLAY "\n"
    DISPLAY "\n"
END FUNCTION
