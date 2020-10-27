################################################################################
#Nombre del Programa => LANZADO RETS278;  LANZADOR RETL278                     #
#Programa            => GENERA UN EXTRACTOR DE LO QUE SE TIENE EN SACI DE      #
#                       RETIROS GENÉRICO. SOLICITA UN RANGO DE FECHA           #
#Fecha creacion      => 20 DE AGOSTO DEL 2014                                  #
#Desarrado por       => FRANCO ULLOA VIDELA                                    #
#Rquerimiento        => PRODINF-275                                            #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
    DEFINE 
       f_solicitud_inicial   DATE ,
       f_solicitud_final     DATE
    
    DEFINE reg_3 RECORD
       ruta_rescate          CHAR(40) ,
       ruta_envio            CHAR(40)
    END RECORD
    
    DEFINE
        v_archivo_salida     CHAR(100)
        
    DEFINE #glo #date
        HOY                  DATE
        
    DEFINE #glo #char
        enter                CHAR(1)
END GLOBALS

MAIN
    DEFINE
       g_pid                 LIKE bat_ctr_proceso.pid     ,-- ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod ,-- codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod ,-- codigo de operacion
       p_usuario_cod         LIKE seg_usuario.usuario_cod ,-- clave del usuario firmado
       v_rest_valida         SMALLINT
       
    --CALL init() #i
    LET p_usuario_cod        = ARG_VAL(1)
    LET g_pid                = ARG_VAL(2)
    LET g_proceso_cod        = ARG_VAL(3)
    LET g_opera_cod          = ARG_VAL(4)
    LET f_solicitud_inicial  = ARG_VAL(5)
    LET f_solicitud_final    = ARG_VAL(6)

    CALL primer_paso()

    CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
    RETURNING v_rest_valida
END MAIN

FUNCTION primer_paso()
#pp-------------------
    DEFINE rec_extractor      RECORD
     id_solicitud        DECIMAL(9,0)  ,
     modalidad_retiro    SMALLINT      ,
     des_modalidad       CHAR(20)      ,
     nss                 CHAR(11)      ,
     rfc                 CHAR(13)      ,
     caso_adai           CHAR(10)      ,
     folio_liquida       DECIMAL(9,0)  ,
     f_solicitud         DATE          ,
     monto_acciones      DECIMAL(16,6) ,
     monto_pesos         DECIMAL(12,2) ,
     estado_solicitud    SMALLINT      ,
     cod_rechazo         SMALLINT      ,
     f_envio_fico        DATE          ,
     f_resp_fico         DATE          ,
     referencia          CHAR(16)      ,
     docto_cta_pagar     CHAR(10)      ,
     f_extraccion        DATE          ,
     f_pago              DATE
    END RECORD
    DEFINE v_hora         CHAR(8)
    DEFINE v_estatus_pago CHAR (2)
    DEFINE v_tabla        CHAR(16);
    DEFINE v_query        STRING;
    DEFINE v_f_pago       CHAR(19)
    SELECT ruta_rescate ,
           ruta_envio
    INTO   reg_3.*
    FROM   seg_modulo
    WHERE  modulo_cod = "dae"
     
    DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_prodinf275;
    WHENEVER ERROR STOP 
     
    CREATE TABLE ret_prodinf275
    (
     id_solicitud        DECIMAL(9,0)  ,
     modalidad_retiro    SMALLINT      ,
     des_modalidad       CHAR(20)      ,
     nss                 CHAR(11)      ,
     rfc                 CHAR(13)      ,
     caso_adai           CHAR(10)      ,
     folio_liquida       DECIMAL(9,0)  ,
     f_solicitud         DATE          ,
     monto_acciones      DECIMAL(16,6) ,
     monto_pesos         DECIMAL(12,2) ,
     estado_solicitud    SMALLINT      ,
     cod_rechazo         SMALLINT      ,
     f_envio_fico        DATE          ,
     f_resp_fico         DATE          ,
     referencia          CHAR(16)      ,
     docto_cta_pagar     CHAR(10)      ,
     f_extraccion        DATE          ,
     f_pago              DATE 
    )
    ;
    
    DISPLAY f_solicitud_inicial , f_solicitud_final
    DATABASE safre_viv

    CREATE TEMP TABLE movtos_amortizacion (
    folio_liquida       DECIMAL(9,0),
    monto_acciones      DECIMAL(16,6),
    monto_pesos         DECIMAL(12,2),
    id_derechohabiente  DECIMAL(9,0),
    subcuenta           SMALLINT,
    movimiento          SMALLINT,
    id_referencia       DECIMAL(9,0)
    );

    DECLARE cur_tablas CURSOR FOR 
    SELECT tabname from systables where tabname matches 'cta_movimiento??'
    LET v_query = ""
    FOREACH cur_tablas INTO v_tabla
       LET v_query = " INSERT INTO movtos_amortizacion ",   
                     "\n SELECT folio_liquida, monto_acciones, monto_pesos, id_derechohabiente, ",
                     "\n        subcuenta, movimiento, id_referencia                            ",
                     "\n FROM   ", v_tabla,
                     "\n WHERE  subcuenta = 46 ",
                     "\n AND    movimiento = 1402 "
       PREPARE prp_inserta_movtos FROM v_query
       --Se ejecuta el query
       EXECUTE prp_inserta_movtos 
    END FOREACH 
    --Se construye la consulta SELECT
    LET v_query = " INSERT INTO movtos_amortizacion ",
                  "\n SELECT folio_liquida, monto_acciones, monto_pesos, id_derechohabiente, ",
                  "\n        subcuenta, movimiento, id_referencia                            ",
                  "\n FROM   cta_movimiento ",
                  "\n WHERE  subcuenta = 46 ",
                  "\n AND    movimiento = 1402 "

    DISPLAY "El Query >", v_query, "<"
    --Se prepara el query
    PREPARE prp_inserta_movtos_2 FROM v_query
    --Se ejecuta el query
    EXECUTE prp_inserta_movtos_2

{    
    INSERT INTO movtos_amortizacion
    SELECT folio_liquida, monto_acciones, monto_pesos, id_derechohabiente, 
                         subcuenta, movimiento, id_referencia
                    FROM cta_movimiento12
                   WHERE subcuenta = 46
                     AND movimiento = 1402;
    INSERT INTO movtos_amortizacion
    SELECT folio_liquida, monto_acciones, monto_pesos, id_derechohabiente, 
                         subcuenta, movimiento, id_referencia
                    FROM cta_movimiento13
                   WHERE subcuenta = 46
                     AND movimiento = 1402;
    INSERT INTO movtos_amortizacion
    SELECT folio_liquida, monto_acciones, monto_pesos, id_derechohabiente, 
                         subcuenta, movimiento, id_referencia
                    FROM cta_movimiento14
                   WHERE subcuenta = 46
                     AND movimiento = 1402;
    INSERT INTO movtos_amortizacion
    SELECT folio_liquida, monto_acciones, monto_pesos, id_derechohabiente, 
                         subcuenta, movimiento, id_referencia
                    FROM cta_movimiento
                   WHERE subcuenta = 46
                     AND movimiento = 1402;
}                     
    -- Datos de ret_ws_consulta_pago_fico
    LET v_query = "SELECT FIRST 1 MAX(h_consulta),rsp_referencia,               "||
                "       rsp_f_pago,rsp_estatus                                "||
                "FROM   ret_ws_consulta_pago_fico rpf                         "||
                "WHERE  f_consulta = (SELECT MAX(f_consulta)                  "||
                "                     FROM   ret_ws_consulta_pago_fico        "||
                "                     WHERE  id_solicitud = rpf.id_solicitud) "||
                "  AND  id_solicitud = ?                                      "||
                " GROUP  BY rsp_referencia,rsp_f_pago,rsp_estatus             "

    PREPARE prp_consulta_pago_fico FROM v_query
    
    DECLARE cur_extractor CURSOR FOR  
    SELECT A.id_solicitud     ,
           A.modalidad_retiro ,
           B.des_corta        ,--Descripción de la modalidad
           A.nss              ,
           A.rfc              ,
           A.caso_adai        ,
           C.folio_liquida    ,
           A.f_solicitud      ,
           C.monto_acciones   ,
           C.monto_pesos      ,
           A.estado_solicitud ,
           A.cod_rechazo      ,
           TODAY
    FROM   ret_solicitud_generico A, 
           ret_modalidad_retiro B,  
           OUTER movtos_amortizacion C
    WHERE  A.modalidad_retiro   = 9 --RETIRO AMORTIZACIONES
    AND    A.modalidad_retiro   = B.modalidad_retiro
    AND    A.id_derechohabiente = C.id_derechohabiente
    AND    A.id_solicitud       = C.id_referencia
    AND    A.f_solicitud  BETWEEN f_solicitud_inicial AND f_solicitud_final
    AND    C.subcuenta          = 46   --AMORTIZACIÓN EXCEDENTE
    AND    C.movimiento         = 1402 --CARGO RETIRO AMORTIZACIÓN EXCEDENTE 

--    DECLARE cur_referencia CURSOR FOR SELECT rsp_referencia
--                                        FROM ret_ws_consulta_pago_fico
--                                       WHERE id_solicitud = ?
--                                       ORDER BY f_consulta DESC, h_consulta DESC

   DECLARE cur_fico CURSOR FOR SELECT cta_x_pagar ,
                                       f_actualiza
                                  FROM ret_respuesta_fico
                                 WHERE referencia = ? 
                                 ORDER BY f_actualiza

    DECLARE cur_f_archivo CURSOR FOR SELECT b.f_actualiza
                                       FROM ret_solicitud_generico a, ret_ctr_archivo_fico b
                                      WHERE a.id_solicitud = ?
                                        AND a.id_archivo_envio = b.id_archivo

    
    FOREACH cur_extractor INTO   rec_extractor.id_solicitud     ,
                                 rec_extractor.modalidad_retiro ,
                                 rec_extractor.des_modalidad    ,
                                 rec_extractor.nss              ,
                                 rec_extractor.rfc              ,
                                 rec_extractor.caso_adai        ,
                                 rec_extractor.folio_liquida    ,
                                 rec_extractor.f_solicitud      ,
                                 rec_extractor.monto_acciones   ,
                                 rec_extractor.monto_pesos      ,
                                 rec_extractor.estado_solicitud ,
                                 rec_extractor.cod_rechazo      ,
                                 rec_extractor.f_extraccion     

      EXECUTE prp_consulta_pago_fico INTO v_hora,rec_extractor.referencia,v_f_pago,v_estatus_pago
                                       USING rec_extractor.id_solicitud

      LET rec_extractor.f_pago = MDY(v_f_pago[5,6],v_f_pago[7,8],v_f_pago[1,4])

                                       
--      FOREACH cur_referencia USING rec_extractor.id_solicitud
--                              INTO rec_extractor.referencia
--         EXIT FOREACH
--      END FOREACH
                                 
      FOREACH cur_fico USING rec_extractor.id_solicitud
                        INTO rec_extractor.docto_cta_pagar  ,
                             rec_extractor.f_resp_fico      
         EXIT FOREACH
      END FOREACH

      FOREACH cur_f_archivo USING rec_extractor.id_solicitud
                             INTO rec_extractor.f_envio_fico
         EXIT FOREACH
      END FOREACH

      IF rec_extractor.f_resp_fico  = "12/31/1899" THEN
         LET rec_extractor.f_resp_fico = ""
      END IF

      IF rec_extractor.f_envio_fico  = "12/31/1899" THEN
         LET rec_extractor.f_envio_fico = ""
      END IF
      
      INSERT INTO safre_tmp:ret_prodinf275 VALUES(rec_extractor.*)
      INITIALIZE rec_extractor.referencia,
                 rec_extractor.docto_cta_pagar,
                 rec_extractor.f_resp_fico,      
                 rec_extractor.f_envio_fico TO NULL
    END FOREACH
    
    LET v_archivo_salida = reg_3.ruta_envio CLIPPED,"/","sol_saci_adai_completo.dae"
    
    UNLOAD TO v_archivo_salida
    SELECT *
    FROM   safre_tmp:ret_prodinf275
    
    DISPLAY "El archivo se generó de manera exitosa en la ruta:"
    DISPLAY reg_3.ruta_envio
    DISPLAY "\n"
    DISPLAY "Con el nombre:","sol_saci_adai_completo.dae"
    DISPLAY "\n"
    DISPLAY "\n"    
END FUNCTION