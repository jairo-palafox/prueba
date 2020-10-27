#################################################################################
#Nombre del Programa => CARINFXVI35                                             #
#Descripción         => Cambia el estado de la solicitud del trabajador a 100,  #
#                       desmarca la cuenta y restituye si es que aplica         #
#Fecha creacion      => 28 de Diciembre de 2016                                 #
#Por                 => Ricardo Pérez Ramírez                                   #
#Requerimiento       => CARINFXVI-35                                            #
#################################################################################
DATABASE safre_viv 
GLOBALS
   DEFINE
      HOY                  DATE,
      enter                CHAR(1),
      v_precio_fondo       LIKE glo_valor_fondo.precio_fondo
END GLOBALS

MAIN
    CALL init()         --Se inicializan variables
    CALL primer_paso()  --Restituye la cuenta
--    CALL segundo_paso() --Cambia el estado de la solicitud de retiro a rechazada  Desmarca las cuentas
--    CALL tercer_paso()  --Desmarca las cuentas
    DISPLAY "Se ejecuto el proceso correctamente."
END MAIN

FUNCTION init()
--------------
    LET HOY = TODAY
    
    SELECT precio_fondo
    INTO   v_precio_fondo
    FROM   glo_valor_fondo
    WHERE  fondo       = 11
    AND    f_valuacion = HOY
END FUNCTION

FUNCTION primer_paso()
----------------------
    --Restitución de solicitudes
    ----------------------
    DEFINE #reg_2 #loc
        reg_1                RECORD LIKE cta_movimiento.*
           
    INSERT INTO glo_folio
    VALUES(seq_glo_folio.nextval  ,--folio              decimal(9,0) not null
           1540                   ,--proceso_cod        smallint             --RETIRO DE AMORTIZACIONES EXCEDENTES
           2                      ,--opera_cod          smallint             --LIQUIDACION
           2                      ,--status             smallint
           ""                     ,--folio_referencia   decimal(9,0)
           TODAY                  ,--f_actualiza        date
           "safreviv"              --usuario            char(20)
           );

    --Movimientos 2014
    DECLARE cur_2014 CURSOR FOR
    SELECT *
    FROM   cta_movimiento14
    WHERE  id_derechohabiente IN(28797016,43793629,4381129,15628640,21969696)
    AND    movimiento     = 1402
    AND    subcuenta      = 46
    ORDER BY id_derechohabiente
    
    FOREACH cur_2014 INTO reg_1.*
        LET reg_1.monto_pesos = reg_1.monto_acciones*v_precio_fondo
        
        INSERT INTO cta_movimiento
        VALUES (HOY                      ,--f_liquida          date not null
                reg_1.id_derechohabiente ,--id_derechohabiente decimal(9,0) not null
                46                       ,--subcuenta          smallint not null
                11                       ,--fondo_inversion    smallint not null
                251                      ,--movimiento         smallint not null
                seq_glo_folio.currval    ,--folio_liquida      decimal(9,0) not null
                reg_1.id_referencia      ,--id_referencia      decimal(9,0) not null
                -reg_1.monto_acciones    ,--monto_acciones     decimal(16,6)
                -reg_1.monto_pesos       ,--monto_pesos        decimal(12,2)
                HOY                      ,--f_valor            date
                HOY                      ,--f_registro         date
                "18:00:00"               ,--h_registro         datetime hour to second
                "RESTITUCION"             --origen             char(20)
               )
    END FOREACH
    FREE cur_2014
END FUNCTION

FUNCTION segundo_paso()
DEFINE v_solicitud DECIMAL(9,0)
---------------------
   LET v_solicitud = 0
    --Cambio de estado y codigo de rechazo para las solicitudes
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud IN (v_solicitud)
      
      UPDATE ret_amort_excedente
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud IN (v_solicitud)
END FUNCTION

FUNCTION tercer_paso()
#---------------------
DEFINE v_solicitud DECIMAL(9,0)
---------------------
   
    --Desmarcar las cuentas
    DEFINE
        v_sql                CHAR(200)
    
    DEFINE
        v_resultado          SMALLINT 
        
    DEFINE
        reg_2                RECORD LIKE sfr_marca_activa.*
    LET v_solicitud = 0    
    DECLARE cur_2 CURSOR FOR
    SELECT *
    FROM   sfr_marca_activa
    WHERE  n_referencia IN   (v_solicitud)
    AND    marca         = 810

        
    FOREACH cur_2 INTO reg_2.*
        LET v_sql = "\nEXECUTE FUNCTION fn_desmarca_cuenta(","\n",reg_2.id_derechohabiente, ",",
                                                             "\n",reg_2.marca, ",",
                                                             "\n",reg_2.n_referencia,",",
                                                             "\n 0,",
                                                             "\n",reg_2.marca_causa,",",
                                                             "\n",'"SAFREVIV"',",",
                                                             "\n",reg_2.proceso_marca,")"
        PREPARE v_desmarca FROM v_sql
        EXECUTE v_desmarca INTO v_resultado                
    END FOREACH
END FUNCTION
