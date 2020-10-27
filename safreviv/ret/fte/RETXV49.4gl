#################################################################################
#Nombre del Programa => RETXV49                                                 #
#Descripción         => Cambia el estado de la solicitud del trabajador a 100,  #
#                       desmarca la cuenta y restituir                          #
#Fecha creacion      => 24 de Septiembre del 2015                               #
#Por                 => Luis Felipe Prieto Cano                                 #
#Requerimiento       => PRODINFXV-49                                            #
#################################################################################
DATABASE safre_viv 
GLOBALS
    DEFINE #DATE
        HOY                  DATE
               
    DEFINE #CHAR
        enter                CHAR(1)
    
    DEFINE #DECIMAL          
        v_precio_fondo       LIKE glo_valor_fondo.precio_fondo
END GLOBALS

MAIN
    CALL init()         --Se inicializan variables
    CALL primer_paso()  --Restituye la cuenta
    CALL segundo_paso() --Cambia el estado de la solicitud de retiro a rechazada  Desmarca las cuentas
    CALL tercer_paso()  --Desmarca las cuentas
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
    DEFINE #reg_2 #loc
        reg_1                RECORD LIKE cta_movimiento.*
           
    INSERT INTO glo_folio
    VALUES(seq_glo_folio.nextval  ,--folio              decimal(9,0) not null
           1530                   ,--proceso_cod        smallint             --RETIRO DE AMORTIZACIONES EXCEDENTES
           2                      ,--opera_cod          smallint             --LIQUIDACION
           2                      ,--status             smallint
           ""                     ,--folio_referencia   decimal(9,0)
           TODAY                  ,--f_actualiza        date
           "safreviv"              --usuario            char(20)
           );

    --Movimientos 2015
    DECLARE cur_2015 CURSOR FOR
    SELECT *
    FROM   cta_movimiento
    WHERE  id_derechohabiente IN(32192932)
    AND    movimiento     = 1402
    AND    subcuenta      = 46
    ORDER BY id_derechohabiente
    
    FOREACH cur_2015 INTO reg_1.*
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
    FREE cur_2015

    --Movimientos 2014
    DECLARE cur_2014 CURSOR FOR
    SELECT *
    FROM   cta_movimiento14
    WHERE  id_derechohabiente IN(13770452,30930406)
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
---------------------
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud in(7155348,7353992,7400247,7268731,7351434,
                             7279446,7402893,7353992,7109769,5349574,5085937,7656407,5088679,
                             7579838,7546293,7662304,7545030,7657089,7686516,7566627)
      
      UPDATE ret_amort_excedente
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud in(7155348,7353992,7400247,7268731,7351434,
                             7279446,7402893,7353992,7109769,5349574,5085937,7656407,5088679,
                             7579838,7546293,7662304,7545030,7657089,7686516,7566627)
END FUNCTION

FUNCTION tercer_paso()
#---------------------
    DEFINE #char #loc
        v_sql                CHAR(200)
    
    DEFINE #smallint #loc
        v_resultado          SMALLINT 
        
    DEFINE #reg_2 #loc
        reg_2                RECORD LIKE sfr_marca_activa.*
        
    DECLARE cur_2 CURSOR FOR
    SELECT *
    FROM   sfr_marca_activa
    WHERE  n_referencia in(7155348,7353992,7400247,7268731,7351434,
                           7279446,7402893,7353992,7109769,5349574,5085937,7656407,5088679,
                           7579838,7546293,7662304,7545030,7657089,7686516,7566627)
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
