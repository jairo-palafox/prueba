#################################################################################
#Nombre del Programa => RET494                                                  #
#Descripción         => Programa que manda a consultar todas las solicitudes    #
#                       suceptibles de una respuesta de pago FICO. Se ejecuta el#
#                       WS de consulta de pago FICO                             #
#Fecha creacion      => 09 de Octubre del 2014                                  #
#Por                 => Franco Ulloa Videla                                     #
#################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS "consultaPago.dir/ret_fico_consultaPago.inc" -- consulta de pago a FICO

GLOBALS   
    DEFINE reg_3 RECORD #glo #reg_3
       ruta_rescate           CHAR(40) ,
       ruta_envio             CHAR(40)
    END RECORD
    
    DEFINE #glo #smalling
        v_ws_status           SMALLINT                                 -- estatus de ejecucion de un webservice
        
    DEFINE #glo #char
        v_archivo_salida      CHAR(100) ,
        enter                 CHAR(1)

    DEFINE
        v_cadena                      STRING                                 , -- cadena auxiliar
        v_estatus_fico                SMALLINT                               , -- estatus de pago en fico en formato numerico
        v_r_ret_respuesta_fico        RECORD LIKE ret_respuesta_fico.*       ,
        v_cambio_cuenta               SMALLINT                               , -- booleana para ver si hubo cambio dee stado de la cuenta
        v_proceso_cod                 LIKE cat_proceso.proceso_cod           , -- codigo de proceso
        v_marca                       LIKE sfr_marca.marca                   , -- marca del proceso
        v_r_ret_ws_consulta_pago_fico RECORD LIKE ret_ws_consulta_pago_fico.* -- registro de la respuesta de FICO
END GLOBALS

MAIN

    --CALL primer_paso()  #pp Carga tabla temporal de trabajo
    --CALL segundo_paso() #sp
    CALL tercer_paso()  #tp Ejecución del WS de consulta pago FICO
    --CALL quinto_paso()  #qp Genera el archivo con los casos que no se tiene respuesta de FICO estado 18
    --CALL sexto_paso()   #xp Busca los numeros de credito
    CALL cuarto_paso()  #cp Salida a archivo plano
END MAIN

FUNCTION primer_paso()
#pp-------------------    
    DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_prodinf497;
    WHENEVER ERROR STOP 
    DISPLAY "En el primer paso, se crea la tabla temporal"
    DISPLAY "         Se crea la tabla temporal ret_prodinf497"  
    CREATE TABLE ret_prodinf497
    (
     id_solicitud            DECIMAL(9,0)  ,
     id_derechohabiente      DECIMAL(9,0)  ,
     modalidad_retiro        SMALLINT      ,
     des_modalidad           CHAR(20)      ,
     nss                     CHAR(11)      ,
     caso_adai               CHAR(10)      ,
     folio_liquida           DECIMAL(9,0)  ,
     f_solicitud             DATE          ,
     monto_acciones          DECIMAL(16,6) ,
     monto_pesos             DECIMAL(12,2) ,
     estado_solicitud        SMALLINT      ,
     cod_rechazo             SMALLINT      ,
     estado_solicitud_paso   SMALLINT      ,
     estado_fico             SMALLINT      ,
     estado_llamado_ws       SMALLINT      ,
     rsp_referencia          CHAR(16)      ,
     rsp_referencia_new      CHAR(16)      ,
     rsp_importe             CHAR(16)      ,
     rsp_f_pago              CHAR(10)      ,
     num_credito             CHAR(10)      ,
     num_respuesta           SMALLINT      ,
     archivo_f_archivo_fico  DATE          ,
     archivo_cta_x_pagar     CHAR(10)      ,
     archivo_anho            CHAR(4)       ,
     archivo_sociedad        CHAR(4)       ,
     archivo_bandera         CHAR(2)       ,
     f_extraccion            DATE     
    )
    ;
    
    DATABASE safre_viv


    DISPLAY "         Se cargan los datos a la tabla temporal ret_prodinf497"  

    INSERT INTO safre_tmp:ret_prodinf497
    SELECT A.id_solicitud       ,
           A.id_derechohabiente ,
           A.modalidad_retiro   ,
           B.des_corta          ,--Descripción de la modalidad
           A.nss                ,
           A.caso_adai          ,
           C.folio_liquida      ,
           A.f_solicitud        ,
           C.monto_acciones     ,
           C.monto_pesos        ,
           A.estado_solicitud   ,
           A.cod_rechazo        ,
           0                    ,--estado_solicitud_paso
           0                    ,--estado_fico
           18                   ,--estado_llamado_ws
           ""                   ,--rsp_referencia
           ""                   ,--rsp_referencia_new
           "0"                  ,--rsp_importe
           ""                   ,--rsp_f_pago
           ""                   ,--num_credito
           0                    ,--num_respuesta
           ""                   ,--archivo_f_archivo_fico
           ""                   ,--archivo_cta_x_pagar
           ""                   ,--archivo_anho
           ""                   ,--archivo_sociedad
           ""                   ,--archivo_bandera
           TODAY                
    FROM   ret_solicitud_generico A, ret_modalidad_retiro B, OUTER cta_movimiento C
    WHERE  A.modalidad_retiro   = 9 --RETIRO AMORTIZACIONES
    AND   (A.estado_solicitud  IN (70,71,72)
    OR    (A.estado_solicitud  IN (90,209,210,211,212,214) AND cod_rechazo = 65))
    AND    A.modalidad_retiro   = B.modalidad_retiro
    AND    A.id_derechohabiente = C.id_derechohabiente
    AND    A.id_solicitud       = C.id_referencia
    AND    C.subcuenta          = 46   --AMORTIZACIÓN EXCEDENTE
    AND    C.movimiento         = 1402 --CARGO RETIRO AMORTIZACIÓN EXCEDENTE 
     
END FUNCTION

FUNCTION segundo_paso()
#sp--------------------
    DEFINE reg_1 RECORD #loc #reg_1
       id_solicitud           DECIMAL(9,0) ,
       f_archivo_fico         DATE         ,
       cta_x_pagar            CHAR(10)     ,
       anho                   CHAR(4)      ,
       sociedad               CHAR(4)      ,
       bandera                CHAR(2)      ,
       f_consulta             DATE         ,
       rsp_referencia         CHAR(16)
    END RECORD
    
    DISPLAY "En el segundo paso, se obtienen los datos de la respuesta del archivo FICO"
    DISPLAY "         Se obtiene informacion y se actualiza en la temporal"  

    DECLARE cur_1 CURSOR FOR
    SELECT *
    FROM   safre_tmp:ret_prodinf497
      
    FOREACH cur_1 INTO reg_1.id_solicitud
        LET reg_1.f_archivo_fico = "01/01/0001"
        LET reg_1.cta_x_pagar    = NULL
        LET reg_1.anho           = NULL
        LET reg_1.sociedad       = NULL
        LET reg_1.bandera        = NULL
                
        SELECT MAX(f_actualiza)
        INTO   reg_1.f_archivo_fico
        FROM   ret_respuesta_fico
        WHERE  referencia = reg_1.id_solicitud
        
        SELECT cta_x_pagar       ,
               anho              ,
               sociedad          ,
               bandera
        INTO   reg_1.cta_x_pagar ,
               reg_1.anho        ,
               reg_1.sociedad    ,
               reg_1.bandera
        FROM   ret_respuesta_fico
        WHERE  referencia  = reg_1.id_solicitud
        AND    f_actualiza = reg_1.f_archivo_fico
        
        IF STATUS <> NOTFOUND THEN
            LET reg_1.f_consulta = "01/01/0001"
            
            SELECT MAX(f_consulta)
            INTO   reg_1.f_consulta
            FROM   ret_ws_consulta_pago_fico
            WHERE  id_solicitud = reg_1.id_solicitud

            LET reg_1.rsp_referencia = ""
            
            SELECT rsp_referencia
            INTO   reg_1.rsp_referencia
            FROM   ret_ws_consulta_pago_fico
            WHERE  id_solicitud = reg_1.id_solicitud
            AND    f_consulta   = reg_1.f_consulta
                        
            UPDATE safre_tmp:ret_prodinf497
            SET    archivo_f_archivo_fico = reg_1.f_archivo_fico ,
                   archivo_cta_x_pagar    = reg_1.cta_x_pagar    ,
                   archivo_anho           = reg_1.anho           ,
                   archivo_sociedad       = reg_1.sociedad       ,
                   archivo_bandera        = reg_1.bandera        ,
                   rsp_referencia         = reg_1.rsp_referencia , 
                   estado_solicitud_paso  = 700
            WHERE  id_solicitud = reg_1.id_solicitud
        END IF
    END FOREACH
END FUNCTION


FUNCTION tercer_paso()
#tp------------------
    DEFINE reg_4 RECORD
        id_solicitud            DECIMAL(9,0)  ,
        id_derechohabiente      DECIMAL(9,0)  ,
        nss                     CHAR(11)      ,
        caso_adai               CHAR(10)      ,        
        f_solicitud             DATE          ,      
        estado_solicitud        SMALLINT      ,
        estado_fico             SMALLINT      ,
        archivo_f_archivo_fico  DATE          ,
        archivo_cta_x_pagar     CHAR(10)      ,
        archivo_anho            CHAR(4)       ,
        archivo_sociedad        CHAR(4)       ,
        archivo_bandera         CHAR(2)
    END RECORD

    DEFINE #loc 
        v_rsp_referencia_new    CHAR(16) ,
        v_rsp_importe           CHAR(16) ,
        v_rsp_f_pago            CHAR(10) ,
        v_contador              SMALLINT -- contador de registros

    DISPLAY "En el tercer paso, se consulta el estado via WS"
    DISPLAY "         Se seleccionan solo las solicitudes con estado_solicitud = 700"  

        
    DECLARE cur_2 CURSOR FOR
    SELECT id_solicitud           ,
           id_derechohabiente     ,
           nss                    ,
           caso_adai              ,
           f_solicitud            ,
           estado_solicitud       ,
           estado_fico            ,
           archivo_f_archivo_fico ,
           archivo_cta_x_pagar    ,
           archivo_anho           ,
           archivo_sociedad       ,
           archivo_bandera 
    FROM   safre_tmp:ret_prodinf497
    WHERE  estado_solicitud_paso = 700
    
    FOREACH cur_2 INTO reg_4.*
        -- se consulta si ya se efectuo el pago o se rechazo
        LET consultar.entrada.documento = reg_4.archivo_cta_x_pagar
        LET consultar.entrada.ejercicio = reg_4.archivo_anho
        LET consultar.entrada.sociedad  = reg_4.archivo_sociedad

        -- se invoca la consulta de pago a FICO
        CALL consultar_g() RETURNING v_ws_status
         
        --si el webservice se ejecuto correctamente
         IF ( v_ws_status = 0 ) THEN      
           -- DISPLAY "CONSULTA DE PAGO WS ejecutada correctamente"
         
            FOR v_contador = 1 TO consultarResponse.salida.getLength()
              -- DISPLAY "documento         : ", consultarResponse.salida[v_contador].documento
              -- DISPLAY "ejercicio         : ", consultarResponse.salida[v_contador].ejercicio
              -- DISPLAY "estatus           : '", consultarResponse.salida[v_contador].estatus, "'"
              -- DISPLAY "importe           : ", consultarResponse.salida[v_contador].importe
              -- DISPLAY "indicadorRetencion: ", consultarResponse.salida[v_contador].indicadorRetencion
              -- DISPLAY "referencia        : ", consultarResponse.salida[v_contador].referencia
              -- DISPLAY "fechaPago         : ", consultarResponse.salida[v_contador].fechaPago
            
            
               --Si ya se pago, se actualiza la solicitud--
               LET v_cadena = consultarResponse.salida[v_contador].estatus
               -- se transforma el estatus a numerico
               LET v_estatus_fico = v_cadena.trim()

               -- DISPLAY "Estatus FICO [numerico]: ", v_estatus_fico

               -- 26 oct 2013. FICO dice que en rechazos no viene la fecha, por tanto
               -- se usara la fecha del dia
               IF ( consultarResponse.salida[v_contador].fechaPago IS NULL ) THEN
                  LET consultarResponse.salida[v_contador].fechaPago = TODAY USING "yyyymmdd"
                  LET v_rsp_f_pago = "00010101"
               ELSE 
                  LET v_rsp_f_pago = consultarResponse.salida[v_contador].fechaPago
               END IF
               LET v_rsp_importe = consultarResponse.salida[v_contador].importe
               
               LET v_rsp_referencia_new = consultarResponse.salida[v_contador].referencia
               
               UPDATE safre_tmp:ret_prodinf497
               SET    estado_fico        = v_estatus_fico       ,
                      estado_llamado_ws  = v_ws_status          ,
                      rsp_referencia_new = v_rsp_referencia_new ,
                      rsp_importe        = v_rsp_importe        ,
                      rsp_f_pago         = v_rsp_f_pago         ,
                      num_respuesta      = v_contador
               WHERE  id_solicitud          = reg_4.id_solicitud
               AND    estado_solicitud_paso = 700
            END FOR
         ELSE
            UPDATE safre_tmp:ret_prodinf497
            SET    estado_fico       = 0 ,
                   estado_llamado_ws = v_ws_status
            WHERE  id_solicitud          = reg_4.id_solicitud
            AND    estado_solicitud_paso = 700
         END IF
    END FOREACH
END FUNCTION

FUNCTION cuarto_paso()
#cp-------------------
    LET v_archivo_salida = "SALIDA497.unl"

    DISPLAY "En el cuarto paso, se genera el archivo de salida"
    DISPLAY "         Manda a archivo plano la informacion de la tabla ret_prodinf497"  

           
    UNLOAD TO v_archivo_salida
    SELECT id_solicitud           ,
           id_derechohabiente     ,
           nss                    ,
           caso_adai              ,
           f_solicitud            ,
           estado_solicitud       ,
           estado_fico            ,
           estado_llamado_ws      ,
           rsp_referencia         ,
           rsp_referencia_new     ,
           rsp_importe            ,
           rsp_f_pago             ,
           num_credito            ,
           num_respuesta          ,
           archivo_f_archivo_fico ,
           archivo_cta_x_pagar    ,
           archivo_anho           ,
           archivo_sociedad       ,
           archivo_bandera
    FROM   safre_tmp:ret_prodinf497
END FUNCTION

FUNCTION quinto_paso()
#qp-------------------
    LET v_archivo_salida = "SALIDA_CASOS_18.unl"

    DISPLAY "En el quinto paso, se genera el archivo de salida de los que se encuentran con estado 18"
    DISPLAY "         Manda a archivo plano la consulta de los registros con estado 18 y la fecha en la que se solicitaron"  

           
    UNLOAD TO v_archivo_salida
    SELECT rsg.id_solicitud, rsg.nss,
           rcaf.f_actualiza,
           rcaf.id_archivo,
           rp.id_solicitud
      FROM ret_solicitud_generico rsg, ret_ctr_archivo_fico rcaf,
           safre_tmp:ret_prodinf497 rp
     WHERE rp.id_solicitud = rsg.id_solicitud
       AND rp.estado_llamado_ws = 18
       AND rsg.id_archivo_envio = rcaf.id_archivo

END FUNCTION

FUNCTION sexto_paso()
#xp--------------------
    DEFINE reg_6 RECORD #loc #reg_1
       id_derechohabiente     DECIMAL(9,0),
       id_solicitud           DECIMAL(9,0),
       num_credito            CHAR(10)
    END RECORD
    
    DISPLAY "En el sexto paso, se obtiene el numero de credito "

    DECLARE cur_6 CURSOR FOR
    SELECT id_derechohabiente, id_solicitud
      FROM safre_tmp:ret_prodinf497
     
    FOREACH cur_6 INTO reg_6.id_derechohabiente, reg_6.id_solicitud

        SELECT DISTINCT num_credito
        INTO   reg_6.num_credito
        FROM   dae_det_solicitud
        WHERE  id_derechohabiente = reg_6.id_derechohabiente
        AND    num_credito <> '0000000000'        
        IF STATUS <> NOTFOUND THEN
                        
            UPDATE safre_tmp:ret_prodinf497
            SET    num_credito  = reg_6.num_credito
            WHERE  id_solicitud = reg_6.id_solicitud
        END IF

    END FOREACH
END FUNCTION
