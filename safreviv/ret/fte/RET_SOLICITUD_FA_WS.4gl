DATABASE safre_viv
-- cliente de prueba para generar solicitud de retiro fondo ahorro
GLOBALS "RETX03.inc"

MAIN
  -- datos de entrada
  DEFINE v_nss               CHAR(11)    -- nss
  DEFINE v_rfc               CHAR(13)    -- rfc
  DEFINE v_causal_ref        SMALLINT    -- causal_referencia 
  DEFINE v_id_beneficiario   SMALLINT    -- Identificador de beneficiario (si aplica)
  DEFINE v_nombre_benef      CHAR(18)    -- Nombre del beneficiario 
  DEFINE v_ap_paterno_benef  CHAR(18)    -- Apellido paterno 
  DEFINE v_ap_materno_benef  CHAR(18)    -- Apellido materno
  DEFINE v_ent_federativa    SMALLINT    -- Entidad federativa 
  DEFINE v_causal_retiro     SMALLINT    -- Causal de retiro

  -- datos de respuesta
  DEFINE r_estado_solicitud INTEGER
  DEFINE r_cod_rechazo      INTEGER
  DEFINE wsstatus           INTEGER
  DEFINE v_ref_ban          CHAR(18)
  DEFINE r_nss              CHAR(18)
  DEFINE v_importe7292      DECIMAL(12,2)
  DEFINE v_error            STRING 
  DEFINE v_cadena           STRING
  DEFINE v_des_larga        VARCHAR(255)
  
  IF num_args()==1 THEN
     LET retiro7292_retiro7292PortTypeLocation = arg_val(1)
  END IF
  
  CLOSE WINDOW SCREEN

  -- se abre la ventana
  OPEN WINDOW w1 WITH FORM "RET_SOLICITUD_FA_WS01"

  -- se llenan los datos del beneficiaro para no perder tiempo en eso
  LET v_id_beneficiario  = 25
  LET v_nombre_benef     = "Nombre"
  LET v_ap_paterno_benef = "Apellido Paterno"
  LET v_ap_materno_benef = "Apellido Materno"
  LET v_ent_federativa   = 16

  INPUT BY NAME 
     v_nss             ,
     v_rfc             ,
     v_causal_ref      ,
     v_id_beneficiario ,
     v_nombre_benef    ,
     v_ap_paterno_benef,
     v_ap_materno_benef,
     v_ent_federativa  ,
     v_causal_retiro   
  ATTRIBUTE (UNBUFFERED, WITHOUT DEFAULTS)

    
     ON ACTION Accept
        CALL fn_retiro(v_nss             ,
                       v_rfc             ,
                       v_causal_ref      ,
                       v_id_beneficiario ,
                       v_nombre_benef    ,
                       v_ap_paterno_benef,
                       v_ap_materno_benef,
                       v_ent_federativa  ,
                       v_causal_retiro   ) 
              RETURNING wsstatus     , 
                        r_nss        , 
                        r_estado_solicitud     , 
                        r_cod_rechazo    ,
                        v_importe7292,
                        v_ref_ban
                        
        IF ( wsstatus = 0 ) THEN
           DISPLAY BY NAME r_estado_solicitud,
                           r_cod_rechazo,
                           v_importe7292,
                           v_ref_ban

           DISPLAY "Consulta ejecutada correctamente" TO msg

           -- se verifica si la solicitud se acepto
           IF ( r_estado_solicitud = 10 ) THEN
              LET v_cadena = "CORRECTO. Solicitud aprobada"
           ELSE
              -- solicitud rechazada
              LET v_cadena = "ERROR. "

              -- se obtiene la descripcion del rechazo
              SELECT des_larga
              INTO   v_des_larga
              FROM   ret_rechazo
              WHERE  cod_rechazo = r_cod_rechazo

              IF ( v_des_larga IS NOT NULL ) THEN
                 LET v_cadena = v_cadena, v_des_larga CLIPPED
              ELSE
                 --LET v_cadena = v_cadena, "Codigo no registrado"


                 -- se verifica el codigo de rechazo
                 CASE r_cod_rechazo

                    -- se deben capturar todos los datos
                    WHEN 91
                       LET v_cadena = v_cadena, "No tiene resolución en el SPESS"
                 
                    -- se deben capturar todos los datos
                    WHEN 99
                       LET v_cadena = v_cadena, "Se deben capturar todos los datos de la solicitud. Capture NSS y/o RFC"

                    -- el NSS es necesario para el proceso que se esta validando, no puede estar vacio o ser "00000000000"
                    WHEN 98
                       LET v_cadena = v_cadena, "El NSS es necesario y no puede ser '00000000000'"
                       
                    -- el NSS no existe
                    WHEN 999
                       LET v_cadena = v_cadena, "El NSS/RFC no se encuentra en la base de datos"
                 
                    -- edad insuficiente para termino de relacion laboral   
                    WHEN 40
                       LET v_cadena = v_cadena, "La edad del trabajador es inferior a 50 años cumplidos"
                 
                    -- ya se tiene una solicitud en tramite
                    WHEN 30
                       LET v_cadena = v_cadena, "El derechohabiente ya tiene una solicitud en trámite"
                 
                 
                    -- causal de retiro invalido
                    WHEN 77
                       LET v_cadena = v_cadena, "El causal de retiro es invalido"
                 
                    -- derechohabiente sin saldo
                    WHEN 10
                       LET v_cadena = v_cadena, "El derechohabiente no tiene saldo de Fondo de Ahorro"
                 
                    -- derechohabiente que no tiene un ano sin relacion laboral
                    WHEN 50
                       LET v_cadena = v_cadena, "El derechohabiente no tiene resolucion de SPESS ni cuenta con un año sin relación laboral"
                 
                    -- derechohabiente con credito vigente
                    WHEN 20
                       LET v_cadena = v_cadena, "El derechohabiente tiene un credito vigente"
                 
                    -- SOLICITUD APROBADA
                    WHEN 0
                       LET v_cadena = v_cadena, "La solicitud fue aprobada"
                       
                       -- motivo del rechazo            
                    OTHERWISE
                       LET v_cadena = "Codigo no identificado"
                 END CASE
              END IF
           END IF
           -- se informa al usuario lo ocurrido
           DISPLAY v_cadena TO MSG1

        ELSE
           -- ocurrio un error al ejecutar la consulta
           LET v_error = "Error: ", wsError.description, " - ", wsError.action," - ", wsError.code ," - ", wsError.codeNS	
           DISPLAY v_error TO msg
        END IF

     ON ACTION CANCEL
        EXIT INPUT    

     ON ACTION nueva
        LET v_nss = NULL
        LET v_rfc = NULL
        LET v_causal_ref = NULL
        LET r_estado_solicitud = NULL
        LET r_cod_rechazo = NULL
        LET v_importe7292 = NULL
        LET v_ref_ban = NULL
        
        DISPLAY BY NAME v_nss, v_rfc, v_causal_ref,
                        r_estado_solicitud,
                        r_cod_rechazo,
                        v_importe7292,
                        v_ref_ban
       
        DISPLAY NULL, NULL TO msg, msg1
        CALL ui.interface.refresh()
        NEXT FIELD v_nss

           
  END INPUT

  CLOSE WINDOW w1
  
END MAIN

