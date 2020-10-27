################################################################################
#Modulo        => RET                                                          #
#Programa      => RETM375                                                      #
#Ojetivo       => Programa para cambiar el estado solicitud de 8 o 10 a 100 en #
#                 las tablas ret_solicitud_generico y ret_amort_excedente.     #
#Fecha inicio  => 23 de Julio, 2015.                                           #
#Requerimiento => 841                                                          #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

MAIN

    DEFINE p_usuario        CHAR(20)
    DEFINE p_tpo_ejecucion  SMALLINT
    DEFINE p_nom_ventana    STRING

    --NSS introducido
    DEFINE  v_nss                       CHAR(11)
    --Caso ADAI introducido
    DEFINE  v_caso_adai                 CHAR(10)
    --Record con los datos de la busqueda
    DEFINE  v_datos_der_hab             RECORD
            --Datos de la tabla afi_derechohabiente
            nombre_af                   CHAR(40),
            ap_paterno_af               CHAR(40),
            ap_materno_af               CHAR(40),
            --Datos obtenidos de la tabla ret_solicitud_generico
            estado_solicitud            SMALLINT,
            id_solicitud                DECIMAL(9,0)
    END RECORD

    --Se obtienen los valores de la ejecucion
    LET p_usuario = ARG_VAL(1)
    LET p_tpo_ejecucion  = ARG_VAL(2)
    LET p_nom_ventana = ARG_VAL(3)

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_nom_ventana)
    END IF

    --Se inicia el log del programa
    CALL STARTLOG (p_usuario CLIPPED||".RETM375.log")

    CLOSE WINDOW SCREEN
    
    --Se llama a la funcion que desplegara la ventana para introducir los 
    --parametros de la consulta
    OPEN WINDOW actualiza WITH FORM "RETM3751"

        DIALOG ATTRIBUTES(UNBUFFERED)
            --Input para realizar la busqueda
            INPUT BY NAME v_nss,v_caso_adai
            
                ON ACTION buscar
                    --Se valida que el NSS sea un numero de 11 digitos
                    IF (NOT  fn_valida_numero(v_nss)) OR (length(v_nss) <> 11) THEN
                        CALL fn_mensaje("Atención",
                                        "El NSS debe ser un numero de 11 digitos.",
                                        "stop")
                        NEXT FIELD v_nss
                    END IF
                    --Se valida que el caso ADAI sea un numero de 10 digitos
                    IF (NOT fn_valida_numero(v_caso_adai)) OR (length(v_caso_adai) < 9) THEN
                        CALL fn_mensaje("Atención",
                                        "El caso ADAI debe ser un número de al menos 9 digitos.",
                                        "stop")
                        NEXT FIELD v_caso_adai
                    END IF
                    --Se realiza la busqueda
                    CALL fn_busca_der_hab(v_nss,v_caso_adai) RETURNING v_datos_der_hab.*
                    --Si existen datos se muestran
                    IF v_datos_der_hab.id_solicitud IS NOT NULL THEN
                        IF v_datos_der_hab.estado_solicitud == 8 OR 
                           v_datos_der_hab.estado_solicitud == 10 OR 
                           v_datos_der_hab.estado_solicitud == 211 OR 
                           v_datos_der_hab.estado_solicitud == 212 OR 
                           v_datos_der_hab.estado_solicitud == 214 OR 
                           v_datos_der_hab.estado_solicitud == 100 THEN
                            DISPLAY BY NAME v_datos_der_hab.*
                        ELSE
                            CALL fn_mensaje("Atención",
                                        "No se encontro ninguna solicitud.",
                                        "information")
                        END IF
                    ELSE
                        CALL fn_mensaje("Atención",
                                        "No se encontro ninguna solicitud.",
                                        "information")
                    END IF

                ON ACTION actualizar
                    --Se verifica que se haya hecho una busqueda
                    IF v_datos_der_hab.id_solicitud IS NULL THEN
                        CALL fn_mensaje("Atención",
                                        "Primero realice la busqueda de una solicitud.",
                                        "information")
                        NEXT FIELD v_nss
                    END IF
                    --En caso de que se haya realizado la busqueda se actualiza
                    IF v_datos_der_hab.estado_solicitud = 8 OR 
                       v_datos_der_hab.estado_solicitud = 10 OR 
                       v_datos_der_hab.estado_solicitud = 211 OR 
                       v_datos_der_hab.estado_solicitud = 212 OR 
                       v_datos_der_hab.estado_solicitud = 214 THEN
                        CALL fn_actualiza_estado(v_datos_der_hab.id_solicitud)
                    ELSE
                        IF v_datos_der_hab.estado_solicitud = 100 THEN 
                            CALL fn_mensaje("Atención",
                                            "La solicitud ya tiene estado de rechazo",
                                            "information")
                        ELSE 
                            CALL fn_mensaje("Atención",
                                            "El estado actual de la solicitud no permite la actualización",
                                            "information")
                        END IF 
                    END IF 
                    CALL fn_busca_der_hab(v_nss,v_caso_adai) RETURNING v_datos_der_hab.*
                    DISPLAY BY NAME v_datos_der_hab.*
            END INPUT

            ON ACTION CLOSE
                IF fn_ventana_confirma("Atención","¿Desea salir?","quest") = 1 THEN
                    EXIT DIALOG
                END IF

        END DIALOG
    
    CLOSE WINDOW actualiza

END MAIN

#Funcion que valida si la cadena que se le pasa es un numero
FUNCTION fn_valida_numero(p_cadena)

    DEFINE p_cadena     STRING
    DEFINE v_entero     BIGINT

    TRY
        LET v_entero = p_cadena
        RETURN TRUE
    CATCH
        RETURN FALSE
    END TRY

END FUNCTION

#Funcion que busca los datos de un derechohabiente de acuerdo al NSS Y caso ADAI
FUNCTION fn_busca_der_hab(p_nss,p_caso_adai)

    --Variable para guardar el nss recibido
    DEFINE  p_nss                       CHAR(11)
    --Variable para guardar el caso ADAI recibido
    DEFINE  p_caso_adai                 CHAR(10)
    --Record con los datos de la busqueda
    DEFINE  r_datos_der_hab             RECORD  
            --Datos de la tabla afi_derechohabiente
            nombre_af                   CHAR(40),
            ap_paterno_af               CHAR(40),
            ap_materno_af               CHAR(40),
            --Datos de la tabla ret_solicitud_generico
            estado_solicitud            SMALLINT,
            id_solicitud                DECIMAL(9,0)
    END RECORD
    --Variable para preparar los queries
    DEFINE v_query                      STRING

    --Se construye la consulta SELECT
    LET v_query = "SELECT nombre_af,ap_paterno_af,ap_materno_af,
                          estado_solicitud,id_solicitud
                   FROM afi_derechohabiente a, ret_solicitud_generico r
                   WHERE a.id_derechohabiente = r.id_derechohabiente
                         AND r.nss = '",p_nss,"' AND caso_adai = '",p_caso_adai,"'"
    --Se prepara el query
    PREPARE prp_datos_der_hab FROM v_query
    --Se ejecuta el query
    EXECUTE prp_datos_der_hab INTO r_datos_der_hab.*
    --Se regresa el record con los valores encontrados del derechohabiente
    RETURN r_datos_der_hab.*

END FUNCTION

#Funcion que actualiza el estado_solicitud a 100 y desmarca la cuenta
FUNCTION fn_actualiza_estado(p_id_solicitud)

    --Variable que contiene el id de la solicitud que se cambiara de estado
    DEFINE p_id_solicitud       CHAR(11)
    --Variable para definir los queries
    DEFINE v_query              STRING
    --Variable para realizar el desmarcado de la solicitud
    DEFINE rec_marca            RECORD LIKE sfr_marca_activa.*
    DEFINE v_resultado          SMALLINT

    --Se realiza la actualizacion
    UPDATE ret_solicitud_generico
        SET estado_solicitud = 100,
            cod_rechazo = 54
    WHERE id_solicitud = p_id_solicitud

    UPDATE ret_amort_excedente
        SET estado_solicitud = 100,
            cod_rechazo = 54
    WHERE id_solicitud = p_id_solicitud

    --Se quita la marca
    SELECT *
    INTO rec_marca.*
    FROM sfr_marca_activa
    WHERE n_referencia = p_id_solicitud
    AND marca = 810

    IF rec_marca.id_derechohabiente IS NOT NULL  THEN 
        LET v_query = "EXECUTE FUNCTION fn_desmarca_cuenta(",rec_marca.id_derechohabiente,","
                                                            ,rec_marca.marca,","
                                                            ,rec_marca.n_referencia,","
                                                            ,"0,"
                                                            ,rec_marca.marca_causa,","
                                                            ,'"SAFREVIV",'
                                                            ,rec_marca.proceso_marca,")"
        PREPARE prp_desmarca FROM v_query
        EXECUTE prp_desmarca INTO v_resultado
    END IF 

END FUNCTION