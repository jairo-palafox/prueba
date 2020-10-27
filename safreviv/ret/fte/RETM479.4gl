################################################################################
#Modulo        => RET                                                          #
#Programa      => RETM479                                                      #
#Ojetivo       => Programa para cambiar el estado solicitud de 8 o 10 a 100 en #
#                 las tablas ret_solicitud_generico y ret_fondo_ahorro_generico#
#Fecha inicio  => 02 de Abril, 2019.                                           #
#Requerimiento => SACI2018-96                                                  #
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
    --Id Solicitud
    DEFINE v_id_solicitud               DECIMAL(9,0)
    --Record con los datos de la busqueda
    DEFINE  v_datos_der_hab             RECORD
            --Datos de la tabla afi_derechohabiente
            nombre_af                   CHAR(40),
            ap_paterno_af               CHAR(40),
            ap_materno_af               CHAR(40),
            --Datos obtenidos de la tabla ret_solicitud_generico
            estado_solicitud            SMALLINT,
            id_solicitud                DECIMAL(9,0),
            modalidad                   CHAR(10),
            nss                         CHAR(11),
            medio_entrega               CHAR(10),
            f_solicitud                 DATE,
            estado_sol_desc             CHAR(15),
            caso_adai                   CHAR(10)
    END RECORD

    DEFINE v_estado                 SMALLINT
    DEFINE v_estado_indicador       SMALLINT
    DEFINE v_diagnostico            SMALLINT
    DEFINE v_estado_cuenta          SMALLINT
    DEFINE v_intentos_desmarca      SMALLINT 
    DEFINE v_continua               SMALLINT
    DEFINE v_cod_rechazo            SMALLINT

    --Se obtienen los valores de la ejecucion
    LET p_usuario = ARG_VAL(1)
    LET p_tpo_ejecucion  = ARG_VAL(2)
    LET p_nom_ventana = ARG_VAL(3)

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_nom_ventana)
    END IF

    --Se inicia el log del programa
    CALL STARTLOG (p_usuario CLIPPED||".RETM479.log")

    CLOSE WINDOW SCREEN
    
    --Se llama a la funcion que desplegara la ventana para introducir los 
    --parametros de la consulta
    OPEN WINDOW actualiza WITH FORM "RETM4791"

        DIALOG ATTRIBUTES(UNBUFFERED)
            --Input para realizar la busqueda
            INPUT BY NAME v_nss,v_caso_adai, v_id_solicitud
               BEFORE INPUT 
               LET v_estado           = 0
               LET v_estado_indicador = 0

                ON ACTION buscar
                    IF v_id_solicitud IS NULL AND v_nss IS NULL AND v_caso_adai IS NULL THEN 
                        CALL fn_mensaje("Atención",
                                        "Debe ingresar al menos un dato para la búsqueda",
                                        "stop")
                        NEXT FIELD v_nss
                    ELSE 
                       IF v_nss IS NOT NULL THEN 
                          IF (NOT  fn_valida_numero(v_nss)) OR (length(v_nss) <> 11) THEN
                              CALL fn_mensaje("Atención",
                                              "El NSS debe ser un número de 11 dígitos.",
                                              "stop")
                              NEXT FIELD v_nss
                          END IF
                        END IF 
                        IF v_caso_adai IS NOT NULL THEN 
                           --Se valida que el caso ADAI sea un numero de 10 digitos
                           IF (NOT fn_valida_numero(v_caso_adai)) OR (length(v_caso_adai) <> 10) THEN
                              CALL fn_mensaje("Atención",
                                              "El caso CRM debe ser un número de 10 dígitos.",
                                              "stop")
                              NEXT FIELD v_caso_adai
                           END IF
                        END IF 
                        IF v_id_solicitud IS NOT NULL THEN 
                           --Se valida que el id_solicitud sea un numero
                           IF (NOT fn_valida_numero(v_id_solicitud)) THEN
                              CALL fn_mensaje("Atención",
                                              "El Id Solicitud debe ser numérico.",
                                              "stop")
                              NEXT FIELD v_id_solicitud
                           END IF
                        END IF 
                    END IF                     
                    --Se valida que el NSS sea un numero de 11 digitos

                    --Se realiza la busqueda
                    CALL fn_busca_der_hab(v_nss,v_caso_adai,v_id_solicitud) RETURNING v_datos_der_hab.*
                    --Si existen datos se muestran
                    IF v_datos_der_hab.id_solicitud IS NOT NULL THEN
                        IF v_datos_der_hab.estado_solicitud =  8 OR 
                           v_datos_der_hab.estado_solicitud = 10 OR 
                           v_datos_der_hab.estado_solicitud = 15 THEN
                            DISPLAY BY NAME v_datos_der_hab.*
                        ELSE
                            CALL fn_mensaje("Atención",
                                        "No se encontró ninguna solicitud.",
                                        "information")
                        END IF
                    ELSE
                        CALL fn_mensaje("Atención",
                                        "No se encontró ninguna solicitud.",
                                        "information")
                    END IF

                ON ACTION actualizar
                    --Se verifica que se haya hecho una busqueda
                    IF v_datos_der_hab.id_solicitud IS NULL THEN
                        CALL fn_mensaje("Atención",
                                        "Primero realice la búsqueda de una solicitud.",
                                        "information")
                        NEXT FIELD v_nss
                    END IF
                    --En caso de que se haya realizado la busqueda se actualiza
                    IF v_datos_der_hab.estado_solicitud =  8 OR 
                       v_datos_der_hab.estado_solicitud = 10 OR 
                       v_datos_der_hab.estado_solicitud = 15 THEN
                       -- Se realiza la desmarca en PROCESAR
                       CALL fn_obtiene_cod_rechazo() RETURNING v_cod_rechazo
                       IF v_cod_rechazo IS NULL OR v_cod_rechazo = 0 THEN 
                          CALL fn_mensaje("Atención",
                                          "Debe elegir un rechazo para actualizar la solicitud.",
                                          "stop")
                          NEXT FIELD v_nss
                       ELSE 
                          CALL fn_actualiza_estado(v_datos_der_hab.id_solicitud, v_cod_rechazo)
                       END IF 
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
                    CALL fn_busca_der_hab(v_nss,v_caso_adai,v_id_solicitud) RETURNING v_datos_der_hab.*
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
FUNCTION fn_busca_der_hab(p_nss,p_caso_adai,p_id_solicitud)

    --Variable para guardar el nss recibido
    DEFINE  p_nss                       CHAR(11)
    --Variable para guardar el caso ADAI recibido
    DEFINE  p_caso_adai                 CHAR(10)
    --Variable para guardar el id_solicitud recibido
    DEFINE  p_id_solicitud              DECIMAL(9,0)
    --Record con los datos de la busqueda
    DEFINE  r_datos_der_hab             RECORD  
            --Datos de la tabla afi_derechohabiente
            nombre_af                   CHAR(40),
            ap_paterno_af               CHAR(40),
            ap_materno_af               CHAR(40),
            --Datos de la tabla ret_solicitud_generico
            estado_solicitud            SMALLINT,
            id_solicitud                DECIMAL(9,0),
            modalidad                   CHAR(10),
            nss                         CHAR(11),
            medio_entrega               CHAR(10),
            f_solicitud                 DATE,
            estado_sol_desc             CHAR(15),
            caso_adai                   CHAR(10)
    END RECORD
    --Variable para preparar los queries
    DEFINE v_query                      STRING

    --Se construye la consulta SELECT
    LET v_query = "SELECT a.nombre_af, a.ap_paterno_af, a.ap_materno_af,
                          r.estado_solicitud, r.id_solicitud,
                          '2 - Fondo de Ahorro', r.nss, c.descripcion, r.f_solicitud,
                          e.des_corta, r.caso_adai
                   FROM   afi_derechohabiente a, ret_solicitud_generico r,
                          ret_sol_medio_entrega rs, ret_cat_medio_entrega c,
                          ret_estado_solicitud e
                   WHERE  a.id_derechohabiente = r.id_derechohabiente
                   AND    r.modalidad_retiro = 2  
                   AND    r.id_solicitud = rs.id_solicitud
                   AND    r.estado_solicitud IN (8,10,15)
                   AND    rs.medio_entrega = c.medio_entrega
                   AND    r.estado_solicitud = e.estado_solicitud "
    IF p_nss IS NOT NULL THEN 
      LET v_query = v_query CLIPPED, " AND r.nss = '",p_nss,"'"
    END IF 
    IF p_caso_adai IS NOT NULL THEN 
      LET v_query = v_query CLIPPED, " AND r.caso_adai = '",p_caso_adai,"'"
    END IF 
    IF p_id_solicitud IS NOT NULL THEN 
      LET v_query = v_query CLIPPED, " AND r.id_solicitud = ", p_id_solicitud 
    END IF 
    --Se prepara el query
    PREPARE prp_datos_der_hab FROM v_query
    --Se ejecuta el query
    EXECUTE prp_datos_der_hab INTO r_datos_der_hab.*
    --Se regresa el record con los valores encontrados del derechohabiente
    RETURN r_datos_der_hab.*

END FUNCTION

#Funcion que actualiza el estado_solicitud a 100 y desmarca la cuenta
FUNCTION fn_actualiza_estado(p_id_solicitud, p_cod_rechazo)

    --Variable que contiene el id de la solicitud que se cambiara de estado
    DEFINE p_id_solicitud       CHAR(11)
    DEFINE p_cod_rechazo        SMALLINT
    --Variable para definir los queries
    DEFINE v_query              STRING
    --Variable para realizar el desmarcado de la solicitud
    DEFINE rec_marca            RECORD LIKE sfr_marca_activa.*
    DEFINE v_resultado          SMALLINT

    --Se realiza la actualizacion
    UPDATE ret_solicitud_generico
        SET estado_solicitud = 100,
            cod_rechazo = p_cod_rechazo
    WHERE id_solicitud = p_id_solicitud

    UPDATE ret_fondo_ahorro_generico
        SET estado_solicitud = 100,
            cod_rechazo = p_cod_rechazo
    WHERE id_solicitud = p_id_solicitud

    --Se quita la marca
    SELECT *
    INTO   rec_marca.*
    FROM   sfr_marca_activa
    WHERE  n_referencia = p_id_solicitud
    AND    marca IN (802)

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

#Funcion que obtiene el caso adai de la solicitud
FUNCTION recuperaDatos(p_id_solicitud)

    -- Parametros recibidos
    DEFINE p_id_solicitud       DECIMAL(9,0)

    -- Datos a devolver
    DEFINE r_caso_crm           CHAR(10)

    -- Variables auxiliares
    DEFINE v_sql                STRING
    DEFINE v_conteo             SMALLINT
    DEFINE v_estado_indicador   STRING
    DEFINE v_f_intento          DATE

    LET r_caso_crm = NULL 
    IF p_id_solicitud IS NOT NULL THEN
      SELECT caso_adai
      INTO   r_caso_crm
      FROM   ret_solicitud_generico
      WHERE  id_solicitud = p_id_solicitud
    END IF

    RETURN r_caso_crm

END FUNCTION

FUNCTION fn_obtiene_cod_rechazo()
DEFINE v_cod_rechazo        SMALLINT 
DEFINE v_cbx_cod_rechazo    ui.ComboBox -- combo de códigos de rechazo
DEFINE v_r_ret_rechazo      RECORD LIKE ret_rechazo_generico.*
DEFINE v_s_cadena           STRING 
DEFINE v_cod_rechazo_return SMALLINT 


   OPEN WINDOW w_codigos_rechazo WITH FORM "RETM4792"
      LET v_cbx_cod_rechazo = ui.ComboBox.forName("formonly.cmb_cod_rechazo")
      CALL v_cbx_cod_rechazo.clear()
      INPUT v_cod_rechazo
      FROM cmb_cod_rechazo
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT
           LET v_cod_rechazo = NULL
           -- se llena el arreglo de los estados de solicitud
           DECLARE cur_cod_rechazo_t CURSOR FOR
           SELECT A.cod_rechazo ,              
                  A.des_corta
           FROM   ret_rechazo_generico A
           WHERE  cod_rechazo IN (2014,2015,2016)
           ORDER BY 2;


           FOREACH cur_cod_rechazo_t INTO v_r_ret_rechazo.cod_rechazo, v_r_ret_rechazo.des_corta
              LET v_s_cadena = v_r_ret_rechazo.cod_rechazo, " - ", v_r_ret_rechazo.des_corta
              CALL v_cbx_cod_rechazo.addItem(v_r_ret_rechazo.cod_rechazo, v_s_cadena)
           END FOREACH

           FREE cur_cod_rechazo_t
      ON ACTION ACCEPT 
         LET v_cod_rechazo_return = v_cod_rechazo
         EXIT INPUT 
      ON ACTION CANCEL
         LET v_cod_rechazo = 0
         EXIT INPUT 
      END INPUT 
   CLOSE WINDOW w_codigos_rechazo
   

RETURN v_cod_rechazo_return
END FUNCTION 