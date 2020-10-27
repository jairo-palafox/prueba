################################################################################
#Modulo        => RET                                                          #
#Programa      => RETC398                                                      #
#Ojetivo       => Realizar consultas de solicitudes de Retiro Ley 73 Grupo 1,  #
#                 de notificacion                                              #
#Fecha inicio  => Noviembre, 2015.                                             #
#Requerimiento => PRODINFXV-91                                                 #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

MAIN

    -- Variables usadas para realizar la busqueda
    DEFINE v_nss                CHAR(11)
    DEFINE v_folio_notificacion DECIMAL(9,0)
    DEFINE v_folio_datamart     DECIMAL(9,0)
    DEFINE v_fecha_inicial      DATE
    DEFINE v_fecha_final        DATE
    DEFINE v_proceso            SMALLINT

    -- Combobox para la busqueda
    DEFINE cbx_proceso          ui.ComboBox

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion     SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo           STRING   -- titulo de la ventana 

    --Se obtienen los valores de la ejecucion
    -- se obtienen los parametros de ejecucion
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF


    CLOSE WINDOW SCREEN

    --Se llama a la funcion que desplegara la ventana para introducir los 
    --parametros de la consulta
    OPEN WINDOW busqueda_notificacion WITH FORM "RETC3961"

        -- Se prepara el combobox
        LET cbx_proceso = ui.ComboBox.forName("v_proceso")
        CALL cbx_proceso.clear()

        -- Se establecen los procesos
        CALL cbx_proceso.addItem(1572,"HISTORICO")
        CALL cbx_proceso.addItem(1573,"DIARIO"    )
        CALL cbx_proceso.addItem(NULL,"AMBAS"    )

        

        INPUT BY NAME  v_proceso,v_nss,v_folio_notificacion,v_folio_datamart,v_fecha_inicial,v_fecha_final
                       ATTRIBUTES(UNBUFFERED,CANCEL = 0)

            BEFORE INPUT
                LET v_proceso = 1573

            ON ACTION ACCEPT
                --Se valida que el NSS sea un numero de 11 digitos
                IF NOT  fn_numero_valido(v_nss) OR (length(v_nss) <> 11 AND length(v_nss) > 0) THEN
                    CALL fn_mensaje("Atención",
                                    "El NSS debe ser un numero de 11 digitos.",
                                    "stop")
                    NEXT FIELD v_nss
                END IF

                --Se valida que el NSS sea un numero de 11 digitos
                IF v_fecha_inicial > v_fecha_final THEN
                    CALL fn_mensaje("Atención",
                                    "Revise el rango de fechas.",
                                    "stop")
                    NEXT FIELD v_nss
                END IF


                CALL fn_busca_notificacion(v_proceso,v_nss,v_folio_notificacion,v_folio_datamart,v_fecha_inicial,v_fecha_final)

            ON ACTION Salir
                IF fn_ventana_confirma("Atención","¿Desea salir?","quest") = 1 THEN
                    EXIT INPUT
                END IF

        END INPUT

    CLOSE WINDOW busqueda_notificacion

END MAIN

#Funcion que valida si la cadena que se le pasa es un numero
FUNCTION fn_numero_valido(p_cadena)

    DEFINE p_cadena     STRING
    DEFINE v_entero     BIGINT

    TRY
        LET v_entero = p_cadena
        RETURN TRUE
    CATCH
        RETURN FALSE
    END TRY

END FUNCTION

#Funcion que realiza la busqueda de los NSS y su indicador
FUNCTION fn_busca_notificacion(p_proceso,p_nss,p_folio_notificacion,
                               p_folio_datamart,p_fecha_inicial,p_fecha_final)

    -- Parametros para realizar la busqueda
    DEFINE p_nss                CHAR(11)
    DEFINE p_folio_notificacion DECIMAL(9,0)
    DEFINE p_folio_datamart     DECIMAL(9,0)
    DEFINE p_fecha_inicial      DATE
    DEFINE p_fecha_final        DATE
    DEFINE p_proceso            SMALLINT

    -- Variables auxiliares
    DEFINE v_contador       INTEGER
    DEFINE v_sql            STRING
    DEFINE v_estado_envio   STRING

    -- Despliegue
    DEFINE v_detalle_notificacion DYNAMIC ARRAY OF RECORD
                nss                 CHAR(11),
                folio_notificacion  DECIMAL(9,0),
                tipo_notificacion   CHAR(20),
                f_envio             DATE,
                archivo             CHAR(40),
                estado_envio        CHAR(30)
    END RECORD

    -- Se barre la tabla de ret_notificacion, se obtiene el tipo de envio
    -- de la tabla afi_ind_notifica y el archivo se obtiene mediante el folio
    -- en ret_datamart y glo_ctr_archivo
    LET v_sql = "SELECT rn.nss,rn.folio_notificacion,ain.tpo_notificacion,        "||
                "f_envio,gca.nombre_archivo,rn.ind_envio                          "||
                "FROM   ret_notificacion rn                                       "||
                "       LEFT JOIN afi_ind_notifica ain                            "||
                "              ON rn.id_derechohabiente = ain.id_derechohabiente  "||
                "            JOIN ret_datamart     rd                             "||
                "              ON rn.id_datamart        = rd.id_datamart          "||
                "            JOIN glo_ctr_archivo  gca                            "||         
                "              ON rd.folio              = gca.folio               "||
                "WHERE 1=1                                                        "

    -- Se verifica el proceso
    IF p_proceso IS NOT NULL THEN
        IF p_proceso = 1572 THEN
            LET v_sql = v_sql||"  AND  rn.folio_datamart = 0 "
        END IF
        IF p_proceso = 1573 THEN
            LET v_sql = v_sql||"  AND  rn.folio_datamart <> 0 "
        END IF
    END IF

    -- Se verifica NSS
    IF p_nss IS NOT NULL THEN
        LET v_sql = v_sql||"  AND  rn.nss                = '"||p_nss||"' "
    END IF

    -- Se verifica el folio de notificacion
    IF p_folio_notificacion IS NOT NULL THEN
        LET v_sql = v_sql||"  AND  rn.folio_notificacion = "||p_folio_notificacion
    END IF

    -- Se verifica el folio datamart
    IF p_folio_notificacion IS NOT NULL THEN
        LET v_sql = v_sql||"  AND  rd.folio              = "||p_folio_datamart
    END IF

    -- Se verifica el rango de fechas
    IF p_fecha_inicial IS NOT NULL THEN
        IF p_fecha_final IS NOT NULL THEN
            LET v_sql = v_sql||" AND (f_envio BETWEEN '"||p_fecha_inicial||
                               "' AND '"||p_fecha_final||"') "
        ELSE
            LET v_sql = v_sql||" AND f_envio >= '"||p_fecha_inicial||"' "
        END IF
    ELSE
        IF p_fecha_final IS NOT NULL THEN
            LET v_sql = v_sql||" AND f_envio <= '"||p_fecha_final||"' "
        END IF
    END IF

    LET v_sql = v_sql||" ORDER BY rn.nss "

    DISPLAY v_sql

    PREPARE prp_detalle_notificacion FROM v_sql
    DECLARE cur_detalle_notificacion CURSOR FOR prp_detalle_notificacion

    LET v_contador = 1

    FOREACH cur_detalle_notificacion INTO v_detalle_notificacion[v_contador].*
        IF v_detalle_notificacion[v_contador].tipo_notificacion = "1" THEN
            LET v_detalle_notificacion[v_contador].tipo_notificacion = "SMS"
        END IF
        IF v_detalle_notificacion[v_contador].tipo_notificacion = "2" THEN
            LET v_detalle_notificacion[v_contador].tipo_notificacion = "CORREO"
        END IF
        LET v_estado_envio = v_detalle_notificacion[v_contador].estado_envio
        LET v_estado_envio = v_estado_envio.trim()
        IF v_estado_envio = "1" THEN
            LET v_detalle_notificacion[v_contador].estado_envio = 
                v_estado_envio||" - Notificado"
        END IF
        IF v_estado_envio = "2" THEN
            LET v_detalle_notificacion[v_contador].estado_envio = 
                v_estado_envio||" - Sin Marca Notificacion"
        END IF
        IF v_estado_envio = "3" THEN
            LET v_detalle_notificacion[v_contador].estado_envio = 
                v_estado_envio||" - Ya tuvo retiro"
        END IF
        IF v_estado_envio = "4" THEN
            LET v_detalle_notificacion[v_contador].estado_envio = 
                v_estado_envio||" - Suma de saldos menor a 0"
        END IF
        IF v_estado_envio = "5" THEN
            LET v_detalle_notificacion[v_contador].estado_envio = 
                v_estado_envio||" - Marca no convive"
        END IF
        LET v_contador = v_contador + 1
    END FOREACH

    -- Se elimina el elemento nulo
    IF v_contador > 1 THEN

        CALL v_detalle_notificacion.deleteElement(v_contador)

        OPEN WINDOW solicitudes_detalle WITH FORM "RETC3962"

            DISPLAY ARRAY v_detalle_notificacion
                       TO notificacion_detalle.* ATTRIBUTES(ACCEPT = FALSE,CANCEL = FALSE)

                ON ACTION salir
                    EXIT DISPLAY

            END DISPLAY

        CLOSE WINDOW solicitudes_detalle
    ELSE
        CALL fn_mensaje("Atención","No se encontro ningún resultado.","stop")
    END IF

END FUNCTION