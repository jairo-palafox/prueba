################################################################################
#Modulo        => RET                                                          #
#Programa      => RETC411                                                      #
#Ojetivo       => Control de marcas y desmarcas en PROCESAR                    #
#Fecha inicio  => Noviembre, 2015.                                             #
#Requerimiento => PRODINFXVI-30                                                #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv

MAIN

    -- Variables usadas para realizar la busqueda
    DEFINE v_nss                CHAR(11)
    DEFINE v_indicador_marca    SMALLINT
    DEFINE v_estado_indicador   SMALLINT
    DEFINE v_origen             SMALLINT
    DEFINE v_fecha_inicio       DATE
    DEFINE v_fecha_fin          DATE

    -- Combobox para la busqueda
    DEFINE cbx_indicador_marca  ui.ComboBox
    DEFINE cbx_estado_cuenta    ui.ComboBox
    DEFINE cbx_modalidad        ui.ComboBox

    DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo          STRING   -- titulo de la ventana 

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
    OPEN WINDOW busqueda_marcas WITH FORM "RETC4111"

        -- Se establecen los combobox
        LET cbx_indicador_marca = ui.ComboBox.forName("v_indicador_marca" )
        LET cbx_estado_cuenta   = ui.ComboBox.forName("v_estado_indicador")
        LET cbx_modalidad       = ui.ComboBox.forName("v_origen"          )

        INPUT BY NAME  v_nss,v_indicador_marca,v_estado_indicador,
                       v_origen,v_fecha_inicio,v_fecha_fin
                       ATTRIBUTES(UNBUFFERED,CANCEL = 0)

            BEFORE INPUT

                -- Se establece el proceso
                CALL cbx_indicador_marca.clear()
                CALL cbx_indicador_marca.addItem(NULL,"AMBAS"   )
                CALL cbx_indicador_marca.addItem(1   ,"MARCA"   )
                CALL cbx_indicador_marca.addItem(2   ,"DESMARCA")

                -- Se establece los estados
                CALL cbx_estado_cuenta.clear()
                CALL cbx_estado_cuenta.addItem(NULL,"TODAS"        )
                CALL cbx_estado_cuenta.addItem(1   ,"NO MARCADA"   )
                CALL cbx_estado_cuenta.addItem(2   ,"MARCADA"      )
                CALL cbx_estado_cuenta.addItem(4   ,"NO DESMARCADA")
                CALL cbx_estado_cuenta.addItem(5   ,"DESMARCADA"   )

                -- Se establecen la modalidad
                CALL cbx_modalidad.clear()
                CALL cbx_modalidad.addItem(NULL,"TODAS" )
                CALL cbx_modalidad.addItem(1   ,"LINEA" )
                CALL cbx_modalidad.addItem(2   ,"BATCH" )
                CALL cbx_modalidad.addItem(3   ,"MANUAL")

                LET v_nss              = NULL
                LET v_indicador_marca  = NULL
                LET v_estado_indicador = NULL
                LET v_origen           = NULL
                LET v_fecha_inicio     = TODAY
                LET v_fecha_fin        = TODAY

            ON CHANGE v_indicador_marca
                -- Ambas
                IF v_indicador_marca IS NULL THEN
                    LET  v_estado_indicador   = NULL
                    CALL cbx_estado_cuenta.clear()
                    CALL cbx_estado_cuenta.addItem(NULL,"TODAS"        )
                    CALL cbx_estado_cuenta.addItem(1   ,"NO MARCADA"   )
                    CALL cbx_estado_cuenta.addItem(2   ,"MARCADA"      )
                    CALL cbx_estado_cuenta.addItem(4   ,"NO DESMARCADA")
                    CALL cbx_estado_cuenta.addItem(5   ,"DESMARCADA"   )
                END IF
                -- Marcas
                IF v_indicador_marca = 1 THEN
                    LET  v_estado_indicador   = NULL
                    CALL cbx_estado_cuenta.clear()
                    CALL cbx_estado_cuenta.addItem(NULL,"TODAS"        )
                    CALL cbx_estado_cuenta.addItem(1   ,"NO MARCADA"   )
                    CALL cbx_estado_cuenta.addItem(2   ,"MARCADA"      )
                END IF
                -- Desmarcas
                IF v_indicador_marca = 2 THEN
                    LET  v_estado_indicador   = NULL
                    CALL cbx_estado_cuenta.clear()
                    CALL cbx_estado_cuenta.addItem(NULL,"TODAS"        )
                    CALL cbx_estado_cuenta.addItem(4   ,"NO DESMARCADA")
                    CALL cbx_estado_cuenta.addItem(5   ,"DESMARCADA"   )
                END IF

            ON ACTION ACCEPT
                --Se valida que el NSS sea un numero de 11 digitos
                IF NOT  fn_numero_valido(v_nss) OR (length(v_nss) <> 11 AND length(v_nss) > 0) THEN
                    CALL fn_mensaje("Atención",
                                    "El NSS debe ser un numero de 11 digitos.",
                                    "stop")
                    NEXT FIELD v_nss
                END IF

                {IF v_fecha_inicio IS NULL OR v_fecha_fin IS NULL THEN
                    CALL fn_mensaje("Atención",
                                    "Establece un rango de fechas.",
                                    "stop")
                    NEXT FIELD v_fecha_inicio
                END IF}

                DISPLAY ""
                DISPLAY ""
                DISPLAY "Parametros de Consulta         "
                DISPLAY "-------------------------------"
                DISPLAY "v_nss              :",v_nss
                DISPLAY "v_indicador_marca  :",v_indicador_marca
                DISPLAY "v_estado_indicador :",v_estado_indicador
                DISPLAY "v_origen           :",v_origen
                DISPLAY "v_fecha_inicio     :",v_fecha_inicio
                DISPLAY "v_fecha_fin        :",v_fecha_fin

                CALL fn_busca_solicitudes(v_nss,v_indicador_marca,v_estado_indicador,
                                          v_origen,v_fecha_inicio,v_fecha_fin)

            ON ACTION salir
                IF fn_ventana_confirma("Atención","¿Desea salir?","quest") = 1 THEN
                    EXIT INPUT
                END IF

        END INPUT

    CLOSE WINDOW busqueda_marcas

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

#Funcion que realiza la busqueda de las solicitudes y las imprime
FUNCTION fn_busca_solicitudes(p_nss,p_indicador_marca,p_estado_indicador,
                              p_origen,p_fecha_inicio,p_fecha_fin)

    -- Parametros para realizar la busqueda
    DEFINE p_nss                CHAR(11)
    DEFINE p_indicador_marca    SMALLINT
    DEFINE p_estado_indicador   SMALLINT
    DEFINE p_origen             SMALLINT
    DEFINE p_fecha_inicio       DATE
    DEFINE p_fecha_fin          DATE

    -- Resultado de la busqueda
    DEFINE v_detalle_solicitud  DYNAMIC ARRAY OF RECORD
                nss                 CHAR(11),
                indicador_marca     CHAR(10),
                estado_indicador    CHAR(40),
                fecha               DATE    ,
                origen              CHAR(10)
    END RECORD

    -- Variables auxiliares
    DEFINE v_contador       INTEGER
    DEFINE v_sql            STRING
    DEFINE v_string         base.StringBuffer

    LET v_string = base.StringBuffer.create()

    -- Se construye el query
    LET v_sql = "SELECT rpu.nss,rim.indicador_desc,                                    "||
                "       rem.estado_desc,rpf.f_intento,roe.origen_desc                  "||
                "FROM   ret_ctr_marca_procesar_maestra rmp,                            "||
                -- Obtenemos solo los ultimos registros
                "       (SELECT MAX(id_solicitud) as id_solicitud,nss                  "||
                "        FROM   ret_ctr_marca_procesar_maestra                         "||
                "        GROUP  BY nss) rpu,                                           "||
                -- Obtenemos la fecha del ultimo intento de envio
                "       (SELECT MAX(f_intento) as f_intento,id_solicitud,              "||
                "               indicador_marca,estado_indicador                       "||
                "        FROM   ret_ctr_marca_procesar                                 "||
                "        GROUP  BY id_solicitud,indicador_marca,estado_indicador) rpf, "||
                -- Descripcion del proceso
                "       ret_indicador_marca     rim,                                   "||
                -- Descripcion del estado del proceso
                "       ret_estado_marca        rem,                                   "||
                -- Descripcion del origen de la ejecucion
                "       ret_origen_ejecucion    roe                                    "||
                "WHERE  rmp.id_solicitud     = rpu.id_solicitud                        "||
                "  AND  rmp.id_solicitud     = rpf.id_solicitud                        "||
                "  AND  rmp.estado_indicador = rpf.estado_indicador                    "||
                "  AND  rpf.indicador_marca  = rim.indicador_marca                     "||
                "  AND  rmp.estado_indicador = rem.id_estado                           "||
                "  AND  rmp.origen           = roe.id_origen                           "

    -- Se verifica si se introdujo un nss
    IF p_nss IS NOT NULL THEN
        LET v_sql = v_sql||"  AND rmp.nss = '"||p_nss||"' "
    END IF

    -- Se verifica el tipo de proceso
    IF p_indicador_marca IS NOT NULL THEN
        LET v_sql = v_sql||"  AND rpf.indicador_marca = "||p_indicador_marca
    END IF

    -- Se verifica el estado del proceso
    IF p_estado_indicador IS NOT NULL THEN
        LET v_sql = v_sql||"  AND rmp.estado_indicador = "||p_estado_indicador
    END IF

    -- Se verifica el origen
    IF p_origen IS NOT NULL THEN
        LET v_sql = v_sql||"  AND rmp.origen = "||p_origen
    END IF

    -- Se verifica el rango de fechas
    IF p_fecha_inicio IS NOT NULL THEN
        -- Se introdujeron ambas fechas
        IF p_fecha_fin IS NOT NULL THEN
            LET v_sql = v_sql||"  AND rpf.f_intento BETWEEN '"||(p_fecha_inicio USING "mm/dd/yyyy")||"' AND '"||(p_fecha_fin USING "mm/dd/yyyy")||"' "
        -- Se introdujo solo la primera fecha
        ELSE
            LET v_sql = v_sql||"  AND rpf.f_intento > '"||(p_fecha_inicio USING "mm/dd/yyyy")||"' "
        END IF
    ELSE
        -- Se introdujo solo la segunda fecha
        IF p_fecha_fin IS NOT NULL THEN
            LET v_sql = v_sql||"  AND rpf.f_intento < '"||(p_fecha_fin USING "mm/dd/yyyy")||"' "
        END IF
    END IF

    LET v_sql = v_sql||" ORDER BY rpf.f_intento,rim.indicador_desc,rem.estado_desc,roe.origen_desc "

    --DISPLAY v_sql

    PREPARE prp_detalle_solicitudes FROM v_sql
    DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes

    LET v_contador = 1

    FOREACH cur_detalle_solicitudes INTO v_detalle_solicitud[v_contador].*
        LET v_contador = v_contador + 1
    END FOREACH

    DISPLAY "Resultados :  ",(v_contador-1)

    -- Se elimina el elemento nulo
    IF v_contador > 1 THEN

        CALL v_detalle_solicitud.deleteElement(v_contador)

        OPEN WINDOW solicitudes_detalle WITH FORM "RETC4112"

            DISPLAY ARRAY v_detalle_solicitud
                       TO solicitud_detalle.* ATTRIBUTES(ACCEPT = FALSE,CANCEL = FALSE)

                ON ACTION regresar
                    EXIT DISPLAY

            END DISPLAY

        CLOSE WINDOW solicitudes_detalle
    ELSE
        CALL fn_mensaje("Atención","No se encontro ningún resultado.","stop")
    END IF

END FUNCTION