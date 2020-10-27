################################################################################
#Modulo        => RET                                                          #
#Programa      => RETC398                                                      #
#Ojetivo       => Realizar consultas de solicitudes de Retiro Ley 73 Grupo 1,  #
#                 que sean de desmarca                                         #
#Fecha inicio  => Noviembre, 2015.                                             #
#Requerimiento => PRODINF-845                                                  #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv

MAIN

    -- Parametros pasados al mandar llamar el programa
    DEFINE p_usuario            CHAR(20)    

    -- Variables usadas para realizar la busqueda
    DEFINE v_nss                CHAR(11)
    DEFINE v_marca              SMALLINT
    DEFINE v_fecha_inicio       DATE
    DEFINE v_fecha_fin          DATE
    DEFINE v_diagnostico        SMALLINT

    -- Variables auxiliares
    DEFINE v_descripcion_marca  CHAR(40)
    DEFINE v_sql                STRING
    DEFINE v_contador           INTEGER

    -- Combobox para la busqueda
    DEFINE cbx_marca            ui.ComboBox
    DEFINE cbx_diagnostico      ui.ComboBox


    DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo          STRING   -- titulo de la ventana 

    --Se obtienen los valores de la ejecucion
    -- se obtienen los parametros de ejecucion
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)
    CLOSE WINDOW SCREEN

    --Se llama a la funcion que desplegara la ventana para introducir los 
    --parametros de la consulta
    OPEN WINDOW busqueda_marcas WITH FORM "RETC3981"

        -- Se llenan los combobox
        LET cbx_marca = ui.ComboBox.forName("v_marca")
        CALL cbx_marca.clear()
        LET cbx_diagnostico = ui.ComboBox.forName("v_diagnostico")
        CALL cbx_diagnostico.clear()

        -- Se obtienen las marcas
        LET v_sql = "SELECT marca,descripcion_marca "||
                    "FROM   sfr_marca               "||
                    "WHERE  marca IN(803,815)       "||
                    "ORDER  BY marca ASC            "
        PREPARE prp_marcas FROM v_sql
        DECLARE cur_marcas CURSOR FOR prp_marcas

        LET v_contador = 0

        FOREACH cur_marcas INTO v_marca,v_descripcion_marca
            CALL cbx_marca.addItem(v_marca,v_marca||" - "||v_descripcion_marca)
            LET v_contador = v_contador + 1
        END FOREACH

        IF v_contador > 0  THEN
            CALL cbx_marca.removeItem(v_contador)
        END IF

        CALL cbx_marca.addItem(NULL,"TODAS")

        -- Se establece el diagnostico
        CALL cbx_diagnostico.addItem(0,"ACEPTADAS")
        CALL cbx_diagnostico.addItem(1,"RECHAZADAS")
        CALL cbx_diagnostico.addItem(NULL,"TODAS")

        INPUT BY NAME  v_nss,v_marca,v_fecha_inicio,v_fecha_fin,v_diagnostico
              ATTRIBUTES(UNBUFFERED,CANCEL = 0)

            ON ACTION ACCEPT
                --Se valida que el NSS sea un numero de 11 digitos
                IF NOT  fn_numero_valido(v_nss) OR (length(v_nss) <> 11 AND length(v_nss) > 0) THEN
                    CALL fn_mensaje("Atención",
                                    "El NSS debe ser un numero de 11 digitos.",
                                    "stop")
                    NEXT FIELD v_nss
                END IF

                CALL fn_busca_solicitudes(v_nss,v_marca,v_fecha_inicio,
                                      v_fecha_fin,v_diagnostico)
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
FUNCTION fn_busca_solicitudes(p_nss,p_marca,p_fecha_inicio,p_fecha_fin,p_diagnostico)

    -- Parametros para realizar la busqueda
    DEFINE p_nss            CHAR(11)
    DEFINE p_marca          SMALLINT
    DEFINE p_fecha_inicio   DATE
    DEFINE p_fecha_fin      DATE
    DEFINE p_diagnostico    SMALLINT

    -- Resultado de la busqueda
    DEFINE v_detalle_solicitud  DYNAMIC ARRAY OF RECORD
                nss             CHAR(11)     ,
                marca           CHAR(50)     ,
                fecha_marca     DATE         ,
                diagnostico     CHAR(10)     ,
                viv92_aivs      DECIMAL(18,2),
                viv92_pesos     DECIMAL(12,2),
                viv97_aivs      DECIMAL(18,2),
                viv97_pesos     DECIMAL(12,2)
    END RECORD

    -- Variables auxiliares
    DEFINE v_contador       INTEGER
    DEFINE v_sql            STRING

    LET v_sql = "SELECT ad.nss,sm.marca||\" - \"||sm.descripcion_marca,             "||
                "       smh.f_inicio,\"Aceptada\",rlg.aivs_viv92,rlg.importe_viv92, "||
                "       rlg.aivs_viv97,rlg.importe_viv97                            "||
                "FROM   ret_ley73_generico rlg ,                                    "||
                "       afi_derechohabiente ad ,                                    "||
                "       sfr_marca_historica smh,                                    "||
                "       sfr_marca sm                                                "||
                "WHERE  rlg.gpo_ley73          = 1                                  "||
                "  AND  rlg.id_derechohabiente = ad.id_derechohabiente              "

    -- Se verifica si se introdujo un nss
    IF p_nss IS NOT NULL THEN
        LET v_sql = v_sql||" AND nss = "||p_nss
    END IF

    LET v_sql = v_sql||"  AND  rlg.id_solicitud = smh.n_referencia "

    -- Se verifica si se introdujo marca
    IF p_marca IS NOT NULL THEN
        LET v_sql = v_sql||" AND marca = "||p_marca
    END IF

    LET v_sql = v_sql||"  AND  smh.f_fin IS NOT NULL "

    -- Se verifica si se introdujo fecha inicial
    IF p_fecha_inicio IS NOT NULL THEN
        IF p_fecha_fin IS NOT NULL THEN 
            LET v_sql = v_sql||" AND (f_inicio BETWEEN '"||p_fecha_inicio||
                        "' AND '"||p_fecha_fin||"') "
        ELSE
            LET v_sql = v_sql||" AND f_inicio > '"||p_fecha_inicio||"' "
        END IF
    ELSE
        IF p_fecha_fin IS NOT NULL THEN 
            LET v_sql = v_sql||" AND f_inicio < '"||p_fecha_fin||"' "
        END IF
    END IF

    LET v_sql = v_sql||"  AND  smh.marca = sm.marca "

    -- Se verifica el diagnostico
    {IF p_diagnostico IS NOT NULL THEN
        LET v_sql = v_sql||" AND p_diagnostico = "||p_diagnostico
    END IF}

    PREPARE prp_detalle_solicitudes FROM v_sql
    DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes

    LET v_contador = 1

    FOREACH cur_detalle_solicitudes INTO v_detalle_solicitud[v_contador].*
        DISPLAY v_detalle_solicitud[v_contador].*
        LET v_contador = v_contador + 1
    END FOREACH

    -- Se elimina el elemento nulo
    IF v_contador > 1 THEN
        CALL v_detalle_solicitud.deleteElement(v_contador)

        OPEN WINDOW solicitudes_detalle WITH FORM "RETC3982"

            DISPLAY ARRAY v_detalle_solicitud
                       TO solicitud_detalle.* ATTRIBUTES(ACCEPT = FALSE,CANCEL = FALSE)

                ON ACTION exportar
                    CALL fn_genera_archivo(v_detalle_solicitud)

                ON ACTION salir
                    EXIT DISPLAY

            END DISPLAY

        CLOSE WINDOW solicitudes_detalle
    ELSE
        CALL fn_mensaje("Atención","No se encontro ningún resultado.","stop")
    END IF

END FUNCTION

#Funcion que genera un archivo plano con el resultado de la consulta,
#separados por pipes.
FUNCTION fn_genera_archivo(p_detalle_solicitud)

    -- Valores a insertar en el archivo de salida
    DEFINE p_detalle_solicitud DYNAMIC ARRAY OF RECORD
                nss             CHAR(11)     ,
                marca           CHAR(50)     ,
                fecha_marca     DATE         ,
                diagnostico     CHAR(10)     ,
                viv92_aivs      DECIMAL(18,2),
                viv92_pesos     DECIMAL(12,2),
                viv97_aivs      DECIMAL(18,2),
                viv97_pesos     DECIMAL(12,2)
    END RECORD

    -- Variables auxiliares
    DEFINE v_channel_sal    base.Channel -- Creacion del archivo de salida
    DEFINE v_texto          STRING
    DEFINE v_ruta_env_arch  LIKE seg_modulo.ruta_envio -- Ruta de envio
    DEFINE v_ruta_arch      STRING -- Ruta de archivo
    DEFINE v_contador       INTEGER
    DEFINE v_string         base.StringBuffer

    --Se obtiene la ruta de envio y ejecutable
    SELECT ruta_envio, ruta_bin
    INTO   v_ruta_env_arch
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- Se prepara el archivo de salida
    LET v_channel_sal = base.Channel.create()

    LET v_string = base.StringBuffer.create()
    LET v_texto = TIME(CURRENT)
    CALL v_string.append(v_texto)
    CALL v_string.replace(":","_",2)

    -- Establecemos el nombre del archivo a partir de la fecha
    LET v_texto = "consulta_"||(TODAY USING "dd_mm_yyyy_")||v_string.toString()||".rmdnp"

    LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/",v_texto
    CALL fn_mensaje("Atención","Archivo de salida: "||v_texto,"stop")
    CALL v_channel_sal.openFile(v_ruta_arch,"w")

    FOR v_contador = 1 TO p_detalle_solicitud.getLength()
        LET v_texto =  p_detalle_solicitud[v_contador].nss                                 ||"|"||
                       p_detalle_solicitud[v_contador].marca                               ||"|"||
                      (p_detalle_solicitud[v_contador].fecha_marca USING "dd.mm.yyyy"     )||"|"||
                       p_detalle_solicitud[v_contador].diagnostico                         ||"|"||
                      (p_detalle_solicitud[v_contador].viv92_aivs  USING "&&&&&&&&&&.&&&&")||"|"||
                      (p_detalle_solicitud[v_contador].viv92_pesos USING "&&&&&&&&&&.&&"  )||"|"||
                      (p_detalle_solicitud[v_contador].viv97_aivs  USING "&&&&&&&&&&.&&&&")||"|"||
                      (p_detalle_solicitud[v_contador].viv97_pesos USING "&&&&&&&&&&.&&"  )
        CALL v_channel_sal.writeLine(v_texto)
    END FOR

    CALL v_channel_sal.close()

END FUNCTION