################################################################################
#Modulo        => RET                                                          #
#Programa      => RETC398                                                      #
#Ojetivo       => Realizar consultas de solicitudes de Retiro Ley 73 Grupo 1,  #
#                 que sean de marca                                            #
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
    DEFINE v_modalidad          SMALLINT
    DEFINE v_fecha_inicio       DATE
    DEFINE v_fecha_fin          DATE
    DEFINE v_diagnostico        SMALLINT

    -- Variables auxiliares
    DEFINE v_descripcion_marca  CHAR(40)
    DEFINE v_sql                STRING
    DEFINE v_contador           INTEGER

    -- Combobox para la busqueda
    DEFINE cbx_modalidad        ui.ComboBox
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

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF


    CLOSE WINDOW SCREEN

    --Se llama a la funcion que desplegara la ventana para introducir los 
    --parametros de la consulta
    OPEN WINDOW busqueda_marcas WITH FORM "RETC3981"

        -- Se llenan los combobox
        LET  cbx_modalidad = ui.ComboBox.forName("v_modalidad")
        CALL cbx_modalidad.clear()
        LET  cbx_marca = ui.ComboBox.forName("v_marca")
        CALL cbx_marca.clear()
        LET  cbx_diagnostico = ui.ComboBox.forName("v_diagnostico")
        CALL cbx_diagnostico.clear()

        -- Se establecen las modalidades
        CALL cbx_modalidad.addItem(1,"MARCA")
        CALL cbx_modalidad.addItem(2,"DESMARCA")
        CALL cbx_modalidad.addItem(NULL,"AMBAS")


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

        -- Se establece los diagnosticos
        -- De acuerdo al requerimiento
        -- todas llevaran de diagnostico (ret_ctr_marca_procesar) 101
        -- estado_cuenta es el que varia
        CALL cbx_diagnostico.addItem(101,fn_diagnostico_desc("101"))
        CALL cbx_diagnostico.addItem(102,fn_diagnostico_desc("102"))
        CALL cbx_diagnostico.addItem(201,fn_diagnostico_desc("201"))
        CALL cbx_diagnostico.addItem(202,fn_diagnostico_desc("202"))
        CALL cbx_diagnostico.addItem(203,fn_diagnostico_desc("203"))
        CALL cbx_diagnostico.addItem(204,fn_diagnostico_desc("204"))
        CALL cbx_diagnostico.addItem(205,fn_diagnostico_desc("204"))
        CALL cbx_diagnostico.addItem(207,fn_diagnostico_desc("205"))
        CALL cbx_diagnostico.addItem(208,fn_diagnostico_desc("207"))
        CALL cbx_diagnostico.addItem(211,fn_diagnostico_desc("211"))
        CALL cbx_diagnostico.addItem(212,fn_diagnostico_desc("212"))
        CALL cbx_diagnostico.addItem(213,fn_diagnostico_desc("213"))
        CALL cbx_diagnostico.addItem(216,fn_diagnostico_desc("216"))
        CALL cbx_diagnostico.addItem(217,fn_diagnostico_desc("217"))
        CALL cbx_diagnostico.addItem(219,fn_diagnostico_desc("219"))
        CALL cbx_diagnostico.addItem(220,fn_diagnostico_desc("220"))
        CALL cbx_diagnostico.addItem(221,fn_diagnostico_desc("221"))
        CALL cbx_diagnostico.addItem(223,fn_diagnostico_desc("223"))
        CALL cbx_diagnostico.addItem(224,fn_diagnostico_desc("224"))
        CALL cbx_diagnostico.addItem(225,fn_diagnostico_desc("225"))
        CALL cbx_diagnostico.addItem(226,fn_diagnostico_desc("226"))

        {CALL cbx_diagnostico.addItem(1,"ACEPTADO")
        CALL cbx_diagnostico.addItem(2,"RECHAZADO")}
        CALL cbx_diagnostico.addItem(NULL,"TODAS")

        INPUT BY NAME  v_nss,v_modalidad,v_marca,v_fecha_inicio,v_fecha_fin,v_diagnostico
              ATTRIBUTES(UNBUFFERED,CANCEL = 0)

            ON ACTION ACCEPT
                --Se valida que el NSS sea un numero de 11 digitos
                IF NOT  fn_numero_valido(v_nss) OR (length(v_nss) <> 11 AND length(v_nss) > 0) THEN
                    CALL fn_mensaje("Atención",
                                    "El NSS debe ser un numero de 11 digitos.",
                                    "stop")
                    NEXT FIELD v_nss
                END IF

                CALL fn_busca_solicitudes(v_nss,v_modalidad,v_marca,v_fecha_inicio,
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
FUNCTION fn_busca_solicitudes(p_nss,p_modalidad,p_marca,p_fecha_inicio,p_fecha_fin,p_diagnostico)

    -- Parametros para realizar la busqueda
    DEFINE p_nss            CHAR(11)
    DEFINE p_modalidad      SMALLINT
    DEFINE p_marca          SMALLINT
    DEFINE p_fecha_inicio   DATE
    DEFINE p_fecha_fin      DATE
    DEFINE p_diagnostico    SMALLINT

    -- Resultado de la busqueda
    DEFINE v_detalle_solicitud  DYNAMIC ARRAY OF RECORD
                nss             CHAR(11)               ,
                marca           CHAR(50)               ,
                tipo            CHAR(10)               ,
                fecha           DATE                   ,
                diagnostico     CHAR(80)               ,
                hora_consulta   DATETIME HOUR TO SECOND,
                viv92_aivs      DECIMAL(18,2)          ,
                viv97_aivs      DECIMAL(18,2)          ,
                viv92_pesos     DECIMAL(12,2)          ,
                viv97_pesos     DECIMAL(12,2)          ,
                tpo_cambio      DECIMAL(8,5)
    END RECORD

    -- Variables auxiliares
    DEFINE v_contador       INTEGER
    DEFINE v_sql            STRING
    DEFINE v_string         base.StringBuffer
    DEFINE v_tipo           STRING
    DEFINE v_diagnostico    CHAR(10)

    LET v_string = base.StringBuffer.create()
    

    -- Se hace el cruce entre ret_ley73_generico y sfr_marca_activa para buscar
    -- Marca. De sfr_marca se obtiene la descripcion y de afi_derechohabiente el
    -- NSS.
    -- De ret_ctr_marca_procesar se obtienen los intentos de marca
    LET v_sql = "SELECT ad.nss,sm.marca||\" - \"||sm.descripcion_marca AS marca_desc,       "||   
                "       rcp.indicador_marca,rcp.f_intento,rcp.estado_cuenta,rcp.h_intento,   "||
                "       rlg.aivs_viv92,rlg.aivs_viv97,rlg.importe_viv92,rlg.importe_viv97,0 "||
                "FROM   ret_ley73_generico     rlg,                                         "||
                "       afi_derechohabiente    ad ,                                         "||
                "       sfr_marca_historica    smh,                                         "||
                "       sfr_marca              sm ,                                         "||
                "       ret_ctr_marca_procesar rcp                                          "||
                "WHERE  rlg.gpo_ley73          = 1                                          "||
                "  AND  rlg.id_derechohabiente = ad.id_derechohabiente                      "

    -- Se verifica si se introdujo un nss
    IF p_nss IS NOT NULL THEN
        LET v_sql = v_sql||" AND ad.nss = "||p_nss
    END IF

    LET v_sql = v_sql||"  AND  rlg.id_solicitud = smh.n_referencia "
    LET v_sql = v_sql||"  AND  rlg.id_derechohabiente = smh.id_derechohabiente "
    LET v_sql = v_sql||"  AND  rlg.id_solicitud = rcp.id_solicitud "

    -- Se verifica la modalidad
    -- Si tiene f_fin NULA quiere decir que tiene marca activa
    -- En ret_ctr_marca_procesar indicador_marca = 1 significa marca
    -- y indicador_marca = 2 significa desmarca
    IF p_modalidad IS NOT NULL THEN
        --Marca
        IF p_modalidad = 1 THEN
            --LET v_sql = v_sql||"  AND  smh.f_fin IS NULL "
            LET v_sql = v_sql||"  AND  indicador_marca = 1"
        -- Desmarca
        ELSE
            --LET v_sql = v_sql||"  AND  smh.f_fin IS NOT NULL "
            LET v_sql = v_sql||"  AND  indicador_marca = 2"
        END IF
    ELSE
        LET v_sql = v_sql||"  AND  indicador_marca IN (1,2)"
    END IF

    -- Se verifica si se introdujo fecha inicial
    IF p_fecha_inicio IS NOT NULL THEN
        IF p_fecha_fin IS NOT NULL THEN 
            LET v_sql = v_sql||" AND (rcp.f_intento BETWEEN '"||p_fecha_inicio||
                        "' AND '"||p_fecha_fin||"') "
        ELSE
            LET v_sql = v_sql||" AND rcp.f_intento >= '"||p_fecha_inicio||"' "
        END IF
    ELSE
        IF p_fecha_fin IS NOT NULL THEN 
            LET v_sql = v_sql||" AND rcp.f_intento <= '"||p_fecha_fin||"' "
        END IF
    END IF

    LET v_sql = v_sql||"  AND  smh.marca              = sm.marca   "
    
    -- Se verifica si se introdujo marca
    IF p_marca IS NOT NULL THEN
        LET v_sql = v_sql||" AND smh.marca = "||p_marca
    ELSE
        LET v_sql = v_sql||" AND smh.marca IN(803,815) "
    END IF

    -- Se verifica el diagnostico
    IF p_diagnostico IS NOT NULL THEN
        LET v_sql = v_sql||" AND rcp.diagnostico   = 101"
        LET v_sql = v_sql||" AND rcp.estado_cuenta = "||p_diagnostico
    END IF

    LET v_sql = v_sql||" ORDER BY nss,rcp.f_intento,rcp.h_intento" --,rcp.indicador_marca,rcp.estado_cuenta"

    DISPLAY v_sql

    PREPARE prp_detalle_solicitudes FROM v_sql
    DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes

    LET v_contador = 1

    FOREACH cur_detalle_solicitudes INTO v_detalle_solicitud[v_contador].*

        -- Se obtiene el tipon de cambio
        IF (v_detalle_solicitud[v_contador].viv92_aivs + v_detalle_solicitud[v_contador].viv97_aivs) > 0 THEN
            LET v_detalle_solicitud[v_contador].tpo_cambio =
                (v_detalle_solicitud[v_contador].viv92_pesos + v_detalle_solicitud[v_contador].viv97_pesos)/ 
                (v_detalle_solicitud[v_contador].viv92_aivs  + v_detalle_solicitud[v_contador].viv97_aivs )
        END IF

        -- Se elimina los espacios en blanco
        CALL v_string.clear()
        CALL v_string.append(v_detalle_solicitud[v_contador].tipo)
        CALL v_string.replace(" ","",0)
        LET  v_tipo = v_string.toString()
        CALL v_string.clear()
        CALL v_string.append(v_detalle_solicitud[v_contador].diagnostico)
        CALL v_string.replace(" ","",0)
        LET  v_diagnostico = v_string.toString()

        LET v_detalle_solicitud[v_contador].diagnostico = fn_diagnostico_desc(v_diagnostico)

        -- Se pone el indicador de marca
        IF v_tipo = "1" THEN
            LET v_detalle_solicitud[v_contador].tipo = "Marca"               
        ELSE
            IF v_tipo = "2" THEN
                LET v_detalle_solicitud[v_contador].tipo = "Desmarca"
            END IF
        END IF

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
                nss             CHAR(11)               ,
                marca           CHAR(50)               ,
                tipo            CHAR(10)               ,
                fecha           DATE                   ,
                diagnostico     CHAR(80)               ,
                hora_consulta   DATETIME HOUR TO SECOND,
                viv92_aivs      DECIMAL(18,2)          ,
                viv97_aivs      DECIMAL(18,2)          ,
                viv92_pesos     DECIMAL(12,2)          ,
                viv97_pesos     DECIMAL(12,2)          ,
                tpo_cambio      DECIMAL(8,5)
    END RECORD

    -- Variables auxiliares
    DEFINE v_channel_sal    base.Channel -- Creacion del archivo de salida
    DEFINE v_texto          STRING
    DEFINE v_archivo        STRING
    DEFINE v_ruta_env_arch  LIKE seg_modulo.ruta_envio -- Ruta de envio
    DEFINE v_ruta_arch      STRING -- Ruta de archivo
    DEFINE v_contador       INTEGER
    DEFINE v_string         base.StringBuffer
    DEFINE v_tipo_cambio    DECIMAL(8,5)
    DEFINE v_hora_consulta  DATE
    

    --Se obtiene la ruta de envio y ejecutable
    SELECT ruta_envio, ruta_bin
    INTO   v_ruta_env_arch
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- Se prepara el archivo de salida
    LET v_channel_sal = base.Channel.create()

    LET v_string = base.StringBuffer.create()
    LET v_archivo = TIME(CURRENT)
    CALL v_string.append(v_archivo)
    CALL v_string.replace(":","",2)

    -- Establecemos el nombre del archivo a partir de la fecha
    LET v_archivo = "consulta_marca_desmarca_"||(TODAY USING "ddmmyyyy_")||v_string.toString()||".rmdnp"

    LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/",v_archivo
    CALL v_channel_sal.openFile(v_ruta_arch,"w")

    -- Se escribe el encabezado
    LET v_texto = "NSS|"                          ||
                  "Código y descripción de marca|"||
                  "Marcaje/Desmarcaje|"           ||
                  "Fecha de proceso de marca|"    ||
                  "Diagnostico de marca|"         ||
                  "Hora de consulta|"             ||
                  "AIVS 92|"                      ||
                  "AIVS 97|"                      ||
                  "PESOS 92|"                     ||
                  "PESOS 97|"                     ||
                  "Tipo de cambio informado "
    CALL v_channel_sal.writeLine(v_texto)

    -- Se escribe el detalle
    FOR v_contador = 1 TO p_detalle_solicitud.getLength()

        LET v_texto =  p_detalle_solicitud[v_contador].nss                                   ,"|",
                       p_detalle_solicitud[v_contador].marca                                 ,"|",
                       p_detalle_solicitud[v_contador].tipo                                  ,"|",
                      (p_detalle_solicitud[v_contador].fecha         USING "dd/mm/yyyy"     ),"|",
                       p_detalle_solicitud[v_contador].diagnostico                           ,"|",
                       p_detalle_solicitud[v_contador].hora_consulta                         ,"|",
                      (p_detalle_solicitud[v_contador].viv92_aivs    USING "&&&&&&&&&&.&&&&"),"|",
                      (p_detalle_solicitud[v_contador].viv97_aivs    USING "&&&&&&&&&&.&&&&"),"|",
                      (p_detalle_solicitud[v_contador].viv92_pesos   USING "&&&&&&&&&&.&&"  ),"|",
                      (p_detalle_solicitud[v_contador].viv97_pesos   USING "&&&&&&&&&&.&&"  ),"|",
                      (p_detalle_solicitud[v_contador].tpo_cambio    USING "&&&.&&&&&"      )
        CALL v_channel_sal.writeLine(v_texto)
    END FOR

    CALL v_channel_sal.close()

    CALL v_string.clear()
    CALL v_string.append(v_ruta_env_arch)
    CALL v_string.replace(" ","",0)

    LET v_texto = v_string.toString()

    CALL fn_mensaje("Atención","Ruta de archivo de salida: "||v_texto||"/"||v_archivo,"stop")

END FUNCTION

#Funcion que regresa la descripcion del diagnostico
FUNCTION fn_diagnostico_desc(p_diagnostico)

    DEFINE p_diagnostico CHAR(10)

    CASE p_diagnostico
        WHEN "101"
            RETURN "101-101 Aceptada"
        WHEN "102"
            RETURN "101-102 NSS no se encuentra registrado en la BDNSAR"
        WHEN "201"
            RETURN "101-201 La cuenta se encuentra en proceso de 43 BIS"
        WHEN "202"
            RETURN "101-202 En proceso de Traspaso A-A"
        WHEN "203"
            RETURN "101-203 En proceso de Unificacion de cuentas"
        WHEN "204"
            RETURN "101-204 En proceso de Fusion de Afore"
        WHEN "205"
            RETURN "101-205 En proceso de separacion de cuentas"
        WHEN "207"
            RETURN "101-207 En proceso de transferencias de recursos"
        WHEN "208"
            RETURN "101-208 En proceso de disposicion de recursos"
        WHEN "211"
            RETURN "101-211 En proceso de Devolucion de pagos efectuados sin Justificacion Legal"
        WHEN "212"
            RETURN "101-212 En proceso de retiros parciales"
        WHEN "213"
            RETURN "101-213 En proceso de tramite judicial iniciado por Afore"
        WHEN "216"
            RETURN "101-216 Cuenta en proceso de aclaracion por conciliacion"
        WHEN "217"
            RETURN "101-217 Cuenta en proceso de seleccion SIEFORE"
        WHEN "219"
            RETURN "101-219 Cuenta en proceso de modificación"
        WHEN "220"
            RETURN "101-220 Cuenta en proceso de crédito de vivienda"
        WHEN "221"
            RETURN "101-221 Cuenta en proceso de crédito de vivienda"
        WHEN "223"
            RETURN "101-223 Cuenta en proceso de saldo previo"
        WHEN "224"
            RETURN "101-224 No se encuentra marcado"
        WHEN "225"
            RETURN "101-225 Existe alguna notificación de pago por Ventanilla INFONAVIT"
        WHEN "226"
            RETURN "101-226 Existe alguna notificación de pago por Ventanilla INFONAVIT"
        OTHERWISE
            RETURN ""
    END CASE

END FUNCTION