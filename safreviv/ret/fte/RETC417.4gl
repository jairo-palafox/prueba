################################################################################
#Modulo        => RET                                                          #
#Programa      => RETC417                                                      #
#Ojetivo       => Opción para generar Reporte Solicitudes Pendientes por Pagar #
#                 con Estatus que estén en trámite para su devolución indicando#
#                 el estatus de la Solicitud                                   #
#Fecha inicio  => Julio, 2016.                                                 #
#Requerimiento => CARINFXVI-13                                                 #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
IMPORT os

DATABASE safre_viv

DEFINE g_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
DEFINE g_tipo_ejecucion     SMALLINT                     -- forma como ejecutara el programa
DEFINE g_s_titulo           STRING                       -- titulo de la ventana
DEFINE g_ruta_bin           LIKE seg_modulo.ruta_bin

MAIN

    DEFINE v_ruta_bitacora  LIKE seg_modulo.ruta_bitacora
    DEFINE v_archivo_log    STRING
    DEFINE v_programa       STRING

    -- se obtienen los parametros de ejecucion
    LET g_usuario_cod    = ARG_VAL(1)
    LET g_tipo_ejecucion = ARG_VAL(2)
    LET g_s_titulo       = ARG_VAL(3)

    LET v_programa = "RETC417"

    SELECT ruta_bin
    INTO   g_ruta_bin
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    SELECT ruta_bitacora
    INTO   v_ruta_bitacora
    FROM   seg_modulo
    WHERE  modulo_cod = 'bat'

    -- Se crea el log
    LET v_archivo_log = v_ruta_bitacora CLIPPED,"/",g_usuario_cod CLIPPED,".",
                        v_programa,".log"
    CALL STARTLOG(v_archivo_log)

    CALL fn_ventana_principal()

END MAIN

# Funcion con la pantalla principal de consulta (Ingreso de parametros)
FUNCTION fn_ventana_principal()

    -- Parametros de consulta
    DEFINE v_nss            LIKE afi_derechohabiente.nss
    DEFINE v_caso_adai      LIKE ret_solicitud_generico.caso_adai
    DEFINE v_estado_sol     LIKE ret_estado_solicitud.estado_solicitud
    DEFINE v_fecha_sol_ini  DATE
    DEFINE v_fecha_sol_fin  DATE

    -- Variables auxiliares
    DEFINE cbx_estado_sol   ui.ComboBox
    DEFINE v_estado_rec     RECORD
        estado_sol      LIKE ret_estado_solicitud.estado_solicitud,
        des_corta       LIKE ret_estado_solicitud.des_corta
    END RECORD
    DEFINE v_raiz_ventana   STRING
    DEFINE v_ventana        ui.Window
    DEFINE v_validos        SMALLINT
    DEFINE v_mensaje        STRING
    DEFINE v_query  STRING

    LET v_query = "SELECT estado_solicitud,des_corta
                   FROM   ret_estado_solicitud
                   WHERE  estado_solicitud IN (8,10,15,50,60,70,700)"
    PREPARE prp_estados_sol FROM v_query
    DECLARE cur_estados_sol CURSOR FOR prp_estados_sol

    LET v_raiz_ventana = g_ruta_bin CLIPPED,"/RETC4171"

    OPEN WINDOW w_consulta WITH FORM v_raiz_ventana

        IF g_s_titulo IS NOT NULL THEN
            CALL ui.Interface.setText(g_s_titulo)
            LET v_ventana = ui.Window.getCurrent()
            CALL v_ventana.setText(g_s_titulo)
        END IF

        DIALOG ATTRIBUTES(UNBUFFERED)

            INPUT BY NAME v_nss,v_caso_adai,v_estado_sol,
                          v_fecha_sol_ini,v_fecha_sol_fin
            END INPUT

            ON ACTION ACCEPT
                CALL fn_parametros_validos(v_nss,v_caso_adai,v_estado_sol,
                                           v_fecha_sol_ini,v_fecha_sol_fin)
                     RETURNING v_validos,v_mensaje
                IF v_validos THEN
                    CALL fn_consulta_solicitud(v_nss,v_caso_adai,v_estado_sol,
                                               v_fecha_sol_ini,v_fecha_sol_fin)
                ELSE
                    CALL fn_mensaje("Atención",
                                    v_mensaje,
                                    "stop")
                END IF -- fn_parametros_validos

            ON ACTION restablecer
                LET v_nss           = NULL
                LET v_caso_adai     = NULL
                LET v_estado_sol    = NULL
                LET v_fecha_sol_ini = NULL
                LET v_fecha_sol_fin = NULL

            ON ACTION salir
                EXIT DIALOG

            BEFORE DIALOG
                LET v_nss           = NULL
                LET v_caso_adai     = NULL
                LET v_estado_sol    = NULL
                LET v_fecha_sol_ini = NULL
                LET v_fecha_sol_fin = NULL
                LET cbx_estado_sol  = ui.ComboBox.forName('v_estado_sol')
                CALL cbx_estado_sol.clear()
                FOREACH cur_estados_sol INTO v_estado_rec.*
                    CALL cbx_estado_sol.addItem(v_estado_rec.estado_sol,
                                                v_estado_rec.estado_sol||" - "||v_estado_rec.des_corta CLIPPED)
                END FOREACH
                CALL cbx_estado_sol.addItem(NULL,'TODOS')

        END DIALOG

    CLOSE WINDOW w_consulta

END FUNCTION -- fn_ventana_principal

# Funcion que valida que una cadena es un numero
FUNCTION fn_numero_valido(p_cadena,p_longitud)

    DEFINE p_cadena     STRING
    DEFINE p_longitud   SMALLINT

    DEFINE v_numero     BIGINT

    LET p_cadena = p_cadena.trim()

    IF NOT (p_cadena.getLength() > 0) THEN
        RETURN FALSE
    ELSE
        -- Si el nss no tiene 11 digitos
        IF p_cadena.getLength() <> p_longitud THEN
            RETURN FALSE
        ELSE
            TRY
                LET v_numero = p_cadena
                RETURN TRUE
            -- El nss no es un numero
            CATCH
                RETURN FALSE
            END TRY
        END IF
    END IF

END FUNCTION -- fn_nss_valido

# Funcion que valida los parametros
FUNCTION fn_parametros_validos(p_nss,p_caso_adai,p_estado_sol,
                               p_fecha_sol_ini,p_fecha_sol_fin)

    -- Parametros
    DEFINE p_nss            LIKE afi_derechohabiente.nss
    DEFINE p_caso_adai      LIKE ret_solicitud_generico.caso_adai
    DEFINE p_estado_sol     LIKE ret_estado_solicitud.estado_solicitud
    DEFINE p_fecha_sol_ini  DATE
    DEFINE p_fecha_sol_fin  DATE

    -- Valores devueltos
    DEFINE r_estado     SMALLINT
    DEFINE r_mensaje    STRING

    LET r_estado  = TRUE
    LET r_mensaje = ""

    {IF p_nss           IS NULL AND
       p_caso_adai     IS NULL AND
       p_estado_sol    IS NULL AND
       p_fecha_sol_ini IS NULL AND
       p_fecha_sol_fin IS NULL THEN

        LET r_estado  = FALSE
        LET r_mensaje = "Debe ingresar un parametro de busqueda."

    ELSE}

        -- Falta una de las dos fechas
        IF (p_fecha_sol_ini IS NULL     AND p_fecha_sol_fin IS NOT NULL) OR
           (p_fecha_sol_ini IS NOT NULL AND p_fecha_sol_fin IS NULL    ) THEN

            LET r_estado  = FALSE
            LET r_mensaje = "Para buscar por fechas, se deben especificar ambas."

        ELSE -- Estan ambas fechas

            IF p_fecha_sol_ini > p_fecha_sol_fin THEN
                LET r_estado  = FALSE
                LET r_mensaje = "La fecha Final no puede ser mayor a la Inicial."
            ELSE -- Si la fecha final es mayor a la inicial
                IF p_fecha_sol_fin > TODAY THEN
                    LET r_estado  = FALSE
                    LET r_mensaje = "La fecha Final no puede ser mayor al día de consulta."
                END IF
            END IF -- p_fecha_ini > p_fecha_fin

        END IF -- p_fecha_ini IS NULL OR

        IF p_nss IS NOT NULL THEN
            IF NOT fn_numero_valido(p_nss,11) THEN
                LET r_estado  = FALSE
                LET r_mensaje = "El NSS debe ser un numero de 11 digitos."
            END IF -- fn_nss_valido(p_nss)
        END IF -- p_nss IS NOT NULL

        IF p_caso_adai IS NOT NULL THEN
            IF NOT fn_numero_valido(p_caso_adai,10) THEN
                LET r_estado  = FALSE
                LET r_mensaje = "Caso ADAI invalido."
            END IF -- fn_nss_valido(p_nss)
        END IF -- p_nss IS NOT NULL

    {END IF -- p_nss IS NULL AND}

    RETURN r_estado,r_mensaje

END FUNCTION -- fn_parametros_validos

# Funcion que obtiene los datos de consulta de acuerdo a los parametros usados
FUNCTION fn_obten_datos(p_nss,p_caso_adai,p_estado_sol,
                        p_fecha_sol_ini,p_fecha_sol_fin)

    -- Parametros
    DEFINE p_nss            LIKE afi_derechohabiente.nss
    DEFINE p_caso_adai      LIKE ret_solicitud_generico.caso_adai
    DEFINE p_estado_sol     LIKE ret_estado_solicitud.estado_solicitud
    DEFINE p_fecha_sol_ini  DATE
    DEFINE p_fecha_sol_fin  DATE

    DEFINE v_query      STRING
    DEFINE v_where      STRING
    DEFINE v_contador   INTEGER

    DEFINE r_datos_consulta DYNAMIC ARRAY OF RECORD
        id_solicitud        LIKE ret_solicitud_generico.id_solicitud,
        modalidad_retiro    CHAR(120),
        num_credito         LIKE cre_acreditado.num_credito,
        nss                 LIKE ret_solicitud_generico.nss,
        caso_adai           LIKE ret_solicitud_generico.caso_adai,
        f_solicitud         LIKE ret_solicitud_generico.f_solicitud,
        total_importe       LIKE ret_amort_excedente.total_importe,
        estado_solicitud    CHAR(120),
        referencia          LIKE ret_respuesta_fico.referencia,
        cta_clabe           LIKE ret_respuesta_fico.cta_clabe
    END RECORD

    LET v_query = "SELECT FIRST 1 ca.num_credito
                   FROM   cre_acreditado      ca,
                          afi_derechohabiente ad,
                          cat_maq_credito     cm
                   WHERE  ad.nss                = ?
                     AND  ca.id_derechohabiente = ad.id_derechohabiente
                     AND  ca.estado             = cm.estado"
    PREPARE prp_num_credito FROM v_query

    LET v_where = ""

    IF p_nss IS NOT NULL THEN
        LET v_where = "  AND  rsg.nss = '",p_nss,"' "
    END IF -- p_nss IS NOT NULL
    
    IF p_caso_adai IS NOT NULL THEN
        LET v_where = v_where,"  AND  rsg.caso_adai = '",p_caso_adai,"' "
    END IF -- p_caso_adai IS NOT NULL

    IF p_estado_sol IS NOT NULL THEN
        LET v_where = v_where,"  AND  rsg.estado_solicitud = ",p_estado_sol," "
    ELSE
        LET v_where = v_where,"  AND  rsg.estado_solicitud IN(8,10,15,50,60,70,700)"
    END IF -- p_estado_sol IS NOT NULL

    IF p_fecha_sol_ini IS NOT NULL THEN
        LET v_where = v_where,"  AND  rsg.f_solicitud >= ",
                      "TO_DATE('",(p_fecha_sol_ini USING "yyyy/mm/dd"),"','%Y/%m/%d') ",
                      "  AND  rsg.f_solicitud <= ",
                      "TO_DATE('",(p_fecha_sol_fin USING "yyyy/mm/dd"),"','%Y/%m/%d') "
    END IF -- p_fecha_sol_ini IS NOT NULL

    LET v_query = "SELECT rsg.id_solicitud,
                          TRIM(rmr.modalidad_retiro||' - '||rmr.des_larga),
                          NULL::DECIMAL AS num_credito,rsg.nss,rsg.caso_adai,
                          rsg.f_solicitud,rae.total_importe,
                          TRIM(rsg.estado_solicitud||' - '||res.des_corta),
                          rrf.referencia,rrf.cta_clabe
                   FROM   ret_solicitud_generico rsg
                          LEFT JOIN ret_respuesta_fico  rrf
                                 ON rrf.referencia   = rsg.id_solicitud
                          LEFT JOIN ret_amort_excedente rae
                                 ON rae.id_solicitud = rsg.id_solicitud,
                          ret_modalidad_retiro   rmr,
                          ret_estado_solicitud   res 
                   WHERE  rsg.modalidad_retiro  = 9 ",
                   v_where,
                  "  AND  rmr.modalidad_retiro  = rsg.modalidad_retiro
                     AND  res.estado_solicitud  = rsg.estado_solicitud
                   ORDER  BY rsg.estado_solicitud,rsg.nss"

    PREPARE prp_sol_pendientes FROM v_query
    DECLARE cur_sol_pendientes CURSOR FOR prp_sol_pendientes

    LET v_contador = 1
    FOREACH cur_sol_pendientes INTO r_datos_consulta[v_contador].*

        EXECUTE prp_num_credito USING r_datos_consulta[v_contador].nss
                                INTO  r_datos_consulta[v_contador].num_credito

        LET v_contador = v_contador + 1

    END FOREACH -- cur_sol_pendientes

    CALL r_datos_consulta.deleteElement(v_contador)

    RETURN r_datos_consulta

END FUNCTION -- fn_obten_datos

# Funcion que valida los parametros
FUNCTION fn_consulta_solicitud(p_nss,p_caso_adai,p_estado_sol,
                               p_fecha_sol_ini,p_fecha_sol_fin)

    -- Parametros
    DEFINE p_nss            LIKE afi_derechohabiente.nss
    DEFINE p_caso_adai      LIKE ret_solicitud_generico.caso_adai
    DEFINE p_estado_sol     LIKE ret_estado_solicitud.estado_solicitud
    DEFINE p_fecha_sol_ini  DATE
    DEFINE p_fecha_sol_fin  DATE

    DEFINE v_datos_consulta DYNAMIC ARRAY OF RECORD
        id_solicitud        LIKE ret_solicitud_generico.id_solicitud,
        modalidad_retiro    CHAR(120),
        num_credito         LIKE cre_acreditado.num_credito,
        nss                 LIKE ret_solicitud_generico.nss,
        caso_adai           LIKE ret_solicitud_generico.caso_adai,
        f_solicitud         LIKE ret_solicitud_generico.f_solicitud,
        total_importe       LIKE ret_amort_excedente.total_importe,
        estado_solicitud    CHAR(120),
        referencia          LIKE ret_respuesta_fico.referencia,
        cta_clabe           LIKE ret_respuesta_fico.cta_clabe
    END RECORD

    DEFINE v_estado         SMALLINT
    DEFINE v_mensaje        STRING
    DEFINE v_nom_archivo    STRING
    DEFINE v_raiz_ventana   STRING
    DEFINE v_ventana        ui.Window

    CALL fn_obten_datos(p_nss,p_caso_adai,p_estado_sol,p_fecha_sol_ini,p_fecha_sol_fin)
         RETURNING v_datos_consulta

    IF v_datos_consulta.getLength() = 0 THEN
        CALL fn_mensaje("Atención",
                    "No hay datos para mostrar.",
                    "stop")
        RETURN
    END IF -- v_datos_consulta.getLength() = 0

    LET v_raiz_ventana = g_ruta_bin CLIPPED,"/RETC4172"

    OPEN WINDOW w_detalle WITH FORM v_raiz_ventana

        IF g_s_titulo IS NOT NULL THEN
            CALL ui.Interface.setText("Detalle "||g_s_titulo)
            LET v_ventana = ui.Window.getCurrent()
            CALL v_ventana.setText("Detalle "||g_s_titulo)
        END IF

        DISPLAY ARRAY v_datos_consulta TO detalle.*
                ATTRIBUTES(CANCEL=FALSE,ACCEPT=FALSE)

            ON ACTION exportar
                CALL fn_archivo_plano(v_datos_consulta) RETURNING v_nom_archivo
                CALL fn_mensaje("Atención",
                                "Archivo generado: "||v_nom_archivo CLIPPED,
                                "stop")

            ON ACTION reporte
                CALL fn_genera_reporte_pdf(p_nss,p_caso_adai,p_estado_sol,
                                           p_fecha_sol_ini,p_fecha_sol_fin,
                                           v_datos_consulta)
                     RETURNING v_estado,v_mensaje
                IF NOT v_estado THEN
                    CALL fn_mensaje("Atención",
                                    v_mensaje,
                                    "stop")
                END IF -- NOT v_estado

            ON ACTION regresar
                EXIT DISPLAY

        END DISPLAY

    CLOSE WINDOW w_detalle

END FUNCTION -- fn_consulta_solicitud

# Funcion que genera un archivo plano con los datos de la consulta
FUNCTION fn_archivo_plano(p_datos_consulta)

    -- Parametros
    DEFINE p_datos_consulta DYNAMIC ARRAY OF RECORD
        id_solicitud        LIKE ret_solicitud_generico.id_solicitud,
        modalidad_retiro    CHAR(120),
        num_credito         LIKE cre_acreditado.num_credito,
        nss                 LIKE ret_solicitud_generico.nss,
        caso_adai           LIKE ret_solicitud_generico.caso_adai,
        f_solicitud         LIKE ret_solicitud_generico.f_solicitud,
        total_importe       LIKE ret_amort_excedente.total_importe,
        estado_solicitud    CHAR(120),
        referencia          LIKE ret_respuesta_fico.referencia,
        cta_clabe           LIKE ret_respuesta_fico.cta_clabe
    END RECORD

    DEFINE v_contador       INTEGER
    DEFINE v_ruta_envio     LIKE seg_modulo.ruta_envio
    DEFINE v_nom_archivo    STRING
    DEFINE v_linea          STRING
    DEFINE v_channel        base.Channel
    DEFINE v_string_buffer  base.StringBuffer

    DEFINE r_ruta_archivo STRING

    SELECT ruta_envio
    INTO   v_ruta_envio
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    LET  v_nom_archivo    = CURRENT HOUR TO SECOND
    LET  v_string_buffer = base.StringBuffer.create()
    CALL v_string_buffer.append(v_nom_archivo)
    CALL v_string_buffer.replace(":","",0)
    LET  v_nom_archivo = TODAY USING "ddmmyyyy","_",v_string_buffer.toString()

    LET  r_ruta_archivo = v_ruta_envio CLIPPED,"/",v_nom_archivo,".txt"
    LET  v_channel = base.Channel.create()
    CALL v_channel.openFile(r_ruta_archivo,"w")
    CALL v_channel.setDelimiter(",")

    FOR v_contador = 1 TO p_datos_consulta.getLength()

        LET v_linea = p_datos_consulta[v_contador].id_solicitud,            "|",
                      p_datos_consulta[v_contador].modalidad_retiro CLIPPED,"|",
                      p_datos_consulta[v_contador].num_credito,             "|",
                      p_datos_consulta[v_contador].nss,                     "|",
                      p_datos_consulta[v_contador].caso_adai,               "|",
                      p_datos_consulta[v_contador].f_solicitud,             "|",
                      p_datos_consulta[v_contador].total_importe,           "|",
                      p_datos_consulta[v_contador].estado_solicitud CLIPPED,"|",
                      p_datos_consulta[v_contador].referencia,              "|",
                      p_datos_consulta[v_contador].cta_clabe
        CALL v_channel.writeLine(v_linea)

    END FOR

    CALL v_channel.close()

    RETURN r_ruta_archivo

END FUNCTION -- fn_archivo_plano

# Funcion que genera el reporte de la consulta
FUNCTION fn_genera_reporte_pdf(p_nss,p_caso_adai,p_estado_sol,
                               p_fecha_sol_ini,p_fecha_sol_fin,p_datos_consulta)

    -- Parametros
    DEFINE p_nss            LIKE afi_derechohabiente.nss
    DEFINE p_caso_adai      LIKE ret_solicitud_generico.caso_adai
    DEFINE p_estado_sol     LIKE ret_estado_solicitud.estado_solicitud
    DEFINE p_fecha_sol_ini  DATE
    DEFINE p_fecha_sol_fin  DATE

    DEFINE p_datos_consulta DYNAMIC ARRAY OF RECORD
        id_solicitud        LIKE ret_solicitud_generico.id_solicitud,
        modalidad_retiro    CHAR(120),
        num_credito         LIKE cre_acreditado.num_credito,
        nss                 LIKE ret_solicitud_generico.nss,
        caso_adai           LIKE ret_solicitud_generico.caso_adai,
        f_solicitud         LIKE ret_solicitud_generico.f_solicitud,
        total_importe       LIKE ret_amort_excedente.total_importe,
        estado_solicitud    CHAR(120),
        referencia          LIKE ret_respuesta_fico.referencia,
        cta_clabe           LIKE ret_respuesta_fico.cta_clabe
    END RECORD

    DEFINE v_estado_desc    CHAR(120)

    DEFINE v_ruta_envio     LIKE seg_modulo.ruta_envio
    DEFINE v_manejador_rpt  om.SaxDocumentHandler
    DEFINE v_contador       INTEGER
    DEFINE v_fecha_hoy      DATE
    DEFINE v_logo           STRING

    -- Valor de retorno
    DEFINE r_estado         SMALLINT
    DEFINE r_mensaje        STRING

    -- Se recupera la ruta
    SELECT ruta_envio
    INTO   v_ruta_envio
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    IF p_estado_sol IS NOT NULL THEN
        SELECT des_corta
        INTO   v_estado_desc
        FROM   ret_estado_solicitud
        WHERE  estado_solicitud = p_estado_sol
    END IF

    LET r_estado     = TRUE
    LET r_mensaje    = ""

    LET v_logo        = os.Path.pwd(),"/../../img/logo_infonavit_pequeno.jpg"
    LET v_fecha_hoy   = TODAY
    LET v_estado_desc = v_estado_desc CLIPPED

    IF fgl_report_loadCurrentSettings("RETC4171.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
        CALL fgl_report_selectPreview(TRUE)
        -- CALL fgl_report_setOutputFileName(r_ruta_reporte)
        LET v_manejador_rpt = fgl_report_commitCurrentSettings()

        START REPORT reporte_consulta TO XML HANDLER v_manejador_rpt

        FOR v_contador = 1 TO p_datos_consulta.getLength()

            OUTPUT TO REPORT reporte_consulta(g_usuario_cod,
                                              v_fecha_hoy,
                                              v_logo,
                                              p_nss,
                                              p_caso_adai,
                                              v_estado_desc,
                                              p_fecha_sol_ini,
                                              p_fecha_sol_fin,
                                              p_datos_consulta[v_contador].*)

        END FOR

        FINISH REPORT reporte_consulta

    ELSE
        LET r_estado  = FALSE
        LET r_mensaje = "No se puede leer la plantilla del reporte RETC4171.4rp"
    END IF -- fgl_report_loadCurrentSettings("RETC0201.4rp")

    RETURN r_estado,r_mensaje

END FUNCTION -- fn_genera_reporte

# Reporte en pdf de la consulta
REPORT reporte_consulta(p_usuario_cod,p_fecha,p_logo,p_nss,p_caso_adai,p_estado_desc,
                        p_fecha_sol_ini,p_fecha_sol_fin,p_datos_consulta)

    -- Parametros
    DEFINE p_usuario_cod    CHAR(20)
    DEFINE p_fecha          DATE
    DEFINE p_logo           STRING
    DEFINE p_nss            LIKE afi_derechohabiente.nss
    DEFINE p_caso_adai      LIKE ret_solicitud_generico.caso_adai
    DEFINE p_estado_desc    STRING
    DEFINE p_fecha_sol_ini  DATE
    DEFINE p_fecha_sol_fin  DATE

    DEFINE p_datos_consulta RECORD
        id_solicitud        LIKE ret_solicitud_generico.id_solicitud,
        modalidad_retiro    CHAR(120),
        num_credito         LIKE cre_acreditado.num_credito,
        nss                 LIKE ret_solicitud_generico.nss,
        caso_adai           LIKE ret_solicitud_generico.caso_adai,
        f_solicitud         LIKE ret_solicitud_generico.f_solicitud,
        total_importe       LIKE ret_amort_excedente.total_importe,
        estado_solicitud    CHAR(120),
        referencia          LIKE ret_respuesta_fico.referencia,
        cta_clabe           LIKE ret_respuesta_fico.cta_clabe
    END RECORD

    FORMAT
        FIRST PAGE HEADER
            PRINTX p_usuario_cod
            PRINTX p_fecha
            PRINTX p_logo
            PRINTX p_nss
            PRINTX p_caso_adai
            PRINTX p_estado_desc
            PRINTX p_fecha_sol_ini
            PRINTX p_fecha_sol_fin

    ON EVERY ROW
        PRINT p_datos_consulta.id_solicitud
        PRINT p_datos_consulta.modalidad_retiro
        PRINT p_datos_consulta.num_credito
        PRINT p_datos_consulta.nss
        PRINT p_datos_consulta.caso_adai
        PRINT p_datos_consulta.f_solicitud
        PRINT p_datos_consulta.total_importe
        PRINT p_datos_consulta.estado_solicitud
        PRINT p_datos_consulta.referencia
        PRINT p_datos_consulta.cta_clabe

END REPORT -- reporte_consulta