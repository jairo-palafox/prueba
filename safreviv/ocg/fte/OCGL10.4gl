################################################################################
#Modulo        => OCG                                                          #
#Programa      => OCGL10                                                       #
#Ojetivo       => Programa lanzador, desde la aplicacion, para generar el      #
#                 archivo maestro de acreditados
#Fecha inicio  => Marzo, 2016.                                                 #
#Requerimiento => 
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv

MAIN

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario
    DEFINE p_nom_ventana        STRING
    DEFINE p_tpo_ejecucion      SMALLINT

    -- Datos del proceso
    DEFINE v_proceso_cod         LIKE cat_proceso.proceso_cod
    DEFINE v_opera_cod           LIKE cat_operacion.opera_cod

    -- Variables auxiliares
    DEFINE v_mensaje             STRING
    DEFINE v_sql                 STRING
    DEFINE v_contador            INTEGER
    DEFINE v_estado              SMALLINT 
    DEFINE v_registros_archivo   INTEGER
    DEFINE v_nombre_archivo      STRING

    DEFINE v_situacion_desc     LIKE cat_ocg_situacion.situacion_desc
    DEFINE v_entidad_desc       LIKE cat_entidad_financiera.ent_financiera_desc

    -- Campos de busqueda
    DEFINE v_entidad        LIKE cat_entidad_financiera.cve_ent_financiera
    DEFINE v_situacion      LIKE cat_ocg_situacion.situacion
    DEFINE v_f_proceso_ini  LIKE ocg_detalle.f_proceso
    DEFINE v_f_proceso_fin  LIKE ocg_detalle.f_proceso

    -- Combobox para la busqueda
    DEFINE cbx_entidad      ui.ComboBox
    DEFINE cbx_situacion    ui.ComboBox

    DEFINE g_reg_modulo          RECORD
           ruta_exp              CHAR(40),
           ruta_rescate          CHAR(40),
           ruta_listados         CHAR(40)
    END RECORD
    DEFINE seg_modulo_bat        RECORD
           ruta_listados         CHAR(40)
    END RECORD


    CALL ARG_VAL(1) RETURNING p_usuario_cod
    CALL ARG_VAL(2) RETURNING p_tpo_ejecucion
    CALL ARG_VAL(3) RETURNING p_nom_ventana

    LET v_proceso_cod = 3912
    LET v_opera_cod   = 1


    -- se obtienen las rutas de control del modulo
    SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
    INTO   g_reg_modulo.*
    FROM   seg_modulo s
    WHERE  s.modulo_cod = 'ocg'

    SELECT b.ruta_listados
    INTO   seg_modulo_bat.ruta_listados
    FROM   seg_modulo b
    WHERE  b.modulo_cod = 'bat'

    -- Log del proceso
    CALL STARTLOG(p_usuario_cod CLIPPED||".OCGL10.log")

    LET v_mensaje = "Se va a generar el Archivo Maestro de Acreditados"

    CLOSE WINDOW SCREEN

    OPEN WINDOW OCGL07 WITH FORM "OCGL101"

        CALL ui.Interface.setText ( p_nom_ventana )

        LET  cbx_entidad = ui.ComboBox.forName("v_entidad")
        CALL cbx_entidad.clear()
        LET  cbx_situacion = ui.ComboBox.forName("v_situacion")
        CALL cbx_situacion.clear()

        -- Se obtienen las entidades financieras
        LET v_sql = "SELECT cve_ent_financiera,ent_financiera_desc "||
                    "FROM   cat_entidad_financiera                     "
        PREPARE prp_entidad FROM v_sql
        DECLARE cur_entidad CURSOR FOR prp_entidad

        LET v_contador = 0

        FOREACH cur_entidad INTO v_entidad,v_entidad_desc
            CALL cbx_entidad.addItem(v_entidad,v_entidad||" - "||v_entidad_desc)
            LET v_contador = v_contador + 1
        END FOREACH

        IF v_contador > 0  THEN
            CALL cbx_entidad.removeItem(v_contador)
        END IF

        CALL cbx_entidad.addItem(NULL,"TODAS")
        
        -- Se obtienen la situaciones
        LET v_sql = "SELECT situacion,situacion_desc       "||
                    "FROM   cat_ocg_situacion              "||
                    "WHERE  situacion IN (55,60,70,80,140,150,160) "
        PREPARE prp_situacion FROM v_sql
        DECLARE cur_situacion CURSOR FOR prp_situacion

        LET v_contador = 0

        FOREACH cur_situacion INTO v_situacion,v_situacion_desc
            CALL cbx_situacion.addItem(v_situacion,v_situacion||" - "||v_situacion_desc)
            LET v_contador = v_contador + 1
        END FOREACH

        IF v_contador > 0  THEN
            CALL cbx_situacion.removeItem(v_contador)
        END IF

        CALL cbx_situacion.addItem(NULL,"TODAS")

        INPUT BY NAME v_entidad,v_situacion,v_f_proceso_ini,v_f_proceso_fin
            ATTRIBUTES(UNBUFFERED,ACCEPT=FALSE,CANCEL=FALSE)

            BEFORE INPUT
                LET v_entidad       = NULL
                LET v_situacion     = NULL
                LET v_f_proceso_ini = NULL
                LET v_f_proceso_fin = NULL

            ON ACTION Salir
                EXIT INPUT

            ON ACTION archivo
                IF v_situacion IS NOT NULL THEN
                    SELECT COUNT(*)
                    INTO   v_contador
                    FROM   ocg_formalizacion
                    WHERE  situacion = v_situacion
                ELSE
                    SELECT COUNT(*)
                    INTO   v_contador
                    FROM   ocg_formalizacion
                    WHERE  situacion IN (55,60,70,80,140,150,160)
                END IF

                IF v_contador > 0 THEN
                    CALL fn_genera_archivo(v_entidad,v_situacion,
                                           v_f_proceso_ini,v_f_proceso_fin)
                                           RETURNING v_estado,v_registros_archivo,v_nombre_archivo
                    LET v_mensaje = "Se encontraron ",v_registros_archivo," registro(s)."||
                                        "El archivo generado es: '",v_nombre_archivo,"'."
                ELSE
                    LET v_mensaje = "No hay registros con esos parametros"
                END IF

                CALL fn_mensaje("Aviso",v_mensaje,"stop")

        END INPUT

END MAIN

#Funcion que genera el archivo
FUNCTION fn_genera_archivo(p_entidad,p_situacion,p_f_proceso_ini,p_f_proceso_fin)

    -- Variables pasadas como parametro
    DEFINE p_entidad        LIKE cat_entidad_financiera.cve_ent_financiera
    DEFINE p_situacion      LIKE cat_ocg_situacion.situacion
    DEFINE p_f_proceso_ini  LIKE ocg_detalle.f_proceso
    DEFINE p_f_proceso_fin  LIKE ocg_detalle.f_proceso 

    --Variables para la generacion del archivo
    DEFINE v_archivo_control    base.Channel
    DEFINE v_archivo_envio      base.Channel
    DEFINE v_nombre_archivo     STRING
    DEFINE v_ruta_archivo       LIKE seg_modulo.ruta_envio
    DEFINE v_string             base.StringBuffer

    -- Record con el resultado de la consulta
    DEFINE v_detalle_acreditado RECORD
        marca_credito           CHAR(2) ,
        subproceso	            CHAR(3),
        f_proceso	            CHAR(10),
        situacion	            SMALLINT,
        cve_ent_financiera      CHAR(3),
        nss                     CHAR(11),
        num_ctr_int_ef          CHAR(18),
        rfc                     CHAR(13),
        curp	                CHAR(18),
        ap_paterno	            CHAR(40),
        ap_materno	            CHAR(40),
        nombre                  CHAR(40),
        bimestres	            CHAR(3) ,
        saldo_sar92             CHAR(1) ,
        viv97                   CHAR(10),
        f_corte                 CHAR(10),
        traspaso_92	            CHAR(12),
        traspaso_97	            CHAR(12),
        vigente_92	            CHAR(12),
        vigente_97	            CHAR(12),
        f_vigente	            CHAR(10),
        num_escritura	        CHAR(8),
        notario                 DECIMAL(4,0),
        ent_fed_notario         CHAR(3),
        mcpio_notario           CHAR(3),
        num_rpp	                CHAR(15),
        folio_real              DECIMAL(8,0),
        partida	                DECIMAL(6,0),
        foja	                DECIMAL(8,0),
        volumen	                DECIMAL(6,0),
        libro	                DECIMAL(6,0),
        tomo	                DECIMAL(6,0),
        seccion	                DECIMAL(6,0),
        ent_fed_inmueble	    CHAR(3),
        domicilio_inmueble	    CHAR(30),
        valor_avaluo	        CHAR(12),
        monto_credito           CHAR(12),
        plazo_credito	        DECIMAL(5,0),
        tpo_moneda	            CHAR(3),
        tasa_base	            CHAR(20),
        margen	                CHAR(20),
        f_otorga_ent_fin        CHAR(10),
        f_registro_carta        CHAR(10),
        usuario_reg_carta	    CHAR(20),
        f_liquida_credito       CHAR(10),
        f_subproceso	        CHAR(10),
        f_solic_marca_prcr	    CHAR(10),
        f_conf_marca_prcr       CHAR(10),
        f_solicitud_traspaso	CHAR(10),
        f_solicitud_aportacion	CHAR(10),
        f_traspaso_aportacion	CHAR(10),
        causa_aceptacion	    CHAR(2),
        anio_ejercicio	        CHAR(4),
        f_autorizacion	        CHAR(10),
        f_autorizacion2	        CHAR(10),
        tpo_saldo1	            CHAR(2),
        saldo1	                CHAR(12),
        f_saldo1	            CHAR(10),
        tpo_saldo2	            CHAR(2),
        saldo2	                CHAR(12),
        f_saldo2	            CHAR(10),
        tpo_saldo3	            CHAR(2),
        saldo3	                CHAR(12),
        f_saldo3	            CHAR(10),
        tpo_saldo4	            CHAR(2),
        saldo4	                CHAR(12),
        f_saldo4	            CHAR(10),
        f_liberacion_gtia       CHAR(10),
        f_solic_desmarca_prcr	CHAR(10),
        f_conf_desmarca_prcr	CHAR(10),
        tpo_credito	            CHAR(1),
        f_seleccion	            CHAR(10),
        marca_eventos	        CHAR(2),
        tpo_credito2            CHAR(1),
        nss_conyuge	            CHAR(11),
        destino_credito         CHAR(1)
    END RECORD

    -- Variables de retorno
    DEFINE r_estado             SMALLINT
    DEFINE r_registros_archivo  INTEGER

    -- Variables auxiliares
    DEFINE v_query              STRING

    -- Se inicializan variables
    LET v_archivo_control   = base.Channel.create()
    LET v_archivo_envio     = base.Channel.create()
    LET r_estado            = 0 --Se da por hecho que no hubo errores
    LET r_registros_archivo = 0

    -- Se obtiene la ruta de envio
    SELECT ruta_envio
    INTO   v_ruta_archivo
    FROM   seg_modulo
    WHERE  modulo_cod = 'ocg'

    -- Hora actual
    LET v_string = base.StringBuffer.create()
    LET v_nombre_archivo = TIME(CURRENT)
    CALL v_string.append(v_nombre_archivo)
    CALL v_string.replace(":","",2)

    -- Se genera un archivo usando la fecha y el folio
    LET v_nombre_archivo = v_ruta_archivo CLIPPED||"/maestro_acreditado_manual_"||
                           (TODAY USING "ddmmyyyy")||"_"||v_string.toString()||".txt"
    CALL v_archivo_control.openFile(v_nombre_archivo,"w")
    
    -- Se genera el archivo normal a enviar
    LET v_nombre_archivo = v_ruta_archivo CLIPPED||"/maestro_acreditado.txt"
    CALL v_archivo_envio.openFile(v_nombre_archivo,"w")

    -- Se establece el separador (pipe: "|")
    CALL v_archivo_control.setDelimiter("|")
    CALL v_archivo_envio.setDelimiter("|")

    -- Se construye el query
    LET v_query = 
        "SELECT  NULL::char as marca_credito,TO_CHAR(ocd.subproceso,'&&&'),
                TO_CHAR(ocd.f_proceso,'%Y/%m/%d') as f_proceso_,
                CASE
                     WHEN oca.situacion = '55' THEN '1'
                     WHEN oca.situacion = '60' THEN '1'
                     WHEN oca.situacion = '70' THEN '1'
                     WHEN oca.situacion = '80' THEN '4'
                     WHEN oca.situacion = '140' THEN '5'
                     WHEN oca.situacion = '158' THEN '5'
                     WHEN oca.situacion = '150' THEN '5'
                     WHEN oca.situacion = '160' THEN '5'
                END CASE,
                TO_CHAR(ocf.cve_ent_financiera,'&&&'),ocd.nss,
                ocf.num_ctr_int_ef,ocf.rfc,ocf.curp,ocf.ap_paterno,
                ocf.ap_materno,ocf.nombre,NULL::char as bimestres,
                NULL::char as saldo_sar92,
                TO_CHAR(ocf.viv97,'###,##&.##') as sar97,
                TO_CHAR(ocf.f_saldo,'%Y/%m/%d') as f_saldo_,
                NULL::char as traspaso_92,NULL::char as traspaso_97,
                NULL::char as vigente_92,NULL::char as vigente_97,
                NULL::char as f_vigente,ocf.num_escritura,
                ocf.notario,TO_CHAR(ocf.ent_fed_notario,'&&&'),
                TO_CHAR(ocf.mcpio_notario,'&&&'),ocf.num_rpp,ocf.folio_real,
                ocf.partida,ocf.foja,ocf.volumen,ocf.libro,ocf.tomo,ocf.seccion,
                TO_CHAR(ocf.ent_fed_inmueble,'&&&'),ocf.domicilio_inmueble,
                TO_CHAR(ocf.valor_avaluo,'##,###,##&.##') as valor_avaluo_,
                TO_CHAR(ocf.monto_credito,'##,###,##&.##') as monto_credito_,
                ocf.plazo_credito,TO_CHAR(ocf.tpo_moneda,'&&&'),
                ocf.tasa_base,ocf.margen,
                TO_CHAR(ocf.f_otorga_ent_fin,'%Y/%m/%d') as f_otorgamiento,
                TO_CHAR(ocf.f_registro_carta,'%Y/%m/%d') as f_registro,
                ocf.usuario_reg_carta,
                TO_CHAR(oca.f_liquida_credito,'%Y/%m/%d') as f_credito,
                NULL::char as f_subproceso,
                TO_CHAR(oca.f_solic_marca_prcr,'%Y/%m/%d') as f_solictud_marca,
                TO_CHAR(oca.f_conf_marca_prcr,'%Y/%m/%d') as f_confirmacion_marca,
                NULL::char as f_solicitud_traspaso,
                NULL::char as f_solicitud_aportacion,
                NULL::char as f_traspaso_aportacion,NULL::char as causa_aceptacion,
                NULL::char as anio_ejercicio,NULL::char as f_autorizacion,
                NULL::char as f_autorizacion2,NULL::char as tpo_saldo1,
                NULL::char as saldo1,NULL::char as f_saldo1,
                NULL::char as tpo_saldo2,NULL::char as saldo2,
                NULL::char as f_saldo2,NULL::char as tpo_saldo3,
                NULL::char as saldo3,NULL::char as f_saldo3,
                NULL::char as tpo_saldo4,NULL::char as saldo4,
                NULL::char as f_saldo4,
                TO_CHAR(ocl.f_liberacion_gtia,'%Y/%m/%d') as f_liberacion,
                TO_CHAR(oca.f_solic_desmarca_prcr,'%Y/%m/%d') as f_solicitud_desmarca,
                TO_CHAR(oca.f_conf_desmarca_prcr,'%Y/%m/%d')
                as f_confirmacion_desmarca,ocf.tpo_credito,
                NULL::char as f_seleccion,NULL::char as marca_eventos,
                ocf.tpo_credito as tpo_credito2,NULL::char as nss_conyuge,
                NULL::char as destino_credito
         FROM   ocg_detalle       ocd,
                ocg_acreditado oca,
                ocg_formalizacion ocf,
                OUTER ocg_liquidacion ocl
         WHERE  ocf.id_ocg_detalle = ocd.id_ocg_detalle
           AND  ocf.id_ocg_formalizacion = oca.id_ocg_formalizacion
           AND  ocl.id_ocg_formalizacion = ocf.id_ocg_formalizacion"

   
    IF p_entidad IS NOT NULL THEN
        LET v_query = v_query||" AND ocf.cve_ent_financiera = "||p_entidad
    END IF

    IF p_situacion IS NOT NULL THEN
        LET v_query = v_query||" AND ocf.situacion = "||p_situacion
    ELSE
        LET v_query = v_query||" AND ocf.situacion IN (55,60,70,80,140,150,158,160)"
    END IF

    IF p_f_proceso_ini IS NOT NULL THEN
        IF p_f_proceso_fin IS NOT NULL THEN
            LET v_query = v_query||"  AND ocd.f_proceso BETWEEN "||
                          "'"||p_f_proceso_ini||"' AND '"||p_f_proceso_fin||"'"
        ELSE
            LET v_query = v_query||"  AND ocd.f_proceso > '"||p_f_proceso_ini||"'"
        END IF
    ELSE
        IF p_f_proceso_fin IS NOT NULL THEN 
            LET v_query = v_query||"  AND ocd.f_proceso < '"||p_f_proceso_fin||"'"
        END IF
    END IF

    --DISPLAY "QUERY: ",v_query

    PREPARE prp_acreditados_detalle FROM v_query
    DECLARE cur_acreditados_detalle CURSOR FOR prp_acreditados_detalle

    --DISPLAY v_query

    FOREACH cur_acreditados_detalle INTO v_detalle_acreditado.*

        -- Se escribe en los archivos
        CALL v_archivo_control.write([v_detalle_acreditado.*])
        CALL v_archivo_envio.write([v_detalle_acreditado.*])

        LET r_registros_archivo = r_registros_archivo + 1

    END FOREACH

    -- Se cierran los archivos
    CALL v_archivo_control.close()
    CALL v_archivo_envio.close()

    RETURN r_estado,r_registros_archivo,v_nombre_archivo

END FUNCTION
