################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP377                                                      #
#Ojetivo       => Programa integrador para cambiar el estado solicitud de 8 o  #
#                 10 a 100 en las tablas ret_solicitud_generico y              #
#                 ret_amort_excedente.                                         #
#Fecha inicio  => Agosto, 2016.                                                #
#Requerimiento =>                                                              #
################################################################################
DATABASE safre_viv

DEFINE g_pid            DECIMAL(9,0)
DEFINE g_proceso_cod    SMALLINT
DEFINE g_opera_cod      SMALLINT
DEFINE g_folio          DECIMAL(9,0)
DEFINE g_archivo        CHAR(50)
DEFINE g_usuario_cod    CHAR(20)

MAIN

    DEFINE v_estado     SMALLINT
    DEFINE r_bnd_edo_act_archivo           SMALLINT

    LET g_usuario_cod = ARG_VAL(1)
    LET g_pid = ARG_VAL(2)
    LET g_proceso_cod = ARG_VAL(3)
    LET g_opera_cod = ARG_VAL(4)
    LET g_folio = ARG_VAL(5)
    LET g_archivo = ARG_VAL(6)

    --Texto a la bitacora
    CALL fn_display_proceso(0,"INTEGRACION")
    --Integracion de los datos del archivo
    CALL fn_integra_archivo() RETURNING v_estado

    -- Actualiza el estado del archivo procesado
    CALL fn_act_edo_archivo(g_archivo,g_folio,2,g_usuario_cod) 
         RETURNING r_bnd_edo_act_archivo

    IF v_estado THEN
        CALL fn_actualiza_opera_fin(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod)
                                    RETURNING v_estado
        DISPLAY "Integración realizada con éxito."
    ELSE
        DISPLAY "El proceso de integracion ha finalizado pero con excepciones.\n."
        CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
    END IF
    CALL fn_display_proceso(1,"INTEGRACION")
    
END MAIN

# Funcion que realiza la actualizacion de estados
FUNCTION fn_integra_archivo()

    DEFINE v_query      STRING

    DEFINE v_nss                LIKE ret_solicitud_generico.nss
    DEFINE v_caso_adai          LIKE ret_solicitud_generico.caso_adai
    DEFINE v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud
    DEFINE v_estado_solicitud   LIKE ret_solicitud_generico.estado_solicitud
    DEFINE v_id_derechohabiente LIKE sfr_marca_activa.id_derechohabiente
    DEFINE v_marca              LIKE sfr_marca_activa.marca
    DEFINE v_n_referencia       LIKE sfr_marca_activa.n_referencia
    DEFINE v_estado_marca       SMALLINT
    DEFINE v_marca_causa        LIKE sfr_marca_activa.marca_causa
    DEFINE v_proceso_marca      LIKE sfr_marca_activa.proceso_marca
    DEFINE v_resultado          SMALLINT
    DEFINE v_nss_desmarcados    INTEGER
    DEFINE v_nss_edo_dif        INTEGER
    DEFINE v_nss_sin_sol        INTEGER
    DEFINE v_channel            base.Channel
    DEFINE v_nom_archivo        LIKE glo_ctr_archivo.nombre_archivo
    DEFINE v_ruta_envio         LIKE seg_modulo.ruta_envio
    DEFINE v_ruta_archivo       STRING
    DEFINE v_linea              STRING

    DEFINE r_estatus            SMALLINT

    -- Se recupera la ruta de envio del modulo de retiros
    SELECT ruta_envio
    INTO   v_ruta_envio
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'
    -- Se recupera el nombre del archivo que se cargo
    SELECT nom_archivo
    INTO   v_nom_archivo
    FROM   bat_ctr_operacion
    WHERE  folio       = g_folio
      AND  proceso_cod = g_proceso_cod
      AND  opera_cod   = g_opera_cod

    LET r_estatus         = TRUE
    LET v_ruta_archivo    = v_ruta_envio CLIPPED,"/",v_nom_archivo CLIPPED
    LET v_nss_desmarcados = 0
    LET v_nss_edo_dif     = 0
    LET v_nss_sin_sol     = 0
    LET v_estado_marca    = NULL

    LET v_channel = base.Channel.create()
    CALL v_channel.openFile(v_ruta_archivo,'w')

    -- Recupera los datos cargados del archivo
    LET v_query = "SELECT nss,caso_adai
                   FROM   safre_tmp:tmp_solicitud_rg"
    PREPARE prp_nss_adai FROM v_query
    DECLARE cur_nss_adai CURSOR FOR prp_nss_adai
    -- Valida si existe la solicitud
    LET v_query = "SELECT id_solicitud,estado_solicitud
                   FROM   ret_solicitud_generico
                   WHERE  nss              = ? 
                     AND  caso_adai        = ?"
    PREPARE prp_solicitud FROM v_query
    -- Se obtienen los datos para hacer la desmarca
    LET v_query = "SELECT id_derechohabiente,marca,n_referencia,
                          marca_causa,proceso_marca
			       FROM   sfr_marca_activa
			       WHERE  n_referencia = ?
			    	 AND  marca        = 810"
    PREPARE prp_datos_desmarca FROM v_query
    -- Ejecuta la funcion que desmarca la cuenta
    LET v_query = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
    PREPARE prp_desmarca_cuenta FROM v_query

    FOREACH cur_nss_adai INTO v_nss,v_caso_adai

        LET v_id_solicitud = NULL
        EXECUTE prp_solicitud USING v_nss,v_caso_adai
                              INTO  v_id_solicitud,v_estado_solicitud

        -- Cuando tiene solicitud
        IF v_id_solicitud IS NOT NULL THEN

            IF (v_estado_solicitud = 8) OR (v_estado_solicitud = 10) THEN
                LET v_nss_desmarcados = v_nss_desmarcados + 1
                --Se actualiza la solicitud
                UPDATE ret_solicitud_generico
                SET    estado_solicitud = 100,
                       cod_rechazo      = 54
                WHERE  id_solicitud     = v_id_solicitud

                UPDATE ret_amort_excedente
                SET    estado_solicitud = 100,
                       cod_rechazo      = 54
                WHERE  id_solicitud     = v_id_solicitud
                -- Se desmarca la cuenta
                EXECUTE prp_datos_desmarca USING v_id_solicitud
                                           INTO  v_id_derechohabiente,
                                                 v_marca,
                                                 v_n_referencia,
                                                 v_marca_causa,
                                                 v_proceso_marca
                EXECUTE prp_desmarca_cuenta USING v_id_derechohabiente,
                                                  v_marca,
                                                  v_n_referencia,
                                                  v_estado_marca,
                                                  v_marca_causa,
                                                  g_usuario_cod,
                                                  v_proceso_marca
                                            INTO  v_resultado
            ELSE
                LET v_nss_edo_dif = v_nss_edo_dif + 1
                LET v_linea = v_nss,"|",v_caso_adai,"|",v_estado_solicitud,"|"
                CALL v_channel.writeLine(v_linea)
            END IF -- (v_edo_solicitud = 8) OR (v_edo_solicitud = 10)

        ELSE -- No tiene solicitud
            LET v_nss_sin_sol = v_nss_sin_sol + 1
            LET v_linea = v_nss,"|",v_caso_adai,"||"
            CALL v_channel.writeLine(v_linea)
        END IF -- v_id_solicitud IS NOT NULL

    END FOREACH -- cur_nss_adai

    CALL v_channel.close()

    DISPLAY ""
    DISPLAY "NSS"
    DISPLAY "Desmarcados:                   ",v_nss_desmarcados
    DISPLAY "Con estado diferente a 8 y 10: ",v_nss_edo_dif
    DISPLAY "Sin solicitud:                 ",v_nss_sin_sol
    DISPLAY ""
    DISPLAY "Archivo Generado: ",v_ruta_archivo

    RETURN r_estatus

END FUNCTION -- fn_integra_archivo