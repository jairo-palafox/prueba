################################################################################
#Modulo        => OCG                                                          #
#Programa      => OCGP06                                                       #
#Ojetivo       => Proceso para construir el archivo maestro de acreditados     #
#Fecha inicio  => Marzo 2016                                                   #
#Requerimiento => 
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################

DATABASE safre_viv

-- Datos pasados como parametros
DEFINE p_pid            LIKE glo_pid.pid
DEFINE p_proceso_cod    LIKE cat_proceso.proceso_cod    
DEFINE p_opera_cod      LIKE cat_operacion.opera_cod    
DEFINE p_usuario_cod    LIKE seg_usuario.usuario

MAIN

    DEFINE v_folio          LIKE glo_folio.folio

    -- Informacion del proceso
    DEFINE v_proceso_desc             CHAR(40)
    DEFINE v_opera_desc               CHAR(40)

   -- Datos del proceso
   DEFINE v_estado               SMALLINT
   DEFINE r_resultado_opera      INTEGER

   CALL ARG_VAL(1) RETURNING p_usuario_cod
   CALL ARG_VAL(2) RETURNING p_pid
   CALL ARG_VAL(3) RETURNING p_proceso_cod
   CALL ARG_VAL(4) RETURNING p_opera_cod

   WHENEVER ERROR CONTINUE

   -- Log del proceso
   CALL STARTLOG(p_usuario_cod CLIPPED||".OCGP06.log")

   --Recupero informacion necesaria del proceso y rutas de trabajo
   --Descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   --Descripcion de la operacion
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   --Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   -- Se solicita el numero de folio asociado a la operacion. Parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   --Se actualiza el folio del proceso
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   --Realizo el proceso de Marcas
   CALL fn_genera_archivo(v_folio) RETURNING v_estado

   --Finaliza la operacion
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
   RETURNING r_resultado_opera

   IF(r_resultado_opera <> 0)THEN         
      # Actualiza a estado erróneo
      DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   END IF

   --Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   WHENEVER ERROR STOP

END MAIN

#Funcion que genera el archivo
FUNCTION fn_genera_archivo(p_folio)

    --Variables pasadas como parametros
    DEFINE p_folio  LIKE glo_folio.folio

    --Variables para la generacion del archivo
    DEFINE v_archivo_control    base.Channel
    DEFINE v_archivo_envio      base.Channel
    DEFINE v_nombre_archivo     STRING
    DEFINE v_ruta_archivo       LIKE seg_modulo.ruta_envio

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
        num_escritura	        DECIMAL(8,0),
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
    DEFINE r_estado     SMALLINT

    -- Variables auxiliares
    DEFINE v_query              STRING
    DEFINE v_registros_archivo  INTEGER

    -- Se inicializan variables
    LET v_archivo_control   = base.Channel.create()
    LET v_archivo_envio     = base.Channel.create()
    LET r_estado            = 0 --Se da por hecho que no hubo errores
    LET v_registros_archivo = 0

    -- Se obtiene la ruta de envio
    SELECT ruta_envio
    INTO   v_ruta_archivo
    FROM   seg_modulo
    WHERE  modulo_cod = 'ocg'

    -- Se genera un archivo usando la fecha y el folio
    LET v_nombre_archivo = v_ruta_archivo CLIPPED||"/maestro_acreditado_automatico_"||
                           (TODAY USING "ddmmyyyy")||"_"||p_folio||".txt"
    CALL v_archivo_control.openFile(v_nombre_archivo,"w")

    -- Se genera el archivo normal a enviar
    LET v_nombre_archivo = v_ruta_archivo CLIPPED||"/maestro_acreditado.txt"
    CALL v_archivo_envio.openFile(v_nombre_archivo,"w")

    DISPLAY ""
    DISPLAY ""
    DISPLAY ""
    DISPLAY "OBTENIENDO INFORMACION..."

    -- Se establece el separador (pipe: "|")
    CALL v_archivo_control.setDelimiter("|")
    CALL v_archivo_envio.setDelimiter("|")

    -- Se construye el query
    LET v_query = 
        "SELECT NULL::char as marca_credito,TO_CHAR(ocd.subproceso,'&&&'),
                TO_CHAR(ocd.f_proceso,'%Y/%m/%d') as f_proceso_,
                CASE
                     WHEN oca.situacion = '55' THEN '1'
                     WHEN oca.situacion = '60' THEN '1'
                     WHEN oca.situacion = '70' THEN '1'
                     WHEN oca.situacion = '80' THEN '4'
                     WHEN oca.situacion = '140' THEN '5'
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
                ocg_formalizacion ocf,
                (SELECT MAX(ocd.f_proceso) as f_proceso_,ocd.nss,ocf.situacion
                 FROM   ocg_formalizacion ocf,
                        ocg_detalle       ocd
                 WHERE  ocf.id_ocg_detalle = ocd.id_ocg_detalle
                 GROUP  BY ocd.nss,ocf.situacion) maxf,
                (SELECT MAX(situacion) as situacion_,nss
                 FROM   (SELECT MAX(ocd.f_proceso),ocd.nss,ocf.situacion
                         FROM   ocg_formalizacion ocf,
                                ocg_detalle       ocd
                         WHERE  ocf.id_ocg_detalle = ocd.id_ocg_detalle
                         GROUP  BY ocd.nss,ocf.situacion)
                 GROUP   BY nss) maxs,
                 OUTER ocg_acreditado    oca,
                 OUTER ocg_liquidacion   ocl
         WHERE  ocf.id_ocg_detalle = ocd.id_ocg_detalle
           AND  ocf.id_ocg_formalizacion = oca.id_ocg_formalizacion
           AND  ocf.id_ocg_formalizacion = ocl.id_ocg_formalizacion
           AND  ocd.f_proceso      = maxf.f_proceso_
           AND  ocd.nss            = maxf.nss
           AND  ocf.situacion      = maxf.situacion
           AND  ocf.situacion      = maxf.situacion
           AND  ocd.nss            = maxs.nss
           AND  maxf.situacion     = maxs.situacion_
           AND  maxs.nss           = maxf.nss
           AND  ocf.situacion     IN (55,60,70,80,140,150,158,160)"

    PREPARE prp_acreditados_detalle FROM v_query
    DECLARE cur_acreditados_detalle CURSOR FOR prp_acreditados_detalle

    DISPLAY ""
    DISPLAY "GENERANDO ARCHIVO..."
    FOREACH cur_acreditados_detalle INTO v_detalle_acreditado.*

        -- Se escribe en los archivos
        CALL v_archivo_control.write([v_detalle_acreditado.*])
        CALL v_archivo_envio.write([v_detalle_acreditado.*])

        LET v_registros_archivo = v_registros_archivo + 1

    END FOREACH
    -- Se cierran los archivos
    CALL v_archivo_control.close()
    CALL v_archivo_envio.close()

    DISPLAY ""
    DISPLAY "Archivo:   ",v_nombre_archivo
    DISPLAY "Registros: ",v_registros_archivo
    DISPLAY ""
    DISPLAY ""

    RETURN r_estado

END FUNCTION