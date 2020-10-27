################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP391                                                      #
#Ojetivo       => Programa integrador del archivo de SIAFF diario. Para        #
#                 actualizar las solicitudes a 90 con codigo rechazo 66 y a 71 #
#Fecha inicio  => Septiembre, 2015.                                            #
#Requerimiento => 782                                                          #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
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

   CALL ARG_VAL(1) RETURNING g_usuario_cod
   CALL ARG_VAL(2) RETURNING g_pid
   CALL ARG_VAL(3) RETURNING g_proceso_cod
   CALL ARG_VAL(4) RETURNING g_opera_cod
   CALL ARG_VAL(5) RETURNING g_folio
   CALL ARG_VAL(6) RETURNING g_archivo

   -- Texto a la bitacora
   CALL fn_display_proceso(0,"INTEGRACION")

   -- Integracion de los datos del archivo
   CALL fn_integra_archivo() RETURNING v_estado

   -- Actualiza el estado del archivo procesado
   CALL fn_act_edo_archivo(g_archivo,g_folio,2,g_usuario_cod) 
      RETURNING r_bnd_edo_act_archivo

   -- Se verifica si hubo errores
   IF v_estado = 0 THEN
      DISPLAY "Integración realizada con éxito."
   ELSE
      DISPLAY "El proceso de integracion ha finalizado pero con errores.\n."
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
   END IF

   CALL fn_actualiza_opera_fin(g_pid,
                               g_proceso_cod,
                               g_opera_cod)
                     RETURNING v_estado

   CALL fn_display_proceso(1,"INTEGRACION")
    
END MAIN

FUNCTION fn_integra_archivo()

   -- Total de registros cargados en el archivo
   DEFINE v_total_registros    INTEGER
   -- Total de registros aceptados
   DEFINE v_total_aceptados    INTEGER
   -- Total de registros aceptados
   DEFINE v_total_rechazados    INTEGER
   -- Para reconocer el NSS
   DEFINE v_id_solicitud       DECIMAL(9,0)
   -- Campos extra en ret_respuesta_siaff
   DEFINE v_id_derechohabiente DECIMAL(9,0)
   DEFINE v_cod_rechazo_efp    SMALLINT
   DEFINE v_estado_solicitud   SMALLINT
   DEFINE v_registro_siaff  RECORD    -- registro de tabla tmp_ret_respuesta_siaff
          des_ramo                 CHAR(100)    ,
          des_unidad               CHAR(100)    ,
          uni_folio                CHAR(12)     ,
          archivo_envio            CHAR(250)    ,
          archivo_salida           CHAR(250)    ,
          archivo_acuse            CHAR(250)    ,
          archivo_devol            CHAR(250)    ,
          archivo_regreso          CHAR(250)    ,
          total_rechazo            DECIMAL(16,2),
          estatus_nom              SMALLINT     ,
          cod_banco                INTEGER      ,
          clave_rastreo            DECIMAL(30,0),
          estatus_det              SMALLINT     ,
          des_estatus_det          CHAR(150)    ,
          ramo                     SMALLINT     ,
          unidad                   CHAR(3)      ,
          folio_clc                DECIMAL(10,0),
          f_presenta               DATE         ,
          f_pago                   DATE         ,
          archivo_entrada          CHAR(250)    ,
          nss                      CHAR(11)     ,
          nombre                   CHAR(150)    ,
          cta_bancaria             DECIMAL(20,0),
          importe                  DECIMAL(16,2),
          numero_oprbanc           DECIMAL(10,0),
          cod_rechazo              SMALLINT     ,
          des_rechazo              CHAR(150)
   END RECORD

   DEFINE r_estado SMALLINT

   -- Inicializar variables    
   LET v_total_registros  = 0
   LET v_total_aceptados  = 0
   LET v_total_rechazados = 0
   LET r_estado = 0

   DECLARE cur_ret_resp_siaff CURSOR FOR SELECT *
                                          FROM safre_tmp:tmp_ret_respuesta_siaff

   DISPLAY ""
   DISPLAY ""
   DISPLAY "           Validando existencia de datos"
   DISPLAY ""
                                         
   FOREACH cur_ret_resp_siaff INTO v_registro_siaff.*

      LET v_cod_rechazo_efp = 0
      LET v_estado_solicitud = 42
      LET v_id_derechohabiente = NULL

      -- Se valida NSS
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = v_registro_siaff.nss

      IF v_id_derechohabiente IS NULL THEN
         LET v_cod_rechazo_efp = 1023
         DISPLAY "NSS no encontrado:                     ",v_registro_siaff.nss
      ELSE
         -- Se busca el id de la solicitud
         SELECT id_solicitud
         INTO   v_id_solicitud
         FROM   ret_solicitud_generico
         WHERE  nss                = v_registro_siaff.nss
         AND    id_derechohabiente = v_id_derechohabiente
         AND    estado_solicitud   = 69
         AND    cod_rechazo        = 0
         AND    modalidad_retiro   = 3
         IF v_id_solicitud IS NOT NULL THEN 
            IF v_registro_siaff.cod_rechazo = 0 THEN
               -- Se realiza el conteo de solicitudes aceptadas
               LET v_total_aceptados = v_total_aceptados + 1

               -- Se actualiza a estado_solicitud 71
               UPDATE ret_solicitud_generico
               SET    estado_solicitud   = 71  -- pagada
               WHERE  id_solicitud = v_id_solicitud
               
               UPDATE ret_ley73_generico
               SET    estado_solicitud = 71
               WHERE  id_solicitud = v_id_solicitud

               LET v_estado_solicitud = 71
            ELSE
               LET v_cod_rechazo_efp = 0

               LET v_total_rechazados = v_total_rechazados + 1

               -- Se actualiza a estado_solicitud 90 con codigo de rechazo 66
               UPDATE ret_solicitud_generico
               SET    estado_solicitud   = 90, -- 
                      cod_rechazo        = 66  -- rechazada por TESOFE
               WHERE  id_solicitud = v_id_solicitud

               UPDATE ret_ley73_generico
               SET    estado_solicitud = 90,
                      cod_rechazo      = 66
               WHERE  id_solicitud = v_id_solicitud

               LET v_estado_solicitud = 90
        
            END IF
         END IF 
      END IF

      IF v_id_solicitud IS NULL THEN
         LET v_id_solicitud = 0
      END IF

      -- Se pasan a la tabla ret_respuesta_siaff, los datos de la tabla temporal
      INSERT INTO ret_respuesta_siaff 
            VALUES(v_id_solicitud,g_folio,v_registro_siaff.*,v_estado_solicitud,v_cod_rechazo_efp)
      -- Se cuentan los registros que se procesaran
      LET v_total_registros = v_total_registros + 1
   END FOREACH

    ###Control de la integracion
    -- Se indica que el archivo ha sido integrado
    UPDATE glo_ctr_archivo
        SET    folio = g_folio,
               estado = 2 -- integrado
        WHERE  proceso_cod    = g_proceso_cod
        AND    opera_cod      = 1 -- archivo cargado
        AND    estado         = 1; -- etapa de carga

    -- Agregar folio a operacion de integracion
    UPDATE bat_ctr_operacion
        SET    folio       = g_folio
        WHERE  proceso_cod = g_proceso_cod
        AND    opera_cod   = 2
        AND    pid         = g_pid;
    UPDATE bat_ctr_proceso
        SET    folio       = g_folio
        WHERE  proceso_cod = g_proceso_cod
        AND    pid         = g_pid;

    DISPLAY ""
    DISPLAY ""
    DISPLAY "                   Registros"
    DISPLAY ""
    DISPLAY "Archivo:                           ",g_archivo
    DISPLAY "Total de registros encontrados:    ",v_total_registros
    DISPLAY "Solicitudes aceptadas (SIAFF):     ",v_total_aceptados
    DISPLAY "Solicitudes con codigo de rechazo: ",v_total_rechazados
    DISPLAY "Folio:                             ",g_folio
    DISPLAY ""
    DISPLAY ""

    IF v_total_aceptados < 1 AND v_total_rechazados < 1 THEN
        RETURN 1
    ELSE
        RETURN 0
    END IF

END FUNCTION