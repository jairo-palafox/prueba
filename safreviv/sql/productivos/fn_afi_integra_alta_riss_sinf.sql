






CREATE FUNCTION "safreviv".fn_afi_integra_alta_riss_sinf(p_folio                          DECIMAL(10) ,
                                         tmp_afi_alta_tpo_movimiento      CHAR(2)     ,
                                         tmp_afi_alta_nrp                 CHAR(11)    ,
                                         tmp_afi_alta_f_movimiento        CHAR(8)     ,
                                         tmp_afi_alta_curp_rfc            CHAR(18)    ,
                                         tmp_afi_alta_t_trabajador        DECIMAL(1,0),
                                         tmp_afi_alta_nss                 CHAR(11)    ,
                                         tmp_afi_alta_nombre              CHAR(50)    ,
                                         tmp_afi_alta_presentacion_extemp DECIMAL(1,0),
                                         tmp_afi_alta_jornada_semana      DECIMAL(1,0),
                                         tmp_afi_alta_sdi                 DECIMAL(6,0),
                                         tmp_afi_alta_sexo                DECIMAL(1,0),
                                         tmp_afi_alta_nss_correcto        CHAR(11)    ,
                                         tmp_afi_alta_nombre_correcto     CHAR(50)    ,
                                         p_usuario_cod                    CHAR(20))

   RETURNING INTEGER, INTEGER, VARCHAR(255)

   -- campos de la tabla de rechazos afi_rch_afiliatorio
   DEFINE afi_rch_afiliatorio_tpo_movimiento      CHAR(2);
   DEFINE afi_rch_afiliatorio_espacios            CHAR(2);
   DEFINE afi_rch_afiliatorio_nrp                 CHAR(11);
   DEFINE afi_rch_afiliatorio_f_movimiento        CHAR(8);
   DEFINE afi_rch_afiliatorio_curp_rfc            CHAR(18);
   DEFINE afi_rch_afiliatorio_t_trabajador        DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss                 CHAR(11);
   DEFINE afi_rch_afiliatorio_nombre              CHAR(50);
   DEFINE afi_rch_afiliatorio_presentacion_extemp DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_jornada_semana      DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_sdi                 DECIMAL(6,0);
   DEFINE afi_rch_afiliatorio_sexo                DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss_correcto        CHAR(11);
   DEFINE afi_rch_afiliatorio_nombre_correcto     CHAR(50);
   DEFINE afi_rch_afiliatorio_cod_rechazo         SMALLINT;

   -- campos de la tabla afi_relacion_laboral
   DEFINE afi_relacion_laboral_id_derechohabiente DECIMAL(9,0);
   DEFINE afi_relacion_laboral_nrp                CHAR(11);
   DEFINE afi_relacion_laboral_f_alta_nrp         DATE;
   DEFINE afi_relacion_laboral_ind_relacion       SMALLINT;
   DEFINE afi_relacion_laboral_folio_lote         DECIMAL(9,0);
   DEFINE afi_relacion_laboral_f_actualiza        DATE;
   DEFINE afi_relacion_laboral_usuario            CHAR(20);
   DEFINE v_alta_tipo_trabajador                  CHAR(1);

   -- para cambiar el formato de la fecha
   DEFINE v_fecha_texto                           VARCHAR(10);
   DEFINE v_rfc_curp                              VARCHAR(18);

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);
   DEFINE v_codigo_rechazo                        SMALLINT;
   DEFINE v_fecha_valida                          SMALLINT;
   DEFINE v_fecha_movimiento                      DATE;
   DEFINE v_nombre_af                             CHAR(40);
   DEFINE v_ap_paterno_af                         CHAR(40);
   DEFINE v_ap_materno_af                         CHAR(40);
   DEFINE v_nombre                                CHAR(40);
   DEFINE v_paterno                               CHAR(40);
   DEFINE v_materno                               CHAR(40);
   DEFINE v_nombre_imss                           CHAR(50);
   DEFINE v_curp                                  CHAR(18);
   DEFINE v_rfc                                   CHAR(13);
   DEFINE tmp_riss                                SMALLINT;
   DEFINE v_id_riss                               SMALLINT;
   DEFINE v_rl                                    SMALLINT;
   DEFINE v_ind_modifica                          SMALLINT;
   DEFINE v_ind_alta_riss                         SMALLINT;

   -- Variables de validaciones
   DEFINE v_d_id_referencia                       DECIMAL(9,0);
   DEFINE v_id_derechohabiente                    DECIMAL(9,0);
   DEFINE v_id_derechohabiente_unificado          DECIMAL(9,0);
   DEFINE v_diagnostico_unificador                SMALLINT;
   DEFINE v_diagnostico_unificadas                SMALLINT;
   DEFINE v_diagnostico_rechazo                   SMALLINT;
   DEFINE v_estado_familia_unificador             SMALLINT;
   DEFINE v_estado_familia_unificado              SMALLINT;

   -- numero de altas aceptadas y rechzadas
   DEFINE v_num_altas_aceptadas                   SMALLINT;
   DEFINE v_num_altas_rechazadas                  SMALLINT;
   DEFINE v_registro_rechazado                    SMALLINT; -- booleana para verificar un registro rechazado
   DEFINE v_solo_nrp                              CHAR(11); -- para indicar si solo se debe dar de alta el NRP
   DEFINE v_f_actualiza                           DATE;     -- para indicar fecha de riss histórico

   -- Variable para marca de cuenta
   DEFINE v_i_estado_marca                        INTEGER;

   -- constantes para codigos de error
   DEFINE v_error_nss_vacio                       SMALLINT;
   DEFINE v_error_nss_ya_existe                   SMALLINT;
   DEFINE v_error_fecha_movimiento_vacia          SMALLINT;
   DEFINE v_error_nrp_vacio                       SMALLINT;
   DEFINE v_error_nombre_vacio                    SMALLINT;
   DEFINE v_error_fec_mov_posterior_actual        SMALLINT; -- la fecha de movimiento es posterior a la actual

   -- se indica que hacer en caso de ocurrir una excepción
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;

    LET v_codigo_rechazo = v_i_resultado;

    -- se intenta insertar
    EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_alta_tpo_movimiento      ,
                                                        tmp_afi_alta_nrp                 ,
                                                        tmp_afi_alta_f_movimiento        ,
                                                        tmp_afi_alta_curp_rfc            ,
                                                        tmp_afi_alta_t_trabajador        ,
                                                        tmp_afi_alta_nss                 ,
                                                        tmp_afi_alta_nombre              ,
                                                        tmp_afi_alta_presentacion_extemp ,
                                                        tmp_afi_alta_jornada_semana      ,
                                                        tmp_afi_alta_sdi                 ,
                                                        tmp_afi_alta_sexo                ,
                                                        tmp_afi_alta_nss_correcto        ,
                                                        tmp_afi_alta_nombre_correcto     ,
                                                        ""                               ,
                                                        v_codigo_rechazo                 ,
                                                        p_folio);

      RETURN v_i_resultado, isam_err, err_txt;
   END EXCEPTION

   -- Variables que almacenan información para su validación
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de Alta RISS finalizó correctamente";

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_alta_riss_sinf.trace";
   --TRACE ON;

   -- se definen las constantes de codigo de error
   LET v_error_nss_vacio                = 8;
   LET v_error_nss_ya_existe            = 2;
   LET v_error_fecha_movimiento_vacia   = 5;
   LET v_error_nrp_vacio                = 6;
   LET v_error_nombre_vacio             = 7;
   LET v_error_fec_mov_posterior_actual = 9;
   LET v_codigo_rechazo                 = 0;

   -- se inician los contadores de altas aceptadas y rechazadas
   LET v_num_altas_aceptadas  = 0;
   LET v_num_altas_rechazadas = 0;

   -- se asume que el registro es correcto
   LET v_registro_rechazado = 0;
   LET v_solo_nrp           = "0";
   LET v_fecha_valida       = 0;
   LET v_fecha_movimiento   = NULL;
   LET tmp_riss             = NULL;
   LET v_id_riss            = 4;
   LET v_rl                 = 1;
   LET v_f_actualiza        = TODAY;
   LET v_ind_modifica       = 0;
   LET v_ind_alta_riss      = 0;
   LET v_rfc                = "";
   LET v_alta_tipo_trabajador = "I";

   -- ====================================================================
   -- ====================================================================
   --   VERIFICACIÓN DE ERRORES EN LA INFORMACIÓN CARGADA
   -- ====================================================================
   -- ====================================================================

   -- se verifica que la fecha de movimiento exista
   IF ( tmp_afi_alta_f_movimiento IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = v_error_fecha_movimiento_vacia; -- Fecha de movimiento está vacia
   END IF

   -- se valida la fecha de movimiento
   EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_alta_f_movimiento,"ddmmyyyy")
      INTO v_fecha_valida, v_fecha_movimiento;

   -- si la fecha no es valida, es decir, es erronea, no se puede validar el registro
   IF ( v_fecha_valida = 0 ) THEN
      -- fecha invalida
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = v_error_fecha_movimiento_vacia; -- Fecha de movimiento esta vacia
   END IF

   -- se verifica que la fecha de movimiento sea igual o inferior a la actual
   IF ( v_fecha_movimiento > TODAY AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = v_error_fec_mov_posterior_actual; -- La fecha de movimiento es posterior a la actual
   END IF

   -- se verifica que el NSS sea válido
   IF ( tmp_afi_alta_nss IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET tmp_afi_alta_nss = "NULO";
      LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_vacio; -- NSS vacio
   END IF

   -- se verifica si se recibio NRP para crear su relacion laboral
   IF ( tmp_afi_alta_nrp IS NULL ) THEN
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = 6;  -- El NRP no existe
   ELSE
      ---se valida que el nrp sea de riss voluntario
      SELECT nrp
        INTO v_solo_nrp
        FROM cat_riss_nrp
       WHERE nrp    = tmp_afi_alta_nrp
         AND id_nrp = 0;

      IF v_solo_nrp IS NULL OR v_solo_nrp = "0" THEN
         LET v_registro_rechazado = 1;
         LET afi_rch_afiliatorio_cod_rechazo = 21;  -- El NRP no existe
      END IF
   END IF

   SELECT curp_rfc
     INTO v_rfc
     FROM safre_tmp:tmp_afi_rfc_riss
    WHERE nss = tmp_afi_alta_nss;

   IF v_rfc IS NULL THEN
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = 22;  -- Relación RISS NSS-RFC no existe
   END IF

   IF ( v_registro_rechazado = 0 ) THEN
      -- se verifica si el NSS ya se ha dado de alta
      SELECT id_derechohabiente, ap_paterno_af, ap_materno_af, nombre_af, nombre_imss, curp
        INTO v_id_derechohabiente, v_ap_paterno_af, v_ap_materno_af, v_nombre_af, v_nombre_imss, v_curp
        FROM afi_derechohabiente
       WHERE nss = tmp_afi_alta_nss;

      -- si el nss se encontró entonces ya no se agrega, si no existe se da de alta
      IF ( v_id_derechohabiente IS NOT NULL ) THEN
         --TRACE "El NSS ya existia en base de datos";

         --se verifica que no esté dado de alta con RISS voluntario
         SELECT id_riss
           INTO v_id_riss
           FROM afi_riss
          WHERE id_derechohabiente = v_id_derechohabiente
            AND id_riss = 4;

         IF v_id_riss = 4 THEN
            LET v_ind_alta_riss      = 1;
            LET v_registro_rechazado = 1;
            LET afi_rch_afiliatorio_cod_rechazo = 17;  -- El NRP ya existe
         ELSE
            LET v_id_riss = 4;
         END IF
      ELSE
         LET v_ind_modifica = 2;

               -- se invoca el alta del derechohabiente
         EXECUTE FUNCTION fn_apertura_cuenta_riss(tmp_afi_alta_nss                 , -- NSS
                                                  tmp_afi_alta_curp_rfc            , -- CURP
                                                  v_rfc                            , -- RFC
                                                  v_rl                             , -- Ind relacion laboral 
                                                  tmp_afi_alta_nombre              , -- Nombre
                                                  v_alta_tipo_trabajador           , -- tipo trabajador. Determinado segun evaluacion
                                                  0                                , -- id_credito (sin credito)
                                                  p_folio                          , -- folio
                                                  "V"                              , -- origen de afiliacion
                                                  v_fecha_movimiento               ) -- fecha de movimiento
                 INTO v_id_derechohabiente;

         IF ( v_id_derechohabiente IS NOT NULL ) THEN
            LET v_registro_rechazado = 0;
         ELSE
            LET v_registro_rechazado            = 1;
            LET afi_rch_afiliatorio_cod_rechazo = 3; -- NO SE PUDO ABRIR LA CUENTA
         END IF
      END IF

      IF ( v_registro_rechazado = 0 ) THEN
         IF ( v_ind_alta_riss = 0 ) THEN
            INSERT INTO afi_riss
                    (id_derechohabiente,
                     id_riss,
                     folio_lote,
                     f_movimiento,
                     nrp,
                     id_riss_rl,
                     f_proceso,
                     usuario)
             VALUES
                    (v_id_derechohabiente,
                     v_id_riss,
                     p_folio,
                     v_fecha_movimiento,
                     tmp_afi_alta_nrp,
                     v_rl,
                     v_f_actualiza,
                     p_usuario_cod);

            LET v_num_altas_aceptadas = 1;
         END IF

         IF ( v_ind_modifica = 0 )THEN
            CALL fn_separa_nombre(tmp_afi_alta_nombre)
            RETURNING v_paterno, v_materno, v_nombre;

            IF v_paterno <> v_ap_paterno_af THEN
               LET v_ind_modifica = 1;
            END IF

            IF v_materno <> v_ap_materno_af THEN
               LET v_ind_modifica = 1;
            END IF

            IF v_nombre <> v_nombre_af THEN
               LET v_ind_modifica = 1;
            END IF

            IF v_nombre_imss <> tmp_afi_alta_nombre THEN
               LET v_ind_modifica = 1;
            END IF

            IF v_curp <> tmp_afi_alta_curp_rfc THEN
               LET v_ind_modifica = 1;
            END IF

            IF  v_ind_modifica = 1 THEN
               UPDATE afi_derechohabiente
                  SET ap_paterno_af = v_paterno,
                      ap_materno_af = v_materno,
                      nombre_af     = v_nombre,
                      nombre_imss   = tmp_afi_alta_nombre,
                      curp          = tmp_afi_alta_curp_rfc
                WHERE id_derechohabiente = v_id_derechohabiente;
            END IF
         END IF
      END IF
   END IF

   IF ( v_registro_rechazado = 1 ) THEN
      -- se inserta en la tabla de rechazos
      -- no se pudo abrir la cuenta, se registra en rechazos
      LET afi_rch_afiliatorio_tpo_movimiento      = tmp_afi_alta_tpo_movimiento;
      LET afi_rch_afiliatorio_nrp                 = tmp_afi_alta_nrp;
      LET afi_rch_afiliatorio_f_movimiento        = tmp_afi_alta_f_movimiento;
      LET afi_rch_afiliatorio_curp_rfc            = tmp_afi_alta_curp_rfc;
      LET afi_rch_afiliatorio_t_trabajador        = tmp_afi_alta_t_trabajador;
      LET afi_rch_afiliatorio_nss                 = tmp_afi_alta_nss;
      LET afi_rch_afiliatorio_nombre              = tmp_afi_alta_nombre;
      LET afi_rch_afiliatorio_presentacion_extemp = tmp_afi_alta_presentacion_extemp;
      LET afi_rch_afiliatorio_jornada_semana      = tmp_afi_alta_jornada_semana;
      LET afi_rch_afiliatorio_sdi                 = tmp_afi_alta_sdi;
      LET afi_rch_afiliatorio_sexo                = tmp_afi_alta_sexo;
      LET afi_rch_afiliatorio_nss_correcto        = tmp_afi_alta_nss_correcto;
      LET afi_rch_afiliatorio_nombre_correcto     = tmp_afi_alta_nombre_correcto;

      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(afi_rch_afiliatorio_tpo_movimiento      ,
                                                          afi_rch_afiliatorio_nrp                 ,
                                                          afi_rch_afiliatorio_f_movimiento        ,
                                                          afi_rch_afiliatorio_curp_rfc            ,
                                                          afi_rch_afiliatorio_t_trabajador        ,
                                                          afi_rch_afiliatorio_nss                 ,
                                                          afi_rch_afiliatorio_nombre              ,
                                                          afi_rch_afiliatorio_presentacion_extemp ,
                                                          afi_rch_afiliatorio_jornada_semana      ,
                                                          afi_rch_afiliatorio_sdi                 ,
                                                          afi_rch_afiliatorio_sexo                ,
                                                          afi_rch_afiliatorio_nss_correcto        ,
                                                          afi_rch_afiliatorio_nombre_correcto     ,
                                                          ""                                      ,
                                                          afi_rch_afiliatorio_cod_rechazo         ,
                                                          p_folio);

      -- se cuenta una alta rechazada
      LET v_num_altas_rechazadas = 1;

      -- se continua con el siguiente registro
      RETURN afi_rch_afiliatorio_cod_rechazo, isam_err, err_txt;
   END IF

   -- se indica cuantas cuentas se abrieron
   LET err_txt = "Altas RISS realizadas: " || v_num_altas_aceptadas || " Altas RISS rechazadas: " || v_num_altas_rechazadas;

   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_i_resultado, isam_err, err_txt;

END FUNCTION
;


