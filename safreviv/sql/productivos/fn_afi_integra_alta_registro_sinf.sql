






CREATE FUNCTION "safreviv".fn_afi_integra_alta_registro_sinf(p_folio                          DECIMAL(10) ,
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
                                                  p_usuario_cod                    CHAR(20)
                                                  )
   RETURNING INTEGER, INTEGER, VARCHAR(255)

   -- campos de la tabla de rechazos afi_rch_afiliatorio
   DEFINE afi_rch_afiliatorio_tpo_movimiento      CHAR(2)     ;
   DEFINE afi_rch_afiliatorio_espacios            CHAR(2)     ;
   DEFINE afi_rch_afiliatorio_nrp                 CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_f_movimiento        CHAR(8)     ;
   DEFINE afi_rch_afiliatorio_curp_rfc            CHAR(18)    ;
   DEFINE afi_rch_afiliatorio_t_trabajador        DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss                 CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_nombre              CHAR(50)    ;
   DEFINE afi_rch_afiliatorio_presentacion_extemp DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_jornada_semana      DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_sdi                 DECIMAL(6,0);
   DEFINE afi_rch_afiliatorio_sexo                DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss_correcto        CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_nombre_correcto     CHAR(50)    ;
   DEFINE afi_rch_afiliatorio_cod_rechazo         SMALLINT    ;

   -- campos de la tabla afi_relacion_laboral
   DEFINE afi_relacion_laboral_id_derechohabiente DECIMAL(9,0);
   DEFINE afi_relacion_laboral_nrp                CHAR(11)    ;
   DEFINE afi_relacion_laboral_f_alta_nrp         DATE        ;
   DEFINE afi_relacion_laboral_ind_relacion       SMALLINT    ;
   DEFINE afi_relacion_laboral_folio_lote         DECIMAL(9,0);
   DEFINE afi_relacion_laboral_f_actualiza        DATE        ;
   DEFINE afi_relacion_laboral_usuario            CHAR(20)    ;
   DEFINE v_alta_tipo_trabajador                  CHAR(1)     ;

   -- para cambiar el formato de la fecha
   DEFINE v_fecha_texto                           VARCHAR(10);
   DEFINE v_rfc                                   VARCHAR(13);
   DEFINE v_curp                                  VARCHAR(18);
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
   DEFINE tmp_riss_imss                           SMALLINT;

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
   DEFINE v_num_altas_aceptadas                   INTEGER;
   DEFINE v_num_altas_rechazadas                  INTEGER;
   DEFINE v_registro_rechazado                    SMALLINT; -- booleana para verificar un registro rechazado
   DEFINE v_solo_NRP                              SMALLINT; -- para indicar si solo se debe dar de alta el NRP

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
   LET err_txt            = "El proceso de integración de Alta finalizó correctamente";

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_alta_sinf.trace";

   -- se definen las constantes de codigo de error
   LET v_error_nss_vacio                = 8;
   LET v_error_nss_ya_existe            = 2;
   LET v_error_fecha_movimiento_vacia   = 5;
   LET v_error_nrp_vacio                = 6;
   LET v_error_nombre_vacio             = 7;
   LET v_error_fec_mov_posterior_actual = 9;
   LET v_codigo_rechazo                 = 0;
   LET tmp_riss_imss                    = NULL;

   -- se inician los contadores de altas aceptadas y rechazadas
   LET v_num_altas_aceptadas  = 0;
   LET v_num_altas_rechazadas = 0;

   -- se asume que el registro es correcto
   LET v_registro_rechazado = 0;
   LET v_solo_NRP           = 0;
   LET v_fecha_valida       = 0;
   LET v_fecha_movimiento   = NULL;
   LET tmp_riss_imss = NULL;

   -- ====================================================================
   -- ====================================================================
   --   VERIFICACION DE ERRORES EN LA INFORMACION CARGADA
   -- ====================================================================
   -- ====================================================================

   -- se verifica que la fecha de movimiento exista
   IF ( tmp_afi_alta_f_movimiento IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;

      LET afi_rch_afiliatorio_cod_rechazo = v_error_fecha_movimiento_vacia; -- Fecha de movimiento esta vacia
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

   -- se verifica que el NSS venga
   IF ( tmp_afi_alta_nss IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET tmp_afi_alta_nss = "NULO";
      LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_vacio; -- NSS vacio
   END IF

   -- se verifica si el NSS ya se ha dado de alta
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss = tmp_afi_alta_nss;

   -- si el nss se encontro entonces ya no se agrega
   IF ( v_id_derechohabiente IS NOT NULL AND v_registro_rechazado = 0 ) THEN
      --TRACE "El NSS ya existia en base de datos";

      -- se verifica si el tipo de trabajador recibido es 5
      IF ( tmp_afi_alta_t_trabajador = "5" ) THEN
         -- se verifica si ya existe asi en afi_derechohabiente
         SELECT tipo_trabajador
         INTO   v_alta_tipo_trabajador
         FROM   afi_derechohabiente
         WHERE  id_derechohabiente = v_id_derechohabiente;

         -- si no esta como tipo E, entonces se actualiza a tipo E
         IF ( v_alta_tipo_trabajador <> "E" ) THEN
            UPDATE afi_derechohabiente
            SET    tipo_trabajador = "E"
            WHERE  id_derechohabiente = v_id_derechohabiente;

            -- se registra el cambio
            -- la tabla de historico no tiene tipo de trabajador
            INSERT INTO afi_his_derechohabiente (
                    id_derechohabiente   ,
                    f_modifica           ,
                    folio_lote_modifica  ,
                    ind_modifica         ,
                    curp                 ,
                    rfc                  ,
                    ind_nrp              ,
                    f_nacimiento         ,
                    nombre_imss          ,
                    nombre_af            ,
                    ap_paterno_af        ,
                    ap_materno_af        
            ) VALUES (
                    v_id_derechohabiente,
                    TODAY               ,
                    p_folio             ,
                    14                  , -- cambio a tipo de trabajado EDO-Municipio
                    NULL                ,
                    NULL                ,
                    NULL                ,
                    NULL                ,
                    NULL                ,
                    NULL                ,
                    NULL                ,
                    NULL                );
         END IF
      END IF

      SELECT nrp
      INTO   afi_relacion_laboral_nrp
      FROM   afi_relacion_laboral
      WHERE  nrp = tmp_afi_alta_nrp
      AND    id_derechohabiente = v_id_derechohabiente;

      IF ( afi_relacion_laboral_nrp IS NULL ) THEN
         -- se indica que solo se de de alta el NRP
         LET v_solo_NRP = 1;
      ELSE
         -- se marca el rechazo porque ya existe el NSS y el NRP
         LET v_registro_rechazado = 1;
         LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_ya_existe; -- NSS YA EXISTE
      END IF

   END IF

   -- se verifica que el NRP exista
   IF ( tmp_afi_alta_nrp IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET tmp_afi_alta_nrp = "NULO";
      LET afi_rch_afiliatorio_cod_rechazo = v_error_nrp_vacio; -- NRP vacio
   END IF

   -- se verifica que el nombre exista
   IF ( tmp_afi_alta_nombre IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET tmp_afi_alta_nombre = "NULO";
      LET afi_rch_afiliatorio_cod_rechazo = v_error_nombre_vacio; -- Nombre vacio
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
      LET v_num_altas_rechazadas = v_num_altas_rechazadas + 1;

      -- se continua con el siguiente registro
      RETURN afi_rch_afiliatorio_cod_rechazo, isam_err, err_txt;
   END IF

   -- se asigna el curp/rfc
   LET v_rfc_curp = tmp_afi_alta_curp_rfc;

   -- se verifica si se recibio RFC o CURP
   IF ( LENGTH(v_rfc_curp) < 14 ) THEN
      -- es un RFC
      LET v_rfc  = v_rfc_curp;
      LET v_curp = NULL;
   ELSE -- se tiene una curp
      LET v_rfc  = v_rfc_curp[1,10];
      LET v_curp = v_rfc_curp;
   END IF

   -- se verifica si se recibio NRP para crear su relacion laboral
   IF ( tmp_afi_alta_nrp IS NOT NULL ) THEN
      -- con relacion laboral
      LET afi_relacion_laboral_ind_relacion = 1;
   ELSE
      -- sin relacion laboral
      LET afi_relacion_laboral_ind_relacion = 0;
   END IF

   -- si se marco que solo se de de alta el NRP (v_solo_NRP=1) entonces no se invoca el alta
   -- de derechohabiente
   IF ( v_solo_NRP = 0 ) THEN

      -- 30Mayo2013. Se verifica el tipo de trabajador para saber como se registrara
      IF ( tmp_afi_alta_t_trabajador = "5" ) THEN
         LET v_alta_tipo_trabajador = "E"; -- especial
      ELSE
         LET v_alta_tipo_trabajador = "S"; -- solo infonavit
      END IF

      -- se invoca el alta del derechohabiente
      EXECUTE FUNCTION fn_apertura_cuenta_afi(tmp_afi_alta_nss                 , -- NSS
                                              v_curp                           , -- CURP
                                              v_rfc                            , -- RFC
                                              afi_relacion_laboral_ind_relacion, -- Ind relacion laboral 
                                              tmp_afi_alta_nombre              , -- Nombre
                                              v_alta_tipo_trabajador           , -- tipo trabajador. Determinado segun evaluacion
                                              0                                , -- id_credito (sin credito)
                                              p_folio                          , -- folio
                                              "A"      )                        -- origen de afiliacion
                    INTO v_id_derechohabiente;

      -- si la cuenta se abrió sin problemas
      IF ( v_id_derechohabiente > 0 ) THEN
         -- para tipo de trabajador E
         IF ( v_alta_tipo_trabajador = "E" ) THEN

            -- se separa el nombre en apellidos y nombres de pila
            CALL fn_separa_nombre(tmp_afi_alta_nombre)
                 RETURNING v_ap_paterno_af, v_ap_materno_af, v_nombre_af;

            -- se debe dar de alta tambien en la tabla afi_nuevo_trabajador
            INSERT INTO afi_nuevo_trabajador (
               id_derechohabiente ,
               nss                ,
               rfc                ,
               curp               ,
               ap_paterno         ,
               ap_materno         ,
               nombre             ,
               f_apertura         ,
               tipo_trabajador    )
            VALUES (
               v_id_derechohabiente   ,
               tmp_afi_alta_nss       ,
               v_rfc                  ,
               v_curp                 ,
               v_ap_paterno_af        ,
               v_ap_materno_af        ,
               v_nombre_af            ,
               TODAY                  ,
               v_alta_tipo_trabajador);
         END IF
      END IF
   END IF

   IF ( v_id_derechohabiente IS NOT NULL ) THEN
      -- se cuenta un derechohabiente dado de alta
      LET v_num_altas_aceptadas = v_num_altas_aceptadas + 1;

      -- se agrega en relacion laboral
      LET afi_relacion_laboral_id_derechohabiente = v_id_derechohabiente;
      LET afi_relacion_laboral_nrp                = tmp_afi_alta_nrp;
      LET afi_relacion_laboral_f_alta_nrp         = v_fecha_movimiento;
      LET afi_relacion_laboral_ind_relacion       = 1;
      LET afi_relacion_laboral_folio_lote         = p_folio;
      LET afi_relacion_laboral_f_actualiza        = TODAY;
      LET afi_relacion_laboral_usuario            = p_usuario_cod;

      -- se inserta en relacion laboral
      INSERT INTO afi_relacion_laboral (
         id_derechohabiente ,
         nrp                ,
         f_alta_nrp         ,
         ind_relacion       ,
         folio_lote         ,
         f_actualiza        ,
         usuario            )
      VALUES (
         afi_relacion_laboral_id_derechohabiente ,
         afi_relacion_laboral_nrp                ,
         afi_relacion_laboral_f_alta_nrp         ,
         afi_relacion_laboral_ind_relacion       ,
         afi_relacion_laboral_folio_lote         ,
         afi_relacion_laboral_f_actualiza        ,
         afi_relacion_laboral_usuario            );
   ELSE
      -- no se pudo abrir la cuenta, se registra en rechazos
      LET afi_rch_afiliatorio_tpo_movimiento      = tmp_afi_alta_tpo_movimiento;
      LET afi_rch_afiliatorio_espacios            = "  ";
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
      LET afi_rch_afiliatorio_cod_rechazo         = 3; -- NO SE PUDO ABRIR LA CUENTA

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
      LET v_num_altas_rechazadas = v_num_altas_rechazadas + 1;
   END IF

   -- se indica cuantas cuentas se abrieron
   LET err_txt = "Altas realizadas: " || v_num_altas_aceptadas || " Altas rechazadas: " || v_num_altas_rechazadas;

   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_i_resultado, isam_err, err_txt;

END FUNCTION
;


