






CREATE FUNCTION "safreviv".fn_afi_integra_alta_registro(p_folio                          DECIMAL(10) ,
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
                                             tmp_afi_riss_imss                SMALLINT    ,
                                             tmp_afi_riss_inf                 SMALLINT    ,
                                             p_usuario_cod                    CHAR(20))
   RETURNING SMALLINT, SMALLINT, VARCHAR(255)

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
   DEFINE afi_rch_afiliatorio_riss_imss           SMALLINT    ;
   DEFINE afi_rch_afiliatorio_cod_rechazo         SMALLINT    ;

   -- campos de la tabla afi_relacion_laboral
   DEFINE afi_relacion_laboral_id_derechohabiente DECIMAL(9,0);
   DEFINE afi_relacion_laboral_nrp                CHAR(11)    ;
   DEFINE afi_relacion_laboral_f_alta_nrp         DATE        ;
   DEFINE afi_relacion_laboral_ind_relacion       SMALLINT    ;
   DEFINE afi_relacion_laboral_folio_lote         DECIMAL(9,0);
   DEFINE afi_relacion_laboral_f_actualiza        DATE        ;
   DEFINE afi_relacion_laboral_usuario            CHAR(20)    ;

   -- para cambiar el formato de la fecha
   DEFINE v_fecha_texto                           VARCHAR(10);

   -- variables de identificación del derechohabiente
   DEFINE v_rfc                                   VARCHAR(13);
   DEFINE v_curp                                  VARCHAR(18);
   DEFINE v_rfc_curp                              VARCHAR(18);
   DEFINE v_id_riss                               SMALLINT;
   DEFINE v_nrp                                   CHAR(11);

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);
   DEFINE v_fecha_valida                          SMALLINT;
   DEFINE v_fecha_movimiento                      DATE;
   DEFINE v_codigo_rechazo                        SMALLINT;
   DEFINE v_tot_riss                              SMALLINT;
   DEFINE v_riss                                  SMALLINT;
   DEFINE v_rl                                    SMALLINT;
   DEFINE v                                       SMALLINT;
   DEFINE v_numero                                SMALLINT;
   DEFINE v_no_nss                                CHAR(1);
   DEFINE v_cont                                  SMALLINT;

   -- Variables de validaciones
   DEFINE v_d_id_referencia                       DECIMAL(9,0);
   DEFINE v_id_derechohabiente                    DECIMAL(9,0);
   DEFINE v_id_derechohabiente_unificado          DECIMAL(9,0);
   DEFINE v_diagnostico_unificador                SMALLINT;
   DEFINE v_diagnostico_unificadas                SMALLINT;
   DEFINE v_diagnostico_rechazo                   SMALLINT;

   -- número de altas aceptadas y rechazadas
   DEFINE v_altas_aceptadas                       SMALLINT;
   DEFINE v_altas_rechazadas                      SMALLINT;
   DEFINE v_rechazo                               SMALLINT;
   DEFINE v_altas_riss_acep                       SMALLINT;
   DEFINE v_altas_riss_rech                       SMALLINT;
   DEFINE v_registro_rechazado                    SMALLINT; -- booleana para verificar un registro rechazado
   DEFINE v_solo_NRP                              SMALLINT; -- para indicar si solo se debe dar de alta el NRP
   DEFINE v_f_actualiza                           DATE;     -- para indicar fecha de riss histórico

   -- Variable para marca de cuenta
   DEFINE v_i_estado_marca                        INTEGER;

   -- constantes para códigos de error
   DEFINE v_error_nss_vacio                       SMALLINT;
   DEFINE v_error_nss_ya_existe                   SMALLINT;
   DEFINE v_error_fecha_movimiento_vacia          SMALLINT;
   DEFINE v_error_nrp_vacio                       SMALLINT;
   DEFINE v_error_nombre_vacio                    SMALLINT;
   DEFINE v_error_fec_mov_posterior_actual        SMALLINT; -- la fecha de movimiento es posterior a la actual
   DEFINE v_error_riss                            SMALLINT;
   DEFINE v_error_nss_inconsistente               SMALLINT;

   DEFINE v_rfc_raiz                              CHAR(4);
   DEFINE v_evalua_curp                           CHAR(1);
   DEFINE v_ind_curp                              SMALLINT;
   DEFINE v_rfc_fecha                             CHAR(6);

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      LET v_codigo_rechazo = v_i_resultado;

      IF tmp_afi_alta_tpo_movimiento = '01' THEN
         LET v_i_resultado      = -1;
      ELSE
         LET tmp_afi_alta_tpo_movimiento = '21';
         LET v_i_resultado               = -21;
      END IF

      -- se intenta insertar
      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_alta_tpo_movimiento,
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
                                                          tmp_afi_riss_imss                ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);

      RETURN v_i_resultado, isam_err, err_txt;
   END EXCEPTION

   ---SET DEBUG FILE TO "/safreviv_int/archivos/fn_afi_integra_alta.trace";
   ---TRACE ON;

   SET PDQPRIORITY HIGH;

   -- Variables que almacenan informacion para su validacion
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET v_codigo_rechazo   = 0;
   LET v_tot_riss         = 0;
   LET v_rl               = 0; 
   LET v_numero           = 0;

   IF tmp_afi_alta_tpo_movimiento = '01' THEN
      LET err_txt                 = "El proceso de integración de Alta finalizó correctamente";
      LET v_i_resultado           = 1;
   ELSE
      LET err_txt                     = "El proceso de integración de Alta RISS finalizó correctamente";
      LET tmp_afi_alta_tpo_movimiento = '21';
      LET v_i_resultado               = 21;
   END IF

   -- se definen las constantes de código de error
   LET v_error_nss_ya_existe            = 2;
   LET v_error_fecha_movimiento_vacia   = 5;
   LET v_error_nrp_vacio                = 6;
   LET v_error_nombre_vacio             = 7;
   LET v_error_nss_vacio                = 8;
   LET v_error_fec_mov_posterior_actual = 9;
   LET v_error_nss_inconsistente        = 14;
   --LET v_error_riss                     = 15;

   -- se inician los contadores de altas aceptadas y rechazadas
   LET v_altas_aceptadas  = 0;
   LET v_altas_rechazadas = 0;
   LET v_altas_riss_acep  = 0;
   LET v_altas_riss_rech  = 0;
   LET v_rechazo          = 0;

   --------------------------- ALTA ----------------------------- 
   -- se verifica el registro recibido

   -- se asume que el registro es correcto
   LET v_registro_rechazado = 0;
   LET v_solo_NRP           = 0;
   LET v_fecha_valida       = 0;
   LET v_fecha_movimiento   = NULL;
   LET v_id_riss            = NULL;
   LET v_id_derechohabiente = NULL;
   LET v_nrp                = NULL;
   LET v_riss               = 0;
   LET v_f_actualiza        = TODAY;

   -- ====================================================================
   -- ====================================================================
   --   VERIFICACIÓN DE ERRORES EN LA INFORMACIÓN CARGADA
   -- ====================================================================
   -- ====================================================================

   -- se verifica que la fecha de movimiento exista
   IF ( tmp_afi_alta_f_movimiento IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = v_error_fecha_movimiento_vacia; -- Fecha de movimiento esta vacía
   END IF

   -- se valida la fecha de movimiento
   EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_alta_f_movimiento,"ddmmyyyy")
      INTO v_fecha_valida, v_fecha_movimiento;

   -- si la fecha no es válida, es decir, es errónea, no se puede validar el registro
   IF ( v_fecha_valida = 0 ) THEN
      -- fecha invalida
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = v_error_fecha_movimiento_vacia; -- Fecha de movimiento esta vacia
   END IF

   -- se verifica que la fecha de movimiento sea igual o anterior a la actual
   IF ( v_fecha_movimiento > TODAY AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET afi_rch_afiliatorio_cod_rechazo = v_error_fec_mov_posterior_actual; -- La fecha de movimiento es posterior a la actual
   END IF

   -- se verifica que el NSS no sea nulo
   IF v_registro_rechazado = 0 THEN
      IF tmp_afi_alta_nss IS NULL OR tmp_afi_alta_nss[1] = " " THEN
         -- se marca el rechazo
         LET v_registro_rechazado = 1;
         LET tmp_afi_alta_nss     = "NULO";
         LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_vacio; -- NSS vacío
      ELSE
         IF (tmp_afi_alta_nss[1]  NOT MATCHES '[0-9]*') OR 
            (tmp_afi_alta_nss[2]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[3]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[4]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[5]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[6]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[7]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[8]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[9]  NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[10] NOT MATCHES '[0-9]*') OR
            (tmp_afi_alta_nss[11] NOT MATCHES '[0-9]*') THEN
            LET v_registro_rechazado = 1;
         END IF
         --FOR v =1 TO 11
            --LET v_no_nss = tmp_afi_alta_nss[v,v];
            --EXECUTE FUNCTION fn_es_numero(v_no_nss)
            --INTO v_numero;
            --IF v_numero = 0 THEN
               --LET v_registro_rechazado = 1;
               --LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_inconsistente; -- El nss es inválido
               --EXIT FOR;
            --END IF
         --END FOR;
      END IF
   END IF

   -- si el nss se encontró entonces ya no se agrega
   IF ( v_registro_rechazado = 0 ) THEN
      -- se verifica si el NSS ya se ha dado de alta
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_afi_alta_nss;

      IF ( v_id_derechohabiente IS NOT NULL ) THEN
         --TRACE "El NSS ya existe en base de datos";
         -- se revisa si ya tiene NRP
         SELECT nrp
           INTO afi_relacion_laboral_nrp
           FROM afi_relacion_laboral
          WHERE id_derechohabiente = v_id_derechohabiente
            AND nrp = tmp_afi_alta_nrp;

         IF ( afi_relacion_laboral_nrp IS NULL ) THEN -- se indica que solo se debe dar de alta el NRP
            LET v_solo_NRP = 1;
         ELSE
            IF tmp_afi_alta_tpo_movimiento = '01' THEN
               -- se marca el rechazo porque ya existe el NSS y el NRP
               LET v_registro_rechazado = 1;
               LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_ya_existe; -- NSS YA EXISTE
            ELSE
               LET v_solo_NRP = 2;
               LET v_rl       = 1;
            END IF
         END IF
      ELSE
         IF tmp_afi_alta_tpo_movimiento = '01' THEN
            LET v_solo_NRP = 1;
         ELSE
            LET v_registro_rechazado            = 1;
            LET v_error_riss                    = 1;
            LET afi_rch_afiliatorio_cod_rechazo = 1;
         END IF
      END IF
   END IF

   -- se verifica que el NRP exista
   IF ( tmp_afi_alta_nrp IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET tmp_afi_alta_nrp     = "NULO";
      LET afi_rch_afiliatorio_cod_rechazo = v_error_nrp_vacio; -- NRP vacio
   END IF

   -- se verifica que el nombre exista
   IF ( tmp_afi_alta_nombre IS NULL AND v_registro_rechazado = 0 ) THEN
      -- se marca el rechazo
      LET v_registro_rechazado = 1;
      LET tmp_afi_alta_nombre  = "NULO";
      LET afi_rch_afiliatorio_cod_rechazo = v_error_nombre_vacio; -- Nombre vacio
   END IF

   IF ( v_registro_rechazado = 0 ) THEN
      IF tmp_afi_alta_tpo_movimiento = '21' AND
         tmp_afi_riss_imss <> 1 AND
         tmp_afi_riss_imss <> 2 THEN
         LET v_registro_rechazado = 1;
         LET afi_rch_afiliatorio_cod_rechazo = 15;  --marca RISS errónea
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
      LET afi_rch_afiliatorio_riss_imss           = tmp_afi_riss_imss;

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
                                                          afi_rch_afiliatorio_riss_imss           ,
                                                          afi_rch_afiliatorio_cod_rechazo         ,
                                                          p_folio);

      IF tmp_afi_alta_tpo_movimiento = '01' THEN
         -- se cuenta una alta rechazada
         LET v_altas_rechazadas = -1;
         LET v_rechazo          = v_altas_rechazadas;
      ELSE
         LET v_altas_riss_rech  = -21;
         LET v_rechazo          = v_altas_riss_rech;
      END IF

      -- se continua con el siguiente registro
      RETURN v_altas_rechazadas, isam_err, err_txt;
   END IF

   -- se asigna el curp/rfc
   LET v_rfc_curp  = tmp_afi_alta_curp_rfc;
   LET v_rfc_raiz  = NULL;
   LET v_rfc_fecha = 0;

   --Punto 1
   IF (LENGTH (v_rfc_curp) <> 13) AND
      (LENGTH (v_rfc_curp) <> 18) THEN
       LET v_rfc = NULL;
       LET v_curp = NULL;
   ELSE
     --Punto 2
      IF (LENGTH (v_rfc_curp) = 13) THEN
         LET v_rfc  = v_rfc_curp;
         LET v_curp = "";

         IF (v_rfc_curp = "0000000000000") THEN
             LET v_rfc = NULL;
             LET v_curp = NULL;
         END IF

         IF (v_rfc[1]  = ' ') OR
            (v_rfc[2]  = ' ') OR
            (v_rfc[3]  = ' ') OR
            (v_rfc[4]  = ' ') OR
            (v_rfc[5]  = ' ') OR
            (v_rfc[6]  = ' ') OR
            (v_rfc[7]  = ' ') OR
            (v_rfc[8]  = ' ') OR
            (v_rfc[9]  = ' ') OR
            (v_rfc[10] = ' ') OR
            (v_rfc[11] = ' ') OR
            (v_rfc[12] = ' ') OR
            (v_rfc[13] = ' ') THEN
            LET v_ind_curp = 1; -- Prende Bandera de la CURP errónea
         END IF

         IF (v_rfc[13] <> "A") AND
            (v_rfc[13] NOT MATCHES '[0-9]') THEN

            -- Elimina la homoclave
            LET v_rfc[11,13] = NULL;
         END IF

         IF (v_rfc[11,13] = "000") THEN
            --Elimina la homoclave
            LET v_rfc [11,13] = NULL;
         END IF

         IF (v_rfc[1]  NOT MATCHES '[0-9]' AND v_rfc[1]  NOT MATCHES '[A-Z]') OR
            (v_rfc[2]  NOT MATCHES '[0-9]' AND v_rfc[2]  NOT MATCHES '[A-Z]') OR
            (v_rfc[3]  NOT MATCHES '[0-9]' AND v_rfc[3]  NOT MATCHES '[A-Z]') OR
            (v_rfc[4]  NOT MATCHES '[0-9]' AND v_rfc[4]  NOT MATCHES '[A-Z]') OR
            (v_rfc[5]  NOT MATCHES '[0-9]' AND v_rfc[5]  NOT MATCHES '[A-Z]') OR
            (v_rfc[6]  NOT MATCHES '[0-9]' AND v_rfc[6]  NOT MATCHES '[A-Z]') OR
            (v_rfc[7]  NOT MATCHES '[0-9]' AND v_rfc[7]  NOT MATCHES '[A-Z]') OR
            (v_rfc[8]  NOT MATCHES '[0-9]' AND v_rfc[8]  NOT MATCHES '[A-Z]') OR
            (v_rfc[9]  NOT MATCHES '[0-9]' AND v_rfc[9]  NOT MATCHES '[A-Z]') OR
            (v_rfc[10] NOT MATCHES '[0-9]' AND v_rfc[10] NOT MATCHES '[A-Z]') OR
            (v_rfc[11] NOT MATCHES '[0-9]' AND v_rfc[11] NOT MATCHES '[A-Z]') OR
            (v_rfc[12] NOT MATCHES '[0-9]' AND v_rfc[12] NOT MATCHES '[A-Z]') OR
            (v_rfc[13] NOT MATCHES '[0-9]' AND v_rfc[13] NOT MATCHES '[A-Z]') THEN

            LET v_rfc = "";
         END IF

         IF (v_rfc[1] NOT MATCHES '[A-Z]') OR
            (v_rfc[2] NOT MATCHES '[A-Z]') OR
            (v_rfc[3] NOT MATCHES '[A-Z]') OR
            (v_rfc[4] NOT MATCHES '[A-Z]') THEN

            LET v_rfc = "";
         END IF

         IF (v_rfc[5]  NOT MATCHES '[0-9]') OR
            (v_rfc[6]  NOT MATCHES '[0-9]') OR
            (v_rfc[7]  NOT MATCHES '[0-9]') OR
            (v_rfc[8]  NOT MATCHES '[0-9]') OR
            (v_rfc[9]  NOT MATCHES '[0-9]') OR
            (v_rfc[10] NOT MATCHES '[0-9]') THEN

            LET v_rfc = "";
         END IF
      ELSE
         IF (v_rfc_curp = "000000000000000000") THEN
             LET v_rfc = NULL;
             LET v_curp = NULL;
         END IF 

         --Punto 3 y 4 
         IF (v_rfc_curp <> "000000000000000000") THEN

             -- Realiza asignación de cadenas
             LET v_curp = v_rfc_curp;

             LET v_ind_curp = 0; -- Se apaga bandera indicando que la CURP está correcta.

            {
             -- Evalúa la CURP
             FOR v_cont = 1 TO 18
                LET v_evalua_curp = v_curp[v_cont];

                IF (v_evalua_curp = " ") THEN
                   LET v_ind_curp = 1; -- Prende Bandera de la CURP errónea
                   EXIT FOR;
                END IF 
             END FOR
            }


            IF (v_curp[1]  = ' ') OR 
               (v_curp[2]  = ' ') OR
               (v_curp[3]  = ' ') OR
               (v_curp[4]  = ' ') OR
               (v_curp[5]  = ' ') OR
               (v_curp[6]  = ' ') OR
               (v_curp[7]  = ' ') OR
               (v_curp[8]  = ' ') OR
               (v_curp[9]  = ' ') OR
               (v_curp[10] = ' ') OR
               (v_curp[11] = ' ') OR
               (v_curp[12] = ' ') OR            
               (v_curp[13] = ' ') OR
               (v_curp[14] = ' ') OR
               (v_curp[15] = ' ') OR
               (v_curp[16] = ' ') OR
               (v_curp[17] = ' ') OR
               (v_curp[18] = ' ') THEN
               LET v_ind_curp = 1; -- Prende Bandera de la CURP errónea
           END IF

           IF (v_curp[1] NOT MATCHES '[A-Z]') OR
              (v_curp[2] NOT MATCHES '[A-Z]') OR
              (v_curp[3] NOT MATCHES '[A-Z]') OR
              (v_curp[4] NOT MATCHES '[A-Z]') THEN

               LET v_ind_curp = 1; -- Prende Bandera de la CURP errónea
           END IF

         IF (v_curp[1] = '0-9') OR
            (v_curp[2] = '0-9') OR
            (v_curp[3] = '0-9') OR
            (v_curp[4] = '0-9') OR
            (v_curp[5] = '0-9') OR
            (v_curp[6] = '0-9') OR
            (v_curp[7] = '0-9') OR
            (v_curp[8] = '0-9') OR
            (v_curp[9] = '0-9') OR
            (v_curp[10]= '0-9') THEN

            LET v_ind_curp = 1; --Enciende bandera de que encontro un numero
         END IF

         IF (v_curp[1]  NOT MATCHES '[0-9]' AND v_curp[1]  NOT MATCHES '[A-Z]') OR
            (v_curp[2]  NOT MATCHES '[0-9]' AND v_curp[2]  NOT MATCHES '[A-Z]') OR
            (v_curp[3]  NOT MATCHES '[0-9]' AND v_curp[3]  NOT MATCHES '[A-Z]') OR
            (v_curp[4]  NOT MATCHES '[0-9]' AND v_curp[4]  NOT MATCHES '[A-Z]') OR
            (v_curp[5]  NOT MATCHES '[0-9]' AND v_curp[5]  NOT MATCHES '[A-Z]') OR
            (v_curp[6]  NOT MATCHES '[0-9]' AND v_curp[6]  NOT MATCHES '[A-Z]') OR
            (v_curp[7]  NOT MATCHES '[0-9]' AND v_curp[7]  NOT MATCHES '[A-Z]') OR
            (v_curp[8]  NOT MATCHES '[0-9]' AND v_curp[8]  NOT MATCHES '[A-Z]') OR
            (v_curp[9]  NOT MATCHES '[0-9]' AND v_curp[9]  NOT MATCHES '[A-Z]') OR
            (v_curp[10] NOT MATCHES '[0-9]' AND v_curp[10] NOT MATCHES '[A-Z]') OR
            (v_curp[11] NOT MATCHES '[0-9]' AND v_curp[11] NOT MATCHES '[A-Z]') OR
            (v_curp[12] NOT MATCHES '[0-9]' AND v_curp[12] NOT MATCHES '[A-Z]') OR
            (v_curp[13] NOT MATCHES '[0-9]' AND v_curp[13] NOT MATCHES '[A-Z]') OR
            (v_curp[14] NOT MATCHES '[0-9]' AND v_curp[14] NOT MATCHES '[A-Z]') OR
            (v_curp[15] NOT MATCHES '[0-9]' AND v_curp[15] NOT MATCHES '[A-Z]') OR
            (v_curp[16] NOT MATCHES '[0-9]' AND v_curp[16] NOT MATCHES '[A-Z]') OR
            (v_curp[17] NOT MATCHES '[0-9]' AND v_curp[17] NOT MATCHES '[A-Z]') OR
            (v_curp[18] NOT MATCHES '[0-9]' AND v_curp[18] NOT MATCHES '[A-Z]') THEN

            LET v_ind_curp = 1;  --Enciende bandera de que encontro un caracter especial
         END IF

             IF (v_ind_curp = 1) THEN
                LET v_curp = NULL;
                LET v_rfc  = NULL;
             ELSE
                -- Se forma la raíz del RFC
                LET v_rfc = v_curp[1,10];

                -- Busca si las primeras 4 posiciones del rfc es una palabra inconveniente (Punto 5)
                FOREACH
                   SELECT raiz_rfc
                     INTO v_rfc_raiz
                     FROM cat_inconveniente
                    WHERE raiz_curp = v_rfc[1,4]
                   ORDER BY raiz_rfc
                END FOREACH;

                -- Si existe la  palabra inconveniente
                IF(v_rfc_raiz IS NOT NULL) THEN 
                   --LET v_rfc_fecha = v_rfc[5,10];
                   LET v_rfc = v_rfc_raiz||v_curp[5,10];

                   LET v_rfc_raiz  = NULL;
                END IF 

             END IF
         END IF
      END IF 
   END IF
  

   -- se verifica si se recibió NRP para crear su relación laboral
   IF ( tmp_afi_alta_nrp IS NOT NULL ) THEN
      -- con relacion laboral
      LET afi_relacion_laboral_ind_relacion = 1;
   ELSE
      -- sin relacion laboral
      LET afi_relacion_laboral_ind_relacion = 0;
   END IF

   -- si se marcó que solo se dé de alta el NRP (v_solo_NRP = 1) entonces no se invoca el alta
   -- de derechohabiente
   IF v_id_derechohabiente IS NULL THEN
      -- se invoca el alta del derechohabiente
      EXECUTE FUNCTION fn_apertura_cuenta_afi(tmp_afi_alta_nss                  ,-- NSS
                                              v_curp                            ,-- CURP
                                              v_rfc                             ,-- RFC
                                              afi_relacion_laboral_ind_relacion ,-- Ind relacion laboral 
                                              tmp_afi_alta_nombre               ,-- Nombre
                                              "I"                               ,-- tipo trabajador (IMSS)
                                              0                                 ,-- id_credito (sin credito)
                                              p_folio                           ,-- folio         
                                              "A"                               )-- origen de afiliacion
                    INTO v_id_derechohabiente;
   END IF

   IF ( v_id_derechohabiente IS NOT NULL ) THEN
      IF ( v_solo_NRP = 1 ) THEN
         IF tmp_afi_alta_tpo_movimiento = '01' THEN
            -- se agrega en relacion laboral
            LET afi_relacion_laboral_id_derechohabiente = v_id_derechohabiente;
            LET afi_relacion_laboral_nrp                = tmp_afi_alta_nrp;
            LET afi_relacion_laboral_f_alta_nrp         = v_fecha_movimiento;
            LET afi_relacion_laboral_ind_relacion       = 1;
            LET afi_relacion_laboral_folio_lote         = p_folio;
            LET afi_relacion_laboral_f_actualiza        = TODAY;
            LET afi_relacion_laboral_usuario            = p_usuario_cod;

            -- se inserta en relacion laboral
            INSERT INTO afi_relacion_laboral
                       (id_derechohabiente ,
                        nrp                ,
                        f_alta_nrp         ,
                        ind_relacion       ,
                        folio_lote         ,
                        f_actualiza        ,
                        usuario)
                VALUES (afi_relacion_laboral_id_derechohabiente ,
                        afi_relacion_laboral_nrp                ,
                        afi_relacion_laboral_f_alta_nrp         ,
                        afi_relacion_laboral_ind_relacion       ,
                        afi_relacion_laboral_folio_lote         ,
                        afi_relacion_laboral_f_actualiza        ,
                        afi_relacion_laboral_usuario);
         END IF

         -- se actualizan los posibles registros de riss que no tenían la RL
         UPDATE afi_riss
            SET id_riss_rl = 1
          WHERE id_derechohabiente = v_id_derechohabiente
            AND nrp = afi_relacion_laboral_nrp
            AND id_riss IN(1,2)
            AND id_riss_rl = 0;

         UPDATE afi_riss
            SET id_riss_rl = 2
          WHERE id_derechohabiente = v_id_derechohabiente
            AND nrp = afi_relacion_laboral_nrp
            AND id_riss = 3
            AND id_riss_rl = 4;

         UPDATE afi_riss
            SET id_riss_rl = 3
          WHERE id_derechohabiente = v_id_derechohabiente
            AND nrp = afi_relacion_laboral_nrp
            AND id_riss = 3
            AND id_riss_rl = 5;
      END IF

      IF tmp_afi_alta_tpo_movimiento = '21' THEN
         IF tmp_afi_riss_imss = 1 OR tmp_afi_riss_imss = 2 THEN
            FOREACH
               SELECT rl.nrp, rl.id_riss
                 INTO v_nrp, v_riss
                 FROM afi_riss rl
                WHERE rl.id_derechohabiente = v_id_derechohabiente
                  AND rl.nrp                = tmp_afi_alta_nrp
                  AND rl.id_riss            IN(1,2) -- tmp_afi_riss_imss

                IF v_nrp IS NULL THEN
                   LET v_nrp = "0";
                ELSE
                   LET v_tot_riss = 1;
                END IF
            END FOREACH

            IF v_tot_riss = 0 THEN
               ---se inserta en afiliación RISS
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
                           tmp_afi_riss_imss,
                           p_folio,
                           v_fecha_movimiento,
                           tmp_afi_alta_nrp,
                           v_rl,
                           v_f_actualiza,
                           p_usuario_cod);

               LET v_altas_riss_acep = v_altas_riss_acep + 1;
            ELSE
               IF v_riss <> tmp_afi_riss_imss THEN
                  UPDATE afi_riss
                     SET id_riss      = tmp_afi_riss_imss,
                         folio_lote   = p_folio,
                         f_proceso    = v_f_actualiza,
                         f_movimiento = v_fecha_movimiento,
                         id_riss_rl   = v_rl,
                         f_proceso    = v_f_actualiza
                   WHERE id_derechohabiente = v_id_derechohabiente
                     AND nrp                = tmp_afi_alta_nrp
                     AND id_riss            = v_riss;

                  LET v_altas_riss_acep = 1;
               ELSE
                  LET v_altas_riss_rech    = 1;
                  LET v_registro_rechazado = 19;
               END IF
            END IF
         ELSE
            LET v_altas_riss_rech    = 1;
            LET v_registro_rechazado = 1;
         END IF

         IF v_registro_rechazado = 1 THEN
            LET v_i_resultado = -21;

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
            LET afi_rch_afiliatorio_cod_rechazo         = 17; -- RISS-NRP ya existe

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
                                                                afi_rch_afiliatorio_riss_imss           ,
                                                                afi_rch_afiliatorio_cod_rechazo         ,
                                                                p_folio);
         END IF
      ELSE
         -- se cuenta un derechohabiente dado de alta
         LET v_altas_aceptadas = v_altas_aceptadas + 1;
      END IF
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
                                                          afi_rch_afiliatorio_riss_imss           ,
                                                          afi_rch_afiliatorio_cod_rechazo         ,
                                                          p_folio);

      -- se cuenta una alta rechazada
      LET v_altas_rechazadas = 1;
      LET v_i_resultado      = -1;
   END IF

   IF v_altas_riss_rech  = 21 THEN
      LET v_altas_riss_rech  = 1;
      LET v_i_resultado      = -21;
   END IF

   -- se indica cuantas cuentas se abrieron
   LET err_txt = "Altas realizadas: " || v_altas_aceptadas || " Altas RISS: " || v_altas_riss_acep || " == Altas rechazadas: " || v_altas_rechazadas|| " == Altas RISS rechazadas: " || v_altas_riss_rech;

   -- se devuelve el resultado de la ejecucion de la función
   RETURN v_i_resultado, isam_err, err_txt;

END FUNCTION;


