






CREATE FUNCTION "safreviv".fn_afi_integra_alta_reingreso_baja_sinf(p_usuario_cod CHAR(20),
                                                        p_folio DECIMAL(10), 
                                                        p_nombre_archivo CHAR(18),
                                                        p_pid DECIMAL(9,0),
                                                        p_proceso_cod SMALLINT)

   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER,
             INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, VARCHAR(11)

   -- campos de tmp_afi_alta
   DEFINE tmp_afi_tpo_movimiento                  CHAR(2)     ;
   DEFINE tmp_afi_espacios                        CHAR(2)     ;
   DEFINE tmp_afi_nrp                             CHAR(11)    ;
   DEFINE tmp_afi_f_movimiento                    CHAR(8)     ;
   DEFINE tmp_afi_curp_rfc                        CHAR(18)    ;
   DEFINE tmp_afi_t_trabajador                    DECIMAL(1,0);
   DEFINE tmp_afi_nss                             CHAR(11)    ;
   DEFINE tmp_afi_nombre                          CHAR(50)    ;
   DEFINE tmp_afi_presentacion_extemp             DECIMAL(1,0);
   DEFINE tmp_afi_jornada_semana                  DECIMAL(1,0);
   DEFINE tmp_afi_sdi                             DECIMAL(6,0);
   DEFINE tmp_afi_sexo                            DECIMAL(1,0);
   DEFINE tmp_afi_nss_correcto                    CHAR(11)    ;
   DEFINE tmp_afi_nombre_correcto                 CHAR(50)    ;

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
   DEFINE afi_relacion_laboral_f_alta_nrp         date        ;
   DEFINE afi_relacion_laboral_ind_relacion       SMALLINT    ;
   DEFINE afi_relacion_laboral_folio_lote         DECIMAL(9,0);
   DEFINE afi_relacion_laboral_f_actualiza        date        ;
   DEFINE afi_relacion_laboral_usuario            CHAR(20)    ;

   -- para cambiar el formato de la fecha
   DEFINE v_fecha_texto                           VARCHAR(10);
   DEFINE v_fecha                                 DATE;

   DEFINE v_rfc                                   VARCHAR(13);
   DEFINE v_curp                                  VARCHAR(18);
   DEFINE v_rfc_curp                              VARCHAR(18);

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);
   DEFINE v_fecha_valida                          SMALLINT;
   DEFINE v_fecha_movimiento                      DATE;
   DEFINE v_resultado                             SMALLINT;
   DEFINE v_isam                                  SMALLINT;
   DEFINE v_mensaje                               VARCHAR(255);

   -- Variables de validaciones
   DEFINE v_d_id_referencia                       DECIMAL(9,0);
   DEFINE v_id_derechohabiente                    DECIMAL(9,0);
   DEFINE v_id_derechohabiente_unificado          DECIMAL(9,0);
   DEFINE v_diagnostico_unificador                SMALLINT;
   DEFINE v_diagnostico_unificadas                SMALLINT;
   DEFINE v_diagnostico_rechazo                   SMALLINT;

   -- número de altas aceptadas y rechzadas
   DEFINE v_altas_aceptadas                       INTEGER;
   DEFINE v_altas_rechazadas                      INTEGER;
   DEFINE v_reingresos_aceptados                  INTEGER;
   DEFINE v_reingresos_rechazados                 INTEGER;
   DEFINE v_bajas_aceptadas                       INTEGER;
   DEFINE v_bajas_rechazadas                      INTEGER;
   DEFINE v_altas_en_reingreso                    INTEGER;
   DEFINE v_altas_riss_aceptadas                  INTEGER;
   DEFINE v_altas_riss_rechazadas                 INTEGER;
   DEFINE v_bajas_riss_aceptadas                  INTEGER;
   DEFINE v_bajas_riss_rechazadas                 INTEGER;
   DEFINE v_rfc_aceptadas                         INTEGER;
   DEFINE v_rfc_rechazadas                        INTEGER;

   DEFINE v_registro_rechazado                    SMALLINT; -- booleana para verificar un registro rechazado
   DEFINE v_solo_NRP                              SMALLINT; -- para indicar si solo se debe dar de alta el NRP
   DEFINE v_hubo_alta_en_reingreso                SMALLINT; -- booleana para saber si se hizo alta en reingreso

   -- Variable para marca de cuenta
   DEFINE v_i_estado_marca                        INTEGER;

   -- constantes para codigos de error
   DEFINE v_error_nss_vacio                       SMALLINT;
   DEFINE v_error_nss_ya_existe                   SMALLINT;
   DEFINE v_error_fecha_movimiento_vacia          SMALLINT;
   DEFINE v_error_nrp_vacio                       SMALLINT;
   DEFINE v_error_nombre_vacio                    SMALLINT;
   DEFINE v_error_fec_mov_posterior_actual        SMALLINT; -- la fecha de movimiento es posterior a la actual

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, isam_err, err_txt, v_altas_aceptadas, v_altas_rechazadas, v_reingresos_aceptados , v_reingresos_rechazados, v_altas_en_reingreso, v_bajas_aceptadas, v_bajas_rechazadas,
             v_altas_riss_aceptadas, v_altas_riss_rechazadas, v_bajas_riss_aceptadas, v_bajas_riss_rechazadas, v_rfc_aceptadas, v_rfc_rechazadas, tmp_afi_nss;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de Alta/Reingreso/Baja finalizó correctamente";
   LET tmp_afi_nss        = NULL; -- no hay nss con error

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_alta_reingreso_baja_sinf.trace";
   --TRACE ON;

   -- se definen las constantes de codigo de error
   LET v_error_nss_vacio                = 8;
   LET v_error_nss_ya_existe            = 2;
   LET v_error_fecha_movimiento_vacia   = 5;
   LET v_error_nrp_vacio                = 6;
   LET v_error_nombre_vacio             = 7;
   LET v_error_fec_mov_posterior_actual = 9;

   -- se inician los contadores de altas aceptadas y rechazadas
   LET v_altas_aceptadas            = 0;
   LET v_altas_rechazadas           = 0;
   LET v_reingresos_aceptados       = 0;
   LET v_reingresos_rechazados      = 0;
   LET v_bajas_aceptadas            = 0;
   LET v_bajas_rechazadas           = 0;
   LET v_altas_en_reingreso         = 0;
   LET v_altas_riss_aceptadas       = 0;
   LET v_altas_riss_rechazadas      = 0;
   LET v_bajas_riss_aceptadas       = 0;
   LET v_bajas_riss_rechazadas      = 0;
   LET v_rfc_aceptadas              = 0;
   LET v_rfc_rechazadas             = 0;

   -- se leen las altas/reingresos/bajas y se almacenan en la tabla temporal
   INSERT INTO safre_tmp:tmp_afi_alta_reingreso_baja_sinf
   SELECT *
   FROM safre_tmp:tmp_afi_sinf_alta;

   INSERT INTO safre_tmp:tmp_afi_alta_reingreso_baja_sinf
   SELECT *
   FROM safre_tmp:tmp_afi_sinf_reingreso;

   INSERT INTO safre_tmp:tmp_afi_alta_reingreso_baja_sinf
   SELECT *
   FROM safre_tmp:tmp_afi_sinf_baja;

   INSERT INTO safre_tmp:tmp_afi_alta_reingreso_baja_sinf
   SELECT *
   FROM safre_tmp:tmp_afi_alta_riss;

   INSERT INTO safre_tmp:tmp_afi_alta_reingreso_baja_sinf
   SELECT *
   FROM safre_tmp:tmp_afi_baja_riss;

   INSERT INTO safre_tmp:tmp_afi_alta_reingreso_baja_sinf
   SELECT *
   FROM safre_tmp:tmp_afi_rfc_riss;

   -- se cambia el tipo de movimiento de las bajas a "09" para que se puedan ordenar
   -- y queden al último
   UPDATE safre_tmp:tmp_afi_alta_reingreso_baja_sinf
      SET tpo_movimiento = "09"
    WHERE tpo_movimiento = "02";

   ------------------------------ ALTA/REINGRESO/BAJA/RISS --------------------------------
   -- se leen los movimientos ordenados por fecha y en orden: altas, reingreso, bajas, riss
   FOREACH
      SELECT tpo_movimiento      ,
             espacios            ,
             nrp                 ,
             f_movimiento        ,
             curp_rfc            ,
             t_trabajador        ,
             nss                 ,
             nombre              ,
             presentacion_extemp ,
             jornada_semana      ,
             sdi                 ,
             sexo                ,
             nss_correcto        ,
             nombre_correcto     ,
             DATE(f_movimiento[3,4] || f_movimiento[1,2] || f_movimiento[5,8]) AS la_fecha
        INTO tmp_afi_tpo_movimiento      ,
             tmp_afi_espacios            ,
             tmp_afi_nrp                 ,
             tmp_afi_f_movimiento        ,
             tmp_afi_curp_rfc            ,
             tmp_afi_t_trabajador        ,
             tmp_afi_nss                 ,
             tmp_afi_nombre              ,
             tmp_afi_presentacion_extemp ,
             tmp_afi_jornada_semana      ,
             tmp_afi_sdi                 ,
             tmp_afi_sexo                ,
             tmp_afi_nss_correcto        ,
             tmp_afi_nombre_correcto     ,
             v_fecha
        FROM safre_tmp:tmp_afi_alta_reingreso_baja_sinf
      ORDER BY la_fecha, tpo_movimiento

      -- se asume que no hay error
      LET v_resultado = 0;
      LET v_isam      = 0;
      LET v_mensaje   = NULL;
      LET v_hubo_alta_en_reingreso = 0; -- se asume que no hubo alta

      -- se procesa cada registro
      -- ========================================================
      -- ALTA
      IF ( tmp_afi_tpo_movimiento = "01" ) THEN
         EXECUTE FUNCTION fn_afi_integra_alta_registro_sinf(p_folio                     ,
                                                            tmp_afi_tpo_movimiento      ,
                                                            tmp_afi_nrp                 ,
                                                            tmp_afi_f_movimiento        ,
                                                            tmp_afi_curp_rfc            ,
                                                            tmp_afi_t_trabajador        ,
                                                            tmp_afi_nss                 ,
                                                            tmp_afi_nombre              ,
                                                            tmp_afi_presentacion_extemp ,
                                                            tmp_afi_jornada_semana      ,
                                                            tmp_afi_sdi                 ,
                                                            tmp_afi_sexo                ,
                                                            tmp_afi_nss_correcto        ,
                                                            tmp_afi_nombre_correcto     ,
                                                            p_usuario_cod)
         INTO v_resultado, v_isam, v_mensaje;

         --TRACE "resultado: " || v_resultado;
         --TRACE "mensaje: " || v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 0 ) THEN
            -- se cuenta una alta aceptada
            LET v_altas_aceptadas  = v_altas_aceptadas + 1;
         ELSE
            -- se cuenta una alta rechazada
            LET v_altas_rechazadas = v_altas_rechazadas + 1;
         END IF
      END IF

      -- ===========================================================
      -- REINGRESO
      IF ( tmp_afi_tpo_movimiento = "08" ) THEN
         EXECUTE FUNCTION fn_afi_integra_reingreso_registro_sinf(p_folio                     ,
                                                                 tmp_afi_tpo_movimiento      ,
                                                                 tmp_afi_nrp                 ,
                                                                 tmp_afi_f_movimiento        ,
                                                                 tmp_afi_curp_rfc            ,
                                                                 tmp_afi_t_trabajador        ,
                                                                 tmp_afi_nss                 ,
                                                                 tmp_afi_nombre              ,
                                                                 tmp_afi_presentacion_extemp ,
                                                                 tmp_afi_jornada_semana      ,
                                                                 tmp_afi_sdi                 ,
                                                                 tmp_afi_sexo                ,
                                                                 tmp_afi_nss_correcto        ,
                                                                 tmp_afi_nombre_correcto     ,
                                                                 p_usuario_cod)
         INTO v_resultado, v_isam, v_mensaje, v_hubo_alta_en_reingreso;

        --TRACE "resultado: " || v_resultado;
        --TRACE "mensaje: " || v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 0 ) THEN
            -- se cuenta un reingreso aceptado
            LET v_reingresos_aceptados  = v_reingresos_aceptados + 1;

            -- si se dio de alta cuenta en el reingreso
            IF ( v_hubo_alta_en_reingreso = 1 ) THEN
               -- se cuentan un alta en reingreso
               LET v_altas_en_reingreso = v_altas_en_reingreso + 1;
            END IF
         ELSE
            -- se cuenta un reingreso rechazado
            LET v_reingresos_rechazados = v_reingresos_rechazados + 1;
         END IF
      END IF

      -- ===========================================================
      -- BAJA (se cambio a 09 para que se pudiera ordenar)
      IF ( tmp_afi_tpo_movimiento = "09" ) THEN
         EXECUTE FUNCTION fn_afi_integra_baja_registro_sinf(p_folio                     ,
                                                            tmp_afi_tpo_movimiento      ,
                                                            tmp_afi_nrp                 ,
                                                            tmp_afi_f_movimiento        ,
                                                            tmp_afi_curp_rfc            ,
                                                            tmp_afi_t_trabajador        ,
                                                            tmp_afi_nss                 ,
                                                            tmp_afi_nombre              ,
                                                            tmp_afi_presentacion_extemp ,
                                                            tmp_afi_jornada_semana      ,
                                                            tmp_afi_sdi                 ,
                                                            tmp_afi_sexo                ,
                                                            tmp_afi_nss_correcto        ,
                                                            tmp_afi_nombre_correcto     ,
                                                            p_usuario_cod )
         INTO v_resultado, v_isam, v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 0 ) THEN
            -- se cuenta una baja aceptada
            LET v_bajas_aceptadas  = v_bajas_aceptadas + 1;
         ELSE
            -- se cuenta una baja rechazada
            LET v_bajas_rechazadas = v_bajas_rechazadas + 1;
         END IF
      END IF

      -- ========================================================
      -- ALTA RISS
      IF ( tmp_afi_tpo_movimiento = "22" ) THEN
         EXECUTE FUNCTION fn_afi_integra_alta_riss_sinf(p_folio                     ,
                                                        tmp_afi_tpo_movimiento      ,
                                                        tmp_afi_nrp                 ,
                                                        tmp_afi_f_movimiento        ,
                                                        tmp_afi_curp_rfc            ,
                                                        tmp_afi_t_trabajador        ,
                                                        tmp_afi_nss                 ,
                                                        tmp_afi_nombre              ,
                                                        tmp_afi_presentacion_extemp ,
                                                        tmp_afi_jornada_semana      ,
                                                        tmp_afi_sdi                 ,
                                                        tmp_afi_sexo                ,
                                                        tmp_afi_nss_correcto        ,
                                                        tmp_afi_nombre_correcto     ,
                                                        p_usuario_cod)
         INTO v_resultado, v_isam, v_mensaje;

         --TRACE "resultado: " || v_resultado;
         --TRACE "mensaje: " || v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 0 ) THEN
            -- se cuenta una alta aceptada
            LET v_altas_riss_aceptadas = v_altas_riss_aceptadas + 1;
         ELSE
            -- se cuenta una alta rechazada
            LET v_altas_riss_rechazadas = v_altas_riss_rechazadas + 1;
         END IF
      END IF

      -- ========================================================
      -- ALTA RFC RISS
      IF ( tmp_afi_tpo_movimiento = "24" ) THEN
         EXECUTE FUNCTION fn_afi_integra_rfc_riss_sinf(p_folio                     ,
                                                       tmp_afi_tpo_movimiento      ,
                                                       tmp_afi_nrp                 ,
                                                       tmp_afi_f_movimiento        ,
                                                       tmp_afi_curp_rfc            ,
                                                       tmp_afi_t_trabajador        ,
                                                       tmp_afi_nss                 ,
                                                       tmp_afi_nombre              ,
                                                       tmp_afi_presentacion_extemp ,
                                                       tmp_afi_jornada_semana      ,
                                                       tmp_afi_sdi                 ,
                                                       tmp_afi_sexo                ,
                                                       tmp_afi_nss_correcto        ,
                                                       tmp_afi_nombre_correcto     ,
                                                       p_usuario_cod)
         INTO v_resultado, v_isam, v_mensaje;

         --TRACE "resultado: " || v_resultado;
         --TRACE "mensaje: " || v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 0 ) THEN
            -- se cuenta una alta aceptada
            LET v_rfc_aceptadas = v_rfc_aceptadas + 1;
         ELSE
            -- se cuenta una alta rechazada
            LET v_rfc_rechazadas = v_rfc_rechazadas + 1;
         END IF
      END IF

      -- ===========================================================
      -- BAJA RISS
      IF ( tmp_afi_tpo_movimiento = "23" ) THEN
         EXECUTE FUNCTION fn_afi_integra_baja_riss_sinf(p_folio                     ,
                                                        tmp_afi_tpo_movimiento      ,
                                                        tmp_afi_nrp                 ,
                                                        tmp_afi_f_movimiento        ,
                                                        tmp_afi_curp_rfc            ,
                                                        tmp_afi_t_trabajador        ,
                                                        tmp_afi_nss                 ,
                                                        tmp_afi_nombre              ,
                                                        tmp_afi_presentacion_extemp ,
                                                        tmp_afi_jornada_semana      ,
                                                        tmp_afi_sdi                 ,
                                                        tmp_afi_sexo                ,
                                                        tmp_afi_nss_correcto        ,
                                                        tmp_afi_nombre_correcto     ,
                                                        p_usuario_cod )
         INTO v_resultado, v_isam, v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 0 ) THEN
            -- se cuenta una baja aceptada
            LET v_bajas_riss_aceptadas = v_bajas_riss_aceptadas + 1;
         ELSE
            -- se cuenta una baja rechazada
            LET v_bajas_riss_rechazadas = v_bajas_riss_rechazadas + 1;
         END IF
      END IF

   END FOREACH;

   -- se elimina la tabla temporal
   --DROP TABLE tmp_afi_alta_reingreso_baja;

   --trace "Finaliza el proceso";

   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   UPDATE STATISTICS FOR TABLE afi_relacion_laboral;
   UPDATE STATISTICS FOR TABLE afi_rch_afiliatorio;
   UPDATE STATISTICS FOR TABLE afi_riss;

    -- se indica cuantas cuentas se abrieron
    LET err_txt = "Proceso de integracion de alta/reingreso/baja finalizado";

    -- se devuelve el resultado de la ejecucion del SP
    RETURN v_i_resultado, isam_err, err_txt, v_altas_aceptadas, v_altas_rechazadas, v_reingresos_aceptados , v_reingresos_rechazados, v_altas_en_reingreso, v_bajas_aceptadas, v_bajas_rechazadas,
           v_altas_riss_aceptadas, v_altas_riss_rechazadas, v_bajas_riss_aceptadas, v_bajas_riss_rechazadas, v_rfc_aceptadas, v_rfc_rechazadas, tmp_afi_nss;

END FUNCTION
;


