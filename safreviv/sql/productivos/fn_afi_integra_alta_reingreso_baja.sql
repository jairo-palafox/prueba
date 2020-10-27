






CREATE FUNCTION "safreviv".fn_afi_integra_alta_reingreso_baja(p_usuario_cod CHAR(20),
                                                   p_folio DECIMAL(10), 
                                                   p_nombre_archivo CHAR(18),
                                                   p_pid DECIMAL(9,0),
                                                   p_proceso_cod SMALLINT)

   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, VARCHAR(11)

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
   DEFINE tmp_afi_riss_imss                       SMALLINT    ;
   DEFINE tmp_afi_riss_inf                        SMALLINT    ;

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
   DEFINE v_fecha                                 DATE;

   -- datos de identificación del derechohabiente
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

   -- numero de altas aceptadas y rechzadas
   DEFINE v_num_altas_aceptadas                   INTEGER;
   DEFINE v_num_altas_rechazadas                  INTEGER;
   DEFINE v_num_reingresos_aceptados              INTEGER;
   DEFINE v_num_reingresos_rechazados             INTEGER;
   DEFINE v_num_bajas_aceptadas                   INTEGER;
   DEFINE v_num_bajas_rechazadas                  INTEGER;
   DEFINE v_num_altas_en_reingreso                INTEGER;
   DEFINE v_num_riss_aceptadas                    INTEGER;
   DEFINE v_num_riss_rechazadas                   INTEGER;

   DEFINE v_registro_rechazado                    SMALLINT; -- booleana para verificar un registro rechazado
   DEFINE v_solo_NRP                              SMALLINT; -- para indicar si solo se debe dar de alta el NRP
   DEFINE v_hubo_alta_en_reingreso                SMALLINT; -- booleana para saber si se hizo alta en reingreso

   -- Variable para marca de cuenta
   DEFINE v_i_estado_marca                        INTEGER;

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, isam_err, err_txt, v_num_altas_aceptadas, v_num_altas_rechazadas, v_num_reingresos_aceptados , v_num_reingresos_rechazados, v_num_altas_en_reingreso, v_num_bajas_aceptadas, v_num_bajas_rechazadas, v_num_riss_aceptadas, v_num_riss_rechazadas, tmp_afi_nss;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_alta_reingreso_baja.trace";
   --TRACE ON;

   -- Variables que almacenan información para su validación
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de Alta/Reingreso/Baja finalizó correctamente";
   LET tmp_afi_nss        = NULL; -- no hay nss con error

   -- se inician los contadores de altas aceptadas y rechazadas
   LET v_num_altas_aceptadas       = 0;
   LET v_num_altas_rechazadas      = 0;
   LET v_num_reingresos_aceptados  = 0;
   LET v_num_reingresos_rechazados = 0;
   LET v_num_bajas_aceptadas       = 0;
   LET v_num_bajas_rechazadas      = 0;
   LET v_num_altas_en_reingreso    = 0;
   LET v_num_riss_aceptadas        = 0;
   LET v_num_riss_rechazadas       = 0;

   -- se crea la tabla temporal que alojará los registros de alta/reingreso y baja para ser procesados
   DROP TABLE IF EXISTS tmp_afi_alta_reingreso_baja;
   CREATE TABLE tmp_afi_alta_reingreso_baja (
      tpo_movimiento      CHAR(2)     ,
      espacios            CHAR(2)     ,
      nrp                 CHAR(11)    ,
      f_movimiento        CHAR(8)     ,
      curp_rfc            CHAR(18)    ,
      t_trabajador        DECIMAL(1,0),
      nss                 CHAR(11)    ,
      nombre              CHAR(50)    ,
      presentacion_extemp DECIMAL(1,0),
      jornada_semana      DECIMAL(1,0),
      sdi                 DECIMAL(6,0),
      sexo                DECIMAL(1,0),
      nss_correcto        CHAR(11)    ,
      nombre_correcto     CHAR(50)    ,
      riss_imss           SMALLINT    ,
      riss_inf            SMALLINT
   ) IN tmp_2_dbs;

   -- se cambia el tipo de movimiento de las bajas a "09" 
   -- y afiliación RISS "21" para que se puedan ordenar
   -- y se procesen los registros con el orden solicitado

   UPDATE safre_tmp:tmp_afi_baja
      SET tpo_movimiento = "09"
    WHERE tpo_movimiento = "02";

   UPDATE safre_tmp:tmp_afi_riss
      SET tpo_movimiento = "03",
          riss_imss      = 0
    WHERE riss_imss IS NULL;

   UPDATE safre_tmp:tmp_afi_riss
      SET tpo_movimiento = "03"
    WHERE tpo_movimiento = "21"
      AND riss_imss <> 3;

   UPDATE safre_tmp:tmp_afi_riss
      SET tpo_movimiento = "10"
    WHERE tpo_movimiento = "21"
      AND riss_imss = 3;

   -- se leen las altas/reingresos/bajas y se almacenan en la tabla temporal
   INSERT INTO tmp_afi_alta_reingreso_baja SELECT *,"","" FROM safre_tmp:tmp_afi_alta;
   INSERT INTO tmp_afi_alta_reingreso_baja SELECT *,"","" FROM safre_tmp:tmp_afi_reingreso;
   INSERT INTO tmp_afi_alta_reingreso_baja SELECT *,"","" FROM safre_tmp:tmp_afi_baja;
   INSERT INTO tmp_afi_alta_reingreso_baja SELECT * FROM safre_tmp:tmp_afi_riss;

   --------------------------- ALTA/REINGRESO/BAJA ----------------------------- 
   -- se leen los movimientos ordenados por fecha y en orden: altas, reingreso, bajas
   FOREACH
   SELECT
      tpo_movimiento      ,
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
      DATE(f_movimiento[3,4] || f_movimiento[1,2] || f_movimiento[5,8]) AS la_fecha,
      riss_imss           ,
      DECODE(riss_inf,1,4,2,5,3,6)
   INTO 
      tmp_afi_tpo_movimiento      ,
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
      v_fecha                     ,
      tmp_afi_riss_imss           ,
      tmp_afi_riss_inf
   FROM tmp_afi_alta_reingreso_baja
   ORDER BY la_fecha, tpo_movimiento

      -- se asume que no hay error
      LET v_resultado = 0;
      LET v_isam      = 0;
      LET v_mensaje   = NULL;
      LET v_hubo_alta_en_reingreso = 0; -- se asume que no hubo alta

      -- se procesa cada registro
      -- ========================================================
      -- ALTA
      IF ( tmp_afi_tpo_movimiento = "01" ) OR
         ( tmp_afi_tpo_movimiento = "03" ) THEN
         EXECUTE FUNCTION fn_afi_integra_alta_registro(p_folio                     ,
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
                                                       tmp_afi_riss_imss           ,
                                                       tmp_afi_riss_inf            ,
                                                       p_usuario_cod)
            INTO v_resultado, v_isam, v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 1 OR v_resultado = 21 ) THEN
            IF v_resultado = 1 THEN
               -- se cuenta una alta aceptada
               LET v_num_altas_aceptadas = v_num_altas_aceptadas + 1;
            ELSE
               LET v_num_riss_aceptadas  = v_num_riss_aceptadas + 1;
            END IF
         ELSE
            IF v_resultado = -1 THEN
               -- se cuenta una alta rechazada
               LET v_num_altas_rechazadas = v_num_altas_rechazadas + 1;
            ELSE
               LET v_num_riss_rechazadas  = v_num_riss_rechazadas + 1;
            END IF
         END IF
      END IF

      -- ===========================================================
      -- REINGRESO
      IF ( tmp_afi_tpo_movimiento = "08" ) THEN
         EXECUTE FUNCTION fn_afi_integra_reingreso_registro(p_folio                     ,
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

         -- si no hubo error
         IF ( v_resultado = 0 ) THEN
            -- se cuenta un reingreso aceptado
            LET v_num_reingresos_aceptados  = v_num_reingresos_aceptados + 1;

            -- si se dio de alta cuenta en el reingreso
            IF ( v_hubo_alta_en_reingreso = 1 ) THEN
               -- se cuentan un alta en reingreso
               LET v_num_altas_en_reingreso = v_num_altas_en_reingreso + 1;
            END IF

         ELSE
            -- se cuenta un reingreso rechazado
            LET v_num_reingresos_rechazados = v_num_reingresos_rechazados + 1;
         END IF
      END IF

      -- ===========================================================
      -- BAJA (se cambio a 09 para que se pudiera ordenar)
      IF ( tmp_afi_tpo_movimiento = "09" ) OR
         ( tmp_afi_tpo_movimiento = "10" ) THEN
         EXECUTE FUNCTION fn_afi_integra_baja_registro(p_folio                     ,
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
                                                       tmp_afi_riss_imss           ,
                                                       tmp_afi_riss_inf            ,
                                                       p_usuario_cod)
            INTO v_resultado, v_isam, v_mensaje;

         -- si no hubo error
         IF ( v_resultado = 1 OR v_resultado = 21 ) THEN
            IF v_resultado = 1 THEN
               -- se cuenta una alta aceptada
               LET v_num_bajas_aceptadas  = v_num_bajas_aceptadas + 1;
            ELSE
               LET v_num_riss_aceptadas  = v_num_riss_aceptadas + 1;
            END IF
         ELSE
            IF v_resultado = -1 THEN
               -- se cuenta una alta rechazada
               LET v_num_bajas_rechazadas = v_num_bajas_rechazadas + 1;
            ELSE
               LET v_num_riss_rechazadas  = v_num_riss_rechazadas + 1;
            END IF
         END IF
      END IF

   END FOREACH;

   -- se elimina la tabla temporal
   --DROP TABLE tmp_afi_alta_reingreso_baja;

   --trace "Finaliza el proceso";

   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   UPDATE STATISTICS FOR TABLE afi_relacion_laboral;
   UPDATE STATISTICS FOR TABLE afi_riss;

    -- se indica cuantas cuentas se abrieron
    LET err_txt = "Proceso de integración de alta/reingreso/baja/riss finalizado";

    -- se devuelve el resultado de la ejecucion del SP
    RETURN v_i_resultado, isam_err, err_txt, v_num_altas_aceptadas, v_num_altas_rechazadas, v_num_reingresos_aceptados , v_num_reingresos_rechazados, v_num_altas_en_reingreso, v_num_bajas_aceptadas, v_num_bajas_rechazadas, v_num_riss_aceptadas, v_num_riss_rechazadas, tmp_afi_nss;

END FUNCTION;


