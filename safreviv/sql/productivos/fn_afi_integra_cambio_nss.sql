






CREATE FUNCTION "safreviv".fn_afi_integra_cambio_nss(p_usuario_cod CHAR(20),
                                          p_folio DECIMAL(10),
                                          p_nombre_archivo CHAR(18),
                                          p_pid DECIMAL(9,0),
                                          p_proceso_cod SMALLINT)

   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER, INTEGER, INTEGER, INTEGER

   -- Variables utilizadas para el detalle
   DEFINE v_det_tpo_movimiento      CHAR(2);
   DEFINE v_det_nrp                 CHAR(11);
   DEFINE v_det_f_movimiento        CHAR(8);
   DEFINE v_det_curp_rfc            CHAR(18);
   DEFINE v_det_t_trabajador        DECIMAL(1,0);
   DEFINE v_det_nss                 CHAR(11);
   DEFINE v_det_nombre              CHAR(50);
   DEFINE v_det_presentacion_extemp decimal(1,0);
   DEFINE v_det_jornada_semana      decimal(1,0);
   DEFINE v_det_sdi                 decimal(6,0);
   DEFINE v_det_sexo                decimal(1,0);
   DEFINE v_det_nss_correcto        CHAR(11);
   DEFINE v_det_nombre_correcto     CHAR(50);

   DEFINE v_nss_buscado             CHAR(11);

   DEFINE v_si_dia                  SMALLINT;
   DEFINE v_si_mes                  SMALLINT;
   DEFINE v_si_ano                  SMALLINT;
   DEFINE v_det_date_movimiento     DATE;

   -- para validar si se recibio CURP o RFC
   DEFINE v_rfc                  VARCHAR(13);
   DEFINE v_curp                 VARCHAR(18);

   -- campos de la tabla afi_relacion_laboral
   DEFINE afi_relacion_laboral_id_derechohabiente decimal(9,0);
   DEFINE afi_relacion_laboral_nrp                char(11)    ;
   DEFINE afi_relacion_laboral_f_alta_nrp         date        ;
   DEFINE afi_relacion_laboral_ind_relacion       smallint    ;
   DEFINE afi_relacion_laboral_folio_lote         decimal(9,0);
   DEFINE afi_relacion_laboral_f_actualiza        date        ;
   DEFINE afi_relacion_laboral_usuario            char(20)    ;

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);
   DEFINE v_codigo_rechazo                        INTEGER;
   DEFINE v_fecha_valida                          SMALLINT;
   DEFINE v_fecha_movimiento                      DATE;

   -- Variables de validaciones
   DEFINE v_d_id_referencia                       DECIMAL(9,0);
   DEFINE v_id_derechohabiente_unificador         DECIMAL(9,0);
   DEFINE v_id_derechohabiente_unificado          DECIMAL(9,0);
   DEFINE v_diagnostico_unificador                SMALLINT;
   DEFINE v_diagnostico_unificadas                SMALLINT;
   DEFINE v_diagnostico_rechazo                   SMALLINT;
   DEFINE v_estado_familia_unificador             SMALLINT;
   DEFINE v_estado_familia_unificado              SMALLINT;

   -- Variable para marca de cuenta
   DEFINE v_i_estado_marca                        INTEGER;
   DEFINE v_registros_rechazados                  INTEGER;
   DEFINE v_secuencia_unificador                  INTEGER;
   DEFINE v_secuencia_unificado                   INTEGER;

   -- contadores de NSS unificados y unificadores
   DEFINE v_cambio_nss_procesados                 INTEGER;
   DEFINE v_num_nss_unificadores                  INTEGER;
   DEFINE v_num_nss_unificados                    INTEGER;

   -- en caso de encontrar un error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;

   LET v_codigo_rechazo = v_i_resultado;
   -- se intenta insertar
      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(v_det_tpo_movimiento      ,
                                                          v_det_nrp                 ,
                                                          v_det_f_movimiento        ,
                                                          v_det_curp_rfc            ,
                                                          v_det_t_trabajador        ,
                                                          v_det_nss                 ,
                                                          v_det_nombre              ,
                                                          v_det_presentacion_extemp ,
                                                          v_det_jornada_semana      ,
                                                          v_det_sdi                 ,
                                                          v_det_sexo                ,
                                                          v_det_nss_correcto        ,
                                                          v_det_nombre_correcto     ,
                                                          ""                        ,
                                                          v_codigo_rechazo          ,
                                                          p_folio
                                                          );

      RETURN sql_err, isam_err, err_txt, v_cambio_nss_procesados, v_num_nss_unificadores, v_num_nss_unificados, v_registros_rechazados;
   END EXCEPTION

   -- se inician las variables de diagnostico
   LET v_diagnostico_unificador    = 0;
   LET v_diagnostico_unificadas    = 0;
   LET v_diagnostico_rechazo       = 0;
   LET v_estado_familia_unificador = 0;
   LET v_estado_familia_unificado  = 0;
   LET v_secuencia_unificador      = 0;
   LET v_secuencia_unificado       = 0;

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado           = 0;
   LET v_d_id_referencia       = 0;
   LET sql_err                 = 0;
   LET isam_err                = 0;
   LET err_txt                 = "El proceso de integración de movimientos afilitarios de cambio de NSS finalizó correctamente";
   LET v_num_nss_unificadores  = 0;
   LET v_num_nss_unificados    = 0;
   LET v_registros_rechazados  = 0;
   LET v_cambio_nss_procesados = 0;
   LET v_codigo_rechazo        = 0;

   ---SET DEBUG FILE TO "/safreviv_int/BD/fn_afi_integra_cambio_nss.trace";
   --SET DEBUG FILE TO "/safreviv/afi/sql/integra_cambio_nss.trace";
   --TRACE ON;
   ---------------------------CTAS UNIFICADOR-----------------------------
   SELECT MAX(id_referencia)
     INTO v_d_id_referencia
     FROM uni_det_rechazos;

     -- Verifica que el id_referencia no venga nullo
     -- en caso de ser contrario, se asigna el valor que trae
   IF ( v_d_id_referencia IS NULL ) THEN
      LET v_d_id_referencia = 0;
   END IF

   -- [Error]
    --trace "Al recuperar datos detalle tmp_afi_cambio_nss unificador";

   LET err_txt = "Al recuperar datos detalle tmp_afi_cambio_nss unificador";

   FOREACH
   SELECT
      tpo_movimiento      ,
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
      nombre_correcto
   INTO
      v_det_tpo_movimiento      ,
      v_det_nrp                 ,
      v_det_f_movimiento        ,
      v_det_curp_rfc            ,
      v_det_t_trabajador        ,
      v_det_nss                 ,
      v_det_nombre              ,
      v_det_presentacion_extemp ,
      v_det_jornada_semana      ,
      v_det_sdi                 ,
      v_det_sexo                ,
      v_det_nss_correcto        ,
      v_det_nombre_correcto
   FROM safre_tmp:tmp_afi_cambio_nss

      -- se cuenta un reistro procesado
      LET v_cambio_nss_procesados = v_cambio_nss_procesados + 1;

      -- Acumula la referencia
      LET v_d_id_referencia           = v_d_id_referencia + 1;
      LET v_diagnostico_unificador    = 1; -- Aceptada
      LET v_diagnostico_unificadas    = 1; -- Aceptada
      LET v_diagnostico_rechazo       = 1; -- Aceptada
      LET v_estado_familia_unificador = 1; -- Aceptada
      LET v_estado_familia_unificado  = 1; -- Aceptada
      LET v_codigo_rechazo            = 0; -- sin rechazo

      -- se valida la fecha de movimiento
      EXECUTE FUNCTION fn_valida_fecha_por_formato(v_det_f_movimiento,"ddmmyyyy")
         INTO v_fecha_valida, v_fecha_movimiento;

      -- si la fecha de movimiento es posterior a la actual, se rechaza el registro
      IF ( v_fecha_movimiento > TODAY ) THEN
         -- se rechaza el registro por tener fecha de movimiento invalida
         LET v_codigo_rechazo = 9;

         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(v_det_tpo_movimiento      ,
                                                             v_det_nrp                 ,
                                                             v_det_f_movimiento        ,
                                                             v_det_curp_rfc            ,
                                                             v_det_t_trabajador        ,
                                                             v_det_nss                 ,
                                                             v_det_nombre              ,
                                                             v_det_presentacion_extemp ,
                                                             v_det_jornada_semana      ,
                                                             v_det_sdi                 ,
                                                             v_det_sexo                ,
                                                             v_det_nss_correcto        ,
                                                             v_det_nombre_correcto     ,
                                                             ""                        ,
                                                             v_codigo_rechazo          ,
                                                             p_folio);

         -- se contabiliza el rechazo
         LET v_registros_rechazados = v_registros_rechazados + 1;

         -- se continua con el siguiente registro
         CONTINUE FOREACH;
      END IF

       --trace "Asigna la fecha de movimiento";
      LET err_txt = "Asigna la fecha de movimiento";
      LET v_det_date_movimiento = v_fecha_movimiento;

       --trace "Valida tipo de movimiento para el registro inicial 05";
      LET err_txt = "Valida tipo de movimiento para el registro inicial 05";

      -- Recupera el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_unificador
      FROM   afi_derechohabiente
      WHERE  nss = v_det_nss_correcto;

      -- Valida el id_derechohabiente
       --trace "En id_derechohabiente unificador, no existe: "||v_id_derechohabiente_unificador;
      LET err_txt = "En id_derechohabiente unificador, no existe: "||v_id_derechohabiente_unificador;

      -- si el unificador no existe, se da de alta
      IF ( v_id_derechohabiente_unificador IS NULL ) THEN
         -- se verifica si se recibio RFC o CURP
         IF ( LENGTH(v_det_curp_rfc) < 14 ) THEN
            -- es un RFC
            LET v_rfc  = v_det_curp_rfc;
            LET v_curp = NULL;
         ELSE -- se tiene una curp
            LET v_rfc  = NULL;
            LET v_curp = v_det_curp_rfc;
         END IF

         -- se verifica si se recibio NRP para crear su relacion laboral
         IF ( v_det_nrp IS NOT NULL ) THEN
            -- con relacion laboral
            LET afi_relacion_laboral_ind_relacion = 1;
         ELSE
            -- sin relacion laboral
            LET afi_relacion_laboral_ind_relacion = 0;
         END IF

         -- se invoca el alta del derechohabiente unificador
         EXECUTE FUNCTION fn_apertura_cuenta_afi(v_det_nss_correcto                 -- NSS
                                                 ,v_curp                            -- CURP
                                                 ,v_rfc                             -- RFC
                                                 ,afi_relacion_laboral_ind_relacion -- Ind relacion laboral
                                                 ,v_det_nombre_correcto             -- Nombre
                                                 ,"I"                               -- tipo trabajador (IMSS)
                                                 ,0                                 -- id_credito (sin credito)
                                                 ,p_folio                           -- folio
                                                 ,"A"      )                        -- origen de afiliacion
                          INTO v_id_derechohabiente_unificador;

         -- si no se pudo dar de alta el NSS unificador, se rechaza el registro
         IF ( v_id_derechohabiente_unificador < 0 ) THEN

            LET v_codigo_rechazo = 3; -- no se pudo abrir la cuenta

            EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(v_det_tpo_movimiento     ,
                                                               v_det_nrp                 ,
                                                               v_det_f_movimiento        ,
                                                               v_det_curp_rfc            ,
                                                               v_det_t_trabajador        ,
                                                               v_det_nss                 ,
                                                               v_det_nombre              ,
                                                               v_det_presentacion_extemp ,
                                                               v_det_jornada_semana      ,
                                                               v_det_sdi                 ,
                                                               v_det_sexo                ,
                                                               v_det_nss_correcto        ,
                                                               v_det_nombre_correcto     ,
                                                               ""                        ,
                                                               v_codigo_rechazo          ,
                                                               p_folio);

            -- se contabiliza el rechazo
            LET v_registros_rechazados = v_registros_rechazados + 1;

            -- se continua con el siguiente registro
            CONTINUE FOREACH;
         END IF
      END IF

       --trace "Al insertar detalle en uni_pre_unificador: "||v_det_nss_correcto;
      LET err_txt = "Al insertar detalle en uni_pre_unificador " || v_det_nss_correcto;

      --IF v_diagnostico_unificador = 1 AND v_estado_familia_unificador = 1 THEN

      SELECT seq_uni_pre_unificador.NEXTVAL
      INTO v_secuencia_unificador
      FROM systables
      WHERE tabid = 1;

      INSERT INTO uni_pre_unificador (
         id_pre_unificador  ,
         folio_lote         ,
         id_derechohabiente ,
         nrp                ,
         tipo_trabajador    ,
         nss_correcto       ,
         nombre_correcto    ,
         estado             ,
         diagnostico        ,
         f_proceso          ,
         f_movimiento)
       VALUES (
         v_secuencia_unificador          , --id_unificador
         p_folio                         ,
         v_id_derechohabiente_unificador ,
         v_det_nrp                       ,
         v_det_t_trabajador              ,
         v_det_nss_correcto              ,
         v_det_nombre_correcto           ,
         v_diagnostico_unificador        ,
         v_diagnostico_rechazo           ,
         TODAY                           ,
         v_det_date_movimiento
         );

      --------------------------- CUENTAS UNIFICADAS -----------------------------
       ----trace "Recupera el id derechohabiente unificado";
      LET err_txt = "Recupera el id derechohabiente unificado";

      ----Recupera el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_unificado
      FROM   afi_derechohabiente
      WHERE  nss = v_det_nss;

      -- Valida el id_derechohabiente
       --trace "En id_derechohabiente, no existe en unificado";
      LET err_txt = "En id_derechohabiente, no existe en unificado";

      IF ( v_id_derechohabiente_unificado IS NULL ) THEN

         -- se verifica si se recibio RFC o CURP
         IF ( LENGTH(v_det_curp_rfc) < 14 ) THEN
            -- es un RFC
            LET v_rfc  = v_det_curp_rfc;
            LET v_curp = NULL;
         ELSE -- se tiene una curp
            LET v_rfc  = NULL;
            LET v_curp = v_det_curp_rfc;
         END IF

         -- se verifica si se recibio NRP para crear su relacion laboral
         IF ( v_det_nrp IS NOT NULL ) THEN
            -- con relacion laboral
            LET afi_relacion_laboral_ind_relacion = 1;
         ELSE
            -- sin relacion laboral
            LET afi_relacion_laboral_ind_relacion = 0;
         END IF

         -- se invoca el alta del derechohabiente unificador
         EXECUTE FUNCTION fn_apertura_cuenta_afi(v_det_nss                          -- NSS
                                                 ,v_curp                            -- CURP
                                                 ,v_rfc                             -- RFC
                                                 ,afi_relacion_laboral_ind_relacion -- Ind relacion laboral
                                                 ,v_det_nombre                      -- Nombre
                                                 ,"I"                               -- tipo trabajador (IMSS)
                                                 ,0                                 -- id_credito (sin credito)
                                                 ,p_folio                           -- folio
                                                 ,"A"      )                        -- origen de afiliacion
                          INTO v_id_derechohabiente_unificado;
      END IF

       --trace "Al insertar detalle en uni_pre_unificado";
      LET err_txt = "Al insertar detalle en uni_pre_unificado";

      -- se verifica si el nss_unificado ya esta en la tabla de unificados para el NSS unificador dado

      -- si ya existe, no se registra
      IF NOT EXISTS (
                      SELECT nss
                        FROM uni_pre_unificado
                       WHERE nss = v_det_nss
                         AND id_pre_unificador = v_secuencia_unificador ) THEN

         SELECT seq_uni_pre_unificado.NEXTVAL
         INTO   v_secuencia_unificado
         FROM   systables
         WHERE  tabid = 1;

         INSERT INTO uni_pre_unificado (
            id_pre_unificado   ,
            id_pre_unificador  ,
            id_derechohabiente ,
            id_unificado       ,
            nss                ,
            nombre             ,
            estado             ,
            diagnostico)
         VALUES (
            v_secuencia_unificado          , --id_unificado
            v_secuencia_unificador         , --id_unificador
            v_id_derechohabiente_unificado ,
            NULL                           ,
            v_det_nss                      ,
            v_det_nombre                   ,
            v_diagnostico_unificadas       ,
            v_diagnostico_rechazo);

         -- se cuenta un NSS unificado
         LET v_num_nss_unificados = v_num_nss_unificados + 1;

          --trace "Valida para marcar la cuenta unificado";
         LET err_txt = "Valida para marcar la cuenta unificado";

         IF ( v_diagnostico_unificadas = 1 AND v_estado_familia_unificado = 1 ) THEN
          LET v_i_estado_marca = 0;
             --trace "Al marcar unificación imss unificado";
            EXECUTE FUNCTION fn_marca_cuenta(
                             v_id_derechohabiente_unificado,
                             512,-- marca de unificado IMSS
                             v_secuencia_unificado,
                             p_folio,
                             0, -- estado marca
                             0, -- codigo de rechazo
                             0, -- marca de la causa
                             NULL, -- fecha de la causa
                             p_usuario_cod,
                             p_proceso_cod)
                INTO v_i_estado_marca;
         END IF
      ELSE
         -- se rechaza el registro porque ya existe la relacion NSS unificado-NSS unificador
         LET v_codigo_rechazo = 13;

         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(v_det_tpo_movimiento      ,
                                                             v_det_nrp                 ,
                                                             v_det_f_movimiento        ,
                                                             v_det_curp_rfc            ,
                                                             v_det_t_trabajador        ,
                                                             v_det_nss                 ,
                                                             v_det_nombre              ,
                                                             v_det_presentacion_extemp ,
                                                             v_det_jornada_semana      ,
                                                             v_det_sdi                 ,
                                                             v_det_sexo                ,
                                                             v_det_nss_correcto        ,
                                                             v_det_nombre_correcto     ,
                                                             ""                        ,
                                                             v_codigo_rechazo          ,
                                                             p_folio);

         -- se contabiliza el rechazo
         LET v_registros_rechazados = v_registros_rechazados + 1;
    END IF

       --trace "Valida para marcar la cuenta unificador";
      LET err_txt = "Valida para marcar la cuenta unificador";

      IF ( v_diagnostico_unificador = 1 AND v_estado_familia_unificador = 1 ) THEN
         LET v_i_estado_marca = 0;
          --trace "Al marcar unificación imss unificador";
         EXECUTE FUNCTION fn_marca_cuenta(
                          v_id_derechohabiente_unificador,
                          511,-- marca de unificador IMSS
                          v_secuencia_unificador,
                          p_folio,
                          0, -- estado marca
                          0, -- codigo de rechazo
                          0, -- marca de la causa
                          NULL, -- fecha de la causa
                          p_usuario_cod,
                          p_proceso_cod)
            INTO v_i_estado_marca;
    END IF
   END FOREACH;

   -- se actualizan las estadisticas de las tablas
   UPDATE STATISTICS FOR TABLE uni_pre_unificador;
   UPDATE STATISTICS FOR TABLE uni_pre_unificado;

   -- se cuentan los NSS unificadores. Los unificados se cuentan conforme se insertan
   SELECT COUNT(*)
     INTO v_num_nss_unificadores
     FROM uni_pre_unificador
    WHERE folio_lote = p_folio;

   -- se indica el numero de NSS unificadores y unificados recibidos
   LET err_txt = "Núm. NSS unificadores: " || v_num_nss_unificadores || " == Núm. NSS unificados: " || v_num_nss_unificados;

   -- se devuelve el resultado de la ejecucion del proceso
   RETURN sql_err, isam_err, err_txt, v_cambio_nss_procesados, v_num_nss_unificadores, v_num_nss_unificados, v_registros_rechazados;

END FUNCTION
;


