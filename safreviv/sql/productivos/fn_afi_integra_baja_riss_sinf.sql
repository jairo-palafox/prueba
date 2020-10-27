






CREATE FUNCTION "safreviv".fn_afi_integra_baja_riss_sinf(p_folio                          DECIMAL(10) ,
                                              tmp_afi_baja_tpo_movimiento      CHAR(2)     ,
                                              tmp_afi_baja_nrp                 CHAR(11)    ,
                                              tmp_afi_baja_f_movimiento        CHAR(8)     ,
                                              tmp_afi_baja_curp_rfc            CHAR(18)    ,
                                              tmp_afi_baja_t_trabajador        DECIMAL(1,0),
                                              tmp_afi_baja_nss                 CHAR(11)    ,
                                              tmp_afi_baja_nombre              CHAR(50)    ,
                                              tmp_afi_baja_presentacion_extemp DECIMAL(1,0),
                                              tmp_afi_baja_jornada_semana      DECIMAL(1,0),
                                              tmp_afi_baja_sdi                 DECIMAL(6,0),
                                              tmp_afi_baja_sexo                DECIMAL(1,0),
                                              tmp_afi_baja_nss_correcto        CHAR(11)    ,
                                              tmp_afi_baja_nombre_correcto     CHAR(50)    ,
                                              p_usuario_cod                    CHAR(20))
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
   DEFINE afi_rch_afiliatorio_cod_rechazo         smallint    ;

   -- campos de la tabla afi_his_derechohabiente
   DEFINE afi_his_derechohab_id_derechohabiente   DECIMAL(9,0);
   DEFINE afi_his_derechohab_f_modifica           DATE        ;
   DEFINE afi_his_derechohab_folio_lote_modifica  DECIMAL(9,0);
   DEFINE afi_his_derechohab_ind_modifica         CHAR(18)    ;
   DEFINE afi_his_derechohab_curp                 CHAR(18)    ;
   DEFINE afi_his_derechohab_rfc                  CHAR(13)    ;
   DEFINE afi_his_derechohab_ind_nrp              CHAR(1)     ;
   DEFINE afi_his_derechohab_f_nacimiento         DATE        ;
   DEFINE afi_his_derechohab_nombre_imss          CHAR(50)    ;
   DEFINE afi_his_derechohab_nombre_af            CHAR(40)    ;
   DEFINE afi_his_derechohab_ap_paterno_af        CHAR(40)    ;
   DEFINE afi_his_derechohab_ap_materno_af        CHAR(40)    ;

   -- campos de la tabla afi_relacion_laboral
   DEFINE afi_relacion_laboral_id_derechohabiente DECIMAL(9,0);
   DEFINE afi_relacion_laboral_nrp                CHAR(11)    ;
   DEFINE afi_relacion_laboral_f_alta_nrp         DATE        ;
   DEFINE afi_relacion_laboral_ind_relacion       smallint    ;
   DEFINE afi_relacion_laboral_folio_lote         DECIMAL(9,0);
   DEFINE afi_relacion_laboral_f_actualiza        DATE        ;
   DEFINE afi_relacion_laboral_usuario            CHAR(20)    ;

   DEFINE v_rfc                                   VARCHAR(13);
   DEFINE v_curp                                  VARCHAR(18);
   DEFINE v_rfc_curp                              VARCHAR(18);
   DEFINE v_conteo                                INTEGER;

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);

   -- Variables de validaciones
   DEFINE v_d_id_referencia                       DECIMAL(9,0);
   DEFINE v_id_derechohabiente                    DECIMAL(9,0);
   DEFINE v_fecha_valida                          SMALLINT;
   DEFINE v_fecha_movimiento                      DATE;
   DEFINE v_codigo_rechazo                        SMALLINT;
   DEFINE tmp_riss_imss                           SMALLINT;

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;

    LET v_codigo_rechazo = v_i_resultado;
    -- se intenta insertar
      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento,
                                                          tmp_afi_baja_nrp                 ,
                                                          tmp_afi_baja_f_movimiento        ,
                                                          tmp_afi_baja_curp_rfc            ,
                                                          tmp_afi_baja_t_trabajador        ,
                                                          tmp_afi_baja_nss                 ,
                                                          tmp_afi_baja_nombre              ,
                                                          tmp_afi_baja_presentacion_extemp ,
                                                          tmp_afi_baja_jornada_semana      ,
                                                          tmp_afi_baja_sdi                 ,
                                                          tmp_afi_baja_sexo                ,
                                                          tmp_afi_baja_nss_correcto        ,
                                                          tmp_afi_baja_nombre_correcto     ,
                                                          ""                               ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);

      RETURN v_i_resultado, isam_err, err_txt;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de movimientos afiliatorios de baja finalizó correctamente";
   LET v_codigo_rechazo   = 0;
   LET tmp_riss_imss      = NULL;

   --SET DEBUG FILE TO "/safreviv_int/BD/debug_fn_afi_integra_baja_riss_sinf" || tmp_afi_baja_nss || ".trace";

   -- el tipo de movimiento de baja es 02, se cambio a 09 para poder ordenarlos

   -- se valida la fecha de movimiento
   EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_baja_f_movimiento,"ddmmyyyy")
      INTO v_fecha_valida, v_fecha_movimiento;

   -- si la fecha de movimiento es invalida
   IF ( v_fecha_valida <> 1 ) THEN
      -- se rechaza el registro por tener fecha de movimiento invalida
      LET v_codigo_rechazo = 9;

      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento,
                                                          tmp_afi_baja_nrp                 ,
                                                          tmp_afi_baja_f_movimiento        ,
                                                          tmp_afi_baja_curp_rfc            ,
                                                          tmp_afi_baja_t_trabajador        ,
                                                          tmp_afi_baja_nss                 ,
                                                          tmp_afi_baja_nombre              ,
                                                          tmp_afi_baja_presentacion_extemp ,
                                                          tmp_afi_baja_jornada_semana      ,
                                                          tmp_afi_baja_sdi                 ,
                                                          tmp_afi_baja_sexo                ,
                                                          tmp_afi_baja_nss_correcto        ,
                                                          tmp_afi_baja_nombre_correcto     ,
                                                          ""                               ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);

      -- se continua con el siguiente registro
      RETURN v_codigo_rechazo, isam_err, err_txt;
   END IF

   -- si la fecha de movimiento es valida y posterior a la actual, se rechaza el registro
   IF ( v_fecha_movimiento > TODAY ) THEN
      -- se rechaza el registro por tener fecha de movimiento invalida
      LET v_codigo_rechazo = 9;

      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento,
                                                          tmp_afi_baja_nrp                 ,
                                                          tmp_afi_baja_f_movimiento        ,
                                                          tmp_afi_baja_curp_rfc            ,
                                                          tmp_afi_baja_t_trabajador        ,
                                                          tmp_afi_baja_nss                 ,
                                                          tmp_afi_baja_nombre              ,
                                                          tmp_afi_baja_presentacion_extemp ,
                                                          tmp_afi_baja_jornada_semana      ,
                                                          tmp_afi_baja_sdi                 ,
                                                          tmp_afi_baja_sexo                ,
                                                          tmp_afi_baja_nss_correcto        ,
                                                          tmp_afi_baja_nombre_correcto     ,
                                                          ""                               ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);

      -- se continua con el siguiente registro
      RETURN v_codigo_rechazo, isam_err, err_txt;
   END IF

   -- se obtiene el id_derechohabiente
   SELECT id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = tmp_afi_baja_nss;

   -- si no se encuentra el NSS
   IF ( v_id_derechohabiente IS NULL ) THEN

      -- NSS no existe
      LET v_codigo_rechazo = 1;

      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento,
                                                          tmp_afi_baja_nrp                 ,
                                                          tmp_afi_baja_f_movimiento        ,
                                                          tmp_afi_baja_curp_rfc            ,
                                                          tmp_afi_baja_t_trabajador        ,
                                                          tmp_afi_baja_nss                 ,
                                                          tmp_afi_baja_nombre              ,
                                                          tmp_afi_baja_presentacion_extemp ,
                                                          tmp_afi_baja_jornada_semana      ,
                                                          tmp_afi_baja_sdi                 ,
                                                          tmp_afi_baja_sexo                ,
                                                          tmp_afi_baja_nss_correcto        ,
                                                          tmp_afi_baja_nombre_correcto     ,
                                                          ""                               ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);
      RETURN v_codigo_rechazo, isam_err, err_txt;
   END IF

   -- se da de baja la relacion laboral
   UPDATE afi_riss
      SET id_riss            = 5,
          id_riss_rl         = 2,
          f_movimiento       = v_fecha_movimiento
    WHERE id_derechohabiente = v_id_derechohabiente
      AND nrp                = tmp_afi_baja_nrp
      AND id_riss            = 4;

   -- se verifica si se elimino un registro
   IF ( DBINFO('sqlca.sqlerrd2') = 0 ) THEN
      LET v_codigo_rechazo = 20;

      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_baja_tpo_movimiento      ,
                                                          tmp_afi_baja_nrp                 ,
                                                          tmp_afi_baja_f_movimiento        ,
                                                          tmp_afi_baja_curp_rfc            ,
                                                          tmp_afi_baja_t_trabajador        ,
                                                          tmp_afi_baja_nss                 ,
                                                          tmp_afi_baja_nombre              ,
                                                          tmp_afi_baja_presentacion_extemp ,
                                                          tmp_afi_baja_jornada_semana      ,
                                                          tmp_afi_baja_sdi                 ,
                                                          tmp_afi_baja_sexo                ,
                                                          tmp_afi_baja_nss_correcto        ,
                                                          tmp_afi_baja_nombre_correcto     ,
                                                          ""                               ,
                                                          v_codigo_rechazo                 ,
                                                          p_folio);
      RETURN v_codigo_rechazo, isam_err, err_txt;
   END IF

   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_i_resultado, isam_err, err_txt;

END FUNCTION
;


