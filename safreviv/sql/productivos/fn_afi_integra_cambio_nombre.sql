






CREATE FUNCTION "safreviv".fn_afi_integra_cambio_nombre(p_usuario_cod CHAR(20),
                                             p_folio DECIMAL(10), 
                                             p_nombre_archivo CHAR(18),
                                             p_pid DECIMAL(9,0),
                                             p_proceso_cod SMALLINT)

   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER

   -- campos de tmp_afi_alta
   DEFINE tmp_afi_cambio_nombre_tpo_movimiento      CHAR(2)     ;
   DEFINE tmp_afi_cambio_nombre_espacios            CHAR(2)     ;
   DEFINE tmp_afi_cambio_nombre_nrp                 CHAR(11)    ;
   DEFINE tmp_afi_cambio_nombre_f_movimiento        CHAR(8)     ;
   DEFINE tmp_afi_cambio_nombre_curp_rfc            CHAR(18)    ;
   DEFINE tmp_afi_cambio_nombre_t_trabajador        DECIMAL(1,0);
   DEFINE tmp_afi_cambio_nombre_nss                 CHAR(11)    ;
   DEFINE tmp_afi_cambio_nombre_nombre              CHAR(50)    ;
   DEFINE tmp_afi_cambio_nombre_presentacion_extemp DECIMAL(1,0);
   DEFINE tmp_afi_cambio_nombre_jornada_semana      DECIMAL(1,0);
   DEFINE tmp_afi_cambio_nombre_sdi                 DECIMAL(6,0);
   DEFINE tmp_afi_cambio_nombre_sexo                DECIMAL(1,0);
   DEFINE tmp_afi_cambio_nombre_nss_correcto        CHAR(11)    ;
   DEFINE tmp_afi_cambio_nombre_nombre_correcto     CHAR(50)    ;

   -- campos de la tabla de rechazos afi_rch_afiliatorio
   DEFINE afi_rch_afiliatorio_tpo_movimiento        CHAR(2)     ;
   DEFINE afi_rch_afiliatorio_espacios              CHAR(2)     ;
   DEFINE afi_rch_afiliatorio_nrp                   CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_f_movimiento          CHAR(8)     ;
   DEFINE afi_rch_afiliatorio_curp_rfc              CHAR(18)    ;
   DEFINE afi_rch_afiliatorio_t_trabajador          DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss                   CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_nombre                CHAR(50)    ;
   DEFINE afi_rch_afiliatorio_presentacion_extemp   DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_jornada_semana        DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_sdi                   DECIMAL(6,0);
   DEFINE afi_rch_afiliatorio_sexo                  DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss_correcto          CHAR(11)    ;
   DEFINE afi_rch_afiliatorio_nombre_correcto       CHAR(50)    ;
   DEFINE afi_rch_afiliatorio_cod_rechazo           SMALLINT    ;

   -- campos de la tabla afi_his_derechohabiente
   DEFINE afi_his_derechohab_id_derechohabiente     DECIMAL(9,0);
   DEFINE afi_his_derechohab_f_modifica             DATE        ;
   DEFINE afi_his_derechohab_folio_lote_modifica    DECIMAL(9,0);
   DEFINE afi_his_derechohab_ind_modifica           CHAR(18)    ;
   DEFINE afi_his_derechohab_curp                   CHAR(18)    ;
   DEFINE afi_his_derechohab_rfc                    CHAR(13)    ;
   DEFINE afi_his_derechohab_ind_nrp                CHAR(1)     ;
   DEFINE afi_his_derechohab_f_nacimiento           DATE        ;
   DEFINE afi_his_derechohab_nombre_imss            CHAR(50)    ;
   DEFINE afi_his_derechohab_nombre_af              CHAR(40)    ;
   DEFINE afi_his_derechohab_ap_paterno_af          CHAR(40)    ;
   DEFINE afi_his_derechohab_ap_materno_af          CHAR(40)    ;

   -- campos de la tabla afi_relacion_laboral
   DEFINE afi_relacion_laboral_id_derechohabiente   DECIMAL(9,0);
   DEFINE afi_relacion_laboral_nrp                  CHAR(11)    ;
   DEFINE afi_relacion_laboral_f_alta_nrp           DATE        ;
   DEFINE afi_relacion_laboral_ind_relacion         SMALLINT    ;
   DEFINE afi_relacion_laboral_folio_lote           DECIMAL(9,0);
   DEFINE afi_relacion_laboral_f_actualiza          DATE        ;
   DEFINE afi_relacion_laboral_usuario              CHAR(20)    ;

   DEFINE v_rfc                                     VARCHAR(13);
   DEFINE v_curp                                    VARCHAR(18);
   DEFINE v_rfc_curp                                VARCHAR(18);

   -- Control de Excepciones
   DEFINE v_i_resultado                             SMALLINT;
   DEFINE sql_err                                   INTEGER;
   DEFINE isam_err                                  INTEGER;
   DEFINE err_txt                                   VARCHAR(255);
   DEFINE v_fecha_valida                            SMALLINT;
   DEFINE v_fecha_movimiento                        DATE;
   DEFINE v_codigo_rechazo                          SMALLINT;
   DEFINE tmp_riss_imss                             SMALLINT;

   -- número de cambios realizados
   DEFINE v_num_cambios_realizados                  INTEGER;

   -- Variable para marca de cuenta
   DEFINE v_i_estado_marca                          INTEGER;

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;

    LET v_codigo_rechazo = v_i_resultado;
    -- se intenta insertar
      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_cambio_nombre_tpo_movimiento,
                                                          tmp_afi_cambio_nombre_nrp                 ,
                                                          tmp_afi_cambio_nombre_f_movimiento        ,
                                                          tmp_afi_cambio_nombre_curp_rfc            ,
                                                          tmp_afi_cambio_nombre_t_trabajador        ,
                                                          tmp_afi_cambio_nombre_nss                 ,
                                                          tmp_afi_cambio_nombre_nombre              ,
                                                          tmp_afi_cambio_nombre_presentacion_extemp ,
                                                          tmp_afi_cambio_nombre_jornada_semana      ,
                                                          tmp_afi_cambio_nombre_sdi                 ,
                                                          tmp_afi_cambio_nombre_sexo                ,
                                                          tmp_afi_cambio_nombre_nss_correcto        ,
                                                          tmp_afi_cambio_nombre_nombre_correcto     ,
                                                          ""                                        ,
                                                          v_codigo_rechazo                          ,
                                                          p_folio);

      RETURN v_i_resultado, isam_err, err_txt, v_num_cambios_realizados;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de cambio de nombre finalizó correctamente.";
   LET v_codigo_rechazo   = 0;
   LET tmp_riss_imss      = NULL;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_cambio_nombre.trace";

   -- se inicia el contador de cambios realizados
   LET v_num_cambios_realizados  = 0;

   --------------------------- CAMBIO DE NOMBRE ----------------------------- 
   --trace "Se leen los cambios de nombre";
   FOREACH SELECT
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
      tmp_afi_cambio_nombre_tpo_movimiento      ,
      tmp_afi_cambio_nombre_nrp                 ,
      tmp_afi_cambio_nombre_f_movimiento        ,
      tmp_afi_cambio_nombre_curp_rfc            ,
      tmp_afi_cambio_nombre_t_trabajador        ,
      tmp_afi_cambio_nombre_nss                 ,
      tmp_afi_cambio_nombre_nombre              ,
      tmp_afi_cambio_nombre_presentacion_extemp ,
      tmp_afi_cambio_nombre_jornada_semana      ,
      tmp_afi_cambio_nombre_sdi                 ,
      tmp_afi_cambio_nombre_sexo                ,
      tmp_afi_cambio_nombre_nss_correcto        ,
      tmp_afi_cambio_nombre_nombre_correcto     
   FROM safre_tmp:tmp_afi_cambio_nombre

      -- se valida la fecha de movimiento
      EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_cambio_nombre_f_movimiento,"ddmmyyyy")
         INTO v_fecha_valida, v_fecha_movimiento;
         
      -- si la fecha de movimiento es posterior a la actual, se rechaza el registro
      IF ( v_fecha_movimiento > TODAY ) THEN
         -- se rechaza el registro por tener fecha de movimiento invalida
         LET v_codigo_rechazo = 9;
         
         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_cambio_nombre_tpo_movimiento      ,
                                                             tmp_afi_cambio_nombre_nrp                 ,
                                                             tmp_afi_cambio_nombre_f_movimiento        ,
                                                             tmp_afi_cambio_nombre_curp_rfc            ,
                                                             tmp_afi_cambio_nombre_t_trabajador        ,
                                                             tmp_afi_cambio_nombre_nss                 ,
                                                             tmp_afi_cambio_nombre_nombre              ,
                                                             tmp_afi_cambio_nombre_presentacion_extemp ,
                                                             tmp_afi_cambio_nombre_jornada_semana      ,
                                                             tmp_afi_cambio_nombre_sdi                 ,
                                                             tmp_afi_cambio_nombre_sexo                ,
                                                             tmp_afi_cambio_nombre_nss_correcto        ,
                                                             tmp_afi_cambio_nombre_nombre_correcto     ,
                                                             ""                                        ,
                                                             v_codigo_rechazo                          ,
                                                             p_folio);

         -- se continua con el siguiente registro
         CONTINUE FOREACH;
      END IF

      -- si el registro no tiene el campo nombre_correcto se rechaza
      IF ( tmp_afi_cambio_nombre_nombre_correcto IS NULL ) THEN
         -- se rechaza por tener el nombre correcto vacio
         LET v_codigo_rechazo = 4; -- nombre correcto vacio

         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_cambio_nombre_tpo_movimiento      ,
                                                             tmp_afi_cambio_nombre_nrp                 ,
                                                             tmp_afi_cambio_nombre_f_movimiento        ,
                                                             tmp_afi_cambio_nombre_curp_rfc            ,
                                                             tmp_afi_cambio_nombre_t_trabajador        ,
                                                             tmp_afi_cambio_nombre_nss                 ,
                                                             tmp_afi_cambio_nombre_nombre              ,
                                                             tmp_afi_cambio_nombre_presentacion_extemp ,
                                                             tmp_afi_cambio_nombre_jornada_semana      ,
                                                             tmp_afi_cambio_nombre_sdi                 ,
                                                             tmp_afi_cambio_nombre_sexo                ,
                                                             tmp_afi_cambio_nombre_nss_correcto        ,
                                                             tmp_afi_cambio_nombre_nombre_correcto     ,
                                                             ""                                        ,
                                                             v_codigo_rechazo                          ,
                                                             p_folio);
         CONTINUE FOREACH;
      END IF

      -- se obtiene el derechohabiente
      SELECT id_derechohabiente
      INTO   afi_his_derechohab_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_afi_cambio_nombre_nss;

      -- si no esta el derechohabiente se rechaza
      IF ( afi_his_derechohab_id_derechohabiente IS NULL ) THEN
         LET v_codigo_rechazo = 1; -- NSS no existe

         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_cambio_nombre_tpo_movimiento      ,
                                                             tmp_afi_cambio_nombre_nrp                 ,
                                                             tmp_afi_cambio_nombre_f_movimiento        ,
                                                             tmp_afi_cambio_nombre_curp_rfc            ,
                                                             tmp_afi_cambio_nombre_t_trabajador        ,
                                                             tmp_afi_cambio_nombre_nss                 ,
                                                             tmp_afi_cambio_nombre_nombre              ,
                                                             tmp_afi_cambio_nombre_presentacion_extemp ,
                                                             tmp_afi_cambio_nombre_jornada_semana      ,
                                                             tmp_afi_cambio_nombre_sdi                 ,
                                                             tmp_afi_cambio_nombre_sexo                ,
                                                             tmp_afi_cambio_nombre_nss_correcto        ,
                                                             tmp_afi_cambio_nombre_nombre_correcto     ,
                                                             ""                                        ,
                                                             v_codigo_rechazo                          ,
                                                             p_folio);
         CONTINUE FOREACH;
      END IF

      -- se guarda el historico del campo que se esta cambiando
      SELECT nombre_imss  ,
             curp         ,
             rfc          ,
             ind_nrp      ,
             f_nacimiento ,
             nombre_af    ,
             ap_paterno_af,
             ap_materno_af
      INTO   afi_his_derechohab_nombre_imss   ,
             afi_his_derechohab_curp          ,
             afi_his_derechohab_rfc           ,
             afi_his_derechohab_ind_nrp       ,
             afi_his_derechohab_f_nacimiento  ,
             afi_his_derechohab_nombre_af     ,
             afi_his_derechohab_ap_paterno_af ,
             afi_his_derechohab_ap_materno_af
      FROM   afi_derechohabiente
      WHERE  nss = tmp_afi_cambio_nombre_nss;

      -- se asingnan los datos al registro de histori	co
      LET afi_his_derechohab_f_modifica           = TODAY; -- fecha de cambio
      LET afi_his_derechohab_folio_lote_modifica  = p_folio;      LET afi_his_derechohab_ind_modifica         = 5; -- cat_afi_ind_modifica CAMBIO NOMBRE IMSS

      -- se inserta el historico
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
         ap_materno_af        )
      VALUES (
         afi_his_derechohab_id_derechohabiente  ,
         afi_his_derechohab_f_modifica          ,
         afi_his_derechohab_folio_lote_modifica ,
         afi_his_derechohab_ind_modifica        ,
         afi_his_derechohab_curp                ,
         afi_his_derechohab_rfc                 ,
         afi_his_derechohab_ind_nrp             ,
         afi_his_derechohab_f_nacimiento        ,
         afi_his_derechohab_nombre_imss         ,
         afi_his_derechohab_nombre_af           ,
         afi_his_derechohab_ap_paterno_af       ,
         afi_his_derechohab_ap_materno_af       );

      -- se separa el nombre completo en sus componentes de pila y apellidos
      CALL fn_separa_nombre(tmp_afi_cambio_nombre_nombre_correcto)
      RETURNING afi_his_derechohab_ap_paterno_af,
                afi_his_derechohab_ap_materno_af,
                afi_his_derechohab_nombre_af;

      -- se cambia el nombre IMSS y componentes de nombre AFORE con el nombre recibido
      UPDATE afi_derechohabiente
         SET nombre_imss = tmp_afi_cambio_nombre_nombre_correcto
       WHERE nss         = tmp_afi_cambio_nombre_nss;

      LET v_num_cambios_realizados = v_num_cambios_realizados + 1;

   END FOREACH;

   --trace "Finaliza el proceso";

   UPDATE STATISTICS FOR TABLE afi_derechohabiente;

   -- se indica cuantas cuentas se abrieron
   LET err_txt = "Cambios realizados: " || v_num_cambios_realizados;

   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_i_resultado, isam_err, err_txt, v_num_cambios_realizados;

END FUNCTION
;


