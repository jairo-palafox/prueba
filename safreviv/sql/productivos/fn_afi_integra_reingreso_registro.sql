






CREATE FUNCTION "safreviv".fn_afi_integra_reingreso_registro(p_folio                               DECIMAL(10) ,
                                                  tmp_afi_reingreso_tpo_movimiento      CHAR(2)     ,
                                                  tmp_afi_reingreso_nrp                 CHAR(11)    ,
                                                  tmp_afi_reingreso_f_movimiento        CHAR(8)     ,
                                                  tmp_afi_reingreso_curp_rfc            CHAR(18)    ,
                                                  tmp_afi_reingreso_t_trabajador        DECIMAL(1,0),
                                                  tmp_afi_reingreso_nss                 CHAR(11)    ,
                                                  tmp_afi_reingreso_nombre              CHAR(50)    ,
                                                  tmp_afi_reingreso_presentacion_extemp DECIMAL(1,0),
                                                  tmp_afi_reingreso_jornada_semana      DECIMAL(1,0),
                                                  tmp_afi_reingreso_sdi                 DECIMAL(6,0),
                                                  tmp_afi_reingreso_sexo                DECIMAL(1,0),
                                                  tmp_afi_reingreso_nss_correcto        CHAR(11)    ,
                                                  tmp_afi_reingreso_nombre_correcto     CHAR(50)    ,
                                                  p_usuario_cod                         CHAR(20))
   RETURNING INTEGER, INTEGER, VARCHAR(255), SMALLINT

   -- campos de la tabla de rechazo de movimientos afiliatorios
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
   DEFINE v_fecha_texto                           VARCHAR(10);
   DEFINE v_fecha_movimiento                      DATE;
   DEFINE v_hubo_alta_en_reingreso                SMALLINT; 

   DEFINE v_rfc                                   VARCHAR(13);
   DEFINE v_curp                                  VARCHAR(18);
   DEFINE v_rfc_curp                              VARCHAR(18);
   DEFINE v_conteo                                INTEGER;
   DEFINE v_codigo_rechazo                        SMALLINT;

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);

   -- Variables de validaciones
   DEFINE v_d_id_referencia                       DECIMAL(9,0);
   DEFINE v_id_derechohabiente                    DECIMAL(9,0);
   DEFINE v_fecha_valida                          SMALLINT;
   DEFINE tmp_riss_imss                           SMALLINT;

   -- número de reingresos realizados
   DEFINE v_num_reingresos_realizados             INTEGER;

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;

      LET v_codigo_rechazo = v_i_resultado;

      -- se intenta insertar
      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_reingreso_tpo_movimiento,
                                                          tmp_afi_reingreso_nrp                 ,
                                                          tmp_afi_reingreso_f_movimiento        ,
                                                          tmp_afi_reingreso_curp_rfc            ,
                                                          tmp_afi_reingreso_t_trabajador        ,
                                                          tmp_afi_reingreso_nss                 ,
                                                          tmp_afi_reingreso_nombre              ,
                                                          tmp_afi_reingreso_presentacion_extemp ,
                                                          tmp_afi_reingreso_jornada_semana      ,
                                                          tmp_afi_reingreso_sdi                 ,
                                                          tmp_afi_reingreso_sexo                ,
                                                          tmp_afi_reingreso_nss_correcto        ,
                                                          tmp_afi_reingreso_nombre_correcto     ,
                                                          tmp_riss_imss                         ,
                                                          v_codigo_rechazo                      ,
                                                          p_folio);

      RETURN v_i_resultado, isam_err, err_txt, v_hubo_alta_en_reingreso;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de reingreso finalizó correctamente.";
   LET v_fecha_valida     = 0;
   LET v_codigo_rechazo   = 0;
   LET v_hubo_alta_en_reingreso = 0; -- no se trata de un alta en reingreso
   LET tmp_riss_imss      = NULL;

   --SET DEBUG FILE TO "/safreviv_int/sql/fn_afi_integra_reingreso.trace";
   --TRACE ON;

   --------------------------- REINGRESO ----------------------------- 
   --trace "Se leen los reingresos";
   SET PDQPRIORITY HIGH;

   -- se inicia el contador de reingresos realizados
   LET v_num_reingresos_realizados  = 0;

   -- se valida la fecha de movimiento
   EXECUTE FUNCTION fn_valida_fecha_por_formato(tmp_afi_reingreso_f_movimiento,"ddmmyyyy")
      INTO v_fecha_valida, v_fecha_movimiento;

   -- si la fecha es valida
   IF ( v_fecha_valida = 1 ) THEN
      -- la fecha de movimiento no puede ser mayor a la fecha actual
      IF ( v_fecha_movimiento > TODAY ) THEN
         -- se rechaza el registro por tener fecha de movimiento invalida
         LET v_codigo_rechazo = 9;

         EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_reingreso_tpo_movimiento      ,
                                                             tmp_afi_reingreso_nrp                 ,
                                                             tmp_afi_reingreso_f_movimiento        ,
                                                             tmp_afi_reingreso_curp_rfc            ,
                                                             tmp_afi_reingreso_t_trabajador        ,
                                                             tmp_afi_reingreso_nss                 ,
                                                             tmp_afi_reingreso_nombre              ,
                                                             tmp_afi_reingreso_presentacion_extemp ,
                                                             tmp_afi_reingreso_jornada_semana      ,
                                                             tmp_afi_reingreso_sdi                 ,
                                                             tmp_afi_reingreso_sexo                ,
                                                             tmp_afi_reingreso_nss_correcto        ,
                                                             tmp_afi_reingreso_nombre_correcto     ,
                                                             tmp_riss_imss                         ,
                                                             v_codigo_rechazo                      ,
                                                             p_folio);

         -- se continua con el siguiente registro
         RETURN v_codigo_rechazo, isam_err, err_txt, v_hubo_alta_en_reingreso;
      END IF
   ELSE
      LET v_codigo_rechazo = 10; -- fecha de reingreso invalida

      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_reingreso_tpo_movimiento,
                                                          tmp_afi_reingreso_nrp                 ,
                                                          tmp_afi_reingreso_f_movimiento        ,
                                                          tmp_afi_reingreso_curp_rfc            ,
                                                          tmp_afi_reingreso_t_trabajador        ,
                                                          tmp_afi_reingreso_nss                 ,
                                                          tmp_afi_reingreso_nombre              ,
                                                          tmp_afi_reingreso_presentacion_extemp ,
                                                          tmp_afi_reingreso_jornada_semana      ,
                                                          tmp_afi_reingreso_sdi                 ,
                                                          tmp_afi_reingreso_sexo                ,
                                                          tmp_afi_reingreso_nss_correcto        ,
                                                          tmp_afi_reingreso_nombre_correcto     ,
                                                          tmp_riss_imss                         ,
                                                          v_codigo_rechazo                      ,
                                                          p_folio);

      -- se continua con el siguiente registro
      RETURN v_codigo_rechazo, isam_err, err_txt, v_hubo_alta_en_reingreso;
   END IF

   --TRACE "NSS para reingreso: " || tmp_afi_reingreso_nss;

   -- se busca el derechohabiente
   SELECT id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = tmp_afi_reingreso_nss;

   -- si no se encuentra el NSS
   IF ( v_id_derechohabiente IS NULL ) THEN
      -- se abre la cuenta
      -- se asigna el curp/rfc
      LET v_rfc_curp = tmp_afi_reingreso_curp_rfc;

      -- se verifica si se recibio RFC o CURP
      IF ( LENGTH(v_rfc_curp) < 14 ) THEN
         -- es un RFC
         LET v_rfc  = v_rfc_curp;
         LET v_curp = NULL;
      ELSE -- se tiene una curp
         LET v_rfc  = NULL;
         LET v_curp = v_rfc_curp;
      END IF

      -- se verifica si se recibio NRP para crear su relacion laboral
      IF ( tmp_afi_reingreso_nrp IS NOT NULL ) THEN
         -- con relacion laboral
         LET afi_relacion_laboral_ind_relacion = 1;
      ELSE
         -- sin relacion laboral
         LET afi_relacion_laboral_ind_relacion = 0;
      END IF

      -- se invoca el alta del derechohabiente
      EXECUTE FUNCTION fn_apertura_cuenta_afi(tmp_afi_reingreso_nss             ,-- NSS
                                              v_curp                            ,-- CURP
                                              v_rfc                             ,-- RFC
                                              afi_relacion_laboral_ind_relacion ,-- Ind relacion laboral 
                                              tmp_afi_reingreso_nombre          ,-- Nombre
                                              "I"                               ,-- tipo trabajador (IMSS)
                                              0                                 ,-- id_credito (sin credito)
                                              p_folio                           ,-- folio         
                                              "A"                               )-- origen de afiliacion
                    INTO v_id_derechohabiente;

      -- si el id_derechohabiente es negativo, entonces hubo un error al abrir la cuenta
      IF ( v_id_derechohabiente < 0 ) THEN
         -- no se pudo abrir la cuenta, se registra en rechazos
         LET afi_rch_afiliatorio_tpo_movimiento      = tmp_afi_reingreso_tpo_movimiento;
         LET afi_rch_afiliatorio_nrp                 = tmp_afi_reingreso_nrp;
         LET afi_rch_afiliatorio_curp_rfc            = tmp_afi_reingreso_curp_rfc;
         LET afi_rch_afiliatorio_f_movimiento        = tmp_afi_reingreso_f_movimiento;
         LET afi_rch_afiliatorio_t_trabajador        = tmp_afi_reingreso_t_trabajador;
         LET afi_rch_afiliatorio_nss                 = tmp_afi_reingreso_nss;
         LET afi_rch_afiliatorio_nombre              = tmp_afi_reingreso_nombre;
         LET afi_rch_afiliatorio_presentacion_extemp = tmp_afi_reingreso_presentacion_extemp;
         LET afi_rch_afiliatorio_jornada_semana      = tmp_afi_reingreso_jornada_semana;
         LET afi_rch_afiliatorio_sdi                 = tmp_afi_reingreso_sdi;
         LET afi_rch_afiliatorio_sexo                = tmp_afi_reingreso_sexo;
         LET afi_rch_afiliatorio_nss_correcto        = tmp_afi_reingreso_nss_correcto;
         LET afi_rch_afiliatorio_nombre_correcto     = tmp_afi_reingreso_nombre_correcto;
         LET afi_rch_afiliatorio_cod_rechazo         = 3;

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
                                                             tmp_riss_imss                           ,
                                                             afi_rch_afiliatorio_cod_rechazo         ,
                                                             p_folio                                 );
         -- se continua con el siguiente registro
         RETURN v_codigo_rechazo, isam_err, err_txt, v_hubo_alta_en_reingreso;
      ELSE
         -- se cuenta un alta en reingreso
         LET v_hubo_alta_en_reingreso = 1;
      END IF
   END IF

   -- se verifica que el derechohabiente no tenga ya una relación laboral con el NRP dado
   SELECT nrp
     INTO afi_relacion_laboral_nrp
     FROM afi_relacion_laboral
    WHERE id_derechohabiente = v_id_derechohabiente
      AND nrp = tmp_afi_reingreso_nrp;

   --TRACE "Buscando si tiene relacion con el NRP: " || tmp_afi_reingreso_nrp;

   -- si no existe, se realiza el reingreso
   IF ( afi_relacion_laboral_nrp IS NULL ) THEN
      --TRACE "No hay relacion, se genera";
      SELECT MAX(ind_relacion)
      INTO   afi_relacion_laboral_ind_relacion
      FROM   afi_relacion_laboral
      WHERE  id_derechohabiente = v_id_derechohabiente;

      -- si no existe, entonces es el primero
      IF ( afi_relacion_laboral_ind_relacion IS NULL ) THEN
         LET afi_relacion_laboral_ind_relacion = 1;
      ELSE
         -- se incrementa en uno
         LET afi_relacion_laboral_ind_relacion = afi_relacion_laboral_ind_relacion + 1;
      END IF

      --TRACE "indice de relacion: " || afi_relacion_laboral_ind_relacion;

      -- se asignan los datos al registro de relacion laboral
      LET afi_relacion_laboral_id_derechohabiente = v_id_derechohabiente;
      LET afi_relacion_laboral_nrp                = tmp_afi_reingreso_nrp;
      LET afi_relacion_laboral_f_alta_nrp         = v_fecha_movimiento;
      LET afi_relacion_laboral_ind_relacion       = afi_relacion_laboral_ind_relacion; -- es consecutivo según el número que le toque
      LET afi_relacion_laboral_folio_lote         = p_folio;
      LET afi_relacion_laboral_f_actualiza        = TODAY;
      LET afi_relacion_laboral_usuario            = p_usuario_cod;

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

      -- se guarda el histórico del campo que se está cambiando
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
      WHERE  id_derechohabiente = v_id_derechohabiente;

      -- se asingnan los datos al registro de histórico
      LET afi_his_derechohab_id_derechohabiente   = v_id_derechohabiente;
      LET afi_his_derechohab_f_modifica           = TODAY;
      LET afi_his_derechohab_folio_lote_modifica  = p_folio;
      LET afi_his_derechohab_ind_modifica         = 8; -- CAMBIO POR REINGRESO

      -- se inserta el histórico
       INSERT INTO afi_his_derechohabiente
                  (id_derechohabiente   ,
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
                  ap_materno_af)
          VALUES (afi_his_derechohab_id_derechohabiente  ,
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
                  afi_his_derechohab_ap_materno_af);
  
      -- se actualiza el estatus del derechohabiente indicando que tiene relación laboral
      UPDATE afi_derechohabiente
      SET    ind_nrp = "1"
      WHERE  id_derechohabiente = v_id_derechohabiente;

      -- se actualizan los posibles registros de riss que no tenían la RL
      UPDATE afi_riss
         SET id_riss_rl = 1
       WHERE id_derechohabiente = afi_relacion_laboral_id_derechohabiente
         AND nrp = afi_relacion_laboral_nrp
         AND id_riss IN(1,2)
         AND id_riss_rl = 0;

      UPDATE afi_riss
         SET id_riss_rl = 2
       WHERE id_derechohabiente = afi_relacion_laboral_id_derechohabiente
         AND nrp = afi_relacion_laboral_nrp
         AND id_riss = 3
         AND id_riss_rl = 4;

      UPDATE afi_riss
         SET id_riss_rl = 3
       WHERE id_derechohabiente = afi_relacion_laboral_id_derechohabiente
         AND nrp = afi_relacion_laboral_nrp
         AND id_riss = 3
         AND id_riss_rl = 5;

      --TRACE "Se inserto y actualizó el registro";

      -- se cuenta un reingreso
      LET v_num_reingresos_realizados = v_num_reingresos_realizados + 1;
   ELSE
      -- la relacion con el NRP ya existia, se rechaza
      LET v_codigo_rechazo = 11; -- NRP ya existe

      EXECUTE PROCEDURE sp_afi_imss_sinf_registra_rechazo(tmp_afi_reingreso_tpo_movimiento,
                                                          tmp_afi_reingreso_nrp                 ,
                                                          tmp_afi_reingreso_f_movimiento        ,
                                                          tmp_afi_reingreso_curp_rfc            ,
                                                          tmp_afi_reingreso_t_trabajador        ,
                                                          tmp_afi_reingreso_nss                 ,
                                                          tmp_afi_reingreso_nombre              ,
                                                          tmp_afi_reingreso_presentacion_extemp ,
                                                          tmp_afi_reingreso_jornada_semana      ,
                                                          tmp_afi_reingreso_sdi                 ,
                                                          tmp_afi_reingreso_sexo                ,
                                                          tmp_afi_reingreso_nss_correcto        ,
                                                          tmp_afi_reingreso_nombre_correcto     ,
                                                          tmp_riss_imss                         ,
                                                          v_codigo_rechazo                      ,
                                                          p_folio);

      RETURN v_codigo_rechazo, isam_err, err_txt, v_hubo_alta_en_reingreso;
   END IF

   SET PDQPRIORITY DEFAULT;

   -- se indica cuantas cuentas se abrieron
   LET err_txt = "Reingresos realizados: " || v_num_reingresos_realizados;

   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_i_resultado, isam_err, err_txt, v_hubo_alta_en_reingreso;

END FUNCTION;


