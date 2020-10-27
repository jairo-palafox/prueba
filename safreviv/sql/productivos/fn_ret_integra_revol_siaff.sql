






CREATE FUNCTION "safreviv".fn_ret_integra_revol_siaff(p_usuario_cod    CHAR(20), p_folio DECIMAL(9,0),
                                           p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                           p_proceso_cod    SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)

-- detalle de la tabla temporal
--tmp_ret_det_his_ley73
DEFINE tmp_registro_desc_ramo        CHAR(26);
DEFINE tmp_registro_desc_unidad      CHAR(9) ;
DEFINE tmp_registro_uni_folio        CHAR(8) ;
DEFINE tmp_registro_arch_envio       CHAR(30);
DEFINE tmp_registro_arch_salida      CHAR(30);
DEFINE tmp_registro_arch_acuse       CHAR(30);
DEFINE tmp_registro_arch_devol       CHAR(30);
DEFINE tmp_registro_arch_regreso     CHAR(30);
DEFINE tmp_registro_total_rechazo    CHAR(22);
DEFINE tmp_registro_estatus_nom      CHAR(20);
DEFINE tmp_registro_cve_banco        CHAR(3) ;
DEFINE tmp_registro_clave_rastreo    CHAR(30);
DEFINE tmp_registro_estatus_det      CHAR(1) ;
DEFINE tmp_registro_desc_estatus_det CHAR(10);
DEFINE tmp_registro_ramo             CHAR(1) ;
DEFINE tmp_registro_unidad           CHAR(3) ;
DEFINE tmp_registro_folio_clc        CHAR(3) ;
DEFINE tmp_registro_fecha_presenta   CHAR(10);
DEFINE tmp_registro_fecha_pago       CHAR(10);
DEFINE tmp_registro_arch_entrada     CHAR(30);
DEFINE tmp_registro_nss              CHAR(18);
DEFINE tmp_registro_nombre           CHAR(40);
DEFINE tmp_registro_cta_bancaria     CHAR(18);
DEFINE tmp_registro_importe          CHAR(22);
DEFINE tmp_registro_numero_oprbanc   CHAR(10);
DEFINE tmp_registro_cve_rechazo      CHAR(3) ;
DEFINE tmp_registro_desc_rechazo     CHAR(40);

-- tablas destino
-- ret_his_anexo1
DEFINE registro_revol_desc_ramo        CHAR(26);
DEFINE registro_revol_desc_unidad      CHAR(9) ;
DEFINE registro_revol_uni_folio        CHAR(8) ;
DEFINE registro_revol_arch_envio       CHAR(30);
DEFINE registro_revol_arch_salida      CHAR(30);
DEFINE registro_revol_arch_acuse       CHAR(30);
DEFINE registro_revol_arch_devol       CHAR(30);
DEFINE registro_revol_arch_regreso     CHAR(30);
DEFINE registro_revol_total_rechazo    CHAR(22);
DEFINE registro_revol_estatus_nom      CHAR(20);
DEFINE registro_revol_cve_banco        CHAR(3) ;
DEFINE registro_revol_clave_rastreo    CHAR(30);
DEFINE registro_revol_estatus_det      CHAR(1) ;
DEFINE registro_revol_desc_estatus_det CHAR(10);
DEFINE registro_revol_ramo             CHAR(1) ;
DEFINE registro_revol_unidad           CHAR(3) ;
DEFINE registro_revol_folio_clc        CHAR(3) ;
DEFINE registro_revol_fecha_presenta   DATE;
DEFINE registro_revol_fecha_pago       DATE;
DEFINE registro_revol_arch_entrada     CHAR(30);
DEFINE registro_revol_nss              CHAR(11);
DEFINE registro_revol_nombre           CHAR(40);
DEFINE registro_revol_cta_bancaria     CHAR(18);
DEFINE registro_revol_importe          DECIMAL(22,2);
DEFINE registro_revol_numero_oprbanc   CHAR(10);
DEFINE registro_revol_cve_rechazo      CHAR(3) ;
DEFINE registro_revol_desc_rechazo     CHAR(40);

DEFINE registro_revol_id_solicitud                DECIMAL(9,0);
DEFINE registro_revol_folio                       DECIMAL(9,0);
DEFINE registro_revol_estado_solicitud            INTEGER;
DEFINE registro_revol_cod_rechazo                 INTEGER;
DEFINE registro_revol_id_solicitud_act            INTEGER;

-- variables de soporte al proceso
DEFINE v_id_solicitud                          DECIMAL(9,0);
DEFINE v_id_solicitud_act                      DECIMAL(9,0);
DEFINE v_importe                               DECIMAL(12,2);
DEFINE v_anio_cont                             CHAR(4);
DEFINE v_mes_cont                              CHAR(2);
DEFINE v_dia_cont                              CHAR(2);
DEFINE v_anio_doc                              CHAR(4);
DEFINE v_mes_doc                               CHAR(2);
DEFINE v_dia_doc                               CHAR(2);
DEFINE v_fecha                                 CHAR(8);
DEFINE v_id_derechohabiente                    DECIMAL(9,0);
DEFINE v_marca_cargo_ssv                       SMALLINT;
DEFINE v_i_estado_marca                        SMALLINT;
DEFINE v_ape_paterno                           CHAR(40);
DEFINE v_ape_materno                           CHAR(40);
DEFINE v_nombre                                CHAR(40);
DEFINE v_posicion_signo                        SMALLINT;
DEFINE v_posicion_segundo_signo                SMALLINT;
DEFINE v_f_nacimiento                          DATE;
DEFINE v_sin_paterno                           SMALLINT;
DEFINE v_sin_materno                           SMALLINT;
DEFINE v_edo_sol_revol                         SMALLINT;
DEFINE v_nss_numerico                          DECIMAL(11,0);
DEFINE v_nss                                   CHAR(11);
DEFINE v_existe_tesofe_liquidado               SMALLINT;
-- =============================================================================

-- para rechazos
DEFINE v_b_rechazo_detalle                     SMALLINT;

DEFINE v_sumario_importe_total                 DECIMAL(22,6);
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;
DEFINE v_total_registros                       DECIMAL(2,0) ;
DEFINE v_numero_registros                      DECIMAL(9,0) ;
DEFINE v_existe_en_hist                        SMALLINT;

-- arreglo de codigos de rechazo
DEFINE v_indice_codigos_rechazo                INTEGER;

-- conteo de rechazos e inserciones
DEFINE v_reg_det_insertados                    INTEGER; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    INTEGER; -- total de registros de detalle rechazados

-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado             INTEGER;
DEFINE v_error_det_tpo_registro_invalido         INTEGER;
DEFINE v_error_monto_invalido                    INTEGER; -- error
DEFINE v_error_marca                             INTEGER;
DEFINE v_error_datos_insuficientes               INTEGER;
-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt, tmp_registro_nss;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/safreviv_int/BD/integra_ssv_siaff.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   LET v_indice_codigos_rechazo = 0;
   LET v_importe = 0;
   -- LET v_edo_vol_resol = 0;

   -- Inicializa las variables temporales
   LET tmp_registro_desc_ramo = NULL;
   LET tmp_registro_desc_unidad = NULL;
   LET tmp_registro_uni_folio = NULL;
   LET tmp_registro_arch_envio = NULL;
   LET tmp_registro_arch_salida = NULL;
   LET tmp_registro_arch_acuse = NULL;
   LET tmp_registro_arch_devol = NULL;
   LET tmp_registro_arch_regreso = NULL;
   LET tmp_registro_total_rechazo = NULL;
   LET tmp_registro_estatus_nom = NULL;
   LET tmp_registro_cve_banco = NULL;
   LET tmp_registro_clave_rastreo = NULL;
   LET tmp_registro_estatus_det = NULL;
   LET tmp_registro_desc_estatus_det = NULL;
   LET tmp_registro_ramo = NULL;
   LET tmp_registro_unidad = NULL;
   LET tmp_registro_folio_clc = NULL;
   LET tmp_registro_fecha_presenta = NULL;
   LET tmp_registro_fecha_pago = NULL;
   LET tmp_registro_arch_entrada = NULL;
   LET tmp_registro_nss = NULL;
   LET tmp_registro_nombre = NULL;
   LET tmp_registro_cta_bancaria = NULL;
   LET tmp_registro_importe = NULL;
   LET tmp_registro_numero_oprbanc = NULL;
   LET tmp_registro_cve_rechazo = NULL;
   LET tmp_registro_desc_rechazo = NULL;

   LET v_anio_cont = NULL;
   LET v_anio_doc = NULL;
   LET v_mes_cont = NULL;
   LET v_mes_doc = NULL;
   LET v_dia_cont = NULL;
   LET v_dia_doc = NULL;
   LET v_ape_paterno = NULL;
   LET v_ape_materno = NULL;
   LET v_nombre = NULL;
   LET v_f_nacimiento = NULL;
   LET v_nss = NULL;
   LET v_existe_tesofe_liquidado = 0;


   -- se asume que el proceso termina bien
   LET v_estatus_proceso    = 0;
   LET v_si_resultado       = 0;
   LET isam_err             = 0;
   LET v_c_msj              = 'El proceso finalizó exitosamente.';
   LET tmp_registro_nss = NULL;

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = P_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    pid         = p_pid;

   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado             = 1;
   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_monto_invalido                    = 11;
   LET v_error_marca                             = 12;
   LET v_error_datos_insuficientes               = 13;
   LET registro_revol_estado_solicitud             = 0;
   LET registro_revol_cod_rechazo                  = 0;
   LET v_id_derechohabiente                      = 0;
   LET registro_revol_id_solicitud                 = 0;
   LET registro_revol_folio                        = p_folio;
   LET v_marca_cargo_ssv                         = 819;

   -- ===================================================
   -- validaciones encabezado contra detalle
   --- se cuentan los registros de la tabla temporal de detalle
   -- trace on;
   -- integracion de detalle
   FOREACH

   SELECT desc_ramo,     desc_unidad,      uni_folio,     arch_envio,
          arch_salida,   arch_acuse,       arch_devol,    arch_regreso,
          total_rechazo, estatus_nom,      cve_banco,     clave_rastreo,
          estatus_det,   desc_estatus_det, ramo,          unidad,
          folio_clc,     fecha_presenta,   fecha_pago,    arch_entrada,
          rfc_curp,      nombre,           cta_bancaria,  importe,
          numero_oprbanc,cve_rechazo,      desc_rechazo
   INTO
         tmp_registro_desc_ramo,     tmp_registro_desc_unidad,     tmp_registro_uni_folio,     tmp_registro_arch_envio,
         tmp_registro_arch_salida,   tmp_registro_arch_acuse,      tmp_registro_arch_devol,    tmp_registro_arch_regreso,
         tmp_registro_total_rechazo, tmp_registro_estatus_nom,     tmp_registro_cve_banco,     tmp_registro_clave_rastreo,
         tmp_registro_estatus_det,   tmp_registro_desc_estatus_det,tmp_registro_ramo,          tmp_registro_unidad,
         tmp_registro_folio_clc,     tmp_registro_fecha_presenta,  tmp_registro_fecha_pago,    tmp_registro_arch_entrada,
         tmp_registro_nss,           tmp_registro_nombre,          tmp_registro_cta_bancaria,  tmp_registro_importe,
         tmp_registro_numero_oprbanc,tmp_registro_cve_rechazo,     tmp_registro_desc_rechazo
   FROM
   safre_tmp:tmp_ret_revolvente_siaff

      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;
      ---- Convertimos el dato a numerico pra eliminar los ceros
      LET v_nss_numerico = tmp_registro_nss;
      IF v_nss_numerico < 10000000000 THEN
         LET v_nss = "0" || v_nss_numerico ;
      ELSE
         LET v_nss = v_nss_numerico;
      END IF
      -- se busca el derechohabiente
      -- Se busca que este dado de alta en afi_derechohabiente
      LET v_existe_en_hist = 0;
      SELECT COUNT(*)
      INTO   v_existe_en_hist
      FROM   afi_derechohabiente -- ret_his_anexo1
      WHERE  nss = v_nss;

      LET registro_revol_estado_solicitud = 0;
      LET registro_revol_cod_rechazo = 0;
      LET registro_revol_id_solicitud_act = 0;
      LET registro_revol_fecha_presenta = NULL;
      LET registro_revol_fecha_pago = NULL;

      -- si no se encontro el id_derechohabiente
      IF ( v_existe_en_hist = 0 OR v_existe_en_hist IS NULL ) THEN
         LET registro_revol_estado_solicitud = 100; -- rechazada
         LET registro_revol_cod_rechazo = v_error_det_nss_no_encontrado;
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      ELSE
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente -- ret_his_anexo1
         WHERE  nss = v_nss;
      END IF

      LET v_id_solicitud_act = NULL;
      LET v_existe_tesofe_liquidado = 0;

      ----- Se busca en la tabla ret_cargos_ssv_siaff, se implementa la busqueda por el mas antiguo
      SELECT MIN(id_solicitud)
      INTO   v_id_solicitud_act
      FROM   ret_cargos_ssv_siaff
      WHERE  nss = v_nss
      AND    estado_solicitud = 300;
      --AND    importe = tmp_registro_importe;    ---Busca que este con estado solicitud 'Pagado por Recuperar'
      -- Busca el movimiento en la tabla ret_preliquida para ver si se liquido y el monto coincide
      SELECT COUNT(*)
      INTO   v_existe_tesofe_liquidado
      FROM   ret_preliquida
      WHERE  id_referencia = v_id_solicitud_act
      AND    id_derechohabiente = v_id_derechohabiente
      AND    subcuenta = 47
      AND    ABS(monto_pesos) = tmp_registro_importe;

      IF v_id_solicitud_act IS NOT NULL AND v_existe_tesofe_liquidado = 1 THEN
         LET v_edo_sol_revol = 301; -- Pagado y Recuperado
         UPDATE ret_cargos_ssv_siaff
         SET    estado_solicitud = v_edo_sol_revol
         WHERE  id_solicitud = v_id_solicitud_act;
      ELSE
         LET v_edo_sol_revol = 302; -- Recuperado sin Pago
      END IF

      IF registro_revol_estado_solicitud = 0 THEN
         LET registro_revol_estado_solicitud = v_edo_sol_revol;
      END IF

      SELECT seq_ret_revolvente_siaff.NEXTVAL
      INTO   registro_revol_id_solicitud
      FROM   systables
      WHERE  tabid = 1;

      LET registro_revol_desc_ramo        = tmp_registro_desc_ramo;
      LET registro_revol_desc_unidad      = tmp_registro_desc_unidad;
      LET registro_revol_uni_folio        = tmp_registro_uni_folio;
      LET registro_revol_arch_envio       = tmp_registro_arch_envio;
      LET registro_revol_arch_salida      = tmp_registro_arch_salida;
      LET registro_revol_arch_acuse       = tmp_registro_arch_acuse;
      LET registro_revol_arch_devol       = tmp_registro_arch_devol;
      LET registro_revol_arch_regreso     = tmp_registro_arch_regreso;
      LET registro_revol_total_rechazo    = tmp_registro_total_rechazo;
      LET registro_revol_estatus_nom      = tmp_registro_estatus_nom;
      LET registro_revol_cve_banco        = tmp_registro_cve_banco;
      LET registro_revol_clave_rastreo    = tmp_registro_clave_rastreo;
      LET registro_revol_estatus_det      = tmp_registro_estatus_det;
      LET registro_revol_desc_estatus_det = tmp_registro_desc_estatus_det;
      LET registro_revol_ramo             = tmp_registro_ramo;
      LET registro_revol_unidad           = tmp_registro_unidad;
      LET registro_revol_folio_clc        = tmp_registro_folio_clc;
      LET registro_revol_fecha_presenta   = mdy(tmp_registro_fecha_presenta[4,5],tmp_registro_fecha_presenta[1,2],tmp_registro_fecha_presenta[7,10]);
      LET registro_revol_fecha_pago       = mdy(tmp_registro_fecha_pago[4,5],tmp_registro_fecha_pago[1,2],tmp_registro_fecha_pago[7,10]);
      LET registro_revol_arch_entrada     = tmp_registro_arch_entrada;
      LET registro_revol_nss              = v_nss;
      LET registro_revol_nombre           = tmp_registro_nombre;
      LET registro_revol_cta_bancaria     = tmp_registro_cta_bancaria;
      LET registro_revol_importe          = tmp_registro_importe;
      LET registro_revol_numero_oprbanc   = tmp_registro_numero_oprbanc;
      LET registro_revol_cve_rechazo      = tmp_registro_cve_rechazo;
      LET registro_revol_desc_rechazo     = tmp_registro_desc_rechazo;

      LET registro_revol_folio            = p_folio;
      LET registro_revol_id_solicitud_act = v_id_solicitud_act;

      INSERT INTO ret_revolvente_siaff (
                  id_solicitud, nss,             folio,        estado_solicitud,
                  cod_rechazo,  id_solicitud_act,desc_ramo,    desc_unidad,
                  uni_folio,    arch_envio,      arch_salida,  arch_acuse,
                  arch_devol,   arch_regreso,    total_rechazo,estatus_nom,
                  cve_banco,    clave_rastreo,   estatus_det,  desc_estatus_det,
                  ramo,         unidad,          folio_clc,    fecha_presenta,
                  fecha_pago,   arch_entrada,    nombre,       cta_bancaria,
                  importe,      numero_oprbanc,  cve_rechazo,  desc_rechazo
      ) VALUES (
                  registro_revol_id_solicitud,    registro_revol_nss,
                  registro_revol_folio,           registro_revol_estado_solicitud,
                  registro_revol_cod_rechazo,     registro_revol_id_solicitud_act,
                  registro_revol_desc_ramo,       registro_revol_desc_unidad,
                  registro_revol_uni_folio,       registro_revol_arch_envio,
                  registro_revol_arch_salida,     registro_revol_arch_acuse,
                  registro_revol_arch_devol,      registro_revol_arch_regreso,
                  registro_revol_total_rechazo,   registro_revol_estatus_nom,
                  registro_revol_cve_banco,       registro_revol_clave_rastreo,
                  registro_revol_estatus_det,     registro_revol_desc_estatus_det,
                  registro_revol_ramo,            registro_revol_unidad,
                  registro_revol_folio_clc,       registro_revol_fecha_presenta,
                  registro_revol_fecha_pago,      registro_revol_arch_entrada,
                  registro_revol_nombre,          registro_revol_cta_bancaria,
                  registro_revol_importe,         registro_revol_numero_oprbanc,
                  registro_revol_cve_rechazo,     registro_revol_desc_rechazo);
      LET v_reg_det_insertados = v_reg_det_insertados + 1;
   END FOREACH;

   --trace off;
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_revolvente_siaff;
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_registro_nss;
END FUNCTION;


