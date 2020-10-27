






CREATE FUNCTION "safreviv".fn_afi_prevalida_integracion(p_usuario_cod CHAR(20),
                                             p_folio DECIMAL(10),
                                             p_nombre_archivo CHAR(40),
                                             p_pid DECIMAL(9,0),
                                             p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255)

-- variables de la tabla del sumario
DEFINE tmp_afi_sumario_num_altas         DECIMAL(8,0);
DEFINE tmp_afi_sumario_num_bajas         DECIMAL(8,0);
DEFINE tmp_afi_sumario_num_cambio_nss    DECIMAL(8,0);
DEFINE tmp_afi_sumario_num_cambio_nombre DECIMAL(8,0);
DEFINE tmp_afi_sumario_num_modif_salario DECIMAL(8,0);
DEFINE tmp_afi_sumario_num_reingreso     DECIMAL(8,0);
DEFINE tmp_afi_sumario_num_riss          DECIMAL(8,0);
DEFINE tmp_afi_sumario_num_registros     DECIMAL(8,0);

-- variables para validaciÓn por tipo de registro
DEFINE v_num_altas         DECIMAL(8,0);
DEFINE v_num_bajas         DECIMAL(8,0);
DEFINE v_num_cambio_nss    DECIMAL(8,0);
DEFINE v_num_cambio_nombre DECIMAL(8,0);
DEFINE v_num_modif_salario DECIMAL(8,0);
DEFINE v_num_reingreso     DECIMAL(8,0);
DEFINE v_num_riss          DECIMAL(8,0);
DEFINE v_num_registros     DECIMAL(8,0);

-- variables para indicar que hubo error en los totales
DEFINE v_error_num_altas         SMALLINT;
DEFINE v_error_num_bajas         SMALLINT;
DEFINE v_error_num_cambio_nss    SMALLINT;
DEFINE v_error_num_cambio_nombre SMALLINT;
DEFINE v_error_num_modif_salario SMALLINT;
DEFINE v_error_num_reingreso     SMALLINT;
DEFINE v_error_num_riss          SMALLINT;
DEFINE v_error_num_registros     SMALLINT;

-- Control de Excepciones
DEFINE v_i_resultado       SMALLINT;
DEFINE sql_err             INTEGER;
DEFINE isam_err            INTEGER;
DEFINE err_txt             VARCHAR(255);

   -- se define el comportamiento en la ocurrencia de una excepción
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN sql_err, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_afi_prevalida_integracion.trace";
   --TRACE ON;

   -- se asigna el folio a la operacion
   UPDATE bat_ctr_operacion
      SET folio = p_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod = 2;

   -- se cambia el estatus del archivo a integrado
   UPDATE glo_ctr_archivo
      SET estado = 2, -- integrado
          folio = p_folio
    WHERE proceso_cod = 1801
      AND nombre_archivo = p_nombre_archivo;

   --trace "nombre archivo " || p_nombre_archivo;

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado                         = 0;
   LET sql_err                               = 0;
   LET isam_err                              = 0;
   LET err_txt = "El proceso de prevalidación de integración de movimientos afiliatorios finalizó correctamente";

   -- se asume que no hay errores
   LET v_error_num_altas         = 0;
   LET v_error_num_bajas         = 0;
   LET v_error_num_cambio_nss    = 0;
   LET v_error_num_cambio_nombre = 0;
   LET v_error_num_modif_salario = 0;
   LET v_error_num_reingreso     = 0;
   LET v_error_num_riss          = 0;
   LET v_error_num_registros     = 0;

   -- se obtienen los totales de la tabla de sumario
   SELECT
      num_altas         ,
      num_bajas         ,
      num_cambio_nss    ,
      num_cambio_nombre ,
      num_modif_salario ,
      num_reingreso     ,
      num_riss          ,
      num_registros
   INTO
      tmp_afi_sumario_num_altas         ,
      tmp_afi_sumario_num_bajas         ,
      tmp_afi_sumario_num_cambio_nss    ,
      tmp_afi_sumario_num_cambio_nombre ,
      tmp_afi_sumario_num_modif_salario ,
      tmp_afi_sumario_num_reingreso     ,
      tmp_afi_sumario_num_riss          ,
      tmp_afi_sumario_num_registros
   FROM safre_tmp:tmp_afi_sumario;

   -- se obtienen los totales por tabla de tipo de movimiento
   SELECT COUNT(*)
     INTO v_num_altas
     FROM safre_tmp:tmp_afi_alta;

   SELECT COUNT(*)
     INTO v_num_bajas
     FROM safre_tmp:tmp_afi_baja;

   SELECT COUNT(*)
     INTO v_num_cambio_nss
     FROM safre_tmp:tmp_afi_cambio_nss;

   SELECT COUNT(*)
     INTO v_num_cambio_nombre
     FROM safre_tmp:tmp_afi_cambio_nombre;

   SELECT COUNT(*)
     INTO v_num_reingreso
     FROM safre_tmp:tmp_afi_reingreso;

   SELECT COUNT(*)
     INTO v_num_modif_salario
     FROM safre_tmp:tmp_afi_modif_salario;

   SELECT COUNT(*)
     INTO v_num_riss
     FROM safre_tmp:tmp_afi_riss;

   -- se obtiene el total de registros
   LET v_num_registros = v_num_altas + v_num_bajas + v_num_cambio_nss + v_num_cambio_nombre + v_num_reingreso + v_num_modif_salario + v_num_riss;

   -- se verifican las cantidades definidas por el sumario comparando con las encontradas en tablas
   -- total de altas
   IF ( tmp_afi_sumario_num_altas <> v_num_altas ) THEN
      LET v_error_num_altas         = 1;
   END IF

   -- total de bajas
   IF ( tmp_afi_sumario_num_bajas <> v_num_bajas ) THEN
      LET v_error_num_bajas         = 1;
   END IF

   -- total de cambio de NSS
   IF ( tmp_afi_sumario_num_cambio_nss <> v_num_cambio_nss ) THEN
      LET v_error_num_cambio_nss    = 1;
   END IF

   -- total de cambio de nombre
   IF ( tmp_afi_sumario_num_cambio_nombre <> v_num_cambio_nombre ) THEN
      LET v_error_num_cambio_nombre = 1;
   END IF

   -- total de modificacion de salario
   IF ( tmp_afi_sumario_num_modif_salario <> v_num_modif_salario ) THEN
      LET v_error_num_modif_salario = 1;
   END IF

   -- total de reingresos
   IF ( tmp_afi_sumario_num_reingreso <> v_num_reingreso ) THEN
      LET v_error_num_reingreso     = 1;
   END IF

   -- total de riss
   IF ( tmp_afi_sumario_num_riss <> v_num_riss ) THEN
      LET v_error_num_riss          = 1;
   END IF

   -- total de registros
   IF ( tmp_afi_sumario_num_registros <> v_num_registros ) THEN
      LET v_error_num_registros     = 1;
   END IF

   -- se verifica si hubo errores
   IF ( (v_error_num_altas         = 1) OR
        (v_error_num_bajas         = 1) OR
        (v_error_num_cambio_nss    = 1) OR
        (v_error_num_cambio_nombre = 1) OR
        (v_error_num_modif_salario = 1) OR
        (v_error_num_reingreso     = 1) OR
        (v_error_num_registros     = 1) OR
        (v_error_num_riss          = 1) ) THEN

      -- se inicia el mensaje
      LET err_txt = "Error al prevalidar\n";

      -- se marca que ocurrió un error
      LET sql_err = 1;

      -- se verifica que cantidad no coincidio
      IF ( v_error_num_altas = 1 ) THEN
         LET err_txt = err_txt || "Total de altas no coincide\n";
      END IF

      IF ( v_error_num_bajas = 1 ) THEN
         LET err_txt = err_txt || "Total de  bajas no coincide\n";
      END IF

      IF ( v_error_num_cambio_nss = 1 ) THEN
         LET err_txt = err_txt || "Total de cambio NSS no coincide\n";
      END IF

      IF ( v_error_num_cambio_nombre = 1 ) THEN
         LET err_txt = err_txt || "Total de cambio Nombre no coincide\n";
      END IF

      IF ( v_error_num_modif_salario = 1 ) THEN
         LET err_txt = err_txt || "Total de  modificación de salario no coincide\n";
      END IF

      IF ( v_error_num_reingreso = 1 ) THEN
         LET err_txt = err_txt || "Total de reingresos no coincide\n";
      END IF

      IF ( v_error_num_riss = 1 ) THEN
         LET err_txt = err_txt || "Total de registros RISS no coincide\n";
      END IF

      IF ( v_error_num_registros = 1 ) THEN
         LET err_txt = err_txt || "Total registros no coincide\n";
      END IF

      -- se devuelve el error
      RETURN sql_err, isam_err, err_txt;
   END IF

   --trace "El proceso de prevalidación de integración de movimientos afiliatorios finalizó correctamente.";
   LET err_txt = "El proceso de prevalidación de integración de movimientos afiliatorios finalizó correctamente";

   RETURN sql_err, isam_err, err_txt;
END FUNCTION
;


