






CREATE FUNCTION "safreviv".fn_ret_integra_aclara_fondo_ahorro(   p_usuario_cod    CHAR(20)
                                                     , p_folio          DECIMAL(9,0)
                                                     , p_nombre_archivo VARCHAR(40)
                                                     , p_pid            DECIMAL(9,0)
                                                     , p_proceso_cod    SMALLINT
                                                   )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(13)
   
-- campos de la tabla de encabezado de retiros de fondo ahorro (sin filler)
DEFINE tmp_ret_cza_tpo_registro                CHAR(2)    ;
DEFINE tmp_ret_cza_id_servicio                 CHAR(2)    ;
DEFINE tmp_ret_cza_id_operacion                CHAR(2)    ;
DEFINE tmp_ret_cza_f_operacion                 CHAR(8)    ;

-- campos de la tabla de detalle de retiros de fondo ahorro (sin filler)
DEFINE tmp_ret_det_tpo_registro                CHAR(2)    ;
DEFINE tmp_ret_det_id_servicio                 CHAR(2)    ;
DEFINE tmp_ret_det_id_operacion                CHAR(2)    ;
DEFINE tmp_ret_det_rfc_saci                    CHAR(13)   ;
DEFINE tmp_ret_det_folio_aclara                CHAR(9)    ;
DEFINE tmp_ret_det_f_aclara                    CHAR(8)    ;
DEFINE tmp_ret_det_rfc_nuevo                   CHAR(13)   ;
DEFINE tmp_ret_det_nss_aclarado                CHAR(11)   ;
DEFINE tmp_ret_det_nombre_aclarado             CHAR(40)   ;
DEFINE tmp_ret_det_tpo_movto                   CHAR(2)    ;
DEFINE tmp_ret_det_imp_movto                   CHAR(13)   ;
DEFINE tmp_ret_det_estatus                     CHAR(1)    ;


DEFINE ret_cza_aclara_folio                    DECIMAL(9,0);
DEFINE ret_cza_aclara_f_operacion              DATE;
DEFINE ret_cza_aclara_f_carga                  DATE;
DEFINE ret_cza_aclara_tot_registros            SMALLINT;


-- detalle de la tabla historica/integrada de aclaraciones del fondo de ahorro
-- ret_det_aclara_fa
DEFINE ret_det_aclara_folio                    DECIMAL(9,0)           ;
DEFINE ret_det_aclara_id_afi_fondo72           DECIMAL(9,0)           ;
DEFINE ret_det_aclara_rfc_saci                 CHAR(13)               ;
DEFINE ret_det_aclara_fiolio_aclara            DECIMAL(9,0)           ;
DEFINE ret_det_aclara_f_aclara                 DATE                   ;
DEFINE ret_det_aclara_rfc_nuevo                CHAR(13)               ;
DEFINE ret_det_aclara_nss_aclarado             CHAR(11)               ;
DEFINE ret_det_aclara_nombre_aclarado          CHAR(40)               ;
DEFINE ret_det_aclara_tpo_movto                CHAR(2)                ;
DEFINE ret_det_aclara_imp_movto                DECIMAL(14,2)          ;
DEFINE ret_det_aclara_estatus                  CHAR(1)                ;
DEFINE ret_det_aclara_estado_solicitud         CHAR(3)                ;
DEFINE ret_det_aclara_cod_rechazo              CHAR(3)                ;

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                  DECIMAL(9,0);
DEFINE v_id_afi_fondo72_cargo                DECIMAL(9,0);
DEFINE v_id_afi_fondo72_abono                DECIMAL(9,0);
DEFINE v_estado_solicitud                    CHAR(3);
DEFINE v_cod_rechazo                         SMALLINT;
DEFINE v_nss                                 CHAR(11);
DEFINE v_nombre                              CHAR(40);
DEFINE v_rfc_nuevo                           CHAR(13);
DEFINE v_ocurrencias                         SMALLINT; -- cantidad registros por busqueda
DEFINE v_marca_ci                            SMALLINT;  -- marca para Aclaracion FA Alta 811
DEFINE v_marca_uc                            SMALLINT;  -- marca para Aclaracion FA Unificacion 812
DEFINE v_marca_fa                            SMALLINT;  -- marca para Aclaracion FA Separacion 813
DEFINE v_tpo_actualiza                       SMALLINT;


--variable de solicitd   
DEFINE v_id_solicitud                        DECIMAL(9,0);
DEFINE v_id_solicitud_fa                     DECIMAL(9,0);
DEFINE v_ret_det_aclara_id_afi_fondo72       DECIMAL(9,0);   
DEFINE v_id_derechohabiente_cargo            DECIMAL(9,0);  
DEFINE v_id_derechohabiente_abono            DECIMAL(9,0);  

-- =============================================================================
-- para calcular las AIVs a pesos
DEFINE v_valor_fondo                           DECIMAL(14,6);
DEFINE v_pesos_aiv97                           DECIMAL(14,6);
DEFINE v_pesos_aiv92                           DECIMAL(14,6);
DEFINE v_saldo                                 DECIMAL(14,2);
                                               
-- para rechazos                               
DEFINE v_b_rechazo_encabezado                  SMALLINT;
DEFINE v_b_rechazo_detalle                     SMALLINT;
DEFINE v_validar_3_primeros_campos             VARCHAR(6); -- se concatenan los 3 primeros campos para validar
DEFINE v_afore_cod                             SMALLINT; -- clave de afore
-- id matriz derecho                           
DEFINE v_id_ret_matriz_derecho                 SMALLINT; -- id de la matriz de derecho de retiros
-- RECUPERADOS                                    
                                               
DEFINE v_sumario_importe_total                 DECIMAL(22,2);
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;

DEFINE v_numero_registros                      DECIMAL(9,0);
DEFINE v_total_imp72                           DECIMAL(22,2);
DEFINE v_total_tanto_imp72                     DECIMAL(22,2);
DEFINE v_saldo_cuenta                          DECIMAL(24,6);
                                               
DEFINE v_motivo_rechazo_1                      SMALLINT;
DEFINE v_motivo_rechazo_2                      SMALLINT;
DEFINE v_motivo_rechazo_3                      SMALLINT;
-- arreglo de codigos de rechazo               
DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 
DEFINE v_subcuenta                             SMALLINT;
                                               
-- conteo de rechazos e inserciones            
DEFINE v_reg_cza_insertados                    DECIMAL(9,0); -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados                    DECIMAL(9,0); -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados                    DECIMAL(9,0); -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    DECIMAL(9,0); -- total de registros de detalle rechazados

DEFINE v_reg_det_insert_15                     DECIMAL(9,0); -- total de registros de detalle insertados
DEFINE v_reg_det_insert_18                     DECIMAL(9,0); -- total de registros de detalle rechazados

-- codigos de error en encabezado
DEFINE v_error_cza_tpo_registro_invalido           SMALLINT;
DEFINE v_error_cza_id_servicio_invalido            SMALLINT;
DEFINE v_error_cza_id_operacion_invalido           SMALLINT;
DEFINE v_error_det_fecha_operacion_no_capturada    SMALLINT;
DEFINE v_resultado                                 SMALLINT;

-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado               SMALLINT;
DEFINE v_error_det_tpo_registro_invalido           SMALLINT;
DEFINE v_error_det_id_servicio_invalido            SMALLINT;
DEFINE v_error_det_id_operacion_invalido           SMALLINT;
DEFINE v_error_det_fecha_liquidacion_no_capturada  SMALLINT;
DEFINE v_error_det_tot_impviv72_no_capturado       SMALLINT;
DEFINE v_error_det_caso_adai_no_capturado          SMALLINT;
DEFINE v_error_det_tanto_no_es_doble_viv97         SMALLINT;

-- codigos de error en sumario
DEFINE v_error_sum_totales_no_coinciden          SMALLINT;

-- para marcar las cuentas
DEFINE v_i_estado_marca                          INTEGER;
DEFINE v_marca_fondo_ahorro                      INTEGER; -- 802 de acuerdo a catalogo

-- Control de Excepciones
DEFINE v_si_resultado                            INTEGER; --SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_rfc_saci;
   END EXCEPTION
     
   --SET DEBUG FILE TO "/safreviv_int/BD/fn_ret_integra_fondo_ahorro.trace";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados              = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados              = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados              = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados              = 0; -- total de registros de detalle rechazados
   LET v_ret_det_aclara_id_afi_fondo72 = 0;   
   LET v_reg_det_insert_15               = 0;
   LET v_reg_det_insert_18               = 0;
   LET v_subcuenta                       = 40;
   LET v_marca_ci                        = 811;
   LET v_marca_uc                        = 812;
   LET v_marca_fa                        = 813;
   LET v_nss                             = "";
   LET v_nombre                          = "";

   
   -- se asume que el proceso termina bien
   LET v_si_resultado  = 0;
   LET isam_err        = 0;
   LET v_c_msj         = 'El proceso finalizó exitosamente.';
   LET tmp_ret_det_rfc_saci = NULL;

   -- se inician los codigos de error en encabezado
   LET v_error_cza_tpo_registro_invalido            = 1;
   LET v_error_cza_id_servicio_invalido             = 2;
   LET v_error_cza_id_operacion_invalido            = 3;
   LET v_error_det_fecha_operacion_no_capturada     = 4;
    
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado                = 49; -- NSS NO ENCONTRADO
   LET v_error_det_tpo_registro_invalido            = 6;
   LET v_error_det_id_servicio_invalido             = 7;
   LET v_error_det_id_operacion_invalido            = 8;
   LET v_error_det_fecha_liquidacion_no_capturada   = 9;
   LET v_error_det_tot_impviv72_no_capturado        = 10;
   LET v_error_det_caso_adai_no_capturado           = 11;
   LET v_error_det_tanto_no_es_doble_viv97          = 12;
   LET v_error_sum_totales_no_coinciden             = 13;

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro = 802; -- marca para fondo ahorro
   LET v_i_estado_marca     = 0;
   LET v_estado_solicitud   = 0;
   LET v_cod_rechazo        = 0;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio  = p_folio,
          estado = 2             -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a proceso
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;
   
   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   CREATE TEMP TABLE tmp_codigos_rechazo (
                                          id_derechohabiente  DECIMAL(9,0)
                                         ,id_codigo          SMALLINT
                                         ,codigo_rechazo     SMALLINT
                                         );
   -- se obtienen los datos del encabezado
   FOREACH
   SELECT 
       tpo_registro
      ,id_servicio
      ,id_operacion
      ,f_opera
   INTO
       tmp_ret_cza_tpo_registro
      ,tmp_ret_cza_id_servicio
      ,tmp_ret_cza_id_operacion
      ,tmp_ret_cza_f_operacion
   FROM
      safre_tmp:tmp_ret_cza_acl_fa 
 
      -- se asume que no hay error
      LET v_b_rechazo_encabezado = 0;
      
      -- se borra la tabla de errores
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
 
      -- se reinicia el indice de codigos de rechazo
      LET v_indice_codigos_rechazo = 1;
      LET v_id_derechohabiente = 0;
      
      -- =========================================================
      -- validando tipo de registro
      IF ( tmp_ret_cza_tpo_registro <> "01" OR tmp_ret_cza_id_servicio <> "04" OR
           tmp_ret_cza_id_operacion <> "20") THEN
         -- se activa la bandera de rechazo de encabezado
         LET v_b_rechazo_encabezado = 1;
         --trace("Validando encabezado entro ");
      
         -- 1	Tipo de Registro	X	02	00	001	-	002	"01" Encabezado de lote
         IF ( tmp_ret_cza_tpo_registro <> "01" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_cza_tpo_registro_invalido);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;           
         END IF
         
         -- validando identificador de servicio
         -- 2	Identificador de Servicio	X	02	00	003	-	004	"04" Retiros
         IF ( tmp_ret_cza_id_servicio <> "04" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_cza_id_servicio_invalido);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF
         -- validando identificador de operacion
         -- 3	Identificador de Operacion	X	02	00	005	-	006	"20" Retiros
         IF ( tmp_ret_cza_id_operacion <> "20" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_cza_id_operacion_invalido);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF
      END IF
      
      -- =========================================================
      -- se verifica si el encabezado contiene fecha procesar
      IF ( tmp_ret_cza_f_operacion IS NULL ) THEN
        INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_fecha_operacion_no_capturada);

        -- se incrementa el indice
        LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
      END IF      

      -- =========================================================
       
      -- si hubo rechazo de encabezado no se inserta en historico
      IF ( v_b_rechazo_encabezado = 1 ) THEN

         -- se leen los tres primeros errores
         FOREACH
            SELECT FIRST 3 id_codigo,
                           codigo_rechazo
              INTO  v_indice_codigos_rechazo, v_codigos_rechazo
              FROM  tmp_codigos_rechazo
            ORDER BY   id_codigo
               
               IF ( v_indice_codigos_rechazo = 1 ) THEN
                  -- se asignan los primeros 3 codigos de rechazo
                  LET v_motivo_rechazo_1 = v_codigos_rechazo;
               END IF
            
               IF ( v_indice_codigos_rechazo = 2 ) THEN
                  -- se asignan los primeros 3 codigos de rechazo
                  LET v_motivo_rechazo_2 = v_codigos_rechazo;
               END IF
            
               IF ( v_indice_codigos_rechazo = 3 ) THEN
                  -- se asignan los primeros 3 codigos de rechazo
                  LET v_motivo_rechazo_3 = v_codigos_rechazo;
                  EXIT FOREACH;
               END IF 
         END FOREACH;
        
         -- se cuenta un encabezado rechazado
         LET v_reg_cza_rechazados = v_reg_cza_rechazados + 1;
         
         -- el registro fue rechazado y no se inserta en el historico
         CONTINUE FOREACH;

      END IF
      -- se cuenta un encabezado insertado

      LET ret_cza_aclara_folio           = p_folio;
      LET ret_cza_aclara_f_operacion     = MDY(SUBSTR(tmp_ret_cza_f_operacion,5,2),SUBSTR(tmp_ret_cza_f_operacion,7,2),SUBSTR(tmp_ret_cza_f_operacion,1,4));
      LET ret_cza_aclara_f_carga         = today;
      LET ret_cza_aclara_tot_registros   = 0;

      LET v_reg_cza_insertados = v_reg_cza_insertados + 1;

      INSERT INTO ret_cza_aclara_fondo_ahorro
                  (
                    folio,
                    f_operacion,
                    f_carga,
                    tot_registros
                  )
           VALUES (
                    ret_cza_aclara_folio,
                    ret_cza_aclara_f_operacion,
                    ret_cza_aclara_f_carga,
                    ret_cza_aclara_tot_registros
                  );
      END FOREACH;

   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud             = 0;
   LET v_id_derechohabiente_cargo = 0;
   LET v_id_derechohabiente_abono = 0;
   
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;    

   -- se obtienen los datos del detalle
   FOREACH
   SELECT
       tpo_registro
      ,id_servicio
      ,id_operacion
      ,rfc_saci
      ,folio_aclara
      ,f_aclara
      ,rfc_nuevo
      ,nss_aclarado
      ,nombre_aclarado
      ,tpo_movto
      ,imp_movto
      ,estatus
   INTO
       tmp_ret_det_tpo_registro
      ,tmp_ret_det_id_servicio
      ,tmp_ret_det_id_operacion
      ,tmp_ret_det_rfc_saci
      ,tmp_ret_det_folio_aclara
      ,tmp_ret_det_f_aclara
      ,tmp_ret_det_rfc_nuevo
      ,tmp_ret_det_nss_aclarado
      ,tmp_ret_det_nombre_aclarado
      ,tmp_ret_det_tpo_movto
      ,tmp_ret_det_imp_movto
      ,tmp_ret_det_estatus
   FROM safre_tmp:tmp_ret_det_acl_fa
   ORDER BY folio_aclara  --tabla para almacenar el detalle
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle        = 0;
      LET v_id_derechohabiente_cargo = 0;
      LET v_id_derechohabiente_abono = 0;

      -- ==========================================================================
      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud         = 0;
	  LET v_id_afi_fondo72_cargo = NULL;
      LET v_tpo_actualiza        = 0;
      -- se obtiene el id_solicitud
      SELECT seq_ret_aclara_fa_id_solicitud.NEXTVAL
        INTO v_id_solicitud
        FROM systables
       WHERE tabid = 1;
       
      LET v_indice_codigos_rechazo = 1;
     
      -- CI Cambio de Identificador 
	  IF ( tmp_ret_det_tpo_movto = "CI" ) THEN
             --- Busca RFC en afi_fondo72
             SELECT COUNT(id_afi_fondo72)
             INTO   v_ocurrencias
             FROM   afi_fondo72
             WHERE  rfc = tmp_ret_det_rfc_saci;
             
             --- si lo encuentra se obtiene el NSS y el Nombre, en caso contrario lo rechaza
             IF v_ocurrencias = 1 THEN
                 SELECT id_afi_fondo72, nss, nombre, id_derechohabiente
                 INTO   v_id_afi_fondo72_cargo, v_nss, v_nombre, v_id_derechohabiente_cargo
                 FROM   afi_fondo72
                 WHERE  rfc = tmp_ret_det_rfc_saci;
                 -- Se busca que no exista el RFC nuevo para poderlo dar de alta
                 -- si no tiene id_derechohabiente se le crea uno para poderlo marcar
                 IF v_id_derechohabiente_cargo IS NULL THEN 
                     SELECT seq_derechohabiente.NEXTVAL
                       INTO v_id_derechohabiente_cargo
                       FROM systables
                      WHERE tabid = 1;
                     INSERT INTO afi_fondo72_d
                                 (nss,
                                  rfc,
                                  id_derechohabiente
                                 )
                          VALUES
                                 (
                                  v_nss,
                                  tmp_ret_det_rfc_saci,
                                  v_id_derechohabiente_cargo
                                 );
                     UPDATE afi_fondo72
                        SET id_derechohabiente = v_id_derechohabiente_cargo
                      WHERE id_afi_fondo72 = v_id_afi_fondo72_cargo;
                 END IF
                 IF tmp_ret_det_rfc_nuevo <> tmp_ret_det_rfc_saci THEN
                     SELECT COUNT(id_afi_fondo72)
                     INTO   v_ocurrencias
                     FROM   afi_fondo72
                     WHERE  rfc = tmp_ret_det_rfc_nuevo;
                     LET v_tpo_actualiza = 2;
                 ELSE
                     SELECT COUNT(id_afi_fondo72)
                     INTO   v_ocurrencias
                     FROM   afi_fondo72
                     WHERE  rfc = tmp_ret_det_rfc_nuevo
                     AND    nss = tmp_ret_det_nss_aclarado;
                     LET v_tpo_actualiza = 12;
                 END IF
                 IF v_ocurrencias = 0 THEN                 
                     --- Crea un nuevo registro con el RFC nuevo y con NSS y Nombre del RFC encontrado
                     -- Se obtiene la secuencia del id_afi_fondo72 para dar de alta el nuevo registro
                     --Busca el saldo a transferir
                     LET v_saldo = 0;
                     SELECT SUM(NVL(importe,0))
                     INTO   v_saldo
                     FROM   afi_fondo72 afi
                            LEFT OUTER JOIN cta_fondo72 cta
                                         ON afi.id_afi_fondo72 =  cta.id_afi_fondo72
                                        AND cta.subcuenta      =  v_subcuenta
                                        AND (cta.movimiento    <> 422 AND  -- 422 CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                                             cta.movimiento    <> 601)     -- 601 ABONO, RETIRO FONDO 72 NO PAGADO, TANTO 
                     WHERE afi.id_afi_fondo72 = v_id_afi_fondo72_cargo;    
                     SELECT seq_afi_fondo72.NEXTVAL
                       INTO v_id_afi_fondo72_abono
                       FROM systables
                      WHERE tabid = 1;
                     --- Se obtiene el id_derechohabiente para guardar la integridad y poder marcar el registro
                     SELECT seq_derechohabiente.NEXTVAL
                       INTO v_id_derechohabiente_abono
                       FROM systables
                      WHERE tabid = 1;
                     IF  tmp_ret_det_rfc_nuevo = tmp_ret_det_rfc_saci THEN
                         LET v_nss    = tmp_ret_det_nss_aclarado;
                         LET v_nombre = tmp_ret_det_nombre_aclarado;
                     END IF 
                     INSERT INTO afi_fondo72_d
                                 (nss,
                                  rfc,
                                  id_derechohabiente
                                 )
                          VALUES
                                 (
                                  v_nss,
                                  tmp_ret_det_rfc_nuevo,
                                  v_id_derechohabiente_abono
                                 );
                     INSERT INTO afi_fondo72
                                 (
                                  id_afi_fondo72,
                                  rfc,
                                  nss,
                                  id_derechohabiente,
                                  nombre,
                                  f_apertura,
                                  ind_estado_cuenta
                                 )
                          VALUES (
                                  v_id_afi_fondo72_abono,
                                  tmp_ret_det_rfc_nuevo,
                                  v_nss,
                                  v_id_derechohabiente_abono,
                                  v_nombre,
                                  today,
                                  0);

                     INSERT INTO afi_his_fondo72
                                 (
                                  folio_aclaracion,
                                  id_afi_fondo72,
                                  f_modifica,
                                  rfc,
                                  nss,
                                  nombre,
                                  ind_modifica,
                                  id_origen
                                 )
                           VALUES 
                                 (
                                  p_folio,
                                  v_id_afi_fondo72_abono,
                                  today,
                                  tmp_ret_det_rfc_nuevo,
                                  v_nss,
                                  v_nombre,
                                  v_tpo_actualiza,     
                                  v_id_afi_fondo72_cargo
                                 );
                                  
                     INSERT INTO ret_aclara_accion_fa
                                 (
                                  id_solicitud,
                                  folio,
                                  rfc_cargo,
                                  id_afi_fondo72_cargo,
                                  rfc_abono,
                                  id_afi_fondo72_abono,
                                  saldo,
                                  tpo_movto
                                 )
                           VALUES
                                 (
                                  v_id_solicitud,
                                  p_folio,
                                  tmp_ret_det_rfc_saci,
                                  v_id_afi_fondo72_cargo,
                                  tmp_ret_det_rfc_nuevo,
                                  v_id_afi_fondo72_abono,
                                  v_saldo,
                                  tmp_ret_det_tpo_movto
                                 );
                     LET v_estado_solicitud = 15;    -- Lista para preliquidación
                     LET v_cod_rechazo      =  0;    -- Sin codigo de rechazo
                     -- Se inhabilita el registro del RFC SACI
                     UPDATE afi_fondo72
                     SET    ind_estado_cuenta = 1
                     WHERE  rfc = tmp_ret_det_rfc_saci
                     AND    id_derechohabiente = v_id_derechohabiente_cargo
                     AND    id_afi_fondo72     = v_id_afi_fondo72_cargo;
                     --- Marca las cuentas involucradas
                     --marca la cuenta de cargo, solo se marcará la cuenta de cargo
                     EXECUTE FUNCTION fn_marca_cuenta(
                             v_id_derechohabiente_cargo
                            ,v_marca_ci  -- marca de amortizaciones excedentes
                            ,v_id_solicitud
                            ,p_folio     -- folio se asignara en la solicitd
                            ,0     -- estado marca
                            ,0     -- codigo de rechazo
                            ,0     -- marca de la causa
                            ,NULL  -- fecha de la causa
                            ,USER  --usuario
                            ,p_proceso_cod) --proceso_cod
                        INTO v_i_estado_marca; 
                     IF v_i_estado_marca <> 0 THEN
                         LET v_estado_solicitud = 100;   -- Solicitud rechazada por registro existente
                         LET v_cod_rechazo      = 103;   -- Problemas al marcar
                     END IF   
                 ELSE
                     IF v_tpo_actualiza       = 12              AND 
                        v_ocurrencias         = 1               AND   
                        tmp_ret_det_nombre_aclarado IS NOT NULL AND 
                        tmp_ret_det_rfc_nuevo = tmp_ret_det_rfc_saci THEN
                         UPDATE afi_fondo72
                         SET    nombre = tmp_ret_det_nombre_aclarado
                         WHERE  RFC = tmp_ret_det_rfc_nuevo;                     
                         LET v_estado_solicitud = 800;   -- Solicitud Procesada
                         LET v_cod_rechazo      =   0;   -- Solicitud Existente
                     ELSE 
                         LET v_estado_solicitud = 100;   -- Solicitud rechazada por registro existente
                         LET v_cod_rechazo      = 850;   -- Solicitud Existente
                     END IF 
                 END IF
             ELSE
                 LET v_estado_solicitud = 100; -- Rechazada
                 IF v_ocurrencias > 1 THEN
                     LET v_cod_rechazo      = 52;  --mas de un registro para el rfc dado
                 ELSE
                     LET v_cod_rechazo      = 7;   -- no existe rfc
                 END IF
             END IF
      END IF
      -- UC Unificación de Cuentas
	  IF ( tmp_ret_det_tpo_movto = "UC" ) THEN
             
             --- si ambos se encuentran se acepta la solicitud, en caso contrario se debe rechazar
             --- asgina el valor correspondiente al estado solicitud (Siguiente paso Preliquidacion)
             --- Busca RFC anterior (SACI Unificado) en afi_fondo72 
             SELECT COUNT(id_afi_fondo72)
             INTO   v_ocurrencias
             FROM   afi_fondo72
             WHERE  rfc = tmp_ret_det_rfc_saci;
             
             --- si lo encuentra se obtiene el NSS y el Nombre, en caso contrario lo rechaza
             IF v_ocurrencias = 1 THEN
                 --- Busca RFC nuevo (Unificador) en afi_fondo72 
                 SELECT COUNT(id_afi_fondo72)
                 INTO   v_ocurrencias
                 FROM   afi_fondo72
                 WHERE  rfc = tmp_ret_det_rfc_nuevo;

                 IF v_ocurrencias = 1 THEN                 
                     --- Crea un nuevo registro con el RFC nuevo y con NSS y Nombre del RFC encontrado
                     
                     -- Se obtiene id_afi_fondo72 del unificado 
                     SELECT id_afi_fondo72, id_derechohabiente
                       INTO v_id_afi_fondo72_cargo, v_id_derechohabiente_cargo
                       FROM afi_fondo72
                      WHERE rfc = tmp_ret_det_rfc_saci;
                     IF v_id_derechohabiente_cargo IS NULL THEN 
                         SELECT seq_derechohabiente.NEXTVAL
                           INTO v_id_derechohabiente_cargo
                           FROM systables
                          WHERE tabid = 1;
                         INSERT INTO afi_fondo72_d
                                     (nss,
                                      rfc,
                                      id_derechohabiente
                                     )
                              VALUES
                                     (
                                      v_nss,
                                      tmp_ret_det_rfc_saci,
                                      v_id_derechohabiente_cargo
                                     );
                         UPDATE afi_fondo72
                            SET id_derechohabiente = v_id_derechohabiente_cargo
                          WHERE id_afi_fondo72 = v_id_afi_fondo72_cargo;
                     END IF                      
                     -- Se obtiene id_afi_fondo72 del unificador 
                     SELECT id_afi_fondo72, id_derechohabiente
                       INTO v_id_afi_fondo72_abono, v_id_derechohabiente_abono
                       FROM afi_fondo72
                      WHERE rfc = tmp_ret_det_rfc_nuevo;
                     IF v_id_derechohabiente_abono IS NULL THEN 
                         SELECT seq_derechohabiente.NEXTVAL
                           INTO v_id_derechohabiente_abono
                           FROM systables
                          WHERE tabid = 1;
                         INSERT INTO afi_fondo72_d
                                     (nss,
                                      rfc,
                                      id_derechohabiente
                                     )
                              VALUES
                                     (
                                      v_nss,
                                      tmp_ret_det_rfc_nuevo,
                                      v_id_derechohabiente_abono
                                     );
                         UPDATE afi_fondo72
                            SET id_derechohabiente = v_id_derechohabiente_abono
                          WHERE id_afi_fondo72 = v_id_afi_fondo72_abono;
                     END IF                     
                     --  Busca el saldo de la cuenta 
                     LET v_saldo = 0;
                     SELECT SUM(NVL(importe,0))
                     INTO   v_saldo
                     FROM   afi_fondo72 afi
                            LEFT OUTER JOIN cta_fondo72 cta
                                         ON afi.id_afi_fondo72 =  cta.id_afi_fondo72
                                        AND cta.subcuenta      =  v_subcuenta
                                        AND (cta.movimiento    <> 422 AND  -- 422 CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                                             cta.movimiento    <> 601)     -- 601 ABONO, RETIRO FONDO 72 NO PAGADO, TANTO 
                     WHERE afi.id_afi_fondo72 = v_id_afi_fondo72_cargo;  
                     INSERT INTO ret_aclara_accion_fa
                                 (
                                  id_solicitud,
                                  folio,
                                  rfc_cargo,
                                  id_afi_fondo72_cargo,
                                  rfc_abono,
                                  id_afi_fondo72_abono,
                                  saldo,
                                  tpo_movto
                                 )
                           VALUES
                                 (
                                 v_id_solicitud,
                                  p_folio,
                                  tmp_ret_det_rfc_saci,
                                  v_id_afi_fondo72_cargo,
                                  tmp_ret_det_rfc_nuevo,
                                  v_id_afi_fondo72_abono,
                                  v_saldo,
                                  tmp_ret_det_tpo_movto
                                 );
                     LET v_estado_solicitud = 15;      -- Lista para preliquidación
                     LET v_cod_rechazo      =  0;      -- Sin codigo de rechazo
                     --- Marca las cuentas involucradas
                     --marca la cuenta de cargo, solo se marcará la cuenta de cargo
                     EXECUTE FUNCTION fn_marca_cuenta(
                             v_id_derechohabiente_cargo
                            ,v_marca_uc  -- marca de amortizaciones excedentes
                            ,v_id_solicitud
                            ,p_folio     -- folio se asignara en la solicitd
                            ,0     -- estado marca
                            ,0     -- codigo de rechazo
                            ,0     -- marca de la causa
                            ,NULL  -- fecha de la causa
                            ,USER  --usuario
                            ,p_proceso_cod) --proceso_cod
                        INTO v_i_estado_marca; 

                     IF v_i_estado_marca <> 0 THEN
 
                         LET v_estado_solicitud = 100;   -- Solicitud rechazada por registro existente
                         LET v_cod_rechazo      = 103;   -- Problemas al marcar
                     END IF
                 ELSE 
                     LET v_estado_solicitud = 100;     -- Rechazada
                     IF v_ocurrencias > 1 THEN
                         LET v_cod_rechazo      = 52;  -- mas de un registro para el rfc unificador 
                     ELSE
                         LET v_cod_rechazo      = 7;   -- no existe rfc unificador
                     END IF
                 END IF
             ELSE
                 LET v_estado_solicitud = 100; -- Rechazada
                 IF v_ocurrencias > 1 THEN
                     LET v_cod_rechazo      = 52;  --mas de un registro para el rfc unificado
                 ELSE
                     LET v_cod_rechazo      = 7;   -- no existe rfc unificado
                 END IF
             END IF
      END IF
      -- FA Separación de Cuentas y A Alta de Cuentas
	  IF ( tmp_ret_det_tpo_movto = "FA" ) THEN
             --- Busca RFC anterior (SACI) en afi_fondo72 
             SELECT COUNT(id_afi_fondo72)
             INTO   v_ocurrencias
             FROM   afi_fondo72
             WHERE  rfc = tmp_ret_det_rfc_saci;
             --- si lo encuentra se obtiene el NSS y el Nombre, en caso contrario lo rechaza
             IF v_ocurrencias = 1 THEN
                 --- Busca su correspondiente registro A
                 SELECT id_afi_fondo72, id_derechohabiente
                   INTO v_id_afi_fondo72_cargo, v_id_derechohabiente_cargo
                   FROM afi_fondo72
                  WHERE rfc = tmp_ret_det_rfc_saci;
                 IF v_id_derechohabiente_cargo IS NULL THEN 
                     SELECT seq_derechohabiente.NEXTVAL
                       INTO v_id_derechohabiente_cargo
                       FROM systables
                      WHERE tabid = 1;
                     INSERT INTO afi_fondo72_d
                                 (nss,
                                  rfc,
                                  id_derechohabiente
                                 )
                          VALUES
                                 (
                                  v_nss,
                                  tmp_ret_det_rfc_saci,
                                  v_id_derechohabiente_cargo
                                 );
                     UPDATE afi_fondo72
                        SET id_derechohabiente = v_id_derechohabiente_cargo
                      WHERE id_afi_fondo72 = v_id_afi_fondo72_cargo;
                 END IF                      
                 SELECT COUNT(*)
                   INTO v_ocurrencias
                   FROM safre_tmp:tmp_ret_det_acl_fa 
                  WHERE SUBSTR(rfc_nuevo,1,10) = SUBSTR(tmp_ret_det_rfc_saci,1,10)
                    AND TRIM(tpo_movto) = "A";
                 IF v_ocurrencias = 1 THEN
                     --- Busca RFC nuevo (Unificador) en la temporal 
                     SELECT rfc_nuevo
                       INTO v_rfc_nuevo
                       FROM safre_tmp:tmp_ret_det_acl_fa 
                      WHERE SUBSTR(rfc_nuevo,1,10) = SUBSTR(tmp_ret_det_rfc_saci,1,10)
                        AND TRIM(tpo_movto) = "A";
                     -- Se busca que no exista en la base de datos del fondo de ahorro
                     SELECT COUNT(*)
                       INTO v_ocurrencias
                       FROM afi_fondo72
                      WHERE rfc = v_rfc_nuevo;
                      
                     IF v_ocurrencias = 0 THEN
                         SELECT nss, nombre
                           INTO v_nss, v_nombre
                           FROM afi_fondo72
                          WHERE rfc = tmp_ret_det_rfc_saci;
                          SELECT seq_afi_fondo72.NEXTVAL
                            INTO v_id_afi_fondo72_abono
                            FROM systables
                           WHERE tabid = 1;
                         --- Se obtiene el id_derechohabiente para guardar la integridad y poder marcar el registro
                         SELECT seq_derechohabiente.NEXTVAL
                           INTO v_id_derechohabiente_abono
                           FROM systables
                          WHERE tabid = 1;
                         -- Se busca el saldo, solo como referencia
                         LET v_saldo = 0;
                         SELECT SUM(NVL(importe,0))
                         INTO   v_saldo
                         FROM   afi_fondo72 afi
                                LEFT OUTER JOIN cta_fondo72 cta
                                             ON afi.id_afi_fondo72 =  cta.id_afi_fondo72
                                            AND cta.subcuenta      =  v_subcuenta
                                            AND (cta.movimiento    <> 422 AND  -- 422 CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                                                 cta.movimiento    <> 601)     -- 601 ABONO, RETIRO FONDO 72 NO PAGADO, TANTO 
                         WHERE afi.id_afi_fondo72 = v_id_afi_fondo72_cargo;                            
                         INSERT INTO afi_fondo72_d
                                     (nss,
                                      rfc,
                                      id_derechohabiente
                                     )
                              VALUES
                                     (
                                      v_nss,
                                      tmp_ret_det_rfc_nuevo,
                                      v_id_derechohabiente_abono
                                     );
                         INSERT INTO afi_fondo72 
                                   (
                                    id_afi_fondo72,
                                    rfc,
                                    nss,
                                    nombre,
                                    id_derechohabiente,
                                    f_apertura,
                                    ind_estado_cuenta
                                   )
                            VALUES (
                                    v_id_afi_fondo72_abono,
                                    tmp_ret_det_rfc_nuevo,
                                    v_nss,
                                    v_nombre,
                                    v_id_derechohabiente_abono,
                                    today,
                                    0 );
                                  
                         INSERT INTO ret_aclara_accion_fa
                                     (
                                      id_solicitud,
                                      folio,
                                      rfc_cargo,
                                      id_afi_fondo72_cargo,
                                      rfc_abono,
                                      id_afi_fondo72_abono,
                                      saldo,
                                      tpo_movto
                                     )
                               VALUES
                                     (
                                      v_id_solicitud,
                                      p_folio,
                                      tmp_ret_det_rfc_saci,
                                      v_id_afi_fondo72_cargo,
                                      v_rfc_nuevo,
                                      v_id_afi_fondo72_abono,
                                      v_saldo,
                                      tmp_ret_det_tpo_movto
                                     );
                         LET v_estado_solicitud = 15;    -- Lista para preliquidación
                         LET v_cod_rechazo      =  0;    -- Sin codigo de rechazo
                         --- Marca las cuentas involucradas
                         --marca la cuenta de cargo
                         EXECUTE FUNCTION fn_marca_cuenta(
                                 v_id_derechohabiente_cargo
                                ,v_marca_fa  -- marca de amortizaciones excedentes
                                ,v_id_solicitud
                                ,p_folio     -- folio se asignara en la solicitd
                                ,0     -- estado marca
                                ,0     -- codigo de rechazo
                                ,0     -- marca de la causa
                                ,NULL  -- fecha de la causa
                                ,USER  --usuario
                                ,p_proceso_cod) --proceso_cod
                            INTO v_i_estado_marca; 
                         IF v_i_estado_marca <> 0 THEN
                             LET v_estado_solicitud = 100;   -- Solicitud rechazada por registro existente
                             LET v_cod_rechazo      = 103;   -- Problemas al marcar
                         END IF
                     ELSE 
                         LET v_estado_solicitud = 100;   -- Solicitud rechazada por registro existente
                         LET v_cod_rechazo      = 850;   --  Solicitud Existente
                     END IF
                 ELSE
                     LET v_estado_solicitud = 100; -- Rechazada
                     IF v_ocurrencias > 1 THEN
                         LET v_cod_rechazo      = 52;  -- mas de un registro para el FA Asociado
                     ELSE
                         LET v_cod_rechazo      = 7;   -- no existe el correspondiente registro A
                     END IF
                 END IF
             ELSE
                 LET v_estado_solicitud = 100; -- Rechazada
                 IF v_ocurrencias > 1 THEN
                     LET v_cod_rechazo      = 52;  -- mas de un registro para el RFC dado
                 ELSE
                     LET v_cod_rechazo      = 7;   -- no existe el RFC dado
                 END IF
             END IF
      END IF
	  IF ( TRIM(tmp_ret_det_tpo_movto) = "A" ) THEN
          --- Busca RFC Nuevo/Destino en afi_fondo72 
              SELECT id_solicitud
                INTO v_id_solicitud_fa
                FROM ret_aclara_accion_fa
               WHERE rfc_abono = tmp_ret_det_rfc_nuevo;
              IF v_id_solicitud_fa IS NOT NULL THEN
                  IF tmp_ret_det_imp_movto > 0 THEN 
                      UPDATE ret_aclara_accion_fa
                         SET id_solicitud_abono = v_id_solicitud
                       WHERE id_solicitud = v_id_solicitud_fa;
                       
                      SELECT estado_solicitud, cod_rechazo
                        INTO v_estado_solicitud, v_cod_rechazo
                        FROM ret_det_aclara_fondo_ahorro
                       WHERE id_solicitud = v_id_solicitud_fa;
                  ELSE 
                      --- Desmarca las cuenta por problemas con el importe
                      SELECT id_derechohabiente
                        INTO v_id_derechohabiente_cargo
                        FROM afi_fondo72
                       WHERE id_afi_fondo72 IN (SELECT id_afi_fondo72_cargo
                                                  FROM ret_aclara_accion_fa
                                                 WHERE id_solicitud = v_id_solicitud_fa);
                      SELECT id_derechohabiente
                        INTO v_id_derechohabiente_abono
                        FROM afi_fondo72
                       WHERE id_afi_fondo72 IN (SELECT id_afi_fondo72_abono
                                                  FROM ret_aclara_accion_fa
                                                 WHERE id_solicitud = v_id_solicitud_fa);
                         --- Desmarca porque no cumple con las validaciones
                         -- Desmarca la cuenta de cargo 
                      EXECUTE FUNCTION fn_desmarca_cuenta(
                          v_id_derechohabiente_cargo
                          ,v_marca_fa
                          ,v_id_solicitud_fa
                          ,0
                          ,0
                          ,p_usuario_cod
                          ,p_proceso_cod) INTO v_i_estado_marca;
 
                      LET v_estado_solicitud     = 100;     -- Rechazado
                      LET v_cod_rechazo          = 851;     -- El Importe debe ser mayor a cero
                      UPDATE ret_det_aclara_fondo_ahorro
                         SET estado_solicitud = v_estado_solicitud,
                             cod_rechazo      = v_cod_rechazo
                       WHERE id_solicitud = v_id_solicitud_fa;
                  END IF 
              ELSE 
                  LET v_estado_solicitud = 100;     -- Rechazo
                  LET v_cod_rechazo      = 852;     -- No se encontro correspondiente FA
              END IF
      END IF
      
      --- se inserta el registro en la tabla ret_det_aclara_fondo_ahorro

      LET ret_det_aclara_folio            = p_folio;
      LET ret_det_aclara_rfc_saci         = tmp_ret_det_rfc_saci; 
      LET ret_det_aclara_fiolio_aclara    = tmp_ret_det_folio_aclara;
      LET v_resultado = 1;
      CALL fn_ret_valida_fecha(SUBSTR(tmp_ret_det_f_aclara,7,2),SUBSTR(tmp_ret_det_f_aclara,5,2),SUBSTR(tmp_ret_det_f_aclara,1,4)) RETURNING v_resultado;
      IF v_resultado = 1 THEN
          LET ret_det_aclara_f_aclara = MDY("01","01","0001");
      ELSE 
          LET ret_det_aclara_f_aclara = MDY(SUBSTR(tmp_ret_det_f_aclara,5,2) , SUBSTR(tmp_ret_det_f_aclara,7,2) , SUBSTR(tmp_ret_det_f_aclara,1,4));
      END IF
      LET ret_det_aclara_rfc_nuevo        = tmp_ret_det_rfc_nuevo;
      LET ret_det_aclara_nss_aclarado     = tmp_ret_det_nss_aclarado;
      LET ret_det_aclara_nombre_aclarado  = tmp_ret_det_nombre_aclarado;
      LET ret_det_aclara_tpo_movto        = tmp_ret_det_tpo_movto;
      LET ret_det_aclara_imp_movto        = tmp_ret_det_imp_movto;
      LET ret_det_aclara_estatus          = tmp_ret_det_estatus;
      LET ret_det_aclara_estado_solicitud = v_estado_solicitud;
      LET ret_det_aclara_cod_rechazo      = v_cod_rechazo;

      INSERT INTO ret_det_aclara_fondo_ahorro
                  (id_solicitud,
                   folio,
                   rfc_saci,
                   folio_aclara,
                   f_aclara,
                   rfc_nuevo,
                   nss_aclarado,
                   nombre_aclarado,
                   tpo_movto,
                   imp_movto,
                   estatus,
                   estado_solicitud,
                   cod_rechazo)
            VALUES (v_id_solicitud,
                    ret_det_aclara_folio,
                    ret_det_aclara_rfc_saci,
                    ret_det_aclara_fiolio_aclara,
                    ret_det_aclara_f_aclara,
                    ret_det_aclara_rfc_nuevo,
                    ret_det_aclara_nss_aclarado,
                    ret_det_aclara_nombre_aclarado,
                    ret_det_aclara_tpo_movto,
                    ret_det_aclara_imp_movto,
                    ret_det_aclara_estatus,
                    ret_det_aclara_estado_solicitud,
                    ret_det_aclara_cod_rechazo);
        LET v_reg_det_insertados = v_reg_det_insertados + 1;
 END FOREACH;

 --END IF 

   UPDATE ret_cza_aclara_fondo_ahorro
      SET tot_registros =  v_reg_det_insertados
    WHERE folio = p_folio;
   
   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_det_aclara_fondo_ahorro;
   UPDATE STATISTICS FOR TABLE ret_aclara_accion_fa;
   UPDATE STATISTICS FOR TABLE ret_cza_aclara_fondo_ahorro;

   -- si no hubo error
   IF ( v_si_resultado = 0 ) THEN 
      -- si se insertaron registro
      IF ( v_reg_det_insert_15 > 0 ) THEN  
         LET isam_err =  v_reg_det_insertados;
      END IF
   END IF 

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_rfc_saci;
END FUNCTION;


