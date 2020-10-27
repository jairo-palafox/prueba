






CREATE FUNCTION "safreviv".fn_ret_integra_rest_pago_vencido_fondo_ahorro(   p_usuario_cod    CHAR(20)
                                              , p_folio          DECIMAL(9,0)
                                              , p_nombre_archivo VARCHAR(40)
                                              , p_pid            DECIMAL(9,0)
                                              , p_proceso_cod    SMALLINT
                                              )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)
   
-- campos de la tabla de detalle de retiros de fondo ahorro (sin filler)
DEFINE tmp_ret_rest_contenido                     CHAR(195)  ;

-- detalle de la tabla historica/integrada de retiros de fondo ahorro
-- ret_fondo_ahorro
DEFINE ret_rest_fondo_ahorro_id_solicitud         DECIMAL(10,0);
DEFINE ret_rest_fondo_ahorro_id_afi_fondo72       DECIMAL(10,0);
DEFINE ret_rest_fondo_ahorro_id_sol_cargo         DECIMAL(10,0);
DEFINE ret_rest_fondo_ahorro_f_solicitud          DATE         ;
DEFINE ret_rest_fondo_ahorro_folio                DECIMAL(10,0);
DEFINE ret_rest_fondo_ahorro_estado_solicitud     SMALLINT     ;
DEFINE ret_rest_fondo_ahorro_cod_rechazo          SMALLINT     ;
DEFINE ret_rest_fondo_ahorro_codigo               CHAR(1)      ;
DEFINE ret_rest_fondo_ahorro_referencia           CHAR(13)     ;
DEFINE ret_rest_fondo_ahorro_indicador            CHAR(1)      ;
DEFINE ret_rest_fondo_ahorro_f_pago_vencimiento   DATE         ;

-- variables de soporte al proceso
DEFINE v_id_afi_fondo72                      DECIMAL(9,0);
DEFINE vc_importe                            CHAR(20);
DEFINE vc_imp_correcto                       CHAR(20);
DEFINE v_i                                   SMALLINT;
DEFINE v_caracter                            CHAR(1);
DEFINE v_no_considerado                      INTEGER;
DEFINE v_cant_ocurrencias                    SMALLINT;
DEFINE v_cve_referencia_busqueda             CHAR(14);


--variable de solicitd   
DEFINE v_id_solicitud                        DECIMAL(9,0);
DEFINE v_id_sol_cargo                        DECIMAL(10,0);

-- =============================================================================
-- para rechazos                               
DEFINE v_b_rechazo_detalle                     SMALLINT;
-- RECUPERADOS                                    
                                               
DEFINE v_numero_registros                      DECIMAL(9,0);
                                               
-- arreglo de codigos de rechazo               
DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 
DEFINE v_subcuenta                             SMALLINT;
                                               
-- conteo de rechazos e inserciones            
DEFINE v_reg_det_insertados                    DECIMAL(9,0); -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    DECIMAL(9,0); -- total de registros de detalle rechazados

-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado               SMALLINT;
DEFINE v_error_det_tpo_registro_invalido           SMALLINT;
DEFINE v_error_det_mas_de_una_ocurrencia           SMALLINT;

DEFINE v_marca_fondo_ahorro                      INTEGER; -- 802 de acuerdo a catalogo

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      trace "Registro erroneo >" || tmp_ret_rest_contenido || "<";
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_rest_contenido;
   END EXCEPTION
     
   SET DEBUG FILE TO "/safreviv_int/BD/fn_ret_integra_rest_fondo_ahorro.trace";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados              = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados              = 0; -- total de registros de detalle rechazados
   LET v_subcuenta                       = 40;
   
   -- se asume que el proceso termina bien
   LET v_si_resultado  = 0;
   LET isam_err        = 0;
   LET v_c_msj         = 'El proceso finalizó exitosamente.';

   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado                = 49; -- NSS NO ENCONTRADO
   LET v_error_det_tpo_registro_invalido            = 6;
   LET v_error_det_mas_de_una_ocurrencia            = 100;  --- Mas de un registro
   LET v_cant_ocurrencias                           = 0;
   
   -- se inician las variables para marca
   LET v_marca_fondo_ahorro = 802; -- marca para fondo ahorro
  
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
   
   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;    

   -- se inicializan las variables 
   LET ret_rest_fondo_ahorro_id_solicitud       = NULL;
   LET ret_rest_fondo_ahorro_id_afi_fondo72     = NULL;
   LET ret_rest_fondo_ahorro_id_sol_cargo       = NULL;
   LET ret_rest_fondo_ahorro_f_solicitud        = NULL;
   LET ret_rest_fondo_ahorro_folio              = NULL;
   LET ret_rest_fondo_ahorro_estado_solicitud   = NULL;
   LET ret_rest_fondo_ahorro_cod_rechazo        = NULL;   
   LET ret_rest_fondo_ahorro_codigo             = "";
   LET ret_rest_fondo_ahorro_referencia         = "";
   LET ret_rest_fondo_ahorro_indicador          = "";
   LET ret_rest_fondo_ahorro_f_pago_vencimiento = NULL;
   LET vc_imp_correcto                          = "";
   LET vc_importe                               = "";
   LET v_caracter                               = "";
   LET v_i                                      = 0;
   LET v_no_considerado                         = 0;
   LET v_id_sol_cargo                           = 0;
   LET v_cve_referencia_busqueda                = '';

   -- se obtienen los datos del detalle
   --- Se crea la tabla temporal para buscar discriminar las referencias con un guion
   DROP TABLE IF EXISTS tmp_ret_fondo_ahorro;
   SELECT id_solicitud, cve_referencia, estado_solicitud, folio
   FROM   ret_fondo_ahorro
   WHERE  estado_solicitud = 60
   AND    cve_referencia NOT LIKE '%-%'
   INTO TEMP tmp_ret_fondo_ahorro;
   CREATE INDEX idx_tmp_ret_fondo_ahorro ON tmp_ret_fondo_ahorro(cve_referencia);
   CREATE INDEX idx_tmp_ret_fondo_ahorro_f_s ON tmp_ret_fondo_ahorro(folio, id_solicitud);

   FOREACH
   SELECT
       contenido
   INTO
       tmp_ret_rest_contenido
   FROM safre_tmp:tmp_ret_rest_fondo_ahorro  -- tabla temporal con el detalle
      IF LENGTH(TRIM(tmp_ret_rest_contenido)) > 0 THEN
         LET ret_rest_fondo_ahorro_estado_solicitud   = "15" ;
         LET ret_rest_fondo_ahorro_cod_rechazo        = "0"  ;
         -- se pasa la información obtenida a las variables
         LET ret_rest_fondo_ahorro_codigo   = tmp_ret_rest_contenido[1,1];
--         IF (ret_rest_fondo_ahorro_codigo <> "5" )  THEN 
--            LET v_no_considerado = v_no_considerado * 1;
--         ELSE 
            trace "antes de la asignacion";
            LET ret_rest_fondo_ahorro_referencia         = tmp_ret_rest_contenido[1,13];
            LET ret_rest_fondo_ahorro_indicador          = tmp_ret_rest_contenido[14,14];
            LET ret_rest_fondo_ahorro_f_pago_vencimiento = MDY(tmp_ret_rest_contenido[19,20],tmp_ret_rest_contenido[21,22],tmp_ret_rest_contenido[15,18]);
            
            -- se asume que no hay rechazos en el detalle del archivo

            LET v_b_rechazo_detalle = 0;
            -- ==========================================================================
            -- para el id solicitud se obtiene de la secuencia
            LET v_id_solicitud = 0;
            LET v_id_afi_fondo72 = NULL;

            --trace "id der luego de buscarlo: " || v_id_derechohabiente;
            -- se obtiene el id_solicitud
            SELECT seq_ret_rest_fondo_ahorro.NEXTVAL
            INTO   v_id_solicitud
            FROM   systables
            WHERE  tabid = 1;
            --trace "El num_documento >" || ret_rest_fondo_ahorro_num_documento[1,2] || "<";
            IF (ret_rest_fondo_ahorro_indicador = "V") THEN 
               -- se busca el registro original para obtener el id_afi_fondo72
               SELECT COUNT(*)
               INTO   v_cant_ocurrencias
               FROM   tmp_ret_fondo_ahorro b
               WHERE  b.cve_referencia = ret_rest_fondo_ahorro_referencia
               AND    b.estado_solicitud = 60;
               IF v_cant_ocurrencias = 0 THEN -- NO EXISTEN REGISTROS 
                  LET ret_rest_fondo_ahorro_estado_solicitud = 100;
                  LET ret_rest_fondo_ahorro_cod_rechazo = v_error_det_nss_no_encontrado;
               ELSE 
                  IF v_cant_ocurrencias = 1 THEN 
                     SELECT DISTINCT a.id_afi_fondo72, b.id_solicitud
                     INTO   ret_rest_fondo_ahorro_id_afi_fondo72, v_id_sol_cargo
                     FROM   ret_preliquida72 a,
                            tmp_ret_fondo_ahorro b
                     WHERE  b.cve_referencia = ret_rest_fondo_ahorro_referencia 
                     AND    b.estado_solicitud = 60
                     AND    a.id_referencia = b.id_solicitud
                     AND    a.folio_liquida = b.folio;
                  ELSE
                     LET ret_rest_fondo_ahorro_estado_solicitud = 100;
                     LET ret_rest_fondo_ahorro_cod_rechazo = v_error_det_mas_de_una_ocurrencia;
                  END IF
               END IF  
            ELSE -- el caso es diferente a vencido
               LET ret_rest_fondo_ahorro_estado_solicitud = 10;
               LET ret_rest_fondo_ahorro_cod_rechazo = 0; 
            END IF 
            LET ret_rest_fondo_ahorro_id_solicitud       = v_id_solicitud                ;     
            LET ret_rest_fondo_ahorro_f_solicitud        = TODAY;
            LET ret_rest_fondo_ahorro_folio              = p_folio;
            trace "el estado   " || ret_rest_fondo_ahorro_estado_solicitud;
            trace "el cod rech " || ret_rest_fondo_ahorro_cod_rechazo;
            trace "la cve ref  " || ret_rest_fondo_ahorro_referencia;
            INSERT INTO ret_rest_fondo_ahorro (
                        id_solicitud       ,
                        id_afi_fondo72     ,
                        id_sol_cargo       ,
                        f_solicitud        ,
                        folio              ,
                        estado_solicitud   ,
                        cod_rechazo        ,
                        cve_referencia     ,
                        indicador          ,
                        f_pago_vencimiento )
                  VALUES (
                        ret_rest_fondo_ahorro_id_solicitud      ,
                        ret_rest_fondo_ahorro_id_afi_fondo72    ,
                        v_id_sol_cargo                          ,
                        ret_rest_fondo_ahorro_f_solicitud       ,
                        ret_rest_fondo_ahorro_folio             ,
                        ret_rest_fondo_ahorro_estado_solicitud  ,
                        ret_rest_fondo_ahorro_cod_rechazo       ,
                        ret_rest_fondo_ahorro_referencia        ,
                        ret_rest_fondo_ahorro_indicador         ,
                        ret_rest_fondo_ahorro_f_pago_vencimiento);
            LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados
         --END IF
      END IF 
   END FOREACH;
   
   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_rest_fondo_ahorro;

   -- si no hubo error
   IF ( v_si_resultado = 0 ) THEN 
      -- si se insertaron registro
      IF ( v_reg_det_insertados > 0 ) THEN  
         LET isam_err =  v_reg_det_insertados;
      END IF
   END IF 

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_rest_contenido;
END FUNCTION;


