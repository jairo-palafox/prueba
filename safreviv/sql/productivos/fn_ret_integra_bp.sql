






CREATE FUNCTION "safreviv".fn_ret_integra_bp(p_usuario_cod CHAR(20), 
                                  p_folio DECIMAL(9,0),
                                  p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT,
                                  P_proceso_cod_origen SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250)

-- detalle de la tabla temporal

DEFINE tmp_ret_bp_nss                  CHAR(11)      ;
DEFINE tmp_ret_bp_tpo_credito          CHAR(10)      ;
DEFINE tmp_ret_bp_estatus_credito      CHAR(9)       ;
DEFINE tmp_ret_bp_marcado_procesar     CHAR(19)      ;
DEFINE tmp_ret_bp_desmarcado_procesar  CHAR(22)      ;

-- tablas destino
-- ret_detalle_bp
DEFINE ret_detalle_bp_folio                DECIMAL(9,0);
DEFINE ret_detalle_bp_folio_proceso_origen DECIMAL(9,0);
DEFINE ret_detalle_bp_nss                  CHAR(11)    ;
DEFINE ret_detalle_bp_tpo_credito          CHAR(3)     ;
DEFINE ret_detalle_bp_estatus_credito      CHAR(3)     ;
DEFINE ret_detalle_bp_marcado_procesar     CHAR(1)     ;
DEFINE ret_detalle_bp_desmarcado_procesar  CHAR(1)     ;

-- constantes nombre de tablas temporales
DEFINE v_c_disp_bp                         CHAR(20);
DEFINE v_c_pmg_bp                          CHAR(20);
DEFINE v_c_tran_bp                         CHAR(20);
DEFINE v_c_tipn_bp                         CHAR(20);
DEFINE v_c_tabla_tmp                       CHAR(20);
-- =============================================================================
DEFINE v_resultado_consulta                 SMALLINT;
DEFINE v_resultado                          SMALLINT;

-- id matriz derecho
DEFINE v_id_ret_matriz_derecho              SMALLINT; -- id de la matriz de derecho de retiros

-- Variables de trabajo

DEFINE v_id_solicitud                           DECIMAL(9,0);
DEFINE v_id_derechohabiente                     DECIMAL(9,0);
DEFINE v_id_decreto                             DECIMAL(9,0);
DEFINE v_i_folio_original                       DECIMAL(9,0);
DEFINE v_i_cant_procesos_batch                  SMALLINT;
DEFINE cust_qry                                 CHAR(550);
DEFINE v_sql                                    VARCHAR(250);
DEFINE v_accion_92                              CHAR(1);
DEFINE v_accion_97                              CHAR(1);
DEFINE v_nss_busqueda                           CHAR(11);
DEFINE v_nss_paso                               CHAR(15);
DEFINE v_cant_nss                               SMALLINT;
DEFINE v_cod_rechazo                            CHAR(3);
DEFINE v_d_aivs_viv97                           DECIMAL(22,6);


-- conteo de rechazos e inserciones
DEFINE v_reg_cza_insertados                    SMALLINT; -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados                    SMALLINT; -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados                    SMALLINT; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    SMALLINT; -- total de registros de detalle rechazados
 
-- codigos de error
DEFINE v_error_estado_solicitud                SMALLINT;
DEFINE v_error_con_credito_vigente             SMALLINT; -- error 
DEFINE v_error_con_saldo_43bis                 SMALLINT; -- error 


-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
 
-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);
DEFINE v_resultado_funcion                       SMALLINT;
DEFINE v_isam_err                                INTEGER;
DEFINE v_mens_fnc                                VARCHAR(250);
DEFINE v_digito                                  SMALLINT;


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   -- se establece el archivo para el debug
   SET DEBUG FILE TO "/safreviv_int/BD/debug_bp.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados  = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados  = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados

   LET v_c_disp_bp           = "tmp_ret_disp_bp_val";
   LET v_c_pmg_bp            = "tmp_ret_pmg_bp_val";
   LET v_c_tran_bp           = "tmp_ret_tran_bp_val";
   LET v_c_tipn_bp           = "tmp_ret_tupn_bp_val";  
   LET v_c_tabla_tmp         = "";
   LET v_cod_rechazo         = "0";
   LET v_d_aivs_viv97        = 0;
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';
  

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;

 
   -- se inician los codigos de error en detalle
   LET v_error_estado_solicitud                = 100;
   LET v_error_con_credito_vigente             = 100; -- error 
   LET v_error_con_saldo_43bis                 = 961; -- error 

   -- Se busca el folio del proceso original
   SELECT COUNT(*)
     INTO v_i_cant_procesos_batch
     FROM bat_ctr_proceso 
    WHERE proceso_cod = p_proceso_cod_origen
      AND estado_cod = 2 
      AND fecha_fin IS NULL;
   --trace "Folios encontrados :" || v_i_cant_procesos_batch;
   IF v_i_cant_procesos_batch <> 1 THEN
       LET v_si_resultado = v_i_cant_procesos_batch;
       LET isam_err       = -1000;
       LET err_txt        = "Error existen más de un folio origen para el proceso, o no existe uno previamente cargado";
       RETURN v_si_resultado, isam_err, err_txt;
   ELSE
       SELECT folio
         INTO v_i_folio_original
         FROM bat_ctr_proceso 
        WHERE proceso_cod = p_proceso_cod_origen
          AND estado_cod = 2 
          AND fecha_fin IS NULL;
       --trace "Folios para relación :" || v_i_folio_original;   
   END IF
   
   IF p_proceso_cod = 1553 THEN
       LET v_c_tabla_tmp = v_c_disp_bp;
   END IF
   IF p_proceso_cod = 1555 THEN
       LET v_c_tabla_tmp = v_c_pmg_bp;
   END IF
   IF p_proceso_cod = 1557 THEN
       LET v_c_tabla_tmp = v_c_tran_bp;
   END IF
   IF p_proceso_cod = 1559 THEN
       LET v_c_tabla_tmp = v_c_tipn_bp;
   END IF

   LET cust_qry = " SELECT  TRIM(nss), TRIM(tpo_credito), TRIM(estatus_credito), " || 
                  " TRIM(marcado_procesar), TRIM(desmarcado_procesar) " ||
                  " from safre_tmp:" || v_c_tabla_tmp ||
                  " WHERE  TRIM(tpo_credito) IS NOT NULL AND TRIM(estatus_credito) IS NOT NULL " ||
                  " AND TRIM(nss) IS NOT NULL"; 
   trace "El query :" || cust_qry;
   PREPARE stmt_tmp FROM cust_qry;
   DECLARE cust_cur cursor FOR stmt_tmp;
   OPEN cust_cur;
       WHILE (1 = 1)
           FETCH cust_cur INTO tmp_ret_bp_nss,
                               tmp_ret_bp_tpo_credito,
                               tmp_ret_bp_estatus_credito,
                               tmp_ret_bp_marcado_procesar,
                               tmp_ret_bp_desmarcado_procesar;
           IF (SQLCODE != 100) THEN
               LET v_cod_rechazo = "0";
               
               IF tmp_ret_bp_tpo_credito <> "6"   AND    --- Estos tipos de credito no se deben considerar
                  tmp_ret_bp_tpo_credito <> "7"   AND
                  tmp_ret_bp_tpo_credito <> "8"   AND
                  tmp_ret_bp_tpo_credito <> "9"   AND
                  tmp_ret_bp_tpo_credito <> "13"  AND
                  tmp_ret_bp_tpo_credito <> "103" AND
                  tmp_ret_bp_tpo_credito <> "202" THEN
                  -- Busca accion en la matriz BP
                  LET v_accion_92 = "0";
                  LET v_accion_97 = "0";
                  SELECT accion_92, accion_97
                    INTO v_accion_92, v_accion_97
                    FROM ret_matriz_bp
                   WHERE tpo_credito     = tmp_ret_bp_tpo_credito
                     AND estatus_credito = tmp_ret_bp_estatus_credito;
                  trace "Acciones, viv 92:" || v_accion_92 || " viv 97 " || v_accion_97;   
                  IF v_accion_92 = "1" OR v_accion_97 = "1" OR
                     v_accion_92 = "2" OR v_accion_97 = "2" THEN -- solo se consideran los posibles rechazos
                      IF LENGTH(TRIM(tmp_ret_bp_nss)) < 10 THEN
                          LET v_nss_paso = "0" ||  TRIM(tmp_ret_bp_nss);
                      ELSE
                          LET v_nss_paso = TRIM(tmp_ret_bp_nss);
                      END IF
                      -- Se obtiene el digito verificador del nss para buscar por la llave completa
                      EXECUTE FUNCTION fn_calcula_digito_verificador_nss(v_nss_paso)
                                  INTO v_resultado_funcion, v_isam_err, v_mens_fnc, v_digito;
                      IF v_digito IS NOT NULL AND v_digito <> 100 THEN 
                          LET v_nss_busqueda = TRIM(v_nss_paso) || v_digito;
                      ELSE
                          LET v_nss_busqueda = TRIM(v_nss_paso) || "0";
                      END IF
                      --LET v_nss_busqueda = "*" ||  TRIM(tmp_ret_bp_nss) || "*";
                      trace "Nss a buscar :" || v_nss_busqueda;
                      --trace "El proceso es " || p_proceso_cod_origen;
                      IF p_proceso_cod_origen = 1505 THEN 
                          IF v_accion_92 = "1" OR v_accion_92 = "2" THEN
                              SELECT COUNT(*) 
                                INTO v_cant_nss
                                FROM afi_decreto 
                               WHERE nss = v_nss_busqueda;
                              IF v_cant_nss = 1 THEN
                                  SELECT id_decreto
                                    INTO v_id_decreto
                                    FROM afi_decreto 
                                   WHERE nss = v_nss_busqueda;
                                  SELECT id_solicitud
                                    INTO v_id_solicitud
                                    FROM ret_tipo_n
                                   WHERE id_decreto = v_id_decreto
                                     AND folio = folio_proceso_origen
                                     AND cod_rechazo = 0;
                                  IF v_id_solicitud > 0 THEN
                                      IF v_accion_92 = "1" THEN
                                         LET v_cod_rechazo = v_error_con_credito_vigente;
                                      ELSE 
                                         LET v_cod_rechazo = v_error_con_saldo_43bis;
                                      END IF
                                      UPDATE ret_tipo_n
                                         SET estado_solicitud = v_error_estado_solicitud,
                                             cod_rechazo      = v_cod_rechazo
                                       WHERE id_solicitud     = v_id_solicitud;
                                      -- se desmarca la solicitud
                                      EXECUTE FUNCTION fn_desmarca_cuenta_decreto(
                                                  v_id_decreto,
                                                  "804",  -- marca de tipo N
                                                  v_id_solicitud ,  -- identificador de registro de archivo o lote
                                                  "40",   -- estado marca / rechazo validacion
                                                  "804",   -- marca de la causa / rechazo por validacion
                                                  TRIM(p_usuario_cod), 
                                                  p_proceso_cod_origen ) INTO v_resultado;
--                                      LET v_sql = "\nEXECUTE FUNCTION fn_desmarca_cuenta_decreto(" ||
--                                                  "\n",v_id_decreto, "," ||
--                                                  "\n 804," || -- marca de tipo N
--                                                  "\n" || v_id_solicitud ||"," || -- identificador de registro de archivo o lote
--                                                  "\n 40," ||  -- estado marca / rechazo validacion
--                                                  "\n 804,"  || -- marca de la causa / rechazo por validacion
--                                                  "\n'" || TRIM(p_usuario_cod) || "'," ||
--                                                  "\n" || p_proceso_cod_origen || ")";
--
--                                      -- se prepara y ejecuta la desmarcar
--                                      PREPARE sid_desmarca FROM v_sql
--                                      EXECUTE sid_desmarca INTO v_resultado    ;                           
                                  END IF
                              END IF
                          END IF 
                      ELSE 
                          SELECT COUNT(*) 
                            INTO v_cant_nss
                            FROM afi_derechohabiente 
                           WHERE nss = v_nss_busqueda;
                          --trace "Busca en afi_derechohabiente" || v_nss_busqueda || " y encuentra " || v_cant_nss;
                          IF v_cant_nss = 1 THEN
                              --trace "Se encontro en afi_derechohabiente " || v_nss_busqueda;
                              SELECT id_derechohabiente
                                INTO v_id_derechohabiente
                                FROM afi_derechohabiente 
                               WHERE nss = v_nss_busqueda;
                              IF p_proceso_cod_origen = 1504 THEN 
                                  IF v_accion_97 = "1" OR v_accion_97 = "2" THEN
                                      LET v_id_solicitud = 0;
                                      SELECT id_solicitud
                                        INTO v_id_solicitud
                                        FROM ret_transferencia
                                       WHERE id_derechohabiente = v_id_derechohabiente
                                         AND folio = v_i_folio_original
                                         AND cod_rechazo = 0;
                                      IF v_id_solicitud > 0 THEN
                                          IF v_accion_97 = "1" THEN
                                             LET v_cod_rechazo = v_error_con_credito_vigente;
                                          ELSE 
                                             LET v_cod_rechazo = v_error_con_saldo_43bis;
                                          END IF
                                          UPDATE ret_transferencia
                                             SET estado_solicitud = v_error_estado_solicitud,
                                                 cod_rechazo  = v_cod_rechazo
                                           WHERE folio        = v_i_folio_original
                                             AND id_solicitud = v_id_solicitud;
                                          -- se desmarca la solicitud
                                          EXECUTE FUNCTION fn_desmarca_cuenta(
                                                      v_id_derechohabiente,
                                                      "806", -- marca de transferencia
                                                      v_id_solicitud, -- identificador de registro de archivo o lote
                                                      "40", -- estado marca / rechazo validacion
                                                      "806", -- marca de la causa / rechazo por validacion
                                                      TRIM(p_usuario_cod),
                                                      p_proceso_cod_origen) INTO v_resultado;

--                                          LET v_sql = "\nEXECUTE FUNCTION fn_desmarca_cuenta(",
--                                                      "\n",v_id_derechohabiente, ",",
--                                                      "\n 806,", -- marca de transferencia
--                                                      "\n",v_id_solicitud,",", -- identificador de registro de archivo o lote
--                                                      "\n 40,", -- estado marca / rechazo validacion
--                                                      "\n 806,", -- marca de la causa / rechazo por validacion
--                                                      "\n'",p_usuario_cod CLIPPED, "',",
--                                                      "\n",p_proceso_cod_origen, ")";

                                          -- se prepara y ejecuta la desmarcar
--                                          PREPARE sid_desmarca FROM v_sql
--                                          EXECUTE sid_desmarca INTO v_resultado                                       
                                      END IF
                                  END IF
                              ELSE 
                                  LET v_id_solicitud = 0;
                                  SELECT id_solicitud
                                    INTO v_id_solicitud
                                    FROM ret_disposicion
                                   WHERE id_derechohabiente = v_id_derechohabiente
                                     AND folio = v_i_folio_original
                                     AND cod_rechazo = 0;
                                  --trace "Se encontro en ret_disposicion " || v_id_solicitud;
                                  IF v_id_solicitud > 0 THEN
                                      IF (tmp_ret_bp_tpo_credito = "2" OR
                                         tmp_ret_bp_tpo_credito = "12" OR
                                         tmp_ret_bp_tpo_credito = "18") THEN
                                         LET v_d_aivs_viv97 = 0;
                                         SELECT aivs_viv97
                                           INTO v_d_aivs_viv97
                                          FROM ret_disposicion
                                         WHERE id_derechohabiente = v_id_derechohabiente
                                           AND folio = v_i_folio_original
                                           AND cod_rechazo = 0;
                                         --trace "Las aivs_97" || v_d_aivs_viv97;  
                                         IF v_d_aivs_viv97 > 0 AND (v_accion_97 = "1" or v_accion_97 = "2") THEN
                                             IF v_accion_97 = "1" THEN 
                                                 LET v_cod_rechazo = v_error_con_credito_vigente;
                                             ELSE 
                                                 LET v_cod_rechazo = v_error_con_saldo_43bis;
                                             END IF
                                         END IF
                                      ELSE 
                                          IF v_accion_92 = "1" OR v_accion_97 = "1" THEN 
                                              LET v_cod_rechazo = v_error_con_credito_vigente;
                                          ELSE 
                                              LET v_cod_rechazo = v_error_con_saldo_43bis;
                                          END IF
                                      END IF
                                      IF v_cod_rechazo <> "0" THEN 
                                          UPDATE ret_disposicion
                                          SET    estado_solicitud = v_error_estado_solicitud,
                                                 cod_rechazo  = v_cod_rechazo
                                          WHERE  folio        = v_i_folio_original
                                          AND    id_solicitud = v_id_solicitud; 
                                          --trace "Las aivs_97" || v_d_aivs_viv97;

                                          IF p_proceso_cod_origen = 1519 THEN
                                              -- se desmarca la solicitud
                                              EXECUTE FUNCTION fn_desmarca_cuenta(
                                                          v_id_derechohabiente,                                                       
                                                          "808", -- marca PMG
                                                          v_id_solicitud, -- identificador de registro de archivo o lote
                                                          "40", -- estado marca / rechazo validacion
                                                          "808", -- marca de la causa / rechazo por validacion
                                                          TRIM(p_usuario_cod),
                                                          p_proceso_cod_origen) INTO v_resultado;
                                          ELSE
                                              -- se desmarca la solicitud
                                              EXECUTE FUNCTION fn_desmarca_cuenta(
                                                          v_id_derechohabiente, 
                                                          "805", -- marca de disposicion
                                                          v_id_solicitud, -- identificador de registro de archivo o lote
                                                          "40", -- estado marca / rechazo validacion
                                                          "805", -- marca de la causa / rechazo por validacion
                                                          TRIM(p_usuario_cod),
                                                          p_proceso_cod) INTO v_resultado;
                                          END IF
                                      END IF
                                      -- se prepara y ejecuta la desmarcar
--                                      PREPARE sid_desmarca FROM v_sql
--                                      EXECUTE sid_desmarca INTO v_resultado
                                  
                                  END IF                                   
                              END IF
                          END IF
                      END IF 
                  END IF 
               END IF

               LET ret_detalle_bp_folio                = p_folio;
               LET ret_detalle_bp_folio_proceso_origen = v_i_folio_original;
               LET ret_detalle_bp_nss                  = tmp_ret_bp_nss;
               LET ret_detalle_bp_tpo_credito          = tmp_ret_bp_tpo_credito;
               LET ret_detalle_bp_estatus_credito      = tmp_ret_bp_estatus_credito;
               LET ret_detalle_bp_marcado_procesar     = tmp_ret_bp_marcado_procesar;
               LET ret_detalle_bp_desmarcado_procesar  = tmp_ret_bp_desmarcado_procesar;
               INSERT INTO ret_detalle_bp (folio               ,
                                           folio_proceso_origen,
                                           nss                 ,
                                           tpo_credito         ,
                                           estatus_credito     ,
                                           marcado_procesar    ,
                                           desmarcado_procesar)
               VALUES (ret_detalle_bp_folio                ,
                       ret_detalle_bp_folio_proceso_origen ,
                       ret_detalle_bp_nss                  ,
                       ret_detalle_bp_tpo_credito          ,
                       ret_detalle_bp_estatus_credito      ,
                       ret_detalle_bp_marcado_procesar     ,
                       ret_detalle_bp_desmarcado_procesar  );
           ELSE
                   -- break the while loop
		       EXIT;
		   END IF

	   END WHILE
       UPDATE STATISTICS FOR TABLE ret_detalle_bp;
	   CLOSE cust_cur;
       FREE cust_cur ;
       FREE stmt_tmp ;
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


