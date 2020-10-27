






CREATE FUNCTION "safreviv".fn_ret_integra_but(p_usuario_cod CHAR(20), 
                                  p_folio DECIMAL(9,0),
                                  p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT,
                                  P_proceso_cod_origen SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250)

-- detalle de la tabla temporal

DEFINE tmp_ret_but_nss                  CHAR(11)      ;
DEFINE tmp_ret_but_marca_grupo          CHAR(10)      ;
DEFINE tmp_ret_but_estatus              CHAR(9)       ;

-- tablas destino
-- ret_detalle_but
DEFINE ret_detalle_but_folio                DECIMAL(9,0);
DEFINE ret_detalle_but_folio_proceso_origen DECIMAL(9,0);
DEFINE ret_detalle_but_nss                  CHAR(11)    ;
DEFINE ret_detalle_but_marca_grupo          CHAR(4)     ;
DEFINE ret_detalle_but_estatus              CHAR(4)     ;

-- constantes nombre de tablas temporales
DEFINE v_c_disp_but                         CHAR(20);
DEFINE v_c_pmg_but                          CHAR(20);
DEFINE v_c_tran_but                         CHAR(20);
DEFINE v_c_tipn_but                         CHAR(20);
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
DEFINE v_accion                                 CHAR(1);
DEFINE v_error                                  CHAR(3);
DEFINE v_nss_busqueda                           CHAR(15);
DEFINE v_cant_nss                               SMALLINT;
DEFINE v_cod_rechazo                            CHAR(3);
DEFINE v_d_aivs_viv97                           DECIMAL(22,6);
DEFINE v_tipo_retiro                            CHAR(1);

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


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   -- se establece el archivo para el debug
   SET DEBUG FILE TO "/safreviv_int/BD/debug_but.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados  = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados  = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados

   LET v_c_disp_but           = "tmp_ret_disp_but_val";
   LET v_c_pmg_but            = "tmp_ret_pmg_but_val";
   LET v_c_tran_but           = "tmp_ret_tran_but_val";
   LET v_c_tipn_but           = "tmp_ret_tupn_but_val";   
   LET v_c_tabla_tmp          = "";
   LET v_cod_rechazo          = "0";
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';
  
   trace ON;
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
       
   END IF
   
   IF p_proceso_cod = 1554 THEN
       LET v_c_tabla_tmp = v_c_disp_but;
   END IF
   IF p_proceso_cod = 1556 THEN
       LET v_c_tabla_tmp = v_c_pmg_but;
   END IF
   IF p_proceso_cod = 1558 THEN
       LET v_c_tabla_tmp = v_c_tran_but;
   END IF
   IF p_proceso_cod = 1560 THEN
       LET v_c_tabla_tmp = v_c_tipn_but;
   END IF

   LET cust_qry = " SELECT  TRIM(nss), TRIM(marca_grupo), TRIM(estatus) " || 
                  " from safre_tmp:" || v_c_tabla_tmp ||
                  " WHERE  TRIM(marca_grupo) IS NOT NULL AND TRIM(estatus) IS NOT NULL " ||
                  " AND TRIM(nss) IS NOT NULL"; 
   
   PREPARE stmt_tmp FROM cust_qry;
   DECLARE cust_cur cursor FOR stmt_tmp;
   OPEN cust_cur;
       WHILE (1 = 1)
           FETCH cust_cur INTO tmp_ret_but_nss,
                               tmp_ret_but_marca_grupo,
                               tmp_ret_but_estatus;
           IF (SQLCODE != 100) THEN
               IF tmp_ret_but_marca_grupo <> "0104"   AND    --- Estos siempre son aceptados
                  tmp_ret_but_marca_grupo <> "0504"   THEN
                  -- Busca accion en la matriz BUT
                  LET v_accion = "0";
                  LET v_error  = "0";
                  IF LENGTH(TRIM(tmp_ret_but_estatus)) = 0 THEN 
                      LET tmp_ret_but_estatus = "0000";
                  END IF
                  IF LENGTH(TRIM(tmp_ret_but_marca_grupo)) = 0 THEN
                      LET tmp_ret_but_marca_grupo = "0000";
                  END IF
                  SELECT accion, codigo_rechazo
                    INTO v_accion, v_error
                    FROM ret_matriz_but
                   WHERE marca_grupo     = tmp_ret_but_marca_grupo
                     AND estatus         = tmp_ret_but_estatus;
                  
                  IF v_accion = "1" THEN -- solo se consideran los posibles rechazos
                      IF LENGTH(TRIM(tmp_ret_but_nss)) = 10 THEN
                          LET v_nss_busqueda = "0" || TRIM(tmp_ret_but_nss);
                      ELSE 
                          LET v_nss_busqueda = TRIM(tmp_ret_but_nss);
                      END IF
                      --LET v_nss_busqueda = "*" ||  TRIM(tmp_ret_but_nss) || "*";
                      trace "Nss a buscar :" || v_nss_busqueda;
                      
                      IF p_proceso_cod_origen = 1505 THEN 
                          IF v_accion = "1" THEN
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
                                      IF v_error IS NOT NULL THEN
                                          LET v_cod_rechazo = v_error;
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
                                      END IF 
                                  END IF
                              END IF
                          END IF 
                      ELSE 
                          SELECT COUNT(*) 
                            INTO v_cant_nss
                            FROM afi_derechohabiente 
                           WHERE nss = v_nss_busqueda;

                          IF v_cant_nss = 1 THEN
                              trace "Se encontro en afi_derechohabiente " || v_nss_busqueda;
                              SELECT id_derechohabiente
                                INTO v_id_derechohabiente
                                FROM afi_derechohabiente 
                               WHERE nss = v_nss_busqueda;
                              IF p_proceso_cod_origen = 1504 THEN 
                                  IF v_accion = "1" THEN
                                      LET v_id_solicitud = 0;
                                      SELECT id_solicitud
                                        INTO v_id_solicitud
                                        FROM ret_transferencia
                                       WHERE id_derechohabiente = v_id_derechohabiente
                                         AND folio = v_i_folio_original
                                         AND cod_rechazo = 0;
                                      IF v_id_solicitud > 0 THEN
                                          IF v_error IS NOT NULL THEN
                                             LET v_cod_rechazo = v_error;
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
                                          END IF

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
                                  
                                  IF v_id_solicitud > 0 THEN
                                      IF v_error IS NOT NULL THEN 
                                          LET v_cod_rechazo = v_error;
                                          IF p_proceso_cod_origen = 1519 THEN
                                             UPDATE ret_disposicion
                                             SET    estado_solicitud = v_error_estado_solicitud,
                                                    cod_rechazo  = v_cod_rechazo
                                             WHERE  folio        = v_i_folio_original
                                             AND    id_solicitud = v_id_solicitud; 
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
                                             SELECT b.tpo_retiro
                                             INTO   v_tipo_retiro
                                             FROM   ret_disposicion a, ret_matriz_derecho b
                                             WHERE  a.id_ret_matriz_derecho = b.id_ret_matriz_derecho
                                             AND    a.id_solicitud = v_id_solicitud;

                                             IF  ((tmp_ret_but_marca_grupo = "0101" OR 
                                                   tmp_ret_but_marca_grupo = "0102" OR 
                                                   tmp_ret_but_marca_grupo = "0103" OR 
                                                   tmp_ret_but_marca_grupo = "0114" OR 
                                                   tmp_ret_but_marca_grupo = "0124" OR 
                                                   tmp_ret_but_marca_grupo = "0201" OR 
                                                   tmp_ret_but_marca_grupo = "0211" OR 
                                                   tmp_ret_but_marca_grupo = "0401" OR 
                                                   tmp_ret_but_marca_grupo = "0402" OR 
                                                   tmp_ret_but_marca_grupo = "0403" OR 
                                                   tmp_ret_but_marca_grupo = "0404" OR 
                                                   tmp_ret_but_marca_grupo = "0411" OR
                                                   tmp_ret_but_marca_grupo = "0412" OR 
                                                   tmp_ret_but_marca_grupo = "0413" OR 
                                                   tmp_ret_but_marca_grupo = "0414") AND  
                                                   (tmp_ret_but_estatus = "0014" OR
                                                    tmp_ret_but_estatus = "0017" OR
                                                    tmp_ret_but_estatus = "0019" OR
                                                    tmp_ret_but_estatus = "0021") AND
                                                   (v_tipo_retiro = "H" OR
                                                    v_tipo_retiro = "J")) THEN 
                                                LET v_cod_rechazo = 0;
                                             ELSE 
                                                  
                                                UPDATE ret_disposicion
                                                SET    estado_solicitud = v_error_estado_solicitud,
                                                       cod_rechazo  = v_cod_rechazo
                                                WHERE  folio        = v_i_folio_original
                                                AND    id_solicitud = v_id_solicitud; 
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
                                      END IF 
                                  
                                  END IF                                   
                              END IF
                          END IF
                      END IF 
                  END IF 
               END IF

               LET ret_detalle_but_folio                = p_folio;
               LET ret_detalle_but_folio_proceso_origen = v_i_folio_original;
               LET ret_detalle_but_nss                  = tmp_ret_but_nss;
               LET ret_detalle_but_marca_grupo          = tmp_ret_but_marca_grupo;
               LET ret_detalle_but_estatus              = tmp_ret_but_estatus;
               INSERT INTO ret_detalle_but (folio               ,
                                           folio_proceso_origen,
                                           nss                 ,
                                           marca_grupo         ,
                                           estatus             )
               VALUES (ret_detalle_but_folio                ,
                       ret_detalle_but_folio_proceso_origen ,
                       ret_detalle_but_nss                  ,
                       ret_detalle_but_marca_grupo          ,
                       ret_detalle_but_estatus              );
           ELSE
                   -- break the while loop
		       EXIT;
		   END IF

	   END WHILE
       UPDATE STATISTICS FOR TABLE ret_detalle_but;
	   CLOSE cust_cur;
       FREE cust_cur ;
       FREE stmt_tmp ;
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


