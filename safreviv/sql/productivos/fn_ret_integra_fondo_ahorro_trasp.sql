






CREATE FUNCTION "safreviv".fn_ret_integra_fondo_ahorro_trasp(   p_usuario_cod    CHAR(20)
                                              , p_folio          DECIMAL(9,0)
                                              , p_nombre_archivo VARCHAR(40)
                                              , p_pid            DECIMAL(9,0)
                                              , p_proceso_cod    SMALLINT
                                              )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)
   
-- campos de la tabla de detalle de retiros de traspaso fondo ahorro (sin filler)
DEFINE tmp_ret_det_rfc                         CHAR(13);
DEFINE tmp_ret_det_nombre                      CHAR(50);
DEFINE tmp_ret_det_nss                         CHAR(11);
DEFINE tmp_ret_det_credito                     CHAR(13);
DEFINE tmp_ret_det_fecha                       CHAR(10);
DEFINE tmp_ret_det_saldo                       CHAR(20);

-- detalle de la tabla historica/integrada de retiros de traspaso fondo ahorro
-- ret_fondo_ahorro_trasp
DEFINE ret_fa_trasp_id_solicitud         DECIMAL(9,0) ;
DEFINE ret_fa_trasp_f_solicitud          DATE         ;
DEFINE ret_fa_trasp_estado_solicitud     SMALLINT     ;
DEFINE ret_fa_trasp_cod_rechazo          SMALLINT     ;
DEFINE ret_fa_trasp_folio                DECIMAL(9,0) ;
DEFINE ret_fa_trasp_nss                  CHAR(11)     ;
DEFINE ret_fa_trasp_rfc                  CHAR(13)     ;
DEFINE ret_fa_trasp_nombre               CHAR(50)     ;
DEFINE ret_fa_trasp_credito              CHAR(13)     ;
DEFINE ret_fa_trasp_fecha                CHAR(10)     ;
DEFINE ret_fa_trasp_saldo                DECIMAL(14,2);

-- variables de soporte al proceso
DEFINE v_id_afi_fondo72                      DECIMAL(9,0);
DEFINE v_estado_solicitud                    SMALLINT;
DEFINE v_cod_rechazo                         SMALLINT;
DEFINE v_ocurrencias                         INTEGER;

--variable de solicitd   
DEFINE v_id_solicitud                        DECIMAL(9,0);

-- conteo de rechazos e inserciones            
DEFINE v_reg_det_rechazados                    DECIMAL(9,0); -- total de registros de detalle rechazados

-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado               SMALLINT;

-- para marcar las cuentas
DEFINE v_i_estado_marca                          INTEGER;
DEFINE v_marca_fondo_ahorro_trasp                SMALLINT; -- 802 de acuerdo a catalogo

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_nss;
   END EXCEPTION
     
   --SET DEBUG FILE TO "/safreviv_int/BD/fn_ret_integra_fondo_ahorro.trace";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_rechazados              = 0; -- total de registros de detalle rechazados
   LET v_estado_solicitud                = 0;
   LET v_cod_rechazo                     = 0;

   
   -- se asume que el proceso termina bien
   LET v_si_resultado  = 0;
   LET isam_err        = 0;
   LET v_c_msj         = 'El proceso finalizó exitosamente.';
   LET tmp_ret_det_nss = NULL;

   
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado                = 49; -- NSS NO ENCONTRADO

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro_trasp = 821; -- marca para traspaso fondo ahorro
   
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
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;
   
   -- se obtienen los datos del detalle
   FOREACH
      SELECT SUBSTR(contenido,4,13),
             SUBSTR(contenido,21,45),
             SUBSTR(contenido,66,11),
             SUBSTR(contenido,81,13),
             SUBSTR(contenido,101,10),
             SUBSTR(contenido,111,20)
      INTO   tmp_ret_det_rfc,
             tmp_ret_det_nombre,
             tmp_ret_det_nss,
             tmp_ret_det_credito,
             tmp_ret_det_fecha,
             tmp_ret_det_saldo
      FROM   safre_tmp:tmp_ret_det_fa_traspaso
      WHERE  LENGTH(TRIM(contenido)) = 122     --- Se valida la longitud del registro
      AND    LENGTH(TRIM(SUBSTR(contenido,4,13)))   = 13  --- Se valida que venga RFC
      AND    LENGTH(TRIM(SUBSTR(contenido,66,11)))  = 11  --- Se valida que venga NSS
      AND    LENGTH(TRIM(SUBSTR(contenido,81,13)))  = 13  --- Se validan las posiciones del credito
      AND    LENGTH(TRIM(SUBSTR(contenido,101,10))) = 10  --- Se validan las posiciones de la fecha
      AND    LENGTH(TRIM(SUBSTR(contenido,111,20))) > 2   --- Solo se procesan los que tengan importe

      -- ==========================================================================
      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud     = 0;
      LET v_id_afi_fondo72   = NULL;
      LET v_estado_solicitud = 15;
      LET v_cod_rechazo      = 0;
     
      -- se busca por NSS-RFC
      SELECT FIRST 1 id_afi_fondo72 
      INTO   v_id_afi_fondo72
      FROM   afi_fondo72
      WHERE  nss = tmp_ret_det_nss
      AND    rfc = tmp_ret_det_rfc;

      LET v_ocurrencias = 0;
      SELECT COUNT(id_afi_fondo72)
      INTO   v_ocurrencias
      FROM   afi_fondo72
      WHERE  nss = tmp_ret_det_nss
      AND    rfc = tmp_ret_det_rfc;
         
      IF v_ocurrencias < 1 THEN 
         LET v_id_afi_fondo72 = NULL;
         -- se busca por RFC
         SELECT FIRST 1 id_afi_fondo72 
         INTO   v_id_afi_fondo72
         FROM   afi_fondo72
         WHERE  rfc = tmp_ret_det_rfc;

         LET v_ocurrencias = 0;
         SELECT COUNT(id_afi_fondo72)
         INTO   v_ocurrencias
         FROM   afi_fondo72
         WHERE  rfc = tmp_ret_det_rfc;
         IF v_ocurrencias < 1 THEN 
            LET v_id_afi_fondo72 = NULL;
            IF tmp_ret_det_nss = "00000000000" THEN 
               LET v_estado_solicitud    = 100;
               LET v_cod_rechazo         = v_error_det_nss_no_encontrado;
            ELSE
               -- se busca por NSS
               SELECT FIRST 1 id_afi_fondo72 
               INTO   v_id_afi_fondo72
               FROM   afi_fondo72
               WHERE  nss = tmp_ret_det_nss;

               LET v_ocurrencias = 0;
               SELECT COUNT(id_afi_fondo72)
               INTO   v_ocurrencias
               FROM   afi_fondo72
               WHERE  nss = tmp_ret_det_nss;
               IF v_ocurrencias < 1 THEN 
                  LET v_id_afi_fondo72 = NULL;
                  LET v_estado_solicitud    = 100;
                  LET v_cod_rechazo         = v_error_det_nss_no_encontrado;
                  LET v_reg_det_rechazados  = v_reg_det_rechazados + 1;
               END IF
            END IF
         END IF
      END IF

      -- se obtiene el id_solicitud
      SELECT seq_ret_fondo_ahorro_trasp.NEXTVAL
      INTO   v_id_solicitud
      FROM   systables
      WHERE  tabid = 1;
       
      -- se asignan los datos al registro de rechazo de detalle                   
      LET ret_fa_trasp_id_solicitud      = v_id_solicitud       ;     
      LET ret_fa_trasp_f_solicitud       = TODAY                ;     
      LET ret_fa_trasp_estado_solicitud  = v_estado_solicitud   ;     
      LET ret_fa_trasp_cod_rechazo       = v_cod_rechazo        ;     
      LET ret_fa_trasp_folio             = p_folio              ;     
      LET ret_fa_trasp_nss               = tmp_ret_det_nss      ;     
      LET ret_fa_trasp_rfc               = tmp_ret_det_rfc      ;
      LET ret_fa_trasp_nombre            = tmp_ret_det_nombre   ;     
      LET ret_fa_trasp_credito           = tmp_ret_det_credito  ;     
      LET ret_fa_trasp_fecha             = tmp_ret_det_fecha    ;     
      LET ret_fa_trasp_saldo             = tmp_ret_det_saldo    ;
        
      -- se inserta en la tabla historia de detalle de retiro de traspasos fondo ahorro
      INSERT INTO ret_fondo_ahorro_trasp( 
                  id_solicitud
                 ,f_solicitud
                 ,estado_solicitud
                 ,cod_rechazo
                 ,folio
                 ,nss
                 ,rfc
                 ,nombre
                 ,credito
                 ,fecha
                 ,saldo
                 )
         VALUES (
                 ret_fa_trasp_id_solicitud
                ,ret_fa_trasp_f_solicitud
                ,ret_fa_trasp_estado_solicitud
                ,ret_fa_trasp_cod_rechazo
                ,ret_fa_trasp_folio
				    ,ret_fa_trasp_nss
				    ,ret_fa_trasp_rfc
                ,ret_fa_trasp_nombre
                ,ret_fa_trasp_credito
                ,ret_fa_trasp_fecha
                ,ret_fa_trasp_saldo
                );
   END FOREACH;
   
   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_fondo_ahorro_trasp;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_nss;
END FUNCTION;


