






CREATE FUNCTION "safreviv".fn_ret_integra_resp_notifica_gpo(  p_usuario_cod    CHAR(20)
                                      , p_folio          DECIMAL(9,0)
                                      , p_nombre_archivo VARCHAR(40)
                                      , p_pid            DECIMAL(9,0)
                                      , p_proceso_cod    SMALLINT
                                      )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)

   -- campos de la tabla de detalle de retiros de Ley 73 (sin filler)
   DEFINE tmp_ret_det_nss                          CHAR(11);
   DEFINE tmp_ret_det_diag_procesar                CHAR(03);
   DEFINE tmp_ret_det_fecha_pago                   DATE;
  
   --variable de solicitd
   DEFINE v_id_solicitud_envio                     DECIMAL(9,0);
   DEFINE v_id_solicitud_retiro                    DECIMAL(9,0);
   DEFINE v_estado_solicitud                       SMALLINT;
   DEFINE v_tipo_retiro                            SMALLINT;

   -- Control de Excepciones
   DEFINE v_si_resultado                            SMALLINT;
   DEFINE sql_err                                   INTEGER;
   DEFINE isam_err                                  INTEGER;
   DEFINE err_txt                                   VARCHAR(250);
   DEFINE v_c_msj                                   VARCHAR(250);
   DEFINE r_bnd_edo_act_archivo                     SMALLINT;


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_nss;
   END EXCEPTION

   LET tmp_ret_det_nss             = "";
   LET v_estado_solicitud          = 0;
   LET v_id_solicitud_envio        = 0;
   LET v_id_solicitud_retiro       = 0;
   LET v_tipo_retiro               = 0;

   --SET DEBUG FILE TO "/safreviv_int/BD/debug_ret_ley73.txt";
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_ley73.txt";

    -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio ,
          estado = 2     -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1 -- etapa de carga
   AND    nombre_archivo = p_nombre_archivo;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    pid         = p_pid;
   
   -- Actualiza el estado del archivo procesado
   CALL fn_act_edo_archivo(p_nombre_archivo,p_folio,2,p_usuario_cod) 
         RETURNING r_bnd_edo_act_archivo;

   -- se asume que el proceso termina bien
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';


   -- se obtienen los datos del detalle
   FOREACH
   SELECT
       nss
      ,diag_procesar
      ,MDY(substr(fecha_pago,5,2),substr(fecha_pago,7,2),substr(fecha_pago,1,4))
    INTO
       tmp_ret_det_nss
      ,tmp_ret_det_diag_procesar
      ,tmp_ret_det_fecha_pago
   FROM
      safre_tmp:tmp_ret_resp_notif_detalle

      LET v_id_solicitud_envio = 0;
      -- se obtiene el id_derechohabiente
      SELECT id_solicitud_envio, id_solicitud_retiro, tipo_retiro
      INTO   v_id_solicitud_envio, v_id_solicitud_retiro, v_tipo_retiro
      FROM   ret_notifica_gpo
      WHERE  nss = tmp_ret_det_nss
      AND    estado_solicitud = 80
      AND    fch_respuesta IS NULL
      AND    diag_procesar = 0
      AND    fch_pago = tmp_ret_det_fecha_pago;

      IF v_id_solicitud_envio IS NOT NULL AND v_id_solicitud_envio <> 0 THEN
         IF  tmp_ret_det_diag_procesar = "400" THEN 
            LET v_estado_solicitud = 81;
            UPDATE ret_notifica_gpo
            SET    diag_procesar = tmp_ret_det_diag_procesar,
                   fch_respuesta = TODAY,
                   estado_solicitud = v_estado_solicitud
            WHERE  id_solicitud_envio = v_id_solicitud_envio;
            INSERT INTO ret_notifica_gpo_his
            SELECT * 
            FROM   ret_notifica_gpo
            WHERE  nss = tmp_ret_det_nss;
            DELETE 
            FROM   ret_notifica_gpo
            WHERE  nss = tmp_ret_det_nss;
         ELSE
            LET v_estado_solicitud = 82;
            UPDATE ret_notifica_gpo
            SET    diag_procesar = tmp_ret_det_diag_procesar,
                   fch_respuesta = TODAY,
                   estado_solicitud = v_estado_solicitud
            WHERE  id_solicitud_envio = v_id_solicitud_envio;
         END IF 
         IF v_tipo_retiro = 1 THEN 
            UPDATE ret_solicitud_generico
            SET    estado_solicitud = v_estado_solicitud
            WHERE  id_solicitud     = v_id_solicitud_retiro;
            UPDATE ret_ley73_generico
            SET    estado_solicitud = v_estado_solicitud
            WHERE  id_solicitud     = v_id_solicitud_retiro;
         END IF
         IF v_tipo_retiro = 2 THEN 
            UPDATE ret_solo_infonavit
            SET    estado_solicitud = v_estado_solicitud
            WHERE  id_solicitud     = v_id_solicitud_retiro;
         END IF 
         IF v_tipo_retiro = 3 THEN 
            UPDATE ret_excep_devol_ssv
            SET    estado_solicitud = v_estado_solicitud
            WHERE  id_solicitud     = v_id_solicitud_retiro;
         END IF 
            
      END IF 
         

   END FOREACH;
   --trace("fin FOREACH ");
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_nss;
END FUNCTION;


