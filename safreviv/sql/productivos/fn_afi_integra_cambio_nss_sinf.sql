






CREATE FUNCTION "safreviv".fn_afi_integra_cambio_nss_sinf(p_usuario_cod CHAR(20), p_folio DECIMAL(10), 
                                           p_nombre_archivo CHAR(18), p_pid DECIMAL(9,0),
                                           p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255)
  
-- Variables utilizadas para el detalle
DEFINE v_det_tpo_movimiento      CHAR(2);
DEFINE v_det_nrp                 CHAR(11);
DEFINE v_det_f_movimiento        CHAR(8);
DEFINE v_si_dia                  SMALLINT;
DEFINE v_si_mes                  SMALLINT;
DEFINE v_si_ano                  SMALLINT;
DEFINE v_det_date_movimiento     DATE;
DEFINE v_det_t_trabajador        DECIMAL(1,0);
DEFINE v_det_nss                 DECIMAL(11,0);
DEFINE v_det_nombre              CHAR(50);
DEFINE v_det_nss_correcto        DECIMAL(11,0);
DEFINE v_det_nombre_correcto     CHAR(50);

-- Control de Excepciones
DEFINE v_i_resultado             SMALLINT;
DEFINE sql_err                   INTEGER;
DEFINE isam_err                  INTEGER;
DEFINE err_txt                   VARCHAR(255);
-- Variables de validaciones
DEFINE v_d_id_referencia                 DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificador   DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado    DECIMAL(9,0);
DEFINE v_diagnostico_unificador          SMALLINT;
DEFINE v_diagnostico_unificadas          SMALLINT;
DEFINE v_diagnostico_rechazo             SMALLINT;
DEFINE v_estado_familia_unificador       SMALLINT;
DEFINE v_estado_familia_unificado        SMALLINT;
-- Variable para marca de cuenta
DEFINE v_i_estado_marca                  INTEGER;

   -- en caso de encontrar un error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN sql_err, isam_err, err_txt;
   END EXCEPTION

   -- se inician las variables de diagnostico
   LET v_diagnostico_unificador    = 0;
   LET v_diagnostico_unificadas    = 0;
   LET v_diagnostico_rechazo       = 0;
   LET v_estado_familia_unificador = 0;
   LET v_estado_familia_unificado  = 0;

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt = "El proceso de integración de movimientos afilitarios de cambio de NSS finalizó correctamente.";

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_afi_integra_cambio_nss.trace";
   ---------------------------CTAS UNIFICADOR----------------------------- 
   SELECT MAX(id_referencia) INTO v_d_id_referencia
   FROM   uni_det_rechazos;
     
     -- Verifica que el id_referencia no venga nullo
     -- en caso de ser contrario, se asigna el valor que trae
   IF ( v_d_id_referencia IS NULL OR v_d_id_referencia = 0 ) THEN
      LET v_d_id_referencia = 0;
   END IF
   -- [Error]
   --trace "Al recuperar datos detalle tmp_afi_cambio_nss unificador";
   LET err_txt = "Al recuperar datos detalle tmp_afi_cambio_nss unificador";
   FOREACH
   SELECT tpo_movimiento,
          nrp,
          f_movimiento,
          t_trabajador,
          nss,
          nombre,
          nss_correcto,
          nombre_correcto
     INTO v_det_tpo_movimiento,
          v_det_nrp,
          v_det_f_movimiento,
          v_det_t_trabajador,
          v_det_nss,
          v_det_nombre,
          v_det_nss_correcto,
          v_det_nombre_correcto
     FROM safre_tmp:tmp_afi_sinf_cambio_nss
     GROUP BY 1,2,3,4,5,6,7,8
          
      -- Acumula la referencia
      LET v_d_id_referencia = v_d_id_referencia + 1;
      LET v_diagnostico_unificador = 1; --Aceptada
      LET v_diagnostico_unificadas = 1; -- Aceptada
      LET v_diagnostico_rechazo = 1; -- Aceptada
      LET v_estado_familia_unificador = 1; --Aceptada
      LET v_estado_familia_unificado  = 1; --Aceptada
      
      trace "Asigna la fecha de movimiento";
      LET err_txt = "Asigna la fecha de movimiento";
      LET v_si_dia = v_det_f_movimiento[1,2];
      LET v_si_mes = v_det_f_movimiento[3,4];
      LET v_si_ano = v_det_f_movimiento[5,8];
      LET v_det_date_movimiento = MDY(v_si_mes, v_si_dia, v_si_ano);
      
      trace "Valida tipo de movimiento para el registro inicial 05";
      LET err_txt = "Valida tipo de movimiento para el registro inicial 05";
      IF (v_det_tpo_movimiento <> "05" OR v_det_tpo_movimiento IS NULL) THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(
            p_folio,5,5,v_d_id_referencia,"No",12,
            v_det_tpo_movimiento);
         LET v_diagnostico_unificador = 2; --Rechazada
         LET v_diagnostico_rechazo = 12;
         LET v_estado_familia_unificador =  2;
      END IF
      
      ----Recupera el id_derechohabiente
      SELECT id_derechohabiente
        INTO v_id_derechohabiente_unificador
        FROM afi_derechohabiente
       WHERE nss = v_det_nss_correcto;
      
      -- Valida el id_derechohabiente
      --trace "En id_derechohabiente unificador, no existe: "||v_id_derechohabiente_unificador;
      LET err_txt = "En id_derechohabiente unificador, no existe: "||v_id_derechohabiente_unificador;
      IF(v_id_derechohabiente_unificador IS NULL)THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(
            p_folio,2,2,v_d_id_referencia,"No",13,
            v_det_nss_correcto);
         LET v_diagnostico_unificador = 2; --Rechazada   
         LET v_diagnostico_rechazo = 13;
         LET v_estado_familia_unificador =  2;
      END IF
      
      --trace "Al insertar detalle en uni_pre_unificador: "||v_det_nss_correcto;
      LET err_txt = "Al insertar detalle en uni_pre_unificador"||v_det_nss_correcto;
      IF v_diagnostico_unificador = 1 AND v_estado_familia_unificador = 1 THEN
         INSERT INTO uni_pre_unificador(id_pre_unificador,
                                        folio_lote,
                                        id_derechohabiente,
                                        nrp,
                                        tipo_trabajador,
                                        nss_correcto,
                                        nombre_correcto,
                                        estado,
                                        diagnostico,
                                        f_proceso,
                                        f_movimiento)
          VALUES(seq_uni_pre_unificador.NEXTVAL, --id_unificador
                 p_folio,                                  
                 v_id_derechohabiente_unificador,
                 v_det_nrp,
                 v_det_t_trabajador,      
                 v_det_nss_correcto,                         
                 v_det_nombre_correcto,                         
                 v_diagnostico_unificador,                         
                 v_diagnostico_rechazo,                         
                 TODAY,        
                 v_det_date_movimiento);
      END IF           
      
---------------------------CTAS UNIFICADAS----------------------------- 
         
         --trace "Recupera el id derechohabiente unificado";
         LET err_txt = "Recupera el id derechohabiente unificado";
         
         ----Recupera el id_derechohabiente
         SELECT id_derechohabiente
           INTO v_id_derechohabiente_unificado
           FROM afi_derechohabiente
          WHERE nss = v_det_nss;
         
         -- Valida el id_derechohabiente
         --trace "En id_derechohabiente, no existe en unificado";
         LET err_txt = "En id_derechohabiente, no existe en unificado";
         IF(v_id_derechohabiente_unificado IS NULL)THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(
               p_folio,3,3,v_d_id_referencia,"No",15,
               v_det_nss);
            LET v_diagnostico_unificadas = 2; --Rechazada
            LET v_diagnostico_rechazo = 15;
            LET v_estado_familia_unificado =  2;
         END IF
         
         --trace "Al insertar detalle en uni_pre_unificado";
         LET err_txt = "Al insertar detalle en uni_pre_unificado";
         IF v_diagnostico_unificadas = 1 AND v_estado_familia_unificado = 1 THEN
            INSERT INTO uni_pre_unificado(id_pre_unificado,
                                          id_pre_unificador,
                                          id_derechohabiente,
                                          id_unificado,
                                          nss,
                                          nombre,
                                          estado,
                                          diagnostico)
             VALUES(seq_uni_pre_unificado.NEXTVAL, --id_unificado
                    seq_uni_pre_unificador.CURRVAL, --id_unificador
                    v_id_derechohabiente_unificado,
                    NULL,
                    v_det_nss,
                    v_det_nombre,
                    v_diagnostico_unificadas,
                    v_diagnostico_rechazo);
         END IF
         
         --trace "Valida para marcar la cuenta unificado";
         LET err_txt = "Valida para marcar la cuenta unificado";
         IF v_diagnostico_unificadas = 1 AND v_estado_familia_unificado = 1 THEN
            LET v_i_estado_marca = 0;
            --trace "Al marcar unificación imss unificado";
             EXECUTE FUNCTION fn_marca_cuenta(
                              v_id_derechohabiente_unificado,
                              502,-- marca de unificado IMSS
                              seq_uni_pre_unificado.CURRVAL,
                              p_folio,
                              0, -- estado marca
                              0, -- codigo de rechazo
                              0, -- marca de la causa
                              NULL, -- fecha de la causa
                              p_usuario_cod,
                              p_proceso_cod)
                INTO v_i_estado_marca;
         END IF
         
         --trace "Valida para marcar la cuenta unificador";
         LET err_txt = "Valida para marcar la cuenta unificador";
         IF v_diagnostico_unificador = 1 AND v_estado_familia_unificador = 1 THEN
            LET v_i_estado_marca = 0;
            --trace "Al marcar unificación imss unificador";
                EXECUTE FUNCTION fn_marca_cuenta(
                                 v_id_derechohabiente_unificador,
                                 501,-- marca de unificador IMSS
                                 seq_uni_pre_unificador.CURRVAL,
                                 p_folio,
                                 0, -- estado marca
                                 0, -- codigo de rechazo
                                 0, -- marca de la causa
                                 NULL, -- fecha de la causa
                                 p_usuario_cod,
                                 p_proceso_cod)
                   INTO v_i_estado_marca;
         END IF
   END FOREACH;
    
    --trace "Al actualizar glo_ctr_archivo";
    --LET err_txt = "Al actualizar glo_ctr_archivo";
    ---- Se asigna el folio al archivo y se indica que ha sido integrado
    --UPDATE safre_viv:glo_ctr_archivo
    --   SET folio = p_folio, estado = 2 -- integrado
    -- WHERE proceso_cod    = p_proceso_cod
    --   AND opera_cod      = 1 -- archivo cargado
    --   AND estado         = 1; -- etapa de carga
    --   
    --
    --UPDATE safre_viv:bat_ctr_operacion 
    --  SET folio       = p_folio,
    --      nom_archivo = p_nombre_archivo
    --WHERE proceso_cod = p_proceso_cod
    --  AND opera_cod   = 2
    --  AND pid         = p_pid;      
   
       
   --trace "El proceso de integración de movimientos afilitarios de cambio de NSS finalizó correctamente.";
   LET err_txt = "El proceso de integración de movimientos afilitarios de cambio de NSS finalizó correctamente.";
   
   update statistics for table uni_pre_unificador;
   update statistics for table uni_pre_unificado;
   
 RETURN sql_err, isam_err, err_txt;
END FUNCTION
;


