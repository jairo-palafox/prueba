






CREATE FUNCTION "selefp".fn_integra_nvo_modelo_imss(p_usuario_cod    CHAR(20),
                                           p_proceso_cod    SMALLINT, 
                                           p_nombre_archivo CHAR(40),
                                           p_folio          DECIMAL(9,0), 
                                           p_pid            DECIMAL(9,0)) 

  RETURNING INTEGER,
            CHAR(200),
            INTEGER,
            INTEGER,
            SMALLINT;

DEFINE v_tmp_cza_tipo_registro CHAR(2);
DEFINE v_tmp_cza_id_servicio CHAR(2);
DEFINE v_tmp_cza_id_operacion CHAR(2);
DEFINE v_tmp_cza_tipo_entidad_origen CHAR(2);
DEFINE v_tmp_cza_cve_entidad_origen CHAR(3);
DEFINE v_tmp_cza_tipo_entidad_destino CHAR(2);
DEFINE v_tmp_cza_clave_entidad_destino CHAR(3);
DEFINE v_tmp_cza_fecha_presentacion CHAR(8);
DEFINE v_tmp_cza_consecutivo_lote_dia DECIMAL(3,0);
DEFINE v_tmp_cza_periodo_unificacion CHAR(6)   ;
DEFINE v_tmp_cza_filler CHAR(228);
DEFINE v_enc_fecha_presentacion DATE;
DEFINE v_secuencia_encabezado DECIMAL(9,0);
DEFINE v_secuencia_unificador DECIMAL(9,0);
DEFINE v_secuencia_unificados DECIMAL(9,0);
DEFINE v_secuencia_sumario DECIMAL(9,0);
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt CHAR(200);
DEFINE v_resultado SMALLINT ;
DEFINE v_total_unificadores INTEGER;  
DEFINE v_total_unificados INTEGER;
DEFINE v_total_registros INTEGER;
DEFINE v_det_folio_unificacion DECIMAL(9,0);
DEFINE v_det_id_derechohabiente DECIMAL(9,0);
DEFINE v_det_tipo_registro CHAR(2);
DEFINE v_det_contador_servicio DECIMAL(9,0);
DEFINE v_det_tipo_entidad_solicitante CHAR(2);
DEFINE v_det_cve_entidad_solicitante CHAR(3);
DEFINE v_det_tipo_entidad_unificador CHAR(2);
DEFINE v_det_clave_entidad_unificador CHAR(3);
DEFINE v_det_curp_unificador CHAR(18);
DEFINE v_curp_afi CHAR(18);
DEFINE v_det_nss_unificador CHAR(11);
DEFINE v_det_rfc_unificador CHAR(13);
DEFINE v_det_paterno_unificador CHAR(40);
DEFINE v_det_materno_unificador CHAR(40);
DEFINE v_det_nombre_unificador CHAR(40);
DEFINE v_det_nombre_imssunificador CHAR(50);
DEFINE v_det_sexo_unificador CHAR(1);
DEFINE v_det_entidad_nacunificador CHAR(2);
DEFINE v_det_fec_nac_unificador CHAR(8);
DEFINE v_det_date_nac_unificador DATE;
DEFINE v_det_tpo_docto_probatorio CHAR(1);
DEFINE v_det_clave_afore_receptora CHAR(3);
DEFINE v_det_numero_cuentasasoc DECIMAL(2,0);
DEFINE v_det_estatus_convocatoria CHAR(1);
DEFINE v_det_resultado_operacion CHAR(2);
DEFINE v_det_ident_movimiento CHAR(2);
DEFINE v_det_cve_afore_aclaracion CHAR(3);
DEFINE v_det_id_credito_infonavit CHAR(1);
DEFINE v_det_indicador_procedencia SMALLINT;
DEFINE v_det_diagnostico_no_procedente CHAR(1);
DEFINE v_det_estado_unificacion DECIMAL(2,0);
DEFINE v_det_diagnostico DECIMAL(3,0);
DEFINE v_det_fec_liquidacion CHAR(8);
DEFINE v_det_date_liquidacion DATE;
DEFINE v_det_fec_notificacion CHAR(8);
DEFINE v_det_date_notificacion DATE;
DEFINE v_det_folio_liquidacion DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificador DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado  DECIMAL(9,0);
DEFINE v_diagnostico_unificador SMALLINT;
DEFINE v_diagnostico_unificadas SMALLINT;
DEFINE v_diagnostico_rechazo SMALLINT;
DEFINE v_estado_familia SMALLINT;
DEFINE v_estado_familia_unificador SMALLINT;
DEFINE v_estado_familia_unificado SMALLINT; 
DEFINE v_existe_unificado SMALLINT;
DEFINE v_existe_unificador SMALLINT;
DEFINE v_unificador_valido SMALLINT;
DEFINE v_unificado_valido SMALLINT;
DEFINE v_det2_folio_unificacion DECIMAL(9,0);
DEFINE v_det2_id_derechohabiente DECIMAL(9,0);
DEFINE v_det2_tipo_registro CHAR(2);
DEFINE v_det2_contador_servicio DECIMAL(9,0);
DEFINE v_det2_nss_unificador CHAR(11);
DEFINE v_det2_tipo_entidad CHAR(2);
DEFINE v_det2_clave_entidad CHAR(3);
DEFINE v_det2_curp CHAR(18);
DEFINE v_det2_nss_unificado CHAR(11);
DEFINE v_det2_rfc CHAR(13);
DEFINE v_det2_apellido_paterno CHAR(40);
DEFINE v_det2_apellido_materno CHAR(40);
DEFINE v_det2_nombre CHAR(40);
DEFINE v_det2_nombre_imss CHAR(50);
DEFINE v_det2_sexo CHAR(1);
DEFINE v_det2_ent_nacimiento CHAR(2);
DEFINE v_det2_fecha_nacimiento CHAR(8);
DEFINE v_det2_fecha_nacimiento_res DATE;
DEFINE v_det2_estatus_traspaso CHAR(2);
DEFINE v_det2_estatus_retiro CHAR(2);
DEFINE v_det2_estatus_convocatoria CHAR(1);
DEFINE v_det2_diagnostico_uni CHAR(2);
DEFINE v_det2_resultado_operacion CHAR(2);
DEFINE v_det2_afore_aclaracion CHAR(3);
DEFINE v_det2_credito43bis CHAR(1);
DEFINE v_det2_estado_unificacion CHAR(2);
DEFINE v_sum_folio_unificacion DECIMAL(9,0);
DEFINE v_sum_tipo_registro CHAR(2);
DEFINE v_sum_total_registro DECIMAL(9,0);
DEFINE v_sum_total_nss_unificador DECIMAL(9,0);
DEFINE v_sum_total_nss_unificados DECIMAL(9,0);
DEFINE v_ip_id_derechohabiente DECIMAL (9,0);
DEFINE v_ip_id_unificador DECIMAL (9,0);
DEFINE v_ip_folio_unificacion DECIMAL (9,0);
DEFINE v_ip_folio_resp_confronta DECIMAL (9,0);
DEFINE v_ip_ind_procedencia SMALLINT     ;
DEFINE v_marca_afi SMALLINT;
DEFINE v_n_referencia_afi DECIMAL(9,0);
DEFINE v_marca_afi_ado SMALLINT;    
DEFINE v_n_referencia_afi_ado DECIMAL(9,0);
DEFINE v_res_desmarca SMALLINT;
DEFINE v_marca_activa SMALLINT;
DEFINE v_estado_marca SMALLINT;
DEFINE v_id_dh_procedencia DECIMAL(9,0);       
DEFINE v_id_dh_det DECIMAL(9,0);
DEFINE v_id_unificador_det DECIMAL(9,0);
DEFINE v_folio_uni_detalle DECIMAL(9,0);
DEFINE v_id_dh_det_ado DECIMAL(9,0);
DEFINE v_id_unificado_det DECIMAL(9,0);
DEFINE v_marca_inhabilita_uni DECIMAL(9,0);
DEFINE v_referencia_inahbilita_uni DECIMAL(9,0);
DEFINE v_id_dor_procedencia DECIMAL(9,0);
DEFINE v_folio_uni_det DECIMAL(9,0);
DEFINE v_id_unificado_confronta DECIMAL(9,0);
DEFINE v_des_id_unificador DECIMAL(9,0);
DEFINE v_des_dor_id_derechohabiente DECIMAL(9,0);
DEFINE v_des_dor_estado_unificacion SMALLINT    ;  
DEFINE v_des_id_unificado DECIMAL(9,0);
DEFINE v_des_ado_id_derechohabiente DECIMAL(9,0);
DEFINE v_des_ado_estado_unificacion SMALLINT    ;  
DEFINE v_rd_id_pre_unificador DECIMAL(9,0);
DEFINE v_rd_folio_lote DECIMAL(9,0);
DEFINE v_rd_id_pre_unificado DECIMAL(9,0);
DEFINE v_rd_folio_lote_ado DECIMAL(9,0);
DEFINE v_act_total_actualizacion INTEGER;
DEFINE v_ind_act_procedencia SMALLINT;
DEFINE v_ind_actualizacion SMALLINT;
DEFINE v_tot_procedencia SMALLINT;

ON EXCEPTION SET sql_err,
                 isam_err

   LET v_resultado          = sql_err;
   LET v_total_unificadores = 0;
   LET v_total_unificados   = 0;
   LET v_total_registros    = 0;
      
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;      

   RETURN v_resultado, 
          err_txt,
          v_total_unificadores,
          v_total_unificados,
          v_ind_actualizacion;
END EXCEPTION
   
  --Se habilita el LOG del SP
  --SET DEBUG FILE TO '/safreviv/uni/unl/integra_nuevo_modelo.trace';
  --SET DEBUG FILE TO '/safreviv_int/archivos/valida_duplicados.txt';
  --SET DEBUG FILE TO '/ds/safreviv_int/BD/integra_nuevo_modelo.trace';
  --TRACE ON;
   
LET v_tmp_cza_tipo_registro = "0";
LET v_tmp_cza_id_servicio = "0";
LET v_tmp_cza_id_operacion = "0";
LET v_tmp_cza_tipo_entidad_origen = "0";
LET v_tmp_cza_cve_entidad_origen = "0";
LET v_tmp_cza_tipo_entidad_destino = "0";
LET v_tmp_cza_clave_entidad_destino = "0";
LET v_tmp_cza_fecha_presentacion = "";
LET v_tmp_cza_consecutivo_lote_dia = 0;
LET v_tmp_cza_periodo_unificacion = "0";
LET v_tmp_cza_filler = "0";
LET v_secuencia_encabezado = 0;
LET v_secuencia_unificador = 0;
LET v_secuencia_unificados = 0;
LET v_secuencia_sumario = 0;
LET v_enc_fecha_presentacion = NULL;
LET v_resultado = 0;
LET sql_err = NULL;
LET isam_err = NULL;
LET err_txt = NULL;
LET v_total_unificadores = 0;
LET v_total_unificados = 0;
LET v_unificador_valido = 0;
LET v_unificado_valido = 0;
LET v_det_resultado_operacion = 1;
LET v_marca_inhabilita_uni = 0;
LET v_referencia_inahbilita_uni = 0;
LET v_id_dor_procedencia = 0;
LET v_folio_uni_det = 0;
LET v_id_unificado_confronta = 0;
LET v_des_id_unificador = 0;
LET v_des_dor_id_derechohabiente = 0;
LET v_des_dor_estado_unificacion = 0;
LET v_des_id_unificado = 0;
LET v_des_ado_id_derechohabiente = 0;
LET v_des_ado_estado_unificacion = 0;
LET v_rd_id_pre_unificador = 0;
LET v_rd_folio_lote = 0;
LET v_rd_id_pre_unificado = 0;
LET v_rd_folio_lote_ado = 0;
LET v_act_total_actualizacion  = 0;
LET v_ind_act_procedencia = 0;
LET v_ind_actualizacion = 0;
LET v_tot_procedencia = 0;


--#ENCABEZADO
FOREACH
   SELECT cza_tipo_registro,
          cza_id_servicio,
          cza_id_operacion,
          cza_tipo_entidad_origen,
          cza_cve_entidad_origen,
          cza_tipo_entidad_destino,
          cza_clave_entidad_destino,
          cza_fecha_presentacion,
          cza_consecutivo_lote_dia,
          cza_periodo_unificacion
   INTO   v_tmp_cza_tipo_registro,
          v_tmp_cza_id_servicio,
          v_tmp_cza_id_operacion,
          v_tmp_cza_tipo_entidad_origen,
          v_tmp_cza_cve_entidad_origen,
          v_tmp_cza_tipo_entidad_destino,
          v_tmp_cza_clave_entidad_destino,
          v_tmp_cza_fecha_presentacion,
          v_tmp_cza_consecutivo_lote_dia,
          v_tmp_cza_periodo_unificacion
   FROM   safre_tmp:tmp_cza_notif_ctas_unificar

   --Recupera la referencia para insertar rechazos
   SELECT seq_uni_cza_unificacion.NEXTVAL
   INTO   v_secuencia_encabezado
   FROM   systables
   WHERE  tabid = 1;

   IF (v_tmp_cza_tipo_registro <> "01" OR v_tmp_cza_tipo_registro IS NULL) THEN
      -- ERROR de encabezado.
      EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                               1,
                                               1,
                                               v_secuencia_encabezado,
                                               "ET",
                                               10,
                                               v_tmp_cza_tipo_registro);
   END IF

   -- Corrige fecha de YYYYMMDD a MMDDYYY
   EXECUTE PROCEDURE sp_cambia_formato_fecha(p_proceso_cod,
                                             v_tmp_cza_fecha_presentacion)
           INTO v_resultado,
                v_enc_fecha_presentacion;

   IF(v_enc_fecha_presentacion > TODAY) OR (v_enc_fecha_presentacion IS NULL)THEN
      -- ERROR de encabezado.
      EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                               1,
                                               1,
                                               v_secuencia_encabezado,
                                               "EF",
                                               11,
                                               v_enc_fecha_presentacion);
   END IF
   
   INSERT INTO uni_cza_unificacion(id_czaunifica,
                                   folio_unificacion,
                                   tipo_registro,
                                   servicio,
                                   operacion,
                                   tipo_entidad_origen,
                                   clave_entidad_origen,
                                   tipo_entidad_destino,
                                   clave_entidad_destino,
                                   f_presentacion,
                                   consecutivo_dia,
                                   periodo_unificacion)
          VALUES(v_secuencia_encabezado,
                 p_folio,
                 v_tmp_cza_tipo_registro,
                 v_tmp_cza_id_servicio,
                 v_tmp_cza_id_operacion,
                 v_tmp_cza_tipo_entidad_origen,
                 v_tmp_cza_cve_entidad_origen,
                 v_tmp_cza_tipo_entidad_destino,
                 v_tmp_cza_clave_entidad_destino,
                 v_enc_fecha_presentacion,
                 v_tmp_cza_consecutivo_lote_dia,
                 v_tmp_cza_periodo_unificacion);
END FOREACH;
--/Termina encabezado

--#UNIFICADORES
FOREACH
   SELECT tipo_registro                   ,
          contador_servicio               ,
          tipo_entidad_solicita           ,
          clave_entidad_solicita          ,
          tipo_entidad_localiza           ,
          clave_entidad_localiza          ,
          curp_unificador                 ,
          nss_unificador                  ,
          rfc_unificador                  ,
          ap_paterno_unificador           ,
          ap_materno_unificador           ,
          nombre_unificador               ,
          nombre_imss_unificador          ,
          sexo_unificador                 ,
          entidad_nacimiento_unificador   ,
          fecha_nacimiento_unificador     ,
          tipo_docto_probatorio           ,
          clave_afore_receptora_cta       ,
          no_cuentas_asociadas            ,
          st_convocatoria_aclaracion      ,
          id_movimiento                   ,
          clave_afore_receptora_aclaracion,
          id_credito_infonavit            ,
          indicador_procedencia           ,
          diagnostico_no_procedente                     
   INTO   v_det_tipo_registro,
          v_det_contador_servicio,
          v_det_tipo_entidad_solicitante,
          v_det_cve_entidad_solicitante,
          v_det_tipo_entidad_unificador,
          v_det_clave_entidad_unificador,
          v_det_curp_unificador,
          v_det_nss_unificador,
          v_det_rfc_unificador,
          v_det_paterno_unificador,
          v_det_materno_unificador,
          v_det_nombre_unificador,
          v_det_nombre_imssunificador,
          v_det_sexo_unificador,
          v_det_entidad_nacunificador,
          v_det_fec_nac_unificador,
          v_det_tpo_docto_probatorio,
          v_det_clave_afore_receptora,
          v_det_numero_cuentasasoc,
          v_det_estatus_convocatoria,
          v_det_ident_movimiento,
          v_det_cve_afore_aclaracion,
          v_det_id_credito_infonavit,
          v_det_indicador_procedencia,
          v_det_diagnostico_no_procedente     
   FROM   safre_tmp:tmp_det_cuenta_unificadora

   --Meter la validación si se trata de unificación inversa.
   --si es inversa desmarcar la 150 y continua normal       
   SELECT id_derechohabiente 
   INTO   v_id_derechohabiente_unificador
   FROM   afi_derechohabiente 
   WHERE  nss = v_det_nss_unificador;
   
   LET v_unificador_valido = 1;
   
   IF v_id_derechohabiente_unificador IS NOT NULL THEN
      IF v_det_estatus_convocatoria = 3 THEN 
         SELECT marca,
                n_referencia
         INTO   v_marca_inhabilita_uni,
                v_referencia_inahbilita_uni
         FROM   sfr_marca_activa 
         WHERE  id_derechohabiente = v_id_derechohabiente_unificador
         AND    marca = 150;
         
         IF v_marca_inhabilita_uni = 150 THEN
            EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente_unificador,
                                                 v_marca_inhabilita_uni,
                                                 v_referencia_inahbilita_uni,
                                                 40,
                                                 0,
                                                 p_usuario_cod,
                                                 p_proceso_cod)
            INTO v_res_desmarca;
         END IF
      END IF   
   END IF
   
   IF v_det_indicador_procedencia = 2 THEN
      --Buscar en historica de indicadores procedencia
      FOREACH 
         SELECT nss_unificado
         INTO   v_det2_nss_unificado
         FROM   safre_tmp:tmp_det_cuenta_unificada
         WHERE  nss_unificador = v_det_nss_unificador
      
         SELECT a.id_derechohabiente, 
                a.id_unificador     ,
                a.folio_unificacion 
         INTO   v_id_dh_det,
                v_id_unificador_det,
                v_folio_uni_detalle
         FROM   uni_det_unificador a, uni_det_unificado b
         WHERE  a.nss_unificador = v_det_nss_unificador
         AND    a.estado_familia = 1
         AND    a.ind_procedencia  = 0
         AND    b.id_unificador = a.id_unificador
         AND    b.nsscta1 = v_det2_nss_unificado;

         IF v_id_dh_det IS NOT NULL THEN 
            SELECT id_derechohabiente
            INTO   v_id_dh_procedencia
            FROM   uni_det_procedencia
            WHERE  id_derechohabiente = v_id_derechohabiente_unificador
            AND    folio_unificacion  = v_folio_uni_detalle
            AND    ind_procedencia    = 0
            AND    id_unificador      = v_id_unificador_det; 

            IF v_id_dh_procedencia IS NOT NULL THEN
               --Actualizar detalles, estado familia y diagnostico nuevo rechao por respuesta 
               UPDATE uni_det_unificador 
               SET    estado_familia  = 2,
                      ind_procedencia = 2,
                      diagnostico     = 42 --Rechazo indicador de procedencia = 2
               WHERE  id_derechohabiente = v_id_dh_det
               AND    id_unificador      = v_id_unificador_det
               AND    folio_unificacion  = v_folio_uni_detalle;
               --Actualizar historica de indicadores procedencia que llegó como rechazo
               UPDATE uni_det_procedencia
               SET    folio_resp_confronta = p_folio,
               	     ind_procedencia      = v_det_indicador_procedencia
               WHERE  id_unificador	      = v_id_unificador_det
               AND    folio_unificacion    = v_folio_uni_detalle
               AND    ind_procedencia      = 0;
               

               --Si existe desmarcar unificador y unificado
               EXECUTE FUNCTION fn_desmarca_cuenta (v_id_derechohabiente_unificador,
                                                    501, 
                                                    v_id_unificador_det,
                                                    40,
                                                    0,
                                                    p_usuario_cod,
                                                    p_proceso_cod)
               INTO v_res_desmarca;
               
               LET v_total_unificadores = v_total_unificadores + 1;
               
               FOREACH
                  SELECT id_derechohabiente,
                         id_unificado
                  INTO   v_id_dh_det_ado,
                         v_id_unificado_det
                  FROM   uni_det_unificado 
                  WHERE  id_unificador = v_id_unificador_det
                  
                  EXECUTE FUNCTION fn_desmarca_cuenta (v_id_dh_det_ado,
                                                       502, 
                                                       v_id_unificado_det,
                                                       40,
                                                       0,
                                                       p_usuario_cod,
                                                       p_proceso_cod)
                                                       
                  INTO v_res_desmarca;
                 
                  UPDATE uni_det_unificado 
                  SET    diagnostico     = 42 --Rechazo indicador de procedencia = 2
                  WHERE  id_derechohabiente = v_id_dh_det_ado
                  AND    id_unificador      = v_id_unificador_det
                  AND    folio_unificacion  = v_folio_uni_detalle;                                                          
                  
                  LET v_total_unificados = v_total_unificados + 1;
               END FOREACH;
            END IF
         END IF
      END FOREACH;
      CONTINUE FOREACH;
   END IF

   --Recupera la referencia para insertar rechazos
   SELECT seq_uni_det_unificador.NEXTVAL
   INTO   v_secuencia_unificador
   FROM   systables
   WHERE  tabid = 1;
   
   LET v_diagnostico_unificador    = 1; --Aceptada
   LET v_diagnostico_rechazo       = 1;
   LET v_estado_familia_unificador = 1;
  
   --trace "Valida tipo de registro para el registro inicial 02";
   LET err_txt = "Valida tipo de registro para el registro inicial 02";
   
   IF (v_det_tipo_registro <> "02" OR v_det_tipo_registro IS NULL) THEN
      -- ERROR de detalle.
      EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                               2,
                                               2,
                                               v_secuencia_unificador,
                                               "No",
                                               12,
                                               v_det_tipo_registro);
                                               
      LET v_diagnostico_unificador = 2; -- Rechazada
      LET v_diagnostico_rechazo = 12;   -- TIPO DE REGISTRO INCORRECTO PARA EL DETALLE UNIFICADOR
      LET v_estado_familia_unificador =  2;
   END IF

   -- Corrige fecha de YYYYMMDD a MMDDYYY
   EXECUTE PROCEDURE sp_cambia_formato_fecha(p_proceso_cod,
                                             v_det_fec_nac_unificador)
           INTO v_resultado,
                v_det_date_nac_unificador;

   --validar resultado
   IF v_resultado <> 0 THEN 
      LET v_det_date_nac_unificador = NULL;
   END IF

   -- Valida el id_derechohabiente
   --trace "En id_derechohabiente, no existe";
   LET err_txt = "En id_derechohabiente, no existe " || v_det_fec_nac_unificador || " - " || v_id_derechohabiente_unificador ;

   IF(v_id_derechohabiente_unificador IS NULL)THEN
      -- ERROR de detalle.
      EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                               2,
                                               2,
                                               v_secuencia_unificador,
                                               "No",
                                               13,
                                               v_det_nss_unificador);

      LET v_diagnostico_unificador = 2; -- Rechazada   
      LET v_diagnostico_rechazo = 13;   -- NO EXISTE EL DERECHOHABIENTE UNIFICADOR
      LET v_estado_familia_unificador =  2;

      LET v_unificador_valido = 2;
   ELSE   
      --Se integra el indicador de procedencia 
      IF  (v_det_indicador_procedencia = 0) OR (v_det_indicador_procedencia = 1) THEN
         IF v_det_indicador_procedencia = 1 THEN 
               LET v_id_dor_procedencia = 0;
            FOREACH 
               SELECT nss_unificado
               INTO   v_det2_nss_unificado
               FROM   safre_tmp:tmp_det_cuenta_unificada
               WHERE  nss_unificador = v_det_nss_unificador

               SELECT a.id_unificador
               INTO   v_id_dor_procedencia
               FROM   uni_det_unificador a, uni_det_unificado b
               WHERE  a.id_derechohabiente = v_id_derechohabiente_unificador
               AND    a.estado_familia = 1
               AND    a.ind_procedencia  = 0
               AND    b.id_unificador = a.id_unificador
               AND    b.nsscta1 = v_det2_nss_unificado;
                           
               IF v_id_dor_procedencia > 0 THEN 
                  --Actualizar historica de indicadores procedencia que llegó como rechazo
                  UPDATE uni_det_procedencia
                  SET    folio_resp_confronta = p_folio,
                  	      ind_procedencia      = v_det_indicador_procedencia
                  WHERE  id_unificador	       = v_id_dor_procedencia
                  AND    folio_unificacion    = v_folio_uni_det
                  AND    ind_procedencia      = 0;
               
                  --Actualizar detalles, estado familia y diagnostico nuevo rechazo por respuesta 
                  UPDATE uni_det_unificador 
                  SET    estado_familia  = 1,
                         ind_procedencia = 1,
                         diagnostico     = 30 --Rechazo indicador de procedencia = 2
                  WHERE  id_derechohabiente = v_id_derechohabiente_unificador
                  AND    id_unificador      = v_id_dor_procedencia
                  AND    folio_unificacion  = v_folio_uni_det;

                  FOREACH
                     SELECT id_unificado
                     INTO   v_id_unificado_confronta
                     FROM   uni_det_unificado
                     WHERE  id_unificador = v_id_dor_procedencia
                     AND    folio_unificacion  = v_folio_uni_det
                     
                     UPDATE uni_det_unificado 
                     SET    diagnostico        = 30
                     WHERE  id_unificador      = v_id_dor_procedencia
                     AND    folio_unificacion  = v_folio_uni_det
                     AND    id_unificado       = v_id_unificado_confronta;

                     LET v_total_unificados = v_total_unificados + 1;
                  END FOREACH

                  LET v_total_unificadores = v_total_unificadores + 1;
               
                  CONTINUE FOREACH;
               END IF
            END FOREACH   
         END IF

         IF v_estado_familia_unificador <> 2 THEN   
               INSERT INTO uni_det_procedencia(id_derechohabiente,
                                               id_unificador     ,
                                               folio_unificacion ,
                                               ind_procedencia
                                               )
                      VALUES (v_id_derechohabiente_unificador ,
                              v_secuencia_unificador ,        
                              p_folio ,
                              v_det_indicador_procedencia
                              );
            --actualizar CURP en afi_derechohabiente 
            IF v_det_indicador_procedencia     = 1 THEN 
               LET v_det2_diagnostico_uni      = 1; --*** aceptado confronta electrónica   
               LET v_det2_resultado_operacion  = "01";
               LET v_diagnostico_rechazo       = 30;
               LET v_estado_familia_unificador = 1;
               LET v_diagnostico_unificadas    = 1;
            ELSE
               LET v_det2_diagnostico_uni      = 0; --*** aceptado confronta electrónica   
               LET v_det2_resultado_operacion  = 0;
               LET v_estado_familia_unificador = 1;
            END IF
         END IF 
      END IF
      --
      SELECT marca
      INTO   v_marca_activa
      FROM   sfr_marca_activa
      WHERE  marca = 501
      AND    id_derechohabiente = v_id_derechohabiente_unificador;
      
      --## Se consulta si la cuenta ya tiene marca activa
      -- y validar indicador de procedencia 0 en la tabla historica 
      IF v_marca_activa = 501 THEN
         -- Si la cuenta ya tiene marca se rechaza
         {EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_secuencia_unificador,
                                                  "EM",
                                                  41,
                                                  v_det_nss_unificador);
                                                  
         LET v_diagnostico_unificador = 2; -- Rechazada   
         LET v_diagnostico_rechazo =   40; -- ERROR AL MARCAR AL UNIFICADOR 
         LET v_estado_familia_unificador =  2;}
      ELSE          
         IF v_unificador_valido = 1 THEN
         --trace "Valores para marcar : " || v_id_derechohabiente_unificador || " - " || v_secuencia_unificador || " - " ||p_folio || " - " ||p_usuario_cod || " - " ||p_proceso_cod;
            --Si la cuenta no tiene marca, se marca.
            EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_unificador,
                                             501,  -- marca de unificador IMSS
                                             v_secuencia_unificador,
                                             p_folio,
                                             0,    -- estado marca
                                             0,    -- codigo de rechazo
                                             0,    -- marca de la causa
                                             NULL, -- fecha de la causa
                                             p_usuario_cod,
                                             p_proceso_cod)
            INTO v_estado_marca;
            --trace "Resultado marca :" || v_estado_marca;
            
            IF v_estado_marca <> 0 THEN 
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        2,
                                                        2,
                                                        v_secuencia_unificador,
                                                        "EM",
                                                        40,
                                                        v_det_nss_unificador);
                                                        
               LET v_diagnostico_unificador    = 2; -- Rechazada   
               LET v_diagnostico_rechazo       = 40;
               LET v_estado_familia_unificador = 2;
      
            ELSE
               --Se consulta si el unificador tiene marca 511 (Prospecto UNIFICADOR)
               FOREACH   
                  SELECT marca, 
                         n_referencia
                  INTO   v_marca_afi, 
                         v_n_referencia_afi
                  FROM   sfr_marca_activa
                  WHERE  marca = 511
                  AND    id_derechohabiente = v_id_derechohabiente_unificador
                  
                  IF v_marca_afi = 511 THEN
                     EXECUTE FUNCTION fn_desmarca_cuenta (v_id_derechohabiente_unificador,
                                                          v_marca_afi,
                                                          v_n_referencia_afi,
                                                          0,
                                                          501,
                                                          p_usuario_cod,
                                                          p_proceso_cod)
                     INTO v_res_desmarca;
      
                     --valida resultado desmarca
                     IF v_res_desmarca = 0 THEN 
                        UPDATE uni_pre_unificador 
                        SET    estado =  10
                        WHERE  id_derechohabiente = v_id_derechohabiente_unificador
                        AND    estado =  1;
                     ELSE 
                        --si es rechazo insertar
                        EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                                 2,
                                                                 2,
                                                                 v_secuencia_unificador,
                                                                 "EM",
                                                                 40,
                                                                 v_det_nss_unificador);
      
                        LET v_diagnostico_unificador    = 2; -- Rechazada   
                        LET v_diagnostico_rechazo       = 40;
                        LET v_estado_familia_unificador = 2;
                     END IF
                   END IF
               END FOREACH;

               --Inserta información para notificaciones UNIFICADOR
               INSERT INTO uni_notifica_op21 (id_referencia,
                                              folio_unificacion,
                                              id_derechohabiente,
                                              tipo_nss,
                                              ind_notificado)
                                      VALUES (v_secuencia_unificador,
                                              p_folio,
                                              v_id_derechohabiente_unificador,
                                              1, --Unificador
                                              0); --Pendiente
            END IF
         END IF
      END IF
   END IF
   --trace "Al insertar encabezado en uni_det_unificador";
   LET err_txt = "Al insertar en uni_det_unificador";
   
   INSERT INTO uni_det_unificador(id_unificador,           
                                  folio_unificacion,       
                                  id_derechohabiente,      
                                  tipo_registro,           
                                  contador_servicio,       
                                  tipo_entidad_solicitante,
                                  cve_entidad_solicitante, 
                                  tipo_entidad_unificador, 
                                  clave_entidad_unificador,
                                  curp_unificador,         
                                  nss_unificador,          
                                  rfc_unificador,          
                                  paterno_unificador,      
                                  materno_unificador,      
                                  nombre_unificador,       
                                  nombre_imssunificador,   
                                  sexo_unificador,         
                                  entidad_nacunificador,   
                                  f_nac_unificador,        
                                  tpo_docto_probatorio,    
                                  clave_afore_receptora,   
                                  numero_cuentasasoc,      
                                  estatus_convocatoria,    
                                  resultado_operacion,     
                                  ident_movimiento,        
                                  estado_familia,           
                                  estado_unificacion,      
                                  diagnostico,             
                                  f_liquidacion,           
                                  f_notificacion,          
                                  folio_liquidacion,       
                                  ind_procedencia)         
          VALUES(v_secuencia_unificador, --id_unificador
                 p_folio,                                  
                 v_id_derechohabiente_unificador,                      
                 v_det_tipo_registro,                      
                 v_det_contador_servicio,                  
                 v_det_tipo_entidad_solicitante,           
                 v_det_cve_entidad_solicitante,            
                 v_det_tipo_entidad_unificador,            
                 v_det_clave_entidad_unificador,           
                 v_det_curp_unificador,                    
                 v_det_nss_unificador,                     
                 v_det_rfc_unificador,                     
                 v_det_paterno_unificador,                 
                 v_det_materno_unificador,                 
                 v_det_nombre_unificador,                  
                 v_det_nombre_imssunificador,              
                 v_det_sexo_unificador,                    
                 v_det_entidad_nacunificador,              
                 v_det_date_nac_unificador,                 
                 v_det_tpo_docto_probatorio,               
                 v_det_clave_afore_receptora,              
                 v_det_numero_cuentasasoc,                 
                 v_det_estatus_convocatoria,               
                 v_det_resultado_operacion,                
                 v_det_ident_movimiento,                     
                 v_estado_familia_unificador,
                 v_diagnostico_unificador,
                 v_diagnostico_rechazo,
                 NULL,                                     
                 NULL,                                     
                 NULL,
                 v_det_indicador_procedencia
                 );

   SELECT COUNT(*)
   INTO   v_existe_unificador
   FROM   uni_det_unificador
   WHERE  folio_unificacion =  p_folio 
   AND    nss_unificador = v_det_nss_unificador;      
   
   IF v_existe_unificador > 1 THEN
      -- ERROR de detalle.
      EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                               2,
                                               2,
                                               v_secuencia_unificados,
                                               "EM",
                                               40,
                                               v_det_nss_unificador);          
                                               
      LET v_diagnostico_unificador = 2; -- Rechazada                                       
      LET v_diagnostico_rechazo = 40;   -- LA MARCA UNIFICADOR IMSS NO ESTA ACTIVA         
      LET v_estado_familia_unificador =  2;                                                
                                                        
      UPDATE uni_det_unificador
         SET estado_unificacion = v_diagnostico_unificador,
             diagnostico        = v_diagnostico_rechazo,
             estado_familia     = v_estado_familia_unificador
      WHERE  id_unificador = v_secuencia_unificador --id_unificador
      AND    folio_unificacion = p_folio;
      
      SELECT marca, 
             n_referencia
      INTO   v_marca_afi, 
             v_n_referencia_afi
      FROM   sfr_marca_activa
      WHERE  marca = 511
      AND    id_derechohabiente = v_id_derechohabiente_unificador ;
      
      EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente_unificador,
                                            v_marca_afi,
                                            v_n_referencia_afi,
                                            p_folio);

      EXECUTE FUNCTION fn_desmarca_cuenta (v_id_derechohabiente_unificador,
                                           501,
                                           v_secuencia_unificador,
                                           0,
                                           511,
                                           p_usuario_cod,
                                           p_proceso_cod)
       INTO v_res_desmarca;

       IF v_res_desmarca <> 0 THEN 
          EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                   2,
                                                   2,
                                                   v_secuencia_unificados,
                                                   "ED",
                                                   43,
                                                   v_det_nss_unificador);          
                                                   
          LET v_diagnostico_unificador = 2; -- Rechazada                                       
          LET v_diagnostico_rechazo = 43;   -- LA MARCA UNIFICADOR IMSS NO ESTA ACTIVA         
          LET v_estado_familia_unificador =  2;          
       END IF
   END IF

--#UNIFICADOS
   --trace "Al recuperar datos detalle tmp_det_cta_unificadas_op21";
   LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadas_op21";

   FOREACH
      SELECT tipo_registro,
             contador_servicio,
             nss_unificador,
             tipo_entidad_unificado,
             clave_entidad_unificado,
             curp_unificado,
             nss_unificado,
             rfc_unificado,
             ap_paterno_unificado,
             ap_materno_unificado,
             nombre_unificado,
             nombre_imss_unificado,
             sexo_unificado,
             entidad_nacimiento_unificado,
             fecha_nacimiento_unificado,                
             st_convocatoria_aclaracion,
             clave_afore_receptora_aclaracion,
             id_credito_infonavit_unificado
      INTO   v_det2_tipo_registro,
             v_det2_contador_servicio,
             v_det2_nss_unificador,
             v_det2_tipo_entidad,
             v_det2_clave_entidad,
             v_det2_curp,
             v_det2_nss_unificado,
             v_det2_rfc,
             v_det2_apellido_paterno,
             v_det2_apellido_materno,
             v_det2_nombre,
             v_det2_nombre_imss,
             v_det2_sexo,
             v_det2_ent_nacimiento,
             v_det2_fecha_nacimiento,
             v_det2_estatus_convocatoria,
             v_det2_afore_aclaracion,
             v_det2_credito43bis
      FROM   safre_tmp:tmp_det_cuenta_unificada
      WHERE  nss_unificador = v_det_nss_unificador
      
      --Recupera la secuencia 
      SELECT seq_uni_det_unificado.NEXTVAL
      INTO   v_secuencia_unificados
      FROM   systables
      WHERE  tabid = 1;

      LET v_unificado_valido         = 1;
      LET v_diagnostico_unificadas   = 1; --Aceptadas
      LET v_estado_familia_unificado = 1;
      LET v_det2_diagnostico_uni      = 1; --*** aceptado confronta electrónica   
      LET v_det2_resultado_operacion  = "01";

      IF v_det_indicador_procedencia = 1 THEN 
         LET v_diagnostico_rechazo      = 30;
      ELSE
         LET v_diagnostico_rechazo      = 1;
      END IF

      --trace "Valida tipo de registro para el registro inicial 03";
      LET err_txt = "Valida tipo de registro para el registro inicial 03";
      
      IF (v_det2_tipo_registro <> "03" OR v_det2_tipo_registro IS NULL) THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  3,
                                                  3,
                                                  v_secuencia_unificados,
                                                  "No",
                                                  14,
                                                  v_det2_tipo_registro);
                                                  
         LET v_diagnostico_unificadas = 2; -- Rechazada
         LET v_diagnostico_rechazo = 14;   -- TIPO DE REGISTRO INCORRECTO PARA EL DETALLE UNIFICADO
         LET v_estado_familia_unificado =  2;
      END IF
      
      LET err_txt = "Recupera el id derechohabiente unificado";
      
      --Recupera el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_unificado
      FROM   afi_derechohabiente
      WHERE  nss = v_det2_nss_unificado;
      
      -- Valida el id_derechohabiente
      LET err_txt = "En id_derechohabiente, no existe en unificado" || " - " || v_det2_nss_unificado  || " - " ||  v_det2_fecha_nacimiento;
      
      IF(v_id_derechohabiente_unificado IS NULL)THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  3,
                                                  3,
                                                  v_secuencia_unificados,
                                                  "No",
                                                  15,
                                                  v_det2_nss_unificado);
                                                  
         LET v_diagnostico_unificadas = 2; -- Rechazada
         LET v_diagnostico_rechazo = 15;   -- NO EXISTE EL DERECHOHABIENTE UNIFICADO
         LET v_estado_familia_unificado = 2;
         LET v_unificado_valido = 2;
      END IF
      
      SELECT marca
      INTO   v_marca_activa
      FROM   sfr_marca_activa
      WHERE  marca = 502
      AND    id_derechohabiente = v_id_derechohabiente_unificado;         

      --## Se consulta si la cuenta ya tiene marca activa
      IF v_marca_activa = 502 THEN
         -- Si la cuenta ya tiene marca se rechaza
         {   EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_secuencia_unificados,
                                                     "No",
                                                     41,
                                                     v_det2_nss_unificado);
            LET v_diagnostico_unificadas   = 2;  -- Rechazada
            LET v_diagnostico_rechazo      = 41; -- ERROR AL MARCAR EL UNIFICADO
            LET v_estado_familia_unificado = 2;}
      ELSE
         IF v_unificado_valido = 1 THEN
            --Si la cuenta no tiene marca, se marca.
            EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_unificado,
                                             502,  -- marca de unificado IMSS
                                             v_secuencia_unificados,
                                             p_folio,
                                             0,    -- estado marca
                                             0,    -- codigo de rechazo
                                             0,    -- marca de la causa
                                             NULL, -- fecha de la causa
                                             p_usuario_cod,
                                             p_proceso_cod)
            INTO v_estado_marca;

            IF v_estado_marca <> 0 THEN 
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        3,
                                                        3,
                                                        v_secuencia_unificados,
                                                        "EM",
                                                        41,
                                                        v_det2_nss_unificado);
               LET v_diagnostico_unificadas = 2; -- Rechazada
               LET v_diagnostico_rechazo = 41;   -- LA MARCA UNIFICADO IMSS NO ESTA ACTIVA
               LET v_estado_familia_unificado = 2;

            ELSE
               --Se consulta si el unificador tiene marca 512
               FOREACH
                  SELECT marca, 
                         n_referencia
                  INTO   v_marca_afi_ado, 
                         v_n_referencia_afi_ado
                  FROM   sfr_marca_activa
                  WHERE  marca = 512
                  AND    id_derechohabiente = v_id_derechohabiente_unificado
                  
                  IF v_marca_afi_ado = 512 THEN      
                    EXECUTE FUNCTION fn_desmarca_cuenta (v_id_derechohabiente_unificado,
                                                         v_marca_afi_ado,
                                                         v_n_referencia_afi_ado,
                                                         0,
                                                         502,
                                                         p_usuario_cod,
                                                         p_proceso_cod)
                                                         
                    INTO v_res_desmarca;
                    
                    --validar resultado desmarca
                    IF v_res_desmarca = 0 THEN 
                       UPDATE uni_pre_unificado
                       SET    estado =  10
                       WHERE  id_derechohabiente = v_id_derechohabiente_unificado;
                    ELSE
                       EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                                3,
                                                                3,
                                                                v_secuencia_unificados,
                                                                "ED",
                                                                43,
                                                                v_det2_nss_unificado);
                       LET v_diagnostico_unificadas = 2; -- Rechazada
                       LET v_diagnostico_rechazo    = 43;   
                       LET v_estado_familia_unificado = 2;
                    END IF    
                  END IF
                END FOREACH;
                
               --Inserta información para notificaciones UNIFICADO
               INSERT INTO uni_notifica_op21 (id_referencia,
                                              folio_unificacion,
                                              id_derechohabiente,
                                              tipo_nss,
                                              ind_notificado)
                                      VALUES (v_secuencia_unificados,
                                              p_folio,
                                              v_id_derechohabiente_unificado,
                                              2, --Unificador
                                              0); --Pendiente
            END IF
            LET v_total_unificados = v_total_unificados + 1;
         END IF
      END IF
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(p_proceso_cod,
                                                v_det2_fecha_nacimiento)
              INTO v_resultado,
                   v_det2_fecha_nacimiento_res;
                   
      --validar resultado
      IF v_resultado <> 0 THEN 
         LET v_det2_fecha_nacimiento_res = NULL;
      END IF
      
      INSERT INTO uni_det_unificado(id_unificado,
                           id_unificador,
                           folio_unificacion,
                           id_derechohabiente,
                           tipo_registro,
                           contador_serviciocta1,
                           tipo_entidadcta1,
                           cve_entidadcta1,
                           curpcta1,
                           nsscta1,
                           rfccta1,
                           paternocta1,
                           maternocta1,
                           nombrecta1,
                           nombre_imsscta1,
                           sexocta1,
                           ent_nacimiento,
                           f_nacimientocta1,
                           estatus_convocatoria,
                           nsscta2,
                           diagnostico_uni,
                           resultado_operacion,
                           afore_aclaracion,
                           credito43bis,
                           estado_unificacion,
                           diagnostico)
             VALUES(v_secuencia_unificados,
                    v_secuencia_unificador, --id_unificador
                    p_folio,                                  
                    v_id_derechohabiente_unificado,
                    v_det2_tipo_registro,
                    v_det2_contador_servicio,
                    v_det2_tipo_entidad,
                    v_det2_clave_entidad,
                    v_det2_curp,
                    v_det2_nss_unificado,
                    v_det2_rfc,
                    v_det2_apellido_paterno,
                    v_det2_apellido_materno,
                    v_det2_nombre,
                    v_det2_nombre_imss,
                    v_det2_sexo,
                    v_det2_ent_nacimiento,
                    v_det2_fecha_nacimiento_res,
                    v_det2_estatus_convocatoria,
                    v_det2_nss_unificador,
                    v_det2_diagnostico_uni,
                    v_det2_resultado_operacion,
                    v_det2_afore_aclaracion,
                    v_det2_credito43bis,
                    v_diagnostico_unificadas,
                    v_diagnostico_rechazo);
   
      -- Verifica si existe unificado con mismo unificador con el folio asignado al proceso
      SELECT COUNT(*)
      INTO   v_existe_unificado
      FROM   uni_det_unificado
      WHERE  folio_unificacion =  p_folio 
      AND    nsscta1 = v_det2_nss_unificado
      AND    nsscta2 = v_det2_nss_unificador;      
      
      IF v_existe_unificado > 1 THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  3,
                                                  3,
                                                  v_secuencia_unificados,
                                                  "No",
                                                  40,
                                                  v_det2_nss_unificado);
                                                  
         LET v_diagnostico_unificador = 2; -- Rechazada                                       
         LET v_diagnostico_rechazo = 44;   -- UNIFICADO DUPLICADO CON UNIFICADOR EN MISMO FOLIO
         LET v_estado_familia_unificador =  2;                                                               
         
         UPDATE uni_det_unificado
            SET estado_unificacion = 2,
                diagnostico        = 40
         WHERE  id_unificador = v_secuencia_unificador --id_unificador
         AND    id_unificado  = v_secuencia_unificados;
         
         SELECT marca, 
                n_referencia
         INTO   v_marca_afi_ado, 
                v_n_referencia_afi_ado
         FROM   sfr_marca_activa
         WHERE  marca = 512
         AND    id_derechohabiente = v_id_derechohabiente_unificado;

         --desmarca validacion 502  tmbn aplica 501 
         EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente_unificado,
                                               v_marca_afi_ado, 
                                               v_n_referencia_afi_ado,
                                               p_folio);
                                               
         EXECUTE FUNCTION fn_desmarca_cuenta (v_id_derechohabiente_unificado,
                                              502,
                                              v_secuencia_unificados,
                                              0,
                                              512,
                                              p_usuario_cod,
                                              p_proceso_cod)
          INTO v_res_desmarca;
         
          IF v_res_desmarca <> 0 THEN 
             EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                      2,
                                                      2,
                                                      v_secuencia_unificados,
                                                      "ED",
                                                      43,
                                                      v_det_nss_unificador);          
                                                      
             LET v_diagnostico_unificador = 2; -- Rechazada                                       
             LET v_diagnostico_rechazo = 43;   -- LA MARCA UNIFICADOR IMSS NO ESTA ACTIVA         
             LET v_estado_familia_unificador =  2;          
          END IF
      END IF
      
      --trace "Al actualizar el estado de la familia";
      LET err_txt = "Al actualizar el estado de la familia";
      --trace "Valida para marcar la cuenta unificado";
      IF v_estado_familia_unificador = 1 THEN 
         IF v_estado_familia_unificado = 1 THEN
            LET v_estado_familia = 1;
         ELSE
            LET v_estado_familia = 2;
         END IF   
      ELSE
         LET v_estado_familia = 2;
      END IF
      
      --Actualiza estado de familia
      UPDATE uni_det_unificador
         SET estado_familia = v_estado_familia
      WHERE  id_unificador = v_secuencia_unificador
      AND    (estado_familia IS NULL
              OR estado_familia = 1
              OR estado_familia = 0);

      LET v_total_unificados = v_total_unificados + 1;
      LET v_existe_unificado = 0;
    END FOREACH; --Unificados       
   LET v_total_unificadores = v_total_unificadores + 1;
   LET v_existe_unificador = 0;
END FOREACH;  --Unificador 

FOREACH 
   SELECT id_unificador,
          id_derechohabiente,
          estado_unificacion
   INTO   v_des_id_unificador,
          v_des_dor_id_derechohabiente,
          v_des_dor_estado_unificacion             
   FROM   uni_det_unificador
   WHERE  folio_unificacion = p_folio
   AND    estado_familia = 2

   DELETE
   FROM   uni_det_procedencia 
   WHERE  id_derechohabiente = v_des_dor_id_derechohabiente
   AND    id_unificador      = v_des_id_unificador
   AND    folio_unificacion  = p_folio;
   
   IF v_des_dor_estado_unificacion = 1 THEN 
      --DESMARCAR UNIFICADOR 
      EXECUTE FUNCTION fn_desmarca_cuenta (v_des_dor_id_derechohabiente,
                                           501,
                                           v_des_id_unificador,
                                           30,
                                           0,
                                           p_usuario_cod,
                                           p_proceso_cod)
     INTO v_res_desmarca;

     IF v_res_desmarca = 0 THEN
        FOREACH
           SELECT id_pre_unificador,
                  folio_lote
           INTO   v_rd_id_pre_unificador,
                  v_rd_folio_lote
           FROM   uni_pre_unificador
           WHERE  id_derechohabiente = v_des_dor_id_derechohabiente
           AND    estado = 10

           ---REVERSAR DESMARCA DE 511
           EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente_unificador,
                                                  511,
                                                  v_rd_id_pre_unificador,
                                                  v_rd_folio_lote);
                                                  
           UPDATE uni_pre_unificador
           SET    estado = 1
           WHERE  id_pre_unificador = v_rd_id_pre_unificador
           AND    id_derechohabiente = v_des_dor_id_derechohabiente
           AND    estado = 10;
        END FOREACH;    
     END IF
   END IF 
   
   FOREACH
      SELECT id_unificado,
             id_derechohabiente,
             estado_unificacion
      INTO   v_des_id_unificado,
             v_des_ado_id_derechohabiente,
             v_des_ado_estado_unificacion
      FROM   uni_det_unificado
      WHERE  folio_unificacion = p_folio
      AND    id_unificador  = v_des_id_unificador
      
      IF v_des_ado_estado_unificacion = 1 THEN 
         --DESMARCAR UNIFICADO
         EXECUTE FUNCTION fn_desmarca_cuenta (v_des_ado_id_derechohabiente,
                                              502,
                                              v_des_id_unificado,
                                              30,
                                              0,
                                              p_usuario_cod,
                                              p_proceso_cod)
         INTO v_res_desmarca;
     
         IF v_res_desmarca = 0 THEN 
            ---REVERSAR DESMARCA DE 512
            FOREACH
               SELECT a.id_pre_unificado, 
                      b.folio_lote
               INTO   v_rd_id_pre_unificado,
                      v_rd_folio_lote_ado
               FROM   uni_pre_unificado a, 
                      uni_pre_unificador b 
               WHERE  a.id_pre_unificador  = b.id_pre_unificador
               AND    a.id_derechohabiente = v_des_ado_id_derechohabiente
               AND    a.estado = 10

               EXECUTE PROCEDURE sp_reversa_desmarca(v_des_ado_id_derechohabiente,
                                                     512,
                                                     v_rd_id_pre_unificado,
                                                     v_rd_folio_lote_ado);
                                                      
               UPDATE uni_pre_unificado
               SET    estado = 1
               WHERE  id_pre_unificado   = v_rd_id_pre_unificado
               AND    id_derechohabiente = v_des_dor_id_derechohabiente
               AND    estado = 10;
            END FOREACH;    
         END IF
      END IF   
   END FOREACH;
END FOREACH;

--#Sumario    
LET err_txt = "Al recuperar datos detalle tmp_sum_cta_unificar_op21";

FOREACH
   SELECT tipo_registro,
          cantidad_regs_detalle,
          total_unificadores,
          total_unificados
   INTO   v_sum_tipo_registro,
          v_sum_total_registro,
          v_sum_total_nss_unificador,
          v_sum_total_nss_unificados
   FROM   safre_tmp:tmp_sum_notif_ctas_unificar
   --Recupera la secuencia 
   SELECT seq_uni_sum_unificacion.NEXTVAL
   INTO   v_secuencia_sumario
   FROM   systables
   WHERE  tabid = 1;
   
   --trace "Valida tipo de registro para el registro inicial 09";
   LET err_txt = "Valida tipo de registro 09";
   
   IF (v_sum_tipo_registro <> "09" OR v_sum_tipo_registro IS NULL) THEN
      -- ERROR de encabezado.
      EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                               9,
                                               9,
                                               v_secuencia_sumario,
                                               "ES",
                                               16,
                                               v_sum_tipo_registro);
   END IF
   
   -- [Error]
   --trace "Al insertar encabezado en uni_sum_unificacion";
   LET err_txt = "Al insertar encabezado en uni_sum_unificacion";
   
   INSERT INTO uni_sum_unificacion(id_sum_unifica,
                                   folio_unificacion,
                                   tipo_registro,
                                   total_registro,
                                   total_nss_unificador,
                                   total_nss_unificados)
          VALUES(v_secuencia_sumario,
                 p_folio,
                 v_sum_tipo_registro,
                 v_sum_total_registro,
                 v_sum_total_nss_unificador,
                 v_sum_total_nss_unificados);
END FOREACH;

SELECT COUNT (*) 
INTO   v_total_unificadores
FROM   safre_tmp:tmp_det_cuenta_unificadora;

SELECT COUNT (*) 
INTO   v_total_unificados
FROM   safre_tmp:tmp_det_cuenta_unificada;

-- Se asigna el folio al archivo y se indica que ha sido integrado
LET err_txt = "Al actualizar glo_ctr_archivo"|| " - " ||p_proceso_cod||" - " || p_pid ;
UPDATE glo_ctr_archivo
SET    folio     = p_folio,
       estado    = 2
WHERE  nombre_archivo = p_nombre_archivo;

UPDATE bat_ctr_operacion 
   SET folio       = p_folio,
       nom_archivo = p_nombre_archivo
WHERE  proceso_cod = p_proceso_cod
AND    opera_cod   = 2
AND    pid         = p_pid;      

LET err_txt = " Registros: "||v_sum_total_registro;

UPDATE STATISTICS FOR TABLE uni_cza_unificacion;
UPDATE STATISTICS FOR TABLE uni_det_unificador;
UPDATE STATISTICS FOR TABLE uni_det_unificado;
UPDATE STATISTICS FOR TABLE uni_sum_unificacion;

SELECT COUNT(*)
INTO   v_act_total_actualizacion
FROM   uni_det_unificador
WHERE  folio_unificacion = p_folio;

IF v_act_total_actualizacion = 0 THEN
   FOREACH
      SELECT ind_procedencia
      INTO   v_ind_act_procedencia 
      FROM   uni_det_procedencia
      WHERE  folio_resp_confronta = p_folio
      AND    ind_procedencia in (1,2)
      ORDER BY 1
      
      IF v_ind_act_procedencia = 1 THEN 
         EXIT FOREACH;
      ELSE
         LET v_ind_actualizacion = 2;
      END IF
   END FOREACH;
END IF

RETURN v_resultado, 
       err_txt,
       v_total_unificadores,
       v_total_unificados,
       v_ind_actualizacion;
END FUNCTION

;


