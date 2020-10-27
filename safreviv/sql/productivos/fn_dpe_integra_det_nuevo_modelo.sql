






CREATE FUNCTION "safreviv".fn_dpe_integra_det_nuevo_modelo(p_usuario_cod    CHAR(20), 
                                   p_pid            DECIMAL(9,0), 
                                   p_nombre_archivo CHAR(40),
                                   p_folio          DECIMAL(10),
                                   p_proceso_cod    SMALLINT) 

  RETURNING INTEGER,
            CHAR(200),
            SMALLINT, 
            INTEGER,
            INTEGER,
            SMALLINT, 
            SMALLINT, 
            SMALLINT, 
            INTEGER

   DEFINE v_si_tipo_archivo                       SMALLINT;
   DEFINE v_si_contador                           SMALLINT;
   -- Saldo disponible para en nss
   DEFINE v_d_saldo_disponible_aivs               DECIMAL(16,6);
   DEFINE v_si_status_solicitud                   SMALLINT; -- {1 = Aceptada, 2 = rechazada, 3 = pendiente}
   DEFINE v_si_aceptada                           SMALLINT;
   DEFINE v_si_rechazada                          SMALLINT;
   DEFINE v_si_pendiente                          SMALLINT;
   -- Detalle temporal
   DEFINE v_tmp_reg_patronal                      CHAR(11)     ;
   DEFINE v_tmp_periodo_pago                      CHAR(6)      ;
   DEFINE v_tmp_rfc_trabajador                    CHAR(13)     ;
   DEFINE v_tmp_curp_trabajador                   CHAR(18)     ;
   DEFINE v_tmp_nombre                            CHAR(50)     ;
   DEFINE v_tmp_dias_cotiza_bim_devolver          DECIMAL(2,0) ;
   DEFINE v_tmp_imp_retiro_devolver               DECIMAL(9,2) ;
   DEFINE v_tmp_imp_actua_recargo_retiro          DECIMAL(9,2) ;
   DEFINE v_tmp_imp_censatia_vejez_patron         DECIMAL(9,2) ;
   DEFINE v_tmp_imp_censatia_vejez_trabajador     DECIMAL(9,2) ;
   DEFINE v_tmp_imp_actua_recargo_censa_vejez     DECIMAL(9,2) ;
   DEFINE v_tmp_imp_patronal_infonavit_devolver   DECIMAL(9,2) ;
   DEFINE v_tmp_apli_interes_vivienda             DECIMAL(16,6);
   DEFINE v_tmp_folio_sua_det                     CHAR(6)      ;
   DEFINE v_tmp_filer                             CHAR(113)    ;
   DEFINE v_tmp_nss                               CHAR(11)     ;


   -- Detalle registro '03'inserta
   DEFINE v_dpe_id_dpe_referencia                 DECIMAL(9,0);
   DEFINE v_dpe_referencia                        DECIMAL(9,0);
   DEFINE v_dpe_folio                             DECIMAL(9,0);
   -- [
   DEFINE v_dpe_reg_patronal_imss                 CHAR(11)     ;
   DEFINE v_dpe_id_derechohabiente                DECIMAL(9,0) ;
   DEFINE v_dpe_periodo_pago                      CHAR(6)      ;
   DEFINE v_dpe_rfc                               CHAR(13)     ;
   DEFINE v_dpe_curp                              CHAR(18)     ;
   DEFINE v_dpe_nombre                            CHAR(50)     ;
   DEFINE v_dpe_dias_cotizados                    SMALLINT     ;
   DEFINE v_dpe_imp_retiro_dev                    DECIMAL(16,6);
   DEFINE v_dpe_imp_act_retiro_dev                DECIMAL(16,6);
   DEFINE v_dpe_imp_cv_pat_dev                    DECIMAL(16,6);
   DEFINE v_dpe_imp_cv_trab_dev                   DECIMAL(16,6);
   DEFINE v_dpe_imp_act_cv_dev                    DECIMAL(16,6);
   DEFINE v_dpe_imp_viv_dev                       DECIMAL(16,6);
   DEFINE v_dpe_avis_viv_dev                      DECIMAL(16,6);
   DEFINE v_dpe_nss                               CHAR(11);
   DEFINE v_id_dpe_patron                         DECIMAL(9,0);
   -- ]
   DEFINE v_dpe_porcentaje_dev                    SMALLINT;
   DEFINE v_dpe_estado_solicitud                  SMALLINT;
   DEFINE v_dpe_diagnostico                       SMALLINT;

   -- Encabezado temporal
   DEFINE v_tmp_enc_tpo_registro                   DECIMAL(2);
   DEFINE v_tmp_enc_ide_servicio                   CHAR(2)   ;
   DEFINE v_tmp_enc_ide_operacion                  CHAR(2)   ;
   DEFINE v_tmp_enc_tpo_entidad_origen             CHAR(2)   ;
   DEFINE v_tmp_enc_cve_entidad_origen             CHAR(3)   ;
   DEFINE v_tmp_enc_tpo_entidad_destino            CHAR(2)   ;
   DEFINE v_tmp_enc_cve_entidad_destino            CHAR(3)   ;
   DEFINE v_tmp_enc_fecha_transferencia            DECIMAL(8);
   DEFINE v_tmp_enc_consecutivo_dia                DECIMAL(8);
   DEFINE v_tmp_enc_modalidad_recepcion            CHAR(2)   ;
   DEFINE v_tmp_enc_resultado_operacion            CHAR(2)   ;
   DEFINE v_tmp_enc_diagnostico_1                  CHAR(3)   ;
   DEFINE v_tmp_enc_diagnostico_2                  CHAR(3)   ;
   DEFINE v_tmp_enc_diagnostico_3                  CHAR(3)   ;
   DEFINE v_tmp_enc_filler                         CHAR(255) ;
   -- Encabezado inserta
   DEFINE v_enc_fecha_transferencia                DATE;
   DEFINE v_enc_consecutivo_dia                    DECIMAL(8);
   DEFINE v_enc_modalidad_recepcion                CHAR(2)   ;

   -- Encabezado patronal tmp
   DEFINE v_enct_patron_reg_patronal               CHAR(11)  ;
   DEFINE v_enct_patron_rfc_patron                 CHAR(13)  ;
   DEFINE v_enct_patron_periodo_pago               CHAR(6)   ;
   DEFINE v_enct_patron_num_folio_sua              DECIMAL(6,0);
   DEFINE v_enct_patron_nom_razon_social           CHAR(50)  ;
   DEFINE v_enct_patron_num_solicitud              CHAR(13)  ;
   DEFINE v_enct_patron_tpo_cotizacion             CHAR(1)   ;
   DEFINE v_enct_patron_tot_dias_cotizados         DECIMAL(7,0);
   DEFINE v_enct_patron_num_trabaja_involucrados   DECIMAL(7,0);
   DEFINE v_enct_patron_fec_pago                   DATE   ;
   DEFINE v_enct_patron_fec_valor_vivienda         DATE;
   DEFINE v_enct_patron_fec_valor_rcv              DATE;
   DEFINE v_enct_patron_cve_enti_recaudadora       CHAR(3)   ;
   DEFINE v_enct_patron_delegacion                 CHAR(2)   ;
   DEFINE v_enct_patron_subdelegacion              CHAR(2)   ;
   DEFINE v_enct_patron_result_operacion           CHAR(2)   ;
   DEFINE v_enct_patron_secuencia_registro_lote    DECIMAL(9,0);

   -- Encabezado patronal salida.
   DEFINE v_dpe_patron_folio              DECIMAL(9,0);
   DEFINE v_dpe_patron_reg_patronal_imss  CHAR(11)    ;
   DEFINE v_dpe_patron_rfc_patron         CHAR(13)    ;
   DEFINE v_dpe_patron_periodo_pago       CHAR(6)     ;
   DEFINE v_dpe_patron_folio_sua          INTEGER     ;
   DEFINE v_dpe_patron_razon_social       CHAR(50)    ;
   DEFINE v_dpe_patron_numero_solicitud   CHAR(13)    ;
   DEFINE v_dpe_patron_tipo_cotizacion    SMALLINT    ;
   DEFINE v_dpe_patron_tot_dias_cotizados INTEGER     ;
   DEFINE v_dpe_patron_tot_tra_solicitud  INTEGER     ;
   DEFINE v_dpe_patron_f_pago             DATE        ;
   DEFINE v_dpe_patron_f_valor_viv        DATE        ;
   DEFINE v_dpe_patron_f_valor_rcv        DATE        ;
   DEFINE v_dpe_patron_clave_entidad_rec  CHAR(3)     ;
   DEFINE v_dpe_patron_delegacion         CHAR(2)     ;
   DEFINE v_dpe_patron_subdelegacion      CHAR(2)     ;
   DEFINE v_dpe_patron_result_op          CHAR(2)     ;
   DEFINE v_dpe_patron_sec_registro_lote  INTEGER     ;
   -- TMP_SUM_PAGO_PATRONAL_DPE
   DEFINE v_tmp_patron_reg_patronal               CHAR(11)   ;
   DEFINE v_tmp_patron_rfc_patron                 CHAR(13)   ;
   DEFINE v_tmp_patron_periodo_pago               CHAR(6)    ;
   DEFINE v_tmp_patron_num_folio_sua              DECIMAL(6) ;
   DEFINE v_tmp_patron_imp_retiro                 DECIMAL(12,2);
   DEFINE v_tmp_patron_imp_censatia_vejez         DECIMAL(12,2);
   DEFINE v_tmp_patron_imp_act_retiro_cv          DECIMAL(12,2);
   DEFINE v_tmp_patron_imp_aporta_patro_infonavit DECIMAL(12,2);
   DEFINE v_tmp_patron_apli_int_viv_devolver      DECIMAL(16,6);
   -- DPE_SUM_PATRON
   DEFINE v_sum_patron_folio                      DECIMAL(9,0) ;
   DEFINE v_sum_patron_reg_patronal_imss          CHAR(11)     ;
   DEFINE v_sum_patron_rfc                        CHAR(13)     ;
   DEFINE v_sum_patron_periodo_pago               CHAR(6)      ;
   DEFINE v_sum_patron_folio_sua                  CHAR(6)      ;
   DEFINE v_sum_patron_subtot_retiro              DECIMAL(16,6);
   DEFINE v_sum_patron_subtot_cv                  DECIMAL(16,6);
   DEFINE v_sum_patron_subtot_act_retiro_cv       DECIMAL(16,6);
   DEFINE v_sum_patron_subtot_viv                 DECIMAL(18,6);
   DEFINE v_sum_patron_subtot_avis_viv            DECIMAL(18,6);

   --
   DEFINE v_sum_devol_tpo_entidad_origen            CHAR(2)    ;
   DEFINE v_sum_devol_cve_entidad_origen            CHAR(3)    ;
   DEFINE v_sum_devol_tpo_entidad_destino           CHAR(2)    ;
   DEFINE v_sum_devol_cve_entidad_destino           CHAR(3)    ;
   DEFINE v_sum_devol_fecha_transferencia           DATE       ;
   DEFINE v_sum_devol_consecutivo_dia               DECIMAL(3) ;
   DEFINE v_sum_devol_total_solicitudes_patron      DECIMAL(7) ;
   DEFINE v_sum_devol_total_registro_trabajadores   DECIMAL(7) ;
   DEFINE v_sum_devol_total_registros               DECIMAL(7) ;
   DEFINE v_sum_devol_imp_total_retiro_cv           DECIMAL(22,2);
   DEFINE v_sum_devol_importe_total_viv             DECIMAL(22,2);
   DEFINE v_sum_devol_aplicaciones_int_viv_devovler DECIMAL(22,6);


   DEFINE v_sum_exceso_folio                        DECIMAL(9,0) ;
   DEFINE v_sum_exceso_tipo_ent_origen              CHAR(2)      ;
   DEFINE v_sum_exceso_cve_ent_origen               CHAR(3)      ;
   DEFINE v_sum_exceso_tipo_ent_destino             CHAR(2)      ;
   DEFINE v_sum_exceso_cve_ent_destino              CHAR(3)      ;
   DEFINE v_sum_exceso_f_transferencia              DATE         ;
   DEFINE v_sum_exceso_consecutivo_dia              SMALLINT     ;
   DEFINE v_sum_exceso_tot_sol_patronales           DECIMAL(9,0) ;
   DEFINE v_sum_exceso_tot_reg_trabajadores         DECIMAL(9,0) ;
   DEFINE v_sum_exceso_tot_registros                DECIMAL(9,0) ;
   DEFINE v_sum_exceso_total_retiro_cv              DECIMAL(22,6);
   DEFINE v_sum_exceso_total_vivienda               DECIMAL(22,6);
   DEFINE v_sum_exceso_total_avis                   DECIMAL(22,6);

   DEFINE v_detalle_id_dpe_referencia    DECIMAL(9,0) ;
   DEFINE v_detalle_folio                DECIMAL(9,0) ;
   DEFINE v_detalle_reg_patronal_imss    CHAR(11)     ;
   DEFINE v_detalle_periodo_pago         CHAR(6)      ;
   DEFINE v_detalle_imp_viv_dev          DECIMAL(16,6);
   DEFINE v_detalle_avis_viv_dev         DECIMAL(16,6);
   DEFINE v_detalle_diagnostico          SMALLINT     ;
   DEFINE v_detalle_resul_op             SMALLINT     ;
   DEFINE v_detalle_diag_procesa         SMALLINT     ;
   DEFINE v_detalle_folio_respuesta      DECIMAL(9,0) ;

   -- Tipos status de pagos
   DEFINE c_pago_total_nss                SMALLINT;
   DEFINE c_pago_parcial_nss              SMALLINT;
   DEFINE c_pago_por_preliquidar_total    SMALLINT;
   DEFINE c_pago_por_preliquidar_parcial  SMALLINT;
   DEFINE c_pago_preliquidado_total       SMALLINT;
   DEFINE c_pago_preliquidado_parcial     SMALLINT;
   DEFINE c_pago_liquidado_total          SMALLINT;
   DEFINE c_pago_liquidado_parcial        SMALLINT;
   DEFINE c_pago_enviado_procesar_total   SMALLINT;
   DEFINE c_pago_enviado_procesar_parcial SMALLINT;

   -- Subtotales de validación de sumario de patron
   DEFINE v_checa_imp_retiro_devolver               DECIMAL(16,6);
   DEFINE v_checa_imp_censatia_vejez_patron         DECIMAL(16,6);
   DEFINE v_checa_imp_actua_recargo_retiro          DECIMAL(16,6);
   DEFINE v_checa_apli_interes_vivienda             DECIMAL(18,6);
   --
   DEFINE v_reg_patronal                            CHAR(11);
   DEFINE v_suma_importes_patronal                  DECIMAL(11,2);
   DEFINE v_total_solicitudes                       DECIMAL(7,0) ;

   -- Totales de solicitudes de archi
   DEFINE v_i_total_solicitudes_patron              INTEGER;
   DEFINE v_i_total_registro_trabajadores           INTEGER;
   DEFINE v_i_total_registros                       INTEGER;
   DEFINE v_i_aplicaciones_int_viv_devovler         DECIMAL(16,6);

   --
   DEFINE v_id_derechohabiente                      DECIMAL(9,0);
   DEFINE v_i_resultado                             SMALLINT;
   DEFINE v_si_correcto_integra                     SMALLINT;
   DEFINE v_id_dpe_referencia                       DECIMAL(11,0);
   DEFINE v_enc_f_operacion_proceso                 DATE;
   DEFINE v_dte_fecha_hoy                           DATE;
   DEFINE v_id_credito_ad                           SMALLINT;
   -- booleana para indicar si un registro se inserta
   DEFINE v_b_registro_correcto                     SMALLINT;

   DEFINE v_saldo_pesos   DECIMAL(16,6);
   DEFINE v_resultado     SMALLINT;
   DEFINE v_dh_marca      DECIMAL(9,0);

   -- Total de solicitudes de archivo
   DEFINE v_si_numero_solicitudes_totales           INTEGER;
   -- Numero de solicitudes sin rechazos
   DEFINE v_si_numero_solicitudes_aceptadas         INTEGER;
   -- Identificador de referencia de sumario patron
   DEFINE v_sum_id_dpe_referencia                   DECIMAL(9,0);
   DEFINE v_sum_exceso_id_dpe_referencia            DECIMAL(9,0);
   DEFINE v_si_total_trabaja                        INTEGER;
   DEFINE v_si_estado                               SMALLINT;
   DEFINE v_i_estado_marca                          INTEGER;
   DEFINE v_subcuenta_tmp                           SMALLINT; 
   DEFINE v_id_sum_patron                           DECIMAL(9,0);

   -- [ Estado procesos]
   DEFINE v_si_status_detalle_trabaj  INTEGER;
   DEFINE v_si_status_sumario_patron  INTEGER;
   DEFINE v_si_status_sumario_exceso  INTEGER;

   --
   -- Control de Excepciones
   DEFINE sql_err INTEGER;
   DEFINE isam_err INTEGER;
   DEFINE err_txt  CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      --LET v_i_resultado = -206;

      LET v_i_resultado = sql_err;
      LET v_si_correcto_integra = 1;
      LET v_si_numero_solicitudes_totales = 0;
      LET v_si_numero_solicitudes_aceptadas = 0;
      LET v_si_total_trabaja = 0;

      RETURN v_i_resultado,
             err_txt,
             v_si_correcto_integra,
             v_si_numero_solicitudes_totales,
             v_si_numero_solicitudes_aceptadas,
             v_si_status_detalle_trabaj,
             v_si_status_sumario_patron,
             v_si_status_sumario_exceso,
             v_si_total_trabaja;
   END EXCEPTION

   -- Constantes de estatus de diagnostico y de pagos
   LET c_pago_total_nss                = 0;
   LET c_pago_parcial_nss              = 1;
   LET c_pago_por_preliquidar_total    = 2;
   LET c_pago_por_preliquidar_parcial  = 3;
   LET c_pago_preliquidado_total       = 4;
   LET c_pago_preliquidado_parcial     = 5;
   LET c_pago_liquidado_total          = 6;
   LET c_pago_liquidado_parcial        = 7;
   LET c_pago_enviado_procesar_total   = 8;
   LET c_pago_enviado_procesar_parcial = 9;


   -- <Por default la integración es correcta>
   LET v_si_correcto_integra             = 0;
   -- <Por default total de solicitudes>
   LET v_si_numero_solicitudes_totales   = 0;
   -- <Por default numero de solicitudes aceptadas>
   LET v_si_numero_solicitudes_aceptadas = 0;
   -- <Por default resultado general correcto>
   LET v_i_resultado = 0;
   -- <Por DEFAULT se inicia el total de trabajadores>
   LET v_si_total_trabaja = 0;

   -- < Inicializa status de validación de tablas>
   LET v_si_status_detalle_trabaj  = 0;
   LET v_si_status_sumario_patron  = 0;
   LET v_si_status_sumario_exceso  = 0;

   --SET DEBUG FILE TO "/safreviv_int/dpe/envio/trace_dpe_integra.txt";
   --TRACE ON;

   LET v_enc_f_operacion_proceso = TODAY;
   LET err_txt = 'Al obtener maximo de id_dpe de dpe_sol_trabajador';

   -- Genera status de las solicitudes
   LET v_si_aceptada  = 1;
   LET v_si_rechazada = 2;
   LET v_si_pendiente = 3;

   LET v_i_total_solicitudes_patron     = 0;
   LET v_i_total_registro_trabajadores  = 0;
   LET v_i_total_registros              = 0;
   LET v_i_aplicaciones_int_viv_devovler= 0;

   -- Fecha actual
   LET v_dte_fecha_hoy = TODAY;

   LET v_id_credito_ad = NULL;

   -- OBtener los datos del archivo con las solicitudes en tabla temporal

   -- [Error]
   LET err_txt = 'Al recuperar datos detalle tmp_det_devolucion_dpe';

   FOREACH
      SELECT t.reg_patronal,
             t.periodo_pago,
             t.rfc_trabajador,
             t.curp_trabajador,
             t.nombre,
             t.dias_cotiza_bim_devolver,
             t.imp_retiro_devolver /100,
             t.imp_actua_recargo_retiro /100,
             t.imp_censatia_vejez_patron /100,
             t.imp_censatia_vejez_trabajador /100,
             t.imp_actua_recargo_censa_vejez /100,
             t.imp_patronal_infonavit_devolver /100,
             t.apli_interes_vivienda /1000000,
             t.filer,
             t.nss,
             t.folio_sua_det,
             a.id_derechohabiente
      INTO   v_tmp_reg_patronal,
             v_tmp_periodo_pago,
             v_tmp_rfc_trabajador,
             v_tmp_curp_trabajador,
             v_tmp_nombre,
             v_tmp_dias_cotiza_bim_devolver,
             v_tmp_imp_retiro_devolver,
             v_tmp_imp_actua_recargo_retiro,
             v_tmp_imp_censatia_vejez_patron,
             v_tmp_imp_censatia_vejez_trabajador,
             v_tmp_imp_actua_recargo_censa_vejez,
             v_tmp_imp_patronal_infonavit_devolver,
             v_tmp_apli_interes_vivienda,
             v_tmp_filer,
             v_tmp_nss,
             v_tmp_folio_sua_det,
             v_id_derechohabiente
      FROM   safre_tmp:tmp_det_devolucion_dpe t
      LEFT OUTER JOIN safre_viv:afi_derechohabiente a
          ON t.nss = a.nss


      LET v_si_status_solicitud = 0;
      LET v_b_registro_correcto = 0;
      LET v_dpe_porcentaje_dev = 100;

      -- <Por default se asigna tipo archivo procesar>
      LET v_si_tipo_archivo = 1;

      -- <Acumula la referencia>
      --LET v_id_dpe_referencia = v_id_dpe_referencia + 1;
      SELECT seq_dpe_sol_trabajador.NEXTVAL
        INTO v_id_dpe_referencia
        FROM systables
       WHERE tabname = "dpe_sol_trabajador";

      -- <Contador de solicitudes de archivo>
      LET v_si_numero_solicitudes_totales = v_si_numero_solicitudes_totales + 1;

      -- < Por default la solicitud es aceptada>
      LET v_si_status_solicitud = v_si_aceptada;

      -- <Por default el diagnostico indica que se pagara el total de la solictud para el nss>
      LET v_dpe_diagnostico         = 0; -- Se paga el total.

      IF(v_si_tipo_archivo = 1)THEN
         IF(v_id_derechohabiente IS NULL OR v_id_derechohabiente <=0)THEN
            LET v_id_derechohabiente = 0;
            -- NO se encontro el NSS en base de datos
            -- [Guardar registro de rechazo]
            EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                     03,
                                                     v_id_dpe_referencia,
                                                     'NO',
                                                     10,
                                                     'NSS - '||v_tmp_nss
                                                     );

            -- Marcar registro como rechazado
            LET v_b_registro_correcto = 1;
            LET v_dpe_diagnostico     = 10;

            LET v_si_status_solicitud = v_si_rechazada;
         END IF-- Nss
      END IF

      --Se recupera la referencia
      SELECT id_dpe_referencia
        INTO v_dpe_referencia
        FROM dpe_patron
       WHERE folio = p_folio
         AND folio_sua = v_tmp_folio_sua_det
         AND reg_patronal_imss =  v_tmp_reg_patronal
         AND periodo_pago =  v_tmp_periodo_pago;
         
--#####################


      -- Recupera datos para insertar.
      LET v_dpe_id_dpe_referencia   = v_id_dpe_referencia                  ;
      LET v_id_dpe_patron           = v_dpe_referencia                     ;
      LET v_dpe_folio               = p_folio                              ;
      LET v_dpe_reg_patronal_imss   = v_tmp_reg_patronal                   ;
      LET v_dpe_id_derechohabiente  = v_id_derechohabiente                 ;
      LET v_dpe_periodo_pago        = v_tmp_periodo_pago                   ;
      LET v_dpe_rfc                 = v_tmp_rfc_trabajador                 ;
      LET v_dpe_curp                = v_tmp_curp_trabajador                ;
      LET v_dpe_nombre              = v_tmp_nombre                         ;
      LET v_dpe_dias_cotizados      = v_tmp_dias_cotiza_bim_devolver       ;
      LET v_dpe_imp_retiro_dev      = v_tmp_imp_retiro_devolver            ;
      LET v_dpe_imp_act_retiro_dev  = v_tmp_imp_actua_recargo_retiro       ;
      LET v_dpe_imp_cv_pat_dev      = v_tmp_imp_censatia_vejez_patron      ;
      LET v_dpe_imp_cv_trab_dev     = v_tmp_imp_censatia_vejez_trabajador  ;
      LET v_dpe_imp_act_cv_dev      = v_tmp_imp_actua_recargo_censa_vejez  ;
      LET v_dpe_imp_viv_dev         = v_tmp_imp_patronal_infonavit_devolver;
      LET v_dpe_avis_viv_dev        = v_tmp_apli_interes_vivienda          ;
      LET v_dpe_nss                 = v_tmp_nss                            ;
      LET v_dpe_estado_solicitud    = v_si_status_solicitud                ;

      LET err_txt = 'Al insertar detalle en dpe_sol_trabajador';

      INSERT INTO dpe_sol_trabajador (id_dpe_referencia,
                                      id_dpe_patron,
                                      folio,
                                      reg_patronal_imss,
                                      id_derechohabiente,
                                      nss,
                                      periodo_pago,
                                      rfc,
                                      curp,
                                      nombre,
                                      dias_cotizados,
                                      imp_retiro_dev,
                                      imp_act_retiro_dev,
                                      imp_cv_pat_dev,
                                      imp_cv_trab_dev,
                                      imp_act_cv_dev,
                                      imp_viv_dev,
                                      avis_viv_dev,
                                      porcentaje_dev,
                                      estado_solicitud,
                                      diagnostico)
             VALUES (v_dpe_id_dpe_referencia,
                     v_id_dpe_patron,
                     v_dpe_folio,
                     v_dpe_reg_patronal_imss,
                     v_dpe_id_derechohabiente,
                     v_dpe_nss,
                     v_dpe_periodo_pago,
                     v_dpe_rfc,
                     v_dpe_curp,
                     v_dpe_nombre,
                     v_dpe_dias_cotizados,
                     v_dpe_imp_retiro_dev,
                     v_dpe_imp_act_retiro_dev,
                     v_dpe_imp_cv_pat_dev,
                     v_dpe_imp_cv_trab_dev,
                     v_dpe_imp_act_cv_dev,
                     v_dpe_imp_viv_dev,
                     v_dpe_avis_viv_dev,
                     v_dpe_porcentaje_dev,
                     v_dpe_estado_solicitud,
                     v_dpe_diagnostico);

      -- Si registro correcto
      IF (v_b_registro_correcto = 0)THEN
          -- Incrementa el contador de solicitudes correctas.
          LET v_si_numero_solicitudes_aceptadas = v_si_numero_solicitudes_aceptadas + 1;
      END IF
--###########
----Cuando termine de insertar los trabajadores en dpe_sol_trabajador buscar los NSS que faltan
--###########

      -- Contador de total de solicitudes
      LET v_i_total_registro_trabajadores = v_i_total_registro_trabajadores + 1;

      -- Acumula total de aplicacones
      LET v_i_aplicaciones_int_viv_devovler = v_i_aplicaciones_int_viv_devovler + v_dpe_avis_viv_dev;

      LET err_txt = 'Al marcar cuenta como aceptada';
      -- # [1.10 Marcar cuenta que fue aceptada en proceso de devolucion]
      IF(v_dpe_estado_solicitud = v_si_aceptada)THEN
         LET v_i_estado_marca = 0;
         --SELECT v_dpe_id_derechohabiente
         --INTO   v_dh_marca
         --FROM   sfr_marca_activa
         --WHERE  id_derechohabiente = v_dpe_id_derechohabiente
         --AND    marca = 401;
         --
         --IF v_dh_marca IS NULL THEN
         EXECUTE FUNCTION fn_marca_cuenta(v_dpe_id_derechohabiente,
                                          401,                       -- marca de imms
                                          v_dpe_id_dpe_referencia,
                                          v_dpe_folio,
                                          0,                         -- estado marca
                                          0,                         -- codigo de rechazo
                                          0,                         -- marca de la causa
                                          NULL,                      -- fecha de la causa
                                          p_usuario_cod,
                                          p_proceso_cod)
         INTO v_i_estado_marca;
         --END IF
      END IF

   END FOREACH;

   -- [Paso. Insertar los datos de los sumario en historicos: dpe_sum_patron]
   -- [Obtener consecutivo maximo para generar nuevo.]
   LET v_sum_id_dpe_referencia = 0;

   LET err_txt = 'Al recuperar datos de sumario de patron tmp_sum_pago_patronal_dpe';

   FOREACH
      SELECT reg_patronal,
             rfc_patron,
             periodo_pago,
             num_folio_sua,
             imp_retiro /100,
             imp_censatia_vejez /100,
             imp_act_retiro_cv /100,
             imp_aporta_patro_infonavit /100,
             apli_int_viv_devolver /1000000
      INTO   v_tmp_patron_reg_patronal,
             v_tmp_patron_rfc_patron,
             v_tmp_patron_periodo_pago,
             v_tmp_patron_num_folio_sua,
             v_tmp_patron_imp_retiro,
             v_tmp_patron_imp_censatia_vejez,
             v_tmp_patron_imp_act_retiro_cv,
             v_tmp_patron_imp_aporta_patro_infonavit,
             v_tmp_patron_apli_int_viv_devolver
      FROM safre_tmp:tmp_sum_pago_patronal_dpe

      LET err_txt = 'Al recuperar datos para dpe_sum_patron';
      -- Realizar asignaciones
      LET v_sum_patron_folio                = p_folio                                ;
      LET v_sum_patron_reg_patronal_imss    = v_tmp_patron_reg_patronal              ;
      LET v_sum_patron_rfc                  = v_tmp_patron_rfc_patron                ;
      LET v_sum_patron_periodo_pago         = v_tmp_patron_periodo_pago              ;
      LET v_sum_patron_folio_sua            = v_tmp_patron_num_folio_sua             ;
      LET v_sum_patron_subtot_retiro        = v_tmp_patron_imp_retiro                ;
      LET v_sum_patron_subtot_cv            = v_tmp_patron_imp_censatia_vejez        ;
      LET v_sum_patron_subtot_act_retiro_cv = v_tmp_patron_imp_act_retiro_cv         ;
      LET v_sum_patron_subtot_viv           = v_tmp_patron_imp_aporta_patro_infonavit;
      LET v_sum_patron_subtot_avis_viv      = v_tmp_patron_apli_int_viv_devolver     ;


      SELECT id_dpe_referencia
      INTO   v_id_sum_patron
      FROM   dpe_sum_patron
      WHERE  folio = p_folio
      AND    folio_sua = v_sum_patron_folio_sua
      AND    reg_patronal_imss = v_sum_patron_reg_patronal_imss
      AND    periodo_pago = v_sum_patron_periodo_pago;
      
      IF v_id_sum_patron IS NULL THEN
         -- Acumula v_sum_id_dpe_referencia para obtener registro consecutivo
         SELECT seq_dpe_sum_patron.NEXTVAL
           INTO v_sum_id_dpe_referencia
                 FROM systables
                WHERE tabname = "dpe_sum_patron";
         
         LET err_txt = 'Al insertar sumario dpe_sum_patron';
         
         -- Insertar nuevo registro
         INSERT INTO dpe_sum_patron (folio,
                                     id_dpe_referencia,
                                     reg_patronal_imss,
                                     rfc,
                                     periodo_pago,
                                     folio_sua,
                                     subtot_retiro,
                                     subtot_cv,
                                     subtot_act_retiro_cv,
                                     subtot_viv,
                                     subtot_avis_viv)
                VALUES (v_sum_patron_folio,
                        v_sum_id_dpe_referencia,
                        v_sum_patron_reg_patronal_imss,
                        v_sum_patron_rfc,
                        v_sum_patron_periodo_pago,
                        v_sum_patron_folio_sua,
                        v_sum_patron_subtot_retiro,
                        v_sum_patron_subtot_cv,
                        v_sum_patron_subtot_act_retiro_cv,
                        v_sum_patron_subtot_viv,
                        v_sum_patron_subtot_avis_viv);
         
         -- Verificar importes de sutotales de patron.
         LET v_checa_imp_retiro_devolver        = 0;
         LET v_checa_imp_censatia_vejez_patron  = 0;
         LET v_checa_imp_actua_recargo_retiro   = 0;
         LET v_checa_apli_interes_vivienda      = 0;
         
         -- [Obtener importe totales de patron.]
         LET err_txt = 'Al recuperar datos de partron y periodo-pago';
         
         SELECT SUM(imp_retiro_devolver       /100),
                SUM(imp_censatia_vejez_patron /100) + SUM(imp_censatia_vejez_trabajador /100),
                SUM(imp_actua_recargo_retiro  /100) + SUM(imp_actua_recargo_censa_vejez /100),
                SUM(apli_interes_vivienda     /1000000)
           INTO v_checa_imp_retiro_devolver,
                v_checa_imp_censatia_vejez_patron,
                v_checa_imp_actua_recargo_retiro,
                v_checa_apli_interes_vivienda
           FROM safre_tmp:tmp_det_devolucion_dpe
          WHERE reg_patronal = v_sum_patron_reg_patronal_imss
            AND periodo_pago = v_sum_patron_periodo_pago;
         
         LET err_txt = 'Al insertar sumario dpe_sum_patron. Compara 1.';
         
         IF(v_checa_imp_retiro_devolver <>v_sum_patron_subtot_retiro)THEN
            -- [Solicitud en estatus de rechazada por diferencia en imp_retiro_devolver sumario dpe_sum_patron]
            EXECUTE PROCEDURE sp_dpe_inserta_rechazo( p_folio,
                                                      04,
                                                      v_sum_id_dpe_referencia,
                                                      'NO',
                                                      40,
                                                      'imp_retiro_devolver-'||v_checa_imp_retiro_devolver);
            -- <status de error>
            LET v_si_status_sumario_patron = 1;
         END IF
         
         LET err_txt = 'Al insertar sumario dpe_sum_patron. Compara 2.';
         
         IF(v_checa_imp_censatia_vejez_patron <> v_sum_patron_subtot_cv)THEN
            -- [Solicitud en estatus de rechazada por diferencia en imp_censatia_vejez_patron sumario dpe_sum_patron]
            EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                     04,
                                                     v_sum_id_dpe_referencia,
                                                     'NO',
                                                     41,
                                                     'imp_censatia_vejez_patron-'||v_checa_imp_censatia_vejez_patron);
            -- <status de error>
            LET v_si_status_sumario_patron = 1;
         END IF
         
         LET err_txt = 'Al insertar sumario dpe_sum_patron. Compara 3.';
         
         IF(v_checa_imp_actua_recargo_retiro <> v_sum_patron_subtot_act_retiro_cv)THEN
            -- [Solicitud en estatus de rechazada por diferencia en imp_actua_recargo_retiro sumario dpe_sum_patron]
            EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                     04,
                                                     v_sum_id_dpe_referencia,
                                                     'NO',
                                                      42,
                                                     'imp_actua_recargo_retiro-'||v_checa_imp_actua_recargo_retiro);
            -- <status de error>
            LET v_si_status_sumario_patron = 1;
         END IF
         
         LET err_txt = 'Al insertar sumario dpe_sum_patron. Compara 4.';
         
         IF(v_checa_apli_interes_vivienda <> v_sum_patron_subtot_avis_viv)THEN
            -- [Solicitud en estatus de rechazada por diferencia en apli_interes_vivienda sumario dpe_sum_patron]
            EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                     04 ,
                                                     v_sum_id_dpe_referencia,
                                                     'NO',
                                                      43,
                                                     'apli_interes_vivienda-'||v_checa_apli_interes_vivienda);
            -- <status de error>
            LET v_si_status_sumario_patron = 1;
         END IF
         
         -- Contador de total de solicitudes de patron
         LET v_i_total_solicitudes_patron = v_i_total_solicitudes_patron + 1;
     ELSE
        CONTINUE FOREACH;
     END IF
      -- Total de patrones
      LET v_si_total_trabaja = v_si_total_trabaja + 1;

   END FOREACH;

   -- [Obtener consecutivo maximo para generar nuevo.]
   LET v_sum_exceso_id_dpe_referencia = 0;

   LET err_txt = 'Al recuperar registro de encabezado general dpe_sum_exceso';
   -- [Paso. Insertar los datos de los sumario en historicos: dpe_sum_exceso]
   FOREACH
      SELECT tpo_entidad_origen,
             cve_entidad_origen,
             tpo_entidad_destino,
             cve_entidad_destino,
             sp_cambia_formato_fecha(fecha_transferencia),
             consecutivo_dia,
             total_solicitudes_patron,
             total_registro_trabajadores,
             total_registros,
             imp_total_retiro_cv /100,
             importe_total_viv /100,
             aplicaciones_int_viv_devovler /1000000
      INTO   v_sum_devol_tpo_entidad_origen,
             v_sum_devol_cve_entidad_origen,
             v_sum_devol_tpo_entidad_destino,
             v_sum_devol_cve_entidad_destino,
             v_sum_devol_fecha_transferencia,
             v_sum_devol_consecutivo_dia,
             v_sum_devol_total_solicitudes_patron,
             v_sum_devol_total_registro_trabajadores,
             v_sum_devol_total_registros,
             v_sum_devol_imp_total_retiro_cv,
             v_sum_devol_importe_total_viv,
             v_sum_devol_aplicaciones_int_viv_devovler
      FROM   safre_tmp:tmp_sum_devolucion_dpe

      -- Genera consecutivo para id referencia
      SELECT seq_dpe_sum_exceso.NEXTVAL
              INTO v_sum_exceso_id_dpe_referencia
             FROM  systables
            WHERE  tabname = "dpe_sum_exceso";

      LET err_txt = 'Al recuperar datos para tmp_sum_devolucion_dpe';

      -- Recuperar datos seleccionados
      LET v_sum_exceso_folio                = p_folio;
      LET v_sum_exceso_tipo_ent_origen      = v_sum_devol_tpo_entidad_origen           ;
      LET v_sum_exceso_cve_ent_origen       = v_sum_devol_cve_entidad_origen           ;
      LET v_sum_exceso_tipo_ent_destino     = v_sum_devol_tpo_entidad_destino          ;
      LET v_sum_exceso_cve_ent_destino      = v_sum_devol_cve_entidad_destino          ;
      LET v_sum_exceso_f_transferencia      = v_sum_devol_fecha_transferencia          ;
      LET v_sum_exceso_consecutivo_dia      = v_sum_devol_consecutivo_dia              ;
      LET v_sum_exceso_tot_sol_patronales   = v_sum_devol_total_solicitudes_patron     ;
      LET v_sum_exceso_tot_reg_trabajadores = v_sum_devol_total_registro_trabajadores  ;
      LET v_sum_exceso_tot_registros        = v_sum_devol_total_registros              ;
      LET v_sum_exceso_total_retiro_cv      = v_sum_devol_imp_total_retiro_cv          ;
      LET v_sum_exceso_total_vivienda       = v_sum_devol_importe_total_viv            ;
      LET v_sum_exceso_total_avis           = v_sum_devol_aplicaciones_int_viv_devovler;

      LET err_txt = 'Al insertar registro de encabezado general tmp_sum_devolucion_dpe';
      -- Insertar registro.
      INSERT INTO dpe_sum_exceso (folio,
                                  id_dpe_referencia,
                                  tipo_ent_origen,
                                  cve_ent_origen,
                                  tipo_ent_destino,
                                  cve_ent_destino,
                                  f_transferencia,
                                  consecutivo_dia,
                                  tot_sol_patronales,
                                  tot_reg_trabajadores,
                                  tot_registros,
                                  total_retiro_cv,
                                  total_vivienda,
                                  total_avis)
             VALUES (v_sum_exceso_folio,
                     v_sum_exceso_id_dpe_referencia,
                     v_sum_exceso_tipo_ent_origen,
                     v_sum_exceso_cve_ent_origen,
                     v_sum_exceso_tipo_ent_destino,
                     v_sum_exceso_cve_ent_destino,
                     v_sum_exceso_f_transferencia,
                     v_sum_exceso_consecutivo_dia,
                     v_sum_exceso_tot_sol_patronales,
                     v_sum_exceso_tot_reg_trabajadores,
                     v_sum_exceso_tot_registros,
                     v_sum_exceso_total_retiro_cv,
                     v_sum_exceso_total_vivienda,
                     v_sum_exceso_total_avis);

      -- Generar validaciones segun layout de los datos que se guardan.

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 1.';

      IF(v_sum_devol_tpo_entidad_origen <> '04')THEN
         -- [Solicitud en estatus de rechazada por diferencia en tpo_entidad_origen sumario dpe_sum_exceso]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09 ,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  50,
                                                  'tpo_entidad_origen-'||v_sum_devol_tpo_entidad_origen);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 2.';

      IF(v_sum_devol_cve_entidad_origen <> '001')THEN
         -- [Solicitud en estatus de rechazada por ]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio, 09 ,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  51,
                                                  'cve_entidad_origen-'||v_sum_devol_cve_entidad_origen);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 3.';

      IF(v_sum_devol_tpo_entidad_destino <> '03')THEN
         -- [Solicitud en estatus de rechazada por ]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09 ,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  52,
                                                  'tpo_entidad_destino-'||v_sum_devol_tpo_entidad_destino);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 4.';

      IF(v_sum_devol_cve_entidad_destino <> '001')THEN
         -- [Solicitud en estatus de rechazada por ]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  53,
                                                  'cve_entidad_destino-'||v_sum_devol_cve_entidad_destino);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 5.';

      IF(v_sum_devol_fecha_transferencia > v_dte_fecha_hoy)THEN
         -- [Solicitud en estatus de rechazada por fecha transferecia mayor a la de operación]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  54,
                                                  'fecha_transferencia-'||v_sum_devol_fecha_transferencia);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      -- Calcular el total de registros sin encabezado (01) y sin sumario (09) general.
      LET v_i_total_registros = v_i_total_registro_trabajadores + (v_i_total_solicitudes_patron) * 2 ;
      -- Se multiplica por dos porque se considera total de encabezados patron
      --    igual a total sumarios patron.  (03 + 02 + 04)

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 6.';

      IF(v_sum_devol_total_solicitudes_patron <>
         v_i_total_solicitudes_patron)THEN

         -- [Solicitud en estatus de rechazada por diferencias en totales patron]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09 ,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  55,
                                                  'total_solicitudes_patron-'||v_sum_devol_total_solicitudes_patron);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 7.';

      IF(v_sum_devol_total_registro_trabajadores <>
         v_i_total_registro_trabajadores )THEN

         -- [Solicitud en estatus de rechazada por diferencias en totales solicitudes]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  56,
                                                  'total_registro_trabajadores-'||v_sum_devol_total_registro_trabajadores);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 8.';

      IF(v_sum_devol_total_registros <> v_i_total_registros )THEN

         -- [Solicitud en estatus de rechazada por diferencias total registros]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09 ,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  57,
                                                  'total_registros-'||v_sum_devol_total_registros);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

      LET err_txt = 'Al insertar sumario dpe_sum_exceso. Compara 9.';

      IF(v_i_aplicaciones_int_viv_devovler <> v_sum_devol_aplicaciones_int_viv_devovler )THEN

         -- [Solicitud en estatus de rechazada por diferencias acciones]
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,
                                                  09 ,
                                                  v_sum_exceso_id_dpe_referencia,
                                                  'NO',
                                                  58,
                                                  'aplicaciones_int_viv_devovler-'||v_i_aplicaciones_int_viv_devovler);
         -- <status de error>
         LET v_si_status_sumario_exceso = 1;
      END IF

   END FOREACH;

   LET err_txt = 'Al actualizar glo_ctr_archivo';
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
      SET folio     = p_folio,
          estado    = 2 -- integrado
    WHERE proceso_cod = 1001
      AND opera_cod   = 1 -- archivo cargado
      AND estado      = 1; -- etapa de carga

   -- ##[Error]
   -- ##LET err_txt = 'Al actualizar la bat_ctr_operacion';
   -- ##-- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion
      SET folio       = p_folio,
          nom_archivo = p_nombre_archivo
    WHERE proceso_cod = 1001
      AND opera_cod   = 2
      AND pid         = p_pid;

   IF(v_si_numero_solicitudes_aceptadas = 0)THEN
      -- [ Se rechazaron todas la solicitudes]

      -- No hay solicitudes aceptadas correctas
      LET v_si_correcto_integra = 1;
      LET v_i_resultado = 1;
      LET v_si_status_detalle_trabaj = 1;
   ELSE
      -- Indica que si existen algunas solicitudes aceptadas
   END IF

   -- [Verifica si ocurrieron errores en las secciones de integracion
   --    sumarios o detalle]
   LET err_txt = " patron:"||v_si_status_sumario_patron
             ||"\n exceso:"||v_si_status_sumario_exceso;
   IF(v_si_status_detalle_trabaj <> 0 OR
          v_si_status_sumario_patron <> 0 OR
          v_si_status_sumario_exceso <>0)THEN

      -- Se marca como fallida la integración.
      --LET v_si_correcto_integra = 1;
   END IF

   -- # Eliminar tabla temporal
   -- # DROP TABLE tmp_dpe_rechazo_temp;

   --UPDATE statistics FOR TABLE dpe_sol_trab_parcial;
   UPDATE statistics FOR TABLE dpe_sol_trabajador;
   UPDATE statistics FOR TABLE dpe_sum_patron;
   UPDATE statistics FOR TABLE dpe_sum_exceso;

 RETURN v_i_resultado,
        err_txt,
        v_si_correcto_integra,
        v_si_numero_solicitudes_totales,
        v_si_numero_solicitudes_aceptadas,
        v_si_status_detalle_trabaj,
        v_si_status_sumario_patron,
        v_si_status_sumario_exceso,
        v_si_total_trabaja;

END FUNCTION
;


