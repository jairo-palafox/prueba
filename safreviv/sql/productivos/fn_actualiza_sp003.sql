






CREATE FUNCTION "selefp".fn_actualiza_sp003()

   RETURNING SMALLINT

--variables generales
   DEFINE v_error                     SMALLINT;
   DEFINE v_id_dh                     decimal(9,0);
   DEFINE v_id_ug                     decimal(9,0);
   DEFINE v_tpo_cred_ug               CHAR(1);
   DEFINE v_nss                       CHAR(11);
--variables para solicutud_uso_garantia
   DEFINE v_id_ocg_solicitud_ug       decimal(9,0);
   DEFINE v_id_ocg_detalle            decimal(9,0);
   DEFINE v_id_ocg_formalizacion      decimal(9,0);
   DEFINE v_id_ocg_tramite            decimal(9,0);
   DEFINE v_id_derechohabiente        decimal(9,0);
   DEFINE v_cve_ent_financiera        smallint;
   DEFINE v_num_ctr_int_ef            char(18);
   DEFINE v_importe_solicitado        decimal(12,2);
   DEFINE v_f_vencimiento             date;
   DEFINE v_importe_utilizado         decimal(12,2);
   DEFINE v_tpo_credito               char(1);
   DEFINE v_solicitud_saldo           smallint;
   DEFINE v_diagnostico               char(2);
   DEFINE v_estado                    smallint;
   DEFINE v_situacion                 smallint;
--variables para nuevos registros
   DEFINE v_nvo_id_ocg_formalizacion  decimal(9,0);
   DEFINE v_nvo_id_ocg_solicitud_ug   decimal(9,0);
   DEFINE v_id_ocg_detalle_f          decimal(9,0);
   DEFINE v_id_ocg_detalle_ug         decimal(9,0);
   DEFINE v_nvo_tpo_credito           char(1);
   DEFINE v_nvo_situacion             smallint;
   DEFINE v_id_ocg_detalle_t          decimal(9,0);
   DEFINE v_nvo_id_ocg_tramite        decimal(9,0);
-- variables para ocg_fecha_mig ug
   DEFINE v_ug_f_envio                date;
   DEFINE v_ug_f_carga                date;
   DEFINE v_ug_f_respuesta            date;
   DEFINE v_ug_f_liquida_cofi         date;
   DEFINE v_ug_subproceso             smallint;
   DEFINE v_ug_f_alta_registro        date;
--variables para ocg_tramite 
   DEFINE v_t_id_ocg_detalle          decimal(9,0);
   DEFINE v_t_cve_ent_financiera      smallint;
   DEFINE v_t_id_derechohabiente      decimal(9,0);
   DEFINE v_t_rfc                     char(13);
   DEFINE v_t_curp                    char(18);
   DEFINE v_t_ap_paterno              char(40);
   DEFINE v_t_ap_materno              char(40);
   DEFINE v_t_nombre                  char(40);
   DEFINE v_t_num_bimestres           smallint;
   DEFINE v_t_viv97                   decimal(12,2);
   DEFINE v_t_f_saldo                 date;
   DEFINE v_t_tpo_credito             char(1);
   DEFINE v_t_f_vigencia              date;
   DEFINE v_t_f_respuesta1            date;
   DEFINE v_t_diagnostico             char(2);
   DEFINE v_t_estado                  smallint;
   DEFINE v_t_situacion               smallint;
--variables para fecha_mig tramite
   DEFINE v_t_f_envio                 date;
   DEFINE v_t_f_carga                 date;
   DEFINE v_t_f_respuesta             date;
   DEFINE v_t_f_liquida_cofi          date;
   DEFINE v_t_subproceso              smallint;
   DEFINE v_t_f_alta_registro         date;
--variables para ocg_formalizacion
   DEFINE v_f_id_ocg_formalizacion    decimal(9,0)  ;
   DEFINE v_f_id_ocg_detalle          decimal(9,0)  ;
   DEFINE v_f_id_ocg_tramite          decimal(9,0)  ;
   DEFINE v_f_id_derechohabiente      decimal(9,0)  ;
   DEFINE v_f_cve_ent_financiera      smallint      ;
   DEFINE v_f_num_ctr_int_ef          char(18)      ;
   DEFINE v_f_rfc                     char(13)      ;
   DEFINE v_f_curp                    char(18)      ;
   DEFINE v_f_ap_paterno              char(40)      ;
   DEFINE v_f_ap_materno              char(40)      ;
   DEFINE v_f_nombre                  char(40)      ;
   DEFINE v_f_viv97                   decimal(12,2) ;
   DEFINE v_f_num_escritura           char(8)       ;
   DEFINE v_f_notario                 decimal(4,0)  ;
   DEFINE v_f_ent_fed_notario         smallint      ;
   DEFINE v_f_mcpio_notario           smallint      ;
   DEFINE v_f_num_rpp                 char(15)      ;
   DEFINE v_f_folio_real              decimal(8,0)  ;
   DEFINE v_f_partida                 decimal(6,0)  ;
   DEFINE v_f_foja                    decimal(8,0)  ;
   DEFINE v_f_volumen                 decimal(6,0)  ;
   DEFINE v_f_libro                   decimal(6,0)  ;
   DEFINE v_f_tomo                    decimal(6,0)  ;
   DEFINE v_f_seccion                 decimal(6,0)  ;
   DEFINE v_f_ent_fed_inmueble        smallint      ;
   DEFINE v_f_mcpio_inmueble          decimal(5,0)  ;
   DEFINE v_f_domicilio_inmueble      char(30)      ;
   DEFINE v_f_valor_avaluo            decimal(15,2) ;
   DEFINE v_f_monto_credito           decimal(15,2) ;
   DEFINE v_f_plazo_credito           decimal(5,0)  ;
   DEFINE v_f_tpo_moneda              smallint      ;
   DEFINE v_f_tasa_base               char(20)      ;
   DEFINE v_f_margen                  char(20)      ;
   DEFINE v_f_tpo_credito             char(1)       ;
   DEFINE v_f_f_otorga_ent_fin        date          ;
   DEFINE v_f_f_registro_carta        date          ;
   DEFINE v_f_diagnostico             char(2)       ;
   DEFINE v_f_estado                  smallint      ;
   DEFINE v_f_usuario_reg_carta       char(20)      ;
   DEFINE v_f_situacion               smallint      ;
   DEFINE v_f_f_vigencia              date          ;
   DEFINE v_f_f_respuesta1            date          ;
   DEFINE v_f_f_saldo                 date          ;
   DEFINE v_f_genero                  char(1)       ;
 --variables para fecha_mig formalizacion
   DEFINE v_f_f_envio                 date;
   DEFINE v_f_f_carga                 date;
   DEFINE v_f_f_respuesta             date;
   DEFINE v_f_f_liquida_cofi          date;
   DEFINE v_f_subproceso              smallint;
   DEFINE v_f_f_alta_registro         date;
--variables para ocg_acreditado
   DEFINE v_acr_f_formalizacion       DATE; 
   DEFINE v_acr_f_marca_infonavit     DATE;
   DEFINE v_acr_f_solic_marca_prcr    DATE;
   DEFINE v_acr_f_conf_marca_prcr     DATE;
   DEFINE v_acr_f_liquida_credito     DATE;
   DEFINE v_acr_f_solic_desmarca_prcr DATE;
   DEFINE v_acr_f_conf_desmarca_prcr  DATE;
   DEFINE v_acr_estado                SMALLINT;
   DEFINE v_acr_situacion             SMALLINT;
   --variables para fecha de ocg_detalle
   DEFINE v_det_f_proceso_t           DATE;
   DEFINE v_det_f_proceso_f           DATE;
   DEFINE v_det_f_proceso_ug          DATE;
   -- define variables para ocg_ctr_transaccion
   DEFINE v_tr_id_ocg_ctr_transaccion   decimal(9,0);
   DEFINE v_tr_id_ocg_formalizacion     decimal(9,0);
   DEFINE v_tr_id_ocg_detalle           decimal(9,0);
   DEFINE v_tr_id_derechohabiente       decimal(9,0);
   DEFINE v_tr_id_referencia_cta        decimal(9,0);
   DEFINE v_tr_folio_referencia         decimal(9,0);
   DEFINE v_tr_proceso_cod              smallint;
   DEFINE v_tr_cve_ent_financiera       smallint;
   DEFINE v_tr_num_ctr_int_ef           char(18);
   DEFINE v_tr_folio                    decimal(9,0) ;
   DEFINE v_tr_f_transaccion            date;
   DEFINE v_tr_nss                      char(11);
   DEFINE v_tr_curp                     char(18);
   DEFINE v_tr_vivienda_97              decimal(12,2);
   DEFINE v_tr_periodo_pago             char(6);
   DEFINE v_tr_f_pago                   date;
   DEFINE v_tr_concepto                 smallint;
   DEFINE v_tr_f_proceso                date;
   DEFINE v_tr_estado                   smallint;
   DEFINE v_nvo_id_ocg_detalle_trx      decimal(9,0);
   DEFINE v_nvo_id_ocg_ctr_transaccion  decimal(9,0);
   DEFINE v_tr_f_envio                  date;
   DEFINE v_tr_f_carga                  date;
   DEFINE v_tr_f_respuesta              date;
   DEFINE v_tr_f_liquida_cofi           date;
   DEFINE v_tr_subproceso               smallint;
   DEFINE v_tr_f_alta_registro          date;
   DEFINE v_det_f_proceso_tr            date;

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_actualiza_sp003.trace';
   TRACE ON;


   LET v_error              = 0;

   FOREACH
-- inicia recuperación de datos no publicados 30/01/2019 con tipo de credito cofi en 43bis y apoyo en procesar
{
    SELECT ocg.id_derechohabiente,
           ocg.id_ocg_formalizacion,
           ocg.id_ocg_tramite,
           ocg.id_ocg_solicitud_ug,
           ocg.tpo_credito,
           t.nss
      INTO v_id_dh,
           v_id_ocg_formalizacion,
           v_id_ocg_tramite,
           v_id_ug,
           v_tpo_cred_ug,
           v_nss
      FROM ocg_solicitud_uso_garantia ocg,
           ocg_detalle det,
           ocg_ctr_transaccion t
     WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
       AND ocg.situacion in (110)
       AND det.subproceso = 3
       AND ocg.id_ocg_formalizacion = t.id_ocg_formalizacion
       AND ocg.id_derechohabiente = t.id_derechohabiente
       AND ocg.cve_ent_financiera = t.cve_ent_financiera
       AND ocg.importe_utilizado = t.vivienda_97
       AND year(ocg.f_vencimiento)||lpad(month(ocg.f_vencimiento  ),2,0)  = t.periodo_pago
       AND t.estado = 80
       AND ocg.solicitud_saldo in (2)
       AND t.concepto IN( 417)
       AND t.f_pago in ('01302019','02132019')
       AND ocg.tpo_credito NOT IN  ('A','C')

-- query para cambiar tipo de crédito a registro no publicado y pagado el día 27/02/2019
   SELECT ocg.id_derechohabiente,
          ocg.id_ocg_formalizacion,
          ocg.id_ocg_tramite,
          ocg.id_ocg_solicitud_ug,
          ocg.tpo_credito,
          t.nss
     INTO v_id_dh,
          v_id_ocg_formalizacion,
          v_id_ocg_tramite,
          v_id_ug,
          v_tpo_cred_ug,
          v_nss
     FROM ocg_solicitud_uso_garantia ocg,
          ocg_detalle det,
          ocg_ctr_transaccion t
    WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
      AND ocg.situacion in (100)
      AND det.subproceso = 3
      AND ocg.id_ocg_formalizacion = t.id_ocg_formalizacion
      AND ocg.id_derechohabiente = t.id_derechohabiente
      AND ocg.cve_ent_financiera = t.cve_ent_financiera
      AND ocg.importe_utilizado = t.vivienda_97
      AND year(ocg.f_vencimiento)||lpad(month(ocg.f_vencimiento  ),2,0) = t.periodo_pago
      AND t.estado = 30
      AND ocg.solicitud_saldo in (2)
      AND t.concepto IN( 407)
      AND ocg.tpo_credito NOT IN  ('A','C')
      AND t.f_proceso = '02272019' 
}
   select  ocg.id_derechohabiente,
	   ocg.id_ocg_formalizacion,
	   ocg.id_ocg_tramite,
	   ocg.id_ocg_solicitud_ug,
	   ocg.tpo_credito,
	   det.nss
     INTO v_id_dh,
          v_id_ocg_formalizacion,
          v_id_ocg_tramite,
          v_id_ug,
          v_tpo_cred_ug,
          v_nss
      from ocg_solicitud_uso_garantia ocg, ocg_detalle det
     where ocg.id_ocg_detalle = det.id_ocg_detalle
       and ocg.id_derechohabiente = det.id_derechohabiente
       and id_ocg_solicitud_ug in 
(
607730,
607437,
606614,
607745,
605837,
606163,
605442,
604985,
606651,
607350,
604614,
604401,
606923,
607983
)

   IF v_tpo_cred_ug <> 'A' OR 
      v_tpo_cred_ug <> 'C' THEN

-- inicia generación y actualización de registro para tramite
      IF v_id_ocg_tramite IS NOT NULL THEN

      SELECT first 1
             id_ocg_tramite,
             id_ocg_detalle,
             cve_ent_financiera,
             id_derechohabiente,
             rfc,
             curp,
             ap_paterno,
             ap_materno,
             nombre,
             num_bimestres,
             viv97,
             f_saldo,
             tpo_credito,
             f_vigencia,
             f_respuesta,
             diagnostico,
             estado,
             situacion
        INTO v_id_ocg_tramite,
             v_t_id_ocg_detalle,
             v_t_cve_ent_financiera,
             v_t_id_derechohabiente,
             v_t_rfc,
             v_t_curp,
             v_t_ap_paterno,
             v_t_ap_materno,
             v_t_nombre,
             v_t_num_bimestres,
             v_t_viv97,
             v_t_f_saldo,
             v_t_tpo_credito,
             v_t_f_vigencia,
             v_t_f_respuesta1,
             v_t_diagnostico,
             v_t_estado,
             v_t_situacion
        FROM ocg_tramite
       WHERE id_ocg_tramite     = v_id_ocg_tramite
         AND id_derechohabiente = v_id_dh;

      LET v_nvo_id_ocg_tramite = seq_ocg_tramite.nextval;
      LET v_id_ocg_detalle_t   = seq_ocg_detalle.nextval;

      SELECT f_envio,
             f_carga,
             f_respuesta,
             f_liquida_cofi,
             subproceso,
             f_alta_registro
        INTO v_t_f_envio,
             v_t_f_carga,
             v_t_f_respuesta,
             v_t_f_liquida_cofi,
             v_t_subproceso,
             v_t_f_alta_registro
        FROM ocg_fecha_mig
       WHERE id_derechohabiente = v_id_dh
         AND id_ocg_detalle     = v_t_id_ocg_detalle
         AND id_ocg_referencia  = v_id_ocg_tramite
         AND subproceso         = 1;

      SELECT f_proceso
        INTO v_det_f_proceso_t
        FROM ocg_detalle
       WHERE id_ocg_detalle = v_t_id_ocg_detalle
         AND subproceso = 1;

      INSERT INTO ocg_detalle
           VALUES(v_id_ocg_detalle_t,
                  0,
                  v_t_id_derechohabiente,
                  1,
                  v_det_f_proceso_t,
                  v_t_cve_ent_financiera,
                  v_nss );

      INSERT INTO ocg_tramite
           VALUES(v_nvo_id_ocg_tramite,
                  v_id_ocg_detalle_t,
                  v_t_cve_ent_financiera,
                  v_t_id_derechohabiente,
                  v_t_rfc,
                  v_t_curp,
                  v_t_ap_paterno,
                  v_t_ap_materno,
                  v_t_nombre,
                  v_t_num_bimestres,
                  v_t_viv97,
                  v_t_f_saldo,
                  "A",
                  v_t_f_vigencia,
                  v_t_f_respuesta1,
                  v_t_diagnostico,
                  v_t_estado,
                  v_t_situacion);

      INSERT INTO ocg_fecha_mig
           VALUES(v_nvo_id_ocg_tramite,
                  v_id_ocg_detalle_t,
                  v_t_id_derechohabiente,
                  v_t_f_envio,
                  v_t_f_carga,
                  v_t_f_respuesta,
                  v_t_f_liquida_cofi,
                  v_t_subproceso,
                  v_t_f_alta_registro);

      UPDATE ocg_tramite 
         SET situacion = 500
       WHERE id_ocg_tramite     = v_id_ocg_tramite
         AND id_derechohabiente = v_id_dh;

      END IF
-- finaliza generación y actualización de registro para tramite

---- inicia generación y actualización de registro para formalización
      SELECT first 1
             id_ocg_formalizacion,
             id_ocg_detalle,
             id_ocg_tramite,
             id_derechohabiente,
             cve_ent_financiera,
             num_ctr_int_ef,
             rfc,
             curp,
             ap_paterno,
             ap_materno,
             nombre,
             viv97,
             num_escritura,
             notario,
             ent_fed_notario,
             mcpio_notario,
             num_rpp,
             folio_real,
             partida,
             foja,
             volumen,
             libro,
             tomo,
             seccion,
             ent_fed_inmueble,
             mcpio_inmueble,
             domicilio_inmueble,
             valor_avaluo,
             monto_credito,
             plazo_credito,
             tpo_moneda,
             tasa_base,
             margen,
             tpo_credito,
             f_otorga_ent_fin,
             f_registro_carta,
             diagnostico,
             estado,
             usuario_reg_carta,
             situacion,
             f_vigencia,
             f_respuesta,
             f_saldo,
             genero
        INTO v_f_id_ocg_formalizacion,
             v_f_id_ocg_detalle,
             v_f_id_ocg_tramite,
             v_f_id_derechohabiente,
             v_f_cve_ent_financiera,
             v_f_num_ctr_int_ef,
             v_f_rfc,
             v_f_curp,
             v_f_ap_paterno,
             v_f_ap_materno,
             v_f_nombre,
             v_f_viv97,
             v_f_num_escritura,
             v_f_notario,
             v_f_ent_fed_notario,
             v_f_mcpio_notario,
             v_f_num_rpp,
             v_f_folio_real,
             v_f_partida,
             v_f_foja,
             v_f_volumen,
             v_f_libro,
             v_f_tomo,
             v_f_seccion,
             v_f_ent_fed_inmueble,
             v_f_mcpio_inmueble,
             v_f_domicilio_inmueble,
             v_f_valor_avaluo,
             v_f_monto_credito,
             v_f_plazo_credito,
             v_f_tpo_moneda,
             v_f_tasa_base,
             v_f_margen,
             v_f_tpo_credito,
             v_f_f_otorga_ent_fin,
             v_f_f_registro_carta,
             v_f_diagnostico,
             v_f_estado,
             v_f_usuario_reg_carta,
             v_f_situacion,
             v_f_f_vigencia,
             v_f_f_respuesta1,
             v_f_f_saldo,
             v_f_genero
        FROM ocg_formalizacion 
       WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
         AND id_derechohabiente   = v_id_dh;

      SELECT f_formalizacion,   
             f_marca_infonavit,
             f_solic_marca_prcr,
             f_conf_marca_prcr,
             f_liquida_credito,
             f_solic_desmarca_prcr,
             f_conf_desmarca_prcr,
             estado,
             situacion
       INTO  v_acr_f_formalizacion,
             v_acr_f_marca_infonavit,
             v_acr_f_solic_marca_prcr,
             v_acr_f_conf_marca_prcr,
             v_acr_f_liquida_credito,
             v_acr_f_solic_desmarca_prcr,
             v_acr_f_conf_desmarca_prcr,
             v_acr_estado,
             v_acr_situacion
        FROM ocg_acreditado 
       WHERE id_ocg_formalizacion = v_id_ocg_formalizacion;

      LET v_nvo_id_ocg_formalizacion = seq_ocg_formalizacion.nextval;
      LET v_id_ocg_detalle_f     = seq_ocg_detalle.nextval;

      SELECT f_envio,
             f_carga,
             f_respuesta,
             f_liquida_cofi,
             subproceso,
             f_alta_registro
        INTO v_f_f_envio,
             v_f_f_carga,
             v_f_f_respuesta,
             v_f_f_liquida_cofi,
             v_f_subproceso,
             v_f_f_alta_registro
        FROM ocg_fecha_mig
       WHERE id_derechohabiente = v_id_dh
         AND id_ocg_detalle     = v_f_id_ocg_detalle
         AND id_ocg_referencia  = v_id_ocg_formalizacion
         AND subproceso         = 2;
         
      SELECT f_proceso
        INTO v_det_f_proceso_f
        FROM ocg_detalle
       WHERE id_ocg_detalle = v_f_id_ocg_detalle
         AND subproceso = 2;

      INSERT INTO ocg_detalle
           VALUES(v_id_ocg_detalle_f,
                  0,
                  v_f_id_derechohabiente,
                  2,
                  v_det_f_proceso_f,
                  v_f_cve_ent_financiera,
                  v_nss );

      INSERT INTO ocg_formalizacion
           VALUES(v_nvo_id_ocg_formalizacion,
                  v_id_ocg_detalle_f,
                  v_f_id_ocg_tramite,
                  v_f_id_derechohabiente,
                  v_f_cve_ent_financiera,
                  v_f_num_ctr_int_ef,
                  v_f_rfc,
                  v_f_curp,
                  v_f_ap_paterno,
                  v_f_ap_materno,
                  v_f_nombre,
                  v_f_viv97,
                  v_f_num_escritura,
                  v_f_notario,
                  v_f_ent_fed_notario,
                  v_f_mcpio_notario,
                  v_f_num_rpp,
                  v_f_folio_real,
                  v_f_partida,
                  v_f_foja,
                  v_f_volumen,
                  v_f_libro,
                  v_f_tomo,
                  v_f_seccion,
                  v_f_ent_fed_inmueble,
                  v_f_mcpio_inmueble,
                  v_f_domicilio_inmueble,
                  v_f_valor_avaluo,
                  v_f_monto_credito,
                  v_f_plazo_credito,
                  v_f_tpo_moneda,
                  v_f_tasa_base,
                  v_f_margen,
                  "A",
                  v_f_f_otorga_ent_fin,
                  v_f_f_registro_carta,
                  v_f_diagnostico,
                  v_f_estado,
                  v_f_usuario_reg_carta,
                  v_f_situacion,
                  v_f_f_vigencia,
                  v_f_f_respuesta1,
                  v_f_f_saldo,
                  v_f_genero);
                  
      INSERT INTO ocg_acreditado
           VALUES( v_nvo_id_ocg_formalizacion,
                  v_acr_f_formalizacion,
                  v_acr_f_marca_infonavit,
                  v_acr_f_solic_marca_prcr,
                  v_acr_f_conf_marca_prcr,
                  v_acr_f_liquida_credito,
                  v_acr_f_solic_desmarca_prcr,
                  v_acr_f_conf_desmarca_prcr,
                  v_acr_estado,
                  v_acr_situacion );

      INSERT INTO ocg_fecha_mig
           VALUES(v_nvo_id_ocg_formalizacion,
                  v_id_ocg_detalle_f,
                  v_f_id_derechohabiente, 
                  v_f_f_envio,         
                  v_f_f_carga,         
                  v_f_f_respuesta,     
                  v_f_f_liquida_cofi,  
                  v_f_subproceso,      
                  v_f_f_alta_registro);

      INSERT INTO safre_tmp:tmp_reg_ug_actualizado
           VALUES(v_id_dh,
                  v_id_ocg_formalizacion,
                  '2',
                  v_nvo_id_ocg_formalizacion);

      UPDATE ocg_formalizacion
         SET situacion = 500
       WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
         AND id_derechohabiente   = v_id_dh;  
         
      UPDATE ocg_acreditado
         SET situacion = 500
       WHERE id_ocg_formalizacion = v_id_ocg_formalizacion;  

-- inicia actualización de registros en solciitud_uso_garantia
      FOREACH
         SELECT id_ocg_solicitud_ug,
                id_ocg_detalle,
                id_ocg_formalizacion,
                id_ocg_tramite,
                id_derechohabiente,
                cve_ent_financiera,
                num_ctr_int_ef,
                importe_solicitado,
                f_vencimiento,
                importe_utilizado, 
                tpo_credito,
                solicitud_saldo,
                diagnostico,
                estado,
                situacion
           INTO v_id_ocg_solicitud_ug,
                v_id_ocg_detalle,
                v_id_ocg_formalizacion,
                v_id_ocg_tramite,
                v_id_derechohabiente,
                v_cve_ent_financiera,
                v_num_ctr_int_ef,
                v_importe_solicitado,
                v_f_vencimiento,
                v_importe_utilizado,
                v_tpo_credito,
                v_solicitud_saldo,
                v_diagnostico,
                v_estado,
                v_situacion
           FROM ocg_solicitud_uso_garantia
          WHERE id_derechohabiente   = v_id_dh
            AND id_ocg_formalizacion = v_id_ocg_formalizacion
            AND situacion  <> 500
            AND  id_ocg_solicitud_ug not in (
         select id_nvo_ocg_referencia
           from safre_tmp:tmp_reg_ug_actualizado
          where subproceso = 3 )


         LET v_nvo_id_ocg_solicitud_ug  = seq_ocg_solic_ug.nextval;
         LET v_id_ocg_detalle_ug    = seq_ocg_detalle.nextval;

        SELECT f_envio,
               f_carga,
               f_respuesta,
               f_liquida_cofi,
               subproceso,
               f_alta_registro
          INTO v_ug_f_envio,
               v_ug_f_carga,
               v_ug_f_respuesta,
               v_ug_f_liquida_cofi,
               v_ug_subproceso,
               v_ug_f_alta_registro
          FROM ocg_fecha_mig
         WHERE id_derechohabiente = v_id_derechohabiente
           AND id_ocg_detalle     = v_id_ocg_detalle
           AND id_ocg_referencia  = v_id_ocg_solicitud_ug
           AND subproceso         = 3;

      SELECT f_proceso
        INTO v_det_f_proceso_ug
        FROM ocg_detalle
       WHERE id_ocg_detalle = v_id_ocg_detalle
         AND subproceso = 3;

      INSERT INTO ocg_detalle
           VALUES(v_id_ocg_detalle_ug,
                  0,
                  v_id_derechohabiente,
                  3,
                  v_det_f_proceso_ug,
                  v_cve_ent_financiera,
                  v_nss );

         INSERT INTO ocg_solicitud_uso_garantia
              VALUES(v_nvo_id_ocg_solicitud_ug,
                     v_id_ocg_detalle_ug,
                     v_nvo_id_ocg_formalizacion,
                     v_id_ocg_tramite,
                     v_id_derechohabiente,
                     v_cve_ent_financiera,
                     v_num_ctr_int_ef,
                     v_importe_solicitado,
                     v_f_vencimiento,
                     v_importe_utilizado,
                     "A",
                     v_solicitud_saldo,
                     v_diagnostico,
                     v_estado,
                     v_situacion );

         INSERT INTO ocg_fecha_mig
              VALUES(v_nvo_id_ocg_solicitud_ug,
                     v_id_ocg_detalle_ug,
                     v_id_derechohabiente,
                     v_ug_f_envio,
                     v_ug_f_carga,
                     v_ug_f_respuesta,
                     v_ug_f_liquida_cofi,
                     v_ug_subproceso,
                     v_ug_f_alta_registro);

         INSERT INTO safre_tmp:tmp_reg_ug_actualizado
              VALUES(v_id_derechohabiente,
                     v_id_ocg_solicitud_ug,
                     '3',
                     v_nvo_id_ocg_solicitud_ug);

         UPDATE ocg_solicitud_uso_garantia
            SET situacion = 500
          WHERE id_derechohabiente = v_id_derechohabiente
            AND id_ocg_detalle     = v_id_ocg_detalle
            AND id_ocg_solicitud_ug  = v_id_ocg_solicitud_ug;
              
          LET v_det_f_proceso_ug      = "" ;
          LET v_ug_f_carga            = "" ;
          LET v_ug_f_respuesta        = "" ;
          LET v_ug_f_liquida_cofi     = "" ;
          LET v_ug_subproceso         = "" ;
          LET v_ug_f_alta_registro    = "" ;
          LET v_id_ocg_solicitud_ug   = "" ;
          LET v_id_ocg_detalle        = "" ;
          LET v_id_ocg_tramite        = "" ;
          LET v_id_derechohabiente    = "" ;
          LET v_cve_ent_financiera    = "" ;
          LET v_num_ctr_int_ef        = "" ;
          LET v_importe_solicitado    = "" ;
          LET v_f_vencimiento         = "" ;
          LET v_importe_utilizado     = "" ;
          LET v_tpo_credito           = "" ;
          LET v_solicitud_saldo       = "" ;
          LET v_diagnostico           = "" ;
          LET v_estado                = "" ;
          LET v_situacion             = "" ;

      END FOREACH

-- inicia actualización de registros para ocg_ctr_transaccion
 
      FOREACH
         SELECT id_ocg_ctr_transaccion,
                id_ocg_formalizacion,
                id_ocg_detalle,
                id_derechohabiente,
                id_referencia_cta,
                folio_referencia,
                proceso_cod,
                cve_ent_financiera ,
                num_ctr_int_ef,
                folio,
                f_transaccion,
                nss,
                curp,
                vivienda_97,
                periodo_pago,
                f_pago,
                concepto,
                f_proceso,
                estado
           INTO v_tr_id_ocg_ctr_transaccion,
                v_tr_id_ocg_formalizacion  ,
                v_tr_id_ocg_detalle        ,
                v_tr_id_derechohabiente    ,
                v_tr_id_referencia_cta     ,
                v_tr_folio_referencia      ,
                v_tr_proceso_cod           ,
                v_tr_cve_ent_financiera    ,
                v_tr_num_ctr_int_ef        ,
                v_tr_folio                 ,
                v_tr_f_transaccion         ,
                v_tr_nss                   ,
                v_tr_curp                  ,
                v_tr_vivienda_97           ,
                v_tr_periodo_pago          ,
                v_tr_f_pago                ,
                v_tr_concepto              ,
                v_tr_f_proceso             ,
                v_tr_estado
           FROM ocg_ctr_transaccion
          WHERE id_derechohabiente   = v_id_dh
            AND id_ocg_formalizacion = v_id_ocg_formalizacion
            AND estado   <> 500
            AND id_ocg_ctr_transaccion not in (
         select id_nvo_ocg_referencia
           from safre_tmp:tmp_reg_ug_actualizado
          where subproceso = 4 )

            
         LET v_nvo_id_ocg_detalle_trx      = seq_ocg_detalle.nextval;
         LET v_nvo_id_ocg_ctr_transaccion  = seq_dis_ctr_aps_tns.nextval ;

        SELECT f_envio,
               f_carga,
               f_respuesta,
               f_liquida_cofi,
               subproceso,
               f_alta_registro
          INTO v_tr_f_envio,
               v_tr_f_carga,
               v_tr_f_respuesta,
               v_tr_f_liquida_cofi,
               v_tr_subproceso,
               v_tr_f_alta_registro
          FROM ocg_fecha_mig
         WHERE id_derechohabiente = v_tr_id_derechohabiente
           AND id_ocg_detalle     = v_tr_id_ocg_detalle
           AND id_ocg_referencia  = v_tr_id_ocg_ctr_transaccion
           AND subproceso         = 4;

         SELECT f_proceso
           INTO v_det_f_proceso_tr
           FROM ocg_detalle
          WHERE id_ocg_detalle = v_tr_id_ocg_detalle
            AND subproceso = 4;

         INSERT INTO ocg_detalle
              VALUES(v_nvo_id_ocg_detalle_trx,
                     0,
                     v_tr_id_derechohabiente,
                     4,
                     v_det_f_proceso_tr,
                     v_tr_cve_ent_financiera,
                     v_tr_nss );

         INSERT INTO ocg_ctr_transaccion
              VALUES(v_nvo_id_ocg_ctr_transaccion,
                     v_nvo_id_ocg_formalizacion,
                     v_nvo_id_ocg_detalle_trx,
                     v_tr_id_derechohabiente,
                     v_tr_id_referencia_cta,
                     v_tr_folio_referencia,
                     v_tr_proceso_cod,
                     v_tr_cve_ent_financiera,
                     v_tr_num_ctr_int_ef,
                     v_tr_folio,
                     v_tr_f_transaccion,
                     v_tr_nss,
                     v_tr_curp,
                     v_tr_vivienda_97,
                     v_tr_periodo_pago,
                     v_tr_f_pago,
                     v_tr_concepto,
                     v_tr_f_proceso,
                     v_tr_estado);

         INSERT INTO ocg_fecha_mig
              VALUES(v_nvo_id_ocg_ctr_transaccion,
                     v_nvo_id_ocg_detalle_trx,
                     v_tr_id_derechohabiente,
                     v_tr_f_envio,
                     v_tr_f_carga,
                     v_tr_f_respuesta,
                     v_tr_f_liquida_cofi,
                     v_tr_subproceso,
                     v_tr_f_alta_registro);

         INSERT INTO safre_tmp:tmp_reg_ug_actualizado
              VALUES(v_tr_id_derechohabiente,
                     v_tr_id_ocg_ctr_transaccion,
                     '4',
                     v_nvo_id_ocg_ctr_transaccion);

         UPDATE ocg_ctr_transaccion
            SET estado = 500
          WHERE id_derechohabiente    =  v_tr_id_derechohabiente
            AND id_ocg_detalle         =  v_tr_id_ocg_detalle
            AND id_ocg_ctr_transaccion =  v_tr_id_ocg_ctr_transaccion;

         LET v_tr_id_ocg_ctr_transaccion   = "";
         LET v_tr_id_ocg_formalizacion     = "";
         LET v_tr_id_ocg_detalle           = "";
         LET v_tr_id_derechohabiente       = "";
         LET v_tr_id_referencia_cta        = "";
         LET v_tr_folio_referencia         = "";
         LET v_tr_proceso_cod              = "";
         LET v_tr_cve_ent_financiera       = "";
         LET v_tr_num_ctr_int_ef           = "";
         LET v_tr_folio                    = "";
         LET v_tr_f_transaccion            = "";
         LET v_tr_nss                      = "";
         LET v_tr_curp                     = "";
         LET v_tr_vivienda_97              = "";
         LET v_tr_periodo_pago             = "";
         LET v_tr_f_pago                   = "";
         LET v_tr_concepto                 = "";
         LET v_tr_f_proceso                = "";
         LET v_tr_estado                   = "";
         LET v_nvo_id_ocg_detalle_trx      = "";
         LET v_nvo_id_ocg_ctr_transaccion  = "";
         LET v_tr_f_envio                  = "";
         LET v_tr_f_carga                  = "";
         LET v_tr_f_respuesta              = "";
         LET v_tr_f_liquida_cofi           = "";
         LET v_tr_subproceso               = "";
         LET v_tr_f_alta_registro          = "";
         LET v_det_f_proceso_tr            = "";

      END FOREACH

   END IF

   LET v_id_dh                        = "" ;
   LET v_id_ug                        = "" ;
   LET v_tpo_cred_ug                  = "" ;
   LET v_nss                          = "" ;
--variables para solicutud_uso_garanti= "" ;
   LET v_id_ocg_solicitud_ug          = "" ;
   LET v_id_ocg_detalle               = "" ;
   LET v_id_ocg_formalizacion         = "" ;
   LET v_id_ocg_tramite               = "" ;
   LET v_id_derechohabiente           = "" ;
   LET v_cve_ent_financiera           = "" ;
   LET v_num_ctr_int_ef               = "" ;
   LET v_importe_solicitado           = "" ;
   LET v_f_vencimiento                = "" ;
   LET v_importe_utilizado            = "" ;
   LET v_tpo_credito                  = "" ;
   LET v_solicitud_saldo              = "" ;
   LET v_diagnostico                  = "" ;
   LET v_estado                       = "" ;
   LET v_situacion                    = "" ;
--vLET s para nuevos registros        = "" ;
   LET v_nvo_id_ocg_formalizacion     = "" ;
   LET v_nvo_id_ocg_solicitud_ug      = "" ;
   LET v_id_ocg_detalle_f             = "" ;
   LET v_id_ocg_detalle_ug            = "" ;
   LET v_nvo_tpo_credito              = "" ;
   LET v_nvo_situacion                = "" ;
   LET v_id_ocg_detalle_t             = "" ;
   LET v_nvo_id_ocg_tramite           = "" ;
-- LET es para ocg_fecha_mig ug       = "" ;
   LET v_ug_f_envio                   = "" ;
   LET v_ug_f_carga                   = "" ;
   LET v_ug_f_respuesta               = "" ;
   LET v_ug_f_liquida_cofi            = "" ;
   LET v_ug_subproceso                = "" ;
   LET v_ug_f_alta_registro           = "" ;
--vLET s para ocg_tramite             = "" ;
   LET v_t_id_ocg_detalle             = "" ;
   LET v_t_cve_ent_financiera         = "" ;
   LET v_t_id_derechohabiente         = "" ;
   LET v_t_rfc                        = "" ;
   LET v_t_curp                       = "" ;
   LET v_t_ap_paterno                 = "" ;
   LET v_t_ap_materno                 = "" ;
   LET v_t_nombre                     = "" ;
   LET v_t_num_bimestres              = "" ;
   LET v_t_viv97                      = "" ;
   LET v_t_f_saldo                    = "" ;
   LET v_t_tpo_credito                = "" ;
   LET v_t_f_vigencia                 = "" ;
   LET v_t_f_respuesta1               = "" ;
   LET v_t_diagnostico                = "" ;
   LET v_t_estado                     = "" ;
   LET v_t_situacion                  = "" ;
--vLET s para fecha_mig tramite       = "" ;
   LET v_t_f_envio                    = "" ;
   LET v_t_f_carga                    = "" ;
   LET v_t_f_respuesta                = "" ;
   LET v_t_f_liquida_cofi             = "" ;
   LET v_t_subproceso                 = "" ;
   LET v_t_f_alta_registro            = "" ;
--vLET s para ocg_formalizacion       = "" ;
   LET v_f_id_ocg_formalizacion       = "" ;
   LET v_f_id_ocg_detalle             = "" ;
   LET v_f_id_ocg_tramite             = "" ;
   LET v_f_id_derechohabiente         = "" ;
   LET v_f_cve_ent_financiera         = "" ;
   LET v_f_num_ctr_int_ef             = "" ;
   LET v_f_rfc                        = "" ;
   LET v_f_curp                       = "" ;
   LET v_f_ap_paterno                 = "" ;
   LET v_f_ap_materno                 = "" ;
   LET v_f_nombre                     = "" ;
   LET v_f_viv97                      = "" ;
   LET v_f_num_escritura              = "" ;
   LET v_f_notario                    = "" ;
   LET v_f_ent_fed_notario            = "" ;
   LET v_f_mcpio_notario              = "" ;
   LET v_f_num_rpp                    = "" ;
   LET v_f_folio_real                 = "" ;
   LET v_f_partida                    = "" ;
   LET v_f_foja                       = "" ;
   LET v_f_volumen                    = "" ;
   LET v_f_libro                      = "" ;
   LET v_f_tomo                       = "" ;
   LET v_f_seccion                    = "" ;
   LET v_f_ent_fed_inmueble           = "" ;
   LET v_f_mcpio_inmueble             = "" ;
   LET v_f_domicilio_inmueble         = "" ;
   LET v_f_valor_avaluo               = "" ;
   LET v_f_monto_credito              = "" ;
   LET v_f_plazo_credito              = "" ;
   LET v_f_tpo_moneda                 = "" ;
   LET v_f_tasa_base                  = "" ;
   LET v_f_margen                     = "" ;
   LET v_f_tpo_credito                = "" ;
   LET v_f_f_otorga_ent_fin           = "" ;
   LET v_f_f_registro_carta           = "" ;
   LET v_f_diagnostico                = "" ;
   LET v_f_estado                     = "" ;
   LET v_f_usuario_reg_carta          = "" ;
   LET v_f_situacion                  = "" ;
   LET v_f_f_vigencia                 = "" ;
   LET v_f_f_respuesta1               = "" ;
   LET v_f_f_saldo                    = "" ;
   LET v_f_genero                     = "" ;
 --LET es para fecha_mig formalizac   = "" ;
   LET v_f_f_envio                    = "" ;
   LET v_f_f_carga                    = "" ;
   LET v_f_f_respuesta                = "" ;
   LET v_f_f_liquida_cofi             = "" ;
   LET v_f_subproceso                 = "" ;
   LET v_f_f_alta_registro            = "" ;

   END FOREACH

   RETURN v_error;
END FUNCTION;


