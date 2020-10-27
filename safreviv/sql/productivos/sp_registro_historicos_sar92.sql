






CREATE PROCEDURE "safreviv".sp_registro_historicos_sar92(p_folio DECIMAL(9,0),
                                              p_proceso_cod  SMALLINT,
                                              p_pid DECIMAL (9,0)
                                              )
 RETURNING SMALLINT, INTEGER, VARCHAR(255), DECIMAL (11,0)
   --tmp_cza_sar92
   DEFINE v_tmp_cza_tpo_registro     CHAR(2);
   DEFINE v_tmp_cza_id_servicio      CHAR(2);
   DEFINE v_tmp_cza_id_operacion     CHAR(2);
   DEFINE v_tmp_cza_tpo_ent_origen   CHAR(2);
   DEFINE v_tmp_cza_cve_ent_origen   CHAR(3);
   DEFINE v_tmp_cza_tpo_ent_destino  CHAR(2);
   DEFINE v_tmp_cza_cve_ent_destino  CHAR(3);
   DEFINE v_tmp_cza_f_envio_lote     DATE;
   DEFINE v_tmp_cza_consecutivo_dia  CHAR(3);
   DEFINE v_tmp_cza_modalidad_recep  CHAR(2);
   DEFINE v_tmp_cza_result_operacion CHAR(2);
   DEFINE v_tmp_cza_motivo_rch_lote  CHAR(9);
   DEFINE v_tmp_cza_valor_aiv        DECIMAL(32);
   
   
   --tmp_det_sar92
   DEFINE v_tmp_det_tpo_registro         CHAR(2);
   DEFINE v_tmp_det_cve_icefa_generadora CHAR(3);
   DEFINE v_tmp_det_rfc_patron           CHAR(13);
   DEFINE v_tmp_det_num_expediente       CHAR(9);
   DEFINE v_tmp_det_nom_patron           CHAR(40);
   DEFINE v_tmp_det_rfc                  CHAR(13);
   DEFINE v_tmp_det_nss                  CHAR(11);
   DEFINE v_tmp_det_nci_banco            CHAR(30);
   DEFINE v_tmp_det_nom_trabajador       CHAR(120);
   DEFINE v_tmp_det_aivs_ap              DECIMAL(24);
   DEFINE v_tmp_det_aivs_val_ant         DECIMAL(24);
   DEFINE v_tmp_det_id_unico             DECIMAL(11,0);
   DEFINE v_tmp_det_cve_referencia       CHAR(27);
   DEFINE v_tmp_det_periodo_pago         CHAR(6);
   DEFINE v_tmp_det_diag                 CHAR(2);
   DEFINE v_tmp_det_motivo_rch_det       CHAR(9);
   DEFINE v_tmp_det_ind_cta_local_bd     CHAR(1);
   DEFINE v_tmp_det_imp_viv              CHAR(12);
   
   --tmp_sum_sar92
   DEFINE v_tmp_sum_tpo_registro     CHAR(2);
   DEFINE v_tmp_sum_tpo_ent_origen   CHAR(2);
   DEFINE v_tmp_sum_cve_ent_origen   CHAR(3);
   DEFINE v_tmp_sum_tot_registros    DECIMAL(9);
   DEFINE v_tmp_sum_tot_aivs_ap      DECIMAL(24);
   DEFINE v_tmp_sum_tot_aivs_val_ant DECIMAL(24);
   DEFINE v_tmp_sum_tot_imp_viv      DECIMAL(20);
   
   --pag_cza_sar92
   DEFINE v_pag_cza_valor_aiv DECIMAL(19,14);
   
   --pag_det_sar92
   DEFINE v_pag_det_aivs_ap      DECIMAL(18,6);
   DEFINE v_pag_det_aivs_val_ant DECIMAL(18,6);
   DEFINE v_pag_det_imp_viv      DECIMAL(12,2);
   DEFINE v_id_decreto           DECIMAL(11,0);
   
   --pag_sum_sar92
   DEFINE v_pag_sum_tot_aivs_ap      DECIMAL(18,6);
   DEFINE v_pag_sum_tot_aivs_val_ant DECIMAL(18,6);
   DEFINE v_pag_sum_tot_imp_viv      DECIMAL(18,2);      
  
  
      --TABLA tmp_sum_sar92
   DEFINE tmp_sum_pag_suma_aivs_a           DECIMAL(18,6);    
   DEFINE tmp_sum_pag_suma_aivs_val_ant     DECIMAL(18,6);    
   DEFINE tmp_sum_pag_suma_imp_viv          DECIMAL(18,2);    
                                                           
   --TABLA tmp_det_sar92
   DEFINE tmp_sum_pag_tot_aivs_ap           DECIMAL(18,6);
   DEFINE tmp_sum_pag_tot_aivs_val_ant      DECIMAL(18,6);
   DEFINE tmp_sum_pag_tot_imp_viv           DECIMAL(18,2);
   
     -- verificacion de registros recibidos
   DEFINE v_num_registros_detalle         SMALLINT; -- registros detalle
   DEFINE v_num_registros_detalle_sum     SMALLINT; -- registros de detalle indicados en sumario
   
   -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide         SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide    SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide           SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                   SMALLINT;

     -- Control de Excepciones
   DEFINE v_error                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;
   DEFINE v_contador                      INTEGER;
   
   
   ON EXCEPTION 
      SET v_error, isam_err, err_txt
      --TRACE 'Ocurrio el error:'||v_error;
      --TRACE isam_err;
      --TRACE err_txt;
      RETURN v_error, isam_err, err_txt, v_tmp_det_id_unico;
   END EXCEPTION --WITH RESUME
   
   --SET DEBUG FILE TO '/safreviv/hist_sar92.TRACE';
      
      --TRACE "FOLIO "||P_folio  ;
   
   ALTER SEQUENCE seq_pag_det_sar92 RESTART 1;
   
   -- se asignan los codigos de error
   LET v_err_numero_regs_detalle_no_coincide         = 1; -- error por inconsistencia de numero de registros de detalle
   LET v_err_suma_aportacion_patronal_no_coincide    = 2;
   LET v_err_suma_amortizacion_no_coincide           = 3;
   LET v_err_suma_AIVS_no_coincide                   = 4;
   
   LET v_contador = 0 ;
   
   LET v_tmp_det_id_unico = NULL;

    -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
       SET 
        folio = P_folio,
       estado = 2 -- integrado
     WHERE proceso_cod    = p_proceso_cod
       AND opera_cod      = 1 -- archivo cargado
       AND estado         = 1; -- etapa de carga
   
    -- Agregar folio a operacion de integracion
    UPDATE bat_ctr_operacion 
       SET folio       = P_folio
     WHERE proceso_cod = p_proceso_cod 
       AND opera_cod   = 2
       AND pid         = p_pid;
       
   
   -- se obtiene la suma de los monto de la tabla del sumario
   SELECT  SUM(tot_aivs_ap)/1000000,
           SUM(tot_aivs_val_ant    )/1000000,        
           SUM(tot_imp_viv  )/100
   INTO
           tmp_sum_pag_suma_aivs_a       ,
           tmp_sum_pag_suma_aivs_val_ant ,
           tmp_sum_pag_suma_imp_viv     
      FROM safre_tmp:tmp_sum_sar92;

   -- se obtiene la suma de los monto de la tabla de detalle       
   SELECT 
      SUM(aivs_ap) / 1000000 ,
      SUM(aivs_val_ant) / 1000000 ,
      SUM(imp_viv      ) / 100
   INTO 
      tmp_sum_pag_tot_aivs_ap       ,
      tmp_sum_pag_tot_aivs_val_ant  ,
      tmp_sum_pag_tot_imp_viv       
   FROM safre_tmp:tmp_det_sar92;

   -- se verifica que el numero de registros en la tabla de detalle sea el mismo
   -- que el encontrado en el sumario
   SELECT COUNT(*)
   INTO v_num_registros_detalle
   FROM
      safre_tmp:tmp_det_sar92;
      
   SELECT tot_registros
   INTO v_num_registros_detalle_sum
   FROM safre_tmp:tmp_sum_sar92;

   IF ( v_num_registros_detalle <> v_num_registros_detalle_sum ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El número de registros de detalle cargado no corresponde con el reportado en archivo";
      
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_id_unico;
   END IF
   
   -- ========================================================================
   -- se verifican las cifras de montos totales recibidos
   -- ========================================================================
   
   -- aportaciones patronales
   IF ( tmp_sum_pag_suma_aivs_a <> tmp_sum_pag_tot_aivs_ap ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_aportacion_patronal_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aportaciones patronales de detalle cargado no corresponde con el reportado en archivo.";
      
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_id_unico;
   END IF

   -- amortizacion del credito
   IF ( tmp_sum_pag_suma_aivs_val_ant <> tmp_sum_pag_tot_aivs_val_ant ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_amortizacion_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de amortización del crédito de detalle cargado no corresponde con el reportado en archivo: " || tmp_sum_pag_tot_aivs_val_ant || " - " || tmp_sum_pag_suma_aivs_val_ant ;
      
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_id_unico;
   END IF

   -- aplicacion de intereses de vivienda
   IF ( tmp_sum_pag_suma_imp_viv <> tmp_sum_pag_tot_imp_viv ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aplicación de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";
      
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_id_unico;
   END IF

   --Recupera todos los registros de la tabla de encabezados para SAR92
   FOREACH SELECT tpo_registro      , 
                  id_servicio       ,
                  id_operacion      ,
                  tpo_ent_origen    ,
                  cve_ent_origen    ,
                  tpo_ent_destino   ,
                  cve_ent_destino   ,
                  f_envio_lote      ,
                  consecutivo_dia   ,
                  modalidad_recep   ,
                  result_operacion  ,
                  motivo_rch_lote   ,
                  valor_aiv         
   	
             INTO v_tmp_cza_tpo_registro     ,
                  v_tmp_cza_id_servicio      ,
                  v_tmp_cza_id_operacion     ,
                  v_tmp_cza_tpo_ent_origen   ,
                  v_tmp_cza_cve_ent_origen   ,
                  v_tmp_cza_tpo_ent_destino  ,
                  v_tmp_cza_cve_ent_destino  ,
                  v_tmp_cza_f_envio_lote     ,
                  v_tmp_cza_consecutivo_dia  ,
                  v_tmp_cza_modalidad_recep  ,
                  v_tmp_cza_result_operacion ,
                  v_tmp_cza_motivo_rch_lote  ,
                  v_tmp_cza_valor_aiv 
             FROM safre_tmp:tmp_cza_sar92
      
      --Iguala el tipo de dato decimal
      LET v_pag_cza_valor_aiv = v_tmp_cza_valor_aiv / 100000000000000;
      
      --Inserta el regigistro       
      INSERT INTO pag_cza_sar92 (folio           ,
                                 tpo_registro    ,
                                 id_servicio     ,
                                 id_operacion    ,
                                 tpo_ent_origen  ,
                                 cve_ent_origen  ,
                                 tpo_ent_destino ,
                                 cve_ent_destino ,
                                 f_envio_lote    ,
                                 consecutivo_dia ,
                                 modalidad_recep ,
                                 result_operacion,
                                 motivo_rch_lote ,
                                 valor_aiv
                                )      
                         VALUES(p_folio                    ,
                                v_tmp_cza_tpo_registro     ,
                                v_tmp_cza_id_servicio      ,
                                v_tmp_cza_id_operacion     ,
                                v_tmp_cza_tpo_ent_origen   ,
                                v_tmp_cza_cve_ent_origen   ,
                                v_tmp_cza_tpo_ent_destino  ,
                                v_tmp_cza_cve_ent_destino  ,
                                v_tmp_cza_f_envio_lote     ,
                                v_tmp_cza_consecutivo_dia  ,
                                v_tmp_cza_modalidad_recep  ,
                                v_tmp_cza_result_operacion ,
                                v_tmp_cza_motivo_rch_lote  ,
                                v_pag_cza_valor_aiv);
   
   END FOREACH
   
   --Recupera todos los registros de la tabla de detalle para SAR92
   FOREACH SELECT tpo_registro          ,
                  cve_icefa_generadora  ,
                  rfc_patron            ,
                  num_expediente        ,
                  nom_patron            ,
                  rfc                   ,
                  nss                   ,
                  nci_banco             ,
                  nom_trabajador        ,
                  aivs_ap               ,
                  aivs_val_ant          ,
                  id_unico              ,
                  cve_referencia        ,
                  periodo_pago          ,
                  diag                  ,
                  motivo_rch_det        ,
                  ind_cta_local_bd      ,
                  imp_viv          
             INTO v_tmp_det_tpo_registro           ,
                  v_tmp_det_cve_icefa_generadora   ,
                  v_tmp_det_rfc_patron             ,
                  v_tmp_det_num_expediente         ,
                  v_tmp_det_nom_patron             ,
                  v_tmp_det_rfc                    ,
                  v_tmp_det_nss                    ,
                  v_tmp_det_nci_banco              ,
                  v_tmp_det_nom_trabajador         ,
                  v_tmp_det_aivs_ap               ,
                  v_tmp_det_aivs_val_ant           ,
                  v_tmp_det_id_unico               ,
                  v_tmp_det_cve_referencia         ,
                  v_tmp_det_periodo_pago           ,
                  v_tmp_det_diag                   ,
                  v_tmp_det_motivo_rch_det         ,
                  v_tmp_det_ind_cta_local_bd       ,
                  v_tmp_det_imp_viv
             FROM safre_tmp:tmp_det_sar92          
            
          LET v_contador = v_contador + 1  ;
      
      --Inicializa id para realizar la consulta en afi_decreto
      LET v_id_decreto = NULL;
      
      --Revisa si existe el número de control interno en la tabla afi_decreto
      SELECT FIRST 1 id_decreto
      INTO   v_id_decreto
      FROM   afi_decreto
      WHERE  consec_cuenta = v_tmp_det_id_unico;
      --WHERE  nci = v_tmp_det_nci_banco;
      
      --Si no existe se genera el registro del decreto en la tabla afi_decreto
      --TRACE " v_id_decreto "||v_id_decreto;
      --TRACE " id_unico     "||v_tmp_det_id_unico;
      
      --se comenta validacion de NCI por reglas de negocio  24 de Mayao de 2012
      --modifica Rubén Haro Castro
      -- Se activa de nuevo el IF por intruccion de Gerardo Vega 17 de Julio 2012
      IF(v_id_decreto IS NULL )THEN
      
      --TRACE ("fn_apertura_decreto_sar92..............................") ;
      
       LET v_id_decreto = fn_apertura_decreto_sar92(v_tmp_det_nci_banco,
                                                    v_tmp_det_id_unico,
                                                    v_tmp_det_nss,
                                                    v_tmp_det_rfc, 
                                                    v_tmp_det_nom_trabajador,
                                                    v_tmp_det_rfc_patron,
                                                    v_tmp_det_nom_patron);
      END IF
      
      --Se iguala el tipo de dato decimal
      LET v_pag_det_aivs_ap      = v_tmp_det_aivs_ap  / 1000000;
      LET v_pag_det_aivs_val_ant = v_tmp_det_aivs_val_ant / 1000000;
      LET v_pag_det_imp_viv      = v_tmp_det_imp_viv   / 100;
      --se insera el registro 
      
      --TRACE ("SE INSERTAN LOS DATOS A pag_det_sar92"||v_id_decreto) ;
      
      INSERT INTO pag_det_sar92 (
                                 folio                 ,
                                 periodo_pago          ,
                                 id_decreto            ,
                                 id_referencia         ,
                                 tpo_registro          ,
                                 cve_icefa_generadora  ,
                                 rfc_patron            ,
                                 num_expediente_ifv    ,
                                 nom_patron            ,
                                 nss                   ,
                                 rfc                   ,
                                 nom_trabajador        ,
                                 aivs_ap               ,
                                 aivs_val_ant          ,
                                 id_unico              ,
                                 cve_referencia        ,
                                 diag                  ,
                                 motivo_rch_det        ,
                                 ind_cta_local_bd      ,
                                 imp_viv                
                                )
      
                         VALUES(p_folio                         ,
                                v_tmp_det_periodo_pago          ,
                                v_id_decreto                    ,
                                seq_pag_det_sar92.NEXTVAL       ,
                                v_tmp_det_tpo_registro          ,
                                v_tmp_det_cve_icefa_generadora  ,
                                v_tmp_det_rfc_patron            ,
                                v_tmp_det_num_expediente        ,
                                v_tmp_det_nom_patron            ,
                                v_tmp_det_nss                   ,
                                v_tmp_det_rfc                   ,
                                v_tmp_det_nom_trabajador        ,
                                v_pag_det_aivs_ap               ,
                                v_pag_det_aivs_val_ant          ,
                                v_tmp_det_id_unico              ,
                                v_tmp_det_cve_referencia        ,
                                v_tmp_det_diag                  ,
                                v_tmp_det_motivo_rch_det        ,
                                v_tmp_det_ind_cta_local_bd      ,
                                v_pag_det_imp_viv
                               );
           --TRACE ("finaliza insert  pag_det_sar92") ;
   
   END FOREACH
   
   --TRACE ("Select  tmp_sum_sar92") ;
   
   --Recupera todos los registros de la tabla de sumario para SAR92
   FOREACH SELECT tpo_registro     ,
                  tpo_ent_origen   ,
                  cve_ent_origen   ,
                  tot_registros    ,
                  tot_aivs_ap      ,
                  tot_aivs_val_ant ,
                  tot_imp_viv      
             INTO v_tmp_sum_tpo_registro     ,
                  v_tmp_sum_tpo_ent_origen   ,
                  v_tmp_sum_cve_ent_origen   , 
                  v_tmp_sum_tot_registros    ,
                  v_tmp_sum_tot_aivs_ap      ,
                  v_tmp_sum_tot_aivs_val_ant ,
                  v_tmp_sum_tot_imp_viv
             FROM safre_tmp:tmp_sum_sar92
      --Iguala el tipo de dato decimal
      LET v_pag_sum_tot_aivs_ap      = v_tmp_sum_tot_aivs_ap / 1000000;
      LET v_pag_sum_tot_aivs_val_ant = v_tmp_sum_tot_aivs_val_ant / 1000000;
      LET v_pag_sum_tot_imp_viv      = v_tmp_sum_tot_imp_viv / 100;
      --Inserta el registro
      
       --TRACE ("SE INSERTANLOS DATOS A pag_sum_sar92") ;
      INSERT INTO pag_sum_sar92(
                                folio            , 
                                tpo_registro     , 
                                tpo_ent_origen   , 
                                cve_ent_origen   , 
                                tot_registros    , 
                                tot_aivs_ap      , 
                                tot_aivs_val_ant , 
                                tot_imp_viv      
                               )      
                        VALUES(p_folio                    ,
                               v_tmp_sum_tpo_registro     ,
                               v_tmp_sum_tpo_ent_origen   ,
                               v_tmp_sum_cve_ent_origen   ,
                               v_tmp_sum_tot_registros    ,
                               v_pag_sum_tot_aivs_ap      ,
                               v_pag_sum_tot_aivs_val_ant ,
                               v_pag_sum_tot_imp_viv
                              );
                              
         --TRACE ("SE INSERTANLOS22222 DATOS A pag_sum_sar92") ;
   
   END FOREACH
   
   UPDATE STATISTICS FOR TABLE pag_cza_sar92;
   UPDATE STATISTICS FOR TABLE pag_det_sar92;
   UPDATE STATISTICS FOR TABLE pag_sum_sar92;
   ---TRACE ("UPDATE STATISTICS pag_cza_sar92") ;
   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_id_unico;
END PROCEDURE;


