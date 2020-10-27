






CREATE PROCEDURE "safreviv".sp_historico_tia_traspaso(p_folio DECIMAL(9,0),
                                           p_pid DECIMAL(9,0),
                                           p_proceso_cod SMALLINT)
RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(11,0), DECIMAL(11,0), CHAR(40), CHAR(40), CHAR(40)
            
   --tabla encabezado TIA    tmp_cza_traspaso      
   DEFINE tmp_cza_tia_tpo_registro     CHAR(2);
   DEFINE tmp_cza_tia_id_servicio      CHAR(2);
   DEFINE tmp_cza_tia_id_operacion     CHAR(2);
   DEFINE tmp_cza_tia_tpo_ent_origen   CHAR(2);
   DEFINE tmp_cza_tia_cve_ent_origen   CHAR(3);
   DEFINE tmp_cza_tia_tpo_ent_destino  CHAR(2);
   DEFINE tmp_cza_tia_cve_ent_destino  CHAR(3);
   DEFINE tmp_cza_tia_filler           CHAR(3);
   DEFINE tmp_cza_tia_f_presentacion   DATE;
   DEFINE tmp_cza_tia_consecutivo_dia  CHAR(3);
   DEFINE tmp_cza_tia_cve_mod_presenta CHAR(2);
   DEFINE tmp_cza_tia_filler2          CHAR(698);
--=======================================================================================================
   --tabla encabezado 	tia_cza_traspaso        
   DEFINE tia_cza_folio            DECIMAL(9);
   DEFINE tia_cza_tpo_registro     CHAR(2);
   DEFINE tia_cza_id_servicio      CHAR(2);
   DEFINE tia_cza_id_operacion     CHAR(2);
   DEFINE tia_cza_tpo_ent_origen   CHAR(2);
   DEFINE tia_cza_cve_ent_origen   CHAR(3);
   DEFINE tia_cza_tpo_ent_destino  CHAR(2);
   DEFINE tia_cza_cve_ent_destino  CHAR(3);
   DEFINE tia_cza_f_presentacion   DATE;
   DEFINE tia_cza_consecutivo_dia  CHAR(3);
   DEFINE tia_cza_cve_mod_presenta CHAR(2);
--========================================================================================================
   
   --tabla detalle   TIA   
   DEFINE tmp_det_tia_tpo_registro      CHAR(2);
   DEFINE tmp_det_tia_contador_serv     CHAR(10);
   DEFINE tmp_det_tia_tpo_ent_receptora CHAR(2);
   DEFINE tmp_det_tia_cve_ent_receptora CHAR(3);
   DEFINE tmp_det_tia_tpo_ent_cedente   CHAR(2);
   DEFINE tmp_det_tia_cve_ent_cedente   CHAR(3);
   DEFINE tmp_det_tia_origen_traspaso   CHAR(2);
   DEFINE tmp_det_tia_f_presentacion    DATE;
   DEFINE tmp_det_tia_f_movimiento      DATE;
   DEFINE tmp_det_tia_curp              CHAR(18);
   DEFINE tmp_det_tia_nss_afo_recep     CHAR(11);
   DEFINE tmp_det_tia_filler            CHAR(15);
   DEFINE tmp_det_tia_rfc_afo_recep     CHAR(13);
   DEFINE tmp_det_tia_paterno_afo_recep CHAR(40);
   DEFINE tmp_det_tia_materno_afo_recep CHAR(40);
   DEFINE tmp_det_tia_nombres_afo_recep CHAR(40);
   DEFINE tmp_det_tia_filler2           CHAR(3);
   DEFINE tmp_det_tia_cve_sector        CHAR(1);
   DEFINE tmp_det_tia_filler3           CHAR(10);
   DEFINE tmp_det_tia_f_recep_solicitud DATE;
   DEFINE tmp_det_tia_id_lote_solicitud CHAR(16);
   DEFINE tmp_det_tia_filler4           CHAR(15);
   DEFINE tmp_det_tia_nss_icefa         CHAR(11);
   DEFINE tmp_det_tia_rfc_icefa         CHAR(13);
   DEFINE tmp_det_tia_nci_icefa         CHAR(30);
   DEFINE tmp_det_tia_paterno_icefa     CHAR(40);
   DEFINE tmp_det_tia_materno_icefa     CHAR(40);
   DEFINE tmp_det_tia_nombre_icefa      CHAR(40);
   DEFINE tmp_det_tia_consec_cuenta     DECIMAL(11,0); -- NUEVO CAMPO 11 Sep 2012
   DEFINE tmp_det_tia_filler5           CHAR(120);
   DEFINE tmp_det_tia_sdo_viv92         DECIMAL(15,2);
   DEFINE tmp_det_tia_aivs              DECIMAL(16,6); -- nvo campo 19 sep 2012
   DEFINE tmp_det_tia_filler6           CHAR(95);
   DEFINE tmp_det_tia_int_viv92         DECIMAL(15,2);
   DEFINE tmp_det_tia_filler7           CHAR(41);     

   
--===================================================================================================
   --TBALA PRODUCTIVA tia_det_traspaso
   DEFINE tia_det_folio             DECIMAL(9,0);
   DEFINE tia_det_id_referencia     DECIMAL(9,0);
   DEFINE tia_det_tpo_ent_receptora CHAR(2);
   DEFINE tia_det_cve_ent_receptora CHAR(3);
   DEFINE tia_det_tpo_ent_cedente   CHAR(2);
   DEFINE tia_det_cve_ent_cedente   CHAR(3);
   DEFINE tia_det_origen_traspaso   CHAR(2);
   DEFINE tia_det_f_presentacion    DATE;
   DEFINE tia_det_f_movimiento      DATE;
   DEFINE tia_det_id_decreto        DECIMAL(9,0);
   DEFINE tia_det_curp              CHAR(18);
   DEFINE tia_det_nss_afo_recep     CHAR(11);
   DEFINE tia_det_rfc_afo_recep     CHAR(13);
   DEFINE tia_det_paterno_afo_recep CHAR(40);
   DEFINE tia_det_materno_afo_recep CHAR(40);
   DEFINE tia_det_nombres_afo_recep CHAR(40);
   DEFINE tia_det_cve_sector        CHAR(1) ;
   DEFINE tia_det_f_recep_solicitud DATE;
   DEFINE tia_det_id_lote_solicitud CHAR(16);
   DEFINE tia_det_nss_icefa         CHAR(11);
   DEFINE tia_det_rfc_icefa         CHAR(13);
   DEFINE tia_det_nci_icefa         CHAR(30);
   DEFINE tia_det_paterno_icefa     CHAR(40);
   DEFINE tia_det_materno_icefa     CHAR(40);
   DEFINE tia_det_nombres_icefa     CHAR(40);
   DEFINE tia_det_sdo_viv92         DECIMAL(14,2);
   DEFINE tia_det_int_viv92         DECIMAL(14,2);
   DEFINE tia_det_aivs_viv92        DECIMAL(16,6);
   DEFINE tia_det_result_operacion  CHAR(2);
--=========================================================================================

   --tabla sumario TIA  tmp_tia_sum_traspaso
   DEFINE  tmp_sum_tia_tpo_registro      CHAR(2);
   DEFINE  tmp_sum_tia_registros_detalle CHAR(9);
   DEFINE  tmp_sum_tia_filler            CHAR(120);
   DEFINE  tmp_sum_tia_tot_sdo_viv92     DECIMAL(15,0);
   DEFINE  tmp_sum_tia_filler1           CHAR(15);
   DEFINE  tmp_sum_tia_tot_aivs          CHAR(15); -- 04abr2014. Se agrega campo del sumario
   DEFINE  tmp_sum_tia_tot_int_viv92     DECIMAL(15,0);
   DEFINE  tmp_sum_tia_filler2           CHAR(554);   

    --tabla sumario TIA  tia_sum_traspaso
   DEFINE tia_sum_folio             DECIMAL(9,0) ;
   DEFINE tia_sum_tpo_registro      CHAR(2);
   DEFINE tia_sum_registros_detalle DECIMAL(9,0) ;
   DEFINE tia_sum_tot_sdo_viv92     DECIMAL(16,2);
   DEFINE tia_sum_tot_avis          DECIMAL(24,6);
   DEFINE tia_sum_tot_int_viv92     DECIMAL(16,2);

--========================================================================================
   --TABLA tia_sum_traspaso
   DEFINE v_tia_sum_folio             decimal(9,0);  -- folio            
   DEFINE v_tia_sum_tpo_registro      char(2);       -- tpo_registro     
   DEFINE v_tia_sum_registros_detalle decimal(9,0);  -- registros_detalle
   DEFINE v_tia_sum_tot_sdo_viv92     decimal(16,6); -- tot_sdo_viv92    
   DEFINE tia_sum_tot_aivs            decimal(24,6); -- total de AIVs
   DEFINE v_tia_sum_tot_int_viv92     decimal(16,2); -- tot_int_viv92    
                                                       
   DEFINE v_id_decreto          DECIMAL(9,0);
   DEFINE v_nombre_aux          VARCHAR(50);
   DEFINE v_seq_ia_det_traspaso DECIMAL(9,0);
                                                       
   -- variables para tratamiento de errores            
   DEFINE v_sql_error    INTEGER;
   DEFINE v_isam_error   INTEGER;
   DEFINE v_mensaje      VARCHAR(250);
   DEFINE v_si_resultado SMALLINT;      

   -- para validar sumario   
   DEFINE v_tmp_num_registros DECIMAL(9,0);
   DEFINE v_tmp_sdo_viv92     DECIMAL(15,0);
   DEFINE v_tmp_int_viv92     DECIMAL(15,0);
   DEFINE v_tmp_aivs92        DECIMAL(15,0);
   DEFINE v_tmp_tot_aivs      DECIMAL(15,0);
   
   DEFINE v_tmp_num_registros_sum DECIMAL(9,0);
   DEFINE v_tmp_sdo_viv92_sum     DECIMAL(15,0);
   DEFINE v_tmp_int_viv92_sum     DECIMAL(15,0);
      
   DEFINE v_error_en_sumario SMALLINT; -- booleana para saber si hubo error en sumario
   
   ON EXCEPTION SET v_sql_error, v_isam_error, v_mensaje
      
      RETURN v_sql_error, v_isam_error, v_mensaje, tmp_det_tia_consec_cuenta, tia_det_id_decreto,
             tmp_det_tia_nombres_afo_recep, tmp_det_tia_paterno_afo_recep, tmp_det_tia_materno_afo_recep;
   END EXCEPTION 
   
   -- se asume que no hay errores
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_mensaje    = "Integración de TIA realizada correctamente.";
   LET tmp_det_tia_consec_cuenta = NULL;
   LET tia_det_id_decreto = NULL;
   LET tmp_det_tia_nombres_afo_recep = NULL;
   LET tmp_det_tia_paterno_afo_recep = NULL;
   LET tmp_det_tia_materno_afo_recep = NULL;
   
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/log_tia.trace';       
   --TRACE 'Inicia el store procedure de registro historicos';    
   
--@@ Se valida que las tablas temporales de detalle y sumario mantengan los mismos datos

   -- se asume que no hay error en sumario
   LET v_error_en_sumario = 0;   
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = P_folio,
          estado = 2     -- integrado
   WHERE proceso_cod = p_proceso_cod
   AND   opera_cod   = 1 -- archivo cargado
   AND   estado      = 1; -- etapa de carga

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET folio = p_folio
   WHERE proceso_cod = p_proceso_cod 
   AND opera_cod     = 2
   AND pid           = p_pid;   

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_proceso
   SET folio = p_folio
   WHERE proceso_cod = p_proceso_cod
   AND pid           = p_pid;   
   
   -- se revisa el sumario contra el detalle
   -- DETALLE
   SELECT COUNT(*)
   INTO v_tmp_num_registros
   FROM safre_tmp:tmp_det_traspaso; 
   IF v_tmp_num_registros IS NULL THEN
      LET v_tmp_num_registros = 0;
   END IF  
     
   SELECT SUM(sdo_viv92)
   INTO v_tmp_sdo_viv92
   FROM safre_tmp:tmp_det_traspaso;  
   IF v_tmp_sdo_viv92 IS NULL THEN
   	  LET v_tmp_sdo_viv92 = 0;
   END IF      

   SELECT SUM(int_viv92)
   INTO v_tmp_int_viv92
   FROM safre_tmp:tmp_det_traspaso;
   IF v_tmp_int_viv92 IS NULL THEN
   	  LET v_tmp_int_viv92 = 0;
   END IF
     
   select sum(aivs)
   INTO v_tmp_aivs92
   FROM safre_tmp:tmp_det_traspaso;     
   IF v_tmp_aivs92 IS NULL THEN
   	  LET v_tmp_aivs92 = 0;
   END IF     
     
    -- SUMARIO    
   SELECT registros_detalle
   INTO v_tmp_num_registros_sum
   FROM safre_tmp:tmp_sum_traspaso;
   IF v_tmp_num_registros_sum IS NULL THEN
   	  LET v_tmp_num_registros_sum = 0;
   END IF   
     
   SELECT tot_sdo_viv92  
   INTO v_tmp_sdo_viv92_sum
   FROM safre_tmp:tmp_sum_traspaso;        
   IF v_tmp_sdo_viv92_sum IS NULL THEN
   	  LET v_tmp_sdo_viv92_sum = 0;
   END IF
 
   SELECT tot_aivs
   INTO v_tmp_tot_aivs
   FROM safre_tmp:tmp_sum_traspaso;
   IF v_tmp_tot_aivs IS NULL THEN
   	  LET v_tmp_tot_aivs = 0;
   END IF
   
 
   
{
   SELECT tot_int_viv92
     INTO v_tmp_int_viv92_sum
     FROM safre_tmp:tmp_sum_traspaso ;
 }   

    LET v_tmp_int_viv92_sum = 0;     -- por correo de adrian del 7 Ago 2012 (tot_int_viv92/100) 
    LET v_tmp_int_viv92 = 0;
    
     -- compara el numero de registros     
     IF ( v_tmp_num_registros <> v_tmp_num_registros_sum ) THEN
        LET v_si_resultado = 10;
        LET v_isam_error       = 0;
        LET v_mensaje        = "@El numero de registros en detalle y en sumario no coinciden";
        
        LET v_error_en_sumario = 1;
     END IF     
   
     -- compara saldo vivienda   --- PESOS
     IF ( (v_tmp_sdo_viv92/100) <> (v_tmp_sdo_viv92_sum/100) ) THEN
        LET v_si_resultado = 20;
        LET v_isam_error       = 0;
        LET v_mensaje        = "@La suma del total de saldo vivienda en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF 
   
     -- compara interés vivienda
     IF ( v_tmp_int_viv92 <> (v_tmp_int_viv92_sum/100) ) THEN
        LET v_si_resultado = 20;
        LET v_isam_error       = 0;
        LET v_mensaje        = "@La suma del total de interés vivienda en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF
     
     -- compara aivs
     IF v_tmp_aivs92 <> v_tmp_tot_aivs THEN
        LET v_si_resultado = 20;
        LET v_isam_error       = 0;
        LET v_mensaje        = "@La suma del total de aivs en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF     
     
     
     -- si hubo error en sumario, entonces se cancela el folio y se devuelve el error
     IF ( v_error_en_sumario = 1 ) THEN
       
        -- se devuelve el error
        RETURN v_si_resultado, v_isam_error, v_mensaje, tmp_det_tia_consec_cuenta, tia_det_id_decreto,
               tmp_det_tia_nombres_afo_recep, tmp_det_tia_paterno_afo_recep, tmp_det_tia_materno_afo_recep;
     END IF
--@@     


   
  --====================ENCABEZADO tia ==================================================
   --se consulta el encabeza de la tabal tia_cza_traspaso 
   
   SELECT * 
   INTO tmp_cza_tia_tpo_registro         ,
        tmp_cza_tia_id_servicio          ,
        tmp_cza_tia_id_operacion         ,
        tmp_cza_tia_tpo_ent_origen       ,
        tmp_cza_tia_cve_ent_origen       ,
        tmp_cza_tia_tpo_ent_destino      ,
        tmp_cza_tia_cve_ent_destino      ,
        tmp_cza_tia_filler               ,
        tmp_cza_tia_f_presentacion       ,
        tmp_cza_tia_consecutivo_dia      ,
        tmp_cza_tia_cve_mod_presenta     ,
        tmp_cza_tia_filler2              
   FROM safre_tmp:tmp_cza_traspaso ;
       
    --SE ASIGNAN LOS DATOS CORRESPONDIENTES AL ENCABEZADO DE tia_cza_traspaso
    LET tia_cza_folio                  = p_folio                           ;
    LET tia_cza_tpo_registro           = tmp_cza_tia_tpo_registro          ;
    LET tia_cza_id_servicio            = tmp_cza_tia_id_servicio           ;
    LET tia_cza_id_operacion           = tmp_cza_tia_id_operacion          ;
    LET tia_cza_tpo_ent_origen         = tmp_cza_tia_tpo_ent_origen        ;
    LET tia_cza_cve_ent_origen         = tmp_cza_tia_cve_ent_origen        ;
    LET tia_cza_tpo_ent_destino        = tmp_cza_tia_tpo_ent_destino       ;
    LET tia_cza_cve_ent_destino        = tmp_cza_tia_cve_ent_destino       ;
    LET tia_cza_f_presentacion         = tmp_cza_tia_f_presentacion        ;
    LET tia_cza_consecutivo_dia        = tmp_cza_tia_consecutivo_dia       ;
    LET tia_cza_cve_mod_presenta       = tmp_cza_tia_cve_mod_presenta      ;
    
        
    INSERT INTO tia_cza_traspaso (
       folio                      ,
       tpo_registro               ,
       id_servicio                ,
       id_operacion               ,
       tpo_ent_origen             ,
       cve_ent_origen             ,
       tpo_ent_destino            ,
       cve_ent_destino            ,
       f_presentacion             ,
       consecutivo_dia            ,
       cve_mod_presenta
       )
    VALUES (
       tia_cza_folio               ,
       tia_cza_tpo_registro        ,
       tia_cza_id_servicio         ,
       tia_cza_id_operacion        ,
       tia_cza_tpo_ent_origen      ,
       tia_cza_cve_ent_origen      ,
       tia_cza_tpo_ent_destino     ,
       tia_cza_cve_ent_destino     ,
       tia_cza_f_presentacion      ,
       tia_cza_consecutivo_dia     ,
       tia_cza_cve_mod_presenta 
       );
--====================DETALLE tia ==================================================

   --se seleccionan los registrso del de talle de la tabla temporal
   FOREACH SELECT 
           tpo_registro            ,
           contador_serv           ,
           tpo_ent_receptora       ,
           cve_ent_receptora       ,
           tpo_ent_cedente         ,
           cve_ent_cedente         ,
           origen_traspaso         ,
           f_presentacion          ,
           f_movimiento            ,
           curp                    ,
           nss_afo_recep           ,
           rfc_afo_recep           ,
           paterno_afo_recep       ,
           materno_afo_recep       ,
           nombres_afo_recep       ,
           cve_sector              ,
           f_recep_solicitud       ,
           id_lote_solicitud       ,
           nss_icefa               ,
           rfc_icesa               ,
           TRIM (nci_icefa)        ,
           paterno_icefa           ,
           materno_icefa           ,
           nombre_icefa            ,
           consec_cuenta           ,
           (sdo_viv92/100)         ,
           (aivs / 1000000)  ,
           (int_viv92/100)         
      INTO tmp_det_tia_tpo_registro         , 
           tmp_det_tia_contador_serv        , 
           tmp_det_tia_tpo_ent_receptora    , 
           tmp_det_tia_cve_ent_receptora    , 
           tmp_det_tia_tpo_ent_cedente      , 
           tmp_det_tia_cve_ent_cedente      , 
           tmp_det_tia_origen_traspaso      , 
           tmp_det_tia_f_presentacion       , 
           tmp_det_tia_f_movimiento         , 
           tmp_det_tia_curp                 , 
           tmp_det_tia_nss_afo_recep        , 
           tmp_det_tia_rfc_afo_recep        , 
           tmp_det_tia_paterno_afo_recep    , 
           tmp_det_tia_materno_afo_recep    , 
           tmp_det_tia_nombres_afo_recep    , 
           tmp_det_tia_cve_sector           , 
           tmp_det_tia_f_recep_solicitud    , 
           tmp_det_tia_id_lote_solicitud    , 
           tmp_det_tia_nss_icefa            , 
           tmp_det_tia_rfc_icefa            , 
           tmp_det_tia_nci_icefa            , 
           tmp_det_tia_paterno_icefa        , 
           tmp_det_tia_materno_icefa        , 
           tmp_det_tia_nombre_icefa         ,
           tmp_det_tia_consec_cuenta        ,
           tmp_det_tia_sdo_viv92            , 
           tmp_det_tia_aivs                 ,
           tmp_det_tia_int_viv92            
        FROM safre_tmp:tmp_det_traspaso

     --TRACE 'Inicia Asigna variables ';

      LET v_id_decreto = NULL;
      -- se obtiene el id_decreto
      -- 11 sep 2012. Se busca id_decreto por consecutivo_cuenta en lugar de por NCI

      SELECT FIRST 1 id_decreto
      INTO   v_id_decreto
      FROM   afi_decreto
      WHERE  consec_cuenta = tmp_det_tia_consec_cuenta;

      --TRACE 'Verifica id_decreto ';  

      --si no se encuentra el id_decreto se asigna 0 y el estado = 02 de que no existe consec_cuenta
      IF ( v_id_decreto IS NULL ) THEN
      	 LET v_id_decreto = 0 ;
         LET tia_det_result_operacion	 = "02";
      ELSE 
         LET tia_det_result_operacion	 = "01";      -- result_operacion = "01" es que es aceptado el registro    
         
--      SE COMENTA LO SIGUIENTE YA QUE ADRIAN CARGO UN ARCHIVO CON
--      DIFERENTE NOMBRE 3 VECES Y ESTA VALIDACIÓN LA HACE LA 
--      FUNCIONALIDAD DE CARGA DEL ARCHIVO
--         SELECT "G"
--         INTO   v_existe
--         FROM   cta_decreto
--         WHERE  id_decreto    = v_id_decreto
--         AND    id_referencia = tia_det_id_referencia
--         IF v_existe = "G" THEN
--            LET tia_det_result_operacion	 = "06";    -- result_operacion = "06" es que ya existe cargo en cta_decreto         
--         ELSE
--           LET tia_det_result_operacion	 = "01";      -- result_operacion = "01" es que es aceptado el registro
--        END IF

      END IF
      

      --se asigan los registrso correspondientes a tia_det_traspaso
      LET tia_det_folio               = p_folio                        ;
      LET tia_det_id_referencia       = tmp_det_tia_contador_serv      ; 
      LET tia_det_tpo_ent_receptora   = tmp_det_tia_tpo_ent_receptora  ; 
      LET tia_det_cve_ent_receptora   = tmp_det_tia_cve_ent_receptora  ; 
      LET tia_det_tpo_ent_cedente     = tmp_det_tia_tpo_ent_cedente    ; 
      LET tia_det_cve_ent_cedente     = tmp_det_tia_cve_ent_cedente    ; 
      LET tia_det_origen_traspaso     = tmp_det_tia_origen_traspaso    ; 
      LET tia_det_f_presentacion      = tmp_det_tia_f_presentacion     ; 
      LET tia_det_f_movimiento        = tmp_det_tia_f_movimiento       ; 
      LET tia_det_id_decreto          = v_id_decreto                   ;
      LET tia_det_curp                = tmp_det_tia_curp               ; 
      LET tia_det_nss_afo_recep       = tmp_det_tia_nss_afo_recep      ; 
      LET tia_det_rfc_afo_recep       = tmp_det_tia_rfc_afo_recep      ; 
      LET tia_det_paterno_afo_recep   = tmp_det_tia_paterno_afo_recep  ; 
      LET tia_det_materno_afo_recep   = tmp_det_tia_materno_afo_recep  ; 
      LET tia_det_nombres_afo_recep   = tmp_det_tia_nombres_afo_recep  ; 
      LET tia_det_cve_sector          = tmp_det_tia_cve_sector         ;
      LET tia_det_f_recep_solicitud   = tmp_det_tia_f_recep_solicitud  ;
      LET tia_det_id_lote_solicitud   = tmp_det_tia_id_lote_solicitud  ;
      LET tia_det_nss_icefa           = tmp_det_tia_nss_icefa          ;
      LET tia_det_rfc_icefa           = tmp_det_tia_rfc_icefa          ;
      LET tia_det_nci_icefa           = tmp_det_tia_nci_icefa          ;
      LET tia_det_paterno_icefa       = tmp_det_tia_paterno_icefa      ;
      LET tia_det_materno_icefa       = tmp_det_tia_materno_icefa      ;
      LET tia_det_nombres_icefa       = tmp_det_tia_nombre_icefa       ;
      LET tia_det_sdo_viv92           = tmp_det_tia_sdo_viv92          ;
      LET tia_det_aivs_viv92          = tmp_det_tia_aivs               ;
      LET tia_det_int_viv92           = tmp_det_tia_int_viv92          ;

      --TRACE 'Termina Asigna variables ';      
      --TRACE 'termina if de decreto null'; 
      --TRACE 'insert tia_det_traspaso';  
      
      ---se insertan los registros correspondientes 
      INSERT INTO tia_det_traspaso  (
                       folio                 ,  
                       id_referencia         ,  
                       tpo_ent_receptora     ,  
                       cve_ent_receptora     ,  
                       tpo_ent_cedente       ,  
                       cve_ent_cedente       ,  
                       origen_traspaso       ,  
                       f_presentacion        ,  
                       f_movimiento          ,  
                       id_decreto            ,  
                       curp                  ,  
                       nss_afo_recep         ,  
                       rfc_afo_recep         ,  
                       paterno_afo_recep     ,  
                       materno_afo_recep     ,  
                       nombres_afo_recep     ,  
                       cve_sector            ,  
                       f_recep_solicitud     ,  
                       id_lote_solicitud     ,  
                       nss_icefa             ,  
                       rfc_icefa             ,  
                       nci_icefa             ,  
                       paterno_icefa         ,  
                       materno_icefa         ,  
                       nombres_icefa         ,  
                       sdo_viv92             ,
                       aivs_viv92            ,
                       int_viv92             ,  
                       result_operacion      
                      )
               VALUES (
                       tia_det_folio              ,
                       tia_det_id_referencia      ,
                       tia_det_tpo_ent_receptora  ,
                       tia_det_cve_ent_receptora  ,
                       tia_det_tpo_ent_cedente    ,
                       tia_det_cve_ent_cedente    ,
                       tia_det_origen_traspaso    ,
                       tia_det_f_presentacion     ,
                       tia_det_f_movimiento       ,
                       tia_det_id_decreto         ,
                       tia_det_curp               ,
                       tia_det_nss_afo_recep      ,
                       tia_det_rfc_afo_recep      ,
                       tia_det_paterno_afo_recep  ,
                       tia_det_materno_afo_recep  ,
                       tia_det_nombres_afo_recep  ,
                       tia_det_cve_sector         ,
                       tia_det_f_recep_solicitud  ,
                       tia_det_id_lote_solicitud  ,
                       tia_det_nss_icefa          ,
                       tia_det_rfc_icefa          ,
                       tia_det_nci_icefa          ,
                       tia_det_paterno_icefa      ,
                       tia_det_materno_icefa      ,
                       tia_det_nombres_icefa      ,
                       tia_det_sdo_viv92          ,
                       tia_det_aivs_viv92         ,
                       0,                            --por correo de adrian del 7 Ago 2012 ---tia_det_int_viv92          ,
                       tia_det_result_operacion
                      )    ;
   END FOREACH

--====================SUMARIO tia ==================================================
   --se seleccionan los datos de la tabla  tmp_sum_traspaso 
   SELECT tpo_registro                 , 
          registros_detalle            , 
          tot_sdo_viv92                , 
          tot_aivs                     , -- 04abr2014 Se agrega el total de AIVs
          0                             -- por correo de adrian del 7 Ago 2012 (tot_int_viv92/100)          ,
   INTO   tmp_sum_tia_tpo_registro       ,
          tmp_sum_tia_registros_detalle  ,
          tmp_sum_tia_tot_sdo_viv92      ,
          tmp_sum_tia_tot_aivs           ,
          tmp_sum_tia_tot_int_viv92      
   FROM   safre_tmp:tmp_sum_traspaso ; 
   

    --se asigana los valores a insertar 
    LET tia_sum_folio              = p_folio                            ; 
    LET tia_sum_tpo_registro       = tmp_sum_tia_tpo_registro           ;
    LET tia_sum_registros_detalle  = tmp_sum_tia_registros_detalle      ;
    LET tia_sum_tot_sdo_viv92      = tmp_sum_tia_tot_sdo_viv92 / 100    ;
    LET tia_sum_tot_aivs           = tmp_sum_tia_tot_aivs      / 1000000; -- 04abr2014 Se agrega total de AIVs a sumario
    LET tia_sum_tot_int_viv92      = tmp_sum_tia_tot_int_viv92          ;
    
    
    --LET  tia_sum_tot_sdo_viv92 = tia_sum_tot_sdo_viv92 /100 ;
    --LET  tia_sum_tot_int_viv92 = tia_sum_tot_int_viv92 /100 ;

    INSERT INTO tia_sum_traspaso (
        folio              ,
        tpo_registro       ,
        registros_detalle  ,
        tot_sdo_viv92      ,
        tot_aivs           ,
        tot_int_viv92       
       )

    VALUES (                              
       tia_sum_folio               ,
       tia_sum_tpo_registro        ,
       tia_sum_registros_detalle   ,
       tia_sum_tot_sdo_viv92       ,
       tia_sum_tot_aivs            ,
       tia_sum_tot_int_viv92          
       );

      --Actualizar la precision para los campos correspondientes
      --LET v_tia_sum_tot_sdo_viv92  = v_tmp_sum_tot_sdo_viv92         /100;
      --LET v_tia_sum_tot_int_viv92  = v_tmp_sum_tot_int_viv92    /100;
      
      
   --TRACE 'Finaliza el store procedure de registro historicos';

   -- Se agrega sentencia de update statics a las tablas temporales
   UPDATE STATISTICS FOR TABLE tia_cza_traspaso    ; 
   UPDATE STATISTICS FOR TABLE tia_det_traspaso    ; 
   UPDATE STATISTICS FOR TABLE tia_sum_traspaso    ;
   	
   -- se devuelve el resultado del proceso
   RETURN v_sql_error, v_isam_error, v_mensaje, tmp_det_tia_consec_cuenta, tia_det_id_decreto,
          tmp_det_tia_nombres_afo_recep, tmp_det_tia_paterno_afo_recep, tmp_det_tia_materno_afo_recep;

END PROCEDURE;


