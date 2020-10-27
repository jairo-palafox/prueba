






CREATE PROCEDURE "safreviv".sp_alta_mandato(p_id_solicitud_mandato DECIMAL(10,0),
                                 p_id_credito           DECIMAL(10,0),
                                 p_id_derechohabiente   DECIMAL(10,0),
                                 p_v_usuario            CHAR(20)     )
   RETURNING INTEGER;

   DEFINE v_num_credito      DECIMAL(10,0);
   DEFINE v_existe           INTEGER;
   DEFINE v_ins_det          SMALLINT;
   DEFINE v_cve_mandato                CHAR(18);
   DEFINE v_m_id_derechohabiente       DECIMAL(9,0) ;
   DEFINE v_m_nss                      CHAR(11)     ;
   DEFINE v_m_id_credito               DECIMAL(10,0);
   DEFINE v_m_f_lote                   DATE         ;
   DEFINE v_m_lote                     SMALLINT     ;
   DEFINE v_m_id_lote                  INTEGER      ;
   DEFINE v_m_tpo_credito              CHAR(3)      ;
   DEFINE v_m_edo_credito              CHAR(3)      ;
   DEFINE v_m_tpo_descuento_credito    SMALLINT     ;
   DEFINE v_m_valor_descuento_credito  DECIMAL(12,2); -- cambio precision
   DEFINE v_m_estado                   SMALLINT     ;
   DEFINE v_m_tpo_prelacion            SMALLINT     ;
   DEFINE v_m_usuario                  CHAR(20)     ;
   DEFINE v_m_id_lote_mandato          DECIMAL(9,0) ;

   -- arreglo de mdt_det_ctr_mandato
   DEFINE v_d_id_det_ctr_mandato       DECIMAL(9,0) ;
   DEFINE v_d_id_ctr_mandato           DECIMAL(9,0) ;
   DEFINE v_d_id_derechohabiente       DECIMAL(9,0) ;
   DEFINE v_d_nss                      CHAR(11)     ;
   DEFINE v_d_tpo_descuento_mandato    SMALLINT     ;
   DEFINE v_d_valor_descuento_mandato  DECIMAL(12,2); -- cambio precision
   DEFINE v_d_f_inicio_mandato         DATE         ;
   DEFINE v_d_f_culmina_mandato        DATE         ;
   DEFINE v_d_referencia               CHAR(40)     ;
   DEFINE v_d_scta_origen_descuento    SMALLINT     ;
   DEFINE v_d_movimiento               SMALLINT     ;
   DEFINE v_d_modalidad_aplicacion     DECIMAL(9,0) ;
   DEFINE v_d_f_presentacion           DATE         ;
   DEFINE v_d_estado                   SMALLINT     ;

   -- arreglo de mdt_solicitud_mandato
   DEFINE v_s_id_solicitud_mandato    DECIMAL(10)  ;
   DEFINE v_s_id_derechohabiente      DECIMAL(10)  ;
   DEFINE v_s_id_origen               SMALLINT     ;
   DEFINE v_s_lote                    SMALLINT     ;
   DEFINE v_s_nss                     CHAR(11)     ;
   DEFINE v_s_id_credito              DECIMAL(10)  ;
   DEFINE v_s_id_mandato              CHAR(7)      ;
   DEFINE v_s_tpo_descuento_mandato   SMALLINT     ;
   DEFINE v_s_valor_descuento_mandato DECIMAL(12,2); -- cambio precision
   DEFINE v_s_f_inicio_mandato        DATE         ;
   DEFINE v_s_f_culmina_mandato       DATE         ;
   DEFINE v_s_referencia              CHAR(40)     ;
   DEFINE v_s_cve_mandato             CHAR(7)      ;
   DEFINE v_s_scta_origen_descuento   SMALLINT     ;
   DEFINE v_s_modalidad_aplicacion    DECIMAL(10)  ;
   DEFINE v_s_usuario                 CHAR(20)     ;
   DEFINE v_s_estado                  SMALLINT     ;
   DEFINE v_s_f_canales               DATE         ;
   DEFINE v_s_id_canales              DECIMAL(9,0) ;
   DEFINE v_s_tipo_operacion          CHAR(1)      ;
   DEFINE v_s_diagnostico             CHAR(3)      ;

   -- arreglo de acr_transferencia
   DEFINE v_c_id_acr_transferencia DECIMAL(9,0) ;
   DEFINE v_c_id_derechohabiente   DECIMAL(9,0) ;
   DEFINE v_c_tpo_registro         CHAR(2)      ;
   DEFINE v_c_lote                 SMALLINT     ;
   DEFINE v_c_id_lote              INTEGER      ;
   DEFINE v_c_folio_liquida        DECIMAL(9,0) ;
   DEFINE v_c_num_credito          DECIMAL(10,0);
   DEFINE v_c_tpo_credito          SMALLINT     ;
   DEFINE v_c_edo_credito          SMALLINT     ;
   DEFINE v_c_sdo_deudor           DECIMAL(12,2); -- cambio precision
   DEFINE v_c_f_otorga             DATE         ;
   DEFINE v_c_f_culmina            DATE         ;
   DEFINE v_c_tpo_dscto            SMALLINT     ;
   DEFINE v_c_valor_dscto          DECIMAL(8,4) ;
   DEFINE v_c_nrp                  CHAR(11)     ;
   DEFINE v_c_f_ini_dscto          DATE         ;
   DEFINE v_c_nss_liberado         CHAR(11)     ;
   DEFINE v_c_f_gen_arh            DATE         ;
   DEFINE v_c_sdo_credito          DECIMAL(12,2); -- cambio precision
   DEFINE v_c_f_prox_liq           DATE         ;
   DEFINE v_c_f_desde              DATE         ;
   DEFINE v_c_f_hasta              DATE         ;
   DEFINE v_c_tpo_rch              SMALLINT     ;
   DEFINE v_c_edo_procesar         SMALLINT     ;
   DEFINE v_c_estado               SMALLINT     ;
   DEFINE v_h_id_mdt_his_mandato   DECIMAL(9,0) ;
   DEFINE v_id_cat_mandato         SMALLINT;

   LET    v_ins_det                = 0;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_alta_mandato.--TRACE';
   ----TRACE 'Inicia el store procedure de alta de mandatos';
   ----TRACE "Parte 1 - verifica existencia en mdt_acr_transferencia ";

   SELECT NVL(count(*),0) INTO v_num_credito
     FROM safre_viv:cre_acreditado
    WHERE id_derechohabiente = p_id_derechohabiente
      AND num_credito        = p_id_credito ;

   IF v_num_credito = 0 OR v_num_credito IS NULL THEN
      UPDATE safre_viv:mdt_solicitud_mandato
         SET estado = 106, diagnostico = '001'
       WHERE id_solicitud_mandato = p_id_solicitud_mandato;

       RETURN 106;
   END IF;

   ----TRACE "Parte 2 - verifica existencia en mdt_det_ctr_mandato ";

      LET v_id_cat_mandato = 0;

      SELECT a.id_cat_mandato ,
             b.cve_mandato   
      INTO   v_id_cat_mandato,
             v_cve_mandato
      FROM   safre_viv:mdt_cat_mandato         a ,
             safre_viv:mdt_solicitud_mandato   b ,
             safre_viv:mdt_cat_mandato_paquete c
      WHERE  b.id_solicitud_mandato = p_id_solicitud_mandato
      AND    b.cve_mandato    = c.cve_mandato
      AND    c.id_cat_mandato = a.id_cat_mandato ;

   IF v_id_cat_mandato is NULL THEN 
      LET v_id_cat_mandato = 0; 
   END IF;

   IF v_id_cat_mandato is NULL OR v_id_cat_mandato = 0  THEN
      UPDATE safre_viv:mdt_solicitud_mandato
         SET estado = 106, diagnostico = '020'
       WHERE id_solicitud_mandato = p_id_solicitud_mandato;
      RETURN 106;
   END IF;

     --SELECT COUNT(*)
       --FROM safre_viv:mdt_det_ctr_mandato a
      --WHERE a.id_derechohabiente = p_id_derechohabiente
        --AND a.cve_mandato        = v_cve_mandato 
--        AND a.estado             = 103 ;
--
     --IF v_existe > 0 THEN
        --UPDATE safre_viv:mdt_solicitud_mandato
           --SET estado = 106, diagnostico = '002'
         --WHERE id_solicitud_mandato = p_id_solicitud_mandato;
         --RETURN 106;
     --END IF;


     IF EXISTS (SELECT a.id_det_ctr_mandato
                    FROM safre_viv:mdt_det_ctr_mandato a
                    WHERE a.id_derechohabiente = p_id_derechohabiente
                    AND a.cve_mandato        = v_cve_mandato
                    AND a.estado             = 103 ) THEN

        UPDATE safre_viv:mdt_solicitud_mandato
           SET estado = 106, diagnostico = '002'
         WHERE id_solicitud_mandato = p_id_solicitud_mandato;
         RETURN 106;
     END IF


   ----TRACE "Parte 3 - verifica existencia en mdt_ctr_mandato antes de insertar";
   --TRACE "id_credito           = "||p_id_credito;
   --TRACE "p_id_derechohabiente = "||p_id_derechohabiente;


        SELECT *
          INTO v_s_id_solicitud_mandato   ,
               v_s_id_derechohabiente     ,
               v_s_id_origen              ,
               v_s_lote                   , --folio
               v_s_nss                    ,
               v_s_id_credito             ,
               v_s_tpo_descuento_mandato  ,
               v_s_valor_descuento_mandato,
               v_s_f_inicio_mandato       ,
               v_s_f_culmina_mandato      ,
               v_s_referencia             ,
               v_s_cve_mandato            ,
               v_s_scta_origen_descuento  ,
               v_s_modalidad_aplicacion   ,
               v_s_usuario                ,
               v_s_estado                 ,
               v_s_f_canales              ,
               v_s_id_canales             ,
               v_s_tipo_operacion         ,
               v_s_diagnostico
          FROM safre_viv:mdt_solicitud_mandato
         WHERE id_solicitud_mandato = p_id_solicitud_mandato
           AND id_credito           = p_id_credito ;

   --SELECT COUNT(*) INTO v_existe
     --FROM safre_viv:mdt_ctr_mandato
    --WHERE id_derechohabiente = p_id_derechohabiente
      --AND id_credito         = p_id_credito;

   --IF v_existe = 0 THEN  --no exite el control padre

     IF NOT EXISTS ( SELECT id_ctr_mandato
                     FROM safre_viv:mdt_ctr_mandato
                     WHERE id_derechohabiente = p_id_derechohabiente
                     AND id_credito         = p_id_credito) THEN
                   
         --TRACE "Parte 4 - Insertar mdt_ctr_mandato";
         --TRACE "Foreach de solicitudes por credito";
         --TRACE "Valida existencia de mdt_ctr_mandato 1de2: "||p_id_credito;

   --TRACE "Recupera datos del credito";

      SELECT tpo_credito ,
             edo_credito ,
             tpo_dscto   ,
             valor_dscto
        INTO v_c_tpo_credito         ,
             v_c_edo_credito         ,
             v_c_tpo_dscto           ,
             v_c_valor_dscto
        FROM safre_viv:cre_acreditado
       WHERE id_derechohabiente = p_id_derechohabiente
         AND num_credito        = p_id_credito
         AND valor_dscto        <> 0 ; -- por duplicados de mauro

    --TRACE "Asignacion antes del insert de mdt_ctr_mandato";
            -- Inicializacion del arreglo de encabezado de mandatos antes de su insercion
            LET v_m_id_derechohabiente      = v_s_id_derechohabiente;
            LET v_m_nss                     = v_s_nss               ;
            LET v_m_id_credito              = v_s_id_credito        ;
            LET v_m_lote                    = v_s_lote              ;
            LET v_m_f_lote                  = today ;
            LET v_m_lote                    = v_s_lote              ;
            LET v_m_tpo_credito             = v_c_tpo_credito       ;
            LET v_m_edo_credito             = v_c_edo_credito       ;
            LET v_m_tpo_descuento_credito   = v_c_tpo_dscto         ;
            LET v_m_valor_descuento_credito = v_c_valor_dscto       ;
            LET v_m_estado                  = 103                   ;
            LET v_m_tpo_prelacion           = ''                    ;
            LET v_m_usuario                 = p_v_usuario           ;


            -- AHM tmp por definir  IF v_m_id_lote_mandato IS NULL THEN
               LET v_m_id_lote_mandato         = 0                     ;
            -- AHM tmp por definir  END IF;

            -- Inicializacion del arreglo de detalle de mandatos antes de su insercion (1ra parte)
            LET v_d_id_ctr_mandato      = seq_mdt_ctr_mandato.NEXTVAL;
            LET v_d_id_derechohabiente  = v_m_id_derechohabiente;
            LET v_d_nss                 = v_m_nss               ;

     --TRACE "v_m_id_ctr_mandato: "||v_d_id_ctr_mandato;

            INSERT INTO safre_viv:mdt_ctr_mandato VALUES(v_d_id_ctr_mandato         ,
                                                         v_m_id_derechohabiente     ,
                                                         v_m_nss                    ,
                                                         v_m_id_credito             ,
                                                         v_m_f_lote                 ,
                                                         v_m_lote                   ,
                                                         v_m_id_lote_mandato        ,
                                                         v_m_tpo_credito            ,
                                                         v_m_edo_credito            ,
                                                         v_m_tpo_descuento_credito  ,
                                                         v_m_valor_descuento_credito,
                                                         v_m_estado                 ,
                                                         v_m_tpo_prelacion          ,
                                                         v_m_usuario                ,
                                                         v_m_id_lote_mandato        );

          
            LET v_ins_det = 1;

     --TRACE "Parte 5 recupera v_d_id_det_ctr_mandato";

   ELSE

      SELECT id_ctr_mandato 
        INTO v_d_id_ctr_mandato
        FROM safre_viv:mdt_ctr_mandato
       WHERE id_derechohabiente = p_id_derechohabiente
         AND id_credito         = p_id_credito;

       LET v_ins_det = 1;

   END IF;

   IF v_ins_det = 1 THEN

         LET v_d_tpo_descuento_mandato   = v_s_tpo_descuento_mandato  ;
         LET v_d_valor_descuento_mandato = v_s_valor_descuento_mandato;
         LET v_d_f_inicio_mandato        = v_s_f_inicio_mandato       ;
         LET v_d_f_culmina_mandato       = v_s_f_culmina_mandato      ;
         LET v_d_referencia              = v_s_referencia             ;
         LET v_d_scta_origen_descuento   = v_s_scta_origen_descuento  ;
         LET v_d_movimiento              = ""                         ;
         LET v_d_modalidad_aplicacion    = v_s_modalidad_aplicacion   ;
         LET v_d_f_presentacion          = today                      ;
         LET v_d_estado                  = 103                        ;
         LET v_d_id_derechohabiente      = p_id_derechohabiente       ;
         LET v_d_nss                     = v_s_nss                    ;

   --TRACE "v_d_id_ctr_mandato: "||v_d_id_ctr_mandato;

         LET v_d_id_det_ctr_mandato = seq_mdt_det_ctr_mandato.NEXTVAL;

   --TRACE "v_d_id_det_ctr_mandato"||v_d_id_det_ctr_mandato;

         INSERT INTO safre_viv:mdt_det_ctr_mandato VALUES(v_d_id_det_ctr_mandato     ,
                                                          v_d_id_ctr_mandato         ,
                                                          v_cve_mandato              ,
                                                          v_id_cat_mandato           ,
                                                          v_d_id_derechohabiente     ,
                                                          v_d_nss                    ,
                                                          v_d_tpo_descuento_mandato  ,
                                                          v_d_valor_descuento_mandato,
                                                          v_d_f_inicio_mandato       ,
                                                          v_d_f_culmina_mandato      ,
                                                          v_d_referencia             ,
                                                          v_d_scta_origen_descuento  ,
                                                          v_d_movimiento             ,
                                                          v_d_modalidad_aplicacion   ,
                                                          v_d_f_presentacion         ,
                                                          v_d_estado
                                                          );

       -- se inserta en la historia también el alta para poder efectuar el reverso

       --TRACE "inserta en mdt_his_mandato";

       LET v_h_id_mdt_his_mandato = 0;
       LET v_h_id_mdt_his_mandato = seq_mdt_his_mandato.NEXTVAL;
       
       --TRACE "his nextval: "||v_h_id_mdt_his_mandato;

         INSERT INTO mdt_his_mandato VALUES (v_h_id_mdt_his_mandato      ,
                                             v_d_id_det_ctr_mandato      , -- id det
                                             4                           , -- estado 
                                             today                       ,
                                             " "                         , 
                                             v_d_estado                  ,
                                             p_id_solicitud_mandato   
                                             ); 

   --TRACE "actualiza solicitud";
      UPDATE safre_viv:mdt_solicitud_mandato
         SET estado = 102
       WHERE id_solicitud_mandato = p_id_solicitud_mandato;

   END IF;

--TRACE "Fin del SP";

   RETURN 102;

END PROCEDURE;


