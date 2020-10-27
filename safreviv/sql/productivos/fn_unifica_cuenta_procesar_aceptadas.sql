






CREATE FUNCTION "safreviv".fn_unifica_cuenta_procesar_aceptadas(p_id_unificador         DECIMAL(9,0), 
			 	                                             p_id_unificado          DECIMAL(9,0), 
                                                     p_id_derecho_unificado  DECIMAL(9,0), 
                                                     p_id_derecho_unificador DECIMAL(9,0),
                                                     p_lote	                 DECIMAL(9,0),
                                                     p_f_lote	               DATE,
                                                     p_id_proceso	           SMALLINT, 
                                                     p_nom_archivo           CHAR(40),
                                                     p_usuario               CHAR(20),
                                                     p_secuencia             INTEGER,
                                                     p_ctr_folio_archivo     DECIMAL(9,0))
RETURNING SMALLINT,DECIMAL(9,0);

DEFINE v_error                   INTEGER;
DEFINE v_finaliza                INTEGER;

DEFINE v_id_cre_acreditado       DECIMAL(9,0) ;
DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0) ;
DEFINE v_folio_liquida           DECIMAL(9,0) ;
DEFINE v_id_derechohabiente      DECIMAL(9,0) ;
DEFINE v_tpo_originacion         SMALLINT     ;
DEFINE v_tpo_credito             SMALLINT     ;
DEFINE v_tpo_registro            CHAR(2)      ;
DEFINE v_num_credito             DECIMAL(10,0);
DEFINE v_sdo_deudor              DECIMAL(16,2);
DEFINE v_f_otorga                DATE         ;
DEFINE v_f_culmina               DATE         ;
DEFINE v_edo_credito             SMALLINT     ;
DEFINE v_tpo_dscto               SMALLINT     ;
DEFINE v_valor_dscto             DECIMAL(8,4) ;
DEFINE v_nrp                     CHAR(11)     ;
DEFINE v_f_ini_dscto             DATE         ;
DEFINE v_nss_liberado            CHAR(11)     ;
DEFINE v_f_gen_arh               DATE         ;
DEFINE v_sdo_credito             DECIMAL(16,2);
DEFINE v_f_prox_liq              DATE         ;
DEFINE v_f_desde                 DATE         ;
DEFINE v_f_hasta                 DATE         ;
DEFINE v_tpo_rch                 SMALLINT     ;
DEFINE v_edo_procesar            SMALLINT     ;
DEFINE v_estado                  SMALLINT     ;

DEFINE v_h_id_cre_acreditado     DECIMAL(9,0) ;
DEFINE v_h_id_cre_ctr_archivo    DECIMAL(9,0) ;
DEFINE v_h_tpo_transferencia     CHAR(2)      ;
DEFINE v_h_edo_procesar          SMALLINT     ;
DEFINE v_h_diagnostico           CHAR(3)      ;
DEFINE v_h_estado                SMALLINT     ;
DEFINE v_h_nss_afore             CHAR(11)     ;
DEFINE v_h_rfc_afore             CHAR(13)     ;
DEFINE v_h_paterno_afore         CHAR(40)     ;
DEFINE v_h_materno_afore         CHAR(40)     ;
DEFINE v_h_nombre_afore          CHAR(40)     ;
DEFINE v_h_nom_imss              CHAR(50)     ;
DEFINE v_h_f_proceso             DATE         ;

DEFINE v_h_c_id_derechohabiente  DECIMAL(9,0) ;
DEFINE v_h_c_proceso_cod         SMALLINT     ;
DEFINE v_h_c_tpo_credito         SMALLINT     ;
DEFINE v_h_c_num_credito         DECIMAL(10,0);
DEFINE v_h_c_f_credito           DATE         ;
DEFINE v_h_c_estado              SMALLINT     ;
DEFINE v_h_c_f_actualiza         DATE         ;

DEFINE v_c_id_derechohabiente	   DECIMAL(9,0) ;
DEFINE v_c_proceso_cod	         SMALLINT     ;
DEFINE v_c_tpo_credito	         SMALLINT     ;
DEFINE v_c_num_credito	         DECIMAL(10,0);
DEFINE v_c_f_credito	           DATE         ;

DEFINE v_ctr_id_cre_ctr_archivo	 DECIMAL(9,0) ;
DEFINE v_ctr_lote	               SMALLINT     ;
DEFINE v_ctr_f_lote	             DATE         ;
DEFINE v_ctr_id_proceso	         SMALLINT     ;
DEFINE v_ctr_operacion	         SMALLINT     ;
DEFINE v_ctr_nom_archivo	       CHAR(40)     ;
DEFINE v_ctr_tot_registros	     DECIMAL(10,0);
DEFINE v_ctr_tot_aceptados	     DECIMAL(10,0);
DEFINE v_ctr_tot_rechazados	     DECIMAL(10,0);
DEFINE v_ctr_tot_sin_origen	     DECIMAL(10,0);
DEFINE v_ctr_estado	             SMALLINT     ;
DEFINE v_ctr_f_proceso	         DATE         ;
DEFINE v_ctr_usuario	           CHAR(20)     ;

DEFINE v_a_id_credito            SMALLINT;
DEFINE v_a_f_credito             DATE    ;

--DEFINE v_seq_cre_archivo         BIGINT;
DEFINE v_num_registros           INTEGER;
--
DEFINE v_si_marca_imss           SMALLINT;
DEFINE v_si_marca_procesar       SMALLINT;
DEFINE v_si_resultado            SMALLINT;
DEFINE v_i_estado_marca          INTEGER;
DEFINE v_marca_inhabilita        SMALLINT;
DEFINE v_cambia_credito          SMALLINT;
DEFINE id_derechohabiente_error  DECIMAL(9,0);

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_error

      LET id_derechohabiente_error = p_id_derecho_unificado;
      
      RETURN v_cambia_credito,
             id_derechohabiente_error; -- ERROR
   END EXCEPTION WITH RESUME;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/safreviv_int/uni/envio/trace.fn_unifica_cuenta.txt';
   --TRACE ON;

   LET v_num_registros          = 0;
   LET v_marca_inhabilita       = 0;
   LET v_cambia_credito         = 0;
   LET id_derechohabiente_error = p_id_derecho_unificado;
    
   FOREACH
      SELECT *
   	    INTO v_id_cre_acreditado ,
             v_id_cre_ctr_archivo,
             v_folio_liquida,
             v_id_derechohabiente,
             v_tpo_originacion,
             v_tpo_credito ,
             v_tpo_registro,
             v_num_credito,
             v_sdo_deudor,
             v_f_otorga,
             v_f_culmina,
             v_edo_credito,
             v_tpo_dscto,
             v_valor_dscto,
             v_nrp,
             v_f_ini_dscto,
             v_nss_liberado,
             v_f_gen_arh,
             v_sdo_credito,
             v_f_prox_liq,
             v_f_desde,
             v_f_hasta,
             v_tpo_rch,
             v_edo_procesar,
             v_estado
   	    FROM cre_acreditado
   	   WHERE id_derechohabiente = p_id_derecho_unificado
   	     AND estado IN (140,145,900) -- ver indice
   	     AND edo_credito = 1
   	        
   	  FOREACH
   	     SELECT *
   	       INTO v_h_id_cre_acreditado,
                v_h_id_cre_ctr_archivo,
                v_h_tpo_transferencia,
                v_h_edo_procesar,
                v_h_diagnostico,
                v_h_estado,
                v_h_nss_afore,
                v_h_rfc_afore,
                v_h_paterno_afore,
                v_h_materno_afore,
                v_h_nombre_afore,
                v_h_nom_imss,
                v_h_f_proceso
   	       FROM cre_his_acreditado
   	      WHERE id_cre_acreditado = v_id_cre_acreditado
   	       -- AND id_cre_ctr_archivo = v_id_cre_ctr_archivo
   	      ORDER BY f_proceso,id_cre_acreditado
          -- Recuperando ultima fecha proceso registrada para el acreditado
          
         LET v_h_diagnostico = 0;
         LET v_h_estado      = 230;
         LET v_h_f_proceso   = TODAY;
         
         INSERT INTO cre_his_acreditado (id_cre_acreditado,
                                         id_cre_ctr_archivo,
                                         tpo_transferencia,
                                         edo_procesar,
                                         diagnostico,
                                         estado,
                                         nss_afore,
                                         rfc_afore,
                                         paterno_afore,
                                         materno_afore,
                                         nombre_afore,
                                         nom_imss,
                                         f_proceso)
                VALUES(v_h_id_cre_acreditado,
                       v_h_id_cre_ctr_archivo,
                       v_h_tpo_transferencia,
                       v_h_edo_procesar,
                       v_h_diagnostico,
                       v_h_estado,
                       v_h_nss_afore,
                       v_h_rfc_afore,
                       v_h_paterno_afore,
                       v_h_materno_afore,
                       v_h_nombre_afore,
                       v_h_nom_imss,
                       v_h_f_proceso);
      END FOREACH;
            
   	  UPDATE cre_acreditado
   	     SET estado = 230
   	   WHERE id_cre_acreditado = v_id_cre_acreditado;
      
      LET v_id_cre_ctr_archivo = p_secuencia;
      LET v_folio_liquida      = 0;
      LET v_id_derechohabiente = p_id_derecho_unificador;
      LET v_estado             = 220;
      
      LET v_num_registros = v_num_registros + 1;
      
      INSERT INTO cre_acreditado (id_cre_acreditado ,
                                  id_cre_ctr_archivo,
                                  folio_liquida,
                                  id_derechohabiente,
                                  tpo_originacion,
                                  tpo_credito,
                                  tpo_registro,
                                  num_credito,
                                  sdo_deudor,
                                  f_otorga,
                                  f_culmina,
                                  edo_credito,
                                  tpo_dscto,
                                  valor_dscto,
                                  nrp,
                                  f_ini_dscto,
                                  nss_liberado,
                                  f_gen_arh,
                                  sdo_credito,
                                  f_prox_liq,
                                  f_desde,
                                  f_hasta,
                                  tpo_rch,
                                  edo_procesar,
                                  estado)
             VALUES(seq_cre_acred.NEXTVAL,
                    v_id_cre_ctr_archivo,
                    v_folio_liquida,
                    v_id_derechohabiente,
                    v_tpo_originacion,
                    v_tpo_credito,
                    v_tpo_registro,
                    v_num_credito,
                    v_sdo_deudor,
                    v_f_otorga,
                    v_f_culmina,
                    v_edo_credito,
                    v_tpo_dscto,
                    v_valor_dscto,
                    v_nrp,
                    v_f_ini_dscto,
                    v_nss_liberado,
                    v_f_gen_arh,
                    v_sdo_credito,
                    v_f_prox_liq,
                    v_f_desde,
                    v_f_hasta,
                    v_tpo_rch,
                    v_edo_procesar,
                    v_estado);

      LET v_h_id_cre_ctr_archivo = p_secuencia;
      LET v_h_estado      = 220;
      LET v_h_f_proceso   = TODAY;

      INSERT INTO cre_his_acreditado (id_cre_acreditado,
                                      id_cre_ctr_archivo,
                                      tpo_transferencia,
                                      edo_procesar,
                                      diagnostico,
                                      estado,
                                      nss_afore,
                                      rfc_afore,
                                      paterno_afore,
                                      materno_afore,
                                      nombre_afore,
                                      nom_imss,
                                      f_proceso)
             VALUES(v_h_id_cre_acreditado,
                    v_h_id_cre_ctr_archivo,
                    v_h_tpo_transferencia,
                    v_h_edo_procesar,
                    v_h_diagnostico,
                    v_h_estado,
                    v_h_nss_afore,
                    v_h_rfc_afore,
                    v_h_paterno_afore,
                    v_h_materno_afore,
                    v_h_nombre_afore,
                    v_h_nom_imss,
                    v_h_f_proceso);
                                      
--###########################################
--#SE AGREGA EL INSERT A uni_cre_unificador #
--###########################################   
      INSERT INTO uni_cre_unificador (id_cre_unificador ,
                                      id_unificador,
                                      folio_lote,
                                      origen_unificacion,
                                      id_cre_acreditado,
                                      num_credito,
                                      tpo_credito,
                                      edo_credito,
                                      tpo_dscto,
                                      valor_dscto,
                                      f_registro,
                                      estado)
                               VALUES(seq_uni_cre_unificador.NEXTVAL,
                                      p_id_unificador,
                                      p_lote,     
                                      0,
                                      seq_cre_acred.CURRVAL,
                                      v_num_credito,
                                      v_tpo_credito,
                                      v_edo_credito,
                                      v_tpo_dscto,
                                      v_valor_dscto,
                                      TODAY,
                                      1); --Estado que índica que fue ACEPTADO
   	    
--INSERT DE uni_cre_unificado
--###########################################
--#SE AGREGA EL INSERT A uni_cre_unificado  #
--###########################################
      INSERT INTO uni_cre_unificado(id_cre_unificado,
                                    id_cre_unificador,
                                    id_unificado,
                                    id_cre_acreditado,
                                    num_credito,
                                    tpo_credito,
                                    edo_credito,
                                    tpo_dscto,
                                    valor_dscto,
                                    estado)
              VALUES(seq_uni_cre_unificado.NEXTVAL,              
                     seq_uni_cre_unificador.CURRVAL,         
                     p_id_unificado,                         
                     v_id_cre_acreditado,                    
                     v_num_credito,                          
                     v_tpo_credito,                          
                     v_edo_credito,                          
                     v_tpo_dscto,                            
                     v_valor_dscto,                          
                     1); --Estado que índica que fue ACEPTADO
------------------
      
      SELECT marca_inf,
             marca_prc
        INTO v_si_marca_imss,
             v_si_marca_procesar
        FROM cat_tipo_credito
       WHERE tpo_credito = v_tpo_credito;
        
        -- Segun el proceso, se selecciona la marca para inhabilitar

      LET v_marca_inhabilita = 501;
      
      EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derecho_unificado,
                                          v_si_marca_imss, -- marca de infonavit
                                          v_id_cre_acreditado,
                                          0,
                                          v_marca_inhabilita,
                                          p_usuario,
                                          p_id_proceso)
                       INTO v_si_resultado;
      
      EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derecho_unificado,
                                          v_si_marca_procesar, -- marca de infonavit
                                          v_id_cre_acreditado,
                                          0,
                                          v_marca_inhabilita,
                                          p_usuario,
                                          p_id_proceso)
                       INTO v_si_resultado;
                       
      FOREACH              
         SELECT *
           INTO v_h_c_id_derechohabiente,
                v_h_c_proceso_cod,
                v_h_c_tpo_credito,
                v_h_c_num_credito,
                v_h_c_f_credito,
                v_h_c_estado,
                v_h_c_f_actualiza
           FROM cta_his_credito
          WHERE id_derechohabiente = p_id_derecho_unificado
         
         LET v_h_c_estado = 230;
         LET v_h_c_f_actualiza = TODAY;
         
         IF v_h_c_id_derechohabiente IS NOT NULL THEN
            INSERT INTO cta_his_credito (id_derechohabiente,
                                         proceso_cod,
                                         tpo_credito,
                                         num_credito,
                                         f_credito,
                                         estado,
                                         f_actualiza)
                   VALUES(v_h_c_id_derechohabiente,
                          v_h_c_proceso_cod,
                          v_h_c_tpo_credito,
                          v_h_c_num_credito,
                          v_h_c_f_credito,
                          v_h_c_estado,
                          v_h_c_f_actualiza);
         END IF
      END FOREACH;
      
      SELECT *
        INTO v_c_id_derechohabiente,
             v_c_proceso_cod,
             v_c_tpo_credito,
             v_c_num_credito,
             v_c_f_credito
        FROM cta_credito
       WHERE id_derechohabiente = p_id_derecho_unificado;
       
      DELETE
        FROM cta_credito
       WHERE id_derechohabiente = p_id_derecho_unificado;
      
      LET v_c_id_derechohabiente = p_id_derecho_unificador;
      
      INSERT INTO cta_credito (id_derechohabiente,
                               proceso_cod,
                               tpo_credito,
                               num_credito,
                               f_credito)
             VALUES (v_c_id_derechohabiente,
                     v_c_proceso_cod,
                     v_c_tpo_credito,
                     v_c_num_credito,
                     v_c_f_credito);
      
      SELECT id_credito,
             f_credito  
        INTO v_a_id_credito,
             v_a_f_credito
        FROM afi_derechohabiente
       WHERE id_derechohabiente = p_id_derecho_unificado;
        
      UPDATE afi_derechohabiente
         SET id_credito = 0,
             f_credito  = TODAY
       WHERE id_derechohabiente = p_id_derecho_unificado;
      
      UPDATE afi_derechohabiente
         SET id_credito = v_a_id_credito,
             f_credito  = v_a_f_credito
       WHERE id_derechohabiente = p_id_derecho_unificador;

       LET v_cambia_credito = 1;
               
       EXECUTE FUNCTION fn_marca_cuenta(p_id_derecho_unificador,
                                        v_si_marca_imss,  -- marca de unificado IMSS
                                        seq_cre_acred.CURRVAL,
                                        p_ctr_folio_archivo,
                                        0,                -- estado marca
                                        0,                -- codigo de rechazo
                                        0,                -- marca de la causa
                                        NULL,             -- fecha de la causa
                                        p_usuario,
                                        p_id_proceso)
                   INTO v_i_estado_marca;            
   END FOREACH;
   
   RETURN v_cambia_credito,
          id_derechohabiente_error;

END FUNCTION;


