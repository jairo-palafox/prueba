






CREATE FUNCTION "safreviv".fn_ocg_unifica(p_id_unificador DECIMAL(9,0),p_id_unificado DECIMAL(9,0),p_folio INTEGER)
   RETURNING SMALLINT,SMALLINT

DEFINE v_nss_unificado         CHAR(11);
DEFINE v_nss_unificador        CHAR(11);
DEFINE v_error                 SMALLINT;
DEFINE v_fecha                 DATE;
DEFINE v_cnt_nss               SMALLINT;
DEFINE v_id_ocg_detalle        decimal(9,0);
DEFINE v_id_ocg_formalizacion  decimal(9,0);
DEFINE v_id_dh_unificador      decimal(9,0);
DEFINE v_id_dh_unificado       decimal(9,0);
DEFINE v_subproceso            SMALLINT;
DEFINE v_cnt_acreditado        SMALLINT;
DEFINE v_t_cve_ent_financiera  CHAR(3);
DEFINE v_t_rfc                 CHAR(13);
DEFINE v_t_curp                CHAR(18);
DEFINE v_t_ap_paterno          CHAR(40);
DEFINE v_t_ap_materno          CHAR(40);
DEFINE v_t_nombre              CHAR(40);
DEFINE v_t_num_bimestres       SMALLINT;
DEFINE v_t_viv97               DECIMAL(12,2);
DEFINE v_t_f_saldo             DATE;
DEFINE v_t_tpo_credito         CHAR(1);
DEFINE v_t_f_vigencia          DATE;
DEFINE v_t_f_respuesta         DATE;
DEFINE v_id_ocg_tramite_unificado  DECIMAL(9,0);
DEFINE v_t_diagnostico         SMALLINT;
DEFINE v_t_estado              SMALLINT;
DEFINE v_t_situacion           SMALLINT;
DEFINE v_cnt_formalizacion     SMALLINT;
DEFINE v_id_ocg_formalizacion_unificado decimal(9,0);
DEFINE v_f_cve_ent_financiera    smallint     ;
DEFINE v_f_num_ctr_int_ef        char(18)     ;
DEFINE v_f_rfc                   char(13)     ;
DEFINE v_f_curp                  char(18)     ;
DEFINE v_f_ap_paterno            char(40)     ;
DEFINE v_f_ap_materno            char(40)     ;
DEFINE v_f_nombre                char(40)     ;
DEFINE v_f_viv97                 decimal(12,2);
DEFINE v_f_num_escritura         char(8)      ;
DEFINE v_f_notario               decimal(4,0) ;
DEFINE v_f_ent_fed_notario       smallint     ;
DEFINE v_f_mcpio_notario         smallint     ;
DEFINE v_f_num_rpp               char(15)     ;
DEFINE v_f_folio_real            decimal(8,0) ;
DEFINE v_f_partida               decimal(6,0) ;
DEFINE v_f_foja                  decimal(8,0) ;
DEFINE v_f_volumen               decimal(6,0) ;
DEFINE v_f_libro                 decimal(6,0) ;
DEFINE v_f_tomo                  decimal(6,0) ;
DEFINE v_f_seccion               decimal(6,0) ;
DEFINE v_f_ent_fed_inmueble      smallint     ;
DEFINE v_f_mcpio_inmueble        decimal(5,0) ;
DEFINE v_f_domicilio_inmueble    char(30)     ;
DEFINE v_f_valor_avaluo          decimal(15,2);
DEFINE v_f_monto_credito         decimal(15,2);
DEFINE v_f_plazo_credito         decimal(5,0) ;
DEFINE v_f_tpo_moneda            smallint     ;
DEFINE v_f_tasa_base             char(20)     ;
DEFINE v_f_margen                char(20)     ;
DEFINE v_f_tpo_credito           char(1)      ;
DEFINE v_f_f_otorga_ent_fin      date         ;
DEFINE v_f_f_registro_carta      date         ;
DEFINE v_f_diagnostico           char(2)      ;
DEFINE v_f_estado                smallint     ;
DEFINE v_f_usuario_reg_carta     char(20)     ;
DEFINE v_f_situacion             smallint     ;
DEFINE v_f_f_vigencia            date         ;
DEFINE v_f_f_respuesta           date         ;
DEFINE v_f_f_saldo               date         ;
DEFINE v_f_genero                char(1)      ;
DEFINE v_f_id_tramite            decimal(9,0) ;
DEFINE v_a_f_formalizacion       date    ;
DEFINE v_a_f_marca_infonavit     date    ;
DEFINE v_a_f_solic_marca_prcr    date    ;
DEFINE v_a_f_conf_marca_prcr     date    ;
DEFINE v_a_f_liquida_credito     date    ;
DEFINE v_a_f_solic_desmarca_prcr date    ;
DEFINE v_a_f_conf_desmarca_prcr  date    ;
DEFINE v_a_estado                smallint;
DEFINE v_a_situacion             smallint;
DEFINE v_cnt_ug                  smallint;
DEFINE v_id_ocg_ug_unificado     decimal(9,0);
DEFINE v_u_id_ocg_formalizacion  decimal(9,0) ;
DEFINE v_u_id_ocg_tramite        decimal(9,0) ;
DEFINE v_u_id_derechohabiente    decimal(9,0) ;
DEFINE v_u_cve_ent_financiera    smallint     ;
DEFINE v_u_num_ctr_int_ef        char(18)     ;
DEFINE v_u_importe_solicitado    decimal(13,2);
DEFINE v_u_f_vencimiento         date         ;
DEFINE v_u_importe_utilizado     decimal(13,2);
DEFINE v_u_tpo_credito           char(1)      ;
DEFINE v_u_solicitud_saldo       smallint     ;
DEFINE v_u_diagnostico           char(2)      ;
DEFINE v_u_estado                smallint     ;
DEFINE v_u_situacion             smallint     ;
DEFINE v_id_ocg_tramite_unificador           decimal(9,0);
DEFINE v_id_ocg_detalle_tramite              decimal(9,0);
DEFINE v_id_ocg_detalle_formaliza_unificador decimal(9,0);
DEFINE v_id_ocg_formaliza_unificador         decimal(9,0);
DEFINE v_id_ocg_detalle_ug_unificador        decimal(9,0);
DEFINE v_id_ocg_solic_ug_unificador          decimal(9,0);
DEFINE v_ind_unificacion                     smallint;
DEFINE v_folio_unifica                       INTEGER;
DEFINE v_ind_recaudacion                     smallint;
DEFINE v_id_ocg_unificacion                  decimal(9,0);
DEFINE v_envio                               DATE;
DEFINE v_carga                               DATE;
DEFINE v_respuesta                           DATE;
DEFINE v_liquida_cofi                        DATE;
DEFINE v_alta                                DATE;
 
ON EXCEPTION SET v_error
      LET v_ind_unificacion = 0;
      RETURN v_ind_unificacion,v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_unifica.trace';
   TRACE ON;

   let v_error = 0;
   
   LET v_id_dh_unificado  = p_id_unificado;
   LET v_id_dh_unificador = p_id_unificador;
   LET v_folio_unifica    = p_folio;
   LET v_fecha            = today;
   LET v_ind_recaudacion  = NULL;
   LET v_ind_unificacion  = 0;
 
-- se recupera id_derechohabiente de nuevo NSS (nss unificador)

   SELECT nss
     INTO v_nss_unificador
     FROM afi_derechohabiente
    WHERE id_derechohabiente = v_id_dh_unificador;

-- se recupera id_derechohabiente de nss anterior (nss unificado)

   SELECT nss
     INTO v_nss_unificado
     FROM afi_derechohabiente
    WHERE id_derechohabiente = v_id_dh_unificado;

-- se verifica si nss unificado es acreditado 43bis

   SELECT count(*)
     INTO v_cnt_acreditado
     FROM ocg_formalizacion 
    WHERE id_derechohabiente = v_id_dh_unificado
      AND situacion IN (55,60,70,80)
      AND diagnostico = 1;

--si el contador es mayor o igual a uno, significa que tiene un crédito vigente

   IF v_cnt_acreditado >= 1 THEN

      LET v_envio        = NULL;
      LET v_carga        = NULL;
      LET v_respuesta    = NULL;
      LET v_liquida_cofi = NULL;
      LET v_alta         = NULL;
      LET v_id_ocg_tramite_unificador = NULL ;

-- se recuperan datos de las tablas de 43bis en donde se tengan registros de este derechohabiente

-- se verifican datos de ocg_tramite del último registro aceptado y concrédito vigente

      SELECT MAX(id_ocg_tramite)
        INTO v_id_ocg_tramite_unificado
        FROM ocg_tramite
       WHERE id_derechohabiente = v_id_dh_unificado
         AND situacion in (55,60,70,80)
         AND diagnostico = 1;

-- si v_id_ocg_tramite_unificado no es nulo, se recuperan datos de ocg_tramite
      IF v_id_ocg_tramite_unificado IS NOT NULL THEN

          LET v_envio        = NULL;
          LET v_carga        = NULL;
          LET v_respuesta    = NULL;
          LET v_liquida_cofi = NULL;
          LET v_alta         = NULL;

         SELECT cve_ent_financiera,
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
           INTO v_t_cve_ent_financiera,
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
                v_t_f_respuesta,
                v_t_diagnostico,
                v_t_estado,
                v_t_situacion
           FROM ocg_tramite
          WHERE id_ocg_tramite = v_id_ocg_tramite_unificado;

         SELECT f_envio,
                f_carga,
                f_respuesta,
                f_liquida_cofi,
                f_alta_registro
           INTO v_envio,
                v_carga,
                v_respuesta,
                v_liquida_cofi,
                v_alta
           FROM ocg_fecha_mig
          WHERE id_ocg_referencia = v_id_ocg_tramite_unificado
            AND subproceso = 1;

         LET v_id_ocg_tramite_unificador = seq_ocg_tramite.nextval;
         LET v_id_ocg_detalle_tramite    = seq_ocg_detalle.nextval;
         
         INSERT INTO ocg_detalle
              VALUES(v_id_ocg_detalle_tramite,
                     0,
                     v_id_dh_unificador,
                     1,
                     today,
                     v_t_cve_ent_financiera,
                     v_nss_unificador);

         INSERT INTO ocg_tramite
              VALUES(v_id_ocg_tramite_unificador,
                     v_id_ocg_detalle_tramite,
                     v_t_cve_ent_financiera,
                     v_id_dh_unificador,
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
                     v_t_f_respuesta,
                     v_t_diagnostico,
                     v_t_estado,
                     v_t_situacion);
                     
-- inserta en ocg_fecha_mig

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_tramite_unificador,
                     v_id_ocg_detalle_tramite,
                     v_id_dh_unificador,
                     v_envio,
                     v_carga,
                     v_respuesta,
                     v_liquida_cofi,
                     1,
                     v_alta);

         UPDATE ocg_tramite
            SET situacion = 300,
                estado    = 300
          WHERE id_ocg_tramite = v_id_ocg_tramite_unificado;

      END IF

-- se verifica si tiene formalización

      SELECT MAX(id_ocg_formalizacion)
        INTO v_id_ocg_formalizacion_unificado
        FROM ocg_formalizacion
       WHERE id_derechohabiente = v_id_dh_unificado
         AND situacion in (55,60,70,80)
         AND diagnostico = 1;
      
-- si tiene datos de formalizacion, se recuperan datos
      IF v_id_ocg_formalizacion_unificado IS NOT NULL THEN

          LET v_envio        = NULL;
          LET v_carga        = NULL;
          LET v_respuesta    = NULL;
          LET v_liquida_cofi = NULL;
          LET v_alta         = NULL;

         SELECT id_ocg_tramite    ,
                cve_ent_financiera,
                num_ctr_int_ef    ,
                rfc               ,
                curp              ,
                ap_paterno        ,
                ap_materno        ,
                nombre            ,
                viv97             ,
                num_escritura     ,
                notario           ,
                ent_fed_notario   ,
                mcpio_notario     ,
                num_rpp           ,
                folio_real        ,
                partida           ,
                foja              ,
                volumen           ,
                libro             ,
                tomo              ,
                seccion           ,
                ent_fed_inmueble  ,
                mcpio_inmueble    ,
                domicilio_inmueble,
                valor_avaluo      ,
                monto_credito     ,
                plazo_credito     ,
                tpo_moneda        ,
                tasa_base         ,
                margen            ,
                tpo_credito       ,
                f_otorga_ent_fin  ,
                f_registro_carta  ,
                diagnostico       ,
                estado            ,
                usuario_reg_carta ,
                situacion         ,
                f_vigencia        ,
                f_respuesta       ,
                f_saldo           ,
                genero
           INTO v_f_id_tramite        ,
                v_f_cve_ent_financiera,  
                v_f_num_ctr_int_ef    ,  
                v_f_rfc               ,  
                v_f_curp              ,  
                v_f_ap_paterno        ,  
                v_f_ap_materno        ,  
                v_f_nombre            ,  
                v_f_viv97             ,  
                v_f_num_escritura     ,  
                v_f_notario           ,  
                v_f_ent_fed_notario   ,  
                v_f_mcpio_notario     ,  
                v_f_num_rpp           ,  
                v_f_folio_real        ,  
                v_f_partida           ,  
                v_f_foja              ,  
                v_f_volumen           ,  
                v_f_libro             ,  
                v_f_tomo              ,  
                v_f_seccion           ,  
                v_f_ent_fed_inmueble  ,  
                v_f_mcpio_inmueble    ,  
                v_f_domicilio_inmueble,  
                v_f_valor_avaluo      ,  
                v_f_monto_credito     ,  
                v_f_plazo_credito     ,  
                v_f_tpo_moneda        ,  
                v_f_tasa_base         ,  
                v_f_margen            ,  
                v_f_tpo_credito       ,  
                v_f_f_otorga_ent_fin  ,  
                v_f_f_registro_carta  ,  
                v_f_diagnostico       ,  
                v_f_estado            ,  
                v_f_usuario_reg_carta ,  
                v_f_situacion         ,  
                v_f_f_vigencia        ,  
                v_f_f_respuesta       ,  
                v_f_f_saldo           ,  
                v_f_genero
           FROM ocg_formalizacion
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion_unificado;
          
         SELECT f_formalizacion,
                f_marca_infonavit,
                f_solic_marca_prcr,
                f_conf_marca_prcr,
                f_liquida_credito,
                f_solic_desmarca_prcr,
                f_conf_desmarca_prcr,
                estado,
                situacion
           INTO v_a_f_formalizacion,
                v_a_f_marca_infonavit,
                v_a_f_solic_marca_prcr,
                v_a_f_conf_marca_prcr,
                v_a_f_liquida_credito,
                v_a_f_solic_desmarca_prcr,
                v_a_f_conf_desmarca_prcr,
                v_a_estado,
                v_a_situacion
           FROM ocg_acreditado
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion_unificado;

         SELECT f_envio,
                f_carga,
                f_respuesta,
                f_liquida_cofi,
                f_alta_registro
           INTO v_envio,
                v_carga,
                v_respuesta,
                v_liquida_cofi,
                v_alta
           FROM ocg_fecha_mig
          WHERE id_ocg_referencia = v_id_ocg_formalizacion_unificado
            AND subproceso = 2;

-- se obtiene consecutivos de secuencias
         LET v_id_ocg_detalle_formaliza_unificador = seq_ocg_detalle.nextval;
         LET v_id_ocg_formaliza_unificador         = seq_ocg_formalizacion.nextval;

-- se verifica si el id_tramite debe ser el recuperado o el actualizado en tramite
IF v_id_ocg_tramite_unificador = 0 OR
   v_id_ocg_tramite_unificador IS NULL THEN
   LET v_f_id_tramite = v_f_id_tramite;
ELSE
   LET v_f_id_tramite = v_id_ocg_tramite_unificador;
END IF

-- se insertan datos correspondientes
         INSERT INTO ocg_detalle
              VALUES(v_id_ocg_detalle_formaliza_unificador,
                     0,
                     v_id_dh_unificador,
                     2,
                     today,
                     v_f_cve_ent_financiera,
                     v_nss_unificador);
                     
         INSERT INTO ocg_formalizacion
              VALUES(v_id_ocg_formaliza_unificador,
                     v_id_ocg_detalle_formaliza_unificador,
                     v_f_id_tramite , 
                     v_id_dh_unificador    ,
                     v_f_cve_ent_financiera,  
                     v_f_num_ctr_int_ef    ,  
                     v_f_rfc               ,  
                     v_f_curp              ,  
                     v_f_ap_paterno        ,  
                     v_f_ap_materno        ,  
                     v_f_nombre            ,  
                     v_f_viv97             ,  
                     v_f_num_escritura     ,  
                     v_f_notario           ,  
                     v_f_ent_fed_notario   ,  
                     v_f_mcpio_notario     ,  
                     v_f_num_rpp           ,  
                     v_f_folio_real        ,  
                     v_f_partida           ,  
                     v_f_foja              ,  
                     v_f_volumen           ,  
                     v_f_libro             ,  
                     v_f_tomo              ,  
                     v_f_seccion           ,  
                     v_f_ent_fed_inmueble  ,  
                     v_f_mcpio_inmueble    ,  
                     v_f_domicilio_inmueble,  
                     v_f_valor_avaluo      ,  
                     v_f_monto_credito     ,  
                     v_f_plazo_credito     ,  
                     v_f_tpo_moneda        ,  
                     v_f_tasa_base         ,  
                     v_f_margen            ,  
                     v_f_tpo_credito       ,  
                     v_f_f_otorga_ent_fin  ,  
                     v_f_f_registro_carta  ,  
                     v_f_diagnostico       ,  
                     v_f_estado            ,  
                     v_f_usuario_reg_carta ,  
                     v_f_situacion         ,  
                     v_f_f_vigencia        ,  
                     v_f_f_respuesta       ,  
                     v_f_f_saldo           ,  
                     v_f_genero);
                     
                  INSERT INTO ocg_acreditado
              VALUES(v_id_ocg_formaliza_unificador,
                     v_a_f_formalizacion,
                     v_a_f_marca_infonavit,
                     v_a_f_solic_marca_prcr,
                     v_a_f_conf_marca_prcr,
                     v_a_f_liquida_credito,
                     v_a_f_solic_desmarca_prcr,
                     v_a_f_conf_desmarca_prcr,
                     v_a_estado,
                     v_a_situacion);

-- inserta en ocg_fecha_mig

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_formaliza_unificador,
                     v_id_ocg_detalle_formaliza_unificador,
                     v_id_dh_unificador,
                     v_envio,
                     v_carga,
                     v_respuesta,
                     v_liquida_cofi,
                     2,
                     v_alta);
                     
-- se actualizan tablas

         UPDATE ocg_formalizacion 
            SET situacion = 300,
                estado    = 300
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion_unificado;
          
         UPDATE ocg_acreditado
            SET situacion = 300,
                estado    = 300
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion_unificado;
                     
           
      END IF

--se verifica si tiene registros en ocg_solicitud_uso_garantia aceptadas

      SELECT count(*)
        INTO v_cnt_ug
        FROM ocg_solicitud_uso_garantia
       WHERE id_derechohabiente = v_id_dh_unificado
         AND situacion in (50,90);

--si existe uno o mas registros en situacion 50 y 90, se actualizan
      IF v_cnt_ug >=1 THEN

         FOREACH
--se recuperan datos de usos para cada registro existente

            SELECT id_ocg_solicitud_ug,
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
              INTO v_id_ocg_ug_unificado,
                   v_u_id_ocg_formalizacion,
                   v_u_id_ocg_tramite      ,
                   v_u_id_derechohabiente  ,
                   v_u_cve_ent_financiera  ,
                   v_u_num_ctr_int_ef      ,
                   v_u_importe_solicitado  ,
                   v_u_f_vencimiento       ,
                   v_u_importe_utilizado   ,
                   v_u_tpo_credito         ,
                   v_u_solicitud_saldo     ,
                   v_u_diagnostico         ,
                   v_u_estado              ,
                   v_u_situacion
              FROM ocg_solicitud_uso_garantia
             WHERE id_derechohabiente = v_id_dh_unificado
               AND situacion IN (50,90)

             LET v_envio        = NULL;
             LET v_carga        = NULL;
             LET v_respuesta    = NULL;
             LET v_liquida_cofi = NULL;
             LET v_alta         = NULL;

         SELECT f_envio,
                f_carga,
                f_respuesta,
                f_liquida_cofi,
                f_alta_registro
           INTO v_envio,
                v_carga,
                v_respuesta,
                v_liquida_cofi,
                v_alta
           FROM ocg_fecha_mig
          WHERE id_ocg_referencia = v_id_ocg_ug_unificado
            AND subproceso = 3;

--se recuperan consecutivos de secuencias

            LET v_id_ocg_detalle_ug_unificador     = seq_ocg_detalle.nextval;      
            LET v_id_ocg_solic_ug_unificador       = seq_ocg_solic_ug.nextval;
            
            INSERT INTO ocg_detalle
                 VALUES(v_id_ocg_detalle_ug_unificador,
                        0,
                        v_id_dh_unificador,
                        3,
                        today,
                        v_u_cve_ent_financiera,
                        v_nss_unificador);

            INSERT INTO ocg_solicitud_uso_garantia
                 VALUES(v_id_ocg_solic_ug_unificador,
                        v_id_ocg_detalle_ug_unificador,
                        v_id_ocg_formaliza_unificador ,
                        v_u_id_ocg_tramite      ,
                        v_id_dh_unificador  ,
                        v_u_cve_ent_financiera  ,
                        v_u_num_ctr_int_ef      ,
                        v_u_importe_solicitado  ,
                        v_u_f_vencimiento       ,
                        v_u_importe_utilizado   ,
                        v_u_tpo_credito         ,
                        v_u_solicitud_saldo     ,
                        v_u_diagnostico         ,
                        v_u_estado              ,
                        v_u_situacion);

-- inserta en ocg_fecha_mig

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_solic_ug_unificador,
                     v_id_ocg_detalle_ug_unificador,
                     v_id_dh_unificador,
                     v_envio,
                     v_carga,
                     v_respuesta,
                     v_liquida_cofi,
                     3,
                     v_alta);
                        
            IF v_u_situacion = 90 THEN
              EXECUTE FUNCTION fn_uso_actualiza_uni(v_nss_unificador,v_id_dh_unificador,v_id_ocg_solic_ug_unificador,
                                                    v_nss_unificado ,v_id_dh_unificado ,v_id_ocg_ug_unificado,v_folio_unifica)
                          INTO v_ind_recaudacion;
            END IF

--se actualiza situacion en registro de unificado

            UPDATE ocg_solicitud_uso_garantia
               SET situacion = 300,
                   estado    = 300
             WHERE id_ocg_solicitud_ug = v_id_ocg_ug_unificado;
             
--falta ingresar registro en ocg_fecha_mig
--falta resetear variables de ug

         END FOREACH

      END IF
      
      LET v_id_ocg_unificacion = seq_ocg_unificacion.nextval;


      IF v_ind_recaudacion IS NULL THEN
-- este v_ind_recaudacion es para indicar unificación en tablas de 43bis sin unificación en RF
         LET v_ind_recaudacion = 1;
      END IF
      
      INSERT INTO ocg_unificacion
           VALUES(v_id_ocg_unificacion,
                  v_nss_unificador,
                  v_id_dh_unificador,
                  v_nss_unificado,
                  v_id_dh_unificado,
                  v_folio_unifica,
                  v_ind_recaudacion,
                  today);

--falta resetear variables de ug
-- este v_ind_unificacion es el que se pasa a la función de unificación para indicar unificación de 43bis
      LET v_ind_unificacion = 1;
   ELSE
-- este v_ind_unificacion es el que se pasa a la función de unificación para indicar que no se unificó en 43bis por no tener crédito vigente
      LET v_ind_unificacion = 0;
   END IF

   RETURN v_ind_unificacion,v_error;

END FUNCTION;


