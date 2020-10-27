






CREATE FUNCTION "safreviv".fn_uni_integra_det_infonavit(p_usuario_cod    CHAR(20),
                                             p_proceso_cod    SMALLINT,
                                             p_nombre_archivo CHAR(18),
                                             p_folio          DECIMAL(9,0),
                                             p_pid            DECIMAL(9,0))
   RETURNING SMALLINT,      --v_i_resultado,    
             SMALLINT,      --v_si_total,       
             VARCHAR(250),  --err_txt,          
             DECIMAL(9,0),  --v_total_unificador
             DECIMAL(9,0)   --v_total_unificado;

-- Variables utilizadas para la integracion solo INFONAVIT
DEFINE v_tpo_movimiento                 CHAR(2)      ;
DEFINE v_espacios                       CHAR(2)      ;
DEFINE v_nrp                            CHAR(11)     ;
DEFINE v_fecha_movimiento               CHAR(8)      ;
DEFINE v_date_movimiento                DATE         ;
DEFINE v_curp_rfc                       CHAR(18)     ;
DEFINE v_tpo_trabajador                 DECIMAL(1,0) ;
DEFINE v_nss                            CHAR(11)     ;
DEFINE v_nombre                         CHAR(50)     ;
DEFINE v_presentacion_extemporanea      DECIMAL(1,0) ;
DEFINE v_jornada_o_semana               DECIMAL(1,0) ;
DEFINE v_sdi                            DECIMAL(6,0) ;
DEFINE v_sexo                           CHAR(1)      ;
DEFINE v_nss_correcto                   CHAR(11)     ;
DEFINE v_nombre_correcto                CHAR(50)     ;
DEFINE v_inserta_unificador             SMALLINT     ;
DEFINE v_inserta_unificado              SMALLINT     ;
DEFINE v_tpo_credito                    SMALLINT;
DEFINE v_tpo_credito_edo                SMALLINT;
--variables  del derechohabiente unificador
DEFINE v_id_derechohabiente_dor         DECIMAL(9,0) ;
DEFINE v_nss_der                        CHAR(11)     ;
DEFINE v_curp_der                       CHAR(18)     ;
DEFINE v_rfc_der                        CHAR(13)     ;
DEFINE v_f_nacimiento_der               DATE         ;
DEFINE v_nombre_imss_der                CHAR(50)     ;
DEFINE v_tipo_trabajador_der            CHAR(1)      ;
DEFINE v_id_credito_der                 SMALLINT     ;
DEFINE v_f_credito_der                  DATE         ;
--variables  del derechohabiente unificado
DEFINE v_id_derechohabiente_ado     DECIMAL(9,0) ;
DEFINE v_nss_der_edo                    CHAR(11)     ;
DEFINE v_curp_der_edo                   CHAR(18)     ;
DEFINE v_rfc_der_edo                    CHAR(13)     ;
DEFINE v_f_nacimiento_der_edo           DATE         ;
DEFINE v_nombre_imss_der_edo            CHAR(50)     ;
DEFINE v_tipo_trabajador_der_edo        CHAR(1)      ;
DEFINE v_id_credito_der_edo             SMALLINT     ;
DEFINE v_f_credito_der_edo              DATE         ;
-- Variables que se retornan            
DEFINE v_i_resultado                    SMALLINT     ;
DEFINE v_si_total                       SMALLINT     ;
DEFINE v_total_unificador               SMALLINT     ;
DEFINE v_total_unificado                SMALLINT     ;
-- Control de Excepciones               
DEFINE sql_err                          INTEGER      ;
DEFINE isam_err                         INTEGER      ;
DEFINE err_txt                          CHAR(200)    ;
--                                      
DEFINE v_si_resultado                   SMALLINT     ;
DEFINE v_d_id_referencia                DECIMAL(9,0) ;
-- Variable para marca de cuenta        
DEFINE v_i_estado_marca                 INTEGER      ;
DEFINE v_diagnostico_unificador         SMALLINT     ;
DEFINE v_diagnostico_unificado          SMALLINT     ;
DEFINE v_diagnostico_rechazo            SMALLINT     ;
DEFINE v_diagnos_rech_unificado         SMALLINT     ;
DEFINE v_tamaniocurp_rfc                SMALLINT     ;
--DEFINE v_inserta_unificador            SMALLINT    ;
DEFINE v_curp                           CHAR(18)     ;
DEFINE v_rfc                            CHAR(13)     ;
DEFINE v_fecha_texto                    VARCHAR(10);
DEFINE v_id_credito_unificado           SMALLINT;
DEFINE v_secuencia_unificador           DECIMAL(9,0);
DEFINE v_secuencia_unificado            DECIMAL(9,0);
DEFINE v_estado_familia                 SMALLINT;

--Variables del unificador para desmarca
DEFINE v_desmarca_id_inf_unificador     DECIMAL(9,0);
DEFINE v_desmarca_id_derecho_unificador DECIMAL(9,0);

--Variables del unificador para desmarca
DEFINE v_desmarca_id_inf_unificado      DECIMAL(9,0);
DEFINE v_desmarca_id_derecho_unificado  DECIMAL(9,0);

DEFINE v_existe_unificacion             SMALLINT;
DEFINE v_contador_tpo_credito_dor       SMALLINT;
DEFINE v_contador_tpo_credito_ado       SMALLINT;

--Variables funcion valida credito UNIFICADOR 
DEFINE r_dor_resultado                  SMALLINT;
DEFINE r_dor_id_derechohabiente         DECIMAL(9,0);
DEFINE r_dor_nss                        CHAR(11);
DEFINE r_dor_tpo_originacion            SMALLINT;
DEFINE r_dor_tpo_credito                SMALLINT;
DEFINE r_dor_num_credito                DECIMAL(10,0);
DEFINE r_dor_f_otorga                   DATE;
DEFINE r_dor_f_liquida                  DATE;
DEFINE r_dor_valida                     SMALLINT;

--Variables funcion valida credito unificado
DEFINE r_ado_resultado                  SMALLINT;
DEFINE r_ado_id_derechohabiente         DECIMAL(9,0);
DEFINE r_ado_nss                        CHAR(11);
DEFINE r_ado_tpo_originacion            SMALLINT;
DEFINE r_ado_tpo_credito                SMALLINT;
DEFINE r_ado_num_credito                DECIMAL(10,0);
DEFINE r_ado_f_otorga                   DATE;
DEFINE r_ado_f_liquida                  DATE;
DEFINE r_ado_valida                     SMALLINT;
DEFINE v_total_creditos                 INTEGER;
DEFINE v_nss_unificador                 CHAR(11);

ON EXCEPTION SET sql_err, isam_err, err_txt

   LET v_si_resultado = sql_err;
   
   RETURN v_si_resultado,
          isam_err,
          err_txt,
          v_id_derechohabiente_dor,
          v_id_derechohabiente_ado;
END EXCEPTION

-- Variables que almacenan informacion para su validacion
LET v_i_resultado               = 0;
LET v_tpo_movimiento            = NULL;
LET v_espacios                  = NULL;
LET v_nrp                       = NULL;
LET v_fecha_movimiento          = NULL;
LET v_curp_rfc                  = NULL;
LET v_tpo_trabajador            = 0;
LET v_nss                       = NULL;
LET v_nss_unificador            = NULL;
LET v_nombre                    = NULL;
LET v_presentacion_extemporanea = NULL;
LET v_jornada_o_semana          = NULL;
LET v_sdi                       = NULL;
LET v_sexo                      = NULL;
LET v_nss_correcto              = NULL;
LET v_nombre_correcto           = NULL;

LET v_id_derechohabiente_dor    = NULL;
LET v_nss_der                   = NULL;
LET v_curp_der                  = NULL;
LET v_rfc_der                   = NULL;
LET v_f_nacimiento_der          = NULL;
LET v_nombre_imss_der           = NULL;
LET v_tipo_trabajador_der       = NULL;
LET v_id_credito_der            = NULL;
LET v_f_credito_der             = NULL;

LET v_id_derechohabiente_ado    = NULL;
LET v_nss_der_edo               = NULL;
LET v_curp_der_edo              = NULL;
LET v_rfc_der_edo               = NULL;
LET v_f_nacimiento_der_edo      = NULL;
LET v_nombre_imss_der_edo       = NULL;
LET v_tipo_trabajador_der_edo   = NULL;
LET v_id_credito_der_edo        = NULL;
LET v_f_credito_der_edo         = NULL;

LET v_total_unificador          = 0;
LET v_total_unificado           = 0;
LET v_si_total                  = 0;
LET v_inserta_unificador        = 0;
LET v_inserta_unificado         = 0;
LET v_tamaniocurp_rfc           = 0;
LET err_txt                     = NULL;
LET v_tpo_credito               = 0;
LET v_tpo_credito_edo           = 0;

LET v_existe_unificacion        = 0;
LET v_contador_tpo_credito_dor  = 0;
LET v_contador_tpo_credito_ado  = 0;

LET v_diagnostico_unificador    = 1; -- aceptado
LET v_diagnostico_rechazo       = 1;
                                
LET v_diagnostico_unificado     = 1; -- aceptado
LET v_diagnos_rech_unificado    = 1;
                                
LET v_estado_familia            = 1;


   --SET DEBUG FILE TO "/safreviv_int/uni/envio/trace.uni.integra_INFONAVIT.txt";
   --TRACE ON;

   -- Selecciona la informacion insertada al cargar el archivo para su validacion
   -- detalle de unificación de cuentas solo INFONAVIT
   SELECT MAX(id_referencia)
   INTO   v_d_id_referencia
   FROM   uni_det_rechazos;

   -- Verifica que el id_referencia no venga nula
   -- en caso de ser contrario, se asigna el valor que trae
   IF(v_d_id_referencia IS NULL OR v_d_id_referencia = 0) THEN
      LET v_d_id_referencia = 0;
   END IF

   LET err_txt = "Al recuperar datos de tmp_mov_afi_trab_op21";

   FOREACH 
   SELECT nss_correcto
   INTO   v_nss_unificador
   FROM   safre_tmp:tmp_mov_afi_trab_op21
   GROUP BY nss_correcto
   
            --Se obtiene el valor de la secuencia de UNIFICADOR
            SELECT seq_uni_inf_unificador.NEXTVAL
            INTO   v_secuencia_unificador
            FROM   systables
            WHERE  tabid = 1;
   
      FOREACH
         SELECT tpo_movimiento,
                espacios,
                nrp,
                fecha_movimiento,
                LENGTH(curp_rfc),
                curp_rfc,
                tpo_trabajador,
                nss,
                nombre,
                presentacion_extemporanea,
                jornada_o_semana,
                sdi,
                sexo,
                nss_correcto,
                nombre_correcto
           INTO v_tpo_movimiento,
                v_espacios,
                v_nrp,
                v_fecha_movimiento,
                v_tamaniocurp_rfc,
                v_curp_rfc,
                v_tpo_trabajador,
                v_nss,
                v_nombre,
                v_presentacion_extemporanea,
                v_jornada_o_semana,
                v_sdi,
                v_sexo,
                v_nss_correcto,
                v_nombre_correcto
           FROM safre_tmp:tmp_mov_afi_trab_op21
          WHERE nss_correcto = v_nss_unificador
   
         -- se asume que no se tiene CURP ni RFC
         LET v_curp = NULL;
         LET v_rfc  = NULL;
   
         LET err_txt = "Al recuperar datos de afi_derechohabiente";
   
         -- Recupera datos de  afi_derechohabiente  unificador
         SELECT id_derechohabiente,
                nss               ,
                curp              ,
   					   rfc               ,
   					   f_nacimiento      ,
   					   nombre_imss       ,
   					   tipo_trabajador   ,
   					   id_credito        ,
   					   f_credito
           INTO v_id_derechohabiente_dor ,
                v_nss_der              ,
                v_curp_der             ,
                v_rfc_der              ,
                v_f_nacimiento_der     ,
                v_nombre_imss_der      ,
                v_tipo_trabajador_der  ,
                v_id_credito_der       ,
                v_f_credito_der
           FROM afi_derechohabiente
          WHERE nss = v_nss_correcto;
   
         -- Recupera datos de  afi_derechohabiente  unificado
         SELECT id_derechohabiente,
                nss               ,
                curp              ,
   					   rfc               ,
   					   f_nacimiento      ,
   					   nombre_imss       ,
   					   tipo_trabajador   ,
   					   id_credito        ,
   					   f_credito
           INTO v_id_derechohabiente_ado ,
                v_nss_der_edo              ,
                v_curp_der_edo             ,
                v_rfc_der_edo             ,
                v_f_nacimiento_der_edo     ,
                v_nombre_imss_der_edo      ,
                v_tipo_trabajador_der_edo  ,
                v_id_credito_der_edo       ,
                v_f_credito_der_edo
           FROM afi_derechohabiente
          WHERE nss = v_nss;
   
         LET err_txt = "v_id_credito_der="||v_id_credito_der;
         
         LET v_existe_unificacion = 0;
         
         --Valida si ya se cuenta con alguna unificación anterior
         SELECT COUNT(*)
         INTO   v_existe_unificacion
         FROM   uni_inf_unificador a, uni_inf_unificado b 
         WHERE  a.nss = v_nss_correcto
         AND    b.nss = v_nss
         AND    a.id_inf_unificador = b.id_unificador
         AND    a.estado_familia = 1;
   
         IF v_existe_unificacion >= 1 THEN
            -- se rechaza porque ya existe unificación ACEPTADA  --INSERT INTO uni_det_rechazos
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,               
                                                     5,                     
                                                     5,                     
                                                     v_d_id_referencia,     
                                                     "EU",   --Existe Unificacion
                                                     35,                    
                                                     v_nss_correcto);       
   
            LET v_diagnostico_unificador = 2;    -- rechazo
            LET v_diagnostico_rechazo    = 35;   -- se rechaza unificador porque ya existe unificación ACEPTADA
   
            -- se rechaza porque la unificacion es de Solo Infonavit        
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     5,
                                                     5,
                                                     v_d_id_referencia,
                                                     "EU",   --Existe Unificacion
                                                     36,
                                                     v_nss_correcto);
   
            -- se rechaza el unificado                                      
            LET v_diagnostico_unificado  = 2;   -- rechazo
            LET v_diagnos_rech_unificado = 36;  -- se rechaza unificado porque ya existe unificación ACEPTADA
   
            --Se rechaza la familia
            LET v_estado_familia  = 2; 
            LET v_existe_unificacion          = 0;                   
         END IF
   
         --Reinicia el contador de créditos      
         LET v_contador_tpo_credito_dor  = 0;
         LET v_contador_tpo_credito_ado  = 0;
         LET v_total_creditos = 0;
   
   
         --Valida si el UNIFICADOR tiene crédito
         EXECUTE FUNCTION fn_credito_vivienda(v_id_derechohabiente_dor,0)
         INTO  r_dor_resultado,
               r_dor_tpo_originacion,
               r_dor_tpo_credito,
               r_dor_num_credito,
               r_dor_f_otorga,
               r_dor_f_liquida;
        
         --Si no tiene tipo de credito 
         IF r_dor_resultado = 1 THEN 
            LET  v_tpo_credito = 0;
         ELSE
            --Si el UNIFICADOR tiene credito 
            IF r_dor_resultado = 0 THEN
               SELECT COUNT(*)
               INTO   v_contador_tpo_credito_dor
               FROM   cta_credito
               WHERE  id_derechohabiente = v_id_derechohabiente_dor;
   
               IF v_contador_tpo_credito_dor > 1 THEN
                  -- se rechaza porque la unificacion es de Solo Infonavit
                  EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                           5,
                                                           5,
                                                           v_d_id_referencia,
                                                           "Mas de un credito",
                                                           36,
                                                           v_nss_correcto);
   
                  -- se rechaza el unificado
                  LET v_diagnostico_unificado  = 2;     -- rechazo
                  LET v_diagnos_rech_unificado = 36;    -- Se rechaza por que el unificador tienes mas de un credito
               
                  --Se rechaza la familia
                  LET v_estado_familia  = 2;
               ELSE
                  LET v_tpo_credito = r_dor_tpo_credito;
               END IF
            END IF    --RESUL 0
   			END IF
         
         --Valida si el unificado tiene crédito
         -- c)	0 el derechohabiente tiene un crédito vigente
         -- d)	1 no hay un crédito vigente ni liquidado 
         EXECUTE FUNCTION fn_credito_vivienda(v_id_derechohabiente_ado,0)
         INTO  r_ado_resultado,
               r_ado_tpo_originacion,
               r_ado_tpo_credito,
               r_ado_num_credito,
               r_ado_f_otorga,
               r_ado_f_liquida;
   
         --Si el derechohabiente no tiene credito
         IF r_ado_resultado = 1 THEN 
            LET  v_tpo_credito_edo = 0;
         ELSE
            --Si el derechohabiente tiene credito
            IF r_ado_resultado = 0 THEN 
               SELECT COUNT(*)
               INTO   v_contador_tpo_credito_ado
               FROM   cta_credito
               WHERE  id_derechohabiente = v_id_derechohabiente_ado;
         
               IF v_contador_tpo_credito_ado > 1 THEN
                  -- se rechaza porque la unificacion es de Solo Infonavit
                  EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                           6,
                                                           6,
                                                           v_d_id_referencia,
                                                           "Mas de un credito",
                                                           37,
                                                           v_nss);
         
                  -- se rechaza el unificado
                  LET v_diagnostico_unificado  = 2;    -- rechazada
                  LET v_diagnos_rech_unificado = 37;   -- Se rechaza por que el unificado tienes mas de un credito
               
                  --Se rechaza la familia
                  LET v_estado_familia  = 2;
               ELSE             
                  LET v_tpo_credito_edo = r_ado_tpo_credito;
               END IF
            END IF --Si DH tiene credito    
   			END IF --¿Tiene credito?
   
   			--Se cuentan los creditos totales de la familia 
   			LET v_total_creditos = v_contador_tpo_credito_dor  + v_contador_tpo_credito_ado;
   
   			--Si la familia tiene mas de un credito 
   			IF v_total_creditos > 1 THEN 
   			   --Se inserta rechazo 
   			   EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                           6,
                                                           6,
                                                           v_d_id_referencia,
                                                           "Mas de un credito",
                                                           37,
                                                           v_nss);
   
            -- se rechaza el unificado
            LET v_diagnostico_unificado  = 2;     -- rechazada
            LET v_diagnos_rech_unificado = 38;    -- Se rechaza por que la familia tienes mas de un credito
   
            --Se rechaza la familia
            LET v_estado_familia = 2;
         END IF;
   
         LET err_txt = "Corrige fecha movimiento:"||v_fecha_movimiento;
         -- se cambia el formato de la fecha antes de asignarlo mediante el SP
         LET v_fecha_texto = v_fecha_movimiento[3,4] || "/" ||
                             v_fecha_movimiento[1,2] || "/" ||
                             v_fecha_movimiento[5,8];
   
         LET v_date_movimiento = DATE(v_fecha_texto);
   
         -- Acumula la referencia
         LET v_d_id_referencia = v_d_id_referencia + 1;
   {
         LET v_diagnostico_unificador = 1; -- aceptado
         LET v_diagnostico_rechazo    = 1;
         
         LET v_diagnostico_unificado  = 1; -- aceptado
         LET v_diagnos_rech_unificado = 1;
         
         LET v_estado_familia         = 1;
   }      
         LET err_txt = "Valida que unificados sean NSS77";
         -- si el NSS no es solo infonavit
         IF ( v_nss[1,2] <> 77 ) THEN
            -- se rechaza porque la unificacion es de Solo Infonavit
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     6,
                                                     6,
                                                     v_d_id_referencia,
                                                     "No",
                                                     28,
                                                     v_nss);
   
            -- se rechaza el unificado
            LET v_diagnostico_unificado  = 2;    -- rechazada
            LET v_diagnos_rech_unificado = 28;   -- NSS NO CORRESPONDE CON NSS 77
   
            --Se rechaza la familia
            LET v_estado_familia  = 2;
         END IF
   
         LET err_txt = "En id_derechohabiente, no existe";
   
         -- Valida el id_derechohabiente
         IF ( v_id_derechohabiente_dor IS NULL) AND (v_diagnostico_unificador = 1 ) THEN
            -- ERROR de detalle
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     5,
                                                     5,
                                                     v_d_id_referencia,
                                                     "No",
                                                     20,
                                                     v_nss_correcto);
                                                     
            LET v_diagnostico_unificador = 2;       -- rechazado
            LET v_diagnostico_rechazo    = 20;      -- NO EXISTE EL DERECHOHABIENTE UNIFICADOR INFONAVIT
   
            --Se rechaza la familia
            LET v_estado_familia  = 2;
         END IF
   
         --Rechazo unificado
         IF ( v_id_derechohabiente_ado IS NULL AND v_diagnostico_unificado = 1 ) THEN
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     6,
                                                     6,
                                                     v_d_id_referencia,
                                                     "No",
                                                     23,
                                                     v_nss);
                                                     
            LET v_diagnostico_unificado  = 2;     -- rechazada
            LET v_diagnos_rech_unificado = 23;    -- CURP/RFC INCORRECTO INFONAVIT
   
            --Se rechaza la familia
            LET v_estado_familia  = 2;
         END IF
   
         LET err_txt = "Valida nss";
   
         -- valida el NSS
         IF (v_nss_correcto <> v_nss_der) THEN
            --Rechazo unificado
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     5,
                                                     5,
                                                     v_d_id_referencia,
                                                     "No",
                                                     24,
                                                     v_nss_correcto);
                                                     
            LET v_diagnostico_unificador  = 2;      -- rechazada
            LET v_diagnostico_rechazo = 24;         -- NSS INCORRECTO INFONAVIT
            --Se rechaza la familia
            LET v_estado_familia  = 2;
         END IF
   
         IF (v_nss <> v_nss_der_edo) THEN
            --Rechazo unificado
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     6,
                                                     6,
                                                     v_d_id_referencia,
                                                     "No",
                                                     24,
                                                     v_nss);
                                                     
            LET v_diagnostico_unificado  = 2;      -- rechazada
            LET v_diagnos_rech_unificado = 24;     -- NSS INCORRECTO INFONAVIT
            --Se rechaza la familia
            LET v_estado_familia  = 2;
         END IF
   
         IF ( v_tamaniocurp_rfc > 13 ) THEN
            -- es CURP
            LET v_curp = v_curp_rfc;
            ---##SE AGREGAN VALIDACIONES DE CURP /unificado
            IF (v_curp <> v_curp_der) THEN
               --Rechazo unificado  Diagnostico: 23 - CURP/RFC
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        5,
                                                        5,
                                                        v_d_id_referencia,
                                                        "No",
                                                        23,
                                                        v_curp);
                                                        
               LET v_diagnostico_unificador  = 2;       -- rechazada
               LET v_diagnostico_rechazo = 23;          -- CURP/RFC INCORRECTO INFONAVIT
               --Se rechaza la familia
               LET v_estado_familia  = 2;
            END IF
   
            IF (v_curp <> v_curp_der_edo) THEN
               --Rechazo unificado  Diagnostico: 23 - CURP/RFC
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        6,
                                                        6,
                                                        v_d_id_referencia,
                                                        "No",
                                                        23,
                                                        v_curp);
                                                        
               LET v_diagnostico_unificado  = 2;        -- rechazada
               LET v_diagnos_rech_unificado = 23;       -- CURP/RFC INCORRECTO INFONAVIT
               --Se rechaza la familia
               LET v_estado_familia  = 2;
            END IF
         END IF
   
         -- valida si es CURP o RFC
         IF ( v_tamaniocurp_rfc <= 13 ) THEN
            -- es RFC
            LET v_rfc = v_curp_rfc;
            ---##SE AGREGAN VALIDACIONES DE RFC /unificado
            IF (v_rfc <> v_rfc_der) THEN
               --Rechazo unificado  Diagnostico: 23 - CURP/RFC
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        5,
                                                        5,
                                                        v_d_id_referencia,
                                                        "No",
                                                        23,
                                                        v_rfc);
                                                        
               LET v_diagnostico_unificador  = 2;      -- rechazada
               LET v_diagnostico_rechazo = 23;         -- CURP/RFC INCORRECTO INFONAVIT
               --Se rechaza la familia
               LET v_estado_familia  = 2;
            END IF
   
            IF (v_rfc <> v_rfc_der_edo) THEN
               --Rechazo unificado  Diagnostico: 23 - CURP/RFC
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        6,
                                                        6,
                                                        v_d_id_referencia,
                                                        "No",
                                                        23,
                                                        v_rfc);
                                                        
               LET v_diagnostico_unificado  = 2;       -- rechazada
               LET v_diagnos_rech_unificado = 23;      -- CURP/RFC INCORRECTO INFONAVIT
               --Se rechaza la familia
               LET v_estado_familia  = 2;
            END IF
         END IF
   
         LET err_txt = "Valida nombre";
         -- Se agregan validaciones nombre IMSS /unificado
         IF ( v_nombre_correcto <> v_nombre_imss_der ) THEN
            --Rechazo unificado  Diagnostico: 25 - NOMBRE
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     5,
                                                     5,
                                                     v_d_id_referencia,
                                                     "No",
                                                     25,
                                                     v_nombre_correcto);
   
            LET v_diagnostico_unificador  = 2;        -- rechazada
            LET v_diagnostico_rechazo = 25;           -- NOMBRE INCORRECTO INFONAVIT
            --Se rechaza la familia
            LET v_estado_familia  = 2;
         END IF
   
         IF ( v_nombre <> v_nombre_imss_der_edo ) THEN
               --Rechazo unificado  Diagnostico: 25 - NOMBRE
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     6,
                                                     6,
                                                     v_d_id_referencia,
                                                     "No",
                                                     25,
                                                     v_nombre);
   
            LET v_diagnostico_unificado  = 2;       -- rechazada
            LET v_diagnos_rech_unificado = 25;      -- NOMBRE INCORRECTO INFONAVIT
            --Se rechaza la familia
            LET v_estado_familia  = 2;
         END IF
   
   
         -- se verifica si el NSS unificador ya se tiene registrado
         SELECT COUNT(*)
         INTO   v_inserta_unificador
         FROM   uni_inf_unificador
         WHERE  nss = v_nss_correcto
         AND    folio_unificacion = p_folio;
   
         -- si el NSS unificador no se tiene, se inserta
         IF ( v_inserta_unificador = 0 ) THEN
   
   
            LET err_txt = "Al insertar  en uni_inf_unificador";
            INSERT INTO uni_inf_unificador(id_inf_unificador,
                                           folio_unificacion,
                                           tipo_registro,
                                           id_derechohabiente,
                                           nrp,
                                           curp,
                                           rfc,
                                           nombre,
                                           tipo_trabajador,
                                           sexo,
                                           nss,
                                           id_credito,
                                           tpo_credito,
                                           estado_familia,
                                           estado_unificacion,
                                           diagnostico,
                                           f_movimiento,
                                           f_liquidacion,
                                           f_notificacion,
                                           folio_liquidacion)
            VALUES(v_secuencia_unificador,
                   p_folio,
                   v_tpo_movimiento,
                   v_id_derechohabiente_dor,
                   v_nrp,
                   v_curp,
                   v_rfc ,
                   v_nombre_correcto,
                   v_tpo_trabajador,
                   v_sexo,
                   v_nss_correcto,
                   v_id_credito_der,
                   v_tpo_credito,
                   v_estado_familia,          --Estado Familia
                   v_diagnostico_unificador,  --Estado unificación
                   v_diagnostico_rechazo,     --Diagnóstico
                   v_date_movimiento,
                   NULL,
                   TODAY,
                   NULL);
   
            -- se cuenta un unificador y un registro insertado
            LET v_si_total = v_si_total + 1;
            LET v_total_unificador = v_total_unificador + 1;
   
            -- si el diagnostico unificador es correcto (1)
            IF ( v_diagnostico_unificador = 1 ) THEN
               LET v_i_estado_marca = 0;
               --LET v_i_resultado = 1;
   
               -- se marca el NSS unificador
               EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_dor,
                                                503,  -- marca de Unificacion de cuenta INFONAVIT
                                                seq_uni_inf_unificador.CURRVAL,
                                                p_folio,
                                                0,    -- estado marca
                                                0,    -- codigo de rechazo
                                                0,    -- marca de la causa
                                                NULL, -- fecha de la causa
                                                p_usuario_cod,
                                                p_proceso_cod)
               INTO v_i_estado_marca;
               
               --#######Validación de la marca UNIFICADOR *C01
               --Si la marca es rechazada se inserta el rechazo
               IF v_i_estado_marca <> 0 THEN 
                  --Inserta rechazo  17 --Rechazo por marca unificador
                  EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                           5,
                                                           5,
                                                           v_d_id_referencia,
                                                           "RM",
                                                           17,
                                                           v_nombre_correcto);
   
                  LET v_diagnostico_unificador  = 2;   -- rechazada
                  LET v_diagnostico_rechazo     = 17;  -- LA MARCA UNIFICADOR IMSS NO ESTA ACTIVA
   
                  --Se rechaza la familia
                  LET v_estado_familia  = 2;
   
                  UPDATE uni_inf_unificador
                  SET    estado_familia     = 2,
                         estado_unificacion = 2,
                         diagnostico        = 17
                  WHERE  id_inf_unificador      = v_secuencia_unificador;
               --### C01   
               END IF--Estado de Marca
            END IF
         END IF
   
         --Inicia integración de UNIFICADOS
         SELECT COUNT(*)
           INTO v_inserta_unificado
           FROM uni_inf_unificado
          WHERE nss = v_nss
          AND   folio_unificacion = p_folio;
   
         -- si el NSS unificador no se tiene, se inserta
         IF ( v_inserta_unificado = 0 ) THEN
   
   			   SELECT seq_uni_inf_unificado.NEXTVAL
              INTO v_secuencia_unificado
              FROM systables
             WHERE tabid = 1;
   
            LET err_txt = "Al insertar  en uni_inf_unificado";
            -- se inserta el NSS unificado
            INSERT INTO uni_inf_unificado(id_inf_unificado,
                                          id_unificador,
                                          folio_unificacion,
                                          tipo_registro,
                                          id_derechohabiente,
                                          nss,
                                          nombre,
                                          id_credito,
                                          tpo_credito,
                                          estado_unificacion,
                                          diagnostico )
            VALUES(v_secuencia_unificado,
                   v_secuencia_unificador,
                   p_folio,
                   "06",
                   v_id_derechohabiente_ado,
                   v_nss,
                   v_nombre,
                   v_id_credito_der_edo, --Pendiente con la tala nueva cre_acreditado
                   v_tpo_credito_edo,    --Pendiente con la tala nueva cre_acreditado
                   v_diagnostico_unificado,
                   v_diagnos_rech_unificado );
   
            LET v_total_unificado = v_total_unificado + 1;
   
            LET err_txt = "Al marcar cuenta como aceptada unificado";
            LET v_i_estado_marca = 0;
   
            -- se marca el NSS unificado solo si se valido correctamente
            IF ( v_diagnostico_unificado = 1 ) THEN
               EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_ado,
                                                504, -- marca de Unificado de cuenta INFONAVIT
                                                seq_uni_inf_unificado.CURRVAL,
                                                p_folio,
                                                0, -- estado marca
                                                0, -- codigo de rechazo
                                                0, -- marca de la causa
                                                NULL, -- fecha de la causa
                                                p_usuario_cod,
                                                p_proceso_cod)
               INTO v_i_estado_marca;
   
               --##Se determina el rechazo para el unificado *C01
               IF v_i_estado_marca <> 0 THEN             
                  --Inserta rechazo  18 --Rechazo por marca unificado
                  EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                           6,
                                                           6,
                                                           v_d_id_referencia,
                                                           "RM",
                                                           18,
                                                           v_nombre);
   
                  LET v_diagnostico_unificado  = 2;     -- rechazada
                  LET v_diagnos_rech_unificado = 18;    -- LA MARCA UNIFICADO IMSS NO ESTA ACTIVA
   
                  --Se rechaza la familia
                  LET v_estado_familia  = 2;               
                  
                  --Se actualizan el estado y diagnóstico del unificador
                  UPDATE uni_inf_unificado
                  SET    estado_unificacion = 2,
                         diagnostico        = 18
                  WHERE  id_inf_unificado   =  v_secuencia_unificado;
               END IF
               --## *C01
            END IF
         END IF
         
         LET v_curp_rfc = NULL;
   
         LET v_curp     = NULL;
         LET v_curp_der = NULL;
   
         LET v_rfc      = NULL;   
         LET v_rfc_der  = NULL;
   
         LET v_diagnostico_unificador = 1; -- aceptado
         LET v_diagnostico_rechazo    = 1;
   
         LET v_diagnostico_unificado  = 1; -- aceptado
         LET v_diagnos_rech_unificado = 1;
         
         LET v_estado_familia         = 1;
      END FOREACH;
      LET v_nss_unificador = NULL;
   END FOREACH;

   --Actualiza estado familia a rechazado
   UPDATE uni_inf_unificador
   SET    estado_familia = 2
   WHERE  folio_unificacion = p_folio
   AND    id_inf_unificador IN (SELECT a.id_unificador
                                FROM   uni_inf_unificado a
                                WHERE  a.folio_unificacion = p_folio
                                AND    a.estado_unificacion = 2
                                )
   AND  estado_familia = 1
   AND  estado_unificacion = 1;

   --Desmarca las cuentas
   FOREACH
      SELECT id_inf_unificador,
             id_derechohabiente
      INTO   v_desmarca_id_inf_unificador,
             v_desmarca_id_derecho_unificador
      FROM   uni_inf_unificador
      WHERE  folio_unificacion = p_folio
      AND    estado_familia = 2
      AND    estado_unificacion = 1

      EXECUTE FUNCTION fn_desmarca_cuenta(v_desmarca_id_derecho_unificador,
                                          503, -- marca de infonavit
                                          v_desmarca_id_inf_unificador,
                                          40,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
         INTO v_si_resultado;
   END FOREACH;

   FOREACH
      SELECT id_inf_unificado,
             id_derechohabiente
      INTO   v_desmarca_id_inf_unificado,
             v_desmarca_id_derecho_unificado
      FROM   uni_inf_unificado
      WHERE  folio_unificacion = p_folio
      AND    estado_unificacion = 1
      AND    id_unificador IN (SELECT id_inf_unificador
                               FROM   uni_inf_unificador
                               WHERE  folio_unificacion = p_folio
                               AND    estado_familia = 2)

      EXECUTE FUNCTION fn_desmarca_cuenta(v_desmarca_id_derecho_unificado,
                                          504, -- marca de infonavit
                                          v_desmarca_id_inf_unificado,
                                          40,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
         INTO v_si_resultado;
   END FOREACH;

    LET err_txt = "Al actualizar glo_ctr_archivo";

    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
    SET    folio = p_folio, 
           estado = 2 -- integrado
    WHERE  proceso_cod    = p_proceso_cod
    AND    opera_cod      = 1 -- archivo cargado
    AND    estado         = 1; -- etapa de carga

    UPDATE bat_ctr_operacion
    SET    folio       = p_folio,
           nom_archivo = p_nombre_archivo
    WHERE  proceso_cod = p_proceso_cod
    AND    opera_cod   = 2
    AND    pid         = p_pid;

    LET err_txt = " Termino  correctamente ";

    UPDATE STATISTICS FOR TABLE uni_inf_unificador;
    UPDATE STATISTICS FOR TABLE uni_inf_unificado;

    RETURN v_i_resultado,
           v_si_total,
           err_txt,
           v_total_unificador,
           v_total_unificado;

END FUNCTION;


