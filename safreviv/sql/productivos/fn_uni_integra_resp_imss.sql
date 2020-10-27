






CREATE FUNCTION "safreviv".fn_uni_integra_resp_imss(p_folio_int_resp_sist_imss DECIMAL(9,0),                                                                  
                                         p_pid                      DECIMAL(9,0),
                                         p_proceso_cod              SMALLINT,
                                         p_nombre_archivo           CHAR(40))                              

       RETURNING SMALLINT,    --Error SQL
                 SMALLINT,    --Error ISAM
                 VARCHAR(250),--Error Texto
                 INTEGER,     --Total UNIFICADOR
                 INTEGER      --Total unificado

--Variables UNIFICADOR
DEFINE v_id_unificador      DECIMAL(9,0);          
DEFINE v_proceso_cod        SMALLINT    ;
DEFINE v_folio              DECIMAL(9,0);          
DEFINE v_id_referencia      DECIMAL(9,0);          
DEFINE v_id_referencia_ado  DECIMAL(9,0);          
DEFINE v_nss                CHAR(11)    ;          
DEFINE v_f_integracion      DATE        ;
DEFINE v_f_liquidacion      DATE        ;          
DEFINE v_f_notificacion     DATE        ;          
DEFINE v_estado             SMALLINT    ;          
DEFINE v_estado_familia     SMALLINT    ;          
--Variables UNIFICADO
DEFINE v_id_unificado       DECIMAL(9,0);  
DEFINE v_nss_ado            CHAR(11)    ;  
DEFINE v_estado_ado         SMALLINT    ;  

DEFINE v_nss_unificador     CHAR(11)    ;
DEFINE v_nss_unificado      CHAR(11)    ;
DEFINE v_nsscta1            CHAR(11)    ;
DEFINE v_nsscta2            CHAR(11)    ;
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_estado_unificacion SMALLINT    ;
-- Control de Excepciones
DEFINE sql_err              INTEGER     ;                           
DEFINE isam_err             INTEGER     ;                           
DEFINE err_txt              CHAR(200)   ;  
DEFINE v_si_resultado       SMALLINT    ; 

DEFINE v_fecha_unificacion  DATE        ;

DEFINE v_fecha_texto          CHAR(10);
DEFINE v_fec_proceso          CHAR(8);
DEFINE v_date_movimiento      DATE;
DEFINE v_inserta_unificador   SMALLINT;
DEFINE v_secuencia_unificador DECIMAL(9,0);
DEFINE v_si_total             SMALLINT;
DEFINE v_total_unificador     SMALLINT;
DEFINE v_secuencia_unificado  DECIMAL(9,0);                                                                                                                  
DEFINE v_total_unificado      SMALLINT;

DEFINE v_resp_id_unificador     DECIMAL(9,0);
DEFINE v_resp_id_referencia     DECIMAL(9,0);
DEFINE v_resp_nss               CHAR(11);
DEFINE v_resp_id_referencia_ado DECIMAL(9,0);
DEFINE v_resp_nsscta1           CHAR(11);

DEFINE v_env_f_integracion      DATE;
DEFINE v_env_id_referencia      DECIMAL(9,0);
        
ON EXCEPTION SET sql_err, 
                 isam_err,
                 err_txt 
   LET v_si_resultado = sql_err;
   RETURN v_si_resultado,
          isam_err,
          err_txt,
          v_total_unificador, 
          v_total_unificado;
END EXCEPTION 

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_integra_respIMSS.trace';
   --TRACE ON;

LET v_total_unificador            = 0;   
LET v_total_unificado             = 0;   
LET v_si_total                    = 0;   
LET v_inserta_unificador          = 0;   

LET v_si_resultado = 0; 
LET isam_err       = 0; 
LET err_txt        = '';

   FOREACH 
      --Se consultan los UNIFICADORES DEL ARCHIVO      
      SELECT nss_unificador,
             nss_unificado,
             fec_proceso
        INTO v_nss_unificador,
             v_nss_unificado,
             v_fec_proceso
        FROM safre_tmp:tmp_det_res_oci a, 
             safre_tmp:tmp_sum_res_oci b

      --Si el NSS UNIFICADOR existe en la tabla de UNIFICACION                
      IF v_nss_unificador IS NOT NULL THEN    
         
         LET err_txt = "Corrige fecha movimiento:"||v_fec_proceso;             

         LET v_fecha_texto = v_fec_proceso[5,6] || "/" ||
                             v_fec_proceso[7,8] || "/" ||
                             v_fec_proceso[1,4] ; 
                             
         LET v_date_movimiento = DATE(v_fecha_texto);                      
      
         SELECT f_notificacion, 
                id_unificador
           INTO v_f_notificacion, 
                v_id_unificador
           FROM uni_det_unificador 
          WHERE nss_unificador = v_nss_unificador;
          
         IF v_id_unificador IS NULL THEN
            LET v_estado     = 2 ; --Rechazado
            LET v_estado_familia = 2 ; --Rechazado
            LET v_id_referencia  = 0;
         ELSE
            LET v_estado     = 1 ; --Aceptado
            LET v_estado_familia = 1 ; --Aceptado
            LET v_id_referencia  = v_id_unificador;
         END IF 
         
         SELECT id_unificado,
                nsscta1
           INTO v_id_unificado,
                v_nsscta1
           FROM uni_det_unificado
          WHERE nsscta1 = v_nss_unificado;

         IF v_nsscta1 IS NULL THEN
            LET v_estado_ado     = 2 ; --Rechazado 
            LET v_id_referencia_ado  = 0;
         ELSE
            LET v_estado_ado     = 1 ; --Aceptado
            LET v_id_referencia_ado = v_id_unificado;
         END IF
                                    
         SELECT COUNT(*)                                                        
           INTO v_inserta_unificador                                            
           FROM uni_resp_int_unificador                                              
          WHERE nss = v_nss_unificador
          AND   folio = p_folio_int_resp_sist_imss;                                                                               
                                                                          
         -- si el NSS unificador no se tiene, se inserta                        
         IF ( v_inserta_unificador = 0 ) THEN      
            --Se obtiene el valor de la secuencia de UNIFICADOR 
            
            SELECT seq_uni_resp_int_dor.NEXTVAL
              INTO v_secuencia_unificador
              FROM systables
             WHERE tabid = 1;

            LET v_proceso_cod    = p_proceso_cod;
            LET v_nss            = v_nss_unificador;
            LET v_f_integracion  = v_date_movimiento ;
            LET v_f_notificacion = v_date_movimiento;
            
            INSERT INTO uni_resp_int_unificador (id_unificador  ,
                                                  proceso_cod   ,
                                                  folio         ,
                                                  id_referencia ,
                                                  nss           ,
                                                  f_integracion ,
                                                  f_notificacion,
                                                  estado        ,
                                                  estado_familia)
                                           VALUES(v_secuencia_unificador, 
                                                  v_proceso_cod   ,
                                                  p_folio_int_resp_sist_imss,
                                                  v_id_referencia ,
                                                  v_nss           ,
                                                  v_f_integracion ,
                                                  v_f_notificacion,
                                                  v_estado        ,
                                                  v_estado_familia
                                                  );        
                 
                 -- se cuenta un unificador y un registro insertado                  
            LET v_si_total = v_si_total + 1;                                    
            LET v_total_unificador = v_total_unificador + 1;                    
         END IF 
             
         SELECT seq_uni_resp_int_ado.NEXTVAL
           INTO v_secuencia_unificado
           FROM systables
          WHERE tabid = 1;

         INSERT INTO uni_resp_int_unificado(id_unificado,
                                            id_unificador,
                                            id_referencia_ado,
                                            nss,          
                                            estado)
                                     VALUES(v_secuencia_unificado,
                                            seq_uni_resp_int_dor.CURRVAL,
                                            v_id_referencia_ado,
                                            v_nss_unificado,
                                            v_estado_ado);       
                                                                                            
         LET v_total_unificado = v_total_unificado + 1;                                    
      END IF
   END FOREACH;
   
   FOREACH 
      SELECT id_unificador,
             id_referencia
      INTO   v_resp_id_unificador,
             v_resp_id_referencia
      FROM   uni_resp_int_unificador
      WHERE  folio = p_folio_int_resp_sist_imss
      AND    estado = 1

      FOREACH
         SELECT nss,
                id_referencia_ado
         INTO   v_resp_nss,
                v_resp_id_referencia_ado
         FROM   uni_resp_int_unificado
         WHERE  id_unificador = v_resp_id_unificador
         
         SELECT nsscta1
         INTO   v_resp_nsscta1
         FROM   uni_det_unificado
         WHERE  id_unificado = v_resp_id_referencia_ado
         AND    id_unificador = v_resp_id_referencia;
         
         IF v_resp_nss <> v_resp_nsscta1 THEN
            UPDATE uni_resp_int_unificador
            SET    esatdo_familia = 2
            WHERE  folio = p_folio_int_resp_sist_imss
            AND    estado = 1;
         END IF
      END FOREACH;
   END FOREACH;
   
   FOREACH 
      SELECT f_integracion,
             id_referencia
      INTO   v_env_f_integracion,
             v_env_id_referencia
      FROM   uni_resp_int_unificador
      WHERE  folio = p_folio_int_resp_sist_imss
      AND    estado_familia = 1

      UPDATE uni_det_unificador
      SET    f_aplicacion = v_env_f_integracion
      WHERE  id_unificador = v_env_id_referencia
      AND    estado_familia = 1
      AND    f_aplicacion IS NULL;
      
   END FOREACH;

   UPDATE bat_ctr_operacion 
      SET folio       = p_folio_int_resp_sist_imss,
          nom_archivo = p_nombre_archivo
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 2
      AND pid         = p_pid;  
      
   UPDATE glo_ctr_archivo
      SET folio       = p_folio_int_resp_sist_imss,
          estado      = 2
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 1  -- etapa de carga
      AND estado      = 1; -- archivo cargado
      
   UPDATE STATISTICS FOR TABLE uni_resp_int_unificador;
   UPDATE STATISTICS FOR TABLE uni_resp_int_unificador;
   UPDATE STATISTICS FOR TABLE uni_det_unificador     ;
      
RETURN v_si_resultado, 
       isam_err, 
       err_txt,
       v_total_unificador,
       v_total_unificado;

END FUNCTION;


