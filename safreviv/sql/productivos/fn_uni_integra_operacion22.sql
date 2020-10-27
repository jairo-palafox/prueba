






CREATE FUNCTION "safreviv".fn_uni_integra_operacion22(p_folio_integracion_op22 DECIMAL(9,0),                         
                                           p_pid                    DECIMAL(9,0),
                                           p_nombre_archivo         CHAR(40))                              

       RETURNING SMALLINT,    --Error SQL
                 SMALLINT,    --Error ISAM
                 VARCHAR(250),--Error Texto
                 INTEGER,     --Total UNIFICADOR
                 INTEGER,     --Total unificado
                 INTEGER      --Total de cuentas actualizadas

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
DEFINE g_proceso_cod        SMALLINT    ;
-- Control de Excepciones
DEFINE sql_err              INTEGER     ;                           
DEFINE isam_err             INTEGER     ;                           
DEFINE err_txt              CHAR(200)   ;  
DEFINE v_si_resultado       SMALLINT    ; 

DEFINE v_fecha_unificacion   DATE        ;

DEFINE v_fecha_texto          CHAR(10);
DEFINE v_fec_proceso          CHAR(8);
--DEFINE v_date_movimiento      DATE;
DEFINE v_date_movimiento      CHAR(8);
DEFINE v_inserta_unificador   INTEGER;
DEFINE v_secuencia_unificador DECIMAL(9,0);
DEFINE v_si_total             INTEGER;
DEFINE v_total_unificador     INTEGER;
DEFINE v_secuencia_unificado  DECIMAL(9,0);                                                                                                                  
DEFINE v_total_unificado      INTEGER;

DEFINE v_resp_id_unificador     DECIMAL(9,0);
DEFINE v_resp_id_referencia     DECIMAL(9,0);
DEFINE v_resp_nss               CHAR(11);
DEFINE v_resp_id_referencia_ado DECIMAL(9,0);
DEFINE v_resp_nsscta1           CHAR(11);

DEFINE v_env_f_integracion      DATE;
DEFINE v_env_id_referencia      DECIMAL(9,0);
DEFINE g_opera_cod              SMALLINT;
DEFINE v_folio_unificacion      DECIMAL(9,0);
DEFINE v_total_actualizados     INTEGER;
DEFINE v_fecha_notificacion     DATE;
DEFINE v_estado_op22_ado        SMALLINT;
        
ON EXCEPTION SET sql_err, 
                 isam_err,
                 err_txt 
   LET v_si_resultado = sql_err;
   LET err_txt = err_txt;
   RETURN v_si_resultado,
          isam_err,
          err_txt,
          v_total_unificador, 
          v_total_unificado,
          v_total_actualizados;
END EXCEPTION 

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/safreviv_int/BD/fn_integra_resp_OP22.trace';
   --TRACE ON;

LET v_total_unificador    = 0;
LET v_total_unificado     = 0;
LET v_si_total            = 0;
LET v_inserta_unificador  = 0;
LET v_total_unificador    = 0;
LET v_total_unificado     = 0;
LET v_total_actualizados  = 0;
LET v_si_resultado        = 0;
LET isam_err              = 0;
LET err_txt               = 'Ok';
LET g_proceso_cod         = 2304;
LET g_opera_cod           = 2;
LET v_estado_op22_ado     = 0;

   LET err_txt = "Al recuperar datos detalle tmp_det_cuenta_notificacion";
   FOREACH 
      --Se consultan los UNIFICADORES DEL ARCHIVO      
      SELECT a.nss_unificador,
             b.nss_cta1,
             --to_date(a.f_integracion,'%Y%m%d'),
             a.f_integracion
        INTO v_nss_unificador,
             v_nss_unificado,
             v_date_movimiento             
        FROM safre_tmp:tmp_det_cuenta_notificacion a,
             safre_tmp:tmp_det_cuentas_unificadas b
       WHERE a.nss_unificador = b.nss_unificador

      --Si el NSS UNIFICADOR existe en la tabla de UNIFICACION                
      LET err_txt = "Al recuperar datos detalle uni_det_unificador";
      IF v_nss_unificador IS NOT NULL THEN
         FOREACH
            SELECT folio_unificacion, 
                   id_unificador
              INTO v_folio_unificacion, 
                   v_id_unificador
              FROM uni_det_unificador 
             WHERE nss_unificador = v_nss_unificador
               AND estado_familia = 1
               AND f_notificacion IS NULL
               AND diagnostico = 6
             
            IF v_id_unificador IS NULL THEN
               LET v_estado     = 2 ; --Rechazado
               LET v_estado_familia = 2 ; --Rechazado
               LET v_id_referencia  = 0;
            ELSE
               LET v_estado     = 1 ; --Aceptado
               LET v_estado_familia = 1 ; --Aceptado
               LET v_id_referencia  = v_id_unificador;
            END IF 
            
            LET err_txt = "Al recuperar datos detalle uni_det_unificado";
            SELECT id_unificado,
                   nsscta1
              INTO v_id_unificado,
                   v_nsscta1
              FROM uni_det_unificado
             WHERE nsscta1 = v_nss_unificado
             AND   id_unificador = v_id_unificador
             AND   estado_unificacion  = 1;
             
            IF v_nsscta1 IS NULL THEN
               LET v_estado_ado     = 2 ; --Rechazado 
               LET v_id_referencia_ado  = 0;
            ELSE
               LET v_estado_ado     = 1 ; --Aceptado
               LET v_id_referencia_ado = v_id_unificado;
            END IF
            LET err_txt = "Al contar uni_resp_op22_unificador";        
            SELECT COUNT(*)                                                        
              INTO v_inserta_unificador                                            
              FROM uni_resp_op22_unificador                                              
             WHERE nss = v_nss_unificador
             AND   folio = p_folio_integracion_op22;                                                                               

            LET err_txt = "Obtiene secuencia unificador";        
            SELECT seq_uni_resp_int_dor.NEXTVAL
              INTO v_secuencia_unificador
              FROM systables
             WHERE tabid = 1;

            -- Corrige fecha de YYYYMMDD a MMDDYYY
            EXECUTE PROCEDURE sp_cambia_formato_fecha(v_date_movimiento)
            INTO v_f_integracion;

            -- si el NSS unificador no se tiene, se inserta                        
            IF ( v_inserta_unificador = 0 ) THEN      
               --Se obtiene el valor de la secuencia de UNIFICADOR 
               LET v_nss            = v_nss_unificador;
               LET v_f_notificacion = v_f_integracion  ;
               INSERT INTO uni_resp_op22_unificador (id_unificador ,
                                                     folio         ,
                                                     id_referencia ,
                                                     nss           ,
                                                     f_integracion ,
                                                     f_notificacion,
                                                     estado        ,
                                                     estado_familia)
                                              VALUES(v_secuencia_unificador,
                                                     p_folio_integracion_op22,
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
            
            LET err_txt = "Obtiene secuencia unificado";                     
            SELECT seq_uni_resp_int_ado.NEXTVAL
              INTO v_secuencia_unificado
              FROM systables
             WHERE tabid = 1;    
            
            INSERT INTO uni_resp_op22_unificado(id_unificado,
                                                 id_unificador,
                                                 id_referencia_ado,
                                                 nss,          
                                                 estado)
                                         VALUES(v_secuencia_unificado,
                                                v_secuencia_unificador,
                                                v_id_referencia_ado,
                                                v_nss_unificado,
                                                v_estado_ado);                                                                                           
            LET v_total_unificado = v_total_unificado + 1;                                    
         END FOREACH;
      END IF
   END FOREACH;

   LET err_txt = "Obtiene referencia y unificador de resp_unificador";                     
   FOREACH 
      SELECT id_unificador,
             id_referencia
      INTO   v_resp_id_unificador,
             v_resp_id_referencia
      FROM   uni_resp_op22_unificador
      WHERE  folio = p_folio_integracion_op22
      AND    estado = 1

      LET err_txt = "Obtiene referencia y unificador de resp_unificado";
      FOREACH
         SELECT nss,
                id_referencia_ado,
                estado
         INTO   v_resp_nss,
                v_resp_id_referencia_ado,
                v_estado_op22_ado
         FROM   uni_resp_op22_unificado
         WHERE  id_unificador = v_resp_id_unificador

         IF v_estado_op22_ado = 2 THEN 
            UPDATE uni_resp_op22_unificador
            SET    estado_familia = 2
            WHERE  id_unificador = v_resp_id_referencia
            AND    folio = p_folio_integracion_op22
            AND    estado = 1;
         END IF    
      END FOREACH;         
   END FOREACH;
         
   LET err_txt = "Obtiene referencia y fecha de integración de resp_unificador";                     
   FOREACH 
      SELECT f_integracion,
             id_referencia
      INTO   v_env_f_integracion,
             v_env_id_referencia
      FROM   uni_resp_op22_unificador
      WHERE  folio = p_folio_integracion_op22
      AND    estado_familia = 1
      
      UPDATE uni_det_unificador
      SET    f_notificacion = v_env_f_integracion
      WHERE  id_unificador = v_env_id_referencia
      AND    estado_familia = 1
      AND    f_notificacion IS NULL
      AND    diagnostico = 6;
      
      LET v_fecha_notificacion = v_env_f_integracion;
   END FOREACH;

   LET err_txt = "Obtiene el total de actualizados";                     
   SELECT COUNT(*)
   INTO   v_total_actualizados
   FROM   uni_resp_op22_unificador
   WHERE  folio = p_folio_integracion_op22
   AND    estado_familia = 1;
   
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio_integracion_op22,
          nom_archivo = p_nombre_archivo
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod   = g_opera_cod
      AND pid         = p_pid;  
          
   UPDATE glo_ctr_archivo
      SET folio       = p_folio_integracion_op22,
          estado      = 2
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod   = 1  -- etapa de carga
      AND estado      = 1; -- archivo cargado
      
   UPDATE STATISTICS FOR TABLE uni_resp_op22_unificador;
   UPDATE STATISTICS FOR TABLE uni_resp_op22_unificado;
   UPDATE STATISTICS FOR TABLE uni_det_unificador     ;
      
RETURN v_si_resultado, 
       isam_err, 
       err_txt,
       v_total_unificador,
       v_total_unificado,
       v_total_actualizados;

END FUNCTION;


