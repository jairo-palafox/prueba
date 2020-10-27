






CREATE FUNCTION "safreviv".consulta_posibles_desmarcas()
RETURNING INTEGER, 
          INTEGER,
          CHAR(200);

DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_id_unificador          DECIMAL(9,0);         
DEFINE v_folio_unificacion      DECIMAL(9,0);        
DEFINE v_folio_resp_confronta   DECIMAL(9,0);        
DEFINE v_id_unificado           DECIMAL(9,0);
DEFINE v_folio_unificacion_ado  DECIMAL(9,0);
DEFINE v_id_derechohabiente_ado DECIMAL(9,0);
DEFINE v_id_dh_marca_ado        DECIMAL(9,0); 
DEFINE v_n_referencia_ado       DECIMAL(9,0);
DEFINE v_id_dh_marca_dor        DECIMAL(9,0); 
DEFINE v_n_referencia_dor       DECIMAL(9,0);
DEFINE v_proceso_cod            SMALLINT;
DEFINE v_tot_desmarca           INTEGER; 
DEFINE v_marca_unificador       SMALLINT; 
DEFINE v_marca_unificado        SMALLINT;
DEFINE v_bnd_marca              SMALLINT;
DEFINE v_nss_unificador         CHAR(11);
DEFINE v_nss_unificado          CHAR(11);

-- Control de Excepciones              
DEFINE sql_err             INTEGER;
DEFINE isam_err            INTEGER;
DEFINE err_txt             VARCHAR(255);
DEFINE v_resultado         SMALLINT;

-- se configura el regreso del codigo de error
ON EXCEPTION SET sql_err, isam_err, err_txt
   LET v_resultado = sql_err;
   RETURN v_resultado,
          isam_err, 
          err_txt;
END EXCEPTION

LET v_marca_unificador = 501;
LET v_marca_unificado = 502;
LET v_proceso_cod = 2318;
LET v_tot_desmarca = 0;
LET v_nss_unificador  = 0;
LET v_nss_unificado = 0;


LET v_resultado =0;
LET isam_err = 0; 
LET err_txt = "Consulta finalizada";
   
   FOREACH
      SELECT a.id_derechohabiente,
             a.id_unificador,
             a.folio_resp_confronta,
             b.nss_unificador
      INTO   v_id_derechohabiente,
             v_id_unificador,
             v_folio_resp_confronta,
             v_nss_unificador
      FROM   uni_det_procedencia a
      INNER JOIN uni_det_unificador b
      ON     a.id_unificador = b.id_unificador
      WHERE  a.ind_procedencia IN (0,1)
      AND    b.estado_familia = 1
   
   --
      SELECT id_derechohabiente, 
             n_referencia
      INTO   v_id_dh_marca_dor, 
             v_n_referencia_dor
      FROM   sfr_marca_activa
      WHERE  marca = v_marca_unificador
      AND    id_derechohabiente = v_id_derechohabiente;
   
      --INSERT INTO safre_tmp:uni_datos_desmarca VALUES (v_nss_unificador, v_nss_unificado);
   
         FOREACH
            SELECT id_unificado,
                   id_derechohabiente,
                   nsscta1
            INTO   v_id_unificado,
                   v_id_derechohabiente_ado,
                   v_nss_unificado
            FROM   uni_det_unificado
            WHERE  id_unificador = v_id_unificador
            AND    estado_unificacion = 1             
            
            SELECT id_derechohabiente, 
                   n_referencia
            INTO   v_id_dh_marca_ado, 
                   v_n_referencia_ado
            FROM   sfr_marca_activa
            WHERE  marca = v_marca_unificado
            AND    id_derechohabiente = v_id_derechohabiente_ado;
      
            IF v_id_dh_marca_ado IS NOT NULL THEN
               INSERT INTO safre_tmp:uni_datos_desmarca VALUES (v_nss_unificador, v_nss_unificado);
            END IF

         END FOREACH
   END FOREACH ;
   
RETURN v_resultado,
          isam_err, 
          err_txt;
END FUNCTION;


