






CREATE FUNCTION "safreviv".fn_resumen_indicadores(p_folio_unificacion DECIMAL(10), 
                                       p_usuario_cod       CHAR(20))

RETURNING INTEGER, INTEGER, INTEGER

DEFINE v_id_derechohabiente_unificador DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado  DECIMAL(9,0);
DEFINE v_id_unificador                 DECIMAL(9,0);
DEFINE v_nss_unificador                CHAR(11)    ;
DEFINE v_tipo_originacion              SMALLINT    ;   
DEFINE v_tipo_origin_des               VARCHAR(25) ;
DEFINE v_tipo_credito                  SMALLINT    ;   
DEFINE v_tipo_credito_des              VARCHAR(25) ;
DEFINE v_totales                       INTEGER     ;
DEFINE v_tot_regs                      INTEGER     ;
DEFINE v_desmarca_unificador           SMALLINT    ;
DEFINE v_total_desmarca_unificador     INTEGER     ;
DEFINE v_desmarca_unificado            SMALLINT    ;
DEFINE v_total_desmarca_unificado      INTEGER     ;


LET v_id_derechohabiente_unificador =  0;
LET v_id_derechohabiente_unificado  =  0;
LET v_id_unificador                 =  0;
LET v_nss_unificador                = "";
LET v_tipo_originacion              =  0;
LET v_tipo_origin_des               = "";
LET v_tipo_credito                  =  0;
LET v_tipo_credito_des              = "";
LET v_totales                       =  0;
LET v_tot_regs                      =  0;
LET v_desmarca_unificador           =  0;
LET v_total_desmarca_unificador     =  0;
LET v_desmarca_unificado            =  0;
LET v_total_desmarca_unificado      =  0;

   FOREACH    
      --Recupera datos de UNIFICADOR
      SELECT b.id_unificador,    
             b.nss_unificador,   
             b.id_derechohabiente
      INTO   v_id_unificador,
      	      v_nss_unificador, 
             v_id_derechohabiente_unificador
      FROM   uni_det_unificador b
      WHERE  b.estado_familia = 1
      AND    b.diagnostico = 4   
      AND    b.folio_unificacion = p_folio_unificacion
   
      --Recupera datos unificado
      FOREACH
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente_unificado
         FROM   uni_det_unificado
         WHERE  id_unificador = v_id_unificador
         --Recupera información de tipo de originación y tipo crédito
         FOREACH
            SELECT ca.tpo_originacion,
                   tr.originacion_desc,
                   ca.tpo_credito,
                   tc.desc_credito,
                   COUNT(*)
            INTO   v_tipo_originacion,
                   v_tipo_origin_des ,
                   v_tipo_credito    ,
                   v_tipo_credito_des,
                   v_totales         
            FROM   cre_acreditado ca,
                   cat_cre_originacion tr,
                   cat_tipo_credito tc
            WHERE  ca.id_derechohabiente = v_id_derechohabiente_unificado
            AND    ca.tpo_originacion = tr.tpo_originacion
            AND    ca.tpo_credito = tc.tpo_credito
            AND    ca.estado <> 230
            GROUP BY 1,2,3,4
            ORDER BY 1,3
    
            INSERT INTO safre_tmp:tmp_dae_indicadores
            VALUES(
                   v_tipo_originacion,
                   v_tipo_origin_des ,
                   v_tipo_credito    ,
                   v_tipo_credito_des,
                   v_totales
                   );

         END FOREACH; --Tipo de Crédito

         SELECT COUNT(*)
         INTO   v_desmarca_unificado
         FROM   sfr_marca_activa
         WHERE  marca = 502
         AND    id_derechohabiente = v_id_derechohabiente_unificado;
         
         LET v_total_desmarca_unificado = v_total_desmarca_unificado + v_desmarca_unificado;

      END FOREACH; --Unificado
      
      SELECT COUNT(*)
	    INTO   v_desmarca_unificador
	    FROM   sfr_marca_activa
	    WHERE  marca = 501
	    AND    id_derechohabiente = v_id_derechohabiente_unificador;
      
	    LET v_total_desmarca_unificador = v_total_desmarca_unificador + v_desmarca_unificador ;
   END FOREACH;    --UNIFICADOR  

   RETURN v_tot_regs,      
          v_total_desmarca_unificador, 
          v_total_desmarca_unificado;
END FUNCTION;


