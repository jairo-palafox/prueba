






CREATE FUNCTION "safreviv".fn_uni_desmarca_recurrente()
RETURNING SMALLINT, 
          CHAR(200)


DEFINE v_det_nss_unificador CHAR(11);
DEFINE v_det_nss_unificado CHAR(11);
DEFINE v_folio_unificacion DECIMAL(9,0);
DEFINE v_id_dh_dor DECIMAL(9,0);
DEFINE v_id_dh_ado DECIMAL(9,0);
DEFINE v_marca_dor SMALLINT; 
DEFINE v_marca_ado SMALLINT;
DEFINE v_marca_inac SMALLINT;

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE v_i_resultado SMALLINT;
DEFINE err_txt CHAR(200);


ON EXCEPTION SET sql_err, isam_err
   LET v_i_resultado = sql_err;
       RETURN v_i_resultado, 
              err_txt;
END EXCEPTION

LET v_det_nss_unificador = "";
LET v_det_nss_unificado  = "";
LET v_id_dh_dor = 0;
LET v_id_dh_ado = 0; 
LET v_marca_dor  = 501;
LET v_marca_ado  = 502;
LET v_marca_inac = 150;
LET v_i_resultado = 0;
LET err_txt = "El reverso ha concluido correctamente";

   FOREACH
      SELECT nss_unificador
      INTO   v_det_nss_unificador
      FROM   safre_tmp:tmp_det_uni_recurrente
      
      SELECT id_derechohabiente
      INTO   v_id_dh_dor
      FROM   afi_derechohabiente 
      WHERE  nss = v_det_nss_unificador;
      
      IF v_id_dh_dor IS NOT NULL THEN   
         DELETE FROM sfr_marca_activa
         WHERE id_derechohabiente  = v_id_dh_dor
         AND marca = v_marca_dor;
         
         DELETE FROM sfr_marca_historica
         WHERE id_derechohabiente  = v_id_dh_dor
         AND marca = v_marca_dor;
         
         LET v_det_nss_unificador = "";
         LET v_id_dh_dor = 0;
      END IF
   END FOREACH
   
   FOREACH
         SELECT nss_unificado
         INTO   v_det_nss_unificado
         FROM   safre_tmp:tmp_det_uni_recurrente

         SELECT id_derechohabiente
         INTO   v_id_dh_ado
         FROM   afi_derechohabiente 
         WHERE  nss = v_det_nss_unificado;
         
         IF v_id_dh_ado IS NOT NULL THEN
            DELETE FROM sfr_marca_activa
            WHERE id_derechohabiente  = v_id_dh_ado
            AND marca = v_marca_ado;
            
            DELETE FROM sfr_marca_historica
            WHERE id_derechohabiente  = v_id_dh_ado
            AND marca = v_marca_ado;
      
            DELETE FROM sfr_marca_activa
            WHERE id_derechohabiente  = v_id_dh_ado
            AND marca = v_marca_inac;
            
            DELETE FROM sfr_marca_historica
            WHERE id_derechohabiente  = v_id_dh_ado
            AND marca = v_marca_inac;
            
            UPDATE afi_derechohabiente
            SET    ind_estado_cuenta = 0
            WHERE id_derechohabiente = v_id_dh_ado;
            
            LET v_det_nss_unificado  = "";
            LET v_id_dh_ado = 0; 
         END IF
   END FOREACH
   
   
   SELECT MAX(folio)
   INTO   v_folio_unificacion
   FROM   glo_folio
   WHERE  proceso_cod = 2314
   AND    opera_cod = 2
   AND    f_actualiza = TODAY;
   
   DELETE FROM uni_det_unificador 
   WHERE folio_unificacion = v_folio_unificacion;
   
   DELETE FROM uni_det_unificado
   WHERE folio_unificacion = v_folio_unificacion;

   DELETE FROM glo_folio
   WHERE folio = v_folio_unificacion;
   
   DELETE FROM glo_ctr_archivo
   WHERE folio = 135140;

   DELETE FROM uni_det_unificador 
   WHERE folio_unificacion = 135140;
   
   DELETE FROM uni_det_unificado
   WHERE folio_unificacion = 135140;

   DELETE FROM uni_preliquida
   WHERE folio_liquida = 135141;   
   
   DELETE FROM cta_movimiento
   WHERE folio_liquida = 135141;

   DELETE FROM cnt_transaccion
   WHERE folio_liquida = 135141;
   
   DELETE FROM glo_folio
   WHERE folio IN (135140, 135141);
   
   DELETE FROM glo_ctr_archivo
   WHERE folio = 135140;

   RETURN v_i_resultado, 
          err_txt;
END FUNCTION;


